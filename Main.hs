{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main (main) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.TH
import qualified Database.Persist           as P
import qualified Database.Persist.Sqlite    as P
import           Database.Esqueleto
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Network.Wai.Handler.Warp
import           Servant
import           Web.Cuid

import           Models
import           Results

type Trapojack = "solutions"
                 :> Get '[JSON] AllSolutions

            :<|> "solutions" :> Capture "id" SolutionId
                 :> Get '[JSON] SolutionDetails

            :<|> "problems"
                 :> Get '[JSON] [ProblemSummary]

            :<|> "problems" :> Capture "pid" ProblemId
                 :> Get '[JSON] Problem

            :<|> "problems" :> Capture "pid" ProblemId
                 :> "solutions" :> ReqBody '[PlainText] T.Text
                 :> Post '[JSON] SolutionId

------------------------------------------------------------------------------

getAllSolutions :: SqlBackend
                -> EitherT ServantErr IO AllSolutions
getAllSolutions _backend = undefined

------------------------------------------------------------------------------

getSolutionResults :: SqlBackend -> SolutionId
                   -> EitherT ServantErr IO SolutionDetails
getSolutionResults backend solutionId =
  runSqlConn (get solutionId) backend >>= \case
    Nothing -> throwError err404
    Just solution -> do
      runs <- runSqlConn (selectKeysList [TestRunOfSolution ==. solutionId]
                                         [Asc TestRunNumber]) backend
      return (SolutionDetails solution runs)

------------------------------------------------------------------------------

getProblemsList :: SqlBackend -> ExceptT ServantErr IO [ProblemSummary]
getProblemsList backend =
  flip runSqlConn backend $
    select $ from $ \p -> return (ProblemSummary (p ^. ProblemId) (p ^. ProblemTitle))

------------------------------------------------------------------------------

postSolutions :: SqlBackend -> T.Text
              -> EitherT ServantErr IO SolutionId
postSolutions backend code = do
  fileID <- newCuid
  runSqlConn (insert (Solution Compiling fileID)) backend

------------------------------------------------------------------------------

trapojack :: SqlBackend -> Server Trapojack
trapojack backend = getSolutionsList backend
               :<|> getSolutionResults backend
               :<|> postSolutions backend

main :: IO ()
main = runNoLoggingT
     . withSqliteConn "dev.db" $ \backend ->
         NoLoggingT $ do
           runSqlConn (runMigration migrateAll) backend
           liftIO $ run 2333 (serve (Proxy :: Proxy Trapojack)
                                    (trapojack backend))
