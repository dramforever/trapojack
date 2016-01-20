{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Main (main) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Trans.Either
import           Database.Persist
import           Database.Persist.Sqlite
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Network.Wai.Handler.Warp
import           Servant

import Models

type Trapojack = "submissions" :> Get '[JSON] [SolutionId]

            :<|> "submissions" :> Capture "id" SolutionId
                               :> Get '[JSON] SolutionResults

            :<|> "submissions" :> ReqBody '[PlainText] T.Text
                               :> Post '[JSON] SolutionId

------------------------------------------------------------------------------

getSolutionsList :: SqlBackend
                 -> EitherT ServantErr IO [SolutionId]
getSolutionsList backend =
  runSqlConn (selectKeysList [] []) backend

------------------------------------------------------------------------------

getSolutionResults :: SqlBackend -> SolutionId
                   -> EitherT ServantErr IO SolutionResults
getSolutionResults backend solutionId =
  runSqlConn (get solutionId) backend >>= \case
    Nothing -> throwError err404
    Just solution -> do
      runs <- runSqlConn (selectKeysList [TestRunOfSolution ==. solutionId]
                                         [Asc TestRunNumber]) backend
      return (SolutionResults solution runs)

------------------------------------------------------------------------------

postSolutions :: SqlBackend -> T.Text
              -> EitherT ServantErr IO SolutionId
postSolutions backend code = do
  
  runSqlConn (insert (Solution Compiling fileName)) backend

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
