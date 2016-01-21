{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

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
import           Web.Cuid

import           Models
import           Results

type Trapojack = "solutions"
                 :> Get '[JSON] [SolutionSummary]

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

getSolutionsList :: SqlBackend
                -> EitherT ServantErr IO [SolutionSummary]
getSolutionsList backend =
  runSqlConn (map go <$> selectList [] []) backend
  where go (Entity key Solution{..}) =
          SolutionSummary key solutionOfProblem solutionState

------------------------------------------------------------------------------

getSolutionDetails :: SqlBackend -> SolutionId
                   -> EitherT ServantErr IO SolutionDetails
getSolutionDetails backend solutionId =
  runSqlConn (get solutionId) backend >>= \case
    Nothing -> throwError err404
    Just solution -> do
      runs <- runSqlConn (selectList [] []) backend
      return (SolutionDetails solution runs)

------------------------------------------------------------------------------

getProblemsList :: SqlBackend -> EitherT ServantErr IO [ProblemSummary]
getProblemsList backend =
    runSqlConn (map go <$> selectList [] []) backend
  where go (Entity key Problem{..}) =
          ProblemSummary key problemTitle

------------------------------------------------------------------------------

getProblem :: SqlBackend -> ProblemId -> EitherT ServantErr IO Problem
getProblem backend pid = runSqlConn (get pid) backend >>= \case
  Nothing -> throwError err404
  Just p -> return p

------------------------------------------------------------------------------

postSolution :: SqlBackend -> ProblemId -> T.Text
              -> EitherT ServantErr IO SolutionId
postSolution backend pid code = do
  fileId <- newCuid
  liftIO $ T.writeFile ("filestore/solution_" ++ T.unpack fileId) code
  runSqlConn (insert (Solution pid Compiling fileId)) backend

------------------------------------------------------------------------------

trapojack :: SqlBackend -> Server Trapojack
trapojack backend = getSolutionsList backend
               :<|> getSolutionDetails backend
               :<|> getProblemsList backend
               :<|> getProblem backend
               :<|> postSolution backend

main :: IO ()
main = runStdoutLoggingT
     . withSqliteConn "dev.db" $ \backend ->
         LoggingT $ \_logger -> do
           runSqlConn (runMigration migrateAll) backend
           liftIO $ run 2333 (serve (Proxy :: Proxy Trapojack)
                                    (trapojack backend))
