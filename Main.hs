{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main (main) where

import Servant.API
import Control.Monad.Trans.Either
import Servant
import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Control.Monad.Logger
import Control.Monad.Reader

import Models

type Trapojack = "test_runs" :> Get '[JSON] [TestRunId]

            :<|> "test_runs" :> Capture "id" Int
                             :> Get '[JSON] TestRun

            :<|> "submissions" :> Get '[JSON] [SolutionId]

            :<|> "submissions" :> ReqBody '[JSON] Solution
                               :> Post '[JSON] SolutionId

-- TODO
getTestRunsList :: SqlBackend -> EitherT ServantErr IO [TestRunId]
getTestRunsList _backend = undefined

-- TODO
getTestRuns :: SqlBackend -> Int -> EitherT ServantErr IO TestRun
getTestRuns _backend = undefined

-- TODO
getSolutions :: SqlBackend -> EitherT ServantErr IO [SolutionId]
getSolutions _backend = undefined

-- TODO
postSolutions :: SqlBackend -> Solution -> EitherT ServantErr IO SolutionId
postSolutions _backend = undefined

trapojack :: SqlBackend -> Server Trapojack
trapojack backend = getTestRunsList backend :<|> getTestRuns backend
               :<|> getSolutions backend :<|> postSolutions backend

main :: IO ()
main = runNoLoggingT
     . withSqliteConn "dev.db" $ \backend ->
         NoLoggingT $ do
           runSqlConn (runMigration migrateAll) backend
           liftIO $ run 2333 (serve (Proxy :: Proxy Trapojack)
                                    (trapojack backend))
