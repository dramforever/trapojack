{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}

module Models
    ( TestRun(..), TestRunId
    , Solution(..), SolutionId
    , SolutionState(..)
    , TestRunState(..)
    , migrateAll
    ) where

import Database.Persist.TH
import Data.Aeson

import SolutionState

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TestRun
    bundle String
    number Int
    state TestRunState
    log String
    deriving Show
Solution
    state SolutionState
    source String
    deriving Show
|]

-- TODO
instance ToJSON TestRun where
  toJSON = undefined

-- TODO
instance FromJSON Solution where
  parseJSON = undefined
