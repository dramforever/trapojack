{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}

module Models
    ( TestRun(..) , TestRunId
    , Solution(..), SolutionId
    , Problem(..), ProblemId
    , SolutionState(..), TestRunState(..)
    , EntityField(..), Key(..)
    , migrateAll
    ) where

import           Database.Persist.TH
import           Data.Aeson.TH

import           Servant
import           Database.Persist
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Aeson

import           Data.Aeson.Types
import           States


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TestRun
  ofSolution SolutionId
  number Int
  state TestRunState
  log Text
  deriving Show
Solution
  ofProblem ProblemId
  state SolutionState
  cuid Text
  UniqueFileId cuid
  deriving Show
Problem
  title Text
  description Text
  handle Text
  UniqueHandle handle
  deriving Show
|]

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (length "TestRun")} ''TestRun)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (length "Solution")} ''Solution)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (length "Problem")} ''Problem)

instance ToJSON (Entity TestRun) where
  toJSON (Entity k v) = object [T.pack "id" .= k, T.pack "value" .= v]

instance FromText (Key Solution) where
  fromText x = SolutionKey . fromInteger <$> fromText x

instance FromText (Key Problem) where
  fromText x = ProblemKey . fromInteger <$> fromText x
