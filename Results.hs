{-# LANGUAGE TemplateHaskell #-}

module Results
    ( ProblemSummary(..)
    , SolutionSummary(..)
    , SolutionDetails(..)
    ) where

import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.Text as T


import Database.Persist
import           Models


data ProblemSummary
  = ProblemSummary
    { psId :: ProblemId
    , psTitle :: T.Text
    }

data SolutionSummary
  = SolutionSummary
    { ssId :: SolutionId
    , ssOfProblem :: ProblemId
    , ssTestRunStates :: [TestRunState]
    }

data SolutionDetails
  = SolutionDetails
    { sdSolution :: Solution
    , sdTestRuns :: [Entity TestRun]
    }

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (length "ps")} ''ProblemSummary)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (length "ss")} ''SolutionSummary)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (length "sd")} ''SolutionDetails)
