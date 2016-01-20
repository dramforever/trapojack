{-# LANGUAGE TemplateHaskell #-}

module Results
    (
    ) where

import qualified Data.Text as T

import           Models

data ProblemSummary
  = ProblemSummary
    { psId :: ProblemId
    , psTitle :: T.Text
    }


$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (length "ps")} ''ProblemSummary)


data AllSolutions

instance ToJSON AllSolutions where
  toJSON _ = undefined


data SolutionDetails
  = SolutionDetails
    { sdSolution :: Solution
    , sdTestRuns :: [TestRunId]
    }

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (length "sd")} ''SolutionDetails)
