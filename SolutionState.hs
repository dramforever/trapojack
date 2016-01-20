{-# LANGUAGE TemplateHaskell #-}
module SolutionState
    ( SolutionState(..)
    , TestRunState(..)
    ) where

import Database.Persist.TH

data SolutionState
  = SolutionCompiling
  | SolutionCompiled
  deriving (Show, Read, Eq)

data TestRunState
  = Running
  | Accepted
  | WrongAnswer
  | PresentationError
  | KilledSignal
  | KilledTLE
  | KilledMLE
  | KilledOLE
  | KilledRE
  deriving (Show, Read, Eq)

$(derivePersistField "TestRunState")
$(derivePersistField "SolutionState")
