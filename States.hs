{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module States
    ( SolutionState(..)
    , TestRunState(..)
    ) where

import Data.Aeson
import Database.Persist.TH


data SolutionState
  = Compiling
  | Compiled
  | CompileError
  deriving (Show, Read, Eq)

data TestRunState
  = Running
  | Accepted
  | WrongAnswer
  | PresentationError
  | RuntimeError
  | TimeLimitExceeded
  | MemoryLimitExceeded
  | OutputLimitExceeded
  | InternalError
  deriving (Show, Read, Eq)

$(derivePersistField "TestRunState")
$(derivePersistField "SolutionState")

instance ToJSON SolutionState where
  toJSON st = case st of
    Compiling    -> "Compiling"
    Compiled     -> "Compiled"
    CompileError -> "CE"

instance ToJSON TestRunState where
  toJSON st = case st of
    Running             -> "Running"
    Accepted            -> "AC"
    WrongAnswer         -> "WA"
    PresentationError   -> "PE"
    RuntimeError        -> "RE"
    TimeLimitExceeded   -> "TLE"
    MemoryLimitExceeded -> "MLE"
    OutputLimitExceeded -> "OLE"
    InternalError       -> "Internal Error"
