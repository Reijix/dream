module AnalysisError where

import Text.Parsec (SourcePos)

data AnalysisError
  = NameError SourcePos String
  | TypeError SourcePos String
  deriving (Show)
