module AnalysisError where

import Text.Parsec (SourcePos)

data AnalysisError
  = NameError SourcePos String
  | TypeError SourcePos String

instance Show AnalysisError where
  show (NameError sourcePos err) = "Encountered error during nameanalysis:\n" ++ show err ++ "\nat: " ++ show sourcePos
  show (TypeError sourcePos err) = "Encountered error during typeanalysis:\n" ++ show err ++ "\nat: " ++ show sourcePos
