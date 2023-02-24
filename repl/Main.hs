module Main where

import Control.Monad.Trans
import Parser
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseProgram "<stdin>" line
  case res of
    Left err -> print err
    Right ex -> print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
