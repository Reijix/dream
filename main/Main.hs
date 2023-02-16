{-# LANGUAGE BangPatterns #-}
module Main where
import Options.Applicative
import Data.Semigroup ((<>))

import Dot (generateDotFile)
import Parser (parseProgram)
import ConstantFolding (foldConstants)
import NameAnalysis (doNameAnalysis)
import Data.List (genericTake)
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )

data CmdOption = CmdOption
  {
    sourceFile :: String,
    destinationFile :: String,
    dotFile :: String
  }

cmdOption :: Parser CmdOption
cmdOption = CmdOption
        <$> argument str (
                metavar "<source file>"
            )
        <*> strOption
            (
                short 'o'
                <> metavar "<destination file>"
                <> help "Place the output into <destination file>."
                <> showDefault
                <> value "a.out"
            )
        <*> strOption
            (
              long "dot"
              <> metavar "<dot file>"
              <> help "Dump the AST in .dot format to <dot file>."
              <> value ""
            )

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (cmdOption <**> helper)
      ( fullDesc
     <> progDesc "Compiles a given dream source-code."
     <> header "dreamc, the compiler for the dream language" )

run :: CmdOption -> IO ()
-- no dotfile
run (CmdOption _ _ []) = return ()
-- dotfile
run (CmdOption sourceFile destinationFile dotFile) = do
  source <- openFile sourceFile ReadMode
  sourceText <- hGetContents source

  let ast = parseProgram sourceFile sourceText
  case ast of
    Left err -> print err
    Right program -> do
      -- do constantFolding
      let !cfProg = foldConstants program
      -- do nameAnalysis
      let !symbTable = doNameAnalysis program
      print cfProg
      generateDotFile dotFile cfProg