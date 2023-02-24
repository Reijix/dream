{-# LANGUAGE BangPatterns #-}

module Main where

import AnalysisError (AnalysisError (NameError, TypeError))
import CodeGenerator
import ConstantFolding (foldConstants)
import Data.List (genericTake)
import Data.Semigroup ((<>))
import Dot (generateDotFile)
import DumpIR (dumpIR)
import IRGenerator (generateIR)
import NameAnalysis (doNameAnalysis)
import Options.Applicative
import Parser (parseProgram)
import SymbolTable (printSymbolTable, showSymbolTable)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetContents, openFile)
import System.Process
import TypeAnalysis (doTypeAnalysis)

data CmdOption = CmdOption
  { sourceFile :: String,
    destinationFile :: String,
    dotFile :: String
  }

cmdOption :: Parser CmdOption
cmdOption =
  CmdOption
    <$> argument
      str
      ( metavar "<source file>"
      )
    <*> strOption
      ( short 'o'
          <> metavar "<destination file>"
          <> help "Place the output into <destination file>."
          <> showDefault
          <> value "a.out"
      )
    <*> strOption
      ( long "dot"
          <> metavar "<dot file>"
          <> help "Dump the AST in .dot format to <dot file>."
          <> value ""
      )

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (cmdOption <**> helper)
        ( fullDesc
            <> progDesc "Compiles a given dream source-code."
            <> header "dreamc, the compiler for the dream language"
        )

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
    Right !program -> do
      -- do constantFolding
      let !cfProg = foldConstants program
      generateDotFile dotFile cfProg
      -- do nameAnalysis
      let !naResult = doNameAnalysis program
      let !symbTable = case naResult of
            Left (NameError sourcePos err) -> error $ "NameError at " ++ show sourcePos ++ "\n" ++ err
            Right st -> st
      -- do typeAnalysis
      let !mResult = doTypeAnalysis symbTable program
      let !(st, taProg) = case mResult of
            Left (TypeError sourcePos err) -> error $ "TypeError at " ++ show sourcePos ++ "\n" ++ err
            Right (st, taProg) -> (st, taProg)
      -- print taProg
      generateDotFile dotFile taProg
      let ir = generateIR st taProg
      print ir
      irFile <- openFile "foo.ir" WriteMode
      dumpIR irFile ir
      hClose irFile
      exeFile <- openFile "foo.s" WriteMode
      generateCode exeFile ir
      hClose exeFile
      -- compile stdlib
      callCommand "cd runtime_lib && make"
      callCommand "as foo.s -o foo.out"
      callCommand "ld foo.out runtime_lib/dreamlib.a -o foo.exe"
      callCommand "chmod +x foo.exe"
