{-# LANGUAGE BangPatterns #-}
module Main where
import Options.Applicative
import Data.Semigroup ((<>))

import Dot (generateDotFile)
import Parser (parseProgram)
import ConstantFolding (foldConstants)
import NameAnalysis (doNameAnalysis)
import Data.List (genericTake)
import System.IO ( hGetContents, openFile, IOMode(ReadMode, WriteMode), hClose )
import TypeAnalysis (doTypeAnalysis)
import AnalysisError (AnalysisError(TypeError, NameError))
import SymbolTable (printSymbolTable, showSymbolTable)
import IRGenerator (generateIR)
import DumpIR (dumpIR)

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