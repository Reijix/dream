{-# LANGUAGE BangPatterns #-}

module Main where

import AnalysisError (AnalysisError (NameError, TypeError))
import CodeGenerator
import ConstantFolding (foldConstants)
import Control.Monad.Except
import Data.Either.Extra (mapLeft)
import Data.Semigroup ((<>))
import Dot (generateDotFile)
import DumpIR (dumpIR)
import IRGenerator (generateIR)
import NameAnalysis (doNameAnalysis)
import Options.Applicative
import Parser (parseProgram)
import ParserError
import System.Directory.Extra (removeFile)
import System.IO (IOMode (ReadMode, WriteMode), hClose, hGetContents, openFile)
import System.Process
import TypeAnalysis (doTypeAnalysis)

data CmdOption = CmdOption
  { sourceFile :: String,
    destinationFile :: String,
    dotFile :: Bool,
    keep :: Bool,
    irDump :: Bool,
    noRuntime :: Bool
  }

cmdOption :: Parser CmdOption
cmdOption =
  CmdOption
    <$> argument
      str
      ( metavar "<source file>"
      )
    <*> strOption
      ( long "out"
          <> short 'o'
          <> metavar "<destination file>"
          <> help "Place the output into <destination file>."
          <> showDefault
          <> value "out"
      )
    <*> switch
      ( long "dot"
          <> short 'd'
          <> help "Dump the AST in .dot format (as used by graphviz) to <destination>.dot and <destination>.typed.dot (being the AST after the typeanalysis)"
      )
    <*> switch
      ( long "keep"
          <> short 'k'
          <> help "Keep all files created in the build process (e.g. _.s, _.o, ...)"
      )
    <*> switch
      ( long "dump-ir"
          <> short 'i'
          <> help "Dump the immediate representation to a file <destination>.ir"
      )
    <*> switch
      ( long "no-runtime"
          <> short 'r'
          <> help "Build the program without a runtime library (useful for compiling your own runtime functions)"
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

data CompilerError
  = AnalysisError AnalysisError
  | ParserError ParserError

instance Show CompilerError where
  show (AnalysisError ae) = "Compiling your program failed during analysis!\n" ++ show ae
  show (ParserError pe) = "Compiling your program failed during parsing!\n" ++ show pe

run :: CmdOption -> IO ()
run cmdOption = do
  result <- runExceptT $ runM cmdOption
  case result of
    Left err -> print err
    Right _ -> return ()

runM :: CmdOption -> ExceptT CompilerError IO ()
runM (CmdOption sourceFile destinationFile dot keep irDump noRuntime) = do
  -- read in sourcefile
  source <- liftIO $ openFile sourceFile ReadMode
  sourceText <- liftIO $ hGetContents source

  -- parse program
  !ast <- liftEither . mapLeft ParserError $ parseProgram sourceFile sourceText

  -- print dotFile if --dot is set
  liftIO $
    when
      dot
      ( do
          let dotFileName = destinationFile ++ ".dot"
          liftIO $ generateDotFile dotFileName ast
      )

  -- do nameAnalysis
  !symbolTable <- liftEither . mapLeft AnalysisError $ doNameAnalysis ast

  -- do constantFolding
  let !cfAst = foldConstants ast

  -- do typeAnalysis
  (taSymbolTable, taAst) <- liftEither . mapLeft AnalysisError $ doTypeAnalysis symbolTable cfAst

  -- print typed dotFile if --dot is set
  liftIO $
    when
      dot
      ( do
          let typedDotFileName = destinationFile ++ ".typed.dot"
          generateDotFile typedDotFileName taAst
      )

  -- generate IR
  let ir = generateIR taSymbolTable taAst

  -- dump IR if --dump-ir is set
  liftIO $
    when
      irDump
      ( do
          let irFileName = destinationFile ++ ".ir"
          irFile <- liftIO $ openFile irFileName WriteMode
          liftIO $ dumpIR irFile ir
          liftIO $ hClose irFile
      )

  -- create assembly
  let assemblyFileName = destinationFile ++ ".s"
  exeFile <- liftIO $ openFile assemblyFileName WriteMode
  liftIO $ generateCode exeFile ir
  liftIO $ hClose exeFile

  -- compile stdlib
  liftIO $ callCommand "cd runtime_lib && make"

  -- assemble program
  let objectFileName = destinationFile ++ ".o"
  liftIO . callCommand $ "as " ++ assemblyFileName ++ " -o " ++ objectFileName

  -- link with stdlib
  liftIO . callCommand $ "ld " ++ objectFileName ++ " runtime_lib/dreamlib.a -o " ++ destinationFile

  -- make executable
  liftIO . callCommand $ "chmod +x " ++ destinationFile

  -- cleanup if --keep not set
  liftIO $
    unless
      keep
      ( do
          removeFile assemblyFileName
          removeFile objectFileName
      )
