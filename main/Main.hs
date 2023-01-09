module Main where
import Options.Applicative
import Data.Semigroup ((<>))

import Dot (generateDotFile)
import Parser (parseProgram)
import Data.List (genericTake)
import System.IO

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

  let ast = parseProgram sourceText
  case ast of
    Left err -> print err
    Right program -> do
      print program
      generateDotFile dotFile program