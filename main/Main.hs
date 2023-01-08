module Main where
import Options.Applicative
import Data.Semigroup ((<>))

data CmdOption = CmdOption
  {
    sourceFile :: String,
    destinationFile :: String
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

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (cmdOption <**> helper)
      ( fullDesc
     <> progDesc "Compiles a given dream source-code."
     <> header "dreamc, the compiler for the dream language" )

run :: CmdOption -> IO ()
run _ = return ()