module Main (main) where

import Lib
import Options.Applicative

data Command
  = Generate
  | Lock
  deriving (Show, Eq)

data Args = Args
  { cmd :: Command
  }

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1.0.0" (long "version" <> help "Show version")

generateCommand :: Mod CommandFields Command
generateCommand =
  command "generate" (info (pure Generate) (progDesc "Generate static website"))

lockCommand :: Mod CommandFields Command
lockCommand =
  command "lock" (info (pure Lock) (progDesc "Lock generated site characteristics"))

commands :: Parser Command
commands = hsubparser
  ( generateCommand
 <> lockCommand
  )

args :: Parser Args
args = Args <$> commands

opts :: ParserInfo Args
opts = info (helper <*> versionOption <*> args)
  ( fullDesc
 <> progDesc "A generic site builder powered by AI"
 <> header "builder - AI Site Generator"
  )

main :: IO ()
main = do
  parsedArgs <- execParser opts
  case cmd parsedArgs of
    Generate -> putStrLn "TODO: Generating site..."
    Lock     -> putStrLn "TODO: Locking site..."
