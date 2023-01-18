module Brainhuck.Options
  ( options
  , Options (..)
  , Input (..)
  ) where

import Options.Applicative

data Input
  = FileInput FilePath
  | StdInput String

data Options
  = Options
      { oDebug :: Bool
      , oSize  :: Int
      , oInput :: Input
      }

options :: ParserInfo Options
options = info (opts <**> helper )
  (  fullDesc
  <> header "Brainhuck - a Brainfuck interpreter written in Haskell"
  )
 
opts :: Parser Options
opts = Options <$> debug <*> size <*> input

debug :: Parser Bool
debug = switch 
  (
    long "debug"
  <> short 'd'
  <> help "Run in debugging mode"
  <> showDefault
  )

size :: Parser Int
size = option auto
  (  long "size"
  <> short 's'
  <> help "Size of the memory array"
  <> showDefault
  <> value 100
  <> metavar "INT"
  )

input :: Parser Input
input = inputType <*> inputString

inputString :: Parser String
inputString = strArgument
  (  metavar "FILENAME"
  <> help "Input file"
  <> action "directory"
  <> action "file"
  )

inputType :: Parser (String -> Input)
inputType = flag FileInput StdInput
  (  long "stdin"
  <> help "Read BF program from stdin"
  )

