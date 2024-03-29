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
      { {-oDebug :: Bool
      ,-} oSize  :: Int
      , oInput :: Input
      -- , oOldInterpreter :: Bool
      }

options :: ParserInfo Options
options = info (opts <**> helper )
  (  fullDesc
  <> header "Brainhuck - a Brainfuck interpreter written in Haskell"
  )

opts :: Parser Options
opts = Options <$>{- debug <*>-} size <*> input {-<*> oldInterpreter-}


-- oldInterpreter :: Parser Bool
-- oldInterpreter = switch
--   (
--      long "old-interpeter"
--   <> short 'o'
--   <> help "Use the old interpeter to execute code"
--   <> showDefault
--   )


-- debug :: Parser Bool
-- debug = switch 
--   (
--     long "debug"
--   <> short 'd'
--   <> help "Run in debugging mode"
--   <> showDefault
--   )

size :: Parser Int
size = option auto
  (  long "size"
  <> short 's'
  <> help "Size of the memory array"
  <> showDefault
  <> value 500
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

