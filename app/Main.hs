module Main (main) where

import Brainhuck.Interpreter 
import Brainhuck.Options
import Options.Applicative (execParser)

main :: IO ()
main = do
  (Options optsSize optsInput ) <- execParser options
  programString <- case optsInput of
                     FileInput filePath -> readFile filePath
                     StdInput stdinStr -> pure stdinStr
  interpretBF optsSize programString
   
  -- helloWorld
