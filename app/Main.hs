module Main (main) where

import Brainhuck.Interpreter2
import Brainhuck.Options
import Options.Applicative (execParser)
import Data.Text as T
import Data.Text.IO as TIO

main :: IO ()
main = do
  (Options {-optsDebug-} optsSize optsInput {-optsOldInt-}) <- execParser options
  programString <- case optsInput of
                     FileInput filePath -> TIO.readFile filePath
                     StdInput stdinStr -> pure $ T.pack stdinStr
  tryToInterpret programString (initializeProgramState optsSize)
