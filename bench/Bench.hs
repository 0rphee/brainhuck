module Main (main) where

import Criterion.Main
import qualified Brainhuck.Interpreter1 as I1
import qualified Brainhuck.Interpreter2 as I2
import Data.Time.Clock
import Data.Time.Format
import Data.Text as T

type FileName = String
type Input = String
type MemSize = Int

{-  maybe add git hash of the latest commit?

import System.Process
main = do
  result <- readProcess "git" ["rev-parse", "HEAD"] []
  putStrLn result

-}

genBenchmark :: (FileName, MemSize, Input) -> IO Benchmark
genBenchmark (fileName, memSize, input) = do
  strFromFile <- readFile $ "bf/" ++ fileName
  let strFromFile' = T.pack strFromFile
  let firstInterpreter = bench "Boxed Vector" $ nfAppIO (I1.tryToInterpret strFromFile) (I1.initializeProgramStateDebug memSize input)
  let seconInterpreter = bench "Unboxed Vector" $ nfAppIO (I2.tryToInterpret strFromFile') (I2.initializeProgramStateDebug memSize input)
  pure $ bgroup ("INTERPRET: " ++ fileName) [firstInterpreter, seconInterpreter]
genListOfBenchmarks :: [(FileName, MemSize, Input)] -> IO [Benchmark]
genListOfBenchmarks = mapM genBenchmark


main :: IO ()
main = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  putStrLn ""
  benchmarks <- genListOfBenchmarks [ ("helloworld.b", 30, "")
                                    , ("196-commented.b", 500,"923743\n")
                                    , ("392quine.b", 400, " ")
                                    , ("quine.b", 400, "")
                                    , ("factor.b", 400, "222222\n")
                                    ]
  defaultMain [bgroup formattedTime benchmarks]
