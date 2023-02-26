module Main (main) where

import Criterion.Main
import Brainhuck.Interpreter1
import Data.Time.Clock
import Data.Time.Format

type FileName = String
type Input = String
type MemSize = Int

genBenchmark :: (FileName, MemSize, Input) -> IO Benchmark
genBenchmark (fileName, memSize, input) = do
  strFromFile <- readFile $ "bf/" ++ fileName
  pure $ bench ("INTERPRET: " ++ fileName) $ nfAppIO (tryToInterpret strFromFile) (initializeProgramStateDebug memSize input)
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
  defaultMain $ [bgroup formattedTime benchmarks]
