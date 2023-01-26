module Main (main) where

import Criterion.Main
import Brainhuck.NewInterpreter

type FileName = String
type Input = String
type MemSize = Int

genBenchmark :: (FileName, MemSize, Input) -> IO Benchmark
genBenchmark (fileName, memSize, input) = do
  strFromFile <- readFile $ "bf/" ++ fileName
  pure $ bench ("INTERPRET: " ++ fileName) (whnfIO (tryToInterpret strFromFile memSize input))

genListOfBenchmarks :: [(FileName, MemSize, Input)] -> IO [Benchmark]
genListOfBenchmarks = mapM genBenchmark


main :: IO ()
main = do
  putStrLn ""
  benchmarks <- genListOfBenchmarks [ ("helloworld.b", 30, " ")
                                    , ("196-commented.b", 500,"923743\n")
                                    , ("392quine.b", 400, " ")
                                    , ("quine.b", 400, " ")
                                    , ("factor.b", 400, "222222\n")
                                    ]
  defaultMain benchmarks





