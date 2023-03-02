module Main (main) where

import Criterion.Main
import qualified Brainhuck.Interpreter1 as I1
import qualified Brainhuck.Interpreter2 as I2
import Brainhuck.Types
import Control.Monad.Trans.Except
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Text.IO as TIO

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
  strFromFile <- TIO.readFile $ "bf/" ++ fileName
  let firstInterpreter = bench "Boxed Vector" $ nfAppIO (I1.runBF strFromFile) (I1.initializeProgramStateDebug memSize input)
  let seconInterpreter = bench "Unboxed Vector" $ nfAppIO (I2.runBF strFromFile) (I2.initializeProgramStateDebug memSize input)
  let seconInterpreter' = bench "Unboxed Vector optimized" $ nfAppIO (I2.runBF' strFromFile) (I2.initializeProgramStateDebug memSize input)
  pure $ bgroup ("INTERPRET: " ++ fileName) [firstInterpreter, seconInterpreter, seconInterpreter']

genListOfBenchmarks :: [(FileName, MemSize, Input)] -> IO [Benchmark]
genListOfBenchmarks = mapM genBenchmark

benchAction :: Instruction a -> Benchmark
benchAction instruction =
  let {-first = bench "Boxed Vector" $ nfAppIO (exitToIO . runExceptT . executeInstruction (I1.initializeProgramStateDebug 2 "a")) instruction-}
      second = bench "Unboxed Vector" $ nfAppIO (exitToIO . runExceptT . executeInstruction (I2.initializeProgramStateDebug 2 "a")) instruction
  in bgroup ("INTREPRET: " ++ show instruction) [{-first,-} second]

actionBenchmarks :: [Benchmark]
actionBenchmarks
  = fmap benchAction [ModifyPointer 1, ModifyPointer (negate 1), ModifyCell (negate 1), ModifyCell 1, GetChar, PutChar]


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
  defaultMain [
      bgroup formattedTime benchmarks
    , bgroup formattedTime actionBenchmarks
    ]
