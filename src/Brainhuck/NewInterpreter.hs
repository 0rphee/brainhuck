{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Brainhuck.NewInterpreter
  (
     tryToInterpret
  )
  where

import qualified Data.Vector as V
import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM)
import Brainhuck.Parsing (parseProgram)
import Brainhuck.Types
import Control.Monad (void)


-- =====================================================================
-- Interpret 

myFoldM :: (Monad m)
        => (st -> Instruction -> m st)
        -> st
        -> Program
        -> m st
myFoldM f initialState (Program instructions) =
-- foldlM :: (b -> a -> m b) -> b -> t a -> m b 
  foldlM f initialState instructions

instance BFState ProgramState where
  executeInstruction :: ProgramState -> Instruction -> IO ProgramState
  executeInstruction (MkState mem ptr) instruction =
    case instruction of
       IncPointer -> pure $ MkState mem (ptr+1)
       DecPointer -> pure $ MkState mem (ptr-1)
       IncCell    -> let modifiedMem = incCellValue mem ptr
                      in pure $ MkState modifiedMem ptr

       DecCell    -> let modifiedMem = decCellValue mem ptr
                      in pure $ MkState modifiedMem ptr

       GetChar    -> let modifiedMem = gChar '\0' mem ptr
                      in (`MkState` ptr) <$> modifiedMem

       PutChar    -> pChar mem ptr
                  >> pure (MkState mem ptr )

       Loop prog  -> loop prog (MkState mem ptr)

    where loop :: Program -> ProgramState -> IO ProgramState
          loop loopProg pST@(MkState loopMem loopPtr') =
            if currentCellIsZero loopMem loopPtr'
            then pure pST -- exits loop
            else interpret' pST loopProg >>= loop loopProg

          gChar :: Char -> Memory -> Pointer -> IO Memory
          gChar _ gmem gptr = do
            charVal <- fromIntegral . fromEnum <$> getChar
            putStrLn ""
            let modifiedMem = gmem V.// [(gptr, charVal)]
            pure modifiedMem

          pChar :: Memory -> Pointer -> IO ()
          pChar pmem pptr = do
            let cellVal = getCurrCellValue  pmem pptr
            putChar $ (toEnum . fromIntegral) cellVal -- converts to char the cellValue (int)

  initializeState :: Int -- ^ memory size
                  -> [Char]
                  -> ProgramState
  initializeState memSize _ = MkState cells 0
    where cells = V.replicate memSize 0

instance BFState BenchState where
  executeInstruction (BenchState input mem ptr) instruction =
    case instruction of
       IncPointer -> pure $ BenchState input mem (ptr+1)
       DecPointer -> pure $ BenchState input mem (ptr-1)
       IncCell    -> let modifiedMem = incCellValue mem ptr
                      in pure $ BenchState input modifiedMem ptr

       DecCell    -> let modifiedMem = decCellValue mem ptr
                      in pure $ BenchState input modifiedMem ptr

       GetChar    -> case input of
                      (x:xs) -> let modifiedMem = gChar x mem ptr
                                 in (\m -> BenchState xs m ptr ) <$> modifiedMem
                      _ -> throw InexistentBenchmarkingInput

       PutChar    -> pChar mem ptr
                  >> pure (BenchState input mem ptr )

       Loop prog  -> loop prog (BenchState input mem ptr)

    where loop :: Program -> BenchState -> IO BenchState
          loop loopProg pST@(BenchState _ loopMem loopPtr') =
            if currentCellIsZero loopMem loopPtr'
            then pure pST -- exits loop
            else interpret' pST loopProg >>= loop loopProg
          gChar :: Char -> Memory -> Pointer -> IO Memory
          gChar char gmem gptr = pure $ gmem V.// [(gptr, charVal)]
            where charVal = (fromIntegral . fromEnum) char

          pChar :: Memory -> Pointer -> IO ()
          pChar _ _ = pure ()


  initializeState :: Int -> [Char] -> BenchState
  initializeState memSize input = BenchState input cells 0
    where cells = V.replicate memSize 0


interpret' :: BFState st => st -> Program -> IO st
interpret' = myFoldM executeInstruction


tryToInterpret :: String -> Int -> [Char] -> IO ()
tryToInterpret strProgram memSize preEnteredInput = case preEnteredInput of
  [] -> run doNotBench
  _  -> run doBench
  where
        doBench = void . interpret' (initialState :: BenchState)
        doNotBench = void. interpret' (initialState :: ProgramState)
        run rightFunc = either print rightFunc (parseProgram strProgram)
        initialState :: BFState state => state
        initialState = initializeState memSize preEnteredInput


-- =====================================================================
-- Execution of Brainfuck operations 

currentCellIsZero :: Memory -> Pointer -> Bool
currentCellIsZero mem ptr = cellValue == 0
  where cellValue = getCurrCellValue mem ptr

modifyCellValue :: (MemoryCell -> MemoryCell -> MemoryCell)
                -> Memory -> Pointer -> Memory
modifyCellValue operation mem ptr = V.accum operation mem [(ptr, 1)]

incCellValue :: Memory -> Pointer -> Memory
incCellValue = modifyCellValue (+)

decCellValue :: Memory -> Pointer -> Memory
decCellValue = modifyCellValue (-)


-- | Helper Function
getCurrCellValue :: Memory -- ^ the Memory Vector
  -> Pointer -- ^ index currently pointed at 
  -> MemoryCell
getCurrCellValue mem ptr
  = fromMaybe (throw InexistentCellValueException) (mem V.!? ptr)

