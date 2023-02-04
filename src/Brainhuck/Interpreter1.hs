{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Brainhuck.Interpreter1 (tryToInterpret, tryToInterpret') where

import qualified Data.Vector.Unboxed.Mutable as VU
import qualified Data.Sequence as S
import qualified Data.Vector as V
import Control.Monad.Primitive
import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Foldable ( foldlM, toList )
import Control.Monad (void)
import Data.Word (Word8)

-- =====================================================================
-- Types

data BrainhuckException
  = InexistentCellValueException
  | InexistentBenchmarkingInput
  deriving (Show)

instance Exception BrainhuckException

data ParsingError = BracketsNotClosed
  deriving Show

type Pointer = Int
type MemoryCell = Word8

type Memory = V.Vector MemoryCell

type AltMem = VU.MVector (PrimState IO) MemoryCell

init' :: Int -> IO (VU.MVector (PrimState IO) Word)
init' memSize = VU.replicate memSize 0

data Instruction
  = IncPointer    --  >
  | DecPointer    --  <
  | IncCell       --  +
  | DecCell       --  -
  | GetChar       --  ,
  | PutChar       --  .
  | Loop Program  --  [


newtype Prog a = Program (S.Seq a)
  deriving (Foldable)

type Program = Prog Instruction

instance Show Program where
  show (Program seqq) = concatMap show $ toList seqq

instance Show Instruction where
  show IncPointer  = ">"
  show DecPointer  = "<"
  show IncCell     = "+"
  show DecCell     = "-"
  show GetChar     = ","
  show PutChar     = "."
  show (Loop prog) = " LOOP["  <> show prog <> "]"

class BFState state where
  executeInstruction :: state -> Instruction -> IO state
  initializeState :: Int -> String -> state

class BFMemory mem where
  gChar :: mem -> Pointer -> IO mem
  pChar :: mem -> Pointer -> IO ()
  gCharDebug :: Char -> mem -> Pointer -> IO mem
  pCharDebug :: mem -> Pointer -> IO ()
  getCurrCellValue :: mem -> Pointer -> MemoryCell

  currentCellIsZero :: mem -> Pointer -> Bool
  currentCellIsZero mem ptr = cellValue == 0
    where cellValue = getCurrCellValue mem ptr 

  modifyCellValue :: (MemoryCell -> MemoryCell -> MemoryCell)
                  -> mem -> Pointer -> mem

  incCellValue :: mem -> Pointer -> mem
  incCellValue = modifyCellValue (+)

  decCellValue :: mem -> Pointer -> mem
  decCellValue = modifyCellValue (+)

data BenchState = BenchState String Memory Pointer

data ProgramState
  = MkState Memory Pointer

class BFSTate' state where
  incPointer :: state -> IO state
  decPointer :: state -> IO state
  incCell    :: state -> IO state
  decCell    :: state -> IO state
  getCharST  :: Bool -> state -> IO state
  putCharST  :: Bool -> state -> IO state
  currentCellIsZeroST :: state -> Bool
  loopST :: Bool -> Program -> state -> IO state
  initializeState' :: Int -> String -> state 

-- =====================================================================
-- Interpret 
data ProgramState' = MkState' [Char] Memory Pointer

instance BFSTate' ProgramState' where
  incPointer, decPointer, incCell :: ProgramState' -> IO ProgramState'
  incPointer (MkState' input mem ptr) = pure $ MkState' input mem (ptr+1)
  decPointer (MkState' input mem ptr) = pure $ MkState' input mem (ptr-1)
  incCell    (MkState' input mem ptr) = let modifiedMem = incCellValue mem ptr
                                        in pure $ MkState' input modifiedMem ptr
  decCell    (MkState' input mem ptr) = let modifiedMem = decCellValue mem ptr
                                  in pure $ MkState' input modifiedMem ptr
  getCharST True (MkState' input mem ptr) = case input of
    (x:xs) -> let modifiedMem = gCharDebug x mem ptr
              in (\m -> MkState' xs m ptr) <$> modifiedMem
    _      -> throw InexistentBenchmarkingInput
  getCharST False (MkState' input mem ptr) = do
    modifiedMem <- gChar mem ptr
    pure $ MkState' input modifiedMem ptr

  putCharST debugOn st@(MkState' _ mem ptr) = if debugOn 
                                              then pCharDebug mem ptr >> pure st 
                                              else pChar mem ptr >> pure st
  currentCellIsZeroST (MkState' _ mem ptr) = currentCellIsZero mem ptr
  loopST debugOn program pST =
    if currentCellIsZeroST pST
    then pure pST -- exits loop
    else interpret debugOn pST program >>= loopST debugOn program

  initializeState' memSize input = MkState' input cells 0
    where cells = V.replicate memSize 0

executeInstruction' :: BFSTate' st => Bool -> st -> Instruction -> IO st
executeInstruction' debugOn state instruction =
  case instruction of
     IncPointer -> incPointer state
     DecPointer -> decPointer state
     IncCell    -> incCell state
     DecCell    -> decCell state
     GetChar    -> getCharST debugOn state
     PutChar    -> putCharST debugOn state
     Loop prog  -> loopST debugOn prog state

interpret :: BFSTate' st => Bool -> st -> Program -> IO st
interpret debugOn = foldlM (executeInstruction' debugOn)

tryToInterpret' :: String -> Int -> [Char] -> IO ()
tryToInterpret' strProgram memSize preEnteredInput = case preEnteredInput of
  [] -> run doNotBench
  _  -> run doBench
  where doBench = void . interpret True (initialState :: ProgramState')
        doNotBench = void. interpret False (initialState :: ProgramState')
        run rightFunc = either print rightFunc (parseProgram strProgram)
        initialState :: BFSTate' state => state
        initialState = initializeState' memSize preEnteredInput

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
            let cellVal = getCurrCellValue'  pmem pptr
            putChar $ (toEnum . fromIntegral) cellVal -- converts to char the cellValue (int)

  initializeState :: Int -- ^ memory size
                  -> [Char]
                  -> ProgramState
  initializeState memSize _ = MkState cells 0
    where cells = V.replicate memSize 0

instance BFState BenchState where
  executeInstruction :: BenchState -> Instruction -> IO BenchState
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
interpret' = foldlM executeInstruction


tryToInterpret :: String -> Int -> [Char] -> IO ()
tryToInterpret strProgram memSize preEnteredInput = case preEnteredInput of
  [] -> run doNotBench
  _  -> run doBench
  where doBench = void . interpret' (initialState :: BenchState)
        doNotBench = void. interpret' (initialState :: ProgramState)
        run rightFunc = either print rightFunc (parseProgram strProgram)
        initialState :: BFState state => state
        initialState = initializeState memSize preEnteredInput


-- =====================================================================
-- Execution of Brainfuck operations 

instance BFMemory Memory where
  gChar :: Memory -> Pointer -> IO Memory
  gChar mem ptr = do
    charVal <- fromIntegral . fromEnum <$> getChar
    putStrLn ""
    let modifiedMem = mem V.// [(ptr, charVal)]
    pure modifiedMem

  pChar :: Memory -> Pointer -> IO ()
  pChar mem ptr = putChar $ (toEnum . fromIntegral) cellVal
    where cellVal = getCurrCellValue mem ptr

  pCharDebug :: Memory -> Pointer -> IO ()
  pCharDebug _ _ = pure ()

  gCharDebug :: Char -> Memory -> Pointer -> IO Memory
  gCharDebug char mem ptr = pure modifiedMem
    where charVal = (fromIntegral . fromEnum) char
          modifiedMem = mem V.// [(ptr, charVal)]                            
  
  currentCellIsZero :: Memory -> Pointer -> Bool
  currentCellIsZero mem ptr = cellValue == 0
    where cellValue = getCurrCellValue' mem ptr

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


-- | Helper Function
getCurrCellValue' :: Memory -- ^ the Memory Vector
  -> Pointer -- ^ index currently pointed at 
  -> MemoryCell
getCurrCellValue' mem ptr
  = fromMaybe (throw InexistentCellValueException) (mem V.!? ptr)

-- =====================================================================
-- Parsing
parseProgram :: String -> Either ParsingError Program
parseProgram strProgram = Program . snd <$> go False strProgram S.empty
  where go :: Bool -> String -> S.Seq Instruction -> Either ParsingError (String, S.Seq Instruction)
        go  loopOpen [] instructions =  if loopOpen  -- if the loop is open, the only valid condition to exit it, is with ']'
                                        then Left BracketsNotClosed
                                        else Right ("", instructions)
        go  loopOpen (x:xs) instructions
          = let addCommonInstruction inst = go  loopOpen xs $ instructions S.|> inst
             in case x of
                 '>' -> addCommonInstruction IncPointer
                 '<' -> addCommonInstruction DecPointer
                 '+' -> addCommonInstruction IncCell
                 '-' -> addCommonInstruction DecCell
                 ',' -> addCommonInstruction GetChar
                 '.' -> addCommonInstruction PutChar
                 '[' -> case go True xs S.empty of
                          Right (accum, instructs) ->
                            go  loopOpen accum $ instructions S.|> Loop (Program instructs)
                          left -> left

                 ']' -> if loopOpen  -- if the loop is open, the only valid condition to exit it, is with ']'
                        then Right (xs, instructions)
                        else Left BracketsNotClosed
                 _   -> go  loopOpen xs instructions


