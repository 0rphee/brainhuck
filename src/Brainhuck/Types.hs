{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Brainhuck.Types where

import qualified Data.Vector.Unboxed.Mutable as VU
import qualified Data.Sequence as S
import Control.Exception
import Control.Monad.Primitive
import Data.Word (Word8)


data BrainhuckException
  = InexistentCellValueException
  | InexistentBenchmarkingInput
  deriving (Show)

instance Exception BrainhuckException

type MemoryCell = Word8

data family Memor 

class BFMemory mem cell where
  gChar :: mem -> ptr -> IO mem
  pChar :: mem -> ptr -> IO ()
  gCharDebug :: Char -> mem -> ptr -> IO mem
  pCharDebug :: mem -> ptr -> IO ()
  pCharDebug _ _ = pure ()
  getCurrCellValue :: mem -> ptr -> cell

  currentCellIsZero :: mem -> ptr -> Bool
  currentCellIsZero mem ptr = cellValue == 0
    where cellValue = getCurrCellValue mem ptr

  modifyCellValue :: (MemoryCell -> MemoryCell -> MemoryCell)
                  -> mem -> ptr-> mem

  incCellValue :: mem -> ptr -> mem
  incCellValue = modifyCellValue (+)

  decCellValue :: mem -> ptr -> mem
  decCellValue = modifyCellValue (+)

class BFSTate' state where
  incPointer :: state -> IO state
  decPointer :: state -> IO state
  incCell    :: state -> IO state
  decCell    :: state -> IO state
  getCharST  :: Bool -> state -> IO state
  putCharST  :: Bool -> state -> IO state
  currentCellIsZeroST :: state -> Bool
  loopST :: Bool -> program instruction -> state -> IO state
  initializeState' :: Int -> String -> state
