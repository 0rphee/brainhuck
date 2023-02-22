{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Brainhuck.Types where

import Data.Kind (Type)
import Control.Exception

data BrainhuckException
  = InexistentCellValueException
  | InexistentBenchmarkingInput
  deriving (Show)

instance Exception BrainhuckException

class (Num cell, Eq cell) => 
      BFMemory (mem :: Type -> Type) (cell :: Type) where

  gChar            :: mem cell -> ptr -> IO (mem cell)
  pChar            :: mem cell -> ptr -> IO ()
  gCharDebug       :: Char -> mem cell  -> ptr -> IO (mem cell)
  getCurrCellValue :: mem cell -> ptr -> cell
  modifyCellValue :: (cell -> cell -> cell)
                  -> mem cell -> ptr -> mem cell

  pCharDebug       :: mem cell-> ptr -> IO ()
  pCharDebug _ _   = pure ()

  currentCellIsZero :: mem cell -> ptr -> Bool
  currentCellIsZero mem ptr = cellValue == 0
    where cellValue = getCurrCellValue mem ptr

  incCellValue :: mem cell -> ptr -> mem cell
  incCellValue = modifyCellValue (+)

  decCellValue :: mem cell -> ptr -> mem cell
  decCellValue = modifyCellValue (+)

class BFSTate state where
  incPointer :: state -> IO state
  decPointer :: state -> IO state
  incCell    :: state -> IO state
  decCell    :: state -> IO state
  getCharST  :: Bool -> state -> IO state
  putCharST  :: Bool -> state -> IO state
  currentCellIsZeroST :: state -> Bool
  loopST :: Bool -> program instruction -> state -> IO state
  initializeState' :: Int -> String -> state
