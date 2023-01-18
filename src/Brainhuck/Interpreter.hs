{-# LANGUAGE TupleSections #-}
module Brainhuck.Interpreter
  (
    interpretBF
  )
  where

import qualified Data.Vector as V
import Data.Word ( Word8 )
import Data.Maybe ( fromMaybe )
import Control.Exception ( Exception, throw, throwIO )
import Control.Monad ( void, when )
import Control.Exception.Base (finally)

-- =====================================================================
-- TYPES 

-- | Exceptions used when the program is interpreted
data BrainhuckException
  = InexistentCellValueException
  | NoMatchingBracketException
  deriving (Show)

instance Exception BrainhuckException

type Pointer = Int
type MemoryCell = Word8

data ProgramState
  = MkState { program :: String              -- ^ The program to be executed
            , memory  :: V.Vector MemoryCell -- ^ The memory
            , pointer :: Pointer             -- ^ The pointer to the memory
            }
  deriving Show

-- =====================================================================
-- Brainfuck operations 

-- | Primitive function to modify the `Pointer` in `ProgramState`
pointerOperation :: (Pointer -> Pointer) -> ProgramState -> ProgramState
pointerOperation op (MkState str mem ptr) = MkState str mem (op ptr)

incPointer :: ProgramState -> ProgramState
incPointer = pointerOperation (+1)

decPointer :: ProgramState -> ProgramState
decPointer = pointerOperation (\x -> x - 1)

-- Helper Function
getCurrCellValueDebug :: Show a => Bool -- ^ debug flag
  -> V.Vector a -- ^ the Memory Vector
  -> Pointer -- ^ index currently pointed at 
  -> IO a
getCurrCellValueDebug debug mem ptr = do -- fromMaybe (throw InexistentCellValueException) (mem V.!? ptr)
  let cellVal = mem V.!? ptr
  case cellVal of
    Nothing -> if debug
               then putStrLn ("\n" <> show mem <> " ptr: " <> show ptr) >> throw InexistentCellValueException
               else throw InexistentCellValueException
    Just a -> pure a

getCurrCellValue :: V.Vector a -- ^ the Memory Vector
  -> Pointer -- ^ index currently pointed at 
  -> a
getCurrCellValue mem ptr = fromMaybe (throw InexistentCellValueException) (mem V.!? ptr)


-- | Primitive function to modify the `MemoryCell` currently pointed at in `ProgramState`
cellOperation :: (MemoryCell -> MemoryCell) -> ProgramState -> ProgramState
cellOperation op (MkState str mem ptr) = MkState str (mem V.// [(ptr, op cellValue)]) ptr
  where cellValue = getCurrCellValue mem ptr

-- | Increment Cell value in current pointer value
incCell :: ProgramState -> ProgramState
incCell = cellOperation (+1)

-- | Decrement Cell value in current pointer value
decCell :: ProgramState -> ProgramState
decCell = cellOperation (\x -> x - 1)

-- | Value in current pointer location is printed
pChar :: ProgramState -> IO ProgramState
pChar (MkState str mem ptr) = do
  let cellValue = getCurrCellValue mem ptr
  putChar $ (toEnum . fromIntegral) cellValue -- converts to char the cellValue (int)
  pure $ MkState str mem ptr

-- | Input from the user is saved in pointer location
gChar :: ProgramState -> IO ProgramState
gChar (MkState str mem ptr) = do
  readCharValue <- fromIntegral . fromEnum <$> getChar
  putChar '\n'
  let modifiedMem = mem V.// [(ptr, readCharValue)]
  pure $ MkState str modifiedMem ptr

-- | Validates if currently pointed at location is 0
currentCellIsZero :: ProgramState -> Bool
currentCellIsZero (MkState _ mem ptr) = cellValue == 0
  where cellValue = getCurrCellValue mem ptr

-- | Used in loopDebug
myPrint :: Show b => String -> b -> IO b
myPrint string other = do
  putStrLn (string <> show other)
  pure other

-- | Recursive function used when entering loops. Prints command String
--   yet to be evaluated in each function call, as well the depth of the 
--   loop (for nested loops). When there are no more nested loops to 
--   evaluate, goes back to `execute`.
loopDebug :: Int -- ^ loop-depth, only used for debugging
  -> String -- ^ accumulated commands in loop
  -> (String, ProgramState) -- ^ accumulated commands from inner loops, 
                            --   along with the yet to be evaluated commands
                            --   `ProgramState`.
  -> IO (String, ProgramState)
loopDebug depth carry (internalCarry, pState) = do
  let carry' = internalCarry <> carry
  putStr $ "L " <> show depth <> ":\t" <> (unwords . words . program) pState <> " | carry:\t" <> carry <> "\t" <> show (pointer pState)
  case pState of
    MkState [] _ _                 -> putChar '\n' >> throwIO NoMatchingBracketException
    cst@(MkState (']':xs) mem ptr) -> do
      putStr "\tinside (]:xs)"
      exitLoop <- myPrint "\tExitLoop? " (currentCellIsZero cst)
      if exitLoop
      then pure (']':carry', MkState xs mem ptr)
      else loopDebug depth "" (internalCarry, MkState (reverse carry <> "]" <> xs) mem ptr)
    MkState (x:xs) mem ptr   -> do
      putStrLn "\tinside (x:xs)"
      getProgramOperation depth True x (MkState xs mem ptr) >>= loopDebug depth (x:carry')

-- | Recursive function used when entering loops. When there are no more
-- | nested loops to evaluate, goes back to `execute`. 
loop :: String -- ^ accumulated commands in loop
  -> (String, ProgramState) -- ^ accumulated commands from inner loops,
                            --   along with the yet to be evaluated commands
                            --   in `ProgramState`.
  -> IO (String, ProgramState)
loop carry (internalCarry, pState ) = let carry' = internalCarry <> carry in
  case pState of                                                    -- myTrace "pState in loop: "
    MkState [] _ _                 -> putChar '\n' >> throwIO NoMatchingBracketException
    cst@(MkState (']':xs) mem ptr) ->                               -- trace "\tinside (]:xs)" $ 
      if currentCellIsZero cst                                      -- myTrace "\tTerminaLoop: " $
      then pure (']':carry', MkState xs mem ptr)
      else loop "" (internalCarry,  MkState (reverse carry <> "]" <> xs) mem ptr)
    MkState (x:xs) mem ptr   ->                                     -- trace "\tinside (x:xs)"
      getProgramOperation 0 False x (MkState xs mem ptr) >>= loop (x:carry')

-- | The ']' command is "embeded" in the loop function
getProgramOperation :: Int -- ^ loop-depth
  -> Bool -- ^ debugging flag
  -> Char -- ^ the char "<>[],." to be matched against
  -> (ProgramState -> IO (String, ProgramState))
getProgramOperation depth debug char = case char of
  '>' -> pure . ("",) . incPointer
  '<' -> pure . ("",) . decPointer
  '+' -> pure . ("",) . incCell
  '-' -> pure . ("",) . decCell
  ',' -> \x -> do
           pgst <- gChar x
           pure ("", pgst)
  '.' -> \pgst -> do
           ("",) <$> pChar pgst
  '[' -> if debug
         then loopDebug (depth+1) "" . ("", )
         else loop "" . ("",)
  _   -> pure . ("",)

-- | Entrypoint for the execution of the code. Here the 
--   top-level (outside of loops) commands are executed. 
execute :: Bool -- ^ debugging flag 
  -> (String, ProgramState) -- ^ String commands from loops, not used and the `PorgramState` to be modified 
  -> IO ProgramState
execute _     (_, MkState [] mem ptr) = pure $ MkState [] mem ptr
execute debug (_, MkState (x:xs) mem ptr) = do
  when debug $ putStrLn ("Exec:\t" <> xs )
  nextProgState >>= execute debug
  where nextProgState = getProgramOperation 0 debug x $ MkState xs mem ptr

-- | Returns an initial `ProgramState` with the provided memory
--   size of 1 byte, initialized to 0 (the standard is of 30,000 
--   memory blocks).
initialState :: Int -- ^ memory size
  -> String -- ^ the program to be executed
  -> ProgramState
initialState memSize progrString = MkState progrString cells 0
 where cells = V.replicate memSize 0


-- | Interprets a Brainfuck program, using a given a size for the memory.
interpretBF :: Bool -- ^ debugging flag 
  -> Int -- ^ memory size
  -> String -- ^ the program to be executed
  -> IO ()
interpretBF debug memSize programString =
  let resultState = execute debug ("", initialState memSize programString)
  in if debug
     then putStrLn "DEBUG MODE ON\n" >> resultState >>= print
     else void resultState


