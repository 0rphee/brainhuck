module Brainhuck.Parsing where

import Brainhuck.Types
import Data.Sequence as S

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

