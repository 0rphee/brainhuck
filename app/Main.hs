module Main (main) where

-- import Brainhuck.Interpreter
import Brainhuck.Interpreter1
-- import Brainhuck.Parsing
import Brainhuck.Options
import Options.Applicative (execParser)

main :: IO ()
main = do
  (Options {-optsDebug-} optsSize optsInput {-optsOldInt-}) <- execParser options
  programString <- case optsInput of
                     FileInput filePath -> readFile filePath
                     StdInput stdinStr -> pure stdinStr
  tryToInterpret programString optsSize []
  
  
  -- if optsOldInt
  -- then do let sanitziedProgram = filter (`elem` "<>[]+-,.") programString
  --         interpretBF optsDebug optsSize sanitziedProgram
  -- else do
  --   let inter :: Program -> IO ()
  --       inter prog = do if optsDebug
  --                       then print prog
  --                       else pure ()
  --                       interpret prog (initState optsSize) 
  --                       pure ()
  --       parsedProg = parseProgram programString
  --   either print inter parsedProg
