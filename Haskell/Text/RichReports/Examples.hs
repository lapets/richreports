---------------------------------------------------------------------
-- 
-- | RichReports
--
-- @Text\/RichReports\/Examples.hs@
--
--   Examples that illustrate how the RichReports module can be
--   used.
--
--   All required modules must be installed in order for this example
--   module to work correctly.
--

---------------------------------------------------------------------
-- 

module Data.RichReports.Examples
  where

import qualified System.IO as IO (IO, putStr, writeFile)
import qualified Text.Ascetic.HTML as H

import qualified Text.RichReports as R

---------------------------------------------------------------------
-- | Definition of an abstract syntax.

data Program =
    Program [Stmt]
  deriving (Eq, Show)

data Stmt =
    Repeat [Stmt]
  | Prints Exp
  | Errors
  | Pass
  deriving (Eq, Show)
  
data Exp =
    OpPlus Exp Exp
  | OpMax Exp Exp
  | OpAbs Exp
  | Num Integer
  deriving (Eq, Show)

---------------------------------------------------------------------
-- | Example conversion algorithm that turns an abstract syntax tree
--   into a report.
  
instance R.ToReport Program where
  report (Program ss) = R.Page $ R.Block [] [] [R.report s | s <- ss]
  
instance R.ToReport Stmt where
  report s = case s of
    Repeat ss -> R.Concat [R.Line [R.Keyword "repeat", R.Block [] [] [R.report s | s <- ss]]]
    Prints e  -> R.Line [R.Keyword "print", R.Entity R.Space, R.report e]
    Errors    ->
      R.Line [
        R.Span [R.HighlightError] [R.Text "This is a message that applies to the whole highlighted span"] [R.Keyword "error"],
        R.Entity R.Space,
        R.Text "outside",
        R.Entity R.Space,
        R.Text "span"
      ]
    Pass      -> R.Line [R.Keyword "pass"]

instance R.ToReport Exp where
  report e = case e of
    OpPlus e1 e2 -> R.Concat [R.report e1, R.Keyword "+", R.report e2]
    OpMax e1 e2  -> R.Concat [R.Builtin "max", R.Punctuation "(", R.report e1, R.Punctuation ",", R.report e2, R.Punctuation ")"]
    OpAbs e      -> R.Concat [R.Builtin "abs", R.Punctuation "(", R.report e, R.Punctuation ")"]
    Num i        -> R.Atom [] [R.Text "int"] [R.Konstant (show i)]

---------------------------------------------------------------------
-- | Example of an abstraxt syntax tree, and rendering thereof as an
--   interactive HTML report.

p = Program [
      Repeat [
        Prints (OpPlus (Num 2) (Num 3)),
        Pass,
        Errors,
        Repeat [Prints (Num 7), Pass]
      ], 
      Prints (OpMax (OpAbs (Num 4)) (Num 5)),
      Pass
    ]
    
main :: IO.IO ()
main =
  do { IO.putStr $ "The program:\n" ++ (show p)
     ; IO.putStr $ "\n\nThe report:\n" ++ (show (R.report p))
     ; IO.writeFile "report.html" (show (H.html (R.report p)))
     ; IO.putStr $ "\n\nThe file \"report.html\" has been written.\n\n"
     }

--eof