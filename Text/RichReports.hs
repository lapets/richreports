----------------------------------------------------------------
--
-- Rich Reports
--
-- RichReports.hs
--   Definitions for the representation and construction of a
--   data structure corresponding to a structured representation
--   of the concrete syntax of a programming language, with
--   annotations corresponding to static analysis results.
--   Includes support for generation of ASCII text, as well as
--   formatted HTML with interactive messages.

----------------------------------------------------------------
--

module Text.RichReports
  where

import Data.List (intersperse)

import qualified Text.Ascetic.HTML as H

----------------------------------------------------------------
-- Data structures (also used for report construction process)
-- and class.

type HTMLHeaders = [(String, String)]
type CSSClass = String
type Message = String

data Report =
    Text String
  | Keyword String
  | Constant String
  | Variable String
  | Literal String
  | Space
  | Lt
  | Gt
  | Conc [Report]
  | Field Report
  | Row [Report]
  | Table [Report]
  | Span           [Message] [CSSClass] [Report]
  | Indent                              Report
  | Line                     [CSSClass] [Report]
  | LineIfFlat               [CSSClass] Report
  | Block          [Message] [CSSClass] [Report]
  | BlockIndent    [Message] [CSSClass] [Report]
  | Intersperse Report [Report]
  | Finalize Report
  deriving (Show, Eq)

class ToReport a where
  report :: a -> Report

----------------------------------------------------------------
-- Generation of an interactive HTML version of the report.

instance H.ToHTML Report where
  html r = case r of
    Text s -> H.content s
    Space -> H.content "&nbsp;"
    Conc rs -> H.conc [H.html r | r <- rs]
    Field r -> H.td (H.html r)
    Row rs -> H.tr [ H.html r | r <- rs ]
    Table rs -> H.table [ H.html r | r <- rs ]
    Line _ rs -> H.div [H.html r | r <- rs]
    Block _ _ rs -> H.div [H.html r | r <- rs]
    BlockIndent _ _ rs -> H.divWith [("class", "RichReports_BlockIndent")] [H.html r | r <- rs]
    Intersperse r rs -> H.conc $ intersperse (H.html r) [H.html r | r <- rs]
    Finalize r -> 
      H.file 
        (H.head [
          H.style (
            H.CSS [
              ( ["body"], 
                Nothing, 
                [ ("font-family", "Courier,Monospace"),
                  ("font-size", "12px")
                ]
              ),
              ([".RichReports_BlockIndent"], Nothing, [("margin-left", "10px")])
            ]
          ),
          H.script ""
        ])
        (H.body [H.html r])
    
    
    
    _ -> H.content ""











{-


reportHTML :: Report -> String
reportHTML r = showConcreteRec 0 (annotLeavesOnly r) where
  showConcreteRec i r = let sh = showConcreteRec i in case r of
    Text s               -> s
    Space                -> " "
    Lt                   -> "&lt;"
    Gt                   -> "&gt;"
    Span        ms cs rs -> elementWithMsgs "span" ms cs $ concat (map sh rs)
    Indent            r  -> showConcreteRec (i+2) r
    Line           cs rs -> element "div"   cs $ concat (map sh $ [Text (concat $ take i (repeat "&nbsp;"))] ++ rs)
    Block       ms cs rs -> elementWithMsgs "div" ms cs $ concat (map sh rs)
    BlockIndent ms cs rs -> elementWithMsgs "div" ms cs $ concat (map (showConcreteRec (i+2)) rs)
    Conc              rs -> concat $ map sh rs




----------------------------------------------------------------
-- Constraints that must be satisfied by a data structure that
-- can be converted into an HTML representation of a concrete
-- syntax.

class ToConcrete a where
  concrete :: a -> Concrete

instance ToConcrete Char where
  concrete c = Text $ show c

instance ToConcrete Int where
  concrete n = Text $ show n

instance ToConcrete Integer where
  concrete n = Text $ show n

instance ToConcrete a => ToConcrete [a] where
  concrete xs = Conc $ intersperse (Text ",") $ map concrete xs

instance ToConcrete (HaXml.Element i) where
  concrete e = Text (HaXml.render (HaXml.element e))

----------------------------------------------------------------
-- Convenient functions for inspecting and modifying an
-- instance.

isFlat :: Concrete -> Bool
isFlat r = case r of
  Text       _   -> True
  Space          -> True
  Lt             -> True
  Gt             -> True
  Indent       _ -> True
  Line       _ _ -> True
  LineIfFlat _ _ -> True
  Span     _ _ _ -> True
  Conc         _ -> True
  _              -> False

hasMsg :: Concrete -> Bool
hasMsg r = case r of
  Indent h             -> hasMsg h
  Conc hs              -> or (map hasMsg hs)
  Span ms cs hs        -> length ms > 0 || or (map hasMsg hs)
  Line cs hs           -> or (map hasMsg hs)
  LineIfFlat cs h      -> hasMsg h
  Block ms cs hs       -> length ms > 0 || or (map hasMsg hs)
  BlockIndent ms cs hs -> length ms > 0 || or (map hasMsg hs)
  Field cs h           -> hasMsg h
  Row cs hs            -> or (map hasMsg hs)
  Table cs hs          -> or (map hasMsg hs)
  _                    -> False

annotLeavesOnly :: Concrete -> Concrete
annotLeavesOnly h = 
  let mlo h = case h of
        Indent h             -> Indent $ mlo h
        Conc hs              -> Conc $ map mlo hs
        Line cs hs           -> Line cs $ map mlo hs
        LineIfFlat cs h      -> LineIfFlat cs $ mlo h
        Field cs h           -> Field cs $ mlo h
        Row cs hs            -> Row cs $ map mlo hs
        Table cs hs          -> Table cs $ map mlo hs
        Block ms cs hs       -> let hs'=map mlo hs; b=or(map hasMsg hs') in Block       (if b then [] else ms) cs hs'
        BlockIndent ms cs hs -> let hs'=map mlo hs; b=or(map hasMsg hs') in BlockIndent (if b then [] else ms) cs hs'
        Span ms cs hs        -> let hs'=map mlo hs; b=or(map hasMsg hs') in Span        (if b then [] else ms) cs hs'
        _                    -> h
  in mlo h

annotRootsOnly :: Concrete -> Concrete
annotRootsOnly h = 
  let mlo b h = case h of
        Indent h             -> Indent $ mlo b h
        Conc hs              -> Conc $ map (mlo b) hs
        Line cs hs           -> Line cs $ map (mlo b) hs
        LineIfFlat cs h      -> LineIfFlat cs $ mlo b h
        Field cs h           -> Field cs $ mlo b h
        Row cs hs            -> Row cs $ map (mlo b) hs
        Table cs hs          -> Table cs $ map (mlo b) hs
        Block ms cs hs       -> Block       (if b then ms else []) (if b then cs else []) $ map (if ms == [] then mlo b else mlo False) hs
        BlockIndent ms cs hs -> BlockIndent (if b then ms else []) (if b then cs else []) $ map (if ms == [] then mlo b else mlo False) hs
        Span ms cs hs        -> Span        (if b then ms else []) (if b then cs else []) $ map (if ms == [] then mlo b else mlo False) hs
        _                    -> h
  in mlo True h

----------------------------------------------------------------
-- Conversion of an instance to a string.

fixTextToFormat :: String -> String
fixTextToFormat s = case s of
  (' ':s)  -> "&nbsp;" ++ fixTextToFormat s
  ('\n':s) -> "<br/>" ++ fixTextToFormat s
  (c:s)    -> c : fixTextToFormat s
  "" -> ""

element :: String -> [CSSClass] -> String -> String
element tag [] s = "<" ++ tag ++ ">" ++ s ++ "</" ++ tag ++ ">"
element tag cs s = "<" ++ tag ++ " class=\"" ++ (join " " cs) ++ "\">" ++ s ++ "</" ++ tag ++ ">"

elementWithMsgs :: String -> [Message] -> [CSSClass] -> String -> String
elementWithMsgs tag [] [] s = "<" ++ tag ++ ">" ++ s ++ "</" ++ tag ++ ">"
elementWithMsgs tag [] cs s = "<" ++ tag ++ " class=\"" ++ (join " " cs) ++ "\">" ++ s ++ "</" ++ tag ++ ">"
elementWithMsgs tag ms cs s = 
     "<" ++ tag 
         ++ " onclick=\"msg(this, [" ++ (join ", " (["'"++m++"'"|m<-ms])) ++ "]);\""
         ++ " class=\"" ++ (join " " cs) ++ " " ++ tag ++ "_msg\">" 
  ++ s 
  ++ "</" ++ tag ++ ">"

tag :: String -> String -> String
tag t s = element t [] s

tagCDATA :: String -> String -> String
tagCDATA t s = element t [] $ "<![CDATA[" ++ s ++ "]]>"    


showASCII :: Concrete -> String
showASCII r = showASCIIRec 0 (annotLeavesOnly r) where
  showASCIIRec i r = let sh = showASCIIRec i in case r of
    ImgScrollZoom _ _    -> "[image]"
    Text s               -> s
    Space                -> " " 
    Lt                   -> "<"
    Gt                   -> ">"
    Indent            r  -> showASCIIRec (i+2) r
    Table          cs rs -> concat (map sh rs)
    Row            cs rs -> "\n" ++ concat (map sh rs)
    Field          cs r  -> sh r
    Block       ms cs rs -> concat (map sh rs)
    BlockIndent ms cs rs -> concat (map (showASCIIRec (i+2)) rs)
    Line           cs rs -> "\n" ++ concat (map sh $ [Text (concat $ take i (repeat " "))] ++ rs)
    LineIfFlat     cs r  -> if isFlat r then "\n" ++ concat (map sh $ [Text (concat $ take i (repeat " "))] ++ [r]) else sh r
    Span        ms cs rs -> concat $ map sh rs
    Conc              rs -> concat $ map sh rs

mkLineIfFlat clss c = case c of
  (LineIfFlat clss' c) -> (LineIfFlat (clss'++clss) c)
  c -> LineIfFlat clss c

mkLineIfFlats clss cs = [mkLineIfFlat [] c | c <- cs]

-}

--eof
