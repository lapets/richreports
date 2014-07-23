----------------------------------------------------------------
--
-- | RichReports
--
-- @Text\/RichReports.hs@
--
--   A library that supports the manual and automated assembly of
--   modules for building interactive HTML reports consisting of
--   abstract syntax trees as concrete syntax annotated with the
--   results of static analysis and abstract interpretation
--   algorithms.
--
--   Web:     richreports.org
--   Version: 0.0.3.0
--
--

----------------------------------------------------------------
--

module Text.RichReports
  where

import Data.List (intersperse)
import Data.String.Utils (join, replace)

import qualified Text.Ascetic.HTML as H

----------------------------------------------------------------
-- | Rich report data structure definitions.

type Message = Report

data Highlight =
    HighlightUnbound
  | HighlightUnreachable
  | HighlightDuplicate
  | HighlightError
  | Highlight [H.Class]
  deriving (Eq, Show)

data Entity = 
    Lt
  | Gt
  | Space
  | Ampersand
  deriving (Eq, Show)

data Report =
    Entity Entity
  | Text String
  | Symbol String
  | Punctuation String
  | Keyword String
  | Literal String
  | Konstant String -- "Constant" is a reserved word in some cases.
  | Operator String
  | Builtin String
  | Library String
  | Variable String
  | Error String

  | Atom [Highlight] [Message] [Report]
  | Span [Highlight] [Message] [Report]
  | Line [Report]
  | Block [Highlight] [Message] [Report]

  | Concat [Report]
  | Intersperse Report [Report]
  | Field [Report]
  | Row [Report]
  | Table [Report]
  
  | Page Report
  deriving (Show, Eq)

----------------------------------------------------------------
-- | Rich report class declaration (typically, abstract syntax
--   data structures would be members).

class ToReport a where
  report :: a -> Report 

----------------------------------------------------------------
-- | Rich report highlight and message class declaration
--   (typically, static analysis results data structures would
--   be members).
  
class ToHighlights a where
  highlights :: a -> [Highlight]

class ToMessages a where
  messages :: a -> [Message]

----------------------------------------------------------------
-- | Default class memberships for polymorphic types.

instance ToReport a => ToReport [a] where
  report xs = Concat $ map report xs
  
instance ToReport a => ToReport (Maybe a) where
  report x = Concat $ maybe [] (\r -> [report r]) x

----------------------------------------------------------------
-- | Generation of an interactive HTML version of the report.

highlightsStr :: [Highlight] -> [H.Class]
highlightsStr hs = concat [highlightStr h | h <- hs]

highlightStr :: Highlight -> [H.Class]
highlightStr h = case h of
  HighlightUnbound     -> ["RichReports_Highlight_Unbound"]
  HighlightUnreachable -> ["RichReports_Highlight_Unreachable"]
  HighlightDuplicate   -> ["RichReports_Highlight_Duplicate"]
  HighlightError       -> ["RichReports_Highlight_Error"]
  Highlight hs         -> hs

entityStr :: Entity -> String
entityStr e = case e of
    Lt        -> "&lt;"
    Gt        -> "&gt;"
    Space     -> "&nbsp;"
    Ampersand ->"&amp;"
  
messagesToAttr :: [Message] -> (H.Property, H.Value)
messagesToAttr ms =
  let conv m = 
        replace "\"" "&quot;" $
        replace "'" "\\'" $
        replace "\n" "" $
        replace "\r" "" $
          show $ H.html m
  in ("onclick", "msg(this, [" ++ (join "," ["'" ++ conv m ++ "'" | m <- ms]) ++ "]);")

instance H.ToHTML Report where
  html r = case r of
    Entity e -> H.span_ [("class", "RichReports_Entity")] [H.content (entityStr e)]
    Text s   -> H.span_ [("class", "RichReports_Text")] [H.content s]
    Symbol s   -> H.span_ [("class", "RichReports_Symbol")] [H.content s]
    Punctuation s   -> H.span_ [("class", "RichReports_Punctuation")] [H.content s]
    Keyword s   -> H.span_ [("class", "RichReports_Keyword")] [H.content s]
    Literal s   -> H.span_ [("class", "RichReports_Literal")] [H.content s]
    Konstant s   -> H.span_ [("class", "RichReports_Konstant")] [H.content s]
    Operator s   -> H.span_ [("class", "RichReports_Operator")] [H.content s]
    Builtin s   -> H.span_ [("class", "RichReports_Builtin")] [H.content s]
    Library s   -> H.span_ [("class", "RichReports_Library")] [H.content s]
    Variable s   -> H.span_ [("class", "RichReports_Variable")] [H.content s]
    Error s   -> H.span_ [("class", "RichReports_Error")] [H.content s]

    Atom hs ms rs ->
      if length ms == 0 then
        H.span_ [("class", join " " (highlightsStr hs))] [H.html r | r <- rs]
      else
        H.span [
          H.span_
            ([("class", join " " ["RichReports_Clickable"])] ++ [messagesToAttr ms])
            [
              H.span_
                [("class", join " " ((if length hs > 0 || length ms > 0 then ["RichReports_Highlight"] else []) ++ highlightsStr hs))]
                [H.html r | r <- rs]
            ]
        ]  
    Span hs ms rs ->
      if length ms == 0 then
        H.span_ ([("class", join " " (highlightsStr hs))]) [H.html r | r <- rs]
      else
        H.conc
          [ H.span_ ([("class", join " " ["RichReports_Clickable", "RichReports_Clickable_Exclamation"])] ++ [messagesToAttr ms]) [H.content "!"]
          , H.span_ ([("class", join " "(highlightsStr hs))]) [H.html r | r <- rs]
          ]
    Line rs -> H.div [H.html r | r <- rs]
    Block _ _ rs -> H.div_ [("class", "RichReports_Block")] [H.html r | r <- rs]

    Concat rs -> H.conc [H.html r | r <- rs]
    Intersperse r rs -> H.conc $ intersperse (H.html r) [H.html r | r <- rs]
    Field rs -> H.td (H.conc [H.html r | r <- rs])
    Row rs -> H.tr [H.html r | r <- rs]
    Table rs -> H.table [H.html r | r <- rs]

    Page r -> 
      H.file 
        (H.head [
          H.meta_ [("http-equiv","Content-type"),("content","text/html;charset=UTF-8")],
          H.style (
            H.CSS [
              ( ["body"], 
                Nothing,
                [ ("font-family", "Courier,Monospace"),
                  ("font-size", "12px")
                ]
              ),
              ( ["table"], 
                Nothing,
                [ ("font-family", "Courier,Monospace"),
                  ("font-size", "12px")
                ]
              ),
              ( ["#RichReports_Message"], 
                Nothing, 
                [ ("background-color","yellow"),
                  ("padding","3px"),
                  ("border","1px solid black"),
                  ("font-family", "Courier,Monospace"),
                  ("font-size", "12px"),
                  ("cursor","pointer")
                ]
              ),
              ( [".RichReports_Clickable"], Nothing, [("cursor","pointer")] ),
              ( [".RichReports_Clickable_Exclamation"], 
                Nothing, 
                [ ("background-color","yellow"), 
                  ("border","1px solid black"), 
                  ("margin","0px 5px 1px 5px"),
                  ("padding","0px 2px 0px 2px"),
                  ("font-size","9px")
                ]
              ),
              ( [".RichReports_Clickable"],
                Just "hover",
                [ ("background-color","yellow")
                ] 
              ),
              ( [".RichReports_Entity"], Nothing, [] ),
              ( [".RichReports_Text"], Nothing, [] ),
              ( [".RichReports_Symbol"], Nothing, [("font-weight","bold"), ("color","black")] ),
              ( [".RichReports_Punctuation"], Nothing, [("font-weight","bold"), ("color","black")] ),
              ( [".RichReports_Keyword"], Nothing, [("font-weight","bold"), ("color","blue")] ),
              ( [".RichReports_Literal"], Nothing, [("font-weight","bold"), ("color","firebrick")] ),
              ( [".RichReports_Konstant"], Nothing, [("font-weight","bold"), ("color","blue")] ),
              ( [".RichReports_Operator"], Nothing, [("font-weight","bold"), ("color","blue")] ),
              ( [".RichReports_Builtin"], Nothing, [("font-weight","bold"), ("color","purple")] ),
              ( [".RichReports_Library"], Nothing, [("font-weight","bold"), ("color","purple")] ),
              ( [".RichReports_Variable"], Nothing, [("font-weight","bold"), ("color","green")] ),
              ( [".RichReports_Error"],
                Nothing,
                [ ("font-weight","bold"),
                  ("color","red"),
                  ("text-decoration","underline")
                ]
              ),
              ( [".RichReports_Highlight"], Nothing, [("margin","2px")] ),
              ( [".RichReports_Highlight_Unbound"], Nothing, [("background-color","orange")] ),
              ( [".RichReports_Highlight_Unreachable"], Nothing, [("background-color","orange")] ),
              ( [".RichReports_Highlight_Duplicate"], Nothing, [("background-color","yellow")] ),
              ( [".RichReports_Highlight_Error"], Nothing, [("background-color","lightpink")] ),
              ( [".RichReports_Block"], Nothing, [("margin-left","10px")] )
            ]
          ),
          H.script_ [("type","text/javascript"), ("src","http://ajax.googleapis.com/ajax/libs/jquery/1.4.3/jquery.min.js")] "",
          H.script $
            "function msg (obj, msgs) {"
              ++ "var html = '';"
              ++ "for (var i = 0; i < msgs.length; i++) html += '<div class=\"RichReports_MessagePortion\">' + msgs[i] + '</div>';"
              ++ "document.getElementById('RichReports_Message').innerHTML = html;"
              ++ "document.getElementById('RichReports_Message').style.display = 'inline-block';"
              ++ "var top = $(obj).offset().top;"
              ++ "var left = $(obj).offset().left;"
              ++ "$('#RichReports_Message').offset({top:top + 15, left:left + 15});"
              ++ "}"
        ])
        (H.body [
          H.html r,
          H.div_ [("id","RichReports_Message"), ("style","display:none;"), ("onclick", "this.style.display='none';")] [H.content ""]
        ])

--eof
