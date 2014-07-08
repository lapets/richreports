<?php /**************************************************************
** 
** richreports.php
**
**   A library that supports the manual and automated assembly of
**   modules for building interactive HTML reports consisting of
**   abstract syntax trees as concrete syntax annotated with the
**   results of static analysis and abstract interpretation
**   algorithms.
**
**   Web:     richreports.org
**   Version: 0.0.3.0
**
*/

namespace richreports;

include_once("uxadt.php"); if (!defined('_')) define('_', null);

/********************************************************************
** Rich report data structure definitions.
*/

\uxadt\_('Highlight', array(
  'HighlightUnbound' => array(),
  'HighlightUnreachable' => array(),
  'HighlightDuplicate' => array(),
  'HighlightError' => array(),
  'Highlight' => array('$')
  ));

\uxadt\_('Entity', array(
  'Space' => array(),
  'Lt' => array(),
  'Gt' => array(),
  'Ampersand' => array()
  ));

\uxadt\_('Report', array(
  'Entity' => array('Entity'),
  'Text' => array('$'),
  'Symbol' => array('$'),
  'Punctuation' => array('$'),
  'Keyword' => array('$'),
  'Literal' => array('$'),
  'Konstant' => array('$'), // "Constant" is a reserved word in some cases.
  'Operator' => array('$'),
  'Builtin' => array('$'),
  'Library' => array('$'), 
  'Variable' => array('$'),
  'Error' => array('$'),

  'Atom' => array(array('Highlight'), array('Report'), array('Report')),
  'Span' => array(array('Highlight'), array('Report'), array('Report')),
  'Line' => array(array('Report')),
  'Block' => array(array('Highlight'), array('Report'), array('Report')),

  'Concat' => array(array('Report')),
  'Intersperse' => array('Report', array('Report')),
  'Field' => array(array('Report')),
  'Row' => array(array('Report')),
  'Table' => array(array('Report')),

  'Page' => array('Report')
  ));

/********************************************************************
** Interactive HTML report rendering functions.
*/

function highlightsStr($hs) {
  $a = array(); foreach ($hs as $h) $a = array_merge($a, highlightStr($h));
  return implode(' ', $a);
}

function highlightStr($h) {
  return $h
    ->_(HighlightUnbound(),     function ()    { return array("RichReports_Highlight_Unbound"); } )
    ->_(HighlightUnreachable(), function ()    { return array("RichReports_Highlight_Unreachable"); } )
    ->_(HighlightDuplicate(),   function ()    { return array("RichReports_Highlight_Duplicate"); } )
    ->_(HighlightError(),       function ()    { return array("RichReports_Highlight_Error"); } )
    ->_(Highlight(_),           function ($hs) { return $hs; })
    ->end;
};

function entityStr($e) {
  return $e
    ->_(Space(),     function () { return '&nbsp;'; })
    ->_(Lt(),        function () { return '&lt;'; })
    ->_(Gt(),        function () { return '&gt;'; })
    ->_(Ampersand(), function () { return '&amp;'; })
    ->end;
};

function messagesToAttr($ms) {
  $conv = function ($m) {
    $m = '\'' . html($m);
    $m = str_replace('"', '&quot;', $m);
    //$m = str_replace('\'', '\\\'', $m); // Not necessary?
    $m = str_replace('\n', '', $m);
    $m = str_replace('\r', '', $m);
    $m = $m . '\'';
    return $m;
  };
  return 'onclick="msg(this, ['. implode(',', array_map($conv, $ms)) .']);"';
};

function conc($rs) {
  $ss = array(); foreach ($rs as $r) array_push($ss, html($r));
  return implode('', $ss);
}

function html($r) {
  $html = function ($r) { return html($r); };
  return $r
    ->_(Entity(_), function ($e) { return '<span class="RichReports_Entity">' . entityStr($e) . '</span>'; })
    ->_(Text(_), function ($s) { return '<span class="RichReports_Text">' . $s . '</span>'; })
    ->_(Symbol(_), function ($s) { return '<span class="RichReports_Symbol">' . $s . '</span>'; })
    ->_(Punctuation(_), function ($s) { return '<span class="RichReports_Punctuation">' . $s . '</span>'; })
    ->_(Keyword(_), function ($s) { return '<span class="RichReports_Keyword">' . $s . '</span>'; })
    ->_(Literal(_), function ($s) { return '<span class="RichReports_Literal">' . $s . '</span>'; })
    ->_(Konstant(_), function ($s) { return '<span class="RichReports_Konstant">' . $s . '</span>'; })
    ->_(Operator(_), function ($s) { return '<span class="RichReports_Operator">' . $s . '</span>'; })
    ->_(Builtin(_), function ($s) { return '<span class="RichReports_Builtin">' . $s . '</span>'; })
    ->_(Library(_), function ($s) { return '<span class="RichReports_Library">' . $s . '</span>'; })
    ->_(Variable(_), function ($s) { return '<span class="RichReports_Variable">' . $s . '</span>'; })
    ->_(Error(_), function ($s) { return '<span class="RichReports_Error">' . $s . '</span>'; })
    ->_(Atom(_,_,_), function ($hs,$ms,$rs) {
        return $ms
          ? '<span><span class="RichReports_Clickable" '.messagesToAttr($ms) . '>'
            . '<span class="' 
            . ((count($hs) > 0 || count($ms) > 0) ? 'RichReports_Highlight' : '') . ' '
            . highlightsStr($hs) . '">' . conc($rs) . '</span></span></span>'
          : '<span class="'. highlightsStr($hs) . '">' . conc($rs) . '</span>';
      })
    ->_(Span(_,_,_), function ($hs,$ms,$rs) {
        return (count($ms) > 0)
          ? '<span><span class="RichReports_Clickable RichReports_Clickable_Exclamation" ' . messagesToAttr($ms) . '>!</span>'
          . '<span class="'. highlightsStr($hs) .'">' . conc($rs) . '</span></span>'
          : '<span class="'. highlightsStr($hs) .'">' . conc($rs) . '</span>';
      })
    ->_(Line(_), function ($rs) { return '<div>' . conc($rs) . '</div>'; })
    ->_(Block(_,_,_), function ($hs,$ms,$rs) {return '<div class="RichReports_Block">' . conc($rs) . '</div>';})
    ->_(Concat(_),  function ($rs) { return conc($rs);})
    ->_(Intersperse(_,_), function ($r,$rs) use ($html) {return implode($html($r), array_map($html, $rs));})
    ->_(Field(_), function ($rs) {return '<td>' . conc($rs) . '</td>';})
    ->_(Row(_),   function ($rs) {return '<tr>' . conc($rs);})
    ->_(Table(_), function ($rs) {return '<table>' . conc($rs) . '</table>';})
    ->_(Page(_), function ($r) {
        $s = html($r);
        return <<<REPORT
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <style>
      body {
        font-family: "Courier", monospace;
        font-size: 12px;
      }
      table {
        font-family: "Courier", monospace;
        font-size: 12px;
      }
      #RichReports_Message {
        background-color: yellow;
        padding: 3px;
        border: 1px solid black;
        font-family: "Courier", monospace;
        font-size: 12px;
        cursor: pointer;
      }
      .RichReports_Clickable {
        cursor: pointer;
      }
      .RichReports_Clickable_Exclamation {
        background-color: yellow;
        border: 1px solid black;
        margin: 0px 5px 1px 5px;
        padding: 0px 2px 0px 2px;
        font-size: 9px;
      }
      .RichReports_Clickable:hover {
        background-color: yellow;
      }

      .RichReports_Entity      { }
      .RichReports_Text        { }
      .RichReports_Symbol      { font-weight: bold;  color: black; }
      .RichReports_Punctuation { font-weight: bold;  color: black; }
      .RichReports_Keyword     { font-weight: bold;  color: blue; }
      .RichReports_Literal     { font-weight: bold;  color: firebrick; }
      .RichReports_Konstant    { font-weight: bold;  color: blue; }
      .RichReports_Operator    { font-weight: bold;  color: blue; }
      .RichReports_Builtin     { font-weight: bold;  color: purple; }
      .RichReports_Library     { font-weight: bold;  color: purple; }
      .RichReports_Variable    { font-style: italic; color: green; }
      .RichReports_Error {
        font-weight: bold;
        color: red;
        text-decoration: underline;
      }
      .RichReports_Highlight             { margin:           2px;       }
      .RichReports_Highlight_Unbound     { background-color: orange;    }
      .RichReports_Highlight_Unreachable { background-color: orange;    }
      .RichReports_Highlight_Duplicate   { background-color: yellow;    }
      .RichReports_Highlight_Error       { background-color: lightpink; }
      .RichReports_Block                 { margin-left:      10px;      }
    </style>
    <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.3/jquery.min.js"></script>
    <script type="text/javascript">
      function msg (obj, msgs) {
        var html = '';
        for (var i = 0; i < msgs.length; i++)
          html += '<div class="RichReports_MessagePortion">' + msgs[i] + '</div>';
        document.getElementById('RichReports_Message').innerHTML = html;
        document.getElementById('RichReports_Message').style.display = 'inline-block';
        var top = $(obj).offset().top;
        var left = $(obj).offset().left;
        $('#RichReports_Message').offset({top:top + 15, left:left + 15});
      }
    </script>
  </head>
  <body>
  {$s}
    <div id="RichReports_Message" style="display:none;" onclick="this.style.display='none';"></div>
  </body>
</html>
REPORT;
      })
    ->end;
};

/*eof*/ ?>