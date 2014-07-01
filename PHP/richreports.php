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
**   Version: 0.0.2.0
**
*/

include('uxadt.php');
define('_', null);

/********************************************************************
** Rich report data structure definitions.
*/

\uxadt\_('Highlight', array(
  'HighlightUnbound' => array(),
  'HighlightUnreachable' => array(),
  'HighlightDuplicate' => array(),
  'HighlightError' => array(),
  'Highlight' => array('$'),
  ));

\uxadt\_('Entity', array(
  'Space' => array(),
  'Lt' => array(),
  'Gt' => array(),
  'Ampersand' => array(),
  ));

\uxadt\_('Category', array(
  'Symbol' => array(),
  'Punctuation' => array(),
  'Keyword' => array(),
  'Literal' => array(),
  'Constant' => array(), // Constant is a reserved word
  'Operator' => array(),
  'Builtin' => array(),
  'Library' => array(), 
  'Variable' => array(),
  'Error' => array(),
  ));

\uxadt\_('Report', array(
  'C' => array('Category', array('Highlight'), array('Report'), '$'),
  'Text' => array('$'),
  'Entity' => array('Entity'),

  'Line' => array(array('Report')),
  'Atom' => array(array('Highlight'), array('Report'), array('Report')),
  'Span' => array(array('Highlight'), array('Report'), array('Report')),
  'Block' => array(array('Highlight'), array('Report'), array('Report')),
  
  'Conc' => array(array('Report')),
  'Intersperse' => array('Report', array('Report')),
  'Field' => array(array('Report')),
  'Row' => array(array('Report')),
  'Table' => array(array('Report')),

  'Page' => array('Report')
  ));

/********************************************************************
** Interactive HTML report rendering functions.
*/

$highlight = function ($h) {
  return $h
    ->_(HighlightUnbound(),     function ()    { return array("RichReports_Highlight_Unbound"); } )
    ->_(HighlightUnreachable(), function ()    { return array("RichReports_Highlight_Unreachable"); } )
    ->_(HighlightDuplicate(),   function ()    { return array("RichReports_Highlight_Duplicate"); } )
    ->_(HighlightError(),       function ()    { return array("RichReports_Highlight_Error"); } )
    ->_(Highlight(_),           function ($hs) { return $hs; })
    ->end;
};

$entity = function ($e) {
  return $e
    ->_(Space(),     function () { return '&nbsp;'; })
    ->_(Lt(),        function () { return '&lt;'; })
    ->_(Gt(),        function () { return '&gt;'; })
    ->_(Ampersand(), function () { return '&amp;'; })
    ->end;
};

$category = function ($c) {
  return $c
    ->_(Symbol(),      function () { return 'Symbol'; })
    ->_(Punctuation(), function () { return 'Punctuation'; })
    ->_(Keyword(),     function () { return 'Keyword'; })
    ->_(Literal(),     function () { return 'Literal'; })
    ->_(Constant(),    function () { return 'Constant'; })
    ->_(Operator(),    function () { return 'Operator'; })
    ->_(Builtin(),     function () { return 'Builtin'; })
    ->_(Library(),     function () { return 'Library'; })
    ->_(Variable(),    function () { return 'Variable'; })
    ->_(Error(),       function () { return 'Error'; })
    ->end;
};

$messageToAttr = function ($ms) {
  $conv = function ($m) {
    $m = '\'' . html($m);
    $m = str_replace('"', '&quot;', $m);
    $m = str_replace('\'', '\\\'', $m);
    $m = str_replace('\n', '', $m);
    $m = str_replace('\r', '');
    $m = $m . '\'';
    return $m;
  };
  return 'onclick=msg(this, ['. implode(',', array_map($conv, $ms)) .']);';
};

$html = function ($r) {
  $html0 = function ($rs) {
    return implode(array_map($html, $rs));
  };
  return $r
    ->_(Text(_), function ($s) {return $s;})
    ->_(C(_,_,_,_), function ($c, $hs, $ms, $s) {
        return '<span class="RichReports_"' . $c
          . ($hs ? ' RichReports_Clickable' : '')
          . ($ms ? ' RichReports_Highlightable' : '')
          . implode(' ', array_map($highlight, $hs))
          . '" ' . ($ms ? $messageToAttr($ms) : '')
          . '>' . $s . '</span>';
      })
    ->_(Entity(), $entity)
    ->_(Space(),   function () {return '&nbsp;';})
    ->_(Conc(_),  function ($rs) {return $html0($rs);})
    ->_(Field(_), function ($rs) {return '<td>' . $html0($rs) . '</td>';})
    ->_(Row(_),   function ($rs) {return '<tr>' . $html0($rs);})
    ->_(Table(_), function ($rs) {return '<table>' . $html0($rs) . '</table>';})
    ->_(Line(_,_), function ($_, $rs) {return '<div>' . $html0($rs) . '</div>';})
    ->_(Atom(_,_,_), function ($hs,$ms,$rs) {
        return $ms ? '<span><span class="RichReports_Clickable" '.$messageToAttr($ms) . '><span class="' . implode(array_map($highlight, $hs)) . '">' . $html0($rs) . '</span></span></span>'
                   : '<span class="'. implode(array_map($highlight, $hs)) . '">' . $html0($rs) . '</span>';
      })
    ->_(Span(_,_,_), function ($hs,$ms,$rs) {
        return $ms ? '<span><span class="RichReports_Clickable RichReports_Clickable_Exclamation" ' . $messageToAttr($ms) . '>!</span><span class="'. implode(array_map($highlight, $hs)) .'">' . $html0($rs) . '</span></span>'
                   : '<span class="'. implode(array_map($highlight, $hs)) .'">' . $html0($rs) . '</span>';
      })
    ->_(Block(_,_,_), function ($_,$_,$rs) {return '<div class="RichReports_Block">' . $html0($rs) . '</div>';})
    ->_(Intersperse(_,_), function ($r,$rs) {return implode($html($r), array_map($html, $rs));})
    ->_(Finalize(_), function ($r) {
        $s = $html($r);
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
    .RichReports_Symbol       { font-weight: bold;  color: black; }
    .RichReports_Punctuation  { font-weight: bold;  color: black; }
    .RichReports_Keyword      { font-weight: bold;  color: blue; }
    .RichReports_Literal      { font-weight: bold;  color: firebrick; }
    .RichReports_Constant     { font-weight: bold;  color: blue; }
    .RichReports_Operator     { font-weight: bold;  color: blue; }
    .RichReports_Builtin      { font-weight: bold;  color: purple; }
    .RichReports_Library      { font-weight: bold;  color: purple; }
    .RichReports_Variable     { font-style: italic; color: green; }
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