/********************************************************************
** 
** richreports.js
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

(function (richreports) {

  "use strict";

  var uxadt = require("uxadt"); var _ = null;
  var R = richreports;

  /******************************************************************
  ** Rich report data structure definitions.
  */

  uxadt._(richreports, 'Highlight', {
    HighlightUnbound: [],
    HighlightUnreachable: [],
    HighlightDuplicate: [],
    HighlightError: [],
    Highlight: ['$']
  });

  uxadt._(richreports, 'Entity', {
    Space: [],
    Lt: [],
    Gt: [],
    Ampersand: []
  });

  uxadt._(richreports, 'Report', {
    Entity: ['Entity'],
    Text: ['$'],
    Symbol: ['$'],
    Punctuation: ['$'],
    Keyword: ['$'],
    Literal: ['$'],
    Konstant: ['$'], // "Constant" is a reserved word in some cases.
    Operator: ['$'],
    Builtin: ['$'],
    Library: ['$'],
    Variable: ['$'],
    Error: ['$'],

    Line: [['Report']],
    Atom: [['Highlight'], ['Report'], ['Report']],
    Span: [['Highlight'], ['Report'], ['Report']],
    Block: [['Highlight'], ['Report'], ['Report']],

    Concat: [['Report']],
    Intersperse: ['Report', ['Report']],
    Field: [['Report']],
    Row: [['Report']],
    Table: [['Report']],

    Page: ['Report']
  });
  
  /******************************************************************
  ** Interactive HTML report rendering functions.
  */

  richreports.highlightsStr = function (hs) {
    // Replace with "_.reduce(_.map(hs, highlightStr), _.conc, []).join(' ')".
    var s = "";
    for (var i = 0; i < hs.length; i++)
      s += richreports.highlightStr(hs[i]) + ' ';
    return s;
  }

  richreports.highlightStr = function (h) {
    return h
      ._(R.HighlightUnbound(),     function ()   { return ["RichReports_Highlight_Unbound"]; } )
      ._(R.HighlightUnreachable(), function ()   { return ["RichReports_Highlight_Unreachable"]; } )
      ._(R.HighlightDuplicate(),   function ()   { return ["RichReports_Highlight_Duplicate"]; } )
      ._(R.HighlightError(),       function ()   { return ["RichReports_Highlight_Error"]; } )
      ._(R.Highlight(_),           function (hs) { return hs; })
      .end;
  }

  richreports.entityStr = function (e) {
    return e
      ._(R.Space(),     function () { return "&nbsp;"; })
      ._(R.Lt(),        function () { return "&lt;"; })
      ._(R.Gt(),        function () { return "&gt;"; })
      ._(R.Ampersand(), function () { return "&amp;"; })
      .end;
  }

  richreports.messagesToAttr = function (ms) {
    function conv (m) {
      return '\'' + R.html(m)
        .replace(/\"/g, '&quot;')
        .replace(/\'/g, '\\\'')
        .replace(/\n/g, '')
        .replace(/\r/g, '')
        + '\'';
    }
    var s = ''; for (var i = 0; i < ms.length; i++) s += conv(ms[i]);
    return 'onclick="msg(this, ['+ s +']);"';
  }

  richreports.html = function (r) {
    var html = R.html;
    var highlightsStr = R.highlightsStr;
    var entityStr = R.entityStr;
    var messagesToAttr = R.messagesToAttr;
    function conc(rs) { var s = ""; for (var i=0; i<rs.length; i++) s += R.html(rs[i]); return s; }

    return r
      ._(R.Entity(_), function(e) { return '<span class="RichReports_Entity">' + entityStr(e) + '</span>'; })
      ._(R.Text(_), function(s) { return '<span class="RichReports_Text">' + s + '</span>'; })
      ._(R.Symbol(_), function(s) { return '<span class="RichReports_Symbol">' + s + '</span>'; })
      ._(R.Punctuation(_), function(s) { return '<span class="RichReports_Punctuation">' + s + '</span>'; })
      ._(R.Keyword(_), function(s) { return '<span class="RichReports_Keyword">' + s + '</span>'; })
      ._(R.Literal(_), function(s) { return '<span class="RichReports_Literal">' + s + '</span>'; })
      ._(R.Konstant(_), function(s) { return '<span class="RichReports_Konstant">' + s + '</span>'; })
      ._(R.Operator(_), function(s) { return '<span class="RichReports_Operator">' + s + '</span>'; })
      ._(R.Builtin(_), function(s) { return '<span class="RichReports_Builtin">' + s + '</span>'; })
      ._(R.Library(_), function(s) { return '<span class="RichReports_Library">' + s + '</span>'; })
      ._(R.Variable(_), function(s) { return '<span class="RichReports_Variable">' + s + '</span>'; })
      ._(R.Error(_), function(s) { return '<span class="RichReports_Error">' + s + '</span>'; })
      ._(R.Atom(_,_,_), function (hs,ms,rs) {
          var out = '<span class="'+ ((hs.length > 0 || ms.length > 0) ? ' RichReports_Highlight' : '') + ' ' + highlightsStr(hs) + '">'+ conc(rs) +'</span>';
          return (ms.length > 0)
              ? '<span><span class="RichReports_Clickable" '+messagesToAttr(ms)+'>'+out+'</span></span>'
              : '<span class="' + highlightsStr(hs) + '">'+ conc(rs) +'</span>';
        })
      ._(R.Span(_,_,_), function (hs,ms,rs) {
          var out = '<span class="'+ highlightsStr(hs) + '">' + conc(rs) + '</span>';
          return (ms.length > 0)
              ? '<span>'
                  + '<span class="RichReports_Clickable RichReports_Clickable_Exclamation" '+ messagesToAttr(ms) +'>!</span>'
                  + out
                + '</span>'
              : out;
        })
      ._(R.Line(_), function (rs) { return '<div>' + conc(rs) + '</div>'; })
      ._(R.Block(_,_,_), function (hs,ms,rs) { return '<div class="RichReports_Block">' + conc(rs) + '</div>'; })
      ._(R.Concat(_), function (rs) { return conc(rs); } )
      ._(R.Intersperse(_,_), function (r, rs) {
          var s = "";
          var sep = R.html(r);
          for (var i=0; i<rs.length; i++)
            s += ((i>0) ? sep : '') + R.html(rs[i]);
          return s;
        })
      ._(R.Field(_), function (rs) { return '<td>' + conc(rs) + '</td>'; })
      ._(R.Row(_), function (rs) { return '<tr>' + conc(rs) + '</tr>'; })
      ._(R.Table(_), function (rs) { return '<table>' + conc(rs) + '</table>'; })
      ._(R.Page(_), function (r) {
          return '\
<!DOCTYPE html>\n\
<html\n>\
  <head>\n\
    <meta charset="utf-8">\n\
    <style>\n\
      body {\n\
        font-family: "Courier", monospace;\n\
        font-size: 12px;\n\
      }\n\
      table {\n\
        font-family: "Courier", monospace;\n\
        font-size: 12px;\n\
      }\n\
      #RichReports_Message {\n\
        background-color: yellow;\n\
        padding: 3px;\n\
        border: 1px solid black;\n\
        font-family: "Courier", monospace;\n\
        font-size: 12px;\n\
        cursor: pointer;\n\
      }\n\
      .RichReports_Clickable {\n\
        cursor: pointer;\n\
      }\n\
      .RichReports_Clickable_Exclamation {\n\
        background-color: yellow;\n\
        border: 1px solid black;\n\
        margin: 0px 5px 1px 5px;\n\
        padding: 0px 2px 0px 2px;\n\
        font-size: 9px;\n\
      }\n\
      .RichReports_Clickable:hover {\n\
        background-color: yellow;\n\
      }\n\
\n\
      .RichReports_Entity      { }\n\
      .RichReports_Text        { }\n\
      .RichReports_Symbol      { font-weight: bold;  color: black; }\n\
      .RichReports_Punctuation { font-weight: bold;  color: black; }\n\
      .RichReports_Keyword     { font-weight: bold;  color: blue; }\n\
      .RichReports_Literal     { font-weight: bold;  color: firebrick; }\n\
      .RichReports_Konstant    { font-weight: bold;  color: blue; }\n\
      .RichReports_Operator    { font-weight: bold;  color: blue; }\n\
      .RichReports_Builtin     { font-weight: bold;  color: purple; }\n\
      .RichReports_Library     { font-weight: bold;  color: purple; }\n\
      .RichReports_Variable    { font-style: italic; color: green; }\n\
      .RichReports_Error {\n\
        font-weight: bold;\n\
        color: red;\n\
        text-decoration: underline;\n\
      }\n\
      .RichReports_Highlight             { margin:           2px;       }\n\
      .RichReports_Highlight_Unbound     { background-color: orange;    }\n\
      .RichReports_Highlight_Unreachable { background-color: orange;    }\n\
      .RichReports_Highlight_Duplicate   { background-color: yellow;    }\n\
      .RichReports_Highlight_Error       { background-color: lightpink; }\n\
      .RichReports_Block                 { margin-left:      10px;      }\n\
    </style>\n\
    <script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.3/jquery.min.js"></script>\n\
    <script type="text/javascript">\n\
      function msg (obj, msgs) {\n\
        var html = \'\';\n\
        for (var i = 0; i < msgs.length; i++)\n\
          html += \'<div class="RichReports_MessagePortion">\' + msgs[i] + \'</div>\';\n\
        document.getElementById(\'RichReports_Message\').innerHTML = html;\n\
        document.getElementById(\'RichReports_Message\').style.display = \'inline-block\';\n\
        var top = $(obj).offset().top;\n\
        var left = $(obj).offset().left;\n\
        $(\'#RichReports_Message\').offset({top:top + 15, left:left + 15});\n\
      }\n\
    </script>\n\
  </head>\n\
  <body>\n\
    '+ html(r) +'\n\
    <div id="RichReports_Message" style="display:none;" onclick="this.style.display=\'none\';"></div>\n\
  </body>\n\
</html>';
        })
      .end;
  }

})(typeof exports !== 'undefined' ? exports : (this.richreports = {}));
/* eof */