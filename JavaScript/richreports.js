
var ux = require('./node_modules/uxadt/lib/uxadt.js')
  ,  _ = require('underscore');



// extend underscore.js
_.conc = function (a,b) { return a.concat(b); };




/**
 * RichReports ADTs
 */

ux._({

  // Highlight
  HighlightUnbound: [],
  HighlightUnreachable: [],
  HighlightDuplicate: [],
  HighlightError: [],
  Highlight: [_],

  // Category
  Keyword: [],
  Literal: [],
  Constant: [],
  Variable: [],
  Error: [],

  // Entity
  Space: [],
  Lt: [],
  Gt: [],
  Ampersand: [],

  // Report
  Entity: [_],
  Text: [_],
  C: [_,_,_,_],
  Conc: [_],
  Field: [_],
  Row: [_],
  Table: [_],
  Indent: [_],
  Line: [_,_],
  LineIfFlat: [_,_],
  Atom: [_,_,_],
  Span: [_,_,_],
  Block: [_,_,_],
  BlockIndent: [_,_,_],
  Intersperse: [_,_],
  Finalize: [_]

});




function entity (e) {
  return e
    ._(Space(),     function () { return "&nbsp;"; })
    ._(Lt(),        function () { return "&lt;"; })
    ._(Gt(),        function () { return "&gt;"; })
    ._(Ampersand(), function () { return "&amp;"; })
    .end;
}



function highlight (h) {
  return h
    ._(HighlightUnbound(),     function ()   { return ["RichReports_Highlight_Unbound"]; } )
    ._(HighlightUnreachable(), function ()   { return ["RichReports_Highlight_Unreachable"]; } )
    ._(HighlightDuplicate(),   function ()   { return ["RichReports_Highlight_Duplicate"]; } )
    ._(HighlightError(),       function ()   { return ["RichReports_Highlight_Error"]; } )
    ._(Highlight(_),           function (hs) { return hs; })
    .end;
}


function messageToAttr (ms) {
  function conv (m) {
    return '\''+html(m)
      .replace('"', '&quot;')
      .replace('\'', '\\\'')
      .replace('\n', '')
      .replace('\r', '')
      + '\'';
  }
  return 'onclick=msg(this, ['+ _.map(ms, conv).join(',') +']);';
}


function html (r) {
  function html0(rs) {
    return _.reduce(_.map(rs, html), _.conc, '');
  }
  return r
    ._(Text(_), _.identity)
    ._(C(_,_,_,_), function (c,hs,ms,s) {
        return '<span class="RichReports_"'+c
            + (hs.length ? ' RichReports_Clickable' : '')
            + (ms.length ? ' RichReports_Highlightable' : '')
            + _.reduce(_.map(hs, highlight), _.conc, []).join(' ')
            + '" ' + (ms.length ? messageToAttr(ms) : '') 
            + '>' + s + '</span>';
      })
    ._(Conc(_), html0)
    ._(Entity(_), entity)
    ._(Field(_), function (rs) {
        return '<td>' + html0(rs) + '</td>';
      })
    ._(Row(_), function (rs) {
        return '<tr>' + html0(rs) + '</tr>';
      })
    ._(Table(_), function (rs) {
        return '<table>' + html0(rs) + '</table>';
      })
    ._(Line(_,_), function (_0,rs) {
        return '<div>' + html0(rs) + '</div>';
      })
    ._(Atom(_,_,_), function (hs,ms,rs) {
        var out = '<span class="'+ _.reduce(_.map(hs, highlight), _.conc, []).join(' ') +'">'+ html0(rs) +'</span>';
        return ms.length
            ? '<span><span class="RichReports_Clickable" '+messageToAttr(ms)+'>'+out+'</span></span>'
            : out;
      })
    ._(Span(_,_,_), function (hs,ms,rs) {
        var out = '<span class="'+ _.reduce(_.map(hs, highlight), _.conc, '') +'">' + html0(rs) + '</span>';
        return ms.length
            ? '<span>'
                + '<span class="RichReports_Clickable RichReports_Clickable_Exclamation" '+ messageToAttr(ms) +'>!</span>'
                + out
              + '</span>'
            : out;
      })
    ._(Block(_,_,_), function (_0,_1,rs) {
        return '<div>' + html0(rs) + '</div>';
      })
    ._(BlockIndent(_,_,_), function (_0,_1,rs) {
        return '<div class="RichReports_BlockIndent">' + html0(rs) + '</div>'; 
      })
    ._(Intersperse(_,_), function (r,rs) {
        return _.map(rs, html).join( html(r) );
      })
    ._(Finalize(_), function (r) {
        return '\
          <!DOCTYPE html>\
          <html>\
          <head>\
            <meta charset="utf-8">\
            <style>\
              body {\
                font-family: "Courier", monospace;\
                font-size: 12px;\
              }\
              table {\
                font-family: "Courier", monospace;\
                font-size: 12px;\
              }\
              #RichReports_Message {\
                background-color: yellow;\
                padding: 3px;\
                border: 1px solid black;\
                font-family: "Courier", monospace;\
                font-size: 12px;\
                cursor: pointer;\
              }\
              .RichReports_Clickable {\
                cursor: pointer;\
              }\
              .RichReports_Clickable_Exclamation {\
                background-color: yellow;\
                border: 1px solid black;\
                margin: 0px 5px 1px 5px;\
                padding: 0px 2px 0px 2px;\
                font-size: 9px;\
              }\
              .RichReports_Clickable:hover {\
                background-color: yellow;\
              }\
              .RichReports_Keyword {\
                font-weight: bold;\
                color: blue;\
              }\
              .RichReports_Variable {\
                font-style: italic;\
                color: green;\
              }\
              .RichReports_Literal {\
                font-weight: bold;\
                color: firebrick;\
              }\
              .RichReports_Error {\
                font-weight: bold;\
                color: red;\
                text-decoration: underline;\
              }\
              .RichReports_Highlight             { margin:           2px;       }\
              .RichReports_Highlight_Unbound     { background-color: orange;    }\
              .RichReports_Highlight_Unreachable { background-color: orange;    }\
              .RichReports_Highlight_Duplicate   { background-color: yellow;    }\
              .RichReports_Highlight_Error       { background-color: lightpink; }\
              .RichReports_BlockIndent           { margin-left:      10px;      }\
            </style>\
            <script type="text/javascript", src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.3/jquery.min.js"></script>\
            <script type="text/javascript">\
              funciton msg (obj, msgs) {\
                var html = \'\';\
                for (var i = 0; i < msgs.length; i++)\
                  html += \'<div class="RichReports_MessagePortion">\' + msgs[i] + \'</div>\';\
                document.getElementById(\'RichReports_Message\').innerHTML = html;\
                document.getElementById(\'RichReports_Message\').style.display = \'inline-block\';\
                var top = _(obj).offset().top;\
                var left = _(obj).offset().left;\
                _(\'#RichReports_Message\').offset({top:top + 15, left:left + 15});\
              }     \
            </script>\
          </head>\
          <body>\
            '+ html(r) +'\
            <div id="RichReports_Message" style="display:none;" onclick="this.style.display=\'none\';"></div>\
          </body>\
          </html>';
      })
    .end;
}




//eof