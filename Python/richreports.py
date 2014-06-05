
from uxadt import uxadt, _

eval(uxadt.definition({

  # Highlight
  'HighlightUnbound': [],
  'HighlightUnreachable': [],
  'HighlightDuplicate': [],
  'HighlightError': [],
  'Highlight': [_],

  # Category
  'Keyword': [],
  'Literal': [],
  'Constant': [],
  'Variable': [],
  'Error': [],

  # Report
  'Text': [_],
  'C': [_,_,_,_],
  'Space': [],
  'Lt': [],
  'Gt': [],
  'Conc': [_],
  'Field': [_],
  'Row': [_],
  'Table': [_],
  'Indent': [_],
  'Line': [_,_],
  'LineIfFlat': [_,_],
  'Atom': [_,_,_],
  'Span': [_,_,_],
  'Block': [_,_,_],
  'BlockIndent': [_,_,_],
  'Intersperse': [_,_],
  'Finalize': [_]
  }))


def highlight(h):
  return h\
    .match(HighlightUnbound(), lambda: ['RichReports_Highlight_Unbound'])\
    .match(HighlightUnreachable(), lambda: ['RichReports_Highlight_Unreachable'])\
    .match(HighlightDuplicate(), lambda: ['RichReports_Highlight_Duplicate'])\
    .match(HighlightError(), lambda: ['RichReports_Highlight_Error'])\
    .match(Highlight(_), lambda hs: hs)


def messageToAttr(ms):
  def conv(m):
    return '\''+html(m)\
      .replace('"', '&quot;')\
      .replace('\'', '\\\'')\
      .replace('\n', '')\
      .replace('\r', '')\
      + '\'';
  return 'onclick=msg(this, ['+ ','.join([conv(m) for m in ms]) +']);'


def html(r):
  def html0(rs):
    return ''.join([html(r) for r in rs])
  return r\
    .match(Text(_), lambda s: s)\
    .match(C(_,_,_,_), lambda c,hs,ms,s:
      '<span class="RichReports_"'+c\
      + (' RichReports_Clickable' if hs else '')\
      + (' RichReports_Highlightable' if ms else '')\
      + ' '.join([highlight(h) for h in hs])\
      + '" ' + (messageToAttr(ms) if ms else '')\
      + '>' + s + '</span>'\
      )\
    .match(Space(), lambda: '&nbsp;')\
    .match(Conc(_), lambda rs: html0(rs))\
    .match(Field(_), lambda rs: '<td>'+ html0(rs) + '</td>')\
    .match(Row(_), lambda rs: '<tr>' + html0(rs) + '</tr>')\
    .match(Table(_), lambda rs: '<table>' + html0(rs) + '</table>')\
    .match(Line(_,_), lambda _,rs: '<div>' + html0(rs) + '</div>')\
    .match(Atom(_,_,_), lambda hs,ms,rs:\
      '<span><span class="RichReports_Clickable" '+messageToAttr(ms)+'><span class="'+ ''.join([highlight(h) for h in hs]) +'">' + html0(rs) + '</span></span></span>'\
      if ms\
      else '<span class="'+ ''.join([highlight(h) for h in hs]) +'">' + html0(rs) + '</span>'\
      )\
    .match(Span(_,_,_), lambda hs,ms,rs:\
      '<span><span class="RichReports_Clickable RichReports_Clickable_Exclamation" '+messageToAttr(ms)+'>!</span><span class="'+ ''.join([highlight(hs) for h in hs]) +'">' + html0(rs) + '</span></span>'\
      if ms\
      else '<span class="'+ ''.join([highlight(hs) for h in hs]) +'">' + html0(rs) + '</span>'\
      )\
    .match(Block(_,_,_), lambda _0,_1,rs: '<div>' + html0(rs) + '</div>')\
    .match(BlockIndent(_,_,_), lambda _0,_1,rs: '<div class="RichReports_BlockIndent">' + html0(rs) + '</div>')\
    .match(Intersperse(_,_), lambda r,rs: html(r).join([html(r0) for r0 in rs]))\
    .match(Finalize(_), lambda r: '''
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
            .RichReports_Keyword {
              font-weight: bold;
              color: blue;
            }
            .RichReports_Variable {
              font-style: italic;
              color: green;
            }
            .RichReports_Literal {
              font-weight: bold;
              color: firebrick;
            }
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
            .RichReports_BlockIndent           { margin-left:      10px;      }
          </style>
          <script type="text/javascript", src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.3/jquery.min.js"></script>
          <script type="text/javascript">
            funciton msg (obj, msgs) {
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
          ''' + html(r) + '''
          <div id="RichReports_Message" style="display:none;" onclick="this.style.display='none';"></div>
        </body>
        </html>
      ''')\
    .end

#eof