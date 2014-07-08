/********************************************************************
** 
** examples.js
**
**   Small examples that illustrate how the richreports.py module can
**   be used.
**
**   All required modules must be installed in order for this example
**   module to work correctly.
**
*/

(function (examples) {

  "use strict";

  var fs = require('fs');
  var uxadt = require('uxadt'); var _ = null;
  var R = require('richreports');

  /******************************************************************
  ** Definition of an abstract syntax.
  */

  uxadt._('Program', {
      Program: [['Stmt']]
    });

  uxadt._('Stmt', {
      Repeat: [['Stmt']],
      Prints: ['Exp'],
      Errors: [],
      Pass: []
    });

  uxadt._('Exp', {
      OpPlus: ['Exp', 'Exp'],
      OpMax: ['Exp', 'Exp'],
      OpAbs: ['Exp'],
      Num: ['#']
    });

  /******************************************************************
  ** Example conversion algorithm that turns an abstract syntax tree
  ** into a report.
  */
  
  function report(a) {
    return a
      ._(Program(_), function(ss) {
          var rs = [];
          for (var i = 0; i < ss.length; i++)
            rs.push(report(ss[i]));
          return R.Page(R.Block([], [], rs));
        })
      ._(Repeat(_), function(ss) {
          var rs = [];
          for (var i = 0; i < ss.length; i++)
            rs.push(report(ss[i]));
          return (
            R.Concat([
              R.Line([R.Keyword('repeat')]),
              R.Block([], [], rs)
            ])
          );
        })
      ._(Prints(_), function(e) {
          return R.Line([R.Keyword('print'), R.Entity(R.Space()), report(e)]);
        })
      ._(Errors(), function() {
          return (
            R.Line([
              R.Span([R.HighlightError()], [R.Text('This is a message that applies to the whole highlighted span.')], [R.Keyword('error')]),
              R.Entity(R.Space()), R.Text('outside'), R.Entity(R.Space()), R.Text('span')
            ])
          );
        })
       ._(Pass(), function() {
          return R.Line([R.Keyword('pass')]);
        })
       ._(OpPlus(_, _), function(e1, e2) {
          return R.Concat([report(e1), R.Keyword('+'), report(e2)]);
        })
       ._(OpMax(_, _), function(e1, e2) {
          return R.Concat([R.Builtin('max'), R.Punctuation('('), report(e1), R.Punctuation(','), report(e2), R.Punctuation(')')]);
        })
       ._(OpAbs(_), function(e) {
          return R.Concat([R.Builtin('abs'), R.Punctuation('('), report(e), R.Punctuation(')')]);
        })
       ._(Num(_), function(n) {
          return R.Atom([], [R.Text('int')], [R.Konstant(''+n+'')]);
        })
       .end;
  }

  /******************************************************************
  ** Example of an abstraxt syntax tree, and rendering thereof as an
  ** interactive HTML report.
  */

  var p = Program([Repeat([Prints(OpPlus(Num(2), Num(3))), Pass(), Errors(), Repeat([Prints(Num(7)), Pass()])]), Prints(OpMax(OpAbs(Num(4)), Num(5))), Pass()]);
  console.log("The program:\n" + p.toString());
  console.log("\nThe report:\n" + report(p).toString());
  fs.writeFile("report.html", R.html(report(p)), function(err) { console.log((err) ? err : '\nThe file "report.html" has been written.'); });

})(typeof exports !== 'undefined' ? exports : (this.examples = {}));
/* eof */