<?php /**************************************************************
**
** examples.php
**
**   Small examples that illustrate how the richreports.py module can
**   be used.
**
**   All required modules must be installed in order for this example
**   module to work correctly.
**
*/

include_once("uxadt.php"); if (!defined('_')) define('_', null);
include_once("richreports.php");

/********************************************************************
** Definition of an abstract syntax.
*/

\uxadt\_('Program', array(
    'Program' => array(array('Stmt'))
  ));

\uxadt\_('Stmt', array(
    'Repeat' => array(array('Stmt')),
    'Prints' => array('Exp'),
    'Errors' => array(),
    'Pass' => array()
  ));

\uxadt\_('Exp', array(
    'OpPlus' => array('Exp', 'Exp'),
    'OpMax' => array('Exp', 'Exp'),
    'OpAbs' => array('Exp'),
    'Num' => array('#')
  ));

/********************************************************************
** Example conversion algorithm that turns an abstract syntax tree
** into a report.
*/

function report($a) {
  if ($a->_(Program(_))) {
    list($ss) = $a();
    $rs = array(); foreach ($ss as $s) array_push($rs, report($s));
    return Page(Block(array(), array(), $rs));
  } else if ($a->_(Repeat(_))) {
    list($ss) = $a();
    $rs = array(); foreach ($ss as $s) array_push($rs, report($s));
    return
      Concat(array(
        Line(array(Keyword('repeat'))),
        Block(array(), array(), $rs)
      ));
  } else if ($a->_(Prints(_))) {
    list($e) = $a();
    return Line(array(Keyword('print'), Entity(Space()), report($e)));
  } else if ($a->_(Errors())) {
    return
      Line(array(
        Span(array(HighlightError()), array(Text('This is a message that applies to the whole highlighted span.')), array(Keyword('error'))),
        Entity(Space()), Text('outside'), Entity(Space()), Text('span')
      ));
  } else if ($a->_(Pass())) {
    return Line(array(Keyword('pass')));
  } else if ($a->_(OpPlus(_,_))) {
    list($e1,$e2) = $a();
    return Concat(array(report($e1), Keyword('+'), report($e2)));
  } else if ($a->_(OpMax(_,_))) {
    list($e1,$e2) = $a();
    return Concat(array(Builtin('max'), Punctuation('('), report($e1), Punctuation(','), report($e2), Punctuation(')')));
  } else if ($a->_(OpAbs(_))) {
    list($e) = $a();
    return Concat(array(Builtin('abs'), Punctuation('('), report($e), Punctuation(')')));
  } else if ($a->_(Num(_))) {
    list($n) = $a();
    return Atom(array(), array(Text('int')), array(Konstant(''.$n.'')));
  }
}

/********************************************************************
** Example of an abstraxt syntax tree, and rendering thereof as an
** interactive HTML report.
*/

$p = Program(array(Repeat(array(Prints(OpPlus(Num(2), Num(3))), Pass(), Errors(), Repeat(array(Prints(Num(7)), Pass())))), Prints(OpMax(OpAbs(Num(4)), Num(5))), Pass()));
echo \richreports\html(report($p));

/*eof*/ ?>