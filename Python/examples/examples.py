#####################################################################
## 
## examples.py
##
##   Examples that illustrate how the richreports.py module can be
##   used.
##
##   All required modules must be installed in order for this example
##   module to work correctly.
##

import uxadt
import richreports as R
_ = None

#####################################################################
## Definition of an abstract syntax.
##

uxadt._('Program', {\
    'Program': [['Stmt']]
  })

uxadt._('Stmt', {\
    'Repeat': [['Stmt']],\
    'Prints': ['Exp'],\
    'Errors': [],\
    'Pass': []\
  })

uxadt._('Exp', {\
    'OpPlus': ['Exp', 'Exp'],\
    'OpMax': ['Exp', 'Exp'],\
    'OpAbs': ['Exp'],\
    'Num': ['#']
  })

#####################################################################
## Example conversion algorithm that turns an abstract syntax tree
## into a report.
##

def report(a):
    if a < Program(_):
        (ss,) = a
        return R.Page(R.Block([], [], [report(s) for s in ss]))

    if a < Repeat(_):
        (ss,) = a
        return\
          R.Concat([\
            R.Line([R.Keyword('repeat')]),\
            R.Block([], [], [report(s) for s in ss])\
            ])
    if a < Prints(_):
        (e,) = a
        return R.Line([R.Keyword('print'), R.Entity(R.Space()), report(e)])
    if a < Errors():
        return\
          R.Line([\
            R.Span([R.HighlightError()], [R.Text('This is a message that applies to the whole highlighted span.')], [R.Keyword('error')]),\
            R.Entity(R.Space()), R.Text('outside'), R.Entity(R.Space()), R.Text('span')\
            ])
    if a < Pass():
        return R.Line([R.Keyword('pass')])

    if a < OpPlus(_,_):
        (e1,e2) = a
        return R.Concat([report(e1), R.Keyword('+'), report(e2)])
    if a < OpMax(_,_):
        (e1,e2) = a
        return R.Concat([R.Builtin('max'), R.Punctuation('('), report(e1), R.Punctuation(','), report(e2), R.Punctuation(')')])
    if a < OpAbs(_):
        (e,) = a
        return R.Concat([R.Builtin('abs'), R.Punctuation('('), report(e), R.Punctuation(')')])
    if a < Num(_):
        (n,) = a
        return R.Atom([], [R.Text('int')], [R.Konstant(str(n))])

#####################################################################
## Example of an abstraxt syntax tree, and rendering thereof as an
## interactive HTML report.
##

p = Program([Repeat([Prints(OpPlus(Num(2), Num(3))), Pass(), Errors(), Repeat([Prints(Num(7)), Pass()])]), Prints(OpMax(OpAbs(Num(4)), Num(5))), Pass()])
print("The program:\n" + str(p))
print("\nThe report:\n" + str(report(p)))
open('report.html', 'w').write(R.html(report(p)))
print('\nThe file "report.html" has been written.')

##eof
