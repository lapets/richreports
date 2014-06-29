#####################################################################
## 
## examples.py
##
##   Examples that illustrate how the richreports.py module can be
##   used.
##
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
    'Print': ['Exp'],\
    'Error': [],\
    'Pass': []\
  })

uxadt._('Exp', {\
    'Plus': ['Exp', 'Exp'],\
    'Max': ['Exp', 'Exp'],\
    'Abs': ['Exp'],\
    'Number': [_]
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
          R.Conc([\
            R.Line([R.C(R.Keyword(), [], [], 'repeat')]),\
            R.Block([], [], [report(s) for s in ss])\
            ])
    if a < Print(_):
        (e,) = a
        return R.Line([R.C(R.Keyword(), [], [], 'print'), R.Entity(R.Space()), report(e)])
    if a < Pass():
        return R.Line([R.C(R.Keyword(), [], [], 'pass')])
    if a < Error():
        return\
          R.Line([\
            R.Span([R.HighlightError()], [R.Text('This is a message that applies to the whole highlighted span.')], [R.C(R.Keyword(), [], [], 'error')]),\
            R.Entity(R.Space()), R.Text('outside'), R.Entity(R.Space()), R.Text('span')\
            ])

    if a < Plus(_,_):
        (e1,e2) = a
        return R.Conc([report(e1), R.C(R.Keyword(), [], [], '+'), report(e2)])
    if a < Max(_,_):
        (e1,e2) = a
        return R.Conc([R.C(R.Builtin(), [], [], 'max'), R.C(R.Punctuation(), [], [], '('), report(e1), R.C(R.Punctuation(), [], [], ','), report(e2), R.C(R.Punctuation(), [], [], ')')])
    if a < Abs(_):
        (e,) = a
        return R.Conc([R.C(R.Builtin(), [], [], 'abs'), R.C(R.Punctuation(), [], [], '('), report(e), R.C(R.Punctuation(), [], [], ')')])
    if a < Number(_):
        (n,) = a
        return R.C(R.Constant(), [], [R.Text('int')], str(n))

#####################################################################
## Example of an abstraxt syntax tree, and rendering thereof as an
## interactive HTML report.
##

p = Program([Repeat([Print(Plus(Number(2), Number(3))), Pass(), Error(), Repeat([Print(Number(7)), Pass()])]), Print(Max(Abs(Number(4)), Number(5))), Pass()])
print(p)
print(report(p))
open('examples.html', 'w').write(R.html(report(p)))

##eof
