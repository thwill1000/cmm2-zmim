Option Explicit On
Option Default Integer

#Include "mem_cmm2.inc"
#Include "dict.inc"
#Include "zstring.inc"
#Include "util.inc"
#Include "io.inc"

Dim a, s$, x

Cls

mem_init("A:/zmim/stories/minizork.z3")
di_init()

Open "A:/zmim/src/tests/minizork.dic" For Input As #1
Timer = 0
Do While Not Eof(#1)
  Line Input #1, s$
  a = Val(Left$(s$, 6))
  s$ = Mid$(s$, 8)
  x = di_lookup(s$)
  If x = a Then
    cout(rpad$(s$, 8) + "=> " + Str$(x) + " - OK") : endl()
  Else
    cout(rpad$(s$, 8) + "=> " + Str$(x) + " - ERROR") : endl()
  EndIf
Loop
cout("Dictionary test took " + Str$(Timer) + " ms") : endl()
