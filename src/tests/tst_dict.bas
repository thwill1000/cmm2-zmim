' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Option Explicit On
Option Default Integer

'#Include "../mem_cmm2_fast.inc"
#Include "../mem_cmm2_safe.inc"
#Include "../dict.inc"
#Include "../zstring.inc"
#Include "../console.inc"
#Include "../util.inc"

Dim a, s$, x

Cls

mem_init("/zmim/stories/minizork.z3")
di_init()

Open "/zmim/src/tests/minizork.dic" For Input As #1
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
