' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Option Explicit On
Option Default Integer

#Include "../splib/system.inc"
#Include "../splib/string.inc"
#Include "../splib/vt100.inc"
#Include "../memory.inc"
#Include "../dict.inc"
#Include "../zstring.inc"
#Include "../console.inc"
#Include "../util.inc"

Dim a, s$, x

Cls

mem_init(Mm.Info(Path) + "../../stories/minizork.z3")
di_init()

Open Mm.Info(Path) + "minizork.dic" For Input As #1
Timer = 0
Do While Not Eof(#1)
  Line Input #1, s$
  a = Val(Left$(s$, 6))
  s$ = Mid$(s$, 8)
  x = di_lookup(s$)
  If x = a Then
    con.println(str.rpad$(s$, 8) + "=> " + Str$(x) + " - OK")
  Else
    con.println(str.rpad$(s$, 8) + "=> " + Str$(x) + " - ERROR")
  EndIf
Loop
con.println("Dictionary test took " + Str$(Timer) + " ms")
