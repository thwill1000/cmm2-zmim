' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Option Explicit On
Option Default Integer

#Include "../splib/system.inc"
#Include "../splib/string.inc"
#Include "../splib/vt100.inc"
#Include "../memory.inc"
#Include "../console.inc"

Dim ad, buf$, buf_sz, file$, i, x

Cls

file$ = Mm.Info(Path) + "../../stories/minizork.z3"
mem_init(file$)

con.endl()
con.println("Executing memory tests")

Open file$ For Random As #1

con.endl()
con.println("Testing sequential access:")
Timer = 0
Do While ad < FILE_LEN
  con.print(".") : con.flush()
  Seek #1, ad + 1
  buf$ = Input$(255, #1)
  buf_sz = Len(buf$)
  For i = 1 To buf_sz
    If ad = FILE_LEN Then con.println("What the hell!")
    If Peek(Var buf$, i) <> rb(ad) Then Error
    ad = ad + 1
  Next i
Loop
con.endl()
con.println("Time taken = " + Str$(Timer) + " ms")

con.endl()
con.println("Testing random access:")
Timer = 0
For i = 1 To 5000
  If i Mod 50 = 0 Then con.print(".") : con.flush()
  ad = Fix(Rnd * FILE_LEN)
  Seek #1, ad + 1
  buf$ = Input$(1, #1)
  If Peek(Var buf$, 1) <> rb(ad) Then Error
Next i
con.endl()
con.println("Time taken = " + Str$(Timer) + " ms")

con.endl()
con.println("Test read/write:")
Timer = 0
For i = 1 To 5000
  If i Mod 50 = 0 Then con.print(".") : con.flush()
  ad = Fix(Rnd * BASE_STATIC)
  x = Fix(Rnd * 255)
  wb(ad, x)
  If x <> rb(ad) Then Error
Next i
con.endl()
con.println("Time taken = " + Str$(Timer) + " ms")

Close #1
