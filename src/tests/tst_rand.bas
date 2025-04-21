' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Option Explicit On
Option Default Integer

#Include "../splib/system.inc"
#Include "../splib/string.inc"
#Include "../splib/vt100.inc"
#Include "../execute.inc"
#Include "../console.inc"

Dim i, buckets(40), x, y

Cls

con.println("ex_random(100) = " + Str$(ex_random(100)))
con.println("ex_random(100) = " + Str$(ex_random(100)))
con.println("ex_random(0)   = " + Str$(ex_random(0)))
con.println("ex_random(-15) = " + Str$(ex_random(-15)))
con.println("ex_random(100) = " + Str$(ex_random(100)))
con.println("ex_random(100) = " + Str$(ex_random(100)))
con.println("ex_random(-15) = " + Str$(ex_random(-15)))
con.println("ex_random(100) = " + Str$(ex_random(100)))
con.println("ex_random(100) = " + Str$(ex_random(100)))

For i = 0 To 10000
  x = ex_random(40)
  buckets(x) = buckets(x) + 1
Next i

For i = 0 To 40
  con.println(Str$(i) + " => " + Str$(buckets(i)))
Next i

If buckets(0) <> 0 Then Error
For i = 1 To 40
  If buckets(i) <= 0 Then Error
Next i

' Test that the same number can be generated twice in a row
x = ex_random(20)
con.print(Str$(x))
y = 0
For i = 0 To 1000
  y = ex_random(20)
  con.print(" " + Str$(y))
  If x = y Then y = 1 : Exit For
  x = y
  y = 0
Next i
con.endl()
If Not(y) Then Error

