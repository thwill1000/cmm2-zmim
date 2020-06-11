Option Explicit On
Option Default Integer

#Include "execute.inc"

Dim i, buckets(40), x, y

Cls

Print "ex_random(100) ="; ex_random(100)
Print "ex_random(100) ="; ex_random(100)
Print "ex_random(0)   ="; ex_random(0)
Print "ex_random(-15) ="; ex_random(-15)
Print "ex_random(100) ="; ex_random(100)
Print "ex_random(100) ="; ex_random(100)
Print "ex_random(-15) ="; ex_random(-15)
Print "ex_random(100) ="; ex_random(100)
Print "ex_random(100) ="; ex_random(100)

For i = 0 To 10000
  x = ex_random(40)
  buckets(x) = buckets(x) + 1
Next i

For i = 0 To 40
  Print Str$(i) + ":" + Str$(buckets(i))
Next i

If buckets(0) <> 0 Then Error
For i = 1 To 40
  If buckets(i) <= 0 Then Error
Next i

' Test that the same number can be generated twice in a row
x = ex_random(20)
Print x;
y = 0
For i = 0 To 1000
  y = ex_random(20)
  Print y;
  If x = y Then y = 1 : Exit For
  x = y
  y = 0
Next i
Print
If Not(y) Then Error

