' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Option Explicit On

#Include "memory_fast.inc"

Dim ad, buf$, buf_sz, file$, i, x

Cls

file$ = "A:/zmim/stories/minizork.z3"
mem_init(file$)

Print
Print "Executing memory tests"

Open file$ For Random As #1

Print
Print "Testing sequential access:"
Timer = 0
Do While ad < FILE_LEN
  Print ".";
  Seek #1, ad + 1
  buf$ = Input$(255, #1)
  buf_sz = Len(buf$)
  For i = 1 To buf_sz
    If ad = FILE_LEN Then Print "What the hell!"
    If Peek(Var buf$, i) <> rb(ad) Then Error
    ad = ad + 1
  Next i
Loop
Print
Print "Time taken ="; Timer; " ms"

Print
Print "Testing random access:"
Timer = 0
For i = 1 To 5000
  If i Mod 50 = 0 Then Print ".";
  ad = Fix(Rnd * FILE_LEN)
  Seek #1, ad + 1
  buf$ = Input$(1, #1)
  If Peek(Var buf$, 1) <> rb(ad) Then Error
Next i
Print
Print "Time taken ="; Timer; " ms"

Print
Print "Test read/write:"
Timer = 0
For i = 1 To 5000
  If i Mod 50 = 0 Then Print ".";
  ad = Fix(Rnd * BASE_STATIC)
  x = Fix(Rnd * 255)
  wb(ad, x)
  If x <> rb(ad) Then Error
Next i
Print
Print "Time taken ="; Timer; " ms"

Close #1
