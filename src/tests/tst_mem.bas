' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Option Explicit On
Option Default Integer

'#Include "../mem_cmm2_fast.inc"
#Include "../mem_cmm2_safe.inc"
#Include "../console.inc"

Dim ad, buf$, buf_sz, file$, i, x

Cls

file$ = "/zmim/stories/minizork.z3"
mem_init(file$)

endl()
cout("Executing memory tests") : endl()

Open file$ For Random As #1

endl()
cout("Testing sequential access:") : endl()
Timer = 0
Do While ad < FILE_LEN
  cout(".") : cflush()
  Seek #1, ad + 1
  buf$ = Input$(255, #1)
  buf_sz = Len(buf$)
  For i = 1 To buf_sz
    If ad = FILE_LEN Then cout("What the hell!") : endl()
    If Peek(Var buf$, i) <> rb(ad) Then Error
    ad = ad + 1
  Next i
Loop
endl()
cout("Time taken = " + Str$(Timer) + " ms") : endl()

endl()
cout("Testing random access:") : endl()
Timer = 0
For i = 1 To 5000
  If i Mod 50 = 0 Then cout(".") : cflush()
  ad = Fix(Rnd * FILE_LEN)
  Seek #1, ad + 1
  buf$ = Input$(1, #1)
  If Peek(Var buf$, 1) <> rb(ad) Then Error
Next i
endl()
cout("Time taken = " + Str$(Timer) + " ms") : endl()

endl()
cout("Test read/write:") : endl()
Timer = 0
For i = 1 To 5000
  If i Mod 50 = 0 Then cout(".") : cflush()
  ad = Fix(Rnd * BASE_STATIC)
  x = Fix(Rnd * 255)
  wb(ad, x)
  If x <> rb(ad) Then Error
Next i
endl()
cout("Time taken = " + Str$(Timer) + " ms") : endl()

Close #1
