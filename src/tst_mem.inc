' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Sub tst_mem()
  Local ad, buf$, buf_sz, i, x

  Print "Executing memory tests"

  Open FILE$ For Random As #2

  Print "Testing sequential access:"
  Timer = 0
  Do While ad < FILE_LEN
    Print ".";
    Seek #2, ad + 1
    buf$ = Input$(255, #2)
    buf_sz = Len(buf$)
    Print buf_sz
    For i = 1 To buf_sz
      If ad = FILE_LEN Then Print "What the hell!"
      If Peek(Var buf$, i) <> rb(ad) Then Error
      ad = ad + 1
    Next i
  Loop
  Print
  Print "Time taken ="; Timer; " ms"

  Print "Testing random access:"
  Timer = 0
  For i = 1 To 5000
    If i Mod 50 = 0 Then Print ".";
    ad = Fix(Rnd * FILE_LEN)
    Seek #2, ad + 1
    buf$ = Input$(1, #2)
    If Peek(Var buf$, 1) <> rb(ad) Then Error
  Next i
  Print
  Print "Time taken ="; Timer; " ms"

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

  Close #2
End Sub
