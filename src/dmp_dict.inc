' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Sub dmp_dict
  Local a, col, i, n, sep$, sz, x

  Print "    **** Dictionary ****"
  Print

  a = rw(&h8) ' Dictionary address

  n = rb(a) : a = a + 1
  sep$ = Space$(n)
  For i = 1 To n : Poke Var sep$, i, rb(a) : a = a + 1 : Next i
  Print "  Word separators = "; Chr$(34); sep$; Chr$(34)

  sz = rb(a) : a = a + 1
  n = rw(a) : a = a + 2
  Print "  Word count ="; n; ", word size ="; sz
  Print

  For i = 1 To n
    Print "["; lpad$(Str$(i), 4, " "); "] ";

    x = 256 * rb(a) + rb(a + 1) : Print lpad$(Hex$(x), 4, "0");
    x = 256 * rb(a + 2) + rb(a + 3) : Print lpad$(Hex$(x), 4, "0"); " ";

    ' Read 4 bytes of text
    print_zstring(a)

    ' Skip (sz - 4) bytes of data
    a = a + sz - 4

    col = col + 1
    Print Chr$(13); Chr$(27); "[" ; Str$(col * 24); "C";
    If col = 3 Then col = 0 : Print
  Next i
End Sub
