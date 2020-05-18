' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Sub dmp_abrv
  Local i
  For i = 0 To 95
    Print Str$(i); ": {";
    print_abrv(i)
    Print "}"
'    If (i + 1) Mod 20 = 0 Then more()
  Next i
End Sub
