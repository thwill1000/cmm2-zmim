' Copyright (c) 2020 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Function cin$()
  ' Read input from a script
  If script And S_READ Then
    Line Input #3, cin$
    If cin$ = "" Then
      script = script And S_WRITE
    Else
      cout(cin$) : endl()
    EndIf
  EndIf

  ' Otherwise read input from the console
  If Not(script And S_READ) Then Line Input cin$
End Function

Sub cout(s$)
  Print s$;
End Sub

Sub endl()
  Print
End Sub
