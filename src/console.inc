' Copyright (c) 2020 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

'Dim BUSY$(1) LENGTH 16
'BUSY$(0) = "\\\\||||////----"

Dim script = 0 ' bit 1 is set for writing (via #2),
               ' bit 2 is set for reading (via #3)

Const S_WRITE = &b01
Const S_READ  = &b10

' @param  p$  prompt
' @param  r   if 1 then write input to the script
Function cin$(p$, r)
  cout(p$)

  ' If configured read input from the script
  If script And S_READ Then
    Line Input #3, cin$
    If cin$ = "" Then
      script = script And S_WRITE
      Close #3
    Else
      cout(cin$) : endl()
    EndIf
  EndIf

  ' Otherwise read input from the console
  If Not(script And S_READ) Then Line Input cin$

  ' If configured write input to the script
  If (r = 1) And (script AND S_WRITE) And (cin$ <> "") Then Print #2, cin$
End Function

Sub cout(s$)
  Print s$;
End Sub

Sub endl()
  Print
End Sub
