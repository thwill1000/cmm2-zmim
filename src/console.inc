' Copyright (c) 2020 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

'Dim BUSY$(1) LENGTH 16
'BUSY$(0) = "\\\\||||////----"

Dim script = 0 ' bit 1 is set for writing (via #2),
               ' bit 2 is set for reading (via #3)

' Console write buffer
Dim cbuf$

' Was the last character written to cbuf$ a space?
Dim cspace

Const S_WRITE = &b01
Const S_READ  = &b10

Dim C_WIDTH = 80

' @param  p$  prompt
' @param  r   if 1 then write input to the script
Function cin$(p$, r)
  cout(p$)
  cflush()

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

' Outputs s$.
'
' Note that calling this function will probably not output the entirety of s$;
' the last 'word' or part thereof may be left in $cbuf.
Sub cout(s$)
  Local c, i
  For i = 1 To Len(s$)
    c = Peek(Var s$, i)

    ' If we have swtiched from non-spaces to spaces, or vice-versa then
    ' flush the current contents of cbuf$ to the console.
    If (c = &h20) Xor cspace Then cflush() : cspace = (c = &h20)

    cbuf$ = cbuf$ + Chr$(c)
  Next i
End Sub

' Writes cbuf$ to the console and clears it.
'
' If cbuf$ does not fit on the current line then first write CRLF and then if
' cbuf$ contains non-spaces write cbuf$ to the console.
Sub cflush()
  If Pos + Len(cbuf$) > C_WIDTH Then
    Print
    If Not cspace Then Print cbuf$;
  Else
    Print cbuf$;
  EndIf
  cbuf$ = ""
End Sub

' Flushes cbuf$ to the console and then writes CRLF.
Sub endl()
  cflush()
  Print
End Sub
