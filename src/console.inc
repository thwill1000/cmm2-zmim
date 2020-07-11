' Copyright (c) 2020 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Dim script   ' bit 1 is set for writing (via #2),
             ' bit 2 is set for reading (via #3)
Dim cbuf$    ' console write buffer
Dim cspace   ' was the last character written to cbuf$ a space?
Dim clines   ' number of lines that have been output since the last prompt
Dim cn_count ' number of consecutive calls to cendl() without an
             ' intervening call to cout()
Dim cn_x = 1 ' current horizontal cursor position, indexed from 1
Dim cn_spin  ' is the spinning cursor currently displayed?

'!comment_if INLINE_CONSTANTS
Const S_WRITE = &b01
Const S_READ  = &b10

Dim C_HEIGHT = 50
Dim C_WIDTH = 100
'!endif

' @param  p$  prompt
' @param  r   if 1 then write input to the script
Function cin$(p$, r)
  Local s$

  cout(p$)
  cflush()
  clines = 0

  ' If configured read input from the script
  If script And S_READ Then
    Line Input #3, s$
    If s$ = "" Then
      script = script And S_WRITE
      Close #3
    Else
      cout(s$) : endl()
    EndIf
  EndIf

  ' Otherwise read input from the console
  If Not(script And S_READ) Then Line Input s$ : cn_x = 1

  ' If configured write input to the script
  If (r = 1) And (script AND S_WRITE) And (s$ <> "") Then Print #2, s$

  cin$ = s$
End Function

' Outputs s$.
'
' s$ is appended to cbuf$ one character at a time. Only when we switch from
' non-spaces to spaces or vice versa do we flush the contents of cbuf$ to the
' console. As a result calling this function will probably not print the
' entirety of s$; ' the last 'word' or part thereof may be left in cbuf$.
Sub cout(s$)
  cn_count = 0

  If Len(s$) = 1 Then
    If (s$ = " ") Xor cspace Then cflush() : cspace = (s$ = " ")
    cbuf$ = cbuf$ + s$
  Else
    Local c$, i
    For i = 1 To Len(s$)
      c$ = Mid$(s$, i, 1)
      If (c$ = " ") Xor cspace Then cflush() : cspace = (c$ = " ")
      cbuf$ = cbuf$ + c$
    Next i
  EndIf
End Sub

' Writes cbuf$ to the console and clears it.
'
' If cbuf$ does not fit on the current line then first write CRLF and then if
' cbuf$ contains non-spaces write cbuf$ to the console.
Sub cflush()
  If cn_spin Then Print Chr$(8); " "; Chr$(8); : cn_spin = 0

  Do
    If cn_x = 1 And clines > C_HEIGHT - 2 Then
      Print "[MORE] ";
      Do While Inkey$ <> "" : Loop
      Do While Inkey$ = "" : Loop
      Print
      clines = 0
    EndIf

    If cn_x + Len(cbuf$) > C_WIDTH Then
      Print
      clines = clines + 1
      cn_x = 1
      If cspace Then cbuf$ = "" : Exit Sub
    Else
      Print cbuf$;
      cn_x = cn_x + Len(cbuf$)
      cbuf$ = ""
      Exit Sub
    EndIf
  Loop
End Sub

' Flushes cbuf$ to the console and then writes CRLF.
Sub endl()
  cflush()

  If cn_count < 0 Then
    Exit Sub
  ElseIf cn_count >= 10 Then
    ' If the story outputs 10 or more empty lines then we assume it was trying
    ' to clear the screen
    Local i
    For i = 0 To C_HEIGHT - cn_count - 1 : Print : Next i
    cn_count = -999 ' to avoid any unnecessary additional empty lines
    clines = 0
    Exit Sub
  EndIf

  Print
  cn_count = cn_count + 1
  clines = clines + 1
  cn_x = 1
End Sub

' Echos the contents of a (hopefully) text file to the console.
Sub cecho(f$)
  Local s$

  Open f$ For Input As #1
  Do
    Line Input #1, s$
    cout(s$)
    endl()
  Loop While Not Eof(#1)
  Close #1
End Sub
