' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.requires("vt100")
sys.provides("console")
If sys.err$ <> "" Then Error sys.err$
'!endif

Const con.SCREEN% = &h01
Const con.SERIAL% = &h02

Dim con.HEIGHT = 50
Dim con.WIDTH = 100

Dim con.buf$     ' console write buffer
Dim con.space    ' was the last character written to con.buf$ a space?
Dim con.lines    ' number of lines that have been output since the last prompt
Dim con.count    ' number of newlines without an intervening character being printed
Dim con.x = 1    ' current horizontal cursor position, indexed from 1
Dim con.fd_in    ' file descriptor to read input from
Dim con.fd_out   ' file descriptor to echo input to
Dim con.more = 1 ' show the [MORE] prompt if more than a page of output
                 ' (approx con.HEIGHT lines) is written without a prompt
Dim con.history%(255)   ' history buffer for user input.
Dim con.spin_enabled    ' is the progress spinner enabled ?
Dim con.spin_shown      ' is the progress spinner currently displayed ?
Dim con.disable_flush%  ' set 1 to temporarily disable flushing the console write buffer, buf$.

Sub con.init(width%, height%, spin_enabled%)
  con.WIDTH = width%
  con.HEIGHT = height%
  con.spin_enabled = spin_enabled%
End Sub

' @param  p$  prompt
Function con.in$(p$, echo)
  Local s$

  con.print(p$)
  con.flush()

  If con.fd_in Then
    Do While s$ = "" And Not Eof(con.fd_in)
      Line Input #con.fd_in, s$
      ' Ignore comments and *record commands.
      If InStr(s$, "#") = 1 Or InStr(LCase$(s$), "*record") = 1 Then s$ = ""
      If s$ <> "" Then con.history_put(con.history%(), s$)
    Loop
    If Eof(con.fd_in) And s$ = "" Then s$ = "*replay off"
  Else
    s$ = con.readln$("", con.WIDTH - con.x - Len(p$), con.history%())
  EndIf

  ' If we read from a file then echo what we read.
  If con.fd_in Then con.println(s$)

  ' If we read from the console then clear con.lines.
  If Not con.fd_in Then con.lines = 0 : con.count = 1

  con.x = 1

  If echo Then
    If con.fd_out Then
      If s$ <> "" Then Print #con.fd_out, s$
    EndIf
  EndIf

  con.in$ = s$
End Function

' Outputs s$.
'
' s$ is appended to con.buf$ one character at a time. Only when we switch from
' non-spaces to spaces or vice versa do we flush the contents of con.buf$ to the
' console. As a result calling this function will probably not print the
' entirety of s$; ' the last 'word' or part thereof may be left in con.buf$.
Sub con.print(s$)
  Local c$, i

  For i = 1 To Len(s$)
    c$ = Mid$(s$, i, 1)
    Select Case c$
      Case Chr$(13) : ' CR
        ' Ignore carriage returns

      Case Chr$(10) : ' LF
        con.endl()
        con.space = 0

      Case " " :
        If Not con.space Then con.flush() : con.space = 1
        Cat con.buf$, " "
        con.count = 0

      Case "`" :
        If con.space Then con.flush() : con.space = 0
        Cat con.buf$, "'"
        con.count = 0

      Case Else :
        If con.space Then con.flush() : con.space = 0
        Cat con.buf$, c$
        con.count = 0

    End Select
  Next
End Sub

' Writes con.buf$ to the console and clears it.
'
' If con.buf$ does not fit on the current line then first write CRLF and then if
' con.buf$ contains non-spaces write con.buf$ to the console.
Sub con.flush()
  If con.disable_flush% Then Exit Sub

  If con.spin_shown Then Print Chr$(8); " "; Chr$(8); : con.spin_shown = 0

  ' If the buffer is wider than the console then split the buffer.
  Local remainder$
  If Len(con.buf$) > con.WIDTH Then
    remainder$ = "..." + Mid$(con.buf$, con.WIDTH + 1)
    con.buf$ = Left$(con.buf$, con.WIDTH)
  EndIf

  Do
    If con.x = 1 And con.lines > con.HEIGHT - 2 Then
      If con.more Then
        Print "[MORE] ";
        Do While Inkey$ <> "" : Loop
        Do While Inkey$ = "" : Pause 1 : Loop
        Print
      EndIf
      con.lines = 0
    EndIf

    If con.x + Len(con.buf$) > con.WIDTH + 1 Then
      Print
      Inc con.lines
      con.x = 1
      If con.space Then con.buf$ = "" : Exit Do
    Else
      Print con.buf$;
      Inc con.x, Len(con.buf$)
      con.buf$ = ""
      Exit Do
    EndIf
  Loop

  If Len(remainder$) Then
    con.buf$ = remainder$
    con.flush()
  EndIf
End Sub

' Flushes con.buf$ to the console and then writes CRLF.
Sub con.endl()
  con.flush()

  If con.count < 0 Then
    Exit Sub
  ElseIf con.count >= 10 Then
    ' If the story outputs 10 or more empty lines then we assume it was trying
    ' to clear the screen
    Local i
    For i = 0 To con.HEIGHT - con.count - 1 : Print : Next
    con.count = -999 ' to avoid any unnecessary additional empty lines
    con.lines = 0
    Exit Sub
  EndIf

  Print
  Inc con.count
  Inc con.lines
  con.x = 1
End Sub

Sub con.println(s$, center)

  If Len(s$) > 0 Then
    If center Then
      If con.x <> 1 Then Error "Cannot center text unless on a new line."
      If Len(s$) < con.WIDTH Then con.print(Space$((con.WIDTH - Len(s$)) \ 2) + s$)
    Else
      con.print(s$)
    EndIf
  EndIf

  con.endl()
End Sub

' Echos the contents of a (hopefully) text file to the console.
Sub con.print_file(f$, center)
  Local s$, w

  If center Then
    Open f$ For Input As #1
    Do
      Line Input #1, s$
      w = Max(w, Len(s$))
    Loop While Not Eof(#1)
    Close #1
  EndIf

  Open f$ For Input As #1
  Do
    Line Input #1, s$
    If center Then
      con.println(s$ + Space$(w - Len(s$)), center)
    Else
      con.println(s$)
    EndIf
  Loop While Not Eof(#1)
  Close #1
End Sub

Sub con.open_in(fd, f$)
  con.close_in()
  Open f$ For Input As #fd
  con.fd_in = fd
End Sub

Sub con.close_in()
  If con.fd_in Then Close #con.fd_in
  con.fd_in = 0
End Sub

Sub con.open_out(fd, f$)
  con.close_out()
  Open f$ For Output As #fd
  con.fd_out = fd
End Sub

Sub con.close_out()
  If con.fd_out Then Close #con.fd_out
  con.fd_out = 0
End Sub

Sub con.close_all()
  con.close_in()
  con.close_out()
End Sub

Sub con.foreground(c$)
  Local type% = con.get_type%()
  If type% And con.SCREEN% Then
    Local scol%
    Select Case LCase$(c$)
      Case "green" : scol% = RGB(Green)
      Case Else    : scol% = RGB(White)
    End Select
    Colour scol%
  EndIf
  If type% And con.SERIAL% Then
    con.set_type(con.SERIAL%)
    Print vt100.colour$(c$);
    con.set_type(type%)
  EndIf
End Sub

Sub con.bell()
  Local type% = con.get_type%()
  If type% And con.SCREEN% Then Play Tone 329.63, 329.63, 100 ' Note E4
  If type% And con.SERIAL% Then
    con.set_type(con.SERIAL%)
    Print Chr$(7);
    con.set_type(type%)
  EndIf
End Sub

' Gets the console type.
' Note that in many cases you don't want to test the return value using equals
' '=' but instead using AND.
' e.g. to test whether the type supports:
'  - serial console do: IF con.get_type%() AND con.SERIAL% THEN
'  - screen console do: IF con.get_type%() AND con.SCREEN% THEN
Function con.get_type%()
  Select Case LCase$(Mm.Info(Option Console))
    Case "both"   : con.get_type% = con.SCREEN% Or con.SERIAL%
    Case "screen" : con.get_type% = con.SCREEN%
    Case "serial" : con.get_type% = con.SERIAL%
    Case Else     : Error "Unknown console type"
  End Select
End Function

Sub con.set_type(type%)
  Select Case type%
    Case con.SCREEN% : Option Console Screen
    Case con.SERIAL%
      ' Note MMB4W cannot call OPTION CONSOLE SERIAL if it is on the same line
      ' as a CASE clause. This is because in MMB4W this OPTION checks
      ' 'CurrentLinePtr' to make sure it is only being used from within a
      ' program. However due to a long-standing and gnarly bug in the MMBasic
      ' core 'CurrentLinePtr' is not correctly set when processing a command on
      ' the same line as a CASE clause.
      Option Console Serial
    Case &h03 : Option Console Both
    Case Else
      Error "Unknown console type"
  End Select
End Sub

' Gets a string element from the command history.
'
' @param  h%()  buffer containing the command history.
' @param  idx%  index of the element to retrieve, the first element is always
'               idx% = 0 irrespective of OPTION BASE.
' @return       string element, or empty string if idx% >= number of elements.
Function con.history_get$(h%(), idx%)
  If idx% < 0 Then Error "index out of bounds: " + Str$(idx%)

  Local h_addr% = Peek(VarAddr h%())
  Local h_size% = (Bound(h%(), 1) - Bound(h%(), 0) + 1) * 8
  Local i%, p% = h_addr%
  For i% = 0 To idx%
    If Peek(Byte p%) = 0 Then Exit Function ' Return empty string.
    If i% < idx% Then Inc p%, Peek(Byte p%) + 1
  Next
  If p% + Peek(Byte p%) < h_addr% + h_size% Then
    Memory Copy p%, Peek(VarAddr con.history_get$), Peek(Byte p%) + 1
  EndIf
End Function

Sub con.history_fill(h%(), items$())
  Local h_addr% = Peek(VarAddr h%())
  Local h_size% = (Bound(h%(), 1) - Bound(h%(), 0) + 1) * 8
  Memory Set h_addr%, &h00, h_size%
  Local i%
  For i% = Bound(items$(), 1) To Bound(items$(), 0) Step -1
    con.history_put(h%(), items$(i%))
  Next
End Sub

Function con.history_find%(h%(), needle$)
  If needle$ = "" Then Error "invalid argument: needle$"

  ' TODO: could be faster by not calling con.history_get()
  Local i% = -1, s$
  Do
    Inc i%
    s$ = con.history_get$(h%(), i%)
  Loop Until s$ = "" Or s$ = needle$

  con.history_find% = Choice(s$ = "", -1, i%)
End Function

Sub con.history_put(h%(), s$)
  If s$ = "" Then Error "invalid empty string"

  Local h_addr% = Peek(VarAddr h%())
  Local h_size% = (Bound(h%(), 1) - Bound(h%(), 0) + 1) * 8
  If Peek(Byte h_addr%) > 0 Then
    ' Can't use MEMORY COPY because the CMM2 does not handle case where src and dst overlap.
    ' Memory Copy h_addr%, h_addr% + 1 + Len(s$), h_size% - 1 - Len(s$)
    Local dst% = h_addr% + h_size% - 1
    Local src% = dst% - Len(s$) - 1
    Do While src% >= h_addr%
      Poke Byte dst%, Peek(Byte src%)
      Inc dst%, -1
      Inc src%, -1
    Loop
  EndIf
  Memory Copy Peek(VarAddr s$), h_addr%, Len(s$) + 1
End Sub

Function con.readln$(initial$, max_len%, history%())
  Local ch$, hidx%, p%, old$, overwrite%, s$
  Local max_len_% = Choice(max_len% < 1 Or max_len% > 255, 255, max_len%)

  con.readln$ = initial$
  p% = Len(initial$) + 1
  Print initial$;
  hidx% = Choice(initial$ = "", -1, con.history_find%(history%(), initial$))

  Local show% = 1
  Local t% = Timer + 500

  Do While Inkey$ <>  "" : Loop ' Discard anything else in the keyboard buffer.

  Do While Not sys.break_flag%
    If Timer > t% Then show% = Not show% : t% = Timer + 500
    con.show_cursor(show%)

    ch$ = Inkey$
    If ch$ = "" Then Pause 1 : Continue Do
    con.show_cursor(0)

    Select Case Asc(ch$)
      Case 8 ' Backspace
        If p% = 1 Then
          con.bell()
        Else
          ' Delete the character immediately to the left.
          s$ = Mid$(con.readln$, p%)
          con.readln$ = Left$(con.readln$, p% - 2) + s$
          Print Chr$(&h08) s$ " " String$(Len(s$) + 1, &h08);
          Inc p%, -1
        EndIf

      Case 10, 13 ' Enter
        Print
        If con.readln$ <> "" Then con.history_put(history%(), con.readln$)
        Exit Do

      Case 127 ' Delete
        If p% = Len(con.readln$) + 1 Then
          con.bell()
        Else
          ' Delete the current character.
          s$ = Mid$(con.readln$, p% + 1)
          con.readln$ = Left$(con.readln$, p% - 1) + s$
          Print s$ " " String$(Len(s$) + 1, &h08);
        EndIf

      Case 128, 129 ' Up, Down
        If hidx% = -1 Then old$ = con.readln$
        Inc hidx%, Choice(Asc(ch$) = 128, 1, -1)
        If hidx% <= -1 Then
          hidx% = -1
          s$ = old$
        Else
          s$ = con.history_get$(history%(), hidx%)
          If s$ = "" Then s$ = con.readln$ : Inc hidx%, -1
        EndIf

        Print String$(p% - 1, &h08) String$(p% - 1, " ") String$(p% - 1, &h08) s$;
        con.readln$ = s$
        p% = Len(s$) + 1

      Case 130 ' Left
        If p% > 1 Then
          Inc p%, -1
          Print Chr$(&h08);
        EndIf

      Case 131 ' Right
        If p% <= Len(con.readln$) Then
          Print Mid$(con.readln$, p%, 1);
          Inc p%
        EndIf

      Case 132 ' Insert
        overwrite% = Not overwrite%

      Case 134 ' Home
        Print String$(p% - 1, &h08);
        p% = 1

      Case 135 ' End
        Do While p% <= Len(con.readln$)
          Print Mid$(con.readln$, p%, 1);
          Inc p%
        Loop

      Case < 32, > 126
        ' Print "<" Str$(Asc(ch$)) ">";
        ' con.bell()

      Case Else
        If p% > Len(con.readln$) Then
          ' Append to end of line.
          If Len(con.readln$) = max_len_% Then
            con.bell()
          Else
            Cat con.readln$, ch$
            Print ch$;
            Inc p%
          EndIf
        ElseIf overwrite% Then
          ' Overwrite.
          con.readln$ = Left$(con.readln$, p% - 1) + ch$ + Mid$(con.readln$, p% + 1)
          Print ch$;
          Inc p%
        Else
          ' Insert.
          If Len(con.readln$) = max_len_% Then
            con.bell()
          Else
            s$ = Mid$(con.readln$, p%)
            con.readln$ = Left$(con.readln$, p% - 1) + ch$ + s$
            Print ch$ s$ String$(Len(s$), &h08);
            Inc p%
          EndIf
        EndIf
    End Select
  Loop

  If sys.break_flag% Then con.readln$ = ""
End Function

' Hide/show cursor on the VGA/LCD screen.
Sub con.show_cursor(show%)
  If con.get_type%() And con.SCREEN% Then
    Local x% = Mm.Info(HPos), y% = Mm.Info(VPos) + Mm.Info(FontHeight) - 1
    Line x%, y%, x% + Mm.Info(FontWidth), y%, 1, Choice(show%, Mm.Info(FColour), Mm.Info(BColour))
  EndIf
End Sub

' Show the progress spinner.
Sub con.spin()
  Static i
'!if !defined(NO_EXTRA_CHECKS)
  If Not con.spin_enabled Then Error "Invalid progress spinner state"
'!endif
  If con.x < con.WIDTH - 1 Then
    If con.spin_shown Then Print Chr$(8); Else con.spin_shown = 1
    Print Mid$("\|/-", i + 1, 1);
    i = (i + 1) Mod 4
  EndIf
End Sub

' Sets the font and updates the console width and height.
Sub con.set_font(size%)
  If size% = Mm.Info(Font) Then Exit Sub
  con.endl()
  Font size%
  con.WIDTH = Mm.HRes \ Mm.Info(FontWidth)
  con.HEIGHT = Mm.VRes \ Mm.Info(FontHeight)
  wb(&h20, con.HEIGHT)
  wb(&h21, con.WIDTH)
  If Mm.Info(Device X) = "MMB4L" Then Console Resize con.WIDTH, con.HEIGHT
  Local x%, y%
  con.get_pos(x%, y%)
  Print @(x% * Mm.Info(FontWidth), (y% - 1) * Mm.Info(FontHeight))
End Sub

Sub con.get_pos(x%, y%)
  con.flush()
  x% = Mm.Info(HPos) \ Mm.Info(FontWidth)
  y% = Mm.Info(VPos) \ Mm.Info(FontHeight)
  if x% <> con.x - 1 Then Error "Mismatched x-values"
End Sub

Sub con.set_pos(x%, y%)
  con.flush()
  Print @(x% * Mm.Info(FontWidth), y% * Mm.Info(FontHeight));
  con.x = x% + 1
End Sub

Sub con.invert()
  Static inverted = 0
  inverted = Not inverted
  If con.get_type%() = con.SERIAL% Then
    Print vt100$(Choice(inverted, "7m", "0m"));
  Else
    Const bg% = Mm.Info(BColour)
    Const fg% = Mm.Info(FColour)
    Colour bg%, fg%
  EndIf
End Sub
