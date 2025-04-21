' Copyright (c) 2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

' Implementation of script record and replay.

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.requires("console")
sys.requires("file")
sys.provides("script")
If sys.err$ <> "" Then Error sys.err$
'!endif

Const script.FD_IN% = 2
Const script.FD_OUT% = 3

Sub script.record_on()
  Local f$, s$

  If con.fd_out <> 0 Then con.println("Already recording script.") : Exit Sub
  If con.fd_in <> 0  Then con.println("Cannot record whilst replaying script.") : Exit Sub

  con.println()
  con.println("Select script to record:")
  con.println()
  Local script% = script.select_script%()
  If script% <> 0 Then f$ = script.script_file$(script%)

  If script% <> 0 And file.exists%(f$) Then
    s$ = con.in$("Overwrite script " + Str$(script%) + " [y|N]? ")
    If LCase$(s$) <> "y" Then script% = 0
  EndIf

  If script% <> 0 Then
    s$ = con.in$("Script name ? ")
    If s$ = "" Then script% = 0
  EndIf

  If script% = 0 Then con.println("Cancelled.") : Exit Sub

  con.println("Recording to '" + f$ + "' ...")

  If file.mkdir%(file.get_parent$(f$)) <> sys.SUCCESS Then Error sys.err$

  Const fd_out% = script.FD_OUT%
  con.open_out(fd_out%, f$)
  Print #fd_out%, "# " Date$ " " Time$
  Print #fd_out%, "# " s$

End Sub

' Prompts the user to select a script slot.
Function script.select_script%()
  Local i, f$, s$
  Const fd% = script.FD_IN%

  For i = 1 To 10
    f$ = script.script_file$(i)
    con.print("  [" + Format$(i, "%2g") + "] ")
    If file.exists%(f$) Then
      Open f$ For Input As #fd%
      Line Input #fd%, s$ ' date/time
      con.print(Mid$(s$, 3) + " - ")
      Line Input #fd%, s$ ' script name
      con.println(Mid$(s$, 3))
      Close #fd%
    Else
      con.println("Empty")
    EndIf
  Next i

  con.println()
  s$ = con.in$("Script number ? ")
  Local script% = Val(s$)
  If script% < 1 Or script% > 10 Then script% = 0
  script.select_script% = script%

End Function

' Gets the path to the file corresponding to script slot 'i'.
Function script.script_file$(i)
  If i < 1 Or i > 10 Then Error "Invalid script number"
  Const name$ = ss$(STORY_FILE) + "_" + Str$(i) + ".scr"
  script.script_file$ = file.resolve$(ss$(SCRIPT_DIR), ss$(STORY_FILE))
  script.script_file$ = file.resolve$(script.script_file$, name$)
End Function

Sub script.record_off()
  If con.fd_out = 0 Then con.println("A script is not being recorded!") : Exit Sub
  con.close_out()
  con.println("Recording stopped.")
End Sub

Sub script.replay_on()
  If con.fd_out <> 0 Then con.println("Cannot replay whilst recording script.") : Exit Sub
  If con.fd_in <> 0  Then con.println("Already replaying script.") : Exit Sub

  con.println()
  con.println("Select script to replay:")
  con.println()
  Local script% = script.select_script%()
  If script% <> 0 Then
    Local f$ = script.script_file$(script%)
    If Not file.exists%(f$) Then script% = 0
  EndIf

  If script% = 0 Then con.println("Cancelled.") : Exit Sub

  con.println("Replaying from '" + f$ + "' ...")

  con.open_in(script.FD_IN%, f$)
End Sub

Sub script.replay_off()
  If con.fd_in = 0 Then con.println("A script is not being replayed!") : Exit Sub
  con.close_in()
  con.println("Replaying stopped.")
End Sub
