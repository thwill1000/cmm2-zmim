' Copyright (c) 2023-2024 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.provides("game")
If sys.err$ <> "" Then Error sys.err$
'!endif

' Common code for cleaning up and returning to any shell program after a game
' ends. Note that it is possible that much of this is (now) unnecessary due to
' improvements in how MMBasic handles the cleanup itself on calling END or RUN.
'
' @param  break%  If 0 then this is a "normal" end, if 1 then it is the result
'                 of Ctrl-C. In the current version of this subroutine this
'                 does not have an effect on behaviour.
Sub game.end(break%)
  If InStr(Mm.Device$, "PicoMite") Then
    FrameBuffer Write N
    FrameBuffer Close
  Else
    Page Write 0
  EndIf
  Colour Rgb(White), Rgb(Black)
  If Mm.HRes = 320 Then Font 7 Else Font 1
  Cls

  sys.restore_break()

  ' Use ON ERROR SKIP because we might not be using these libraries.
  On Error Skip : sound.term()
  On Error Skip : ctrl.term()

  SetTick 0,0,1 : SetTick 0,0,2 : SetTick 0,0,3 : SetTick 0,0,4
  Play Stop

  ' For the moment always return to shell/menu if available.
  break% = 0

  Local msg$
  If break% Then
    msg$ = "Exited due to Ctrl-C"
  ElseIf InStr(Mm.CmdLine$, "--shell") Then
    msg$ = "Loading menu ..."
  EndIf

  If msg$ <> "" Then
    Text Mm.HRes / 2, Mm.VRes / 2, msg$, CM
    If Mm.Info$(Device X) = "MMB4L" Then Pause 2000
  EndIf

  ' TODO: twm.term() should subsume twm.free() and also do this.
  On Error Skip : twm.enable_cursor(1)

  If Not break% And InStr(Mm.CmdLine$, "--shell") Then sys.run_shell()
  End
End Sub

Sub game.init_window(title$, version%, title_suffix$)
  If Mm.Info$(Device X) = "MMB4L" Then
    Local s$ = title$
    If version% Then Cat s$, ", v" + sys.format_version$(version%)
    Cat s$, title_suffix$
    Graphics Title 0, s$
    Graphics Interrupt 0, game.on_window_event
  EndIf
End Sub

Sub game.on_window_event(window_id%, event_id%)
  If event_id% = WINDOW_EVENT_CLOSE Then game.end(0)
End Sub

Sub game.on_break()
  game.end(1)
End Sub

' @return  Full path to file to score high-score in, or empty-string if it cannot be determined.
Function game.highscore_file$()
  Const s$ = Mm.Info(Current)
  If s$ <> "" Then
    Local p%
    For p% = Len(s$) To 0 Step -1
      If Mid$(s$, p%, 1) = "." Then Exit For
    Next
    If p% > 0 Then
      game.highscore_file$ = Mid$(s$, 1, p% - 1) + ".hsc"
    EndIf
  EndIf
End Function

' Reads highscores from file.
'
' @param  s$()   On exit this array is filled with the highscores as strings.
'                Will only read enough highscores to fill this array,
'                any excess array entries will be unchanged.
' @param  fnbr%  Optional file number, defaults to 9.
Sub game.highscore_read(s$(), fnbr%)
  Const _fnbr% = Choice(fnbr%, fnbr%, 9)
  Const f$ = game.highscore_file$()
  If f$ = "" Then Exit Sub
  If Not Mm.Info(Exists File f$) Then Exit Sub
  Open f$ For Input As _fnbr%
  Local i%
  For i% = Bound(s$(), 0) To Bound(s$(), 1)
    If Eof(_fnbr%) Then Exit For
    Line Input #_fnbr%, s$(i%)
  Next
  Close _fnbr%
End Sub

' Writes highscores to file.
'
' @param  s$()   Array of highscores as strings.
' @param  fnbr%  Optional file number, defaults to 9.
Sub game.highscore_write(s$(), fnbr%)
  Const _fnbr% = Choice(fnbr%, fnbr%, 9)
  Const f$ = game.highscore_file$()
  If f$ = "" Then Exit Sub
  Open f$ For Output As _fnbr%
  Local i%
  For i% = Bound(s$(), 0) To Bound(s$(), 1)
    Print #_fnbr%, s$(i%)
  Next
  Close _fnbr%
End Sub
