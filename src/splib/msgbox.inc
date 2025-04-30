' Copyright (c) 2023-2024 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.requires("ctrl", "string")
sys.provides("msgbox")
If sys.err$ <> "" Then Error sys.err$
'!endif

Const msgbox.NO_PAGES = &h01
Dim msgbox.buffer% = 64

Function msgbox.show%(x%, y%, w%, h%, msg$, buttons$(), default%, ctrl$, fg%, bg%, frame%, flags%)
  Const base% = Mm.Info(Option Base), num% = Bound(buttons$(), 1) - base% + 1
  Local i%, btn_x%(num%), p% = 1
  btn_x%(base%) = x% + 2
  For i% = base% + 1 To base% + num% - 1
    btn_x%(i%) = btn_x%(i% - 1) + Len(buttons$(i% - 1)) + 5
  Next

  ' Backup display.
  Const fh% = Mm.Info(FontHeight), fw% = Mm.Info(FontWidth)
  Blit Read msgbox.buffer%, x% * fw%, y% * fh%, w% * fw%, h% * fh%

  msgbox.box(x%, y%, w%, h%, 1, Choice(frame% = -1, fg%, frame%), bg%)
  i% = y% + 2
  Do While p% <= Len(msg$)
    msgbox.print_at(x% + 2, i%, str.wwrap$(msg$, p%, w% - 4), fg%, bg%)
    Inc i%
  Loop

  Local key%, released%, valid% = 1
  msgbox.show% = default%
  Do
    If sys.break_flag% Then msgbox.show% = default% : Exit Function
    If valid% Then
      For i% = base% To base% + num% - 1
        msgbox.button(btn_x%(i%), y% + h% - 4, buttons$(i%), i% = msgbox.show%, fg%, bg%)
      Next
      If Not flags% And msgbox.NO_PAGES Then
        If InStr(Mm.Device$, "PicoMite") Then FrameBuffer Copy F, N, B Else Page Copy 1 To 0, B
      EndIf
      valid% = 0
    EndIf
    Call ctrl$, key%
    If Not key% Then keys_cursor_ext(key%)
    If Not key% Then released% = 1 : Continue Do
    If Not released% Then key% = 0 : Continue Do
    valid% = 0
    Select Case key%
      Case ctrl.A, ctrl.SELECT
        key% = ctrl.SELECT
        valid% = 1
      Case ctrl.LEFT
        If msgbox.show% > 0 Then Inc msgbox.show%, -1 : valid% =1
      Case ctrl.RIGHT
        If msgbox.show% < num% - 1 Then Inc msgbox.show% : valid% =1
    End Select
    msgbox.beep(valid%)
    Pause ctrl.UI_DELAY - 100
  Loop Until key% = ctrl.SELECT

  ctrl.wait_until_idle(ctrl$, "keys_cursor_ext")

  ' Restore display.
  Blit Write msgbox.buffer%, x% * fw%, y% * fh%
  Blit Close msgbox.buffer%
  If Not flags% And msgbox.NO_PAGES Then
    If InStr(Mm.Device$, "PicoMite") Then FrameBuffer Copy F, N, B Else Page Copy 1 To 0, B
  EndIf
End Function

Sub msgbox.button(x%, y%, txt$, selected%, fg%, bg%)
  msgbox.box(x%, y%, Len(txt$) + 4, 3, 0, fg%, -1)
  Const fg_% = Choice(selected%, bg%, fg%)
  Const bg_% = Choice(selected%, fg%, bg%)
  msgbox.print_at(x% + 2, y% + 1, txt$, fg_%, bg_%)
End Sub

Sub msgbox.box(x%, y%, w%, h%, dbl%, fg%, bg%)
  Const fh% = Mm.Info(FontHeight), fw% = Mm.Info(FontWidth)
  Local d% = fw% \ 2
  If bg% >= 0 Then Box x% * fw%, y% * fh%, w% * fw%, h% * fh%, , bg%, bg%
  Box x% * fw% + d%, y% * fh% + d%, w% * fw% - 2 * d%, h% * fh% - 2 * d%, 1, fg%
  Inc d%, d%
  If dbl% Then Box x% * fw% + d%, y% * fh% + d%, w% * fw% - 2 * d%, h% * fh% - 2 * d%, 1, fg%
End Sub

Sub msgbox.print_at(x%, y%, s$, fg%, bg%)
  Text x% * Mm.Info(FontWidth), y% * Mm.Info(FontHeight), s$, , , , fg%, bg%
End Sub

Sub msgbox.beep(valid%)
  ' These are the same frequencies as for the sound.BLART and sound.SELECT effects.
  If valid% Then
    ' Local notes!(3) = (493.88, 783.99, 987.77, 0.0) ' B4,G5,B5,-
    Local notes!(3) = (987.77, 1567.98, 1975.53, 30.87) ' B5,G6,B6,-
  Else
    ' Local notes!(3) = (523.25, 493.88, 369.99, 349.23) ' C5,B4,F#4,F4
    Local notes!(4) = (1046.50, 987.77, 739.99, 698.46, 30.87) ' C6,B5,F#5,F5,-
  EndIf
  Play Stop
  Pause 10 ' The PAUSE helps to suppress an (MMB4L specific?) audio glitch.
  Local i%
  For i% = Bound(notes!(), 0) To Bound(notes!(), 1)
    If notes!(i%) > 16.0 Then Play Sound 4, B, S, notes!(i%), 25
    Pause 40
  Next
  Play Stop
End Sub
