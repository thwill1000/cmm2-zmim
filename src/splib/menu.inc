' Copyright (c) 2023-2024 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.requires("ctrl", "sound", "string", "txtwm")
sys.provides("menu")
If sys.err$ <> "" Then Error sys.err$
'!endif

Sub menu.init(ctrl$, callback$)
  Dim menu.OK_BTN$(1) Length 2 = ("OK", "")
  Dim YES_NO_BTNS$(1) Length 3 = ("Yes", "No")

  Dim menu.callback$ = callback$, menu.ctrl$ = ctrl$
  Dim menu.item_count%, menu.selection%

  Font 1
'!if defined(GAMEMITE)
  '!uncomment_if true
  ' Dim menu.height% = Mm.VRes \ Mm.Info(FontHeight)
  ' Dim menu.width% = Mm.HRes \ Mm.Info(FontWidth)
  '!endif
'!else
  If Mm.Device$ = "MMB4L" Then
    Dim menu.height% = Min(Mm.VRes, 50)
    Dim menu.width% = Min(Mm.HRes, 100)
  Else
    Dim menu.height% = Mm.VRes \ Mm.Info(FontHeight)
    Dim menu.width% = Mm.HRes \ Mm.Info(FontWidth)
  EndIf
'!endif
  Local sz%
  Inc sz%, 7 + 2 * menu.height% * menu.width%
  Inc sz%, 7 + 2 * (menu.height% - 10) * (menu.width% - 10)
  twm.init(2, sz%)
  Dim menu.win1% = twm.new_win%(0, 0, menu.width%, menu.height%)
  Dim menu.win2% = twm.new_win%(5, 5, menu.width% - 10, menu.height% - 10)
End Sub

Sub menu.term(msg$)
  sys.restore_break()
  twm.enable_cursor(1)
  Page Write 0
  Cls
  If Len(msg$) Then Text 160, 110, msg$, CM
End Sub

Sub menu.on_break()
  menu.term("Exited due to Ctrl-C")
  End
End Sub

Sub menu.load_data(data_label$)
  Read Save
  Restore data_label$
  menu.item_count% = 0
  Local item$
  Do
    Read item$
    If Not Len(item$) Then Exit Do
    Inc menu.item_count%
    ' Don't use FIELD$() here because we want to respect whitespace.
    item$ = str.decode$(Left$(item$, InStr(item$, "|") - 1))
  Loop

  On Error Skip
  Erase menu.items$()
  Dim menu.items$(menu.item_count% - 1) Length 128
  Local i%
  Restore data_label$
  For i% = 0 To menu.item_count% - 1
    Read menu.items$(i%)
    menu.items$(i%) = str.decode$(menu.items$(i%))
  Next
  Read Restore

  For menu.selection% = 0 To menu.item_count% - 1
    If Len(Field$(menu.items$(menu.selection%), 2, "|")) Then Exit For
  Next
End Sub

Sub menu.render(draw_box%)
  twm.switch(menu.win1%)
  twm.cls(1, 1, menu.width% - 2, menu.height% - 2)
  twm.foreground(twm.WHITE%)
  If draw_box% Then twm.box(0, 0, menu.width%, menu.height%)

  ' Items.
  Local i%
  For i% = 0 To menu.item_count% - 1
    menu.render_item(i%)
  Next

  If Len(menu.callback$) Then Call menu.callback$, "render"

  Page Copy 1 To 0, B
End Sub

Sub menu.render_item(idx%)
  twm.foreground(Choice(idx% = 0, twm.YELLOW%, twm.WHITE%))
  twm.inverse(idx% = menu.selection%)
  ' Don't use FIELD$() here because we want to respect whitespace.
  Local item$ = Left$(menu.items$(idx%), InStr(menu.items$(idx%), "|") - 1)
  Local x% = (menu.width% - Len(item$)) \ 2
  twm.print_at(x%, idx% + 2, item$)
  twm.inverse(0)
End Sub

Sub menu.main_loop()
  Local key%
  Do
    If sys.break_flag% Then menu.on_break()
    Call menu.ctrl$, key%
    If Not key% Then keys_cursor_ext(key%)
    menu.process_key(key%)
  Loop
End Sub

Sub menu.process_key(key%)
  Local cmd$, new_sel%
  If Not key% Then Exit Sub

  Select Case key%
    Case ctrl.A, ctrl.B, ctrl.HOME, ctrl.START, ctrl.SELECT, ctrl.LEFT, ctrl.RIGHT
      ' TODO: there is a bug in CALL which means it can't handle the case
      '       where an expression that provides the command name contains a
      '       comma or single-quote.
      cmd$ = Field$(menu.items$(menu.selection%), 2, "|")
      Call cmd$, key%

    Case ctrl.DOWN, ctrl.UP
      new_sel% = menu.selection%
      Do
        Inc new_sel%, Choice(key% = ctrl.DOWN, 1, -1)
        If new_sel% < 0 Or new_sel% >= menu.item_count% Then
          new_sel% = menu.selection%
          menu.play_invalid_fx()
          Pause ctrl.UI_DELAY
          Exit Do
        EndIf
      Loop Until Len(Field$(menu.items$(new_sel%), 2, "|"))
      menu.select_item(new_sel%)

    Case Else
      menu.play_invalid_fx()
      Pause ctrl.UI_DELAY
  End Select
End Sub

Sub menu.select_item(new_sel%)
  If new_sel% = menu.selection% Then Exit Sub
  menu.play_valid_fx()
  Pause ctrl.UI_DELAY
  Local old_sel% = menu.selection%
  menu.selection% = new_sel%
  Page Write 1
  menu.render_item(menu.selection%)
  menu.render_item(old_sel%)
  If Len(menu.callback$) Then Call menu.callback$, "selection_changed"
  Page Copy 1 To 0 , B
End Sub

Sub menu.play_valid_fx(block%)
  sound.play_fx(sound.FX_SELECT%(), block%)
End Sub

Sub menu.play_invalid_fx(block%)
  If Mm.Device$ = "MMB4L" Then
    Console Bell
  Else
    sound.play_fx(sound.FX_BLART%(), block%)
  EndIf
End Sub

' @param  frameCol%  frame colour; one of the colours from 'txtwm.inc'.
'                    If unspecified then uses twm.CYAN%.
Function menu.msgbox%(msg$, buttons$(), default%, frameCol%)
  menu.msgbox% = default%

  Local base% = Mm.Info(Option Base), num% = Bound(buttons$(), 1) - base% + 1
  If buttons$(base% + 1) = "" Then num% = 1 ' Single button.
  Local i%, key%, p%, released%, valid% = 1, word$, x%(num%)
  x%(base%) = 2
  For i% = base% + 1 To base% + num% - 1
    x%(i%) = x%(i% - 1) + Len(buttons$(i% - 1)) + 5
  Next

  twm.switch(menu.win2%)
  twm.cls()
  twm.foreground(Choice(frameCol%, frameCol%, twm.CYAN%))
  twm.box(0, 0, twm.w%, twm.h%)
  twm.foreground(twm.WHITE%)

  i% = 1 : p% = 1
  Do While p% <= Len(msg$)
    twm.print_at(2, i%, str.wwrap$(msg$, p%, twm.w% - 4))
    Inc i%
  Loop

  Do
    If sys.break_flag% Then menu.on_break()
    If valid% Then
      For i% = base% To base% + num% - 1
        menu.button(x%(i%), twm.h% - 4, buttons$(i%), menu.msgbox% = i%)
      Next
      Page Copy 1 To 0 , B
      valid% = 0
    EndIf
    Call menu.ctrl$, key%
    If Not key% Then keys_cursor_ext(key%)
    If Not key% Then released% = 1 : Continue Do
    If Not released% Then key% = 0 : Continue Do
    valid% = 0
    Select Case key%
      Case ctrl.A, ctrl.SELECT
        key% = ctrl.SELECT
        valid% = 1
      Case ctrl.LEFT
        If menu.msgbox% = 1 Then menu.msgbox% = 0 : valid% = 1
      Case ctrl.RIGHT
        If menu.msgbox% = 0 Then menu.msgbox% = 1 : valid% = 1
    End Select
    If valid% Then menu.play_valid_fx() Else menu.play_invalid_fx()
    Pause ctrl.UI_DELAY
  Loop Until key% = ctrl.SELECT
End Function

Sub menu.button(x%, y%, txt$, selected%)
  twm.lock_vga_cursor(1)
  twm.box1(x%, y%, Len(txt$) + 4, 3)
  If selected% Then twm.inverse(1)
  twm.print_at(x% + 2, y% + 1, txt$)
  If selected% Then twm.inverse(0)
  twm.lock_vga_cursor(0)
End Sub
