' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Save/Restore game
'
' NOTE: Use a single subroutine to cover both because of the 50 subroutine
'       limit in MMBasic 4.5 on the CMM1
'
' @param  res  set 0 to save and 1 to restore
' @return      1 on success, 0 on failure or if cancelled
Function zsave(res)
  Local exists(10), i, f$, line$(2) Length 30, old$

  If res Then
    Print "Select game to restore:"
  Else
    Print "Select save game slot:"
  EndIf

  old$ = Cwd$

  Chdir ss$(SAVE_DIR)
  f$ = Dir$(ss$(STORY), File)
  If f$ <> "" Then Error "Unexpected file"
  f$ = Dir$(ss$(STORY), Dir)
  If f$ = "" Then MkDir(ss$(STORY))
  Chdir ss$(STORY)
  For i = 1 To 10
    f$ = Dir$("game" + Str$(i) + ".sav")
    Print "  [" + Format$(i, "%2g") + "] ";
    If f$ = "" Then
      Print "Empty"
    Else
      exists(i) = 1
      Open "game" + Str$(i) + ".sav" For Input As #1
      Line Input #1, line$(0) ' header
      Line Input #1, line$(1) ' version
      Line Input #1, line$(2) ' date/time
      Line Input #1, f$       ' game name
      ' TODO: verify header / version
      Print line$(2); " - "; f$
      Close #1
    EndIf
  Next i

  Line Input "Game number? ", f$
  i = Val(f$)
  If i < 1 Or i > 10 Then i = 0

  If i > 0 And res And Not exists(i) Then i = 0

  If i > 0 And Not res And exists(i) Then
    Line Input "Overwrite game " + Str$(i) + " [y|N]?", f$
    If LCase$(f$) <> "y" Then i = 0
  EndIf

  If i > 0 And Not res Then
    Line Input "Save game name? ", f$
    If f$ = "" Then i = 0
  EndIf

  If i = 0 Then
    Print "Cancelled."
  ElseIf res Then
    Open "game" + Str$(i) + ".sav" For Input As #1
    Line Input #1, line$(0)
    Line Input #1, line$(1)
    Line Input #1, line$(2)
    Line Input #1, f$
    Print "Restoring '" + f$ + "' ..."
  Else
    Print "Saving '" + f$ + "' ..."
    Open "game" + Str$(i) + ".sav" For Output As #1
    Print #1, "ZMIM save file"
    Print #1, "1"
    Print #1, Date$ + " " + Time$
    Print #1, f$
  EndIf

  If i > 0 Then Close #1 : zsave = 1
  Chdir old$

End Function
