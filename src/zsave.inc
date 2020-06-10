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

    Local err, new_pc, new_fp, new_sp, new_sz

    ' Read program counter (3 bytes)
    ' Read frame pointer (2 bytes)
    ' Read stack size (2 bytes)
    ' Read dynamic memory size, (2 bytes)
    f$ = Input$(9, #1)
    new_pc = Peek(Var f$, 1) * 65536 + Peek(Var f$, 2) * 256 + Peek(Var f$, 3)
    new_fp = Peek(Var f$, 4) * 256 + Peek(Var f$, 5)
    new_sp = Peek(Var f$, 6) * 256 + Peek(Var f$, 7)
    new_sz = Peek(Var f$, 8) * 256 + Peek(Var f$, 9)
    If new_pc < 0 Or new_pc >= FILE_LEN Then err = 1
    If new_fp < 0 Or new_fp > new_sp Then err = 2
    If new_sp < 0 Or new_sp > 511 Then err = 3
    If new_sz <> BASE_STATIC Then err = 4

    If err = 0 Then
      pc = new_pc
      fp = new_fp
      sp = new_sp

      ' Read stack (2 bytes / entry)
      Local j
      For j = 0 To sp - 1
        f$ = Input$(2, #1)
        stack(j) = Peek(Var f$, 1) * 256 + Peek(Var f$, 2)
      Next j

      ' Read dynamic memory
      Local ad = 0
      Do
        f$ = Input$(254, #1)
        For j = 1 To Len(f$)
          wb(ad) = Peek(Var f$, j)
          ad = ad + 1
        Next j
      Loop Until Len(f$) = 0

      If ad <> BASE_STATIC Then Error "Unrecoverable restore error!"
    Else
      Print "Save file is invalid (error"; err; ")"
    EndIf

  Else
    Print "Saving '" + f$ + "' ..."
    Open "game" + Str$(i) + ".sav" For Output As #1
    Print #1, "ZMIM save file"
    Print #1, "1"
    Print #1, Date$ + " " + Time$
    Print #1, f$

    ' Write program counter (3 bytes)
    Print #1, Chr$(pc \ 65536); Chr$(pc \ 256); Chr$(pc Mod 256);
    ' Write frame pointer (2 bytes)
    Print #1, Chr$(fp \ 256); Chr$(fp Mod 256);
    ' Write stack size (2 bytes)
    Print #1, Chr$(sp \ 256); Chr$(sp Mod 256);
    ' Write dynamic memory size (2 bytes)
    Print #1, Chr$(BASE_STATIC \ 256); Chr$(BASE_STATIC Mod 256);
    ' Write stack (2 bytes / entry)
    For i = 0 To sp - 1
      Print #1, Chr$(stack(i) \ 256); Chr$(stack(i) Mod 256);
    Next i
    ' Write dynamic memory
    For i = 0 To BASE_STATIC - 1
       Print #1, Chr$(rb(i));
    Next i
  EndIf

  If i > 0 Then Close #1 : zsave = 1
  Chdir old$

End Function
