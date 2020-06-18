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
  Local exists(10), i, old_dir$, s$, s2$(2) Length 40

  If res Then
    cout("Select game to restore:") : endl()
  Else
    cout("Select save game slot:") : endl()
  EndIf

  old_dir$ = Cwd$
  ChDir ss$(SAVE_DIR) + "/" + ss$(STORY)
  For i = 1 To 10
    s$ = Dir$("game" + Str$(i) + ".sav")
    cout("  [" + Format$(i, "%2g") + "] ")
    If s$ = "" Then
      cout("Empty") : endl()
    Else
      exists(i) = 1
      Open "game" + Str$(i) + ".sav" For Input As #1
      Line Input #1, s2$(0) ' header
      Line Input #1, s2$(1) ' version
      Line Input #1, s2$(2) ' date/time
      Line Input #1, s$     ' game name
      ' TODO: verify header / version
      cout(s2$(2) + " - " + s$) : endl()
      Close #1
    EndIf
  Next i
  ChDir old_dir$

  s$ = cin$("Game number? ")
  i = Val(s$)
  If i < 1 Or i > 10 Then i = 0

  If i > 0 And res And Not exists(i) Then i = 0

  If i > 0 And Not res And exists(i) Then
    s$ = cin$("Overwrite game " + Str$(i) + " [y|N]? ")
    If LCase$(s$) <> "y" Then i = 0
  EndIf

  If i > 0 And Not res Then
    s$ = cin$("Save game name? ")
    If s$ = "" Then i = 0
  EndIf

  If i = 0 Then cout("Cancelled") : endl() : Exit Function
  
  s2$(0) = ss$(SAVE_DIR) + "/" + ss$(STORY) + "/game" + Str$(i) + ".sav"

  If res Then
    Open s2$(0) For Input As #1

    ' Read text header
    Line Input #1, s2$(0)
    Line Input #1, s2$(1)
    Line Input #1, s2$(2)
    Line Input #1, s$
    cout("Restoring '" + s$ + "' ...") : endl()

    Local ad, err, new_pc, new_fp, stack_sz, mem_sz

    ' Read 9 byte data header
    ' 3 bytes - program counter
    ' 2 bytes - frame pointer
    ' 2 bytes - stack size
    ' 2 bytes - dynamic memory size
    s$ = Input$(9, #1)
    new_pc = Peek(Var s$, 1) * 65536 + Peek(Var s$, 2) * 256 + Peek(Var s$, 3)
    new_fp = Peek(Var s$, 4) * 256 + Peek(Var s$, 5)
    stack_sz = Peek(Var s$, 6) * 256 + Peek(Var s$, 7)
    mem_sz = Peek(Var s$, 8) * 256 + Peek(Var s$, 9)

    ' Validate data header
    If new_pc < 0 Or new_pc >= FILE_LEN Then err = 1
    If new_fp < 0 Or new_fp > stack_sz Then err = 2
    If stack_sz < 0 Or stack_sz > 511 Then err = 3
    If mem_sz <> BASE_STATIC Then err = 4
    If err <> 0 Then
      cout("Save file is invalid (error " + Str$(err) + ")")
      Close #1
      Exit Function
    EndIf

    pc = new_pc
    fp = new_fp
    sp = 0

    ' Read stack (2 bytes / entry)
    For i = 0 To stack_sz - 1
      s$ = Input$(2, #1)
      push(Peek(Var s$, 1) * 256 + Peek(Var s$, 2))
    Next i

    ' Read dynamic memory
    Do
      s$ = Input$(255, #1)
      For i = 1 To Len(s$)
        wb(ad, Peek(Var s$, i))
        ad = ad + 1
      Next i
    Loop Until Len(s$) = 0

    If ad <> BASE_STATIC Then Error "Unrecoverable restore error!"

  Else
    cout("Saving '" + s$ + "' ...") : endl()
    Open s2$(0) For Output As #1

    ' Write text header
    Print #1, "ZMIM save file"
    Print #1, "1"
    Print #1, Date$ + " " + Time$
    Print #1, s$

    ' Write 9 byte data header
    ' 3 bytes - program counter
    ' 2 bytes - frame pointer
    ' 2 bytes - stack size
    ' 2 bytes - dynamic memory size
    Print #1, Chr$(pc \ 65536); Chr$(pc \ 256); Chr$(pc Mod 256);
    Print #1, Chr$(fp \ 256); Chr$(fp Mod 256);
    Print #1, Chr$(sp \ 256); Chr$(sp Mod 256);
    Print #1, Chr$(BASE_STATIC \ 256); Chr$(BASE_STATIC Mod 256);

    ' Write stack (2 bytes / entry)
    For i = 0 To sp - 1
      Print #1, Chr$(st_peek(i) \ 256); Chr$(st_peek(i) Mod 256);
    Next i

    ' Write dynamic memory
    For i = 0 To BASE_STATIC - 1
       Print #1, Chr$(rb(i));
    Next i
  EndIf

  Close #1
  zsave = 1

End Function
