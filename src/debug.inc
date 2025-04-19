' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Dim bp(9) ' The addresses of up to 10 breakpoints, -1 for unset
          ' execute#num_bp should also be updated when these are changed

' Interactive debugger.
Function debug()
  Local a, b, c, cmd$(9) Length 20, cn, i, op, pc_old, s$, t, sp_old

  ' Decode and display the next instruction but don't execute it.
  pc_old = pc : sp_old = sp
  op = decode(1)
  pc = pc_old : sp = sp_old

  Do
    ' Read line of input and parse into space separated commands/arguments.
    cn = 0
    For i = 0 To 9 : cmd$(i) = "" : Next i
    t = Timer
    s$ = cin$("DEBUG >> ") + " "
    Timer = t
    For i = 1 To Len(s$)
      c = Peek(Var s$, i)
      If Chr$(c) = " " Then
        If Len(cmd$(cn)) > 0 Then cn = cn + 1
        If cn = 10 Then Error "Too many arguments"
      Else
        cmd$(cn) = cmd$(cn) + Chr$(c)
      EndIf
    Next i

    debug = E_DEBUG

    If cmd$(0) = "abrv" Then
      dmp_abrv()

    ElseIf cmd$(0) = "b" Then
      ' Set address breakpoint
      a = Val(cmd$(1))
      If a >= 0 And a < FILE_LEN Then
        For i = 0 To 9
          If bp(i) = a Then
            cout("Duplicate breakpoint [" + Str$(i) + "]") : endl()
            a = -1
            Exit For
          EndIf
        Next i
        For i = 0 To 9
          If a = -1 Then
            ' Duplicate breakpoint previously reported
            Exit For
          ElseIf bp(i) = -1 Then
            bp(i) = a
            num_bp = num_bp + 1
            cout("Set breakpoint [" + Str$(i) + "] at " + fmt_hex$(bp(i)))
            endl()
            Exit For
          EndIf
          If i = 9 Then cout("No free address breakpoints") : endl()
        Next i
      Else
        cout("Invalid breakpoint address") : endl()
      EndIf

    ElseIf cmd$(0) = "B" Then
      ' List address breakpoints
      If num_bp = 0 Then
        cout("No address breakpoints set") : endl()
      Else
        For i = 0 To 9
          If bp(i) <> -1 Then
            cout("[" + Str$(i) + "] " + fmt_hex$(bp(i))) : endl()
          EndIf
        Next i
      EndIf

    ElseIf cmd$(0) = "c" Then
      ' Continue
      endl()
      If oc = &h4 And op >= &hE0 Then cout(">") ' Display READ prompt
      debug = E_OK

    ElseIf cmd$(0) = "C" Then
      ' Stack dump
      dmp_stack(Val(cmd$(1)))

    ElseIf cmd$(0) = "d" Then
      ' Dump memory
      If Len(cmd$(1)) = 0 Then a = pc Else a = Val(cmd$(1))
      dmp_mem(a, Val(cmd$(2)))

    ElseIf cmd$(0) = "dict" Then
      dmp_dict()

    ElseIf cmd$(0) = "dmap" Then
      dmp_mmap()

    ElseIf cmd$(0) = "G" Then
      ' Dump global variables
      dmp_global(Val(cmd$(1)), Val(cmd$(2)))

    ElseIf cmd$(0) = "h" Then
      cecho(file.resolve$(ss$(RESOURCES_DIR), "debug_help.txt"))

    ElseIf cmd$(0) = "H" Then
      ' Dump header
      dmp_hdr()

    ElseIf cmd$(0) = "q" Then
      ' Quit
      debug = E_QUIT

    ElseIf cmd$(0) = "o" Then
      ' Dump object
      dmp_obj(Val(cmd$(1)))

    ElseIf cmd$(0) = "s" Then
      ' Step
      If oc = &h4 And op >= &hE0 Then cout(">") ' Display READ prompt
      debug = exec(0)
      If debug = E_OK Then debug = E_BREAK

    ElseIf cmd$(0) = "troff" Then
      ' Disable trace
      cout("Trace OFF") : endl()
      ztrace = 0

    ElseIf cmd$(0) = "tron" Then
      ' Enable trace
      cout("Trace ON") : endl()
      ztrace = 1

    ElseIf cmd$(0) = "v" Then
      ' Clear address breakpoint
      a = Val(cmd$(1))
      If a < 0 Or a > 9 Then
        cout("Invalid address breakpoint") : endl()
      ElseIf bp(a) = -1 Then
        cout("Address breakpoint [" + Str$(a) + "] already cleared") :endl()
      Else
        bp(a) = -1
        num_bp = num_bp - 1
        cout("Address breakpoint [" + Str$(a) + "] cleared") : endl()
      EndIf

    ElseIf cmd$(0) = "V" Then
      ' Lookup word in dictionary
      a = di_lookup(LCase$(cmd$(1)))
      cout(fmt_hex$(a)) : endl()

    ElseIf cmd$(0) = "x" Then
      ' Parse and print value
      a = Val(cmd$(1))
      cout(Str$(a))
      cout("  " + fmt_hex$(a))
      cout("  &b" + LPad$(Bin$(a), 16, "0"))
      endl()

    ElseIf cmd$(0) = "z" Then
      ' Clear all breakpoints
      For i = 0 To 9 : bp(i) = -1 : Next i
      num_bp = 0
      cout("All breakpoints cleared") : endl()

    Else
      cout("Unknown debug command") : endl()

    EndIf

  Loop While debug = E_DEBUG

End Function

' Prints abbreviations.
' z3 always has 96 abbreviations.
Sub dmp_abrv()
  Local i, j, nc, nr, width, x

  nc = 3 : nr = CInt(96 / nc + 0.4999) : width = 24
  For i = 0 To nr - 1
    For j = 0 To nc - 1
      cflush()
      x = (j * nr) + i
      If x < 96 Then
        If j * width > Pos Then cout(Space$(j * width - Pos))
        cout("[" + lpad$(Str$(x), 2) + "] {")
        print_abrv(x)
        cout "}"
      EndIf
    Next j
    endl()
  Next i

End Sub

' Prints dictionary.
Sub dmp_dict()
  Local a, i, j, nc, nr, width, x

  cout("Word separators = {" + DICT_SEP$(0) + "}") : endl()
  cout("Word count      = " + Str$(DICT_ENTRY_COUNT)) : endl()
  cout("Word size       = " + Str$(DICT_ENTRY_LEN)) : endl()
  endl()

  nc = 3 : nr = CInt(DICT_ENTRY_COUNT / nc + 0.4999) : width = 24
  For i = 0 To nr - 1
    For j = 0 To nc - 1
      cflush()
      x = (j * nr) + i
      If x < DICT_ENTRY_COUNT Then
        If j * width > Pos Then cout(Space$(j * width - Pos))
        cout("[" + lpad$(Str$(x + 1), 4) + "] ")
        a = x * DICT_ENTRY_LEN + DICT_ENTRY_AD
        x = rw(a) : cout(lpad$(Hex$(x), 4, "0"))
        x = rw(a + 2) : cout(lpad$(Hex$(x), 4, "0") + " ")
        print_zstring(a)
      EndIf
    Next j
    endl()
  Next i

End Sub

' Prints 'n' global variables starting from 'a'.
Sub dmp_global(a, n)
  Local i, x
  If n < 1 Then n = 1
  If a > 239 Then a = 239
  For i = a To a + n - 1
    If i > 239 Then Exit For
    x = vget(i + 16)
    cout("G" + lpad$(Str$(i), 2, "0") + " = " + fmt_hex$(x))
    If x And &h8000 Then
      cout("  " + Str$(x - &h10000))
    Else
      cout("  " + Str$(x))
    EndIf
    endl()
  Next i
End Sub

' Prints Z-machine header.
Sub dmp_hdr()
  Local i
  cout("Version      = " + Str$(rb(&h00))) : endl()
  cout("Flags1       = &b" + lpad$(Bin$(rb(&h01)), 8, "0")) : endl()
  cout("Release      = " + Str$(rw(&h02))) : endl()
  cout("Base high    = " + fmt_hex$(rw(&h04))) : endl()
  cout("Initial PC   = " + fmt_hex$(rw(&h06))) : endl()
  cout("Dictionary   = " + fmt_hex$(rw(&h08))) : endl()
  cout("Object tbl   = " + fmt_hex$(rw(&h0A))) : endl()
  cout("Glob var tbl = " + fmt_hex$(rw(&h0C))) : endl()
  cout("Base static  = " + fmt_hex$(rw(&h0E))) : endl()
  cout("Flags2       = &b" + lpad$(Bin$(rb(&h10)), 8, "0")) : endl()
  cout("Serial num   = ")
  For i = &h12 To &h17 : cout(Chr$(rb(i))) : Next i
  endl()
  cout("Abbrv tbl    = " + fmt_hex$(rw(&h18))) : endl()
  cout("File length  = " + Str$(2 * rw(&h1A)) + " bytes") : endl()
  cout("File chksum  = " + Str$(rw(&h1C))) : endl()
  cout("Std revision = " + Str$(rw(&h32))) : endl()
End Sub

' Prints 'n' bytes from 'mem' starting from 'a'
Sub dmp_mem(a, n)
  Local i, x
  If n = 0 Then n = 128
  For i = 0 To n - 1
    If i Mod 16 = 0 Then cout("[" + fmt_hex$(a + i) + "] ")
    If a + i < FILE_LEN Then
      x = rb(a + i)
      cout(lpad$(Hex$(x), 2, "0") + " ")
    Else
      cout("XX ")
    EndIf
    If ((i + 1) Mod 16 = 0) And (i <> n - 1) Then endl()
  Next i
  endl()
End Sub

' Dump CMM1 virtual memory map.
Sub dmp_mmap
  Local i, j, nc, nr, width, x

  On Error Skip 1
  i = NUM_PHYSICAL_PAGES
  If MM.ERRNO <> 0 Then
    ' If NUM_PHYSICAL_PAGES in undefined then ...
    cout("Not using virtual memory implementation.") : endl()
    On Error Clear
    Exit Sub
  EndIf

  cout("Physical page -> Virtual page") : endl()
  nc = 6 : nr = CInt(NUM_PHYSICAL_PAGES / nc + 0.4999) : width = 15
  For i = 0 To nr - 1
    For j = 0 To nc - 1
      cflush()
      x = (j * nr) + i
      If x < NUM_PHYSICAL_PAGES Then
        If j * width > Pos Then cout(Space$(j * width - Pos))
        cout(lpad$(Str$(x), 3) + " -> " + lpad$(Str$(pp_to_vp(x)), 3))
      EndIf
    Next j
    endl()
  Next i

  endl()

  cout("Virtual page -> Physical page") : endl()
  nr = CInt(NUM_VIRTUAL_PAGES / nc + 0.4999)
  For i = 0 To nr - 1
    For j = 0 To nc - 1
      cflush()
      x = (j * nr) + i
      If x < NUM_VIRTUAL_PAGES Then
        If j * width > Pos Then cout(Space$(j * width - Pos))
        cout(lpad$(Str$(x), 3) + " -> " + lpad$(Str$(vp_to_pp(x)), 3))
      EndIf
    Next j
    endl()
  Next i

End Sub

' Prints object 'o'.
Sub dmp_obj(o)
  Local ad, i, p, sz, x

  If o <= 0 Then
    cout("Property defaults:") : endl()
    ad = rw(&h0A)
    For i = 0 To 30
      x = rw(ad)
      cout(lpad$(Hex$(x), 4, "0") + " ")
      If (i + 1) Mod 10 = 0 Then endl()
      ad = ad + 2
    Next i
    endl()
    Exit Sub
  EndIf

  cout(Str$(o) + ". ")
  cout("Attributes: ")
  For i = 0 To 31
    x = ob_attr(o, i)
    If x <> 0 Then cout(Str$(i))
  Next i
  endl()

  x = ob_rel(o, PARENT)
  cout("   Parent object: " + Str$(x) + "  ")
  x = ob_rel(o, SIBLING)
  cout("Sibling object: " + Str$(x) + "  ")
  x = ob_rel(o, CHILD)
  cout("Child object:" + Str$(x)) : endl()

  ad = ob_prop_base(o)
  cout("   Property address: " + fmt_hex$(ad)) : endl()
  cout("        Description: '")
  ob_print(o);
  cout("'") : endl()
  cout("         Properties:") : endl()
  p = 0
  Do
    p = ob_next_prop(o, p)
    If p > 0 Then
      ad = ob_prop_addr(o, p)
      sz = ob_prop_len(ad)
      x = rb(ad - 1)
      If x \ 32 + 1 <> sz Then Error
      cout("             [" + Str$(p) + "] ")
      For i = 1 To sz
        x = rb(ad)
        ad = ad + 1
        cout(lpad$(Hex$(x), 2, "0") + " ")
      Next i
      endl()
    EndIf
  Loop Until p = 0
End Sub

' Prints dissassembly for the current instruction.
'
' @param  m$  instruction mnemonic
Sub dmp_op(m$)
  cout(rpad$(m$, 14))
  If m$ = "CALL" Then
    cout(fmt_call_operands$())
  ElseIf m$ = "JUMP" Then
    cout(fmt_jump_operands$())
  ElseIf m$ = "STORE" Or m$ = "DEC_CHK" Or m$ = "INC_CHK" Then
    cout(fmt_store_operands$())
  ElseIf m$ = "DEC" Or m$ = "INC" Or m$ = "PULL" Or m$= "LOAD" Then
    cout(fmt_store_operands$())
  Else
    cout(fmt_normal_operands$())
  EndIf

  If st > -1 Then cout(" -> " + fmt_store_value$(st))

  If br = 0 Then endl() : Exit Sub
  If br And &h80000 Then cout(" [TRUE] ") Else cout(" [FALSE] ")
  If (br And &h7FFFF) = pc - 2 Then cout("RFALSE") : endl() : Exit Sub
  If (br And &h7FFFF) = pc - 1 Then cout("RTRUE") : endl() : Exit Sub
  cout(Hex$(br And &h7FFFF)) : endl()
End Sub

' Print information about the routine starting at 'new_pc'.
Sub dmp_routine(new_pc)
  Local i, locals_sz, x
  locals_sz = rb(new_pc)
  cout("Routine " + Hex$(new_pc) + ", " + Str$(locals_sz) + " locals (")
  For i = 0 To locals_sz - 1
    If i > 0 Then cout(", ")
    x = rw(new_pc + 1 + i * 2)
    cout(lpad$(Hex$(x), 4, "0"))
  Next i
  cout(")") : endl()
End Sub

' Prints the contents of the stack.
' If 'a' = 0 then prints only the current stack frame,
' otherwise prints them all.
Sub dmp_stack(a)
  Local i, tmp_fp, x

  cout("TOP: ")
  If sp = 0 Then cout("*empty*") : endl()
  tmp_fp = fp

  For i = sp - 1 To 0 Step -1
    If i < sp - 1 Then cout("     ")
    x = st_peek(i)

    If x And &h8000 Then
      cout(fmt_hex$(x) + "  " + lpad$(Str$(x - &h10000), 6, " ") + "  ")
    Else
      cout(fmt_hex$(x) + "  " + lpad$(Str$(x), 6, " ") + "  ")
    EndIf

    If tmp_fp = &hFFFF Then
      ' Nothing
    ElseIf i = tmp_fp Then
      cout("previous fp")
      If a <> 0 Then endl() : cout(String$(35, "-"))
      tmp_fp = x
    ElseIf i = tmp_fp + 1 Then
      cout("store result")
    ElseIf i = tmp_fp + 2 Then
      cout("return address (lo)")
    ElseIf i = tmp_fp + 3 Then
      cout("return address (hi)")
    ElseIf i = tmp_fp + 4 Then
      cout("num locals")
    Else
      cout("L" + lpad$(Str$(i - tmp_fp - 5), 2, "0"))
      ' TODO: Not everything on top of the stack is a local variable
    EndIf
    endl()
    If a = 0 And i = fp Then Exit For
  Next i
End Sub

Function fmt_operand$(i)
  Local a$, x
  x = ov(i)
  If ot(i) <> &b10 Then ' &b10 = VARIABLE
    fmt_operand$ = "#" + lpad$(Hex$(x), 2, "0")
    Exit Function
  EndIf
  If x = 0 Then
    a$ = "(SP)+"
  ElseIf x < &h10 Then
    a$ = "L" + lpad$(Hex$(x - 1), 2, "0")
  Else
    a$ = "G" + lpad$(Hex$(x - &h10), 2, "0")
  EndIf
  If x > 0 Then a$ = a$ + " (=" + Hex$(vget(x)) + ")"
  fmt_operand$ = a$
End Function

Function fmt_call_operands$()
  Local a$, i
  If ot(0) = &b10 Then ' &b10 = VARIABLE
    a$ = fmt_operand$(i)
  Else
    a$ = Hex$(2 * ov(0))
  EndIf
  a$ = a$ + " ("
  For i = 1 To onum - 1
    If i > 1 Then a$ = a$ + ", "
    a$ = a$ + fmt_operand$(i)
  Next i
  a$ = a$ + ")"
  fmt_call_operands$ = a$
End Function

Function fmt_jump_operands$()
  Local of
  If onum > 1 Then Error "Too many operands."
  of = oa(0)
  If of And 2^15 Then of = of - 65536
  fmt_jump_operands$ = Hex$(pc + of - 2)
End Function

Function fmt_store_value$(st)
  If st = 0 Then
    cout("-(SP)")
  ElseIf st < &h10 Then
    cout("L" + lpad$(Hex$(st - 1), 2, "0"))
  Else
    cout("G" + lpad$(Hex$(st - &h10), 2, "0"))
  EndIf
End Function

Function fmt_store_operands$()
  Local a$, i
  If ot(0) = &b10 Then Error "Unexpected VARIABLE operand"
  a$ = a$ + fmt_store_value$(ov(0))
  For i = 1 To onum - 1
    a$ = a$ + ", " + fmt_operand$(i)
  Next i
  fmt_store_operands$ = a$
End Function

Function fmt_normal_operands$()
  Local a$, i
  For i = 0 To onum - 1
    If i > 0 Then a$ = a$ + ", "
    a$ = a$ + fmt_operand$(i)
  Next i
  fmt_normal_operands$ = a$
End Function
