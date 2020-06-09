' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Interactive debugger
Function debug()
  Local a, b, c, cmd$(9) Length 20, cn, i, op, pc_old, s$, sp_old

  ' Decode and display the next instruction but don't execute it.
  pc_old = pc : sp_old = sp
  op = decode(1)
  pc = pc_old : sp = sp_old

  Do
    ' Read line of input and parse into space separated commands/arguments.
    cn = 0
    For i = 0 To 9 : cmd$(i) = "" : Next i
    rtime = rtime - Timer
    Line Input "DEBUG >> ", s$
    rtime = rtime + Timer
    s$ = s$ + " "
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

    If cmd$(0) = "b" Then
      ' Set address breakpoint
      a = Val(cmd$(1))
      If a >= 0 And a < FILE_LEN Then
        For i = 0 To NUM_BP - 1
          If bp(i) = a Then
            Print "Duplicate breakpoint [" + Str$(i) + "]"
            a = -1
            Exit For
          EndIf
        Next i
        For i = 0 To NUM_BP - 1
          If a = -1 Then
            ' Duplicate breakpoint previously reported
            Exit For
          ElseIf bp(i) = -1 Then
            bp(i) = a
            Print "Set breakpoint [" + Str$(i) + "] at " + fmt_hex$(bp(i))
            Exit For
          EndIf
          If i = 9 Then Print "No free address breakpoints"
        Next i
      Else
        Print "Invalid breakpoint address"
      EndIf

    ElseIf cmd$(0) = "B" Then
      ' List address breakpoints
      a = 0
      For i = 0 To NUM_BP - 1
        If bp(i) <> -1 Then
          Print "[" + Str$(i) + "] " + fmt_hex$(bp(i))
          a = a + 1
        EndIf
      Next i
      If a = 0 Then Print "No address breakpoints set"

    ElseIf cmd$(0) = "c" Then
      ' Continue
      If oc = &h4 And op >= &hE0 Then Print ">"; ' Display READ prompt
      debug = E_OK

    ElseIf cmd$(0) = "C" Then
      ' Stack dump
      dmp_stack(Val(cmd$(1)))

    ElseIf cmd$(0) = "d" Then
      ' Dump memory
      If Len(cmd$(1)) = 0 Then a = pc Else a = Val(cmd$(1))
      dmp_mem(a, Val(cmd$(2)))

    ElseIf cmd$(0) = "dict" Then
      ' Dump dictionary
      dmp_dict()

    ElseIf cmd$(0) = "G" Then
      ' Dump global variables
      dmp_global(Val(cmd$(1)), Val(cmd$(2)))

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
      If oc = &h4 And op >= &hE0 Then Print ">"; ' Display READ prompt
      debug = exec(0)
      If debug = E_OK Then debug = E_BREAK

    ElseIf cmd$(0) = "troff" Then
      ' Disable trace
      Print "Trace OFF"
      ztrace = 0

    ElseIf cmd$(0) = "tron" Then
      ' Enable trace
      Print "Trace ON"
      ztrace = 1

    ElseIf cmd$(0) = "v" Then
      ' Clear address breakpoint
      a = Val(cmd$(1))
      If a < 0 Or a >= NUM_BP Then
        Print "Invalid address breakpoint"
      ElseIf bp(a) = -1 Then
        Print "Address breakpoint [" + Str$(a) + "] already cleared"
      Else
        bp(a) = -1
        Print "Address breakpoint [" + Str$(a) + "] cleared"
      EndIf

    ElseIf cmd$(0) = "V" Then
      ' Lookup word in dictionary
      a = di_lookup(LCase$(cmd$(1)))
      Print fmt_hex$(a)

    ElseIf cmd$(0) = "x" Then
      ' Parse and print value
      a = Val(cmd$(1))
      Print Str$(a);
      Print "  " + fmt_hex$(a)
      Print "  &b" + LPad$(Bin$(a), 16, "0")

    ElseIf cmd$(0) = "z" Then
      ' Clear all breakpoints
      For i = 0 To NUM_BP - 1 : bp(i) = -1 : Next i
      Print "All breakpoints cleared"

    Else
      Print "Unknown debug command"

    EndIf

  Loop While debug = E_DEBUG

End Function

' Prints 'n' bytes from 'mem' starting at 'a'
Sub dmp_mem(a, n)
  Local i, x
  If n = 0 Then n = 128
  For i = 0 To n - 1
    If i Mod 16 = 0 Then Print "[" + fmt_hex$(a + i) + "] ";
    If a + i < FILE_LEN Then
      x = rb(a + i)
      Print lpad$(Hex$(x), 2, "0"); " ";
    Else
      Print "XX ";
    EndIf
    If ((i + 1) Mod 16 = 0) And (i <> n - 1) Then Print
  Next i
  Print
End Sub

Sub dmp_obj(o)
  Local ad, i, p, sz, x

  If o <= 0 Then
    Print "Property defaults:"
    ad = rw(&h0A)
    For i = 0 To 30
      x = rw(ad)
      Print lpad$(Hex$(x), 4, "0"); " ";
      If (i + 1) Mod 10 = 0 Then Print
      ad = ad + 2
    Next i
    Print
    Exit Sub
  EndIf

  Print Str$(o); ". ";
  Print "Attributes: ";
  For i = 0 To 31
    x = oattr(o, i)
    If x <> 0 Then Print i;
  Next i
  Print

  x = orel(o, PARENT)
  Print "   Parent object:"; x; "  ";
  x = orel(o, SIBLING)
  Print "Sibling object:"; x; "  ";
  x = orel(o, CHILD)
  Print "Child object:"; x

  ad = get_prop_base(o)
  Print "   Property address: "; lpad$(Hex$(ad), 4, "0")
  Print "        Description: '";
  print_obj(o);
  Print "'"
  Print "         Properties:"
  p = 0
  Do
    p = get_next_prop(o, p)
    If p > 0 Then
      ad = get_prop_addr(o, p)
      sz = get_prop_len(ad)
      x = rb(ad - 1)
      If x\32 + 1 <> sz Then Error
      Print "             ["; Str$(p); "] ";
      For i = 1 To sz
        x = rb(ad)
        ad = ad + 1
        Print lpad$(Hex$(x), 2, "0"); " ";
      Next i
      Print
    EndIf
  Loop Until p = 0
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
    Print "-(SP)";
  ElseIf st < &h10 Then
    Print "L" + lpad$(Hex$(st - 1), 2, "0");
  Else
    Print "G" + lpad$(Hex$(st - &h10), 2, "0");
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

Sub dmp_op(m$, st, br)
  Print rpad$(m$, 14);
  If m$ = "CALL" Then
    Print fmt_call_operands$();
  ElseIf m$ = "JUMP" Then
    Print fmt_jump_operands$();
  ElseIf m$ = "STORE" Or m$ = "DEC_CHK" Or m$ = "INC_CHK" Then
    Print fmt_store_operands$();
  ElseIf m$ = "DEC" Or m$ = "INC" Or m$ = "PULL" Or m$= "LOAD" Then
    Print fmt_store_operands$();
  Else
    Print fmt_normal_operands$();
  EndIf

  If st > -1 Then Print " -> "; fmt_store_value$(st);

  If br = 0 Then Print : Exit Sub
  If br And &h10000 Then Print " [TRUE] "; Else Print " [FALSE] ";
  If (br And &hFFFF) = pc - 2 Then Print "RFALSE" : Exit Sub
  If (br And &hFFFF) = pc - 1 Then Print "RTRUE" : Exit Sub
  Print Hex$(br And &hFFFF)
End Sub

Sub dmp_routine(new_pc)
  Local i, locals_sz, x
  locals_sz = rb(new_pc)
  Print "Routine "; Hex$(new_pc); ", "; Str$(locals_sz); " locals (";
  For i = 0 To locals_sz - 1
    If i > 0 Then Print ", ";
    x = rw(new_pc + 1 + i * 2)
    Print lpad$(Hex$(x), 4, "0");
  Next i
  Print ")"
End Sub

' Expects global variables:
'  fp    - current frame index/pointer
'  sp    - current stack index/pointer
'  stack - the stack (an array of 32-bit floats)
Sub dmp_stack(a)
  Local i, tmp_fp, x

  Print "TOP: ";
  If sp = 0 Then Print "*empty*"
  tmp_fp = fp

  For i = sp To 1 Step -1
    If i < sp Then Print "     ";
    x = stack(i - 1)

    If x And &h8000 Then
      Print fmt_hex$(x); "  "; lpad$(Str$(x - &h10000), 6, " ") + "  ";
    Else
      Print fmt_hex$(x); "  "; lpad$(Str$(x), 6, " ") + "  ";
    EndIf

    If tmp_fp = &hFFFF Then
      ' Nothing
    ElseIf i = tmp_fp Then
      Print "previous fp";
      If a <> 0 Then Print : Print String$(35, "-");
      tmp_fp = x
    ElseIf i = tmp_fp + 1 Then
      Print "store result";
    ElseIf i = tmp_fp + 2 Then
      Print "return address";
    ElseIf i = tmp_fp + 3 Then
      Print "num locals";
    Else
      Print "L" + lpad$(Str$(i - tmp_fp - 4), 2, "0");
      ' TODO: Not everything on top of the stack is a local variable
    EndIf
    Print
    If a = 0 And i = fp Then Exit For
  Next i
End Sub

' Dump global variable.
Sub dmp_global(a, n)
  Local i, x
  If n < 1 Then n = 1
  If a > 239 Then a = 239
  For i = a To a + n - 1
    If i > 239 Then Exit For
    x = vget(i + 16)
    Print "G" + lpad$(Str$(i), 2, "0") + " = &h" + lpad$(Hex$(x), 4, "0");
    If x And &h8000 Then
      Print "  " + Str$(x - &h10000)
    Else
      Print "  " + Str$(x)
    EndIf
  Next i
End Sub
