' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Function exec(tr)
  Local op, pc_old, sp_old

  ' Store the PC and SP in case we need to roll-back.
  pc_old = pc : sp_old = sp

  op = decode(tr)
  num_ops = num_ops + 1
  If op < &h80 Then
    exec = ex_2op()
  ElseIf op < &hB0 Then
    exec = ex_1op()
  ElseIf op < &hC0 Then
    exec = ex_0op()
  ElseIf op < &hE0 Then
    exec = ex_2op()
  Else
    exec = ex_varop()
  EndIf

  If exec = E_UNKNOWN Then
    Print "Unsupported instruction "; fmt_hex$(op, 2)
  ElseIf exec = E_UNIMPLEMENTED Then
    Print "Unimplemented instruction "; fmt_hex$(op, 2)
  EndIf

  If exec <> E_OK Then pc = pc_old : sp = sp_old

End Function

Function ex_2op()
  Local a, b, x, y, z, _

  a = oa(0)
  b = oa(1)

  If oc = &h1 Then
    ' JE
    x = (a = b)
    If (Not x) And (onum > 2) Then x = (a = oa(2))
    If (Not x) And (onum > 3) Then x = (a = oa(3))
    _branch(x, br)

  ElseIf oc = &h2 Then
    ' JL
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    _branch(a < b, br)

  ElseIf oc = &h3 Then
    ' JG
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    _branch(a > b, br)

  ElseIf oc = &h4 Then
    ' DEC_CHK
    x = vget(a)
    If x > 32767 Then x = x - 65536
    If b > 32767 Then b = b - 65536
    x = x - 1
    y = x < b
    If x < 0 Then x = 65536 + x
    vset(a, x)
    _branch(y, br)

  ElseIf oc = &h5 Then
    ' INC_CHK
    x = vget(a)
    If x > 32767 Then x = x - 65536
    If b > 32767 Then b = b - 65536
    x = x + 1
    y = x > b
    If x < 0 Then x = 65536 + x
    vset(a, x)
    _branch(y, br)

  ElseIf oc = &h6 Then
    ' JIN
    x = orel(a, PARENT)
    _branch(x = b, br)

  ElseIf oc = &h7 Then
    ' TEST
    _branch((a And b) = b, br)

  ElseIf oc = &h8 Then
    ' OR
    vset(st, a Or b)

  ElseIf oc = &h9 Then
    ' AND
    vset(st, a And b)

  ElseIf oc = &hA Then
    ' TEST_ATTR: a = object, b = attribute
    x = oattr(a, b)
    _branch(x = 1, br)

  ElseIf oc = &hB Then
    ' SET_ATTR
    _ = oattr(a, b, 1, 1)

  ElseIf oc = &hC Then
    ' CLEAR_ATTR
    _ = oattr(a, b, 1, 0)

  ElseIf oc = &hD Then
    ' STORE
    ' Note special handing for op with indirect reference to the stack pointer
    ' BUG: The one liner doesn't work
    '      If a = 0 Then _ = pop() : push(b) Else vset(a, b)
    If a = 0 Then
      _ = pop()
      push(b)
    Else
      vset(a, b)
    EndIf

  ElseIf oc = &hE Then
    ' INSERT_OBJ: a = object, b = destination
    x = orel(b, CHILD)         ' original child of destination
    y = orel(a, SIBLING)       ' original sibling of object
    z = orel(a, PARENT)        ' original parent of object
    _ = orel(b, CHILD, 1, a)   ' object is new child of destination
    _ = orel(a, PARENT, 1, b)  ' destination is new parent of object
    _ = orel(a, SIBLING, 1, x) ' original child of destination is new sibling of object
    _ = orel(z, CHILD, 1, y)   ' original sibling of object is new child of original parent

  ElseIf oc = &hF Then
    ' LOADW
    x = rw(a + 2 * b)
    vset(st, x)

  ElseIf oc = &h10 Then
    ' LOADB
    x = rb(a + b)
    vset(st, x)

  ElseIf oc = &h11 Then
    ' GET_PROP
    x = get_prop(a, b)
    vset(st, x)

  ElseIf oc = &h12 Then
    ' GET_PROP_ADDR
    x = get_prop_addr(a, b)
    vset(st, x)

  ElseIf oc = &h13 Then
    ' GET_NEXT_PROP
    x = get_next_prop(a, b)
    vset(st, x)

  ElseIf oc < &h19 Then
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536

    If oc = &h14 Then
      ' ADD
      x = a + b
    ElseIf oc = &h15 Then
      ' SUB
      x = a - b
    ElseIf oc = &h16 Then
      ' MUL
      x = a * b
    ElseIf oc = &h17 Then
      ' DIV
      x = a \ b
    Else
      ' MOD
      x = a Mod b
    EndIf

    If x < 0 Then x = 65536 + x
    vset(st, x)

  Else
    ex_2op = E_UNKNOWN
  EndIf
End Function

Function ex_1op()
  Local a, x, y, z, _

  a = oa(0)

  If oc = &h0 Then
    ' JZ
    _branch(a = 0, br)

  ElseIf oc = &h1 Then
    ' GET_SIBLING
    x = orel(a, SIBLING)
    vset(st, x)
    _branch(x <> 0, br)

  ElseIf oc = &h2 Then
    ' GET_CHILD
    x = orel(a, CHILD)
    vset(st, x)
    _branch(x <> 0, br)

  ElseIf oc = &h3 Then
    ' GET_PARENT
    x = orel(a, PARENT)
    vset(st, x)

  ElseIf oc = &h4 Then
    ' GET_PROP_LEN
    x = get_prop_len(a)
    vset(st, x)

  ElseIf oc = &h5 Then
    ' INC
    x = vget(a)
    If x > 32767 Then x = x - 65536
    x = x + 1
    If x < 0 Then x = 65536 + x
    vset(a, x)

  ElseIf oc = &h6 Then
    ' DEC
    x = vget(a)
    If x > 32767 Then x = x - 65536
    x = x - 1
    If x < 0 Then x = 65536 + x
    vset(a, x)

  ElseIf oc = &h7 Then
    ' PRINT_ADDR
    print_zstring(a)

  ElseIf oc = &h9 Then
    ' REMOVE_OBJ
    x = orel(a, PARENT)       ' parent of object
    y = orel(a, SIBLING)      ' sibling of object
    z = orel(x, CHILD)        ' first child of parent
    _ = orel(a, PARENT, 1, 0) ' object no longer has parent
    If z = a Then
      _ = orel(x, CHILD, 1, y) ' if object was first child then now its sibling is
    Else
      Do
        If orel(z, SIBLING) = a Then _ = orel(z, SIBLING, 1, y) : Exit Do
        z = orel(z, SIBLING)
      Loop Until z = 0
    EndIf

  ElseIf oc = &hA Then
    ' PRINT_OBJECT
    print_obj(a)

  ElseIf oc = &hB Then
    ' RET
    _return(a)

  ElseIf oc = &hC Then
    ' JUMP
    If a And &h8000 Then a = a - 65536
    pc = pc + a - 2

  ElseIf oc = &hD Then
    ' PRINT_PADDR
    print_zstring(a * 2)

  ElseIf oc = &hE Then
    ' LOAD
    ' Note special handing for op with indirect reference to the stack pointer
    If a = 0 Then
      x = pop()
      push(x)
    Else
      x = vget(a)
    EndIf
    vset(st, x)

  ElseIf oc = &hF Then
    ' NOT
    x = a Xor &b1111111111111111
    vset(st, x)

  Else
    ex_1op = E_UNKNOWN
  EndIf
End Function

Function ex_0op()
  Local x

  If oc = &h0 Then
    ' RTRUE
    _return(1)

  ElseIf oc = &h1 Then
    ' RFALSE
    _return(0)

  ElseIf oc = &h2 Then
    ' PRINT
    print_zstring(pc)

  ElseIf oc = &h3 Then
    ' PRINT_RET
    print_zstring(pc)
    Print
    _return(1)

  ElseIf oc = &h4 Then
    ' NOP

  ElseIf oc = &h5 Then
    ' SAVE
    x = zsave()

  ElseIf oc = &h6 Then
    ' RESTORE
    x = zsave(1)

  ElseIf oc = &h7 Then
    ' RESTART
    ex_restart()
    ex_0op = E_UNIMPLEMENTED

  ElseIf oc = &h8 Then
    ' RET_POPPED
    x = pop()
    _return(x)

  ElseIf oc = &h9 Then
    ' POP - discards top item in stack
    x = pop()

  ElseIf oc = &hA Then
    ' QUIT
    ex_0op = E_QUIT

  ElseIf oc = &hB Then
    ' NEW_LINE
    Print

  ElseIf oc = &hC Then
    ' SHOW_STATUS
    ex_show_status()
    ex_0op = E_UNIMPLEMENTED

  ElseIf oc = &hD Then
    ' VERIFY
    ' Actually verifying the story checksum is pointless, always branch.
    _branch(1, br)

  Else
    ex_0op = E_UNKNOWN
  EndIf
End Function

Function ex_varop()
  Local x, _

  If oc = &h0 Then
    ' CALL
    _call(st)

  ElseIf oc = &h1 Then
    ' STOREW
    ww(oa(0) + 2 * oa(1), oa(2))

  ElseIf oc = &h2 Then
  ' STOREB
    wb(oa(0) + oa(1), oa(2))

  ElseIf oc = &h3 Then
    ' PUT_PROP
    put_prop(oa(0), oa(1), oa(2))

  ElseIf oc = &h4 Then
    ' READ
    Print "> ";
    ex_varop = _read(oa(0), oa(1))

  ElseIf oc = &h5 Then
    ' PRINT_CHAR
    Print Chr$(oa(0));

  ElseIf oc = &h6 Then
    ' PRINT_NUM
    x = oa(0)
    If x > 32767 Then x = x - 65536
    Print Str$(x);

  ElseIf oc = &h7 Then
    ' RANDOM
    x = oa(0)
    If x > 32767 Then x = x - 65536
    If x = 0 Then
      x = cmm2rnd(-1) ' Reseed from Timer
      x = 0
    ElseIf x < 0 Then
      x = cmm2rnd(Abs(x))
      x = 0
    Else
      x = 1 + Cint(cmm2rnd() * x)
    EndIf
    vset(st, x)

  ElseIf oc = &h8 Then
    ' PUSH
    push(oa(0))

  ElseIf oc = &h9 Then
    ' PULL
    x = pop()
    ' Note special handing for op with indirect reference to the stack pointer
    If oa(0) = 0 Then
      _ = pop()
      push(x)
    Else
      vset(oa(0), x)
    EndIf

  ElseIf oc = &h13 Then
    ' INPUT_STREAM
    ex_varop = E_UNIMPLEMENTED

  ElseIf oc = &h14 Then
    ' OUTPUT_STREAM
    ex_varop = E_UNIMPLEMENTED

  Else
    ex_varop = E_UNKNOWN
  EndIf
End Function

Sub _branch(z, br)
  Local x
  If Not (z = (br And &h10000) > 0) Then Exit Sub
  x = br And &hFFFF ' Bottom 16-bits
  If x = pc - 1 Then _return(1) : Exit Sub
  If x = pc - 2 Then _return(0) : Exit Sub
  pc = x
End Sub

Sub _return(x)
  Local st, _
  Do While sp > fp + 2 : _ = pop() : Loop
  pc = pop()
  st = pop()
  fp = pop()
  vset(st, x)
  If ztrace Then dmp_stack()
End Sub

Sub _call(st)
  Local i, nl, x

  ' When address 0 is called, nothing happens and return value is false
  If oa(0) = 0 Then vset(st, 0) : Exit Sub

  push(fp)
  fp = sp
  push(st)
  push(pc)
  pc = 2 * oa(0)
  nl = rp() ' number of local variables
  push(nl)
  For i = 1 To nl
    x = rp() * 256 + rp()
    If i < onum Then push(oa(i)) Else push(x)
  Next i

  If ztrace Then dmp_routine(2 * oa(0)) : dmp_stack()
End Sub

Function _read(text_buf, parse_buf)
  Local ad, c, i, n, word$, s$, wc

  rtime = rtime - Timer

  s$ = LCase$(read_line$())
  If Left$(s$, 1) = "*" Then _read = ex_special(s$)

  rtime = rtime + Timer

  If _read <> E_OK Then Exit Function

  ' Write non-empty input to a script
  If script And S_WRITE Then If s$ <> "" Then Print #2, s$

  n = Len(s$)
  ' TODO: check for input too long
  For i = 1 To n : wb(text_buf + i, Peek(Var s$, i)) : Next i
  wb(text_buf + n + 1, 0)
  s$ = s$ + " "

  For i = 1 To n + 1
    c = Peek(Var s$, i)
    If c = &h20 Or Instr(DICT_SEP$(0), Chr$(c)) > 0 Then
      If Len(word$) > 0 Then
        ad = di_lookup(word$)
        ww(parse_buf + 2 + wc * 4, ad)
        wb(parse_buf + 4 + wc * 4, Len(word$))
        wb(parse_buf + 5 + wc * 4, i - Len(word$)) ' position in 'text_buf'
        wc = wc + 1
        word$ = ""
      EndIf
      If c <> &h20 Then ' ASCII code for space
        ad = di_lookup(Chr$(c))
        ww(parse_buf + 2 + wc * 4, ad)
        wb(parse_buf + 4 + wc * 4, 1)
        wb(parse_buf + 5 + wc * 4, i - 1) ' position in 'text_buf'
        wc = wc + 1
      EndIf
    Else
      word$ = word$ + Chr$(c)
    EndIf
  Next i

  wb(parse_buf + 1, wc)

End Function

Function read_line$()
  ' Read input from a script
  If script And S_READ Then
    Line Input #3, read_line$
    If read_line$ = "" Then script = script And S_WRITE Else Print read_line$
  EndIf

  ' Otherwise read input from the console
  If Not(script And S_READ) Then Line Input read_line$
End Function

Function ex_special(cmd$)
  Local f$, _

  If cmd$ = "*break" Then
    ex_special = E_BREAK

  ElseIf cmd$ = "*replay" Then
    Print "Select a script file from '"; ss$(SCRIPT_DIR); "/"; ss$(STORY); "':"
    f$ = file_choose$(ss$(SCRIPT_DIR) + "/" + ss$(STORY), "*.scr")
    If f$ <> "" Then
      Open f$ For Input As #3
      script = script Or S_READ
    EndIf
    Print ">";
    ex_special = E_REPEAT

  EndIf
End Function

Sub ex_show_status()
End Sub

Sub ex_restart()
End Sub
