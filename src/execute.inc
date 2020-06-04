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

  ' JE
  If oc = &h1 Then
    x = (a = b)
    If (Not x) And (onum > 2) Then x = (a = oa(2))
    If (Not x) And (onum > 3) Then x = (a = oa(3))
    _branch(x, br)

  ' JL
  ElseIf oc = &h2 Then
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    _branch(a < b, br)

  ' JG
  ElseIf oc = &h3 Then
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    _branch(a > b, br)

  ' DEC_CHK
  ElseIf oc = &h4 Then
    x = vget(a)
    If x > 32767 Then x = x - 65536
    If b > 32767 Then b = b - 65536
    x = x - 1
    y = x < b
    If x < 0 Then x = 65536 + x
    vset(a, x)
    _branch(y, br)

  ' INC_CHK
  ElseIf oc = &h5 Then
    x = vget(a)
    If x > 32767 Then x = x - 65536
    If b > 32767 Then b = b - 65536
    x = x + 1
    y = x > b
    If x < 0 Then x = 65536 + x
    vset(a, x)
    _branch(y, br)

  ' JIN
  ElseIf oc = &h6 Then
    x = orel(a, PARENT)
    _branch(x = b, br)

  ' TEST
  ElseIf oc = &h7 Then
    _branch((a And b) = b, br)

  ' OR
  ElseIf oc = &h8 Then
    vset(st, a Or b)

  ' AND
  ElseIf oc = &h9 Then
    vset(st, a And b)

  ' TEST_ATTR: a = object, b = attribute
  ElseIf oc = &hA Then
    x = oattr(a, b)
    _branch(x = 1, br)

  ' SET_ATTR
  ElseIf oc = &hB Then
    _ = oattr(a, b, 1, 1)

  ' CLEAR_ATTR
  ElseIf oc = &hC Then
    _ = oattr(a, b, 1, 0)

  ' STORE
  ElseIf oc = &hD Then
    ' Note special handing for op with indirect reference to the stack pointer
    If a = 0 Then stack(sp) = b Else vset(a, b)

  ' INSERT_OBJ: a = object, b = destination
  ElseIf oc = &hE Then
    x = orel(b, CHILD)         ' original child of destination
    y = orel(a, SIBLING)       ' original sibling of object
    z = orel(a, PARENT)        ' original parent of object
    _ = orel(b, CHILD, 1, a)   ' object is new child of destination
    _ = orel(a, PARENT, 1, b)  ' destination is new parent of object
    _ = orel(a, SIBLING, 1, x) ' original child of destination is new sibling of object
    _ = orel(z, CHILD, 1, y)   ' original sibling of object is new child of original parent

  ' LOADW
  ElseIf oc = &hF Then
    x = rw(a + 2 * b)
    vset(st, x)

  ' LOADB
  ElseIf oc = &h10 Then
    x = rb(a + b)
    vset(st, x)

  ' GET_PROP
  ElseIf oc = &h11 Then
    x = get_prop(a, b)
    vset(st, x)

  ' GET_PROP_ADDR
  ElseIf oc = &h12 Then
    x = get_prop_addr(a, b)
    vset(st, x)

  ' GET_NEXT_PROP
  ElseIf oc = &h13 Then
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

  ' JZ
  If oc = &h0 Then
    _branch(a = 0, br)

  ' GET_SIBLING
  ElseIf oc = &h1 Then
    x = orel(a, SIBLING)
    vset(st, x)
    _branch(x <> 0, br)

  ' GET_CHILD
  ElseIf oc = &h2 Then
    x = orel(a, CHILD)
    vset(st, x)
    _branch(x <> 0, br)

  ' GET_PARENT
  ElseIf oc = &h3 Then
    x = orel(a, PARENT)
    vset(st, x)

  ' GET_PROP_LEN
  ElseIf oc = &h4 Then
    x = get_prop_len(a)
    vset(st, x)

  ' INC
  ElseIf oc = &h5 Then
    x = vget(a)
    If x > 32767 Then x = x - 65536
    x = x + 1
    If x < 0 Then x = 65536 + x
    vset(a, x)

  ' DEC
  ElseIf oc = &h6 Then
    x = vget(a)
    If x > 32767 Then x = x - 65536
    x = x - 1
    If x < 0 Then x = 65536 + x
    vset(a, x)

  ' PRINT_ADDR
  ElseIf oc = &h7 Then
    print_zstring(a)

  ' REMOVE_OBJ
  ElseIf oc = &h9 Then
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

  ' PRINT_OBJECT
  ElseIf oc = &hA Then
    print_obj(a)

  ' RET
  ElseIf oc = &hB Then
    _return(a)

  ' JUMP
  ElseIf oc = &hC Then
    If a And &h8000 Then a = a - 65536
    pc = pc + a - 2

  ' PRINT_PADDR
  ElseIf oc = &hD Then
    print_zstring(a * 2)

  ' LOAD
  ElseIf oc = &hE Then
    ' Note special handing for op with indirect reference to the stack pointer
    If a = 0 Then x = stack(sp) Else x = vget(a)
    vset(st, x)

  ' NOT
  ElseIf oc = &hF Then
    x = a Xor &b1111111111111111
    vset(st, x)

  Else
    ex_1op = E_UNKNOWN
  EndIf
End Function

Function ex_0op()
  Local x

  ' RTRUE
  If oc = &h0 Then
    _return(1)

  ' RFALSE
  ElseIf oc = &h1 Then
    _return(0)

  ' PRINT
  ElseIf oc = &h2 Then
    print_zstring(pc)

  ' PRINT_RET
  ElseIf oc = &h3 Then
    print_zstring(pc)
    Print
    _return(1)

  ' RET_POPPED
  ElseIf oc = &h8 Then
    x = pop()
    _return(x)

  ' POP - discards top item in stack
  ElseIf oc = &h9 Then
    x = pop()

  ' QUIT
  ElseIf oc = &hA Then
    ex_0op = E_QUIT

  ' NEW_LINE
  ElseIf oc = &hB Then
    Print

  ' SHOW_STATUS
  ElseIf oc = &hC Then
    ex_0op = E_UNIMPLEMENTED

  ' VERIFY
  ElseIf oc = &hD Then
    ' Actually verifying the story checksum is pointless, always branch.
    _branch(1, br)

  Else
    ex_0op = E_UNKNOWN
  EndIf
End Function

Function ex_varop()
  Local x

  ' CALL
  If oc = &h0 Then
    _call(st)

  ' STOREW
  ElseIf oc = &h1 Then
    ww(oa(0) + 2 * oa(1), oa(2))

  ' STOREB
  ElseIf oc = &h2 Then
    wb(oa(0) + oa(1), oa(2))

  ' PUT_PROP
  ElseIf oc = &h3 Then
    put_prop(oa(0), oa(1), oa(2))

  ' READ
  ElseIf oc = &h4 Then
    Print " ";
    ex_varop = _read(oa(0), oa(1))

  ' PRINT_CHAR
  ElseIf oc = &h5 Then
    Print Chr$(oa(0));

  ' PRINT_NUM
  ElseIf oc = &h6 Then
    x = oa(0)
    If x > 32767 Then x = x - 65536
    Print Str$(x);

  ' RANDOM
  ElseIf oc = &h7 Then
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

  ' PUSH
  ElseIf oc = &h8 Then
    push(oa(0))

  ' PULL
  ElseIf oc = &h9 Then
    x = pop()
    ' Note special handing for op with indirect reference to the stack pointer
    If oa(0) = 0 Then stack(sp) = x Else vset(oa(0), x)

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
  Local st
  sp = fp - 1
  pc = stack(fp + 2)
  st = stack(fp + 1)
  fp = stack(fp)
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

' Lookup w$ in the vocabulary. w$ should already be in lower-case
Function lookup(w$)
  Local a, i, n, sz, z(3)

  ' Convert w$ into 4-byte Z-string
  sz = Len(w$) : If sz > 6 Then sz = 6
  For i = 1 To 6
    If i > sz Then a = a + 5 Else a = a + Instr(ALPHABET$(0), Mid$(w$, i, 1)) - 1
    If i = 3 Then
      z(0) = a \ 256 : z(1) = a And &hFF : a = 0
    ElseIf i = 6 Then ' End of word
      a = a Or &h8000 : z(2) = a \ 256 : z(3) = a And &hFF
    Else
      a = a * 32
    EndIf
  Next i

  ' Lookup Z-string in dictionary
  ' TODO: binary search instead of linear search
  a = rw(&h08)  ' dictionary address
  n = rb(a)     ' number of word separators
  a = a + n + 1 ' skip word separators
  sz = rb(a)    ' entry length
  n = rw(a + 1) ' number of entries
  a = a + 3

  For i = 1 To n
    If z(0)=rb(a) And z(1)=rb(a+1) And z(2)=rb(a+2) And z(3)=rb(a+3) Then
      lookup = a : Exit For
    EndIf
    a = a + sz
  Next i

End Function

Function _read(text_buf, parse_buf)
  Local ad, c, i, n, word$, s$, sep$, wc

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
  sep$ = " .," + Chr$(34)

  For i = 1 To n + 1
    c = Peek(Var s$, i)
    If Instr(sep$, Chr$(c)) > 0 Then
      If Len(word$) > 0 Then
        ad = lookup(word$)
        ww(parse_buf + 2 + wc * 4, ad)
        wb(parse_buf + 4 + wc * 4, Len(word$))
        wb(parse_buf + 5 + wc * 4, i - Len(word$)) ' position in 'text_buf'
        wc = wc + 1
        word$ = ""
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
    Print "Select a script file from 'A:/zmim/scripts/"; story$(0); ":"
    f$ = file_choose$("A:/zmim/scripts/" + story$(0), "*.scr")
    If f$ <> "" Then
      Open f$ For Input As #3
      script = script Or S_READ
    EndIf
    Print ">";
    ex_special = E_REPEAT

  EndIf
End Function
