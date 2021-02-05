' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

'!comment_if INLINE_CONSTANTS
Const E_OK = 0            ' last operation succeeded
Const E_UNKNOWN = 1       ' last operation unkown
Const E_UNIMPLEMENTED = 2 ' last operation implemented
Const E_BREAK = 3         ' TODO: document
Const E_QUIT = 4          ' last operation was a "Quit"
Const E_DEBUG = 5         ' TODO: document
Const E_REPEAT = 6        ' repeat last operation
'!endif

Dim fp      ' frame pointer; points to the stack element that is the first
            ' element for the current routine. This has the value &FFFF if
            ' we are not in a routine.
Dim num_bp  ' numer of active breakpoints
Dim num_ops ' number of instructions processed
Dim ztrace  ' is instruction tracing enabled? 0 or 1

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
    cout("Unsupported instruction " + fmt_hex$(op, 2)) : endl()
  ElseIf exec = E_UNIMPLEMENTED Then
    cout("Unimplemented instruction " + fmt_hex$(op, 2)) : endl()
  EndIf

  If exec <> E_OK Then pc = pc_old : sp = sp_old

End Function

Function ex_2op()
  Local a, b, x, y, _

  a = oa(0)
  b = oa(1)

  If oc = &h1 Then
    ' JE
    x = (a = b)
    If (Not x) And (onum > 2) Then x = (a = oa(2))
    If (Not x) And (onum > 3) Then x = (a = oa(3))
    ex_branch(x, br)

  ElseIf oc = &h2 Then
    ' JL
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    ex_branch(a < b, br)

  ElseIf oc = &h3 Then
    ' JG
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    ex_branch(a > b, br)

  ElseIf oc = &h4 Then
    ' DEC_CHK
    x = vget(a)
    If x > 32767 Then x = x - 65536
    If b > 32767 Then b = b - 65536
    x = x - 1
    y = x < b
    If x < 0 Then x = 65536 + x
    vset(a, x)
    ex_branch(y, br)

  ElseIf oc = &h5 Then
    ' INC_CHK
    x = vget(a)
    If x > 32767 Then x = x - 65536
    If b > 32767 Then b = b - 65536
    x = x + 1
    y = x > b
    If x < 0 Then x = 65536 + x
    vset(a, x)
    ex_branch(y, br)

  ElseIf oc = &h6 Then
    ' JIN
    x = ob_rel(a, PARENT)
    ex_branch(x = b, br)

  ElseIf oc = &h7 Then
    ' TEST
    ex_branch((a And b) = b, br)

  ElseIf oc = &h8 Then
    ' OR
    vset(st, a Or b)

  ElseIf oc = &h9 Then
    ' AND
    vset(st, a And b)

  ElseIf oc = &hA Then
    ' TEST_ATTR: a = object, b = attribute
    x = ob_attr(a, b)
    ex_branch(x = 1, br)

  ElseIf oc = &hB Then
    ' SET_ATTR
    _ = ob_attr(a, b, 1, 1)

  ElseIf oc = &hC Then
    ' CLEAR_ATTR
    _ = ob_attr(a, b, 1, 0)

  ElseIf oc = &hD Then
    ' STORE
    ' Note special handing for op with indirect reference to the stack pointer
    If a = 0 Then st_poke(sp - 1, b) Else vset(a, b)

  ElseIf oc = &hE Then
    ' INSERT_OBJ: a = object, b = destination
    ob_insert(a, b)

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
    x = ob_prop_get(a, b)
    vset(st, x)

  ElseIf oc = &h12 Then
    ' GET_PROP_ADDR
    x = ob_prop_addr(a, b)
    vset(st, x)

  ElseIf oc = &h13 Then
    ' GET_NEXT_PROP
    x = ob_next_prop(a, b)
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
  Local a, x

  a = oa(0)

  If oc = &h0 Then
    ' JZ
    ex_branch(a = 0, br)

  ElseIf oc = &h1 Then
    ' GET_SIBLING
    x = ob_rel(a, SIBLING)
    vset(st, x)
    ex_branch(x <> 0, br)

  ElseIf oc = &h2 Then
    ' GET_CHILD
    x = ob_rel(a, CHILD)
    vset(st, x)
    ex_branch(x <> 0, br)

  ElseIf oc = &h3 Then
    ' GET_PARENT
    x = ob_rel(a, PARENT)
    vset(st, x)

  ElseIf oc = &h4 Then
    ' GET_PROP_LEN
    x = ob_prop_len(a)
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
    ob_remove(a)

  ElseIf oc = &hA Then
    ' PRINT_OBJECT
    ob_print(a)

  ElseIf oc = &hB Then
    ' RET
    ex_return(a)

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
    If a = 0 Then x = st_peek(sp - 1) Else x = vget(a)
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
    ex_return(1)

  ElseIf oc = &h1 Then
    ' RFALSE
    ex_return(0)

  ElseIf oc = &h2 Then
    ' PRINT
    print_zstring(pc)

  ElseIf oc = &h3 Then
    ' PRINT_RET
    print_zstring(pc)
    endl()
    ex_return(1)

  ElseIf oc = &h4 Then
    ' NOP

  ElseIf oc = &h5 Then
    ' SAVE
    If script AND S_READ Then
      cout("IGNORED 'save' command read from script") : endl()
    Else
      x = zsave()
      ex_branch(x, br)
    EndIf

  ElseIf oc = &h6 Then
    ' RESTORE
    If script AND S_READ Then
      cout("IGNORED 'restore' command read from script") : endl()
    Else
      x = zsave(1)
    EndIf

  ElseIf oc = &h7 Then
    ' RESTART
    If script AND S_READ Then
      cout("IGNORED 'restart' command read from script") : endl()
    Else
      main_init()
      For x = 0 To 10 : endl() : Next x ' this will clear the console
    EndIf

  ElseIf oc = &h8 Then
    ' RET_POPPED
    x = st_pop()
    ex_return(x)

  ElseIf oc = &h9 Then
    ' POP - discards top item in stack
    sp = sp - 1

  ElseIf oc = &hA Then
    ' QUIT
    ex_0op = E_QUIT

  ElseIf oc = &hB Then
    ' NEW_LINE
    endl()

  ElseIf oc = &hC Then
    ' SHOW_STATUS
    ex_show_status()

  ElseIf oc = &hD Then
    ' VERIFY
    ' Actually verifying the story checksum is pointless, always branch.
    ex_branch(1, br)

  Else
    ex_0op = E_UNKNOWN
  EndIf
End Function

Function ex_varop()
  Local x, _

  If oc = &h0 Then
    ' CALL
    ex_call(st)

  ElseIf oc = &h1 Then
    ' STOREW
    ww(oa(0) + 2 * oa(1), oa(2))

  ElseIf oc = &h2 Then
  ' STOREB
    wb(oa(0) + oa(1), oa(2))

  ElseIf oc = &h3 Then
    ' PUT_PROP
    ob_prop_set(oa(0), oa(1), oa(2))

  ElseIf oc = &h4 Then
    ' READ
    ex_varop = ex_read(oa(0), oa(1))

  ElseIf oc = &h5 Then
    ' PRINT_CHAR
    cout(Chr$(oa(0)))

  ElseIf oc = &h6 Then
    ' PRINT_NUM
    x = oa(0)
    If x > 32767 Then x = x - 65536
    cout(Str$(x))

  ElseIf oc = &h7 Then
    ' RANDOM
    x = oa(0)
    If x > 32767 Then x = x - 65536
    x = ex_random(x)
    vset(st, x)

  ElseIf oc = &h8 Then
    ' PUSH
    st_push(oa(0))

  ElseIf oc = &h9 Then
    ' PULL
    x = st_pop()
    ' Note special handing for op with indirect reference to the stack pointer
    If oa(0) = 0 Then st_poke(sp - 1, x) Else vset(oa(0), x)

  ElseIf oc = &h13 Then
    ' INPUT_STREAM
    ex_varop = E_UNIMPLEMENTED

  ElseIf oc = &h14 Then
    ' OUTPUT_STREAM
    ex_varop = E_UNIMPLEMENTED

  ElseIf oc = &h15 Then
    ' SOUND_EFFECT
    ' TODO

  Else
    ex_varop = E_UNKNOWN
  EndIf
End Function

Sub ex_branch(z, br)
  Local x
  If Not (z = (br And &h80000) > 0) Then Exit Sub
  x = br And &h7FFFF ' Bottom 19-bits
  If x = pc - 1 Then ex_return(1) : Exit Sub
  If x = pc - 2 Then ex_return(0) : Exit Sub
  pc = x
End Sub

Sub ex_return(x)
  Local st, _
  sp = fp + 4
  pc = st_pop() * &h10000 + st_pop()
  st = st_pop()
  fp = st_pop()
  vset(st, x)
  If ztrace Then dmp_stack()
End Sub

Sub ex_call(st)
  Local i, nl, x

  ' When address 0 is called, nothing happens and return value is false
  If oa(0) = 0 Then vset(st, 0) : Exit Sub

  st_push(fp)
  fp = sp - 1
  st_push(st)
  st_push(pc Mod &h10000)
  st_push(pc \ &h10000)
  pc = 2 * oa(0)
  nl = rp() ' number of local variables
  st_push(nl)
  For i = 1 To nl
    x = rp() * 256 + rp()
    If i < onum Then st_push(oa(i)) Else st_push(x)
  Next i

  If ztrace Then dmp_routine(2 * oa(0)) : dmp_stack()
End Sub

Function ex_read(text_buf, parse_buf)
  Local ad, c, i, n, word$, s$, t, wc

  t = Timer

  s$ = LCase$(cin$("> ", 1))
  If Left$(s$, 1) = "*" Then ex_read = ex_special(s$)

  Timer = t

  If ex_read <> E_OK Then Exit Function

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

Function ex_special(cmd$)
  Local f$, _

  ex_special = E_REPEAT

  If cmd$ = "*break" Then
'!comment_if NO_DEBUG
    ex_special = E_BREAK
'!endif
'!uncomment_if NO_DEBUG
'    ex_special = E_OK
'!endif

  ElseIf cmd$ = "*credits" Then
    cecho(ss$(RESOURCES_DIR) + "\credits.txt")

  ElseIf cmd$ = "*replay" Then
    If script AND S_READ Then
      cout("IGNORED '*replay' command read from script")
    Else
      cout("Select a script file from '")
      cout(ss$(SCRIPT_DIR) + "\" + ss$(STORY_FILE) + "':")
      endl()
      f$ = fi_choose$(ss$(SCRIPT_DIR) + "\" + ss$(STORY_FILE), "*.scr")
      If f$ <> "" Then
        Open f$ For Input As #3
        script = script Or S_READ
      EndIf
    EndIf
    endl()

  ElseIf cmd$ = "*screenshot" Then
    ex_screenshot()

  ElseIf cmd$ = "*status" Then
    ex_show_status()

  Else
    ' Let the game's parser deal with the command
    ex_special = E_OK

  EndIf

  If ex_special = E_REPEAT Then cout(">")

End Function

Function ex_random(range)
'!comment_if TARGET_CMM1
  Static x = 7
  Static a = 1103515245 ' a, c and m are same values as used by glibc
  Static c = 12345
  Static m = 2^31

  If range = 0 Then
    x = Timer
    Exit Function
  ElseIf range < 0 Then
    x = Abs(range)
    Exit Function
  EndIf

  x = (a * x + c) Mod m

  ex_random = 1 + CInt((range - 1) * (x / m))
'!endif
'!uncomment_if TARGET_CMM1
'  If range = 0 Then
'    Randomize Timer
'    Exit Function
'  ElseIf range < 0 Then
'    Randomize Abs(range)
'    Exit Function
'  EndIf
'
'  ex_random = 1 + CInt((range - 1) * Rnd())
'!endif
End Function

Sub ex_screenshot()
  Local f$, i = 1
  Do
    f$ = "screen" + Str$(i) + ".bmp"
    If Dir$(f$) = "" Then Exit Do
    i = i + 1
  Loop
'!comment_if TARGET_CMM1
  Save Image f$
'!endif
'!uncomment_if TARGET_CMM1
'  SaveBmp f$
'!endif
  cout("Saved '" + Cwd$ + "/" + f$ + "'") : endl()
  endl()
End Sub

Sub ex_show_status()
  Local x
  x = vget(&h10) : ob_print(x) : cout(", ")
  x = vget(&h11) : cout("Score: " + Str$(x) + ", ")
  x = vget(&h12) : cout("Moves: " + Str$(x))
  endl()
End Sub

