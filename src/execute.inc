' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.requires("file")
sys.requires("string")
sys.requires("script")
sys.requires("objects")
sys.provides("execute")
If sys.err$ <> "" Then Error sys.err$
'!endif

Const E_OK = 0            ' last operation succeeded
Const E_UNKNOWN = 1       ' last operation unkown
Const E_UNIMPLEMENTED = 2 ' last operation implemented
Const E_BREAK = 3         ' TODO: document
Const E_QUIT = 4          ' last operation was a "Quit"
Const E_DEBUG = 5         ' TODO: document
Const E_REPEAT = 6        ' repeat last operation

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
  Inc num_ops
  Select Case op
    Case < &h80
      exec = ex_2op()
    Case < &hB0
      exec = ex_1op()
    Case < &hC0
      exec = ex_0op()
    Case < &hE0
      exec = ex_2op()
    Case Else
      exec = ex_varop()
  End Select

  Select Case exec
    Case E_UNKNOWN
      con.println("Unsupported instruction " + fmt_hex$(op, 2))
    Case E_UNIMPLEMENTED
      con.println("Unimplemented instruction " + fmt_hex$(op, 2))
  End Select

  If exec <> E_OK Then pc = pc_old : sp = sp_old

End Function

Function ex_2op()
  Local a, b, x, y, _

  a = oa(0)
  b = oa(1)

  Select Case oc
    Case &h1
      ' JE
      x = (a = b)
      If (Not x) And (onum > 2) Then x = (a = oa(2))
      If (Not x) And (onum > 3) Then x = (a = oa(3))
      ex_branch(x, br)

    Case &h2
      ' JL
      If a > 32767 Then Inc a, -65536
      If b > 32767 Then Inc b, -65536
      ex_branch(a < b, br)

    Case &h3
      ' JG
      If a > 32767 Then Inc a, -65536
      If b > 32767 Then Inc b, -65536
      ex_branch(a > b, br)

    Case &h4
      ' DEC_CHK
      x = vget(a)
      If x > 32767 Then Inc x, -65536
      If b > 32767 Then Inc b, -65536
      Inc x, -1
      y = x < b
      If x < 0 Then Inc x, 65536
      vset(a, x)
      ex_branch(y, br)

    Case &h5
      ' INC_CHK
      x = vget(a)
      If x > 32767 Then Inc x, -65536
      If b > 32767 Then Inc b, -65536
      Inc x
      y = x > b
      If x < 0 Then Inc x, 65536
      vset(a, x)
      ex_branch(y, br)

    Case &h6
      ' JIN
      x = ob_rel(a, PARENT)
      ex_branch(x = b, br)

    Case &h7
      ' TEST
      ex_branch((a And b) = b, br)

    Case &h8
      ' OR
      vset(st, a Or b)

    Case &h9
      ' AND
      vset(st, a And b)

    Case &hA
      ' TEST_ATTR: a = object, b = attribute
      x = ob_attr(a, b)
      ex_branch(x = 1, br)

    Case &hB
      ' SET_ATTR
      _ = ob_attr(a, b, 1, 1)

    Case &hC
      ' CLEAR_ATTR
      _ = ob_attr(a, b, 1, 0)

    Case &hD
      ' STORE
      ' Note special handing for op with indirect reference to the stack pointer
      If a = 0 Then st_poke(sp - 1, b) Else vset(a, b)

    Case &hE
      ' INSERT_OBJ: a = object, b = destination
      ob_insert(a, b)

    Case &hF
      ' LOADW
      x = rw(a + 2 * b)
      vset(st, x)

    Case &h10
      ' LOADB
      x = rb(a + b)
      vset(st, x)

    Case &h11
      ' GET_PROP
      x = ob_prop_get(a, b)
      vset(st, x)

    Case &h12
      ' GET_PROP_ADDR
      x = ob_prop_addr(a, b)
      vset(st, x)

    Case &h13
      ' GET_NEXT_PROP
      x = ob_next_prop(a, b)
      vset(st, x)

    Case < &h19
      If a > 32767 Then Inc a, -65536
      If b > 32767 Then Inc b, -65536

      Select Case oc
        Case &h14
          ' ADD
          x = a + b
        Case &h15
          ' SUB
          x = a - b
        Case &h16
          ' MUL
          x = a * b
        Case &h17
          ' DIV
          x = a \ b
        Case Else
          ' MOD
          x = a Mod b
      End Select

      If x < 0 Then Inc x, 65536
      vset(st, x)

    Case Else
      ex_2op = E_UNKNOWN
  End Select
End Function

Function ex_1op()
  Local a, x

  a = oa(0)

  Select Case oc
    Case &h0
      ' JZ
      ex_branch(a = 0, br)

    Case &h1
      ' GET_SIBLING
      x = ob_rel(a, SIBLING)
      vset(st, x)
      ex_branch(x <> 0, br)

    Case &h2
      ' GET_CHILD
      x = ob_rel(a, CHILD)
      vset(st, x)
      ex_branch(x <> 0, br)

    Case &h3
      ' GET_PARENT
      x = ob_rel(a, PARENT)
      vset(st, x)

    Case &h4
      ' GET_PROP_LEN
      x = ob_prop_len(a)
      vset(st, x)

    Case &h5
      ' INC
      x = vget(a)
      If x > 32767 Then Inc x, -65536
      Inc x
      If x < 0 Then Inc x, 65536
      vset(a, x)

    Case &h6
      ' DEC
      x = vget(a)
      If x > 32767 Then Inc x, -65536
      Inc x, -1
      If x < 0 Then Inc x, 65536
      vset(a, x)

    Case &h7
      ' PRINT_ADDR
      print_zstring(a)

    Case &h9
      ' REMOVE_OBJ
      ob_remove(a)

    Case &hA
      ' PRINT_OBJECT
      ob_print(a)

    Case &hB
      ' RET
      ex_return(a)

    Case &hC
      ' JUMP
      If a And &h8000 Then Inc a, -65536
      Inc pc, a - 2

    Case &hD
      ' PRINT_PADDR
      print_zstring(a * 2)

    Case &hE
      ' LOAD
      ' Note special handing for op with indirect reference to the stack pointer
      If a = 0 Then x = st_peek(sp - 1) Else x = vget(a)
      vset(st, x)

    Case &hF
      ' NOT
      x = a Xor &b1111111111111111
      vset(st, x)

    Case Else
      ex_1op = E_UNKNOWN
  End Select
End Function

Function ex_0op()
  Local x

  Select Case oc
    Case &h0
      ' RTRUE
      ex_return(1)

    Case &h1
      ' RFALSE
      ex_return(0)

    Case &h2
      ' PRINT
      print_zstring(pc)

    Case &h3
      ' PRINT_RET
      print_zstring(pc)
      con.endl()
      ex_return(1)

    Case &h4
      ' NOP

    Case &h5
      ' SAVE
      If con.fd_in Then
        con.println("IGNORED 'save' command read from script")
      Else
        x = zsave()
        ex_branch(x, br)
      EndIf

    Case &h6
      ' RESTORE
      If con.fd_in Then
        con.println("IGNORED 'restore' command read from script")
      Else
        x = zsave(1)
      EndIf

    Case &h7
      ' RESTART
      If con.fd_in Then
        con.println("IGNORED 'restart' command read from script")
      Else
        main_init()
        For x = 0 To 10 : con.endl() : Next ' this will clear the console
      EndIf

    Case &h8
      ' RET_POPPED
      x = st_pop()
      ex_return(x)

    Case &h9
      ' POP - discards top item in stack
      Inc sp, -1

    Case &hA
      ' QUIT
      ex_0op = E_QUIT

    Case &hB
      ' NEW_LINE
      con.endl()

    Case &hC
      ' SHOW_STATUS
      ex_show_status()

    Case &hD
      ' VERIFY
      ' Actually verifying the story checksum is pointless, always branch.
      ex_branch(1, br)

    Case Else
      ex_0op = E_UNKNOWN
  End Select
End Function

Function ex_varop()
  Local x, _

  Select Case oc
    Case &h0
      ' CALL
      ex_call(st)

    Case &h1
      ' STOREW
      ww(oa(0) + 2 * oa(1), oa(2))

    Case &h2
    ' STOREB
      wb(oa(0) + oa(1), oa(2))

    Case &h3
      ' PUT_PROP
      ob_prop_set(oa(0), oa(1), oa(2))

    Case &h4
      ' READ
      ex_varop = ex_read(oa(0), oa(1))

    Case &h5
      ' PRINT_CHAR
      con.print(Chr$(oa(0)))

    Case &h6
      ' PRINT_NUM
      x = oa(0)
      If x > 32767 Then Inc x, -65536
      con.print(Str$(x))

    Case &h7
      ' RANDOM
      x = oa(0)
      If x > 32767 Then Inc x, -65536
      x = ex_random(x)
      vset(st, x)

    Case &h8
      ' PUSH
      st_push(oa(0))

    Case &h9
      ' PULL
      x = st_pop()
      ' Note special handing for op with indirect reference to the stack pointer
      If oa(0) = 0 Then st_poke(sp - 1, x) Else vset(oa(0), x)

    Case &h13
      ' INPUT_STREAM
      ex_varop = E_UNIMPLEMENTED

    Case &h14
      ' OUTPUT_STREAM
      ex_varop = E_UNIMPLEMENTED

    Case &h15
      ' SOUND_EFFECT
      ' TODO

    Case Else
      ex_varop = E_UNKNOWN
  End Select
End Function

Sub ex_branch(z, br)
  Local x
  If Not (z = (br And &h80000) > 0) Then Exit Sub
  x = br And &h7FFFF ' Bottom 19-bits
  Select Case x
    Case pc - 1
      ex_return(1)
    Case pc - 2
      ex_return(0)
    Case Else
      pc = x
  End Select
End Sub

Sub ex_return(x)
  Local st, _
  sp = fp + 4
  ' This single line version won't work if inlining st_pop():
  ' pc = st_pop() * &h10000 + st_pop()
  pc = &h10000 * st_pop()
  Inc pc, st_pop()
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
  Next

  If ztrace Then dmp_routine(2 * oa(0)) : dmp_stack()
End Sub

Function ex_read(text_buf, parse_buf)
  Local ad, c, i, n, word$, s$, t, wc

  ex_show_status()

  t = Timer

  s$ = LCase$(con.in$("> ", 1))
  If Left$(s$, 1) = "*" Then ex_read = ex_special(s$)

  Timer = t

  If ex_read <> E_OK Then Exit Function

  n = Len(s$)
  ' TODO: check for input too long
  For i = 1 To n : wb(text_buf + i, Peek(Var s$, i)) : Next
  wb(text_buf + n + 1, 0)
  Cat s$, " "

  For i = 1 To n + 1
    c = Peek(Var s$, i)
    If c = &h20 Or Instr(DICT_SEP$(0), Chr$(c)) > 0 Then
      If Len(word$) > 0 Then
        ad = di_lookup(word$)
        ww(parse_buf + 2 + wc * 4, ad)
        wb(parse_buf + 4 + wc * 4, Len(word$))
        wb(parse_buf + 5 + wc * 4, i - Len(word$)) ' position in 'text_buf'
        Inc wc
        word$ = ""
      EndIf
      If c <> &h20 Then ' ASCII code for space
        ad = di_lookup(Chr$(c))
        ww(parse_buf + 2 + wc * 4, ad)
        wb(parse_buf + 4 + wc * 4, 1)
        wb(parse_buf + 5 + wc * 4, i - 1) ' position in 'text_buf'
        Inc wc
      EndIf
    Else
      Cat word$, Chr$(c)
    EndIf
  Next

  wb(parse_buf + 1, wc)

End Function

Function ex_special(cmd$)
  Local f$, _

  ex_special = E_REPEAT

  Select Case Field$(cmd$, 1, " ")

    Case "*break"
'!comment_if NO_DEBUG
      ex_special = E_BREAK
'!endif
'!uncomment_if NO_DEBUG
'      ex_special = E_OK
'!endif

    Case "*credits"
      con.print_file(file.resolve$(ss$(RESOURCES_DIR), "credits.txt"), 1)

    Case "*eof"
      con.println("End of script.")
      con.endl()

    Case "*font"
      ex_special = ex_font(cmd$)

    Case "*more"
      Select Case Field$(cmd$, 2, " ")
        Case "", "on"
          con.more = 1
          con.println("Paged output enabled.")
        Case "off"
          con.more = 0
          con.println("Paged output disabled.")
        Case Else
          con.println("Invalid '*more' command.")
      End Select
      con.endl()

    Case "*record"
      Select Case Field$(cmd$, 2, " ")
        Case "", "on"
          script.record_on()
        Case "off"
          script.record_off()
        Case Else
          con.println("Invalid '*record' command.")
      End Select
      con.endl()

    Case "*replay"
      Select Case Field$(cmd$, 2, " ")
        Case "", "on"
          script.replay_on()
        Case "off"
          script.replay_off()
        Case Else
          con.println("Invalid '*replay' command.")
      End Select
      con.endl()

    Case "*screenshot"
      ex_screenshot()

    Case "*spin"
      Select Case Field$(cmd$, 2, " ")
        Case "", "on"
          con.spin_enabled = 1
          con.println("Spinning cursor enabled.")
        Case "off"
          con.spin_enabled = 0
          con.println("Spinning cursor disabled.")
        Case Else
          con.println("Invalid '*spin' command.")
      End Select
      con.endl()

    Case "*status"
      ex_show_status(1)

    Case "*save"
      If zsave(0) Then con.println("Ok.") Else con.println("Save failed.")
      con.endl()

    Case "*restore"
      If zsave(1) Then con.println("Ok.") Else con.println("Restore failed.")
      con.endl()

    Case Else
      ' Let the game's parser deal with the command
      ex_special = E_OK

  End Select

  If ex_special = E_REPEAT Then con.print(">")

End Function

Function ex_random(range)
  Static x = 7
  Static a = 1103515245 ' a, c and m are same values as used by glibc
  Static c = 12345
  Static m = 2^31

  Select Case range
    Case 0
      x = Timer
      Exit Function
    Case < 0
      x = Abs(range)
      Exit Function
  End Select

  x = (a * x + c) Mod m

  ex_random = 1 + CInt((range - 1) * (x / m))
End Function

' @param  on_console%  If 1 then as well as updating the status line also write
'                      the status to the console.
Sub ex_show_status(on_console%)
  Local s$(1) Length 120, x, y

  con.get_pos(x, y)

  ' Capture the room name.
  con.disable_flush% = 1
  ob_print(vget(&h10))
  s$(0) = Left$(con.buf$, 120)
  con.buf$ = ""
  con.disable_flush% = 0

  ' Truncate room name to console width.
  If Len(s$(0)) > con.WIDTH Then s$(0) = Left$(s$(0), con.WIDTH - 3) + "..."

  ' Construct the score/moves/time string.
  If rb(&h01) And &b00000010 Then
    s$(1) = "Time: " + Str$(vget(&h11)) + ":" + Str$(vget(&h12))
  Else
    s$(1) = "Score: " + Str$(vget(&h11)) + ", Moves: " + Str$(vget(&h12))
  EndIf

  If on_console% Then
    con.println(s$(0) + ", " + s$(1))
    con.println
  EndIf

  ' If the status line would be too long then try the short form.
  If Len(s$(0)) + Len(s$(1)) + 1 >= con.WIDTH Then
    If rb(&h01) And &b00000010 Then
      s$(1) = Str$(vget(&h11)) + ":" + Str$(vget(&h12))
    Else
      s$(1) = Str$(vget(&h11)) + "/" + Str$(vget(&h12))
    EndIf
  EndIf

  ' If still too long then don't display score/moves/time.
  If Len(s$(0)) + Len(s$(1)) + 1 >= con.WIDTH Then s$(1) = ""

  con.set_pos(0, 0)
  con.invert()
  con.print(s$(0))
  con.print(Space$(con.WIDTH - Len(s$(0)) - Len(s$(1))))
  con.println(s$(1))
  con.set_pos(x, y)
  con.invert()
End Sub

Sub ex_screenshot()
  Local f$, i = 0, root$ = Cwd$
  Do
    Inc i
    f$ = file.resolve$(root$, ss$(STORY_FILE) + "_" + Str$(i) + ".bmp")
  Loop While file.exists%(f$)
  If InStr(Mm.Device$, "PicoMite") Then
    On Error Skip
    Save Compressed Image f$
  Else
    On Error Skip
    Save Image f$
  EndIf
  If Mm.ErrNo Then
    con.println("Screenshot failed or is not supported.")
  Else
    con.println("Saved '" + f$ + "'")
  EndIf
  con.endl()
End Sub

Function ex_font(cmd$)
  ex_font = E_REPEAT
  If Field$(cmd$, 3, " ") <> "" Then
    con.println("Invalid '*font' command.")
    con.endl()
  Else
    Select Case Field$(cmd$, 2, " ")
      Case "small"
        con.set_font(7)
        ex_font = E_OK
      Case "medium"
        con.set_font(1)
        ex_font = E_OK
      Case Else
        con.println("Unknown font.")
        con.endl()
    End Select
  EndIf

  if ex_font = E_OK Then cmd$ = "look"
End Function
