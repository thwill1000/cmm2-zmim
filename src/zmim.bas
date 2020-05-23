' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Option Explicit On

#Include "memory.inc"
#Include "stack.inc"
#Include "variable.inc"
#Include "debug.inc"
#Include "util.inc"
#Include "dmp_abrv.inc"
#Include "dmp_dict.inc"
#Include "dmp_hdr.inc"
#Include "dmp_mmap.inc"
#Include "zstring.inc"
#Include "objects.inc"
#Include "decode.inc"

' If > 0 then produce debug output
' If bit 7 is set then print a new line before the current value of 'pc'
Dim debug = 0

Dim GLOBAL_VAR = 0

Dim BUSY$(1) LENGTH 16
BUSY$(0) = "\\\\||||////----"

Dim i = 0
Dim BIT(7)
For i = 0 To 7 : BIT(i) = 2 ^ i : Next i

Const BTM_2_BITS  = &b00000011
Const BTM_4_BITS  = &b00001111
Const BTM_5_BITS  = &b00011111
Const BTM_6_BITS  = &b00111111

Const E_OK = 0
Const E_UNKNOWN = 1
Const E_UNIMPLEMENTED = 2
Const E_BREAK = 3
Const E_QUIT = 4

' If > 0 then an error has occurred
Dim err = E_OK

Dim bp = 0 ' breakpoint address

Sub _2op
  Local a, b, x, _

  a = oa(0)
  b = oa(1)

  ' JE
  If oc = &h1 Then
    x = (a = b)
    If (Not x) And (onum = 3) Then x = (a = oa(2))
    If (Not x) And (onum = 4) Then x = (a = oa(3))
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
    x = vget(a) - 1
    If x < 0 Then x = &hFFFF
    vset(a, x)
    _branch(x < b, br)

  ' INC_CHK
  ElseIf oc = &h5 Then
    x = vget(a) + 1
    If x > &hFFFF Then x = 0
    vset(a, x)
    _branch(x > b, br)

  ' JIN
  ElseIf oc = &h6 Then
    x = orel(a, PARENT)
    _branch(x = b, br)

  ' TEST
  ElseIf oc = &h7 Then
    _branch(a And b = b, br)

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
    vset(a, b)

  ' INSERT_OBJ: a = object, b = destination
  ElseIf oc = &hE Then
    x = orel(b, CHILD)
    _ = orel(b, CHILD, 1, a)
    _ = orel(a, PARENT, 1, b)
    _ = orel(a, SIBLING, 1, x)

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
    err = E_UNIMPLEMENTED

  ' GET_NEXT_PROP
  ElseIf oc = &h13 Then
    err = E_UNIMPLEMENTED

  ElseIf oc < &h19 Then
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536

    ' ADD
    If oc = &h14 Then
      x = a + b
    ElseIf oc = &h15 Then
      x = a - b
    ElseIf oc = &h16 Then
      x = a * b
    ElseIf oc = &h17 Then
      x = a \ b
    Else
      err = E_UNIMPLEMENTED
    EndIf

    If x < 0 Then x = 65536 - x
    vset(st, x)

  Else
    err = E_UNKNOWN
  EndIf
End Sub

Sub _1op
  Local a, x

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
    err = E_UNIMPLEMENTED

  ' INC
  ElseIf oc = &h5 Then
    x = vget(a)
    If x > 32767 Then x = x - 65536
    x = x + 1
    If x < 0 Then x = 65536 - x
    vset(a, x)

  ' DEC
  ElseIf oc = &h6 Then
    x = vget(a)
    If x > 32767 Then x = x - 65536
    x = x - 1
    If x < 0 Then x = 65536 - x
    vset(a, x)

  ' PRINT_ADDR
  ElseIf oc = &h7 Then
    err = E_UNIMPLEMENTED

  ' REMOVE_OBJ
  ElseIf oc = &h9 Then
    err = E_UNIMPLEMENTED

  ' PRINT_OBJECT
  ElseIf oc = &hA Then
    print_obj(a)
    If debug Then debug = debug Or BIT(7)

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
    If debug Then debug = debug Or BIT(7)

  ' LOAD
  ElseIf oc = &hE Then
    err = E_UNIMPLEMENTED

  Else
    err = E_UNKNOWN
  EndIf
End Sub

Sub _0op
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
    If debug Then debug = debug Or BIT(7)

  ' PRINT_RET
  ElseIf oc = &h3 Then
    print_zstring(pc)
    err = E_UNIMPLEMENTED

  ' RET_POPPED
  ElseIf oc = &h8 Then
    x = pop()
    _return(x)

  ' NEWLINE
  ElseIf oc = &hB Then
'    If debug Then Print Else Print Chr$(8); " " : Print " ";
    Print

  Else
    err = E_UNKNOWN
  EndIf
End Sub

Sub _varop
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

  ' READ
  ElseIf oc = &h4 Then
'    If Not debug Then Print Chr$(8); " ";
    Print " ";
    _read(oa(0), oa(1))

  ' PRINT_CHAR
  ElseIf oc = &h5 Then
'    If debug Then
'      Print Chr$(oa(0));
'      debug = debug Or BIT(7)
'    Else
'      Print Chr$(8); Chr$(oa(0)); " ";
'    EndIf
    Print Chr$(oa(0));

  ' PRINT_NUM
  ElseIf oc = &h6 Then
'    If debug Then
'      Print Str$(oa(0));
'      debug = debug Or BIT(7)
'    Else
'      Print Chr$(8); Str$(oa(0)); " ";
'    EndIf
    Print Str$(oa(0));

  Else
    err = E_UNKNOWN
  EndIf
End Sub

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
  dmp_stack()
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

  dmp_routine(2 * oa(0))
  dmp_stack()
End Sub

Function execute()
  Local op, pc_old, sp_old

  err = E_OK

'  If debug Then
'    If debug And BIT(7) Then Print : debug = (debug And (BIT(7) Xor &hFF))
'    Print Hex$(pc); ": ";
'  Else
'    Print Chr$(8); Mid$(BUSY$(0), (num_ops Mod 16) + 1, 1);
'  EndIf

  ' Store the PC and SP in case we want to roll-back.
  pc_old = pc : sp_old = sp

  If pc <> bp Then
    op = decode()
    num_ops = num_ops + 1
    If op < &h80 Then
      _2op()
    ElseIf op < &hB0 Then
      _1op()
    ElseIf op < &hC0 Then
      _0op()
    ElseIf op < &hE0 Then
      _2op()
    Else
      _varop()
    EndIf
  EndIf

  execute = err
  If execute = E_UNKNOWN Then
    Print "Unsupported instruction "; Hex$(op)
  ElseIf execute = E_UNIMPLEMENTED Then
    Print "Unimplemented instruction "; Hex$(op)
  ElseIf pc = bp Then
    Print "[Breakpoint reached] - resetting bp = 0"
    bp = 0
    execute = E_BREAK
  EndIf
  If execute <> E_OK Then pc = pc_old : sp = sp_old

End Function

Function lookup(s$)
  Local b(3), i, sl, x

'  Print "lookup: *" + s$ + "* => ";

  s$ = LCase$(Left$(s$, 6))
  sl = Len(s$)

  ' Convert s$ into 4-byte Z-string
  For i = 1 To 6

    If i > sl Then
      x = x + 5
    Else
      x = x + Instr(ALPHABET$(0), Mid$(s$, i, 1)) - 1
    EndIf

    If i = 3 Then
      b(0) = x \ 256
      b(1) = x And &hFF
      x = 0
    ElseIf i = 6 Then
      x = x Or &h8000 ' End of word
      b(2) = x \ 256
      b(3) = x And &hFF
    Else
      x = x * 32
    EndIf

  Next i

  ' Lookup Z-string in dictionary
  ' TODO: binary search instead of linear search
  Local ad, n, sz, word(3)
  ad = rw(&h8) ' dictionary address
  n = rb(ad) ' number of word separators
  ad = ad + 1 + n ' skip word separators
  sz = rb(ad) : ad = ad + 1 ' entry length
  n  = rw(ad) : ad = ad + 2 ' number of entries
  For i = 1 To n
    word(0) = rb(ad) : ad = ad + 1
    word(1) = rb(ad) : ad = ad + 1
    word(2) = rb(ad) : ad = ad + 1
    word(3) = rb(ad) : ad = ad + 1
    ad = ad + sz - 4 ' skip (sz - 4) bytes of data
    If b(0) = word(0) And b(1) = word(1) And b(2) = word(2) And b(3) = word(3) Then
      lookup = ad - sz
      i = n + 1
    EndIf
  Next i

'  Print " => "; Hex$(lookup)
End Function

Sub _read(text_buf, parse_buf)
  Local c, i, n, word$, s$, sep$, wc, _

'  Print "text_buf = "; Hex$(text_buf)
'  Print "parse_buf = "; Hex$(parse_buf)

  Line Input s$
  s$ = LCase$(s$)

  If Left$(s$, 1) = "*" Then
    If s$ = "*break" Then
      err = E_BREAK
      Exit Sub
    EndIf
  EndIf

  n = Len(s$)
  ' TODO: check for input too long
  For i = 1 To n : wb(text_buf + i, Peek(Var s$, i)) : Next i
  wb(text_buf + n + 1, 0)
  s$ = s$ + " "
  sep$ = " .," + Chr$(34)

  For i = 1 To n + 1
    c = Peek(Var s$, i)
    If Instr(sep$, Chr$(c)) > 0 Then
      If Len(word$) > 0 Then _ = lookup(word$)
      'Print Hex$(_) ;
      'Print Len(word$);
      'Print i - Len(word$) - 1
      ww(parse_buf + 2 + wc * 4, _)
      wb(parse_buf + 4 + wc * 4, Len(word$))
      wb(parse_buf + 5 + wc * 4, i - Len(word$)) ' position in 'text_buf'
      wc = wc + 1
      word$ = ""
    Else
      word$ = word$ + Chr$(c)
    EndIf
  Next i
  wb(parse_buf + 1, wc)
'  dmp_mem(text_buf, 32)
'  dmp_mem(parse_buf, 32)

'  Print " ";
End Sub

' Interactive debugger
Function gdb()
  Local c, cmd$(9) Length 20, cn, i, op, pc_old, s$, sp_old

  ' Decode and display the next instruction but don't execute it.
  Print Hex$(pc); ": ";
  pc_old = pc : sp_old = sp
  debug = 1
  op = decode()
  debug = 0
  pc = pc_old : sp = sp_old

  ' Read line of input and parse into space separated commands/arguments.
  cn = 0
  For i = 0 To 9 : cmd$(i) = "" : Next i
  Line Input "DEBUG >> ", s$
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

  If cmd$(0) = "c" Then
    gdb = E_OK
  ElseIf cmd$(0) = "b" Then
    Error "Implement this"
  ElseIf cmd$(0) = "q" Then
    gdb = E_QUIT
  ElseIf cmd$(0) = "s" Then
    If oc = &h4 And op >= &hE0 Then Print ">";
    gdb = execute()
    If gdb = E_OK Then gdb = E_BREAK
  EndIf

End Function

Sub main()
  Local err, s$

  Mode 1
  Cls

  Print "             Sockpuppet Studios"
  Print "                  presents"
  Print "       A Toy Plastic Trumpet Production"
  Print
  Print "ZMIM: a Z-Machine Interpreter for the Maximite"
  Print
  Print "Copyright (c) 2019-20 Thomas Hugo Williams"
  Print "Version 0.1 for Colour Maximite 2, MMBasic 5.05"
  Print

  Input "Start in debugger [Y|n]"; s$
  If LCase$(s$) = "n" Then err = E_OK Else err = E_BREAK
  Print

  mem_init("B:\zmim\examples\minizork.z3")
  'mem_init("B:\zmim\examples\advent.z3"
  'mem_init("B:\zmim\examples\ZORK1\DATA\ZORK1.DAT"
  decode_init()
  Print

  Dim num_ops = 0
  Timer = 0

  debug = 0

  Do While err <> E_QUIT
    Do While err = E_OK
      err = execute()
    Loop
    Do While err <> E_OK And err <> E_QUIT
      err = gdb()
    Loop
  Loop

  Print
  Print "Num instructions processed ="; num_ops
  Print "Instructions / second      ="; num_ops / (Timer / 1000)
  Print "Num page faults            ="; pf
  Print
End Sub

main()
