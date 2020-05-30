' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Option Explicit On

'#Include "memory.inc"
#Include "memory_fast.inc"
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
#Include "tst_obj.inc"

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
Const E_DEBUG = 5

Dim num_ops = 0 ' Number of instructions processed.
Dim ztrace = 0  ' Is instruction tracing enabled?
Dim bp = 0      ' Breakpoint address.

Function execute_2op()
  Local a, b, x, y, _

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
    x = get_prop_addr(a, b)
    vset(st, x)

  ' GET_NEXT_PROP
  ElseIf oc = &h13 Then
    execute_2op = E_UNIMPLEMENTED

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
      execute_2op = E_UNIMPLEMENTED
    EndIf

    If x < 0 Then x = 65536 + x
    vset(st, x)

  Else
    execute_2op = E_UNKNOWN
  EndIf
End Function

Function execute_1op()
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
    execute_1op = E_UNIMPLEMENTED

  ' REMOVE_OBJ
  ElseIf oc = &h9 Then
    execute_1op = E_UNIMPLEMENTED

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
    execute_1op = E_UNIMPLEMENTED

  Else
    execute_1op = E_UNKNOWN
  EndIf
End Function

Function execute_0op()
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

  ' NEWLINE
  ElseIf oc = &hB Then
    Print

  Else
    execute_0op = E_UNKNOWN
  EndIf
End Function

Function execute_varop()
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
    Print " ";
    execute_varop = _read(oa(0), oa(1))

  ' PRINT_CHAR
  ElseIf oc = &h5 Then
    Print Chr$(oa(0));

  ' PRINT_NUM
  ElseIf oc = &h6 Then
    Print Str$(oa(0));

  ' RANDOM
  ElseIf oc = &h7 Then
    execute_varop = E_UNIMPLEMENTED

  ' PUSH
  ElseIf oc = &h8 Then
    push(oa(0))

  ' PULL
  ElseIf oc = &h9
    x = pop(oa(0))
    vset(st, x)

  Else
    execute_varop = E_UNKNOWN
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

Function execute(tr)
  Local op, pc_old, sp_old

  ' Store the PC and SP in case we need to roll-back.
  pc_old = pc : sp_old = sp

  op = decode(tr)
  num_ops = num_ops + 1
  If op < &h80 Then
    execute = execute_2op()
  ElseIf op < &hB0 Then
    execute = execute_1op()
  ElseIf op < &hC0 Then
    execute = execute_0op()
  ElseIf op < &hE0 Then
    execute = execute_2op()
  Else
    execute = execute_varop()
  EndIf

  If execute = E_UNKNOWN Then
    Print "Unsupported instruction "; Hex$(op)
  ElseIf execute = E_UNIMPLEMENTED Then
    Print "Unimplemented instruction "; Hex$(op)
  EndIf

  If execute <> E_OK Then pc = pc_old : sp = sp_old

End Function

Function lookup(w$)
  Local b(3), i, sl, x, s$

'  Print "lookup: *" + w$ + "* => ";

  s$ = LCase$(Left$(w$, 6))
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

Function _read(text_buf, parse_buf)
  Local ad, c, i, n, word$, s$, sep$, wc

  Line Input s$
  s$ = LCase$(s$)

  If s$ = "*break" Then
    _read = E_BREAK
    Exit Function
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

    debug = E_DEBUG

    If cmd$(0) = "b" Then
      ' Set breakpoint
      bp = Val(cmd$(1))
      Print "Breakpoint set to &h"; Hex$(bp)

    ElseIf cmd$(0) = "c" Then
      ' Continue
      If oc = &h4 And op >= &hE0 Then Print ">"; ' Display READ prompt
      debug = E_OK

    ElseIf cmd$(0) = "C" Then
      ' Stack dump
      dmp_stack(Val(cmd$(1)))

    ElseIf cmd$(0) = "d" Then
      ' Memory dump
      If Len(cmd$(1)) = 0 Then a = pc Else a = Val(cmd$(1))
      dmp_mem(a, Val(cmd$(2)))

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
      debug = execute(0)
      If debug = E_OK Then debug = E_BREAK

    ElseIf cmd$(0) = "troff" Then
      ' Disable trace
      Print "Trace OFF"
      ztrace = 0

    ElseIf cmd$(0) = "tron" Then
      ' Enable trace
      Print "Trace ON"
      ztrace = 1

    Elseif cmd$(0) = "x" Then
      ' Parse and print value
      a = Val(cmd$(1))
      Print Str$(a);
      Print "  &h" + LPad$(Hex$(a), 4, "0");
      Print "  &b" + LPad$(Bin$(a), 16, "0")

    Else
      Print "Unknown debug command"

    EndIf

  Loop While debug = E_DEBUG

End Function

Sub main()
  Local state, s$

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
  If LCase$(s$) = "n" Then state = E_OK Else state = E_BREAK
  Print

  mem_init("B:\zmim\examples\minizork.z3")
  'mem_init("B:\zmim\examples\advent.z3"
  'mem_init("B:\zmim\examples\ZORK1\DATA\ZORK1.DAT"
  decode_init()
  Print

  Timer = 0

  Do While state <> E_QUIT
    If pc = bp Then
      Print "[Breakpoint reached] - resetting bp = 0"
      bp = 0
      state = E_BREAK
    EndIf

    If state = E_OK Then
      state = execute(ztrace)
    Else
      state = debug()
    EndIf
  Loop

  Print
  Print "Num instructions processed ="; num_ops
  Print "Instructions / second      ="; num_ops / (Timer / 1000)
  Print "Num page faults            ="; pf
  Print
End Sub

main()
