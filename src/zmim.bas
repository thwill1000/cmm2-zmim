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
#Include "instruct.inc"

Mode 1
Cls

Print "Sockpuppet Software presents a Toy Plastic Trumpet production of"
Print "ZMIM: a Z-Machine Interpreter for the Maximite"
Print "Copyright (c) 2019-20 Thomas Hugo Williams"
Print "Version 0.1 for Colour Maximite 2, MMBasic 5.05"
Print

' If > 0 then produce debug output
' If bit 7 is set then print a new line before the current value of 'pc'
Dim debug = 0

'Input "Save 'ZMIM.BAS' [y|N]"; s$
'If (s$ = "y") Or (s$ = "Y") Then Save "ZMIM.BAS"
'Dim s$
'Input "Run with debug output [Y|n]"; s$
'If (s$ = "n") Or (s$ = "N") Then debug = 0
'Print

Const FILE$ = "B:\zmim\examples\minizork.z3"
'FILE$ = "B:\zmim\examples\advent.z3"
'FILE$ = "B:\zmim\examples\ZORK1\DATA\ZORK1.DAT"

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

' Variable to assign unused result of a Function call to
Dim _ = 0

' If > 0 then an error has occurred
Dim err = 0

Const MAX_NUM_OPERANDS = 4 ' requires up to 8 for z4+

' Instruction encoding
Const LARGE    = &b00
Const SMALL    = &b01
Const VARIABLE = &b10
Const OMITTED  = &b11

' The currently decoded instruction
Dim oc = 0               ' operand code
Dim onum = 0             ' number of operands
Dim oa(MAX_NUM_OPERANDS) ' operand values with variables looked-up
Dim ot(MAX_NUM_OPERANDS) ' operand types
Dim ov(MAX_NUM_OPERANDS) ' operand raw values

Dim bp = 0 ' breakpoint address

Sub long_decode(op)
  oc = op And BTM_5_BITS
  onum = 2
  ov(0) = rp()
  ov(1) = rp()
  If op <= &h1F Then
    ot(0) = SMALL : oa(0) = ov(0)
    ot(1) = SMALL : oa(1) = ov(1)
  ElseIf op <= &h3F Then
    ot(0) = SMALL : oa(0) = ov(0)
    ot(1) = VARIABLE : oa(1) = vget(ov(1))
  ElseIf op <= &h5F Then
    ot(0) = VARIABLE : oa(0) = vget(ov(0))
    ot(1) = SMALL : oa(1) = ov(1)
  Else
    ot(0) = VARIABLE : oa(0) = vget(ov(0))
    ot(1) = VARIABLE : oa(1) = vget(ov(1))
  EndIf
End Sub

Sub short_decode(op)
  oc = op And BTM_4_BITS
  onum = 1
  If op <= &h8F Then
    ot(0) = LARGE : ov(0) = rp() * 256 + rp() : oa(0) = ov(0)
  ElseIf op <= &h9F Then
    ot(0) = SMALL : ov(0) = rp() : oa(0) = ov(0)
  ElseIf op <= &hAF Then
    ot(0) = VARIABLE : ov(0) = rp() : oa(0) = vget(ov(0))
  Else
    onum = 0
  EndIf
End Sub

Sub var_decode(op)
  Local i, x
  oc = op And &b11111
  onum = 4
  x = rp()
  For i = 3 To 0 Step -1
    ot(i) = x And &b11
    If ot(i) = OMITTED Then onum = onum - 1
    x = x \ 4
  Next i
  For i = 0 To onum - 1
    If ot(i) = LARGE Then ov(i) = rp() * 256 + rp() : oa(i) = ov(i)
    If ot(i) = SMALL Then ov(i) = rp() : oa(i) = ov(i)
    If ot(i) = VARIABLE Then ov(i) = rp() : oa(i) = vget(ov(i))
  Next i
End Sub

Sub _2op
  Local a, b, br, st, x

  a = oa(0)
  b = oa(1)

  ' JE
  If oc = &h1 Then
    br = read_branch()
    dmp_op("JE", -1, br)
    x = (a = b)
    If (Not x) And (onum = 3) Then x = (a = oa(2))
    If (Not x) And (onum = 4) Then x = (a = oa(3))
    _branch(x, br)

  ' JL
  ElseIf oc = &h2 Then
    br = read_branch()
    dmp_op("JL", -1, br)
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    _branch(a < b, br)

  ' JG
  ElseIf oc = &h3 Then
    br = read_branch()
    dmp_op("JG", -1, br)
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    _branch(a > b, br)

  ' DEC_CHK
  ElseIf oc = &h4 Then
    br = read_branch()
    dmp_op("DEC_CHK", -1, br)
    x = vget(a) - 1
    If x < 0 Then x = &hFFFF
    vset(a, x)
    _branch(x < b, br)

  ' INC_CHK
  ElseIf oc = &h5 Then
    br = read_branch()
    dmp_op("INC_CHK", -1, br)
    x = vget(a) + 1
    If x > &hFFFF Then x = 0
    vset(a, x)
    _branch(x > b, br)

  ' JIN
  ElseIf oc = &h6 Then
    br = read_branch()
    dmp_op("JIN", -1, br)
    x = orel(a, PARENT)
    _branch(x = b, br)

  ' TEST
  ElseIf oc = &h7 Then
    br = read_branch()
    dmp_op("TEST", -1, br)
    _branch(a And b = b, br)

  ' OR
  ElseIf oc = &h8 Then
    st = rp()
    dmp_op("OR", st)
    vset(st, a Or b)

  ' AND
  ElseIf oc = &h9 Then
    st = rp()
    dmp_op("AND", st)
    vset(st, a And b)

  ' TEST_ATTR: a = object, b = attribute
  ElseIf oc = &hA Then
    br = read_branch()
    dmp_op("TEST_ATTR", -1, br)
    x = oattr(a, b)
    _branch(x = 1, br)

  ' SET_ATTR
  ElseIf oc = &hB Then
    dmp_op("SET_ATTR", -1)
    _ = oattr(a, b, 1, 1)

  ' CLEAR_ATTR
  ElseIf oc = &hC Then
    dmp_op("CLEAR_ATTR", -1)
    _ = oattr(a, b, 1, 0)

  ' STORE
  ElseIf oc = &hD Then
    dmp_op("STORE", -1)
    vset(a, b)

  ' INSERT_OBJ: a = object, b = destination
  ElseIf oc = &hE Then
    dmp_op("INSERT_OBJ", -1)
    x = orel(b, CHILD)
    _ = orel(b, CHILD, 1, a)
    _ = orel(a, PARENT, 1, b)
    _ = orel(a, SIBLING, 1, x)

  ' LOADW
  ElseIf oc = &hF Then
    st = rp()
    dmp_op("LOADW", st)
    x = rw(a + 2 * b)
    vset(st, x)

  ' LOADB
  ElseIf oc = &h10 Then
    st = rp()
    dmp_op("LOADB", st)
    x = rb(a + b)
    vset(st, x)

  ' GET_PROP
  ElseIf oc = &h11 Then
    st = rp()
    dmp_op("GET_PROP", st)
    x = get_prop(a, b)
    vset(st, x)

  ' GET_PROP_ADDR
  ElseIf oc = &h12 Then
    st = rp()
    dmp_op("!GET_PROP_ADDR", st)
    err = 1

  ' GET_NEXT_PROP
  ElseIf oc = &h13 Then
    st = rp()
    dmp_op("!GET_NEXT_PROP", st)
    err = 1

  ElseIf oc < &h19 Then
    st = rp()
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536

    ' ADD
    If oc = &h14 Then
      dmp_op("ADD", st)
      x = a + b
    ElseIf oc = &h15 Then
      dmp_op("SUB", st)
      x = a - b
    ElseIf oc = &h16 Then
      dmp_op("MUL", st)
      x = a * b
    ElseIf oc = &h17 Then
      dmp_op("DIV", st)
      x = a \ b
    Else
      dmp_op("!MOD", st)
      err = 1
    EndIf

    If x < 0 Then x = 65536 - x
    vset(st, x)

  Else
    err = 1
  EndIf
End Sub

Sub _1op
  Local a, st, br, x

  a = oa(0)

  ' JZ
  If oc = &h0 Then
    br = read_branch()
    dmp_op("JZ", -1, br)
    _branch(a = 0, br)

  ' GET_SIBLING
  ElseIf oc = &h1 Then
    st = rp()
    br = read_branch()
    dmp_op("GET_SIBLING", st, br)
    x = orel(a, SIBLING)
    vset(st, x)
    _branch(x <> 0, br)

  ' GET_CHILD
  ElseIf oc = &h2 Then
    st = rp()
    br = read_branch()
    dmp_op("GET_CHILD", st, br)
    x = orel(a, CHILD)
    vset(st, x)
    _branch(x <> 0, br)

  ' GET_PARENT
  ElseIf oc = &h3 Then
    st = rp()
    dmp_op("GET_PARENT", st)
    x = orel(a, PARENT)
    vset(st, x)

  ' GET_PROP_LEN
  ElseIf oc = &h4 Then
    st = rp()
    dmp_op("!GET_PROP_LEN", st)
    err = 1

  ' INC
  ElseIf oc = &h5 Then
    dmp_op("INC", -1)
    x = vget(a)
    If x > 32767 Then x = x - 65536
    x = x + 1
    If x < 0 Then x = 65536 - x
    vset(a, x)

  ' DEC
  ElseIf oc = &h6 Then
    dmp_op("DEC", -1)
    x = vget(a)
    If x > 32767 Then x = x - 65536
    x = x - 1
    If x < 0 Then x = 65536 - x
    vset(a, x)

  ' PRINT_ADDR
  ElseIf oc = &h7 Then
    dmp_op("!PRINT_ADDR", -1)
    err = 1

  ' REMOVE_OBJ
  ElseIf oc = &h9 Then
    dmp_op("!REMOVE_OBJ", -1)
    err = 1

  ' PRINT_OBJECT
  ElseIf oc = &hA Then
    dmp_op("PRINT_OBJECT", -1)
    print_obj(a)
    If debug Then debug = debug Or BIT(7)

  ' RET
  ElseIf oc = &hB Then
    dmp_op("RET", -1)
    _return(a)

  ' JUMP
  ElseIf oc = &hC Then
    dmp_op("JUMP", -1)
    If a And &h8000 Then a = a - 65536
    pc = pc + a - 2

  ' PRINT_PADDR
  ElseIf oc = &hD Then
    dmp_op("PRINT_PADDR", -1)
    print_zstring(a * 2)
    If debug Then debug = debug Or BIT(7)

  ' LOAD
  ElseIf oc = &hE Then
    st = rp()
    dmp_op("!LOAD", st)
    err = 1

  Else
    err = 1
  EndIf
End Sub

Sub _0op
  Local x

  ' RTRUE
  If oc = &h0 Then
    dmp_op("RTRUE", -1)
    _return(1)

  ' RFALSE
  ElseIf oc = &h1 Then
    dmp_op("RFALSE", -1)
    _return(0)

  ' PRINT
  ElseIf oc = &h2 Then
    dmp_op("PRINT", -1)
    print_zstring(pc)
    If debug Then debug = debug Or BIT(7)

  ' PRINT_RET
  ElseIf oc = &h3 Then
    dmp_op("!PRINT_RET", -1)
    print_zstring(pc)
    err = 1

  ' RET_POPPED
  ElseIf oc = &h8 Then
    dmp_op("RET_POPPED", -1)
    x = pop()
    _return(x)

  ' NEWLINE
  ElseIf oc = &hB Then
    dmp_op("NEWLINE", -1)
    If debug Then Print Else Print Chr$(8); " " : Print " ";

  Else
    err = 1
  EndIf
End Sub

Sub _varop
  Local st, br, x

  ' CALL
  If oc = &h0 Then
    st = rp()
    dmp_op("CALL", st)
    _call(st)

  ' STOREW
  ElseIf oc = &h1 Then
    dmp_op("STOREW", -1)
    ww(oa(0) + 2 * oa(1), oa(2))

  ' STOREB
  ElseIf oc = &h2 Then
    dmp_op("STOREB", -1)
    wb(oa(0) + oa(1), oa(2))

  ' READ
  ElseIf oc = &h4 Then
    dmp_op("!READ", -1)
    If Not debug Then Print Chr$(8); " ";
    _read(oa(0), oa(1))

  ' PRINT_CHAR
  ElseIf oc = &h5 Then
    dmp_op("PRINT_CHAR", -1)
    If debug Then
      Print Chr$(oa(0));
      debug = debug Or BIT(7)
    Else
      Print Chr$(8); Chr$(oa(0)); " ";
    EndIf

  ' PRINT_NUM
  ElseIf oc = &h6 Then
    dmp_op("PRINT_NUM", -1)
    If debug Then
      Print Str$(oa(0));
      debug = debug Or BIT(7)
    Else
      Print Chr$(8); Str$(oa(0)); " ";
    EndIf

  Else
    err = 1
  EndIf
End Sub

' Reads branch offset.
' @return bits 0-15 - new value for the program counter.
'                   - if = pc - 2 then -> return false.
'                   - if = pc - 1 then -> return true.
'         bit 16    - set = branch on True, unset = branch on False.
Function read_branch()
  Local a, of
  a = rp()
  of = a And BTM_6_BITS

  If (a And BIT(6)) = 0 Then
    of = 256 * of + rp()
    If a And BIT(5) Then of = of - 16384
  EndIf

  read_branch = pc + of - 2
  If a And BIT(7) Then read_branch = read_branch Or &h10000
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

Sub init
  Local i

  Print "Loading "; FILE$

  ' Load page 0 which contains the header.
  Print "  Header page: 0"
  If mem_load(0) <> 0 Then Error

  ' Read header data.
  pc = rw(&h06)
  GLOBAL_VAR = rw(&h0C)
  BASE_STATIC = rw(&h0E)
  FILE_LEN = rw(&h1A) * 2

  ' Initialise dynamic memory.
  FIRST_SWAP_PAGE = BASE_STATIC \ PAGE_SIZE
  If BASE_STATIC Mod PAGE_SIZE > 0 Then FIRST_SWAP_PAGE = FIRST_SWAP_PAGE + 1
  Print "  Dynamic pages: ";
  For i = 1 To FIRST_SWAP_PAGE - 1
    If i > 1 Then Print ", ";
    Print Str$(i);
    If mem_load(i) <> i Then Error
  Next i
  Print
  Print "  Paged memory starts at page "; Str$(FIRST_SWAP_PAGE)
End Sub

Sub _step(n)
  Local i, op

  If n = 0 Then n = 1 Else If n < 0 Then n = &hFFFF

  If Not debug Then Print " ";

  For i = 0 To n - 1

    If debug Then
      If debug And BIT(7) Then Print : debug = (debug And (BIT(7) Xor &hFF))
      Print Hex$(pc); ": ";
    Else
      Print Chr$(8); Mid$(BUSY$(0), (i Mod 16) + 1, 1);
    EndIf

    If pc <> bp Then
      op = rp()
      num_ops = num_ops + 1
      If op < &h80 Then
        long_decode(op)
        _2op()
      ElseIf op < &hC0 Then
        short_decode(op)
        If op < &hB0 Then _1op() Else _0op()
      Else
        var_decode(op)
        If op < &hE0 Then _2op() Else _varop()
      EndIf
    EndIf

    If err > 0 Then
      Print
      Print "Unsupported instruction "; Hex$(op)
      i = n ' Exit loop
    ElseIf pc = bp Then
      Print "[Breakpoint reached] - resetting bp = 0"
      bp = 0
      i = n ' Exit loop
    ElseIf n = &hFFFF And i > 15 Then
      i = 0 ' Loop indefinitely
    EndIf

  Next i
End Sub

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
  Local c, i, n, word$, s$, sep$, wc

  Print "text_buf = "; Hex$(text_buf)
  Print "parse_buf = "; Hex$(parse_buf)

  Line Input s$
  s$ = LCase$(s$)
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
      Print Hex$(_) ;
      Print Len(word$);
      Print i - Len(word$) - 1
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
  dmp_mem(text_buf, 32)
  dmp_mem(parse_buf, 32)
End Sub

' Interactive debugger
Sub gdb()
  Local br, c, cmd$(9) Length 20, cn, i, old_pc, old_sp, op, s$, st

  Do
    ' Decode and display the next instruction but don't execute it.
    Print Hex$(pc); ": ";
    old_pc = pc
    old_sp = sp
    op = rp()
    If op < &h80 Then
      long_decode(op)
      s$ = inst_2op$(oc)
    ElseIf op < &hC0 Then
      short_decode(op)
      If op < &hB0 Then s$ = inst_1op$(oc) Else s$ = inst_0op$(oc)
    Else
      var_decode(op)
      If op < &hE0 Then s$ = inst_2op$(oc) Else s$ = inst_varop$(oc)
    EndIf
    If Left$(s$, 1) = "B" Then
      st = -1
      br = read_branch()
    ElseIf Left$(s$, 1) = "S" Then
      st = rp()
      br = 0
    ElseIf Left$(s$, 1) = "X" Then
      st = rp()
      br = read_branch()
    Else
      st = -1
      br = 0
    EndIf
    debug = 1
    dmp_op(Mid$(s$, 2), st, br)
    debug = 0
    pc = old_pc
    sp = old_sp

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
      _step(-1)
    ElseIf cmd$(0) = "b" Then
      ' TODO: set breakpoint
    ElseIf cmd$(0) = "q" Then
      Exit Do
    ElseIf cmd$(0) = "s" Then
      _step(1)
    EndIf
  Loop

End Sub

init()
instruct_init()
Print

Dim num_ops = 0
Timer = 0

gdb()

'bp = &h41d3
'_step(-1)

Print
Print "Num instructions processed ="; num_ops
Print "Instructions / second      ="; num_ops / (Timer / 1000)
Print "Num page faults            ="; pf
Print

End
