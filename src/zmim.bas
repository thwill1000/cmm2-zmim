' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Option Explicit On

#Include "debug.inc"
#Include "util.inc"
#Include "dmp_abrv.inc"
#Include "dmp_dict.inc"
#Include "dmp_hdr.inc"
#Include "dmp_mmap.inc"

Mode 1
Cls

Print "ZMIM: a Z-Machine Interpreter for the Maximite"
Print "Copyright (c) 2019-20 Thomas Hugo Williams"
Print "Version 0.1 for Colour Maximite 2, MMBasic 5.05"
Print

' If > 0 then produce debug output
' If bit 7 is set then print a new line before the current value of 'pc'
Dim debug = 1

'Input "Save 'ZMIM.BAS' [y|N]"; s$
'If (s$ = "y") Or (s$ = "Y") Then Save "ZMIM.BAS"
Dim s$
Input "Run with debug output [Y|n]"; s$
If (s$ = "n") Or (s$ = "N") Then debug = 0
Print

Const FILE$ = "B:\zmim\examples\minizork.z3"
'FILE$ = "B:\zmim\examples\advent.z3"
'FILE$ = "B:\zmim\examples\ZORK1\DATA\ZORK1.DAT"

Const PAGE_SIZE = 512
Const NUM_PHYSICAL_PAGES = 80
Const NUM_VIRTUAL_PAGES = 128 * 1024 / PAGE_SIZE

' Memory addresses below this are read on startup and not swapped in/out
' - not properly set until the z-machine header is read
Dim BASE_STATIC = PAGE_SIZE

Dim FILE_LEN = PAGE_SIZE
Dim GLOBAL_VAR = 0
Dim FIRST_SWAP_PAGE = -1

Const MAX_WORD = 256 * 256 - 1

Dim BUSY$(1) LENGTH 16
BUSY$(0) = "\\\\||||////----"

Dim ALPHABET$(2) LENGTH 32
ALPHABET$(0) = " 123[]abcdefghijklmnopqrstuvwxyz"
ALPHABET$(1) = " 123[]ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET$(2) = " 123[]@^0123456789.,!?_#'" + Chr$(34) + "/\-:()"

Dim i = 0
Dim BIT(7)
For i = 0 To 7 : BIT(i) = 2 ^ i : Next i

Const BTM_2_BITS  = &b00000011
Const BTM_4_BITS  = &b00001111
Const BTM_5_BITS  = &b00011111
Const BTM_6_BITS  = &b00111111

' Constants for orel()
Const PARENT = 4 : Const SIBLING = 5 : Const CHILD = 6

Dim m(NUM_PHYSICAL_PAGES * PAGE_SIZE \ 4)

Dim pf = 0 ' Counter for number of page faults
Dim pp = 0 ' Physical page number
Dim vp = 0 ' Virtual page number

' Map of physical pages -> virtual pages
Dim pp_to_vp(NUM_PHYSICAL_PAGES - 1)

' Map of virtual pages -> physical pages
Dim vp_to_pp(NUM_VIRTUAL_PAGES - 1)

Dim next_page = 0

Dim stack(511)
Dim sp = -1

' Current stack frame pointer
Dim fp = -1

' Variable to assign unused result of a Function call to
Dim _ = 0

Dim pc = 0

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
Dim ov(MAX_NUM_OPERANDS) ' opeanrd raw values

Dim bp = 0 ' breakpoint address

' Reads a byte from 'pc' and increments 'pc'
Function rp()
  If pc < 0 Or pc >= FILE_LEN Then Error
  vp = pc \ PAGE_SIZE
  pp = vp_to_pp(vp)
  If pp = 0 Then pp = mem_load(vp) : pf = pf + 1
  rp = Peek(Var m(0), pp * PAGE_SIZE + (pc Mod PAGE_SIZE))
  pc = pc + 1
End Function

' Reads a byte from 'a' but DOES NOT increment a
Function rb(a)
  If a < 0 Or a >= FILE_LEN Then
    Print "Address"; a ; " > file length"; FILE_LEN
    End : Error
  EndIf
  If a < BASE_STATIC Then rb = Peek(Var m(0), a) : Exit Function
  vp = a \ PAGE_SIZE
  pp = vp_to_pp(vp)
  If pp = 0 Then pp = mem_load(vp) : pf = pf + 1
  rb = Peek(Var m(0), pp * PAGE_SIZE + (a Mod Page_SIZE))
End Function

' Reads a 16-bit word from 'a' but DOES NOT increment a
Function rw(a)
  rw = rb(a) * 256 + rb(a + 1)
End Function

' Writes byte 'x' to 'a'
Sub wb(a, x)
  If a < 0 Or a >= BASE_STATIC Then Error
  If x < 0 Or x > 255 Then Error
  Poke Var m(0), a, x
End Sub

' Writes 16-bit word 'x' to 'a'
Sub ww(a, x)
  If a < 0 Or a >= BASE_STATIC - 1 Then Error
  If x < 0 Or x > MAX_WORD Then Error
  Poke Var m(0), a, x \ 256
  Poke Var m(0), a + 1, x Mod 256
End Sub

' Pops a 16-bit word from the stack.
Function pop()
  pop = stack(sp)
  sp = sp - 1
End Function

' Pushes a 16-bit word onto the stack.
Sub push(w)
  sp = sp + 1
  stack(sp) = w
End Sub

' Loads virtual page 'vp' from '$file'.
' @return physical page number
Function mem_load(vp)
  Local ad, buf$, buf_sz, i, pp, to_read

  pp = next_page

  ' TODO: Implement some form of Least Recently Used algorithm.
  next_page = next_page + 1
  If next_page = NUM_PHYSICAL_PAGES Then next_page = FIRST_SWAP_PAGE

  ' TODO: Should the file be opened once globally and kept open until exit?
  Open FILE$ For random As #1
  Seek #1, vp * PAGE_SIZE + 1
  ad = pp * PAGE_SIZE
  to_read = PAGE_SIZE
  buf_sz = 255
  Do While to_read > 0
    If to_read < 255 Then buf_sz = to_read
    buf$ = Input$(buf_sz, 1)
    For i = 1 To buf_sz
'      Print ad;
      Poke Var m(0), ad, Peek(Var buf$, i)
      ad = ad + 1
    Next i
    to_read = to_read - buf_sz
  Loop
  Close #1

  vp_to_pp(pp_to_vp(pp)) = 0
  vp_to_pp(vp) = pp
  pp_to_vp(pp) = vp

  mem_load = pp
End Function

' Gets variable 'i'.
' If i = 0 then pops and returns the top value of the stack.
Function vget(i)
  If i = 0 Then
    vget = pop()
  ElseIf i < &h10 Then
    vget = stack(fp + i + 3)
  ElseIf i <= &hFF Then
    vget = rw(GLOBAL_VAR + 2 * (i - &h10))
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Function

' Sets variable 'i'.
' If i = 0 then pushes the value onto the stack.
Sub vset(i, x)
  If i = 0 Then
    push(x)
  ElseIf i < &h10 Then
    stack(fp + i + 3) = x
  ElseIf i <= &hFF Then
    ww(GLOBAL_VAR + 2 * (i - &h10), x)
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Sub

' Prints Z-string starting at 'a' incrementing 'a' by the number of bytes read
Sub print_zstring(a)
  Local b, c, i, s, x, zc(2)

  If Not debug Then Print Chr$(8);

  ' The state of Z-string processing is recorded in 's':
  '   0, 1, 2 - Expecting a character from alphabet 's'
  '   3, 4, 5 - Expecting an abbreviation from table 's - 3'
  '   6       - Expecting the top 5-bits of a ZSCII character
  '   > 6     - Expecting the btm 5-bits of a ZSCII character whose
  '             top 5-bits are 's - 7'

  ' Should be 'Do While x = 0' but there is an MMBasic bug using that
  ' in recursive functions.
  For x = 0 To 0 Step 0
    x = rb(a) * 256 + rb(a + 1)

    For i = 2 To 0 Step -1
      zc(i) = x And 31 ' &b00011111
      x = x \ 32 ' rshift 5
    Next i

    ' x is now the top-bit of the word. If x = 1 then we have reached the end
    ' of the string and will exit the loop after this iteration.

    For i = 0 To 2
      c = zc(i)
      If s < 3 Then
        If c = 0 Then
          Print " ";
        ElseIf c < 4 Then
          s = c + 2
        ElseIf c < 6 Then
          s = c - 3
        Else
          If c = 6 And s = 2 Then
            s = 6
          ElseIf c = 7 And s = 2 Then
            Print
            s = 0
          Else
            Print Mid$(ALPHABET$(s), c + 1, 1);
            s = 0
          EndIf
        EndIf
      ElseIf s < 6 Then
        b = a ' Backup the address
        print_abrv((s - 3) * 32 + c)
        a = b ' Restore the address
        s = 0
      ElseIf s = 6 Then
        s = c + 7
      Else
        Print Chr$((s - 7) * 32 + c);
        s = 0
      EndIf
    Next i

    a = a + 2
  Next x

  If Not debug Then Print " ";

End Sub

' Prints abbreviation 'x'
Sub print_abrv(x)
  Local a, b
  a = rw(&h18)
  b = rw(a + x * 2)
  If Not debug Then Print " ";
  print_zstring(b * 2)
  If Not debug Then Print Chr$(8);
End Sub

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

' Gets/sets object attribute
Function oattr(o, a, s, x)
  Local ad, m, y
  ad = rw(&h0A) + 62 + (o - 1) * 9 + a \ 8
  y = rb(ad)
  m = BIT(7 - a Mod 8)
  If s = 0 Then oattr = (y And m) > 0 : Exit Function
  If x = 0 Then y = (y And (m Xor &hFF)) Else y = (y Or m)
  wb(ad, y)
  oattr = x
End Function

' Gets/sets object relatives
Function orel(o, r, s, x)
  Local ad
  ad = rw(&h0A) + 62 + (o - 1) * 9 + r
  If s = 0 Then orel = rb(ad) : Exit Function
  wb(ad, x)
  orel = x
End Function

Function get_next_prop(o, p)
  Local ad, x

  If p = 0 Then
    ad = get_prop_base(o)
    ad = ad + 1 + 2 * rb(ad) ' Skip length & description
  Else
    ad = get_prop_addr(o, p)
    If ad = 0 Then Error "Property does not exist"
    x = rb(ad)
    ad = ad + 2 + x\32
  EndIf

  x = rb(ad)
  get_next_prop = x And BTM_5_BITS
End Function

Function get_prop_len(o, p)
  Local ad, x
  If o > 0 Then
    ad = get_prop_addr(o, p)
    If ad = 0 Then Error "Property does not exist"
    x = rb(ad)
    get_prop_len = x\32 + 1
  EndIf
End Function

Function get_prop_base(o)
  Local ad
  ad = rw(&h0A) + 62 + (o - 1) * 9 + 7
  get_prop_base = rw(ad)
End Function

Function get_prop_addr(o, p)
  Local ad, x
  ad = get_prop_base(o)
  ad = ad + 1 + 2 * rb(ad) ' Skip length & description
  Do
    x = rb(ad)
    If (x And BTM_5_BITS) = p Then get_prop_addr = ad : Exit Function
    If (x And BTM_5_BITS) < p Then get_prop_addr = 0 : Exit Function
    ad = ad + 2 + x\32
  Loop
End Function

Function get_prop(o, p)
  Local ad, sz, x
  ad = get_prop_addr(o, p)
  If ad > 0 Then
    x = rb(ad)
    If (x And BTM_5_BITS) <> p Then Error
    sz = x\32 + 1
    If sz = 1 Then get_prop = rb(ad + 1) : Exit Function
    If sz = 2 Then get_prop = rw(ad + 1) : Exit Function
    Error "Property length > 2"
  EndIf
  ad = rw(&h0A) + 2 * (p - 1)
  get_prop = rw(ad)
End Function

Sub print_obj(o)
  Local ad
  ad = get_prop_base(o) + 1
  print_zstring(ad)
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

'  Print lpad$(Hex$(b(0)), 2, "0");
'  Print lpad$(Hex$(b(1)), 2, "0");
'  Print lpad$(Hex$(b(2)), 2, "0");
'  Print lpad$(Hex$(b(3)), 2, "0");

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
  Local c, i, n, word$, sep$, wc

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

init()
Print

Dim num_ops = 0
Timer = 0

'#Include "tst_mem.inc"
'tst_mem()
'End

_step(-1)

'dmp_dict()
'Do
'_read()
'Loop

Print
Print "Num instructions processed ="; num_ops
Print "Instructions / second      ="; num_ops / (Timer / 1000)
Print "Num page faults            ="; pf
Print
