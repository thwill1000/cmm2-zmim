' ZMIM a Z-Machine Interpreter for the Colour Maximite
' Copyright (c) 2019 Thomas Hugo Williams
' For Maximite BASIC v4.5C

Mode 1
Cls

' If > 0 then produce debug output
' If bit 7 is set then print a new line before the current value of 'pc'
dbg = 1

Input "Save 'ZMIM.BAS' [y|N]"; s$
If (s$ = "y") Or (s$ = "Y") Then Save "ZMIM.BAS"
Input "Run with debug output [Y|n]"; s$
If (s$ = "n") Or (s$ = "N") Then dbg = 0
Print

file$ = "B:\zmim\examples\minizork.z3"
'file$ = "B:\zmim\examples\advent.z3"
'file$ = "B:\zmim\examples\ZORK1\DATA\ZORK1.DAT"

' By convention variables declared in UPPER CASE are constant
'  - this is not enforced by the language!
PAGE_SIZE = 512
NUM_PHYSICAL_PAGES = 80
NUM_VIRTUAL_PAGES = 128 * 1024 / PAGE_SIZE

' Memory addresses below this are read on startup and not swapped in/out
' - not properly set until the z-machine header is read
BASE_STATIC = PAGE_SIZE

FILE_LEN = PAGE_SIZE
GLOBAL_VAR = 0
FIRST_SWAP_PAGE = -1

MAX_WORD = 256 * 256 - 1

Dim ALPHABET$(2) LENGTH 32
ALPHABET$(0) = " 123[]abcdefghijklmnopqrstuvwxyz"
ALPHABET$(1) = " 123[]ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET$(2) = " 123[]@^0123456789.,!?_#'" + Chr$(34) + "/\-:()"
MAX_NUM_OPERANDS = 4 ' requires up to 8 for z4+
OT_LARGE_CONST = &b00
OT_SMALL_CONST = &b01
OT_VARIABLE = &b10
OT_OMITTED = &b11

Dim BIT(7)
For i = 0 To 7 : BIT(i) = 2 ^ i : Next i

BTM_2_BITS  = &b00000011
BTM_4_BITS  = &b00001111
BTM_5_BITS  = &b00011111
BTM_6_BITS  = &b00111111

' Constants for orel()
PARENT = 4 : SIBLING = 5 : CHILD = 6

Dim m(NUM_PHYSICAL_PAGES * PAGE_SIZE \ 4)

pf = 0 ' Counter for number of page faults
pp = 0 ' Physical page number
vp = 0 ' Virtual page number

' Map of physical pages -> virtual pages
Dim pp_to_vp(NUM_PHYSICAL_PAGES - 1)

' Map of virtual pages -> physical pages
Dim vp_to_pp(NUM_VIRTUAL_PAGES - 1)

next_page = 0

Dim stack(511)
sp = -1

' Current stack frame pointer
fp = -1

' Variable to assign unused result of a Function call to
_ = 0

pc = 0

' If > 0 then an error has occurred
err = 0

' The currently decoded operation
op = 0
op_code = 0
op_num = 0 ' number of operands
Dim op_type(MAX_NUM_OPERANDS)
Dim op_value(MAX_NUM_OPERANDS)

' Reads a byte from 'pc' and increments 'pc'
Function rp
  If pc < 0 Or pc >= FILE_LEN Then Error
  vp = pc \ PAGE_SIZE
  pp = vp_to_pp(vp)
  If pp = 0 Then pp = mem_load(vp) : pf = pf + 1
  rp = Peek(Var m(0), pp * PAGE_SIZE + (pc Mod PAGE_SIZE))
  pc = pc + 1
End Function

' Reads a byte from 'a' but DOES NOT increment a
Function rb(a)
  If a < 0 Or a >= FILE_LEN Then Error
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
Function pop
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
  Open file$ For random As #1
  Seek #1, vp * PAGE_SIZE + 1
  ad = pp * PAGE_SIZE
  to_read = PAGE_SIZE
  buf_sz = 255
  Do While to_read > 0
    If to_read < 255 Then buf_sz = to_read
    buf$ = Input$(buf_sz, 1)
    For i = 1 To buf_sz
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
Function get_var(i)
  If i = 0 Then
    get_var = pop()
  ElseIf i < &h10 Then
    get_var = stack(fp + i + 3)
  ElseIf i <= &hFF Then
    get_var = rw(GLOBAL_VAR + 2 * (i - &h10))
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Function

' Sets variable 'i'.
' If i = 0 then pushes the value onto the stack.
Sub set_var(i, x)
  If i = 0 Then
    push(x)
  ElseIf i < &h10 Then
    stack(fp + i + 3) = x
  ElseIf i < &hFF Then
    ww(GLOBAL_VAR + 2 * (i - &h10), x)
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Sub

' Prints ZString starting at 'addr'.
' @return the number of bytes read.
Function print_zstring(addr)
  Local abbrv, ad, al, ch, i, x, z, zchar(2)

  abbrv = 0
  ad = addr
  al = 0
  x = 0
  ' Should be 'Do While x = 0' but there is an MMBasic bug using that
  ' in recursive functions.
  For z = 0 To 0 Step 0
    If x > 0 Then Exit For

    x = rw(ad)

    For i = 2 To 0 Step -1
      zchar(i) = x And BTM_5_BITS
      x = rshift(x, 5)
    Next i

    ' x is now the top-bit of the word. If x = 1 then we have reached the end
    ' of the string and will exit the loop after this iteration.

    For i = 0 To 2
      ch = zchar(i)
      If abbrv > 0 Then
        print_abrv((abbrv - 1) * 32 + ch)
        abbrv = 0
      ElseIf ch > 0 And ch < 4 Then
        abbrv = ch
      ElseIf ch = 4 Then
        al = 1
      ElseIf ch = 5 Then
        al = 2
      ElseIf ch = 7 And al = 2 Then
        Print
      Else
        Print Mid$(ALPHABET$(al), ch + 1, 1);
        al = 0
      EndIf
    Next i

    ad = ad + 2
  Next z

  print_zstring = ad - addr
End Function

' Prints abbreviation 'i'.
Sub print_abrv(i)
  Local ad, x
  ad = rw(&h18)
  x = rw(ad + i * 2)
  _ = print_zstring(x * 2)
End Sub

' Performs instruction at 'pc'
Sub do_op
  Local i, x

  num_ops = num_ops + 1

  If dbg Then
    If dbg And BIT(7) Then Print : dbg = (dbg And (BIT(7) Xor &hFF))
    Print Hex$(pc); ": ";
  EndIf

  If pc = bp Then ' Breakpoint
    Print "[Breakpoint reached] - resetting bp = 0"
    bp = 0
    err = -1
    Exit Sub
  EndIf

  op = rp()

  If op <= &h7F Then
    ' Long form
    op_code = op And BTM_5_BITS
    op_num = 2
    op_type(0) = OT_VARIABLE
    op_type(1) = OT_VARIABLE
    If op <= &h1F Then
      op_type(0) = OT_SMALL_CONST
      op_type(1) = OT_SMALL_CONST
    ElseIf op <= &h3F Then
      op_type(0) = OT_SMALL_CONST
    ElseIf op <= &h5F Then
      op_type(1) = OT_SMALL_CONST
    EndIf

  ElseIf op <= &hBF Then
    ' Short form
    op_code = op And BTM_4_BITS
    op_num = 1
    If op <= &h8F Then
      op_type(0) = OT_LARGE_CONST
    ElseIf op <= &h9F Then
      op_type(0) = OT_SMALL_CONST
    ElseIf op <= &hAF Then
      op_type(0) = OT_VARIABLE
    Else
      op_num = 0
    EndIf

  Else
    ' Variable form
    op_code = op And BTM_5_BITS
    op_num = 4

    ' Read operand types
    x = rp()
    For i = 3 To 0 Step -1
      op_type(i) = x And BTM_2_BITS
      If op_type(i) = OT_OMITTED Then op_num = op_num - 1
      x = rshift(x, 2)
    Next i

  EndIf

  ' Read operands
  For i = 0 To op_num - 1
    If op_type(i) = OT_LARGE_CONST Then
      op_value(i) = rp() * 256 + rp()
    ElseIf op_type(i) <> OT_OMITTED Then
      op_value(i) = rp()
    EndIf
  Next i

  ' Perform the operation
  If op < 128 Then
    _2op()
  ElseIf op < 176 Then
    _1op()
  ElseIf op < 192 Then
    _0op()
  ElseIf op < 224 Then
    _2op()
  Else
    _varop()
  EndIf

  If err > 0 Then Print : Print "Unsupported instruction "; Hex$(op)

End Sub

Sub _2op
  Local a, b, br, st, x

  a = get_op(0)
  b = get_op(1)

  ' JE
  If op_code = &h1 Then
    br = read_branch()
    dmp_op("JE", -1, br)
    x = (a = b)
    If (Not x) And (op_num = 3) Then x = (a = get_op(2))
    If (Not x) And (op_num = 4) Then x = (a = get_op(3))
    do_branch(x, br)

  ' JL
  ElseIf op_code = &h2 Then
    br = read_branch()
    dmp_op("JL", -1, br)
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    do_branch(a < b, br)

  ' JG
  ElseIf op_code = &h3 Then
    br = read_branch()
    dmp_op("JG", -1, br)
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536
    do_branch(a > b, br)

  ' DEC_CHK
  ElseIf op_code = &h4 Then
    br = read_branch()
    dmp_op("DEC_CHK", -1, br)
    x = get_var(a) - 1
    If x < 0 Then x = &hFFFF
    set_var(a, x)
    do_branch(x < b, br)

  ' INC_CHK
  ElseIf op_code = &h5 Then
    br = read_branch()
    dmp_op("INC_CHK", -1, br)
    x = get_var(a) + 1
    If x > &hFFFF Then x = 0
    set_var(a, x)
    do_branch(x > b, br)

  ' JIN
  ElseIf op_code = &h6 Then
    br = read_branch()
    dmp_op("JIN", -1, br)
    x = orel(a, PARENT)
    do_branch(x = b, br)

  ' TEST
  ElseIf op_code = &h7 Then
    br = read_branch()
    dmp_op("TEST", -1, br)
    do_branch(a And b = b, br)

  ' OR
  ElseIf op_code = &h8 Then
    st = rp()
    dmp_op("OR", st)
    set_var(st, a Or b)

  ' AND
  ElseIf op_code = &h9 Then
    st = rp()
    dmp_op("AND", st)
    set_var(st, a And b)

  ' TEST_ATTR: a = object, b = attribute
  ElseIf op_code = &hA Then
    br = read_branch()
    dmp_op("TEST_ATTR", -1, br)
    x = oattr(a, b)
    do_branch(x = 1, br)

  ' SET_ATTR
  ElseIf op_code = &hB Then
    dmp_op("SET_ATTR", -1)
    _ = oattr(a, b, 1, 1)

  ' CLEAR_ATTR
  ElseIf op_code = &hC Then
    dmp_op("CLEAR_ATTR", -1)
    _ = oattr(a, b, 1, 0)

  ' STORE
  ElseIf op_code = &hD Then
    dmp_op("STORE", -1)
    set_var(a, b)

  ' INSERT_OBJ: a = object, b = destination
  ElseIf op_code = &hE Then
    dmp_op("INSERT_OBJ", -1)
    x = orel(b, CHILD)
    _ = orel(b, CHILD, 1, a)
    _ = orel(a, PARENT, 1, b)
    _ = orel(a, SIBLING, 1, x)

  ' LOADW
  ElseIf op_code = &hF Then
    st = rp()
    dmp_op("LOADW", st)
    x = rw(a + 2 * b)
    set_var(st, x)

  ' LOADB
  ElseIf op_code = &h10 Then
    st = rp()
    dmp_op("LOADB", st)
    x = rb(a + b)
    set_var(st, x)

  ' GET_PROP
  ElseIf op_code = &h11 Then
    st = rp()
    dmp_op("GET_PROP", st)
    x = get_prop(a, b)
    set_var(st, x)

  ' GET_PROP_ADDR
  ElseIf op_code = &h12 Then
    st = rp()
    dmp_op("!GET_PROP_ADDR", st)
    err = 1

  ' GET_NEXT_PROP
  ElseIf op_code = &h13 Then
    st = rp()
    dmp_op("!GET_NEXT_PROP", st)
    err = 1

  ElseIf op_code < &h19 Then
    st = rp()
    If a > 32767 Then a = a - 65536
    If b > 32767 Then b = b - 65536

    ' ADD
    If op_code = &h14 Then
      dmp_op("ADD", st)
      x = a + b
    ElseIf op_code = &h15 Then
      dmp_op("SUB", st)
      x = a - b
    ElseIf op_code = &h16 Then
      dmp_op("MUL", st)
      x = a * b
    ElseIf op_code = &h17 Then
      dmp_op("DIV", st)
      x = a \ b
    Else
      dmp_op("!MOD", st)
      err = 1
    EndIf

    If x < 0 Then x = 65536 - x
    set_var(st, x)

  Else
    err = 1
  EndIf
End Sub

Sub _1op
  Local a, st, br, x

  a = get_op(0)

  ' JZ
  If op_code = &h0 Then
    br = read_branch()
    dmp_op("JZ", -1, br)
    do_branch(a = 0, br)

  ' GET_SIBLING
  ElseIf op_code = &h1 Then
    st = rp()
    br = read_branch()
    dmp_op("GET_SIBLING", st, br)
    x = orel(a, SIBLING)
    set_var(st, x)
    do_branch(x <> 0, br)

  ' GET_CHILD
  ElseIf op_code = &h2 Then
    st = rp()
    br = read_branch()
    dmp_op("GET_CHILD", st, br)
    x = orel(a, CHILD)
    set_var(st, x)
    do_branch(x <> 0, br)

  ' GET_PARENT
  ElseIf op_code = &h3 Then
    st = rp()
    dmp_op("GET_PARENT", st)
    x = orel(a, PARENT)
    set_var(st, x)

  ' GET_PROP_LEN
  ElseIf op_code = &h4 Then
    st = rp()
    dmp_op("!GET_PROP_LEN", st)
    err = 1

  ' INC
  ElseIf op_code = &h5 Then
    dmp_op("INC", -1)
    x = get_var(a)
    If x > 32767 Then x = x - 65536
    x = x + 1
    If x < 0 Then x = 65536 - x
    set_var(a, x)

  ' DEC
  ElseIf op_code = &h6 Then
    dmp_op("DEC", -1)
    x = get_var(a)
    If x > 32767 Then x = x - 65536
    x = x - 1
    If x < 0 Then x = 65536 - x
    set_var(a, x)

  ' PRINT_ADDR
  ElseIf op_code = &h7 Then
    dmp_op("!PRINT_ADDR", -1)
    err = 1

  ' REMOVE_OBJ
  ElseIf op_code = &h9 Then
    dmp_op("!REMOVE_OBJ", -1)
    err = 1

  ' PRINT_OBJECT
  ElseIf op_code = &hA Then
    dmp_op("PRINT_OBJECT", -1)
    print_obj(a)
    If dbg Then dbg = dbg Or BIT(7)

  ' RET
  ElseIf op_code = &hB Then
    dmp_op("RET", -1)
    do_return(a)

  ' JUMP
  ElseIf op_code = &hC Then
    dmp_op("JUMP", -1)
    If a And &h8000 Then a = a - 65536
    pc = pc + a - 2

  ' PRINT_PADDR
  ElseIf op_code = &hD Then
    dmp_op("PRINT_PADDR", -1)
    _ = print_zstring(a * 2)
    If dbg Then dbg = dbg Or BIT(7)

  ' LOAD
  ElseIf op_code = &hE Then
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
  If op_code = &h0 Then
    dmp_op("RTRUE", -1)
    do_return(1)

  ' RFALSE
  ElseIf op_code = &h1 Then
    dmp_op("RFALSE", -1)
    do_return(0)

  ' PRINT
  ElseIf op_code = &h2 Then
    dmp_op("PRINT", -1)
    pc = pc + print_zstring(pc)
    If dbg Then dbg = dbg Or BIT(7)

  ' PRINT_RET
  ElseIf op_code = &h3 Then
    dmp_op("!PRINT_RET", -1)
    pc = pc + print_zstring(pc)
    err = 1

  ' RET_POPPED
  ElseIf op_code = &h8 Then
    dmp_op("RET_POPPED", -1)
    x = pop()
    do_return(x)

  ' NEWLINE
  ElseIf op_code = &hB Then
    dmp_op("NEWLINE", -1)
    Print

  Else
    err = 1
  EndIf
End Sub

Sub _varop
  Local a, b, c, st, br, x

  ' CALL
  If op_code = &h0 Then
    do_call()

  ' STOREW
  ElseIf op_code = &h1 Then
    a = get_op(0)
    b = get_op(1)
    c = get_op(2)
    dmp_op("STOREW", -1)
    ww(a + 2 * b, c)

  ' STOREB
  ElseIf op_code = &h2 Then
    a = get_op(0)
    b = get_op(1)
    c = get_op(2)
    dmp_op("STOREB", -1)
    wb(a + b, c)

  ' READ
  ElseIf op_code = &h4 Then
    a = get_op(0)
    b = get_op(1)
    dmp_op("!READ", -1)
    err = 1

  ' PRINT_CHAR
  ElseIf op_code = &h5 Then
    a = get_op(0)
    dmp_op("PRINT_CHAR", -1)
    Print Chr$(a);
    If dbg Then dbg = dbg Or BIT(7)

  ' PRINT_NUM
  ElseIf op_code = &h6 Then
    a = get_op(0)
    dmp_op("PRINT_NUM", -1)
    Print Str$(a);
    If dbg Then dbg = dbg Or BIT(7)

  Else
    err = 1
  EndIf
End Sub

' Reads branch offset.
' @return bits 0-15 - new value for the program counter.
'                   - if = pc - 2 then -> return false.
'                   - if = pc - 1 then -> return true.
'         bit 16    - set = branch on True, unset = branch on False.
Function read_branch
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

' Gets the value of an operand.
' For VARIABLE operands gets the value of the referenced Variable.
Function get_op(i)
  Local a
  a = op_value(i)
  If op_type(i) = OT_VARIABLE Then get_op = get_var(a) Else get_op = a
End Function

Sub do_branch(z, br)
  If Not (z = (br And &h10000) > 0) Then Exit Sub
  If (br And &hFFFF) = pc - 2 Then do_return(0) : Exit Sub
  If (br And &hFFFF) = pc - 1 Then do_return(1) : Exit Sub
  pc = br And &hFFFF ' Bottom 16-bits
End Sub

Sub do_return(x)
  Local st
  Do While sp > fp + 2 : _ = pop() : Loop
  pc = pop()
  st = pop()
  fp = pop()
  set_var(st, x)
  dmp_stack()
End Sub

Sub do_call
  Local args(2), i, locals_sz, new_pc, st, x

  new_pc = 2 * get_op(0)
  For i = 1 To op_num - 1 : args(i - 1) = get_op(i) : Next i
  st = rp()

  dmp_op("CALL", st)

  ' When address 0 is called, nothing happens and return value is false
  If new_pc = 0 Then set_var(st, 0) : Exit Sub

  push(fp)
  fp = sp
  push(st)
  push(pc)
  pc = new_pc
  locals_sz = rp()
  push(locals_sz)
  For i = 0 To locals_sz - 1
    x = rp() * 256 + rp()
    If i > op_num - 2 Then push(x) Else push(args(i))
  Next i

  dmp_routine(new_pc)
  dmp_stack()
End Sub

Sub init
  Local i

  Print "Loading "; file$

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
  _ = print_zstring(ad)
End Sub

Sub cont(a)
  If a = 0 Then a = &hFFFF
  For i = 0 To a - 1
    do_op()
    If a = &hFFFF Then i = 0
    If err <> 0 Then i = a - 1
  Next i
End Sub

Library Load "util"

If dbg Then
  Library Load "debug.lib"
Else
  Library Load "nodebug.lib"
EndIf

Memory
Print
init()
Print

num_ops = 0
Timer = 0

cont(&hFFFF)

Print
Print "Num instructions processed ="; num_ops
Print "Instructions / second      ="; num_ops / (Timer / 1000)
Print "Num page faults            ="; pf
Print
Memory
Print
