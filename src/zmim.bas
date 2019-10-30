' ZMIM a Z-Machine Interpreter for the Colour Maximite
' Copyright (c) 2019 Thomas Hugo Williams
' For Maximite BASIC v4.5C

Mode 1
Cls

file$ = "B:\zmim\examples\minizork.z3"
'file$ = "B:\zmim\examples\advent.z3"
'file$ = "B:\zmim\examples\ZORK1\DATA\ZORK1.DAT"

' By convention variables declared in UPPER CASE are constant
'  - this is not enforced by the language!
PAGE_SIZE = 512
NUM_PHYSICAL_PAGES = 100
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
FORM_LONG = 1
FORM_SHORT = 2
FORM_VARIABLE = 3
OT_LARGE_CONST = &b00
OT_SMALL_CONST = &b01
OT_VARIABLE = &b10
OT_OMITTED = &b11

BIT_6       = &b01000000
BIT_7       = &b10000000
BIT_15      = &b1000000000000000
BTM_2_BITS  = &b00000011
BTM_4_BITS  = &b00001111
BTM_5_BITS  = &b00011111
BTM_6_BITS  = &b00111111
BTM_15_BITS = &b0111111111111111

Dim mem(NUM_PHYSICAL_PAGES * PAGE_SIZE \ 4)

' Map of physical pages -> virtual pages
Dim pp_to_vp(NUM_PHYSICAL_PAGES - 1)

' Map of virtual pages -> physical pages
Dim vp_to_pp(NUM_VIRTUAL_PAGES - 1)

next_page = 0

page_faults = 0

Dim stack(511)
sp = -1

' Current stack frame pointer
fp = -1

' Variable to assign unused result of a Function call to
devnull = 0

pc = 0

' The currently decoded operation
op = 0
op_code = 0
op_form = 0
op_num = 0 ' number of operands
Dim op_type(MAX_NUM_OPERANDS)
Dim op_value(MAX_NUM_OPERANDS)

new_line = 0

' Converts a virtual address to a physical address.
Function paddr(va)
  Local of, pp, vp

  ' TODO: in practice any page below the one
  '       containing BASE_STATIC is unpaged
  If va < BASE_STATIC Then
    paddr = va
    Exit Function
  EndIf

  vp = va \ PAGE_SIZE
  of = va Mod PAGE_SIZE
  pp = vp_to_pp(vp)
  If pp = 0 Then
    pp = mem_load(vp)
    page_faults = page_faults + 1
  EndIf

  paddr = pp * PAGE_SIZE + of
End Function

' Reads a byte from 'pc' and increments 'pc'.
Function pcreadb
  pcreadb = readb(pc)
  pc = pc + 1
End Function

' Reads a byte from 'ad'.
Function readb(ad)
  If ad < 0 Or ad >= FILE_LEN Then Error Hex$(ad)
  readb = Peek(Var mem(0), paddr(ad))
End Function

' Reads a 16-bit word from 'pc' and increments 'bc' by 2.
Function pcreadw
  pcreadw = readw(pc)
  pc = pc + 2
End Function

' Reads a 16-bit word from 'ad'.
Function readw(ad)
  Local pa1, pa2

  If ad < 0 Or ad >= FILE_LEN - 1 Then Error Hex$(ad)

  pa1 = paddr(ad)
  pa2 = paddr(ad + 1)

  ' Does this ever happen? If not then pa2 = pa + 1
  ' Apparently it can happen
  ' If pa1 + 1 <> pa2 Then Error "Unaligned word read"

  readw = Peek(Var mem(0), pa1) * 256 + Peek(Var mem(0), pa2)
End Function

' Writes byte 'x' to 'ad'.
Sub writeb(ad, x)
  If ad < 0 Or ad >= BASE_STATIC Then Error Hex$(ad)
  If x < 0 Or x > 255 Then Error Str$(x)
  Poke Var mem(0), ad, x
End Sub

' Writes 16-bit word 'x' to 'ad'.
Sub writew(ad, x)
  If ad < 0 Or ad >= BASE_STATIC - 1 Then
    Error "Non dynamic mem write: " + Hex$(ad)
  EndIf
  If x < 0 Or x > MAX_WORD Then Error Str$(x)
  Poke Var mem(0), ad, x \ 256
  Poke Var mem(0), ad + 1, x Mod 256
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
  If next_page = NUM_PHYSICAL_PAGES Then
    next_page = FIRST_SWAP_PAGE
  EndIf

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
      Poke Var mem(0), ad, Peek(Var buf$, i)
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
    get_var = readw(GLOBAL_VAR + 2 * (i - &h10))
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
    writew(GLOBAL_VAR + 2 * (i - &h10), x)
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Sub

' Prints ZString starting at 'addr'.
' @return the number of bytes read.
Function print_zstring(addr)
  Local abbrv, ad, al, ch, i, x, zchar(2)

  abbrv = 0
  ad = addr
  al = 0
  x = 0
  ' Should be 'Do While x = 0' but seems to be an issue
  ' using Do While in recursive functions.
  start_loop:
    If x > 0 Then GoTo exit_loop

    x = readw(ad)

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
    GoTo start_loop
  exit_loop:

  print_zstring = ad - addr
End Function

' Prints abbreviation 'i'.
Sub print_abrv(i)
  Local ad, x
  ad = readw(&h18)
  x = readw(ad + i * 2)
  devnull = print_zstring(x * 2)
End Sub

Sub more
  Local a$
  Print
  Input "More...", a$
  Print
End Sub

' Decodes instruction at 'pc' into 'op_*' vars.
Sub decode_op
  Local i, x

  op = pcreadb()

  If op <= &h7F Then

    op_form = FORM_LONG
    op_code = op And BTM_5_BITS
    op_num = 2

    If op <= &h1F Then
      op_type(0) = OT_SMALL_CONST
      op_type(1) = OT_SMALL_CONST
    ElseIf op <= &h3F Then
      op_type(0) = OT_SMALL_CONST
      op_type(1) = OT_VARIABLE
    ElseIf op <= &h5F Then
      op_type(0) = OT_VARIABLE
      op_type(1) = OT_SMALL_CONST
    Else
      op_type(0) = OT_VARIABLE
      op_type(1) = OT_VARIABLE
    EndIf

  ElseIf op <= &hBF Then

    op_form = FORM_SHORT
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
    op_form = FORM_VARIABLE
    op_code = op And BTM_5_BITS
    op_num = 4
  '  If op <= &hDF Then
  '    op_num = 2
  '  Else
  '    op_num = 4 ' actually VAR
  '  EndIf

    ' Read operand types
    x = pcreadb()
    For i = 3 To 0 Step -1
      op_type(i) = x And BTM_2_BITS
      If op_type(i) = OT_OMITTED Then op_num = op_num - 1
      x = rshift(x, 2)
    Next i

  EndIf

  ' Read operands
  For i = 0 To op_num - 1
    If op_type(i) = OT_LARGE_CONST Then
      op_value(i) = pcreadw()
    ElseIf op_type(i) = OT_OMITTED Then
      ' Do nothing
    Else
      op_value(i) = pcreadb()
    EndIf
  Next i

End Sub

' Performs the last decoded instruction.
Sub perform_op
  If op = &h04 Then
    dec_chk
  ElseIf op = &h05 Then
    inc_chk
  ElseIf op = &h0D Or op = &h2D Or op = &h4D Then
    store
  ElseIf op = &h0F Or op = &h4F Then
    loadw
  ElseIf op = &h30 Then
    loadb
  ElseIf op = &h41 Or op = &h61 Then
    je
  ElseIf op = &h46 Then
    jin
  ElseIf op = &h54 Or op = &h74 Then
    add
  ElseIf op = &h55 Then
    sub_
  ElseIf op = &h6E Then
    insert_obj
  ElseIf op = &h8C Then
    jump
  ElseIf op = &h9B Or op = &hAB Then
    ret
  ElseIf op = &hA0 Then
    jz
  ElseIf op = &hA5 Then
    inc
  ElseIf op = &hAD Then
    print_paddr
  ElseIf op = &hB0 Then
    rtrue
  ElseIf op = &hB1 Then
    rfalse
  ElseIf op = &hB2 Then
    print_
  ElseIf op = &hBB Then
    newline
  ElseIf op = &hC9 Then
    and_
  ElseIf op = &hE0 Then
    call_
  ElseIf op = &hE1 Then
    storew
  ElseIf op = &hE5 Then
    print_char
  ElseIf op = &hE6 Then
    print_num
  Else
    Error "Unsupported instruction " + Hex$(op)
  EndIf
End Sub

' Reads branch offset.
' @return bits 0-14 - new value for the program counter.
'                   - if = pc - 2 then -> return false.
'                   - if = pc - 1 then -> return true.
'         bit 15    - set = branch on True, unset = branch on False.
Function read_branch
  Local a, of
  a = pcreadb()
  of = a And BTM_6_BITS

  If a And BIT_6 = 0 Then
    of = 256 * of + pcreadb()
    If a And BIT_5 Then
      of = of - 16384
    EndIf
  EndIf

  read_branch = pc + of - 2
  If a And BIT_7 Then read_branch = read_branch Or BIT_15
End Function

' Gets the value of an operand.
' For VARIABLE operands gets the value of the referenced Variable.
Function get_op(i)
  Local a
  a = op_value(i)
  If op_type(i) = OT_VARIABLE Then get_op = get_var(a) Else get_op = a
End Function

Sub do_branch(z, br)
  Local new_pc
  If Not (z = (br And BIT_15) > 0) Then Exit Sub
  new_pc = br And BTM_15_BITS
  If new_pc = pc - 2 Then
    do_return(0)
  ElseIf new_pc = pc - 1 Then
    do_return(1)
  Else
    pc = new_pc
  EndIf
End Sub

Sub do_return(x)
  Local st
  Do While sp > fp + 2 : devnull = pop() : Loop
  pc = pop()
  st = pop()
  fp = pop()
  set_var(st, x)
End Sub

Sub add
  Local a, b, st
  a = get_op(0)
  b = get_op(1)
  st = pcreadb()
  dmp_op("ADD", st)
  set_var(st, a + b)
End Sub

Sub and_
  Local a, b, st
  a = get_op(0)
  b = get_op(1)
  st = pcreadb()
  dmp_op("AND", st)
  set_var(st, a And b)
End Sub

Sub call_
  Local args(2), i, locals_sz, new_pc, st, x

  new_pc = 2 * op_value(0)
  For i = 1 To op_num - 1
    args(i - 1) = get_op(i)
  Next i
  st = pcreadb()

  dmp_op("CALL", st)

  push(fp)
  fp = sp
  push(st)
  push(pc)
  pc = new_pc
  locals_sz = pcreadb()
  push(locals_sz)
  For i = 0 To locals_sz - 1
    x = pcreadw()
    If i > op_num - 2 Then push(x) Else push(args(i))
  Next i

  dmp_routine(new_pc)
  dmp_stack()
End Sub

Sub dec_chk
  Local a, b, br, x
  a = get_op(0)
  b = get_op(1)
  br = read_branch()
  dmp_op("DEC_CHK", -1, br)
  x = get_var(a)
  x = x - 1
  set_var(a, x)
  do_branch(x < b, br)
End Sub

Sub inc
  Local a
  a = get_op(0)
  dmp_op("INC", -1)
  x = get_var(a)
  set_var(a, x + 1)
End Sub

Sub inc_chk
  Local a, b, br, x
  a = get_op(0)
  b = get_op(1)
  br = read_branch()
  dmp_op("INC_CHK", -1, br)
  x = get_var(a)
  x = x + 1
  set_var(a, x)
  do_branch(x > b, br)
End Sub

Sub je
  Local a, b, br
  a = get_op(0)
  b = get_op(1)
  br = read_branch()
  dmp_op("JE", -1, br)
  do_branch(a = b, br)
End Sub

Sub jump
  Local of
  dmp_op("JUMP", -1)
  of = get_op(i)
  If of And BIT_15 Then of = of - 65536
  pc = pc + of - 2
End Sub

Sub jin
  Local a, b, br
  a = get_op(0)
  b = get_op(1)
  br = read_branch()
  dmp_op("!JIN", -1, br)
End Sub

Sub jz
  Local a, br
  a = get_op(0)
  br = read_branch()
  dmp_op("JZ", -1, br)
  do_branch(a = 0, br)
End Sub

Sub insert_obj
  Local a, b
  a = get_op(0)
  b = get_op(1)
  dmp_op("!INSERT_OBJ", -1)
End Sub

Sub loadb
  Local a, b, st, x
  a = get_op(0)
  b = get_op(1)
  st = pcreadb()
  dmp_op("LOADB", st)
  x = readb(a + b)
  set_var(st, x)
End Sub

Sub loadw
  Local a, b, st, x
  a = get_op(0)
  b = get_op(1)
  st = pcreadb()
  dmp_op("LOADW", st)
  x = readw(a + 2 * b)
  set_var(st, x)
End Sub

Sub newline
  Print
End Sub

Sub print_
  dmp_op("PRINT", -1)
  pc = pc + print_zstring(pc)
  new_line = 1
End Sub

Sub print_char
  Local a
  dmp_op("PRINT_CHAR", -1)
  a = get_op(0)
  Print Chr$(a);
  new_line = 1
End Sub

Sub print_num
  Local a
  a = get_op(0)
  dmp_op("PRINT_NUM", -1)
  Print Str$(a);
  new_line = 1
End Sub

Sub print_paddr
  Local a
  a = get_op(0)
  dmp_op("PRINT_PADDR", -1)
  devnull = print_zstring(a * 2)
  new_line = 1
End Sub

Sub ret
  Local a
  a = get_op(0)
  dmp_op("RET", -1)
  do_return(a)
End Sub

Sub rfalse
  dmp_op("RFALSE", -1)
  do_return(0)
End Sub

Sub rtrue
  dmp_op("RTRUE", -1)
  do_return(1)
End Sub

Sub store
  Local a, b
  a = get_op(0)
  b = get_op(1)
  dmp_op("STORE", -1)
  set_var(a, b)
End Sub

Sub storew
  Local a, b, c
  a = get_op(0)
  b = get_op(1)
  c = get_op(2)
  dmp_op("STOREW", -1)
  writew(a + 2 * b, c)
End Sub

Sub sub_
  Local a, b, st
  a = get_op(0)
  b = get_op(1)
  st = pcreadb()
  dmp_op("SUB", st)
  set_var(st, a - b)
End Sub

Sub init
  Local i

  Print "Loading "; file$

  ' Load page 0 which contains the header.
  Print "  Header page: 0"
  If mem_load(0) <> 0 Then Error

  ' Read header data.
  pc = readw(&h06)
  GLOBAL_VAR = readw(&h0C)
  BASE_STATIC = readw(&h0E)
  FILE_LEN = readw(&h1A) * 2

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

Sub main_loop
  Local i

  For i = 0 To 10
'    If new_line Then Print : new_line = 0
'    Print Hex$(pc); ": ";
    decode_op()
    perform_op()
    If (i + 1) Mod 10 = 0 Then i = 0 ': more()
  Next i
End Sub

Library Load "util"
'Library Load "dmp_hdr"
'Library Load "dmp_mem"
'Library Load "dmp_op"
'Library Load "dmp_stak"
'Library Load "dmp_rout"
Sub dmp_op(m$, st, br) : End Sub
Sub dmp_stack() : End Sub
Sub dmp_routine(new_pc) : End Sub

Memory
Print
init()
Print
main_loop()
Print
Print "Num page faults ="; page_faults
Memory
