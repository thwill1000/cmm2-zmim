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
ALPHABET$ =             " 123[]abcdefghijklmnopqrstuvwxyz"
ALPHABET$ = ALPHABET$ + " 123[]ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET$ = ALPHABET$ + " 123[]@^0123456789.,!?_#'" + Chr$(34) + "/\-:()"
ALPHABET_0_OFFSET = 1
ALPHABET_1_OFFSET = 33
ALPHABET_2_OFFSET = 65
MAX_NUM_OPERANDS = 4 ' requires up to 8 for z4+
FORM_LONG = 1
FORM_SHORT = 2
FORM_VARIABLE = 3
OT_LARGE_CONST = &b00
OT_SMALL_CONST = &b01
OT_VARIABLE = &b10
OT_OMITTED = &b11

Dim mem(NUM_PHYSICAL_PAGES * PAGE_SIZE \ 4)

' Map of physical pages -> virtual pages
Dim pp_to_vp(NUM_PHYSICAL_PAGES - 1)

' Map of virtual pages -> physical pages
Dim vp_to_pp(NUM_VIRTUAL_PAGES - 1)

next_page = 0

page_faults = 0

Dim stack(511)
sp = 0

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

' Converts a virtual address to a physical address
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

Function readb(ad)
  If ad < 0 Or ad >= FILE_LEN Then Error Hex$(ad)
  readb = Peek(Var mem(0), paddr(ad))
End Function

Function readw(ad)
  Local pa1, pa2

  If ad < 0 Or ad >= FILE_LEN - 1 Then Error Hex$(ad)

  pa1 = paddr(ad)
  pa2 = paddr(ad + 1)

  ' Does this ever happen?
  ' If not then pa2 = pa + 1
  If pa1 + 1 <> pa2 Then Print "Unaligned word read!"

  readw = Peek(Var mem(0), pa1) * 256 + Peek(Var mem(0), pa2)
End Function

Sub writeb(ad, x)
  If ad < 0 Or ad >= BASE_STATIC Then Error Hex$(ad)
  If x < 0 Or x > 255 Then Error Str$(x)
  Poke Var mem(0), ad, x
End Sub

Sub writew(ad, x)
  If ad < 0 Or ad >= BASE_STATIC - 1 Then Error Hex$(ad)
  If x < 0 Or x > MAX_WORD Then Error Str$(x)
  Poke Var mem(0), ad, x \ 256
  Poke Var mem(0), ad + 1, x Mod 256
End Sub

' Loads 'src' page of 'file$' into 'mem'
' Returns destination / physical page number
' TODO: open the file once globally and keep it open until exit
Function mem_load(vp)
  Local ad, buf$, buf_sz, i, pp, to_read

  pp = next_page

  ' TODO: Implement some form of Least Recently Used algorithm
  next_page = next_page + 1
  If next_page = NUM_PHYSICAL_PAGES Then
    next_page = FIRST_SWAP_PAGE
  EndIf

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

Function get_var(i)
  If i = 0 Then
    ' TODO: Pop from stack
  ElseIf i < &h10 Then
    ' TODO: Local variable
    get_var = stack(i)
  ElseIf i <= &hFF Then
    get_var = readw(GLOBAL_VAR + 2 * (i - &h10))
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Function

Sub set_var(i, x)
  If i = 0 Then
    ' TODO: Push to stack
  ElseIf i < &h10 Then
    ' TODO: Local variable
    stack(i) = x
  ElseIf i < &hFF Then
    writew(GLOBAL_VAR + 2 * (i - &h10))
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Sub

' Returns the number of bytes read
Function dmp_zstring(addr)
  Local abbrv, ad, al, ch, i, x, zchar(2)

  abbrv = 0
  ad = addr
  al = ALPHABET_0_OFFSET
  x = 0
  ' Should be 'Do While x = 0' but seems to be an issue
  ' using Do While in recursive functions.
  start_loop:
    If x > 0 Then GoTo exit_loop

    x = readw(ad)

    For i = 2 To 0 Step -1
      zchar(i) = x And &b11111
      x = rshift(x, 5)
    Next i

    ' x is now the top-bit of the word. If x = 1 then we have reached the end
    ' of the string and will exit the loop after this iteration.

    For i = 0 To 2
      ch = zchar(i)
      If abbrv > 0 Then
        dmp_abrv((abbrv - 1) * 32 + ch)
        abbrv = 0
      ElseIf ch > 0 And ch < 4 Then
        abbrv = ch
      ElseIf ch = 4 Then
        al = ALPHABET_1_OFFSET
      ElseIf ch = 5 Then
        al = ALPHABET_2_OFFSET
      Else
        Print Mid$(ALPHABET$, ch + al, 1);
        al = ALPHABET_0_OFFSET
      EndIf
    Next i

    ad = ad + 2
    GoTo start_loop
  exit_loop:

  dmp_zstring = ad - addr
End Function

Sub dmp_abrv(idx)
  Local ad, x
  ad = readw(&h18)
  x = readw(ad + idx * 2)
  devnull = dmp_zstring(x * 2)
End Sub

Sub more
  Local a$
  Print
  Input "More...", a$
  Print
End Sub

' Decodes instruction at 'pc' to 'op_*' vars
Sub decode_op
  Local i, x

  op = readb(pc)
  pc = pc + 1

  If op <= &h7F Then

    op_form = FORM_LONG
    op_code = op And &b00011111
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
    op_code = op And &b00001111
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
    op_code = op And &b00011111
    op_num = 4
  '  If op <= &hDF Then
  '    op_num = 2
  '  Else
  '    op_num = 4 ' actually VAR
  '  EndIf

    ' Read operand types
    x = readb(pc)
    pc = pc + 1
    For i = 3 To 0 Step -1
      op_type(i) = x And &b00000011
      If op_type(i) = OT_OMITTED Then op_num = op_num - 1
      x = rshift(x, 2)
    Next i

  EndIf

  ' Read operands
  For i = 0 To op_num - 1
    If op_type(i) = OT_LARGE_CONST Then
      op_value(i) = readw(pc)
      pc = pc + 2
    ElseIf op_type(i) = OT_OMITTED Then
      ' Do nothing
    Else
      op_value(i) = readb(pc)
      pc = pc + 1
    EndIf
  Next i

End Sub

' Performs the last decoded instruction
Sub perform_op
  If op = 4 Then
    dec_chk
  ElseIf op = 13 Or op = 77 Then
    store
  ElseIf op = 84 Or op = 116 Then
    add
  ElseIf op = 97 Then
    je
  ElseIf op = 140 Then
    jump
  ElseIf op = 160 Then
    jz
  ElseIf op = 165 Then
    inc
  ElseIf op = 178 Then
    print_
  ElseIf op = 187 Then
    newline
  ElseIf op = 224 Then
    call_
  Else
    Error "Unsupported instruction"
  EndIf
End Sub

Sub branch(cond)
  Local b1, b2, of
  b1 = readb(pc)
  pc = pc + 1

  If b1 And &b10000000 = 0 Then
    Print "  branch on false, ";
    Error "Not implemented yet"
  Else
    Print "  branch on true, ";
  EndIf

  of = b1 And &b00111111
  If b1 And &b01000000 = 1 Then
    Print "1 byte";
  Else
    Print "2 byte";
    b2 = readb(pc)
    pc = pc + 1
    of = 256 * of + b2
    ' TODO: 'of' should be SIGNED!!!
  EndIf
  Print " offset = "; Str$(of)

  If cond Then
    pc = pc + of - 2
  EndIf

  ' TODO: of = 0 -> return false
  '       of = 1 -> return true
End Sub

Function get_op(i)
  Local a

  a = op_value(i)
  If op_type(i) = OT_VARIABLE Then
    Print " ("; Str$(a); ":";
    a = get_var(a)
    Print " "; Str$(a); ")";
  Else
    Print " "; Str$(a);
  EndIf

  get_op = a
End Function

Sub add
  Local a, b, dest, result
  Print "  add:";
  a = get_op(0)
  b = get_op(1)
  dest = readb(pc)
  pc = pc + 1
  Print " -> "; Str$(dest)
  set_var(dest, a + b)
  Print "  result ="; get_var(dest)
End Sub

Sub call_
  Local i, num_local, x

  Print "  call: "; Hex$(2 * op_value(0))
  pc = 2 * op_value(0)
  num_local = readb(pc)
  Print "  " + Str$(num_local); ":";
  pc = pc + 1
  For i = 0 To num_local - 1
    x = readw(pc)
    pc = pc + 2
    Print x; ", ";
  Next i
  Print
End Sub

Sub dec_chk
  Local a, b, x
  Print "  dec_chk:";
  a = get_op(0)
  b = get_op(1)
  Print
  x = get_var(a)
  set_var(a, x - 1)
  branch(x < b)
End Sub

Sub inc
  Local a
  Print "  inc:";
  a = get_op(0)
  Print
  x = get_var(a)
  set_var(a, x + 1)
End Sub

Sub je
  Local a, b
  Print "  je:";
  a = get_op(0)
  b = get_op(1)
  Print
  branch(a = b)
End Sub

Sub jump
  Local a
  Print "  jump:";
  a = get_op(0) ' TODO: interpret as SIGNED
  Print
  pc = pc + a - 2
End Sub

Sub jz
  Local a
  Print "jz:";
  a = get_op(0)
  Print
  branch(a = 0)
End Sub

Sub newline
  Print
End Sub

Sub print_
  pc = pc + dmp_zstring(pc)
  Print
End Sub

Sub store
  Local a, b
  Print "store:";
  a = get_op(0)
  b = get_op(1)
  Print
  set_var(a, b)
End Sub

Sub main
  Local i

  init()

  For i = 0 To 10
    Print "{"; Hex$(pc); "} ";
    decode_op()
    Print fmt_op$()
    perform_op()
    If (i + 1) Mod 20 = 0 Then more()
  Next i
End Sub

Sub init
  Local i

  Print "Loading "; file$

  ' Load page 0 which contains the header
  Print "  Header page: 0"
  If (mem_load(0) <> 0) Then Error

  ' Read header data
  pc = readw(&h06)
  GLOBAL_VAR = readw(&h0C)
  BASE_STATIC = readw(&h0E)
  FILE_LEN = readw(&h1A) * 2

  ' Initialise dynamic memory
  FIRST_SWAP_PAGE = BASE_STATIC \ PAGE_SIZE
  If BASE_STATIC Mod PAGE_SIZE > 0 Then FIRST_SWAP_PAGE = FIRST_SWAP_PAGE + 1
  Print "  Dynamic pages: ";
  For i = 1 To FIRST_SWAP_PAGE - 1
    If i > 1 Then Print ", ";
    Print Str$(i);
    If (mem_load(i) <> i) Then Error
  Next i
  Print
  Print "  Paged memory starts at page "; Str$(FIRST_SWAP_PAGE)
End Sub

Sub main_loop
  Local i

  For i = 0 To 10
    Print "{"; Hex$(pc); "} ";
    decode_op()
    Print fmt_op$()
    perform_op()
    If (i + 1) Mod 20 = 0 Then more()
  Next i
End Sub

Library Load "util"
Library Load "dmp_mem"
Library Load "fmt_op"

Memory
Print
init()
Print
main_loop()
Print
Print "Num page faults ="; page_faults
Memory
