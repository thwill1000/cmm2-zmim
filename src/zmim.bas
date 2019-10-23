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
NUM_PAGES = 50
PAGE_SIZE = 1024 ' TODO: 512
MEM_SIZE = NUM_PAGES * PAGE_SIZE ' in bytes

' Memory addresses below this are read on startup and not swapped in/out
' - not properly set until the z-machine header is read
BASE_STATIC = PAGE_SIZE

FILE_LEN = PAGE_SIZE
GLOBAL_VAR = 0

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

Dim mem(NUM_PAGES * PAGE_SIZE \ 4)

' Map of virtual pages -> physical pages such that the
' n'th byte is the physical page of the n'th virtual page
Dim mem_map(128 * 1024 \ PAGE_SIZE)

next_page = 0

Dim stack(511)
sp = 0

' TODO: I have recently found I can use a Local variable for this
Dim zchar(2)

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
  pp = Peek(Var mem_map(0), vp)
  If pp = 0 Then pp = load_page(vp)

  paddr = pp * PAGE_SIZE + of
End Function

Function readb(ad)
  If ad < 0 Or ad >= FILE_LEN Then Error
  readb = Peek(Var mem(0), paddr(ad))
End Function

Function readw(ad)
  Local pa1, pa2

  If ad < 0 Or ad >= FILE_LEN - 1 Then Error

  pa1 = paddr(ad)
  pa2 = paddr(ad + 1)

  ' Does this ever happen?
  ' If not then pa2 = pa + 1
  If pa1 + 1 <> pa2 Then Print "Unaligned word read!"

  readw = Peek(Var mem(0), pa1) * 256 + Peek(Var mem(0), pa2)
End Function

Sub writeb(ad, x)
  If ad < 0 Or ad >= BASE_STATIC Then Error
  If x < 0 Or ad > 255 Then Error
  Poke Var mem(0), ad, x
End Sub

Sub writew(ad, x)
  If ad < 0 Or ad >= BASE_STATIC - 1 Then Error
  If x < 0 Or ad > MAX_WORD Then Error
  Poke Var mem(0), ad, x \ 256
  Poke Var mem(0), ad + 1, x Mod 256
End Sub

' Loads 'src' page of 'file$' into 'dest' page of 'mem'
' TODO: open the file once globally and keep it open until exit
Sub mem_load(src, dest)
  Local ad, buf$, buf_size, i, to_read

  Open file$ For random As #1
  Seek #1, src * PAGE_SIZE + 1
  ad = dest * PAGE_SIZE
  to_read = PAGE_SIZE
  buf_size = 255
  Do While to_read > 0
    If to_read < 255 Then buf_size = to_read
    buf$ = Input$(buf_size, 1)
    For i = 1 To buf_size
      Poke Var mem(0), ad, Peek(Var buf$, i)
      ad = ad + 1
    Next i
    to_read = to_read - buf_size
  Loop
  Close #1

  Poke Var mem_map(0), src, dest
End Sub

Function load_page(p)
  mem_load(p, next_page)
  load_page = next_page
  next_page = next_page + 1

  ' TODO: Handle reusing an existing page
  If next_page = NUM_PAGES Then Error "No more pages"
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

' TODO: extract constants for shifts, e.g.
' SHIFT_1_BIT = 2
' SHIFT_2_BIT = 4
Function rshift(v, num)
  rshift = v\(2^num)
End Function

Function hex2$(byte)
  If byte <= &hF Then hex2$ = "0"
  hex2$ = hex2$ + Hex$(byte)
End Function

' Prints 'sz' bytes from 'mem' starting at 'addr'
Sub dump_mem(addr, sz)
  Local i, x
  For i = 0 To sz - 1
    x = readb(addr + i)
    Print hex2$(x); " ";
    If (i + 1) Mod 16 = 0 Then Print ""
  Next i
  Print ""
End Sub

Sub dump_mem_map
  Local i
  For i = 0 To NUM_PAGES - 1
    Print Str$(i); " -> "; Str$(Peek(Var mem_map(0), i))
    If (i + 1) Mod 20 = 0 Then more()
  Next i
End Sub

' Prints zmachine header from start of 'mem$'
Sub dump_header
  Local i, serial$

  Print "Version      =";  readb(&h00)
  Print "Flags1       = "; Bin$(readb(&h01))
  Print "Release      =";  readw(&h02)
  Print "Base high    = "; Hex$(readw(&h04))
  Print "Initial PC   = "; Hex$(readw(&h06))
  Print "Dictionary   = "; Hex$(readw(&h08))
  Print "Object tbl   = "; Hex$(readw(&h0A))
  Print "Glob var tbl = "; Hex$(readw(&h0C))
  Print "Base static  = "; Hex$(readw(&h0E))
  Print "Flags2       = "; Bin$(readb(&h10))
  serial$ = "000000"
  For i = 0 To 5
    Poke Var serial$, i + 1, readb(&H12 + i)
  Next i
  Print "Serial num   = "; serial$
  Print "Abbrv tbl    = "; Hex$(readw(&H18))
  Print "File length  ="; 2 * readw(&H1A)
  Print "File chksum  ="; readw(&H1C)
  Print "Std revision ="; readw(&H32)
  more()
End Sub

' Returns the number of bytes read
Function dump_zstring(addr)
  Local abbrv, ad, al, ch, i, x, z0, z1, z2

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
        ' Make local copy of 'zchar' because we will
        ' be making a recursive call into dump_zstring
        z0 = zchar(0) : z1 = zchar(1) : z2 = zchar(2)
        dump_abbrv((abbrv - 1) * 32 + ch)
        zchar(0) = z0 : zchar(1) = z1 : zchar(2) = z2
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

  dump_zstring = ad - addr
End Function

Sub dump_abbrv(idx)
  Local ad, x
  ad = readw(&h18)
  x = readw(ad + idx * 2)
  devnull = dump_zstring(x * 2)
End Sub

Sub dump_all_abbrv
  Local i
  For i = 0 To 95
    Print Str$(i); ": {";
    dump_abbrv(i)
    Print "}"
    If (i + 1) Mod 20 = 0 Then more()
  Next i
End Sub

Sub more
  Local a$
  Print ""
  Input "More...", a$
  Print ""
End Sub

Sub dump_dictionary
  Local addr_dict, n, i

  addr_dict = readw(&h8)
  Print ""
  n = readb(addr_dict)
  Print "n = "; n; ""
  codes$ = Space$(n)
  For i = 1 To n
    Poke Var codes$, i, readb(addr_dict + i)
  Next i
  Print "codes        = "; codes$
  entry_length = readb(addr_dict + n * 2 + 1)
  Print "entry length = "; entry_length
  num_entries = readb(addr_dict + n * 2 + 2 + 2)
  Print "num entries  = "; num_entries
  addr_entries = addr_dict + n * 2 + 4 + 4
  Print Hex$(addr_entries)
  more()
  For i = 0 To num_entries
    dump_zstring(addr_entries)
    dump_zstring(addr_entries + 2)
    addr_entries = addr_entries + entry_length
    Print ""
  Next i
End Sub

' Decodes instruction at 'pc' to 'op_*' vars
Sub decode_op()
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

Function rpad$(s$, i)
  Local a
  a = Len(s$)
  If (a < i) Then
    rpad$ = s$ + Space$(i - a)
  Else
    rpad$ = s$
  EndIf
End Function

' Returns a string representation of the last decoded instruction
Function format_op$()
  Local i, x$, y$

  x$ = "VAR:"
  If op_num < 3 Then x$ = Str$(op_num) + "OP:"
  x$ = hex2$(op_code) + " " + x$ + Str$(op)
  x$ = rpad$(x$, 10)

  For i = 0 To op_num - 1
    If op_type(i) = OT_LARGE_CONST Then
      y$ = " L,"
    ElseIf op_type(i) = OT_SMALL_CONST Then
      y$ = " S,"
    ElseIf op_type(i) = OT_VARIABLE Then
      y$ = " V,"
    Else
      y$ = " O,"
    EndIf

    y$ = y$ + Str$(op_value(i))
    x$ = x$ + rpad$(y$, 8)
  Next i

  format_op$ = x$
End Function

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
    call
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

Sub call
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
  Print ""
End Sub

Sub dec_chk
  Local a, b, x
  Print "  dec_chk:";
  a = get_op(0)
  b = get_op(1)
  Print ""
  x = get_var(a)
  set_var(a, x - 1)
  branch(x < b)
End Sub

Sub inc
  Local a
  Print "  inc:";
  a = get_op(0)
  Print ""
  x = get_var(a)
  set_var(a, x + 1)
End Sub

Sub je
  Local a, b
  Print "  je:";
  a = get_op(0)
  b = get_op(1)
  Print ""
  branch(a = b)
End Sub

Sub jump
  Local a
  Print "  jump:";
  a = get_op(0) ' TODO: interpret as SIGNED
  Print ""
  pc = pc + a - 2
End Sub

Sub jz
  Local a
  Print "jz:";
  a = get_op(0)
  Print ""
  branch(a = 0)
End Sub

Sub newline
  Print ""
End Sub

Sub print_
  pc = pc + dump_zstring(pc)
  Print ""
End Sub

Sub store
  Local a, b
  Print "store:";
  a = get_op(0)
  b = get_op(1)
  Print ""
  set_var(a, b)
End Sub

Sub main_loop
  Local i

  pc = readw(&h6)
  For i = 0 To 10
    Print "{"; Hex$(pc); "} ";
    decode_op()
    Print format_op$()
    perform_op()
    If (i + 1) Mod 20 = 0 Then more()
  Next i
End Sub

Sub zload
  Local i, num_pages

  Print "Loading "; file$

  ' Load page 0 / the header
  Print "  Header page: 0"
  devnull = load_page(0)
  'dump_header()
  GLOBAL_VAR = readw(&h0C)
  BASE_STATIC = readw(&h0E)
  FILE_LEN = readw(&h1A) * 2

  num_pages = BASE_STATIC \ PAGE_SIZE
  If BASE_STATIC Mod PAGE_SIZE > 0 Then num_pages = num_pages + 1

  Print "  Dynamic pages: ";
  For i = 1 To num_pages
    If i > 1 Then Print ", ";
    Print Str$(i);
    devnull = load_page(i)
  Next i
  Print ""

  Print "  Paged memory starts at page "; Str$(num_pages + 1)
End Sub

Memory
Print ""

zload()
Print ""

main_loop

Print ""
Memory
