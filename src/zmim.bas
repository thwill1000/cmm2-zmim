' ZMIM a Z-Machine Interpreter for the Colour Maximite
' Copyright (c) 2019 Thomas Hugo Williams
' For Maximite BASIC v4.5C

Mode 1

' By convention variables declared in UPPER CASE are constant
'  - this is not enforced by the language!
PAGE_SIZE = 1024 ' TODO: 512
MEM_SIZE = 50 * PAGE_SIZE
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

Dim mem(MEM_SIZE / 4 - 1)
Dim zchar(2)

pc = 0

' The currently decoded operation
op = 0
op_code = 0
op_form = 0
op_num_operands = 0
Dim op_type(MAX_NUM_OPERANDS)
Dim op_value(MAX_NUM_OPERANDS)

Sub check_bounds(value, min, max, emsg$)
  If value < min Or value > max Then
    Error emsg$
  EndIf
End Sub

Sub writeb(addr, value)
  Print addr, value
  check_bounds(addr, 0, MEM_SIZE - 1, "writeb: Invalid address")
  check_bounds(value, 0, 255, "writeb: Invalid value")
  Poke Var mem(0), addr, value
End Sub

Function readb(addr)
  check_bounds(addr, 0, MEM_SIZE - 1, "readb: Invalid address")
  readb = Peek(Var mem(0), addr)
End Function

Sub writew(addr, value)
  check_bounds(addr, 0, MEM_SIZE - 2, "writew: Invalid address")
  check_bounds(value, 0, MAX_WORD, "writew: Invalid value")
  Poke Var mem(0), addr, value \ 256
  Poke Var mem(0), addr + 1, value Mod 256
End Sub

Function readw(addr)
  check_bounds(addr, 0, MEM_SIZE - 2, "readw: Invalid address")
  readw = Peek(Var mem(0), addr) * 256 + Peek(Var mem(0), addr + 1)
End Function

' TODO: extract constants for shifts, e.g.
' SHIFT_1_BIT = 2
' SHIFT_2_BIT = 4
Function rshift(v, num)
  rshift = v\(2^num)
End Function

Function hex2$(byte)
  If byte <= &hF Then
    hex2$ = "0"
  EndIf
  hex2$ = hex2$ + Hex$(byte)
End Function

' Prints 'sz' bytes from 'mem' starting at 'addr'
Sub dump_mem(addr, sz)
  Local i, x

  For i = 0 To sz - 1
    x = readb(addr + i)
    Print hex2$(x); " ";
    If (i + 1) Mod 16 = 0 Then
      Print ""
    EndIf
  Next
  Print ""
End Sub

' Loads bytes from 'file$' into 'mem'
Sub mem_load(file$, page)
  Local ad, buf$, buf_size, i, to_read

  Open file$ For random As #1
  Seek #1, page * PAGE_SIZE + 1
  ad = page * PAGE_SIZE
  to_read = PAGE_SIZE
  buf_size = 255
  Do While to_read > 0
    If to_read < 255 Then
      buf_size = to_read
    EndIf
    buf$ = Input$(buf_size, 1)
    For i = 1 To buf_size
      Poke Var mem(0), ad, Peek(Var buf$, i)
      ad = ad + 1
    Next
    to_read = to_read - buf_size
  Loop
  Close #1
End Sub

' Prints zmachine header from start of 'mem$'
Sub dump_header
  Local i, serial$

  Print "Version      =";  readb(&H0)
  Print "Flags1       = "; Bin$(readb(&H1))
  Print "Release      =";  readw(&H2)
  Print "Base high    = "; Hex$(readw(&H4))
  Print "Initial PC   = "; Hex$(readw(&H6))
  Print "Dictionary   = "; Hex$(readw(&H8))
  Print "Object tbl   = "; Hex$(readw(&HA))
  Print "Glob var tbl = "; Hex$(readw(&HC))
  Print "Base static  = "; Hex$(readw(&HE))
  Print "Flags2       = "; Bin$(readb(&H10))
  serial$ = "000000"
  For i = 0 To 5
    Poke Var serial$, i + 1, readb(&H12 + i)
  Next
  Print "Serial num   = "; serial$
  Print "Abbrv tbl    = "; Hex$(readw(&H18))
  Print "File length  ="; 2 * readw(&H1A)
  Print "File chksum  ="; readw(&H1C)
  Print "Std revision ="; readw(&H32)
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
  Local ad, null, x

  ad = readw(&h18)
  x = readw(ad + idx * 2)
  null = dump_zstring(x * 2)
End Sub

Sub dump_all_abbrv
  Local i
  For i = 0 To 95
    Print Str$(i); ": {";
    dump_abbrv(i)
    Print "}"
    If ((i + 1) Mod 20) = 0 Then
      prompt()
    EndIf
  Next i
End Sub

Sub prompt
  Print ""
  Input "More...", a$
  Print ""
End Sub

Sub dump_dictionary
  Local addr_dict, n, i

  addr_dict = readw(&h8)
  Print ""8
  n = readb(addr_dict)
  Print "n = "; n; ""
  codes$ = Space$(n)
  For i = 1 To n
    Poke Var codes$, i, readb(addr_dict + i)
  Next
  Print "codes        = "; codes$
  entry_length = readb(addr_dict + n * 2 + 1)
  Print "entry length = "; entry_length
  num_entries = readb(addr_dict + n * 2 + 2 + 2)
  Print "num entries  = "; num_entries
  addr_entries = addr_dict + n * 2 + 4 + 4
  Print Hex$(addr_entries)
  prompt
  For i = 0 To num_entries
    dump_zstring(addr_entries)
    dump_zstring(addr_entries + 2)
    addr_entries = addr_entries + entry_length
    Print ""
  Next
End Sub

Sub dump_global_vars
  Local i

  For i = 1 To 240
    Print "Global var"; i; " ="; get_global_var(i - 1)
  Next
End Sub

Function get_global_var(index)
  Local base

  base = readw(&hc) - &h10
  get_global_var = readw(base + index * 2)
End Function

' Decodes instruction at 'pc' to 'op_*' vars
Sub decode_op()
  Local i

  op = readb(pc)
  pc = pc + 1

  If op <= &h7F Then

    op_form = FORM_LONG
    op_code = op And &b00011111
    op_num_operands = 2

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
    op_num_operands = 1

    If op <= &h8F Then
      op_type(0) = OT_LARGE_CONST
    ElseIf op <= &h9F Then
      op_type(0) = OT_SMALL_CONST
    ElseIf op <= &hAF Then
      op_type(0) = OT_VARIABLE
    Else
      op_num_operands = 0
    EndIf

  Else
    op_form = FORM_VARIABLE
    op_code = op And &b00011111

    If op <= &hDF Then
      op_num_operands = 2
    Else
      op_num_operands = 3 ' actually VAR
    EndIf

  EndIf

  ' Read operands
  For i = 0 To op_num_operands - 1
    If op_type(i) = OT_LARGE_CONST Then
      op_value(i) = readw(pc)
      pc = pc + 2
    ElseIf op_type(i) = OT_OMITTED Then
      ' Do nothing
    Else
      op_value(i) = readb(pc)
      pc = pc + 1
    EndIf
  Next

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

  x$ = hex2$(op) + " "

  If op_num_operands = 0 Then
    x$ = x$ + "0OP"
  ElseIf op_num_operands = 1 Then
    x$ = x$ + "1OP"
  ElseIf op_num_operands = 2 Then
    x$ = x$ + "2OP"
  Else
    x$ = x$ + "VAR"
  EndIf

  x$ = rpad$(x$ + ":" + Str$(op), 10)

  For i = 0 To op_num_operands - 1
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
    x$ = x$ + rpad$(y$, 7)
  Next

  format_op$ = x$

End Function

' Performs the last decoded instruction
Sub perform_op

  Print format_op$()

  If op = 178 Then ' print
    pc = pc + dump_zstring(pc)
    Print ""
  EndIf

End Sub

Sub main_loop()
  Local i

  pc = readw(&h6)
  For i = 0 To 100
    Print "{"; Hex$(pc); "} ";
    decode_op()
    perform_op()
    If (i + 1) Mod 20 = 0 Then
      prompt()
    EndIf
  Next
End Sub

Sub zload(file$)
  Local p

  Print "Loading "; file$
  Print "  Pages ";
  For p = 0 To 31
    If p > 0 Then
      Print ", ";
    EndIf
    Print Str$(p);
    mem_load(file$, p)
  Next
  Print ""
End Sub

Memory
Print ""

zload("B:\zmim\dat\advent.Z3")
'zload("B:\zmim\dat\ZORK1\DATA\ZORK1.DAT")
Print ""

'dump_header
'prompt
'dump_zstring(&h44EF)

main_loop

Print ""
Memory
