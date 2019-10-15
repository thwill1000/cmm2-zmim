' ZMIM a Z-Machine Interpreter for the Colour Maximite
' Copyright (c) 2019 Thomas Hugo Williams
' For Maximite BASIC v4.5C

Mode 1

PAGE_SIZE = 1024 ' TODO: 512
MEM_SIZE = 64 * PAGE_SIZE
MAX_WORD = 256 * 256 - 1
ALPHABET$ =             " 123[]abcdefghijklmnopqrstuvwxyz"
ALPHABET$ = ALPHABET$ + " 123[]ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET$ = ALPHABET$ + " 123[]@^0123456789.,!?_#'" + Chr$(34) + "/\-:()"
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

' Prints 'count' bytes from 'mem' starting at 'addr'
Sub mem_dump(addr, count)
  Local i, s$

  For i = 0 To count - 1
    s$ = Hex$(readb(addr + i))
    If Len(s$) = 1 Then
      Print "0";
    EndIf
    Print s$; " ";
    If (i + 1) Mod 16 = 0 Then
      Print ""
    EndIf
  Next
  Print ""
End Sub

' Loads bytes from 'file$' into 'mem'
Sub mem_load(file$, page)
  Local addr, i, j

  Open file$ For random As #1
  Seek #1, page * PAGE_SIZE + 1
  addr = page * PAGE_SIZE
  For i = 1 To (PAGE_SIZE / 128)
    ' TODO: load the maximum 255 characters at a time
    tmp$ = Input$(128, 1)
    ' Note first byte of string is its length
    For j = 1 To Len(tmp$)
      Poke Var mem(0), addr, Peek(Var tmp$, j)
      addr = addr + 1
    Next j
  Next i
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

Sub dump_zstring(addr2)
  Local tmp_addr, end_bit, alph, val, j

  Print Hex$(addr2)
  tmp_addr = addr2
  end_bit = 0
  alph = 0
  Do While end_bit = 0
    val = readw(tmp_addr)
    zchar(2) = val And &b11111
    val = rshift(val, 5)
    zchar(1) = val And &b11111
    val = rshift(val, 5)
    zchar(0) = val And &b11111
    val = rshift(val, 5)
    end_bit = val
    For j = 0 To 2
      If zchar(j) = 1 Or zchar(j) = 2 Or zchar(j) = 3 Then
        Print "{"; Str$(zchar(j)); "}";
      ElseIf zchar(j) = 4 Then
        alph = 1
      ElseIf zchar(j) = 5 Then
        alph = 2
      Else
        Print Chr$(Peek(Var ALPHABET$, zchar(j) + alph * 32 + 1));
        alph = 0
      EndIf
    Next
    tmp_addr = tmp_addr + 2
  Loop
End Sub

' TODO: extract constants for shifts, e.g.
' SHIFT_1_BIT = 2
' SHIFT_2_BIT = 4
Function rshift(v, num)
  rshift = v\(2^num)
End Function

Sub prompt
  Print ""
  Input "More...", a$
  Print ""
End Sub

Sub dump_dictionary
  Local addr_dict, n, i

  addr_dict = readw(&h8)
  ' mem_dump(addr_dict, 127)
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

Sub dump_abbreviations
  Local i, addr_abbr

  addr_abbr = readw(&h18)
  For i = 0 To 95
    tmp = readw(addr_abbr + i * 2)
    dump_zstring(tmp * 2)
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

' Returns a string representation of the last decoded instruction
Function format_op$()
  Local i

  format_op$ = Hex2$(op) + " "

  If op_num_operands = 0 Then
    format_op$ = format_op$ + "0OP"
  ElseIf op_num_operands = 1 Then
    format_op$ = format_op$ + "1OP"
  ElseIf op_num_operands = 2 Then
    format_op$ = format_op$ + "2OP"
  Else
    format_op$ = format_op$ + "VAR"
  EndIf

  format_op$ = format_op$ + ":" + Str$(op)

  For i = 0 To op_num_operands - 1
    format_op$ = format_op$ + " " + Str$(op_value(i))
  Next

End Function

' Performs the last decoded instruction
Sub perform_op

  Print format_op$()

  If op = 178 Then ' print
'    Print read_zstring$(pc)
    dump_zstring(pc)
  EndIf

End Sub

Function Hex2$(byte)
  If byte <= &hF Then
    Hex2$ = "0"
  EndIf
  hex2$ = hex2$ + Hex$(byte)
End Function

Sub main_loop()
  Local k

  pc = readw(&h6)
  mem_dump(pc, 64)
  For k = 0 To 15
    Print "pc = "; pc
    decode_op()
    perform_op()
    prompt()
  Next
End Sub

Sub zload(file$)
  Local p

  Print "Loading "; file$
  For p = 0 To 31
    Print "  Page"; p; " ..."
    mem_load(file$, p)
  Next
  Print ""
End Sub

Memory
Print ""

zload("B:\zmim\dat\advent.Z3")
' zload("B:\zmim\dat\ZORK1\DATA\ZORK1.DAT")
dump_header
prompt

'dump_zstring(&h44EF)
'Print ""
'Print ""

main_loop

Memory
