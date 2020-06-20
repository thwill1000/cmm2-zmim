' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Const MAX_NUM_OPERANDS = 4 ' requires up to 8 for z4+

' The currently decoded instruction
Dim oc = 0               ' operand code
Dim onum = 0             ' number of operands
Dim oa(MAX_NUM_OPERANDS) ' operand values with variables looked-up
Dim ot(MAX_NUM_OPERANDS) ' operand types
Dim ov(MAX_NUM_OPERANDS) ' operand raw values
Dim st = 0
Dim br = 0

Sub de_init()
  Local i, s$, x

  Read x
  Dim inst_2op$(x - 1) LENGTH 15
  For i = 0 To x - 1 : Read s$ : inst_2op$(i) = de_format$(s$) : Next i

  Read x
  Dim inst_1op$(x - 1) LENGTH 14
  For i = 0 To x - 1 : Read s$ : inst_1op$(i) = de_format$(s$) : Next i

  Read x
  Dim inst_0op$(x - 1) LENGTH 12
  For i = 0 To x - 1 : Read s$ : inst_0op$(i) = de_format$(s$) : Next i

  Read x
  Dim inst_varop$(x - 1) LENGTH 14
  For i = 0 To x - 1 : Read s$ : inst_varop$(i) = de_format$(s$) : Next i
End Sub

' Decode next instruction into global variables.
'
' @param tr  if true then print the instruction.
Function decode(tr)
  Local op, s$

  If tr Then cout(Hex$(pc) + ": ")

  op = rp()

  If op < &h80 Then
    de_long(op)
    s$ = inst_2op$(oc)
  ElseIf op < &hC0 Then
    de_short(op)
    If op < &hB0 Then s$ = inst_1op$(oc) Else s$ = inst_0op$(oc)
  Else
    de_var(op)
    If op < &hE0 Then s$ = inst_2op$(oc) Else s$ = inst_varop$(oc)
  EndIf

  If Left$(s$, 1) = "B" Then
    st = -1
    br = de_branch()
  ElseIf Left$(s$, 1) = "S" Then
    st = rp()
    br = 0
  ElseIf Left$(s$, 1) = "X" Then
    st = rp()
    br = de_branch()
  Else
    st = -1
    br = 0
  EndIf

  If tr Then dmp_op(Mid$(s$, 2), st, br)

  decode = op
End Function

Sub de_long(op)
  oc = op And &b11111 ' bottom 5 bits
  onum = 2
  ov(0) = rp()
  ov(1) = rp()
  If op <= &h1F Then
    ot(0) = &b01 : oa(0) = ov(0) ' SMALL
    ot(1) = &b01 : oa(1) = ov(1) ' SMALL
  ElseIf op <= &h3F Then
    ot(0) = &b01 : oa(0) = ov(0) ' SMALL
    ot(1) = &b10 : oa(1) = vget(ov(1)) ' VARIABLE
  ElseIf op <= &h5F Then
    ot(0) = &b10 : oa(0) = vget(ov(0)) ' VARIABLE
    ot(1) = &b01 : oa(1) = ov(1) ' SMALL
  Else
    ot(0) = &b10 : oa(0) = vget(ov(0)) ' VARIABLE
    ot(1) = &b10 : oa(1) = vget(ov(1)) ' VARIABLE
  EndIf
End Sub

Sub de_short(op)
  oc = op And &b1111 ' bottom 4 bits
  onum = 1
  If op <= &h8F Then
    ot(0) = &b00 : ov(0) = rp() * 256 + rp() : oa(0) = ov(0)
  ElseIf op <= &h9F Then
    ot(0) = &b01 : ov(0) = rp() : oa(0) = ov(0)
  ElseIf op <= &hAF Then
    ot(0) = &b10 : ov(0) = rp() : oa(0) = vget(ov(0))
  Else
    onum = 0
  EndIf
End Sub

Sub de_var(op)
  Local i, x
  oc = op And &b11111 ' bottom 5 bits
  onum = 4
  x = rp()
  For i = 0 To 3
    ot(i) = (x And &b11000000) \ 64
    If ot(i) = &b00 Then
      ' LARGE
      ov(i) = rp() * 256 + rp() : oa(i) = ov(i)
    ElseIf ot(i) = &b01 Then
      ' SMALL
      ov(i) = rp() : oa(i) = ov(i)
    ElseIf ot(i) = &b10 Then
      ' VARIABLE
      ov(i) = rp() : oa(i) = vget(ov(i))
    Else ' ot(i) = &11
      ' OMITTED
      onum = onum - 1
    EndIf
    x = x * 4
  Next i
End Sub

' Reads branch offset.
' @return bits 0-19 - new value for the program counter.
'                   - if = pc - 2 then -> return false.
'                   - if = pc - 1 then -> return true.
'         bit 20    - set = branch on True, unset = branch on False.
Function de_branch()
  Local a, of, x
  a = rp()
  of = a And &b111111 ' bottom 6 bits

  If (a And &b1000000) = 0 Then ' check bit 6
    of = 256 * of + rp()
    If a And &b100000 Then of = of - 16384 ' check bit 5
  EndIf

  x = pc + of - 2

  ' If bit 7 of 'a' is set then set bit 20 in the return value
  If a And &h80 Then x = x Or &h80000

  de_branch = x
End Function

Function de_format$(a$)
  Local p, s$

  If Instr(a$, " SB") > 0 Then
    s$ = "X"
  ElseIf Instr(a$, " B") > 0 Then
    s$ = "B"
  ElseIf Instr(a$, " S") > 0 Then
    s$ = "S"
  Else
    s$ = " "
  EndIf
  p = Instr(a$, " ")
  If p = 0 Then p = Len(a$) + 1
  s$ = s$ + Left$(a$, p - 1)
  de_format$ = s$
End Function

' 2-operand instructions &h0 - &h18
Data 25
Data "Unknown&h0",  "JE B",     "JL B",       "JG B",            "DEC_CHK B"
Data "INC_CHK B",   "JIN B",    "TEST B",     "OR S",            "AND S"
Data "TEST_ATTR B", "SET_ATTR", "CLEAR_ATTR", "STORE",           "INSERT_OBJ"
Data "LOADW S",     "LOADB S",  "GET_PROP S", "GET_PROP_ADDR S", "GEN_NEXT_PROP S"
Data "ADD S",       "SUB S",    "MUL S",      "DIV S",           "MOD S"

' 1-operand instructions &h0 - &hF
Data 16
Data "JZ B",         "GET_SIBLING SB", "GET_CHILD SB", "GET_PARENT S", "GET_PROP_LEN S"
Data "INC",          "DEC",            "PRINT_ADDR",   "Unknown&h8",   "REMOVE_OBJ"
Data "PRINT_OBJECT", "RET",            "JUMP",         "PRINT_PADDR",  "LOAD S"
Data "NOT S"

' 0-operand instructions &h0 - &hD
Data 14
Data "RTRUE",        "RFALSE",      "PRINT",       "PRINT_RET",   "NOP"
Data "SAVE B",       "RESTORE B",   "RESTART",     "RET_POPPED",  "POP"
Data "QUIT",         "NEW_LINE",    "SHOW_STATUS", "VERIFY B"

' Var-operand instructions &h0 - &h15
Data 22
Data "CALL S",       "STOREW",      "STOREB",      "PUT_PROP",    "READ"
Data "PRINT_CHAR",   "PRINT_NUM",   "RANDOM S",      "PUSH",        "PULL"
Data "SPLIT_WINDOW", "SET_WINDOW",  "Unknown&hC",  "Unknown&hD",  "Unknown&hE"
Data "Unknown&hF",   "Unknown&h10", "Unknown&h11", "Unknown&h12", "OUTPUT_STREAM"
Data "INPUT_STREAM", "SOUND_EFFECT"
