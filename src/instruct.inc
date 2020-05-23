' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Sub instruct_init()
  Local i, s$, x

  Read x
  Dim inst_2op$(x - 1) LENGTH 15
  For i = 0 To x - 1 : Read s$ : inst_2op$(i) = instruct_format$(s$) : Next i

  Read x
  Dim inst_1op$(x - 1) LENGTH 14
  For i = 0 To x - 1 : Read s$ : inst_1op$(i) = instruct_format$(s$) : Next i

  Read x
  Dim inst_0op$(x - 1) LENGTH 11
  For i = 0 To x - 1 : Read s$ : inst_0op$(i) = instruct_format$(s$) : Next i

  Read x
  Dim inst_varop$(x - 1) LENGTH 11
  For i = 0 To x - 1 : Read s$ : inst_varop$(i) = instruct_format$(s$) : Next i
End Sub

Function instruct_format$(a$)
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
  instruct_format$ = s$
End Function

' 2-operand instructions &h0 - &h18
Data 25
Data "Unknown&h0",  "JE B",     "JL B",       "JG B",            "DEC_CHK B"
Data "INC_CHK B",   "JIN B",    "TEST B",     "OR S",            "AND S"
Data "TEST_ATTR B", "SET_ATTR", "CLEAR_ATTR", "STORE",           "INSERT_OBJ"
Data "LOADW S",     "LOADB S",  "GET_PROP S", "GET_PROP_ADDR S", "GEN_NEXT_PROP S"
Data "ADD S",       "SUB S",    "MUL S",      "DIV S",           "MOD S"

' 1-operand instructions &h0 - &hD
Data 15
Data "JZ B",         "GET_SIBLING SB", "GET_CHILD SB", "GET_PARENT S", "GET_PROP_LEN S"
Data "INC",          "DEC",            "PRINT_ADDR",   "Unknown&h8",   "REMOVE_OBJ"
Data "PRINT_OBJECT", "RET",            "JUMP",         "PRINT_PADDR",  "LOAD S"

' 0-operand instructions
Data 12
Data "RTRUE",      "RFALSE",     "PRINT",      "PRINT_RET",  "Unknown&h4"
Data "Unknown&h5", "Unknown&h6", "Unknown&h7", "RET_POPPED", "Unknown&h9"
Data "Unknown&hA", "NEWLINE"

' Var-operand instructions &h0 - &h6
Data 7
Data "CALL S",     "STOREW",    "STOREB", "Unknown&h3", "READ"
Data "PRINT_CHAR", "PRINT_NUM"
