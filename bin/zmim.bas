' Transpiled on 29-04-2025 20:51:15

' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

' Transpile with command:
'   sptrans -T -DNO_INCLUDE_GUARDS -DNO_EXTRA_CHECKS -DINLINE_CONSTANTS src/main.bas bin/zmim.bas
'   sptrans -T -DNO_INCLUDE_GUARDS -DLOW_MEMORY -DNO_EXTRA_CHECKS -DINLINE_CONSTANTS src/main.bas bin/zmim_rp2040.bas

Option Base 0
Option Default Integer
Option Explicit On

If InStr(Mm.Device$, "PicoMite") Then
  If Mm.Ver < 6.0 Then Error "PicoMite firmware v6.0 or later required"
EndIf

' src/splib/system.inc ++++
' Copyright (c) 2020-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>

On Error Skip
xyz_not_declared% = 1
If Not Mm.ErrNo Then Error "OPTION EXPLICIT is required by splib"
On Error Clear

Const sys.VERSION = 103300 ' 1.3.0

If Mm.Info$(Device X) = "MMB4L" Then
  Const sys.FIRMWARE = Mm.Info(Version)
Else
  Const sys.FIRMWARE = Int(1000000 * Mm.Info(Version))
EndIf

Const sys.SUCCESS = 0
Const sys.FAILURE = -1

Dim sys.break_flag%
Dim sys.err$

Function sys.SEPARATOR$()
  sys.SEPARATOR$ = Choice(Mm.Device$ = "MMBasic for Windows", "\", "/")
End Function

Function sys.error%(code%, msg$)
  If Not code% Then Error "Invalid error code"
  sys.error% = code%
  sys.err$ = msg$
End Function

' ---- src/splib/system.inc

' src/splib/file.inc ++++
' Copyright (c) 2020-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

' Does the file/directory 'f$' exist?
'
' @param  f$     name/path of file/directory
' @param  type$  type of files to check for: "dir", "file" or "all".
' @return        1 if the file/directory exists, otherwise 0
Function file.exists%(f$, type$)
  Local f_$ = Choice(Len(f$), f$, "."), i%
  Local type_$ = "|" + Choice(Len(type$), LCase$(type$), "all") + "|"
  If f_$ = "."  Then f_$ = Cwd$
  If Len(f_$) = 2 Then
    If Mid$(f_$, 2, 1) = ":" Then Cat f_$, "/"
  EndIf
  For i% = 1 To Len(f_$)
    If Peek(Var f_$, i%) = 92 Then Poke Var f_$, i%, 47
  Next
  If InStr("|file|all|", type_$) Then Inc file.exists%, Mm.Info(Exists File f_$)
  If InStr("|dir|all|", type_$) Then Inc file.exists%, Mm.Info(Exists Dir f_$)
  file.exists% = file.exists% > 0
End Function

' Gets the parent directory of 'f$', or the empty string if it does not have one.
Function file.get_parent$(f$)
  Select Case Len(f$)
    Case 1
      If f$ = "/" Or f$ = "\" Then Exit Function
    Case 2
      If Mid$(f$, 2, 1) = ":" Then Exit Function
    Case 3
      Select Case Right$(f$, 2)
        Case ":/", ":\"
          Exit Function
      End Select
  End Select
  Local i%
  For i% = Len(f$) To 1 Step -1
    If InStr("/\", Mid$(f$, i%, 1)) > 0 Then Exit For
  Next
  If i% > 0 Then file.get_parent$ = Left$(f$, i% - 1)
  If file.get_parent$ = "" Then
    If InStr("/\", Left$(f$, 1)) Then file.get_parent$ = Left$(f$, 1)
  EndIf
End Function

Function file.is_directory%(f$)
  file.is_directory% = file.exists%(f$, "dir")
End Function

' Makes directory 'f$' and its parents if they do not already exist.
Function file.mkdir%(f$)
  Local ad%, faddr% = Peek(VarAddr f$), parent$
  sys.err$ = ""
  For ad% = faddr% + 1 To faddr% + Len(f$)
    Select Case Peek(Byte ad%)
      Case 47, 92 ' / and \
        parent$ = Left$(f$, ad% - faddr% - 1)
        If file.exists%(parent$) Then
          If Not file.is_directory%(parent$) Then
            file.mkdir% = sys.error%(sys.FAILURE, "File exists")
            Exit Function
          EndIf
        Else
          If parent$ <> "" Then MkDir parent$
        EndIf
    End Select
  Next
  If file.exists%(f$) Then
    If Not file.is_directory%(f$) Then file.mkdir% = sys.error%(sys.FAILURE, "File exists")
  Else
    MkDir f$
  EndIf
End Function

' Resolves a file against a given parent.
Function file.resolve$(parent$, f$)
  file.resolve$ = parent$ + sys.SEPARATOR$() + f$
End Function

' ---- src/splib/file.inc

' src/splib/string.inc ++++
' Copyright (c) 2020-2024 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

' Gets a string padded to a given width with to the left.
'
' @param  s$  the string.
' @param  w%  the width.
' @param  c$  the padding character, a space if omitted.
' @return     the padded string.
'             If Len(s$) > w% then returns the unpadded string.
Function str.lpad$(s$, x%, c$)
  str.lpad$ = s$
  If c$ = "" Then c$ = " "
  If Len(s$) < x% Then str.lpad$ = String$(x% - Len(s$), c$) + s$
End Function

' Gets a string padded to a given width to the right.
'
' @param  s$  the string.
' @param  w%  the width.
' @param  c$  the padding character, a space if omitted.
' @return     the padded string.
'             If Len(s$) > w% then returns the unpadded string.
Function str.rpad$(s$, x%, c$)
  str.rpad$ = s$
  If c$ = "" Then c$ = " "
  If Len(s$) < x% Then str.rpad$ = s$ + String$(x% - Len(s$), c$)
End Function

' Returns a copy of s$ with leading and trailing spaces removed.
Function str.trim$(s$)
  Local st%, en%
  For st% = 1 To Len(s$)
    If Peek(Var s$, st%) <> 32 Then Exit For
  Next
  For en% = Len(s$) To 1 Step -1
    If Peek(Var s$, en%) <> 32 Then Exit For
  Next
  If en% >= st% Then str.trim$ = Mid$(s$, st%, en% - st% + 1)
End Function

' ---- src/splib/string.inc

' src/splib/vt100.inc ++++
' Copyright (c) 2020-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

' ---- src/splib/vt100.inc

' src/memory.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

' Storage for z-machine memory
Dim m(128 * 1024 / 8 + 16)
Dim mad = Peek(VarAddr m()) + 8
Dim FILE_LEN
Dim BASE_STATIC

' Reads a byte from 'a' but DOES NOT increment a
Function rb(a)
  rb = LGetByte(m(), a)
End Function

' Reads a 16-bit word from 'a' but DOES NOT increment a
Function rw(a)
  rw = LGetByte(m(), a) * 256 + LGetByte(m(), a + 1)
End Function

' Writes byte 'x' to 'a'
Sub wb(a, x)
  Poke Byte mad + a, x
End Sub

' Writes 16-bit word 'x' to 'a'
Sub ww(a, x)
  Poke Byte mad + a, x \ 256
  Poke Byte mad + a + 1, x Mod 256
End Sub

Sub mem_init(f$)
  Local ad, i, j, s$, sz

  LongString Clear m()

  con.println("Loading '" + f$ + "'")
  con.print("0% ... ")

  sz = mm.info(filesize f$)
  Open f$ For Input As #1
  j = 1
  Do
    s$ = Input$(255, #1)
    If Len(s$) = 0 Then Exit Do
    LongString Append m(), s$
    Inc ad, Len(s$)
    If ad > j * sz \ 10 Then con.print(Str$(j * 10) + "% ... ") : Inc j
  Loop
  con.println("100%")
  con.println("Read " + Str$(ad) + " bytes")

  Close #1

  ' Read memory sizes from header
  FILE_LEN = 512 ' temporary value to avoid "Invalid address" error from rw()
  BASE_STATIC = rw(&h0E)
  FILE_LEN = rw(&h1A) * 2

  If FILE_LEN > ad Then Error "Story file is truncated"
End Sub

' ---- src/memory.inc

' src/stack.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Dim stack(511)  ' storage for the stack  - we only store 16-bit unsigned
                ' integers 0 .. &hFFFF although they are often interpreted as
                ' signed.
Dim sp          ' stack pointer; points to the *next* element in the stack.

' Pushes a 16-bit word to the stack.
Sub st_push(x)
  stack(sp) = x
  Inc sp
End Sub

' Peeks at a value in the stack.
Function st_peek(i)
  st_peek = stack(i)
End Function

' ---- src/stack.inc

' src/variable.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Dim GLOBAL_VAR ' base address for global variables

' Gets variable 'i'.
' If i = 0 then pops and returns the top value of the stack.
Function vget(i)
  If i = 0 Then
    vget = stack(sp - 1) : Inc sp, -1
  ElseIf i < &h10 Then
    vget = stack(fp + i + 4)
  ElseIf i <= &hFF Then
    vget = rw(GLOBAL_VAR + 2 * (i - &h10))
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Function

' Sets variable 'i'.
' If i = 0 then pushes the value onto the stack.
Sub vset(i, v)
  If i = 0 Then
    stack(sp) = v : Inc sp
  ElseIf i < &h10 Then
    stack(fp + i + 4) = v
  ElseIf i <= &hFF Then
    ww(GLOBAL_VAR + 2 * (i - &h10), v)
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Sub

' ---- src/variable.inc

' src/decode.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Dim pc       ' the program counter

' The currently decoded instruction
Dim oc = 0   ' operand code
Dim onum = 0 ' number of operands
Dim oa(4)    ' operand values with variables looked-up
Dim ot(4)    ' operand types
Dim ov(4)    ' operand raw values
Dim st = 0   ' store location, -1 for no store
Dim br = 0   ' branch desintation, 0 for no branch

Sub de_init()
  Local i, s$, x

  Read x
  Dim OP0$(x - 1) LENGTH 12
  For i = 0 To x - 1 : Read s$ : OP0$(i) = de_format$(s$) : Next

  Read x
  Dim OP1$(x - 1) LENGTH 14
  For i = 0 To x - 1 : Read s$ : OP1$(i) = de_format$(s$) : Next

  Read x
  Dim OP2$(x - 1) LENGTH 15
  For i = 0 To x - 1 : Read s$ : OP2$(i) = de_format$(s$) : Next

  Read x
  Dim OPV$(x - 1) LENGTH 14
  For i = 0 To x - 1 : Read s$ : OPV$(i) = de_format$(s$) : Next
End Sub

' Decode next instruction into global variables.
'
' @param tr  if true then print the instruction.
Function decode(tr)
  Local op, s$

  If tr Then con.print(Hex$(pc) + ": ")

  op = LGetByte(m(), pc) : Inc pc

  If op < &h80 Then
    de_long(op)
    s$ = OP2$(oc)
  ElseIf op < &hC0 Then
    de_short(op)
    If op < &hB0 Then s$ = OP1$(oc) Else s$ = OP0$(oc)
  Else
    de_var(op)
    If op < &hE0 Then s$ = OP2$(oc) Else s$ = OPV$(oc)
  EndIf

  If Left$(s$, 1) = "B" Then
    st = -1
    br = de_branch()
  ElseIf Left$(s$, 1) = "S" Then
    st = LGetByte(m(), pc) : Inc pc
    br = 0
  ElseIf Left$(s$, 1) = "X" Then
    st = LGetByte(m(), pc) : Inc pc
    br = de_branch()
  Else
    st = -1
    br = 0
  EndIf

  If tr Then dmp_op(Mid$(s$, 2))

  decode = op
End Function

Sub de_long(op)
  oc = op And &b11111 ' bottom 5 bits
  onum = 2
  ov(0) = LGetByte(m(), pc) : Inc pc
  ov(1) = LGetByte(m(), pc) : Inc pc
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
    ot(0) = &b00 : ov(0) = LGetByte(m(), pc) * 256 + LGetByte(m(), pc + 1) : Inc pc, 2 : oa(0) = ov(0)
  ElseIf op <= &h9F Then
    ot(0) = &b01 : ov(0) = LGetByte(m(), pc) : Inc pc : oa(0) = ov(0)
  ElseIf op <= &hAF Then
    ot(0) = &b10 : ov(0) = LGetByte(m(), pc) : Inc pc : oa(0) = vget(ov(0))
  Else
    onum = 0
  EndIf
End Sub

Sub de_var(op)
  Local i, x
  oc = op And &b11111 ' bottom 5 bits
  onum = 4
  x = LGetByte(m(), pc) : Inc pc
  For i = 0 To 3
    ot(i) = (x And &b11000000) \ 64
    If ot(i) = &b00 Then
      ' LARGE
      ov(i) = LGetByte(m(), pc) * 256 + LGetByte(m(), pc + 1) : Inc pc, 2 : oa(i) = ov(i)
    ElseIf ot(i) = &b01 Then
      ' SMALL
      ov(i) = LGetByte(m(), pc) : Inc pc : oa(i) = ov(i)
    ElseIf ot(i) = &b10 Then
      ' VARIABLE
      ov(i) = LGetByte(m(), pc) : Inc pc : oa(i) = vget(ov(i))
    Else ' ot(i) = &b11
      ' OMITTED
      onum = onum - 1
    EndIf
    x = x * 4
  Next
End Sub

' Reads branch offset.
' @return bits 0-19 - new value for the program counter.
'                   - if = pc - 2 then -> return false.
'                   - if = pc - 1 then -> return true.
'         bit 20    - set = branch on True, unset = branch on False.
Function de_branch()
  Local a, of, x
  a = LGetByte(m(), pc) : Inc pc
  of = a And &b111111 ' bottom 6 bits

  If (a And &b1000000) = 0 Then ' check bit 6
    of = 256 * of + LGetByte(m(), pc) : Inc pc
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
  Cat s$, Left$(a$, p - 1)
  de_format$ = s$
End Function

' 0-operand instructions &h0 - &hD
Data 14
Data "RTRUE",        "RFALSE",      "PRINT",       "PRINT_RET",   "NOP"
Data "SAVE B",       "RESTORE B",   "RESTART",     "RET_POPPED",  "POP"
Data "QUIT",         "NEW_LINE",    "SHOW_STATUS", "VERIFY B"

' 1-operand instructions &h0 - &hF
Data 16
Data "JZ B",         "GET_SIBLING SB", "GET_CHILD SB", "GET_PARENT S", "GET_PROP_LEN S"
Data "INC",          "DEC",            "PRINT_ADDR",   "Unknown&h8",   "REMOVE_OBJ"
Data "PRINT_OBJECT", "RET",            "JUMP",         "PRINT_PADDR",  "LOAD S"
Data "NOT S"

' 2-operand instructions &h0 - &h18
Data 25
Data "Unknown&h0",  "JE B",     "JL B",       "JG B",            "DEC_CHK B"
Data "INC_CHK B",   "JIN B",    "TEST B",     "OR S",            "AND S"
Data "TEST_ATTR B", "SET_ATTR", "CLEAR_ATTR", "STORE",           "INSERT_OBJ"
Data "LOADW S",     "LOADB S",  "GET_PROP S", "GET_PROP_ADDR S", "GEN_NEXT_PROP S"
Data "ADD S",       "SUB S",    "MUL S",      "DIV S",           "MOD S"

' Var-operand instructions &h0 - &h15
Data 22
Data "CALL S",       "STOREW",      "STOREB",      "PUT_PROP",    "READ"
Data "PRINT_CHAR",   "PRINT_NUM",   "RANDOM S",    "PUSH",        "PULL"
Data "SPLIT_WINDOW", "SET_WINDOW",  "Unknown&hC",  "Unknown&hD",  "Unknown&hE"
Data "Unknown&hF",   "Unknown&h10", "Unknown&h11", "Unknown&h12", "OUTPUT_STREAM"
Data "INPUT_STREAM", "SOUND_EFFECT"

' ---- src/decode.inc

' src/console.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Const con.SCREEN% = &h01
Const con.SERIAL% = &h02

Dim con.HEIGHT = 50
Dim con.WIDTH = 100

Dim con.buf$     ' console write buffer
Dim con.space    ' was the last character written to con.buf$ a space?
Dim con.lines    ' number of lines that have been output since the last prompt
Dim con.count    ' number of newlines without an intervening character being printed
Dim con.x = 1    ' current horizontal cursor position, indexed from 1
Dim con.fd_in    ' file descriptor to read input from
Dim con.fd_out   ' file descriptor to echo input to
Dim con.more = 1 ' show the [MORE] prompt if more than a page of output
                 ' (approx con.HEIGHT lines) is written without a prompt
Dim con.history%(255) ' history buffer for user input.
Dim con.spin_enabled  ' is the spinning cursor enabled ?
Dim con.spin_shown    ' is the spinning cursor currently displayed ?

Sub con.init(width%, height%, spin_enabled%)
  con.WIDTH = width%
  con.HEIGHT = height%
  con.spin_enabled = spin_enabled%
End Sub

' @param  p$  prompt
Function con.in$(p$, echo)
  Local s$

  con.print(p$)
  con.flush()

  If con.fd_in Then
    Do While s$ = "" And Not Eof(con.fd_in)
      Line Input #con.fd_in, s$
      ' Ignore comments and *record commands.
      If InStr(s$, "#") = 1 Or InStr(LCase$(s$), "*record") = 1 Then s$ = ""
      If s$ <> "" Then con.history_put(con.history%(), s$)
    Loop
    If Eof(con.fd_in) And s$ = "" Then s$ = "*replay off"
  Else
    s$ = con.readln$("", con.WIDTH - con.x - Len(p$), con.history%())
  EndIf

  ' If we read from a file then echo what we read.
  If con.fd_in Then con.println(s$)

  ' If we read from the console then clear con.lines.
  If Not con.fd_in Then con.lines = 0 : con.count = 1

  con.x = 1

  If echo Then
    If con.fd_out Then
      If s$ <> "" Then Print #con.fd_out, s$
    EndIf
  EndIf

  con.in$ = s$
End Function

' Outputs s$.
'
' s$ is appended to con.buf$ one character at a time. Only when we switch from
' non-spaces to spaces or vice versa do we flush the contents of con.buf$ to the
' console. As a result calling this function will probably not print the
' entirety of s$; ' the last 'word' or part thereof may be left in con.buf$.
Sub con.print(s$)
  Local c$, i

  For i = 1 To Len(s$)
    c$ = Mid$(s$, i, 1)
    Select Case c$
      Case Chr$(13) : ' CR
        ' Ignore carriage returns

      Case Chr$(10) : ' LF
        con.endl()
        con.space = 0

      Case " " :
        If Not con.space Then con.flush() : con.space = 1
        Cat con.buf$, " "
        con.count = 0

      Case "`" :
        If con.space Then con.flush() : con.space = 0
        Cat con.buf$, "'"
        con.count = 0

      Case Else :
        If con.space Then con.flush() : con.space = 0
        Cat con.buf$, c$
        con.count = 0

    End Select
  Next
End Sub

' Writes con.buf$ to the console and clears it.
'
' If con.buf$ does not fit on the current line then first write CRLF and then if
' con.buf$ contains non-spaces write con.buf$ to the console.
Sub con.flush()
  If con.spin_shown Then Print Chr$(8); " "; Chr$(8); : con.spin_shown = 0

  ' If the buffer is wider than the console then split the buffer.
  Local remainder$
  If Len(con.buf$) > con.WIDTH Then
    remainder$ = "..." + Mid$(con.buf$, con.WIDTH + 1)
    con.buf$ = Left$(con.buf$, con.WIDTH)
  EndIf

  Do
    If con.x = 1 And con.lines > con.HEIGHT - 2 Then
      If con.more Then
        Print "[MORE] ";
        Do While Inkey$ <> "" : Loop
        Do While Inkey$ = "" : Pause 1 : Loop
        Print
      EndIf
      con.lines = 0
    EndIf

    If con.x + Len(con.buf$) > con.WIDTH + 1 Then
      Print
      Inc con.lines
      con.x = 1
      If con.space Then con.buf$ = "" : Exit Do
    Else
      Print con.buf$;
      Inc con.x, Len(con.buf$)
      con.buf$ = ""
      Exit Do
    EndIf
  Loop

  If Len(remainder$) Then
    con.buf$ = remainder$
    con.flush()
  EndIf
End Sub

' Flushes con.buf$ to the console and then writes CRLF.
Sub con.endl()
  con.flush()

  If con.count < 0 Then
    Exit Sub
  ElseIf con.count >= 10 Then
    ' If the story outputs 10 or more empty lines then we assume it was trying
    ' to clear the screen
    Local i
    For i = 0 To con.HEIGHT - con.count - 1 : Print : Next
    con.count = -999 ' to avoid any unnecessary additional empty lines
    con.lines = 0
    Exit Sub
  EndIf

  Print
  Inc con.count
  Inc con.lines
  con.x = 1
End Sub

Sub con.println(s$, center)

  If Len(s$) > 0 Then
    If center Then
      If con.x <> 1 Then Error "Cannot center text unless on a new line."
      If Len(s$) < con.WIDTH Then con.print(Space$((con.WIDTH - Len(s$)) \ 2) + s$)
    Else
      con.print(s$)
    EndIf
  EndIf

  con.endl()
End Sub

' Echos the contents of a (hopefully) text file to the console.
Sub con.print_file(f$, center)
  Local s$, w

  If center Then
    Open f$ For Input As #1
    Do
      Line Input #1, s$
      w = Max(w, Len(s$))
    Loop While Not Eof(#1)
    Close #1
  EndIf

  Open f$ For Input As #1
  Do
    Line Input #1, s$
    If center Then
      con.println(s$ + Space$(w - Len(s$)), center)
    Else
      con.println(s$)
    EndIf
  Loop While Not Eof(#1)
  Close #1
End Sub

Sub con.open_in(fd, f$)
  con.close_in()
  Open f$ For Input As #fd
  con.fd_in = fd
End Sub

Sub con.close_in()
  If con.fd_in Then Close #con.fd_in
  con.fd_in = 0
End Sub

Sub con.open_out(fd, f$)
  con.close_out()
  Open f$ For Output As #fd
  con.fd_out = fd
End Sub

Sub con.close_out()
  If con.fd_out Then Close #con.fd_out
  con.fd_out = 0
End Sub

Sub con.bell()
  Local type% = con.get_type%()
  If type% And con.SCREEN% Then Play Tone 329.63, 329.63, 100 ' Note E4
  If type% And con.SERIAL% Then
    con.set_type(con.SERIAL%)
    Print Chr$(7);
    con.set_type(type%)
  EndIf
End Sub

' Gets the console type.
' Note that in many cases you don't want to test the return value using equals
' '=' but instead using AND.
' e.g. to test whether the type supports:
'  - serial console do: IF con.get_type%() AND con.SERIAL% THEN
'  - screen console do: IF con.get_type%() AND con.SCREEN% THEN
Function con.get_type%()
  Select Case LCase$(Mm.Info(Option Console))
    Case "both"   : con.get_type% = con.SCREEN% Or con.SERIAL%
    Case "screen" : con.get_type% = con.SCREEN%
    Case "serial" : con.get_type% = con.SERIAL%
    Case Else     : Error "Unknown console type"
  End Select
End Function

Sub con.set_type(type%)
  Select Case type%
    Case con.SCREEN% : Option Console Screen
    Case con.SERIAL%
      ' Note MMB4W cannot call OPTION CONSOLE SERIAL if it is on the same line
      ' as a CASE clause. This is because in MMB4W this OPTION checks
      ' 'CurrentLinePtr' to make sure it is only being used from within a
      ' program. However due to a long-standing and gnarly bug in the MMBasic
      ' core 'CurrentLinePtr' is not correctly set when processing a command on
      ' the same line as a CASE clause.
      Option Console Serial
    Case &h03 : Option Console Both
    Case Else
      Error "Unknown console type"
  End Select
End Sub

' Gets a string element from the command history.
'
' @param  h%()  buffer containing the command history.
' @param  idx%  index of the element to retrieve, the first element is always
'               idx% = 0 irrespective of OPTION BASE.
' @return       string element, or empty string if idx% >= number of elements.
Function con.history_get$(h%(), idx%)
  If idx% < 0 Then Error "index out of bounds: " + Str$(idx%)

  Local h_addr% = Peek(VarAddr h%())
  Local h_size% = (Bound(h%(), 1) - Bound(h%(), 0) + 1) * 8
  Local i%, p% = h_addr%
  For i% = 0 To idx%
    If Peek(Byte p%) = 0 Then Exit Function ' Return empty string.
    If i% < idx% Then Inc p%, Peek(Byte p%) + 1
  Next
  If p% + Peek(Byte p%) < h_addr% + h_size% Then
    Memory Copy p%, Peek(VarAddr con.history_get$), Peek(Byte p%) + 1
  EndIf
End Function

Function con.history_find%(h%(), needle$)
  If needle$ = "" Then Error "invalid argument: needle$"

  ' TODO: could be faster by not calling con.history_get()
  Local i% = -1, s$
  Do
    Inc i%
    s$ = con.history_get$(h%(), i%)
  Loop Until s$ = "" Or s$ = needle$

  con.history_find% = Choice(s$ = "", -1, i%)
End Function

Sub con.history_put(h%(), s$)
  If s$ = "" Then Error "invalid empty string"

  Local h_addr% = Peek(VarAddr h%())
  Local h_size% = (Bound(h%(), 1) - Bound(h%(), 0) + 1) * 8
  If Peek(Byte h_addr%) > 0 Then
    ' Can't use MEMORY COPY because the CMM2 does not handle case where src and dst overlap.
    ' Memory Copy h_addr%, h_addr% + 1 + Len(s$), h_size% - 1 - Len(s$)
    Local dst% = h_addr% + h_size% - 1
    Local src% = dst% - Len(s$) - 1
    Do While src% >= h_addr%
      Poke Byte dst%, Peek(Byte src%)
      Inc dst%, -1
      Inc src%, -1
    Loop
  EndIf
  Memory Copy Peek(VarAddr s$), h_addr%, Len(s$) + 1
End Sub

Function con.readln$(initial$, max_len%, history%())
  Local ch$, hidx%, p%, old$, overwrite%, s$
  Local max_len_% = Choice(max_len% < 1 Or max_len% > 255, 255, max_len%)

  con.readln$ = initial$
  p% = Len(initial$) + 1
  Print initial$;
  hidx% = Choice(initial$ = "", -1, con.history_find%(history%(), initial$))

  Local show% = 1
  Local t% = Timer + 500

  Do While Inkey$ <>  "" : Loop ' Discard anything else in the keyboard buffer.

  Do While Not sys.break_flag%
    If Timer > t% Then show% = Not show% : t% = Timer + 500
    con.show_cursor(show%)

    ch$ = Inkey$
    If ch$ = "" Then Pause 1 : Continue Do
    con.show_cursor(0)

    Select Case Asc(ch$)
      Case 8 ' Backspace
        If p% = 1 Then
          con.bell()
        Else
          ' Delete the character immediately to the left.
          s$ = Mid$(con.readln$, p%)
          con.readln$ = Left$(con.readln$, p% - 2) + s$
          Print Chr$(&h08) s$ " " String$(Len(s$) + 1, &h08);
          Inc p%, -1
        EndIf

      Case 10, 13 ' Enter
        Print
        If con.readln$ <> "" Then con.history_put(history%(), con.readln$)
        Exit Do

      Case 127 ' Delete
        If p% = Len(con.readln$) + 1 Then
          con.bell()
        Else
          ' Delete the current character.
          s$ = Mid$(con.readln$, p% + 1)
          con.readln$ = Left$(con.readln$, p% - 1) + s$
          Print s$ " " String$(Len(s$) + 1, &h08);
        EndIf

      Case 128, 129 ' Up, Down
        If hidx% = -1 Then old$ = con.readln$
        Inc hidx%, Choice(Asc(ch$) = 128, 1, -1)
        If hidx% <= -1 Then
          hidx% = -1
          s$ = old$
        Else
          s$ = con.history_get$(history%(), hidx%)
          If s$ = "" Then s$ = con.readln$ : Inc hidx%, -1
        EndIf

        Print String$(p% - 1, &h08) String$(p% - 1, " ") String$(p% - 1, &h08) s$;
        con.readln$ = s$
        p% = Len(s$) + 1

      Case 130 ' Left
        If p% > 1 Then
          Inc p%, -1
          Print Chr$(&h08);
        EndIf

      Case 131 ' Right
        If p% <= Len(con.readln$) Then
          Print Mid$(con.readln$, p%, 1);
          Inc p%
        EndIf

      Case 132 ' Insert
        overwrite% = Not overwrite%

      Case 134 ' Home
        Print String$(p% - 1, &h08);
        p% = 1

      Case 135 ' End
        Do While p% <= Len(con.readln$)
          Print Mid$(con.readln$, p%, 1);
          Inc p%
        Loop

      Case < 32, > 126
        ' Print "<" Str$(Asc(ch$)) ">";
        ' con.bell()

      Case Else
        If p% > Len(con.readln$) Then
          ' Append to end of line.
          If Len(con.readln$) = max_len_% Then
            con.bell()
          Else
            Cat con.readln$, ch$
            Print ch$;
            Inc p%
          EndIf
        ElseIf overwrite% Then
          ' Overwrite.
          con.readln$ = Left$(con.readln$, p% - 1) + ch$ + Mid$(con.readln$, p% + 1)
          Print ch$;
          Inc p%
        Else
          ' Insert.
          If Len(con.readln$) = max_len_% Then
            con.bell()
          Else
            s$ = Mid$(con.readln$, p%)
            con.readln$ = Left$(con.readln$, p% - 1) + ch$ + s$
            Print ch$ s$ String$(Len(s$), &h08);
            Inc p%
          EndIf
        EndIf
    End Select
  Loop

  If sys.break_flag% Then con.readln$ = ""
End Function

' Hide/show cursor on the VGA/LCD screen.
Sub con.show_cursor(show%)
  If con.get_type%() And con.SCREEN% Then
    Local x% = Mm.Info(HPos), y% = Mm.Info(VPos) + Mm.Info(FontHeight) - 1
    Line x%, y%, x% + Mm.Info(FontWidth), y%, 1, Choice(show%, RGB(White), RGB(Black))
  EndIf
End Sub

' Show the spinning cursor.
Sub con.spin()
  Static i
  If con.x < con.WIDTH - 1 Then
    If con.spin_shown Then Print Chr$(8); Else con.spin_shown = 1
    Print Mid$("\|/-", i + 1, 1);
    i = (i + 1) Mod 4
  EndIf
End Sub

' ---- src/console.inc

' src/script.inc ++++
' Copyright (c) 2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

' Implementation of script record and replay.

Const script.FD_IN% = 2
Const script.FD_OUT% = 3

Sub script.record_on()
  Local f$, s$

  If con.fd_out <> 0 Then con.println("Already recording script.") : Exit Sub
  If con.fd_in <> 0  Then con.println("Cannot record whilst replaying script.") : Exit Sub

  con.println()
  con.println("Select script to record:")
  con.println()
  Local script% = script.select_script%()
  If script% <> 0 Then f$ = script.script_file$(script%)

  If script% <> 0 And file.exists%(f$) Then
    s$ = con.in$("Overwrite script " + Str$(script%) + " [y|N]? ")
    If LCase$(s$) <> "y" Then script% = 0
  EndIf

  If script% <> 0 Then
    s$ = con.in$("Script name ? ")
    If s$ = "" Then script% = 0
  EndIf

  If script% = 0 Then con.println("Cancelled.") : Exit Sub

  con.println("Recording to '" + f$ + "' ...")

  If file.mkdir%(file.get_parent$(f$)) <> sys.SUCCESS Then Error sys.err$

  Const fd_out% = script.FD_OUT%
  con.open_out(fd_out%, f$)
  Print #fd_out%, "# " Date$ " " Time$
  Print #fd_out%, "# " s$

End Sub

' Prompts the user to select a script slot.
Function script.select_script%()
  Local i, f$, s$
  Const fd% = script.FD_IN%

  For i = 1 To 10
    f$ = script.script_file$(i)
    con.print("  [" + Format$(i, "%2g") + "] ")
    If file.exists%(f$) Then
      Open f$ For Input As #fd%
      Line Input #fd%, s$ ' date/time
      con.print(Mid$(s$, 3) + " - ")
      Line Input #fd%, s$ ' script name
      con.println(Mid$(s$, 3))
      Close #fd%
    Else
      con.println("Empty")
    EndIf
  Next i

  con.println()
  s$ = con.in$("Script number ? ")
  Local script% = Val(s$)
  If script% < 1 Or script% > 10 Then script% = 0
  script.select_script% = script%

End Function

' Gets the path to the file corresponding to script slot 'i'.
Function script.script_file$(i)
  If i < 1 Or i > 10 Then Error "Invalid script number"
  Const name$ = ss$(STORY_FILE) + "_" + Str$(i) + ".scr"
  script.script_file$ = file.resolve$(ss$(SCRIPT_DIR), ss$(STORY_FILE))
  script.script_file$ = file.resolve$(script.script_file$, name$)
End Function

Sub script.record_off()
  If con.fd_out = 0 Then con.println("A script is not being recorded!") : Exit Sub
  con.close_out()
  con.println("Recording stopped.")
End Sub

Sub script.replay_on()
  If con.fd_out <> 0 Then con.println("Cannot replay whilst recording script.") : Exit Sub
  If con.fd_in <> 0  Then con.println("Already replaying script.") : Exit Sub

  con.println()
  con.println("Select script to replay:")
  con.println()
  Local script% = script.select_script%()
  If script% <> 0 Then
    Local f$ = script.script_file$(script%)
    If Not file.exists%(f$) Then script% = 0
  EndIf

  If script% = 0 Then con.println("Cancelled.") : Exit Sub

  con.println("Replaying from '" + f$ + "' ...")

  con.open_in(script.FD_IN%, f$)
End Sub

Sub script.replay_off()
  If con.fd_in = 0 Then con.println("A script is not being replayed!") : Exit Sub
  con.close_in()
  con.println("Replaying stopped.")
End Sub

' ---- src/script.inc

' src/objects.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

' Gets/sets object attribute.
Function ob_attr(o, a, s, x)
  Local ad, mask, y

  If o = 0 Then Exit Function ' return 0

  ad = rw(&h0A) + 62 + (o - 1) * 9 + a \ 8
  y = LGetByte(m(), ad)
  mask = 2^(7 - a Mod 8) ' mask
  If s = 0 Then ob_attr = (y And mask) > 0 : Exit Function
  If x = 0 Then y = (y And (mask Xor &hFF)) Else y = (y Or mask)
  Poke Byte mad + ad, y
  ob_attr = x
End Function

' Gets/sets object relatives.
Function ob_rel(o, r, s, x)
  Local ad
  ad = rw(&h0A) + 62 + (o - 1) * 9 + r
  If s = 0 Then ob_rel = LGetByte(m(), ad) : Exit Function
  Poke Byte mad + ad, x
  ob_rel = x
End Function

Function ob_next_prop(o, p)
  Local ad, x

  If o = 0 Then
    Exit Function ' return 0
  ElseIf p = 0 Then
    ad = ob_prop_base(o)
    Inc ad, 1 + 2 * LGetByte(m(), ad) ' skip length & description
  Else
    ad = ob_prop_addr(o, p)
    If ad = 0 Then Error "Property does not exist"
    x = LGetByte(m(), ad - 1)
    Inc ad, 1 + x\32
  EndIf

  x = LGetByte(m(), ad)
  ob_next_prop = x And &b11111 ' bottom 5 bits
End Function

Function ob_prop_len(ad)
  Local x
  If ad = 0 Then Exit Function ' return 0
  x = LGetByte(m(), ad - 1)
  ob_prop_len = x\32 + 1
End Function

Function ob_prop_base(o)
  Local ad
  ad = rw(&h0A) + 62 + (o - 1) * 9 + 7
  ob_prop_base = LGetByte(m(), ad) * 256 + LGetByte(m(), ad + 1)
End Function

Function ob_prop_addr(o, p)
  Local ad, x
  ad = ob_prop_base(o)
  Inc ad, 1 + 2 * LGetByte(m(), ad) ' Skip length & description
  Do
    x = LGetByte(m(), ad)
    ' Mask with bottom 5 bits
    If (x And &b11111) = p Then ob_prop_addr = ad + 1 : Exit Function
    If (x And &b11111) < p Then ob_prop_addr = 0 : Exit Function
    Inc ad, 2 + x\32
  Loop
End Function

Function ob_prop_get(o, p)
  Local ad, sz, x
  ad = ob_prop_addr(o, p)
  If ad > 0 Then
    x = LGetByte(m(), ad - 1)
    If (x And &b11111) <> p Then Error ' Mask with bottom 5 bits
    sz = x\32 + 1
    If sz = 1 Then ob_prop_get = LGetByte(m(), ad) : Exit Function
    If sz = 2 Then ob_prop_get = LGetByte(m(), ad) * 256 + LGetByte(m(), ad + 1) : Exit Function
    Error "Property length > 2"
  EndIf
  ad = rw(&h0A) + 2 * (p - 1)
  ob_prop_get = LGetByte(m(), ad) * 256 + LGetByte(m(), ad + 1)
End Function

Sub ob_prop_set(o, p, x)
  Local ad, sz
  ad = ob_prop_addr(o, p)
  If ad = 0 Then Error "Object " + Str$(o) + " does not have property " + Str$(p)
  Select Case ob_prop_len(ad)
    Case 1
      wb(ad, x And &hFF)
    Case 2
      Poke Byte mad + ad, x \ 256 : Poke Byte mad + ad + 1, x Mod 256
    Case Else
      Error "Object " + Str$(o) + " has length " + Str$(ob_prop_len(ad))
  End Select
End Sub

Sub ob_print(o)
  Local ad
  ad = ob_prop_base(o) + 1
  print_zstring(ad)
End Sub

Sub ob_insert(o, d)
  Local c, _

  ob_remove(o)                 ' remove object from its original parent
  c = ob_rel(d, 6)         ' original child of destination
  _ = ob_rel(d, 6, 1, o)   ' object is new child of destination
  _ = ob_rel(o, 4, 1, d)  ' destination is new parent of object
  _ = ob_rel(o, 5, 1, c) ' original child of dest is new sibling of object
End Sub

Sub ob_remove(o)
  Local c, p, s, _

  p = ob_rel(o, 4)        ' parent of object
  s = ob_rel(o, 5)       ' sibling of object
  c = ob_rel(p, 6)         ' first child of parent
  _ = ob_rel(o, 4, 1, 0)  ' object no longer has parent
  _ = ob_rel(o, 5, 1, 0) ' object no longer has sibling
  If o = c Then
    _ = ob_rel(p, 6, 1, s) ' if object was first child then now its sibling is
  Else
    Do
      If ob_rel(c, 5) = o Then _ = ob_rel(c, 5, 1, s) : Exit Do
      c = ob_rel(c, 5)
    Loop Until c = 0
  EndIf
End Sub

' ---- src/objects.inc

' src/execute.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Const E_OK = 0            ' last operation succeeded
Const E_UNKNOWN = 1       ' last operation unkown
Const E_UNIMPLEMENTED = 2 ' last operation implemented
Const E_BREAK = 3         ' TODO: document
Const E_QUIT = 4          ' last operation was a "Quit"
Const E_DEBUG = 5         ' TODO: document
Const E_REPEAT = 6        ' repeat last operation

Dim fp      ' frame pointer; points to the stack element that is the first
            ' element for the current routine. This has the value &FFFF if
            ' we are not in a routine.
Dim num_bp  ' numer of active breakpoints
Dim num_ops ' number of instructions processed
Dim ztrace  ' is instruction tracing enabled? 0 or 1

Function exec(tr)
  Local op, pc_old, sp_old

  ' Store the PC and SP in case we need to roll-back.
  pc_old = pc : sp_old = sp

  op = decode(tr)
  Inc num_ops
  If op < &h80 Then
    exec = ex_2op()
  ElseIf op < &hB0 Then
    exec = ex_1op()
  ElseIf op < &hC0 Then
    exec = ex_0op()
  ElseIf op < &hE0 Then
    exec = ex_2op()
  Else
    exec = ex_varop()
  EndIf

  If exec = E_UNKNOWN Then
    con.println("Unsupported instruction " + fmt_hex$(op, 2))
  ElseIf exec = E_UNIMPLEMENTED Then
    con.println("Unimplemented instruction " + fmt_hex$(op, 2))
  EndIf

  If exec <> E_OK Then pc = pc_old : sp = sp_old

End Function

Function ex_2op()
  Local a, b, x, y, _

  a = oa(0)
  b = oa(1)

  If oc = &h1 Then
    ' JE
    x = (a = b)
    If (Not x) And (onum > 2) Then x = (a = oa(2))
    If (Not x) And (onum > 3) Then x = (a = oa(3))
    ex_branch(x, br)

  ElseIf oc = &h2 Then
    ' JL
    If a > 32767 Then Inc a, -65536
    If b > 32767 Then Inc b, -65536
    ex_branch(a < b, br)

  ElseIf oc = &h3 Then
    ' JG
    If a > 32767 Then Inc a, -65536
    If b > 32767 Then Inc b, -65536
    ex_branch(a > b, br)

  ElseIf oc = &h4 Then
    ' DEC_CHK
    x = vget(a)
    If x > 32767 Then Inc x, -65536
    If b > 32767 Then Inc b, -65536
    Inc x, -1
    y = x < b
    If x < 0 Then Inc x, 65536
    vset(a, x)
    ex_branch(y, br)

  ElseIf oc = &h5 Then
    ' INC_CHK
    x = vget(a)
    If x > 32767 Then Inc x, -65536
    If b > 32767 Then Inc b, -65536
    Inc x
    y = x > b
    If x < 0 Then Inc x, 65536
    vset(a, x)
    ex_branch(y, br)

  ElseIf oc = &h6 Then
    ' JIN
    x = ob_rel(a, 4)
    ex_branch(x = b, br)

  ElseIf oc = &h7 Then
    ' TEST
    ex_branch((a And b) = b, br)

  ElseIf oc = &h8 Then
    ' OR
    vset(st, a Or b)

  ElseIf oc = &h9 Then
    ' AND
    vset(st, a And b)

  ElseIf oc = &hA Then
    ' TEST_ATTR: a = object, b = attribute
    x = ob_attr(a, b)
    ex_branch(x = 1, br)

  ElseIf oc = &hB Then
    ' SET_ATTR
    _ = ob_attr(a, b, 1, 1)

  ElseIf oc = &hC Then
    ' CLEAR_ATTR
    _ = ob_attr(a, b, 1, 0)

  ElseIf oc = &hD Then
    ' STORE
    ' Note special handing for op with indirect reference to the stack pointer
    If a = 0 Then stack(sp - 1) = b Else vset(a, b)

  ElseIf oc = &hE Then
    ' INSERT_OBJ: a = object, b = destination
    ob_insert(a, b)

  ElseIf oc = &hF Then
    ' LOADW
    x = rw(a + 2 * b)
    vset(st, x)

  ElseIf oc = &h10 Then
    ' LOADB
    x = LGetByte(m(), a + b)
    vset(st, x)

  ElseIf oc = &h11 Then
    ' GET_PROP
    x = ob_prop_get(a, b)
    vset(st, x)

  ElseIf oc = &h12 Then
    ' GET_PROP_ADDR
    x = ob_prop_addr(a, b)
    vset(st, x)

  ElseIf oc = &h13 Then
    ' GET_NEXT_PROP
    x = ob_next_prop(a, b)
    vset(st, x)

  ElseIf oc < &h19 Then
    If a > 32767 Then Inc a, -65536
    If b > 32767 Then Inc b, -65536

    If oc = &h14 Then
      ' ADD
      x = a + b
    ElseIf oc = &h15 Then
      ' SUB
      x = a - b
    ElseIf oc = &h16 Then
      ' MUL
      x = a * b
    ElseIf oc = &h17 Then
      ' DIV
      x = a \ b
    Else
      ' MOD
      x = a Mod b
    EndIf

    If x < 0 Then Inc x, 65536
    vset(st, x)

  Else
    ex_2op = E_UNKNOWN
  EndIf
End Function

Function ex_1op()
  Local a, x

  a = oa(0)

  If oc = &h0 Then
    ' JZ
    ex_branch(a = 0, br)

  ElseIf oc = &h1 Then
    ' GET_SIBLING
    x = ob_rel(a, 5)
    vset(st, x)
    ex_branch(x <> 0, br)

  ElseIf oc = &h2 Then
    ' GET_CHILD
    x = ob_rel(a, 6)
    vset(st, x)
    ex_branch(x <> 0, br)

  ElseIf oc = &h3 Then
    ' GET_PARENT
    x = ob_rel(a, 4)
    vset(st, x)

  ElseIf oc = &h4 Then
    ' GET_PROP_LEN
    x = ob_prop_len(a)
    vset(st, x)

  ElseIf oc = &h5 Then
    ' INC
    x = vget(a)
    If x > 32767 Then Inc x, -65536
    Inc x
    If x < 0 Then Inc x, 65536
    vset(a, x)

  ElseIf oc = &h6 Then
    ' DEC
    x = vget(a)
    If x > 32767 Then Inc x, -65536
    Inc x, -1
    If x < 0 Then Inc x, 65536
    vset(a, x)

  ElseIf oc = &h7 Then
    ' PRINT_ADDR
    print_zstring(a)

  ElseIf oc = &h9 Then
    ' REMOVE_OBJ
    ob_remove(a)

  ElseIf oc = &hA Then
    ' PRINT_OBJECT
    ob_print(a)

  ElseIf oc = &hB Then
    ' RET
    ex_return(a)

  ElseIf oc = &hC Then
    ' JUMP
    If a And &h8000 Then Inc a, -65536
    Inc pc, a - 2

  ElseIf oc = &hD Then
    ' PRINT_PADDR
    print_zstring(a * 2)

  ElseIf oc = &hE Then
    ' LOAD
    ' Note special handing for op with indirect reference to the stack pointer
    If a = 0 Then x = stack(sp - 1) Else x = vget(a)
    vset(st, x)

  ElseIf oc = &hF Then
    ' NOT
    x = a Xor &b1111111111111111
    vset(st, x)

  Else
    ex_1op = E_UNKNOWN
  EndIf
End Function

Function ex_0op()
  Local x

  If oc = &h0 Then
    ' RTRUE
    ex_return(1)

  ElseIf oc = &h1 Then
    ' RFALSE
    ex_return(0)

  ElseIf oc = &h2 Then
    ' PRINT
    print_zstring(pc)

  ElseIf oc = &h3 Then
    ' PRINT_RET
    print_zstring(pc)
    con.endl()
    ex_return(1)

  ElseIf oc = &h4 Then
    ' NOP

  ElseIf oc = &h5 Then
    ' SAVE
    If con.fd_in Then
      con.println("IGNORED 'save' command read from script")
    Else
      x = zsave()
      ex_branch(x, br)
    EndIf

  ElseIf oc = &h6 Then
    ' RESTORE
    If con.fd_in Then
      con.println("IGNORED 'restore' command read from script")
    Else
      x = zsave(1)
    EndIf

  ElseIf oc = &h7 Then
    ' RESTART
    If con.fd_in Then
      con.println("IGNORED 'restart' command read from script")
    Else
      main_init()
      For x = 0 To 10 : con.endl() : Next ' this will clear the console
    EndIf

  ElseIf oc = &h8 Then
    ' RET_POPPED
    x = stack(sp - 1) : Inc sp, -1
    ex_return(x)

  ElseIf oc = &h9 Then
    ' POP - discards top item in stack
    Inc sp, -1

  ElseIf oc = &hA Then
    ' QUIT
    ex_0op = E_QUIT

  ElseIf oc = &hB Then
    ' NEW_LINE
    con.endl()

  ElseIf oc = &hC Then
    ' SHOW_STATUS
    ex_show_status()

  ElseIf oc = &hD Then
    ' VERIFY
    ' Actually verifying the story checksum is pointless, always branch.
    ex_branch(1, br)

  Else
    ex_0op = E_UNKNOWN
  EndIf
End Function

Function ex_varop()
  Local x, _

  If oc = &h0 Then
    ' CALL
    ex_call(st)

  ElseIf oc = &h1 Then
    ' STOREW
    ww(oa(0) + 2 * oa(1), oa(2))

  ElseIf oc = &h2 Then
  ' STOREB
    wb(oa(0) + oa(1), oa(2))

  ElseIf oc = &h3 Then
    ' PUT_PROP
    ob_prop_set(oa(0), oa(1), oa(2))

  ElseIf oc = &h4 Then
    ' READ
    ex_varop = ex_read(oa(0), oa(1))

  ElseIf oc = &h5 Then
    ' PRINT_CHAR
    con.print(Chr$(oa(0)))

  ElseIf oc = &h6 Then
    ' PRINT_NUM
    x = oa(0)
    If x > 32767 Then Inc x, -65536
    con.print(Str$(x))

  ElseIf oc = &h7 Then
    ' RANDOM
    x = oa(0)
    If x > 32767 Then Inc x, -65536
    x = ex_random(x)
    vset(st, x)

  ElseIf oc = &h8 Then
    ' PUSH
    stack(sp) = oa(0) : Inc sp

  ElseIf oc = &h9 Then
    ' PULL
    x = stack(sp - 1) : Inc sp, -1
    ' Note special handing for op with indirect reference to the stack pointer
    If oa(0) = 0 Then stack(sp - 1) = x Else vset(oa(0), x)

  ElseIf oc = &h13 Then
    ' INPUT_STREAM
    ex_varop = E_UNIMPLEMENTED

  ElseIf oc = &h14 Then
    ' OUTPUT_STREAM
    ex_varop = E_UNIMPLEMENTED

  ElseIf oc = &h15 Then
    ' SOUND_EFFECT
    ' TODO

  Else
    ex_varop = E_UNKNOWN
  EndIf
End Function

Sub ex_branch(z, br)
  Local x
  If Not (z = (br And &h80000) > 0) Then Exit Sub
  x = br And &h7FFFF ' Bottom 19-bits
  If x = pc - 1 Then ex_return(1) : Exit Sub
  If x = pc - 2 Then ex_return(0) : Exit Sub
  pc = x
End Sub

Sub ex_return(x)
  Local st, _
  sp = fp + 4
  ' This single line version won't work if inlining st_pop():
  ' pc = st_pop() * &h10000 + st_pop()
  pc = &h10000 * stack(sp - 1) : Inc sp, -1
  Inc pc, stack(sp - 1) : Inc sp, -1
  st = stack(sp - 1) : Inc sp, -1
  fp = stack(sp - 1) : Inc sp, -1
  vset(st, x)
  If ztrace Then dmp_stack()
End Sub

Sub ex_call(st)
  Local i, nl, x

  ' When address 0 is called, nothing happens and return value is false
  If oa(0) = 0 Then vset(st, 0) : Exit Sub

  stack(sp) = fp : Inc sp
  fp = sp - 1
  stack(sp) = st : Inc sp
  st_push(pc Mod &h10000)
  st_push(pc \ &h10000)
  pc = 2 * oa(0)
  nl = LGetByte(m(), pc) : Inc pc ' number of local variables
  stack(sp) = nl : Inc sp
  For i = 1 To nl
    x = LGetByte(m(), pc) * 256 + LGetByte(m(), pc + 1) : Inc pc, 2
    If i < onum Then st_push(oa(i)) Else st_push(x)
  Next

  If ztrace Then dmp_routine(2 * oa(0)) : dmp_stack()
End Sub

Function ex_read(text_buf, parse_buf)
  Local ad, c, i, n, word$, s$, t, wc

  t = Timer

  s$ = LCase$(con.in$("> ", 1))
  If Left$(s$, 1) = "*" Then ex_read = ex_special(s$)

  Timer = t

  If ex_read <> E_OK Then Exit Function

  n = Len(s$)
  ' TODO: check for input too long
  For i = 1 To n : wb(text_buf + i, Peek(Var s$, i)) : Next
  wb(text_buf + n + 1, 0)
  Cat s$, " "

  For i = 1 To n + 1
    c = Peek(Var s$, i)
    If c = &h20 Or Instr(DICT_SEP$(0), Chr$(c)) > 0 Then
      If Len(word$) > 0 Then
        ad = di_lookup(word$)
        ww(parse_buf + 2 + wc * 4, ad)
        wb(parse_buf + 4 + wc * 4, Len(word$))
        wb(parse_buf + 5 + wc * 4, i - Len(word$)) ' position in 'text_buf'
        Inc wc
        word$ = ""
      EndIf
      If c <> &h20 Then ' ASCII code for space
        ad = di_lookup(Chr$(c))
        ww(parse_buf + 2 + wc * 4, ad)
        wb(parse_buf + 4 + wc * 4, 1)
        wb(parse_buf + 5 + wc * 4, i - 1) ' position in 'text_buf'
        Inc wc
      EndIf
    Else
      Cat word$, Chr$(c)
    EndIf
  Next

  wb(parse_buf + 1, wc)

End Function

Function ex_special(cmd$)
  Local f$, _

  ex_special = E_REPEAT

  If cmd$ = "*break" Then
    ex_special = E_BREAK
'    ex_special = E_OK

  ElseIf cmd$ = "*credits" Then
    con.print_file(file.resolve$(ss$(RESOURCES_DIR), "credits.txt"), 1)

  ElseIf cmd$ = "*eof" Then
    con.println("End of script.")
    con.endl()

  ElseIf InStr(cmd$, "*more") = 1 Then
    Select Case str.trim$(Mid$(cmd$, Len("*more") + 1))
      Case "", "on"
        con.more = 1
        con.println("Paged output enabled.")
      Case "off"
        con.more = 0
        con.println("Paged output disabled.")
      Case Else
        con.println("Invalid '*more' command.")
    End Select
    con.endl()

  ElseIf InStr(cmd$, "*record") = 1 Then
    Select Case str.trim$(Mid$(cmd$, Len("*record") + 1))
      Case "", "on"
        script.record_on()
      Case "off"
        script.record_off()
      Case Else
        con.println("Invalid '*record' command.")
    End Select
    con.endl()

  ElseIf InStr(cmd$, "*replay") = 1 Then
    Select Case str.trim$(Mid$(cmd$, Len("*replay") + 1))
      Case "", "on"
        script.replay_on()
      Case "off"
        script.replay_off()
      Case Else
        con.println("Invalid '*replay' command.")
    End Select
    con.endl()

  ElseIf cmd$ = "*screenshot" Then
    ex_screenshot()

  ElseIf InStr(cmd$, "*spin") = 1 Then
    Select Case str.trim$(Mid$(cmd$, Len("*spin") + 1))
      Case "", "on"
        con.spin_enabled = 1
        con.println("Spinning cursor enabled.")
      Case "off"
        con.spin_enabled = 0
        con.println("Spinning cursor disabled.")
      Case Else
        con.println("Invalid '*spin' command.")
    End Select
    con.endl()

  ElseIf cmd$ = "*status" Then
    ex_show_status()

  ElseIf cmd$ = "*save" Then
    If zsave(0) Then con.println("Ok.") Else con.println("Save failed.")
    con.endl()

  ElseIf cmd$ = "*restore" Then
    If zsave(1) Then con.println("Ok.") Else con.println("Restore failed.")
    con.endl()

  Else
    ' Let the game's parser deal with the command
    ex_special = E_OK

  EndIf

  If ex_special = E_REPEAT Then con.print(">")

End Function

Function ex_random(range)
  Static x = 7
  Static a = 1103515245 ' a, c and m are same values as used by glibc
  Static c = 12345
  Static m = 2^31

  If range = 0 Then
    x = Timer
    Exit Function
  ElseIf range < 0 Then
    x = Abs(range)
    Exit Function
  EndIf

  x = (a * x + c) Mod m

  ex_random = 1 + CInt((range - 1) * (x / m))
End Function

Sub ex_show_status()
  Local x
  x = vget(&h10) : ob_print(x) : con.print(", ")
  x = vget(&h11) : con.print("Score: " + Str$(x) + ", ")
  x = vget(&h12) : con.print("Moves: " + Str$(x))
  con.endl()
End Sub

Sub ex_screenshot()
  Local f$, i = 0, root$ = Cwd$
  Do
    Inc i
    f$ = file.resolve$(root$, ss$(STORY_FILE) + "_" + Str$(i) + ".bmp")
  Loop While file.exists%(f$)
  If InStr(Mm.Device$, "PicoMite") Then
    On Error Skip
    Save Compressed Image f$
  Else
    On Error Skip
    Save Image f$
  EndIf
  If Mm.ErrNo Then
    con.println("Screenshot failed or is not supported.")
  Else
    con.println("Saved '" + f$ + "'")
  EndIf
  con.endl()
End Sub

' ---- src/execute.inc

' src/zstring.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Dim ALPHABET$(2) LENGTH 32
ALPHABET$(0) = " 123[]abcdefghijklmnopqrstuvwxyz"
ALPHABET$(1) = " 123[]ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET$(2) = " 123[]@^0123456789.,!?_#'" + Chr$(34) + "/\-:()"

' Prints Z-string starting at 'a' incrementing 'a' by the number of bytes read
Sub print_zstring(ad)
  Local b, c, i, s, x, zc(2)

  ' The state of Z-string processing is recorded in 's':
  '   0, 1, 2 - Expecting a character from alphabet 's'
  '   3, 4, 5 - Expecting an abbreviation from table 's - 3'
  '   6       - Expecting the top 5-bits of a ZSCII character
  '   > 6     - Expecting the btm 5-bits of a ZSCII character whose
  '             top 5-bits are 's - 7'

  ' Should be 'Do While x = 0' but there is an MMBasic bug using that
  ' in recursive functions.
  For x = 0 To 0 Step 0

    x = LGetByte(m(), ad) * 256 + LGetByte(m(), ad + 1)
    zc(0) = (x And &h7C00) \ &h400
    zc(1) = (x And &h3E0) \ &h20
    zc(2) = (x And &h1F)
    x = x \ &h8000

    ' x is now the top-bit of the word. If x = 1 then we have reached the end
    ' of the string and will exit the loop after this iteration.

    For i = 0 To 2
      c = zc(i)
      If s < 3 Then
        If c = 0 Then
          con.print(" ")
        ElseIf c < 4 Then
          s = c + 2
        ElseIf c < 6 Then
          s = c - 3
        Else
          If c = 6 And s = 2 Then
            s = 6
          ElseIf c = 7 And s = 2 Then
            con.endl()
            s = 0
          Else
            con.print(Mid$(ALPHABET$(s), c + 1, 1))
            s = 0
          EndIf
        EndIf
      ElseIf s < 6 Then
        b = ad ' Backup the address
        print_abrv((s - 3) * 32 + c)
        ad = b ' Restore the address
        s = 0
      ElseIf s = 6 Then
        s = c + 7
      Else
        con.print(Chr$((s - 7) * 32 + c))
        s = 0
      EndIf
    Next i ' Variable is required for this to work in recursive function.

    Inc ad, 2
  Next x ' Variable is required for this to work in recursive function.

End Sub

' Prints abbreviation 'x'
Sub print_abrv(x)
  Local a, b
  a = rw(&h18)
  b = rw(a + x * 2)
  print_zstring(b * 2)
End Sub

' ---- src/zstring.inc

' src/util.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Function fmt_hex$(x, i)
  If i < 1 Then i = 4
  fmt_hex$ = "&h" + str.lpad$(Hex$(x), i, "0")
End Function

' ---- src/util.inc

' src/dict.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Dim DICT_AD               ' base address of the dictionary
Dim DICT_SEP$(1) Length 5 ' word separators, use an array so as to avoid
                          ' 256 byte string
Dim DICT_ENTRY_LEN        ' length of a dictionary entry
Dim DICT_ENTRY_COUNT      ' number of dictionary entries
Dim DICT_ENTRY_AD         ' base address of the dictionary entries

Sub di_init()
  Local i, ns

  DICT_AD = rw(&h08)
  ns = rb(DICT_AD) ' number of word separators
  Poke Var DICT_SEP$(0), 0, ns
  For i = 1 To ns : Poke Var DICT_SEP$(0), i, rb(DICT_AD + i) : Next
  DICT_ENTRY_LEN = rb(DICT_AD + ns + 1)
  DICT_ENTRY_COUNT = rw(DICT_AD + ns + 2)
  DICT_ENTRY_AD = DICT_AD + ns + 4
End Sub

' Lookup w$ in the vocabulary. w$ should already be in lower-case
Function di_lookup(w$)

  ' Convert first 6 characters of w$ into an array of Z-char 'z'
  Local c, i, j, nz, sz, z(9)
  sz = Len(w$) : If sz > 6 Then sz = 6
  i = 1 ' the index into w$ that is being processed
  Do While i < 7 And nz < 7
    If i > sz Then
      z(nz) = 5 : Inc nz ' pad with Z-char 5
    Else
      c = Asc(Mid$(w$, i, 1))
      j = Instr(ALPHABET$(0), Chr$(c)) - 1
      If j > -1 Then
        ' Character is in Alphabet 0
        z(nz) = j : Inc nz
      Else
        ' Because we know w$ is in lower-case we never have to search Alphabet 1
        ' which contains only upper-case A-Z
        j = Instr(ALPHABET$(2), Chr$(c)) - 1
        If j > -1 Then
          ' Character is in Alphabet 2
          z(nz) = 5 : Inc nz ' shift into Alphabet 2
          z(nz) = j : Inc nz
        Else
          ' Encode character in ZSCII
          z(nz) = 5 : Inc nz ' shift into Alphabet 2
          z(nz) = 6 : Inc nz ' indicate ZSCII encoding
          z(nz) = c \ 32 : Inc nz ' top 5 bits
          z(nz) = c And &b11111 : Inc nz ' bottom 5 bits
        EndIf
      EndIf
    EndIf
    Inc i
  Loop

  ' Copy the first 6 Z-chars into 2 x 16-bit words x(0) and x(1)
  Local x(1)
  x(0) = z(0) * 1024 + z(1) * 32 + z(2)
  x(1) = z(3) * 1024 + z(4) * 32 + z(5) + 32768 ' set bit-15 at end of word

  ' Binary search to lookup the Z-string in the dictionary
  Local ad, lb, ub, y(1)
  lb = 0
  ub = DICT_ENTRY_COUNT - 1
  Do
    i = (lb + ub) \ 2
    ad = DICT_ENTRY_AD + DICT_ENTRY_LEN * i
    y(0) = LGetByte(m(), ad) * 256 + LGetByte(m(), ad + 1)
    y(1) = rw(ad + 2)
    If x(0) > y(0) Then
      lb = i + 1
    ElseIf x(0) < y(0) Then
      ub = i - 1
    ElseIf x(1) > y(1) Then
      lb = i + 1
    ElseIf x(1) < y(1) Then
      ub = i - 1
    Else
      di_lookup = ad
      ub = lb - 1
    EndIf
  Loop Until ub < lb

End Function

' ---- src/dict.inc

' src/zsave.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

' Save/Restore game
'
' NOTE: Use a single subroutine to cover both because of the 50 subroutine
'       limit in MMBasic 4.5 on the CMM1
'
' @param  res  set 0 to save and 1 to restore
' @return      1 on success, 0 on failure or if cancelled
Function zsave(res)
  Local exists(10), i, old_dir$, s$
  If InStr(Mm.Info(Device X), "PicoMite") Then
    Local s2$(2) Length 64
  Else
    Local s2$(2)
  EndIf

  If res Then
    con.println("Select game to restore:")
  Else
    con.println("Select save game slot:")
  EndIf

  old_dir$ = Cwd$
  ChDir file.resolve$(ss$(SAVE_DIR), ss$(STORY_FILE))
  For i = 1 To 10
    s$ = Dir$("game" + Str$(i) + ".sav")
    con.print("  [" + Format$(i, "%2g") + "] ")
    If s$ = "" Then
      con.println("Empty")
    Else
      exists(i) = 1
      Open "game" + Str$(i) + ".sav" For Input As #1
      Line Input #1, s2$(0) ' header
      Line Input #1, s2$(1) ' version
      Line Input #1, s2$(2) ' date/time
      Line Input #1, s$     ' game name
      ' TODO: verify header / version
      con.println(s2$(2) + " - " + s$)
      Close #1
    EndIf
  Next
  ChDir old_dir$

  s$ = con.in$("Game number? ")
  i = Val(s$)
  If i < 1 Or i > 10 Then i = 0

  If i > 0 And res And Not exists(i) Then i = 0

  If i > 0 And Not res And exists(i) Then
    s$ = con.in$("Overwrite game " + Str$(i) + " [y|N]? ")
    If LCase$(s$) <> "y" Then i = 0
  EndIf

  If i > 0 And Not res Then
    s$ = con.in$("Save game name? ")
    If s$ = "" Then i = 0
  EndIf

  If i = 0 Then con.println("Cancelled") : Exit Function

  s2$(0) = file.resolve$(ss$(SAVE_DIR), ss$(STORY_FILE))
  s2$(0) = file.resolve$(s2$(0), "game" + Str$(i) + ".sav")

  If res Then
    Open s2$(0) For Input As #1

    ' Read text header
    Line Input #1, s2$(0)
    Line Input #1, s2$(1)
    Line Input #1, s2$(2)
    Line Input #1, s$
    con.println("Restoring '" + s$ + "' ...")

    Local ad, err, new_pc, new_fp, stack_sz, mem_sz

    ' Read 9 byte data header
    ' 3 bytes - program counter
    ' 2 bytes - frame pointer
    ' 2 bytes - stack size
    ' 2 bytes - dynamic memory size
    s$ = Input$(9, #1)
    new_pc = Peek(Var s$, 1) * 65536 + Peek(Var s$, 2) * 256 + Peek(Var s$, 3)
    new_fp = Peek(Var s$, 4) * 256 + Peek(Var s$, 5)
    stack_sz = Peek(Var s$, 6) * 256 + Peek(Var s$, 7)
    mem_sz = Peek(Var s$, 8) * 256 + Peek(Var s$, 9)

    ' Validate data header
    If new_pc < 0 Or new_pc >= FILE_LEN Then err = 1
    If new_fp < 0 Or new_fp > stack_sz Then err = 2
    If stack_sz < 0 Or stack_sz > 511 Then err = 3
    If mem_sz <> BASE_STATIC Then err = 4
    If err <> 0 Then
      con.print("Save file is invalid (error " + Str$(err) + ")")
      Close #1
      Exit Function
    EndIf

    pc = new_pc
    fp = new_fp
    sp = 0

    ' Read stack (2 bytes / entry)
    For i = 0 To stack_sz - 1
      s$ = Input$(2, #1)
      st_push(Peek(Var s$, 1) * 256 + Peek(Var s$, 2))
    Next

    ' Read dynamic memory
    Do
      s$ = Input$(255, #1)
      For i = 1 To Len(s$)
        wb(ad, Peek(Var s$, i))
        Inc ad
      Next
    Loop Until Len(s$) = 0

    If ad <> BASE_STATIC Then Error "Unrecoverable restore error!"

  Else
    con.println("Saving '" + s$ + "' ...")
    Open s2$(0) For Output As #1

    ' Write text header
    Print #1, "ZMIM save file"
    Print #1, "1"
    Print #1, Date$ + " " + Time$
    Print #1, s$

    ' Write 9 byte data header
    ' 3 bytes - program counter
    ' 2 bytes - frame pointer
    ' 2 bytes - stack size
    ' 2 bytes - dynamic memory size
    Print #1, Chr$(pc \ 65536); Chr$(pc \ 256); Chr$(pc Mod 256);
    Print #1, Chr$(fp \ 256); Chr$(fp Mod 256);
    Print #1, Chr$(sp \ 256); Chr$(sp Mod 256);
    Print #1, Chr$(BASE_STATIC \ 256); Chr$(BASE_STATIC Mod 256);

    ' Write stack (2 bytes / entry)
    For i = 0 To sp - 1
      Print #1, Chr$(st_peek(i) \ 256); Chr$(st_peek(i) Mod 256);
    Next

    ' Write dynamic memory
    For i = 0 To BASE_STATIC - 1
       Print #1, Chr$(rb(i));
    Next
  EndIf

  Close #1
  zsave = 1

End Function

' ---- src/zsave.inc

' src/zfile.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Function fi_choose$(d$, fspec$)
  Local f$, i, j, nc, nr, old_dir$, sz, width, x

  old_dir$ = Cwd$
  ChDir d$

  ' Count number of entries 'sz' and their maximum length 'width'.
  ' Ignore files prefixed with full-stop.
  f$ = Dir$(fspec$)
  Do While f$ <> ""
    If Left$(f$, 1) <> "." Then
      Inc sz
      If Len(f$) > width Then width = Len(f$)
    EndIf
    f$ = Dir$()
  Loop

  If sz = 0 Then con.println("No files found") : ChDir old_dir$ : Exit Function

  ' Create sorted array of entries 'all$'.
  ' Once again ignore all files prefixed with full-stop.
  ' MMBasic doesn't allow creation of an array with a single element so just
  ' in case we include an extra dummy element that will always be sorted last.
  Local all$(sz) LENGTH width
  all$(sz) = Chr$(&h7F)
  f$ = Dir$(fspec$)
  i = 0
  Do
    If Left$(f$, 1) <> "." Then all$(i) = f$ : Inc i
    f$ = Dir$()
  Loop Until i = sz
  Sort all$()

  ChDir old_dir$

  ' Display entries in one column if < 8, otherwise 2 columns.
  If (sz < 8 Or con.WIDTH <= 40) Then nc = 1 Else nc = 2
  nr = CInt(sz / nc + 0.4999)
  Inc width, 10
  For i = 0 To nr - 1
    For j = 0 to nc - 1
      con.flush()
      x = (j * nr) + i
      If x < sz Then
        If j * width > Pos Then con.print(Space$(j * width - Pos))
        con.print("  [" + Format$(x + 1, "%2g") + "] " + all$(x))
      EndIf
    Next
    con.endl()
  Next

  f$ = con.in$("File number? ")
  If Val(f$) > 0 And Val(f$) <= sz Then fi_choose$ = file.resolve$(d$, all$(Val(f$) - 1))

End Function

' ---- src/zfile.inc

' src/debug.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Dim bp(9) ' The addresses of up to 10 breakpoints, -1 for unset
          ' execute#num_bp should also be updated when these are changed

' Interactive debugger.
Function debug()
  Local a, b, c, cmd$(9) Length 20, cn, i, op, pc_old, s$, t, sp_old

  ' Decode and display the next instruction but don't execute it.
  pc_old = pc : sp_old = sp
  op = decode(1)
  pc = pc_old : sp = sp_old

  Do
    ' Read line of input and parse into space separated commands/arguments.
    cn = 0
    For i = 0 To 9 : cmd$(i) = "" : Next
    t = Timer
    s$ = con.in$("DEBUG >> ") + " "
    Timer = t
    For i = 1 To Len(s$)
      c = Peek(Var s$, i)
      If Chr$(c) = " " Then
        If Len(cmd$(cn)) > 0 Then cn = cn + 1
        If cn = 10 Then Error "Too many arguments"
      Else
        cmd$(cn) = cmd$(cn) + Chr$(c)
      EndIf
    Next

    debug = E_DEBUG

    If cmd$(0) = "abrv" Then
      dmp_abrv()

    ElseIf cmd$(0) = "b" Then
      ' Set address breakpoint
      a = Val(cmd$(1))
      If a >= 0 And a < FILE_LEN Then
        For i = 0 To 9
          If bp(i) = a Then
            con.println("Duplicate breakpoint [" + Str$(i) + "]")
            a = -1
            Exit For
          EndIf
        Next
        For i = 0 To 9
          If a = -1 Then
            ' Duplicate breakpoint previously reported
            Exit For
          ElseIf bp(i) = -1 Then
            bp(i) = a
            num_bp = num_bp + 1
            con.println("Set breakpoint [" + Str$(i) + "] at " + fmt_hex$(bp(i)))
            Exit For
          EndIf
          If i = 9 Then con.println("No free address breakpoints")
        Next
      Else
        con.println("Invalid breakpoint address")
      EndIf

    ElseIf cmd$(0) = "B" Then
      ' List address breakpoints
      If num_bp = 0 Then
        con.println("No address breakpoints set")
      Else
        For i = 0 To 9
          If bp(i) <> -1 Then
            con.println("[" + Str$(i) + "] " + fmt_hex$(bp(i)))
          EndIf
        Next
      EndIf

    ElseIf cmd$(0) = "c" Then
      ' Continue
      con.endl()
      If oc = &h4 And op >= &hE0 Then con.print(">") ' Display READ prompt
      debug = E_OK

    ElseIf cmd$(0) = "C" Then
      ' Stack dump
      dmp_stack(Val(cmd$(1)))

    ElseIf cmd$(0) = "d" Then
      ' Dump memory
      If Len(cmd$(1)) = 0 Then a = pc Else a = Val(cmd$(1))
      dmp_mem(a, Val(cmd$(2)))

    ElseIf cmd$(0) = "dict" Then
      dmp_dict()

    ElseIf cmd$(0) = "dmap" Then
      dmp_mmap()

    ElseIf cmd$(0) = "G" Then
      ' Dump global variables
      dmp_global(Val(cmd$(1)), Val(cmd$(2)))

    ElseIf cmd$(0) = "h" Then
      con.print_file(file.resolve$(ss$(RESOURCES_DIR), "debug_help.txt"))

    ElseIf cmd$(0) = "H" Then
      ' Dump header
      dmp_hdr()

    ElseIf cmd$(0) = "q" Then
      ' Quit
      debug = E_QUIT

    ElseIf cmd$(0) = "o" Then
      ' Dump object
      dmp_obj(Val(cmd$(1)))

    ElseIf cmd$(0) = "s" Then
      ' Step
      If oc = &h4 And op >= &hE0 Then con.print(">") ' Display READ prompt
      debug = exec(0)
      If debug = E_OK Then debug = E_BREAK

    ElseIf cmd$(0) = "troff" Then
      ' Disable trace
      con.println("Trace OFF")
      ztrace = 0

    ElseIf cmd$(0) = "tron" Then
      ' Enable trace
      con.println("Trace ON")
      ztrace = 1

    ElseIf cmd$(0) = "v" Then
      ' Clear address breakpoint
      a = Val(cmd$(1))
      If a < 0 Or a > 9 Then
        con.println("Invalid address breakpoint")
      ElseIf bp(a) = -1 Then
        con.println("Address breakpoint [" + Str$(a) + "] already cleared")
      Else
        bp(a) = -1
        num_bp = num_bp - 1
        con.println("Address breakpoint [" + Str$(a) + "] cleared")
      EndIf

    ElseIf cmd$(0) = "V" Then
      ' Lookup word in dictionary
      a = di_lookup(LCase$(cmd$(1)))
      con.println(fmt_hex$(a))

    ElseIf cmd$(0) = "x" Then
      ' Parse and print value
      a = Val(cmd$(1))
      con.print(Str$(a))
      con.print("  " + fmt_hex$(a))
      con.println("  &b" + str.lpad$(Bin$(a), 16, "0"))

    ElseIf cmd$(0) = "z" Then
      ' Clear all breakpoints
      For i = 0 To 9 : bp(i) = -1 : Next
      num_bp = 0
      con.println("All breakpoints cleared")

    Else
      con.println("Unknown debug command")

    EndIf

  Loop While debug = E_DEBUG

End Function

' Prints abbreviations.
' z3 always has 96 abbreviations.
Sub dmp_abrv()
  Local i, j, nc, nr, width, x

  nc = 3 : nr = CInt(96 / nc + 0.4999) : width = 24
  For i = 0 To nr - 1
    For j = 0 To nc - 1
      con.flush()
      x = (j * nr) + i
      If x < 96 Then
        If j * width > Pos Then con.print(Space$(j * width - Pos))
        con.print("[" + str.lpad$(Str$(x), 2) + "] {")
        print_abrv(x)
        con.print "}"
      EndIf
    Next
    con.endl()
  Next

End Sub

' Prints dictionary.
Sub dmp_dict()
  Local a, i, j, nc, nr, width, x

  con.println("Word separators = {" + DICT_SEP$(0) + "}")
  con.println("Word count      = " + Str$(DICT_ENTRY_COUNT))
  con.println("Word size       = " + Str$(DICT_ENTRY_LEN))
  con.endl()

  nc = 3 : nr = CInt(DICT_ENTRY_COUNT / nc + 0.4999) : width = 24
  For i = 0 To nr - 1
    For j = 0 To nc - 1
      con.flush()
      x = (j * nr) + i
      If x < DICT_ENTRY_COUNT Then
        If j * width > Pos Then con.print(Space$(j * width - Pos))
        con.print("[" + str.lpad$(Str$(x + 1), 4) + "] ")
        a = x * DICT_ENTRY_LEN + DICT_ENTRY_AD
        x = rw(a) : con.print(str.lpad$(Hex$(x), 4, "0"))
        x = rw(a + 2) : con.print(str.lpad$(Hex$(x), 4, "0") + " ")
        print_zstring(a)
      EndIf
    Next
    con.endl()
  Next

End Sub

' Prints 'n' global variables starting from 'a'.
Sub dmp_global(a, n)
  Local i, x
  If n < 1 Then n = 1
  If a > 239 Then a = 239
  For i = a To a + n - 1
    If i > 239 Then Exit For
    x = vget(i + 16)
    con.print("G" + str.lpad$(Str$(i), 2, "0") + " = " + fmt_hex$(x))
    If x And &h8000 Then
      con.print("  " + Str$(x - &h10000))
    Else
      con.print("  " + Str$(x))
    EndIf
    con.endl()
  Next
End Sub

' Prints Z-machine header.
Sub dmp_hdr()
  Local i
  con.println("Version      = " + Str$(rb(&h00)))
  con.println("Flags1       = &b" + str.lpad$(Bin$(rb(&h01)), 8, "0"))
  con.println("Release      = " + Str$(rw(&h02)))
  con.println("Base high    = " + fmt_hex$(rw(&h04)))
  con.println("Initial PC   = " + fmt_hex$(rw(&h06)))
  con.println("Dictionary   = " + fmt_hex$(rw(&h08)))
  con.println("Object tbl   = " + fmt_hex$(rw(&h0A)))
  con.println("Glob var tbl = " + fmt_hex$(rw(&h0C)))
  con.println("Base static  = " + fmt_hex$(rw(&h0E)))
  con.println("Flags2       = &b" + str.lpad$(Bin$(rb(&h10)), 8, "0"))
  con.print("Serial num   = ")
  For i = &h12 To &h17 : con.print(Chr$(rb(i))) : Next
  con.endl()
  con.println("Abbrv tbl    = " + fmt_hex$(rw(&h18)))
  con.println("File length  = " + Str$(2 * rw(&h1A)) + " bytes")
  con.println("File chksum  = " + Str$(rw(&h1C)))
  con.println("Std revision = " + Str$(rw(&h32)))
End Sub

' Prints 'n' bytes from 'mem' starting from 'ad'
Sub dmp_mem(ad, n)
  Local i, x
  If n = 0 Then n = 128
  For i = 0 To n - 1
    If i Mod 16 = 0 Then con.print("[" + fmt_hex$(ad + i) + "] ")
    If ad + i < FILE_LEN Then
      x = rb(ad + i)
      con.print(str.lpad$(Hex$(x), 2, "0") + " ")
    Else
      con.print("XX ")
    EndIf
    If ((i + 1) Mod 16 = 0) And (i <> n - 1) Then con.endl()
  Next
  con.endl()
End Sub

' Dump CMM1 virtual memory map.
Sub dmp_mmap
  Local i, j, nc, nr, width, x

  On Error Skip 1
  i = NUM_PHYSICAL_PAGES
  If MM.ERRNO <> 0 Then
    ' If NUM_PHYSICAL_PAGES in undefined then ...
    con.println("Not using virtual memory implementation.")
    On Error Clear
    Exit Sub
  EndIf

  con.println("Physical page -> Virtual page")
  nc = 6 : nr = CInt(NUM_PHYSICAL_PAGES / nc + 0.4999) : width = 15
  For i = 0 To nr - 1
    For j = 0 To nc - 1
      con.flush()
      x = (j * nr) + i
      If x < NUM_PHYSICAL_PAGES Then
        If j * width > Pos Then con.print(Space$(j * width - Pos))
        con.print(str.lpad$(Str$(x), 3) + " -> " + str.lpad$(Str$(pp_to_vp(x)), 3))
      EndIf
    Next
    con.endl()
  Next

  con.endl()

  con.println("Virtual page -> Physical page")
  nr = CInt(NUM_VIRTUAL_PAGES / nc + 0.4999)
  For i = 0 To nr - 1
    For j = 0 To nc - 1
      con.flush()
      x = (j * nr) + i
      If x < NUM_VIRTUAL_PAGES Then
        If j * width > Pos Then con.print(Space$(j * width - Pos))
        con.print(str.lpad$(Str$(x), 3) + " -> " + str.lpad$(Str$(vp_to_pp(x)), 3))
      EndIf
    Next
    con.endl()
  Next

End Sub

' Prints object 'o'.
Sub dmp_obj(o)
  Local ad, i, p, sz, x

  If o <= 0 Then
    con.println("Property defaults:")
    ad = rw(&h0A)
    For i = 0 To 30
      x = LGetByte(m(), ad) * 256 + LGetByte(m(), ad + 1)
      con.print(str.lpad$(Hex$(x), 4, "0") + " ")
      If (i + 1) Mod 10 = 0 Then con.endl()
      Inc ad, 2
    Next
    con.endl()
    Exit Sub
  EndIf

  con.print(Str$(o) + ". ")
  con.print("Attributes: ")
  For i = 0 To 31
    x = ob_attr(o, i)
    If x <> 0 Then con.print(Str$(i))
  Next
  con.endl()

  x = ob_rel(o, 4)
  con.print("   Parent object: " + Str$(x) + "  ")
  x = ob_rel(o, 5)
  con.print("Sibling object: " + Str$(x) + "  ")
  x = ob_rel(o, 6)
  con.println("Child object: " + Str$(x))

  ad = ob_prop_base(o)
  con.println("   Property address: " + fmt_hex$(ad))
  con.print("        Description: '")
  ob_print(o);
  con.println("'")
  con.println("         Properties:")
  p = 0
  Do
    p = ob_next_prop(o, p)
    If p > 0 Then
      ad = ob_prop_addr(o, p)
      sz = ob_prop_len(ad)
      x = LGetByte(m(), ad - 1)
      If x \ 32 + 1 <> sz Then Error
      con.print("             [" + Str$(p) + "] ")
      For i = 1 To sz
        x = LGetByte(m(), ad)
        Inc ad, 1
        con.print(str.lpad$(Hex$(x), 2, "0") + " ")
      Next
      con.endl()
    EndIf
  Loop Until p = 0
End Sub

' Prints dissassembly for the current instruction.
'
' @param  m$  instruction mnemonic
Sub dmp_op(m$)
  con.print(str.rpad$(m$, 14))
  If m$ = "CALL" Then
    con.print(fmt_call_operands$())
  ElseIf m$ = "JUMP" Then
    con.print(fmt_jump_operands$())
  ElseIf m$ = "STORE" Or m$ = "DEC_CHK" Or m$ = "INC_CHK" Then
    con.print(fmt_store_operands$())
  ElseIf m$ = "DEC" Or m$ = "INC" Or m$ = "PULL" Or m$= "LOAD" Then
    con.print(fmt_store_operands$())
  Else
    con.print(fmt_normal_operands$())
  EndIf

  If st > -1 Then con.print(" -> " + fmt_store_value$(st))

  If br = 0 Then con.endl() : Exit Sub
  If br And &h80000 Then con.print(" [TRUE] ") Else con.print(" [FALSE] ")
  If (br And &h7FFFF) = pc - 2 Then con.println("RFALSE") : Exit Sub
  If (br And &h7FFFF) = pc - 1 Then con.println("RTRUE") : Exit Sub
  con.println(Hex$(br And &h7FFFF))
End Sub

' Print information about the routine starting at 'new_pc'.
Sub dmp_routine(new_pc)
  Local i, locals_sz, x
  locals_sz = rb(new_pc)
  con.print("Routine " + Hex$(new_pc) + ", " + Str$(locals_sz) + " locals (")
  For i = 0 To locals_sz - 1
    If i > 0 Then con.print(", ")
    x = rw(new_pc + 1 + i * 2)
    con.print(str.lpad$(Hex$(x), 4, "0"))
  Next
  con.println(")")
End Sub

' Prints the contents of the stack.
' If 'a' = 0 then prints only the current stack frame,
' otherwise prints them all.
Sub dmp_stack(a)
  Local i, tmp_fp, x

  con.print("TOP: ")
  If sp = 0 Then con.println("*empty*")
  tmp_fp = fp

  For i = sp - 1 To 0 Step -1
    If i < sp - 1 Then con.print("     ")
    x = st_peek(i)

    If x And &h8000 Then
      con.print(fmt_hex$(x) + "  " + str.lpad$(Str$(x - &h10000), 6, " ") + "  ")
    Else
      con.print(fmt_hex$(x) + "  " + str.lpad$(Str$(x), 6, " ") + "  ")
    EndIf

    If tmp_fp = &hFFFF Then
      ' Nothing
    ElseIf i = tmp_fp Then
      con.print("previous fp")
      If a <> 0 Then con.endl() : con.print(String$(35, "-"))
      tmp_fp = x
    ElseIf i = tmp_fp + 1 Then
      con.print("store result")
    ElseIf i = tmp_fp + 2 Then
      con.print("return address (lo)")
    ElseIf i = tmp_fp + 3 Then
      con.print("return address (hi)")
    ElseIf i = tmp_fp + 4 Then
      con.print("num locals")
    Else
      con.print("L" + str.lpad$(Str$(i - tmp_fp - 5), 2, "0"))
      ' TODO: Not everything on top of the stack is a local variable
    EndIf
    con.endl()
    If a = 0 And i = fp Then Exit For
  Next
End Sub

Function fmt_operand$(i)
  Local a$, x
  x = ov(i)
  If ot(i) <> &b10 Then ' &b10 = VARIABLE
    fmt_operand$ = "#" + str.lpad$(Hex$(x), 2, "0")
    Exit Function
  EndIf
  If x = 0 Then
    a$ = "(SP)+"
  ElseIf x < &h10 Then
    a$ = "L" + str.lpad$(Hex$(x - 1), 2, "0")
  Else
    a$ = "G" + str.lpad$(Hex$(x - &h10), 2, "0")
  EndIf
  If x > 0 Then a$ = a$ + " (=" + Hex$(vget(x)) + ")"
  fmt_operand$ = a$
End Function

Function fmt_call_operands$()
  Local a$, i
  If ot(0) = &b10 Then ' &b10 = VARIABLE
    a$ = fmt_operand$(i)
  Else
    a$ = Hex$(2 * ov(0))
  EndIf
  a$ = a$ + " ("
  For i = 1 To onum - 1
    If i > 1 Then a$ = a$ + ", "
    a$ = a$ + fmt_operand$(i)
  Next
  a$ = a$ + ")"
  fmt_call_operands$ = a$
End Function

Function fmt_jump_operands$()
  Local of
  If onum > 1 Then Error "Too many operands."
  of = oa(0)
  If of And 2^15 Then of = of - 65536
  fmt_jump_operands$ = Hex$(pc + of - 2)
End Function

Function fmt_store_value$(st)
  If st = 0 Then
    con.print("-(SP)")
  ElseIf st < &h10 Then
    con.print("L" + str.lpad$(Hex$(st - 1), 2, "0"))
  Else
    con.print("G" + str.lpad$(Hex$(st - &h10), 2, "0"))
  EndIf
End Function

Function fmt_store_operands$()
  Local a$, i
  If ot(0) = &b10 Then Error "Unexpected VARIABLE operand"
  a$ = a$ + fmt_store_value$(ov(0))
  For i = 1 To onum - 1
    a$ = a$ + ", " + fmt_operand$(i)
  Next
  fmt_store_operands$ = a$
End Function

Function fmt_normal_operands$()
  Local a$, i
  For i = 0 To onum - 1
    If i > 0 Then a$ = a$ + ", "
    a$ = a$ + fmt_operand$(i)
  Next
  fmt_normal_operands$ = a$
End Function

' ---- src/debug.inc

' ss$ is for string "constants" that I don't want to take up 256 bytes on the
' micro-controller devices.
If InStr(Mm.Info(Device), "PicoMite") Then
  Dim ss$(5) Length 32
Else
  Dim ss$(5)
EndIf

On Error Ignore
Mode 1
Font 1
On Error Abort

Const INSTALL_DIR = 0
Const RESOURCES_DIR = 1
Const SAVE_DIR = 2
Const SCRIPT_DIR = 3
Const STORY_DIR = 4
Const STORY_FILE = 5

Sub main_init()
  Local i, x

  con.endl()
  mem_init(file.resolve$(ss$(STORY_DIR), ss$(STORY_FILE) + ".z3"))
  di_init()
  con.endl()
  GLOBAL_VAR = rw(&h0C)

  ' Hack header bits
  x = rb(&h01)
  x = x Or  &b00010000 ' set bit 4 - status line not available
  x = x And &b10011111 ' clear bits 5 & 6 - no screen-splitting, fixed-pitch font
  wb(&h01, x)
  wb(&h20, con.HEIGHT)
  wb(&h21, con.WIDTH)

  pc = rw(&h06)
  For i = Bound(stack(), 0) To Bound(stack(), 1) : stack(i) = 0 : Next
  sp = 0
  fp = &hFFFF

End Sub

Sub main()
  Local i, old_dir$, old_pc, state, s$

  ss$(INSTALL_DIR) = get_install_dir$()
  ss$(RESOURCES_DIR) = file.resolve$(ss$(INSTALL_DIR), "resources")
  ss$(SAVE_DIR)   = file.resolve$(ss$(INSTALL_DIR), "saves")
  ss$(SCRIPT_DIR) = file.resolve$(ss$(INSTALL_DIR), "scripts")
  ss$(STORY_DIR)  = file.resolve$(ss$(INSTALL_DIR), "stories")

  main.init_console()

  Cls

  con.print_file(file.resolve$(ss$(RESOURCES_DIR), "title.txt"), 1)

  de_init()
  For i = 0 To 9 : bp(i) = -1 : Next

  ' Select a story file
  con.println("Select a story file from '" + ss$(STORY_DIR) + "':")
  Do While s$ = ""
    s$ = fi_choose$(ss$(STORY_DIR), "*.z3")
  Loop
  s$ = Mid$(s$, Len(ss$(STORY_DIR)) + 2)
  ss$(STORY_FILE) = Left$(s$, Len(s$) - 3)

  ' Ensure subdirectories for the current story exist in "saves/" and "scripts/"
  old_dir$ = Cwd$
  ChDir(ss$(SAVE_DIR))
  s$ = Dir$(ss$(STORY_FILE), File) : If s$ <> "" Then Error "Unexpected file: " + s$
  s$ = Dir$(ss$(STORY_FILE), Dir) : If s$ = "" Then MkDir(ss$(STORY_FILE))
  ChDir(ss$(SCRIPT_DIR))
  s$ = Dir$(ss$(STORY_FILE), File) : If s$ <> "" Then Error "Unexpected file:" + s$
  s$ = Dir$(ss$(STORY_FILE), Dir) : If s$ = "" Then MkDir(ss$(STORY_FILE))
  ChDir(old_dir$)

  main_init()

'  If LCase$(con.in$("Start in debugger [Y|n] ")) <> "n" Then
'     state = E_BREAK
'  Else
'     state = E_OK
'  EndIf

  Pause 1000

  ' This will clear the console, see con.endl()
  For i = 0 To 10 : con.endl() : Next

  Timer = 0

  Do While state <> E_QUIT
    ' If there are active breakpoint and the PC has changed since we last checked
    If num_bp > 0 And pc <> old_pc Then
      For i = 0 To 9
        If pc = bp(i) Then
          con.println("[Breakpoint " + Str$(i) + " reached]")
          state = E_BREAK
        EndIf
      Next
    EndIf

    If con.spin_enabled And num_ops Mod 16 = 0 Then con.spin()

    old_pc = pc
    If state = E_OK Or state = E_REPEAT Then
      state = exec(ztrace)
    Else
      state = debug()
    EndIf
  Loop

  con.endl()
  con.println("Num instructions processed = " + Str$(num_ops))
  con.print("Instructions / second      = ")
  con.println(Format$(1000 * num_ops / Timer, "%.1f"))
'  con.println("Num page faults            = " + Str$(pf))

  con.close_out()
  con.close_in()
End Sub

main()
End

Sub main.init_console()
  Local cmdline$ = LCase$(Mm.CmdLine$)

  If InStr(cmdline$, "--platform") Then
    ' Platform already supplied explicitly, do nothing.
  ElseIf Mm.HRes = 320 And (Mm.VRes = 480 Or Mm.VRes = 320) Then
    Cat cmdline$, " --platform=picocalc"
  ElseIf Mm.HRes = 320 And Mm.VRes = 240 Then
    Cat cmdline$, " --platform=320x240"
  ElseIf Mm.Device$ = "MMBasic for Windows" Or InStr(Mm.Device$, "Colour Maximite 2") Then
    Cat cmdline$, " --platform=cmm2"
  ElseIf InStr(Mm.Device$, "PicoMiteVGA") Then
    Cat cmdline$, " --platform=pmvga"
  ElseIf InStr(Mm.Device$, "PicoMiteHDMI") Then
    Cat cmdline$, " --platform=pmhdmi"
  EndIf

  If InStr(cmdline$, "--platform=picocalc") Then
    con.init(40, 26, 1)
  ElseIf InStr(cmdline$, "--platform=cmm2") Then
    con.init(100, 50)
  ElseIf InStr(cmdline$, "--platform=320x240") Then
    con.init(40, 20, 1)
  ElseIf InStr(cmdline$, "--platform=pmvga") Then
    con.init(80, 40, 1)
  ElseIf InStr(cmdline$, "--platform=pmhdmi") Then
    con.init(80, 40, 1)
  ElseIf InStr(cmdline$, "--platform") Then
    Error "Unknown platform"
  ElseIf Mm.Info(Device X) = "MMB4L" Then
    Local w%, h%
    Console GetSize w%, h%
    w% = Max(w%, 40)
    h% = Max(h%, 20)
    con.init(w%, h%)
  Else
    con.init(80, 40)
  EndIf

  If Mm.Info(Device X) = "MMB4L" Then Console Resize con.WIDTH, con.HEIGHT
End Sub

Function get_install_dir$()
  get_install_dir$ = Left$(Mm.Info(Path), Len(Mm.Info(Path)) - 1)
  If get_install_dir$ = "NON" Then get_install_dir$ = Cwd$
  If Not Mm.Info(Exists Dir file.resolve$(get_install_dir$, "resources")) Then
    get_install_dir$ = file.get_parent$(get_install_dir$)
  EndIf
End Function
