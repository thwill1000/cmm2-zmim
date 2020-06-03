' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Option Explicit On

'#Include "memory.inc"
#Include "memory_fast.inc"
#Include "stack.inc"
#Include "variable.inc"
#Include "decode.inc"
#Include "execute.inc"
#Include "zstring.inc"
#Include "objects.inc"
#Include "util.inc"
#Include "file.inc"
#Include "random.inc"
#Include "debug.inc"
#Include "dmp_abrv.inc"
#Include "dmp_dict.inc"
#Include "dmp_hdr.inc"
#Include "dmp_mmap.inc"
#Include "tst_obj.inc"

Dim BUSY$(1) LENGTH 16
BUSY$(0) = "\\\\||||////----"

Dim i = 0
Dim BIT(7)
For i = 0 To 7 : BIT(i) = 2 ^ i : Next i

Const BTM_2_BITS  = &b00000011
Const BTM_4_BITS  = &b00001111
Const BTM_5_BITS  = &b00011111
Const BTM_6_BITS  = &b00111111

Const E_OK = 0
Const E_UNKNOWN = 1
Const E_UNIMPLEMENTED = 2
Const E_BREAK = 3
Const E_QUIT = 4
Const E_DEBUG = 5

Const NUM_BP = 10

Dim num_ops = 0    ' Number of instructions processed.
Dim ztrace = 0     ' Is instruction tracing enabled?
Dim bp(NUM_BP - 1) ' The addresses of up to 10 breakpoints, -1 for unset.
Dim rtime = 0      ' Time (ms) spent waiting for user input.

' Interactive debugger
Function debug()
  Local a, b, c, cmd$(9) Length 20, cn, i, op, pc_old, s$, sp_old

  ' Decode and display the next instruction but don't execute it.
  pc_old = pc : sp_old = sp
  op = decode(1)
  pc = pc_old : sp = sp_old

  Do
    ' Read line of input and parse into space separated commands/arguments.
    cn = 0
    For i = 0 To 9 : cmd$(i) = "" : Next i
    rtime = rtime - Timer
    Line Input "DEBUG >> ", s$
    rtime = rtime + Timer
    s$ = s$ + " "
    For i = 1 To Len(s$)
      c = Peek(Var s$, i)
      If Chr$(c) = " " Then
        If Len(cmd$(cn)) > 0 Then cn = cn + 1
        If cn = 10 Then Error "Too many arguments"
      Else
        cmd$(cn) = cmd$(cn) + Chr$(c)
      EndIf
    Next i

    debug = E_DEBUG

    If cmd$(0) = "b" Then
      ' Set address breakpoint
      a = Val(cmd$(1))
      If a >= 0 And a < FILE_LEN Then
        For i = 0 To NUM_BP - 1
          If bp(i) = a Then
            Print "Duplicate breakpoint [" + Str$(i) + "]"
            a = -1
            Exit For
          EndIf
        Next i
        For i = 0 To NUM_BP - 1
          If a = -1 Then
            ' Duplicate breakpoint previously reported
            Exit For
          ElseIf bp(i) = -1 Then
            bp(i) = a
            Print "Set breakpoint [" + Str$(i) + "] at " + fmt_hex$(bp(i))
            Exit For
          EndIf
          If i = 9 Then Print "No free address breakpoints"
        Next i
      Else
        Print "Invalid breakpoint address"
      EndIf

    ElseIf cmd$(0) = "B" Then
      ' List address breakpoints
      a = 0
      For i = 0 To NUM_BP - 1
        If bp(i) <> -1 Then
          Print "[" + Str$(i) + "] " + fmt_hex$(bp(i))
          a = a + 1
        EndIf
      Next i
      If a = 0 Then Print "No address breakpoints set"

    ElseIf cmd$(0) = "c" Then
      ' Continue
      If oc = &h4 And op >= &hE0 Then Print ">"; ' Display READ prompt
      debug = E_OK

    ElseIf cmd$(0) = "C" Then
      ' Stack dump
      dmp_stack(Val(cmd$(1)))

    ElseIf cmd$(0) = "d" Then
      ' Dump memory
      If Len(cmd$(1)) = 0 Then a = pc Else a = Val(cmd$(1))
      dmp_mem(a, Val(cmd$(2)))

    ElseIf cmd$(0) = "dict" Then
      ' Dump dictionary
      dmp_dict()

    ElseIf cmd$(0) = "G" Then
      ' Dump global variables
      dmp_global(Val(cmd$(1)), Val(cmd$(2)))

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
      If oc = &h4 And op >= &hE0 Then Print ">"; ' Display READ prompt
      debug = exec(0)
      If debug = E_OK Then debug = E_BREAK

    ElseIf cmd$(0) = "troff" Then
      ' Disable trace
      Print "Trace OFF"
      ztrace = 0

    ElseIf cmd$(0) = "tron" Then
      ' Enable trace
      Print "Trace ON"
      ztrace = 1

    ElseIf cmd$(0) = "v" Then
      ' Clear address breakpoint
      a = Val(cmd$(1))
      If a < 0 Or a >= NUM_BP Then
        Print "Invalid address breakpoint"
      ElseIf bp(a) = -1 Then
        Print "Address breakpoint [" + Str$(a) + "] already cleared"
      Else
        bp(a) = -1
        Print "Address breakpoint [" + Str$(a) + "] cleared"
      EndIf

    ElseIf cmd$(0) = "V" Then
      ' Lookup word in vocabulary
      a = lookup(LCase$(cmd$(1)))
      Print fmt_hex$(a)

    ElseIf cmd$(0) = "x" Then
      ' Parse and print value
      a = Val(cmd$(1))
      Print Str$(a);
      Print "  " + fmt_hex$(a)
      Print "  &b" + LPad$(Bin$(a), 16, "0")

    ElseIf cmd$(0) = "z" Then
      ' Clear all breakpoints
      For i = 0 To NUM_BP - 1 : bp(i) = -1 : Next i
      Print "All breakpoints cleared"

    Else
      Print "Unknown debug command"

    EndIf

  Loop While debug = E_DEBUG

End Function

Sub main()
  Local i, old_pc, state, s$

  Mode 1
  Cls

  Print "             Sockpuppet Studios"
  Print "                  presents"
  Print "       A Toy Plastic Trumpet Production"
  Print
  Print "ZMIM: a Z-Machine Interpreter for the Maximite"
  Print
  Print "Copyright (c) 2019-20 Thomas Hugo Williams"
  Print "Version 0.1 for Colour Maximite 2, MMBasic 5.05"
  Print

  Print "Select a story file from 'A:/zmim/stories':"
  s$ = file_choose$("A:/zmim/stories", "*.z3")
  Print

  mem_init(s$)
  decode_init()
  Print

  Input "Start in debugger [Y|n]"; s$
  If LCase$(s$) = "n" Then state = E_OK Else state = E_BREAK
  Print

  For i = 0 To NUM_BP - 1 : bp(i) = -1 : Next i

  Timer = 0

  Do While state <> E_QUIT
    If pc <> old_pc Then ' Prevents repeatedly hitting breakpoint when continuing
      For i = 0 To NUM_BP - 1
        If pc = bp(i) Then
          Print "[Breakpoint " + Str$(i) + " reached]"
          state = E_BREAK
        EndIf
      Next i
      old_pc = pc
    EndIf

    If state = E_OK Then
      state = exec(ztrace)
    Else
      state = debug()
    EndIf
  Loop

  Print
  Print "Num instructions processed ="; num_ops
  Print "Instructions / second      = "; Format$(num_ops / ((Timer - rtime) / 1000), "%.1f")
  If MM.DEVICE$ <> "Colour Maximite 2" Then Print "Num page faults            ="; pf

End Sub

main()
