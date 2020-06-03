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
