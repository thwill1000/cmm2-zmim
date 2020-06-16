' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Option Explicit On
Option Default Integer

'#Include "mem_cmm2_fast.inc"
#Include "mem_cmm2_safe.inc"
'#Include "mem_cmm1.inc"
#Include "stack.inc"
#Include "variable.inc"
#Include "decode.inc"
#Include "execute.inc"
#Include "zstring.inc"
#Include "objects.inc"
#Include "util.inc"
#Include "dict.inc"
#Include "zsave.inc"
#Include "console.inc"
#Include "file.inc"
#Include "debug.inc"

Const E_OK = 0
Const E_UNKNOWN = 1
Const E_UNIMPLEMENTED = 2
Const E_BREAK = 3
Const E_QUIT = 4
Const E_DEBUG = 5
Const E_REPEAT = 6 ' Repeat last operation

Const NUM_BP = 10

Dim num_ops = 0    ' Number of instructions processed.
Dim ztrace = 0     ' Is instruction tracing enabled?
Dim bp(NUM_BP - 1) ' The addresses of up to 10 breakpoints, -1 for unset.
Dim rtime = 0      ' Time (ms) spent waiting for user input.

' String "constants" that I don't want to take up 256 bytes
Dim ss$(4) Length 20
Const INSTALL_DIR = 0
Const SAVE_DIR = 1
Const SCRIPT_DIR = 2
Const STORY_DIR = 3
Const STORY = 4

Const DESCRIPTION$ = "Z-MIM: a Z-Machine Interpreter for the Maximite"
Const VERSION$ = "Release 1 for Colour Maximite 2, MMBasic 5.05"
Const COPYRIGHT$ = "Copyright (c) 2019-20 Thomas Hugo Williams"

Sub main()
  Local f$, i, old_pc, state, s$, x

  Mode 1
  Cls

  cout("        ______     __  __ _____ __  __ ") : endl()
  cout("       |___  /    |  \/  |_   _|  \/  |") : endl()
  cout("          / /_____| \  / | | | | \  / |") : endl()
  cout("         / /______| |\/| | | | | |\/| |") : endl()
  cout("        / /__     | |  | |_| |_| |  | |") : endl()
  cout("       /_____|    |_|  |_|_____|_|  |_|") : endl()
  endl()
  cout(DESCRIPTION$) : endl()
  endl()
  cout(COPYRIGHT$) : endl()
  cout(VERSION$) : endl()
  endl()

  ss$(INSTALL_DIR) = "A:/zmim"
  ss$(SAVE_DIR) = ss$(INSTALL_DIR) + "/saves"
  ss$(SCRIPT_DIR) = ss$(INSTALL_DIR) + "/scripts"
  ss$(STORY_DIR) = ss$(INSTALL_DIR) + "/stories"

  cout("Select a story file from '" + ss$(STORY_DIR) + "':") : endl()
  Do While f$ = ""
    f$ = file_choose$(ss$(STORY_DIR), "*.z3")
  Loop
  ss$(STORY) = Mid$(f$, Len(ss$(STORY_DIR)) + 2)
  ss$(STORY) = Left$(ss$(STORY), Len(ss$(STORY)) - 3)
  endl()

  mem_init(f$)
  endl()

  ' Ensure subdirectories for the current story exist in "saves/" and "scripts/"
  ChDir(ss$(SAVE_DIR))
  s$ = Dir$(ss$(STORY), File) : If s$ <> "" Then Error
  s$ = Dir$(ss$(STORY), Dir) : If s$ = "" Then MkDir(ss$(STORY))
  ChDir(ss$(SCRIPT_DIR))
  s$ = Dir$(ss$(STORY), File) : If s$ <> "" Then Error
  s$ = Dir$(ss$(STORY), Dir) : If s$ = "" Then MkDir(ss$(STORY))
  ChDir(ss$(INSTALL_DIR))

  ' Jump through hoops to create the name of the script file
  f$ = ss$(SCRIPT_DIR) + "/" + ss$(STORY) + "/"
  f$ = f$ + ss$(STORY) + "-" + Date$ + "-" + Time$ + ".scr"
  For i = Len(ss$(SCRIPT_DIR)) To Len(f$)
    If Peek(Var f$, i) = Asc(":") Then Poke Var f$, i, Asc("-")
  Next i
  s$ = cin$("Write script to '" + f$ + "' [Y|n] ")
  If LCase$(s$) <> "n" Then
    Open f$ For Output As #2
    script = S_WRITE
  EndIf

'  Input "Start in debugger [Y|n]"; s$
'  If LCase$(s$) = "n" Then state = E_OK Else state = E_BREAK
  endl()

  ' Hack header bits
  x = rb(&h01)
  x = x Or  &b00010000 ' set bit 4 - status line not available
  x = x And &b10011111 ' clear bits 5 & 6 - no screen-splitting, fixed-pitch font
  wb(&h01, x)

  decode_init()
  di_init()

  For i = 0 To NUM_BP - 1 : bp(i) = -1 : Next i

  Timer = 0

  Do While state <> E_QUIT
    If pc <> old_pc Then ' Prevents repeatedly hitting breakpoint when continuing
      For i = 0 To NUM_BP - 1
        If pc = bp(i) Then
          cout("[Breakpoint " + Str$(i) + " reached]") : endl()
          state = E_BREAK
        EndIf
      Next i
      old_pc = pc
    EndIf

    If state = E_OK Or state = E_REPEAT Then
      state = exec(ztrace)
    Else
      state = debug()
    EndIf
  Loop

  endl()
  cout("Num instructions processed = " + Str$(num_ops)) : endl()
  cout("Instructions / second      = ")
  cout(Format$(num_ops / ((Timer - rtime) / 1000), "%.1f"))
  endl()
  If MM.DEVICE$ <> "Colour Maximite 2" Then
    cout("Num page faults            = " + Str$(pf)) : endl()
  EndIf

  If script And S_WRITE Then Close #2
  If script And S_READ Then Close #3

End Sub

main()
