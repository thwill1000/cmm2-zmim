' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Option Explicit On
Option Default Integer

'!set CMM1
'!set NO_DEBUG

'!comment_if CMM1
'#Include "mem_cmm2_fast.inc"
'!endif
'#Include "mem_cmm2_safe.inc"
'!uncomment_if CMM1
#Include "mem_cmm1.inc"
'!endif
#Include "stack.inc"
#Include "variable.inc"
#Include "decode.inc"
#Include "execute.inc"
#Include "console.inc"
#Include "zstring.inc"
#Include "objects.inc"
#Include "util.inc"
#Include "dict.inc"
#Include "zsave.inc"
#Include "file.inc"
'!comment_if NO_DEBUG
#Include "debug.inc"
'!endif

' String "constants" that I don't want to take up 256 bytes
Dim ss$(4) Length 20
Const INSTALL_DIR = 0
Const SAVE_DIR = 1
Const SCRIPT_DIR = 2
Const STORY_DIR = 3
Const STORY = 4

Const DESCRIPTION$ = "Z-MIM: a Z-Machine Interpreter for the Maximite"
Const VERSION$ = "Release 2 for Colour Maximite 2, MMBasic 5.05"
Const COPYRIGHT$ = "Copyright (c) 2019-20 Thomas Hugo Williams"

Sub main_init()
  Local i, x

  endl()
  mem_init(ss$(STORY_DIR) + "/" + ss$(STORY) + ".z3")
  di_init()
  endl()
  GLOBAL_VAR = rw(&h0C)

  ' Hack header bits
  x = rb(&h01)
  x = x Or  &b00010000 ' set bit 4 - status line not available
  x = x And &b10011111 ' clear bits 5 & 6 - no screen-splitting, fixed-pitch font
  wb(&h01, x)
  wb(&h20, C_HEIGHT)
  wb(&h21, C_WIDTH)

  pc = rw(&h06)
  For i = 0 To 511 : stack(i) = 0 : Next i
  sp = 0
  fp = &hFFFF

End Sub

Sub main()
  Local i, old_dir$, old_pc, state, s$

  de_init()
'!comment_if NO_DEBUG
  For i = 0 To 9 : bp(i) = -1 : Next i
'!endif

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

  ' Select a story file
  cout("Select a story file from '" + ss$(STORY_DIR) + "':") : endl()
  Do While s$ = ""
    s$ = fi_choose$(ss$(STORY_DIR), "*.z3")
  Loop
  s$ = Mid$(s$, Len(ss$(STORY_DIR)) + 2)
  ss$(STORY) = Left$(s$, Len(s$) - 3)

  ' Ensure subdirectories for the current story exist in "saves/" and "scripts/"
  old_dir$ = Cwd$
  ChDir(ss$(SAVE_DIR))
  s$ = Dir$(ss$(STORY), File) : If s$ <> "" Then Error "Unexpected file: " + s$
  s$ = Dir$(ss$(STORY), Dir) : If s$ = "" Then MkDir(ss$(STORY))
  ChDir(ss$(SCRIPT_DIR))
  s$ = Dir$(ss$(STORY), File) : If s$ <> "" Then Error "Unexpected file:" + s$
  s$ = Dir$(ss$(STORY), Dir) : If s$ = "" Then MkDir(ss$(STORY))
  ChDir(old_dir$)

  main_init()

'  If LCase$(cin$("Start in debugger [Y|n] ")) <> "n" Then
'     state = E_BREAK
'  Else
'     state = E_OK
'  EndIf

  ' Jump through hoops to create the name of the script file
  s$ = ss$(STORY) + "-" + Date$ + "-" + Time$ + ".scr"
  For i = 1 To Len(s$)
    If Peek(Var s$, i) = Asc(":") Then Poke Var s$, i, Asc("-")
  Next i
  s$ = ss$(SCRIPT_DIR) + "/" + ss$(STORY) + "/" + s$
  If LCase$(cin$("Write script to '" + s$ + "' [Y|n] ")) <> "n" Then
    Open s$ For Output As #2
    script = S_WRITE
  EndIf

  ' This will clear the console, see console#endl
  For i = 0 To 10 : endl() : Next i

  Timer = 0

  Do While state <> E_QUIT
'!comment_if NO_DEBUG
    ' If there are active breakpoint and the PC has changed since we last checked
    If num_bp > 0 And pc <> old_pc Then
      For i = 0 To 9
        If pc = bp(i) Then
          cout("[Breakpoint " + Str$(i) + " reached]") : endl()
          state = E_BREAK
        EndIf
      Next i
    EndIf
'!endif

    old_pc = pc
    If state = E_OK Or state = E_REPEAT Then
      state = exec(ztrace)
    Else
      state = debug()
    EndIf
  Loop

  endl()
  cout("Num instructions processed = " + Str$(num_ops)) : endl()
  cout("Instructions / second      = ")
  cout(Format$(1000 * num_ops / Timer, "%.1f"))
  endl()
'!uncomment_if CMM1
'  cout("Num page faults            = " + Str$(pf)) : endl()
'!endif

  If script And S_WRITE Then Close #2
  If script And S_READ Then Close #3

End Sub

main()
End
