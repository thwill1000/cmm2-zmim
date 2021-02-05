' Copyright (c) 2019-21 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

If Mm.Device$ <> "Colour Maximite" Then
  Option Explicit On
  Option Default Integer
EndIf

Mode 1

#Include "mem_cmm2_safe.inc"
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
Dim ss$(5) Length 32

'!comment_if INLINE_CONSTANTS
Const INSTALL_DIR = 0
Const RESOURCES_DIR = 1
Const SAVE_DIR = 2
Const SCRIPT_DIR = 3
Const STORY_DIR = 4
Const STORY_FILE = 5
'!endif

'!comment_if TARGET_CMM1
' Gets name in format "story-DD-MM-YY-hh-mm-ss.scr"
Function script_file_name$()
  Local i, s$
  s$ = ss$(STORY_FILE) + "-" + Date$ + "-" + Time$ + ".scr"
  For i = 1 To Len(s$)
    If Peek(Var s$, i) = Asc(":") Then Poke Var s$, i, Asc("-")
  Next i
  script_file_name$ = s$
End Function
'!endif

'!uncomment_if TARGET_CMM1
'' Returns name in format "MMDDhhmm.scr"
'Function script_file_name$()
'  Local a(7), i, s$(1) Length 20
'  a(0) = 4  : a(1) = 5  : a(2) = 1  : a(3) = 2
'  a(4) = 12 : a(5) = 13 : a(6) = 15 : a(7) = 16
'  s$(0) = Date$ + ":" + Time$
'  For i = 0 To 7
'    s$(1) = s$(1) + Mid$(s$(0), a(i), 1)
'  Next i
'  script_file_name$ = s$(1) + ".scr"
'End Function
'!endif

Sub main_init()
  Local i, x

  endl()
  mem_init(ss$(STORY_DIR) + "\" + ss$(STORY_FILE) + ".z3")
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

'!comment_if TARGET_CMM1
  s$ = Mm.Info(Path)
  If Right$(s$, 1) = "/" Then s$ = Left$(s$, Len(s$) - 1)
  If Right$(s$, 1) = "\" Then s$ = Left$(s$, Len(s$) - 1)
  ss$(INSTALL_DIR) = ""
  For i = 1 To Len(s$)
    Cat ss$(INSTALL_DIR), Choice(Mid$(s$, i, 1) = "/", "\", Mid$(s$, i, 1))
  Next
  Cat ss$(INSTALL_DIR), "\.."
  ss$(RESOURCES_DIR) = ss$(INSTALL_DIR) + "\resources"
'!endif
'!uncomment_if TARGET_CMM1
'  ss$(INSTALL_DIR) = "\zmim"
'  ss$(RESOURCES_DIR) = ss$(INSTALL_DIR) + "\resour~1"
'!endif
  ss$(SAVE_DIR)      = ss$(INSTALL_DIR) + "\saves"
  ss$(SCRIPT_DIR)    = ss$(INSTALL_DIR) + "\scripts"
  ss$(STORY_DIR)     = ss$(INSTALL_DIR) + "\stories"

  Cls

  cecho(ss$(RESOURCES_DIR) + "\title.txt")

  de_init()
'!comment_if NO_DEBUG
  For i = 0 To 9 : bp(i) = -1 : Next i
'!endif

  ' Select a story file
  cout("Select a story file from '" + ss$(STORY_DIR) + "':") : endl()
  Do
    s$ = fi_choose$(ss$(STORY_DIR), "*.z3")
  Loop Until s$ <> ""
  s$ = Mid$(s$, Len(ss$(STORY_DIR)) + 2)
  ss$(STORY_FILE) = Left$(s$, Len(s$) - 3)

  ' Ensure subdirectories for the current story exist in "saves\" and "scripts\"
  old_dir$ = Cwd$
  ChDir(ss$(SAVE_DIR))
  s$ = Dir$(ss$(STORY_FILE), File) : If s$ <> "" Then Error "Unexpected file: " + s$
  s$ = Dir$(ss$(STORY_FILE), Dir) : If s$ = "" Then MkDir(ss$(STORY_FILE))
  ChDir(ss$(SCRIPT_DIR))
  s$ = Dir$(ss$(STORY_FILE), File) : If s$ <> "" Then Error "Unexpected file:" + s$
  s$ = Dir$(ss$(STORY_FILE), Dir) : If s$ = "" Then MkDir(ss$(STORY_FILE))
  ChDir(old_dir$)

  main_init()

'  If LCase$(cin$("Start in debugger [Y|n] ")) <> "n" Then
'     state = E_BREAK
'  Else
'     state = E_OK
'  EndIf

  s$ = ss$(SCRIPT_DIR) + "\" + ss$(STORY_FILE) + "\" + script_file_name$()
  If LCase$(cin$("Write script to '" + s$ + "' [y|N] ")) = "y" Then
    Open s$ For Output As #2
    script = S_WRITE
  EndIf

  ' This will clear the console, see console#endl
'  For i = 0 To 10 : endl() : Next i
  endl()

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

'!uncomment_if TARGET_CMM1
'    ' On the CMM1 we display a spinning cursor to indicate instruction execution
'    If cn_spin Then Print Chr$(8); Else cn_spin = 1
'    Print Mid$("\\\\||||////----", (num_ops Mod 16) + 1, 1);
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
'!uncomment_if USING_VIRTUAL_MEMORY
'  cout("Num page faults            = " + Str$(pf)) : endl()
'!endif

  If script And S_WRITE Then Close #2
  If script And S_READ Then Close #3

End Sub

main()
End
