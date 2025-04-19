' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Option Base 0
Option Default Integer
Option Explicit On

If InStr(Mm.Device$, "Colour Maximite 2") Then Mode 1

#Include "splib/system.inc"
#Include "splib/vt100.inc"
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
If Mm.Info(Device X) = "MMB4L" Then
  Dim ss$(5)
Else
  Dim ss$(5) Length 20
EndIf

'!comment_if INLINE_CONSTANTS
Const INSTALL_DIR = 0
Const RESOURCES_DIR = 1
Const SAVE_DIR = 2
Const SCRIPT_DIR = 3
Const STORY_DIR = 4
Const STORY_FILE = 5
'!endif

' Gets name in format "story-DD-MM-YY-hh-mm-ss.scr"
Function script_file_name$()
  Local i, s$
  s$ = ss$(STORY_FILE) + "-" + Date$ + "-" + Time$ + ".scr"
  For i = 1 To Len(s$)
    If Peek(Var s$, i) = Asc(":") Then Poke Var s$, i, Asc("-")
  Next i
  script_file_name$ = s$
End Function

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
  For i = 0 To 511 : stack(i) = 0 : Next i
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

  Cls

  con.print_file(file.resolve$(ss$(RESOURCES_DIR), "title.txt"))

  de_init()
'!comment_if NO_DEBUG
  For i = 0 To 9 : bp(i) = -1 : Next i
'!endif

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

  s$ = file.resolve$(ss$(SCRIPT_DIR), ss$(STORY_FILE))
  s$ = file.resolve$(s$, script_file_name$())
  If LCase$(con.in$("Write script to '" + s$ + "' [y|N] ")) = "y" Then
    con.open_out(2, s$)
  EndIf

  ' This will clear the console, see console#endl
  For i = 0 To 10 : con.endl() : Next i

  Timer = 0

  Do While state <> E_QUIT
'!comment_if NO_DEBUG
    ' If there are active breakpoint and the PC has changed since we last checked
    If num_bp > 0 And pc <> old_pc Then
      For i = 0 To 9
        If pc = bp(i) Then
          con.println("[Breakpoint " + Str$(i) + " reached]")
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

  con.endl()
  con.println("Num instructions processed = " + Str$(num_ops))
  con.print("Instructions / second      = ")
  con.println(Format$(1000 * num_ops / Timer, "%.1f"))
'!uncomment_if USING_VIRTUAL_MEMORY
'  con.println("Num page faults            = " + Str$(pf))
'!endif

  con.close_out()
  con.close_in()
End Sub

main()
End

Function get_install_dir$()
  get_install_dir$ = Left$(Mm.Info(Path), Len(Mm.Info(Path)) - 1)
  If get_install_dir$ = "NON" Then get_install_dir$ = Cwd$
  If Not Mm.Info(Exists Dir file.resolve$(get_install_dir$, "resources")) Then
    get_install_dir$ = file.get_parent$(get_install_dir$)
  EndIf
End Function
