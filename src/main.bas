' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Option Base 0
Option Default Integer
Option Explicit On

'!if defined(LOW_MEMORY)
  '!replace "memory.inc" "memory_virtual.inc"
  '!replace "stack.inc" "stack_compact.inc"
'!endif

#Include "splib/system.inc"
#Include "splib/file.inc"
#Include "splib/string.inc"
#Include "splib/vt100.inc"
#Include "memory.inc"

'!if defined(LOW_MEMORY)
  '!replace { rw ( ad ) }              { rb(ad) * 256 + rb(ad + 1) }
  '!replace { wb ( ad , x ) }          { Poke Var m(0), ad, x }
  '!replace { wb ( ad , y ) }          { Poke Var m(0), ad, y }
  '!replace { ww ( ad , x ) }          { Poke Var m(0), ad, x \ 256 : Poke Var m(0), ad + 1, x Mod 256 }
'!else
  '!replace { rp ( ) * 256 + rp ( ) }  { LGetByte(m(), pc) * 256 + LGetByte(m(), pc + 1) : Inc pc, 2 }
  '!replace { rp ( ) }                 { LGetByte(m(), pc) : Inc pc }
  '!replace { rb ( ad ) }              { LGetByte(m(), ad) }
  '!replace { rb ( ad - 1 ) }          { LGetByte(m(), ad - 1) }
  '!replace { rb ( a + b ) }           { LGetByte(m(), a + b) }
  '!replace { rw ( ad ) }              { LGetByte(m(), ad) * 256 + LGetByte(m(), ad + 1) }
  '!replace { wb ( ad , x ) }          { Poke Byte mad + ad, x }
  '!replace { wb ( ad , y ) }          { Poke Byte mad + ad, y }
  '!replace { ww ( ad , x ) }          { Poke Byte mad + ad, x \ 256 : Poke Byte mad + ad + 1, x Mod 256 }
'!endif

#Include "stack.inc"

'!if defined(LOW_MEMORY)
  '!replace { st_pop ( ) }                  { Peek(Short stack.base + sp * 2 - 2) : Inc sp, -1 }
  '!replace { st_push ( oa ( 0 ) ) }        { Poke Short stack.base + sp * 2, oa(0) : Inc sp }
  '!replace { st_push ( fp ) }              { Poke Short stack.base + sp * 2, fp : Inc sp }
  '!replace { st_push ( nl ) }              { Poke Short stack.base + sp * 2, nl : Inc sp }
  '!replace { st_push ( st ) }              { Poke Short stack.base + sp * 2, st : Inc sp }
  '!replace { st_push ( v ) }               { Poke Short stack.base + sp * 2, v : Inc sp }
  '!replace { st_peek ( sp - 1 ) }          { Peek(Short stack.base + sp * 2 - 2) }
  '!replace { st_peek ( fp + i + 4 ) }      { Peek(Short stack.base + (fp + i + 4) * 2) }
  '!replace { st_poke ( fp + i + 4 , v ) }  { Poke Short stack.base + (fp + i + 4) * 2, v }
  '!replace { st_poke ( sp - 1 , b ) }      { Poke Short stack.base + sp * 2 - 2, b }
  '!replace { st_poke ( sp - 1 , x ) }      { Poke Short stack.base + sp * 2 - 2, x }
'!else
  '!replace { st_pop ( ) }                  { stack(sp - 1) : Inc sp, -1 }
  '!replace { st_push ( oa ( 0 ) ) }        { stack(sp) = oa(0) : Inc sp }
  '!replace { st_push ( fp ) }              { stack(sp) = fp : Inc sp }
  '!replace { st_push ( nl ) }              { stack(sp) = nl : Inc sp }
  '!replace { st_push ( st ) }              { stack(sp) = st : Inc sp }
  '!replace { st_push ( v ) }               { stack(sp) = v : Inc sp }
  '!replace { st_peek ( sp - 1 ) }          { stack(sp - 1) }
  '!replace { st_peek ( fp + i + 4 ) }      { stack(fp + i + 4) }
  '!replace { st_poke ( fp + i + 4 , v ) }  { stack(fp + i + 4) = v }
  '!replace { st_poke ( sp - 1 , b ) }      { stack(sp - 1) = b }
  '!replace { st_poke ( sp - 1 , x ) }      { stack(sp - 1) = x }
'!endif

#Include "variable.inc"
#Include "decode.inc"
#Include "console.inc"
#Include "script.inc"
#Include "objects.inc"
#Include "execute.inc"
#Include "zstring.inc"
#Include "util.inc"
#Include "dict.inc"
#Include "zsave.inc"
#Include "zfile.inc"
'!comment_if NO_DEBUG
#Include "debug.inc"
'!endif

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
'!comment_if NO_DEBUG
  For i = 0 To 9 : bp(i) = -1 : Next
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

  Pause 1000

  ' This will clear the console, see con.endl()
  For i = 0 To 10 : con.endl() : Next

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
      Next
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
'!uncomment_if USE_VIRTUAL_MEMORY
'  con.println("Num page faults            = " + Str$(pf))
'!endif

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
  ElseIf Mm.Device$ = "MMBasic for Windows" Or InStr(Mm.Device$, "Colour Maximite 2") Then
    Cat cmdline$, " --platform=cmm2"
  ElseIf InStr(Mm.Device$, "PicoMiteVGA") Then
    Cat cmdline$, " --platform=pmvga"
  ElseIf InStr(Mm.Device$, "PicoMiteHDMI") Then
    Cat cmdline$, " --platform=pmhdmi"
  EndIf

  If InStr(cmdline$, "--platform=picocalc") Then
    con.init(40, 26)
  ElseIf InStr(cmdline$, "--platform=cmm2") Then
    con.init(100, 50)
  ElseIf InStr(cmdline$, "--platform=pmvga") Then
    con.init(80, 40)
  ElseIf InStr(cmdline$, "--platform=pmhdmi") Then
    con.init(80, 40)
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
