' Copyright (c) 2022-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>

' MMBasic Controller Library

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
On Error Clear
sys.provides("ctrl")
If sys.err$ <> "" Then Error sys.err$
'!endif

' Button values as returned by controller driver subroutines.
Const ctrl.R      = &h01
Const ctrl.START  = &h02
Const ctrl.HOME   = &h04
Const ctrl.SELECT = &h08
Const ctrl.L      = &h10
Const ctrl.DOWN   = &h20
Const ctrl.RIGHT  = &h40
Const ctrl.UP     = &h80
Const ctrl.LEFT   = &h100
Const ctrl.ZR     = &h200
Const ctrl.X      = &h400
Const ctrl.A      = &h800
Const ctrl.Y      = &h1000
Const ctrl.B      = &h2000
Const ctrl.ZL     = &h4000

Const ctrl.OPEN  = -1
Const ctrl.CLOSE = -2
Const ctrl.SOFT_CLOSE = -3

' The NES standard specifies a 12 micro-second pulse, but all the controllers
' I've tested work with 1 micro-second, and possibly less.
Const ctrl.PULSE = 0.001 ' 1 micro-second

' Suggested default delay between actions when using keys to navigate UI.
Const ctrl.UI_DELAY = 200 ' 200 micro-seconds.

' Comma separated list of OPEN controller drivers that should automatically
' be CLOSEd when ctrl.term() is called. If a driver does not need closing
' (i.e. keyboard) then it does not need to be added to this list.
Dim ctrl.driver_list$

' Mechanism to use for reading the keyboard.
'   0 = INKEY$
'   1 = KEYDOWN
'   2 = ON PS2
Dim ctrl.key_type%

' When a key is down the corresponding byte of this 256-byte map is set,
' when the key is up then it is unset.
'
' Note that when using INKEY$ (as opposed to the CMM2 'KEYDOWN' function or
' the PicoMiteVGA 'ON PS2' command) to read the keyboard we cannot detect
' keyup events and instead automatically clear a byte after it is read.
Dim ctrl.key_map%(31 + Mm.Info(Option Base))

' Map used to convert PS/2 set 2 scan codes to entries in ctrl.key_map%().
' The scan code first has to be converted into a single byte value,
' see ctrl.on_ps2().
'!if !defined(GAMEMITE)
If InStr(Mm.Device$, "PicoMiteVGA") Then Dim ctrl.scan_map%(31)
'!endif

' Timer number configured for reading the KEYDOWN state on the CMM2.
'!if !defined(GAMEMITE)
If InStr(Mm.Device$, "Colour Maximite 2") Then Dim ctrl.tick_nbr%
If Mm.Device$ = "MMBasic for Windows"  Then Dim ctrl.tick_nbr%
'!endif

' Gets a default controller driver based on the current platform.
Function ctrl.default_driver$()
  If sys.PLATFORM$() = "Game*Mite" Then
    ctrl.default_driver$ = "ctrl.gamemite"
  ElseIf InStr(Mm.Device$, "PicoMiteVGA") Then
    ctrl.default_driver$ = "nes_a"
  ElseIf InStr(Mm.Device$, "Colour Maximite 2") Then
    ctrl.default_driver$ = "wii_classic_3"
  ElseIf Mm.Device$ = "MMB4L" Then
    ctrl.default_driver$ = "mmb4l_gamepad_1"
  Else
    ctrl.default_driver$ = "keys_cursor_ext"
  EndIf
End Function

' Opens a controller driver without throwing an ERROR on failure.
'
' @param  ctrl$  The controller driver.
' @return        0 on success, otherwise failure.
Function ctrl.open_no_error%(ctrl$)
  On Error Ignore
  Call ctrl$, ctrl.OPEN
  If Mm.ErrNo Then ctrl.open_no_error% = sys.FAILURE
  On Error Abort
End Function

' Initialises keyboard reading.
'
' @param  use_inkey%  Use INKEY$ even on platforms with KEYDOWN or ON PS2.
' @param  period%     CMM2 only, interval to read KEYDOWN state, default 40 ms.
' @param  nbr%        CMM2 only, timer nbr to read KEYDOWN state, default 4.
Sub ctrl.init_keys(use_inkey%, period%, nbr%)
  ctrl.term_keys()
'!if defined(GAMEMITE)
  '!uncomment_if true
  ' ctrl.key_type% = 0 ' INKEY$
  ' On Key ctrl.on_key()
  '!endif
'!else
  Select Case Choice(use_inkey%, "Inkey", Mm.Device$)
    Case "Colour Maximite 2", "Colour Maximite 2 G2", "MMBasic for Windows"
      ctrl.key_type% = 1 ' KEYDOWN
      ctrl.tick_nbr% = Choice(nbr% = 0, 4, nbr%)
      SetTick Choice(period% = 0, 40, period%), ctrl.on_tick(), ctrl.tick_nbr%
    Case "PicoMiteVGA"
      ctrl.key_type% = 2 ' ON PS 2
      Read Save
      Restore ctrl.scan_map_data
      Local i%
      For i% = Bound(ctrl.scan_map%(), 0) To Bound(ctrl.scan_map%(), 1)
        Read ctrl.scan_map%(i%)
      Next
      Read Restore
      On Ps2 ctrl.on_ps2()
    Case Else
      ctrl.key_type% = 0 ' INKEY$
      On Key ctrl.on_key()
  End Select
'!endif
End Sub

' TODO: use the 'lower-case' character for all keys, not just letters.
Sub ctrl.on_key()
  Poke Var ctrl.key_map%(), Asc(LCase$(Inkey$)), 1
End Sub

'!if !defined(GAMEMITE)

Sub ctrl.on_ps2()
  Local ps2% = Mm.Info(PS2)
  Select Case ps2%
    Case < &hE000 : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), ps2% And &hFF), 1
    Case < &hF000 : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), (ps2% And &hFF) + &h80), 1
    Case < &hE0F000 : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), ps2% And &hFF), 0
    Case Else : Poke Var ctrl.key_map%(), Peek(Var ctrl.scan_map%(), (ps2% And &hFF) + &h80), 0
  End Select
End Sub

' Note there is little point in calling KeyDown(0) to determine the number of
' keys that are down, hardware limitations mean it's unlikely ever to be > 4
' and if a given key isn't down it just returns 0 so we harmlessly set that
' byte in the key map.
Sub ctrl.on_tick()
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
  Poke Var ctrl.key_map%(), KeyDown(1), 1
  Poke Var ctrl.key_map%(), KeyDown(2), 1
  Poke Var ctrl.key_map%(), KeyDown(3), 1
  Poke Var ctrl.key_map%(), KeyDown(4), 1
End Sub

'!endif ' !defined(GAMEMITE)

Sub ctrl.term()
  ctrl.term_keys()
  On Error Ignore
  Do While Len(ctrl.driver_list$)
    Call Field$(ctrl.driver_list$, 1), ctrl.CLOSE
  Loop
  On Error Abort
End Sub

' Terminates keyboard reading.
Sub ctrl.term_keys()
'!if defined(GAMEMITE)
  '!uncomment_if true
  ' On Key 0
  '!endif
'!else
  Select Case ctrl.key_type%
    Case 0 ' INKEY$
      On Key 0
    Case 1 ' KEYDOWN
      If ctrl.tick_nbr% <> 0 Then SetTick 0, 0, ctrl.tick_nbr%
    Case 2 ' ON PS2
      On Ps2 0
    Case Else
      Error "Invalid state"
  End Select
'!endif
  Memory Set Peek(VarAddr ctrl.key_map%()), 0, 256
  Do While Inkey$ <> "" : Loop
End Sub

Function ctrl.keydown%(i%)
  ctrl.keydown% = Peek(Var ctrl.key_map%(), i%)
  If ctrl.key_type% = 0 Then Poke Var ctrl.key_map%(), i%, 0
End Function

' Iterates through array of controller drivers$(), opening, polling and closing
' each in turn waiting for the user to provide an input that matches a mask%.
'
' @param[in]      drivers$()  controller drivers to poll.
' @param[in]      mask%       bit mask to match against.
' @param[in,out]  duration%   on entry the maximum duration to poll,
'                             or 0 to poll indefinitely.
'                             on exit the remaining duration,
'                             which will be 0 if it was 0 on entry.
' @param[out]     key%        matching bits, otherwise 0.
' @return                     entry from drivers$() or empty string if no match.
Function ctrl.poll_multiple$(drivers$(), mask%, duration%, key%)
  Local expires% = Choice(duration%, Timer + duration%, &h7FFFFFFFFFFFFFFF), i%
  Do
    For i% = Bound(drivers$(), 0) To Bound(drivers$(), 1)
      key% = ctrl.poll_single%(drivers$(i%), mask%)
      If key% Then ctrl.poll_multiple$ = drivers$(i%) : Exit Do
    Next
  Loop While Timer < expires%
  If duration% Then duration% = Max(0, expires% - Timer)
End Function

' Opens, polls (for a maximum of 5ms) and closes a controller.
'
' @param  driver$  controller driver function.
' @param  mask%    bit mask to match against.
' @return          matching bits, otherwise 0.
Function ctrl.poll_single%(driver$, mask%)
  If Mm.Info(Device X) = "MMB4L" And Left$(driver$, 5) = "atari" Then Exit Function ' Unsupported
  On Error Ignore
  Call driver$, ctrl.OPEN
  If Mm.ErrNo = 0 Then
    Local key%, t% = Timer + 5
    Do
      Call driver$, key%
      ctrl.poll_single% = key% And mask%
      If ctrl.poll_single% Then
        ' Wait for user to release key.
        Do While key% : Pause 5 : Call driver$, key% : Loop
        Exit Do
      EndIf
    Loop While Timer < t%
    Call driver$, ctrl.SOFT_CLOSE
  EndIf
  On Error Abort
End Function

' Gets a string representation of bits read from a controller driver.
'
' @param  x%  bits returned by driver.
' @return     string representation.
Function ctrl.bits_to_string$(x%)
  Static BUTTONS$(14) = ("R","Start","Home","Select","L","Down","Right","Up","Left","ZR","X","A","Y","B","ZL")

  If x% = 0 Then
    ctrl.bits_to_string$ = "No buttons down"
    Exit Function
  EndIf

  ctrl.bits_to_string$ = Str$(x%) + " = "
  Local count%, i%, s$
  For i% = 0 To Bound(BUTTONS$(), 1)
    If x% And 2^i% Then
      s$ = BUTTONS$(i%)
      If count% > 0 Then Cat ctrl.bits_to_string$, ", "
      Cat ctrl.bits_to_string$, s$
      Inc count%
    EndIf
  Next
End Function

' Waits until specified controllers are all idle.
Sub ctrl.wait_until_idle(d1$, d2$, d3$, d4$, d5$)
  Local k%
  Do
    Call d1$, k%
    If Not k% Then If Len(d2$) Then Call d2$, k%
    If Not k% Then If Len(d3$) Then Call d3$, k%
    If Not k% Then If Len(d4$) Then Call d4$, k%
    If Not k% Then If Len(d5$) Then Call d5$, k%
    If Not k% Then Exit Do
    Pause 5
  Loop
End Sub

' Reads the keyboard as if it were a controller.
Sub keys_cursor(x%)
  If x% < 0 Then Exit Sub
  x% =    ctrl.keydown%(32)  * ctrl.A ' Space
  Inc x%, ctrl.keydown%(128) * ctrl.UP
  Inc x%, ctrl.keydown%(129) * ctrl.DOWN
  Inc x%, ctrl.keydown%(130) * ctrl.LEFT
  Inc x%, ctrl.keydown%(131) * ctrl.RIGHT
End Sub

Sub keys_cursor_ext(x%)
  If x% < 0 Then Exit Sub
  x% = (ctrl.keydown%(32) Or ctrl.keydown%(120)) * ctrl.A ' Space or X
  Inc x%, ctrl.keydown%(122) * ctrl.B ' Z
  Inc x%, (ctrl.keydown%(&hA) Or ctrl.keydown%(97))  * ctrl.SELECT ' Enter or A
  Inc x%, ctrl.keydown%(115) * ctrl.START ' S
  Inc x%, ctrl.keydown%(128) * ctrl.UP
  Inc x%, ctrl.keydown%(129) * ctrl.DOWN
  Inc x%, ctrl.keydown%(130) * ctrl.LEFT
  Inc x%, ctrl.keydown%(131) * ctrl.RIGHT
  Inc x%, ctrl.keydown%(27)  * ctrl.HOME ' Escape
End Sub

Sub ctrl.add_driver(d$)
  If Mm.ErrNo Then Exit Sub ' Do not record driver opening if failed.
  Cat ctrl.driver_list$, d$ + ","
End Sub

Sub ctrl.remove_driver(d$)
  Local idx% = InStr(ctrl.driver_list$, d$)
  Select Case idx%
    Case 0
      ' d$ not present, ignore.
    Case 1
      ctrl.driver_list$ = Mid$(ctrl.driver_list$, Len(d$) + 2)
    Case Else
      ctrl.driver_list$ = Mid$(ctrl.driver_list$, 1, idx% - 1) + Mid$(ctrl.driver_list$, idx% + Len(d$) + 1)
  End Select
End Sub

' Driver subroutine for "No controller".
'!dynamic_call ctrl.no_controller
Sub ctrl.no_controller(x%)
  x% = 0
End Sub

' Atari joystick on PicoGAME Port A.
Sub atari_a(x%)
  Select Case x%
    Case >= 0
      x% =    Not Pin(GP14) * ctrl.A
      Inc x%, Not Pin(GP0)  * ctrl.UP
      Inc x%, Not Pin(GP1)  * ctrl.DOWN
      Inc x%, Not Pin(GP2)  * ctrl.LEFT
      Inc x%, Not Pin(GP3)  * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP0, DIn : SetPin GP1, DIn : SetPin GP2, DIn : SetPin GP3, DIn : SetPin GP14, DIn
      ctrl.add_driver("atari_a")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP0, Off : SetPin GP1, Off : SetPin GP2, Off : SetPin GP3, Off : SetPin GP14, Off
      ctrl.remove_driver("atari_a")
  End Select
End Sub

' Atari joystick on PicoGAME Port B.
Sub atari_b(x%)
  Select Case x%
    Case >= 0
      x% =    Not Pin(GP15) * ctrl.A
      Inc x%, Not Pin(GP28) * ctrl.UP
      Inc x%, Not Pin(GP4)  * ctrl.DOWN
      Inc x%, Not Pin(GP5)  * ctrl.LEFT
      Inc x%, Not Pin(GP22) * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin GP4, DIn : SetPin GP5, DIn : SetPin GP15, DIn : SetPin GP22, DIn : SetPin GP28, DIn
      ctrl.add_driver("atari_b")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP4, Off : SetPin GP5, Off : SetPin GP15, Off : SetPin GP22, Off : SetPin GP28, Off
      ctrl.remove_driver("atari_b")
    End Select
End Sub

' SNES gamepad on PicoGAME Port A.
'
'   GP2: Latch, GP3: Clock, GP1: Data
Sub snes_a(x%)
  Select Case x%
    Case >= 0
      Pulse GP2, ctrl.PULSE
      x% =    Not Pin(GP1) * ctrl.B      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.Y      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.SELECT : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.START  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.UP     : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.DOWN   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.LEFT   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.RIGHT  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.A      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.X      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.L      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.R      : Pulse GP3, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      If Mm.Info(Device X) = "MMB4L" And InStr(ctrl.driver_list$, "snes_a") Then Exit Sub
      SetPin GP1, Din : SetPin GP2, Dout : SetPin GP3, Dout
      Pin(GP2) = 0 : Pin(GP3) = 0
      snes_a(0) ' Discard the first reading.
      ctrl.add_driver("snes_a")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      If Mm.Info(Device X) = "MMB4L" And x% = ctrl.SOFT_CLOSE Then Exit Sub
      SetPin GP1, Off : SetPin GP2, Off : SetPin GP3, Off
      ctrl.remove_driver("snes_a")
  End Select
End Sub

' SNES gamepad on PicoGAME Port B.
'
'   GP5: Latch, GP22: Clock, GP4: Data
Sub snes_b(x%)
  Select Case x%
    Case >= 0
      Pulse GP5, ctrl.PULSE
      x% =    Not Pin(GP4) * ctrl.B      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.Y      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.SELECT : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.START  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.UP     : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.DOWN   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.LEFT   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.RIGHT  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.A      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.X      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.L      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.R      : Pulse GP22, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      If Mm.Info(Device X) = "MMB4L" And InStr(ctrl.driver_list$, "snes_b") Then Exit Sub
      SetPin GP4, Din : SetPin GP5, Dout : SetPin GP22, Dout
      Pin(GP5) = 0 : Pin(GP22) = 0
      snes_b(0) ' Discard the first reading.
      ctrl.add_driver("snes_b")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      If Mm.Info(Device X) = "MMB4L" And x% = ctrl.SOFT_CLOSE Then Exit Sub
      SetPin GP4, Off : SetPin GP5, Off : SetPin GP22, Off
      ctrl.remove_driver("snes_b")
  End Select
End Sub

' Reads port A connected to a NES gamepad.
'
' Note that the extra pulse after reading bit 7 (Right) should not be necessary,
' but in practice some NES clone controllers require it to behave correctly.
'
'   GP2: Latch, GP3: Clock, GP1: Data
Sub nes_a(x%)
  If Mm.Info(Device X) = "MMB4L" Then snes_a(x%) : Exit Sub
  Select Case x%
    Case >= 0
      Pulse GP2, ctrl.PULSE
      x% =    Not Pin(GP1) * ctrl.A      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.B      : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.SELECT : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.START  : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.UP     : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.DOWN   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.LEFT   : Pulse GP3, ctrl.PULSE
      Inc x%, Not Pin(GP1) * ctrl.RIGHT  : Pulse GP3, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin GP1, Din : SetPin GP2, Dout : SetPin GP3, Dout
      Pin(GP2) = 0 : Pin(GP3) = 0
      nes_a(0) ' Discard the first reading.
      ctrl.add_driver("nes_a")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP1, Off : SetPin GP2, Off : SetPin GP3, Off
      ctrl.remove_driver("nes_a")
  End Select
End Sub

' NES gamepad on PicoGAME Port B.
'
'   GP5: Latch, GP22: Clock, GP4: Data
Sub nes_b(x%)
  If Mm.Info(Device X) = "MMB4L" Then snes_b(x%) : Exit Sub
  Select Case x%
    Case >= 0
      Pulse GP5, ctrl.PULSE
      x% =    Not Pin(GP4) * ctrl.A      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.B      : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.SELECT : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.START  : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.UP     : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.DOWN   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.LEFT   : Pulse GP22, ctrl.PULSE
      Inc x%, Not Pin(GP4) * ctrl.RIGHT  : Pulse GP22, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin GP4, Din : SetPin GP5, Dout : SetPin GP22, Dout
      Pin(GP5) = 0 : Pin(GP22) = 0
      nes_b(0) ' Discard the first reading.
      ctrl.add_driver("nes_b")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin GP4, Off : SetPin GP5, Off : SetPin GP22, Off
      ctrl.remove_driver("nes_b")
  End Select
End Sub

' Gamepad on the Game*Mite
' The single 'crazy' PORT call efficiently reads the buttons in the correct
' order for the return bitmap but we then have to mask out filler values.
Sub ctrl.gamemite(x%)
  Select Case x%
    Case >= 0
      x% = Port(GP12,2,GP11,2,GP8,1,GP8,1,GP11,1,GP10,1,GP9,1,GP13,3,GP13,3)
      x% = (x% Xor &h7FFF) And &h29EA
      Exit Sub
    Case ctrl.OPEN
      Local i%
      For i% = 8 To 15 : SetPin Mm.Info(PinNo "GP" + Str$(i%)), Din, PullUp : Next
      ctrl.add_driver("ctrl.gamemite")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      Local i%
      For i% = 8 To 15 : SetPin Mm.Info(PinNo "GP" + Str$(i%)), Off : Next
      ctrl.remove_driver("ctrl.gamemite")
  End Select
End Sub

' Atari joystick port on CMM2 Deluxe G2.
Sub atari_dx(x%)
  Select Case x%
    Case >= 0
      x% =    Not Pin(32) * ctrl.A
      Inc x%, Not Pin(33) * ctrl.B
      Inc x%, Not Pin(35) * ctrl.UP
      Inc x%, Not Pin(36) * ctrl.DOWN
      Inc x%, Not Pin(38) * ctrl.LEFT
      Inc x%, Not Pin(40) * ctrl.RIGHT
      Exit Sub
    Case ctrl.OPEN
      SetPin 32, Din, PullUp : SetPin 33, Din, PullUp : SetPin 35, Din, PullUp
      SetPin 36, Din, PullUp : SetPin 38, Din, PullUp : SetPin 40, Din, PullUp
      ctrl.add_driver("atari_dx")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin 32, Off : SetPin 33, Off : SetPin 35, Off
      SetPin 36, Off : SetPin 38, Off : SetPin 40, Off
      ctrl.remove_driver("atari_dx")
  End Select
End Sub

' NES gamepad attached USING ADAPTER to Atari joystick port on CMM2 Deluxe G2.
'
' IMPORTANT! the adapter is required to swap the Male DB9 (CMM2) +5V supply on
' Pin 7 to Pin 6 on the Female DB9 (Gamepad).
'
'   Pin 38: Latch, Pin 40: Clock, Pin 36: Data
Sub nes_dx(x%)
  Select Case x%
    Case >= 0
      Pulse 38, ctrl.PULSE
      x% =    Not Pin(36) * ctrl.A      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.B      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.SELECT : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.START  : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.UP     : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.DOWN   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.LEFT   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.RIGHT  : Pulse 40, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin 36, Din, PullUp : SetPin 38, Dout : SetPin 40, Dout
      Pin(38) = 0 : Pin(40) = 0
      nes_dx(0) ' Discard the first reading.
      ctrl.add_driver("nes_dx")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin 36, Off : SetPin 38, Off : SetPin 40, Off
      ctrl.remove_driver("nes_dx")
  End Select
End Sub

' SNES gamepad attached USING ADAPTER to Atari joystick port on CMM2 Deluxe G2.
'
' IMPORTANT! the adapter is required to swap the Male DB9 (CMM2) +5V supply on
' Pin 7 to Pin 6 on the Female DB9 (Gamepad).
'
'   Pin 38: Latch, Pin 40: Clock, Pin 36: Data
Sub snes_dx(x%)
  Select Case x%
    Case >= 0
      Pulse 38, ctrl.PULSE
      x% =    Not Pin(36) * ctrl.B      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.Y      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.SELECT : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.START  : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.UP     : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.DOWN   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.LEFT   : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.RIGHT  : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.A      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.X      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.L      : Pulse 40, ctrl.PULSE
      Inc x%, Not Pin(36) * ctrl.R      : Pulse 40, ctrl.PULSE
      Exit Sub
    Case ctrl.OPEN
      SetPin 36, Din, PullUp : SetPin 38, Dout : SetPin 40, Dout
      Pin(38) = 0 : Pin(40) = 0
      snes_dx(0) ' Discard the first reading.
      ctrl.add_driver("snes_dx")
    Case ctrl.CLOSE, ctrl.SOFT_CLOSE
      SetPin 36, Off : SetPin 38, Off : SetPin 40, Off
      ctrl.remove_driver("snes_dx")
  End Select
End Sub

Sub wii_internal(i2c%, x%, type%)
  Static is_open%(3)
  If is_open%(i2c%) = -1 Then Error "I2C" + Str$(i2c%) + " failed to open" : Exit Sub

  If x% >= 0 Then
    Select Case is_open%(i2c%)
      Case &hA4200101
        x% = Classic(B, i2c%)
'        Inc x%, (Classic(LY, i2c%) > 200) * ctrl.UP
'        Inc x%, (Classic(LY, i2c%) < 60)  * ctrl.DOWN
'        Inc x%, (Classic(LX, i2c%) < 60)  * ctrl.LEFT
'        Inc x%, (Classic(LX, i2c%) > 200) * ctrl.RIGHT
        If x% = &h7FFF Then x% = 0 ' Ignore this glitch.
      Case &hA4200000
        x% =    Nunchuk(Z,  i2c%) * ctrl.A
        Inc x%, Nunchuk(C,  i2c%) * ctrl.B
        Inc x%, (Nunchuk(JY, i2c%) > 200) * ctrl.UP
        Inc x%, (Nunchuk(JY, i2c%) < 60)  * ctrl.DOWN
        Inc x%, (Nunchuk(JX, i2c%) < 60)  * ctrl.LEFT
        Inc x%, (Nunchuk(JX, i2c%) > 200) * ctrl.RIGHT
    End Select
    Exit Sub
  EndIf

  Select Case x%
    Case ctrl.SOFT_CLOSE
      ' Do nothing
      Exit Sub

    Case ctrl.CLOSE
      Select Case is_open%(i2c%)
        Case &hA4200000
          Controller Nunchuk Close i2c%
        Case &hA4200101
          Controller Classic Close i2c%
      End Select
      is_open%(i2c%) = 0
      ctrl.remove_driver("wii_any_" + Str$(i2c%))
      Exit Sub

    Case ctrl.OPEN
      If is_open%(i2c%) Then Exit Sub
      is_open%(i2c%) = -1
      Controller Nunchuk Open i2c%
      If Mm.ErrNo Then
        If InStr(Mm.ErrMsg$, "already OPEN") Then
          Error "I2C" + Str$(i2c%) + " already open"
        Else
          Error "I2C" + Str$(i2c%) + " not connected"
        EndIf
        Exit Sub
      EndIf
      is_open%(i2c%) = Nunchuk(T, i2c%)
      Select Case is_open%(i2c%)
        Case &hA4200000
          If Not(type% And &h01) Then
            Controller Nunchuk Close i2c%
            is_open%(i2c%) = -1
            Error "Nunchuk controller on I2C" + Str$(i2c%) + " not supported"
          EndIf
        Case &hA4200101
          Controller Nunchuk Close i2c%
          If Not(type% And &h10) Then
            is_open%(i2c%) = -1
            Error "Classic controller on I2C" + Str$(i2c%) + " not supported"
          Else
            Controller Classic Open i2c%
          EndIf
        Case Else
          Controller Nunchuck Close i2c%
          is_open%(i2c%) = -1
          Error "Unrecognised controller on I2C" + Str$(i2c%)
      End Select
      ctrl.add_driver("wii_any_" + Str$(i2c%))
  End Select
End Sub

' Wii Nunchuk OR Classic gamepad on I2C1.
Sub wii_any_1(x%)
  wii_internal(1, x%, &h11)
End Sub

' Wii Nunchuk OR Classic gamepad on I2C2.
Sub wii_any_2(x%)
  wii_internal(2, x%, &h11)
End Sub

' Wii Nunchuk OR Classic gamepad on I2C3.
Sub wii_any_3(x%)
  wii_internal(3, x%, &h11)
End Sub

' Wii Classic gamepad on I2C1.
Sub wii_classic_1(x%)
  wii_internal(1, x%, &h10)
End Sub

' Wii Classic gamepad on I2C2.
Sub wii_classic_2(x%)
  wii_internal(2, x%, &h10)
End Sub

' Wii Classic gamepad on I2C3.
Sub wii_classic_3(x%)
  wii_internal(3, x%, &h10)
End Sub

' Wii Nunchuk on I2C1.
Sub wii_nunchuk_1(x%)
  wii_internal(1, x%, &h01)
End Sub

' Wii Nunchuk on I2C2.
Sub wii_nunchuk_2(x%)
  wii_internal(2, x%, &h01)
End Sub

' Wii Nunchuk on I2C3.
Sub wii_nunchuk_3(x%)
  wii_internal(3, x%, &h01)
End Sub

Sub wii_classic_pm(x%)
  Select Case x%
    Case >= 0
      x% = Device(Wii b)
      Exit Sub

    Case ctrl.SOFT_CLOSE
      ' Do nothing

    Case ctrl.CLOSE
      Device Wii Close
      ctrl.remove_driver("wii_classic_pm")

    Case ctrl.OPEN
      Device Wii Open
      ctrl.add_driver("wii_classic_pm")
  End Select
End Sub

Sub wii_nunchuk_pm(x%)
  Select Case x%
    Case >= 0
      Const xa%, ya%, za%, c%, z%
      read_val(xa%, ya%, za%, c%, z%)
      x% =    z% * ctrl.A
      Inc x%, c% * ctrl.B
      Inc x%, ya% > 200 * ctrl.UP
      Inc x%, ya% < 60  * ctrl.DOWN
      Inc x%, xa% < 60  * ctrl.LEFT
      Inc x%, xa% > 200 * ctrl.RIGHT
      Exit Sub

    Case ctrl.SOFT_CLOSE
      ' Do nothing

    Case ctrl.CLOSE
      ' TODO
      ctrl.remove_driver("wii_classic_pm")

    Case ctrl.OPEN
      init_no_enc()
      Const id% = read_id%()
      If id% <> &hA4200000 Then Error "Not a Nunchuk controller"
      ctrl.add_driver("wii_nunchuk_pm")
  End Select
End Sub

Sub init_no_enc()
  I2C Write &h52, 0, 2, &h40, &h00
  Pause 10
  I2C Write &h52, 0, 2, &hF0, &h55
  I2C Write &h52, 0, 2, &hFB, &h00
End Sub
 
Function read_id%()
  Local x%(5)
  I2C Write &h52, 0, 1, &hFA
  I2C Read &h52, 0, 6, x%()
  read_id% = ((x(2)*256+x(3))*256+x(4))*256+x(5)
End Function
 
Sub read_val(xa%, ya%, za%, c%, z%)
  Local x%(5)
  I2C Write &h52, 0, 1, &h00
  Pause 1 'needed for nunchuk response....
  I2C Read &h52, 0, 6, x%()
  xa% = 4*x%(2) + ((x%(5) And &b11000000) >> 6)
  ya% = 4*x%(3) + ((x%(5) And &b00110000) >> 4)
  za% = 4*x%(4) + ((x%(5) And &b00001100) >> 2)
  c% = (x%(5) And &b00000010) >> 1
  z% = x%(5) And &b00000001
End Sub

Sub mmb4l_gamepad_internal(id%, x%)
  If x% >= 0 Then
    x% = Device(Gamepad id%, B)
    Exit Sub
  EndIf

  Select Case x%
    Case ctrl.SOFT_CLOSE
      Device Gamepad Close id%
      ctrl.remove_driver("mmb4l_gamepad_" + Str$(id%))
      Exit Sub

    Case ctrl.CLOSE
      Device Gamepad Close id%
      ctrl.remove_driver("mmb4l_gamepad_" + Str$(id%))
      Exit Sub

    Case ctrl.OPEN
      Device Gamepad Open id%
      ctrl.add_driver("mmb4l_gamepad_" + Str$(id%))
      Exit Sub
  End Select
End Sub

' USB Gamepad 1 on MMB4L
Sub mmb4l_gamepad_1(x%)
  mmb4l_gamepad_internal(1, x%)
End Sub

' USB Gamepad 2 on MMB4L
Sub mmb4l_gamepad_2(x%)
  mmb4l_gamepad_internal(2, x%)
End Sub

' USB Gamepad 3 on MMB4L
Sub mmb4l_gamepad_3(x%)
  mmb4l_gamepad_internal(3, x%)
End Sub

' USB Gamepad 4 on MMB4L
Sub mmb4l_gamepad_4(x%)
  mmb4l_gamepad_internal(4, x%)
End Sub

'!if !defined(GAMEMITE)

ctrl.scan_map_data:

Data &h9C92919395009900, &h0060099496989A00, &h0031710000008B00, &h00327761737A0000
Data &h0033346564786300, &h0035727466762000, &h0036796768626E00, &h003837756A6D0000
Data &h0039306F696B2C00, &h002D703B6C2F2E00, &h00003D5B00270000, &h000023005D0A0000
Data &h0008000000005C00, &h0000003734003100, &h001B383635322E30, &h008C392A2D332B9B
Data &h0000000097000000, &h0000000000000000, &h0000000000008B00, &h0000000000000000
Data &h0000000000000000, &h0000000000000000, &h0000000000000000, &h0000000000000000
Data &h0000000000000000, &h00000000002F0000, &h0000000000000000, &h00000000000A0000
Data &h0000000000000000, &h0000008682008700, &h0000808300817F84, &h0000889D00890000

'!endif ' !defined(GAMEMITE)
