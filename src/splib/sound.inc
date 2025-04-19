' Copyright (c) 2022-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.provides("sound")
If sys.err$ <> "" Then Error sys.err$
'!endif

'!info defined SOUND_MONO
'!info defined SOUND_RAW

'!if defined(SOUND_MONO) && defined(SOUND_RAW)
  '!error "File 'sound.inc' was pre-processed with more than one of SOUND_MONO and SOUND_RAW defined"
'!endif

'!if defined SOUND_RAW
  '!dynamic_call sound.fx_raw_int
  '!dynamic_call sound.music_raw_int
'!else
  '!dynamic_call sound.fx_default_int
  '!dynamic_call sound.music_default_int
'!endif

'!uncomment_if SOUND_RAW
'SetPin GP4,PWM2A
'SetPin GP6,PWM3A
'!endif

Const sound.FX_FLAG% = &b01
Const sound.MUSIC_FLAG% = &b10

Dim sound.F!(127)
Dim sound.FX_BLART%(2)
Dim sound.FX_SELECT%(2)
Dim sound.FX_DIE%(4)
Dim sound.FX_WIPE%(4)
Dim sound.FX_READY_STEADY_GO%(13)

Dim sound.enabled%
Dim sound.fx_int$
Dim sound.fx_ptr%
Dim sound.fx_start_ptr%
Dim sound.music_done_cb$
Dim sound.music_int$
Dim sound.music_ptr%
Dim sound.music_start_ptr%
Dim sound.music_tick% = 200
Dim sound.music_volume% = 15

' Initialises sound engine.
Sub sound.init(fx_int$, music_int$)
  sound.fx_int$ = Choice(Len(fx_int$), fx_int$, "sound.fx_default_int")
  sound.music_int$ = Choice(Len(music_int$), music_int$, "sound.music_default_int")

  ' On the Game*Mite we always play an octave higher.
  Const offset% = Choice(sys.PLATFORM$() = "Game*Mite", 12, 0)
  Local i%
  For i% = 0 To 127
    ' 16.35 Hz is C0, the lowest (?) frequency allowed.
    sound.F!(i%) = Max(440 * 2^((i% - 58 + offset%) / 12.0), 16.35)
  Next

  sound.load_data("sound.blart_data", sound.FX_BLART%())
  sound.load_data("sound.select_data", sound.FX_SELECT%())
  sound.load_data("sound.die_data", sound.FX_DIE%())
  sound.load_data("sound.wipe_data", sound.FX_WIPE%())
  sound.load_data("sound.ready_steady_go_data", sound.FX_READY_STEADY_GO%())

  sound.enable(sound.FX_FLAG% Or sound.MUSIC_FLAG%)
End Sub

' Enable/disable music and FX.
'
' @param  flags%  Flags indicating which of music/FX should be enabled.
'                 Those not specified are disabled.
Sub sound.enable(flags%)
  Local raw%
'!uncomment_if SOUND_RAW
'  raw% = 1
'!endif

  If flags% And sound.MUSIC_FLAG% Then
    If Not (sound.enabled% And sound.MUSIC_FLAG%) Then
      Execute "SetTick " + Str$(sound.music_tick%) + ", " + sound.music_int$ + ", 1"
      sound.enabled% = sound.enabled% Or sound.MUSIC_FLAG%
    EndIf
  Else
    If sound.enabled% And sound.MUSIC_FLAG% Then
      sound.enabled% = sound.enabled% Xor sound.MUSIC_FLAG%
      SetTick 0, 0, 1
      If raw% Then
        Pwm 2, sound.F!(0), 0
      Else
        Play Sound 1, B, O
        Play Sound 2, B, O
        Play Sound 3, B, O
      EndIf
      sound.music_ptr% = 0
    EndIf
  EndIf

  If flags% And sound.FX_FLAG% Then
    If Not (sound.enabled% And sound.FX_FLAG%) Then
      Execute "SetTick 40, " + sound.fx_int$ + ", 2"
      sound.enabled% = sound.enabled% Or sound.FX_FLAG%
    EndIf
  Else
    If sound.enabled% And sound.FX_FLAG% Then
      sound.enabled% = sound.enabled% Xor sound.FX_FLAG%
      SetTick 0, 0, 2
      If raw% Then
        Pwm 3, sound.F!(0), 0
      Else
        Play Sound 4, B, O
      EndIf
      sound.fx_ptr% = 0
    EndIf
  EndIf
End Sub

Function sound.is_playing%(flags%)
  Select Case flags%
    Case 0
      sound.is_playing% = (sound.music_ptr% > 0) Or (sound.fx_ptr% > 0)
    Case sound.FX_FLAG%
      sound.is_playing% = sound.fx_ptr% > 0
    Case sound.MUSIC_FLAG%
      sound.is_playing% = sound.music_ptr% > 0
  End Select
End Function

' Terminates sound engine.
Sub sound.term()
  sound.enable(&h00)
'!ifndef SOUND_RAW
  Play Stop
'!endif
'!uncomment_if SOUND_RAW
  ' Pwm 2, Off
  ' Pwm 3, Off
'!endif
End Sub

' Gets the size of INTEGER array required to hold music/fx DATA.
Function sound.data_size%(data_label$)
  Local num_bytes%
  Read Save
  Restore data_label$
  Read num_bytes%
  Read Restore
  sound.data_size% = num_bytes% \ 8
  If sound.data_size% Mod 8 Then Inc sound.data_size%
  If Mm.Info(Option Base) Then Inc sound.data_size%
End Function

' Loads music/fx from DATA statements into notes%() array.
Sub sound.load_data(data_label$, notes%())
  Local i%, num_notes%, num_channels%
  Read Save
  Restore data_label$
  Read num_notes%, num_channels%
  Const max_notes% = 8 * (Bound(notes%(), 1) - Bound(notes%(), 0))
  If num_notes% > max_notes% Then
    Local err$ = "Too much DATA; "
    Error err$ + "expected " + Str$(max_notes%) + " bytes but found " + Str$(num_notes%)
  EndIf
  notes%(0) = num_channels% + 256 * num_notes%
  For i% = 1 To num_notes% \ 8 : Read notes%(i%) : Next
  Read Restore
End Sub

' Plays a music score.
Sub sound.play_music(music%(), done_cb$)
  If Not (sound.enabled% And sound.MUSIC_FLAG%) Then Exit Sub
  sound.music_start_ptr% = Peek(VarAddr music%())
  sound.music_ptr% = sound.music_start_ptr% + 8
  sound.music_done_cb$ = done_cb$
End Sub

' Interrupt routine to play next note of music using PLAY SOUND with B, S options.
Sub sound.music_default_int()
  If Not sound.music_ptr% Then Exit Sub
  Local n% = Peek(Byte sound.music_ptr%)
  If n% < 255 Then
    Play Sound 1, B, S, sound.F!(n%), (n% > 0) * sound.music_volume%
    n% = Peek(Byte sound.music_ptr% + 1)
    Play Sound 2, B, S, sound.F!(n%), (n% > 0) * sound.music_volume%
    n% = Peek(Byte sound.music_ptr% + 2)
    Play Sound 3, B, S, sound.F!(n%), (n% > 0) * sound.music_volume%
    Inc sound.music_ptr%, 3
    Exit Sub
  EndIf
  sound.music_ptr% = 0
  If Len(sound.music_done_cb$) Then Call sound.music_done_cb$
End Sub

' Interrupt routine to play next note of music using raw PWM.
Sub sound.music_raw_int()
  If Not sound.music_ptr% Then Exit Sub
  Local n% = Peek(Byte sound.music_ptr%)
  If n% < 255 Then
    If Not n% Then n% = Peek(Byte sound.music_ptr% + 1)
    If Not n% Then n% = Peek(Byte sound.music_ptr% + 2)
    Pwm 2, sound.F!(n%), (n% > 0) * 2
    Inc sound.music_ptr%, 3
    Exit Sub
  EndIf
  sound.music_ptr% = 0
  If Len(sound.music_done_cb$) Then Call sound.music_done_cb$
End Sub

' Plays a new sound effect.
Sub sound.play_fx(fx%(), block%)
  If Not (sound.enabled% And sound.FX_FLAG%) Then Exit Sub
  sound.fx_start_ptr% = Peek(VarAddr fx%())
  sound.fx_ptr% = sound.fx_start_ptr% + 8
  If block% Then Do While sound.fx_ptr% > 0 : Loop
End Sub

' Interrupt routine to play next note of sound fx using PLAY SOUND with B, S options.
Sub sound.fx_default_int()
  If Not sound.fx_ptr% Then Exit Sub
  Local n% = Peek(Byte sound.fx_ptr%)
  If n% < 255 Then
    Play Sound 4, B, S, sound.F!(n%), (n% > 0) * 25
    Inc sound.fx_ptr%
  Else
    sound.fx_ptr% = 0
  EndIf
End Sub

' Interrupt routine to play next note of sound fx using raw PWM.
Sub sound.fx_raw_int()
  If Not sound.fx_ptr% Then Exit Sub
  Local n% = Peek(Byte sound.fx_ptr%)
  If n% < 255 Then
    Pwm 3, sound.F!(n%), (n% > 0) * 5
    Inc sound.fx_ptr%
  Else
    sound.fx_ptr% = 0
  EndIf
End Sub

sound.blart_data:
Data 16 ' Number of bytes of music data.
Data 1  ' Number of channels.
Data &hFFFFFF0036373C3D, &hFFFFFFFFFFFFFFFF

sound.select_data:
Data 16 ' Number of bytes of music data.
Data 1  ' Number of channels.
Data &hFFFFFFFF0048443C, &hFFFFFFFFFFFFFFFF

sound.die_data:
Data 32 ' Number of bytes of music data.
Data 1  ' Number of channels.
Data &h4748494A4B4C4D4E, &h3F40414243444546, &h0038393A3B3C3D3E, &hFFFFFFFFFFFFFFFF

sound.wipe_data:
Data 32 ' Number of bytes of music data.
Data 1  ' Number of channels.
Data &h3F3E3D3C3B3A3938, &h4746454443424140, &h004E4D4C4B4A4948, &hFFFFFFFFFFFFFFFF

sound.ready_steady_go_data:
Data 104 ' Number of bytes of music data.
Data 1   ' Number of channels.
Data &h3C3C3C3C3C3C3C3C, &h3C3C3C3C3C3C3C3C, &h0000000000000000, &h0000000000000000
Data &h3C3C3C3C3C3C3C3C, &h3C3C3C3C3C3C3C3C, &h0000000000000000, &h0000000000000000
Data &h4848484848484848, &h4848484848484848, &h4848484848484848, &h0000000048484848
Data &hFFFFFFFFFFFFFFFF
