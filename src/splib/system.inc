' Copyright (c) 2020-2024 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>

'!info defined CMM2
'!info defined GAMEMITE
'!info defined MMB4L
'!info defined MMB4W
'!info defined PICOMITE
'!info defined PICOMITEVGA

'!if defined(CMM2)
  '!define PLATFORM_DEFINED
'!endif

'!if defined(GAMEMITE)
  '!if defined(PLATFORM_DEFINED)
    '!error "Multiple platforms defined"
  '!else
    '!define PLATFORM_DEFINED
  '!endif
'!endif

'!if defined(MMB4L)
  '!if defined(PLATFORM_DEFINED)
    '!error "Multiple platforms defined"
  '!else
    '!define PLATFORM_DEFINED
  '!endif
'!endif

'!if defined(MMB4W)
  '!if defined(PLATFORM_DEFINED)
    '!error "Multiple platforms defined"
  '!else
    '!define PLATFORM_DEFINED
  '!endif
'!endif

'!if defined(PICOMITE)
  '!if defined(PLATFORM_DEFINED)
    '!error "Multiple platforms defined"
  '!else
    '!define PLATFORM_DEFINED
  '!endif
'!endif

'!if defined(PICOMITEVGA)
  '!if defined(PLATFORM_DEFINED)
    '!error "Multiple platforms defined"
  '!else
    '!define PLATFORM_DEFINED
  '!endif
'!endif

On Error Skip
xyz_not_declared% = 1
If Not Mm.ErrNo Then Error "OPTION EXPLICIT is required by splib"
On Error Clear

Const sys.VERSION = 103300 ' 1.3.0
Const sys.NO_DATA$ = Chr$(&h7F)
Const sys.CRLF$ = Chr$(13) + Chr$(10)
'!if defined(GAMEMITE)
  '!uncomment_if true
  ' Const sys.FIRMWARE = Int(1000000 * Mm.Info(Version))
  '!endif
'!else
If Mm.Info$(Device X) = "MMB4L" Then
  Const sys.FIRMWARE = Mm.Info(Version)
Else
  Const sys.FIRMWARE = Int(1000000 * Mm.Info(Version))
EndIf
'!endif

Const sys.SUCCESS = 0
Const sys.FAILURE = -1

Dim sys.break_flag%
Dim sys.err$

'!if !defined(NO_INCLUDE_GUARDS)
Const sys.MAX_INCLUDES% = 30
Dim sys.includes$(sys.MAX_INCLUDES%) Length 20

Sub sys.provides(f$)
  Local f_$ = LCase$(f$)
  Local i% = 1
  Do
    Select Case sys.includes$(i%)
      Case f_$ : sys.err$ = "file already included: " + f_$ + ".inc" : Exit Sub
      Case ""  : sys.includes$(i%) = f_$ : Exit Sub
    End Select
    Inc i%
    If i% > sys.MAX_INCLUDES% Then sys.err$ = "too many includes" : Exit Sub
  Loop
End Sub

Sub sys.requires(f1$, f2$, f3$, f4$, f5$, f6$, f7$, f8$, f9$, f10$)

  ' Use indexes from 1..10 even when Option Base 0.
  Local f$(10) Length 20
  f$(1) = f1$ : f$(2) = f2$ : f$(3) = f3$ : f$(4) = f4$ : f$(5) = f5$
  f$(6) = f6$ : f$(7) = f7$ : f$(8) = f8$ : f$(9) = f9$ : f$(10) = f10$
  Local i%
  For i% = 1 To 10 : f$(i%) = LCase$(f$(i%)) : Next

  Local j%, ok%, fail%
  For i% = 1 To 10
    If f$(i%) <> "" Then
      ok% = 0
      For j% = 1 To sys.MAX_INCLUDES%
        Select Case sys.includes$(j%)
          Case f$(i%) : ok% = 1 : Exit For
          Case ""     : Exit For
        End Select
      Next
      If Not ok% Then
        If Not fail% Then
          sys.err$ = "required file(s) not included: " + f$(i%) + ".inc"
          fail% = 1
        Else
          Cat sys.err$, ", " + f$(i%) + ".inc"
        EndIf
      EndIf
    EndIf
  Next
End Sub
'!endif

' Reads property from config (.ini) file.
'
' @param  key$      case-insensitive key for property to lookup.
' @param  default$  value to return if property or file is not present.
' @param  file$     file to read. If empty then read "A:/.spconfig", or
'                   if that is not present "A:/.config".
Function sys.get_config$(key$, default$, file$)
  sys.get_config$ = default$
  If file$ = "" Then
    Const file_$ = Choice(Mm.Info(Exists File "A:/.spconfig"), "A:/.spconfig", "A:/.config")
  Else
    Const file_$ = file$
  EndIf
  If Not Mm.Info(Exists File file_$) Then Exit Function

  Local key_$ = LCase$(key$), s$, v$
  Open file_$ For Input As #1
  Do While Not Eof(#1)
    Line Input #1, s$
    If LCase$(Field$(Field$(s$, 1, "=", Chr$(34)),1, "#;", Chr$(34))) = key_$ Then
      v$ = Field$(Field$(s$, 2, "=", Chr$(34)), 1, "#;", Chr$(34))
      If Left$(v$, 1) = Chr$(34) Then v$ = Mid$(v$, 2)
      If Right$(v$, 1) = Chr$(34) Then v$ = Mid$(v$, 1, Len(v$) - 1)
      sys.get_config$ = v$
      Exit Do
    EndIf
  Loop
  Close #1
End Function

' Formats an integer version as a string.
'
' @param  v%   version number: AABBCDD
'              - AA is the 2-digit major version.
'              - BB is the 2-digit minor version.
'              - C  = 0 for alpha
'                   = 1 for beta
'                   = 2 for release candidate
'                   = 3..9 for release.
'              - DD is the micro version if c <= 3.
'                If c > 3 Then CDD - 300 is the micro version.
Function sys.format_version$(v%)
  Const v_% = Choice(v%, v%, sys.VERSION)
  Local s$ = Str$(v_%\10^5) + "." + Str$((v_% Mod 10^5) \ 10^3)
  Select Case v_% Mod 1000
    Case < 100 : Cat s$, " alpha " + Str$(v_% Mod 1000)
    Case < 200 : Cat s$, " beta " + Str$((v_% Mod 1000) - 100)
    Case < 300 : Cat s$, " RC " + Str$((v_% Mod 1000) - 200)
    Case Else  : Cat s$, "." + Str$((v_% Mod 1000) - 300)
  End Select
  sys.format_version$ = s$
End Function

' Formats a firmware integer version as a string.
Function sys.format_firmware_version$(v%)
  Const v_% = Choice(v%, v%, sys.FIRMWARE)
  If Mm.Info(Device X) = "MMB4L" Then
    sys.format_firmware_version$ = sys.format_version$(v_% \ 10000)
    If v_% Mod 10000 Then Cat sys.format_firmware_version$, " build " + Str$(v_% Mod 10000)
  Else
    Const a% = v_%\10^6, b% = (v_ Mod 10^6)\10^4, c% = (v_% Mod 10^4)\100
    sys.format_firmware_version$ = Str$(a%) + "." + Str$(b%,2,0,"0") + "." + Str$(c%,2,0,"0")
    Cat sys.format_firmware_version$, Choice(v_% Mod 100, "b" + Str$(v_% Mod 100), "")
  EndIf
End Function

' Overrides Ctrl-C behaviour such that:
'   - Ctrl-C will call sys.break_handler()
'   - Ctrl-D will perform an actual MMBasic break
Sub sys.override_break(callback$)
  sys.break_flag% = 0
  Option Break 4
  If Len(callback$) Then
    Execute "On Key 3, " + callback$ + "()"
  Else
    On Key 3, sys.break_handler()
  EndIf
End Sub

' Called as an ON KEY interrupt when Ctrl-C is overridden by sys.override_break().
' Increments the sys.break_flag%, if the flag is then > 1 then END the program.
Sub sys.break_handler()
  Inc sys.break_flag%
  If sys.break_flag% > 1 Then
    sys.restore_break()
    End
  EndIf
End Sub

' Restores default Ctrl-C behaviour.
Sub sys.restore_break()
  sys.break_flag% = 0
  On Key 3, 0
  Option Break 3
End Sub

'!if defined(REAL_GAMEMITE)
  '!replace { sys.HOME$ ( ) } { "A:" }
'!else
Function sys.HOME$()
  Select Case Mm.Info$(Device X)
    Case "MMB4L"
      sys.HOME$ = Mm.Info$(EnvVar "HOME")
    Case "MMBasic for Windows"
      sys.HOME$ = Mm.Info$(EnvVar "HOMEDIR") + Mm.Info$(EnvVar "HOMEPATH")
    Case Else
      sys.HOME$ = "A:"
  End Select
End Function
'!endif

'!if defined(GAMEMITE)
  '!replace { sys.PLATFORM$ ( ) } { "Game*Mite" }
'!else
Function sys.PLATFORM$()
  sys.PLATFORM$ = Choice(InStr(Mm.Device$, "PicoMite"), Mm.Info$(Platform), "")
  If sys.PLATFORM$ = "" Then sys.PLATFORM$ = Mm.Device$

  ' Allow platform to be overridden for unit-tests, but not if there is already
  ' an error because the use of ON ERROR would discard the existing error.
  If Mm.ErrNo Then Exit Function
  On Error Skip
  sys.PLATFORM$ = sys.OVERRIDE_PLATFORM$
  On Error Clear
End Function
'!endif

'!if defined(GAMEMITE)
  '!replace { sys.SEPARATOR$ ( ) } { "/" }
'!else
Function sys.SEPARATOR$()
  sys.SEPARATOR$ = Choice(Mm.Device$ = "MMBasic for Windows", "\", "/")
End Function
'!endif

'!if defined(REAL_GAMEMITE)
  '!replace { sys.TMPDIR$ ( ) } { "A:/tmp" }
'!else
Function sys.TMPDIR$()
  Select Case Mm.Info$(Device X)
    Case "MMB4L"
      sys.TMPDIR$ = Choice(Mm.Info$(EnvVar "TMPDIR") = "", "/tmp", Mm.Info$(EnvVar "TMPDIR"))
    Case "MMBasic for Windows"
      sys.TMPDIR$ = Mm.Info$(EnvVar "TMP")
    Case Else
      sys.TMPDIR$ = "A:/tmp"
  End Select
End Function
'!endif

' Are we running on one of the given devices ?
'
' @param  d1$, d2$, ... d5$  one of:
'                mmb4l    - MMB4L (aka MMBasic for Linux)
'                mmb4w    - MMBasic for Windows
'                cmm2     - Colour Maximite 2 (G1)
'                cmm2g2   - Colour Maximite 2 (G2)
'                cmm2*    - any Colour Maximite 2
'                gamemite - Game*Mite
'                pgvga    - PicoGAME VGA
'                pm       - PicoMite
'                pmvga    - PicoMiteVGA
'                pmvga*   - PicoMiteVGA or PicoGAME VGA
'                pm*      - PicoMite, PicoMiteVGA, PicoGAME VGA and Game*Mite
Function sys.is_platform%(d1$, d2$, d3$, d4$, d5$)
  Const platform$ = LCase$(sys.PLATFORM$())
  Local devices$(5 + Mm.Info(Option Base)) Length 16 = (d1$, d2$, d3$, d4$, d5$, "")
  Local ii% = Mm.Info(Option Base), match$

  sys.is_platform% = 1
  Do While Len(devices$(ii%)) > 0
    If platform$ = LCase$(devices$(ii%)) Then Exit Function
    Select Case platform$
      Case "colour maximite 2" : match$ = "|cmm2|cmm2*|"
      Case "colour maximite 2 g2" : match$ = "|cmm2g2|cmm2*"
      Case "game*mite" : match$ = "|pm*|gamemite|"
      Case "mmb4l"
        If LCase$(devices$(ii%)) = "mmb4l-armv6l" Then
          If Mm.Info$(Arch) = "Linux armv6l" Then Exit Function
        EndIf
        match$ = "|mmb4l|"
      Case "mmbasic for windows" : match$ = "|mmb4w|"
      Case "picogame vga" : match$ = "|pgvga|pmvga*|pm*|"
      Case "picomite" : match$ = "|pm|pm*|"
      Case "picomitevga": match$ = "|pmvga|pmvga*|pm*|"
      Case Else : Error "Unknown platform: " + sys.PLATFORM$()
    End Select
    If InStr(match$, "|" + LCase$(devices$(ii%)) + "|") Then Exit Function
    Inc ii%
  Loop
  sys.is_platform% = 0
End Function

Function sys.error%(code%, msg$)
  If Not code% Then Error "Invalid error code"
  sys.error% = code%
  sys.err$ = msg$
End Function

' Reads the '.mmbasic-shell' file.
'
' @param  exist%  If 1 then ERROR if file does not exist, otherwise return empty string.
' @param  fnbr%   File number to use for accessing the file, if unspecified/0 then uses #9.
' @return         The contents of the first line of the file.
Function sys.read_shell_file$(exist%, fnbr%)
  Const f$ = sys.HOME$() + "/.mmbasic-shell"
  If Not exist% And Not Mm.Info(Exists f$) Then Exit Function
  Const _fnbr% = Choice(fnbr%, fnbr%, 9)
  Open f$ For Input As _fnbr%
  Line Input #_fnbr%, sys.read_shell_file$
  Close _fnbr%
End Function

' Writes the '.mmbasic-shell' file for later reading by sys.run_shell().
'
' @param  s$     Contents to write to the file, if empty then writes path of current program.
' @param  fnbr%  File number to use for accessing the file, if unspecified/0 then uses #9.
Sub sys.write_shell_file(s$, fnbr%)
  Const f$ = sys.HOME$() + "/.mmbasic-shell"
  Const _s$ = Choice(s$ = "", Mm.Info(Current), s$)
  Const _fnbr% = Choice(fnbr%, fnbr%, 9)
  If sys.read_shell_file$(0, _fnbr%) <> _s$ Then
    ' Only write if content has changed.
    Open f$ For Output As _fnbr%
    Print #_fnbr%, _s$
    Close _fnbr%
  EndIf
End Sub

' Runs the program specified by the '.mmbasic-shell' file.
'
' @param  fnbr%  File number to use for accessing the file, if unspecified/0 then uses #9.
Sub sys.run_shell(fnbr%)
  Const prog$ = sys.read_shell_file$(1, fnbr%)
  If prog$ <> "" Then Run prog$
End Sub
