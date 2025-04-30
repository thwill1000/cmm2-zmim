' Copyright (c) 2020-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.provides("vt100")
If sys.err$ <> "" Then Error sys.err$
'!endif

Function vt100$(s$)
  vt100$ = Chr$(27) + "[" + s$
End Function

Function vt100.colour$(c$)
  Select Case LCase$(c$)
    Case "black"   : vt100.colour$ = vt100$("30m")
    Case "red"     : vt100.colour$ = vt100$("31m")
    Case "green"   : vt100.colour$ = vt100$("32m")
    Case "yellow"  : vt100.colour$ = vt100$("33m")
    Case "blue"    : vt100.colour$ = vt100$("34m")
    Case "magenta", "purple" : vt100.colour$ = vt100$("35m")
    Case "cyan"    : vt100.colour$ = vt100$("36m")
    Case "white"   : vt100.colour$ = vt100$("37m")
    Case "reset"   : vt100.colour$ = vt100$("0m")
    Case Else      : Error "Unknown VT100 colour: " + c$
  End Select
End Function

