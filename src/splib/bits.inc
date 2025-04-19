' Copyright (c) 2021-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.provides("bits")
If sys.err$ <> "" Then Error sys.err$
'!endif

' Sets bit i% of x%.
Sub bits.set(x%, i%)
  If i% < 0 Or i% > 63 Then Error "Index out of bounds"
  x% = x% Or 1 << i%
End Sub

' Clears bit i% of x%.
Sub bits.clear(x%, i%)
  If i% < 0 Or i% > 63 Then Error "Index out of bounds"
  x% = x% And Inv (1 << i%)
End Sub

' Gets bit i% of x%.
Function bits.get%(x%, i%)
  If i% < 0 Or i% > 63 Then Error "Index out of bounds"
  bits.get% = (x% And 1 << i%) <> 0
End Function

Sub bits.fill(x%, y%)
  Select Case y%
    Case 0 : x% = 0
    Case 1 : x% = -1
    Case Else : Error "Invalid bit value"
  End Select
End Sub

Sub bits.big_set(x%(), i%)
  If i% < 0 Then Error "Index out of bounds"
  Const j% = Bound(x%(), 0) + i% \ 64
  x%(j%) = x%(j%) Or 1 << (i% Mod 64)
End Sub

Sub bits.big_clear(x%(), i%)
  If i% < 0 Then Error "Index out of bounds"
  Const j% = Bound(x%(), 0) + i% \ 64
  x%(j%) = x%(j%) And Inv (1 << (i% Mod 64))
End Sub

Function bits.big_get%(x%(), i%)
  If i% < 0 Then Error "Index out of bounds"
  Const j% = Bound(x%(), 0) + i% \ 64
  bits.big_get% = (x%(j%) And 1 << (i% Mod 64)) <> 0
End Function

Sub bits.big_fill(x%(), y%)
  Local i%, z%
  Select Case y%
    Case 0 : z% = 0
    Case 1 : z% = -1
    Case Else : Error "Invalid bit value"
  End Select
  For i% = Bound(x%(), 0) To Bound(x%(), 1) : x%(i%) = z% : Next
End Sub
