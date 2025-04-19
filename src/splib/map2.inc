' Copyright (c) 2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

' A string => string map that stores each key and value as a single string array
' element key$ + value$ with the key padded with spaces to a fixed length.
' The capacity of the map is the capacity of the array - 1, with the final element
' storing "${key_len},${size}".

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.requires("array")
sys.provides("map2")
If sys.err$ <> "" Then Error sys.err$
'!endif

' Gets the upper-bound that should be used to dimension an array to hold a map
' of the given capacity, irrespective of OPTION BASE.
'
' e.g. To create a string array that can be used as a map of 10 elements:
'        Dim my_map$(map2.new%(10))
Function map2.new%(capacity%)
  map2.new% = capacity% + Mm.Info(Option Base)
End Function

' Initialises the map.
Sub map2.init(mp$(), key_len%)
  If key_len% <= 0 Then Error "Invalid key length, must be > 0" : Exit Sub
  Const lb% = Bound(mp$(), 0), ub% = Bound(mp$(), 1)
  Local i%
  For i% = lb% To ub% : mp$(i%) = sys.NO_DATA$ : Next
  mp$(ub%) = Str$(key_len%) + ",0"
End Sub

' Gets the value corresponding to a key, or sys.NO_DATA$ if the key is not present.
Function map2.get$(mp$(), k$)
  Const idx% = map2.get_key_index%(mp$(), k$)
  If idx% >= 0 Then
    Const ub% = Bound(mp$(), 1), key_len% = Val(Field$(mp$(ub%), 1))
    map2.get$ = Mid$(mp$(idx%), key_len% + 1)
  Else
    map2.get$ = sys.NO_DATA$
  EndIf
End Function

' Gets the index of a key in the map, or -1 if the key is not present.
Function map2.get_key_index%(mp$(), k$)
  Local i%, lb% = Bound(mp$(), 0), ub% = Bound(mp$(), 1)
  Const key_len% = Val(Field$(mp$(ub%), 1)), key$ = k$ + Space$(key_len% - Len(k$))

  Inc ub%, -1
  Do While ub% >= lb%
    i% = (lb% + ub%) \ 2
    If key$ > Left$(mp$(i%), key_len%) Then
      lb% = i% + 1
    ElseIf key$ < Left$(mp$(i%), key_len%) Then
      ub% = i% - 1
    Else
      Exit Do
    EndIf
  Loop

  map2.get_key_index% = Choice(ub% >= lb%, i%, -1)
End Function

' Adds a key/value pair to the map.
Sub map2.put(mp$(), k$, v$)
  Const lb% = Bound(mp$(), 0), ub% = Bound(mp$(), 1)
  Const key_len% = Val(Field$(mp$(ub%), 1))
  Local sz% = Val(Field$(mp$(ub%), 2))
  If Len(k$) > key_len% Then Error "Key too long" : Exit Sub
  Local idx% = map2.get_key_index%(mp$(), k$)
  If idx% = -1 Then
    If sz% = ub% - lb% Then Error "Map full" : Exit Sub
    idx% = lb% + sz%
    Inc sz%
  EndIf
  mp$(idx%) = k$ + Space$(key_len% - Len(k$)) + v$
  mp$(ub%) = Str$(key_len%) + "," + Str$(sz%)
  Sort mp$(), , , lb%, sz%
End Sub

' Adds a key/value pair to the map if the key is not already present
'
' @param  mp$  the map/array.
' @param  k$   the key.
' @param  v$   the value.
' @return      if the key is already present then the existing value,
'              otherwise v$.
Function map2.put_if_absent$(mp$(), k$, v$)
  map2.put_if_absent$ = map2.get$(mp$(), k$)
  If map2.put_if_absent$ = sys.NO_DATA$ Then
    map2.put(mp$(), k$, v$)
    map2.put_if_absent$ = v$
  EndIf
End Function

' Removes a key/value pair from the map.
Sub map2.remove(mp$(), k$)
  Const idx% = map2.get_key_index%(mp$(), k$)
  If idx% >= 0 Then
    mp$(idx%) = sys.NO_DATA$
    Const lb% = Bound(mp$(), 0), ub% = Bound(mp$(), 1)
    Const key_len% = Val(Field$(mp$(ub%), 1)), sz% = Val(Field$(mp$(ub%), 2))
    mp$(ub%) = Str$(key_len%) + "," + Str$(sz% - 1)
    Sort mp$(), , , lb%, sz%
  EndIf
End Sub

' Gets the number of key/value pairs in the map. */
Function map2.size%(mp$())
  Const ub% = Bound(mp$(), 1)
  map2.size% = Val(Field$(mp$(ub%), 2))
End Function

Function map2.is_full%(mp$())
  Const lb% = Bound(mp$(), 0), ub% = Bound(mp$(), 1), sz% = Val(Field$(mp$(ub%), 2))
  map2.is_full% = (ub% - lb% - sz%) = 0
End Function

' Gets the maximum capacity of the map.
Function map2.capacity%(mp$())
  Const lb% = Bound(mp$(), 0), ub% = Bound(mp$(), 1)
  map2.capacity% = ub% - lb%
End Function

' Removes all the key/value pairs from the map.
Sub map2.clear(mp$())
  Const lb% = Bound(mp$(), 0), ub% = Bound(mp$(), 1), key_len% = Val(Field$(mp$(ub%), 1))
  Local i%
  For i% = lb% To ub%
    mp$(i%) = sys.NO_DATA$
  Next
  mp$(ub%) = Str$(key_len%) + ",0"
End Sub

' Prints the contents of the map.
Sub map2.dump(mp$(), fnbr%)
  Const lb% = Bound(mp$(), 0), ub% = Bound(mp$(), 1), sz% = Val(Field$(mp$(ub%), 2))
  Const width% = Len(Str$(sz%))
  Local i%
  For i% = lb% To lb% + sz% - 1
    Print #fnbr%, "[" Str$(i%, width%) "] ";
    Print #fnbr%, mp$(i%)
  Next
  Print #fnbr%, "END"
End Sub
