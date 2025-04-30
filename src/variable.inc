' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Dim GLOBAL_VAR ' base address for global variables

' Gets variable 'i'.
' If i = 0 then pops and returns the top value of the stack.
Function vget(i)
  If i = 0 Then
    vget = st_pop()
  ElseIf i < &h10 Then
    vget = st_peek(fp + i + 4)
  ElseIf i <= &hFF Then
    vget = rw(GLOBAL_VAR + 2 * (i - &h10))
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Function

' Sets variable 'i'.
' If i = 0 then pushes the value onto the stack.
Sub vset(i, v)
  If i = 0 Then
    st_push(v)
  ElseIf i < &h10 Then
    st_poke(fp + i + 4, v)
  ElseIf i <= &hFF Then
    ww(GLOBAL_VAR + 2 * (i - &h10), v)
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Sub
