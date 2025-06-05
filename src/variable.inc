' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Dim GLOBAL_VAR ' base address for global variables

' Gets variable 'i'.
' If i = 0 then pops and returns the top value of the stack.
Function vget(i)
  Select Case i
    Case 0
      vget = st_pop()
    Case < &h10
      vget = st_peek(fp + i + 4)
    Case <= &hFF
      vget = rw(GLOBAL_VAR + 2 * (i - &h10))
    Case Else
      Error "Unknown variable " + Str$(i)
  End Select
End Function

' Sets variable 'i'.
' If i = 0 then pushes the value onto the stack.
Sub vset(i, v)
  Select Case i
    Case 0
      st_push(v)
    Case < &h10
      st_poke(fp + i + 4, v)
    Case <= &hFF
      ww(GLOBAL_VAR + 2 * (i - &h10), v)
    Case Else
      Error "Unknown variable " + Str$(i)
  End Select
End Sub
