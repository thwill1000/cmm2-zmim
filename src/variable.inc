' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Gets variable 'i'.
' If i = 0 then pops and returns the top value of the stack.
Function vget(i)
  If i = 0 Then
    vget = pop()
  ElseIf i < &h10 Then
    vget = stack(fp + i + 4)
  ElseIf i <= &hFF Then
    vget = rw(GLOBAL_VAR + 2 * (i - &h10))
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Function

' Sets variable 'i'.
' If i = 0 then pushes the value onto the stack.
Sub vset(i, x)
  If i = 0 Then
    push(x)
  ElseIf i < &h10 Then
    stack(fp + i + 4) = x
  ElseIf i <= &hFF Then
    ww(GLOBAL_VAR + 2 * (i - &h10), x)
  Else
    Error "Unknown variable " + Str$(i)
  EndIf
End Sub

