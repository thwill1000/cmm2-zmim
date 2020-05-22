' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Dim stack(511)
Dim sp = -1 ' Stack pointer
Dim fp = -1 ' Stack frame pointer

' Pops a 16-bit word from the stack.
Function pop()
  pop = stack(sp)
  sp = sp - 1
End Function

' Pushes a 16-bit word to the stack.
Sub push(w)
  sp = sp + 1
  stack(sp) = w
End Sub
