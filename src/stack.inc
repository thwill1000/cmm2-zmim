' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Storage for the stack  - we only store 16-bit unsigned integers
' 0 .. &hFFFF although they are often interpreted as signed.
Dim stack(511)

' Stack pointer; points to the *next* element in the stack.
Dim sp = 0

' Frame pointer; points to the stack element that is the first element
' for the current routine. This has the value &hFFFF if we are not in
' a routine.
Dim fp = &hFFFF

' Pops a 16-bit word from the stack.
Function st_pop()
  sp = sp - 1
  st_pop = stack(sp)
End Function

' Pushes a 16-bit word to the stack.
Sub st_push(x)
  stack(sp) = x
  sp = sp + 1
End Sub

' Peeks at a value in the stack.
Function st_peek(i)
  If i >= sp Then Error "Attempt to peek beyond stack pointer"
  st_peek = stack(i)
End Function

' Pokes a value into the stack.
Sub st_poke(i, x)
  If i >= sp Then Error "Attempt to poke beyond stack pointer"
  If x < 0 Or x > &hFFFF Then Error "Invalid unsigned 16-bit value"
  stack(i) = x
End Sub
