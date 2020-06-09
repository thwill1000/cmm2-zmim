' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Storrage for the stack  - we only store 16-bit unsigned integers
' 0 .. &hFFFF although they are often interpreted as signed.
Dim stack(511)

' Stack pointer; points to the *next* element in the stack.
Dim sp = 0

' Frame pointer; points to the stack element that is the first element
' for the current routine. This has the value &hFFFF if we are not in
' a routine.
Dim fp = &hFFFF

' Pops a 16-bit word from the stack.
Function pop()
  sp = sp - 1
  pop = stack(sp)
End Function

' Pushes a 16-bit word to the stack.
Sub push(w)
  stack(sp) = w
  sp = sp + 1
End Sub
