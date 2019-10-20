' Scratch pad for storing currently unused code fragments
' Copyright (c) 2019 Thomas Hugo Williams
' For Maximite BASIC v4.5C

' Storage for the stack (512 x 16 bit words)
' This allocates enough memory for 256 x 32-bit floats = 1K
Dim stack(255)

' Stack pointer, a byte offset into the memory behind 'stack'
Let sp = -1

Sub push(word)
  sp = sp + 1
  Poke Var stack(0), sp, word \ 256
  sp = sp + 1
  Poke Var stack(0), sp, word Mod 256
End Sub

Function pop()
  pop = Peek(Var stack(0), sp)
  sp = sp - 1
  pop = pop + Peek(Var stack(0), sp) * 256
  sp = sp - 1
End Function

' Storage for the stack (512 x 16 bit words)
' Storing each 16-bit word as a 32-bit float
Dim stack(511)

' Stack pointer, a simple index into 'stack'
Let sp = -1

Sub push(word)
  sp = sp + 1
  stack(sp) = word
End Sub

Function pop()
  pop = stack(sp)
  sp = sp - 1
End Function
