' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

' Lower memory, but lower performance, implementation of 'stack.inc'.

Dim stack(511 \ 4)  ' storage for the stack  - we only store 16-bit unsigned
                    ' integers 0 .. &hFFFF although they are often interpreted as
                    ' signed.
Const stack.base = Peek(VarAddr stack())
Dim sp              ' stack pointer; points to the *next* element in the stack.

' Pops a 16-bit word from the stack.
Function st_pop()
  Inc sp, -1
  st_pop = Peek(Short stack.base + sp * 2)
End Function

' Pushes a 16-bit word to the stack.
Sub st_push(x)
'!if !defined(NO_EXTRA_CHECKS)
  If x < 0 Or x > &hFFFF Then Error "Invalid unsigned 16-bit value"
'!endif
  Poke Short stack.base + sp * 2, x
  Inc sp
End Sub

' Peeks at a value in the stack.
Function st_peek(i)
'!if !defined(NO_EXTRA_CHECKS)
  If i >= sp Then Error "Attempt to peek beyond stack pointer"
'!endif
  st_peek = Peek(Short stack.base + i * 2)
End Function

' Pokes a value into the stack.
Sub st_poke(i, x)
'!if !defined(NO_EXTRA_CHECKS)
  If i >= sp Then Error "Attempt to poke beyond stack pointer"
  If x < 0 Or x > &hFFFF Then Error "Invalid unsigned 16-bit value"
'!endif
  Poke Short stack.base + i * 2, x
End Sub
