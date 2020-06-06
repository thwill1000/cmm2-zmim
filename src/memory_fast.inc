' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Reads a byte from 'pc' and increments 'pc'
Function rp()
  If pc >= FILE_LEN Then
    Error "PC " + Str$(pc) + " > file length " + Str$(FILE_LEN)
  EndIf
  rp = m(pc)
  pc = pc + 1
End Function

' Reads a byte from 'a' but DOES NOT increment a
Function rb(a)
  If a >= FILE_LEN Then
    Error "Address " + Str$(a) + " > file length " + Str$(FILE_LEN)
  EndIf
  rb = m(a)
End Function

' Reads a 16-bit word from 'a' but DOES NOT increment a
Function rw(a)
  If a >= FILE_LEN - 1 Then
    Error "Address " + Str$(a) + " > file length " + Str$(FILE_LEN)
  EndIf
  rw = m(a) * 256 + m(a + 1)
End Function

' Writes byte 'x' to 'a'
Sub wb(a, x)
  If a >= BASE_STATIC Then
    Error "Address for write is invalid: &h" + lpad$(Hex$(a), 4, "0")
  EndIf
  If x < 0 Or x > 255 Then Error "Value for write is invalid: " + Str$(x)
  m(a) = x
End Sub

' Writes 16-bit word 'x' to 'a'
Sub ww(a, x)
  If a >= BASE_STATIC - 1 Then
    Error "Address for write is invalid: &h" + lpad$(Hex$(a), 4, "0")
  EndIf
  If x < 0 Or x > 65536 Then Error "Value for write is invalid: " + Str$(x)
  m(a) = x \ 256
  m(a + 1) = x Mod 256
End Sub

Sub mem_init(f$)
  Local ad, i, s$, sz

  Print "Loading '"; f$; "'"

' This doesn't seem to work given an absolute path
'  sz = mm.info(filesize f$)

  ' Storage for z-machine memory.
  ' Each z-machine byte is stored as a MMBasic float (4 bytes).
  ' Terribly memory inefficient, but likely to provide the fastest memory access.
  Dim m(128 * 1024 - 1)

  Open f$ For Input As #1

  Do
    s$ = Input$(255, #1)
    If Len(s$) = 0 Then Exit Do
    For i = 1 To Len(s$)
      m(ad) = Peek(Var s$, i)
      ad = ad + 1
      If ad Mod 10000 = 0 Then Print "Read"; ad; " bytes"
    Next i
  Loop

  Close #1

  Print "Read"; ad; " bytes"

  ' Read header data.
  Dim pc = m(&h06) * 256 + m(&h07)
  Dim GLOBAL_VAR = m(&h0C) * 256 + m(&h0D)
  Dim BASE_STATIC = m(&h0E) * 256 + m(&h0F)
  Dim FILE_LEN = (m(&h1A) * 256 + m(&h1B)) * 2

  If FILE_LEN > ad Then Error "Story file is truncated"
End Sub
