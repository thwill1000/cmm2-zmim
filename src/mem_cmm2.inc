' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Storage for z-machine memory
Dim m(128 * 1024 / 8 + 16)
Dim mad = Peek(VarAddr m()) + 8

' Reads a byte from 'pc' and increments 'pc'
Function rp()
  If pc >= FILE_LEN Then
    Error "PC " + Str$(pc) + " > file length " + Str$(FILE_LEN)
  EndIf
  rp = Peek(Byte mad + pc)
  pc = pc + 1
End Function

' Reads a byte from 'a' but DOES NOT increment a
Function rb(a)
  If a >= FILE_LEN Then
    Error "Address " + Str$(a) + " > file length " + Str$(FILE_LEN)
  EndIf
  rb = Peek(Byte mad + a)
End Function

' Reads a 16-bit word from 'a' but DOES NOT increment a
Function rw(a)
  If a >= FILE_LEN - 1 Then
    Error "Address " + Str$(a) + " > file length " + Str$(FILE_LEN)
  EndIf
  rw = Peek(Byte mad + a) * 256 + Peek(Byte mad + a + 1)
End Function

' Writes byte 'x' to 'a'
Sub wb(a, x)
  If a >= BASE_STATIC Then
    Error "Address for write is invalid: &h" + lpad$(Hex$(a), 4, "0")
  EndIf
  If x < 0 Or x > 255 Then Error "Value for write is invalid: " + Str$(x)
  Poke Byte mad + a, x
End Sub

' Writes 16-bit word 'x' to 'a'
Sub ww(a, x)
  If a >= BASE_STATIC - 1 Then
    Error "Address for write is invalid: &h" + lpad$(Hex$(a), 4, "0")
  EndIf
  If x < 0 Or x > 65536 Then Error "Value for write is invalid: " + Str$(x)
  Poke Byte mad + a, x \ 256
  Poke Byte mad + a + 1, x Mod 256
End Sub

Sub mem_init(f$)
  Local ad, i, j, s$, sz

  cout("Loading '" + f$ + "'") : endl()
  cout("0% ... ")

  sz = mm.info(filesize f$)
  Open f$ For Input As #1
  j = 1
  Do
    s$ = Input$(255, #1)
    If Len(s$) = 0 Then Exit Do
    LongString Append m(), s$
    ad = ad + Len(s$)
    If ad > j * sz \ 10 Then cout(Str$(j * 10) + "% ... ") : j = j + 1
  Loop
  cout(" 100%") : endl()
  cout("Read " + Str$(ad) + " bytes") : endl()

  Close #1

  ' Read header data.
  Dim pc = Peek(Byte mad + &h06) * 256 + Peek(Byte mad + &h07)
  Dim GLOBAL_VAR = Peek(Byte mad + &h0C) * 256 + Peek(Byte mad + &h0D)
  Dim BASE_STATIC = Peek(Byte mad + &h0E) * 256 + Peek(Byte mad + &h0F)
  Dim FILE_LEN = (Peek(Byte mad + &h1A) * 256 + Peek(Byte mad + &h1B)) * 2

  If FILE_LEN > ad Then Error "Story file is truncated"
End Sub
