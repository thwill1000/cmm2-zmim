' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Storage for z-machine memory
Dim m(128 * 1024 / 8 + 16)
Dim MAD = Peek(VarAddr m()) + 8
Dim FILE_LEN
Dim BASE_STATIC

' Reads a byte from 'pc' and increments 'pc'
Function rp()
  rp = LGetByte(m(), pc)
  pc = pc + 1
End Function

' Reads a byte from 'a' but DOES NOT increment a
Function rb(a)
  rb = LGetByte(m(), a)
End Function

' Reads a 16-bit word from 'a' but DOES NOT increment a
Function rw(a)
  rw = LGetByte(m(), a) * 256 + LGetByte(m(), a + 1)
End Function

' Writes byte 'x' to 'a'
Sub wb(a, x)
  Poke Byte MAD + a, x
End Sub

' Writes 16-bit word 'x' to 'a'
Sub ww(a, x)
  Poke Byte MAD + a, x \ 256
  Poke Byte MAD + a + 1, x Mod 256
End Sub

Sub mem_init(f$)
  Local ad, i, j, s$, sz

  LongString Clear m()

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
  cout("100%") : endl()
  cout("Read " + Str$(ad) + " bytes") : endl()

  Close #1

  ' Read memory sizes from header
  BASE_STATIC = rw(&h0E)
  FILE_LEN = rw(&h1A) * 2

  If FILE_LEN > ad Then Error "Story file is truncated"
End Sub
