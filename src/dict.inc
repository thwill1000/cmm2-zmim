' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Sub di_init()
  Dim DICT_AD = rw(&h08)                      ' base address of dictionary
  Local ns = rb(DICT_AD)                      ' number of word separators
  Dim DICT_ENTRY_LEN = rb(DICT_AD + ns + 1)   ' entry length
  Dim DICT_ENTRY_COUNT = rw(DICT_AD + ns + 2) ' number of entries
  Dim DICT_ENTRY_AD = DICT_AD + ns + 4        ' base address for the entries
End Sub

' Lookup w$ in the vocabulary. w$ should already be in lower-case
Function di_lookup(w$)

  ' Convert first 6 characters of w$ into an array of Z-char 'z'
  Local c, i, j, nz, sz, z(9)
  sz = Len(w$) : If sz > 6 Then sz = 6
  i = 1 ' the index into w$ that is being processed
  Do While i < 7 And nz < 7
    If i > sz Then
      z(nz) = 5 : nz = nz + 1 ' pad with Z-char 5
    Else
      c = Asc(Mid$(w$, i, 1))
      j = Instr(ALPHABET$(0), Chr$(c)) - 1
      If j > -1 Then
        ' Character is in Alphabet 0
        z(nz) = j : nz = nz + 1
      Else
        ' Because we know w$ is in lower-case we never have to search Alphabet 1
        ' which contains only upper-case A-Z
        j = Instr(ALPHABET$(2), Chr$(c)) - 1
        If j > -1 Then
          ' Character is in Alphabet 2
          z(nz) = 5 : nz = nz + 1 ' shift into Alphabet 2
          z(nz) = j : nz = nz + 1
        Else
          ' Encode character in ZSCII
          z(nz) = 5 : nz = nz + 1 ' shift into Alphabet 2
          z(nz) = 6 : nz = nz + 1 ' indicate ZSCII encoding
          z(nz) = c \ 32 : nz = nz + 1 ' top 5 bits
          z(nz) = c And &b11111 : nz = nz + 1 ' bottom 5 bits
        EndIf
      EndIf
    EndIf
    i = i + 1
  Loop

  ' Copy the first 6 Z-chars into 2 x 16-bit words x(0) and x(1)
  Local x(1)
  x(0) = z(0) * 1024 + z(1) * 32 + z(2)
  x(1) = z(3) * 1024 + z(4) * 32 + z(5) + 32768 ' set bit-15 at end of word

  ' Binary search to lookup the Z-string in the dictionary
  Local a, lb, ub, y(1)
  lb = 0
  ub = DICT_ENTRY_COUNT - 1
  Do
    i = (lb + ub) \ 2
    a = DICT_ENTRY_AD + DICT_ENTRY_LEN * i
    y(0) = rw(a)
    y(1) = rw(a + 2)
    If x(0) > y(0) Then
      lb = i + 1
    ElseIf x(0) < y(0) Then
      ub = i - 1
    ElseIf x(1) > y(1) Then
      lb = i + 1
    ElseIf x(1) < y(1) Then
      ub = i - 1
    Else
      di_lookup = a
      ub = lb - 1
    EndIf
  Loop Until ub < lb

End Function
