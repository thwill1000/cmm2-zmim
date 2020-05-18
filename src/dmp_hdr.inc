' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Prints zmachine header from start of 'mem'
Sub dmp_hdr
  Local i, serial$

  Print "Version      =";  rb(&h00)
  Print "Flags1       = "; Bin$(rb(&h01))
  Print "Release      =";  rw(&h02)
  Print "Base high    = "; Hex$(rw(&h04))
  Print "Initial PC   = "; Hex$(rw(&h06))
  Print "Dictionary   = "; Hex$(rw(&h08))
  Print "Object tbl   = "; Hex$(rw(&h0A))
  Print "Glob var tbl = "; Hex$(rw(&h0C))
  Print "Base static  = "; Hex$(rw(&h0E))
  Print "Flags2       = "; Bin$(rb(&h10))
  serial$ = "000000"
  For i = 0 To 5
    Poke Var serial$, i + 1, rb(&H12 + i)
  Next i
  Print "Serial num   = "; serial$
  Print "Abbrv tbl    = "; Hex$(rw(&h18))
  Print "File length  ="; 2 * rw(&h1A)
  Print "File chksum  ="; rw(&h1C)
  Print "Std revision ="; rw(&h32)
End Sub
