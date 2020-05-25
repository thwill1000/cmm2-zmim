' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Prints zmachine header from start of 'mem'
Sub dmp_hdr
  Local x
  Print "Version      =";  rb(&h00)
  Print "Flags1       = &b"; lpad$(Bin$(rb(&h01)), 8, "0")
  Print "Release      =";  rw(&h02)
  Print "Base high    = &h"; lpad$(Hex$(rw(&h04)), 4, "0")
  Print "Initial PC   = &h"; lpad$(Hex$(rw(&h06)), 4, "0")
  Print "Dictionary   = &h"; lpad$(Hex$(rw(&h08)), 4, "0")
  Print "Object tbl   = &h"; lpad$(Hex$(rw(&h0A)), 4, "0")
  Print "Glob var tbl = &h"; lpad$(Hex$(rw(&h0C)), 4, "0")
  Print "Base static  = &h"; lpad$(Hex$(rw(&h0E)), 4, "0")
  Print "Flags2       = &b"; lpad$(Bin$(rb(&h10)), 8, "0")
  Print "Serial num   = ";
  Print Chr$(rb(&h12));
  Print Chr$(rb(&h13));
  Print Chr$(rb(&h14));
  Print Chr$(rb(&h15));
  Print Chr$(rb(&h16));
  Print Chr$(rb(&h17))
  Print "Abbrv tbl    = &h"; lpad$(Hex$(rw(&h18)), 4, "0")
  x = 2 * rw(&h1A)
  Print "File length  ="; x; " &h" + lpad$(Hex$(x), 4, "0")
  Print "File chksum  ="; rw(&h1C)
  Print "Std revision ="; rw(&h32)
End Sub
