' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Function fi_choose$(d$, fspec$)
  Local f$, i, j, nc, nr, old_dir$, sz, width, x

  old_dir$ = Cwd$
  ChDir d$

  ' Count number of entries 'sz' and their maximum length 'width'.
  ' Ignore files prefixed with full-stop.
  f$ = Dir$(fspec$)
  Do While f$ <> ""
    If Left$(f$, 1) <> "." Then
      sz = sz + 1
      If Len(f$) > width Then width = Len(f$)
    EndIf
    f$ = Dir$()
  Loop

  If sz = 0 Then cout("No files found") : endl() : ChDir old_dir$ : Exit Function

  ' Create sorted array of entries 'all$'.
  ' Once again ignore all files prefixed with full-stop.
  Local all$(sz - 1) LENGTH width
  f$ = Dir$(fspec$)
  i = 0
  Do
    If Left$(f$, 1) <> "." Then all$(i) = f$ : i = i + 1
    f$ = Dir$()
  Loop Until i = sz
'!comment_if CMM1
  Sort all$()
'!endif

  ChDir old_dir$

  ' Display entries in one column if < 8, otherwise 2 columns.
  If sz < 8 Then nc = 1 Else nc = 2
  nr = CInt(sz / nc + 0.4999)
  width = width + 10
  For i = 0 To nr - 1
    For j = 0 to nc - 1
      cflush()
      x = (j * nr) + i
      If x < sz Then
        If j * width > Pos Then cout(Space$(j * width - Pos))
        cout("  [" + Format$(x + 1, "%2g") + "] " + all$(x))
      EndIf
    Next j
    endl()
  Next i

  f$ = cin$("File number? ")
  If Val(f$) > 0 And Val(f$) <= sz Then fi_choose$ = d$ + "\" + all$(Val(f$) - 1)

End Function
