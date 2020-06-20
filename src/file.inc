' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Function fi_choose$(d$, fspec$)
  Local col, f$, i, m, n, old_dir$, x

  old_dir$ = Cwd$
  ChDir d$

  ' Count number of entries 'n' and their maximum length 'm'
  f$ = Dir$(fspec$)
  Do While f$ <> ""
    n = n + 1
    If Len(f$) > m Then m = Len(f$)
    f$ = Dir$()
  Loop

  If n = 0 Then cout("No files found") : endl() : ChDir old_dir$ : Exit Function

  ' Create sorted array of entries 'all$'.
  ' Element 0 is always empty, even after the sort.
  Local all$(n) LENGTH m
  f$ = Dir$(fspec$)
  For i = 1 To n : all$(i) = f$ : f$ = Dir$() : Next i
  Sort all$()

  ChDir old_dir$

  ' Display entries
  If n < 6 Then
    For i = 1 To n
      cout("  [" + Format$(i, "%2g") + "] " + all$(i)) : endl()
    Next i
  Else
    col = 0
    For i = 1 To n
      x = 1 + (i \ 2) + col * (n \ 2)
      If x > n Then Exit For
      cout("  [" + Format$(x, "%2g") + "] " + rpad$(all$(x), m + 2))
      If col = 1 Then endl()
      col = (col + 1) Mod 2
    Next i
    If col = 1 Then endl()
  EndIf

  f$ = cin$("File number? ")
  If Val(f$) <> 0 Then fi_choose$ = d$ + "/" + all$(Val(f$))

End Function
