' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Function file_choose$(d$, fspec$)
  Local c$, col, f$, i, n, m, x

  c$ = Cwd$
  Chdir d$

  ' Count number of entries 'n' and their maximum length 'm'
  f$ = Dir$(fspec$)
  Do While f$ <> ""
    n = n + 1
    If Len(f$) > m Then m = Len(f$)
    f$ = Dir$()
  Loop

  ' Create sorted array of entries 'all$'.
  ' Element 0 is always empty, even after the sort.
  Local all$(n) LENGTH m
  f$ = Dir$(fspec$)
  For i = 1 To n : all$(i) = f$ : f$ = Dir$() : Next i
  Sort all$()

  ' Display entries
  If n < 6 Then
    For i = 1 To n
      Print "  [" + Format$(i, "%2g") + "] "; all$(i)
    Next i
  Else
    col = 0
    For i = 1 To n
      x = 1 + (i \ 2) + col * (n \ 2)
      Print "  [" + Format$(x, "%2g") + "] "; rpad$(all$(x), m + 2);
      If col = 1 Then Print
      col = (col + 1) Mod 2
    Next i
    If col = 1 Then Print
  EndIf

  Line Input "File number? ", f$
  If Val(f$) <> 0 Then file_choose$ = d$ + "/" + all$(Val(f$))

  Chdir c$ ' Restore working directory
End Function

