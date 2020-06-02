' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Function file_choose$(d$, fspec$)
  Local c$, f$, n, m

  c$ = Cwd$
  Chdir d$

  ' Count number of entries 'n' and their mzimum length 'm'
  f$ = Dir$(fspec$)
  Do While f$ <> ""
    n = n + 1
    If Len(f$) > m Then m = Len(f$)
    f$ = Dir$()
  Loop

  ' Create sorted array of entries 'all$'
  Dim all$(n - 1) LENGTH m
  f$ = Dir$(fspec$)
  For i = 0 To n - 1 : all$(i) = f$ : f$ = Dir$() : Next i
  Sort all$()

  ' Display entries
  If i < 6 Then
    For i = 0 To n - 1
      Print "  [" + Format$(i + 1, "%2g") + "] "; all$(i)
    Next i
  Else
    For i = 0 To (n - 1) \ 2
      Print "  [" + Format$(i + 1, "%2g") + "] "; rpad$(all$(i), m + 2);
      Print "  [" + Format$(i + n \ 2 + 1, "%2g") + "] "; all$(i + n \ 2)
    Next i
  EndIf

  Line Input "> ", f$
  file_choose$ = d$ + "/" + all$(Val(f$) - 1)

  ' TODO: Error handling for non-numeric selection

  Chdir c$ ' Restore working directory
End Function

