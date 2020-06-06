' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Function file_choose$(d$, fspec$)
  Local c$, f$, i, n, m

  c$ = Cwd$
  Chdir d$

  ' Count number of entries 'n' and their maximum length 'm'
  f$ = Dir$(fspec$)
  Do While f$ <> ""
    n = n + 1
    If Len(f$) > m Then m = Len(f$)
    f$ = Dir$()
  Loop
  If n > 20 Then n = 21

  ' Create sorted array of entries 'all$'.
  ' Element 0 is always empty, even after the sort.
  Local all$(n) LENGTH m
  f$ = Dir$(fspec$)
  For i = 1 To n : all$(i) = f$ : f$ = Dir$() : Next i
  Sort all$()

  ' Display entries
  If i < 6 Then
    For i = 1 To n
      Print "  [" + Format$(i, "%2g") + "] "; all$(i)
    Next i
  Else
    For i = 1 To n \ 2
      Print "  [" + Format$(i, "%2g") + "] "; rpad$(all$(i), m + 2);
      Print "  [" + Format$(i + n \ 2, "%2g") + "] "; all$(i + n \ 2)
    Next i
  EndIf

  If n = 21 Then
    Print "  There are more than 20 files, only the first 20 are shown."
  EndIf

  Line Input "File number? ", f$
  If Val(f$) <> 0 Then file_choose$ = d$ + "/" + all$(Val(f$))

  Chdir c$ ' Restore working directory
End Function

