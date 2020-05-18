' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Sub dmp_mmap
  Local i

  Print "Physical page -> Virtual page"
  For i = 0 To NUM_PHYSICAL_PAGES - 1
    Print Str$(i); " -> "; Str$(pp_to_vp(i))
'    If (i + 1) Mod 20 = 0 Then more()
  Next i

  Print "Virtual page -> Physical page"
  For i = 0 To NUM_VIRTUAL_PAGES - 1
    Print Str$(i); " -> "; Str$(vp_to_pp(i))
'    If (i + 1) Mod 20 = 0 Then more()
  Next i
End Sub
