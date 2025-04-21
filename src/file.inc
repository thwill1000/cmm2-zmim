' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.requires("console")
sys.provides("zmim_file")
If sys.err$ <> "" Then Error sys.err$
'!endif

Function fi_choose$(d$, fspec$)
  Local f$, i, j, nc, nr, old_dir$, sz, width, x

  old_dir$ = Cwd$
  ChDir d$

  ' Count number of entries 'sz' and their maximum length 'width'.
  ' Ignore files prefixed with full-stop.
  f$ = Dir$(fspec$)
  Do While f$ <> ""
    If Left$(f$, 1) <> "." Then
      Inc sz
      If Len(f$) > width Then width = Len(f$)
    EndIf
    f$ = Dir$()
  Loop

  If sz = 0 Then con.println("No files found") : ChDir old_dir$ : Exit Function

  ' Create sorted array of entries 'all$'.
  ' Once again ignore all files prefixed with full-stop.
  ' MMBasic doesn't allow creation of an array with a single element so just
  ' in case we include an extra dummy element that will always be sorted last.
  Local all$(sz) LENGTH width
  all$(sz) = Chr$(&h7F)
  f$ = Dir$(fspec$)
  i = 0
  Do
    If Left$(f$, 1) <> "." Then all$(i) = f$ : Inc i
    f$ = Dir$()
  Loop Until i = sz
  Sort all$()

  ChDir old_dir$

  ' Display entries in one column if < 8, otherwise 2 columns.
  If (sz < 8 Or con.WIDTH <= 40) Then nc = 1 Else nc = 2
  nr = CInt(sz / nc + 0.4999)
  Inc width, 10
  For i = 0 To nr - 1
    For j = 0 to nc - 1
      con.flush()
      x = (j * nr) + i
      If x < sz Then
        If j * width > Pos Then con.print(Space$(j * width - Pos))
        con.print("  [" + Format$(x + 1, "%2g") + "] " + all$(x))
      EndIf
    Next
    con.endl()
  Next

  f$ = con.in$("File number? ")
  If Val(f$) > 0 And Val(f$) <= sz Then fi_choose$ = file.resolve$(d$, all$(Val(f$) - 1))

End Function

Function file.resolve$(parent$, f$)
  file.resolve$ = parent$ + "/" + f$
End Function
