' Copyright (c) 2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Cls
Print "Z-MIM is loading, please wait ..."
Const path$ = Choice(Mm.Info$(Path) = "NONE", Cwd$ + "/", Mm.Info$(Path))
If InStr(Mm.Device$, "PicoMite") And Not InStr(Mm.Device$, "RP2350") Then
  Const file$ = path$ + "bin/zmim_rp2040.bas"
Else
  Const file$ = path$ + "bin/zmim.bas"
EndIf

If InStr(Mm.Info$(Device X), "MMB4L") Then
  If Mm.Info(Version) >= 80000000 Then
    Run file$, Mm.CmdLine$ As Mm.Info(Option Simulate)
  Else
    Run file$, Mm.CmdLine$
  EndIf
ElseIf InStr(Mm.Info(Device), "PicoMite") Then
  Run file$, Mm.CmdLine$
Else
  cmd$ = "Run " + quote$(file$) + ", " + quote$(Mm.CmdLine$)
  Execute cmd$
EndIf

Function quote$(s$)
  quote$ = Chr$(34) + s$ + Chr$(34)
End Function
