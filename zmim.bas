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

' EXECUTE "RUN ..." doesn't work from B: on the PicoMite.
If InStr(Mm.Info(Device X), "PicoMite") Then Run file$, Mm.CmdLine$

cmd$ = "Run "
If Mm.Info(Device X) = "MMB4L" Then
  If Mm.Info(Version) >= 80000000 Then Cat cmd$, Mm.Info(Option Simulate) + " "
EndIf
Cat cmd$, quote$(file$) + ", " + quote$(Mm.CmdLine$)
Execute cmd$

Function quote$(s$)
  quote$ = Chr$(34) + s$ + Chr$(34)
End Function
