' Copyright (c) 2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Cls
Print "Z-MIM is loading, please wait ..."
If InStr(Mm.Device$, "PicoMite") And Not InStr(Mm.Device$, "RP2350") Then
  Const file$ = Mm.Info$(Path) + "bin/zmim_rp2040.bas"
Else
  Const file$ = Mm.Info$(Path) + "bin/zmim.bas"
EndIf

cmd$ = "Run "
If Mm.Info(Device X) = "MMB4L" Then
  If Mm.Info(Version) >= 80000000 Then Cat cmd$, Mm.Info(Option Simulate) + " "
EndIf
Execute cmd$ + Chr$(34) + file$ + Chr$(34) + ", " + Mm.CmdLine$
