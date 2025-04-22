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

If InStr(Mm.Device$, "Colour Maximite 2") Then
  Execute "Run " + Chr$(34) + file$ + Chr$(34) + ", " + Mm.CmdLine$
Else
  Run file$, Mm.CmdLine$
EndIf
