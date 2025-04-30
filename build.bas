#!/usr/local/bin/mmbasic

' Copyright (c) 2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

If Mm.Device$ <> "MMB4L" Then Error "Requires MMBasic for Linux"

Const PATH$ = Choice(Mm.Info$(Path) = "NONE", Cwd$, Mm.Info$(Path))
Const OLD$ = Cwd$

ChDir PATH$

cmd$ = "sptrans -T -DNO_INCLUDE_GUARDS -DNO_EXTRA_CHECKS -DINLINE_CONSTANTS src/main.bas bin/zmim.bas"
Print cmd$
System cmd$

cmd$ = "sptrans -T -DNO_INCLUDE_GUARDS -DLOW_MEMORY -DNO_EXTRA_CHECKS -DINLINE_CONSTANTS src/main.bas bin/zmim_rp2040.bas"
Print cmd$
System cmd$

ChDir OLD$
