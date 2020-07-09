' Copyright (c) 2020 Thomas Hugo Williams
'
' Settings for building an optimised CMM2 version of Z-MIM

'!set      TARGET_CMM2
'!set      NO_DEBUG
'!comments off
'!flatten  on
'!indent   1
'!spacing  tight
'!replace  "mem_cmm2_safe.inc" "mem_cmm2_fast.inc"
'!replace  C_HEIGHT            50
'!replace  C_WIDTH             100

#Include "constants.mbt"
#Include "ids.mbt"
#Include "main.bas"
