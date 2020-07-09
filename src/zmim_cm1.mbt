' Copyright (c) 2020 Thomas Hugo Williams
'
' Settings for building an optimised CMM1 version of Z-MIM

'!set      TARGET_CMM1
'!set      NO_DEBUG
'!comments off
'!flatten  on
'!indent   0
'!spacing  tight
'!replace  "mem_cmm2_safe.inc" "mem_cmm1.inc"
'!replace  "\title.txt"        "\titl_cm1.txt"
'!replace  "\credits.txt"      "\cred_cm1.txt"
'!replace  C_HEIGHT            36
'!replace  C_WIDTH             80

#Include "constants.mbt"
#Include "ids.mbt"
#Include "main.bas"
