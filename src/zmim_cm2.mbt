' Copyright (c) 2020-2023 Thomas Hugo Williams
'
' Optimised CMM2 version of Z-MIM

'!comments     off
'!empty-lines  single
'!indent       1
'!spacing      minimal
'!define       TARGET_CMM2
'!define       NO_DEBUG
'!replace      "mem_cmm2_safe.inc"  "mem_cmm2_fast.inc"
'!replace      C_HEIGHT             50
'!replace      C_WIDTH              100

#Include "constants.mbt"
#Include "ids.mbt"
#Include "main.bas"
