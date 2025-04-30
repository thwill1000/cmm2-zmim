' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Option Explicit On

#Include "../splib/system.inc"
#Include "../splib/vt100.inc"
#Include "../console.inc"

Cls

con.WIDTH = 50

Print "-- Expected --------------------------------------"
Print
Print "Moses supposes his toeses are roses, but Moses"
Print "supposes eroneously.
Print "For Moses he knowses his toeses aren't roses as"
Print "Moses supposes his toeses to be."
Print
Print "-- Actual ----------------------------------------"
Print

con.print("Moses supposes his toeses are roses, but Moses supposes eroneously.")
con.endl()
con.print("For Moses he knowses his toeses aren't roses as Moses supposes his ")
con.print("toeses to be.")
con.endl()

Print

Print "-- Expected --------------------------------------"
Print
Print "    foo          bar"
Print "                                             foo  "
Print "bar"
Print
Print "-- Actual ----------------------------------------"
Print
con.print( "    foo          bar")
con.endl()
con.print( "                                             foo              bar")
con.endl()

Print

Print "-- Expected --------------------------------------"
Print
Print
Print "foobar"
Print
Print "-- Actual ----------------------------------------"
Print
con.print( "                                               foobar")
con.endl()

Print
Print "-- End --"
