' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

#Include "../console.inc"

Cls

C_WIDTH = 50

Print "-- Expected --------------------------------------"
Print
Print "Moses supposes his toeses are roses, but Moses"
Print "supposes eroneously.
Print "For Moses he knowses his toeses aren't roses as"
Print "Moses supposes his toeses to be."
Print
Print "-- Actual ----------------------------------------"
Print

cout("Moses supposes his toeses are roses, but Moses supposes eroneously.")
endl()
cout("For Moses he knowses his toeses aren't roses as Moses supposes his ")
cout("toeses to be.")
endl()

Print

Print "-- Expected --------------------------------------"
Print
Print "    foo          bar"
Print "                                             foo  "
Print "bar"
Print
Print "-- Actual ----------------------------------------"
Print
cout( "    foo          bar")
endl()
cout( "                                             foo              bar")
endl()

Print

Print "-- Expected --------------------------------------"
Print
Print
Print "foobar"
Print
Print "-- Actual ----------------------------------------"
Print
cout( "                                               foobar")
endl()

Print
Print "-- End --"
