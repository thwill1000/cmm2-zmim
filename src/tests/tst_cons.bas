' Copyright (c) 2020 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

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
