' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Function fmt_hex$(x, i)
  If i < 1 Then i = 4
  fmt_hex$ = "&h" + str.lpad$(Hex$(x), i, "0")
End Function
