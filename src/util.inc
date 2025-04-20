' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

Function lpad$(s$, i, c$)
  Local a = Len(s$)
  If c$ = "" Then c$ = " "
  If a < i Then lpad$ = String$(i - a, c$) + s$ Else lpad$ = s$
End Function

Function rpad$(s$, i, c$)
  Local a = Len(s$)
  If c$ = "" Then c$ = " "
  If a < i Then rpad$ = s$ + String$(i - a, c$) Else rpad$ = s$
End Function

Function fmt_hex$(x, i)
  If i < 1 Then i = 4
  fmt_hex$ = "&h" + lpad$(Hex$(x), i, "0")
End Function
