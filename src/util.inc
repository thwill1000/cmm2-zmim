' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Function rshift(v, num)
  rshift = v\(2^num)
End Function

Function lpad$(s$, i, c$)
  Local a
  a = Len(s$)
  If c$ = "" Then c$ = " "
  If a < i Then lpad$ = String$(i - a, c$) + s$ Else lpad$ = s$
End Function

Function rpad$(s$, i, c$)
  Local a
  a = Len(s$)
  If c$ = "" Then c$ = " "
  If a < i Then rpad$ = s$ + String$(i - a, c$) Else rpad$ = s$
End Function