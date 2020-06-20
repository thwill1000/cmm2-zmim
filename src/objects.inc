' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Constants for ob_rel()
Const PARENT = 4 : Const SIBLING = 5 : Const CHILD = 6

' Gets/sets object attribute.
Function ob_attr(o, a, s, x)
  Local ad, m, y

  If o = 0 Then Exit Function ' return 0

  ad = rw(&h0A) + 62 + (o - 1) * 9 + a \ 8
  y = rb(ad)
  m = 2^(7 - a Mod 8) ' mask
  If s = 0 Then ob_attr = (y And m) > 0 : Exit Function
  If x = 0 Then y = (y And (m Xor &hFF)) Else y = (y Or m)
  wb(ad, y)
  ob_attr = x
End Function

' Gets/sets object relatives.
Function ob_rel(o, r, s, x)
  Local ad
  ad = rw(&h0A) + 62 + (o - 1) * 9 + r
  If s = 0 Then ob_rel = rb(ad) : Exit Function
  wb(ad, x)
  ob_rel = x
End Function

Function ob_next_prop(o, p)
  Local ad, x

  If o = 0 Then
    Exit Function ' return 0
  ElseIf p = 0 Then
    ad = ob_prop_base(o)
    ad = ad + 1 + 2 * rb(ad) ' skip length & description
  Else
    ad = ob_prop_addr(o, p)
    If ad = 0 Then Error "Property does not exist"
    x = rb(ad - 1)
    ad = ad + 1 + x\32
  EndIf

  x = rb(ad)
  ob_next_prop = x And &b11111 ' bottom 5 bits
End Function

Function ob_prop_len(ad)
  Local x
  If ad = 0 Then Exit Function ' return 0
  x = rb(ad - 1)
  ob_prop_len = x\32 + 1
End Function

Function ob_prop_base(o)
  Local ad
  ad = rw(&h0A) + 62 + (o - 1) * 9 + 7
  ob_prop_base = rw(ad)
End Function

Function ob_prop_addr(o, p)
  Local ad, x
  ad = ob_prop_base(o)
  ad = ad + 1 + 2 * rb(ad) ' Skip length & description
  Do
    x = rb(ad)
    ' Mask with bottom 5 bits
    If (x And &b11111) = p Then ob_prop_addr = ad + 1 : Exit Function
    If (x And &b11111) < p Then ob_prop_addr = 0 : Exit Function
    ad = ad + 2 + x\32
  Loop
End Function

Function ob_prop_get(o, p)
  Local ad, sz, x
  ad = ob_prop_addr(o, p)
  If ad > 0 Then
    x = rb(ad - 1)
    If (x And &b11111) <> p Then Error ' Mask with bottom 5 bits
    sz = x\32 + 1
    If sz = 1 Then ob_prop_get = rb(ad) : Exit Function
    If sz = 2 Then ob_prop_get = rw(ad) : Exit Function
    Error "Property length > 2"
  EndIf
  ad = rw(&h0A) + 2 * (p - 1)
  ob_prop_get = rw(ad)
End Function

Sub ob_prop_set(o, p, x)
  Local a, sz
  a = ob_prop_addr(o, p)
  If a = 0 Then Error "Object " + Str$(o) + " does not have property " + Str$(p)
  sz = ob_prop_len(a)
  If sz < 0 Or sz > 2 Then Error "Object " + Str$(o) + " has length " + Str$(sz)
  If sz = 1 Then wb(a, x And &hFF)
  If sz = 2 Then ww(a, x)
End Sub

Sub ob_print(o)
  Local ad
  ad = ob_prop_base(o) + 1
  print_zstring(ad)
End Sub

Sub ob_insert(o, d)
  Local c, _

  ob_remove(o)                  ' remove object from its original parent
  c = ob_rel(d, CHILD)         ' original child of destination
  _ = ob_rel(d, CHILD, 1, o)   ' object is new child of destination
  _ = ob_rel(o, PARENT, 1, d)  ' destination is new parent of object
  _ = ob_rel(o, SIBLING, 1, c) ' original child of dest is new sibling of object
End Sub

Sub ob_remove(o)
  Local c, p, s, _

  p = ob_rel(o, PARENT)        ' parent of object
  s = ob_rel(o, SIBLING)       ' sibling of object
  c = ob_rel(p, CHILD)         ' first child of parent
  _ = ob_rel(o, PARENT, 1, 0)  ' object no longer has parent
  _ = ob_rel(o, SIBLING, 1, 0) ' object no longer has sibling
  If o = c Then
    _ = ob_rel(p, CHILD, 1, s) ' if object was first child then now its sibling is
  Else
    Do
      If ob_rel(c, SIBLING) = o Then _ = ob_rel(c, SIBLING, 1, s) : Exit Do
      c = ob_rel(c, SIBLING)
    Loop Until c = 0
  EndIf
End Sub
