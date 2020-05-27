' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

' Constants for orel()
Const PARENT = 4 : Const SIBLING = 5 : Const CHILD = 6

' Gets/sets object attribute.
Function oattr(o, a, s, x)
  Local ad, m, y
  ad = rw(&h0A) + 62 + (o - 1) * 9 + a \ 8
  y = rb(ad)
  m = BIT(7 - a Mod 8)
  If s = 0 Then oattr = (y And m) > 0 : Exit Function
  If x = 0 Then y = (y And (m Xor &hFF)) Else y = (y Or m)
  wb(ad, y)
  oattr = x
End Function

' Gets/sets object relatives.
Function orel(o, r, s, x)
  Local ad
  ad = rw(&h0A) + 62 + (o - 1) * 9 + r
  If s = 0 Then orel = rb(ad) : Exit Function
  wb(ad, x)
  orel = x
End Function

Function get_next_prop(o, p)
  Local ad, x

  If p = 0 Then
    ad = get_prop_base(o)
    ad = ad + 1 + 2 * rb(ad) ' Skip length & description
  Else
    ad = get_prop_addr(o, p)
    If ad = 0 Then Error "Property does not exist"
    x = rb(ad)
    ad = ad + 2 + x\32
  EndIf

  x = rb(ad)
  get_next_prop = x And BTM_5_BITS
End Function

Function get_prop_len(ad)
  Local x
  If ad = 0 Then Error "Property does not exist"
  x = rb(ad)
  get_prop_len = x\32 + 1
End Function

Function get_prop_base(o)
  Local ad
  ad = rw(&h0A) + 62 + (o - 1) * 9 + 7
  get_prop_base = rw(ad)
End Function

Function get_prop_addr(o, p)
  Local ad, x
  ad = get_prop_base(o)
  ad = ad + 1 + 2 * rb(ad) ' Skip length & description
  Do
    x = rb(ad)
    If (x And BTM_5_BITS) = p Then get_prop_addr = ad : Exit Function
    If (x And BTM_5_BITS) < p Then get_prop_addr = 0 : Exit Function
    ad = ad + 2 + x\32
  Loop
End Function

Function get_prop(o, p)
  Local ad, sz, x
  ad = get_prop_addr(o, p)
  If ad > 0 Then
    x = rb(ad)
    If (x And BTM_5_BITS) <> p Then Error
    sz = x\32 + 1
    If sz = 1 Then get_prop = rb(ad + 1) : Exit Function
    If sz = 2 Then get_prop = rw(ad + 1) : Exit Function
    Error "Property length > 2"
  EndIf
  ad = rw(&h0A) + 2 * (p - 1)
  get_prop = rw(ad)
End Function

Sub print_obj(o)
  Local ad
  ad = get_prop_base(o) + 1
  print_zstring(ad)
End Sub
