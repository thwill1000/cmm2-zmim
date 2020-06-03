' LCG random number generator
' Derived from code by Peter Mather
' For Colour Maximite 2, MMBasic 5.05

' Generates a pseudo-random number from 0 to 0.999999 (ish)
' If seed% < 0 Then re-seed using the Timer
' If seed% = 0 Then use next random number from current seed
' If seed% > 0 Then re-seed using the value of seed%
Function cmm2rnd(seed%)
  Static x% = 7
  Static a% = 1103515245 ' a%, c% and m% are same values as used by glibc
  Static c% = 12345
  Static m% = 2^31

  If seed% < 0 Then
    x% = Timer
  ElseIf seed% > 0 Then
    x% = seed%
  EndIf

  x% = (a% * x% + c%) mod m%
  cmm2rnd = x% / m%
End Function


