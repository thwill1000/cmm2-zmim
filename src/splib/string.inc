' Copyright (c) 2020-2024 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 5.07

'!if !defined(NO_INCLUDE_GUARDS)
On Error Skip 1 : Dim sys.VERSION = -1
If sys.VERSION = -1 Then Error "'system.inc' not included"
sys.provides("string")
If sys.err$ <> "" Then Error sys.err$
'!endif

' Pads a string with spaces to the left and right so that it will be centred
' within a fixed length field. If the string is longer than the field then
' this function just returns the string. If an odd number of spaces are
' required then the extra space is added to the left hand side of the string.
'
' @param  s$  the string to centre.
' @param  x   the field length.
Function str.centre$(s$, x%)
  If Len(s$) < x% Then
    str.centre$ = s$ + Space$((x% - Len(s$)) \ 2)
    str.centre$ = Space$(x% - Len(str.centre$)) + str.centre$
  Else
    str.centre$ = s$
  EndIf
End Function

' Compares s1$ and s2$ ignoring case considerations.
Function str.equals_ignore_case%(s1$, s2$)
  str.equals_ignore_case% = LCase$(s1$) = LCase$(s2$)
End Function

' Does a string contain only printable and/or whitespace ASCII ?
'
' @param  s$  the string.
' @return     1 if the string only contains printable and/or whitespace ASCII, otherwise 0.
Function str.is_plain_ascii%(s$)
  Local i%
  For i% = 1 To Len(s$)
    Select Case Peek(Var s$, i%)
      Case 9, 10, 13   : ' Tab, line-feed and carriage-return are acceptable.
      Case < 32, > 126 : Exit Function
      Case Else        : ' ASCII printable characters.
    End Select
  Next
  str.is_plain_ascii% = 1
End Function

' Gets a string padded to a given width with to the left.
'
' @param  s$  the string.
' @param  w%  the width.
' @param  c$  the padding character, a space if omitted.
' @return     the padded string.
'             If Len(s$) > w% then returns the unpadded string.
Function str.lpad$(s$, x%, c$)
  str.lpad$ = s$
  If c$ = "" Then c$ = " "
  If Len(s$) < x% Then str.lpad$ = String$(x% - Len(s$), c$) + s$
End Function

' Tokenises a string separated by delimiters.
'
'  - delimiters are ignored between pairs of double-quotes.
'  - a double-quote preceeded by a backslash does not end a pair of
'    double-quotes.
'  - double-quotes and backslashes are included verbatim in the returned tokens.
'
' @param   s$     string to tokenise.
' @param   dlm$   one or more token delimiter characters.
'                 If empty then use space and skip empty tokens.
' @param   skip%  1 to skip empty tokens, 0 to return them.
' @return  the first token. To retrieve subsequent tokens call this function
'          with no parameters, i.e. tk$ = str.next_token$().
'          Returns sys.NO_DATA$ if there are no more tokens.
'
' WARNING! Take care when calling this function naively in cases where s$ might
' be the empty string as that will return data from a previously incomplete
' tokenisation. If necessary call str.next_token$(sys.NO_DATA$) to clear the
' internal state first.
Function str.next_token$(s$, dlm$, skip%)
  Static s_$, dlm_$, skip_%, p%

  If s$ <> "" Then
    s_$ = s$
    dlm_$ = Choice(dlm$ = "", " ", dlm$)
    skip_% = Choice(dlm$ = "", 1, skip%)
    p% = 1 ' Index to process from.
  EndIf

  ' If we've reached the end of the string then return either NO_DATA or an
  ' empty token depending on the value of skip_% and then force skip_% =
  If p% > Len(s_$) Then
    str.next_token$ = Choice(skip_%, sys.NO_DATA$, "")
    skip_% = 1
    Exit Function
  EndIf

  Local ch%, state% = 0
  For p% = p% To Len(s_$)
    ch% = Peek(Var s_$, p%)

    Select Case state%
      Case 0  ' Base state
        If ch% = &h22 Then
          state% = 1
        ElseIf InStr(dlm_$, Chr$(ch%)) Then
          If skip_% Then
            If Len(str.next_token$) = 0 Then Continue For
          EndIf
          Inc p%
          Exit For
        EndIf

      Case 1  ' Inside double-quote
        Select Case ch%
          Case &h22 : state% = 0  ' double-quote
          Case &h5C : state% = 2  ' backslash
        End Select

      Case 2  ' Inside double-quote and following backslash
        state% = 1

    End Select

    Cat str.next_token$, Chr$(ch%)
  Next

  ' If we reach the end of the string and the last character is not a delimiter
  ' then force skip_% = 1 so we do not return an empty token on the next call.
  If p% > Len(s_$) Then
    If Not InStr(dlm_$, Chr$(ch%)) Then skip_% = 1
  EndIf
End Function

' Gets a string "quoted" with given characters.
'
' @param  s$      the string.
' @param  begin$  the character to put at the start, defaults to double-quote.
' @param  end$    the character to put at the end, defaults to double-quote.
' @return         the "quoted" string.
Function str.quote$(s$, begin$, end$)
  Local begin_$ = Choice(begin$ = "", Chr$(34), Left$(begin$, 1))
  Local end_$ = Choice(end$ = "", begin_$, Left$(end$, 1))
  str.quote$ = begin_$ + s$ + end_$
End Function

' Gets copy of 'haystack$' with occurrences of 'needle$' replaced by 'rep$'.
Function str.replace$(haystack$, needle$, rep$)
  Local p%, st%, s$
  Do
    Inc st%
    p% = InStr(st%, haystack$, needle$)
    If p% < 1 Then Exit Do
    Cat s$, Mid$(haystack$, st%, p% - st%) + rep$
    st% = p% + Len(needle$) - 1
  Loop
  Cat s$, Mid$(haystack$, st%)
  str.replace$ = s$
End Function

' Reverses the order of the characters in a string.
'
' @param  s$  the string.
' @return     the reversed string.
Function str.reverse$(s$)
  Local i%
  For i% = Len(s$) To 1 Step -1
    Cat str.reverse$, Mid$(s$, i%, 1)
  Next
End Function

' Gets a string padded to a given width to the right.
'
' @param  s$  the string.
' @param  w%  the width.
' @param  c$  the padding character, a space if omitted.
' @return     the padded string.
'             If Len(s$) > w% then returns the unpadded string.
Function str.rpad$(s$, x%, c$)
  str.rpad$ = s$
  If c$ = "" Then c$ = " "
  If Len(s$) < x% Then str.rpad$ = s$ + String$(x% - Len(s$), c$)
End Function

' Returns a copy of s$ with leading and trailing spaces removed.
Function str.trim$(s$)
  Local st%, en%
  For st% = 1 To Len(s$)
    If Peek(Var s$, st%) <> 32 Then Exit For
  Next
  For en% = Len(s$) To 1 Step -1
    If Peek(Var s$, en%) <> 32 Then Exit For
  Next
  If en% >= st% Then str.trim$ = Mid$(s$, st%, en% - st% + 1)
End Function

' Returns a copy of s$ with trailing spaces removed.
Function str.rtrim$(s$)
  Local en%
  For en% = Len(s$) To 1 Step -1
    If Peek(Var s$, en%) <> 32 Then Exit For
  Next
  If en% >= 1 Then str.rtrim$ = Mid$(s$, 1, en%)
End Function

' If s$ both begins and ends with " then returns a copy of s$ with those characters removed,
' otherwise returns an unmodified copy of s$.
Function str.unquote$(s$)
  str.unquote$ = s$
  If Len(s$) < 2 Then Exit Function
  If Peek(Var s$, 1) <> 34 Then Exit Function
  If Peek(var s$, Len(s$)) = 34 Then
    str.unquote$ = Mid$(s$, 2, Len(s$) - 2)
  EndIf
End Function

' Decodes a string containing "C" escape codes.
'
' Note: does not handle octal escape codes.
'
' state%:
'   0 = base state
'   1 = looking for escape code
'   2 = expecting 1st hex digit
'   3 = expecting 2nd hex digit
Function str.decode$(s$)
  Local ad%, ch%, prev%, state%, t$

  For ad% = Peek(VarAddr s$) + 1 TO Peek(VarAddr s$) + Len(s$)
    ch% = Peek(Byte ad%)
    Select Case ch%
      Case &h5C ' \
        state% = 1
      Case Else
        Select Case state%
          Case 0
            Cat t$, Chr$(ch%)
          Case 1
            Select Case ch%
              Case &h22 : ch% = &h22 ' \"
              Case &h27 : ch% = &h27 ' \'
              Case &h30 : ch% = &h00 ' \0
              Case &h3F : ch% = &h3F ' \?
              Case &h5C : ch% = &h5C ' \
              Case &h61 : ch% = &h07 ' \a
              Case &h62 : ch% = &h08 ' \b
              Case &h65 : ch% = &h1B ' \e
              Case &h66 : ch% = &h0C ' \f
              Case &h6E : ch% = &h0A ' \n
              Case &h71 : ch% = &h22 ' \q
              Case &h72 : ch% = &h0D ' \r
              Case &h74 : ch% = &h09 ' \t
              Case &h76 : ch% = &h0B ' \v
              Case &h78 : state% = 2 ' \x
              Case Else : Cat t$, "\"
            End Select
            If state% = 1 Then
              Cat t$, Chr$(ch%)
              state% = 0
            EndIf
          Case 2
            Select Case ch%
              Case &h30 To &h39, &h41 To &h46, &h61 To &h66 ' 0-9, A-F, a-f
                prev% = ch%
                state% = 3
              Case Else
                Cat t$, "\x" + Chr$(ch%)
                state% = 0
            End Select
          Case 3
            Select Case ch%
              Case &h30 To &h39, &h41 To &h46, &h61 To &h66 ' 0-9, A-F, a-f
                Cat t$, Chr$(Val("&h" + Chr$(prev%) + Chr$(ch%)))
              Case Else
                Cat t$, "\x" + Chr$(prev%) + Chr$(ch%)
            End Select
            state% = 0
        End Select
    End Select
  Next

  Select Case state%
    Case 1 : Cat t$, "\"
    Case 2 : Cat t$, "\x"
    Case 3 : Cat t$, "\x" + Chr$(prev%)
  End Select

  str.decode$ = t$
End Function

' Encodes a string using "C" escape codes.
Function str.encode$(s$)
  Local ad%, ch%, t$
  For ad% = Peek(VarAddr s$) + 1 TO Peek(VarAddr s$) + Len(s$)
    ch% = Peek(Byte ad%)
    Select Case ch%
      Case &h00 : Cat t$, "\0"
      Case &h07 : Cat t$, "\a"
      Case &h08 : Cat t$, "\b"
      Case &h09 : Cat t$, "\t"
      Case &h0A : Cat t$, "\n"
      Case &h0B : Cat t$, "\v"
      Case &h0C : Cat t$, "\f"
      Case &h0D : Cat t$, "\r"
      Case &h1B : Cat t$, "\e"
      Case &h22 : Cat t$, "\q"
      Case &h27 : Cat t$, "\'"
      Case &h3F : Cat t$, "\?"
      Case &h5C : Cat t$, "\\"
      Case < &h20, >= &h7F : Cat t$, "\x" + Hex$(ch%, 2)
      Case Else : Cat t$, Chr$(ch%)
    End Select
  Next
  str.encode$ = t$
End Function

' Implements word wrapping by splitting a string on spaces.
'
' @param[in]       s$    the string.
' @param[in, out]  p%    position in the string to start from.
' @param[in]       len%  the 'line length'.
' @return                segment of string up to len% characters with no broken
'                        words, unless a word is longer than len%.
Function str.wwrap$(s$, p%, len%)
  Const slen% = Len(s$)
  Local ch%, q%, word$
  For q% = p% To slen% + 1
    ch% = Choice(q% > slen%, 0, Peek(Var s$, q%))
    Select Case ch%
      Case 0, &h0A, &h0D, &h20 ' null, \n, \r, space
        If Len(str.wwrap$) + Len(word$) > len% Then
          If Len(word$) > len% Then
            word$ = Left$(word$, len% - Len(str.wwrap$))
            Cat str.wwrap$, word$
            Inc p%, Len(word$)
          EndIf
          Exit For
        EndIf
        Cat str.wwrap$, word$
        p% = q% + 1
        Select Case ch%
          Case &h0D
            If Choice(p% > slen%, 0, Peek(Var s$, p%)) = &h0A Then Inc p%
            Exit For
          Case &h20
            If Len(str.wwrap$) = len% Then Exit For
            Cat str.wwrap$, " "
            word$ = ""
          Case Else
            Exit For
        End Select
      Case Else
        Cat word$, Chr$(ch%)
    End Select
  Next
  p% = Min(p%, slen% + 1)
End Function
