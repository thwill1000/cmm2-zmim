' Transpiled on 08-07-2025 20:34:06
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Option Base 0
Option Default Integer
Option Explicit On
If InStr(Mm.Device$,"PicoMite") Then
 If Mm.Ver<6.0 Then Error "PicoMite firmware v6.0 or later required"
EndIf
If Mm.Device$="MMB4L" Then Option Resolution Pixel
' src/splib/system.inc ++++
' Copyright (c) 2020-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
On Error Skip
xyz_not_declared%=1
If Not Mm.ErrNo Then Error "OPTION EXPLICIT is required by splib"
On Error Clear
Const sys.VERSION=103300
If Mm.Info$(Device X)="MMB4L" Then
 Const sys.FIRMWARE=Mm.Info(Version)
Else
 Const sys.FIRMWARE=Int(1000000*Mm.Info(Version))
EndIf
Const sys.SUCCESS=0
Const sys.FAILURE=-1
Dim sys.break_flag%
Dim sys.err$

Function sys.HOME$()
 Select Case Mm.Info$(Device X)
  Case "MMB4L"
   sys.HOME$=Mm.Info$(EnvVar "HOME")
  Case "MMBasic for Windows"
   sys.HOME$=Mm.Info$(EnvVar "HOMEDIR")+Mm.Info$(EnvVar "HOMEPATH")
  Case Else
   sys.HOME$="A:"
 End Select
End Function

Function sys.SEPARATOR$()
 sys.SEPARATOR$=Choice(Mm.Device$="MMBasic for Windows","\","/")
End Function

Function sys.error%(code%,msg$)
 If Not code% Then Error "Invalid error code"
 sys.error%=code%
 sys.err$=msg$
End Function

Function sys.read_shell_file$(exist%,fnbr%)
 Const f$=sys.HOME$()+"/.mmbasic-shell"
 If Not exist% And Not Mm.Info(Exists f$) Then Exit Function
 Const _fnbr%=Choice(fnbr%,fnbr%,9)
 Open f$ For Input As _fnbr%
 Line Input #_fnbr%,sys.read_shell_file$
 Close _fnbr%
End Function

Sub sys.run_shell(fnbr%)
 Const prog$=sys.read_shell_file$(1,fnbr%)
 If prog$<>"" Then Run prog$
End Sub

Function file.exists%(f$,type$)
 Local f_$=Choice(Len(f$),f$,"."),i%
 Local type_$="|"+Choice(Len(type$),LCase$(type$),"all")+"|"
 If f_$="." Then f_$=Cwd$
 If Len(f_$)=2 Then
  If Mid$(f_$,2,1)=":" Then Cat f_$,"/"
 EndIf
 For i%=1 To Len(f_$)
  If Peek(Var f_$,i%)=92 Then Poke Var f_$,i%,47
 Next
 If InStr("|file|all|",type_$) Then Inc file.exists%,Mm.Info(Exists File f_$)
 If InStr("|dir|all|",type_$) Then Inc file.exists%,Mm.Info(Exists Dir f_$)
 file.exists%=file.exists%>0
End Function

Function file.get_parent$(f$)
 Select Case Len(f$)
  Case 1
   If f$="/" Or f$="\" Then Exit Function
  Case 2
   If Mid$(f$,2,1)=":" Then Exit Function
  Case 3
   Select Case Right$(f$,2)
    Case ":/",":\"
     Exit Function
   End Select
 End Select
 Local i%
 For i%=Len(f$) To 1 Step -1
  If InStr("/\",Mid$(f$,i%,1))>0 Then Exit For
 Next
 If i%>0 Then file.get_parent$=Left$(f$,i%-1)
 If file.get_parent$="" Then
  If InStr("/\",Left$(f$,1)) Then file.get_parent$=Left$(f$,1)
 EndIf
End Function

Function file.is_directory%(f$)
 file.is_directory%=file.exists%(f$,"dir")
End Function

Function file.mkdir%(f$)
 Local ad%,faddr%=Peek(VarAddr f$),parent$
 sys.err$=""
 For ad%=faddr%+1 To faddr%+Len(f$)
  Select Case Peek(Byte ad%)
   Case 47,92
    parent$=Left$(f$,ad%-faddr%-1)
    If file.exists%(parent$) Then
     If Not file.is_directory%(parent$) Then
      file.mkdir%=sys.error%(sys.FAILURE,"File exists")
      Exit Function
     EndIf
    Else
     If parent$<>"" Then MkDir parent$
    EndIf
  End Select
 Next
 If file.exists%(f$) Then
  If Not file.is_directory%(f$) Then file.mkdir%=sys.error%(sys.FAILURE,"File exists")
 Else
  MkDir f$
 EndIf
End Function

Function file.resolve$(parent$,f$)
 file.resolve$=parent$+sys.SEPARATOR$()+f$
End Function

Function str.lpad$(s$,x%,c$)
 str.lpad$=s$
 If c$="" Then c$=" "
 If Len(s$)<x% Then str.lpad$=String$(x%-Len(s$),c$)+s$
End Function

Function str.rpad$(s$,x%,c$)
 str.rpad$=s$
 If c$="" Then c$=" "
 If Len(s$)<x% Then str.rpad$=s$+String$(x%-Len(s$),c$)
End Function

' ---- src/splib/string.inc
' src/splib/vt100.inc ++++
' Copyright (c) 2020-2023 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Function vt100$(s$)
 vt100$=Chr$(27)+"["+s$
End Function

' ---- src/splib/vt100.inc
' src/memory.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Dim m(128*1024/8+16)
Dim mad=Peek(VarAddr m())+8
Dim FILE_LEN
Dim BASE_STATIC

Function rb(a)
 rb=LGetByte(m(),a)
End Function

Function rw(a)
 rw=LGetByte(m(),a)*256+LGetByte(m(),a+1)
End Function

Sub wb(a,x)
 Poke Byte mad+a,x
End Sub

Sub ww(a,x)
 Poke Byte mad+a,x\256
 Poke Byte mad+a+1,x Mod 256
End Sub

Sub mem_init(f$)
 Local ad,i,j,s$,sz
 LongString Clear m()
 con.println("Loading '"+f$+"'")
 con.print("0% ... ")
 sz=mm.info(filesize f$)
 Open f$ For Input As #1
 j=1
 Do
  s$=Input$(255,#1)
  If Len(s$)=0 Then Exit Do
  LongString Append m(),s$
  Inc ad,Len(s$)
  If ad>j*sz\10 Then con.print(Str$(j*10)+"% ... ") : Inc j
 Loop
 con.println("100%")
 con.println("Read "+Str$(ad)+" bytes")
 Close #1
 FILE_LEN=512
 BASE_STATIC=rw(&h0E)
 FILE_LEN=rw(&h1A)*2
 If FILE_LEN>ad Then Error "Story file is truncated"
End Sub

' ---- src/memory.inc
' src/stack.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Dim stack(511)
Dim sp

Sub st_push(x)
 stack(sp)=x
 Inc sp
End Sub

Function st_peek(i)
 st_peek=stack(i)
End Function

' ---- src/stack.inc
' src/variable.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Dim GLOBAL_VAR

Function vget(i)
 Select Case i
  Case 0
   vget=stack(sp-1) : Inc sp,-1
  Case <&h10
   vget=stack(fp+i+4)
  Case <=&hFF
   vget=rw(GLOBAL_VAR+2*(i-&h10))
  Case Else
   Error "Unknown variable "+Str$(i)
 End Select
End Function

Sub vset(i,v)
 Select Case i
  Case 0
   stack(sp)=v : Inc sp
  Case <&h10
   stack(fp+i+4)=v
  Case <=&hFF
   ww(GLOBAL_VAR+2*(i-&h10),v)
  Case Else
   Error "Unknown variable "+Str$(i)
 End Select
End Sub

' ---- src/variable.inc
' src/decode.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Dim pc
Dim oc=0
Dim onum=0
Dim oa(4)
Dim ot(4)
Dim ov(4)
Dim st=0
Dim br=0

Sub de_init()
 Local i,s$,x
 Read x
 Dim OP0$(x-1) LENGTH 12
 For i=0 To x-1 : Read s$ : OP0$(i)=de_format$(s$) : Next
 Read x
 Dim OP1$(x-1) LENGTH 14
 For i=0 To x-1 : Read s$ : OP1$(i)=de_format$(s$) : Next
 Read x
 Dim OP2$(x-1) LENGTH 15
 For i=0 To x-1 : Read s$ : OP2$(i)=de_format$(s$) : Next
 Read x
 Dim OPV$(x-1) LENGTH 14
 For i=0 To x-1 : Read s$ : OPV$(i)=de_format$(s$) : Next
End Sub

Function decode(tr)
 Local op,s$
 If tr Then con.print(Hex$(pc)+": ")
 op=LGetByte(m(),pc) : Inc pc
 Select Case op
  Case <&h80
   de_long(op)
   s$=OP2$(oc)
  Case <&hC0
   de_short(op)
   s$=Choice(op<&hB0,OP1$(oc),OP0$(oc))
  Case Else
   de_var(op)
   s$=Choice(op<&hE0,OP2$(oc),OPV$(oc))
 End Select
 Select Case Left$(s$,1)
  Case "B"
   st=-1
   br=de_branch()
  Case "S"
   st=LGetByte(m(),pc) : Inc pc
   br=0
  Case "X"
   st=LGetByte(m(),pc) : Inc pc
   br=de_branch()
  Case Else
   st=-1
   br=0
 End Select
 If tr Then dmp_op(Mid$(s$,2))
 decode=op
End Function

Sub de_long(op)
 oc=op And &b11111
 onum=2
 ov(0)=LGetByte(m(),pc) : Inc pc
 ov(1)=LGetByte(m(),pc) : Inc pc
 Select Case op
  Case <=&h1F
   ot(0)=&b01 : oa(0)=ov(0)
   ot(1)=&b01 : oa(1)=ov(1)
  Case <=&h3F
   ot(0)=&b01 : oa(0)=ov(0)
   ot(1)=&b10 : oa(1)=vget(ov(1))
  Case <=&h5F
   ot(0)=&b10 : oa(0)=vget(ov(0))
   ot(1)=&b01 : oa(1)=ov(1)
  Case Else
   ot(0)=&b10 : oa(0)=vget(ov(0))
   ot(1)=&b10 : oa(1)=vget(ov(1))
 End Select
End Sub

Sub de_short(op)
 oc=op And &b1111
 onum=1
 Select Case op
  Case <=&h8F
   ot(0)=&b00 : ov(0)=LGetByte(m(),pc)*256+LGetByte(m(),pc+1) : Inc pc,2 : oa(0)=ov(0)
  Case <=&h9F
   ot(0)=&b01 : ov(0)=LGetByte(m(),pc) : Inc pc : oa(0)=ov(0)
  Case <=&hAF
   ot(0)=&b10 : ov(0)=LGetByte(m(),pc) : Inc pc : oa(0)=vget(ov(0))
  Case Else
   onum=0
 End Select
End Sub

Sub de_var(op)
 Local i,x
 oc=op And &b11111
 onum=4
 x=LGetByte(m(),pc) : Inc pc
 For i=0 To 3
  ot(i)=(x And &b11000000)\64
  Select Case ot(i)
   Case &b00
    ov(i)=LGetByte(m(),pc)*256+LGetByte(m(),pc+1) : Inc pc,2 : oa(i)=ov(i)
   Case &b01
    ov(i)=LGetByte(m(),pc) : Inc pc : oa(i)=ov(i)
   Case &b10
    ov(i)=LGetByte(m(),pc) : Inc pc : oa(i)=vget(ov(i))
   Case Else
    Inc onum,-1
  End Select
  x=x*4
 Next
End Sub

Function de_branch()
 Local a,of,x
 a=LGetByte(m(),pc) : Inc pc
 of=a And &b111111
 If (a And &b1000000)=0 Then
  of=256*of+LGetByte(m(),pc) : Inc pc
  If a And &b100000 Then of=of-16384
 EndIf
 x=pc+of-2
 If a And &h80 Then x=x Or &h80000
 de_branch=x
End Function

Function de_format$(a$)
 Local p,s$
 If Instr(a$," SB")>0 Then
  s$="X"
 ElseIf Instr(a$," B")>0 Then
  s$="B"
 ElseIf Instr(a$," S")>0 Then
  s$="S"
 Else
  s$=" "
 EndIf
 p=Instr(a$," ")
 If p=0 Then p=Len(a$)+1
 Cat s$,Left$(a$,p-1)
 de_format$=s$
End Function

Data 14
Data "RTRUE","RFALSE","PRINT","PRINT_RET","NOP"
Data "SAVE B","RESTORE B","RESTART","RET_POPPED","POP"
Data "QUIT","NEW_LINE","SHOW_STATUS","VERIFY B"
Data 16
Data "JZ B","GET_SIBLING SB","GET_CHILD SB","GET_PARENT S","GET_PROP_LEN S"
Data "INC","DEC","PRINT_ADDR","Unknown&h8","REMOVE_OBJ"
Data "PRINT_OBJECT","RET","JUMP","PRINT_PADDR","LOAD S"
Data "NOT S"
Data 25
Data "Unknown&h0","JE B","JL B","JG B","DEC_CHK B"
Data "INC_CHK B","JIN B","TEST B","OR S","AND S"
Data "TEST_ATTR B","SET_ATTR","CLEAR_ATTR","STORE","INSERT_OBJ"
Data "LOADW S","LOADB S","GET_PROP S","GET_PROP_ADDR S","GEN_NEXT_PROP S"
Data "ADD S","SUB S","MUL S","DIV S","MOD S"
Data 22
Data "CALL S","STOREW","STOREB","PUT_PROP","READ"
Data "PRINT_CHAR","PRINT_NUM","RANDOM S","PUSH","PULL"
Data "SPLIT_WINDOW","SET_WINDOW","Unknown&hC","Unknown&hD","Unknown&hE"
Data "Unknown&hF","Unknown&h10","Unknown&h11","Unknown&h12","OUTPUT_STREAM"
Data "INPUT_STREAM","SOUND_EFFECT"
' ---- src/decode.inc
' src/console.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Const con.SCREEN%=&h01
Const con.SERIAL%=&h02
Dim con.HEIGHT=50
Dim con.WIDTH=100
Dim con.buf$
Dim con.space
Dim con.lines
Dim con.count
Dim con.x=1
Dim con.fd_in
Dim con.fd_out
Dim con.more=1
Dim con.history%(255)
Dim con.spin_enabled
Dim con.spin_shown
Dim con.disable_flush%

Sub con.init(width%,height%,spin_enabled%)
 con.WIDTH=width%
 con.HEIGHT=height%
 con.spin_enabled=spin_enabled%
End Sub

Function con.in$(p$,echo)
 Local s$
 con.print(p$)
 con.flush()
 If con.fd_in Then
  Do While s$="" And Not Eof(con.fd_in)
   Line Input #con.fd_in,s$
   If InStr(s$,"#")=1 Or InStr(LCase$(s$),"*record")=1 Then s$=""
   If s$<>"" Then con.history_put(con.history%(),s$)
  Loop
  If Eof(con.fd_in) And s$="" Then s$="*replay off"
 Else
  s$=con.readln$("",con.WIDTH-con.x-Len(p$),con.history%())
 EndIf
 If con.fd_in Then con.println(s$)
 If Not con.fd_in Then con.lines=0 : con.count=1
 con.x=1
 If echo Then
  If con.fd_out Then
   If s$<>"" Then Print #con.fd_out,s$
  EndIf
 EndIf
 con.in$=s$
End Function

Sub con.print(s$)
 Local c$,i
 For i=1 To Len(s$)
  c$=Mid$(s$,i,1)
  Select Case c$
   Case Chr$(13) :
   Case Chr$(10) :
    con.endl()
    con.space=0
   Case " " :
    If Not con.space Then con.flush() : con.space=1
    Cat con.buf$," "
    con.count=0
   Case "`" :
    If con.space Then con.flush() : con.space=0
    Cat con.buf$,"'"
    con.count=0
   Case Else :
    If con.space Then con.flush() : con.space=0
    Cat con.buf$,c$
    con.count=0
  End Select
 Next
End Sub

Sub con.flush()
 If con.disable_flush% Then Exit Sub
 If con.spin_shown Then Print Chr$(8);" ";Chr$(8); : con.spin_shown=0
 Local remainder$
 If Len(con.buf$)>con.WIDTH Then
  remainder$="..."+Mid$(con.buf$,con.WIDTH+1)
  con.buf$=Left$(con.buf$,con.WIDTH)
 EndIf
 Do
  If con.x=1 And con.lines>con.HEIGHT-2 Then
   If con.more Then
    Print "[MORE] ";
    Do While Inkey$<>"" : Loop
    Do While Inkey$="" : Pause 1 : Loop
    Print
   EndIf
   con.lines=0
  EndIf
  If con.x+Len(con.buf$)>con.WIDTH+1 Then
   Print
   Inc con.lines
   con.x=1
   If con.space Then con.buf$="" : Exit Do
  Else
   Print con.buf$;
   Inc con.x,Len(con.buf$)
   con.buf$=""
   Exit Do
  EndIf
 Loop
 If Len(remainder$) Then
  con.buf$=remainder$
  con.flush()
 EndIf
End Sub

Sub con.endl()
 con.flush()
 If con.count<0 Then
  Exit Sub
 ElseIf con.count>=10 Then
  Local i
  For i=0 To con.HEIGHT-con.count-1 : Print : Next
  con.count=-999
  con.lines=0
  Exit Sub
 EndIf
 Print
 Inc con.count
 Inc con.lines
 con.x=1
End Sub

Sub con.println(s$,center)
 If Len(s$)>0 Then
  If center Then
   If con.x<>1 Then Error "Cannot center text unless on a new line."
   If Len(s$)<con.WIDTH Then con.print(Space$((con.WIDTH-Len(s$))\2)+s$)
  Else
   con.print(s$)
  EndIf
 EndIf
 con.endl()
End Sub

Sub con.print_file(f$,center)
 Local s$,w
 If center Then
  Open f$ For Input As #1
  Do
   Line Input #1,s$
   w=Max(w,Len(s$))
  Loop While Not Eof(#1)
  Close #1
 EndIf
 Open f$ For Input As #1
 Do
  Line Input #1,s$
  If center Then
   con.println(s$+Space$(w-Len(s$)),center)
  Else
   con.println(s$)
  EndIf
 Loop While Not Eof(#1)
 Close #1
End Sub

Sub con.open_in(fd,f$)
 con.close_in()
 Open f$ For Input As #fd
 con.fd_in=fd
End Sub

Sub con.close_in()
 If con.fd_in Then Close #con.fd_in
 con.fd_in=0
End Sub

Sub con.open_out(fd,f$)
 con.close_out()
 Open f$ For Output As #fd
 con.fd_out=fd
End Sub

Sub con.close_out()
 If con.fd_out Then Close #con.fd_out
 con.fd_out=0
End Sub

Sub con.bell()
 Local type%=con.get_type%()
 If type% And con.SCREEN% Then Play Tone 329.63,329.63,100
 If type% And con.SERIAL% Then
  con.set_type(con.SERIAL%)
  Print Chr$(7);
  con.set_type(type%)
 EndIf
End Sub

Function con.get_type%()
 Select Case LCase$(Mm.Info(Option Console))
  Case "both" : con.get_type%=con.SCREEN% Or con.SERIAL%
  Case "screen" : con.get_type%=con.SCREEN%
  Case "serial" : con.get_type%=con.SERIAL%
  Case Else : Error "Unknown console type"
 End Select
End Function

Sub con.set_type(type%)
 Select Case type%
  Case con.SCREEN% : Option Console Screen
  Case con.SERIAL%
   Option Console Serial
  Case &h03 : Option Console Both
  Case Else
   Error "Unknown console type"
 End Select
End Sub

Function con.history_get$(h%(),idx%)
 If idx%<0 Then Error "index out of bounds: "+Str$(idx%)
 Local h_addr%=Peek(VarAddr h%())
 Local h_size%=(Bound(h%(),1)-Bound(h%(),0)+1)*8
 Local i%,p%=h_addr%
 For i%=0 To idx%
  If Peek(Byte p%)=0 Then Exit Function
  If i%<idx% Then Inc p%,Peek(Byte p%)+1
 Next
 If p%+Peek(Byte p%)<h_addr%+h_size% Then
  Memory Copy p%,Peek(VarAddr con.history_get$),Peek(Byte p%)+1
 EndIf
End Function

Function con.history_find%(h%(),needle$)
 If needle$="" Then Error "invalid argument: needle$"
 Local i%=-1,s$
 Do
  Inc i%
  s$=con.history_get$(h%(),i%)
 Loop Until s$="" Or s$=needle$
 con.history_find%=Choice(s$="",-1,i%)
End Function

Sub con.history_put(h%(),s$)
 If s$="" Then Error "invalid empty string"
 Local h_addr%=Peek(VarAddr h%())
 Local h_size%=(Bound(h%(),1)-Bound(h%(),0)+1)*8
 If Peek(Byte h_addr%)>0 Then
  Local dst%=h_addr%+h_size%-1
  Local src%=dst%-Len(s$)-1
  Do While src%>=h_addr%
   Poke Byte dst%,Peek(Byte src%)
   Inc dst%,-1
   Inc src%,-1
  Loop
 EndIf
 Memory Copy Peek(VarAddr s$),h_addr%,Len(s$)+1
End Sub

Function con.readln$(initial$,max_len%,history%())
 Local ch$,hidx%,p%,old$,overwrite%,s$
 Local max_len_%=Choice(max_len%<1 Or max_len%>255,255,max_len%)
 con.readln$=initial$
 p%=Len(initial$)+1
 Print initial$;
 hidx%=Choice(initial$="",-1,con.history_find%(history%(),initial$))
 Local show%=1
 Local t%=Timer+500
 Do While Inkey$<>"" : Loop
 Do While Not sys.break_flag%
  If Timer>t% Then show%=Not show% : t%=Timer+500
  con.show_cursor(show%)
  ch$=Inkey$
  If ch$="" Then Pause 1 : Continue Do
  con.show_cursor(0)
  Select Case Asc(ch$)
   Case 8
    If p%=1 Then
     con.bell()
    Else
     s$=Mid$(con.readln$,p%)
     con.readln$=Left$(con.readln$,p%-2)+s$
     Print Chr$(&h08) s$ " " String$(Len(s$)+1,&h08);
     Inc p%,-1
    EndIf
   Case 10,13
    Print
    If con.readln$<>"" Then con.history_put(history%(),con.readln$)
    Exit Do
   Case 127
    If p%=Len(con.readln$)+1 Then
     con.bell()
    Else
     s$=Mid$(con.readln$,p%+1)
     con.readln$=Left$(con.readln$,p%-1)+s$
     Print s$ " " String$(Len(s$)+1,&h08);
    EndIf
   Case 128,129
    If hidx%=-1 Then old$=con.readln$
    Inc hidx%,Choice(Asc(ch$)=128,1,-1)
    If hidx%<=-1 Then
     hidx%=-1
     s$=old$
    Else
     s$=con.history_get$(history%(),hidx%)
     If s$="" Then s$=con.readln$ : Inc hidx%,-1
    EndIf
    Print String$(p%-1,&h08) String$(p%-1," ") String$(p%-1,&h08) s$;
    con.readln$=s$
    p%=Len(s$)+1
   Case 130
    If p%>1 Then
     Inc p%,-1
     Print Chr$(&h08);
    EndIf
   Case 131
    If p%<=Len(con.readln$) Then
     Print Mid$(con.readln$,p%,1);
     Inc p%
    EndIf
   Case 132
    overwrite%=Not overwrite%
   Case 134
    Print String$(p%-1,&h08);
    p%=1
   Case 135
    Do While p%<=Len(con.readln$)
     Print Mid$(con.readln$,p%,1);
     Inc p%
    Loop
   Case <32,>126
   Case Else
    If p%>Len(con.readln$) Then
     If Len(con.readln$)=max_len_% Then
      con.bell()
     Else
      Cat con.readln$,ch$
      Print ch$;
      Inc p%
     EndIf
    ElseIf overwrite% Then
     con.readln$=Left$(con.readln$,p%-1)+ch$+Mid$(con.readln$,p%+1)
     Print ch$;
     Inc p%
    Else
     If Len(con.readln$)=max_len_% Then
      con.bell()
     Else
      s$=Mid$(con.readln$,p%)
      con.readln$=Left$(con.readln$,p%-1)+ch$+s$
      Print ch$ s$ String$(Len(s$),&h08);
      Inc p%
     EndIf
    EndIf
  End Select
 Loop
 If sys.break_flag% Then con.readln$=""
End Function

Sub con.show_cursor(show%)
 If con.get_type%() And con.SCREEN% Then
  Local x%=Mm.Info(HPos),y%=Mm.Info(VPos)+Mm.Info(FontHeight)-1
  Line x%,y%,x%+Mm.Info(FontWidth),y%,1,Choice(show%,Mm.Info(FColour),Mm.Info(BColour))
 EndIf
End Sub

Sub con.spin()
 Static i
 If con.x<con.WIDTH-1 Then
  If con.spin_shown Then Print Chr$(8);Else con.spin_shown=1
  Print Mid$("\|/-",i+1,1);
  i=(i+1) Mod 4
 EndIf
End Sub

Sub con.set_font(size%)
 If size%=Mm.Info(Font) Then Exit Sub
 con.endl()
 Font size%
 con.WIDTH=Mm.HRes\Mm.Info(FontWidth)
 con.HEIGHT=Mm.VRes\Mm.Info(FontHeight)
 wb(&h20,con.HEIGHT)
 wb(&h21,con.WIDTH)
 If Mm.Info(Device X)="MMB4L" Then Console Resize con.WIDTH,con.HEIGHT
 Local x%,y%
 con.get_pos(x%,y%)
 Print @(x%*Mm.Info(FontWidth),(y%-1)*Mm.Info(FontHeight))
End Sub

Sub con.get_pos(x%,y%)
 con.flush()
 x%=Mm.Info(HPos)\Mm.Info(FontWidth)
 y%=Mm.Info(VPos)\Mm.Info(FontHeight)
 if x%<>con.x-1 Then Error "Mismatched x-values"
End Sub

Sub con.set_pos(x%,y%)
 con.flush()
 Print @(x%*Mm.Info(FontWidth),y%*Mm.Info(FontHeight));
 con.x=x%+1
End Sub

Sub con.invert()
 Static inverted=0
 inverted=Not inverted
 If con.get_type%()=con.SERIAL% Then
  Print vt100$(Choice(inverted,"7m","0m"));
 Else
  Const bg%=Mm.Info(BColour)
  Const fg%=Mm.Info(FColour)
  Colour bg%,fg%
 EndIf
End Sub

' ---- src/console.inc
' src/script.inc ++++
' Copyright (c) 2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Const script.FD_IN%=2
Const script.FD_OUT%=3

Sub script.record_on()
 Local f$,s$
 If con.fd_out<>0 Then con.println("Already recording script.") : Exit Sub
 If con.fd_in<>0 Then con.println("Cannot record whilst replaying script.") : Exit Sub
 con.println()
 con.println("Select script to record:")
 con.println()
 Local script%=script.select_script%()
 If script%<>0 Then f$=script.script_file$(script%)
 If script%<>0 And file.exists%(f$) Then
  s$=con.in$("Overwrite script "+Str$(script%)+" [y|N]? ")
  If LCase$(s$)<>"y" Then script%=0
 EndIf
 If script%<>0 Then
  s$=con.in$("Script name ? ")
  If s$="" Then script%=0
 EndIf
 If script%=0 Then con.println("Cancelled.") : Exit Sub
 con.println("Recording to '"+f$+"' ...")
 If file.mkdir%(file.get_parent$(f$))<>sys.SUCCESS Then Error sys.err$
 Const fd_out%=script.FD_OUT%
 con.open_out(fd_out%,f$)
 Print #fd_out%,"# " Date$ " " Time$
 Print #fd_out%,"# " s$
End Sub

Function script.select_script%()
 Local i,f$,s$
 Const fd%=script.FD_IN%
 For i=1 To 10
  f$=script.script_file$(i)
  con.print("  ["+Format$(i,"%2g")+"] ")
  If file.exists%(f$) Then
   Open f$ For Input As #fd%
   Line Input #fd%,s$
   con.print(Mid$(s$,3)+" - ")
   Line Input #fd%,s$
   con.println(Mid$(s$,3))
   Close #fd%
  Else
   con.println("Empty")
  EndIf
 Next i
 con.println()
 s$=con.in$("Script number ? ")
 Local script%=Val(s$)
 If script%<1 Or script%>10 Then script%=0
 script.select_script%=script%
End Function

Function script.script_file$(i)
 If i<1 Or i>10 Then Error "Invalid script number"
 Const name$=ss$(STORY_FILE)+"_"+Str$(i)+".scr"
 script.script_file$=file.resolve$(ss$(SCRIPT_DIR),ss$(STORY_FILE))
 script.script_file$=file.resolve$(script.script_file$,name$)
End Function

Sub script.record_off()
 If con.fd_out=0 Then con.println("A script is not being recorded!") : Exit Sub
 con.close_out()
 con.println("Recording stopped.")
End Sub

Sub script.replay_on()
 If con.fd_out<>0 Then con.println("Cannot replay whilst recording script.") : Exit Sub
 If con.fd_in<>0 Then con.println("Already replaying script.") : Exit Sub
 con.println()
 con.println("Select script to replay:")
 con.println()
 Local script%=script.select_script%()
 If script%<>0 Then
  Local f$=script.script_file$(script%)
  If Not file.exists%(f$) Then script%=0
 EndIf
 If script%=0 Then con.println("Cancelled.") : Exit Sub
 con.println("Replaying from '"+f$+"' ...")
 con.open_in(script.FD_IN%,f$)
End Sub

Sub script.replay_off()
 If con.fd_in=0 Then con.println("A script is not being replayed!") : Exit Sub
 con.close_in()
 con.println("Replaying stopped.")
End Sub

' ---- src/script.inc
' src/objects.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Function ob_attr(o,a,s,x)
 Local ad,mask,y
 If o=0 Then Exit Function
 ad=rw(&h0A)+62+(o-1)*9+a\8
 y=LGetByte(m(),ad)
 mask=2^(7-a Mod 8)
 If s=0 Then ob_attr=(y And mask)>0 : Exit Function
 If x=0 Then y=(y And (mask Xor &hFF)) Else y=(y Or mask)
 Poke Byte mad+ad,y
 ob_attr=x
End Function

Function ob_rel(o,r,s,x)
 Local ad
 ad=rw(&h0A)+62+(o-1)*9+r
 If s=0 Then ob_rel=LGetByte(m(),ad) : Exit Function
 Poke Byte mad+ad,x
 ob_rel=x
End Function

Function ob_next_prop(o,p)
 Local ad,x
 If o=0 Then
  Exit Function
 ElseIf p=0 Then
  ad=ob_prop_base(o)
  Inc ad,1+2*LGetByte(m(),ad)
 Else
  ad=ob_prop_addr(o,p)
  If ad=0 Then Error "Property does not exist"
  x=LGetByte(m(),ad-1)
  Inc ad,1+x\32
 EndIf
 x=LGetByte(m(),ad)
 ob_next_prop=x And &b11111
End Function

Function ob_prop_len(ad)
 If ad=0 Then Exit Function
 Const x=LGetByte(m(),ad-1)
 ob_prop_len=x\32+1
End Function

Function ob_prop_base(o)
 Const ad=rw(&h0A)+62+(o-1)*9+7
 ob_prop_base=LGetByte(m(),ad)*256+LGetByte(m(),ad+1)
End Function

Function ob_prop_addr(o,p)
 Local ad,x
 ad=ob_prop_base(o)
 Inc ad,1+2*LGetByte(m(),ad)
 Do
  x=LGetByte(m(),ad)
  If (x And &b11111)=p Then ob_prop_addr=ad+1 : Exit Function
  If (x And &b11111)<p Then ob_prop_addr=0 : Exit Function
  Inc ad,2+x\32
 Loop
End Function

Function ob_prop_get(o,p)
 Local ad,sz,x
 ad=ob_prop_addr(o,p)
 If ad>0 Then
  x=LGetByte(m(),ad-1)
  If (x And &b11111)<>p Then Error
  sz=x\32+1
  Select Case sz
   Case 1
    ob_prop_get=LGetByte(m(),ad)
   Case 2
    ob_prop_get=LGetByte(m(),ad)*256+LGetByte(m(),ad+1)
   Case Else
    Error "Property length > 2"
  End Select
  Exit Function
 EndIf
 ad=rw(&h0A)+2*(p-1)
 ob_prop_get=LGetByte(m(),ad)*256+LGetByte(m(),ad+1)
End Function

Sub ob_prop_set(o,p,x)
 Local ad,sz
 ad=ob_prop_addr(o,p)
 If ad=0 Then Error "Object "+Str$(o)+" does not have property "+Str$(p)
 Select Case ob_prop_len(ad)
  Case 1
   wb(ad,x And &hFF)
  Case 2
   Poke Byte mad+ad,x\256 : Poke Byte mad+ad+1,x Mod 256
  Case Else
   Error "Object "+Str$(o)+" has length "+Str$(ob_prop_len(ad))
 End Select
End Sub

Sub ob_print(o)
 Const ad=ob_prop_base(o)+1
 print_zstring(ad)
End Sub

Sub ob_insert(o,d)
 Local c,x
 ob_remove(o)
 c=ob_rel(d,6)
 x=ob_rel(d,6,1,o)
 x=ob_rel(o,4,1,d)
 x=ob_rel(o,5,1,c)
End Sub

Sub ob_remove(o)
 Local c,p,s,x
 p=ob_rel(o,4)
 s=ob_rel(o,5)
 c=ob_rel(p,6)
 x=ob_rel(o,4,1,0)
 x=ob_rel(o,5,1,0)
 If o=c Then
  x=ob_rel(p,6,1,s)
 Else
  Do
   If ob_rel(c,5)=o Then x=ob_rel(c,5,1,s) : Exit Do
   c=ob_rel(c,5)
  Loop Until c=0
 EndIf
End Sub

' ---- src/objects.inc
' src/execute.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Const E_OK=0
Const E_UNKNOWN=1
Const E_UNIMPLEMENTED=2
Const E_BREAK=3
Const E_QUIT=4
Const E_DEBUG=5
Const E_REPEAT=6
Dim fp
Dim num_bp
Dim num_ops
Dim ztrace

Function exec(tr)
 Local op,pc_old,sp_old
 pc_old=pc : sp_old=sp
 op=decode(tr)
 Inc num_ops
 Select Case op
  Case <&h80
   exec=ex_2op()
  Case <&hB0
   exec=ex_1op()
  Case <&hC0
   exec=ex_0op()
  Case <&hE0
   exec=ex_2op()
  Case Else
   exec=ex_varop()
 End Select
 Select Case exec
  Case E_UNKNOWN
   con.println("Unsupported instruction "+fmt_hex$(op,2))
  Case E_UNIMPLEMENTED
   con.println("Unimplemented instruction "+fmt_hex$(op,2))
 End Select
 If exec<>E_OK Then pc=pc_old : sp=sp_old
End Function

Function ex_2op()
 Local a,b,x,y
 a=oa(0)
 b=oa(1)
 Select Case oc
  Case &h1
   x=(a=b)
   If (Not x) And (onum>2) Then x=(a=oa(2))
   If (Not x) And (onum>3) Then x=(a=oa(3))
   ex_branch(x,br)
  Case &h2
   If a>32767 Then Inc a,-65536
   If b>32767 Then Inc b,-65536
   ex_branch(a<b,br)
  Case &h3
   If a>32767 Then Inc a,-65536
   If b>32767 Then Inc b,-65536
   ex_branch(a>b,br)
  Case &h4
   x=vget(a)
   If x>32767 Then Inc x,-65536
   If b>32767 Then Inc b,-65536
   Inc x,-1
   y=x<b
   If x<0 Then Inc x,65536
   vset(a,x)
   ex_branch(y,br)
  Case &h5
   x=vget(a)
   If x>32767 Then Inc x,-65536
   If b>32767 Then Inc b,-65536
   Inc x
   y=x>b
   If x<0 Then Inc x,65536
   vset(a,x)
   ex_branch(y,br)
  Case &h6
   x=ob_rel(a,4)
   ex_branch(x=b,br)
  Case &h7
   ex_branch((a And b)=b,br)
  Case &h8
   vset(st,a Or b)
  Case &h9
   vset(st,a And b)
  Case &hA
   x=ob_attr(a,b)
   ex_branch(x=1,br)
  Case &hB
   x=ob_attr(a,b,1,1)
  Case &hC
   x=ob_attr(a,b,1,0)
  Case &hD
   If a=0 Then stack(sp-1)=b Else vset(a,b)
  Case &hE
   ob_insert(a,b)
  Case &hF
   x=rw(a+2*b)
   vset(st,x)
  Case &h10
   x=LGetByte(m(),a+b)
   vset(st,x)
  Case &h11
   x=ob_prop_get(a,b)
   vset(st,x)
  Case &h12
   x=ob_prop_addr(a,b)
   vset(st,x)
  Case &h13
   x=ob_next_prop(a,b)
   vset(st,x)
  Case <&h19
   If a>32767 Then Inc a,-65536
   If b>32767 Then Inc b,-65536
   Select Case oc
    Case &h14
     x=a+b
    Case &h15
     x=a-b
    Case &h16
     x=a*b
    Case &h17
     x=a\b
    Case Else
     x=a Mod b
   End Select
   If x<0 Then Inc x,65536
   vset(st,x)
  Case Else
   ex_2op=E_UNKNOWN
 End Select
End Function

Function ex_1op()
 Local a,x
 a=oa(0)
 Select Case oc
  Case &h0
   ex_branch(a=0,br)
  Case &h1
   x=ob_rel(a,5)
   vset(st,x)
   ex_branch(x<>0,br)
  Case &h2
   x=ob_rel(a,6)
   vset(st,x)
   ex_branch(x<>0,br)
  Case &h3
   x=ob_rel(a,4)
   vset(st,x)
  Case &h4
   x=ob_prop_len(a)
   vset(st,x)
  Case &h5
   x=vget(a)
   If x>32767 Then Inc x,-65536
   Inc x
   If x<0 Then Inc x,65536
   vset(a,x)
  Case &h6
   x=vget(a)
   If x>32767 Then Inc x,-65536
   Inc x,-1
   If x<0 Then Inc x,65536
   vset(a,x)
  Case &h7
   print_zstring(a)
  Case &h9
   ob_remove(a)
  Case &hA
   ob_print(a)
  Case &hB
   ex_return(a)
  Case &hC
   If a And &h8000 Then Inc a,-65536
   Inc pc,a-2
  Case &hD
   print_zstring(a*2)
  Case &hE
   If a=0 Then x=stack(sp-1) Else x=vget(a)
   vset(st,x)
  Case &hF
   x=a Xor &b1111111111111111
   vset(st,x)
  Case Else
   ex_1op=E_UNKNOWN
 End Select
End Function

Function ex_0op()
 Local x
 Select Case oc
  Case &h0
   ex_return(1)
  Case &h1
   ex_return(0)
  Case &h2
   print_zstring(pc)
  Case &h3
   print_zstring(pc)
   con.endl()
   ex_return(1)
  Case &h4
  Case &h5
   If con.fd_in Then
    con.println("IGNORED 'save' command read from script")
   Else
    x=zsave()
    ex_branch(x,br)
   EndIf
  Case &h6
   If con.fd_in Then
    con.println("IGNORED 'restore' command read from script")
   Else
    x=zsave(1)
   EndIf
  Case &h7
   If con.fd_in Then
    con.println("IGNORED 'restart' command read from script")
   Else
    main_init()
    For x=0 To 10 : con.endl() : Next
   EndIf
  Case &h8
   x=stack(sp-1) : Inc sp,-1
   ex_return(x)
  Case &h9
   Inc sp,-1
  Case &hA
   ex_0op=E_QUIT
  Case &hB
   con.endl()
  Case &hC
   ex_show_status()
  Case &hD
   ex_branch(1,br)
  Case Else
   ex_0op=E_UNKNOWN
 End Select
End Function

Function ex_varop()
 Local x
 Select Case oc
  Case &h0
   ex_call(st)
  Case &h1
   ww(oa(0)+2*oa(1),oa(2))
  Case &h2
   wb(oa(0)+oa(1),oa(2))
  Case &h3
   ob_prop_set(oa(0),oa(1),oa(2))
  Case &h4
   ex_varop=ex_read(oa(0),oa(1))
  Case &h5
   con.print(Chr$(oa(0)))
  Case &h6
   x=oa(0)
   If x>32767 Then Inc x,-65536
   con.print(Str$(x))
  Case &h7
   x=oa(0)
   If x>32767 Then Inc x,-65536
   x=ex_random(x)
   vset(st,x)
  Case &h8
   stack(sp)=oa(0) : Inc sp
  Case &h9
   x=stack(sp-1) : Inc sp,-1
   If oa(0)=0 Then stack(sp-1)=x Else vset(oa(0),x)
  Case &h13
   ex_varop=E_UNIMPLEMENTED
  Case &h14
   ex_varop=E_UNIMPLEMENTED
  Case &h15
  Case Else
   ex_varop=E_UNKNOWN
 End Select
End Function

Sub ex_branch(z,br)
 If Not(z=(br And &h80000)>0) Then Exit Sub
 Const x=br And &h7FFFF
 Select Case x
  Case pc-1
   ex_return(1)
  Case pc-2
   ex_return(0)
  Case Else
   pc=x
 End Select
End Sub

Sub ex_return(x)
 sp=fp+4
 pc=&h10000*stack(sp-1) : Inc sp,-1
 Inc pc,stack(sp-1) : Inc sp,-1
 Const st=stack(sp-1) : Inc sp,-1
 fp=stack(sp-1) : Inc sp,-1
 vset(st,x)
 If ztrace Then dmp_stack()
End Sub

Sub ex_call(st)
 Local i,nl,x
 If oa(0)=0 Then vset(st,0) : Exit Sub
 stack(sp)=fp : Inc sp
 fp=sp-1
 stack(sp)=st : Inc sp
 st_push(pc Mod &h10000)
 st_push(pc\&h10000)
 pc=2*oa(0)
 nl=LGetByte(m(),pc) : Inc pc
 stack(sp)=nl : Inc sp
 For i=1 To nl
  x=LGetByte(m(),pc)*256+LGetByte(m(),pc+1) : Inc pc,2
  If i<onum Then st_push(oa(i)) Else st_push(x)
 Next
 If ztrace Then dmp_routine(2*oa(0)) : dmp_stack()
End Sub

Function ex_read(text_buf,parse_buf)
 Local ad,c,i,n,word$,s$,t,wc
 ex_show_status()
 t=Timer
 s$=LCase$(con.in$("> ",1))
 If Left$(s$,1)="*" Then ex_read=ex_special(s$)
 Timer=t
 If ex_read<>E_OK Then Exit Function
 n=Len(s$)
 For i=1 To n : wb(text_buf+i,Peek(Var s$,i)) : Next
 wb(text_buf+n+1,0)
 Cat s$," "
 For i=1 To n+1
  c=Peek(Var s$,i)
  If c=&h20 Or Instr(DICT_SEP$(0),Chr$(c))>0 Then
   If Len(word$)>0 Then
    ad=di_lookup(word$)
    ww(parse_buf+2+wc*4,ad)
    wb(parse_buf+4+wc*4,Len(word$))
    wb(parse_buf+5+wc*4,i-Len(word$))
    Inc wc
    word$=""
   EndIf
   If c<>&h20 Then
    ad=di_lookup(Chr$(c))
    ww(parse_buf+2+wc*4,ad)
    wb(parse_buf+4+wc*4,1)
    wb(parse_buf+5+wc*4,i-1)
    Inc wc
   EndIf
  Else
   Cat word$,Chr$(c)
  EndIf
 Next
 wb(parse_buf+1,wc)
End Function

Function ex_special(cmd$)
 ex_special=E_REPEAT
 Select Case Field$(cmd$,1," ")
  Case "*break"
   ex_special=E_BREAK
  Case "*credits"
   con.print_file(file.resolve$(ss$(RESOURCES_DIR),"credits.txt"),1)
  Case "*eof"
   con.println("End of script.")
   con.endl()
  Case "*font"
   ex_special=ex_font(cmd$)
  Case "*more"
   Select Case Field$(cmd$,2," ")
    Case "","on"
     con.more=1
     con.println("Paged output enabled.")
    Case "off"
     con.more=0
     con.println("Paged output disabled.")
    Case Else
     con.println("Invalid '*more' command.")
   End Select
   con.endl()
  Case "*record"
   Select Case Field$(cmd$,2," ")
    Case "","on"
     script.record_on()
    Case "off"
     script.record_off()
    Case Else
     con.println("Invalid '*record' command.")
   End Select
   con.endl()
  Case "*replay"
   Select Case Field$(cmd$,2," ")
    Case "","on"
     script.replay_on()
    Case "off"
     script.replay_off()
    Case Else
     con.println("Invalid '*replay' command.")
   End Select
   con.endl()
  Case "*screenshot"
   ex_screenshot()
  Case "*spin"
   Select Case Field$(cmd$,2," ")
    Case "","on"
     con.spin_enabled=1
     con.println("Progress spinner enabled.")
    Case "off"
     con.spin_enabled=0
     con.println("Progress spinner disabled.")
    Case Else
     con.println("Invalid '*spin' command.")
   End Select
   con.endl()
  Case "*status"
   ex_show_status(1)
  Case "*save"
   If zsave(0) Then con.println("Ok.") Else con.println("Save failed.")
   con.endl()
  Case "*restore"
   If zsave(1) Then con.println("Ok.") Else con.println("Restore failed.")
   con.endl()
  Case Else
   ex_special=E_OK
 End Select
 If ex_special=E_REPEAT Then con.print(">")
End Function

Function ex_random(range)
 Static x=7
 Static a=1103515245
 Static c=12345
 Static m=2^31
 Select Case range
  Case 0
   x=Timer
   Exit Function
  Case <0
   x=Abs(range)
   Exit Function
 End Select
 x=(a*x+c) Mod m
 ex_random=1+CInt((range-1)*(x/m))
End Function

Sub ex_show_status(on_console%)
 Local s$(1) Length 120,x,y
 con.get_pos(x,y)
 con.disable_flush%=1
 ob_print(vget(&h10))
 s$(0)=Left$(con.buf$,120)
 con.buf$=""
 con.disable_flush%=0
 If Len(s$(0))>con.WIDTH Then s$(0)=Left$(s$(0),con.WIDTH-3)+"..."
 If rb(&h01) And &b00000010 Then
  Local hours%=vget(&h11) Mod 24
  Const mins%=vget(&h12) Mod 60
  Const am%=hours%<12
  Select Case hours%
   Case 0 : hours%=12
   Case >12 : Inc hours%,-12
  End Select
  s$(1)="Time: "+Str$(hours% Mod 13)+":"+Str$(mins%,2,0,"0")+Choice(am%,"am","pm")
 Else
  s$(1)="Score: "+Str$(vget(&h11))+", Moves: "+Str$(vget(&h12))
 EndIf
 If on_console% Then
  con.println(s$(0)+", "+s$(1))
  con.println
 EndIf
 s$(0)=" "+s$(0)
 s$(1)=s$(1)+" "
 If Len(s$(0))+Len(s$(1))+1>=con.WIDTH Then
  If rb(&h01) And &b00000010 Then
   s$(1)=Str$(vget(&h11))+":"+Str$(vget(&h12),2,0,"0")+" "
  Else
   s$(1)=Str$(vget(&h11))+"/"+Str$(vget(&h12))+" "
  EndIf
 EndIf
 If Len(s$(0))+Len(s$(1))+1>=con.WIDTH Then s$(1)=""
 con.set_pos(0,0)
 con.invert()
 con.print(s$(0))
 con.print(Space$(con.WIDTH-Len(s$(0))-Len(s$(1))))
 con.println(s$(1))
 con.set_pos(x,y)
 con.invert()
End Sub

Sub ex_screenshot()
 Local f$,i=0,root$=Cwd$
 Do
  Inc i
  f$=file.resolve$(root$,ss$(STORY_FILE)+"_"+Str$(i)+".bmp")
 Loop While file.exists%(f$)
 If InStr(Mm.Device$,"PicoMite") Then
  On Error Skip
  Save Compressed Image f$
 Else
  On Error Skip
  Save Image f$
 EndIf
 If Mm.ErrNo Then
  con.println("Screenshot failed or is not supported.")
 Else
  con.println("Saved '"+f$+"'")
 EndIf
 con.endl()
End Sub

Function ex_font(cmd$)
 ex_font=E_REPEAT
 If Field$(cmd$,3," ")<>"" Then
  con.println("Invalid '*font' command.")
  con.endl()
 Else
  Select Case Field$(cmd$,2," ")
   Case "small"
    con.set_font(7)
    ex_font=E_OK
   Case "medium"
    con.set_font(1)
    ex_font=E_OK
   Case Else
    con.println("Unknown font.")
    con.endl()
  End Select
 EndIf
 if ex_font=E_OK Then cmd$="look"
End Function

' ---- src/execute.inc
' src/zstring.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Dim ALPHABET$(2) LENGTH 32
ALPHABET$(0)=" 123[]abcdefghijklmnopqrstuvwxyz"
ALPHABET$(1)=" 123[]ABCDEFGHIJKLMNOPQRSTUVWXYZ"
ALPHABET$(2)=" 123[]@^0123456789.,!?_#'"+Chr$(34)+"/\-:()"

Sub print_zstring(ad)
 Local b,c,i,s,x,zc(2)
 For x=0 To 0 Step 0
  x=LGetByte(m(),ad)*256+LGetByte(m(),ad+1)
  zc(0)=(x And &h7C00)\&h400
  zc(1)=(x And &h3E0)\&h20
  zc(2)=(x And &h1F)
  x=x\&h8000
  For i=0 To 2
   c=zc(i)
   If s<3 Then
    If c=0 Then
     con.print(" ")
    ElseIf c<4 Then
     s=c+2
    ElseIf c<6 Then
     s=c-3
    Else
     If c=6 And s=2 Then
      s=6
     ElseIf c=7 And s=2 Then
      con.endl()
      s=0
     Else
      con.print(Mid$(ALPHABET$(s),c+1,1))
      s=0
     EndIf
    EndIf
   ElseIf s<6 Then
    b=ad
    print_abrv((s-3)*32+c)
    ad=b
    s=0
   ElseIf s=6 Then
    s=c+7
   Else
    con.print(Chr$((s-7)*32+c))
    s=0
   EndIf
  Next i
  Inc ad,2
 Next x
End Sub

Sub print_abrv(x)
 Local a,b
 a=rw(&h18)
 b=rw(a+x*2)
 print_zstring(b*2)
End Sub

' ---- src/zstring.inc
' src/util.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Function fmt_hex$(x,i)
 If i<1 Then i=4
 fmt_hex$="&h"+str.lpad$(Hex$(x),i,"0")
End Function

' ---- src/util.inc
' src/dict.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Dim DICT_AD
Dim DICT_SEP$(1) Length 5
Dim DICT_ENTRY_LEN
Dim DICT_ENTRY_COUNT
Dim DICT_ENTRY_AD

Sub di_init()
 Local i,ns
 DICT_AD=rw(&h08)
 ns=rb(DICT_AD)
 Poke Var DICT_SEP$(0),0,ns
 For i=1 To ns : Poke Var DICT_SEP$(0),i,rb(DICT_AD+i) : Next
 DICT_ENTRY_LEN=rb(DICT_AD+ns+1)
 DICT_ENTRY_COUNT=rw(DICT_AD+ns+2)
 DICT_ENTRY_AD=DICT_AD+ns+4
End Sub

Function di_lookup(w$)
 Local c,i,j,nz,sz,z(9)
 sz=Len(w$) : If sz>6 Then sz=6
 i=1
 Do While i<7 And nz<7
  If i>sz Then
   z(nz)=5 : Inc nz
  Else
   c=Asc(Mid$(w$,i,1))
   j=Instr(ALPHABET$(0),Chr$(c))-1
   If j>-1 Then
    z(nz)=j : Inc nz
   Else
    j=Instr(ALPHABET$(2),Chr$(c))-1
    If j>-1 Then
     z(nz)=5 : Inc nz
     z(nz)=j : Inc nz
    Else
     z(nz)=5 : Inc nz
     z(nz)=6 : Inc nz
     z(nz)=c\32 : Inc nz
     z(nz)=c And &b11111 : Inc nz
    EndIf
   EndIf
  EndIf
  Inc i
 Loop
 Local x(1)
 x(0)=z(0)*1024+z(1)*32+z(2)
 x(1)=z(3)*1024+z(4)*32+z(5)+32768
 Local ad,lb,ub,y(1)
 lb=0
 ub=DICT_ENTRY_COUNT-1
 Do
  i=(lb+ub)\2
  ad=DICT_ENTRY_AD+DICT_ENTRY_LEN*i
  y(0)=LGetByte(m(),ad)*256+LGetByte(m(),ad+1)
  y(1)=rw(ad+2)
  If x(0)>y(0) Then
   lb=i+1
  ElseIf x(0)<y(0) Then
   ub=i-1
  ElseIf x(1)>y(1) Then
   lb=i+1
  ElseIf x(1)<y(1) Then
   ub=i-1
  Else
   di_lookup=ad
   ub=lb-1
  EndIf
 Loop Until ub<lb
End Function

' ---- src/dict.inc
' src/zsave.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Function zsave(res)
 Local exists(10),i,old_dir$,s$
 If InStr(Mm.Info(Device X),"PicoMite") Then
  Local s2$(2) Length 64
 Else
  Local s2$(2)
 EndIf
 If res Then
  con.println("Select game to restore:")
 Else
  con.println("Select save game slot:")
 EndIf
 old_dir$=Cwd$
 ChDir file.resolve$(ss$(SAVE_DIR),ss$(STORY_FILE))
 For i=1 To 10
  s$=Dir$("game"+Str$(i)+".sav")
  con.print("  ["+Format$(i,"%2g")+"] ")
  If s$="" Then
   con.println("Empty")
  Else
   exists(i)=1
   Open "game"+Str$(i)+".sav" For Input As #1
   Line Input #1,s2$(0)
   Line Input #1,s2$(1)
   Line Input #1,s2$(2)
   Line Input #1,s$
   con.println(s2$(2)+" - "+s$)
   Close #1
  EndIf
 Next
 ChDir old_dir$
 s$=con.in$("Game number? ")
 i=Val(s$)
 If i<1 Or i>10 Then i=0
 If i>0 And res And Not exists(i) Then i=0
 If i>0 And Not res And exists(i) Then
  s$=con.in$("Overwrite game "+Str$(i)+" [y|N]? ")
  If LCase$(s$)<>"y" Then i=0
 EndIf
 If i>0 And Not res Then
  s$=con.in$("Save game name? ")
  If s$="" Then i=0
 EndIf
 If i=0 Then con.println("Cancelled") : Exit Function
 s2$(0)=file.resolve$(ss$(SAVE_DIR),ss$(STORY_FILE))
 s2$(0)=file.resolve$(s2$(0),"game"+Str$(i)+".sav")
 If res Then
  Open s2$(0) For Input As #1
  Line Input #1,s2$(0)
  Line Input #1,s2$(1)
  Line Input #1,s2$(2)
  Line Input #1,s$
  con.println("Restoring '"+s$+"' ...")
  Local ad,err,new_pc,new_fp,stack_sz,mem_sz
  s$=Input$(9,#1)
  new_pc=Peek(Var s$,1)*65536+Peek(Var s$,2)*256+Peek(Var s$,3)
  new_fp=Peek(Var s$,4)*256+Peek(Var s$,5)
  stack_sz=Peek(Var s$,6)*256+Peek(Var s$,7)
  mem_sz=Peek(Var s$,8)*256+Peek(Var s$,9)
  If new_pc<0 Or new_pc>=FILE_LEN Then err=1
  If new_fp<0 Or new_fp>stack_sz Then err=2
  If stack_sz<0 Or stack_sz>511 Then err=3
  If mem_sz<>BASE_STATIC Then err=4
  If err<>0 Then
   con.print("Save file is invalid (error "+Str$(err)+")")
   Close #1
   Exit Function
  EndIf
  pc=new_pc
  fp=new_fp
  sp=0
  For i=0 To stack_sz-1
   s$=Input$(2,#1)
   st_push(Peek(Var s$,1)*256+Peek(Var s$,2))
  Next
  Do
   s$=Input$(255,#1)
   For i=1 To Len(s$)
    wb(ad,Peek(Var s$,i))
    Inc ad
   Next
  Loop Until Len(s$)=0
  If ad<>BASE_STATIC Then Error "Unrecoverable restore error!"
 Else
  con.println("Saving '"+s$+"' ...")
  Open s2$(0) For Output As #1
  Print #1,"ZMIM save file"
  Print #1,"1"
  Print #1,Date$+" "+Time$
  Print #1,s$
  Print #1,Chr$(pc\65536);Chr$(pc\256);Chr$(pc Mod 256);
  Print #1,Chr$(fp\256);Chr$(fp Mod 256);
  Print #1,Chr$(sp\256);Chr$(sp Mod 256);
  Print #1,Chr$(BASE_STATIC\256);Chr$(BASE_STATIC Mod 256);
  For i=0 To sp-1
   Print #1,Chr$(st_peek(i)\256);Chr$(st_peek(i) Mod 256);
  Next
  For i=0 To BASE_STATIC-1
   Print #1,Chr$(rb(i));
  Next
 EndIf
 Close #1
 zsave=1
End Function

' ---- src/zsave.inc
' src/zfile.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Function fi_choose$(d$,fspec$)
 Local f$,i,j,nc,nr,old_dir$,sz,width,x
 old_dir$=Cwd$
 ChDir d$
 f$=Dir$(fspec$)
 Do While f$<>""
  If Left$(f$,1)<>"." Then
   Inc sz
   If Len(f$)>width Then width=Len(f$)
  EndIf
  f$=Dir$()
 Loop
 If sz=0 Then con.println("No files found") : ChDir old_dir$ : Exit Function
 Local all$(sz) LENGTH width
 all$(sz)=Chr$(&h7F)
 f$=Dir$(fspec$)
 i=0
 Do
  If Left$(f$,1)<>"." Then all$(i)=f$ : Inc i
  f$=Dir$()
 Loop Until i=sz
 Sort all$()
 ChDir old_dir$
 If (sz<8 Or con.WIDTH<=40) Then nc=1 Else nc=2
 nr=CInt(sz/nc+0.4999)
 Inc width,10
 For i=0 To nr-1
  For j=0 to nc-1
   con.flush()
   x=(j*nr)+i
   If x<sz Then
    If j*width>Pos Then con.print(Space$(j*width-Pos))
    con.print("  ["+Format$(x+1,"%2g")+"] "+all$(x))
   EndIf
  Next
  con.endl()
 Next
 f$=con.in$("File number? ")
 If Val(f$)>0 And Val(f$)<=sz Then fi_choose$=file.resolve$(d$,all$(Val(f$)-1))
End Function

' ---- src/zfile.inc
' src/debug.inc ++++
' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
Dim bp(9)

Function debug()
 Local a,b,c,cmd$(9) Length 20,cn,i,op,pc_old,s$,t,sp_old
 pc_old=pc : sp_old=sp
 op=decode(1)
 pc=pc_old : sp=sp_old
 Do
  cn=0
  For i=0 To 9 : cmd$(i)="" : Next
  t=Timer
  s$=con.in$("DEBUG >> ")+" "
  Timer=t
  For i=1 To Len(s$)
   c=Peek(Var s$,i)
   If Chr$(c)=" " Then
    If Len(cmd$(cn))>0 Then cn=cn+1
    If cn=10 Then Error "Too many arguments"
   Else
    cmd$(cn)=cmd$(cn)+Chr$(c)
   EndIf
  Next
  debug=E_DEBUG
  Select Case cmd$(0)
   Case "abrv"
    dmp_abrv()
   Case "b"
    a=Val(cmd$(1))
    If a>=0 And a<FILE_LEN Then
     For i=0 To 9
      If bp(i)=a Then
       con.println("Duplicate breakpoint ["+Str$(i)+"]")
       a=-1
       Exit For
      EndIf
     Next
     For i=0 To 9
      If a=-1 Then
       Exit For
      ElseIf bp(i)=-1 Then
       bp(i)=a
       num_bp=num_bp+1
       con.println("Set breakpoint ["+Str$(i)+"] at "+fmt_hex$(bp(i)))
       Exit For
      EndIf
      If i=9 Then con.println("No free address breakpoints")
     Next
    Else
     con.println("Invalid breakpoint address")
    EndIf
   Case "B"
    If num_bp=0 Then
     con.println("No address breakpoints set")
    Else
     For i=0 To 9
      If bp(i)<>-1 Then
       con.println("["+Str$(i)+"] "+fmt_hex$(bp(i)))
      EndIf
     Next
    EndIf
   Case "c"
    con.endl()
    If oc=&h4 And op>=&hE0 Then con.print(">")
    debug=E_OK
   Case "C"
    dmp_stack(Val(cmd$(1)))
   Case "d"
    If Len(cmd$(1))=0 Then a=pc Else a=Val(cmd$(1))
    dmp_mem(a,Val(cmd$(2)))
   Case "dict"
    dmp_dict()
   Case "dmap"
    dmp_mmap()
   Case "G"
    dmp_global(Val(cmd$(1)),Val(cmd$(2)))
   Case "h"
    If con.WIDTH<80 Then
     con.print_file(file.resolve$(ss$(RESOURCES_DIR),"debug_help_40col.txt"))
    Else
     con.print_file(file.resolve$(ss$(RESOURCES_DIR),"debug_help.txt"))
    EndIf
   Case "H"
    dmp_hdr()
   Case "q"
    debug=E_QUIT
   Case "o"
    dmp_obj(Val(cmd$(1)))
   Case "s"
    If oc=&h4 And op>=&hE0 Then con.print(">")
    debug=exec(0)
    If debug=E_OK Then debug=E_BREAK
   Case "troff"
    con.println("Trace OFF")
    ztrace=0
   Case "tron"
    con.println("Trace ON")
    ztrace=1
   Case "v"
    a=Val(cmd$(1))
    If a<0 Or a>9 Then
     con.println("Invalid address breakpoint")
    ElseIf bp(a)=-1 Then
     con.println("Address breakpoint ["+Str$(a)+"] already cleared")
    Else
     bp(a)=-1
     num_bp=num_bp-1
     con.println("Address breakpoint ["+Str$(a)+"] cleared")
    EndIf
   Case "V"
    a=di_lookup(LCase$(cmd$(1)))
    con.println(fmt_hex$(a))
   Case "x"
    a=Val(cmd$(1))
    con.print(Str$(a))
    con.print("  "+fmt_hex$(a))
    con.println("  &b"+str.lpad$(Bin$(a),16,"0"))
   Case "z"
    For i=0 To 9 : bp(i)=-1 : Next
    num_bp=0
    con.println("All breakpoints cleared")
   Case Else
    con.println("Unknown debug command")
  End Select
 Loop While debug=E_DEBUG
End Function

Sub dmp_abrv()
 Local i,j,nc,nr,width,x
 nc=3 : nr=CInt(96/nc+0.4999) : width=24
 For i=0 To nr-1
  For j=0 To nc-1
   con.flush()
   x=(j*nr)+i
   If x<96 Then
    If j*width>Pos Then con.print(Space$(j*width-Pos))
    con.print("["+str.lpad$(Str$(x),2)+"] {")
    print_abrv(x)
    con.print "}"
   EndIf
  Next
  con.endl()
 Next
End Sub

Sub dmp_dict()
 Local a,i,j,nc,nr,width,x
 con.println("Word separators = {"+DICT_SEP$(0)+"}")
 con.println("Word count      = "+Str$(DICT_ENTRY_COUNT))
 con.println("Word size       = "+Str$(DICT_ENTRY_LEN))
 con.endl()
 nc=3 : nr=CInt(DICT_ENTRY_COUNT/nc+0.4999) : width=24
 For i=0 To nr-1
  For j=0 To nc-1
   con.flush()
   x=(j*nr)+i
   If x<DICT_ENTRY_COUNT Then
    If j*width>Pos Then con.print(Space$(j*width-Pos))
    con.print("["+str.lpad$(Str$(x+1),4)+"] ")
    a=x*DICT_ENTRY_LEN+DICT_ENTRY_AD
    x=rw(a) : con.print(str.lpad$(Hex$(x),4,"0"))
    x=rw(a+2) : con.print(str.lpad$(Hex$(x),4,"0")+" ")
    print_zstring(a)
   EndIf
  Next
  con.endl()
 Next
End Sub

Sub dmp_global(a,n)
 Local i,x
 If n<1 Then n=1
 If a>239 Then a=239
 For i=a To a+n-1
  If i>239 Then Exit For
  x=vget(i+16)
  con.print("G"+str.lpad$(Str$(i),2,"0")+" = "+fmt_hex$(x))
  If x And &h8000 Then
   con.print("  "+Str$(x-&h10000))
  Else
   con.print("  "+Str$(x))
  EndIf
  con.endl()
 Next
End Sub

Sub dmp_hdr()
 Local i
 con.println("Version      = "+Str$(rb(&h00)))
 con.println("Flags1       = &b"+str.lpad$(Bin$(rb(&h01)),8,"0"))
 con.println("Release      = "+Str$(rw(&h02)))
 con.println("Base high    = "+fmt_hex$(rw(&h04)))
 con.println("Initial PC   = "+fmt_hex$(rw(&h06)))
 con.println("Dictionary   = "+fmt_hex$(rw(&h08)))
 con.println("Object tbl   = "+fmt_hex$(rw(&h0A)))
 con.println("Glob var tbl = "+fmt_hex$(rw(&h0C)))
 con.println("Base static  = "+fmt_hex$(rw(&h0E)))
 con.println("Flags2       = &b"+str.lpad$(Bin$(rb(&h10)),8,"0"))
 con.print("Serial num   = ")
 For i=&h12 To &h17 : con.print(Chr$(rb(i))) : Next
 con.endl()
 con.println("Abbrv tbl    = "+fmt_hex$(rw(&h18)))
 con.println("File length  = "+Str$(2*rw(&h1A))+" bytes")
 con.println("File chksum  = "+Str$(rw(&h1C)))
 con.println("Std revision = "+Str$(rw(&h32)))
End Sub

Sub dmp_mem(ad,n)
 Local i,x
 If n=0 Then n=128
 For i=0 To n-1
  If i Mod 16=0 Then con.print("["+fmt_hex$(ad+i)+"] ")
  If ad+i<FILE_LEN Then
   x=rb(ad+i)
   con.print(str.lpad$(Hex$(x),2,"0")+" ")
  Else
   con.print("XX ")
  EndIf
  If ((i+1) Mod 16=0) And (i<>n-1) Then con.endl()
 Next
 con.endl()
End Sub

Sub dmp_mmap
 Local i,j,nc,nr,width,x
 On Error Skip 1
 i=NUM_PHYSICAL_PAGES
 If MM.ERRNO<>0 Then
  con.println("Not using virtual memory implementation.")
  On Error Clear
  Exit Sub
 EndIf
 con.println("Physical page -> Virtual page")
 nc=6 : nr=CInt(NUM_PHYSICAL_PAGES/nc+0.4999) : width=15
 For i=0 To nr-1
  For j=0 To nc-1
   con.flush()
   x=(j*nr)+i
   If x<NUM_PHYSICAL_PAGES Then
    If j*width>Pos Then con.print(Space$(j*width-Pos))
    con.print(str.lpad$(Str$(x),3)+" -> "+str.lpad$(Str$(pp_to_vp(x)),3))
   EndIf
  Next
  con.endl()
 Next
 con.endl()
 con.println("Virtual page -> Physical page")
 nr=CInt(NUM_VIRTUAL_PAGES/nc+0.4999)
 For i=0 To nr-1
  For j=0 To nc-1
   con.flush()
   x=(j*nr)+i
   If x<NUM_VIRTUAL_PAGES Then
    If j*width>Pos Then con.print(Space$(j*width-Pos))
    con.print(str.lpad$(Str$(x),3)+" -> "+str.lpad$(Str$(vp_to_pp(x)),3))
   EndIf
  Next
  con.endl()
 Next
End Sub

Sub dmp_obj(o)
 Local ad,i,p,sz,x
 If o<=0 Then
  con.println("Property defaults:")
  ad=rw(&h0A)
  For i=0 To 30
   x=LGetByte(m(),ad)*256+LGetByte(m(),ad+1)
   con.print(str.lpad$(Hex$(x),4,"0")+" ")
   If (i+1) Mod 10=0 Then con.endl()
   Inc ad,2
  Next
  con.endl()
  Exit Sub
 EndIf
 con.print(Str$(o)+". ")
 con.print("Attributes: ")
 For i=0 To 31
  x=ob_attr(o,i)
  If x<>0 Then con.print(Str$(i))
 Next
 con.endl()
 x=ob_rel(o,4)
 con.print("   Parent object: "+Str$(x)+"  ")
 x=ob_rel(o,5)
 con.print("Sibling object: "+Str$(x)+"  ")
 x=ob_rel(o,6)
 con.println("Child object: "+Str$(x))
 ad=ob_prop_base(o)
 con.println("   Property address: "+fmt_hex$(ad))
 con.print("        Description: '")
 ob_print(o);
 con.println("'")
 con.println("         Properties:")
 p=0
 Do
  p=ob_next_prop(o,p)
  If p>0 Then
   ad=ob_prop_addr(o,p)
   sz=ob_prop_len(ad)
   x=LGetByte(m(),ad-1)
   If x\32+1<>sz Then Error
   con.print("             ["+Str$(p)+"] ")
   For i=1 To sz
    x=LGetByte(m(),ad)
    Inc ad,1
    con.print(str.lpad$(Hex$(x),2,"0")+" ")
   Next
   con.endl()
  EndIf
 Loop Until p=0
End Sub

Sub dmp_op(m$)
 con.print(str.rpad$(m$,14))
 If m$="CALL" Then
  con.print(fmt_call_operands$())
 ElseIf m$="JUMP" Then
  con.print(fmt_jump_operands$())
 ElseIf m$="STORE" Or m$="DEC_CHK" Or m$="INC_CHK" Then
  con.print(fmt_store_operands$())
 ElseIf m$="DEC" Or m$="INC" Or m$="PULL" Or m$="LOAD" Then
  con.print(fmt_store_operands$())
 Else
  con.print(fmt_normal_operands$())
 EndIf
 If st>-1 Then con.print(" -> "+fmt_store_value$(st))
 If br=0 Then con.endl() : Exit Sub
 If br And &h80000 Then con.print(" [TRUE] ") Else con.print(" [FALSE] ")
 If (br And &h7FFFF)=pc-2 Then con.println("RFALSE") : Exit Sub
 If (br And &h7FFFF)=pc-1 Then con.println("RTRUE") : Exit Sub
 con.println(Hex$(br And &h7FFFF))
End Sub

Sub dmp_routine(new_pc)
 Local i,locals_sz,x
 locals_sz=rb(new_pc)
 con.print("Routine "+Hex$(new_pc)+", "+Str$(locals_sz)+" locals (")
 For i=0 To locals_sz-1
  If i>0 Then con.print(", ")
  x=rw(new_pc+1+i*2)
  con.print(str.lpad$(Hex$(x),4,"0"))
 Next
 con.println(")")
End Sub

Sub dmp_stack(a)
 Local i,tmp_fp,x
 con.print("TOP: ")
 If sp=0 Then con.println("*empty*")
 tmp_fp=fp
 For i=sp-1 To 0 Step -1
  If i<sp-1 Then con.print("     ")
  x=st_peek(i)
  If x And &h8000 Then
   con.print(fmt_hex$(x)+"  "+str.lpad$(Str$(x-&h10000),6," ")+"  ")
  Else
   con.print(fmt_hex$(x)+"  "+str.lpad$(Str$(x),6," ")+"  ")
  EndIf
  If tmp_fp=&hFFFF Then
  ElseIf i=tmp_fp Then
   con.print("previous fp")
   If a<>0 Then con.endl() : con.print(String$(35,"-"))
   tmp_fp=x
  ElseIf i=tmp_fp+1 Then
   con.print("store result")
  ElseIf i=tmp_fp+2 Then
   con.print("return address (lo)")
  ElseIf i=tmp_fp+3 Then
   con.print("return address (hi)")
  ElseIf i=tmp_fp+4 Then
   con.print("num locals")
  Else
   con.print("L"+str.lpad$(Str$(i-tmp_fp-5),2,"0"))
  EndIf
  con.endl()
  If a=0 And i=fp Then Exit For
 Next
End Sub

Function fmt_operand$(i)
 Local a$,x
 x=ov(i)
 If ot(i)<>&b10 Then
  fmt_operand$="#"+str.lpad$(Hex$(x),2,"0")
  Exit Function
 EndIf
 If x=0 Then
  a$="(SP)+"
 ElseIf x<&h10 Then
  a$="L"+str.lpad$(Hex$(x-1),2,"0")
 Else
  a$="G"+str.lpad$(Hex$(x-&h10),2,"0")
 EndIf
 If x>0 Then a$=a$+" (="+Hex$(vget(x))+")"
 fmt_operand$=a$
End Function

Function fmt_call_operands$()
 Local a$,i
 If ot(0)=&b10 Then
  a$=fmt_operand$(i)
 Else
  a$=Hex$(2*ov(0))
 EndIf
 a$=a$+" ("
 For i=1 To onum-1
  If i>1 Then a$=a$+", "
  a$=a$+fmt_operand$(i)
 Next
 a$=a$+")"
 fmt_call_operands$=a$
End Function

Function fmt_jump_operands$()
 Local of
 If onum>1 Then Error "Too many operands."
 of=oa(0)
 If of And 2^15 Then of=of-65536
 fmt_jump_operands$=Hex$(pc+of-2)
End Function

Function fmt_store_value$(st)
 If st=0 Then
  fmt_store_value$="-(SP)"
 ElseIf st<&h10 Then
  fmt_store_value$="L"+str.lpad$(Hex$(st-1),2,"0")
 Else
  fmt_store_value$="G"+str.lpad$(Hex$(st-&h10),2,"0")
 EndIf
End Function

Function fmt_store_operands$()
 Local a$,i
 If ot(0)=&b10 Then Error "Unexpected VARIABLE operand"
 a$=a$+fmt_store_value$(ov(0))
 For i=1 To onum-1
  a$=a$+", "+fmt_operand$(i)
 Next
 fmt_store_operands$=a$
End Function

Function fmt_normal_operands$()
 Local a$,i
 For i=0 To onum-1
  If i>0 Then a$=a$+", "
  a$=a$+fmt_operand$(i)
 Next
 fmt_normal_operands$=a$
End Function

' ---- src/debug.inc
If InStr(Mm.Info(Device X),"PicoMite") Then
 Dim ss$(5) Length 32
Else
 Dim ss$(5)
EndIf
On Error Ignore
Mode 1
Font 1
On Error Abort
Const INSTALL_DIR=0
Const RESOURCES_DIR=1
Const SAVE_DIR=2
Const SCRIPT_DIR=3
Const STORY_DIR=4
Const STORY_FILE=5
If InStr(Mm.CmdLine$,"--shell") Then
 Option Break 4
 On Key 3,end_game()
EndIf
main()
end_game()

Sub main_init()
 Local i,x
 con.endl()
 mem_init(file.resolve$(ss$(STORY_DIR),ss$(STORY_FILE)+".z3"))
 di_init()
 con.endl()
 GLOBAL_VAR=rw(&h0C)
 x=rb(&h01)
 x=x And &b10011111
 wb(&h01,x)
 wb(&h20,con.HEIGHT)
 wb(&h21,con.WIDTH)
 pc=rw(&h06)
 For i=Bound(stack(),0) To Bound(stack(),1) : stack(i)=0 : Next
 sp=0
 fp=&hFFFF
End Sub

Sub main()
 Local i,old_dir$,old_pc,state,s$
 ss$(INSTALL_DIR)=get_install_dir$()
 ss$(RESOURCES_DIR)=file.resolve$(ss$(INSTALL_DIR),"resources")
 ss$(SAVE_DIR)=file.resolve$(ss$(INSTALL_DIR),"saves")
 ss$(SCRIPT_DIR)=file.resolve$(ss$(INSTALL_DIR),"scripts")
 ss$(STORY_DIR)=file.resolve$(ss$(INSTALL_DIR),"stories")
 main.init_console()
 Cls
 con.print_file(file.resolve$(ss$(RESOURCES_DIR),"title.txt"),1)
 de_init()
 For i=0 To 9 : bp(i)=-1 : Next
 con.println("Select a story file from '"+ss$(STORY_DIR)+"':")
 Do While s$=""
  s$=fi_choose$(ss$(STORY_DIR),"*.z3")
 Loop
 s$=Mid$(s$,Len(ss$(STORY_DIR))+2)
 ss$(STORY_FILE)=Left$(s$,Len(s$)-3)
 old_dir$=Cwd$
 ChDir(ss$(SAVE_DIR))
 s$=Dir$(ss$(STORY_FILE),File) : If s$<>"" Then Error "Unexpected file: "+s$
 s$=Dir$(ss$(STORY_FILE),Dir) : If s$="" Then MkDir(ss$(STORY_FILE))
 ChDir(ss$(SCRIPT_DIR))
 s$=Dir$(ss$(STORY_FILE),File) : If s$<>"" Then Error "Unexpected file:"+s$
 s$=Dir$(ss$(STORY_FILE),Dir) : If s$="" Then MkDir(ss$(STORY_FILE))
 ChDir(old_dir$)
 main_init()
 Pause 1000
 For i=0 To 10 : con.endl() : Next
 Timer=0
 Do While state<>E_QUIT
  If num_bp>0 And pc<>old_pc Then
   For i=0 To 9
    If pc=bp(i) Then
     con.println("[Breakpoint "+Str$(i)+" reached]")
     state=E_BREAK
    EndIf
   Next
  EndIf
  If con.spin_enabled And num_ops Mod 16=0 Then con.spin()
  old_pc=pc
  Select Case state
   Case E_OK,E_REPEAT
    state=exec(ztrace)
   Case Else
    state=debug()
  End Select
 Loop
 con.endl()
 con.println("Num instructions processed = "+Str$(num_ops))
 con.print("Instructions / second      = ")
 con.println(Format$(1000*num_ops/Timer,"%.1f"))
 con.close_out()
 con.close_in()
End Sub

Sub main.init_console()
 Const w%=Mm.HRes\Mm.Info(FontWidth)
 Const h%=Mm.VRes\Mm.Info(FontHeight)
 Const spin_enabled%=InStr(Mm.Device$,"PicoMite") Or InStr(Mm.Device$,"WebMite")
 con.init(w%,h%,spin_enabled%)
 If Mm.Info(Device X)="MMB4L" Then Console Resize con.WIDTH,con.HEIGHT
End Sub

Function get_install_dir$()
 get_install_dir$=Left$(Mm.Info(Path),Len(Mm.Info(Path))-1)
 If get_install_dir$="NON" Then get_install_dir$=Cwd$
 If Not Mm.Info(Exists Dir file.resolve$(get_install_dir$,"resources")) Then
  get_install_dir$=file.get_parent$(get_install_dir$)
 EndIf
End Function

Sub end_game()
 If InStr(Mm.CmdLine$,"--shell") Then
  Option Break 3
  Pause 2000
  sys.run_shell()
 EndIf
 End
End Sub

