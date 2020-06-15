' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Const PAGE_SIZE = 512
Const NUM_PHYSICAL_PAGES = 80
Const NUM_VIRTUAL_PAGES = 128 * 1024 \ PAGE_SIZE

' Memory addresses below this are read on startup and not swapped in/out
' - not properly set until the z-machine header is read
Dim BASE_STATIC = PAGE_SIZE

Dim FILE_LEN = PAGE_SIZE
Dim FIRST_SWAP_PAGE = -1

Dim m(NUM_PHYSICAL_PAGES * PAGE_SIZE \ 4)

Dim pf = 0 ' Counter for number of page faults

' Map of physical pages -> virtual pages
Dim pp_to_vp(NUM_PHYSICAL_PAGES - 1)

' Map of virtual pages -> physical pages
Dim vp_to_pp(NUM_VIRTUAL_PAGES - 1)

Dim next_page = 0

Dim pc = 0

Dim GLOBAL_VAR

' Reads a byte from 'pc' and increments 'pc'
Function rp()
  Local pp, vp
  If pc < 0 Or pc >= FILE_LEN Then Error "Invalid address: " + fmt_hex$(a)
  vp = pc \ PAGE_SIZE
  pp = vp_to_pp(vp)
  If pp = 0 Then pp = mem_load(vp) : pf = pf + 1
  rp = Peek(Var m(0), pp * PAGE_SIZE + (pc Mod PAGE_SIZE))
  pc = pc + 1
End Function

' Reads a byte from 'a' but DOES NOT increment a
Function rb(a)
  Local pp, vp
  If a < 0 Or a >= FILE_LEN Then Error "Invalid address: " + fmt_hex$(a)
  If a < BASE_STATIC Then rb = Peek(Var m(0), a) : Exit Function
  vp = a \ PAGE_SIZE
  pp = vp_to_pp(vp)
  If pp = 0 Then pp = mem_load(vp) : pf = pf + 1
  rb = Peek(Var m(0), pp * PAGE_SIZE + (a Mod Page_SIZE))
End Function

' Reads a 16-bit word from 'a' but DOES NOT increment a
Function rw(a)
  rw = rb(a) * 256 + rb(a + 1)
End Function

' Writes byte 'x' to 'a'
Sub wb(a, x)
  If a < 0 Or a >= BASE_STATIC Then Error "Invalid address: " + fmt_hex$(a)
  If (x And &hFF) <> x Then Error "Invalid write value: " + Str$(x)
  Poke Var m(0), a, x
End Sub

' Writes 16-bit word 'x' to 'a'
Sub ww(a, x)
  If a < 0 Or a >= BASE_STATIC - 1 Then Error "Invalid address: " + fmt_hex$(a)
  If (x And &hFFFF) <> x Then Error "Invalid write value: " + Str$(x)
  Poke Var m(0), a, x \ 256
  Poke Var m(0), a + 1, x Mod 256
End Sub

' Loads virtual page 'vp' from '$file'.
' @return physical page number
Function mem_load(vp)
  Local ad, buf$, buf_sz, i, pp, to_read

  pp = next_page

  ' TODO: Implement some form of Least Recently Used algorithm.
  next_page = next_page + 1
  If next_page = NUM_PHYSICAL_PAGES Then next_page = FIRST_SWAP_PAGE

  ' TODO: Should the file be opened once globally and kept open until exit?
  Open MEM_FILE$ For random As #1
  Seek #1, vp * PAGE_SIZE + 1
  ad = pp * PAGE_SIZE
  to_read = PAGE_SIZE
  buf_sz = 255
  Do While to_read > 0
    If to_read < 255 Then buf_sz = to_read
    buf$ = Input$(buf_sz, 1)
    For i = 1 To buf_sz
      Poke Var m(0), ad, Peek(Var buf$, i)
      ad = ad + 1
    Next i
    to_read = to_read - buf_sz
  Loop
  Close #1

  vp_to_pp(pp_to_vp(pp)) = 0
  vp_to_pp(vp) = pp
  pp_to_vp(pp) = vp

  mem_load = pp
End Function

Sub mem_init(f$)
  Local i

  Dim MEM_FILE$ = f$

  cout("Loading '" + MEM_FILE$ + "'") : endl()

  ' Load page 0 which contains the header.
  cout("  Header page: 0") : endl()
  If mem_load(0) <> 0 Then Error

  ' Read header data.
  pc = rw(&h06)
  GLOBAL_VAR = rw(&h0C)
  BASE_STATIC = rw(&h0E)
  FILE_LEN = rw(&h1A) * 2

  ' Initialise dynamic memory.
  FIRST_SWAP_PAGE = BASE_STATIC \ PAGE_SIZE
  If BASE_STATIC Mod PAGE_SIZE > 0 Then FIRST_SWAP_PAGE = FIRST_SWAP_PAGE + 1
  cout("  Dynamic pages: ")
  For i = 1 To FIRST_SWAP_PAGE - 1
    If i > 1 Then cout(", ")
    cout(Str$(i))
    If mem_load(i) <> i Then Error
  Next i
  endl()
  cout("  Paged memory starts at page " + Str$(FIRST_SWAP_PAGE)) : endl()
End Sub
