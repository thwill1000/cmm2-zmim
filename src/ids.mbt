' Copyright (c) 2019-2025 Thomas Hugo Williams
' License MIT <https://opensource.org/licenses/MIT>
' For MMBasic 6.00

' Global variable and function/subroutine name replacements.

' From "console.inc"
'!replace script            csc
'!replace cbuf$             cb$
'!replace cspace            csp
'!replace clines            cli
'!replace cn_count          cco
'!replace cn_x              cx
'!replace cn_spin           csi
'!replace C_HEIGHT          CHE
'!replace C_WIDTH           CWI
'!replace cflush            cfl
'!replace cin$              ci$
'!replace cout              cou
'!replace endl              cen

' From "decode.inc"
'!replace onum              osz
'!replace OP0$              O0$
'!replace OP1$              O1$
'!replace OP2$              O2$
'!replace OPV$              O3$
'!replace decode            dec
'!replace de_long           dlo
'!replace de_short          dsh
'!replace de_var            dva
'!replace de_branch         dbr

' From "dict.inc"
'!replace DICT_AD           DAD
'!replace DICT_SEP$         DS$
'!replace DICT_ENTRY_LEN    DEL
'!replace DICT_ENTRY_COUNT  DSZ
'!replace DICT_ENTRY_AD     DEA
'!replace di_lookup         dlk

' From "execute.inc"
'!replace num_ops           nop
'!replace exec              exe
'!replace ex_2op            e2o
'!replace ex_1op            e1o
'!replace ex_0op            e0o
'!replace ex_varop          evo
'!replace ex_branch         ebr
'!replace ex_return         ert
'!replace ex_call           ecl
'!replace ex_read           erd
'!replace ex_special        esp
'!replace ex_random         era
'!replace ex_show_status    ess

' From "mem_xxx.inc"
'!replace FILE_LEN          FSZ
'!replace BASE_STATIC       BST
'!replace FIRST_SWAP_PAGE   FSP
'!replace MEM_FILE$         MF$
'!replace pp_to_vp          mpv
'!replace vp_to_pp          mvp
'!replace next_page         mnx

' From "objects.inc"
'!replace ob_attr           oat
'!replace ob_relative       orl
'!replace ob_next_prop      onp
'!replace ob_prop_len       opl
'!replace ob_prop_base      opb
'!replace ob_prop_addr      opa
'!replace ob_prop_get       opg
'!replace ob_prop_put       opp
'!replace ob_print          opr
'!replace ob_insert         oin
'!replace ob_remove         ore

' From "stack.inc"
'!replace stack             stk
'!replace st_pop            spo
'!replace st_push           spu
'!replace st_peek           spe
'!replace st_poke           spk

' From "util.inc"
'!replace lpad$             lp$
'!replace rpad$             rd$
'!replace fmt_hex$          hx$

' From "variable.inc"
'!replace GLOBAL_VAR        GVA
'!replace vget              vge
'!replace vset              vse

' From "zsave.inc"
'!replace zsave             zsv

' From "zstring.inc"
'!replace ALPHABET$         AL$
'!replace print_zstring     pzs
'!replace print_abrv        pab
