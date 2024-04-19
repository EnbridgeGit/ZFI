REPORT ZFFIR005 MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-SIZE 255
   line-count 65.
************************************************************************
* Description:  G/L Balances for a specified version using a rolling
*               year.
*
*
* CHANGES: Latest on top
*
* 2002/04/16      mdemeest Fix the adding of months 12 & 13 in report
* 2001/11/30 #875 mdemeest New report
*
************************************************************************
TABLES: SKAT,              "Account Description Table
        BKPF,              "Accounting Transaction Header
        BSEG,              "Accounting Transaction Detail
        T001,              "Company Code table
        T011t,             "Version Description
        T015M.             "Name of Months

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-001.
PARAMETER:    P_BUKRS LIKE BKPF-BUKRS DEFAULT 'UGL'         OBLIGATORY,
              P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4)   OBLIGATORY,
              P_MONAT like BKPF-monat DEFAULT SY-DATUM+4(2)
                             obligatory,
              P_VERSN Like T011-VERSN default 'Z001'        obligatory.
skip.
Select-options: s_saknr for skat-saknr.
SELECTION-SCREEN END OF BLOCK BOX3.

skip.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE text-002.
PARAMETER:    P_CUM    radiobutton group rept,
              P_MTHLY  radiobutton group rept.
SELECTION-SCREEN END OF BLOCK BOX2.

data:  ctr          type i.
data:  yearflag(6)  type c.
*-----------------------------------------------------------------------
*  used to build selection date based on parameters p_gjahr & p_monat.
*-----------------------------------------------------------------------
data: begin of s_date   occurs 13,
        sign(1)            type c,
        option(2)          type c,
        low(6)             type c,
        high(6)            type c,
      end of s_date.
*-----------------------------------------------------------------------

data:  begin of X011Q  occurs 100.                    "Description Table
       include structure RF011Q.
data:  end of X011Q.

data:  begin of X011P  occurs 100.
       include structure RF011P.
data:  end of X011P.

data:  begin of X011V occurs 100.
       include structure RF011V.
data:  end of X011V.

data:  begin of X011Z  occurs 100.
       include structure RF011Z.
data:  end of X011Z.


data:  begin of account_balances  occurs 16.
       include structure bapi1028_4.
data:  end of account_balances.

data: returnx     like bapireturn.



DATA: BEGIN OF WA OCCURS 50000,
      ergso    like x011z-ergso,
      account  like account_balances-gl_account,
      txt20    like skat-txt20,
      BUKRS    LIKE BKPF-BUKRS,
      amt1     like account_balances-balance,
      amt2     like account_balances-balance,
      amt3     like account_balances-balance,
      amt4     like account_balances-balance,
      amt5     like account_balances-balance,
      amt6     like account_balances-balance,
      amt7     like account_balances-balance,
      amt8     like account_balances-balance,
      amt9     like account_balances-balance,
      amt10    like account_balances-balance,
      amt11    like account_balances-balance,
      amt12    like account_balances-balance,
      amt13    like account_balances-balance,
      cumamt1  like account_balances-balance,
      cumamt2  like account_balances-balance,
      cumamt3  like account_balances-balance,
      cumamt4  like account_balances-balance,
      cumamt5  like account_balances-balance,
      cumamt6  like account_balances-balance,
      cumamt7  like account_balances-balance,
      cumamt8  like account_balances-balance,
      cumamt9  like account_balances-balance,
      cumamt10 like account_balances-balance,
      cumamt11 like account_balances-balance,
      cumamt12 like account_balances-balance,
      cumamt13 like account_balances-balance,

      END OF WA.

data:  period_year(4)  type c,
       period_mth(2)   type c,
       period(16)      type c,
       period1(16)     type c,
       period2(16)     type c,
       period3(16)     type c,
       period4(16)     type c,
       period5(16)     type c,
       period6(16)     type c,
       period7(16)     type c,
       period8(16)     type c,
       period9(16)   type c,
       period10(16)  type c,
       period11(16)  type c,
       period12(16)  type c.

*----------------------  START-OF-SELECTION  ---------------------------
start-of-selection.
*-----------------------------------------------------------------------
*  Determine reporting period
*-----------------------------------------------------------------------
data:  next_year like bkpf-gjahr.
data:  last_mth  like bkpf-monat.

compute next_year = p_gjahr + 1.
compute last_mth  = p_monat - 1.

*-----------------------------------------------------------------------
* Call function to determine grouping in the reporting structure
* using the version id as entered on the screen
*-----------------------------------------------------------------------
call FUNCTION 'FI_IMPORT_BALANCE_SHEET_POS'
     EXPORTING
            VERSION = P_VERSN
     tables
            I011Z = X011z
            X011P = X011P
            X011v = X011v
     EXCEPTIONS
            NEW_BALANCE_SHEET = 04.
*-----------------------------------------------------------------------
*  X011Z contains group # & range of accounts within that group
*  X011Q contains group # & description
*-----------------------------------------------------------------------
loop at X011z.
  select * from skat
    where spras = sy-langu
      and ktopl = x011z-ktopl
      and saknr between x011z-vonkt and x011z-bilkt
      and saknr in s_saknr.
    move 'FIRST'  to yearflag.
    perform populate_wa using p_gjahr.
    if returnx-type = ' '.
       collect wa.
    endif.

    move 'SECOND' to yearflag.
    perform populate_wa using next_year.
    if returnx-type = ' '.         "Add only if valid balance sheet info
       collect wa.
   endif.
   endselect.
endloop.

loop at wa.
   add wa-amt1    from 1 to 12 giving wa-amt13.
   add wa-cumamt1 from 1 to 12 giving wa-cumamt13.
   modify wa.
endloop.
*-----------------------------------------------------------------------
* Call function to determine description based on ERGSO contents
*-----------------------------------------------------------------------
call FUNCTION 'FI_IMPORT_BALANCE_SHEET_TEXT'
     EXPORTING
            VERSION = P_VERSN
            SPRACHE = sy-langu
     tables
            X011Q = X011Q.

*sort wa by ergso account.  "DO NOT SORT THE INTERNAL TABLE

loop at wa.
    write: / wa-account  under text-010,
             wa-txt20    under text-011 decimals 2.
    if p_mthly = 'X'.
       perform write_amts.
    elseif p_cum = 'X'.
       perform write_cumamts.
    endif.

at end of ergso.
   uline.
   loop at X011q.                       "Total Description
     if  wa-ergso = x011q-ergsl and
         x011q-txtyp = 'E'      and
         x011q-zeile = '1'.
         write: / x011q-txt45 under text-010.
      endif.
   endloop.

   sum.
   if  p_mthly = 'X'.
       perform write_amts.
   elseif p_cum = 'X'.
       perform write_cumamts.
   endif.
   uline.
   skip.

endat.

endloop.

*------------------------  POPULATE_WA ---------------------------------
*  Calculate which amt column, amount will fall into
*-----------------------------------------------------------------------
form populate_wa using wa_gjahr.
  refresh account_balances.
  clear wa.
  move x011z-ergso to wa-ergso.
  move skat-txt20  to wa-txt20.

  call function 'BAPI_GL_GETGLACCPERIODBALANCES'
    exporting
      companycode       = p_bukrs
      glacct            = skat-saknr
      fiscalyear        = wa_gjahr
      currencytype      = '10'
    importing
      return            = returnx

    tables
      account_balances  = account_balances.

loop at account_balances.
  if ( yearflag = 'FIRST' and account_balances-fis_period < p_monat )
  or ( yearflag = 'FIRST' and account_balances-fis_period > 13 )
  or ( yearflag = 'SECOND' and account_balances-fis_period >= p_monat ).
  else.
     if account_balances-fis_period = 13.
        compute ctr = account_balances-fis_period - p_monat.
     else.
        compute ctr = account_balances-fis_period - p_monat + 1.
     endif.
     if  ctr <= 0.
         compute ctr = ctr + 12.
     endif.
     case ctr.
     when 01.
         add  account_balances-per_sales   to wa-amt1.
         move account_balances-balance     to wa-cumamt1.
     when 02.
         add  account_balances-per_sales   to wa-amt2.
         move account_balances-balance     to wa-cumamt2.
     when 03.
         add  account_balances-per_sales   to wa-amt3.
         move account_balances-balance     to wa-cumamt3.
     when 04.
         add  account_balances-per_sales   to wa-amt4.
         move account_balances-balance     to wa-cumamt4.
     when 05.
         add  account_balances-per_sales   to wa-amt5.
         move account_balances-balance     to wa-cumamt5.
     when 06.
         add  account_balances-per_sales   to wa-amt6.
         move account_balances-balance     to wa-cumamt6.
     when 07.
         add  account_balances-per_sales   to wa-amt7.
         move account_balances-balance     to wa-cumamt7.
     when 08.
         add  account_balances-per_sales   to wa-amt8.
         move account_balances-balance     to wa-cumamt8.
     when 09.
         add  account_balances-per_sales   to wa-amt9.
         move account_balances-balance     to wa-cumamt9.
     when 10.
         add  account_balances-per_sales   to wa-amt10.
         move account_balances-balance     to wa-cumamt10.
     when 11.
         add  account_balances-per_sales   to wa-amt11.
         move account_balances-balance     to wa-cumamt11.
     when 12.
         add  account_balances-per_sales   to wa-amt12.
         move account_balances-balance     to wa-cumamt12.

     endcase.
  endif.
endloop.

 move account_balances-gl_account  to wa-account.
endform.

form populate_wa_with_pos.

*loop at X011z.
*   loop at wa.
*      if wa-altkt between x011z-vonkt and x011z-bilkt.
*         move X011z-ergso to wa-ergso.
*         modify wa.
*         exit.
*     endif.
*    endloop.
*  endloop.
endform.

*--------------------------- TOP-OF-PAGE  ------------------------------
*  Headings
*-----------------------------------------------------------------------
Top-of-Page.
perform calculate_mths.
write: /1 text-rpt, sy-repid, 90 t001-butxt,
       200 text-dte, sy-datum, text-amp, sy-uzeit.
write: / text-clt under text-rpt, sy-mandt under sy-repid, sy-sysid,
         text-ttl under t001-butxt,
         text-pge under text-dte, sy-pagno under sy-datum.

if  p_mthly = 'X'.
    write: / text-003 under text-rpt color col_heading.
elseif p_cum = 'X'.
    write: / text-004 under text-rpt color col_heading.
endif.
write:  t011t-vstxt under t001-butxt.

uline.
write:  /1 text-010, 10 text-011,
         45  period1,  61 period2,   77  period3,  93  period4,
        109  period5, 125 period6,  141  period7, 157  period8,
        173  period9, 189 period10, 205 period11, 221 period12.

if p_mthly = 'X'.                          "TOTAL COLUMN only for Mthly
   write: 237 text-024.
endif.

uline.
*-----------------------------------------------------------------------
*------------------------  WRITE_AMTS ----------------------------------
*  Write current amounts
*-----------------------------------------------------------------------
FORM WRITE_AMTS.
  WRITE:    (16) wa-amt1   under period1  decimals 2,
            (16) wa-amt2   under period2  decimals 2,
            (16) wa-amt3   under period3  decimals 2,
            (16) wa-amt4   under period4  decimals 2,
            (16) wa-amt5   under period5  decimals 2,
            (16) wa-amt6   under period6  decimals 2,
            (16) wa-amt7   under period7  decimals 2,
            (16) wa-amt8   under period8  decimals 2,
            (16) wa-amt9   under period9  decimals 2,
            (16) wa-amt10  under period10 decimals 2,
            (16) wa-amt11  under period11 decimals 2,
            (16) wa-amt12  under period12 decimals 2,
            (18) wa-amt13  under text-024 decimals 2.
ENDFORM.
*------------------------  WRITE_CUMAMTS  ------------------------------
*  Write cumlative amounts
*-----------------------------------------------------------------------
FORM WRITE_CUMAMTS.
  WRITE:    (16) wa-cumamt1   under period1  decimals 2,
            (16) wa-cumamt2   under period2  decimals 2,
            (16) wa-cumamt3   under period3  decimals 2,
            (16) wa-cumamt4   under period4  decimals 2,
            (16) wa-cumamt5   under period5  decimals 2,
            (16) wa-cumamt6   under period6  decimals 2,
            (16) wa-cumamt7   under period7  decimals 2,
            (16) wa-cumamt8   under period8  decimals 2,
            (16) wa-cumamt9   under period9  decimals 2,
            (16) wa-cumamt10  under period10 decimals 2,
            (16) wa-cumamt11  under period11 decimals 2,
            (16) wa-cumamt12  under period12 decimals 2.
ENDFORM.

*------------------------- CALCULATE_MTHS  -----------------------------
*  Calculate months based on info entered in the variant
*-----------------------------------------------------------------------
form calculate_mths.
 do 12 times.
  case sy-index.
  when 1.
   move: p_gjahr to period+9(4), p_gjahr to period_year.
   move '/'     to period+13(1).
   move: p_monat to period+14(2), p_monat to period_mth.
   move period to period1.
  when 2.
    move period to period2.
  when 3.
    move period to period3.
  when 4.
    move period to period4.
  when 5.
    move period to period5.
  when 6.
    move period to period6.
  when 7.
    move period to period7.
  when 8.
    move period to period8.
  when 9.
    move period to period9.
  when 10.
    move period to period10.
  when 11.
    move period to period11.
  when 12.
    move period to period12.
  endcase.

  compute period_mth = period_mth + 1.
  if period_mth > 12.                            "Calculate next period
     move 1         to period_mth.
     compute period_year = period_year + 1.
  endif.
  move period_year  to period+9(4).
  move period_mth(2)   to period+14(2).

 enddo.

 select single * from T001
     where bukrs = p_bukrs.

 select single * from T011t
     where versn = p_versn
       and spras = sy-langu.

endform.









