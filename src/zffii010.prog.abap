REPORT ZFFII010 MESSAGE-ID ZS line-count 65 line-size 255.
************************************************************************
*  Extract SAP G/L Account Numbers for Conversion to PeopleSoft
************************************************************************
* 2005/08/29 mkhan TR105 Select Actual amount only as PLAN amount
*                     is going to be introduced in FI.
* 2002/07/23 mdemeest #--- Add MNPL company code, adjust size of some
*                          fields.
* 2002/06/21 mdemeest #--- Copied IFFII001 and made modifications
*                          Read spreadsheet and load to interal table
*                          Sort spreadsheet by SAP G/L
*                          Read G/L and match to spreadsheet and produce
*                          report
*                          Download an an excel spreadsheet
*
************************************************************************
* Selection Screen

PARAMETER: p_bukrs              like skb1-bukrs obligatory,
           p_bu(5)      type c  obligatory,
           p_ledger(10) type c  obligatory default 'local' lower case.
*PARAMETER:  FILE1   like filenameci-fileextern
*                 default '/usr/sap/interfaces/P01/CFMM001/GL.SAP',
PARAMETER:  filein  like filenameci-fileextern
                 default '/usr/sap/interfaces/P01/CFMM001/GLIN.SAP'.
*           recsize type i default 90,
*           TABLE2 like RSEUX-CDT_VALUE default 'MAKT',
*           FILE2 like filenameci-fileextern
*                 default '/usr/sap/interfaces/P01/CFMM001/MAKT.SAP',
*           recsize1 type i default 98.
*PARAMETER: LFILE LIKE FILENAME-FILEINTERN.
*PARAMETER: RECSIZE TYPE I.

TABLES: SKB1,                "Company Code G/L's
        GLT0,                "Account Balances
        t001.                "Company Code - for currency

*---------------------- SAP to PS Spreadsheet Info ---------------------
data: inrec(400) type c.             "Comma delimited record from xls

data: begin of ps_fields occurs 30,
         ps_field_det(20) type c.
data: end of ps_fields.

data: begin of xref        occurs 500,
        sapgl(6)         type c,
        sapdesc(20)       type c,
        dept(10)          type c,
        psacct(10)        type c,
        electacct(10)     type c,
        gaap(6)           type c,
        type(10)          type c,
        descr(20)         type c,
        rtdesc(20)        type c,
        rcto(10)          type c,
        rcfrom(10)        type c,
        project(20)       type c,
        activity(20)      type c,
        process(20)       type c,
        product(20)       type c,
        affiliate(20)     type c,
        location(20)      type c.
data:  end of xref.
*-----------------------------------------------------------------------
* Global Data Definitions
DATA: PHYFILE LIKE FILENAMECI-FILEEXTERN.
DATA: PHYFILE1 LIKE FILENAMECI-FILEEXTERN.
data: recsize type i.

data:  wa_balance like glt0-tsl01.
data:  wa_temp    like glt0-tsl01.
data:  wa_waers   like t001-waers.

DATA: RECORD(90).

DATA: BEGIN OF WA_GL OCCURS 25000,
      saknr        like skb1-saknr,
      bukrs        like skb1-bukrs,
      unit(5)      type c,
      ledger(6)   type c,
      account(7)   type c,
      restype(10)  type c,
      RC_To(4)     type c,
      RC_From(4)   type c,
      project(10)  type c,
      process(10)  type c,
      location(10) type c,
      product(10)  type c,
      custseg(10)  type c,
      prodmth(10)  type c,
      currency(10) type c,
      amount       like glt0-tsl01,
      nr(10)       type c,
      ratetype(10) type c,
      rate(10)     type c,
      baseamt(15)  type c,
      stat(10)     type c,
      statamt(15)  type c,
      affiliate(10) type c,
      reference(10) type c,
      description(30) type c,
      projunit(10)  type c,
      activity(10)  type c,
      category(10)  type c,
      subcat(10)    type c,
      analysis(10)  type c,
      END OF WA_GL.


  perform OPEN_FILE.

  clear wa_waers.
  select single * from t001
      where bukrs = p_bukrs.
  if sy-subrc = '0'.
     move t001-waers to wa_waers.
  else.
     move '???'      to wa_waers.
  endif.
  perform process_excel_spreadsheet.
  sort xref by sapgl.

*-----------------------------------------------------------------------
* Build report required by PeopleSoft
*-----------------------------------------------------------------------
  new-page.
  perform print_headings.

  SELECT * FROM skb1                "Selection based on company code
      where bukrs = p_bukrs.
*        and saknr = '0000130000'.    "MNPP test

     select * from glt0
       where rrcty = '0'                          "Actual        TR105
         and bukrs = p_bukrs
         and ryear = sy-datum(4)
         and racct = skb1-saknr.

       clear: wa_balance.
       clear: wa_gl.
       add glt0-hsl01 from 1 to 16 giving wa_balance.
       compute wa_balance = wa_balance + glt0-hslvt. "Balance forward

       move wa_balance          to wa_gl-amount.   "Balance
*       move glt0-rtcur          to wa_gl-currency. "Currency Key
       move wa_waers             to wa_gl-currency. "Currency Key


*    clear: wa_gl.
       perform match_excel_spreadsheet.
       perform move_info.
*      TRANSFER WA_GL TO FILE1 LENGTH RECSIZE.  "Output record

     endselect.                     " end of GLT0

  endselect.                        " end of SKB1 selection

  perform print_info.
  close dataset: filein.  ", file1.


form print_info.
   loop at wa_gl.
   write: / wa_gl-saknr(6) under text-100, "SAP G/L
            wa_gl-bukrs      under text-101, "Company Code
            wa_gl-unit       under text-001, "P/S Business Unit
            wa_gl-ledger     under text-002, "Ledger
            wa_gl-account    under text-003, "P/S Account
            wa_gl-restype    under text-004, "Resource Type
            wa_gl-rc_to      under text-005, "RC To
            wa_gl-rc_from    under text-006, "RC From
            wa_gl-project    under text-007, "Project
            wa_gl-process    under text-008, "Process
            wa_gl-location   under text-009, "Location
            wa_gl-product    under text-010, "Product
            wa_gl-custseg    under text-011, "CustSeg
            wa_gl-prodmth    under text-012, "Prod Month
            wa_gl-currency   under text-013, "Transaction Currency
       (15) wa_gl-amount     under text-014 decimals 2, "Account Balance
            wa_gl-nr         under text-015, "NR
            wa_gl-ratetype   under text-016, "Rate type
            wa_gl-rate       under text-017, "Rate
* per Allison - base amount = amount when posted
*       (15) wa_gl-amount     under text-018, "Base Amount
            wa_gl-stat       under text-019, "Stat
       (15) wa_gl-statamt    under text-020 decimals 2, "Stat Amount
            wa_gl-affiliate  under text-021, "Affiliate
* 22
            wa_gl-reference  under text-023, "Reference
            wa_gl-description under text-024, "Description
            wa_gl-projunit   under text-025, "Project Unit
            wa_gl-activity   under text-026, "Activity
            wa_gl-category   under text-027, "Category
            wa_gl-subcat     under text-028, "Sub-category
            wa_gl-analysis   under text-029. "Analysis

*   uline.

   at last.
     sum.
     write: /30 'TOTAL',
         (10) wa_gl-amount under text-014 decimals 2.  "Amount
   endat.

   endloop.
endform.

*CLOSE DATASET: FILE1.

form move_info.
   move skb1-saknr+4(6)     to wa_gl-saknr.     "SAP G/L Account
   move skb1-bukrs          to wa_gl-bukrs.     "Company Code
   move p_bu                to wa_gl-unit.      "Business Unit
   move p_ledger            to wa_gl-ledger.    "Ledger
   move 'SAP trans $ to P/Soft' to wa_gl-description.
   move p_bukrs to wa_gl-description+25(4).

   case p_bukrs.
     when 'MNPP'.           "BU 10717"
        perform MNPP_mapping.
     when 'MNPL'.           "BU 10715
        perform MNPL_mapping.
   endcase.
   collect wa_gl.
endform.

form MNPP_mapping.
   move 'A269'              to wa_gl-RC_To.
   move 'A269'              to wa_gl-RC_From.
   if ( wa_gl-saknr+4(6) = '100490' or
        wa_gl-saknr+4(6) = '110490' or
        wa_gl-saknr+4(6) = '115090' or
        wa_gl-saknr+4(6) = '115900' ).
      if wa_gl-amount <> 0.
         move 'A273'        to wa_gl-RC_To.
         move 'A273'        to wa_gl-RC_From.
         move 'ACT'         to wa_gl-analysis.
         move '???'         to wa_gl-project.
         move '???'         to wa_gl-activity.
      endif.
   endif.
endform.

form MNPL_mapping.
   move 'A267'              to wa_gl-RC_To.
   move 'A267'              to wa_gl-RC_From.
   if ( wa_gl-saknr+4(6) = '100490' or
        wa_gl-saknr+4(6) = '110490' or
        wa_gl-saknr+4(6) = '115090' or
        wa_gl-saknr+4(6) = '115900' ).
      if wa_gl-amount <> 0.
         move 'A271'        to wa_gl-RC_To.
         move 'A271'        to wa_gl-RC_From.
         move 'ACT'         to wa_gl-analysis.
         move '???'         to wa_gl-project.
         move '???'         to wa_gl-activity.
      endif.
   endif.
endform.







*--------------------------  OPEN_FILE ---------------------------------
* Routine to convert the logical file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
*-----------------------------------------------------------------------
FORM OPEN_FILE.
*   CALL FUNCTION 'FILE_GET_NAME'
*      EXPORTING
*         LOGICAL_FILENAME        = LFILE
*      IMPORTING
*         FILE_NAME               = PHYFILE
*      EXCEPTIONS
*         FILE_NOT_FOUND          = 1
*         OTHERS                  = 2.
*   IF SY-SUBRC <> 0.
*      MESSAGE E018.
*   ELSE.
      OPEN dataset filein for input in text mode.
*      OPEN DATASET FILE1  FOR OUTPUT IN TEXT MODE.
*   ENDIF.
ENDFORM.

*----------------------- PROCESS_EXCEL_SPREADSHEET ---------------------
*  Read spreadsheet - each line being a record
*  Parse record splitting at commas
*  Move parsed fields to internal table - This table will be used as
*  cross reference between SAP and PeopleSoft
*-----------------------------------------------------------------------
form process_excel_spreadsheet.
  do.
    clear inrec.
    read dataset filein into inrec.
*  write: / inrec.
    if sy-subrc <> 0.
       exit.
    else.
       clear ps_fields.
       split inrec at ',' into table ps_fields.
*      loop at ps_fields.
*        write: / sy-tabix, ps_fields-ps_field_det.
*      endloop.
    endif.

    clear xref.
    loop at ps_fields.              "moves to internal table -xref
      case sy-tabix.
        when 1.
            move ps_fields-ps_field_det to xref-sapgl.
        when 2.
            move ps_fields-ps_field_det to xref-sapdesc.
        when 3.
            move ps_fields-ps_field_det to xref-dept.
        when 4.                                         "Extra column
        when 5.
*            move ps_fields-ps_field_det to xref-psacct.
        when 6.
             move ps_fields-ps_field_det to xref-psacct.
        when 7.
        when 8.
            move ps_fields-ps_field_det to xref-type.
        when 9.
            move ps_fields-ps_field_det to xref-descr.
        when 10.
            move ps_fields-ps_field_det to xref-rtdesc.
        when 11.
            move ps_fields-ps_field_det to xref-rcto.
        when 12.
            move ps_fields-ps_field_det to xref-rcfrom.
        when 13.
            move ps_fields-ps_field_det to xref-project.
        when 14.
            move ps_fields-ps_field_det to xref-activity.
        when 15.
            move ps_fields-ps_field_det to xref-process.
        when 16.
            move ps_fields-ps_field_det to xref-product.
        when 17.
            move ps_fields-ps_field_det to xref-affiliate.
        when 18.
            move ps_fields-ps_field_det to xref-location.
      endcase.
    endloop.
    append xref.
  enddo.

*   loop at xref.
*     write: / xref-sapgl, xref-dept, xref-psacct, xref-type,
*              xref-rtdesc, xref-rcto, xref-rcfrom, xref-project,
*              xref-activity, xref-process, xref-product,
*              xref-affiliate, xref-location.
*   endloop.


endform.

*----------------------- PRINT_HEADINGS --------------------------------
* PeopleSoft Report Headings
*-----------------------------------------------------------------------
form print_headings.
  write: / text-100, text-101,
           text-001, text-002, text-003, text-004, text-005,
           text-006, text-007, text-008, text-009, text-010,
           text-011, text-012, text-013, text-014, text-015,
           text-016, text-017, text-018, text-019, text-020,
           text-021, text-022, text-023, text-024, text-025,
           text-026, text-027, text-028, text-029.

endform.

*-------------------- MATCH_EXCEL_SPREADSHEET --------------------------
* Using SAP G/L number, match to SAP G/L number in spreadsheet and
* pick up all associated information.
*-----------------------------------------------------------------------
form match_excel_spreadsheet.
  read table xref with key sapgl(6) = glt0-racct+4(6) binary search.
  if sy-subrc = '0'.
     move xref-psacct              to wa_gl-account.
     move xref-type                to wa_gl-restype.
     move xref-rcto                to wa_gl-RC_To.
     move xref-rcfrom              to wa_gl-RC_From.
     move xref-project             to wa_gl-project.
     move xref-activity            to wa_gl-activity.
     move xref-process             to wa_gl-process.
     move xref-product             to wa_gl-product.
     move xref-affiliate           to wa_gl-affiliate.
     move xref-location            to wa_gl-location.
  else.
     write: / glt0-racct, ' not found in spreadsheet. Please verify'.
  endif.
endform.
*-----------------------------------------------------------------------
