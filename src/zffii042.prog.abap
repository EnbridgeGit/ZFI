REPORT ZFFII042 MESSAGE-ID ZS NO STANDARD PAGE HEADING LINE-SIZE 255.
*
*  THIS ABAP IS USED IN BOTH THE EAST AND THE WEST.  PLEASE MAKE SAME
*  CHANGES TO BOTH
*
************************************************************************
* 2008/06/11 mdemeest TR342 Multiple company code selection
* 2003/06/04 mdemeest 1019 Eliminate recurring entries template
* 2003/05/23 mdemeest 1019 Copied from EAST SAP and removed selections
*                          from variant S_MATNR, S_AUFNR and S_KOSTL and
*                          S_HKONT
************************************************************************
TABLES: BKPF, BSEG,
        LFA1,               "Vendor Name
        LFB1,
        skat,               "Account Description
        MAKT.               "Material Description

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-100.
PARAMETER: PHYFILE LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/BKPF.CSV'.
PARAMETER: RECSIZE TYPE I DEFAULT 118.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-101.
PARAMETER: PHYFILE2 LIKE FILENAMECI-FILEEXTERN
                     DEFAULT '/usr/sap/interfaces/P01/CFMM001/BSEG.CSV'.
PARAMETER: RECSIZE2 TYPE I default 205.
SELECTION-SCREEN END OF BLOCK BOX2.


SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-001.
select-options: S_BUKRS for BKPF-BUKRS memory id BUK OBLIGATORY. "TR342
PARAMETER:      P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4) OBLIGATORY.
*              P_MONAT LIKE BKPF-MONAT OBLIGATORY DEFAULT SY-DATUM+4(2).
select-options: s_monat for bkpf-monat obligatory default '01' to '12'.
SELECT-OPTIONS: S_BLART FOR  BKPF-BLART,
*                S_HKONT FOR  BSEG-HKONT,
                S_LIFNR FOR BSEG-LIFNR.
Parameter:      P_ktopl like skat-ktopl default 'COAT'.
*
SELECTION-SCREEN END OF BLOCK BOX3.



DATA: WA-NAME  LIKE LFA1-NAME1.
DATA: WA-ZWELS LIKE LFB1-ZWELS.
DATA: WA-MAKTX LIKE MAKT-MAKTX.
DATA: PRV-BELNR LIKE BKPF-BELNR.
DATA: PRV-LIFNR LIKE BSEG-LIFNR.
DATA: TMP-LIFNR LIKE BSEG-LIFNR.
DATA: ITEM-COUNT(7) TYPE N VALUE 0.
DATA: ITEM-TOTAL(9) TYPE N VALUE 0.
DATA: DOLLAR-TOTAL(13) TYPE P DECIMALS 2 value 0.
DATA: GRAND-TOTAL(13) TYPE P DECIMALS 2 value 0.
DATA: PPRV-BELNR LIKE BKPF-BELNR.

data: begin of wadelete  occurs 0,
      belnr  like bseg-belnr,
      end of wadelete.


DATA: BEGIN OF WAbkpf OCCURS 50000,
      mandt like bkpf-mandt,
      comma1(1) type c,
      BUKRS LIKE BKPF-BUKRS,
      comma2(1) type c,
      BELNR LIKE BKPF-BELNR,
      comma3(1) type c,
      GJAHR LIKE BKPF-GJAHR,
      comma4(1) type c,
      MONAT LIKE BKPF-MONAT,
      comma5(1) type c,
      BLART LIKE BKPF-BLART,
      comma6(1) type c,
      BLDAT LIKE BKPF-BLDAT,
      comma7(1) type c,
      budat like bkpf-budat,
      comma8(1) type c,
      cpudt like bkpf-cpudt,
      comma9(1) type c,
      xblnr like bkpf-xblnr,
      commaa(1) type c,
      stblg like bkpf-stblg,
      commab(1) type c,
      bktxt like bkpf-bktxt,
      commac(1) type c,
      hwaer like bkpf-hwaer,
      commad(1) type c,
end of wabkpf.

data: begin of wabseg occurs 50000,
      belnr like bseg-belnr,
      comma1(1) type c,
      LIFNR LIKE BSEG-LIFNR,
      comma2(1) type c,
      buzei like bseg-buzei,
      comma3(1) type c,
      bschl like bseg-bschl,
      comma4(1) type c,
      koart like bseg-koart,
      comma5(1) type c,
      SHKZG like bseg-shkzg,
      comma6(1) type c,
      dmbtr(18) type c,
      comma7(1) type c,
*      rewrt(18) type c,       "Invoice Value (loc$)

      mwskz like bseg-mwskz,
      comma9(1) type c,
*      gsber like bseg-gsber,
      mwart like bseg-mwart,
      commab(1) type c,
      sgtxt like bseg-sgtxt,
      commac(1) type c,
      bewar like bseg-bewar,
      commad(1) type c,
      saknr like bseg-saknr,
      commae(1) type c,
      hkont like bseg-hkont,
      commaf(1) type c,
      txt20 like skat-txt20,        "20
      commag(1) type c,
      augdt like bseg-augdt,
      commah(1) type c,
      matnr like bseg-matnr,
      commai(1) type c,
      ebeln like bseg-ebeln,
      commaj(1) type c,
      zlsch like bseg-zlsch,
      commak(1) type c,
      zterm like bseg-zterm,
      commal(1) type c,
      end of wabseg.

initialization.
  move sy-sysid to phyfile+20(3).
  move sy-sysid to phyfile2+20(3).

start-of-selection.

perform open_output_files.

SELECT * FROM BKPF                       "Selection based on fiscal year
   WHERE BUKRS in s_BUKRS                 "and month              "TR342
     AND GJAHR = P_GJAHR
     and monat in s_monat                                   "99/06/07
     AND BLART IN S_BLART
     and tcode <> 'FBD1'.         "Eliminates recurring entries template

  PERFORM MOVE_BKPF_TO_WA.                      "Move info to work area

*   PRV-BELNR = WA-BELNR.
*   PRV-LIFNR = SPACE.

  SELECT * FROM BSEG
    WHERE BUKRS = BKPF-BUKRS
      AND BELNR = BKPF-BELNR
      AND GJAHR = BKPF-GJAHR.
*      AND LIFNR IN S_LIFNR.
    PERFORM MOVE_BSEG_TO_WA.                    "Move info to work area

  ENDSELECT.                                     "END of BSEG
ENDSELECT.                                       "END of BKPF

*-----------------------------------------------------------------------
* This next routine checks which vendors will be selected and puts all
* other document numbers in a "TO BE DELETED" table (wadelete)
*-----------------------------------------------------------------------
sort wadelete by belnr.
sort wabkpf   by belnr.
sort wabseg   by belnr.

loop at wabseg.
  if ( wabseg-lifnr in s_lifnr ) or ( wabseg-lifnr = space ).
  else.
    move wabseg-belnr to wadelete-belnr.
    write: /1 wadelete-belnr, wabseg-lifnr, ' DELETE'.
    append wadelete.
  endif.
endloop.

loop at wabkpf.
  loop at wadelete.
    if wabkpf-belnr = wadelete-belnr.
      write: /1 wabkpf-belnr, 'deleted from table'.
      delete wabkpf.
    endif.
  endloop.
endloop.

loop at wabseg.
  loop at wadelete.
    if wabseg-belnr = wadelete-belnr.
      write: /1 wabseg-belnr, 'deleted from table'.
      delete wabseg.
    endif.
  endloop.
endloop.


loop at wabkpf.
  TRANSFER WABKPF TO PHYFILE  LENGTH RECSIZE.   "Output BKPF


endloop.
loop at wabseg.
  TRANSFER WABSEG TO PHYFILE2 LENGTH RECSIZE2."Output BSEG
endloop.



.
*&---------------------------------------------------------------------*
*&      Form  Totals_by_Companycode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form Totals_by_Companycode.
*loop at wa.
* at new lifnr.
*   select single name1 from lfa1 into wa-name
*     where lifnr = wa-lifnr.
* endat.
* at end of bukrs.
*    sum.
*    write: /1 wa-lifnr, 15 wa-name, 50 wa-bukrs,
*             65 wa-itemcnt no-zero,
*             86 wa-dmbtr no-zero.
* endat.
* at end of lifnr.
*    sum.
*    write: /1 'Total for', wa-name,
*             65 wa-itemcnt no-zero,
*             86 wa-dmbtr no-zero.
*    skip 2.
* endat.
* at last.
*    sum.
*    write: /1 'Grand Total',
*            65 wa-itemcnt no-zero,
*            86 wa-dmbtr no-zero.
* endat.


*endloop.
endform.                    "Totals_by_Companycode

*&---------------------------------------------------------------------*
*&      Form  Totals_by_Vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form Totals_by_Vendor.
*loop at wa.
* at new lifnr.
*   select single name1 from lfa1 into wa-name
*     where lifnr = wa-lifnr.
* endat.****

* at end of lifnr.
*    sum.
*    write: /1 wa-lifnr, 15 wa-name,
*             65 wa-itemcnt no-zero,
*             86 wa-dmbtr no-zero.
* endat.

* at last.
*    sum.
*    write: /1 'Grand Total',
*            65 wa-itemcnt no-zero,
*            86 wa-dmbtr no-zero.
* endat.


*endloop.
endform.                    "Totals_by_Vendor




*&---------------------------------------------------------------------*
*&      Form  MOVE_BKPF_TO_WA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOVE_BKPF_TO_WA.
  CLEAR WAbkpf.
  move-corresponding bkpf to wabkpf.
  translate: wabkpf-bktxt using ',_'.
  translate: wabkpf-xblnr using ',_'.
  translate: wabkpf-bktxt using '"_'.
  translate: wabkpf-xblnr using '"_'.
  move ',' to: wabkpf-comma1, wabkpf-comma2, wabkpf-comma3,
               wabkpf-comma4, wabkpf-comma5, wabkpf-comma6,
               wabkpf-comma7, wabkpf-comma8, wabkpf-comma9,
               wabkpf-commaa, wabkpf-commab, wabkpf-commac,
               wabkpf-commad.

  append wabkpf.

ENDFORM.                    "MOVE_BKPF_TO_WA

*---------------------- MOVE_BSET_TO_WA  -------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_BSEG_TO_WA.
  clear wabseg.
  move-corresponding bseg to wabseg.

*  move bseg-rewrt         to wabseg-rewrt.
  move bseg-dmbtr         to wabseg-dmbtr.
  move ',' to: wabseg-comma1, wabseg-comma2, wabseg-comma3,
               wabseg-comma4, wabseg-comma5, wabseg-comma6,
               wabseg-comma7, wabseg-comma9,
               wabseg-commab, wabseg-commac,
               wabseg-commad, wabseg-commae, wabseg-commaf,
               wabseg-commag, wabseg-commah, wabseg-commai,
               wabseg-commaj, wabseg-commak, wabseg-commal.


  select single * from skat
     where saknr = wabseg-hkont
       and ktopl = p_ktopl.
  if sy-subrc = '0'.
    move skat-txt20 to wabseg-txt20.
  else.
    clear wabseg-txt20.

  endif.
   translate wabseg-sgtxt using ',_'.
  translate wabseg-txt20 using ',_'.
  translate wabseg-sgtxt using '"_'.
  translate wabseg-txt20 using '"_'.
  append wabseg.



ENDFORM.                    "MOVE_BSEG_TO_WA

*&---------------------------------------------------------------------*
*&      Form  OPEN_OUTPUT_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM OPEN_OUTPUT_FILES.

  OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
  OPEN DATASET PHYFILE2 FOR OUTPUT IN TEXT MODE.
  concatenate: 'Clt'              'CCde'
              'Document#'          'FiscalYear'
              'Period'            'Doc Type'
              'DocDate'           'PostDate'
              'EntryDate'         'RefDoc#'
              'RevrsDoc#'         'DocHeaderTxt'
              'LCur'        into  wabkpf separated by ','.
  transfer wabkpf to phyfile length recsize.

  concatenate: 'Document#'         'Vendor'
               'Item'              'PostingKey'
               'AcctType'          'Dr/Cr'
               'InvAmt(Loc$)'      'TaxCode'
               'TaxType'           'LineItemText'
               'TransType'         'GL#'
               'Line Item Acct'    'GL Acct Desc'
               'ClearDt'           'Material#'
               'PurDoc#'           'PayMeth'
               'PayTerms'   into wabseg separated by ','.
  transfer wabseg to phyfile2 length recsize2.





ENDFORM.                    "OPEN_OUTPUT_FILES
