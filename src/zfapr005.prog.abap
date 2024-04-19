REPORT Zfapr005 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
      MESSAGE-ID U2.
************************************************************************
*  Author:      M DeMeester
*  Description: Extract vendors from file, and download to exdel
*               spreadsheet.

* memory id on parameters/select-options initializes fields with user
*           parameters
************************************************************************
* 08/02/25 mdemeest TR342 - add alternate payee & alternate vendor to
*                           report
* 00/09/11 mdemeest #--- Original program
************************************************************************
TABLES: lfa1,         "Vendor Master
        lfb1,         "Vendor Details
        lfbk,         "Bank Info
        t077k.        "Account Group Master Data

FIELD-SYMBOLS: <F1>,
               <F2>,
               <FS>.

data:  begin of wa     occurs 1000,
       konzs(10)     type c,
       lifnr(10)     type c,
       zwels(1)      type c,
       bankl(9)      type c,
       bankn(18)     type c,
       ktokk(10)     type c,
       zahls(1)      type c,
       name1(30)     type c,
       zterm(4)      type c,
       xpore(1)      type c,
       name1a(30)    type c,
       stras(30)     type c,
       ort01(30)     type c,
       regio(3)      type c,
       pstlz(10)     type c,
       land1(3)      type c,
       telf1(20)     type c,
       telfx(20)     type c,
       stcd1(15)     type c,
       akont(10)     type c,
       lnrzb(10)     type c,  "Alternate payee
       altkn(10)     type c.  "Alternate vendor
data:  end of wa.

*------------------- Excel Spreadsheet Internal Table ------------------


*-----------------------------------------------------------------------

*----------------------  SELECTION SCREEN  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-127.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
            S_lifnr  FOR lfa1-lifnr,
            s_konzs  for lfa1-konzs,
            s_ktokk  for t077k-ktokk,
            s_bukrs  for lfb1-bukrs no intervals memory id BUK.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.

*-------------------  TOP-OF-PAGE  -------------------------------------
* Report Title & Column Headings
*-----------------------------------------------------------------------
TOP-OF-PAGE.
*-----------------------------------------------------------------------


*---------------------- START-of-SELECTION -----------------------------


START-OF-SELECTION.
* Select all vendors and their associated details & banks

  select * from lfa1
    where lifnr in s_lifnr
      and konzs in s_konzs
      and ktokk in s_ktokk.

    perform move_vendor_to_wa.

      select single * from lfb1
          where lifnr = lfa1-lifnr
            and bukrs in s_bukrs.
      if  sy-subrc = 0.
          perform move_details_to_wa.
      endif.

      select single * from lfbk
          where lifnr = lfa1-lifnr.
      if sy-subrc = 0.
          perform move_bank_to_wa.
      endif.

      append wa.
  endselect.

skip 10.
write: /30 text-001.
skip 5.
write: /30 text-002, sy-tabix.

at pf8.
   perform create_excel_spreadsheet.

*--------------------- MOVE_VENDOR_TO_WA ------------------------------
form move_vendor_to_wa.
  clear wa.
  move lfa1-konzs to wa-konzs.
  move lfa1-lifnr to wa-lifnr.
  move lfa1-ktokk to wa-ktokk.
  move lfa1-name1 to wa-name1.
  move lfa1-name1 to wa-name1a.
  move lfa1-ort01 to wa-ort01.
  move lfa1-regio to wa-regio.
  move lfa1-pstlz to wa-pstlz.
  move lfa1-telf1 to wa-telf1.
  move lfa1-telfx to wa-telfx.
  move lfa1-stcd1 to wa-stcd1.
  move lfa1-stras to wa-stras.
endform.

*--------------------- move_details_to_wa  -----------------------------
form move_details_to_wa.
  move lfb1-zwels to wa-zwels.
  move lfb1-zterm to wa-zterm.
  move lfb1-zahls to wa-zahls.
  move lfb1-xpore to wa-xpore.
  move lfb1-akont to wa-akont.
  move lfb1-lnrzb to wa-lnrzb.  "Alternate payee
  move lfb1-altkn to wa-altkn.  "Alternate vendor
endform.

*------------------------ move_bank_to_wa ------------------------------
form move_bank_to_wa.
  write lfbk-bankn to wa-bankn.
  write lfbk-bankl to wa-bankl.
endform.




************************************************************************
* Excel Spreadsheet Routines
************************************************************************
*------------------------ CREATE_ESCEL_SPREADSHEET ---------------------
* This routine copied from "Developing SAP's R/3 Applications pg. 566
*-----------------------------------------------------------------------
FORM CREATE_EXCEL_SPREADSHEET.
 INCLUDE OLE2INCL.
 DATA: Zfapr005 TYPE OLE2_OBJECT,
       WORKBOOK TYPE OLE2_OBJECT,
       SHEET    TYPE OLE2_OBJECT,
       CELLS    TYPE OLE2_OBJECT,
       INDEX    TYPE I,
       ROW_MAX  TYPE I VALUE 256,
       F        TYPE I.
 FIELD-SYMBOLS: <NAME>.

* This routine moves info to internal table for download to Excel
* PERFORM EXCEL_TITLES.
*  LOOP AT wa.
*    MOVE BIG_TABLE-KSTAR       TO XLS_NAME-KSTAR.
*    MOVE BIG_TABLE-LTEXT       TO XLS_NAME-LTEXT.
*    PERFORM MOVE_AMTS_TO_EXCEL.
*    APPEND XLS_NAME.

*   AT END OF SETNR.                                "Totals for CE Group
*       SUM.
*       MOVE BIG_TABLE-SETNR+2(10) TO XLS_NAME-KSTAR.
*       LOOP AT NAME_TAB.
*        IF BIG_TABLE-SETNR = NAME_TAB-SETNR.
*            MOVE NAME_TAB-TITLE TO XLS_NAME-LTEXT.
*         ENDIF.
*      ENDLOOP.
*      PERFORM MOVE_AMTS_TO_EXCEL.
*      APPEND XLS_NAME.
*      CLEAR XLS_NAME.            "Blank line on spreadsheet after total
*       APPEND XLS_NAME.
*    ENDAT.

*    AT LAST.                                       "Report Total
*       SUM.
*       CLEAR  XLS_NAME-KSTAR.
*       MOVE 'TOTAL REPORT'        TO XLS_NAME-LTEXT.
*      PERFORM MOVE_AMTS_TO_EXCEL.
*       APPEND XLS_NAME.
*    ENDAT.
*ENDLOOP.

* This section creates the spreadsheet, workbooks, worksheets & cells
* and moves the entries from the internal table to the CELLS

 CREATE OBJECT Zfapr005 'excel.application'.
 SET PROPERTY OF zfapr005 'visible' = 1.
 CALL METHOD OF zfapr005 'Workbooks' = WORKBOOK.
 CALL METHOD OF WORKBOOK 'Add'.
 CALL METHOD OF zfapr005 'Worksheets' = SHEET
                  EXPORTING #1 = 1.
 CALL METHOD OF SHEET 'Activate'.

 LOOP AT wa.
   INDEX = ROW_MAX * ( SY-TABIX - 1 ) + 1.
   DO 22 TIMES.
     ASSIGN COMPONENT SY-INDEX OF STRUCTURE wa TO <NAME>.
     CALL METHOD OF SHEET 'Cells' = CELLS
                 EXPORTING #1 = INDEX.
     SET PROPERTY OF CELLS 'Value' = <NAME>.
     ADD 1 TO INDEX.
   ENDDO.
 ENDLOOP.

ENDFORM.
