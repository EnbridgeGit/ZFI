REPORT Zfapri027 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
      MESSAGE-ID U2.
************************************************************************
*  Author:      M DeMeester
*  Description: Extract vendors from file, and download to file for
*               Enlogix

* memory id on parameters/select-options initializes fields with user
*           parameters
************************************************************************
* 02/07/15 mdemeest #--- Added LFB1-LNRZB Alternate Payee for L Callow

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
       a_lifnr(15)   type c,          "Vendor Id
       b_name1(30)   type c,          "Vendor Name
       c(30)         type c,          "Vendor Cheque Name
       d(15)         type c,          "Primary Address ID
       e_ktokk(10)   type c,          "Class ID
       f_name1A(15)  type c,          "Vendor Short Name
       g(30)         type c,          "Vendor Contact name
       h_stras(30)   type c,          "Address line 1
       i(30)         type c,          "Address line 2
       j_ort01(30)   type c,          "City
       k_regio(4)    type c,          "Province/State
       l_pstlz(10)   type c,          "Postal code
       m_telf1(14)   type c,          "Phone Number 1
       n(14)         type c,          "Phone Number 2
       o_telfx(14)   type c,          "Fax Number
       p(15)         type c,          "Shipping Method
       q(15)         type c,          "Tax Schedule
       r_zterm(15)   type c,          "Payment Terms
       s(15)         type c,          "Currency Id
       t(15)         type c,          "Checkbook Id
       u(1)          type c,          "Minimum Payment Option
       v_lnrzb(10)   type c,          "Alternate Payee
       w_stcd1(16)   type c.          "Tax Code
data:  end of wa.

data:  vendor_cnt    type i.

*------------------- Excel Spreadsheet Internal Table ------------------


*-----------------------------------------------------------------------

*----------------------  SELECTION SCREEN  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-127.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
            s_bukrs  for lfb1-bukrs,    "Company Code
            S_lifnr  FOR lfa1-lifnr,
            s_konzs  for lfa1-konzs,
            s_ktokk  for t077k-ktokk.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.

*-------------------  TOP-OF-PAGE  -------------------------------------
* Report Title & Column Headings
*-----------------------------------------------------------------------
TOP-OF-PAGE.
*WRITE: /1 TEXT-RPT, 15 SY-REPID INTENSIFIED ON,                "Title
*      110 TEXT-HD1,
*      220 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
*WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
*         TEXT-HD2 UNDER TEXT-HD1,                            "Title
*         TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
*WRITE: / TEXT-CMP UNDER TEXT-RPT, P_BUKRS  UNDER SY-REPID.  "Company Cd
*WRITE: / TEXT-VER UNDER TEXT-RPT, P_VERS   UNDER SY-REPID,   "Version
*         TEXT-034 UNDER TEXT-HD1.
*WRITE:   132 S_MTH+3(2).
*IF S_MTH+5(2) = '00'.                         "Entered from period only
*         WRITE: 135 P_GJAHR.
*ELSE.                                        "Entered from & to periods
*         WRITE: 137 S_MTH+5(2), 140 P_GJAHR.
*ENDIF.

*IF P_COSTGR IS INITIAL.                       "Cost Element Group
*   WRITE: / TEXT-035, 'N/A' UNDER SY-REPID.
*ELSE.
*   WRITE: / TEXT-035, P_COSTGR UNDER SY-REPID.
*ENDIF.

*ULINE.
*PERFORM WRITE_VERT.
*WRITE:  2 TEXT-001,  81 TEXT-004,  108 TEXT-009,  135 TEXT-014,
*      162 TEXT-019, 189 TEXT-024, 216 TEXT-030.


*-----------------------------------------------------------------------
* Either a cost centre group is entered or at least one cost centre
* for column 1.  Columns 2 to 5 are optional.  However, if information
* is entered, it can be in only the cost centre group or the cost
* centre, not both.
*-----------------------------------------------------------------------


*---------------------- START-of-SELECTION -----------------------------
START-OF-SELECTION.
* Select all vendors and their associated details & banks
  select * from lfb1
      where lifnr in s_lifnr
        and bukrs in s_bukrs.
                                                   .
     select single * from lfa1
       where lifnr = lfb1-lifnr
         and konzs in s_konzs
         and ktokk in s_ktokk.

         perform move_vendor_to_wa.
         perform move_details_to_wa.
         append wa.
         compute vendor_cnt = vendor_cnt + 1.

 endselect.
*---------------------- End of Selection Criteria ----------------------

skip 10.
write: /30 text-001.
skip 5.
write: /30 text-002, vendor_cnt.

at pf8.
   perform create_excel_spreadsheet.

*--------------------- MOVE_VENDOR_TO_WA ------------------------------
form move_vendor_to_wa.
  clear wa.
  move lfa1-lifnr to wa-a_lifnr.
  move lfa1-name1 to wa-b_name1.
  move lfa1-ktokk to wa-e_ktokk.
  move lfa1-name1 to wa-f_name1a.
  move lfa1-stras to wa-h_stras.
  move lfa1-ort01 to wa-j_ort01.
  move lfa1-regio to wa-k_regio.
  move lfa1-pstlz to wa-l_pstlz.
  move lfa1-telf1 to wa-m_telf1.
  move lfa1-telfx to wa-o_telfx.
  move lfa1-stcd1 to wa-w_stcd1.






endform.

*--------------------- move_details_to_wa  -----------------------------
form move_details_to_wa.
  move lfb1-zterm to wa-r_zterm.
  move lfb1-lnrzb to wa-v_lnrzb.          "2002/07/15

*  move lfb1-zwels to wa-zwels.

*  move lfb1-zahls to wa-zahls.
*  move lfb1-xpore to wa-xpore.
*  move lfb1-akont to wa-akont.
endform.

*------------------------ move_bank_to_wa ------------------------------
*orm move_bank_to_wa.
* write lfbk-bankn to wa-bankn.
* write lfbk-bankl to wa-bankl.
*ndform.




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
   DO 23 TIMES.
     ASSIGN COMPONENT SY-INDEX OF STRUCTURE wa TO <NAME>.
     CALL METHOD OF SHEET 'Cells' = CELLS
                 EXPORTING #1 = INDEX.
     SET PROPERTY OF CELLS 'Value' = <NAME>.
     ADD 1 TO INDEX.
   ENDDO.
 ENDLOOP.

ENDFORM.























