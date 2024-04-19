REPORT ZFFIR003 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65
      MESSAGE-ID U2.
************************************************************************
*  Author:      M DeMeester
*  Description: Report listing selected G/L accounts with activity
*               during the requested year.
*
* memory id on parameters/select-options initializes fields with user
*           parameters
************************************************************************
* 00/06/30 mdemeest #--- Original request
* 2001/01/12 gymana - increase field widths in Excel spreadsheet to
*                     accommodate large numbers (Billions)
* 2005/08/24 mkhan TR105 Select Actual amount only as PLAN amount is
*                        going to be introduces in FI.
************************************************************************
TABLES: GLT0,       "Account Master Record - mthly debits & credits
        SKAT,       "G/L Master Description
        T001.       "Company Codes @ Currency Codes

DATA: CTR(2)   TYPE N.
*----------------------------------------------------------------------
*  List of Company Codes
*----------------------------------------------------------------------
DATA:   BEGIN OF TABLEA OCCURS 26,
          BUKRS LIKE GLT0-BUKRS,                    "Company Code
          WAERS LIKE T001-WAERS.                    "Company Currency Cd
DATA:   END OF TABLEA.

*----------------------------------------------------------------------
* Summary of Debits/Credits by Company Code/GL Account
*----------------------------------------------------------------------
DATA:   BEGIN OF TABLEB OCCURS 10000,               "Cost Elements
          BUKRS LIKE GLT0-BUKRS,                    "Company Code
          RACCT LIKE GLT0-RACCT,
          HSLXX LIKE GLT0-HSL01,                    "Sum amts for yr
          TSLXX LIKE GLT0-TSL01.
DATA:   END OF TABLEB.

*----------------------------------------------------------------------
* Matrix of G/L Account Balances by G/L Account & Company Code
*----------------------------------------------------------------------
DATA:   BEGIN OF TABLEC OCCURS 10000,
          RACCT LIKE GLT0-RACCT,                    "G/L Account
          TXT20 LIKE SKAT-TXT20,                    "Description
          HSL01 LIKE GLT0-HSL01,                    "AMT for Company 1
          HSL02 LIKE GLT0-HSL01,                    "AMT for Company 2
          HSL03 LIKE GLT0-HSL01,                    "AMT for Company 3
          HSL04 LIKE GLT0-HSL01,                    "AMT for Company 4
          HSL05 LIKE GLT0-HSL01,                    "AMT for Company 5
          HSL06 LIKE GLT0-HSL01,                    "AMT for Company 6
          HSL07 LIKE GLT0-HSL01,                    "AMT for Company 7
          HSL08 LIKE GLT0-HSL01,                    "AMT for Company 8
          HSL09 LIKE GLT0-HSL01,                    "AMT for Company 9
          HSL10 LIKE GLT0-HSL01,                    "AMT for Company 10
          HSL11 LIKE GLT0-HSL01,                    "AMT for Company 11
          HSL12 LIKE GLT0-HSL01,                    "AMT for Company 12
          HSL13 LIKE GLT0-HSL01,                    "AMT for Company 13
          HSL14 LIKE GLT0-HSL01,                    "AMT for Company 14
          HSL15 LIKE GLT0-HSL01,                    "AMT for Company 15
          HSL16 LIKE GLT0-HSL01,                    "AMT for Company 16
          HSL17 LIKE GLT0-HSL01,                    "AMT for Company 17
          HSL18 LIKE GLT0-HSL01,                    "AMT for Company 18
          HSL19 LIKE GLT0-HSL01,                    "AMT for Company 19
          HSL20 LIKE GLT0-HSL01,                    "AMT for Company 20
          HSL21 LIKE GLT0-HSL01,                    "AMT for Company 21
          HSL22 LIKE GLT0-HSL01,                    "AMT for Company 22
          HSL23 LIKE GLT0-HSL01,                    "AMT for Company 23
          HSL24 LIKE GLT0-HSL01,                    "AMT for Company 24
          HSL25 LIKE GLT0-HSL01,                    "AMT for Company 25
          HSL26 LIKE GLT0-HSL01.                    "AMT for Company 26
DATA:   END OF TABLEC.

*---------------------  EXCEL SPREADSHEET  -----------------------------
* Matrix of G/L Account Balances by G/L Account & Company Code (TEXT)
*----------------------------------------------------------------------
DATA:   BEGIN OF TABLED OCCURS 10000,
          RACCT LIKE GLT0-RACCT,                    "G/L Account
          TXT20 LIKE SKAT-TXT20,                    "Description
          HSL01(15) TYPE C,                         "AMT for Company 1
          HSL02(15) TYPE C,                         "AMT for Company 2
          HSL03(15) TYPE C,                         "AMT for Company 3
          HSL04(15) TYPE C,                         "AMT for Company 4
          HSL05(15) TYPE C,                         "AMT for Company 5
          HSL06(15) TYPE C,                         "AMT for Company 6
          HSL07(15) TYPE C,                         "AMT for Company 7
          HSL08(15) TYPE C,                         "AMT for Company 8
          HSL09(15) TYPE C,                         "AMT for Company 9
          HSL10(15) TYPE C,                         "AMT for Company 10
          HSL11(15) TYPE C,                         "AMT for Company 11
          HSL12(15) TYPE C,                         "AMT for Company 12
          HSL13(15) TYPE C,                         "AMT for Company 13
          HSL14(15) TYPE C,                         "AMT for Company 14
          HSL15(15) TYPE C,                         "AMT for Company 15
          HSL16(15) TYPE C,                         "AMT for Company 16
          HSL17(15) TYPE C,                         "AMT for Company 17
          HSL18(15) TYPE C,                         "AMT for Company 18
          HSL19(15) TYPE C,                         "AMT for Company 19
          HSL20(15) TYPE C,                         "AMT for Company 20
          HSL21(15) TYPE C,                         "AMT for Company 21
          HSL22(15) TYPE C,                         "AMT for Company 22
          HSL23(15) TYPE C,                         "AMT for Company 23
          HSL24(15) TYPE C,                         "AMT for Company 24
          HSL25(15) TYPE C,                         "AMT for Company 25
          HSL26(15) TYPE C.                         "AMT for Company 26
DATA:   END OF TABLED.


*                                  TR105.
CONSTANTS: TRUE    TYPE BOOLEAN VALUE 'X',
           FLASE   TYPE BOOLEAN VALUE '-',
           UNKNOWN TYPE BOOLEAN VALUE ' '.

*-----------------------------------------------------------------------

*----------------------  SELECTION SCREEN  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-127.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
             S_RACCT  FOR GLT0-RACCT.
PARAMETERS:  P_GJAHR  LIKE COSP-GJAHR   OBLIGATORY DEFAULT SY-DATUM(4),
             P_KTOPL  LIKE SKAT-KTOPL   OBLIGATORY MEMORY ID KPL.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-128.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER: B_ACTUAL RADIOBUTTON GROUP BUTN DEFAULT 'X' USER-COMMAND RAD.
* "Actual
SELECTION-SCREEN COMMENT 4(15) TEXT-003.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETER:   B_PLAN   RADIOBUTTON GROUP BUTN.  "Plan
SELECTION-SCREEN COMMENT 4(15) TEXT-004.
SELECTION-SCREEN POSITION 22.
PARAMETERS: P_RVERS LIKE GLT0-RVERS.
*PARAMETERS: P_RVERS LIKE GLT0-RVERS OBLIGATORY.
SELECTION-SCREEN COMMENT 26(15) TEXT-005.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN END OF BLOCK BOX.

******************************************************************TR105
AT SELECTION-SCREEN OUTPUT.
   CASE TRUE.
        WHEN B_ACTUAL.
             LOOP AT SCREEN.
                  IF SCREEN-NAME = 'P_RVERS'.
                     SCREEN-INPUT = '0'.
                     CLEAR P_RVERS.
                     MODIFY SCREEN.
                  ENDIF.
             ENDLOOP.

        WHEN B_PLAN.
             LOOP AT SCREEN.
                  IF SCREEN-NAME = 'P_RVERS'.
                     SCREEN-INPUT = '1'.
                     IF P_RVERS = SPACE.
                        P_RVERS = '002'.
                     ENDIF.
                     MODIFY SCREEN.
                  ENDIF.
             ENDLOOP.
   ENDCASE.
*----------------------- TOP-of-PAGE ----------------------------------

*--------------------- START-OF-SELECTION  ----------------------------
START-OF-SELECTION.
* SELECT * FROM GLT0                        "START OF TR105 CHANGES
*  WHERE RACCT IN S_RACCT
*    AND RYEAR = P_GJAHR.
DATA: W_RRCTY LIKE GLT0-RRCTY,
      W_RVERS LIKE GLT0-RVERS.

 IF B_ACTUAL = 'X'.
    MOVE 0 TO W_RRCTY.
    WRITE '001' TO W_RVERS.
 ELSE.
    MOVE 1 TO W_RRCTY.
    MOVE P_RVERS TO W_RVERS.
 ENDIF.

 SELECT * FROM GLT0
  WHERE RRCTY = W_RRCTY                     "Actual or Plan Amount
    AND RVERS = W_RVERS
    AND RYEAR = P_GJAHR
    AND RACCT IN S_RACCT.                   "END OF TR105 CHANGES

* determine all the company codes
  MOVE GLT0-BUKRS TO TABLEA-BUKRS.
  COLLECT TABLEA.

* accumulate the balances for each selected record.
  MOVE GLT0-RACCT  TO TABLEB-RACCT.
  MOVE GLT0-BUKRS  TO TABLEB-BUKRS.
  ADD GLT0-HSL01 FROM 1 TO 16 GIVING TABLEB-HSLXX.
  ADD GLT0-TSL01 FROM 1 TO 16 GIVING TABLEB-TSLXX.
  COLLECT TABLEB.

 ENDSELECT.                                      "End of GLT0

* Get Company Code's appropriate currency code
 LOOP AT TABLEA.
   SELECT SINGLE * FROM T001
      WHERE BUKRS = TABLEA-BUKRS.
      MOVE T001-WAERS TO TABLEA-WAERS.
   MODIFY TABLEA.
 ENDLOOP.

 SORT TABLEA BY BUKRS.
 SORT TABLEB BY BUKRS RACCT.
* This does not work at the current time - Requires Group G/L's
* If Company Currency is USD and transaction is USD, there is no
* way to convert to CANADIAN DOLLARS  (2000/08/14 - eg. 251000 for 2000
* Depending on the currency code used by the Company, move the
* info from TSLXX to HSLXX and then all info from report is in HSLXX

*LOOP AT TABLEB.
*  READ TABLE TABLEA WITH KEY BUKRS = TABLEB-BUKRS BINARY SEARCH.
*  IF SY-SUBRC = 0.
*     IF TABLEA-WAERS = 'CAD'.
*     ELSE.
*        MOVE TABLEB-TSLXX TO TABLEB-HSLXX.
*        MODIFY TABLEB.
*     ENDIF.
*  ENDIF.
*ENDLOOP.
*---------------------------------------------------------------------
 LOOP AT TABLEB.
  CLEAR: CTR, TABLEC.
  MOVE TABLEB-RACCT TO TABLEC-RACCT.
  LOOP AT TABLEA.
  CTR = CTR + 1.
     IF TABLEB-BUKRS = TABLEA-BUKRS.
        CASE CTR.
          WHEN '01'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL01.
          WHEN '02'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL02.
          WHEN '03'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL03.
          WHEN '04'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL04.
          WHEN '05'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL05.
          WHEN '06'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL06.
          WHEN '07'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL07.
          WHEN '08'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL08.
          WHEN '09'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL09.
          WHEN '10'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL10.
          WHEN '11'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL11.
          WHEN '12'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL12.
          WHEN '13'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL13.
          WHEN '14'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL14.
          WHEN '15'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL15.
          WHEN '16'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL16.
          WHEN '17'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL17.
          WHEN '18'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL18.
          WHEN '19'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL19.
          WHEN '20'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL20.
          WHEN '21'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL21.
          WHEN '22'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL22.
          WHEN '23'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL23.
          WHEN '24'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL24.
          WHEN '25'.
            MOVE TABLEB-HSLXX TO TABLEC-HSL25.
          WHEN OTHERS.
            MOVE TABLEB-HSLXX TO TABLEC-HSL26.
          ENDCASE.
        COLLECT TABLEC.
        EXIT.                      "Leave loop once match established
     ENDIF.
  ENDLOOP.                 "tablea
 ENDLOOP.                  "tableb

 SORT TABLEC BY RACCT.

 LOOP AT TABLEC.                           "Get description for each G/L
   CLEAR TABLEC-TXT20.
   SELECT SINGLE * FROM SKAT
    WHERE SPRAS = SY-LANGU
      AND KTOPL = P_KTOPL
      AND SAKNR = TABLEC-RACCT.
   IF SY-SUBRC = '0'.
      MOVE SKAT-TXT20 TO TABLEC-TXT20.
   ENDIF.
   MODIFY TABLEC.
 ENDLOOP.

* This routine moves required company codes to Excel Matrix
  CLEAR TABLED.
  MOVE TEXT-CLT TO TABLED-TXT20.
  MOVE SY-SYSID TO TABLED-TXT20+10(3).
  MOVE SY-MANDT TO TABLED-TXT20+14(3).

  LOOP AT TABLEA.
   CASE SY-TABIX.
   WHEN '1'.
    MOVE TABLEA-BUKRS TO TABLED-HSL01.
   WHEN '2'.
    MOVE TABLEA-BUKRS TO TABLED-HSL02.
   WHEN '3'.
    MOVE TABLEA-BUKRS TO TABLED-HSL03.
   WHEN '4'.
    MOVE TABLEA-BUKRS TO TABLED-HSL04.
   WHEN '5'.
    MOVE TABLEA-BUKRS TO TABLED-HSL05.
   WHEN '6'.
    MOVE TABLEA-BUKRS TO TABLED-HSL06.
   WHEN '7'.
    MOVE TABLEA-BUKRS TO TABLED-HSL07.
   WHEN '8'.
    MOVE TABLEA-BUKRS TO TABLED-HSL08.
   WHEN '9'.
    MOVE TABLEA-BUKRS TO TABLED-HSL09.
   WHEN '10'.
    MOVE TABLEA-BUKRS TO TABLED-HSL10.
   WHEN '11'.
    MOVE TABLEA-BUKRS TO TABLED-HSL11.
   WHEN '12'.
    MOVE TABLEA-BUKRS TO TABLED-HSL12.
   WHEN '13'.
    MOVE TABLEA-BUKRS TO TABLED-HSL13.
   WHEN '14'.
    MOVE TABLEA-BUKRS TO TABLED-HSL14.
   WHEN '15'.
    MOVE TABLEA-BUKRS TO TABLED-HSL15.
   WHEN '16'.
    MOVE TABLEA-BUKRS TO TABLED-HSL16.
   WHEN '17'.
    MOVE TABLEA-BUKRS TO TABLED-HSL17.
   WHEN '18'.
    MOVE TABLEA-BUKRS TO TABLED-HSL18.
   WHEN '19'.
    MOVE TABLEA-BUKRS TO TABLED-HSL19.
   WHEN '20'.
    MOVE TABLEA-BUKRS TO TABLED-HSL20.
   WHEN '21'.
    MOVE TABLEA-BUKRS TO TABLED-HSL21.
   WHEN '22'.
    MOVE TABLEA-BUKRS TO TABLED-HSL22.
   WHEN '23'.
    MOVE TABLEA-BUKRS TO TABLED-HSL23.
   WHEN '24'.
    MOVE TABLEA-BUKRS TO TABLED-HSL24.
   WHEN '25'.
    MOVE TABLEA-BUKRS TO TABLED-HSL25.
   WHEN '26'.
    MOVE TABLEA-BUKRS TO TABLED-HSL26.
   ENDCASE.
  ENDLOOP.
  APPEND TABLED.

* This routine moves company code balances by G/L to Excel Matrix
  LOOP AT TABLEC.
   MOVE TABLEC-RACCT TO TABLED-RACCT.
   MOVE TABLEC-TXT20 TO TABLED-TXT20.

   MOVE TABLEC-HSL01 TO TABLED-HSL01.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL01.
   MOVE TABLEC-HSL02 TO TABLED-HSL02.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL02.
   MOVE TABLEC-HSL03 TO TABLED-HSL03.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL03.
   MOVE TABLEC-HSL04 TO TABLED-HSL04.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL04.
   MOVE TABLEC-HSL05 TO TABLED-HSL05.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL05.
   MOVE TABLEC-HSL06 TO TABLED-HSL06.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL06.
   MOVE TABLEC-HSL07 TO TABLED-HSL07.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL07.
   MOVE TABLEC-HSL08 TO TABLED-HSL08.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL08.
   MOVE TABLEC-HSL09 TO TABLED-HSL09.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL09.
   MOVE TABLEC-HSL10 TO TABLED-HSL10.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL10.
   MOVE TABLEC-HSL11 TO TABLED-HSL11.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL11.
   MOVE TABLEC-HSL12 TO TABLED-HSL12.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL12.
   MOVE TABLEC-HSL13 TO TABLED-HSL13.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL13.
   MOVE TABLEC-HSL14 TO TABLED-HSL14.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL14.
   MOVE TABLEC-HSL15 TO TABLED-HSL15.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL15.
   MOVE TABLEC-HSL16 TO TABLED-HSL16.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL16.
   MOVE TABLEC-HSL17 TO TABLED-HSL17.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL17.
   MOVE TABLEC-HSL18 TO TABLED-HSL18.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL18.
   MOVE TABLEC-HSL19 TO TABLED-HSL19.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL19.
   MOVE TABLEC-HSL20 TO TABLED-HSL20.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL20.
   MOVE TABLEC-HSL21 TO TABLED-HSL21.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL21.
   MOVE TABLEC-HSL22 TO TABLED-HSL22.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL22.
   MOVE TABLEC-HSL23 TO TABLED-HSL23.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL23.
   MOVE TABLEC-HSL24 TO TABLED-HSL24.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL24.
   MOVE TABLEC-HSL25 TO TABLED-HSL25.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL25.
   MOVE TABLEC-HSL26 TO TABLED-HSL26.
   PERFORM SHIFT_NEGATIVE USING TABLED-HSL26.

   APPEND TABLED.

  ENDLOOP.

PERFORM DISPLAY_REPORT.


AT PF8.
 PERFORM CREATE_EXCEL_SPREADSHEET.
*-----------------------------------------------------------------------

************************************************************************
*                Subroutines used by program
************************************************************************
*----------------------- DISPLAY_REPORT --------------------------------
* Displays report with subtotals & grand total for report
*-----------------------------------------------------------------------
FORM DISPLAY_REPORT.
  SKIP 5.
  WRITE: /30 TEXT-001.
  SKIP 5.
  WRITE: /30 TEXT-002.
ENDFORM.

************************************************************************
* Excel Spreadsheet Routines
************************************************************************
*------------------------ CREATE_ESCEL_SPREADSHEET ---------------------
* This routine copied from "Developing SAP's R/3 Applications pg. 566  *
*-----------------------------------------------------------------------
FORM CREATE_EXCEL_SPREADSHEET.
 INCLUDE OLE2INCL.
 DATA: ZFFIR003 TYPE OLE2_OBJECT,
       WORKBOOK TYPE OLE2_OBJECT,
       SHEET    TYPE OLE2_OBJECT,
       CELLS    TYPE OLE2_OBJECT,
       INDEX    TYPE I,
       ROW_MAX  TYPE I VALUE 256.
 FIELD-SYMBOLS: <NAME>.


* This section creates the spreadsheet, workbooks, worksheets & cells
* and moves the entries from the internal table to the CELLS

  CREATE OBJECT ZFFIR003 'excel.application'.
  SET PROPERTY OF ZFFIR003 'visible' = 1.
  CALL METHOD OF ZFFIR003 'Workbooks' = WORKBOOK.
  CALL METHOD OF WORKBOOK 'Add'.
  CALL METHOD OF ZFFIR003 'Worksheets' = SHEET
                   EXPORTING #1 = 1.
  CALL METHOD OF SHEET 'Activate'.


  LOOP AT TABLED.
    INDEX = ROW_MAX * ( SY-TABIX - 1 ) + 1.     "Row 1 (cells (1- 256)
    DO 28 TIMES.
      ASSIGN COMPONENT SY-INDEX OF STRUCTURE TABLED TO <NAME>.
      CALL METHOD OF SHEET 'Cells' = CELLS
                  EXPORTING #1 = INDEX.
      SET PROPERTY OF CELLS 'Value' = <NAME>.
      ADD 1 TO INDEX.
    ENDDO.
  ENDLOOP.

ENDFORM.


*---------------------- SHIFT_NEGATIVE ---------------------------------
* This routine checks for a trailing negative sign in the amount field
* and if found, moves it as a leading sign.  This is required because
* fields with trailing signs are treated as characters in Excel but
* leading signs are treated as positives.
*-----------------------------------------------------------------------
FORM SHIFT_NEGATIVE USING AMT.
   SEARCH AMT FOR '-'.
   IF SY-SUBRC = '0'.
     SHIFT AMT UP TO '-' CIRCULAR.
   ENDIF.
ENDFORM.
