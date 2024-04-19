REPORT ZFGLR005 LINE-SIZE 132 NO STANDARD PAGE HEADING.
******************************************************************
*       Owner: Centra/Union                                      *
*  Programmer: Yaxin Veliz  - OmniLogic Systems Group (Toronto)  *
*        Date: July 19, 1996                                     *
*  Request ID: DRFI0026                                          *
*                                                                *
* This program will produce a report that would be used to       *
* reconcile the GST return with 7% of the revenue items that are *
* taxable: Billed Gas Sales, T-Service, and Rentals & Leases.    *
*                                                                *
******* ************** ***************** *********** *************
* 2005/08/29mkhan TR105 Select Actual amount only as PLAN amount *
*                     is going to be introduced in FI.           *
******* ************** ***************** *********** *************
*  modified by Nancy Gilligan, OmniLogic 98/10    D30K906115     *
*   - changed title, standardized headers, made GSt a parameter, *
*     set company code based on client, took out hard coding of  *
*     months and selected short text from t247                   *
******************************************************************

TABLES: BSEG,     "Accounting Document Segment
        GLT0,     "G/L Account Master Record Monthly Debits and Credits.
        SKB1,     "G/L Account Master (Company Codes)
        T001,     "Company Codes                        "ngilligan 98/10
        T247.     "month text

DATA:  " gst(3)    type p decimals 2 value '.07',  - made into parameter
        GSTAMT1   TYPE P DECIMALS 2,             "GST for Gas
        GSTAMT2   LIKE GSTAMT1,                  "GST for T-Service
        GSTAMT3   LIKE GSTAMT1,                  "GST for Rent/Leases
        AMOUNT1   LIKE GSTAMT1,                  "Amount for Gas
        AMOUNT2   LIKE GSTAMT1,                  "Amount for T-Service
        AMOUNT3   LIKE GSTAMT1,                  "Amount for Rent/Leases
        HOLD1     LIKE GSTAMT1,                  "Output field for Gas
        HOLD2     LIKE GSTAMT1,                  "Output field for T-S
        HOLD3     LIKE GSTAMT1,                  "Output field for Rent
        SAVE1     LIKE GSTAMT1,                  "Output for GST Gas
        SAVE2     LIKE GSTAMT1,                  "Output for GST T-Serv
        SAVE3     LIKE GSTAMT1,                  "Output for GST Rent
        FINAMT1   LIKE GSTAMT1,                  "Total for Gas
        FINAMT2   LIKE GSTAMT1,                  "Total for T-Service
        FINAMT3   LIKE GSTAMT1,                  "Total for Rent
        FINGST1   LIKE GSTAMT1,                  "Total for GST Gas
        FINGST2   LIKE GSTAMT1,                  "Total for GST T-Serv
        FINGST3   LIKE GSTAMT1,                  "Total for GST Rent
        TOTGST(7) TYPE P DECIMALS 2,             "Total for GST calc.
        GRDGST    LIKE TOTGST,                   "Final total for GST
        MONTH     TYPE I VALUE '1',              "Month counter variable
        TEXT(4)   TYPE C.                        "Text string for Months

* This first block produces a border like frame around the following: *

SELECTION-SCREEN BEGIN OF BLOCK
    INTRO WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 22(31) TEXT-001 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 31(12) TEXT-002 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN SKIP.

* The following second block produces a frame within the first block. *

SELECTION-SCREEN BEGIN OF BLOCK
    FISCAL WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 02(13) TEXT-003.
PARAMETERS:
  P_YEAR(4)  OBLIGATORY MODIF ID ABC.                 "Fiscal year input
SELECTION-SCREEN: COMMENT 28(14) TEXT-004.
PARAMETERS:
  P_COMPNY LIKE BSEG-BUKRS
              OBLIGATORY MODIF ID ABC.   "Company Code
SELECTION-SCREEN: COMMENT 55(10) TEXT-020.
PARAMETERS: P_GST(3) TYPE P DECIMALS 2 DEFAULT '0.07'
                      OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

* Second block ends after this statement. *

SELECTION-SCREEN END OF BLOCK FISCAL.

SELECTION-SCREEN SKIP.

* This third block produces a frame around the first block. *

SELECTION-SCREEN BEGIN OF BLOCK
     ACCOUNT WITH FRAME TITLE TEXT-005.

SELECTION-SCREEN SKIP.
SELECT-OPTIONS:
  S_GASACT FOR SKB1-SAKNR MODIF ID ABC.       "Gas Sales Accounts
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:
  S_TSERV  FOR SKB1-SAKNR MODIF ID ABC.       "T-Service Account
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:
  S_RNTLES FOR SKB1-SAKNR MODIF ID ABC.       "Rentals & Leases Accounts

* This statement ends the third block. *

SELECTION-SCREEN END OF BLOCK ACCOUNT.

SELECTION-SCREEN SKIP.

* This statement ends the first block. *

SELECTION-SCREEN END OF BLOCK INTRO.

* initialization
INITIALIZATION.
IF SY-MANDT+2(1) = '1'.
  P_COMPNY = 'UEC'.
ELSEIF  SY-MANDT+2(1) = '0'.
  P_COMPNY = 'UGL'.
ENDIF.


* The following will highlight the screen's output for certain texts. *

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'ABC'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.

*********************BEGINNING OF MAIN PROGRAM *************************

START-OF-SELECTION.
PERFORM INITIALIZE.
PERFORM WRT_HDG.
WHILE MONTH < 13.
  PERFORM GETDATA.
ENDWHILE.
PERFORM WRT_FINAL.
END-OF-SELECTION.

*************************BEGINNING OF DISPLAY***************************
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         25 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
        105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-006 UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10



*************************BEGINNING OF FORMS*****************************

* This routine intializes and re-sets the following values. *

FORM INITIALIZE.

* get company code text
SELECT SINGLE BUTXT FROM T001 INTO T001-BUTXT WHERE BUKRS = P_COMPNY.

CLEAR: HOLD1, HOLD2, HOLD3, SAVE1, SAVE2, SAVE3, TOTGST,
       AMOUNT1, AMOUNT2, AMOUNT3, GSTAMT1, GSTAMT2, GSTAMT3.
ENDFORM.

* This routine writes the sub headings. *

FORM WRT_HDG.
NEW-PAGE.
*write: /31 text-006.
*write:  29 sy-vline, 100 sy-vline.
*uline: /29(72).
*uline: /1(21).
WRITE: /3 TEXT-013, 16 P_YEAR.
WRITE:  1 SY-VLINE, 21 SY-VLINE.
ULINE: /1(21).
SKIP.
ULINE: /.
WRITE: /1 SY-VLINE, 8 SY-VLINE, 65 SY-VLINE, 132 SY-VLINE.
WRITE:  30  TEXT-007,  91 TEXT-008.
WRITE: /1 SY-VLINE, 8 SY-VLINE, 65 SY-VLINE, 132 SY-VLINE.
ULINE: /.
WRITE: /1 SY-VLINE, 2 TEXT-014, 14 TEXT-009, 32 TEXT-010, 49 TEXT-011,
        69 TEXT-009, 86 TEXT-010, 100 TEXT-011, 120 TEXT-012.
PERFORM VERTICAL1.
ULINE: /.
PERFORM VERTICAL2.
FORMAT INTENSIFIED OFF.
ENDFORM.

* This routine selects all the specified information for GAS, T-SERVICE,
* and RENT/LEASES accounts giving their monthly figures.               *

FORM GETDATA.

*get month from t247
SELECT SINGLE KTX FROM T247 INTO TEXT WHERE SPRAS = SY-LANGU
                                        AND MNR   = MONTH.

SELECT * FROM GLT0
         WHERE RRCTY = '0'                     "TR105
         AND BUKRS = P_COMPNY
         AND RYEAR = P_YEAR
         AND RACCT IN S_GASACT.                "All info for GAS
         IF S_GASACT <> ' '.
           PERFORM CALCULATE USING MONTH
                   CHANGING AMOUNT1 GSTAMT1.
         ENDIF.
ENDSELECT.
TOTGST = TOTGST + GSTAMT1.

MOVE AMOUNT1 TO HOLD1.
MOVE GSTAMT1 TO SAVE1.


SELECT * FROM GLT0
              WHERE RRCTY = '0'                "TR105
              AND BUKRS = P_COMPNY
              AND RYEAR = P_YEAR
              AND RACCT IN S_TSERV.            "All info for T-Service
              IF S_TSERV <> ' '.
                PERFORM CALCULATE USING MONTH
                        CHANGING AMOUNT2 GSTAMT2.
              ENDIF.
ENDSELECT.
TOTGST = TOTGST + GSTAMT2.
MOVE AMOUNT2 TO HOLD2.
MOVE GSTAMT2 TO SAVE2.

SELECT * FROM GLT0
              WHERE RRCTY = '0'                "TR105
              AND BUKRS = P_COMPNY
              AND RYEAR = P_YEAR
              AND RACCT IN S_RNTLES.           "All info for Rent/Leases
              IF S_RNTLES <> ' '.
                PERFORM CALCULATE USING MONTH
                        CHANGING AMOUNT3 GSTAMT3.
              ENDIF.
ENDSELECT.
TOTGST = TOTGST + GSTAMT3.
MOVE AMOUNT3 TO HOLD3.
MOVE GSTAMT3 TO SAVE3.
PERFORM WRT_SUMMARY.
PERFORM ACCUMULATE.
PERFORM INITIALIZE.
ENDFORM.

* This routine Calculates and Accumulates all values for the Months. *

FORM CALCULATE USING MONTH CHANGING AMOUNT1 GSTAMT1.

SELECT SINGLE KTX FROM T247 INTO TEXT WHERE SPRAS = SY-LANGU
                                        AND MNR   = MONTH.
IF MONTH = '1'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL01.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Jan.'.
ELSEIF MONTH = '2'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL02.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Feb.'.
ELSEIF MONTH = '3'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL03.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Mar.'.
ELSEIF MONTH = '4'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL04.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Apr.'.
ELSEIF MONTH = '5'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL05.
  GSTAMT1 = AMOUNT1 * P_GST.
*  text   =  'May.'.
ELSEIF MONTH = '6'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL06.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Jun.'.
ELSEIF MONTH = '7'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL07.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Jul.'.
ELSEIF MONTH = '8'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL08.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Aug.'.
ELSEIF MONTH = '9'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL09.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Sep.'.
ELSEIF MONTH = '10'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL10.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Oct.'.
ELSEIF MONTH = '11'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL11.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Nov.'.
ELSEIF MONTH = '12'.
  AMOUNT1 = AMOUNT1 + GLT0-TSL12.
  GSTAMT1 = AMOUNT1 * P_GST.
* text    = 'Dec.'.
ENDIF.
ENDFORM.

* This routine writes the summary of Gas, T-Service, and Rent/Leases   *
* a long with their calculated figures for each month.                 *

FORM WRT_SUMMARY.
PERFORM VERTICAL1.
WRITE: /2 TEXT, 10 HOLD1, 29 HOLD2, 48 HOLD3,
       65 SAVE1, 82 SAVE2, 99 SAVE3, 117 TOTGST.
PERFORM VERTICAL1.
WRITE:  9 '$', 28 '$', 47 '$', 66 '$', 83 '$', 100 '$', 117 '$'.
PERFORM VERTICAL2.
ENDFORM.

* This routine accumulates all final values. *

FORM ACCUMULATE.
FINAMT1 = FINAMT1 + AMOUNT1.
FINAMT2 = FINAMT2 + AMOUNT2.
FINAMT3 = FINAMT3 + AMOUNT3.
FINGST1 = FINGST1 + GSTAMT1.
FINGST2 = FINGST2 + GSTAMT2.
FINGST3 = FINGST3 + GSTAMT3.
GRDGST  = GRDGST + TOTGST.
MONTH = MONTH + 1.
ENDFORM.

* This routine writes the final totals for Gas, T-Service, and Rent &  *
* Leases accounts.                                                     *

FORM WRT_FINAL.
FORMAT INTENSIFIED ON.
ULINE: 1(132).
PERFORM VERTICAL2.
FORMAT COLOR COL_TOTAL ON INTENSIFIED ON.
WRITE: /2 TEXT-015, 10 FINAMT1, 29 FINAMT2, 48 FINAMT3,
       65 FINGST1, 82 FINGST2, 99 FINGST3, 117 GRDGST.
FORMAT COLOR COL_TOTAL OFF INTENSIFIED OFF.
PERFORM VERTICAL1.
FORMAT COLOR COL_TOTAL ON INTENSIFIED ON.
WRITE:  9 '$', 28 '$', 47 '$', 66 '$', 83 '$', 100 '$', 117 '$'.
FORMAT COLOR COL_TOTAL OFF INTENSIFIED OFF.
PERFORM VERTICAL2.
ULINE: 1(132).
ENDFORM.

* This routine writes the vertical lines on the report. *

FORM VERTICAL1.
WRITE: 1 SY-VLINE, 8 SY-VLINE, 27 SY-VLINE, 46 SY-VLINE, 65 SY-VLINE,
       82 SY-VLINE, 99 SY-VLINE, 116 SY-VLINE, 132 SY-VLINE.
ENDFORM.

* This routine writes the vertical lines on the report after a line. *

FORM VERTICAL2.
WRITE: /1 SY-VLINE, 8 SY-VLINE, 27 SY-VLINE, 46 SY-VLINE, 65 SY-VLINE,
       82 SY-VLINE, 99 SY-VLINE, 116 SY-VLINE, 132 SY-VLINE.
ENDFORM.
