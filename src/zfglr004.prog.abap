REPORT ZFGLR004 NO STANDARD PAGE HEADING LINE-SIZE 132.

******************************************************************
*       Owner: Centra/Union                                      *
*  Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)   *
*        Date: November 13th, 1996.                              *
*  Request ID: DRFI0028                                          *
*                                                                *
* This program will produce an Addback Expense Report displaying *
* paid expense Meals items with their specified value of expense *
* addback.                                                       *
******************************************************************
* 98/009/30 md7140 #--- Separation - changed BSAK to BKPF
******************************************************************

*ables: bsak,"Secondary Index Accounts for G/L Accounts - Cleared Items
TABLES: BKPF, "Accounting Document Header
        BSEG, "Accounting Document Segment
        SKB1, "G/L Account Master (Company Code)
        T001A,"Company Codes
        T001, "Company Names
        T003A."Document Types For Posting With Clearing
DATA:
        MWSKZ LIKE BSEG-MWSKZ,                  "Tax Code
        GJAHR LIKE BSEG-GJAHR,                  "Fiscal Year
        BLART LIKE BKPF-BLART,                  "Document Type
        BUKRS LIKE BSEG-BUKRS,                  "Company Code
        BUDAT LIKE BKPF-BUDAT,                  "Posting Date
        ADBKGST TYPE P DECIMALS 2,              "Addback GST Calculation
        TOTAL2  LIKE BSEG-DMBTR,                "Total
        TOTAL3  LIKE ADBKGST,                   "Addback Total
        TOTAL4  TYPE P DECIMALS 2,              "Addback GST Total

      BEGIN OF WTAB OCCURS 500,
        HKONT LIKE BSEG-HKONT,                  "G/L Account
        MWSKZ LIKE BSEG-MWSKZ,                  "Tax Code
        BLART LIKE BKPF-BLART,                  "Document Type
        BELNR LIKE BSEG-BELNR,                  "Document Reference No.
        WRBTR LIKE BSEG-WRBTR,                  "Amount
        BUKRS LIKE BSEG-BUKRS,                  "Company Code
        BUDAT LIKE BKPF-BUDAT,                  "Posting Date
        GJAHR LIKE BSEG-GJAHR,                  "Fiscal Year
        ADDBACK LIKE BSEG-WRBTR,                "Addback Calculation
      END OF WTAB,

      BEGIN OF ACT OCCURS 500,
        BELNR LIKE BSEG-BELNR,
        WRBTR LIKE BSEG-WRBTR,
        HKONT LIKE BSEG-HKONT,
      END OF ACT.
DATA:
        POST1      TYPE D,                      "First Date Range
        POST2      LIKE POST1,                  "Second Date Range
        LN_CNTR    TYPE I.                      "Line Counter

SELECTION-SCREEN BEGIN OF BLOCK
    INTRO WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 25(28) TEXT-007.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN ULINE  25(28).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(14) TEXT-005 MODIF ID ABC.

PARAMETERS:  P_COMPNY LIKE T001A-BUKRS                 "Company Code
                       OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN: COMMENT 40(17) TEXT-004 MODIF ID ABC.
PARAMETERS:  P_ADDPER(2) TYPE P DEFAULT 50 OBLIGATORY
                             MODIF ID ABC.              "Addback Percent
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK
    PERIOD WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 31(10) TEXT-008 MODIF ID ABC.
SELECTION-SCREEN: COMMENT 56(10) TEXT-008 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 17(13) TEXT-002.
PARAMETERS: P_DAY1(2)   DEFAULT '01'           OBLIGATORY MODIF ID ABC,
            P_MONTH1(2) DEFAULT '01'           OBLIGATORY MODIF ID ABC,
            P_YEAR1(4)  DEFAULT SY-DATUM(4)    OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN: COMMENT 42(13) TEXT-003.
PARAMETERS: P_DAY2(2)   DEFAULT SY-DATUM+4(2)  OBLIGATORY MODIF ID ABC,
            P_MONTH2(2) DEFAULT SY-DATUM+6(2)  OBLIGATORY MODIF ID ABC,
            P_YEAR2(4)  DEFAULT SY-DATUM(4)    OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK PERIOD.

SELECTION-SCREEN BEGIN OF BLOCK
     ACCNT WITH FRAME.

SELECT-OPTIONS: S_GLACCT FOR SKB1-SAKNR DEFAULT '0000415002'
                         NO INTERVALS OBLIGATORY
                         MODIF ID ABC,         "G/L Accounts
                S_TXCODE FOR BSEG-MWSKZ DEFAULT 'I4'
                         NO INTERVALS              "98/10/05 per Wilma
*                        no intervals obligatory
                         MODIF ID ABC,         "Tax Codes
                S_DOCTYP FOR T003A-BLART DEFAULT 'KN'
                         NO INTERVALS              "98/10/05 per Wilma
*                        no intervals obligatory
                         MODIF ID ABC.         "Document Types
SELECTION-SCREEN END OF BLOCK ACCNT.
PARAMETER: P_HKONT LIKE BSEG-HKONT DEFAULT '256950' OBLIGATORY
                             MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK INTRO.

* The following will highlight the screen's output for certain texts. *
AT SELECTION-SCREEN OUTPUT.

 IF SY-MANDT+2(1) = '0'.       "Write appropriate company code to screen
    MOVE 'UGL' TO P_COMPNY.
 ELSE.
 IF SY-MANDT+2(1) = '1'.
    MOVE 'UEC' TO P_COMPNY.
 ENDIF. ENDIF.

 LOOP AT SCREEN.
  CHECK SCREEN-GROUP1 = 'ABC'.
  SCREEN-INTENSIFIED = '1'.
  MODIFY SCREEN.
 ENDLOOP.

*********************BEGINNING OF MAIN PROGRAM *************************

START-OF-SELECTION.
 SELECT SINGLE * FROM T001
   WHERE BUKRS = P_COMPNY.

 PERFORM INITIALIZE.
 PERFORM GET_PAY.
 PERFORM DSP_INFO.
END-OF-SELECTION.

*************************BEGINNING OF FORMS*****************************

* This routine will initialize some variables and concatenate others. *
FORM INITIALIZE.
 REFRESH: WTAB, ACT.
 CONCATENATE P_YEAR1 P_DAY1 P_MONTH1 INTO POST1. "Combines date to post1
 CONCATENATE P_YEAR2 P_DAY2 P_MONTH2 INTO POST2. "Combines date to post2
ENDFORM.

* This routine will get all info. for the cleared items selected. *
FORM GET_PAY.
 SELECT * FROM BKPF
    WHERE BLART IN S_DOCTYP
     AND BUKRS = P_COMPNY
      AND BUDAT BETWEEN POST1 AND POST2.
*    order by hkont mwskz budat.
   SELECT * FROM BSEG
       WHERE BUKRS = BKPF-BUKRS
         AND BELNR = BKPF-BELNR
         AND GJAHR = BKPF-GJAHR
         AND MWSKZ IN S_TXCODE
         AND HKONT IN S_GLACCT.
         PERFORM POP_WTAB.
   ENDSELECT.                        "End of BSEG
 ENDSELECT.                          "End of BSAK
ENDFORM.

* This routine will process all relevant info. and display it. *
FORM DSP_INFO.
 SORT: WTAB BY HKONT MWSKZ BLART BELNR.
 LOOP AT WTAB.

   ON CHANGE OF WTAB-BELNR OR WTAB-MWSKZ.
      MWSKZ = WTAB-MWSKZ.
      BUDAT = WTAB-BUDAT.
      BLART = WTAB-BLART.
      PERFORM GET_ACT.
      PERFORM PROC_ACT.
   ENDON.

   AT NEW HKONT.
      ULINE.
      WRITE: /1 TEXT-029, WTAB-HKONT.
      PERFORM COLUMN_HEADINGS.
   ENDAT.


   AT END OF BELNR.
      SUM.
*     perform get_act.
      WRITE: / WTAB-BELNR   UNDER TEXT-009, MWSKZ UNDER TEXT-011,
               WTAB-WRBTR   UNDER TEXT-017, BLART UNDER TEXT-013,
                    BUDAT   UNDER TEXT-015,
               WTAB-ADDBACK UNDER TEXT-027.
      PERFORM PROC_ACT.
      WRITE: ACT-WRBTR UNDER TEXT-019, ADBKGST UNDER TEXT-028.
   ENDAT.

   AT END OF HKONT.
      SUM.
      SKIP.
      FORMAT COLOR COL_TOTAL ON INTENSIFIED ON.
*     perform proc_act.
      WRITE: /3 TEXT-026,
               WTAB-WRBTR   UNDER TEXT-017, MWSKZ  UNDER TEXT-012,
               WTAB-ADDBACK UNDER TEXT-027,
               TOTAL2    UNDER TEXT-019,    TOTAL4 UNDER TEXT-028.
      FORMAT COLOR COL_TOTAL OFF INTENSIFIED OFF.
      PERFORM RSH_CLR.
   ENDAT.
   CLEAR: TOTAL2,  TOTAL4.
 ENDLOOP.
ENDFORM.

* This routine will get all pertinant info. for GST. *
FORM GET_ACT.
   SELECT * FROM BSEG
     WHERE BELNR = WTAB-BELNR
       AND BUKRS = WTAB-BUKRS
       AND GJAHR = WTAB-GJAHR
       AND HKONT = P_HKONT
*      and mwskz in s_txcode.         "MD7140
       AND MWSKZ = WTAB-MWSKZ.       "MD7140
       PERFORM POP_ACT.
    ENDSELECT.
ENDFORM.

* This routine will process records the GST. *
FORM PROC_ACT.
 LOOP AT ACT.
   TOTAL2 = TOTAL2 + ACT-WRBTR.
   COMPUTE: ADBKGST = ( ACT-WRBTR * ( P_ADDPER / 100 ) ).
   TOTAL4 = TOTAL4 + ADBKGST.
 ENDLOOP.
ENDFORM.

* This routine will populate the table wtab. *
FORM POP_WTAB.
 CLEAR: WTAB.
 IF BSEG-SHKZG = 'S'.                          "Debit/Credit Amounts
    MOVE BSEG-WRBTR TO WTAB-WRBTR.
 ELSE.
   IF BSEG-SHKZG = 'H'.
    COMPUTE WTAB-WRBTR = BSEG-WRBTR * ( -1 ).
   ENDIF.
 ENDIF.

 MOVE:  BKPF-BLART TO WTAB-BLART,
        BSEG-MWSKZ TO WTAB-MWSKZ,
        BSEG-BUKRS TO WTAB-BUKRS,
        BKPF-BUDAT TO WTAB-BUDAT,
        BSEG-GJAHR TO WTAB-GJAHR,
        BSEG-BELNR TO WTAB-BELNR,
        BSEG-HKONT TO WTAB-HKONT.
     COMPUTE: WTAB-ADDBACK = ( WTAB-WRBTR * ( P_ADDPER / 100 ) ).
 APPEND WTAB.
ENDFORM.

* This routine will populate the accounting tax. *
FORM POP_ACT.
 CLEAR ACT.
 MOVE: BSEG-BELNR TO ACT-BELNR,
       BSEG-HKONT TO ACT-HKONT.
 IF BSEG-SHKZG = 'S'.                              "Debit/Credit Amts
    MOVE BSEG-WRBTR TO ACT-WRBTR.
 ELSE.
    IF BSEG-SHKZG = 'H'.
       ACT-WRBTR = BSEG-WRBTR * ( - 1 ).
    ENDIF.
 ENDIF.

 APPEND ACT.
ENDFORM.

* This routine will refresh and clear a table and variables. *
FORM RSH_CLR.
 REFRESH ACT.
 CLEAR: ACT, TOTAL3, TOTAL2, TOTAL4.
ENDFORM.

* This routine writes the sub headings. *
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID, 50 TEXT-TTL,
        105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT   UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
           T001-BUTXT UNDER TEXT-TTL,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
  WRITE: / TEXT-022 UNDER TEXT-RPT, P_COMPNY,
         36 TEXT-002, 48 P_DAY1, 50 '/', 51 P_MONTH1,53 '/',54 P_YEAR1,
         58 TEXT-003,  70 P_DAY2, 72 '/', 73 P_MONTH2,75 '/',76 P_YEAR2,
            TEXT-023 UNDER TEXT-DTE, P_ADDPER.


FORM COLUMN_HEADINGS.
  ULINE.
  SKIP.
  WRITE: /1 TEXT-009, 15 TEXT-011,  30 TEXT-013,  45 TEXT-015,
         60 TEXT-017, 78 TEXT-019,  96 TEXT-027, 114 TEXT-028.
  WRITE: / TEXT-010 UNDER TEXT-009, TEXT-012 UNDER TEXT-011,
           TEXT-014 UNDER TEXT-013, TEXT-016 UNDER TEXT-015,
           TEXT-018 UNDER TEXT-017, TEXT-018 UNDER TEXT-019,
           TEXT-020 UNDER TEXT-027, TEXT-020 UNDER TEXT-028.
 ULINE:  /.
ENDFORM.
