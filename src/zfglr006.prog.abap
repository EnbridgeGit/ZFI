REPORT ZFGLR006 NO STANDARD PAGE HEADING LINE-SIZE 132.

***********************************************************************
*       Owner: Centra/Union                                           *
*  Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)        *
*        Date: November 12th, 1996                                    *
*  Request ID: DRFI0023 - Track GST on expenses related to Risk Mgmt. *
*  Request ID: DRFI0027 - Track payments on leased vehicles by Mgrs.  *
*  Request ID: DRFI0029 - Special Payment/Membership Report.          *
*                                                                     *
* This program allows you to report on multiple G/L Accounts,         *
* multiple Tax Codes, and a dollar range to accomodate all of the     *
* above requests.                                                     *
***********************************************************************

TABLES: BSAS, "Secondary Index Accounts for G/L Accounts - Cleared Items
        BSEG, "Accounting Document Segment
        SKAT, "G/L Account Master Record Chart of Accounts - Description
        SKB1, "G/L Account Master (Company Code)
        T001A,"Company Codes
        T003A."Document Types For Posting With Clearing

DATA:
     BEGIN OF WTAB OCCURS 500,
       BLART LIKE BSAS-BLART,                  "Document Type
       MWSKZ LIKE BSEG-MWSKZ,                  "Tax Code
       HKONT LIKE BSEG-HKONT,                  "G/L Account
       WRBTR LIKE BSEG-WRBTR,                  "Amount
       BUKRS LIKE BSEG-BUKRS,                  "Company Code
       AUGDT LIKE BSAS-AUGDT,                  "Clearing Date
       GJAHR LIKE BSEG-GJAHR,                  "Fiscal Year
       BELNR LIKE BSEG-BELNR,                  "Document Reference No.
      END OF WTAB.
DATA:
      POST1      TYPE D,                       "First Date Range
      POST2      LIKE POST1,                   "Second Date Range
      LN_CNTR    TYPE I.                       "Line Counter

* This first block produces a border like frame around the following: *
SELECTION-SCREEN BEGIN OF BLOCK
    INTRO WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 23(34) TEXT-001 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN ULINE  23(34).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(14) TEXT-002 MODIF ID ABC.
PARAMETERS:
  P_COMPNY LIKE T001A-BUKRS DEFAULT 'UGL'       "Company Code
           OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

* The following second block produces a frame within the first block: *
SELECTION-SCREEN BEGIN OF BLOCK
    PERCENT WITH FRAME.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(29) TEXT-003 MODIF ID ABC.
PARAMETERS: P_TITLE(40) TYPE C MODIF ID ABC.   "Report Title
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* Second block ends after this statement. *
SELECTION-SCREEN END OF BLOCK PERCENT.

SELECTION-SCREEN SKIP.

* The following third block produces a frame within the first block. *
SELECTION-SCREEN BEGIN OF BLOCK
    PERIOD WITH FRAME TITLE TEXT-004.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 31(10) TEXT-005 MODIF ID ABC.
SELECTION-SCREEN: COMMENT 56(10) TEXT-005 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 17(13) TEXT-006.
PARAMETERS: P_DAY1(2)   DEFAULT '01'           OBLIGATORY MODIF ID ABC,
            P_MONTH1(2) DEFAULT '01'           OBLIGATORY MODIF ID ABC,
            P_YEAR1(4)  DEFAULT SY-DATUM(4)    OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN: COMMENT 42(13) TEXT-007.
PARAMETERS: P_DAY2(2)   DEFAULT SY-DATUM+4(2)  OBLIGATORY MODIF ID ABC,
            P_MONTH2(2) DEFAULT SY-DATUM+6(2)  OBLIGATORY MODIF ID ABC,
            P_YEAR2(4)  DEFAULT SY-DATUM(4)    OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

* This statement ends the third block. *
SELECTION-SCREEN END OF BLOCK PERIOD.

* This fourth block produces a frame around the first block. *
SELECTION-SCREEN BEGIN OF BLOCK
     ACCNT WITH FRAME.


SELECT-OPTIONS: S_GLACCT FOR SKB1-SAKNR NO INTERVALS OBLIGATORY
                         MODIF ID ABC,         "G/L Accounts
                S_TXCODE FOR BSEG-MWSKZ NO INTERVALS
                         MODIF ID ABC,         "Tax Codes
                S_DOCTYP FOR T003A-BLART NO INTERVALS OBLIGATORY
                         MODIF ID ABC.          "Document Types

* This statement ends the fourth block. *
SELECTION-SCREEN END OF BLOCK ACCNT.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK DOLLAR
                 WITH FRAME TITLE TEXT-022.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(13)  TEXT-023 MODIF ID ABC.
SELECTION-SCREEN: COMMENT 23(7) TEXT-024.
PARAMETERS:
  P_AMT1(6) TYPE P DECIMALS 2 DEFAULT '.05'
            OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN: COMMENT 50(5) TEXT-025.     "First Amount "FROM" Ramge
PARAMETERS:
  P_AMT2(6) TYPE P DECIMALS 2 DEFAULT '100000'
            OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.                 "Second Amount "TO" Range

SELECTION-SCREEN END OF BLOCK DOLLAR.

* This statement ends the first block. *
SELECTION-SCREEN END OF BLOCK INTRO.

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
 PERFORM GET_PAY.
 PERFORM DSP_INFO.
END-OF-SELECTION.

*************************BEGINNING OF FORMS*****************************

* This routine will initialize some variables and concatenate others. *
FORM INITIALIZE.
 REFRESH: WTAB.
 MOVE '60' TO LN_CNTR.
 CONCATENATE P_YEAR1 P_DAY1 P_MONTH1 INTO POST1. "Combines date to post1
 CONCATENATE P_YEAR2 P_DAY2 P_MONTH2 INTO POST2. "Combines date to post2
ENDFORM.

* This routine will get all info. for the cleared items selected. *
FORM GET_PAY.
 SELECT * FROM BSAS WHERE BLART IN S_DOCTYP AND BUKRS = P_COMPNY
          AND AUGDT BETWEEN POST1 AND POST2
          ORDER BY HKONT MWSKZ BUDAT.
   SELECT * FROM BSEG WHERE BELNR = BSAS-BELNR
            AND BUKRS = BSAS-BUKRS AND GJAHR = BSAS-GJAHR
            AND WRBTR BETWEEN P_AMT1 AND P_AMT2
            AND MWSKZ IN S_TXCODE
            AND HKONT IN S_GLACCT.
            PERFORM POP_WTAB.
   ENDSELECT.
 ENDSELECT.
ENDFORM.

* This routine will process all relevant info. and display it. *
FORM DSP_INFO.
 SORT: WTAB BY HKONT MWSKZ BLART BELNR. "MWSKZ BLART.
 LOOP AT WTAB.
   AT NEW HKONT. "BELNR.
      IF LN_CNTR >= 59.
         PERFORM WRT_HDG.
      ENDIF.
      SUM.
      WRITE: /1 WTAB-HKONT, 13 WTAB-MWSKZ, 20 WTAB-BLART, 30 WTAB-WRBTR.
      LN_CNTR = LN_CNTR + 1.
   ENDAT.
   SELECT * FROM BSEG WHERE BELNR = WTAB-BELNR AND BUKRS = WTAB-BUKRS
            AND GJAHR = WTAB-GJAHR.
     SELECT * FROM SKAT WHERE KTOPL = 'COAT' AND SPRAS = 'E'
              AND SAKNR = BSEG-HKONT.
       IF LN_CNTR >= 59.
          PERFORM WRT_HDG.
       ENDIF.
       WRITE: /48 BSEG-BELNR, 60 BSEG-AUGDT, 72 BSEG-WRBTR,
               97 BSEG-HKONT,  110 SKAT-TXT20.
       LN_CNTR = LN_CNTR + 1.
     ENDSELECT.
   ENDSELECT.
 ENDLOOP.
ENDFORM.

* This routine will populate the table wtab. *
FORM POP_WTAB.
 CLEAR: WTAB.
 MOVE:  BSAS-BLART TO WTAB-BLART,
        BSEG-WRBTR TO WTAB-WRBTR,
        BSEG-MWSKZ TO WTAB-MWSKZ,
        BSEG-BUKRS TO WTAB-BUKRS,
        BSAS-AUGDT TO WTAB-AUGDT,
        BSEG-GJAHR TO WTAB-GJAHR,
        BSEG-BELNR TO WTAB-BELNR,
        BSEG-HKONT TO WTAB-HKONT.
 APPEND WTAB.
ENDFORM.

* This routine writes the sub headings. *
FORM WRT_HDG.
 NEW-PAGE WITH-TITLE.
 CLEAR LN_CNTR.
 FORMAT INTENSIFIED ON.
 WRITE: /56 P_TITLE.
 SKIP.
 WRITE: /1  TEXT-008, 15 P_COMPNY.
 WRITE: 41  TEXT-006, 53 P_DAY1, 55 '/', 56 P_MONTH1,
        58 '/', 59 P_YEAR1.
 WRITE: 63  TEXT-007, 80 P_DAY2, 82 '/', 83 P_MONTH2, 85 '/'.
 WRITE: 86 P_YEAR2, 112 TEXT-009.
 ULINE: /.
 WRITE: /1 TEXT-010, 12 TEXT-012, 17 TEXT-016, 38 TEXT-011, 48 TEXT-016.
 WRITE: 60 TEXT-027, 81 TEXT-020, 97 TEXT-020, 110 TEXT-011.
 SKIP.
 WRITE: /1 TEXT-011, 12 TEXT-013, 19 TEXT-014, 39 TEXT-021, 48 TEXT-017.
 WRITE: 60 TEXT-019, 81 TEXT-021, 97 TEXT-011, 110 TEXT-015.
 ULINE: /.
 MOVE '11' TO LN_CNTR.
 FORMAT INTENSIFIED OFF.
ENDFORM.
