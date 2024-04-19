REPORT ZZTESTYV NO STANDARD PAGE HEADING LINE-SIZE 90
                                         LINE-COUNT 58.

***********************************************************************
*       Owner: Centra/Union                                           *
*  Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)        *
*        Date: November 19th, 1996                                    *
*  Request ID: DRFI0022 - Accounts With No Input Tax                  *
*                                                                     *
* This program generates a report for accounts with no input tax.     *
* It also allows to report on other G/L Accounts.                     *
********* *************** ******************* *********** *************
*  modified by Nancy Gilligan, OmniLogic  98/10  D30K906123           *
*   - standardized headers, get company code text based on client,    *
*                                                                     *
*                                                                     *
*                                                                     *
*                                                                     *
***********************************************************************
TABLES: BSAS,"Accounting: Secondary index for G/L accounts cleared items
        BSEG,"Accountind document segment
        SKAT,"G/L accounts master records - Chart of Accounts
        SKB1,
        T003A,"Document Types For Posting With Clearing
        T001.  "Company Codes
DATA:
      POST1      TYPE D,                       "First Date Range
      POST2      LIKE POST1,                   "Second Date Range
      LN_CNTR    TYPE I.                       "Line Counter

DATA:
     BEGIN OF WTAB OCCURS 500,
       BLART LIKE BSAS-BLART,                  "Document Type
       BELNR LIKE BSEG-BELNR,                  "Document Reference No.
       MWSKZ LIKE BSEG-MWSKZ,                  "Tax Code
       HKONT LIKE BSEG-HKONT,                  "G/L Account
       WRBTR LIKE BSEG-WRBTR,                  "Amount
       BUKRS LIKE BSEG-BUKRS,                  "Company Code
       AUGDT LIKE BSAS-AUGDT,                  "Clearing Date
       GJAHR LIKE BSEG-GJAHR,                  "Fiscal Year
      END OF WTAB,

     BEGIN OF TEMPTAB OCCURS 500,
       BLART LIKE BSAS-BLART,                  "Document Type
       MWSKZ LIKE BSEG-MWSKZ,                  "Tax Code
       HKONT LIKE BSEG-HKONT,                  "G/L Account
       WRBTR LIKE BSEG-WRBTR,                  "Amount
       BUKRS LIKE BSEG-BUKRS,                  "Company Code
       AUGDT LIKE BSAS-AUGDT,                  "Clearing Date
       GJAHR LIKE BSEG-GJAHR,                  "Fiscal Year
       BELNR LIKE BSEG-BELNR,                  "Document Reference No.
      END OF TEMPTAB.

* This first block produces a border like frame around the following: *
SELECTION-SCREEN BEGIN OF BLOCK
    INTRO WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 26(30) TEXT-001 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN ULINE  26(30).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(14) TEXT-002 MODIF ID ABC.
PARAMETERS:
  P_COMPNY LIKE T001A-BUKRS       "Company Code
           OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.
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

SELECTION-SCREEN SKIP.

* This fourth block produces a frame around the first block. *
SELECTION-SCREEN BEGIN OF BLOCK
     ACCNT WITH FRAME TITLE TEXT-003.


SELECT-OPTIONS:  S_GLACCT FOR SKB1-SAKNR NO INTERVALS
                          DEFAULT '0000256950'   MODIF ID ABC,
                 S_DOCTYP FOR T003A-BLART NO INTERVALS
                          MODIF ID ABC.          "Document Types

* This statement ends the fourth block. *
SELECTION-SCREEN END OF BLOCK ACCNT.



* This statement ends the first block. *
SELECTION-SCREEN END OF BLOCK INTRO.

* The following will highlight the screen's output for certain texts. *
AT SELECTION-SCREEN OUTPUT.
 LOOP AT SCREEN.
  CHECK SCREEN-GROUP1 = 'ABC'.
  SCREEN-INTENSIFIED = '1'.
  MODIFY SCREEN.
 ENDLOOP.

* initialization                                         ngilligan 98/10
INITIALIZATION.                                         "ngilligan 98/10
IF SY-MANDT+2(1) = '1'.                                 "ngilligan 98/10
  P_COMPNY = 'UEC'.                                     "ngilligan 98/10
ELSEIF  SY-MANDT+2(1) = '0'.                            "ngilligan 98/10
  P_COMPNY = 'UGL'.                                     "ngilligan 98/10
ENDIF.                                                  "ngilligan 98/10

*TOP OF PAGE
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         25 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
         63 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-001 UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10


*********************BEGINNING OF MAIN PROGRAM *************************

START-OF-SELECTION.

* get company code text
   SELECT SINGLE BUTXT FROM T001 INTO T001-BUTXT WHERE BUKRS = P_COMPNY.

 PERFORM INITIALIZE.
 PERFORM GET_PAY.
 PERFORM GET_GST.
 PERFORM PROC_INFO.
END-OF-SELECTION.

FORM INITIALIZE.
 REFRESH: WTAB, TEMPTAB.
 CLEAR:   WTAB, TEMPTAB.
 MOVE '60' TO LN_CNTR.


 CONCATENATE P_YEAR1 P_DAY1 P_MONTH1 INTO POST1. "Combines date to post1
 CONCATENATE P_YEAR2 P_DAY2 P_MONTH2 INTO POST2. "Combines date to post2
ENDFORM.


FORM GET_PAY.
 SELECT * FROM BSAS WHERE BLART IN S_DOCTYP AND BUKRS = P_COMPNY
          AND AUGDT BETWEEN POST1 AND POST2
          ORDER BY BELNR BLART AUGDT.
   SELECT * FROM BSEG WHERE BELNR = BSAS-BELNR
            AND BUKRS = BSAS-BUKRS AND GJAHR = BSAS-GJAHR.
            PERFORM POP_WTAB.
   ENDSELECT.
 ENDSELECT.
ENDFORM.


FORM GET_GST.
 SORT: WTAB BY BELNR.
 LOOP AT WTAB.
  IF WTAB-HKONT IN S_GLACCT.
     MOVE: WTAB-BELNR TO TEMPTAB-BELNR,
           WTAB-HKONT TO TEMPTAB-HKONT.
     APPEND TEMPTAB.
  ENDIF.
 ENDLOOP.
ENDFORM.

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


FORM PROC_INFO.
 LOOP AT WTAB.
   READ TABLE TEMPTAB WITH KEY BELNR = WTAB-BELNR.
   IF WTAB-BELNR = TEMPTAB-BELNR.
     CONTINUE.
   ELSE.
   AT NEW BELNR.
    IF LN_CNTR >= 59.
       PERFORM WRT_HDG.
    ENDIF.
      WRITE: /1 WTAB-BELNR, 12 WTAB-BLART.
      LN_CNTR = LN_CNTR + 1.
   ENDAT.
     SELECT SINGLE * FROM SKAT WHERE SAKNR = WTAB-HKONT
            AND KTOPL = 'COAT' AND SPRAS = 'E'.
     IF LN_CNTR >= 59.
        PERFORM WRT_HDG.
     ENDIF.
    WRITE: /22 WTAB-AUGDT, 34 WTAB-MWSKZ, 40 WTAB-HKONT,
              53 SKAT-TXT20, 75 WTAB-WRBTR.
      LN_CNTR = LN_CNTR + 1.
   ENDIF.
 ENDLOOP.
ENDFORM.


* This routine writes the sub headings. *
FORM WRT_HDG.
 NEW-PAGE.
 CLEAR LN_CNTR.
 FORMAT INTENSIFIED ON.
 SKIP.
 WRITE: /1  TEXT-008, 15 P_COMPNY.
 WRITE: 21  TEXT-006, 33 P_DAY1, 35 '/', 36 P_MONTH1,
        38 '/', 39 P_YEAR1.
 WRITE: 46  TEXT-018, 51 P_DAY2, 53 '/', 54 P_MONTH2, 56 '/'.
 WRITE: 57 P_YEAR2, 71 TEXT-009.
 ULINE: /.
 WRITE: /1 TEXT-016, 12 TEXT-016, 22 TEXT-027, 34 TEXT-012, 40 TEXT-010.
 WRITE: 53 TEXT-011, 83 TEXT-011.
 SKIP.
 WRITE: /1 TEXT-017, 12 TEXT-014, 22 TEXT-019, 34 TEXT-013, 40 TEXT-011.
 WRITE: 53 TEXT-015, 84 TEXT-021.
 ULINE: /.
 MOVE '11' TO LN_CNTR.
 FORMAT INTENSIFIED OFF.
ENDFORM.
