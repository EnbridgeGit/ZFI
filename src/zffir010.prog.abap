*&---------------------------------------------------------------------*
*& Report  ZFFIR010                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        April, 2007.                                           *
*  Issue Log:   TR383                                                  *
*  Description:                                                        *
*     - The purpose of this program is to produce the following reports*
*       for internal auditor Sherry Dewet:                             *
*       1- Company code activity by document type                      *
*       2- Activity by document type                                   *
*     Note: This program takes debit entries only from the BSEG table  *
*           and amount in local currency is used for reporting. It's   *
*           as per advice of functionl consultant Anil Mehta.          *
*&---------------------------------------------------------------------*
* CHANGES:                                                             *
* 2009/11/02 Mohammad TR582  IFRS - Include BKPF-LDGRP in selection    *
*                            screen, DB data selection and report.     *
*&---------------------------------------------------------------------*

REPORT ZFFIR010 NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 58.

TABLES: BKPF,    "Accounting Document Header
        T001,    "Accounting Document Segment
        BSEG.    "Company Codes

DATA: BEGIN OF ITAB1 OCCURS 0,
        LDGRP         LIKE BKPF-LDGRP,            "Ledger Group  TR582
        BUKRS         LIKE BKPF-BUKRS,            "Company
        BLART         LIKE BKPF-BLART,            "Doc Type
        BELNR         LIKE BKPF-BELNR,            "Doc number
        DMBTR         LIKE BSEG-DMBTR,            "Amt in local currency
     END OF ITAB1.

DATA: BEGIN OF ITAB2 OCCURS 0,
        LDGRP         LIKE BKPF-LDGRP,            "Ledger Group  TR582
        BLART         LIKE BKPF-BLART,            "Doc Type
        KOUNT         TYPE I,                     "Total document
        DMBTR         LIKE BSEG-DMBTR,            "Amt in local currency
     END OF ITAB2.

DATA: W_TEXT35(35)  TYPE C,
      W_TEXT101(12) TYPE C,
      W_TITLE(33)   TYPE C,
      COUNT_BY_DOC_TYPE TYPE I,
      COUNT_BY_COMPANY  TYPE I,
      TOTAL_DOC_COUNT   TYPE I,
      W_DMBTR       LIKE BSEG-DMBTR,
      W_BLART       LIKE BKPF-BLART,
      W_LDGRP       LIKE BKPF-LDGRP,             "TR582
      PREV_BLART    LIKE BKPF-BLART.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
        S_BUDAT  FOR BKPF-BUDAT,                    "Posting Date
        S_LDGRP  FOR BKPF-LDGRP.                    "TR582
SELECTION-SCREEN END OF BLOCK BOX1.

START-OF-SELECTION.

*SELECT BUKRS BELNR GJAHR BLART                                   "TR582
*  INTO (BKPF-BUKRS, BKPF-BELNR, BKPF-GJAHR, BKPF-BLART)          "TR582
SELECT BUKRS BELNR GJAHR BLART LDGRP                              "TR582
  INTO (BKPF-BUKRS, BKPF-BELNR, BKPF-GJAHR, BKPF-BLART, BKPF-LDGRP) "582
  FROM BKPF
 WHERE BUDAT IN S_BUDAT
   AND LDGRP IN S_LDGRP.                                       "TR582

 SELECT DMBTR SHKZG
   INTO (BSEG-DMBTR, BSEG-SHKZG)
   FROM BSEG
  WHERE BUKRS = BKPF-BUKRS
    AND BELNR = BKPF-BELNR
    AND GJAHR = BKPF-GJAHR
    AND SHKZG = 'S'.         "Debit entries

*    IF BSEG-SHKZG <> 'H'.
*       BSEG-DMBTR = BSEG-DMBTR * -1.
*    ENDIF.
       ADD BSEG-DMBTR TO W_DMBTR.

 ENDSELECT.
    MOVE: BKPF-BUKRS TO ITAB1-BUKRS,
          BKPF-BLART TO ITAB1-BLART,
          BKPF-BELNR TO ITAB1-BELNR,
          W_DMBTR    TO ITAB1-DMBTR,
          BKPF-LDGRP TO ITAB1-LDGRP.                          "TR582
    APPEND ITAB1.
    CLEAR: ITAB1, W_DMBTR.
ENDSELECT.


*SELECT BUKRS BLART COUNT( * ) INTO (BKPF-BUKRS, BKPF-BLART, W_COUNT)
*  FROM BKPF
* WHERE BUDAT IN S_BUDAT
* GROUP BY BUKRS BLART
* ORDER BY BUKRS BLART.
*
*    MOVE: BKPF-BUKRS  TO  ITAB1-BUKRS,
*          BKPF-BLART  TO  ITAB1-BLART,
*          W_COUNT     TO  ITAB1-KOUNT.
*
*    APPEND ITAB1.
*
*ENDSELECT.

MOVE TEXT-101 TO W_TEXT101.
MOVE TEXT-TL1 TO W_TITLE.

SORT ITAB1 BY BUKRS BLART BELNR.
LOOP AT ITAB1.

MOVE ITAB1-LDGRP TO W_LDGRP.                      "TR582
AT NEW LDGRP.                                     "TR582
   NEW-PAGE.
ENDAT.

AT NEW BUKRS.
   SELECT SINGLE BUTXT INTO T001-BUTXT
     FROM T001
    WHERE BUKRS = ITAB1-BUKRS.
    CLEAR W_TEXT35.
    CONCATENATE ITAB1-BUKRS '-' T001-BUTXT INTO W_TEXT35.
    WRITE: /2 W_TEXT35.
ENDAT.

AT NEW BELNR.
    ADD 1 TO COUNT_BY_DOC_TYPE.
ENDAT.

    MOVE ITAB1-BLART TO W_BLART.

AT END OF BLART.
   SUM.
   WRITE: /36 W_BLART, 42 COUNT_BY_DOC_TYPE, 65 ITAB1-DMBTR.
   ADD COUNT_BY_DOC_TYPE TO COUNT_BY_COMPANY.
   MOVE: W_BLART            TO  ITAB2-BLART,
         COUNT_BY_DOC_TYPE  TO  ITAB2-KOUNT,
         ITAB1-DMBTR        TO  ITAB2-DMBTR,
         ITAB1-LDGRP        TO  ITAB2-LDGRP.                  "TR582
   COLLECT ITAB2.
   CLEAR: ITAB2, COUNT_BY_DOC_TYPE.
ENDAT.

AT END OF BUKRS.
   SUM.
   ULINE.
   WRITE: /2 TEXT-108, 42 COUNT_BY_COMPANY, 65 ITAB1-DMBTR.
   ADD COUNT_BY_COMPANY TO TOTAL_DOC_COUNT.
   CLEAR COUNT_BY_COMPANY.
   ULINE.
ENDAT.

AT LAST.
   SUM.
   WRITE: /2 TEXT-107, 42 TOTAL_DOC_COUNT, 65 ITAB1-DMBTR.
   ULINE.
ENDAT.
ENDLOOP.

SORT ITAB2 BY BLART.
CLEAR: W_TEXT101, COUNT_BY_DOC_TYPE, TOTAL_DOC_COUNT.
MOVE TEXT-TL2 TO W_TITLE.
NEW-PAGE.

LOOP AT ITAB2.
MOVE ITAB2-LDGRP TO W_LDGRP.                      "TR582
AT NEW LDGRP.                                     "TR582
   NEW-PAGE.                                      "TR582
ENDAT.                                            "TR582

*  MOVE ITAB2-BLART TO PREV_BLART.
*
*  AT END OF BLART.
*     SUM.
     WRITE: /36 ITAB2-BLART, 42 ITAB2-KOUNT, 65 ITAB2-DMBTR.
*     ADD COUNT_BY_DOC_TYPE TO TOTAL_DOC_COUNT.
*     CLEAR COUNT_BY_DOC_TYPE.
*  ENDAT.
AT LAST.
   SUM.
   ULINE.
   WRITE: /2 TEXT-107, 42 ITAB2-KOUNT, 65 ITAB2-DMBTR.
   ULINE.
ENDAT.
ENDLOOP.


*LOOP AT ITAB1.
*     CLEAR ITAB1-BUKRS.
*     MODIFY ITAB1.
*ENDLOOP.

*SORT ITAB1 BY BLART BELNR BUKRS.
*CLEAR: W_TEXT101, COUNT_BY_DOC_TYPE, TOTAL_DOC_COUNT.
*MOVE TEXT-TL2 TO W_TITLE.
*NEW-PAGE.
*
*LOOP AT ITAB1.
*  AT NEW BELNR.
*     ADD 1 TO COUNT_BY_DOC_TYPE.
*  ENDAT.
*
*  MOVE ITAB1-BLART TO PREV_BLART.
*
*  AT END OF BLART.
*     SUM.
*     WRITE: /36 ITAB1-BLART, 42 COUNT_BY_DOC_TYPE, 65 ITAB1-DMBTR.
*     ADD COUNT_BY_DOC_TYPE TO TOTAL_DOC_COUNT.
*     CLEAR COUNT_BY_DOC_TYPE.
*  ENDAT.
*AT LAST.
*   SUM.
*   ULINE.
*   WRITE: /2 TEXT-107, 42 TOTAL_DOC_COUNT, 65 ITAB1-DMBTR.
*   ULINE.
*ENDAT.
*ENDLOOP.

*------------------- TOP-OF-PAGE ---------------------------------------
TOP-OF-PAGE.
   WRITE: /1 TEXT-RPT, SY-REPID, 18 W_TITLE,
         53 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID+0(3), SY-MANDT,  "Title
            20 TEXT-FRM, S_BUDAT-LOW.
IF S_BUDAT-HIGH > 1.
   WRITE:   TEXT-103, S_BUDAT-HIGH.
ENDIF.
   WRITE:   TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
   WRITE: /1 TEXT-LGR, W_LDGRP.                            "TR582
   ULINE.
   WRITE: 2 W_TEXT101, 35 TEXT-102, 47 TEXT-104, 72 TEXT-105.
   ULINE.
