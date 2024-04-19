REPORT ZFGLR008 LINE-Size 185.

******************************************************************
*       Owner: Centra/Union                                      *
*  Programmer: Marv Radsma                                       *
*        Date: Sept 18, 1996                                     *
*  Request ID: DRCO0079                                          *
*                                                                *
* This program produces a report which will reconcile the COPA   *
* and FI revenue amounts.  It is required to ensure that the     *
* detailed level of data stored in CO-PA matches the summary     *
* level of data in FI.                                           *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YY/MM/DD - USERID - MOD# - DESCRIPTION                         *
* -------------------------------------------------------------- *
* 97/01/21 - mr9722 -      - changed report so that FI portion   *
*                            would also display GL number        *
* 97/07/23 - ctgkk  -      - changed report to use invoice dt    *
*                            for CO and Doc dt for FI            *
* 98/01/22 - MRadsma-      - include period for CO selection     *
* 98/02/24 - MRadsma-      - include '2' with CO selection on    *
*                            field CE11100-VRGAR (record type)   *
*          -        -      -                                     *
******************************************************************

TABLES: CE11100,   "Union/Centra Operating Line Items
        BKPF,      "Accounting Document Header
        BSEG,      "Accounting Document Segment
        ZF004,     "G/L Revenue Accounts
        ZF005.     "Link FI document types to COPA system of origin

DATA:   LN_CNTR    TYPE I,                        "Line Counter
        BEGIN OF SUM_TAB     OCCURS 5000,         "Accum of data
          ARTNR    LIKE CE11100-ARTNR,            "Material Number
          HKONT    LIKE BSEG-HKONT,               "GL Account Number
          VVBRV    LIKE CE11100-VVBRV,            "Amount
          VVBVL    TYPE P DECIMALS 2,             "Quantity
          WRBTR    LIKE BSEG-WRBTR,               "Amount
          MENGE    TYPE P DECIMALS 2,             "Quantity
          AMT_DIFF LIKE BSEG-WRBTR,               "Amount Difference
          QTY_DIFF TYPE P DECIMALS 2,             "Quantity Difference
        END OF SUM_TAB.

* This first block produces a border like frame around the following: *

SELECTION-SCREEN BEGIN OF BLOCK
    INTRO WITH FRAME.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 26(30) TEXT-001 MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN ULINE  26(30).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* The following second block produces a frame within the first block: *

SELECTION-SCREEN BEGIN OF BLOCK COMPANY
                 WITH FRAME TITLE TEXT-002.

SELECT-OPTIONS:
  S_COMPNY FOR BKPF-BUKRS  default 'UGL'            "COMPANY CODE
           OBLIGATORY NO INTERVALS MODIF ID ABC.

* Second block ends after this statement. *

SELECTION-SCREEN END OF BLOCK COMPANY.

SELECTION-SCREEN SKIP.

* The following third block produces a frame within the first block: *

SELECTION-SCREEN BEGIN OF BLOCK ORIGIN
                 WITH FRAME TITLE TEXT-005.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 17(13) TEXT-009.
PARAMETERS:
  P_ORIGIN      LIKE CE11100-WWSID               "COMPANY OF ORIGIN
                default 'G005'
                OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* Third block ends after this statement. *

SELECTION-SCREEN END OF BLOCK ORIGIN.

SELECTION-SCREEN SKIP.

* The following fourth block produces a frame within the first block. *

SELECTION-SCREEN BEGIN OF BLOCK POSTING
                 WITH FRAME TITLE TEXT-006.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(20) TEXT-007.
SELECTION-SCREEN: COMMENT 22(8) TEXT-003.
PARAMETERS:
  P_CO_DT1    LIKE CE11100-BUDAT              "COPA From Date
              default sy-datum
              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN: COMMENT 42(13) TEXT-004.
PARAMETERS:
  P_CO_DT2    LIKE CE11100-BUDAT              "COPA To Date
              default sy-datum
              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(20) TEXT-008.
SELECTION-SCREEN: COMMENT 22(8) TEXT-003.
PARAMETERS:
  P_FI_DT1    LIKE BKPF-BUDAT                 "FI From Date
              default sy-datum
              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN: COMMENT 42(13) TEXT-004.
PARAMETERS:
  P_FI_DT2    LIKE BKPF-BUDAT                 "FI To Date
              default sy-datum
              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(29) TEXT-017.
PARAMETERS:
  P_perio     LIKE ce11100-PERIO              "CO Posting Period
              default sy-datum
              OBLIGATORY MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

* This statement ends the third block. *

SELECTION-SCREEN END OF BLOCK POSTING.

SELECTION-SCREEN SKIP.

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

  SELECT * FROM ZF005
  WHERE  WWSID = P_ORIGIN.

    SELECT * FROM BKPF
    WHERE  BUKRS IN S_COMPNY
** start of change CTGKK     July 23, 1997
*   AND    BUDAT BETWEEN P_FI_DT1 AND P_FI_DT2
    AND    BLDAT BETWEEN P_FI_DT1 AND P_FI_DT2
** end   of change CTGKK     July 23, 1997
    AND    GJAHR BETWEEN P_FI_DT1(4) AND P_FI_DT2(4)
    AND    BLART = ZF005-BLART.

      SELECT * FROM ZF004.

        SELECT * FROM BSEG
        WHERE  BUKRS = BKPF-BUKRS
        AND    BELNR = BKPF-BELNR
        AND    GJAHR = BKPF-GJAHR
        AND    HKONT = ZF004-SAKNR.

          MOVE BSEG-MATNR TO SUM_TAB-ARTNR.
          MOVE BSEG-HKONT TO SUM_TAB-HKONT.
          MOVE BSEG-WRBTR TO SUM_TAB-WRBTR.
          MOVE BSEG-MENGE TO SUM_TAB-MENGE.
          IF BSEG-BSCHL = '40'.
            MULTIPLY SUM_TAB-WRBTR BY -1.
            MULTIPLY SUM_TAB-MENGE BY -1.
          ENDIF.
          APPEND SUM_TAB.
          CLEAR SUM_TAB.

        ENDSELECT.
      ENDSELECT.
    ENDSELECT.
  ENDSELECT.

  SELECT * FROM CE11100
  WHERE  BUKRS IN S_COMPNY
** start of change CTGKK     July 23, 1997
* AND    BUDAT BETWEEN P_CO_DT1 AND P_CO_DT2
  AND    FADAT BETWEEN P_CO_DT1 AND P_CO_DT2
** start of change CTGKK     July 23, 1997
  AND    WWSID = ZF005-WWSID
  AND    perio = p_perio
  AND  ( VRGAR = '1'
  OR     VRGAR = '2' ).

    MOVE CE11100-ARTNR TO SUM_TAB-ARTNR.
    MOVE CE11100-VVBRV TO SUM_TAB-VVBRV.
    MOVE CE11100-VVBVL TO SUM_TAB-VVBVL.
    ADD  CE11100-VVURV TO SUM_TAB-VVBRV.
    ADD  CE11100-VVORD TO SUM_TAB-VVBRV.
    ADD  CE11100-VVORI TO SUM_TAB-VVBRV.
    ADD  CE11100-VVUVL TO SUM_TAB-VVBVL.
    APPEND SUM_TAB.
    CLEAR SUM_TAB.

  ENDSELECT.

* Output the report headings. *
  PERFORM WRT_HDG.

* Sort the table just extracted by material number. *
  SORT SUM_TAB BY ARTNR HKONT.

* Process the table, calculate the difference and sum the amounts. *
  LOOP AT SUM_TAB.
    AT END OF HKONT.
      IF SUM_TAB-HKONT <> SPACE.
        SUM.
        PERFORM WRT_DET.
      ENDIF.
    ENDAT.
    AT END OF ARTNR.
      SUM.
      SUM_TAB-AMT_DIFF = SUM_TAB-VVBRV - SUM_TAB-WRBTR.
      SUM_TAB-QTY_DIFF = SUM_TAB-VVBVL - SUM_TAB-MENGE.
      PERFORM WRT_TOT.
    ENDAT.
    AT LAST.
      SUM.
      SUM_TAB-AMT_DIFF = SUM_TAB-VVBRV - SUM_TAB-WRBTR.
      SUM_TAB-QTY_DIFF = SUM_TAB-VVBVL - SUM_TAB-MENGE.
      PERFORM WRT_TOT.
    ENDAT.
  ENDLOOP.

END-OF-SELECTION.

*************************BEGINNING OF FORMS*****************************

* This routine writes the specifics of the specified G/L Account(s). *

FORM WRT_DET.
  IF LN_CNTR >= 59.                     "Line counter for page break
    PERFORM WRT_HDG.
  ENDIF.

  WRITE: /1  SUM_TAB-ARTNR, 20 SUM_TAB-HKONT,
          35 SUM_TAB-MENGE, 60 SUM_TAB-WRBTR.
ENDFORM.

* This routine writes the specifics of the specified Material Numbers*

FORM WRT_TOT.
  IF LN_CNTR >= 59.                     "Line counter for page break
    PERFORM WRT_HDG.
  ENDIF.

  WRITE: /1  SUM_TAB-ARTNR, 35 SUM_TAB-MENGE,     60 SUM_TAB-WRBTR,
                            85 SUM_TAB-VVBVL,    110 SUM_TAB-VVBRV,
                           135 SUM_TAB-QTY_DIFF, 160 SUM_TAB-AMT_DIFF.
ENDFORM.

* This routine writes the report headings. *

FORM WRT_HDG.
  NEW-PAGE WITH-TITLE.
  MOVE '0' TO LN_CNTR.
  FORMAT INTENSIFIED ON.
  WRITE: /56 TEXT-001.
  SKIP.
  WRITE: /1  TEXT-002, 15 S_COMPNY+3(3).
  WRITE: /1  TEXT-006, 15 TEXT-007, 35 'FROM', 40 P_CO_DT1,
                                    50 '  TO', 55 P_CO_DT2.
  WRITE: /15              TEXT-008, 35 'FROM', 40 P_FI_DT1,
                                    50 '  TO', 55 P_FI_DT2.
  ULINE: /.
  WRITE: /1  text-010, 20 TEXT-016, 54 TEXT-008,
                      105 TEXT-007,161 TEXT-014.
  SKIP.
  WRITE: /1  text-011, 20 TEXT-011, 42 TEXT-012,  66 TEXT-013,
                       92 TEXT-012, 121 TEXT-013,
                      142 TEXT-012, 166 TEXT-013.
  ULINE: /.
  MOVE '11' TO LN_CNTR.
  FORMAT INTENSIFIED OFF.
ENDFORM.
