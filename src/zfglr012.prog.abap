REPORT ZFGLR012 LINE-COUNT 65 LINE-SIZE 255 NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
*  Author: Selwyn Rodricks                                             *
*          OmniLogic Systems Group                                     *
*  Brief Description:                                                  *
*  - FI Document Summary Report with Descriptions                      *
***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ******
*  modified by Nancy Gilligan, OmniLogic 98/10      D0K906135          *
*    - standardized headers, set company code based on client          *
*----------------------------------------------------------------------*
TABLES: BKPF, BSEG, TBSL, SKAT, MAKT, T001.

DATA:   DRAMT  LIKE BSEG-WRBTR,
        CRAMT  LIKE BSEG-WRBTR,
        QTAMT  LIKE BSEG-WRBTR.

*FIELD-GROUPS: HEADER, DATA.
*INSERT BSEG-GJAHR                      "Year
*       BSEG-BUKRS                      "Company code
*       BSEG-HKONT                      "G/L account
*       BSEG-BSCHL                      "Posting key
*  INTO HEADER.

*INSERT BKPF-BUDAT                      "Posting date
*       BSEG-WRBTR                      "Amount
*       BSEG-MWSKZ                      "Tax code
*       BSEG-MENGE                      "Quantity
*       BSEG-MEINS                      "Unit
*       BSEG-BELNR                      "Doc.#
*  INTO DATA.

DATA:  DETAIL,
       SUMMARY.

DATA:  BEGIN OF ITAB OCCURS 500,
         BUKRS LIKE BSEG-BUKRS,
         GJAHR LIKE BSEG-GJAHR,
         BELNR LIKE BSEG-BELNR,
         BUDAT LIKE BKPF-BUDAT,
         HKONT LIKE BSEG-HKONT,
         KOSTL LIKE BSEG-KOSTL,
         AUFNR LIKE BSEG-AUFNR,
         BSCHL LIKE BSEG-BSCHL,
         WRBTR LIKE BSEG-WRBTR,
         MWSKZ LIKE BSEG-MWSKZ,
         MATNR LIKE BSEG-MATNR,
         MENGE LIKE BSEG-MENGE,
         MEINS LIKE BSEG-MEINS,
         SGTXT LIKE BSEG-SGTXT.
DATA:  END OF ITAB.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-003.

SELECT-OPTIONS: S_BUKRS FOR  BKPF-BUKRS,
                BELNR FOR  BKPF-BELNR.
PARAMETERS:     GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4).

PARAMETERS :    P_NEWPAG AS CHECKBOX.  "New page for new document
SELECTION-SCREEN END OF BLOCK BOX1.

* initialization                                         ngilligan 98/10
INITIALIZATION.                                         "ngilligan 98/10
IF SY-MANDT+2(1) = '1'.                                 "ngilligan 98/10
   S_BUKRS-LOW = 'UEC'.                                 "ngilligan 98/10
   S_BUKRS-SIGN = 'I'.                                  "ngilligan 98/10
   S_BUKRS-OPTION = 'EQ'.                               "ngilligan 98/10
ELSEIF  SY-MANDT+2(1) = '0'.                            "ngilligan 98/10
   S_BUKRS-LOW = 'UGL'.                                 "ngilligan 98/10
   S_BUKRS-SIGN = 'I'.                                  "ngilligan 98/10
   S_BUKRS-OPTION = 'EQ'.                               "ngilligan 98/10
ENDIF.                                                  "ngilligan 98/10
APPEND S_BUKRS.


***** Start of Main Program
START-OF-SELECTION.
DETAIL = 'X'.

* get company code text                                  ngilligan 98/10
SELECT SINGLE BUTXT FROM T001 INTO T001-BUTXT WHERE BUKRS IN S_BUKRS.

SELECT * FROM BKPF WHERE BUKRS IN S_BUKRS AND
                         BELNR IN BELNR AND
                         GJAHR =  GJAHR.
  SELECT * FROM BSEG WHERE BUKRS = BKPF-BUKRS AND
                           BELNR = BKPF-BELNR AND
                           GJAHR = BKPF-GJAHR.

    SELECT SINGLE * FROM TBSL WHERE BSCHL = BSEG-BSCHL.
    IF SY-SUBRC = 0.
      IF TBSL-SHKZG = 'H'.
        BSEG-WRBTR = - BSEG-WRBTR.
        BSEG-MENGE = - BSEG-MENGE.
      ENDIF.
    ENDIF.


    MOVE-CORRESPONDING BSEG TO ITAB.
    MOVE BKPF-BUDAT TO ITAB-BUDAT.
    APPEND ITAB.
*   EXTRACT DATA.
  ENDSELECT.
ENDSELECT.

SORT ITAB BY BUKRS GJAHR BELNR BUDAT HKONT.
LOOP AT ITAB.
  ON CHANGE OF ITAB-BELNR.
    IF P_NEWPAG = 'X'.
      NEW-PAGE.
    ELSE.
      SKIP.
      RESERVE 5 LINES.
    ENDIF.
  ENDON.

  WRITE: /1  ITAB-BUKRS,
          6  ITAB-GJAHR,
         12  ITAB-BELNR,               "Doc.#
         24  ITAB-BUDAT,               "Posting date
         36  ITAB-HKONT.               "G/L account

  SELECT SINGLE * FROM SKAT WHERE SAKNR = ITAB-HKONT AND
                                  SPRAS = SY-LANGU   AND
                                  KTOPL = 'COAT'.
  IF SY-SUBRC = 0.
    WRITE: 45 SKAT-TXT50(40).
  ENDIF.

  WRITE:  87  ITAB-KOSTL,              "Cost center
          98  ITAB-AUFNR,              "Order number
         108  ITAB-BSCHL,              "Posting key
         112  ITAB-WRBTR,              "Amount
         134  ITAB-MWSKZ,              "Tax code
         140  ITAB-MATNR+12.           "Material number

  SELECT SINGLE * FROM MAKT WHERE MATNR = ITAB-MATNR AND
                                  SPRAS = SY-LANGU.
  IF SY-SUBRC = 0.
    WRITE: 148 MAKT-MAKTX.
  ENDIF.

  WRITE: 189  ITAB-MENGE,              "Quantity
              ITAB-MEINS,              "Unit
              ITAB-SGTXT(40).          "Text

  AT END OF HKONT.
    SUM.
    WRITE: /112 '---------------'.
    WRITE:  189 '----------------'.
    WRITE: /112 ITAB-WRBTR,            "Amount
            189 ITAB-MENGE.            "Quantity
    WRITE: /112 '==============='.
    WRITE:  189 '================'.
    SKIP.
  ENDAT.

  QTAMT = QTAMT + ITAB-MENGE.

ENDLOOP.

WRITE: /189 '----------------'.
WRITE: /150 'Total Quantity',
        189 QTAMT.                 "Quantity
WRITE: /189 '================'.
SKIP.


*SORT.
*SUMMARY = 'X'.
*CLEAR DETAIL.
*
*LOOP.
*  AT NEW BSEG-BUKRS.
*    NEW-PAGE.
*    WRITE: / 'SUMMARY:', BSEG-BUKRS.
*  ENDAT.
*
*  AT END OF BSEG-BSCHL.
*    WRITE: /38  BSEG-HKONT,            "G/L account
*            46  BSEG-BSCHL,            "Posting key
*            50  SUM(BSEG-WRBTR),       "Amount
*            68  SUM(BSEG-MENGE).       "Quantity
*  ENDAT.
*
*  AT END OF BSEG-HKONT.
*    write: /50  sum(itab-wrbtr),       "Amount
*  endat.
*
*ENDLOOP.


* top of page
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         80 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
        220 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-001 UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10


* FORMAT INTENSIFIED OFF.

  IF DETAIL = 'X'.
    WRITE: /1  TEXT-101,
            6  TEXT-102,
           13  TEXT-103,
           24  TEXT-104,
           36  TEXT-105,
           45  TEXT-106,
           85  TEXT-107,
           98  TEXT-108,
          108  TEXT-109,
          121  TEXT-110,
          131  TEXT-111,
          139  TEXT-112,
          148  TEXT-113,
          197  TEXT-114,
          207  TEXT-115,
          211  TEXT-116.
  ENDIF.

  IF SUMMARY = 'X'.
    WRITE: /38 TEXT-105,
            46 TEXT-109,
            59 TEXT-110,
            76 TEXT-114.
  ENDIF.


  ULINE.
