REPORT ZFAPI013 NO STANDARD PAGE HEADING LINE-SIZE 132 LINE-COUNT 65
                MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZFAPI013 - FI/AP: CGO/UGL Meger Vendor Scrub Report
*    Programmer  :  Ric Aarssen
*    Date        :  March 23, 1998
*
*    This ABAP will create a report that will be downloaded to excel.
*    This will assist A/P in identifying inactive and duplicate vendors.
*
*    Report 3: Vendors that exist in CGO, not UGL - for only EMPL group
*
*    This ABAP will use an internal table to gather the data
*    that falls within the vendors selected.  It will then be
*    sorted and looped through to create a report.
*
*    The prgrm will be executed in background and spooled, where it can
*    can be saved as a text file to be read into excel.

************************************************************************
*  98/04/08  raarssen change to accumulate the debits/credits for the
*            fiscal year 1998
************************************************************************

*****************************  TABLES   ********************************

TABLES: LFA1,                "Vendor master (general section)
*            name1 - Vendor name
*            ktokk - Vendor group
        LFB1,                "Vendor master (company code)
*            lifnr - vendor number
*            bukrs - company code (CGO or UGL)
*            sperr - blocked
*            loevm - marked for deletion
        LFC1,                "Vendor master (transaction figures)
*            gjahr - fiscal year                             98/04/08 ra
*            umsav - balance forward from previous year               ra
*            umXXs - 12 months debits     "XX = 01 to 12              ra
*            umXXh - 12 months credits    "unsigned                   ra
        BSIK,                "Documents for vendors
*            belnr - last document number
*            bldat - last documemt date
        BSAK.                "Cleared documents for vendor

*---------  Internal table for Vendor records selected  ----------------
*  the first 4 fields are listed in the order required for the sort

DATA: BEGIN OF TABLE_1 OCCURS 0,
        NAME1            LIKE LFA1-NAME1,     "Vendor name        (sort)
        LIFNR            LIKE LFB1-LIFNR,     "Vendor number
        OSBAL            LIKE LFC1-UMSAV,     "outstanding balance
        BELNR            LIKE BSIK-BELNR,     "last document number
        BLDAT            LIKE BSIK-BLDAT,     "last document date
        SPERR            LIKE LFB1-SPERR,     "blocked
        LOEVM            LIKE LFB1-LOEVM,     "marked for deletion
      END OF TABLE_1.

**************************  DATA ELEMENTS  *****************************

*---------------------- Work Area --------------------------------------
DATA: W_LIFNR         LIKE LFA1-LIFNR.          "Vendor Number

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_LIFNR FOR LFB1-LIFNR.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN COMMENT 1(80) TEXT-901.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-902.

***************************  MAIN ROUTINE  *****************************
*top-of-page is NOT being used. The heading is only to appear once on
*            the report for ease when the report gets dumped to excel
*------------------------ print report header --------------------------
FORMAT COLOR 4.
WRITE: /1 TEXT-RPT, SY-REPID COLOR 7 INTENSIFIED ON,
       45 TEXT-HDG,                                            "Title
      105 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT,
         SY-MANDT UNDER SY-REPID INTENSIFIED ON,
         TEXT-PGE UNDER TEXT-DTE,
         SY-PAGNO UNDER SY-DATUM INTENSIFIED ON.
SKIP.
IF S_LIFNR-LOW = 0 AND S_LIFNR-HIGH = 0.
    WRITE: /1 TEXT-022.
  ELSE.
    WRITE: /1 TEXT-020, 14 S_LIFNR-LOW, 25 TEXT-021, 29 S_LIFNR-HIGH.
ENDIF.
ULINE.

SKIP.
FORMAT COLOR 1.
WRITE: /1 TEXT-002, 15 TEXT-002, 51 TEXT-005, 70 TEXT-007, 85 TEXT-007,
       98 TEXT-009, 108 TEXT-010.
WRITE: /1 TEXT-003, 15 TEXT-004, 51 TEXT-006, 70 TEXT-012, 85 TEXT-008,
                    108 TEXT-011.
ULINE.
WRITE: /.
FORMAT RESET.

START-OF-SELECTION.
*---------------------  read vendor tables  ----------------------------
CLEAR: TABLE_1.
*---  read vendor master (Company Code)  for CGO Vendors  --------------
SELECT * FROM LFB1
   WHERE LIFNR IN S_LIFNR
   AND   BUKRS = 'CGO'.
   IF SY-SUBRC = 0.
      MOVE LFB1-LIFNR TO TABLE_1-LIFNR.             "Vendor number
      MOVE LFB1-SPERR TO TABLE_1-SPERR.             "blocked status
      MOVE LFB1-LOEVM TO TABLE_1-LOEVM.             "marked for deletion

      MOVE LFB1-LIFNR TO W_LIFNR.
*---  read vendor master (Company Code)  does it exists in UGL  --------
      SELECT SINGLE * FROM LFB1
         WHERE LIFNR =  W_LIFNR
         AND   BUKRS = 'UGL'.
         IF SY-SUBRC <> 0.
*        endif.

*---  read vendor master for name and vendor group  --------------------
            SELECT SINGLE * FROM LFA1
               WHERE LIFNR =   W_LIFNR
               AND   KTOKK =  'EMPL'
               AND   SPRAS =   SY-LANGU.
               IF SY-SUBRC = 0.
                  MOVE LFA1-NAME1 TO TABLE_1-NAME1.       "Vendor name
*              endif.

*---  read vendor master (transaction figures)  ------------------------
                  SELECT SINGLE * FROM LFC1
                  WHERE LIFNR = W_LIFNR
                  AND   BUKRS = 'CGO'
                  AND   GJAHR = 1998.
                  IF SY-SUBRC = 0.
*---  sum bal. forward and debits and credits for fiscal year  ---0408ra
                     TABLE_1-OSBAL = LFC1-UMSAV
                                   + LFC1-UM01S - LFC1-UM01H
                                   + LFC1-UM02S - LFC1-UM02H
                                   + LFC1-UM03S - LFC1-UM03H
                                   + LFC1-UM04S - LFC1-UM04H
                                   + LFC1-UM05S - LFC1-UM05H
                                   + LFC1-UM06S - LFC1-UM06H
                                   + LFC1-UM07S - LFC1-UM07H
                                   + LFC1-UM08S - LFC1-UM08H
                                   + LFC1-UM09S - LFC1-UM09H
                                   + LFC1-UM10S - LFC1-UM10H
                                   + LFC1-UM11S - LFC1-UM11H
                                   + LFC1-UM12S - LFC1-UM12H.
                  ENDIF.

*---  read documents for vendors  --------------------------------------
                  SELECT MAX( BELNR ) FROM BSIK
                     INTO TABLE_1-BELNR                "last doc number
                     WHERE BUKRS = 'CGO'
                     AND   LIFNR =  W_LIFNR.
                  IF SY-SUBRC = 0.
                     SELECT SINGLE BLDAT FROM BSIK
                        INTO TABLE_1-BLDAT             "last doc date
                        WHERE BUKRS = 'CGO'
                        AND   LIFNR =  W_LIFNR
                        AND   BELNR =  TABLE_1-BELNR.
*               if sy-subrc = 0.
*               move bsik-bldat to table_1-bldat.
*               endif.
                  ELSE.

*---  read cleared documents for vendors  ------------------------------
                     SELECT MAX( BELNR ) FROM BSAK
                        INTO TABLE_1-BELNR             "last doc number
                        WHERE BUKRS = 'CGO'
                        AND   LIFNR =  W_LIFNR.
                        IF SY-SUBRC = 0.
                           SELECT SINGLE BLDAT FROM BSAK
                              INTO TABLE_1-BLDAT       "last doc date
                              WHERE BUKRS = 'CGO'
                              AND   LIFNR =  W_LIFNR
                              AND   BELNR =  TABLE_1-BELNR.
                        ENDIF.
                  ENDIF.
*--------------------  load internal tables  ---------------------------
       APPEND TABLE_1.
               ENDIF.
         ENDIF.
   ENDIF.
   CLEAR TABLE_1.
ENDSELECT.

*-------  create report of vendors in CGO only excluding group EMPL ----

*- sort table by vendor name -------------------------------------------
    SORT TABLE_1 ASCENDING BY NAME1.
    LOOP AT TABLE_1.
*-------------------- write report from table --------------------------
      WRITE: / TABLE_1-LIFNR  UNDER TEXT-003,   "Vendor number
               TABLE_1-NAME1  UNDER TEXT-004,   "Vendor name
          (11) TABLE_1-OSBAL  UNDER TEXT-006,   "outstanding balance
               TABLE_1-BELNR  UNDER TEXT-012,   "last document number
               TABLE_1-BLDAT  UNDER TEXT-008,   "last document date
               TABLE_1-SPERR  UNDER TEXT-009,   "blocked
               TABLE_1-LOEVM  UNDER TEXT-011.   "marked for deletion
    ENDLOOP.

END-OF-SELECTION.

***************************  END OF PROGRAM  ***************************
