REPORT ZFAPI011 NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65
                MESSAGE-ID ZM.
************************************************************************
*    Program     :  ZFAPI011 - FI/AP: CGO/UGL Meger Vendor Scrub Report
*    Programmer  :  Ric Aarssen
*    Date        :  March 12, 1998
*
*    This ABAP will create one of three reports that will be downloaded
*    to excel.  This will assist A/P in identifying inactive and
*    duplicate vendors.
*    Report 1: Vendors that exist in both CGO and UGL
*
*    This ABAP will use an internal table to gather the data
*    that falls within the vendors selected.  It will then be
*    sorted and looped through to create report.
*
*    The prgrm will be executed in background and spooled, where it can
*    can be saved as a text file to be read into excel.
*
************************************************************************
*  98/04/08  raarssen change to accumulate the debits/credits for the
*                     fiscal year selected
*  98/03/25  MRadsma  Change to allow larger font in report, ie 80 ch
*                     per line instead of 132, reqd heading changes
************************************************************************

*****************************  TABLES   ********************************

TABLES: LFA1,                "Vendor master (general section)
*            name1 - Vendor name
        LFB1,                "Vendor master (company code)
*            lifnr - vendor number
*            bukrs - company code (CGO or UGL)
        LFC1.                "Vendor master (transaction figures)
*            gjahr - fiscal year                             98/04/08 ra
*            umsav - balance forward from previous year               ra
*            umXXs - 12 months debits     "XX = 01 to 12              ra
*            umXXh - 12 months credits    "unsigned                   ra

*---------  Internal table for Vendor records selected  ----------------
*  the first 4 fields are listed in the order required for the sort

DATA: BEGIN OF TABLE_1 OCCURS 0,
        LIFNR            LIKE LFB1-LIFNR,     "Vendor number
        NAME1            LIKE LFA1-NAME1,     "Vendor name
        OSBAL            LIKE LFC1-UMSAV,     "outstanding balance
      END OF TABLE_1.

**************************  DATA ELEMENTS  *****************************

*---------------------- Work Area --------------------------------------
DATA: W_LIFNR         LIKE LFA1-LIFNR.          "Vendor Number

***********************  SELECTION SCREEN  *****************************

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-900.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-010.
  PARAMETERS: P_GJAHR LIKE LFC1-GJAHR DEFAULT 1998.
SELECTION-SCREEN END OF BLOCK BOX1.

*---  added 98/04/08 ra  -----------------------------------------------
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_LIFNR FOR LFA1-LIFNR.
SELECTION-SCREEN END OF BLOCK BOX2.
*-----------------------------------------------------------------------

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN COMMENT 1(80) TEXT-901.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(80) TEXT-902.

***************************  MAIN ROUTINE  *****************************
*top-of-page is NOT being used. The heading is only to appear once on
*            the report for ease when the report gets dumped to excel
*
*------------------------ print report header --------------------------
FORMAT COLOR 4.
WRITE: /1 TEXT-RPT, SY-REPID COLOR 7 INTENSIFIED ON,
*      45 text-hdg,                                            "Title
*     105 text-dte, sy-datum, text-amp, sy-uzeit.
       32 TEXT-HDG,                                            "MRadsma
       62 TEXT-DTE, SY-DATUM.                                  "MRadsma
WRITE: / TEXT-CLT UNDER TEXT-RPT,
         SY-MANDT UNDER SY-REPID INTENSIFIED ON,
         TEXT-PGE UNDER TEXT-DTE,
         SY-PAGNO UNDER SY-DATUM INTENSIFIED ON.
ULINE.
SKIP.
WRITE: /1 TEXT-002, 15 TEXT-002, 51 TEXT-005.
FORMAT COLOR 1.
WRITE: /1 TEXT-003, 15 TEXT-004, 51 TEXT-006.
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

      MOVE LFB1-LIFNR TO W_LIFNR.
*---  read vendor master (Company Code)  does it exists in UGL  --------
      SELECT SINGLE * FROM LFB1
         WHERE LIFNR = W_LIFNR
         AND   BUKRS = 'UGL'.
         IF SY-SUBRC = 0.
            MOVE LFB1-LIFNR TO TABLE_1-LIFNR.           "Vendor number

*---  read vendor master for name and vendor group  --------------------
            SELECT SINGLE * FROM LFA1
               WHERE LIFNR = W_LIFNR
               AND   SPRAS = SY-LANGU.
               IF SY-SUBRC = 0.
                  MOVE LFA1-NAME1 TO TABLE_1-NAME1.     "Vendor name
               ENDIF.

*---  read vendor master (transaction figures)  ------------------------
               SELECT SINGLE * FROM LFC1
                  WHERE LIFNR = W_LIFNR
                  AND   BUKRS = 'CGO'
                  AND   GJAHR = P_GJAHR.
                  IF SY-SUBRC = 0.
*---  sum bal. forward and debits and credits for fiscal year  ---------
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

*--------------------  load internal tables  ---------------------------
         APPEND TABLE_1.
         CLEAR TABLE_1.
         ENDIF.
   ENDIF.
ENDSELECT.

*-------  create report listing Vendors in CGO & UGL  ------------------

*---  sort table by vendor number  -------------------------------------
    SORT TABLE_1 ASCENDING BY LIFNR.
    LOOP AT TABLE_1.

*-------------------  write report from table  -------------------------
      WRITE: /    TABLE_1-LIFNR     UNDER TEXT-003,
                  TABLE_1-NAME1     UNDER TEXT-004,
             (11) TABLE_1-OSBAL     UNDER TEXT-006.

    ENDLOOP.
    ULINE.

END-OF-SELECTION.
***************************  END OF PROGRAM  ***************************
