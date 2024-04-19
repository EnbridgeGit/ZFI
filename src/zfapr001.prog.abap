REPORT ZFAPR001A NO STANDARD PAGE HEADING LINE-SIZE 132 line-count 65.
******************************************************************
*      Owner: Centra/Union Gas Ltd. - BIS                        *
* Programmer: Yaxin Veliz - OmniLogic Systems Group (Toronto)    *
*       Date: September 10th, 1996                               *
* Request ID: DRFI0143                                           *
*                                                                *
* The following program will generate a control-break report     *
* based on Accounting Clerks and the block invoice payment       *
* selected.   Detailed information a long with the appropriate   *
* totals will be produced.                                       *
******************************************************************
* CHANGES                                                        *
* 2006/11/10 Mohammad TR10 Changed to:1)Add clerk# and vendor# in*
*                          variant screen. 2)Improve var. screen.*
*                          3) Apply DB/CR for amount column.     *
*                          4) Cleared items not to be included in*
*                          the report.                           *
* 2004/01/23 mdemeest ---- Changed company code to range         *
*                          Changed to Posting Date range         *
* 2003/04/01 mdemeest 1019 Changed default company code of 'UGL  *
*                          to memory id of BUK                   *
*----------------------------------------------------------------*

TABLES: BSIK,                   "Accounting: Secondary Index for Vendors
        LFA1,                   "Vendor Master - General Description
        LFB1,                   "Vendor Master - Company Code
        T001A,                  "Company Codes
        t001,                   "Company Code Descriptions
        trdirt,                 "Program (abap) Titles
        T001S.                  "Accounting Clerks
DATA:
 wabukrs like bsik-bukrs,
 wasname like t001s-sname,
 TOTAL(16)    TYPE P DECIMALS 2,       "Field to store Grand Total
 POST1        TYPE D,                  "First Date Range
 POST2        LIKE POST1,              "Second Date Range
 LN_CNTR      TYPE I,                  "Counter for Page Break
 BLK-TEXT(25) TYPE C,                  "Block Payment Text
 REC(4)       TYPE N,                  "Record Counter
 TREC         LIKE REC,                "Total Record Counter
 BEGIN OF WTAB OCCURS 1000,             "Table for Control break
   bukrs like bsik-bukrs,
   BUSAB LIKE T001S-BUSAB,
   ZLSPR LIKE BSIK-ZLSPR,
   SNAME LIKE T001S-SNAME,
   ZFBDT LIKE BSIK-ZFBDT,
   DMBTR(16) TYPE P DECIMALS 2,
   XBLNR LIKE BSIK-XBLNR,
   BUDAT LIKE BSIK-BUDAT,
   BELNR LIKE BSIK-BELNR,
   LIFNR LIKE BSIK-LIFNR,
   NAME(28) TYPE C,
   rec-ctr type i,
 END OF WTAB.


SELECTION-SCREEN BEGIN OF BLOCK BORDER WITH FRAME TITLE TEXT-001.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT 30(21) TEXT-001 MODIF ID ABC.
*SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN ULINE 30(21).
*SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT 34(12) TEXT-002 MODIF ID ABC.
*SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN ULINE 34(12).
*SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN SKIP.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT  2(13) TEXT-003 MODIF ID ABC.
*SELECTION-SCREEN: COMMENT 20(12) TEXT-069.
*PARAMETERS: P_COMPNY LIKE T001A-BUKRS
*            memory id BUK OBLIGATORY MODIF ID ABC.
*                        memory id BUK modif id abc.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK PERIOD WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN SKIP.
*SELECT-OPTIONS: S_COMPNY FOR BSIK-BUKRS.
SELECT-OPTIONS: S_COMPNY FOR T001-BUKRS.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 31(10) TEXT-005. " MODIF ID ABC.
SELECTION-SCREEN: COMMENT 56(10) TEXT-005. " MODIF ID ABC.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT 17(13) TEXT-006.
*SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS: S_BUDAT FOR BSIK-BUDAT. " MODIF ID ABC.
*PARAMETERS: P_DAY1(2)   DEFAULT sy-datum+4(2)  OBLIGATORY MODIF ID ABC,
*            P_MONTH1(2) DEFAULT sy-datum+6(2)  OBLIGATORY MODIF ID ABC,
*            P_YEAR1(4)  DEFAULT SY-DATUM(4)    OBLIGATORY MODIF ID ABC.
*SELECTION-SCREEN: COMMENT 42(13) TEXT-007.
*PARAMETERS: P_DAY2(2)   DEFAULT '01'           OBLIGATORY MODIF ID ABC,
*            P_MONTH2(2) DEFAULT '01'           OBLIGATORY MODIF ID ABC,
*            P_YEAR2(4)  DEFAULT SY-DATUM(4)    OBLIGATORY MODIF ID ABC.
*SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN SKIP.

*SELECTION-SCREEN END OF BLOCK PERIOD.

*SELECTION-SCREEN SKIP.

*SELECTION-SCREEN BEGIN OF BLOCK KEY
*          WITH FRAME TITLE TEXT-008.

*SELECTION-SCREEN SKIP.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT 5(16) TEXT-009.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT 5(23) TEXT-010.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT 5(24) TEXT-011.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN: COMMENT 1(53) TEXT-012 MODIF ID ABC.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS S_LIFNR FOR BSIK-LIFNR. " MODIF ID ABC.  "Vendor#
SELECTION-SCREEN SKIP.
SELECT-OPTIONS S_BUSAB FOR LFB1-BUSAB. " MODIF ID ABC.  "Clerk#
SELECTION-SCREEN SKIP.
SELECT-OPTIONS S_BLKPMT FOR BSIK-ZLSPR NO INTERVALS OBLIGATORY.

*SELECTION-SCREEN END OF BLOCK KEY.
SELECTION-SCREEN END OF BLOCK PERIOD.
SELECTION-SCREEN END OF BLOCK BORDER.

INITIALIZATION.
  S_BLKPMT-SIGN   = 'I'.
  S_BLKPMT-OPTION = 'EQ'.
  S_BLKPMT-LOW    = 'A'.
  APPEND S_BLKPMT.
  S_BLKPMT-LOW    = 'R'.
  APPEND S_BLKPMT.

**at selection-screen.
** if s_budat+3(8) = '00000000'.
**    move 'IBT' to s_budat+0(3).
**    s_budat+3(4) = sy-datum(4) - 1.   "First day previous year
**    s_budat+7(4) = '0101'.
**
**    s_budat+11(4) = SY-DATUM(4).      "Last day current year
**    s_budat+15(4) = '1231'.
**    append s_budat.
** endif.



* The following will highlight the screen's output for certain texts. *

*AT SELECTION-SCREEN OUTPUT.
* LOOP AT SCREEN.
*   CHECK SCREEN-GROUP1 = 'ABC'.
*   SCREEN-INTENSIFIED = '1'.
*   MODIFY SCREEN.
* ENDLOOP.

***********************BEGINNING OF MAIN PROGRAM************************

START-OF-SELECTION.
  select single * from trdirt    "Report Title
    where name = 'ZFAPR001'
    and sprsl = sy-langu.
  PERFORM INITIALIZE.

  SELECT * FROM BSIK   WHERE BUKRS in s_COMPNY
                       AND  LIFNR IN S_LIFNR                "TR10
                       AND  BUDAT in s_budat
                       AND  ZLSPR IN S_BLKPMT
                       ORDER BY ZLSPR LIFNR.
*     if bsik-zlspr <> ' '.
        SELECT * FROM LFB1 WHERE LIFNR =  BSIK-LIFNR
                             AND BUKRS =  BSIK-BUKRS
                             AND BUSAB IN S_BUSAB.          "TR10
          SELECT SINGLE * FROM T001S WHERE BUSAB = LFB1-BUSAB
                                       AND BUKRS = LFB1-BUKRS.
          SELECT SINGLE * FROM LFA1  WHERE LIFNR = LFB1-LIFNR.
        PERFORM BUILD-TAB1.
        ENDSELECT.                                          "end of LFB1
*        PERFORM BUILD-TAB1.
*     endif.
  ENDSELECT.                                            "end of BSIK

  PERFORM GETBLOCK.
  PERFORM CHK_DATA.

END-OF-SELECTION.

***********************BEGINNING OF FORMS*******************************

* This routine initializes the date periods and line counter fields. *

FORM INITIALIZE.
 move s_budat+3(8) to post1.
 move s_budat+11(8) to post2.
 REFRESH WTAB.
ENDFORM.

* This routine writes the Sub Headings. *

*FORM WRT_HDG.
top-of-page.
* MOVE '0' TO LN_CNTR.
 FORMAT INTENSIFIED ON.
 write: /1 text-rpt, trdirt-name, 56 trdirt-text,         "Report Title
       100 text-dte, sy-datum, text-amp, sy-uzeit.
 WRITE: / text-clt under text-rpt, sy-mandt, sy-sysid,
          text-pge under text-dte, sy-pagno under sy-datum.
 WRITE: /56  TEXT-014, t001-butxt.
 write: /(10) s_budat+3(8) using edit mask '____/__/__' under text-014,
            '-', (10) s_budat+11(8) using edit mask '____/__/__'.
 ULINE: /.
 WRITE: /1 TEXT-015, 28 TEXT-018, 39 TEXT-018, 69 TEXT-021,      "Header
        88 TEXT-022, 100 TEXT-023, 124 TEXT-025.
 write: / text-016 under text-015,  6 text-017,
       28 text-019               , 39 text-020,
          text-019 under text-021, text-019 under text-022,
          text-024 under text-023, text-026 under text-025.
 ULINE: /.
* MOVE '11' TO LN_CNTR.
 FORMAT INTENSIFIED OFF.

*-------------------------  BUILD-TAB1  --------------------------------
*This routine clears and moves all the info to the wtab internal table.*
*-----------------------------------------------------------------------
FORM BUILD-TAB1.
 CLEAR WTAB.
 MOVE-CORRESPONDING BSIK TO WTAB.
 IF BSIK-SHKZG = 'H'.
    WTAB-DMBTR = BSIK-DMBTR * -1.
 ENDIF.
 MOVE LFA1-NAME1 TO WTAB-NAME.
 MOVE T001S-BUSAB TO WTAB-BUSAB.
 MOVE T001S-SNAME TO WTAB-SNAME.
 move 1 to wtab-rec-ctr.           "Counts number of records for totals
 APPEND WTAB.
 TOTAL = TOTAL + BSIK-DMBTR.
ENDFORM.

* This routine sorts and prepares all info in a control break format. *

FORM GETBLOCK.
 SORT: WTAB ASCENDING BY bukrs BUSAB zlspr.
*PERFORM CHK_KEY.

 loop at wtab.
  move wtab-bukrs to wabukrs.
  at new bukrs.                         "Company Header
     select single * from t001
       where bukrs = wabukrs.
     new-page.
  endat.
  at new busab.
    new-page.
  endat.
  move wtab-sname to wasname.
  at new zlspr.                         "Block Key Header
     perform wrt_new.
  endat.
  perform wrt_det.                      "Detail Line
  at end of zlspr.
     sum.
     perform chk_key.                   "Summary for Block Key
     perform wrt_end.
  endat.
  at end of bukrs.
     sum.                               "Summary for Company Code
     perform wrt_final.
  endat.
 endloop.

* LOOP AT WTAB.
*   REC = REC + 1.
*   PERFORM CHK_KEY.
*   AT NEW ZLSPR.
*      PERFORM WRT_NEW.
*   ENDAT.
*   PERFORM WRT_DET.
*   AT END OF ZLSPR.
*      PERFORM WRT_END.
*   ENDAT.
* ENDLOOP.
ENDFORM.


* This routine checks for the block key and moves the appropriate text.*

FORM CHK_KEY.
 IF WTAB-ZLSPR = '*'.
    BLK-TEXT = TEXT-029.
 ELSEIF  WTAB-ZLSPR = 'A'.
    BLK-TEXT = TEXT-030.
 ELSEIF WTAB-ZLSPR = 'R'.
    BLK-TEXT = TEXT-031.
 ENDIF.
ENDFORM.

* This routine breaks and prints out the Accounting Clerk and number. *

FORM WRT_NEW.
*    PERFORM WRT_HDG.
 ULINE: /1(35).
 WRITE: /2  WTAB-BUSAB, 6 wasname.  "WTAB-SNAME.
 WRITE:  1 SY-VLINE, 35 SY-VLINE.
 ULINE: /1(35).
 SKIP.
* LN_CNTR = LN_CNTR + 4.
ENDFORM.

* This routine prints the detail of the report. *

FORM WRT_DET.
* IF LN_CNTR >= 59.
*    PERFORM WRT_HDG.
* ENDIF.
* write: /1 wtab-zlspr,
 write: /28 WTAB-LIFNR, 39 WTAB-NAME, 69 WTAB-XBLNR, 88 WTAB-BELNR,
         100 WTAB-ZFBDT, 116 WTAB-DMBTR.
* LN_CNTR = LN_CNTR + 1.
ENDFORM.

* This routine prints the sub-totals for every set of block keys. *

FORM WRT_END.
 SUM.
 ULINE  /116(16).
 FORMAT INTENSIFIED ON.
 WRITE: /2 BLK-TEXT, 28 TEXT-033, 51 wtab-REC-ctr, 100 TEXT-027,
       114 '$', 116 WTAB-DMBTR.
 FORMAT INTENSIFIED OFF.
 SKIP.
 LN_CNTR = LN_CNTR + 3.
 TREC = TREC + REC.
 CLEAR REC.
ENDFORM.

* This routine checks to see if there is data and either writes an    *
* error message if it finds no data or it performs wrt_final routine. *

FORM CHK_DATA.
 IF SY-SUBRC NE 0.
    WRITE /1 TEXT-032.
 ELSE.
*  PERFORM WRT_FINAL.
 ENDIF.
ENDFORM.

* This routine writes the Grand Totals for the report.  *

FORM WRT_FINAL.
*    PERFORM WRT_HDG.
 SKIP.
 FORMAT COLOR COL_TOTAL ON INTENSIFIED ON.
 WRITE: /6 TEXT-034, 28 TEXT-033, 51 wtab-REC-ctr, 98 TEXT-028,
         113 '$', 115 wtab-dmbtr.
 FORMAT COLOR COL_TOTAL OFF.
 ULINE: /115(17).
 ULINE: /115(17).
ENDFORM.
