REPORT ZFGLI003 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132.

************************************************************************
* Program Description
* This abap reads an input file, separate the new Rate Rider records and
* splits them into component records.  The new component records will be
* merged with the non-RR records and written to a file to be fed into
* the Banner G/L interface
* - All Rate Rider records will be separated into component records and
*   each component will have a corresponding rate applied to the
*   volume to calculate the correct dollar amount.
*----------------------------------------------------------------------
* Changes:
* 2006/07/07 mdemeest - xxxx - added additional test to identify
*                              rate rider records
* 2006/08/15 gymana - Added a check to look for rate rider records with
*                     a blank effective date. If found, move the posting
*                     date to the eff date field.
* 2006/08/24 gymana - Changed the tolerance limit from 1.00 to 2.00 and
*                     modified the report headings to include units.
*                     Also removed report highlighting of variances over
*                     the tolerance limit.
* 2006/12/06 gymana - Fixed SELECT statement in form MAP_RATERIDER. The
*                     ENDSELECT was at the bottom of the routine and if
*                     a ZRRSCRC entry was not found, it was bypassing
*                     the error logic.  Moved ENDSELECT before IF
*                     statement.
* 2007/04/03 gymana - Added another total amount column by split code
*                     in the summary report.
* 2007/08/07 gymana - TR467 - Modified code to put tolerance limit as a
*                     variant parameter. Changed tolerance from 2.00 to
*                     5.00
* 2007/08/22 gymana - TR392 - Added new Rate Class field in input file
*                     and removed references to the ZRRSCRC table.
*                     Block all records with service category 'NONE' &
*                     rate class 'OT'.
* 2007/10/01 gymana - FIX - Code to block service cat 'NONE', rate class
*                     'OT' was removed.
* 2007/10/05 gymana - FIX - Replace all rate class 'OT' with SPACE.
* 2008/03/13 gymana - TR540 - Modified code to handle new base customer
*                     charge coming across in the old budget amt field.
*                     This program will take the delivery record and
*                     create a new customer charge record to be posted
*                     to SAP.
* 2009/01/19 gymana - FIX - Due to a table re-org, the sort order of the
*                     summary report changed. Adding an 'ORDER BY'
*                     statement to the select statement to correct this.
************************************************************************
TABLES: ZRRSPLT,          "Rate Rider Split Table
        ZRRATE,           "Rate Rider Component Rate Table
        ZFB08.            "Base Charge Breakout Table

*------------------------  Selection Screen  ---------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER: INFILE LIKE FILENAMECI-FILEEXTERN
                  DEFAULT '/usr/sap/interfaces/P01/BANNER/zbis100.chk'.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
* Output Files
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
OUTFILE LIKE FILENAMECI-FILEEXTERN DEFAULT
        '/usr/sap/interfaces/P01/BANNER/zbis100.dat',
P_TOLER(7)    TYPE P DECIMALS 2 DEFAULT '5.00'.
SELECTION-SCREEN END OF BLOCK BOX2.
*-------------------------  End of Input Screen  -----------------------

*-----------------------------------------------------------------------

* Used to parse file into various fields

DATA:  BEGIN OF BANN_FILE OCCURS 0,
       PROCESS_DATE       TYPE D,
       SERV_TYPE(4)       TYPE C,
       SERV_CAT(4)        TYPE C,
       GL_CLASSCD(4)      TYPE C,
       SERV_CLASS(2)      TYPE C,
       TOWN_CD(2)         TYPE C,
       MUNIC_CD(4)        TYPE C,
       BUDGET_IND(1)      TYPE C,
       CUST_NUM(16)       TYPE N,
       TRANS_AMT_SIGN(1)  TYPE C,
       TRANS_AMT(13)      TYPE C,
       CUST_CHRG_SIGN(1)  TYPE C,
       CUST_CHRG(11)      TYPE C,
       CONS_SIGN(1)       TYPE C,
       CONS(13)           TYPE C,
       NO_OF_CUSTS(6)     TYPE N,
       EFF_DATE           TYPE D,
       RATE_CLASS(4)      TYPE C,
       END OF BANN_FILE.

DATA:
    BEGIN OF INTAB OCCURS 0.
        INCLUDE STRUCTURE BANN_FILE.
DATA: END OF INTAB.

DATA:
    BEGIN OF WA_FILE OCCURS 0.
        INCLUDE STRUCTURE BANN_FILE.
DATA: END OF WA_FILE.

DATA:
    BEGIN OF WA_FILE2 OCCURS 0.
        INCLUDE STRUCTURE BANN_FILE.
DATA: END OF WA_FILE2.

DATA:  BEGIN OF RRDETTAB OCCURS 0,
       RRCODE             LIKE ZRRATE-RRCODE,
       CONS(13)           TYPE P DECIMALS 3,
       RATE               LIKE ZRRATE-RATE,
       RRTEXT             LIKE ZRRATE-RRTEXT,
       EFFDATE            LIKE BANN_FILE-EFF_DATE,
       TRANS_AMT(13)      TYPE P DECIMALS 2,
       END OF RRDETTAB.

DATA:  BEGIN OF ERR_FILE OCCURS 0,
       PROCESS_DATE       TYPE D,
       GL_CLASSCD(4)      TYPE C,
       SERV_CLASS(2)      TYPE C,
       TOWN_CD(2)         TYPE C,
       MUNIC_CD(4)        TYPE C,
       BUDGET_IND(1)      TYPE C,
       CUST_NUM(16)       TYPE C,
       EFF_DATE           TYPE D,
       ERR_MSG(50)        TYPE C,
       END OF ERR_FILE.

DATA:  BEGIN OF SPLIT_TABLE OCCURS 500,
       RRCODE             LIKE ZRRATE-RRCODE,
       RRTEXT             LIKE ZRRATE-RRTEXT,
       RATE               LIKE ZRRATE-RATE,
       FROM_DATE          LIKE ZRRATE-FROM_DATE,
       TOT_RATE_AMT       TYPE P DECIMALS 2,
       END OF SPLIT_TABLE.

DATA:
      BEGIN OF SPLTTAB OCCURS 0.
        INCLUDE STRUCTURE SPLIT_TABLE.
DATA: END OF SPLTTAB.

DATA: RRTAB LIKE ZRRSPLT OCCURS 0 WITH HEADER LINE.


DATA:  INREC(251),
       LN_CNTR                    TYPE I VALUE 0,
       W_RATE                     LIKE ZRRATE-RATE,
       PREV_DATE                  LIKE ZRRATE-FROM_DATE,
       W_EFFDATE                  LIKE BANN_FILE-EFF_DATE,
       W_CONS(13)                 TYPE P DECIMALS 3,
       W_TRANS_AMT(13)            TYPE P DECIMALS 2,
       W_RRTRANS_AMT(13)          TYPE P DECIMALS 2,
       W_CUST_CHRG(11)            TYPE P DECIMALS 2,
       SUM_TRANS_AMT(13)          TYPE P DECIMALS 2,
       W_GRAND_TOT_TRANS_AMT(15)  TYPE P DECIMALS 2,
       W_FILE_TOT_TRANS_AMT(15)   TYPE P DECIMALS 2,
       W_R999_TOT_VARIANCE(15)    TYPE P DECIMALS 2,
       W_R998_TOT_VARIANCE(15)    TYPE P DECIMALS 2,
       W_ADJ_SUM_AMT(7)           TYPE P DECIMALS 2,
       W_VARIANCE(13)             TYPE P DECIMALS 2,
       SPLIT_SUM_AMT(13)          TYPE P DECIMALS 2,
       SUM_AMT_BY_SPLIT_CD(13)    TYPE P DECIMALS 2,
       PREV_RRCODE                LIKE SPLTTAB-RRCODE,
       W_SPLIT_CNT(2)             TYPE N,
       W_REC_CNTR(2)              TYPE N,
       W_RRIDER_NOT_FOUND_FLAG(1) TYPE C,
       W_ERRFILE_CREATED_FLAG(1)  TYPE C,
       W_VAR_TOLERANCE_FLAG(1)    TYPE C,
*       W_TOLERANCE_LIMIT          TYPE P DECIMALS 2 value '2.00',
       MSG_TEXT(50).

************************************************************************
PERFORM OPEN_FILES.                      "Open files
PERFORM LOAD_ZRRATE.
PERFORM SPLIT_FILE.
PERFORM PROCESS_CUSTOMER_CHARGE.
PERFORM WRITE_OUTPUT_FILE.
PERFORM GENERATE_SPLIT_SUM_RPT.
PERFORM GENERATE_ERR_RPT.


************************************************************************
* Routine to open the physical files for input & output in text mode.
*************************  OPEN_FILES **********************************
FORM OPEN_FILES.
  OPEN DATASET INFILE FOR INPUT IN TEXT MODE
               MESSAGE MSG_TEXT.

  IF SY-SUBRC NE 0.
    WRITE: 'File cannot be opened. Reason: ', MSG_TEXT.
  ENDIF.

  OPEN DATASET OUTFILE FOR OUTPUT IN TEXT MODE.

* Read Payfile and perform payroll check.

  DO.
    READ DATASET INFILE INTO BANN_FILE.
    IF SY-SUBRC <> '0'.
      EXIT.
    ENDIF.
    APPEND BANN_FILE.
  ENDDO.

ENDFORM.                    "OPEN_FILES

************************************************************************
*                          LOAD_ZRRATE
************************************************************************
FORM LOAD_ZRRATE.
    SELECT * FROM ZRRATE
       ORDER BY RRCODE ASCENDING FROM_DATE ASCENDING.
    MOVE ZRRATE-RRCODE    TO SPLTTAB-RRCODE.
    MOVE ZRRATE-RRTEXT    TO SPLTTAB-RRTEXT.
    MOVE ZRRATE-RATE      TO SPLTTAB-RATE.
    MOVE ZRRATE-FROM_DATE TO SPLTTAB-FROM_DATE.
    MOVE 0                TO SPLTTAB-TOT_RATE_AMT.
    APPEND SPLTTAB.
  ENDSELECT.

*   SELECT RRCODE, RRTEXT, RATE, FROM_DATE
*      FROM ZRRATE
*     INTO SPLTTAB.
*     FROM ( ZRRATE LEFT OUTER JOIN ZRRSPLT
*            ON ZRRATE~RRCODE = ZRRSPLT~RRCODE ).

*   DELETE ADJACENT DUPLICATES FROM SPLTTAB
*      COMPARING ALL FIELDS.

ENDFORM.                    "LOAD_ZRRATE

************************************************************************
*  This routine reads the Banner input file one record at a time and
*  splits out the Rate Rider records in order to map the GL class &
*  service class to the correct RR component codes. If it is a non Rate
*  Rider record, write the record to the output file directly.
************************************************************************
FORM SPLIT_FILE.
  LN_CNTR = '0'.
  LOOP AT BANN_FILE.

    IF LN_CNTR = 0.
      PERFORM WRITE_RECONC_HDG.
    ENDIF.

*   If there is a blank effective date, replace with the process date.
*   This has to be done since ABAP will truncate the field if it is
*   blank thereby creating a shorter record length.
    IF BANN_FILE-EFF_DATE = '        '.
      MOVE BANN_FILE-PROCESS_DATE TO BANN_FILE-EFF_DATE.
    ENDIF.

    IF BANN_FILE-GL_CLASSCD+2(1) = 'R'
       AND BANN_FILE-SERV_CAT = 'GAS'.                  "2006/07/07 md
      PERFORM MAP_RATERIDER.
    ELSE.
*     Replace all rate class 'OT' with blanks.
      IF BANN_FILE-RATE_CLASS = 'OT'.
        MOVE SPACE TO BANN_FILE-RATE_CLASS.
      ENDIF.
*      TRANSFER BANN_FILE TO OUTFILE LENGTH 103.
      MOVE-CORRESPONDING BANN_FILE TO WA_FILE.
      APPEND WA_FILE.
    ENDIF.

    IF LN_CNTR >= 55.
      NEW-PAGE.
      PERFORM WRITE_RECONC_HDG.
    ENDIF.

  ENDLOOP.

  FORMAT INTENSIFIED ON.
  SKIP 4.
  WRITE /46 'INPUT FILE TOTAL'.
  WRITE (14) W_FILE_TOT_TRANS_AMT UNDER TEXT-008.
  WRITE /46 'COMPONENT GRAND TOTAL'.
  WRITE (14) W_GRAND_TOT_TRANS_AMT UNDER TEXT-008.
  WRITE /46 'R998 VARIANCE GRAND TOTAL'.
  WRITE (14) W_R998_TOT_VARIANCE UNDER TEXT-008.
  WRITE /46 'R999 VARIANCE GRAND TOTAL'.
  WRITE (14) W_R999_TOT_VARIANCE UNDER TEXT-008.
  LN_CNTR = LN_CNTR + 7.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "SPLIT_FILE

************************************************************************
* Using the GL Class code, find the corresponding component rate(s)
************************************************************************
FORM  MAP_RATERIDER.

  MOVE 'N' TO W_RRIDER_NOT_FOUND_FLAG.
  MOVE BANN_FILE-CONS TO W_CONS.
  IF BANN_FILE-CONS_SIGN = '-'.
    W_CONS = W_CONS * -1.
  ENDIF.
  MOVE BANN_FILE-TRANS_AMT TO W_TRANS_AMT.
  IF BANN_FILE-TRANS_AMT_SIGN = '-'.
    W_TRANS_AMT = W_TRANS_AMT * -1.
  ENDIF.

  SKIP.

* Select all Rate Rider records that match the Banner GL class
* code and the rate class code.

  SELECT *
    INTO TABLE RRTAB
    FROM ZRRSPLT
   WHERE GLCODE = BANN_FILE-GL_CLASSCD AND
         RATECL = BANN_FILE-RATE_CLASS.

  IF SY-SUBRC = 0.
    PERFORM PROCESS-SPLIT-RECORDS.
    PERFORM PRINT-AND-WRITE-SPLIT-DETAIL.
  ELSE.
*    TRANSFER BANN_FILE TO OUTFILE LENGTH 103.
    MOVE-CORRESPONDING BANN_FILE TO WA_FILE.
    APPEND WA_FILE.
    MOVE-CORRESPONDING BANN_FILE TO ERR_FILE.
    MOVE 'No rate rider component codes found' TO
    ERR_FILE-ERR_MSG.
    COLLECT ERR_FILE.
    MOVE 'Y' TO W_RRIDER_NOT_FOUND_FLAG.
    MOVE 'Y' TO W_ERRFILE_CREATED_FLAG.
  ENDIF.
ENDFORM.                    "MAP_RATERIDER

************************************************************************
* Determine the transaction amt for each split. Any rounding variances
* will be added to the last split record and variances over the
* tolerance limit will be reported as an R998 / R999 record
************************************************************************
FORM PROCESS-SPLIT-RECORDS.

  REFRESH RRDETTAB.
  SUM_TRANS_AMT = 0.
  W_SPLIT_CNT = 0.
  W_REC_CNTR = 1.
  LOOP AT RRTAB.
    MOVE RRTAB-RRCODE TO RRDETTAB-RRCODE.
    MOVE W_CONS       TO RRDETTAB-CONS.
    MOVE RRTAB-RRTEXT TO RRDETTAB-RRTEXT.
    MOVE '00000000'   TO PREV_DATE.
    MOVE '0'          TO W_RATE.
    PERFORM APPLY_RR_RATES USING BANN_FILE-EFF_DATE.
    APPEND RRDETTAB.
    W_REC_CNTR = W_REC_CNTR + 1.

  ENDLOOP.
ENDFORM.                    "PROCESS-SPLIT-RECORDS


************************************************************************
* FIND THE APPROPRIATE RATE RIDER RATES USING EFFECTIVE DATE AND
* COMPONENT CODES AND APPLY THEM TO THE DOLLARS.
************************************************************************
FORM APPLY_RR_RATES USING W_EFFDATE.

  SELECT *
    FROM ZRRATE
   WHERE RRCODE = RRTAB-RRCODE
     AND FROM_DATE <= W_EFFDATE.
    IF ZRRATE-FROM_DATE > PREV_DATE.
      MOVE ZRRATE-FROM_DATE TO PREV_DATE.
      MOVE ZRRATE-RATE TO W_RATE.
    ENDIF.
  ENDSELECT.

  IF SY-SUBRC = 0.
    W_RRTRANS_AMT = ( W_CONS * W_RATE ) / 100.
    RRDETTAB-EFFDATE   = PREV_DATE.
    RRDETTAB-TRANS_AMT = W_RRTRANS_AMT.
    RRDETTAB-RATE      = W_RATE.
    SUM_TRANS_AMT = SUM_TRANS_AMT + W_RRTRANS_AMT.
    W_SPLIT_CNT = W_SPLIT_CNT + 1.
  ELSE.
    RRDETTAB-EFFDATE   = PREV_DATE.
    RRDETTAB-TRANS_AMT = 0.
    RRDETTAB-RATE      = 0.
    MOVE-CORRESPONDING BANN_FILE TO ERR_FILE.
    MOVE 'No rate found for rate rider code and/or date' TO
         ERR_FILE-ERR_MSG.
    COLLECT ERR_FILE.
    MOVE 'Y' TO W_ERRFILE_CREATED_FLAG.
  ENDIF.

ENDFORM.                    "APPLY_RR_RATES

************************************************************************
* Print each input record and it's corresponding rate rider splits on
* the detail report then save each split to the output file to be
* passed to the Banner interface.
************************************************************************
FORM PRINT-AND-WRITE-SPLIT-DETAIL.

  W_VARIANCE = W_TRANS_AMT - SUM_TRANS_AMT.
  W_VAR_TOLERANCE_FLAG = 'N'.

* commented out to remove report highlighting    GY

*  IF ABS( W_VARIANCE ) > W_TOLERANCE_LIMIT.
*     W_VAR_TOLERANCE_FLAG = 'Y'.
*  ENDIF.

* Highlight input record and raterider splits if over the tolerance

*  IF W_VAR_TOLERANCE_FLAG = 'Y'.
*     FORMAT INTENSIFIED ON.
*     ULINE.
*     LN_CNTR = LN_CNTR + 1.
*  ENDIF.

* Print original input record.

  WRITE /(4) BANN_FILE-GL_CLASSCD UNDER TEXT-005.
  WRITE (16) W_CONS UNDER TEXT-006 DECIMALS 3.
  WRITE (10) BANN_FILE-EFF_DATE UNDER TEXT-010 USING
             EDIT MASK '____/__/__'.
  WRITE (14) W_TRANS_AMT UNDER TEXT-008 DECIMALS 2.
  SKIP.
  LN_CNTR = LN_CNTR + 3.
  ADD W_TRANS_AMT TO W_FILE_TOT_TRANS_AMT.

* Print component splits

  W_REC_CNTR = 1.
  LOOP AT RRDETTAB.
    WRITE /(4) RRDETTAB-RRCODE UNDER TEXT-005.
    WRITE (20) RRDETTAB-RRTEXT UNDER TEXT-017.
    WRITE (16) RRDETTAB-CONS UNDER TEXT-006 DECIMALS 3.
    WRITE (12)  RRDETTAB-RATE UNDER TEXT-007 DECIMALS 5.
    WRITE (10) RRDETTAB-EFFDATE UNDER TEXT-010 USING
                EDIT MASK '____/__/__'.
    WRITE (14) RRDETTAB-TRANS_AMT UNDER TEXT-008.
    LN_CNTR = LN_CNTR + 1.
    W_REC_CNTR = W_REC_CNTR + 1.
  ENDLOOP.

* Print R999 record on report if a variance exists and is under
* the variance tolerance or R998 record if over the tolerance.

  IF W_VARIANCE <> 0.
    IF ABS( W_VARIANCE ) > P_TOLER.
      WRITE /(4) 'R998' UNDER TEXT-005.
    ELSE.
      WRITE /(4) 'R999' UNDER TEXT-005.
    ENDIF.
    WRITE (15) 'V A R I A N C E' UNDER TEXT-017.
    WRITE (14) W_VARIANCE UNDER TEXT-008.
    SUM_TRANS_AMT = SUM_TRANS_AMT + W_VARIANCE.
    LN_CNTR = LN_CNTR + 1.
    W_REC_CNTR = W_REC_CNTR + 1.
  ENDIF.

  SKIP.
  WRITE /46 'COMPONENT TOTAL AMT'.
  WRITE (14) SUM_TRANS_AMT UNDER TEXT-008.

* commented out to remove report highlighting    GY
*  IF W_VAR_TOLERANCE_FLAG = 'Y'.
*    WRITE 105 '****'.
*    WRITE 110 'VARIANCE: '.
*    WRITE (10) W_VARIANCE.
*    WRITE 134 '****'.
*  ELSE.
  WRITE 105 '    '.
  WRITE 110 'VARIANCE: '.
  WRITE (10) W_VARIANCE.
*  ENDIF.

  LN_CNTR = LN_CNTR + 2.

* commented out to remove report highlighting    GY
*  IF W_VAR_TOLERANCE_FLAG = 'Y'.
*     ULINE.
*       LN_CNTR = LN_CNTR + 1.
*     FORMAT INTENSIFIED OFF.
*  ENDIF.

* Write split detail record to the file.

  W_REC_CNTR = 1.
  LOOP AT RRDETTAB.
    CLEAR INTAB.
    MOVE-CORRESPONDING BANN_FILE TO INTAB.
    MOVE RRDETTAB-RRCODE TO INTAB-GL_CLASSCD.
    INTAB-TRANS_AMT = RRDETTAB-TRANS_AMT.

    PERFORM SPLIT_SUMMARY.
    IF RRDETTAB-TRANS_AMT < 0.
      MOVE '-' TO INTAB-TRANS_AMT_SIGN.
    ELSE.
      MOVE '+' TO INTAB-TRANS_AMT_SIGN.
    ENDIF.
    TRANSLATE INTAB-TRANS_AMT USING '- '.
    SHIFT INTAB-TRANS_AMT RIGHT DELETING TRAILING SPACE.
    TRANSLATE INTAB-TRANS_AMT USING ' 0'.
*    TRANSFER INTAB TO OUTFILE LENGTH 103.
    MOVE-CORRESPONDING INTAB TO WA_FILE.
    APPEND WA_FILE.
    W_REC_CNTR = W_REC_CNTR + 1.
    ADD RRDETTAB-TRANS_AMT TO W_GRAND_TOT_TRANS_AMT.
  ENDLOOP.

* If variance exists, generate an R999 record.
  IF W_VARIANCE <> 0.
    CLEAR INTAB.
    MOVE-CORRESPONDING BANN_FILE TO INTAB.
    IF ABS( W_VARIANCE ) > P_TOLER.
      MOVE 'R998' TO INTAB-GL_CLASSCD.
    ELSE.
      MOVE 'R999' TO INTAB-GL_CLASSCD.
    ENDIF.
    INTAB-TRANS_AMT = W_VARIANCE.

    IF W_VARIANCE < 0.
      MOVE '-' TO INTAB-TRANS_AMT_SIGN.
    ELSE.
      MOVE '+' TO INTAB-TRANS_AMT_SIGN.
    ENDIF.
    TRANSLATE INTAB-TRANS_AMT USING '- '.
    SHIFT INTAB-TRANS_AMT RIGHT DELETING TRAILING SPACE.
    TRANSLATE INTAB-TRANS_AMT USING ' 0'.
*    TRANSFER INTAB TO OUTFILE LENGTH 103.
    MOVE-CORRESPONDING INTAB TO WA_FILE.
    APPEND WA_FILE.
    W_REC_CNTR = W_REC_CNTR + 1.
    IF ABS( W_VARIANCE ) > P_TOLER.
      W_R998_TOT_VARIANCE = W_R998_TOT_VARIANCE + W_VARIANCE.
    ELSE.
      W_R999_TOT_VARIANCE = W_R999_TOT_VARIANCE + W_VARIANCE.
    ENDIF.

  ENDIF.

ENDFORM.                    "PRINT-AND-WRITE-SPLIT-DETAIL

************************************************************************
* Sum up amounts per split code / effective date
************************************************************************
FORM SPLIT_SUMMARY.

  LOOP AT SPLTTAB
   WHERE RRCODE = RRDETTAB-RRCODE
     AND FROM_DATE = RRDETTAB-EFFDATE.
    ADD RRDETTAB-TRANS_AMT TO SPLTTAB-TOT_RATE_AMT.
    MODIFY SPLTTAB.
  ENDLOOP.

ENDFORM.                    "SPLIT_SUMMARY


************************************************************************
* Generate Split Report header
************************************************************************
FORM WRITE_RECONC_HDG.

  CLEAR LN_CNTR.
  FORMAT INTENSIFIED ON.
  WRITE: /1 TEXT-001, 36 TEXT-002.
  WRITE: 106 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.

  WRITE: /1 TEXT-004, 44 'FILE PROCESS DATE:'.
  WRITE (10) BANN_FILE-PROCESS_DATE USING EDIT MASK '____/__/__'.
  WRITE: 121 TEXT-PGE, SY-PAGNO.
  SKIP.
  WRITE: /3 TEXT-005, 13 TEXT-017, 35 TEXT-006, 58 TEXT-007,
            77 TEXT-010, 91 TEXT-008.
  SKIP.
  MOVE '5' TO LN_CNTR.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "WRITE_RECONC_HDG

************************************************************************
* Generate Error Report
************************************************************************
FORM GENERATE_ERR_RPT.

  PERFORM WRITE_ERR_HDG.
  IF W_ERRFILE_CREATED_FLAG = 'Y'.
    LOOP AT ERR_FILE.
      IF LN_CNTR >= 59.
        PERFORM WRITE_ERR_HDG.
      ENDIF.
      WRITE ERR_FILE.
      LN_CNTR = LN_CNTR + 1.
    ENDLOOP.
  ELSE.
    WRITE 43 '*** No errors to report ***'.
  ENDIF.
ENDFORM.                    "GENERATE_ERR_RPT

************************************************************************
* Generate Error Report Header
************************************************************************

FORM WRITE_ERR_HDG.
  NEW-PAGE.
  CLEAR LN_CNTR.
  FORMAT INTENSIFIED ON.
  WRITE: /1 TEXT-001, 41 TEXT-009.
  WRITE: 106 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: /1 TEXT-004, 50 SY-DATUM.
  WRITE: 121 TEXT-PGE, SY-PAGNO.
  SKIP.
  MOVE '3' TO LN_CNTR.
  FORMAT INTENSIFIED OFF.
ENDFORM.                    "WRITE_ERR_HDG

************************************************************************
* This routine will look for all delivery records and split out the
* base customer charge and place it in a new record.
* TR540 2008/03/14 GYMANA
************************************************************************
FORM PROCESS_CUSTOMER_CHARGE.
  LOOP AT WA_FILE.
    CLEAR WA_FILE2.
    MOVE-CORRESPONDING WA_FILE TO WA_FILE2.
    APPEND WA_FILE2.

    SELECT * FROM ZFB08
      WHERE GLCODE = WA_FILE-GL_CLASSCD.
    ENDSELECT.

*   Add new breakout cust charge record.
    IF SY-SUBRC = 0.
      CLEAR WA_FILE2.
      MOVE-CORRESPONDING WA_FILE  TO WA_FILE2.
      MOVE ZFB08-BC_GLCODE        TO WA_FILE2-GL_CLASSCD.
      MOVE WA_FILE-CUST_CHRG_SIGN TO WA_FILE2-TRANS_AMT_SIGN.
      MOVE WA_FILE-CUST_CHRG      TO WA_FILE2-TRANS_AMT.
      SHIFT WA_FILE2-TRANS_AMT RIGHT DELETING TRAILING SPACE.
      TRANSLATE WA_FILE2-TRANS_AMT USING ' 0'.
      MOVE '+'                    TO WA_FILE2-CUST_CHRG_SIGN.
      MOVE '00000000.00'          TO WA_FILE2-CUST_CHRG.
      MOVE '+'                    TO WA_FILE2-CONS_SIGN.
      MOVE '000000000.000'        TO WA_FILE2-CONS.
      APPEND WA_FILE2.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "PROCESS_CUSTOMER_CHARGE

************************************************************************
* Write working area file to output file.
************************************************************************
FORM WRITE_OUTPUT_FILE.
  LOOP AT WA_FILE2.
    TRANSFER WA_FILE2 TO OUTFILE LENGTH 103.
  ENDLOOP.

ENDFORM.                    "WRITE_OUTPUT_FILE

************************************************************************
* Generate Summary by Split Code Report
************************************************************************
FORM GENERATE_SPLIT_SUM_RPT.

  PERFORM WRITE_SUM_HDG.
* Loop through SPLTTAB and generate a summary line for each split code &
* rate with non-zero amounts.

  LOOP AT SPLTTAB.
    IF LN_CNTR >= 59.
      PERFORM WRITE_SUM_HDG.
    ENDIF.
* Print summed amount by split code for the previous code.
    IF NOT SPLTTAB-RRCODE = PREV_RRCODE AND
       NOT PREV_RRCODE IS INITIAL AND
       NOT SUM_AMT_BY_SPLIT_CD IS INITIAL.
      WRITE (15) SUM_AMT_BY_SPLIT_CD UNDER TEXT-022.
      SKIP.
      MOVE 0 TO SUM_AMT_BY_SPLIT_CD.
      MOVE SPLTTAB-RRCODE TO PREV_RRCODE.
    ENDIF.

    IF SPLTTAB-TOT_RATE_AMT <> 0.
      WRITE /(4)  SPLTTAB-RRCODE UNDER TEXT-012.
      WRITE  (20) SPLTTAB-RRTEXT UNDER TEXT-017.
      WRITE  (12)  SPLTTAB-RATE UNDER TEXT-013.
      WRITE  (10) SPLTTAB-FROM_DATE UNDER TEXT-014.
      WRITE  (15)  SPLTTAB-TOT_RATE_AMT UNDER TEXT-015.
      LN_CNTR = LN_CNTR + 1.
      ADD SPLTTAB-TOT_RATE_AMT TO SPLIT_SUM_AMT.
      ADD SPLTTAB-TOT_RATE_AMT TO SUM_AMT_BY_SPLIT_CD.
      MOVE SPLTTAB-RRCODE TO PREV_RRCODE.
    ENDIF.
  ENDLOOP.

  IF NOT SUM_AMT_BY_SPLIT_CD IS INITIAL.
    WRITE (15) SUM_AMT_BY_SPLIT_CD UNDER TEXT-022.
  ENDIF.
  SKIP.
  WRITE /(4)  'R998' UNDER TEXT-012.
  WRITE  (15)  'V A R I A N C E' UNDER TEXT-017.
  WRITE  (15)  W_R998_TOT_VARIANCE UNDER TEXT-015.
  LN_CNTR = LN_CNTR + 1.
  SPLIT_SUM_AMT = SPLIT_SUM_AMT + W_R998_TOT_VARIANCE.
  WRITE /(4)  'R999' UNDER TEXT-012.
  WRITE  (15)  'V A R I A N C E' UNDER TEXT-017.
  WRITE  (15)  W_R999_TOT_VARIANCE UNDER TEXT-015.
  LN_CNTR = LN_CNTR + 1.
  SPLIT_SUM_AMT = SPLIT_SUM_AMT + W_R999_TOT_VARIANCE.
  SKIP.
  WRITE /40 'SPLIT SUMMARY GRAND TOTAL'.
  WRITE (15) SPLIT_SUM_AMT UNDER TEXT-015.
  LN_CNTR = LN_CNTR + 2.

ENDFORM.                    "GENERATE_SPLIT_SUM_RPT

************************************************************************
* Generate Summary by Split Code Report Header
************************************************************************

FORM WRITE_SUM_HDG.
  NEW-PAGE.
  CLEAR LN_CNTR.
  FORMAT INTENSIFIED ON.
  WRITE: /1 TEXT-001, 32 TEXT-011.
  WRITE: 106 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: /1 TEXT-004, 50 SY-DATUM.
  WRITE: 121 TEXT-PGE, SY-PAGNO.
  SKIP.
  WRITE:  /3 TEXT-012,
           9 TEXT-017,
          42 TEXT-013,
          60 TEXT-014,
          83 TEXT-015,
         110 TEXT-021.
  WRITE:  /3 TEXT-018,
          42 TEXT-019,
          60 TEXT-020,
         110 TEXT-022.
  SKIP 2.
  MOVE '6' TO LN_CNTR.
  FORMAT INTENSIFIED OFF.
ENDFORM.                    "WRITE_SUM_HDG
