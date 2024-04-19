*----------------------------------------------------------------------*
***INCLUDE ZFFII017B_FORMS
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SET_REPORT_HEADER                            "TR654
*&---------------------------------------------------------------------*
FORM SET_REPORT_HEADER.
*- Set Report Heading, file number
   IF SY-HOST+0(3) = 'WEI'.
*      MOVE '0004'                           TO CON_FILE.
      IF B_OPBAL = 'X'.
      MOVE 'Canada West - Opening Balance(C/F) For' TO TEXT_COMN.
      ELSE.
      MOVE 'Canada West - Trial Balance For' TO TEXT_COMN.
      ENDIF.
   ELSE.
*      MOVE '0001'                           TO CON_FILE.
      IF B_OPBAL = 'X'.
      MOVE 'Canada East - Opening Balance(C/F) For' TO TEXT_COMN.
      ELSE.
      MOVE 'Canada East - Trial Balance For' TO TEXT_COMN.
      ENDIF.
   ENDIF.

   IF P_LEDGER = '0L'.
      MOVE '  LEAD LEDGER  ' TO W_LEDGER.
   ELSE.
      IF P_LEDGER = 'NL'.
         MOVE 'NON LEAD LEDGER' TO W_LEDGER.
      ELSE.
         CALL FUNCTION 'POPUP_FOR_INTERACTION'
         EXPORTING
         HEADLINE    = '!! ERROR !!'
         TEXT1       = 'Invalid Ledger Specified'
         BUTTON_1    = 'OK'.
      STOP.
      ENDIF.
   ENDIF.

   CONCATENATE TEXT_COMN P_PERD '/' P_YEAR INTO TEXT_COMN
                  SEPARATED BY SPACE.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM SETUP_DATES                                              *
*                                                                     *
*  1. Determine last day of period for which data is being extracted. *
*  2. Determine the first day of next period.                         *
*  3. Period could be from 01 to 13, consider 13 as 12.               *
*---------------------------------------------------------------------*
FORM SETUP_DATES.

 IF P_PERD > 12.
    CONCATENATE P_YEAR '1201' INTO FIRST_DAY.        "For period 13
 ELSE.
    CONCATENATE P_YEAR  P_PERD '01' INTO FIRST_DAY.
 ENDIF.

 CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
           DAY_IN            = FIRST_DAY
      IMPORTING
           LAST_DAY_OF_MONTH = LAST_DAY
      EXCEPTIONS
           DAY_IN_NO_DATE    = 1
           OTHERS            = 2.

 IF SY-SUBRC <> 0.
    WRITE: /5 'INVALID DATA FOR LAST_DAY'.
    STOP.
 ELSE.
    IF P_PERD > 11.
       NEXT_PERIOD = 01.
       NEXT_YEAR   = P_YEAR + 1.
       MOVE NEXT_PERIOD TO FIRST_DAY+4(2).
       MOVE NEXT_YEAR   TO FIRST_DAY+0(4).
    ELSE.
       NEXT_PERIOD = P_PERD + 1.
       MOVE NEXT_PERIOD    TO    FIRST_DAY+4(2).
    ENDIF.
 ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHK_P_PERIOD                                             *
*---------------------------------------------------------------------*
FORM VALID_PERIOD_CHECK.

*- Check Valid Period entered
   IF P_PERD GT 13  OR  P_PERD < 01.
      CALL FUNCTION 'POPUP_FOR_INTERACTION'
           EXPORTING
           HEADLINE    = '!! ERROR !!'
           TEXT1       = 'Invalid Period Selected'
           BUTTON_1    = 'OK'.
      STOP.
   ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM GET_GL_DATA                                               *
*                                                                      *
*  This form reads data from G/L posting summary table FAGLFLEXT.      *
*  Use ZACCTNEW and Ledger Type to determine correct HFM account no.   *
*  to process per SAP Company/G/L account.                             *
*----------------------------------------------------------------------*
FORM GET_GL_DATA.

DATA: L_BUKRS LIKE FAGLFLEXT-RBUKRS.

*- Retrieve all GL data from FAGLFLEXT (TR582 - IFRS)
   SELECT RLDNR RBUKRS RACCT DRCRK RTCUR HSL01 HSL02 HSL03 HSL04
          HSL05 HSL06 HSL07 HSL08 HSL09 HSL10 HSL11 HSL12 HSLVT
         FROM FAGLFLEXT INTO CORRESPONDING FIELDS OF TABLE GTFAGL
         WHERE RLDNR = P_LEDGER              "Ledger Only
           AND RRCTY = '0'                   "Actual            TR105
           AND RBUKRS IN S_BUKRS             "Company Code
           AND RYEAR = P_YEAR                "Year
           AND RACCT IN S_RACCT.             "G/L Account #

   SORT GTFAGL BY RBUKRS RACCT.

*- Loop every G/L Account Transaction
   LOOP AT GTFAGL.

*-   Validate Company Code against ZJHDR
     IF GTFAGL-RBUKRS NE L_BUKRS.
        COMPANY_MAP = 'YES'.
        PERFORM CHECK_COMPANY_CODE.
     ENDIF.
     L_BUKRS = GTFAGL-RBUKRS.

     IF COMPANY_MAP = 'YES'.
        IF SAVE_BUKRS  = GTFAGL-RBUKRS AND
           SAVE_RACCT = GTFAGL-RACCT AND
           POSTED_IND  = 'Y'.
        ELSE.
           POSTED_IND  = 'N'.
           SAVE_BUKRS  = GTFAGL-RBUKRS.
           SAVE_RACCT = GTFAGL-RACCT.

*-         Populate Internal Table with valid FAGLFLEXT records
           PERFORM BUILD_SUBFAGL_TABLE.
        ENDIF.
     ENDIF.
   ENDLOOP.

*- Remove any record with zero qty
   LOOP AT SUBFAGL.
     CHECK SUBFAGL-AMOUNT = 0.
     DELETE SUBFAGL.
   ENDLOOP.

*- Apply PS Account Flipping & Affiliate Rules (ZPSACCT)
   PERFORM APPLY_PS_ACCT_RULES.         "<<<Ins. C11K918555 (Iss# TR495)

ENDFORM.

*----------------------------------------------------------------------*
*       FORM     BUILD_SUBFAGL_TABLE.                                  *
*----------------------------------------------------------------------*
FORM BUILD_SUBFAGL_TABLE.

    CLEAR SUBFAGL.
    MOVE GTFAGL-RLDNR  TO SUBFAGL-RLDNR.
    MOVE GTFAGL-RACCT  TO SUBFAGL-RACCT.
    MOVE W_WAERS       TO SUBFAGL-RTCUR.
    MOVE GTFAGL-RBUKRS TO SUBFAGL-RBUKRS.

*-  Calculate YTD Account Balance + add Balance Carried Forward
    CLEAR: W_DOLLARS.
    IF B_OPBAL = 'X'.                          "TR564
       W_DOLLARS = W_DOLLARS + GTFAGL-HSLVT.   "TR564
    ELSE.
       IF BOY_DATE = '00000000'.
          ADD GTFAGL-HSL01 FROM 1 TO P_PERD GIVING W_DOLLARS.
          W_DOLLARS = W_DOLLARS + GTFAGL-HSLVT.
       ELSE.
          W_DOLLARS = W_DOLLARS + GTFAGL-HSLVT * -1.
       ENDIF.
    ENDIF.

    PERFORM CHECK_ACCOUNT_MAPPING.

ENDFORM.

*----------------------------------------------------------------------*
*                CHEK_COMPANY_CODE                                     *
*----------------------------------------------------------------------*
FORM CHECK_COMPANY_CODE.

*- Check Journal Header for Consolidation (ZJHDR)
   SELECT SINGLE * FROM ZJHDR INTO ZJHDR
                   WHERE BUKRS = GTFAGL-RBUKRS.

   IF SY-SUBRC <> 0.
      MOVE 'NO' TO COMPANY_MAP.
      PERFORM COMPANY_NOT_FOUND.
   ELSE.
      SELECT SINGLE WAERS INTO W_WAERS FROM T001
      "Get comp.currency code
                           WHERE BUKRS = GTFAGL-RBUKRS.
   ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*                COMPANY_NOT_FOUND                                     *
*  Creates an internal table of company codes not found on table ZJHDR.*
*----------------------------------------------------------------------*
FORM COMPANY_NOT_FOUND.

   MOVE GTFAGL-RBUKRS TO NOTFOUND-BUKRS.
   APPEND NOTFOUND.
   CLEAR  NOTFOUND.

ENDFORM.
*----------------------------------------------------------------------*
*               CHECK_ACOUNT_MAPPING                                   *
*                                                                      *
* Determines how each account is to be handled depending upon the data *
* in ZACCTNEW table.  Mappings are done against a single GTFAGL record *
* from BUILD_SUBFAGL_TABLE                                             *
*----------------------------------------------------------------------*
FORM  CHECK_ACCOUNT_MAPPING.

   DATA: DFALT_IND, DBDC_IND, COMP_IND, NORML_IND, MULT_IND, ERROR_IND.

   DATA: LT_ZACCTNEW TYPE STANDARD TABLE OF ZACCTNEW WITH HEADER LINE.

   CLEAR: DFALT_IND,  DBDC_IND, COMP_IND, NORML_IND,
          MULT_IND, ERROR_IND.

*- Select all entries from ZACCTNEW for GTFAGL Account#
   REFRESH LT_ZACCTNEW.
   SELECT * FROM ZACCTNEW INTO TABLE LT_ZACCTNEW
           WHERE GLACCT = GTFAGL-RACCT.

*- For each ZACCTNEW determine Account Type
   LOOP AT LT_ZACCTNEW.

     IF LT_ZACCTNEW-DBDC = GTFAGL-DRCRK AND
        LT_ZACCTNEW-COCODE = GTFAGL-RBUKRS.
        MOVE 'Y' TO MULT_IND.

     ELSEIF LT_ZACCTNEW-COCODE = GTFAGL-RBUKRS AND
            LT_ZACCTNEW-DBDC  =  SPACE.
            MOVE 'Y' TO COMP_IND.

     ELSEIF LT_ZACCTNEW-DBDC  = GTFAGL-DRCRK AND
            LT_ZACCTNEW-COCODE = SPACE.
            MOVE 'Y' TO DBDC_IND.

     ELSEIF LT_ZACCTNEW-COCODE =  SPACE  AND
            LT_ZACCTNEW-DBDC  =  SPACE.
            MOVE 'Y' TO NORML_IND.
     ELSE.
            MOVE 'Y' TO ERROR_IND.
     ENDIF.
   ENDLOOP.

   IF ( LT_ZACCTNEW[] IS INITIAL ).
      MOVE 'Y' TO DFALT_IND.
      MOVE 'NO MAPPING FOUND-DEFAULTS USED' TO SUBFAGL-STATUS.
   ENDIF.

   MOVE W_DOLLARS TO SUBFAGL-AMOUNT.

*- Execute FORM based on Account Type determined
   IF  MULT_IND = 'Y'.
     PERFORM ACCOUNT_BY_MULTI.

   ELSEIF COMP_IND = 'Y'.
      PERFORM ACCOUNT_BY_COMPANY.

   ELSEIF DBDC_IND = 'Y'.
      PERFORM ACCOUNT_BY_DBDC.

   ELSEIF NORML_IND = 'Y'.
      PERFORM ACCOUNT_NORMAL.

   ELSEIF DFALT_IND = 'Y'.
      PERFORM ACCOUNT_BY_DEFAULT.

*- No Valid Combo in ZACCT Table
   ELSE.
      PERFORM ACCOUNT_WITH_ERROR.
   ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_BY_MULTI
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_MULTI.

DATA:  MULTI_FAGL_$  LIKE W_DOLLARS,
       SAVE_DRCRK    LIKE GTFAGL-DRCRK.

DATA: L_FAGL LIKE FAGLFLEXT.

*- Get total for this account from table FAGL
   SELECT * FROM FAGLFLEXT INTO L_FAGL
         WHERE RRCTY = '0'   "Actual
           AND RLDNR = P_LEDGER
           AND RBUKRS = SAVE_BUKRS
           AND RYEAR = P_YEAR
           AND RACCT = SAVE_RACCT.

      CLEAR: W_DOLLARS.
      IF B_OPBAL = 'X'.                              "TR564
         MULTI_FAGL_$ = MULTI_FAGL_$ + L_FAGL-HSLVT. "TR564
      ELSE.
         IF BOY_DATE = '00000000'.
            ADD L_FAGL-HSL01 FROM 1 TO P_PERD GIVING W_DOLLARS.
            MULTI_FAGL_$ = MULTI_FAGL_$ + W_DOLLARS + L_FAGL-HSLVT.
         ELSE.
            MULTI_FAGL_$ = MULTI_FAGL_$ + W_DOLLARS + L_FAGL-HSLVT * -1.
         ENDIF.
      ENDIF.
    ENDSELECT.

    IF MULTI_FAGL_$ <> 0.
       MOVE MULTI_FAGL_$  TO SUBFAGL-AMOUNT.
       IF   MULTI_FAGL_$ < 0.
            MOVE 'H'  TO  SAVE_DRCRK.
       ELSE.
            MOVE 'S'  TO  SAVE_DRCRK.
       ENDIF.

       SELECT SINGLE * FROM ZACCTNEW INTO ZACCTNEW
           WHERE GLACCT = L_FAGL-RACCT
             AND COCODE = L_FAGL-RBUKRS
             AND DBDC  = SAVE_DRCRK.     "H-Credit, S-Debit

       IF SY-SUBRC = 0.
          PERFORM ADD_THIS_RECORD.
          POSTED_IND = 'Y'.
          CLEAR SUBFAGL.
       ELSE.
          MOVE 'INVALID A/C, DB/DC COMBO-DEF.USED' TO  SUBFAGL-STATUS.
          PERFORM ACCOUNT_BY_DEFAULT.
          POSTED_IND = 'Y'.
          CLEAR SUBFAGL.
       ENDIF.
    ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_BY_DBDC
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_DBDC.

DATA:  DBDC_FAGL_$   LIKE W_DOLLARS,
       SAVE_DRCRK    LIKE GTFAGL-DRCRK.

DATA: L_FAGL LIKE FAGLFLEXT.

*- Get total for this account from table FAGLFLEXT
   SELECT * FROM FAGLFLEXT INTO L_FAGL
     WHERE RRCTY = '0'                               "Actual     TR105
       AND RLDNR = P_LEDGER
       AND RBUKRS = SAVE_BUKRS
       AND RYEAR = P_YEAR
       AND RACCT = SAVE_RACCT.

     CLEAR: W_DOLLARS.
     IF B_OPBAL = 'X'.                            "TR564
        DBDC_FAGL_$ = DBDC_FAGL_$ + L_FAGL-HSLVT. "TR564
     ELSE.
        IF BOY_DATE = '00000000'.
           ADD L_FAGL-HSL01 FROM 1 TO P_PERD GIVING W_DOLLARS.
           DBDC_FAGL_$ = DBDC_FAGL_$ + W_DOLLARS + L_FAGL-HSLVT.
        ELSE.
           DBDC_FAGL_$ = DBDC_FAGL_$ + W_DOLLARS + L_FAGL-HSLVT * -1.
        ENDIF.
     ENDIF.
   ENDSELECT.

   IF DBDC_FAGL_$ <> 0.
      MOVE DBDC_FAGL_$  TO SUBFAGL-AMOUNT.
      IF   DBDC_FAGL_$ < 0.
           MOVE 'H'  TO  SAVE_DRCRK.
      ELSE.
           MOVE 'S'  TO  SAVE_DRCRK.
      ENDIF.

      SELECT SINGLE * FROM ZACCTNEW INTO ZACCTNEW
          WHERE GLACCT = L_FAGL-RACCT
            AND COCODE = SPACE
            AND DBDC  = SAVE_DRCRK.     "H-Credit, S-Debit

      IF SY-SUBRC = 0.
         PERFORM ADD_THIS_RECORD.
         POSTED_IND = 'Y'.
         CLEAR SUBFAGL.
      ELSE.
         MOVE 'INVALID A/C, DB/DC COMBO-DEF.USED' TO  SUBFAGL-STATUS.
         PERFORM ACCOUNT_BY_DEFAULT.
         POSTED_IND = 'Y'.
         CLEAR SUBFAGL.
      ENDIF.
   ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_BY_COMPANY
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_COMPANY.

  SELECT SINGLE * FROM ZACCTNEW INTO ZACCTNEW
      WHERE GLACCT = GTFAGL-RACCT
        AND COCODE = GTFAGL-RBUKRS
        AND DBDC  = SPACE.

  IF SY-SUBRC = 0.
     PERFORM ADD_THIS_RECORD.
     CLEAR SUBFAGL.
  ELSE.
     MOVE 'INVALID A/C, COMPNY COMBO-DEF.USED' TO  SUBFAGL-STATUS.
     PERFORM ACCOUNT_BY_DEFAULT.
     CLEAR SUBFAGL.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_NORMAL
*----------------------------------------------------------------------*
FORM ACCOUNT_NORMAL.

  SELECT SINGLE * FROM ZACCTNEW
     WHERE GLACCT = GTFAGL-RACCT
       AND COCODE = SPACE
       AND DBDC  = SPACE.

  IF SY-SUBRC = 0.
     PERFORM ADD_THIS_RECORD.
     CLEAR SUBFAGL.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_BY_DEFAULT                                             *
*                                                                      *
*This routine is performed when account no. is not found in ZACCTNEW   *
*Or when no valid COMBO is found in ZACCTNEW table.                    *
*----------------------------------------------------------------------*
FORM ACCOUNT_BY_DEFAULT.

   MOVE ZJHDR-BUNIT TO SUBFAGL-BUNIT.
   MOVE ZJHDR-BUNAM TO SUBFAGL-BUNAM.
   MOVE GC_PS_DEFAULT TO SUBFAGL-HFMACCT.
   COLLECT SUBFAGL.

ENDFORM.

*----------------------------------------------------------------------*
*       ACCOUNT_WITH_ERROR
*----------------------------------------------------------------------*
FORM ACCOUNT_WITH_ERROR.

   MOVE 'NO VALID A/C COMBO-DEFAULTS USED' TO  SUBFAGL-STATUS.
   PERFORM ACCOUNT_BY_DEFAULT.
   CLEAR SUBFAGL.

ENDFORM.

*----------------------------------------------------------------------*
*        ADD_THIS_RECORD
*----------------------------------------------------------------------*
FORM ADD_THIS_RECORD.

   CLEAR: W_HFMACCT.

* Get AFFIL & HFM acct number from ZACCTNEW

   MOVE ZACCTNEW-AFFIL TO SUBFAGL-AFFIL.
   IF P_LEDGER = '0L'.
      MOVE ZACCTNEW-HFMLLACCT TO W_HFMACCT.
   ELSE.
      MOVE ZACCTNEW-HFMNLACCT TO W_HFMACCT.
   ENDIF.

   MOVE W_HFMACCT   TO SUBFAGL-HFMACCT.
   MOVE ZJHDR-BUNIT TO SUBFAGL-BUNIT.
   MOVE ZJHDR-BUNAM TO SUBFAGL-BUNAM.
   COLLECT SUBFAGL.

ENDFORM.

*----------------------------------------------------------------------*
*        BUILD_DETAIL_RECORD
*----------------------------------------------------------------------*
FORM BUILD_DETAIL_RECORD.

*- Start of Mod. -> C11K918555 (Iss# TR495)
*- New/Simplified File Structure for Hyperion Output

*- Business Unit & YTD Balance
   MOVE SUBFAGL-BUNIT   TO DETAIL-BUSINESS_UNIT_LN.
   MOVE SUBFAGL-AMOUNT  TO DETAIL-MONETARY_AMOUNT.
*- Account & SubAccount (Affiliate)
   MOVE SUBFAGL-HFMACCT   TO DETAIL-ACCOUNT.
   MOVE SUBFAGL-AFFIL   TO DETAIL-SUBACCOUNT.
*- End of Mod. -> C11K918555 (Iss# TR495)

   APPEND: DETAIL.
   CLEAR:  DETAIL.
   MOVE SUBFAGL-BUNAM   TO SAVE_BUNAM.
   MOVE SUBFAGL-RTCUR   TO SAVE_RTCUR.

ENDFORM.

*----------------------------------------------------------------------*
*                 CREATE_OUTPUT_FILE                                   *
*                                                                      *
*Data available in Detail is transfered to Output file on the server   *
*----------------------------------------------------------------------*
FORM CREATE_OUTPUT_FILE.

DATA: MSG(100),
      F_LENGTH TYPE I,
      LONG_DESCR TYPE STRING.

   CHECK NOT ( DETAIL[] IS INITIAL ).

   OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE MESSAGE MSG.

   IF SY-SUBRC NE '0'.
      MESSAGE E002 WITH P_FILE MSG.
      STOP.
   ELSE.

*-   First line in file is 'ACTUAL' or 'IFRS' depending on Ledger

   IF P_LEDGER = '0L'.                          "TR831
      TRANSFER GC_ACTUAL TO P_FILE.             "TR831
   ELSE.                                        "TR831
      TRANSFER GC_IFRS TO P_FILE.               "TR831
   ENDIF.                                       "TR831

*-   Second Line in file is 'Beginning Month'
     TRANSFER P_PERD TO P_FILE.      "<<<Ins. C11K918555 (Iss# TR495)

*-   Third Line in file is 'Ending Month'
     TRANSFER P_PERD TO P_FILE.      "<<<Ins. C11K918555 (Iss# TR495)

*-   Start of Mod. -> C11K918555 (Iss# TR495)
     PERFORM WRITE_DETAIL_LINES.

     CLOSE DATASET P_FILE.
   ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM PUT_SIGN_IN_FRONT
*----------------------------------------------------------------------*
FORM PUT_SIGN_IN_FRONT CHANGING VALUE.

DATA: TEXT1(1) TYPE C.

  SEARCH VALUE FOR '-'.
  IF SY-SUBRC = 0 AND SY-FDPOS <> 0.
    SPLIT VALUE AT '-' INTO VALUE TEXT1.
    CONDENSE VALUE.
    CONCATENATE '-' VALUE INTO VALUE.
  ELSE.
    CONDENSE VALUE.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM PRINT_REPORT
*----------------------------------------------------------------------*
FORM PRINT_REPORT.

DATA: TOT_AMOUNT LIKE SUBFAGL-AMOUNT.

  SORT SUBFAGL BY RBUKRS RACCT RTCUR HFMACCT.

   LOOP AT SUBFAGL.
     AT NEW RBUKRS.
        SELECT SINGLE * FROM ZJHDR
         WHERE BUKRS = SUBFAGL-RBUKRS.
         IF SY-SUBRC = 0.
            WRITE: /1 TEXT-116, ZJHDR-BUNIT.
            WRITE: /1 TEXT-DSH.
         ENDIF.
     ENDAT.

     WRITE: /  SUBFAGL-RBUKRS UNDER TEXT-104,
               8(16) SUBFAGL-AMOUNT,
               SUBFAGL-RACCT  UNDER TEXT-105,
               SUBFAGL-HFMACCT UNDER TEXT-127,
               SUBFAGL-AFFIL  UNDER TEXT-109,
               SUBFAGL-STATUS UNDER TEXT-115.

     TOT_AMOUNT = TOT_AMOUNT + SUBFAGL-AMOUNT.

     IF SUBFAGL-ERROR = 'Y'.
        DELETE SUBFAGL.
     ENDIF.

     AT END OF RBUKRS.
        ULINE.
        WRITE: /1 TEXT-120, 7(17) TOT_AMOUNT.
        CLEAR TOT_AMOUNT.
        ULINE.
     ENDAT.
   ENDLOOP.

*- Write error report section for missing Company Codes in ZJHDR
   SKIP 2.
   LOOP AT NOTFOUND.
      WRITE: /2 NOTFOUND-BUKRS, TEXT-118.
   ENDLOOP.
   ULINE.

*- Start of Ins. -> C11K918555 (Iss# TR495)
*- Write error report section for missing PS Account Rules in ZPSACCT
   SORT GT_NO_PSACCT.
   LOOP AT GT_NO_PSACCT.
      WRITE: /2 GT_NO_PSACCT, TEXT-119.
   ENDLOOP.
   ULINE.
*- End of Ins. -> C11K918555 (Iss# TR495)

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  WRITE_TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM WRITE_TOP_OF_PAGE.

  WRITE: / TEXT-RPT, SY-REPID, 42 TEXT-100,                 "Title
           90 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,
*           35 TEXT_COMN, 74 P_PERD, 76 '/', 77 P_YEAR,     "TR654
           35 TEXT_COMN, TEXT-PGE UNDER TEXT-DTE, SY-PAGNO. "TR654
  WRITE: /49 W_LEDGER.
  ULINE.
  WRITE: /1 TEXT-104, 8  TEXT-108, 28 TEXT-105, 38 TEXT-127,
         51 TEXT-109, 66 TEXT-115.
  ULINE.

ENDFORM.                    " WRITE_TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  BUILD_EXTRACT
*&---------------------------------------------------------------------*
FORM BUILD_EXTRACT.

*- Build Extract Records
   LOOP AT SUBFAGL.

     CHECK SUBFAGL-ERROR <> 'Y'.

*-   Build Extract Record
     PERFORM BUILD_DETAIL_RECORD.
   ENDLOOP.

ENDFORM.                    " BUILD_EXTRACT

*&---------------------------------------------------------------------*
*&      Form  GET_ZPSACCT_REC
*&---------------------------------------------------------------------*
FORM GET_ZPSACCT_REC USING P_ACCT.

   CLEAR ZPSACCT.
   SELECT SINGLE * FROM ZPSACCT INTO ZPSACCT
                      WHERE PSACT EQ P_ACCT.

   CHECK SY-SUBRC NE 0.

*- Report PeopleSoft Accounts with no mapping in ZPSACCT
   GT_NO_PSACCT = P_ACCT.
   COLLECT GT_NO_PSACCT.

*- When no ZPSACCT record found use PS Acct '0184007'
   SELECT SINGLE * FROM ZPSACCT INTO ZPSACCT
                      WHERE PSACT EQ GC_PS_DEFAULT.

   CHECK SY-SUBRC NE 0.
   GT_NO_PSACCT = GC_PS_DEFAULT.
   COLLECT GT_NO_PSACCT.

ENDFORM.                    " GET_ZPSACCT_REC

*&---------------------------------------------------------------------*
*&      Form  APPLY_YTD_BALANCE_RULE
*&---------------------------------------------------------------------*
FORM APPLY_YTD_BALANCE_RULE CHANGING P_DOLLARS.

*- Based on the entry in ZPSACCT for Peoplesoft Account#, perform
*- calculation on YTD Balance based on D_IC_ACCT to Flip the sign or not

   CHECK NOT ( P_DOLLARS IS INITIAL ).

   CASE ZPSACCT-D_IC_ACCT.

*---FLIP YTD Balace
*   'A' - Not IC Acct - Sign Flipped - Acct Bal * -1
*   'C' - Not IC Acct - Affiliate needed Sign Flipped - Acct Bal * -1
*   'E' - IC Acct     - Not Balancing - Sign Flipped - Acct Bal * -1
*   'G' - IC Acct     - Sign Flipped - Acct Bal * -1
    WHEN 'A' OR 'C' OR 'E' OR 'G'.
     P_DOLLARS = P_DOLLARS * -1.

*---LEAVE YTD Balace "as is"
*   'B' - Not IC Acct - Sign Not Flipped - no change
*   'D' - Not IC Acct - Affiliate needed Sign not Flipped - no change
*   'F' - IC Acct     - Not Balancing Sign not Flipped - no change
*   'H' - IC Acct     - Sign Not Flipped - no change
    WHEN 'B' OR 'D' OR 'F' OR 'H'.
*-     no action

*---SET YTD Balance to Zero as all records with zero balance are not
*extracted
*   'X' - Exclude from Hyperion Extract - Exclude Amounts from file
    WHEN 'X'.
       P_DOLLARS = 0.

   ENDCASE.

ENDFORM.                    " APPLY_YTD_BALANCE_RULE

*&---------------------------------------------------------------------*
*&      Form  WRITE_DETAIL_LINES
*&---------------------------------------------------------------------*
FORM WRITE_DETAIL_LINES.

DATA: L_ACCT_OUT(13) TYPE C,
      L_REC(100) TYPE C,
      L_AMT(18) TYPE C.

   LOOP AT DETAIL.

*-    Move YTD Balance to sign
      PERFORM PUT_SIGN_IN_FRONT CHANGING DETAIL-MONETARY_AMOUNT.

*-    Format the YTD Balance -00000000000000.00
      PERFORM FORMAT_AMOUNT USING DETAIL-MONETARY_AMOUNT
                         CHANGING L_AMT.

*-    Format Account.SubAccount field
      IF ( DETAIL-SUBACCOUNT IS INITIAL ).
         L_ACCT_OUT = DETAIL-ACCOUNT.
      ELSE.
         CONCATENATE DETAIL-ACCOUNT '.' DETAIL-SUBACCOUNT INTO
         L_ACCT_OUT.
      ENDIF.

      CLEAR L_REC.
      L_REC+0(5) = DETAIL-BUSINESS_UNIT_LN.
      L_REC+5(1) = ','.
      L_REC+6(13) = L_ACCT_OUT.
      L_REC+19(1) = ','.
      L_REC+20(19) = L_AMT.

*-    Write Detail record to file
      TRANSFER L_REC TO P_FILE.
   ENDLOOP.

ENDFORM.                    " WRITE_DETAIL_LINES

*&---------------------------------------------------------------------*
*&      Form  FORMAT_AMOUNT
*&---------------------------------------------------------------------*
FORM FORMAT_AMOUNT USING    P_AMT
                   CHANGING P_AMT_FORMATTED.

DATA: L_AMT(18) TYPE C.

    L_AMT = P_AMT.

    IF L_AMT CS '-'.
       REPLACE '-' WITH SPACE INTO L_AMT.
    ENDIF.

    SHIFT L_AMT RIGHT DELETING TRAILING SPACE.
    OVERLAY L_AMT WITH GC_ZEROS.

    IF P_AMT CS '-'.
       L_AMT+0(1) = '-'.
    ENDIF.

    P_AMT_FORMATTED = L_AMT.

ENDFORM.                    " FORMAT_AMOUNT

*&---------------------------------------------------------------------*
*&      Form  APPLY_PS_ACCT_RULES
*&---------------------------------------------------------------------*
FORM APPLY_PS_ACCT_RULES.

  LOOP AT SUBFAGL.

*-  Get the ZPSACCT record to determine PS rule based on PS Account
    PERFORM GET_ZPSACCT_REC USING SUBFAGL-HFMACCT.

*-  Perform rule calculation on YTD Balance
    PERFORM APPLY_YTD_BALANCE_RULE CHANGING SUBFAGL-AMOUNT.

*-  Remove Affiliate based on rule
    IF ( ZPSACCT-D_IC_ACCT EQ 'A' ) OR ( ZPSACCT-D_IC_ACCT EQ 'B' ).
       CLEAR SUBFAGL-AFFIL.
    ENDIF.

    IF SUBFAGL-AMOUNT EQ 0.
       DELETE SUBFAGL.
    ELSE.
       MODIFY SUBFAGL TRANSPORTING AMOUNT AFFIL.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " APPLY_PS_ACCT_RULES
