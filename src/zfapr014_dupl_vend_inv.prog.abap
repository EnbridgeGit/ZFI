REPORT ZFAPR014_DUPL_VEND_INV.
TYPE-POOLS: SLIS.
************************************************************************
*  Date:        October, 2013                                          *
*  Issue Log:   SDP55605                                               *
*  Developer:   Mohammad T. khan                                       *
*  Description:                                                        *
*     - This program is a clone of zfapr010 and major changes are done.*
*       The purpose of this program is to produce the listing of       *
*       duplicate invoices - either within a company code or           *
*       across several company codes.                                  *
*       Criteria for comparing duplicate has been made flexable and    *
*       more fields can be used for this purpose.                      *
*       Drill down has been provided from the report to use trans. FB03*
*       Reversal option would allow to compare amounts without sign.   *
*       ALV report will be displayed with Hotspot field Doc. Number.   *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*DD/MM/YY  TICKET  BY      Description                                 *
*25/04/14  67349   M.Khan  Add Dynamic SORT according to Variant Screen*
*                                                                      *
*08/07/14  71332   M.Khan  Add item line (BSEG-SGTXT) to the report.   *
************************************************************************
TABLES:  BKPF, "Document Header for posting date
         BSEG. "Accounting Document Segment

DATA:  BEGIN OF NEWITAB   OCCURS 0,
       XBLNR  LIKE BKPF-XBLNR,
       BLDAT  LIKE BKPF-BLDAT,
       BUKRS  LIKE BKPF-BUKRS,
       LIFNR  LIKE BSEG-LIFNR,
       WAERS  LIKE BKPF-WAERS,
       WRBTR  LIKE BSEG-WRBTR,
       BELNR  LIKE BKPF-BELNR,
       BLART  LIKE BKPF-BLART,
       BUDAT  LIKE BKPF-BUDAT,
       XREVERSAL LIKE BKPF-XREVERSAL,
       CPUDT  LIKE BKPF-CPUDT,
       GJAHR  LIKE BKPF-GJAHR,
       MONAT  LIKE BKPF-MONAT,
       USNAM  LIKE BKPF-USNAM,
       TCODE  LIKE BKPF-TCODE,
       STBLG  LIKE BKPF-STBLG,
       STGRD  LIKE BKPF-STGRD,
       AUGCP  LIKE BSEG-AUGCP,
       AUGBL  LIKE BSEG-AUGBL,
       BSCHL  LIKE BSEG-BSCHL,
       SHKZG  LIKE BSEG-SHKZG,
       DMBTR  LIKE BSEG-DMBTR,
       MWSKZ  LIKE BSEG-MWSKZ,
       REBZG  LIKE BSEG-REBZG,
       REBZJ  LIKE BSEG-REBZJ,
       ZLSPR  LIKE BSEG-ZLSPR,
       SGTXT  LIKE BSEG-SGTXT,       "Ticket 71332
       WRBTR_POSITIVE LIKE BSEG-WRBTR,
       COUNT TYPE I.
DATA:  END OF NEWITAB.

DATA: BEGIN OF DUP_BSIP  OCCURS 0,
      LIFNR LIKE BSEG-LIFNR,
      XBLNR LIKE BKPF-XBLNR,
      WRBTR(12)  TYPE C,
      WRBTR_POSITIVE LIKE BSEG-WRBTR,
      BLDAT LIKE BKPF-BLDAT,
      BUKRS LIKE BKPF-BUKRS,
      COUNT LIKE NEWITAB-COUNT.
DATA:  END OF DUP_BSIP.

DATA: BEGIN OF WACOND  OCCURS 0,
      CONDITION(75) TYPE C,
      END OF WACOND.

DATA: BEGIN OF TBL_KEEP OCCURS 0,
        XBLNR   LIKE BKPF-XBLNR,
      END OF TBL_KEEP.

DATA: NEWDTIND LIKE SY-DATUM.
DATA: LIFNR_IND  TYPE C,
      XBLNR_IND  TYPE C,
      WRBTR_IND  TYPE C,
      BLDAT_IND  TYPE C,
      BUKRS_IND  TYPE C,
      ALL_IND    TYPE C,
      MATCH_IND  TYPE C.

DATA: W_HEAD02(60)  TYPE C,
      ES_VARIANT    LIKE DISVARIANT,
      IS_VARIANT    LIKE DISVARIANT.

*----------------------SCREEN ----------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
         S_BUKRS FOR BKPF-BUKRS,                    "Company Code
         S_GJAHR FOR BKPF-GJAHR DEFAULT SY-DATUM(4) OBLIGATORY,
*                                                   "Fiscal year
         S_BUDAT FOR BKPF-BLDAT,                    "Posting date
         S_CPUDT FOR BKPF-CPUDT,                    "Entry date
         S_LIFNR FOR BSEG-LIFNR,                    "Vendor
         S_XBLNR FOR BKPF-XBLNR,                    "Reference Numer
         S_BELNR FOR BKPF-BELNR,                    "Document Number
         S_BLART FOR BKPF-BLART.
PARAMETER: P_DAYS(3) TYPE C DEFAULT 10.             "Number of Days
SELECTION-SCREEN SKIP 1.
PARAMETER: REVRS_CH AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETERS:
  WRBTR_CH    AS CHECKBOX DEFAULT 'X',
  LIFNR_CH    AS CHECKBOX DEFAULT 'X',
  XBLNR_CH    AS CHECKBOX DEFAULT 'X',
  BLDAT_CH    AS CHECKBOX,
  BUKRS_CH    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-005.
PARAMETERS PVARIANT LIKE DISVARIANT-VARIANT.           "Display Variant
SELECTION-SCREEN END OF BLOCK BOX3.
*----------------------end SCREEN ------------------------------------*


***********************************************************************
*                  INITIALIZATION                                     *
***********************************************************************
INITIALIZATION.

  REFRESH S_BUDAT.
  DATA: STARTDT LIKE SY-DATUM,
        ENDDT   LIKE SY-DATUM.
  REFRESH S_BUDAT.
  COMPUTE STARTDT = SY-DATUM - 180.
  MOVE 'IBT'       TO S_BUDAT+0(3).
  MOVE STARTDT     TO S_BUDAT+3(8).
  MOVE SY-DATUM    TO S_BUDAT+11(8).
  APPEND S_BUDAT.

  REFRESH S_GJAHR.
  MOVE 'IBT'         TO S_GJAHR+0(3).
  MOVE S_BUDAT+3(4)  TO S_GJAHR+3(4).
  MOVE S_BUDAT+11(4) TO S_GJAHR+7(4).
  APPEND S_GJAHR.

***********************************************************************
*                  SELECT DISPLAY VARIANT                             *
***********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZLMMR002_GASSUPPLY_INTRF_RECON'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = IS_VARIANT
      I_SAVE        = 'A'
    IMPORTING
      ES_VARIANT    = ES_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.
  PVARIANT = ES_VARIANT-VARIANT.

***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
START-OF-SELECTION.

  PERFORM CHECK_DUP_COMPARE_SELECTION.
  COMPUTE NEWDTIND = SY-DATUM - P_DAYS.

* Because FISCAL YEAR is required, it needs to go first.
* Any following condition will have  AND at the beginning of the
* condition.

  MOVE 'GJAHR in s_gjahr' TO WACOND-CONDITION.
  APPEND WACOND.

* These are optional parameters.
  IF S_BUKRS <> ' '.
    MOVE 'and BUKRS in s_bukrs' TO WACOND-CONDITION.
    APPEND WACOND.
  ENDIF.

  IF S_BELNR <> ' '.
    MOVE 'and BELNR in s_belnr' TO WACOND-CONDITION.
    APPEND WACOND.
  ENDIF.

  IF S_BLART <> ' '.
    MOVE 'and BLART in s_blart' TO WACOND-CONDITION.
    APPEND WACOND.
  ENDIF.

  IF S_BUDAT <> ' '.
    MOVE 'and BUDAT in s_budat' TO WACOND-CONDITION.
    APPEND WACOND.
  ENDIF.

  IF S_CPUDT <> ' '.
    MOVE 'and CPUDT in s_cpudt' TO WACOND-CONDITION.
    APPEND WACOND.
  ENDIF.

  IF S_XBLNR <> ' '.
    MOVE 'and XBLNR in s_xblnr' TO WACOND-CONDITION.
    APPEND WACOND.
  ENDIF.

*-------------------------------------------------------------
*  Select all documents posted based on the parameters in variant
*-------------------------------------------------------------
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT   = 'Selecting BKPF & BSEG records'
    EXCEPTIONS
      OTHERS = 1.

  SELECT XBLNR BLDAT BUKRS WAERS BELNR BLART BUDAT XREVERSAL CPUDT
         GJAHR MONAT USNAM TCODE STBLG STGRD
  INTO (NEWITAB-XBLNR, NEWITAB-BLDAT, NEWITAB-BUKRS, NEWITAB-WAERS,
       NEWITAB-BELNR, NEWITAB-BLART, NEWITAB-BUDAT, NEWITAB-XREVERSAL,
       NEWITAB-CPUDT, NEWITAB-GJAHR, NEWITAB-MONAT, NEWITAB-USNAM,
       NEWITAB-TCODE, NEWITAB-STBLG, NEWITAB-STGRD)
   FROM BKPF
   WHERE (WACOND).

    SELECT SINGLE * FROM BSEG
       WHERE BUKRS = NEWITAB-BUKRS
         AND BELNR = NEWITAB-BELNR
         AND GJAHR = NEWITAB-GJAHR
         AND LIFNR <> SPACE
         AND LIFNR IN S_LIFNR.

    IF SY-SUBRC = '0'.
      MOVE BSEG-LIFNR TO NEWITAB-LIFNR.
      MOVE BSEG-WRBTR TO NEWITAB-WRBTR.
      MOVE BSEG-WRBTR TO NEWITAB-WRBTR_POSITIVE.
      MOVE BSEG-SGTXT TO NEWITAB-SGTXT.   "Ticket 71332
      IF BSEG-SHKZG = 'H'.
        COMPUTE NEWITAB-WRBTR = NEWITAB-WRBTR * -1.
      ENDIF.
      IF BSEG-SHKZG = 'H' AND REVRS_CH = 'X'.
      ELSEIF BSEG-SHKZG = 'H'.
        COMPUTE NEWITAB-WRBTR_POSITIVE = NEWITAB-WRBTR_POSITIVE * -1.
      ENDIF.
      MOVE 1 TO NEWITAB-COUNT.
      MOVE: BSEG-AUGCP TO NEWITAB-AUGCP,
           BSEG-AUGBL TO NEWITAB-AUGBL,
           BSEG-BSCHL TO NEWITAB-BSCHL,
           BSEG-SHKZG TO NEWITAB-SHKZG,
           BSEG-DMBTR TO NEWITAB-DMBTR,
           BSEG-MWSKZ TO NEWITAB-MWSKZ,
           BSEG-REBZG TO NEWITAB-REBZG,
           BSEG-REBZJ TO NEWITAB-REBZJ,
           BSEG-ZLSPR TO NEWITAB-ZLSPR.
      APPEND NEWITAB.
    ENDIF.
  ENDSELECT.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT   = 'Creating report ZFAPR010'
    EXCEPTIONS
      OTHERS = 1.

*Start of Dynamic Sort depending upon variant screen    Ticket 67349
* SORT NEWITAB BY LIFNR XBLNR WRBTR_POSITIVE. "Ticket 67349
DATA: LT_SORT TYPE ABAP_SORTORDER_TAB,
        LS_SORT LIKE LINE OF LT_SORT.
  IF BUKRS_CH = 'X'.
     LS_SORT-NAME = 'BUKRS'.
     APPEND LS_SORT TO LT_SORT.
  ENDIF.
  IF LIFNR_CH = 'X'.
     LS_SORT-NAME = 'LIFNR'.
     APPEND LS_SORT TO LT_SORT.
  ENDIF.
  IF XBLNR_CH = 'X'.
     LS_SORT-NAME = 'XBLNR'.
     APPEND LS_SORT TO LT_SORT.
  ENDIF.
  IF BLDAT_CH = 'X'.
     LS_SORT-NAME = 'BLDAT'.
     APPEND LS_SORT TO LT_SORT.
  ENDIF.
  IF WRBTR_CH = 'X'.
     LS_SORT-NAME = 'WRBTR_POSITIVE'.
     APPEND LS_SORT TO LT_SORT.
  ENDIF.

  SORT NEWITAB BY (LT_SORT).
*End of Dynamic Sort depending upon variant screen    Ticket 67349

  LOOP AT NEWITAB.
    IF SY-TABIX = 1.
      MOVE-CORRESPONDING NEWITAB TO DUP_BSIP.
      MOVE NEWITAB-WRBTR_POSITIVE TO DUP_BSIP-WRBTR_POSITIVE.
      CONTINUE.
    ENDIF.
    IF LIFNR_CH = 'X'.
      IF NEWITAB-LIFNR <> DUP_BSIP-LIFNR.
        MOVE 'X' TO LIFNR_IND.
      ELSE.
        CLEAR LIFNR_IND.
      ENDIF.
    ENDIF.

    IF XBLNR_CH = 'X'.
      IF NEWITAB-XBLNR <> DUP_BSIP-XBLNR.
        MOVE 'X' TO XBLNR_IND.
      ELSE.
        CLEAR XBLNR_IND.
      ENDIF.
    ENDIF.

    IF WRBTR_CH = 'X'.
      IF NEWITAB-WRBTR_POSITIVE <> DUP_BSIP-WRBTR_POSITIVE.
        MOVE 'X' TO WRBTR_IND.
      ELSE.
        CLEAR WRBTR_IND.
      ENDIF.
    ENDIF.

    IF BLDAT_CH = 'X'.
      IF NEWITAB-BLDAT <> DUP_BSIP-BLDAT.
        MOVE 'X' TO BLDAT_IND.
      ELSE.
        CLEAR BLDAT_IND.
      ENDIF.
    ENDIF.

    IF BUKRS_CH = 'X'.
      IF NEWITAB-BUKRS <> DUP_BSIP-BUKRS.
        MOVE 'X' TO BUKRS_IND.
      ELSE.
        CLEAR BUKRS_IND.
      ENDIF.
    ENDIF.

    IF LIFNR_IND = 'X' OR XBLNR_IND = 'X' OR WRBTR_IND = 'X'
                       OR BLDAT_IND = 'X' OR BUKRS_IND = 'X'.

      IF DUP_BSIP-COUNT > 1.                       "save the duplicate
        APPEND DUP_BSIP.
      ENDIF.
      MOVE-CORRESPONDING NEWITAB TO DUP_BSIP.
      MOVE NEWITAB-WRBTR_POSITIVE TO DUP_BSIP-WRBTR_POSITIVE.
    ELSE.                              " dupl can be credits or debits
      DUP_BSIP-COUNT = DUP_BSIP-COUNT + 1.
    ENDIF.
  ENDLOOP.
  IF DUP_BSIP-COUNT > 1.                       "save the duplicate
    APPEND DUP_BSIP.
  ENDIF.

** This creates a table of all records that have duplicate
** informatation based on the variant.
* At the end of this routine, only records to be reported are left
* in NEWITAB

  LOOP AT NEWITAB.
    LOOP AT DUP_BSIP.
      MOVE 'Y' TO MATCH_IND.
      IF LIFNR_CH = 'X'.
        IF DUP_BSIP-LIFNR = NEWITAB-LIFNR.
          MOVE 'X' TO LIFNR_IND.
          MOVE 'Y' TO MATCH_IND.
        ELSE.
          CLEAR LIFNR_IND.
          MOVE 'N' TO MATCH_IND.
        ENDIF.
      ENDIF.

      IF XBLNR_CH = 'X' AND MATCH_IND = 'Y'.
        IF DUP_BSIP-XBLNR = NEWITAB-XBLNR.
          MOVE 'X' TO XBLNR_IND.
          MOVE 'Y' TO MATCH_IND.
        ELSE.
          CLEAR XBLNR_IND.
          MOVE 'N' TO MATCH_IND.
        ENDIF.
      ENDIF.

      IF WRBTR_CH = 'X' AND MATCH_IND = 'Y'.
        IF DUP_BSIP-WRBTR_POSITIVE = NEWITAB-WRBTR_POSITIVE.
          MOVE 'X' TO WRBTR_IND.
          MOVE 'Y' TO MATCH_IND.
        ELSE.
          CLEAR WRBTR_IND.
          MOVE 'N' TO MATCH_IND.
        ENDIF.
      ENDIF.

      IF BLDAT_CH = 'X' AND MATCH_IND = 'Y'.
        IF DUP_BSIP-BLDAT = NEWITAB-BLDAT.
          MOVE 'X' TO BLDAT_IND.
          MOVE 'Y' TO MATCH_IND.
        ELSE.
          CLEAR BLDAT_IND.
          MOVE 'N' TO MATCH_IND.
        ENDIF.
      ENDIF.

      IF BUKRS_CH = 'X' AND MATCH_IND = 'Y'.
        IF DUP_BSIP-BUKRS = NEWITAB-BUKRS.
          MOVE 'X' TO BUKRS_IND.
          MOVE 'Y' TO MATCH_IND.
        ELSE.
          CLEAR BUKRS_IND.
          MOVE 'N' TO MATCH_IND.
        ENDIF.
      ENDIF.

      IF MATCH_IND = 'Y'.
        ALL_IND = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF ALL_IND = 'X'.
      CLEAR ALL_IND.
    ELSE.
      DELETE NEWITAB.   "TABLE FOR ALV
    ENDIF.
  ENDLOOP.

PERFORM DROP_DOCS_OUTSIDE_LAST_DAYS.      "Ticket #67349

*  SORT NEWITAB BY LIFNR BLDAT XBLNR.      "Ticket #67349
  SORT NEWITAB BY (LT_SORT).               "Ticket #67349
  PERFORM DISPLAY_ALV_GRID_DATA.

*&---------------------------------------------------------------------*
*&      Form  DROP_DOCS_OUTSIDE_LAST_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DROP_DOCS_OUTSIDE_LAST_DAYS .
DATA: BEGIN OF COMPSTR OCCURS 0,
          XBLNR LIKE NEWITAB-XBLNR,
          BLDAT LIKE NEWITAB-BLDAT,
          BUKRS LIKE NEWITAB-BUKRS,
          LIFNR LIKE NEWITAB-LIFNR,
          WRBTR_POSITIVE LIKE NEWITAB-WRBTR_POSITIVE.
DATA: END OF COMPSTR.
DATA: DEL_REC TYPE C,
      DEL_PRV TYPE C.

  LS_SORT-NAME = 'CPUDT'.
  LS_SORT-DESCENDING = 'X'.
  APPEND LS_SORT TO LT_SORT.
  SORT NEWITAB BY (LT_SORT).

  LOOP AT NEWITAB.
     IF NEWITAB-CPUDT < NEWDTIND.
        MOVE 'Y' TO DEL_REC.
        IF BUKRS_CH = 'X'.
           IF NEWITAB-BUKRS = COMPSTR-BUKRS.
              MOVE 'N' TO DEL_REC.
           ELSE.
              MOVE 'Y' TO DEL_REC.
           ENDIF.
        ENDIF.

        IF LIFNR_CH = 'X'.
           IF NEWITAB-LIFNR = COMPSTR-LIFNR.
              MOVE 'N' TO DEL_REC.
           ELSE.
              MOVE 'Y' TO DEL_REC.
           ENDIF.
        ENDIF.

        IF BLDAT_CH = 'X'.
           IF NEWITAB-BLDAT = COMPSTR-BLDAT.
              MOVE 'N' TO DEL_REC.
           ELSE.
              MOVE 'Y' TO DEL_REC.
           ENDIF.
        ENDIF.

        IF WRBTR_CH = 'X'.
           IF NEWITAB-WRBTR_POSITIVE = COMPSTR-WRBTR_POSITIVE.
              MOVE 'N' TO DEL_REC.
           ELSE.
              MOVE 'Y' TO DEL_REC.
           ENDIF.
        ENDIF.

        IF BUKRS_CH = 'X'.
           IF NEWITAB-BUKRS = COMPSTR-BUKRS.
              MOVE 'N' TO DEL_REC.
           ELSE.
              MOVE 'Y' TO DEL_REC.
           ENDIF.
        ENDIF.

      IF DEL_REC = 'Y'.
         DELETE NEWITAB.
         DEL_PRV = 'Y'.
      ELSEIF DEL_REC = 'N' AND DEL_PRV = 'Y'.
         DELETE NEWITAB.
         DEL_PRV = 'Y'.
      ELSE.
         CLEAR DEL_PRV.
      ENDIF.
    ELSE.
      DEL_PRV = 'N'.
    ENDIF.
     MOVE-CORRESPONDING NEWITAB TO COMPSTR.
  ENDLOOP.

*  CLEAR TBL_KEEP.
*  REFRESH TBL_KEEP.
*
** identify the documents inside the last days range
*  LOOP AT NEWITAB.
*    IF NEWITAB-CPUDT >= NEWDTIND.
*      TBL_KEEP-XBLNR = NEWITAB-XBLNR.
**      TBL_KEEP-BUDAT = NEWITAB-BUDAT.
*      APPEND TBL_KEEP.
*    ENDIF.
*
*  ENDLOOP.
*
*  SORT TBL_KEEP.
*  DELETE ADJACENT DUPLICATES FROM TBL_KEEP.
*
*  LOOP AT NEWITAB.
*    READ TABLE TBL_KEEP WITH KEY XBLNR = NEWITAB-XBLNR
**                                BUDAT = NEWITAB-BUDAT
*                                 BINARY SEARCH.
*    IF SY-SUBRC <> 0.
*      DELETE NEWITAB.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " DROP_DOCS_OUTSIDE_LAST_DAYS


***********************************************************************
*                  CHECK_DUP_COMPARE_SELECTION                        *
***********************************************************************
FORM CHECK_DUP_COMPARE_SELECTION.
  DATA: W_TEXT1(65),
        W_TEXT2(65),
        W_POPUP,
        W_DUP,
        W_COUNT TYPE I.
  CLEAR: W_POPUP.    ", w_count.
  IF REVRS_CH = 'X' AND WRBTR_CH NE 'X'.
    MOVE TEXT-008 TO W_TEXT1.
    MOVE SPACE TO W_TEXT2.
    MOVE 'X' TO W_POPUP.
  ENDIF.

  IF WRBTR_CH = 'X'.
    ADD 1 TO W_COUNT.
  ENDIF.
  IF XBLNR_CH = 'X'.
    ADD 1 TO W_COUNT.
  ENDIF.
  IF BLDAT_CH = 'X'.
    ADD 1 TO W_COUNT.
  ENDIF.
  IF LIFNR_CH = 'X'.
    ADD 1 TO W_COUNT.
  ENDIF.
  IF W_COUNT < 2.
    MOVE TEXT-006 TO W_TEXT1.
    MOVE TEXT-007 TO W_TEXT2.
    MOVE 'X' TO W_POPUP.
  ENDIF.

  IF W_POPUP = 'X'.
    PERFORM POPUP_SCREEN01 USING W_TEXT1 W_TEXT2.
  ENDIF.

  IF WRBTR_CH = 'X' OR LIFNR_CH = 'X'.
  ELSE.
    MOVE TEXT-010 TO W_TEXT1.
    PERFORM POPUP_SCREEN02 USING W_TEXT1.
  ENDIF.


ENDFORM.                    "check_dup_compare_selection

*
FORM POPUP_SCREEN01 USING W_TEXT1 W_TEXT2.
  CALL FUNCTION 'POPUP_FOR_INTERACTION'
    EXPORTING
      HEADLINE = '!! ERROR !!'
      TEXT1    = W_TEXT1
      TEXT2    = W_TEXT2
      TEXT3    = ' '
      TEXT4    = 'Press OK Button to Continue'
      BUTTON_1 = 'OK'.
  STOP.
ENDFORM.                    "check_dup_compare_selection

*&---------------------------------------------------------------------*
*&      Form  POPUP_SCREEN02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->W_TEXT1    text
*----------------------------------------------------------------------*
FORM POPUP_SCREEN02 USING W_TEXT1.
  DATA: W_ANSWER(1).

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
     TITLEBAR                    = 'WARNING'
*   DIAGNOSE_OBJECT             = ' '
      TEXT_QUESTION               = W_TEXT1  "W_QUEST
      TEXT_BUTTON_1               = 'Continue'
      ICON_BUTTON_1               = 'ICON_CHECKED'
      TEXT_BUTTON_2               = 'Cancel'
      ICON_BUTTON_2               = 'ICON_CANCEL'
*   DEFAULT_BUTTON              = '1'
      DISPLAY_CANCEL_BUTTON       = ''
*   USERDEFINED_F1_HELP         = ' '
*   START_COLUMN                = 25
*   START_ROW                   = 6
*   POPUP_TYPE                  =
*   IV_QUICKINFO_BUTTON_1       = ' '
*   IV_QUICKINFO_BUTTON_2       = ' '
    IMPORTING
      ANSWER                      = W_ANSWER
* TABLES
*   PARAMETER                   =
* EXCEPTIONS
*   TEXT_NOT_FOUND              = 1
*   OTHERS                      = 2
    .
  IF W_ANSWER = 2.
    STOP.
  ENDIF.

ENDFORM.                    "POPUP_SCREEN02

***********************************************************************
*                        DISPLAY_ALV_GRID_DATA                        *
***********************************************************************
FORM DISPLAY_ALV_GRID_DATA.

  DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
        FC_STR   TYPE SLIS_FIELDCAT_ALV,
        LAYOUT   TYPE SLIS_LAYOUT_ALV,
        TITLE    TYPE LVC_TITLE,
        REPID    LIKE SY-REPID,
        VARIANT  LIKE DISVARIANT,
        SORT     TYPE SLIS_T_SORTINFO_ALV,
        SORT_STR TYPE SLIS_SORTINFO_ALV.

  DATA: IT_EVENTEXIT TYPE SLIS_T_EVENT_EXIT,
        WA_EVENTEXIT TYPE SLIS_EVENT_EXIT.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-COUNTFNAME = 'BELNR'.
  LAYOUT-ZEBRA = 'X'.
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.
  CLEAR:  FIELDCAT, FC_STR.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = REPID
      I_INTERNAL_TABNAME     = 'NEWITAB'
      I_INCLNAME             = REPID
      I_BYPASSING_BUFFER     = 'X'
    CHANGING
      CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
DEFINE HIDE_COLUMN.
  LOOP AT FIELDCAT INTO FC_STR.
    CASE FC_STR-FIELDNAME.
      WHEN &1.
       IF FC_STR-FIELDNAME = 'BELNR'.
          FC_STR-HOTSPOT = 'X'.
       ENDIF.
       IF FC_STR-FIELDNAME <> 'BUKRS' AND FC_STR-FIELDNAME <> 'BELNR'
                                      AND FC_STR-FIELDNAME <> 'SGTXT'.
          FC_STR-NO_OUT = 'X'.            "Hide Columns
       ENDIF.
       IF FC_STR-FIELDNAME = 'SGTXT'.     "Ticket 71332
          FC_STR-SELTEXT_L = TEXT-C01.    "Ticket 71332 Alt col header
          FC_STR-DDICTXT = 'L'.           "Ticket 71332
       ENDIF.                             "Ticket 71332
       FC_STR-KEY    = ' '.               "Key columns-not first
      ENDCASE.
      MODIFY FIELDCAT FROM FC_STR.
    ENDLOOP.
  END-OF-DEFINITION.

  HIDE_COLUMN:
  'BUKRS', 'GJAHR', 'MONAT', 'USNAM', 'TCODE', 'STBLG', 'STGRD',
  'AUGCP', 'AUGBL', 'BSCHL', 'SHKZG', 'DMBTR', 'MWSKZ', 'BELNR',
  'WRBTR_POSITIVE', 'COUNT', 'REBZG', 'REBZJ', 'ZLSPR', 'SGTXT'.

*--- ALV List Display
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = REPID    "G_PROGNAME
*     I_GRID_TITLE            = V_GRID_TITLE
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IS_LAYOUT               = LAYOUT
      IT_EVENT_EXIT           = IT_EVENTEXIT
      I_CALLBACK_TOP_OF_PAGE  = 'ALV_TOP_OF_PAGE'
      IT_FIELDCAT             = FIELDCAT
      I_DEFAULT               = 'X'
      I_SAVE                  = 'A'
      IS_VARIANT              = VARIANT
    TABLES
      T_OUTTAB                = NEWITAB
    EXCEPTIONS
      PROGRAM_ERROR           = 1
      OTHERS                  = 2.
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "DISPLAY_ALV_GRID_DATA
*SUBROUTINE TO REFRESH ALV
*FORM event_exits.
*     CLEAR WA_EVENTEXIT.
*     WA_EVENTEXIT-UCOMM = '&REFRESH'.
*     WA_EVENTEXIT-AFTER = 'X'.
*     APPEND WA_EVENTEXIT TO IT_EVENTEXIT.
*ENDFORM.                    "EVENT_EXITS

***************************************************************
*& FORM USER_COMMAND
***********************************************************************
*USER ACTIONS HANDELING ON ALV
FORM USER_COMMAND USING R_UCOMM TYPE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE R_UCOMM.
    WHEN '&IC1'.
      IF RS_SELFIELD-FIELDNAME = 'BELNR'.
        READ TABLE NEWITAB INDEX RS_SELFIELD-TABINDEX.
        IF SY-SUBRC = 0.
          SET PARAMETER ID 'BLN' FIELD NEWITAB-BELNR.
          SET PARAMETER ID 'BUK' FIELD NEWITAB-BUKRS.
          SET PARAMETER ID 'GJR' FIELD NEWITAB-GJAHR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.                    "USER_COMMAND

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- Heading Line: Type H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- Action Line:  Type S
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'S'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.
ENDFORM.                    "ALV_TOP_OF_PAGE
