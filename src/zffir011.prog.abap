*&---------------------------------------------------------------------*
*& Report  ZFFIR011                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        August 2009.                                           *
*  Issue Log:   TR582 (IFRS)                                           *
*  Description:                                                        *
*     - The purpose of this program is to produce the following reports*
*       1- Deferral Accounts IFRS Adjustment                           *
*       2- Deferral Closing Balance Report                             *
*       It can also create BDC session for Adjustment posting.         *
*                                                                      *
*&---------------------------------------------------------------------*
REPORT ZFFIR011A MESSAGE-ID ZS.
TYPE-POOLS: SLIS.

TABLES: FAGLFLEXA,    "General Ledger: Actual Line Items
        FAGLFLEXT,    "General Ledger: Totals
        ZFDEF.        "IFRS Deferral Adjustment Mapping

DATA: BEGIN OF REPTAB1 OCCURS 0,          "for Adjustment Report
        RBUKRS   LIKE FAGLFLEXA-RBUKRS,         "Company Code
        RACCT    LIKE FAGLFLEXA-RACCT,          "Account Number
        DOCNR    LIKE FAGLFLEXA-DOCNR,          "Accounting Doc Number
        DRCRK    LIKE FAGLFLEXA-DRCRK,          "Debit/Credit Indicator
        HSL      LIKE FAGLFLEXA-HSL,            "Amt in local currency
     END OF REPTAB1.

DATA: BEGIN OF REPTAB2 OCCURS 0,          "for Closing Balance Report
        RBUKRS   LIKE FAGLFLEXT-RBUKRS,         "Company Code
        RACCT    LIKE FAGLFLEXT-RACCT,          "Account Number
        CLSBAL   LIKE FAGLFLEXT-TSLVT,          "Closing Balance
     END OF REPTAB2.

DATA: BEGIN OF REPTAB3 OCCURS 0,          "for BDC Session
        ACCTGRP  LIKE ZFDEF-ACCTGRP,            "Accounting Group
        RBUKRS   LIKE FAGLFLEXT-RBUKRS,         "Company Code
        ADJACCT  LIKE ZFDEF-ADJACCT,            "Adjustment Acct Number
        RACCT    LIKE FAGLFLEXT-RACCT,          "G/L Account Number
        DRCRK    LIKE FAGLFLEXT-DRCRK,          "Debit/Credit Indicator
        CLSBAL   LIKE FAGLFLEXT-TSLVT,          "Closing Balance
     END OF REPTAB3.

* Batch input data
DATA: BEGIN OF BDCDATA OCCURS 100.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: WRK_TOTAL LIKE FAGLFLEXT-HSLVT,
      W_HEAD01(60)     TYPE C,
      W_HEAD02(60)     TYPE C,
      BDC_SES_NAME(12) TYPE C.
CONSTANTS: TRUE    TYPE BOOLEAN VALUE 'X'.
************************************************************************
*********************  SELECTION SCREEN     ****************************
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETER: B_CBALN RADIOBUTTON GROUP BUTN DEFAULT 'X'
                               USER-COMMAND RAD, "Closing Balance Report
           B_ADJST RADIOBUTTON GROUP BUTN,    "Adjustment Report
           B_BDC   RADIOBUTTON GROUP BUTN.    "BDC Session
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME. " TITLE TEXT-001.
SELECT-OPTIONS: S_RBUKRS FOR FAGLFLEXT-RBUKRS OBLIGATORY,  "Company Code
                             S_BUDAT  FOR FAGLFLEXA-BUDAT. "Posting Date
PARAMETERS: P_RLDNR  LIKE FAGLFLEXT-RLDNR DEFAULT 'NL',    "Ledger
            P_RYEAR  LIKE FAGLFLEXT-RYEAR OBLIGATORY,      "Fiscal Year
            P_POPER  LIKE FAGLFLEXA-POPER OBLIGATORY,      "Fiscal Month
            P_BLDAT  LIKE FAGLFLEXA-BUDAT OBLIGATORY,   "Doc Date Single
            P_BUDAT  LIKE FAGLFLEXA-BUDAT OBLIGATORY, "Postg Date Single
            P_BLART(2) TYPE C DEFAULT 'NL',                "Doc Type
            P_RTCUR  LIKE FAGLFLEXA-RTCUR DEFAULT 'CAD',   "Currency
            P_REFER(16)  TYPE C,                           "Reference
            P_HDRTXT(25) TYPE C,                        "Doc Header Text
            P_ACTGRP LIKE ZFDEF-ACCTGRP OBLIGATORY.     "Account Group
SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN END OF BLOCK BOX1.

************************************************************************
***                   SELECTION-SCREEN OUTPUT                        ***
************************************************************************
AT SELECTION-SCREEN OUTPUT.
CASE TRUE.
     WHEN B_CBALN OR B_ADJST.
          LOOP AT SCREEN.
               IF SCREEN-NAME = 'P_BLDAT' OR SCREEN-NAME = 'P_BUDAT' OR
                  SCREEN-NAME = 'P_BLART' OR SCREEN-NAME = 'P_RTCUR' OR
                  SCREEN-NAME = 'P_REFER' OR SCREEN-NAME = 'P_HDRTXT' OR
                  SCREEN-NAME = 'P_ACTGRP'.
                  CLEAR: P_BLDAT, P_BUDAT, P_BLART, P_RTCUR,
                         P_REFER, P_HDRTXT, P_ACTGRP.
                  SCREEN-INPUT = '0'.
                  MODIFY SCREEN.
               ENDIF.
          ENDLOOP.
     WHEN B_BDC.
          LOOP AT SCREEN.
               IF SCREEN-NAME = 'S_BUDAT-LOW' OR
                  SCREEN-NAME = 'S_BUDAT-HIGH'.
                  SCREEN-INPUT = '0'.
                  MODIFY SCREEN.
               ENDIF.
               IF SCREEN-NAME = 'P_BLART'.
                  P_BLART = 'NL'.
                  MODIFY SCREEN.
               ENDIF.
               IF SCREEN-NAME = 'P_RTCUR'.
                  P_RTCUR = 'CAD'.
                  MODIFY SCREEN.
               ENDIF.
          ENDLOOP.
   ENDCASE.

LOOP AT SCREEN.
     IF SCREEN-NAME = 'P_RLDNR'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
     ENDIF.
ENDLOOP.

************************************************************************
*********************  START-OF-SELECTION    ***************************
************************************************************************
START-OF-SELECTION.

   IF B_CBALN = 'X'.
      PERFORM CLOSING_BALANCE_REPORT.           "Closing Balance Report
      PERFORM DISPLAY_ALV_GRID_DATA.
   ELSEIF B_ADJST = 'X'.
      PERFORM ADJUSTMENT_REPORT.                "Adjustment Report
      PERFORM DISPLAY_ALV_GRID_DATA.
   ELSE.
      PERFORM COLLECT_BDC_DATA.
      IF NOT REPTAB3[] IS INITIAL.
         PERFORM OPEN_BDC_SESSION.
         PERFORM CREATE_BATCH_INPUT.
         PERFORM CLOSE_BDC_SESSION.
      ELSE.
         MESSAGE E028.
      ENDIF.
   ENDIF.

************************************************************************
*********************  CLOSING_BALANCE_REPORT  *************************
************************************************************************
FORM CLOSING_BALANCE_REPORT.

   SELECT DEFACCT INTO ZFDEF-DEFACCT FROM ZFDEF.

   SELECT * FROM FAGLFLEXT
    WHERE  RYEAR  =  P_RYEAR
      AND  RBUKRS IN S_RBUKRS
      AND  RLDNR  =  P_RLDNR
      AND  RACCT  =  ZFDEF-DEFACCT.

    MOVE: FAGLFLEXT-RBUKRS  TO  REPTAB2-RBUKRS,
          FAGLFLEXT-RACCT   TO  REPTAB2-RACCT.
    ADD FAGLFLEXT-HSL01 FROM 1 TO P_POPER GIVING WRK_TOTAL.
    REPTAB2-CLSBAL = WRK_TOTAL + FAGLFLEXT-HSLVT.
    APPEND REPTAB2.
    CLEAR  REPTAB2.
   ENDSELECT.
   ENDSELECT.

ENDFORM.

************************************************************************
*********************  ADJUSTMENT_REPORT  ******************************
************************************************************************
FORM ADJUSTMENT_REPORT.
   SELECT DEFACCT INTO ZFDEF-DEFACCT FROM ZFDEF.

   SELECT * FROM FAGLFLEXA
    WHERE  RLDNR  =  P_RLDNR      "Index 1
      AND  RBUKRS IN S_RBUKRS
      AND  RACCT  =  ZFDEF-DEFACCT
      AND  BUDAT  IN S_BUDAT
      AND  RYEAR  =  P_RYEAR
      AND  POPER  =  P_POPER.

      MOVE: FAGLFLEXA-RBUKRS TO REPTAB1-RBUKRS,
            FAGLFLEXA-RACCT  TO REPTAB1-RACCT,
            FAGLFLEXA-DOCNR  TO REPTAB1-DOCNR,
            FAGLFLEXA-DRCRK  TO REPTAB1-DRCRK,
            FAGLFLEXA-HSL    TO REPTAB1-HSL.
      APPEND REPTAB1.
      CLEAR  REPTAB1.
   ENDSELECT.
   ENDSELECT.

ENDFORM.

************************************************************************
*********************  DISPLAY_ALV_GRID_DATA  **************************
************************************************************************
FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  VARIANT-REPORT = REPID.
  LAYOUT-GET_SELINFOS = 'X'.
  LAYOUT-ZEBRA = 'X'.

  REFRESH FIELDCAT.

 IF B_CBALN = 'X'.     "Closing Balance Report
 CONCATENATE TEXT-101 P_RYEAR '/' P_POPER+1(2) INTO W_HEAD01
             SEPARATED BY SPACE.
* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'REPTAB2'
       I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
LOOP AT FIELDCAT INTO FC_STR.

CASE FC_STR-FIELDNAME.
     WHEN 'RBUKRS'.
          FC_STR-SELTEXT_L = TEXT-C01.       "Alternative colheader
          FC_STR-DDICTXT = 'L'.
*          FC_STR-KEY    = ' '.                  Key columns -not first
     WHEN 'RACCT'.
          FC_STR-SELTEXT_L = TEXT-C02.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
*          FC_STR-KEY    = ' '.                 " Key columns -not first
     WHEN 'CLSBAL'.
          FC_STR-SELTEXT_L = TEXT-C03.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
*     WHEN OTHERS.
** fc_str-no_out = 'X'.           " hide column
ENDCASE.
*
MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
          IT_FIELDCAT  = FIELDCAT
          IS_LAYOUT    = LAYOUT
          I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = REPID
          I_SAVE       = 'A'
          IS_VARIANT   = variant
          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = REPTAB2
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ELSE.
 CONCATENATE TEXT-102 P_RYEAR '/' P_POPER+1(2) INTO W_HEAD01
             SEPARATED BY SPACE.
* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'REPTAB1'
       I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
LOOP AT FIELDCAT INTO FC_STR.
CASE FC_STR-FIELDNAME.
     WHEN 'RBUKRS'.
          FC_STR-SELTEXT_L = TEXT-C01.       "Alternative colheader
          FC_STR-DDICTXT = 'L'.
*          FC_STR-KEY    = ' '.                  Key columns -not first
     WHEN 'RACCT'.
          FC_STR-SELTEXT_L = TEXT-C02.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'DOCNR'.
          FC_STR-SELTEXT_L = TEXT-C05.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'DRCRK'.
          FC_STR-SELTEXT_L = TEXT-C07.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'HSL'.
          FC_STR-SELTEXT_L = TEXT-C06.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN OTHERS.
** fc_str-no_out = 'X'.           " hide column
ENDCASE.
*
MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
          IT_FIELDCAT  = FIELDCAT
          IS_LAYOUT    = LAYOUT
          I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = REPID
          I_SAVE       = 'A'
          IS_VARIANT   = variant
          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = REPTAB1
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.
ENDIF.

ENDFORM.
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
*  LS_LINE-INFO = SY-TITLE.             "sy-title.
  LS_LINE-INFO = w_head01.              "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
*  ls_line-typ  = 'S'.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.

ENDFORM.
************************************************************************
*********************  COLLECT BDC DATA       **************************
************************************************************************
FORM COLLECT_BDC_DATA.

   SELECT * FROM ZFDEF
    WHERE ACCTGRP = P_ACTGRP.

   SELECT * FROM FAGLFLEXT
    WHERE  RYEAR  =  P_RYEAR
      AND  RBUKRS IN S_RBUKRS
      AND  RLDNR  =  P_RLDNR
      AND  RACCT  =  ZFDEF-DEFACCT.

    MOVE: ZFDEF-ACCTGRP     TO  REPTAB3-ACCTGRP,
          FAGLFLEXT-RBUKRS  TO  REPTAB3-RBUKRS,
          FAGLFLEXT-DRCRK   TO  REPTAB3-DRCRK,
          FAGLFLEXT-RACCT   TO  REPTAB3-RACCT,
          ZFDEF-ADJACCT     TO  REPTAB3-ADJACCT.
    ADD FAGLFLEXT-HSL01 FROM 1 TO P_POPER GIVING WRK_TOTAL.
    REPTAB3-CLSBAL = WRK_TOTAL + FAGLFLEXT-HSLVT.
    APPEND REPTAB3.
    CLEAR  REPTAB3.
   ENDSELECT.
   ENDSELECT.
   SORT REPTAB3 BY ACCTGRP RBUKRS ADJACCT.
***TEMP CODE
*  WRITE: /1 'ACCTGRP', 'RBUKRS', 'RACCT', '   ADJACCT', '       DRCRK',
*   '          CLOSING BALANCE'.
*   LOOP AT REPTAB3.
*   WRITE: /1 REPTAB3-ACCTGRP, 9 REPTAB3-RBUKRS, 16 REPTAB3-RACCT,
*26 REPTAB3-ADJACCT, 42 REPTAB3-DRCRK, 64 REPTAB3-CLSBAL LEFT-JUSTIFIED.
*   ENDLOOP.
*   SKIP 2.
*   STOP.
ENDFORM.
************************************************************************
*********************CREATE_BDC_SESSION  *******************************
************************************************************************
FORM CREATE_BATCH_INPUT.

DATA:   OPEN_DOCUMENT(1).               "flag - trans in process

  REFRESH BDCDATA.
  CLEAR REPTAB3.

  LOOP AT REPTAB3.

       AT NEW RBUKRS.
          IF OPEN_DOCUMENT = ' '.
             PERFORM START_NEW_TRANSACTION.
             MOVE 'X' TO OPEN_DOCUMENT.
          ELSE.
             PERFORM CLOSE_TRANSACTION.
             PERFORM START_NEW_TRANSACTION.
          ENDIF.
       ENDAT.
*   Next line item
        PERFORM START_NEXT_LINEITEM.
  ENDLOOP.
  PERFORM CLOSE_TRANSACTION.
ENDFORM.

*======================================================================*
*                 FORM START_NEW_TRANSACTION                           *
* - This routine provides the BDC mapping for the initial screen in    *
* the transaction.                                                     *
*======================================================================*
FORM START_NEW_TRANSACTION.

* Header Data Screen (initial screen)
  PERFORM BDC_SCREEN USING 'SAPMF05A' '100'.
  PERFORM BDC_FIELD  USING 'BKPF-BLDAT' P_BLDAT.
  PERFORM BDC_FIELD  USING 'BKPF-BLART' P_BLART.
  PERFORM BDC_FIELD  USING 'BKPF-BUKRS' REPTAB3-RBUKRS.
  PERFORM BDC_FIELD  USING 'BKPF-BUDAT' P_BUDAT.
  PERFORM BDC_FIELD  USING 'BKPF-WAERS' P_RTCUR.
  PERFORM BDC_FIELD  USING 'BKPF-LDGRP' P_BLART.
  PERFORM BDC_FIELD  USING 'BKPF-XBLNR' P_REFER.
  PERFORM BDC_FIELD  USING 'BKPF-BKTXT' P_HDRTXT.

ENDFORM.
*======================================================================*
*                 FORM CLOSE_TRANSACTION                               *
* - This routine closes the current transaction.                       *
*======================================================================*
FORM CLOSE_TRANSACTION.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'BU'.
  PERFORM INSERT_SESSION.

ENDFORM.

*======================================================================*
*                   FORM START_NEXT_LINEITEM                           *
* - This routine enters the posting key and account for the next line  *
* item.  This was put in a seperate routine for clarity as only these  *
* 2-3 fields appear on the previous screen.  The rest of the line item *
* information appears on a subsequent screen in the transaction.       *
*======================================================================*
FORM START_NEXT_LINEITEM.
DATA: W_NEWBS_1(2)  TYPE C,
      W_NEWBS_2(2)  TYPE C,
      W_CLSBAL LIKE REPTAB3-CLSBAL,
      W_SGTXT(50)   TYPE C.
   IF REPTAB3-DRCRK = 'H'.
      MOVE 50 TO W_NEWBS_1.
      MOVE 40 TO W_NEWBS_2.
   ELSE.
      MOVE 40 TO W_NEWBS_1.
      MOVE 50 TO W_NEWBS_2.
   ENDIF.
*Amount should not be negative as it's represented by posting keys
   IF REPTAB3-CLSBAL < 0.
      W_CLSBAL = REPTAB3-CLSBAL * -1.
   ELSE.
      W_CLSBAL = REPTAB3-CLSBAL.
   ENDIF.

   CONCATENATE P_HDRTXT REPTAB3-ADJACCT INTO W_SGTXT SEPARATED BY SPACE.
   PERFORM BDC_FIELD  USING 'RF05A-NEWBS' W_NEWBS_1.
   PERFORM BDC_FIELD  USING 'RF05A-NEWKO' REPTAB3-ADJACCT.
   PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.
   PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
   PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  W_CLSBAL.
   PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  W_SGTXT.

  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/7'.    "More Data
* pop-up window "Coding block" appears - select F8 to continue
  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '330'.

*Offset entry
   PERFORM BDC_FIELD  USING 'RF05A-NEWBS' W_NEWBS_2.
   PERFORM BDC_FIELD  USING 'RF05A-NEWKO' REPTAB3-RACCT.
   PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.
   PERFORM BDC_SCREEN USING 'SAPMF05A' '300'.
   PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  W_CLSBAL.
   PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  W_SGTXT.
* select F14 - Document overview
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/7'.    "More Data
* pop-up window "Coding block" appears - select F8 to continue
  PERFORM BDC_SCREEN USING 'SAPLKACB' '002'.
  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.
  PERFORM BDC_SCREEN USING 'SAPMF05A' '330'.

ENDFORM.
*======================================================================*
*     FORM INSERT_SESSION                                              *
*======================================================================*
FORM INSERT_SESSION.

  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'FB01L'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4.
  IF SY-SUBRC <> 0.
    MESSAGE E013 WITH SY-SUBRC.
  ENDIF.

  REFRESH BDCDATA.                 "Refresh BDCData

ENDFORM.


************************************************************************
*********************  OPEN_BDC_SESSION  *******************************
************************************************************************
*This routine will call the BDC function to open up a new BDC session
FORM OPEN_BDC_SESSION.
DATA: W_ERROR_OPEN(35) TYPE C.

  CONCATENATE TEXT-111 P_ACTGRP INTO BDC_SES_NAME.

 CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        CLIENT            = SY-MANDT
        GROUP             = BDC_SES_NAME
*       HOLDDATE          =
        KEEP              = 'X'
        USER              = SY-UNAME
      EXCEPTIONS
        GROUP_INVALID     = 1
        GROUP_IS_LOCKED   = 2
        HOLDDATE_INVALID  = 3
        INTERNAL_ERROR    = 4
        QUEUE_ERRORID     = 5
        RUNNING           = 6
        SYSTEM_LOCK_ERROR = 7
        USER_INVALIDD     = 8.
 IF SY-SUBRC <> 0.
 MOVE SY-SUBRC TO W_ERROR_OPEN.
 CONCATENATE 'BDC OPEN GROUP ERROR. RC=' W_ERROR_OPEN INTO W_ERROR_OPEN.
    MESSAGE E019 WITH W_ERROR_OPEN.
    EXIT.
 ENDIF.
ENDFORM.

* This routine closes a batch input session. *
FORM CLOSE_BDC_SESSION.
* IF OPEN_DOCUMENT = 'X'.
*    PERFORM CLOSE_TRANSACTION.
*    CLEAR OPEN_DOCUMENT.
* ENDIF.
 CALL FUNCTION 'BDC_CLOSE_GROUP'.
 IF SY-SUBRC NE 0.
    MESSAGE E004 WITH BDC_SES_NAME.
 ELSE.
    MESSAGE I003 WITH BDC_SES_NAME.
 ENDIF.
 EXIT.
ENDFORM.
*======================================================================*
*                FORM BDC_SCREEN                                       *
*======================================================================*
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.
*======================================================================*
*               FORM BDC_FIELD                                         *
*======================================================================*
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  SHIFT BDCDATA-FVAL LEFT DELETING LEADING SPACE.
  APPEND BDCDATA.

ENDFORM.
