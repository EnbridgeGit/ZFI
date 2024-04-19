REPORT ZFGLR022B.

TYPE-POOLS: SLIS.

************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        JANUARY, 2005.                                         *
*  Issue Log:   1369 W and 1090 E                                      *
*  Description:                                                        *
*     - The purpose of this program is to produce the Trial balance    *
*       report by year or by year and period.                          *
*                                                                      *
************************************************************************
*CHANGES****************************************************************
* 2010/08/20 M.Khan TR854  Copy program ZFGLR022 and make IFRS changes *
*                          to add Ledger field in selection screen,    *
*                          DB data selection and report heading. Also  *
*                          replace table GLT0 with FAGLFLEXT
* 2006/11/24 mkhan TR344 Provide the period range as From Period & To  *
*                        Period for "Trial balance for period" option. *
* 2005/08/29 mkhan TR105 Select Actual amount only as PLAN amount      *
*                     is going to be introduced in FI.                 *
*----------------------------------------------------------------------*

*TABLES: GLT0, SKAT, BSIS, "NOTE: Table BSIS is used for reference only
TABLES: FAGLFLEXT, SKAT, BSIS,                               "TR854
        AUFK, SETHIER.    "TR569

DATA: BEGIN OF RPTTAB OCCURS 0,
      BUKRS LIKE FAGLFLEXT-RBUKRS,
      RACCT LIKE FAGLFLEXT-RACCT,
      TXT50 LIKE SKAT-TXT50,
      HSLV2 LIKE FAGLFLEXT-HSLVT,
      HSLV1 LIKE FAGLFLEXT-HSLVT,
      DIFFR LIKE FAGLFLEXT-HSLVT,
      END OF RPTTAB.

DATA:
   W_HEAD01(60)  TYPE C,
   W_HEAD02(60)  TYPE C,
   W_HEAD03(20)  TYPE C,                                         "TR854
   W_KTOPL       LIKE SKAT-KTOPL VALUE 'COAT'.

CONSTANTS: TRUE    TYPE BOOLEAN VALUE 'X'.

*----------------------  Selection Screen  -----------------------------
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
PARAMETER: B_SRPRT RADIOBUTTON GROUP BUTN USER-COMMAND RAD, "SumryReport
           B_DRPRT RADIOBUTTON GROUP BUTN.                "Detail Report
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-000.
PARAMETERS:
       P_FYEAR  LIKE FAGLFLEXT-RYEAR DEFAULT SY-DATUM+0(4) OBLIGATORY.
SELECT-OPTIONS:
       S_BUKRS  FOR  FAGLFLEXT-RBUKRS,
       S_AUFNR  FOR  AUFK-AUFNR NO-DISPLAY,                     "TR569
       S_CCGRP  FOR  SETHIER-SHORTNAME NO INTERVALS NO-DISPLAY, "TR569
       S_KOSTL  FOR  AUFK-KOSTL NO-DISPLAY,                     "TR569
       S_RACCT  FOR  FAGLFLEXT-RACCT.
PARAMETERS:
       F_PERIO(2)  TYPE N,
       T_PERIO(2)  TYPE N,
       P_RLDNR     LIKE FAGLFLEXT-RLDNR DEFAULT '0L'.           "TR854
*       S_PERIO  FOR  BSIS-MONAT.
SELECTION-SCREEN END OF BLOCK BOX2.

*SELECTION-SCREEN SKIP.               "TR344
*
*PARAMETERS:
*       P_PERIO(2) TYPE N.

SELECTION-SCREEN END OF BLOCK BOX.


*------------------------ INITIALIZATION.   ------------------------
INITIALIZATION.

MOVE TRUE TO B_SRPRT.
************************************************************************
***                   SELECTION-SCREEN OUTPUT                        ***
************************************************************************
AT SELECTION-SCREEN OUTPUT.
LOOP AT SCREEN.
    IF B_SRPRT = 'X'.    "SumryReport
       IF SCREEN-NAME = 'F_PERIO'.
          CLEAR: F_PERIO.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
       ENDIF.
    ELSE.
       IF SCREEN-NAME = 'F_PERIO'.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
       ENDIF.
    ENDIF.
ENDLOOP.
************************************************************************
********************** START-OF-SELECTION ******************************
************************************************************************
INCLUDE ZFICO_AUTH_CHECK_INCLUDE.                  "TR569

START-OF-SELECTION.

  PERFORM DO_THE_SETUP.

  PERFORM GET_FAGLFLEXT_DATA.

  SORT RPTTAB BY BUKRS RACCT.

  PERFORM DISPLAY_ALV_GRID_DATA.

************************************************************************
***************************** GET_FAGLFLEXT_DATA  **********************
************************************************************************
FORM GET_FAGLFLEXT_DATA.

 SELECT *
   FROM FAGLFLEXT
  WHERE RRCTY = '0'                          "Actual        TR105
    AND RBUKRS IN S_BUKRS
    AND RYEAR = P_FYEAR
    AND RACCT IN S_RACCT
    AND RLDNR = P_RLDNR.                                    "TR854

IF B_SRPRT = 'X'.        "Summry Report
    RPTTAB-BUKRS = FAGLFLEXT-RBUKRS.
    RPTTAB-RACCT = FAGLFLEXT-RACCT.
    RPTTAB-HSLV2 = FAGLFLEXT-HSLVT.
    ADD FAGLFLEXT-HSL01 FROM 1 TO T_PERIO GIVING RPTTAB-HSLV1.
    RPTTAB-HSLV1 = RPTTAB-HSLV1 + FAGLFLEXT-HSLVT.  "Add Bal.Carried fwd
    RPTTAB-DIFFR = RPTTAB-HSLV1 - FAGLFLEXT-HSLVT.
ELSE.
    RPTTAB-BUKRS = FAGLFLEXT-RBUKRS.
    RPTTAB-RACCT = FAGLFLEXT-RACCT.
 IF FAGLFLEXT-DRCRK = 'S'.
    ADD FAGLFLEXT-HSL01 FROM F_PERIO TO T_PERIO GIVING RPTTAB-HSLV1.
 ELSE.
    ADD FAGLFLEXT-HSL01 FROM F_PERIO TO T_PERIO GIVING RPTTAB-HSLV2.
    RPTTAB-HSLV2 = RPTTAB-HSLV2 * -1.
 ENDIF.
    RPTTAB-DIFFR = RPTTAB-HSLV1 - RPTTAB-HSLV2.
ENDIF.
    PERFORM GET_ACCOUNT_DESCRIPTION.

    COLLECT RPTTAB.
    CLEAR   RPTTAB.

 ENDSELECT.
ENDFORM.

**----------------------------------------------------------------------
*
**               GET_ACCOUNT_DESCRIPTION.
*
**----------------------------------------------------------------------
FORM GET_ACCOUNT_DESCRIPTION.

  CLEAR RPTTAB-TXT50.
  SELECT SINGLE TXT50 INTO RPTTAB-TXT50
  FROM   SKAT
  WHERE  SPRAS = 'EN'
    AND  KTOPL = W_KTOPL
    AND  SAKNR = FAGLFLEXT-RACCT.

ENDFORM.
**----------------------------------------------------------------------
*
**               GET_ACCOUNT_DESCRIPTION.
*
**----------------------------------------------------------------------
FORM DO_THE_SETUP.
IF B_DRPRT = 'X'.     "Detail Report

   IF F_PERIO = SPACE OR T_PERIO = SPACE OR
      F_PERIO > '16' OR T_PERIO > '16'.
      WRITE:/.
      WRITE:/ '***ERROR - PLEASE ENTER VALID PERIOD: 01 TO 16'.
      STOP.
   ENDIF.

   IF F_PERIO > T_PERIO.
      WRITE:/.
      WRITE:/ '***ERROR -FROM PERIOD CAN NOT BE GREATER THAN TO PERIOD'.
      STOP.
   ENDIF.
ELSE.
   IF T_PERIO = SPACE OR T_PERIO > '16'.
      WRITE:/.
      WRITE:/ '***ERROR - PLEASE ENTER VALID PERIOD: 01 TO 16'.
      STOP.
   ENDIF.
ENDIF.

IF SY-HOST+0(3) = 'WEI'.
   MOVE 'WECA' TO W_KTOPL.
ENDIF.
ENDFORM.

**----------------------------------------------------------------------
*
**               DISPLAY_ALV_GRID_DATA.
*
**----------------------------------------------------------------------
*
FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

 IF B_SRPRT = 'X'.
 CONCATENATE TEXT-101 T_PERIO '/' P_FYEAR INTO W_HEAD01
             SEPARATED BY SPACE.
 ELSE.
      CONCATENATE TEXT-102 P_FYEAR '/' F_PERIO '-' T_PERIO
                  INTO W_HEAD01 SEPARATED BY SPACE.
 ENDIF.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).

  CONCATENATE TEXT-LDG P_RLDNR INTO W_HEAD03 SEPARATED BY SPACE. "TR854
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  VARIANT-REPORT = REPID.
  REFRESH FIELDCAT.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'RPTTAB'
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
     WHEN 'BUKRS'.
          FC_STR-SELTEXT_L = TEXT-C01.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
          FC_STR-KEY    = ' '.                 " Key columns -not first
     WHEN 'RACCT'.
          FC_STR-SELTEXT_L = TEXT-C02.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
          FC_STR-KEY    = ' '.                 " Key columns -not first
     WHEN 'TXT50'.
          FC_STR-SELTEXT_L = TEXT-C03.        " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'HSLV1'.
       IF B_SRPRT = 'X'.
          FC_STR-SELTEXT_L = TEXT-C04.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
       ELSE.
          FC_STR-SELTEXT_L = TEXT-C05.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
       ENDIF.
     WHEN 'HSLV2'.
        IF B_SRPRT = 'X'.
           FC_STR-SELTEXT_L = TEXT-C06.          " Alternative colheader
           FC_STR-DDICTXT = 'L'.
        ELSE.
           FC_STR-SELTEXT_L = TEXT-C07.          " Alternative colheader
           FC_STR-DDICTXT = 'L'.
*          FC_STR-KEY    = ' '.                 " Key columns -not first
        ENDIF.
     WHEN 'DIFFR'.
       IF B_SRPRT = 'X'.
          FC_STR-SELTEXT_L = TEXT-C09.          " Alternative colheader
       ELSE.
          FC_STR-SELTEXT_L = TEXT-C10.
       ENDIF.
          FC_STR-DDICTXT = 'L'.
     WHEN OTHERS.
* fc_str-no_out = 'X'.           " hide column
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.


* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
          I_SAVE       = 'A'
*          IS_VARIANT   = variant
          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = RPTTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
*  LS_LINE-INFO = SY-TITLE.             "sy-title.
  LS_LINE-INFO = w_head01.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- ACTION LINE:  TYPE A                            "TR854
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD03.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.

ENDFORM.
