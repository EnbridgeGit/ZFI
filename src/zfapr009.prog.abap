REPORT ZFAPR009.
TYPE-POOLS: SLIS.

************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        AUGUST, 2006.                                          *
*  Issue Log:   TR284                                                  *
*  Description:                                                        *
*     - The purpose of this program is to produce the listing of vendor*
*       /employee with no activity during a specified period as per    *
*       data selected through variants.                                *
*                                                                      *
************************************************************************
*CHANGES:
*2009/08/17 TR277 - M Demeester - Exclude at both the Vendor & Company
**                                                                     *
*9/11/06 TR342 Mohammad Khan Added table LFB1 for company code check.  *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
TABLES: LFA1,               "Vendor Master (General Section)
        LFB1,               "Vendor Master (Company Code)
        LFC1,               "Vendor master (transaction figures)
        BKPF.    "JUST FOR REFERENCE PURPOSE

DATA: BEGIN OF VENTAB OCCURS 0,
       BUKRS    LIKE LFC1-BUKRS,
       LIFNR    LIKE LFA1-LIFNR,
       NAME1    LIKE LFA1-NAME1,
       DTOTL    LIKE LFC1-UM01S,       "Debit
       CTOTL    LIKE LFC1-UM01H,       "Credit
       blvdr    like lfa1-sperr,       "Blocked Vendor
       blcocd   like lfb1-sperr,       "Blocked company vendor
       dfvdr    like lfa1-loevm,       "Deleted Vendor
       dfcocd   like lfb1-loevm,       "Deleted company vendor
      END OF VENTAB.

DATA: W_HEAD01(60)  TYPE C,
      F_YEAR(4)     TYPE C,
      T_YEAR(4)     TYPE C.

**************SELECTION SCREEN******************************************
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME title text-003.
SELECTION-SCREEN SKIP 1.
*PARAMETERS: P_BUKRS  LIKE  LFC1-BUKRS OBLIGATORY DEFAULT 'UGL'.

SELECT-OPTIONS: s_bukrs  for  lfb1-bukrs,
                S_GJAHR  FOR  LFC1-GJAHR OBLIGATORY,
                S_MONAT  FOR  BKPF-MONAT OBLIGATORY DEFAULT
                                   '01' TO '16',
                S_LIFNR  FOR  LFA1-LIFNR.
SELECTION-SCREEN SKIP 1.
PARAMETERS: P_ZERO  radiobutton group BAL DEFAULT 'X',
            P_BAL   radiobutton group BAL.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME title text-004.

selection-screen begin of line.
PARAMETER: P_BLOCK  AS CHECKBOX DEFAULT ' '.
selection-screen: comment 4(60) text-200.
selection-screen end of line.

selection-screen begin of line.
PARAMETER: P_BLCOM  AS CHECKBOX DEFAULT ' '.
selection-screen: comment 4(60) text-201.
selection-screen end of line.

selection-screen begin of line.
PARAMETER: P_DELET  AS CHECKBOX DEFAULT ' '.
selection-screen: comment 4(60) text-202.
selection-screen end of line.

selection-screen begin of line.
PARAMETER: P_DLCOM  AS CHECKBOX DEFAULT ' '.
selection-screen: comment 4(60) text-203.
selection-screen end of line.

SELECTION-SCREEN END OF BLOCK BOX1.
SELECTION-SCREEN END OF BLOCK BOX.


**************END OF SELECTION SCREEN***********************************
INITIALIZATION.
  S_GJAHR-LOW  = SY-DATUM+0(4) - 1.
  S_GJAHR-HIGH = SY-DATUM+0(4).
  APPEND S_GJAHR.

START-OF-SELECTION.

SELECT LIFNR NAME1 SPERR LOEVM
  INTO (LFA1-LIFNR, LFA1-NAME1, LFA1-SPERR, LFA1-LOEVM)
  FROM LFA1
 WHERE LIFNR IN S_LIFNR.

SELECT LIFNR BUKRS SPERR LOEVM
  INTO (LFB1-LIFNR, LFB1-BUKRS, LFB1-SPERR, LFB1-LOEVM)
  FROM LFB1
 WHERE LIFNR = LFA1-LIFNR
   AND BUKRS in S_BUKRS.

SELECT * FROM  LFC1
 WHERE  LIFNR = LFB1-LIFNR
   AND  BUKRS = lfb1-BUKRS
   AND  GJAHR IN S_GJAHR.

 IF SY-SUBRC = 0.
    MOVE: LFC1-BUKRS TO VENTAB-BUKRS,
          LFA1-LIFNR TO VENTAB-LIFNR,
          LFA1-NAME1 TO VENTAB-NAME1,
          lfa1-sperr to ventab-blvdr,
          lfa1-loevm to ventab-dfvdr,
          lfb1-sperr to ventab-blcocd,
          lfb1-loevm to ventab-dfcocd.
          ADD LFC1-UM01S THEN LFC1-UM02S UNTIL LFC1-UM16S GIVING
              VENTAB-DTOTL ACCORDING TO S_MONAT.
          ADD LFC1-UM01H THEN LFC1-UM02H UNTIL LFC1-UM16H GIVING
              VENTAB-CTOTL ACCORDING TO S_MONAT.
    PERFORM BUILD_VENDOR_TABLE.
 ENDIF.
ENDSELECT.
 IF SY-SUBRC <> 0.
    MOVE: lfb1-BUKRS    TO VENTAB-BUKRS,
          LFA1-LIFNR TO VENTAB-LIFNR,
          LFA1-NAME1 TO VENTAB-NAME1,
          lfa1-sperr to ventab-blvdr,
          lfa1-loevm to ventab-dfvdr,
          lfb1-sperr to ventab-blcocd,
          lfb1-loevm to ventab-dfcocd.
    MOVE 0 TO: VENTAB-DTOTL, VENTAB-CTOTL.
    PERFORM BUILD_VENDOR_TABLE.
 ENDIF.
ENDSELECT.
ENDSELECT.

IF P_ZERO = 'X'.
   DELETE VENTAB WHERE DTOTL <> 0 AND CTOTL <> 0. "doesn't delete???
   DELETE VENTAB WHERE DTOTL =  0 AND CTOTL <> 0.
   DELETE VENTAB WHERE DTOTL <> 0 AND CTOTL =  0.
ENDIF.

SORT VENTAB BY BUKRS LIFNR.

PERFORM DISPLAY_ALV_GRID_DATA.

************************************************************************
*              BUILD_VENDOR_TABLE                                      *
************************************************************************
FORM BUILD_VENDOR_TABLE.

DATA: ADD_FLAG TYPE C.

IF P_BLOCK = 'X' AND LFA1-SPERR = 'X'.
   MOVE 'N' TO ADD_FLAG.                "Vendor Level
ENDIF.

IF P_BLCOM = 'X' AND LFB1-SPERR = 'X'.  "TR277 2009/08/17
   MOVE 'N' TO ADD_FLAG.                "Company Level
ENDIF.

IF P_DELET = 'X' AND LFA1-LOEVM = 'X'.
   MOVE 'N' TO ADD_FLAG.                "Vendor Level
ENDIF.

IF P_DLCOM = 'X' AND LFB1-LOEVM = 'X'.  "TR277 2009/08/17
   MOVE 'N' TO ADD_FLAG.                "Company Level
ENDIF.

IF ADD_FLAG <> 'N'.
   COLLECT VENTAB.
ENDIF.

ENDFORM.

************************************************************************
*              DISPLAY_ALV_GRID_DATA.                                  *
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

  MOVE TEXT-CLT  TO W_HEAD01+0(7).
  MOVE SY-SYSID  TO W_HEAD01+8(5).
  MOVE SY-MANDT  TO W_HEAD01+14(4).
  MOVE TEXT-DTE  TO W_HEAD01+21(5).
  WRITE SY-DATUM TO W_HEAD01+27(10).
  MOVE TEXT-TME  TO W_HEAD01+40(5).
  WRITE SY-UZEIT TO W_HEAD01+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  REFRESH FIELDCAT.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'VENTAB'
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
          FC_STR-DDICTXT = 'L'.
     WHEN 'LIFNR'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'NAME1'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'CTOTL'.
          FC_STR-SELTEXT_L = TEXT-C05.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'DTOTL'.
          FC_STR-SELTEXT_L = TEXT-C04.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'BLVDR'.
          FC_STR-SELTEXT_L = TEXT-C06.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
     WHEN 'BLCOCD'.
          FC_STR-SELTEXT_L = TEXT-C07.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
     WHEN 'DFVDR'.
          FC_STR-SELTEXT_L = TEXT-C08.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
     WHEN 'DFCOCD'.
          FC_STR-SELTEXT_L = TEXT-C09.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.
     WHEN OTHERS.
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

LAYOUT-ZEBRA = 'X'.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
          I_SAVE       = 'A'
          IS_VARIANT   = variant
          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = VENTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: F_LENGTH     TYPE I,
        KOUNT        TYPE  I,
        HEAD_INFO_01 TYPE STRING,
        HEAD_INFO_02 TYPE STRING,
        FROM_DATE(11) TYPE C,
        TO_DATE(11)   TYPE C.

IF S_MONAT-HIGH > '01'.
   CONCATENATE TEXT-102 S_MONAT-LOW TEXT-103 S_MONAT-HIGH
               INTO HEAD_INFO_01 SEPARATED BY SPACE.
ELSE.
   CONCATENATE TEXT-102 S_MONAT-LOW
               INTO HEAD_INFO_01 SEPARATED BY SPACE.
ENDIF.
CONCATENATE TEXT-CMA HEAD_INFO_01 TEXT-CMA INTO HEAD_INFO_01.

IF S_GJAHR-HIGH = SPACE.
   CONCATENATE TEXT-104 S_GJAHR-LOW
               INTO HEAD_INFO_02 SEPARATED BY SPACE.
ELSE.
   CONCATENATE TEXT-104 S_GJAHR-LOW TEXT-103 S_GJAHR-HIGH
               INTO HEAD_INFO_02 SEPARATED BY SPACE.
ENDIF.
CONCATENATE HEAD_INFO_01 HEAD_INFO_02 INTO HEAD_INFO_01
            SEPARATED BY SPACE.
CONCATENATE TEXT-101 lfb1-BUKRS INTO HEAD_INFO_02 SEPARATED BY SPACE.
CONCATENATE HEAD_INFO_02 HEAD_INFO_01 INTO HEAD_INFO_01
            SEPARATED BY SPACE.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*line 1:
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = HEAD_INFO_01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*line 2:
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO  = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.

ENDFORM.
