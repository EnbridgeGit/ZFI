REPORT ZFAPR008.
TYPE-POOLS: SLIS.
************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        AUGUST, 2006.                                          *
*  Issue Log:   TR284                                                  *
*  Description:                                                        *
*     - The purpose of this program is to produce the listing of dupli-*
*       cate vendor/employees in the vendor master.                    *
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*                                                                      *
************************************************************************
TABLES: LFA1.               "Vendor Master

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME title text-003.
SELECTION-SCREEN SKIP 1.
PARAMETERS: B_NAME1  RADIOBUTTON GROUP DUPL DEFAULT 'X',
            B_NAME2  RADIOBUTTON GROUP DUPL,
            B_CITY   RADIOBUTTON GROUP DUPL.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME title text-004.
PARAMETERS: P_BLOCK  AS CHECKBOX DEFAULT ' ',
            P_DELET  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN END OF BLOCK BOX.

DATA: BEGIN OF VENTAB OCCURS 0,
        LIFNR    LIKE LFA1-LIFNR,
        NAME1    LIKE LFA1-NAME1,
        NAME2    LIKE LFA1-NAME2,
        NAME3    LIKE LFA1-NAME3,
        ORT01    LIKE LFA1-ORT01,
        REGIO    LIKE LFA1-REGIO,
        LAND1    LIKE LFA1-LAND1,
        PFACH    LIKE LFA1-PFACH,
        STRAS    LIKE LFA1-STRAS,
        SPERR    LIKE LFA1-SPERR,
        LOEVM    LIKE LFA1-LOEVM,
      END OF VENTAB.

DATA: W_HEAD01(60)  TYPE C,
      PREV_NAME1    LIKE LFA1-NAME1,
      PREV_NAME2    LIKE LFA1-NAME2,
      PREV_ORT01    LIKE LFA1-ORT01,
      LAST_NAME1_OK LIKE LFA1-NAME1,
      LAST_NAME2_OK LIKE LFA1-NAME2,
      W_TABIX       LIKE SY-TABIX,
      VEN_ROWS      TYPE I.

*-----------------------------------------------------------------------

START-OF-SELECTION.
SELECT LIFNR NAME1 NAME2 NAME3 ORT01 REGIO LAND1 PFACH STRAS SPERR LOEVM
  INTO TABLE VENTAB
  FROM LFA1
 WHERE LIFNR > 0
 ORDER BY NAME1 NAME2 ORT01.

IF P_BLOCK = 'X'.
   DELETE VENTAB WHERE SPERR = 'X'.
ENDIF.

IF P_DELET = 'X'.
   DELETE VENTAB WHERE LOEVM = 'X'.
ENDIF.

IF B_NAME1 = 'X'.          "Name1 duplicate
   PERFORM CHECK_BY_NAME1.
ENDIF.

IF B_NAME2 = 'X'.          "Name1 & Name2 Duplicate
   SORT VENTAB descending BY NAME1.
   PERFORM CHECK_BY_NAME1.
   SORT VENTAB BY NAME1 name2.
   PERFORM CHECK_BY_NAME2.
   SORT VENTAB BY NAME1.
   PERFORM CHECK_BY_NAME1.
ENDIF.

IF B_CITY = 'X'.           "Name1, Name2 & City Duplicate
   SORT VENTAB descending BY NAME1.
   PERFORM CHECK_BY_NAME1.
   SORT VENTAB BY NAME1 name2.
   PERFORM CHECK_BY_NAME2.
   SORT VENTAB BY NAME1 name2 ort01.
   PERFORM CHECK_BY_CITY.
   SORT VENTAB descending BY NAME1 name2.
   PERFORM CHECK_BY_NAME2.
   SORT VENTAB DESCENDING BY NAME1.
   PERFORM CHECK_BY_NAME1.
   SORT VENTAB BY NAME1.
   PERFORM CHECK_BY_NAME1.
ENDIF.

   DESCRIBE TABLE VENTAB LINES VEN_ROWS.
   IF VEN_ROWS = 1.
      REFRESH VENTAB.
   ENDIF.

   SORT VENTAB BY NAME1 NAME2 ORT01.

   PERFORM DISPLAY_ALV_GRID_DATA.

***************************************************************
FORM CHECK_BY_NAME1.
   CLEAR: PREV_NAME1, LAST_NAME1_OK.
   LOOP AT VENTAB.
        IF VENTAB-NAME1 <> PREV_NAME1.
           W_TABIX = SY-TABIX - 1.
           IF W_TABIX <> 0.
              IF PREV_NAME1 <> LAST_NAME1_OK.
                 DELETE VENTAB INDEX W_TABIX.
              ENDIF.
           ENDIF.
           MOVE VENTAB-NAME1 TO PREV_NAME1.
        ELSE.
           MOVE VENTAB-NAME1 TO LAST_NAME1_OK.
        ENDIF.
   ENDLOOP.


ENDFORM.

*****************************************************************
FORM CHECK_BY_NAME2.

   SORT VENTAB BY NAME1 NAME2.
   CLEAR: PREV_NAME1, PREV_NAME2.
   LOOP AT VENTAB.
        IF VENTAB-NAME1 = PREV_NAME1 AND VENTAB-NAME2 <> PREV_NAME2.
                  DELETE VENTAB WHERE NAME1 = PREV_NAME1
                                  AND NAME2 <> PREV_NAME2.
           MOVE VENTAB-NAME1 TO PREV_NAME1.
           MOVE VENTAB-NAME2 TO PREV_NAME2.
        ELSE.
           MOVE VENTAB-NAME1 TO PREV_NAME1.
           MOVE VENTAB-NAME2 TO PREV_NAME2.
        ENDIF.
        AT LAST.
           IF VENTAB-NAME1 <> PREV_NAME1 AND VENTAB-NAME2 <> PREV_NAME2.
              DELETE VENTAB WHERE NAME1 = PREV_NAME1
                              AND NAME2 <> PREV_NAME2.
           ENDIF.
        ENDAT.
   ENDLOOP.

ENDFORM.

*****************************************************************
FORM CHECK_BY_CITY.

SORT VENTAB BY NAME1 NAME2 .
CLEAR: PREV_NAME1, PREV_NAME2, PREV_ORT01.
LOOP AT VENTAB.
  IF VENTAB-NAME1 = PREV_NAME1 AND VENTAB-NAME2  = PREV_NAME2
                               AND VENTAB-ORT01 <> PREV_ORT01.
     DELETE VENTAB WHERE NAME1 =  PREV_NAME1
                     AND NAME2 =  PREV_NAME2
                     AND ORT01 <> PREV_ORT01.
  ENDIF.
     MOVE VENTAB-NAME1 TO PREV_NAME1.
     MOVE VENTAB-NAME2 TO PREV_NAME2.
     MOVE VENTAB-ORT01 TO PREV_ORT01.
ENDLOOP.

ENDFORM.

*-----------------------------------------------------------------------
*                     DISPLAY_ALV_GRID_DATA.
*-----------------------------------------------------------------------
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
     WHEN 'LIFNR'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'LAND1'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'NAME1'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'NAME2'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'NAME3'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'ORT01'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
     WHEN 'REGIO'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
     WHEN 'PFACH'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
     WHEN 'STRAS'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
     WHEN 'SPERR'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
     WHEN 'LOEVM'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
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

IF B_NAME1 = 'X'.
   CONCATENATE TEXT-102 TEXT-103 INTO HEAD_INFO_01 SEPARATED BY SPACE.
ELSEIF B_NAME2 = 'X'.
   CONCATENATE TEXT-102 TEXT-104 INTO HEAD_INFO_01 SEPARATED BY SPACE.
ELSE.
   CONCATENATE TEXT-102 TEXT-105 INTO HEAD_INFO_01 SEPARATED BY SPACE.
ENDIF.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = HEAD_INFO_01.
*  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*line 1:
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'H'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO  = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.

ENDFORM.
