REPORT ZFAPR004.
TYPE-POOLS: SLIS.
*-----------------------------------------------------------------------
*  THIS ABAP WAS DOCUMENTED under the DOCUMENTATION tab as of 2003/10/28
*  Please review the documentation and make any necessary changes before
*  transporting.
*
************************************************************************
* 2006/08/2 Mohammad TR23 Change userid field from single value to range
*                         Also change the report format to ALV.
* 2004/02/23 mdemeest #--- Select all transactions rather than just AP
* 2004/01/23 mdemeest #--- D30K911675
*                          - Company code from single to range
*                          - User name from multiple to single
* 2003/09/18 mdemeest 1017 Add Posting Key & Username D30K911531
* 2003/04/08 mdemeest 1019 Change to allow memory id parameters
* 2000/05/05 mdemeest #787 docctr Documents and $$ paid based on variant
*
************************************************************************
TABLES: BKPF, BSEG,
        LFA1.               "Vendor Name

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-011.
select-options: S_BUKRS FOR BKPF-BUKRS memory id BUK,
                S_LIFNR FOR BSEG-LIFNR OBLIGATORY,       "Vendor
                S_KONZS FOR LFA1-KONZS NO INTERVALS.     "Group Key
select-options: s_WAERS for BKPF-WAERS memory id FWS no intervals,
                s_BLART FOR BKPF-BLART NO INTERVALS,    "Doc Type
                S_BUDAT FOR BKPF-BUDAT OBLIGATORY,
                S_DMBTR FOR BSEG-DMBTR,
                S_USNAM FOR BKPF-USNAM OBLIGATORY.      "User Name
*Parameters:     p_usnam like bkpf-usnam obligatory.      "User Name
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-109.
PARAMETERS:      PVARIANT LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX3.

DATA: BEGIN OF WA OCCURS 0,
        company  LIKE BKPF-BUKRS,
        vendor   like bseg-lifnr,
        NAME1    LIKE LFA1-NAME1,
        doctype  LIKE BKPF-BLART,
        pstkey   like bseg-bschl,           "D30K911531
        document LIKE BKPF-BELNR,
        user     like bkpf-usnam,           "D30K911531
        DMBTR    LIKE BSEG-DMBTR,
        currency LIKE BKPF-WAERS,
        DOCCTR   TYPE I,                   "Only item 1 will be = 1
      END OF WA.

data: cnt type i.
data: set_column(10) type c.
DATA: ES_VARIANT    LIKE DISVARIANT,
      IS_VARIANT    LIKE DISVARIANT,
      W_HEAD01(60)  TYPE C.

*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZFAPR004'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = IS_VARIANT
*           I_TABNAME_HEADER    =
*           I_TABNAME_ITEM      =
*           IT_DEFAULT_FIELDCAT =
            I_SAVE              = 'A'
        IMPORTING
*           E_EXIT              =
            ES_VARIANT          = ES_VARIANT
       EXCEPTIONS
            NOT_FOUND           = 1
            PROGRAM_ERROR       = 2
            OTHERS              = 3.
  PVARIANT = ES_VARIANT-VARIANT.
*-----------------------------------------------------------------------

START-OF-SELECTION.
SELECT * FROM BKPF                       "Selection based on fiscal year
   WHERE BUKRS in s_BUKRS
     AND WAERS in s_waers
     AND BLART IN S_BLART
     AND BUDAT IN S_BUDAT
     AND USNAM IN S_USNAM.
*     and usnam = p_usnam.

     PERFORM MOVE_BKPF_TO_WA.                 "Move info to work area

     SELECT * FROM BSEG
       WHERE BUKRS = BKPF-BUKRS
         AND BELNR = BKPF-BELNR
         AND GJAHR = BKPF-GJAHR
         AND LIFNR IN S_LIFNR
         AND DMBTR IN S_DMBTR.
*         AND BUZID = ' '.                    "only AP transactions


     PERFORM MOVE_BSEG_TO_WA.                    "Move info to work area
     ENDSELECT.
ENDSELECT.                    "End of BKPF

 IF NOT WA[] IS INITIAL.
    PERFORM DISPLAY_ALV_GRID_DATA.
 ENDIF.


*---------------------- MOVE_BKPF_TO_WA  -------------------------------
FORM MOVE_BKPF_TO_WA.
  CLEAR WA.
  MOVE: BKPF-BUKRS TO WA-company,
        BKPF-BLART TO WA-doctype,
        BKPF-WAERS TO WA-currency,
        BKPF-BELNR TO WA-document,
*        BKPF-BUDAT TO WA-BUDAT,
        bkpf-usnam to wa-user,
        1          TO WA-DOCCTR.
ENDFORM.

*---------------------- MOVE_BSEG_TO_WA  -------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_BSEG_TO_WA.
  MOVE BSEG-DMBTR      TO WA-DMBTR.
  move bseg-bschl      to wa-pstkey.
  IF BSEG-SHKZG = 'H'.
     WA-DMBTR = WA-DMBTR * -1.
  ENDIF.
*  MOVE BSEG-BUZEI      TO WA-BUZEI.
  MOVE BSEG-LIFNR      TO WA-vendor.
  SELECT SINGLE * FROM LFA1
     WHERE LIFNR = WA-vendor
       AND KONZS IN S_KONZS.
  IF SY-SUBRC = '0'.
     MOVE LFA1-NAME1      TO WA-NAME1.
     APPEND WA.
  ENDIF.
  CLEAR: WA-DMBTR, WA-vendor, WA-DOCCTR.

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
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'WA'
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
     WHEN 'COMPANY'.
          FC_STR-DDICTXT = 'L'.
          FC_STR-KEY    = ' '.                 " Key columns -not first
     WHEN 'VENDOR'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'NAME1'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'DOCTYPE'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'PSTKEY'.
          FC_STR-DDICTXT = 'L'.
     WHEN 'DOCUMENT'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-KEY    = ' '.                 " Key columns -not first
     WHEN 'USER'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
     WHEN 'DMBTR'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'CURRENCY'.
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
     WHEN 'DOCCTR'.
          FC_STR-SELTEXT_L = TEXT-C09.          " Alternative colheader
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN OTHERS.
* fc_str-no_out = 'X'.           " hide column
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

LAYOUT-ZEBRA = 'X'.
*APPEND LAYOUT.

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
           T_OUTTAB = WA
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

CONCATENATE TEXT-103 S_BUDAT-LOW+0(4) TEXT-104 S_BUDAT-LOW+4(2) TEXT-104
            S_BUDAT-LOW+6(2) INTO FROM_DATE.
 CONCATENATE S_BUDAT-HIGH+0(4) TEXT-104 S_BUDAT-HIGH+4(2) TEXT-104
             S_BUDAT-HIGH+6(2) TEXT-105 INTO TO_DATE.
 CONCATENATE SY-REPID TEXT-102 FROM_DATE TEXT-106 TO_DATE
             INTO HEAD_INFO_01 SEPARATED BY SPACE.


*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = HEAD_INFO_01.             "sy-title.
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
