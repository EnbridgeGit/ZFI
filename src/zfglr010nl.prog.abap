REPORT ZFGLR010NL. "LINE-COUNT 58 LINE-SIZE 170 NO STD PAGE HEADING.
TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
*  Author: Glenn Ymana                                                 *
*  Brief Description:                                                  *
*  - FI Document Summary Report (Non-Lead Ledger)                      *
*    This program is a copy of ZFGLR010 but uses BSEG_ADD instead of   *
*    BSEG.                                                             *
*****   **********  ********   ********   **********   ***********  ****
* Modifications:                                                       *
*----------------------------------------------------------------------*
TABLES: BKPF, BSEG_ADD, TBSL, T001.

*DATA:   DRAMT  LIKE BSEG_ADD-WRBTR,
*        CRAMT  LIKE BSEG_ADD-WRBTR.

DATA: BEGIN OF REPTAB OCCURS 1,        "TR584 new
     BUKRS    LIKE  BSEG_ADD-BUKRS,
     BELNR    LIKE  BSEG_ADD-BELNR,             "Doc.#
     BUDAT    LIKE  BKPF-BUDAT,             "Posting date
     HKONT    LIKE  BSEG_ADD-HKONT,             "G/L account
     KOSTL    LIKE  BSEG_ADD-KOSTL,             "Cost center
     AUFNR    LIKE  BSEG_ADD-AUFNR,             "Order number
     ANLN1    LIKE  BSEG_ADD-ANLN1,             "Asset #
     ANLN2    LIKE  BSEG_ADD-ANLN2,             "Sub Asset #
     MATNR    LIKE  BSEG_ADD-MATNR,             "Material number
     BSCHL    LIKE  BSEG_ADD-BSCHL,             "Posting key
     WRBTR    LIKE  BSEG_ADD-WRBTR,             "Amount
     MWSKZ    LIKE  BSEG_ADD-MWSKZ,             "Tax code
     MENGE    LIKE  BSEG_ADD-MENGE,             "Quantity
     MEINS    LIKE  BSEG_ADD-MEINS,             "Unit
     SGTXT    LIKE  BSEG_ADD-SGTXT,             "Item Text
     END OF REPTAB.

DATA: ES_VARIANT LIKE DISVARIANT,
      IS_VARIANT LIKE DISVARIANT,
      W_HEAD01(60)        TYPE C,
      W_HEAD02(60)        TYPE C.

*FIELD-GROUPS: HEADER, DATA.            "TR584
*INSERT BSEG_ADD-GJAHR                      "Year
*       BSEG_ADD-BUKRS                      "Company code
*       BSEG_ADD-HKONT                      "G/L account
*       BSEG_ADD-BSCHL                      "Posting key
*  INTO HEADER.
*
*INSERT BKPF-BUDAT                      "Posting date
*       BSEG_ADD-WRBTR                      "Amount
*       BSEG_ADD-MWSKZ                      "Tax code
*       BSEG_ADD-MENGE                      "Quantity
*       BSEG_ADD-MEINS                      "Unit
*       BSEG_ADD-BELNR                      "Doc.#
*  INTO DATA.
*
*DATA:  DETAIL,
*       SUMMARY.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-003.

SELECT-OPTIONS: S_BUKRS FOR  BKPF-BUKRS,
                S_BELNR FOR  BKPF-BELNR.
PARAMETERS:     P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4).
PARAMETERS:     PVARIANT LIKE DISVARIANT-VARIANT.

*PARAMETERS :    P_NEWPAG AS CHECKBOX. TR584 "New page for new doc
SELECTION-SCREEN END OF BLOCK BOX1.

* initialization                                   "ngilligan 98/10
*INITIALIZATION.                                   "ngilligan98/10
*IF SY-MANDT+2(1) = '1'.                           "ngilligan98/10
*   S_BUKRS-LOW = 'UEC'.                           "ngilligan98/10
*   S_BUKRS-SIGN = 'I'.                            "ngilligan98/10
*   S_BUKRS-OPTION = 'EQ'.                         "ngilligan98/10
*ELSEIF  SY-MANDT+2(1) = '0'.                      "ngilligan98/10
*   S_BUKRS-LOW = 'UGL'.                           "ngilligan98/10
*   S_BUKRS-SIGN = 'I'.                            "ngilligan98/10
*   S_BUKRS-OPTION = 'EQ'.                         "ngilligan98/10
*ENDIF.                                            "ngilligan98/10
*APPEND S_BUKRS.

*-------------------------  AT SELECTION SCREEN-------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZFGLR010NL'.
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

*-------------------------  START-OF-SELECTION -------------------------


* start of main program
START-OF-SELECTION.
   PERFORM BUILD_INTERNAL_TABLE.
   PERFORM DISPLAY_ALV_GRID_DATA.


FORM BUILD_INTERNAL_TABLE.
* get company code text
SELECT SINGLE BUTXT FROM T001 INTO T001-BUTXT WHERE BUKRS = S_BUKRS.

*DETAIL = 'X'.
SELECT * FROM BKPF WHERE BUKRS IN S_BUKRS AND
                         BELNR IN S_BELNR AND
                         GJAHR =  P_GJAHR.
  SELECT * FROM BSEG_ADD WHERE BUKRS = BKPF-BUKRS AND
                           BELNR = BKPF-BELNR AND
                           GJAHR = BKPF-GJAHR.

    SELECT SINGLE * FROM TBSL WHERE BSCHL = BSEG_ADD-BSCHL.
    IF SY-SUBRC = 0.
      IF TBSL-SHKZG = 'H'.
        BSEG_ADD-WRBTR = - BSEG_ADD-WRBTR.
        BSEG_ADD-MENGE = - BSEG_ADD-MENGE.
      ENDIF.
    ENDIF.
    MOVE:  BSEG_ADD-BUKRS TO REPTAB-BUKRS,         "Company code
           BSEG_ADD-BELNR TO REPTAB-BELNR,         "Doc.#
           BKPF-BUDAT TO REPTAB-BUDAT,             "Posting date
           BSEG_ADD-HKONT TO REPTAB-HKONT,         "G/L account
           BSEG_ADD-KOSTL TO REPTAB-KOSTL,         "Cost center
           BSEG_ADD-AUFNR TO REPTAB-AUFNR,         "Order number
           BSEG_ADD-ANLN1 TO REPTAB-ANLN1,         "Asset #
           BSEG_ADD-ANLN2 TO REPTAB-ANLN2,         "Sub Asset #
           BSEG_ADD-MATNR TO REPTAB-MATNR,         "Material number
           BSEG_ADD-BSCHL TO REPTAB-BSCHL,         "Posting key
           BSEG_ADD-WRBTR TO REPTAB-WRBTR,         "Amount in Doc curr
           BSEG_ADD-MWSKZ TO REPTAB-MWSKZ,         "Tax code
           BSEG_ADD-MENGE TO REPTAB-MENGE,         "Quantity
           BSEG_ADD-MEINS TO REPTAB-MEINS,         "Unit of Measurment
           BSEG_ADD-SGTXT TO REPTAB-SGTXT.         "Item Text
           APPEND REPTAB.
           CLEAR  REPTAB.
  ENDSELECT.
ENDSELECT.
ENDFORM.

*----------------------- DISPLAY_ALV_GRID_DATA -------------------------
FORM DISPLAY_ALV_GRID_DATA.
SORT REPTAB BY BUKRS BELNR BUDAT.

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
  LAYOUT-ZEBRA = 'X'.
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'REPTAB'
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
          FC_STR-SELTEXT_L = TEXT-C02.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'BELNR'.
          FC_STR-SELTEXT_L = TEXT-C03.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'BUDAT'.
*          FC_STR-KEY    = ' '.                 " Key columns -not first
          FC_STR-SELTEXT_L = TEXT-C04.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'HKONT'.
*          FC_STR-KEY    = ' '.                 " Key columns -not first
          FC_STR-SELTEXT_L = TEXT-C05.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'KOSTL'.
          FC_STR-SELTEXT_L = TEXT-C06.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use Large system text
     WHEN 'AUFNR'.
          FC_STR-SELTEXT_L = TEXT-C07.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'ANLN1'.
          FC_STR-SELTEXT_L = TEXT-C08.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN 'ANLN2'.
          FC_STR-SELTEXT_L = TEXT-C09.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
     WHEN 'MATNR'.
          FC_STR-SELTEXT_L = TEXT-C10.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
     WHEN 'BSCHL'.
          FC_STR-SELTEXT_L = TEXT-C11.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
     WHEN 'WRBTR'.
          FC_STR-SELTEXT_L = TEXT-C12.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'MWSKZ'.
          FC_STR-SELTEXT_L = TEXT-C13.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'MENGE'.
          FC_STR-SELTEXT_L = TEXT-C14.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
          FC_STR-DO_SUM  = 'X'.                 " Do Sum
     WHEN 'MEINS'.
          FC_STR-SELTEXT_L = TEXT-C15.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
     WHEN 'SGTXT'.
          FC_STR-SELTEXT_L = TEXT-C16.          " Alternative col header
          FC_STR-DDICTXT = 'L'.                 " Use small system text
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
          IS_VARIANT   = variant
*          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = REPTAB
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: W_TITLE LIKE SY-TITLE.
  DATA: KOUNT    TYPE I,
        F_LENGTH TYPE I.
  DATA: BEGIN OF HDATA,
        HEAD03(60) TYPE C,
        HEAD04(60) TYPE C,
        HEAD05(60) TYPE C,
        END OF HDATA.


*CONCATENATE SY-REPID '-' SY-TITLE P_GJAHR INTO W_TITLE
MOVE SY-TITLE TO W_TITLE.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
*  LS_LINE-INFO = SY-TITLE.             "sy-title.
  LS_LINE-INFO = W_TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*CONCATENATE SY-REPID '-' SY-TITLE P_GJAHR INTO W_TITLE
CONCATENATE TEXT-004 P_GJAHR INTO W_TITLE
            SEPARATED BY SPACE.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
*  LS_LINE-INFO = SY-TITLE.             "sy-title.
  LS_LINE-INFO = W_TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
*  ls_line-typ  = 'A'.
  LS_LINE-TYP   = 'S'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.


ENDFORM.
*End of TR584 changes

*    ON CHANGE OF BSEG_ADD-BELNR.
*      IF P_NEWPAG = 'X'.
*        NEW-PAGE.
*      ELSE.
*        SKIP.
*        RESERVE 5 LINES.
*      ENDIF.
*    ENDON.

*    WRITE: /1  BSEG_ADD-BUKRS,
*            6  BSEG_ADD-GJAHR.
*
*    WRITE: 12  BSEG_ADD-BELNR,             "Doc.#
*           24  BKPF-BUDAT,             "Posting date
*           38  BSEG_ADD-HKONT,             "G/L account
*           47  BSEG_ADD-KOSTL,             "Cost center
*           58  BSEG_ADD-AUFNR,             "Order number
*           72  BSEG_ADD-MATNR+12,          "Material number
*           82  BSEG_ADD-BSCHL,             "Posting key
*           86  BSEG_ADD-WRBTR,             "Amount
*          104  BSEG_ADD-MWSKZ,             "Tax code
*          108  BSEG_ADD-MENGE,             "Quantity
*               BSEG_ADD-MEINS,             "Unit
*               BSEG_ADD-SGTXT(40).         "Text

*    EXTRACT DATA.
*  ENDSELECT.
*ENDSELECT.
*SORT.
*SUMMARY = 'X'.
*CLEAR DETAIL.

*LOOP.
*  AT NEW BSEG_ADD-BUKRS.
*    NEW-PAGE.
*    WRITE: / 'SUMMARY:', BSEG_ADD-BUKRS.
*  ENDAT.
*
*  AT END OF BSEG_ADD-BSCHL.
*    WRITE: /38  BSEG_ADD-HKONT,            "G/L account
*            46  BSEG_ADD-BSCHL,            "Posting key
*            50  SUM(BSEG_ADD-WRBTR),       "Amount
*            68  SUM(BSEG_ADD-MENGE).       "Quantity
*  ENDAT.
*
*  AT END OF BSEG_ADD-HKONT.
*    WRITE: /50  '---------------   ----------------'.
*    WRITE: /50  SUM(BSEG_ADD-WRBTR),       "Amount
*            68  SUM(BSEG_ADD-MENGE).       "Quantity
*    WRITE: /50  '===============   ================'.
*    SKIP.
*  ENDAT.
*
*  AT LAST.
*    WRITE: /50  '---------------   ----------------'.
*    WRITE: /20  TEXT-002,
*            50  SUM(BSEG_ADD-WRBTR),       "Amount
*            68  SUM(BSEG_ADD-MENGE).       "Quantity
*    WRITE: /50  '===============   ================'.
*    SKIP.
*  ENDAT.
*
*ENDLOOP.
*
*
** top of page
*TOP-OF-PAGE.
*  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan
*98/10
*         42 T001-BUTXT INTENSIFIED ON,                  "ngilligan
*98/10
*        100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan
*98/10
*
*  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan
*98/10
*                                        SY-SYSID.       "ngilligan
*98/10
*  WRITE: TEXT-001 UNDER T001-BUTXT.                     "ngilligan
*98/10
*  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan
*98/10
*       SKIP 1.                                         "ngilligan 98/10
*  ULINE.                                                "ngilligan
*98/10
*
** FORMAT INTENSIFIED OFF.
*
*  IF DETAIL = 'X'.
*    WRITE: /1  TEXT-101,
*            6  TEXT-102,
*           13  TEXT-103,
*           24  TEXT-104,
*           36  TEXT-105,
*           45  TEXT-106,
*           58  TEXT-107,
*           69  TEXT-108,
*           82  TEXT-109,
*           95  TEXT-110,
*          103  TEXT-111,
*          116  TEXT-112,
*          126  TEXT-113,
*          130  TEXT-114.
*  ENDIF.
*
*  IF SUMMARY = 'X'.
*    WRITE: /36 TEXT-105,
*            46 TEXT-109,
*            59 TEXT-110,
*            76 TEXT-112.
*  ENDIF.
*
*
*  ULINE.
