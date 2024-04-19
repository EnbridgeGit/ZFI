REPORT ZFGLR010. "LINE-COUNT 58 LINE-SIZE 170 NO STANDARD PAGE HEADING.
TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
*  Author: Selwyn Rodricks                                             *
*          OmniLogic Systems Group                                     *
*  Brief Description:                                                  *
*  - FI Document Summary Report                                        *
*****   **********  ********   ********   **********   ***********  ****
*  modified by Nancy Gilligan, OmniLogic 98/10   D30K906129            *
*     - standardized headers, set company code text based on client    *
*                                                                      *
* 2008/06/24 TR584 M Khan  Add 2 new columns asset # and sub asset # in*
*                          the report and use ALV report format.       *
*                                                                      *
* 2013/02/01 SDP39827 M Khan  Add Assignment field at the end of report*
*----------------------------------------------------------------------*
TABLES: BKPF, BSEG, TBSL, T001.

*DATA:   DRAMT  LIKE BSEG-WRBTR,
*        CRAMT  LIKE BSEG-WRBTR.

DATA: BEGIN OF REPTAB1 OCCURS 1,        "TR584 new
     BUKRS    LIKE  BSEG-BUKRS,
     BELNR    LIKE  BSEG-BELNR,             "Doc.#
     BUDAT    LIKE  BKPF-BUDAT,             "Posting date
     HKONT    LIKE  BSEG-HKONT,             "G/L account
     KOSTL    LIKE  BSEG-KOSTL,             "Cost center
     AUFNR    LIKE  BSEG-AUFNR,             "Order number
     ANLN1    LIKE  BSEG-ANLN1,             "Asset #
     ANLN2    LIKE  BSEG-ANLN2,             "Sub Asset #
     MATNR    LIKE  BSEG-MATNR,             "Material number
     BSCHL    LIKE  BSEG-BSCHL,             "Posting key
     WRBTR    LIKE  BSEG-WRBTR,             "Amount
     MWSKZ    LIKE  BSEG-MWSKZ,             "Tax code
     MENGE    LIKE  BSEG-MENGE,             "Quantity
     MEINS    LIKE  BSEG-MEINS,             "Unit
     SGTXT    LIKE  BSEG-SGTXT,             "Item Text
     ZUONR    LIKE  BSEG-ZUONR,             "Assignment Number SDP39827
     END OF REPTAB1.

DATA: ES_VARIANT LIKE DISVARIANT,
      IS_VARIANT LIKE DISVARIANT,
      W_HEAD01(50)        TYPE C,
      W_HEAD02(60)        TYPE C.

*FIELD-GROUPS: HEADER, DATA.            "TR584
*INSERT BSEG-GJAHR                      "Year
*       BSEG-BUKRS                      "Company code
*       BSEG-HKONT                      "G/L account
*       BSEG-BSCHL                      "Posting key
*  INTO HEADER.
*
*INSERT BKPF-BUDAT                      "Posting date
*       BSEG-WRBTR                      "Amount
*       BSEG-MWSKZ                      "Tax code
*       BSEG-MENGE                      "Quantity
*       BSEG-MEINS                      "Unit
*       BSEG-BELNR                      "Doc.#
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
  IS_VARIANT-REPORT = 'ZFGLR010'.
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
  SELECT * FROM BSEG WHERE BUKRS = BKPF-BUKRS AND
                           BELNR = BKPF-BELNR AND
                           GJAHR = BKPF-GJAHR.

    SELECT SINGLE * FROM TBSL WHERE BSCHL = BSEG-BSCHL.
    IF SY-SUBRC = 0.
      IF TBSL-SHKZG = 'H'.
        BSEG-WRBTR = - BSEG-WRBTR.
        BSEG-MENGE = - BSEG-MENGE.
      ENDIF.
    ENDIF.
    MOVE:  BSEG-BUKRS TO REPTAB1-BUKRS,             "Company code
           BSEG-BELNR TO REPTAB1-BELNR,             "Doc.#
           BKPF-BUDAT TO REPTAB1-BUDAT,             "Posting date
           BSEG-HKONT TO REPTAB1-HKONT,             "G/L account
           BSEG-KOSTL TO REPTAB1-KOSTL,             "Cost center
           BSEG-AUFNR TO REPTAB1-AUFNR,             "Order number
           BSEG-ANLN1 TO REPTAB1-ANLN1,             "Asset #
           BSEG-ANLN2 TO REPTAB1-ANLN2,             "Sub Asset #
           BSEG-MATNR TO REPTAB1-MATNR,             "Material number
           BSEG-BSCHL TO REPTAB1-BSCHL,             "Posting key
           BSEG-WRBTR TO REPTAB1-WRBTR,             "Amount in Doc curr
           BSEG-MWSKZ TO REPTAB1-MWSKZ,             "Tax code
           BSEG-MENGE TO REPTAB1-MENGE,             "Quantity
           BSEG-MEINS TO REPTAB1-MEINS,             "Unit of Measurment
           BSEG-SGTXT TO REPTAB1-SGTXT,             "Item Text
           BSEG-ZUONR TO REPTAB1-ZUONR.           "Assignment# SDP39827
           APPEND REPTAB1.
           CLEAR  REPTAB1.
  ENDSELECT.
ENDSELECT.
ENDFORM.

*----------------------- DISPLAY_ALV_GRID_DATA -------------------------
FORM DISPLAY_ALV_GRID_DATA.
SORT REPTAB1 BY BUKRS BELNR BUDAT.

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
     WHEN 'ZUONR'.
          FC_STR-SELTEXT_L = TEXT-C17.          " Alt col hder SDP39827
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
           T_OUTTAB = REPTAB1
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
CONCATENATE SY-TITLE TEXT-004 P_GJAHR INTO W_TITLE
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

*    ON CHANGE OF BSEG-BELNR.
*      IF P_NEWPAG = 'X'.
*        NEW-PAGE.
*      ELSE.
*        SKIP.
*        RESERVE 5 LINES.
*      ENDIF.
*    ENDON.

*    WRITE: /1  BSEG-BUKRS,
*            6  BSEG-GJAHR.
*
*    WRITE: 12  BSEG-BELNR,             "Doc.#
*           24  BKPF-BUDAT,             "Posting date
*           38  BSEG-HKONT,             "G/L account
*           47  BSEG-KOSTL,             "Cost center
*           58  BSEG-AUFNR,             "Order number
*           72  BSEG-MATNR+12,          "Material number
*           82  BSEG-BSCHL,             "Posting key
*           86  BSEG-WRBTR,             "Amount
*          104  BSEG-MWSKZ,             "Tax code
*          108  BSEG-MENGE,             "Quantity
*               BSEG-MEINS,             "Unit
*               BSEG-SGTXT(40).         "Text

*    EXTRACT DATA.
*  ENDSELECT.
*ENDSELECT.
*SORT.
*SUMMARY = 'X'.
*CLEAR DETAIL.

*LOOP.
*  AT NEW BSEG-BUKRS.
*    NEW-PAGE.
*    WRITE: / 'SUMMARY:', BSEG-BUKRS.
*  ENDAT.
*
*  AT END OF BSEG-BSCHL.
*    WRITE: /38  BSEG-HKONT,            "G/L account
*            46  BSEG-BSCHL,            "Posting key
*            50  SUM(BSEG-WRBTR),       "Amount
*            68  SUM(BSEG-MENGE).       "Quantity
*  ENDAT.
*
*  AT END OF BSEG-HKONT.
*    WRITE: /50  '---------------   ----------------'.
*    WRITE: /50  SUM(BSEG-WRBTR),       "Amount
*            68  SUM(BSEG-MENGE).       "Quantity
*    WRITE: /50  '===============   ================'.
*    SKIP.
*  ENDAT.
*
*  AT LAST.
*    WRITE: /50  '---------------   ----------------'.
*    WRITE: /20  TEXT-002,
*            50  SUM(BSEG-WRBTR),       "Amount
*            68  SUM(BSEG-MENGE).       "Quantity
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
