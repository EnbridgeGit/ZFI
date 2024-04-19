*&---------------------------------------------------------------------*
*& Report  ZFAPI040                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
************************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        JULY, 2007.                                            *
*  Issue Log:   TR442                                                  *
*  Description:                                                        *
*     - The purpose of this program is to produce the files for PST    *
*       audit. The data will be dowload from the following tables:     *
*       BKPF, BSEG, PRPS, SKAT, LFA1, EKKO, EKPO.                      *
************************************************************************
*&---------------------------------------------------------------------*

REPORT  ZFAPI040 MESSAGE-ID 00.                               .
TABLES: BKPF,
        SKAT,
        PRPS,
        BSEG,
        LFA1,
        LFB1,
        EKKO,
        EKPO,
        DD03L.         "FOR REFERENCE PURPOSE ONLY

DATA: BEGIN OF IT_DFIES OCCURS 1.               "Table Field names
      INCLUDE STRUCTURE DFIES.
DATA: END OF IT_DFIES.

DATA: MAX_POSITION LIKE DFIES-POSITION,
      BIG_STRING TYPE STRING,
      W_VALUE(60) TYPE C,
      COUNT TYPE I.

* Field Symbols
FIELD-SYMBOLS:
      <FS1>, <FS2>.


*&---------------------------------------------------------------------*
*&      Selection Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
PARAMETERS:      P_TABNME LIKE DD03L-TABNAME OBLIGATORY.

PARAMETERS:      P_FILE   TYPE RLGRAP-FILENAME DEFAULT TEXT-004.
SELECTION-SCREEN SKIP 1.
PARAMETERS:      P_BUKRS  LIKE BKPF-BUKRS DEFAULT 'UGL',
                 P_GJAHR  LIKE BSEG-GJAHR.
SELECT-OPTIONS:  S_BUDAT  FOR  BKPF-BUDAT.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) TEXT-005.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(75) TEXT-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) TEXT-006.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) TEXT-007.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) TEXT-008.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) TEXT-009.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) TEXT-010.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) TEXT-011.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(72) TEXT-012.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX.

*&---------------------------------------------------------------------*
*&      Start of selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

OPEN DATASET P_FILE FOR OUTPUT IN TEXT MODE. " ENCODING DEFAULT.

PERFORM FM_DDIF_FIELDINFO_GET .

CASE P_TABNME.
     WHEN 'BKPF'.
           PERFORM GET_BKPF_DATA.
     WHEN 'SKAT'.
           PERFORM GET_SKAT_DATA.
     WHEN 'PRPS'.
           PERFORM GET_PRPS_DATA.
     WHEN 'LFA1'.
           PERFORM GET_LFA1_DATA.
     WHEN 'EKKO'.
           PERFORM GET_EKKO_DATA.
     WHEN 'EKPO'.
           PERFORM GET_EKPO_DATA.
     WHEN 'BSEG'.
           PERFORM GET_BSEG_DATA.
     WHEN  OTHERS.
           PERFORM INVALID_TABLE_SELECTED.
ENDCASE.
CLOSE DATASET P_FILE.

*&---------------------------------------------------------------------*
*&      Form  GET_BKPF_DATA
*&---------------------------------------------------------------------*
FORM GET_BKPF_DATA .

DATA: BEGIN OF ITAB OCCURS 1.
            INCLUDE STRUCTURE BKPF.
DATA: END OF ITAB.

SELECT * FROM BKPF
  INTO TABLE ITAB
 WHERE BUKRS = P_BUKRS
   AND BUDAT IN S_BUDAT.

PERFORM CREATE_FILE TABLES ITAB.

ENDFORM.                    " GET_BKPF_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_SKAT_DATA
*&---------------------------------------------------------------------*
FORM GET_SKAT_DATA .

DATA: BEGIN OF ITAB OCCURS 1.
            INCLUDE STRUCTURE SKAT.
DATA: END OF ITAB.

SELECT * FROM SKAT
  INTO TABLE ITAB
 WHERE SPRAS = 'EN'
   AND KTOPL = 'COAT'.

PERFORM CREATE_FILE TABLES ITAB.

ENDFORM.                    " GET_SKAT_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_PRPS_DATA
*&---------------------------------------------------------------------*
FORM GET_PRPS_DATA .

DATA: BEGIN OF ITAB OCCURS 1.
            INCLUDE STRUCTURE PRPS.
DATA: END OF ITAB.

SELECT * FROM PRPS
 WHERE PBUKR = P_BUKRS
   AND ERDAT IN S_BUDAT.

IF PRPS-POSID+5(2) CO '1234567890'.          "No templates
   MOVE-CORRESPONDING PRPS TO ITAB.
   WRITE PRPS-POSID TO ITAB-POSID.
   APPEND ITAB.
   CLEAR  ITAB.
ENDIF.

ENDSELECT.

PERFORM CREATE_FILE TABLES ITAB.

ENDFORM.                    " GET_PRPS_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_LFA1_DATA
*&---------------------------------------------------------------------*
FORM GET_LFA1_DATA .

DATA: BEGIN OF ITAB OCCURS 1.
            INCLUDE STRUCTURE LFA1.
DATA: END OF ITAB.

SELECT LIFNR INTO LFB1-LIFNR
  FROM LFB1
 WHERE BUKRS = P_BUKRS.

   SELECT * FROM LFA1
    WHERE   LIFNR = LFB1-LIFNR.

     MOVE-CORRESPONDING LFA1 TO ITAB.
     APPEND ITAB.
     CLEAR  ITAB.
   ENDSELECT.
ENDSELECT.

PERFORM CREATE_FILE TABLES ITAB.


ENDFORM.                    " GET_LFA1_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_EKKO_DATA
*&---------------------------------------------------------------------*
FORM GET_EKKO_DATA .

DATA: BEGIN OF ITAB OCCURS 1.
            INCLUDE STRUCTURE EKKO.
DATA: END OF ITAB.

SELECT * FROM EKKO
  INTO TABLE ITAB
 WHERE BUKRS = P_BUKRS
   AND AEDAT IN S_BUDAT.

PERFORM CREATE_FILE TABLES ITAB.


ENDFORM.                    " GET_EKKO_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_EKPO_DATA
*&---------------------------------------------------------------------*
FORM GET_EKPO_DATA .

DATA: BEGIN OF ITAB OCCURS 1.
            INCLUDE STRUCTURE EKPO.
DATA: END OF ITAB.

SELECT * FROM EKPO
  INTO TABLE ITAB
 WHERE BUKRS = P_BUKRS
   AND AEDAT IN S_BUDAT.

PERFORM CREATE_FILE TABLES ITAB.


ENDFORM.                    " GET_EKPO_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_BSEG_DATA
*&---------------------------------------------------------------------*
FORM GET_BSEG_DATA .

DATA: BEGIN OF ITAB OCCURS 1.
            INCLUDE STRUCTURE BSEG.
DATA: END OF ITAB.

*SELECT * UP TO 200 ROWS

SELECT * FROM BSEG
  INTO TABLE ITAB
 WHERE BUKRS = P_BUKRS
   AND GJAHR = P_GJAHR.

PERFORM CREATE_FILE TABLES ITAB.


ENDFORM.                    " GET_BSEG_DATA

*&---------------------------------------------------------------------*
*&      Form  INVALID_TABLE_SELECTED
*&---------------------------------------------------------------------*
FORM INVALID_TABLE_SELECTED .

MESSAGE E398 WITH 'Table' P_TABNME ' program code not developed yet'.

ENDFORM.                    " INVALID_TABLE_SELECTED

*&---------------------------------------------------------------------*
*&      Form  FM_DDIF_FIELDINFO_GET
*&---------------------------------------------------------------------*
*    Read table info
*----------------------------------------------------------------------*

FORM FM_DDIF_FIELDINFO_GET .

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME              = P_TABNME
*     FIELDNAME            = ' '
*      LANGU                = 'E'
*     LFIELDNAME           = ' '
*     ALL_TYPES            = ' '
*    IMPORTING
*     X030L_WA             =
*      DDOBJTYPE            = W_DDOBJTYPE
*     DFIES_WA             =
*     LINES_DESCR          =
    TABLES
      DFIES_TAB            = IT_DFIES
*     FIXED_VALUES         =
    EXCEPTIONS
      NOT_FOUND            = 1
      INTERNAL_ERROR       = 2
      OTHERS               = 3.
*
  IF SY-SUBRC <> 0.
     MESSAGE E398 WITH 'Table' P_TABNME ' does not exist'.
  ENDIF.

SORT IT_DFIES BY POSITION.
     LOOP AT IT_DFIES.
          IF IT_DFIES-POSITION > MAX_POSITION.
             MOVE IT_DFIES-POSITION TO MAX_POSITION.
          ENDIF.
     ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_FILE
*&---------------------------------------------------------------------*
FORM CREATE_FILE TABLES ITAB.
CLEAR: BIG_STRING.
COUNT = 1.
LOOP AT ITAB.
  DO MAX_POSITION TIMES.
     ASSIGN COMPONENT COUNT OF STRUCTURE ITAB TO <FS1>.
     READ TABLE IT_DFIES INDEX COUNT.
          CASE IT_DFIES-DATATYPE.
               WHEN 'CURR' OR 'QUAN' OR 'DEC'
                 OR 'INT1' OR 'INT2' OR 'INT4'.  " numeric
               WRITE <FS1> TO W_VALUE.
               CONDENSE W_VALUE.

               WHEN 'DATS'.  " date
               CONCATENATE <FS1>(4) <FS1>+4(2) <FS1>+6(2)
                      INTO W_VALUE.     " SEPARATED BY '/'.

               WHEN OTHERS.  " OTHERS
                    MOVE <FS1> TO W_VALUE.
          ENDCASE.

        IF COUNT = 1.
        MOVE W_VALUE TO BIG_STRING.
        ELSE.
        CONCATENATE BIG_STRING W_VALUE INTO BIG_STRING SEPARATED BY '|'.
        ENDIF.
        COUNT = COUNT + 1.
  ENDDO.
  TRANSFER BIG_STRING TO P_FILE.
  CLEAR: BIG_STRING.
  COUNT = 1.
ENDLOOP.

ENDFORM.                    " CREATE_FILE
