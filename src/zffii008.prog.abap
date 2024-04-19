REPORT ZFFII008 LINE-SIZE  79
                LINE-COUNT 59
                MESSAGE-ID 00.
*---------------------------------------------------------------------*
*       REPORT ZFFII008                                               *
*       AUTHOR M. Khan                                                *
*       DATE   APRIL 18, 2002.                                        *
*---------------------------------------------------------------------*
*   This program creates a BDC session   for  SAP    Change posting   *
*   periods (Transaction - OB52). The posting periods will be updated *
*   according to the information supplied through VARIANTS.           *
*   If the key fields supplied through variants for update of a record*
*   are not found in table T001B, there will be no action for it.     *
*---------------------------------------------------------------------*
*CHANGES:
*
* BY:          ISSUE:  DATE:        DESCRIPTION
*Mohammad Khan  771   9/11/04  Update the year filed as well.
*
*---------------------------------------------------------------------*

TABLES:
  T001B.        "Permitted Posting Periods

DATA:
  FIRST_TIME VALUE 'Y'.

* batch input data
DATA: BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA:   R_FRPE1 LIKE T001B-FRPE1,
        R_FYEAR LIKE T001B-TOYE1,
        ZSUBRC  TYPE I,                  "return code
        ANY_UPDATES(1)   VALUE 'N'.      "update indicator

RANGES: R_VKONT  FOR T001B-VKONT,
        R_BKONT  FOR T001B-BKONT,
        R_MKOAR  FOR T001B-MKOAR,
        R_BUKRS  FOR T001B-BUKRS.

*------------------------ Selection Screen  ---------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX0 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
PARAMETERS:     VCBOX1 AS CHECKBOX.
SELECT-OPTIONS: S1_BUKRS  FOR  T001B-BUKRS,        "Company Code
                S1_MKOAR  FOR  T001B-MKOAR,        "Account Type
                S1_VKONT  FOR  T001B-VKONT,        "From Account #
                S1_BKONT  FOR  T001B-BKONT.        "To Account #
PARAMETERS:     S1_FRPE1  LIKE T001B-FRPE1.        "From Period (update)
PARAMETERS:     S1_FYEAR  LIKE T001B-TOYE1.      "I771 -From Year update
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
PARAMETERS:     VCBOX2 AS CHECKBOX.
SELECT-OPTIONS: S2_BUKRS  FOR  T001B-BUKRS,        "Company Code
                S2_MKOAR  FOR  T001B-MKOAR,        "Account Type
                S2_VKONT  FOR  T001B-VKONT,        "From Account #
                S2_BKONT  FOR  T001B-BKONT.        "To Account #
PARAMETERS:     S2_FRPE1  LIKE T001B-FRPE1.        "From Period (update)
PARAMETERS:     S2_FYEAR  LIKE T001B-TOYE1.      "I771 -From Year update
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
PARAMETERS:     VCBOX3 AS CHECKBOX.
SELECT-OPTIONS: S3_BUKRS  FOR  T001B-BUKRS,        "Company Code
                S3_MKOAR  FOR  T001B-MKOAR,        "Account Type
                S3_VKONT  FOR  T001B-VKONT,        "From Account #
                S3_BKONT  FOR  T001B-BKONT.        "To Account #
PARAMETERS:     S3_FRPE1  LIKE T001B-FRPE1.        "From Period (update)
PARAMETERS:     S3_FYEAR  LIKE T001B-TOYE1.      "I771 -From Year update
SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN END OF BLOCK BOX0.

*----------------------------- MAINLINE --------------------------------
START-OF-SELECTION.
REFRESH BDCDATA.

PERFORM SCREEN_HEADER USING 'SAPLSPO4' '0300' 'X'.
PERFORM SCREEN_FIELD  USING 'BDC_CURSOR' 'SVALD-VALUE(01)'.
PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' 'FURT'. "skip first screen.

IF VCBOX1 = 'X'.
   CLEAR: R_BUKRS, R_MKOAR, R_VKONT, R_BKONT, R_FRPE1.
   MOVE S1_BUKRS[] TO R_BUKRS[].
   MOVE S1_MKOAR[] TO R_MKOAR[].
   MOVE S1_VKONT[] TO R_VKONT[].
   MOVE S1_BKONT[] TO R_BKONT[].
   MOVE S1_FRPE1 TO R_FRPE1.
   MOVE S1_FYEAR TO R_FYEAR.                             "I771
   PERFORM BUILD_SESSION.
ENDIF.

IF VCBOX2 = 'X'.
   CLEAR: R_BUKRS, R_MKOAR, R_VKONT, R_BKONT, R_FRPE1.
   REFRESH: R_BUKRS, R_VKONT, R_BKONT, R_MKOAR.
   MOVE S2_BUKRS[] TO R_BUKRS[].
   MOVE S2_MKOAR[] TO R_MKOAR[].
   MOVE S2_VKONT[] TO R_VKONT[].
   MOVE S2_BKONT[] TO R_BKONT[].
   MOVE S2_FRPE1 TO R_FRPE1.
   MOVE S2_FYEAR TO R_FYEAR.                             "I771
   PERFORM BUILD_SESSION.
ENDIF.

IF VCBOX3 = 'X'.
   CLEAR: R_BUKRS, R_MKOAR, R_VKONT, R_BKONT, R_FRPE1.
   REFRESH: R_BUKRS, R_VKONT, R_BKONT, R_MKOAR.
   MOVE S3_BUKRS[] TO R_BUKRS[].
   MOVE S3_MKOAR[] TO R_MKOAR[].
   MOVE S3_VKONT[] TO R_VKONT[].
   MOVE S3_BKONT[] TO R_BKONT[].
   MOVE S3_FRPE1 TO R_FRPE1.
   MOVE S3_FYEAR TO R_FYEAR.                             "I771
   PERFORM BUILD_SESSION.
ENDIF.

   IF ANY_UPDATES = 'Y'.
    PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' '/3'.

    PERFORM SCREEN_HEADER USING 'SAPLSPO1' '0100' 'X'.

    PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' 'YES'.

      PERFORM EXECUTE_SESSION.
      IF ZSUBRC <> 0.
         MESSAGE E368 WITH 'Error in Updating Posting Periods.'.
      ELSE.
         MESSAGE I368 WITH 'Posting Periods Updated.'.
      ENDIF.
   ELSE.
      MESSAGE I368 WITH 'No Posting Periods Updates to Perform.'.
   ENDIF.



************************************************************************
*                 FORM BUILD_SESSION
************************************************************************
FORM BUILD_SESSION.
  SELECT * FROM T001B
         WHERE BUKRS IN R_BUKRS
           AND MKOAR IN R_MKOAR
           AND VKONT IN R_VKONT
           AND BKONT IN R_BKONT.
    IF SY-SUBRC = 0.
    MOVE 'Y' TO ANY_UPDATES.

    PERFORM SCREEN_HEADER USING 'SAPL0F00' '65' 'X'.

    PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' 'POSI'.   "add entries

    PERFORM SCREEN_HEADER USING 'SAPLSPO4' '0300' 'X'.

    PERFORM SCREEN_FIELD  USING 'SVALD-VALUE(1)' T001B-BUKRS.

    PERFORM SCREEN_FIELD  USING 'SVALD-VALUE(2)' T001B-MKOAR.

    PERFORM SCREEN_FIELD  USING 'SVALD-VALUE(3)' T001B-BKONT.

    PERFORM SCREEN_HEADER USING 'SAPL0F00' '65' 'X'.

    PERFORM SCREEN_FIELD  USING 'V_T001B-FRPE1(1)'  R_FRPE1.

*IF R_FYEAR <> SPACE.    "changes requested by Sanjeev 12/2005 "I771
* IF R_FYEAR >= SY-DATUM+0(4).                                 "I771
    PERFORM SCREEN_FIELD  USING 'V_T001B-FRYE1(1)'  R_FYEAR.  "I771
*    PERFORM SCREEN_FIELD  USING 'V_T001B-TOYE1(1)'  R_FYEAR.  "I771
* ENDIF.                                                       "I771
*ENDIF.                                                        "I771
    PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' 'SAVE'.

    ENDIF.
  ENDSELECT.

ENDFORM.

*----------------------------------------------------------------------
* form screen_header ... create BDC header entry using parameters
*                        (1) program   ...  screen program
*                        (2) screen    ...  screen number
*                        (3) indicator ...  indicator
*----------------------------------------------------------------------
FORM SCREEN_HEADER USING PROGRAM SCREEN INDICATOR.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = SCREEN.
  BDCDATA-DYNBEGIN = INDICATOR.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------
* form screen_field ... create BDC entry using parameters
*                       (1) fnam ... field name to fill
*                       (2) fval ... value to fill in
*----------------------------------------------------------------------
FORM SCREEN_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------
* form execute_session ... execute transaction OB52 using
*                  (1) bdcdata as data source
*                  (2) in no display mode
*                  (3) in synchronous update mode
*----------------------------------------------------------------------
FORM EXECUTE_SESSION.
   CALL TRANSACTION 'OB52' USING BDCDATA         "data source
*                         MODE 'Y'              "Y - Display
                         MODE 'N'              "N - no display
                           UPDATE 'S'.           "S - synchronous update
   MOVE SY-SUBRC TO ZSUBRC.
ENDFORM.
*----------------------------------------------------------------------
