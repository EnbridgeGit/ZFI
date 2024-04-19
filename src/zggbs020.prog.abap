PROGRAM ZGGBS030 .
*---------------------------------------------------------------------*
*                                                                     *
*   Substitutions: EXIT-Formpool for Uxxx-Exits                       *
*                                                                     *
*                                                                     *
*                                                                     *
*---------------------------------------------------------------------*
INCLUDE FGBBGD00.              "Standard data types

TABLES: CSKS,
        GLU1,
        BKPF,
        BSEG,
        COBL.

TABLES: AUFK .
TYPE-POOLS: GB002.

*----------------------------------------------------------------------*
*       FORM GET_EXIT_TITLES                                           *
*----------------------------------------------------------------------*
*       returns name and title of all available standard-exits         *
*       every exit in this formpool has to be added to this form       *
*----------------------------------------------------------------------*
*  -->  EXIT_TAB  table with exit-name and exit-titles                 *
*                 structure: NAME(5), PARAM(1), TITEL(60)
*----------------------------------------------------------------------*
FORM GET_EXIT_TITLES TABLES ETAB.

  DATA: BEGIN OF EXITS OCCURS 50,
          NAME(5)   TYPE C,
          PARAM     LIKE C_EXIT_PARAM_NONE,
          TITLE(60) TYPE C,
        END OF EXITS.

  EXITS-NAME  = 'U100'.
  EXITS-PARAM = C_EXIT_PARAM_NONE.
  EXITS-TITLE = TEXT-100.             "Cost center from CSKS
  APPEND EXITS.

  EXITS-NAME  = 'U101'.
  EXITS-PARAM = C_EXIT_PARAM_FIELD.
  EXITS-TITLE = TEXT-101.             "Cost center from CSKS
  APPEND EXITS.

  EXITS-NAME  = 'U102'.
  EXITS-PARAM = C_EXIT_PARAM_CLASS.
  EXITS-TITLE = TEXT-102.             "Sum is used for the reference.
  APPEND EXITS.

  EXITS-NAME  = 'U103'.
  EXITS-PARAM = C_EXIT_PARAM_FIELD.
  EXITS-TITLE = TEXT-103.             "Change company code to UGL
  APPEND EXITS.

  REFRESH ETAB.
  LOOP AT EXITS.
    ETAB = EXITS.
    APPEND ETAB.
  ENDLOOP.

ENDFORM.


* eject
*---------------------------------------------------------------------*
*       FORM U100                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table .                   *
*---------------------------------------------------------------------*
FORM U100.

  SELECT * FROM CSKS
            WHERE KOSTL EQ COBL-KOSTL
              AND KOKRS EQ COBL-KOKRS.
    IF CSKS-DATBI >= SY-DATUM AND
       CSKS-DATAB <= SY-DATUM.

      MOVE CSKS-ABTEI TO COBL-KOSTL.

    ENDIF.
  ENDSELECT.

ENDFORM.

* eject
*---------------------------------------------------------------------*
*       FORM U101                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table for accounting      *
*       area '0001'.                                                  *
*       This exit uses a parameter for the cost_center so it can      *
*       be used irrespective of the table used in the callup point.   *
*---------------------------------------------------------------------*
FORM U101 USING COST_CENTER.

  SELECT * FROM CSKS
            WHERE KOSTL EQ COST_CENTER
              AND KOKRS EQ '0001'.
    IF CSKS-DATBI >= SY-DATUM AND
       CSKS-DATAB <= SY-DATUM.

      MOVE CSKS-ABTEI TO COST_CENTER .

    ENDIF.
  ENDSELECT.

ENDFORM.

* eject
*---------------------------------------------------------------------*
*       FORM U102                                                     *
*---------------------------------------------------------------------*
*       Inserts the sum of the posting into the reference field.      *
*       This exit can be used in FI for the complete document.        *
*       The complete data is passed in one parameter.                 *
*---------------------------------------------------------------------*
FORM U102 USING BOOL_DATA TYPE GB002_015.
DATA: SUM(10) TYPE C.

    LOOP AT BOOL_DATA-BSEG INTO BSEG
                    WHERE    SHKZG = 'S'.
       ADD BSEG-DMBTR TO SUM.
    ENDLOOP.

    BOOL_DATA-BKPF-XBLNR = TEXT-001.
    REPLACE '&' WITH SUM INTO BOOL_DATA-BKPF-XBLNR.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM U103                                                     *
*---------------------------------------------------------------------*
*       Change the company code to UGL on the Order Master     .      *
*---------------------------------------------------------------------*
FORM U103  .
BREAK-POINT 1.
   AUFK-BUKRS = 'UGL' .
ENDFORM.
