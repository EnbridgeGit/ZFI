PROGRAM ZGGBR020 .
*---------------------------------------------------------------------*
*                                                                     *
*   Regeln: EXIT-Formpool for Uxxx-Exits                              *
*                                                                     *
*   This formpool is used by SAP for demonstration purposes only.     *
*                                                                     *
*---------------------------------------------------------------------*
INCLUDE FGBBGD00.               "Data types
TYPE-POOLS: GB002.
TABLES: BKPF,
        BSEG,
        COBL,
        GLU1.

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
  EXITS-TITLE = TEXT-100.                 "Posting date check
  APPEND EXITS.

  EXITS-NAME  = 'U101'.
  EXITS-PARAM = C_EXIT_PARAM_CLASS.       "Complete data used in exit.
  EXITS-TITLE = TEXT-101.                 "Posting date check
  APPEND EXITS.

  REFRESH ETAB.
  LOOP AT EXITS.
    ETAB = EXITS.
    APPEND ETAB.
  ENDLOOP.

ENDFORM.

*eject
*----------------------------------------------------------------------*
*       FORM U100                                                      *
*----------------------------------------------------------------------*
*       Example of an exit for a boolean rule                          *
*       This exit can be used in FI for callup points 1,2 or 3.        *
*----------------------------------------------------------------------*
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*
FORM U100  USING B_RESULT.

  IF SY-DATUM = BKPF-BUDAT.
    B_RESULT  = B_TRUE.
  ELSE.
    B_RESULT  = B_FALSE.
  ENDIF.

ENDFORM.

*eject
*----------------------------------------------------------------------*
*       FORM U101                                                      *
*----------------------------------------------------------------------*
*       Example of an exit using the complete data from one            *
*       multi-line rule.                                               *
*       This exit is intended for use from callup point 3, in FI.      *
*                                                                      *
*       If account 400000 is used, then account 399999 must be posted  *
*       to in another posting line.                                    *
*----------------------------------------------------------------------*
*  -->  BOOL_DATA   The complete posting data.                         *
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*
FORM U101 USING    BOOL_DATA TYPE GB002_015
          CHANGING B_RESULT.
  DATA: B_ACC_400000_USED LIKE D_BOOL VALUE 'F'.

  B_RESULT = B_TRUE.

* Has account 400000 has been used?
  LOOP AT BOOL_DATA-BSEG INTO BSEG
                 WHERE HKONT  = '0000400000'.
     B_ACC_400000_USED = B_TRUE.
     EXIT.
  ENDLOOP.

* Check that account 400000 has been used.
  CHECK B_ACC_400000_USED = B_TRUE.

  B_RESULT = B_FALSE.
  LOOP AT BOOL_DATA-BSEG INTO BSEG
                 WHERE HKONT  = '0000399999'.
     B_RESULT = B_TRUE.
     EXIT.
  ENDLOOP.

ENDFORM.
