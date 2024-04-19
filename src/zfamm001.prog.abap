*****OBSOLETE*****
**REFER TO ZGGBS000
*PROGRAM RGGBS000 .
**---------------------------------------------------------------------*
**                                                                     *
**   Substitutions: EXIT-Formpool for Uxxx-Exits                       *
**                                                                     *
**   This formpool is used by SAP for testing purposes only.           *
**                                                                     *
**                                                                     *
**   Changes:                                                          *
**        By: Mohammad Khan.                                           *
**      Date: August 21, 2001.                                         *
**      Issue Log: 902                                                 *
**      Description: Add user exit U003 to accomodate Empire state     *
**                   historical and regular group asset assignment.    *
**      NOTE: The code related to historical assets may be removed     *
**            after successfully running it in production.             *
**---------------------------------------------------------------------*
*INCLUDE FGBBGD00.              "Standard data types
*
*TABLES: CSKS,
*        GLU1,
*        BKPF,
*        BSEG,
*        COBL.
*TABLES: ANLA,
*        ANLB.
*TABLES: AUFK .
*TABLES: COAS .
*TYPE-POOLS: GB002.
*
**----------------------------------------------------------------------*
**       FORM GET_EXIT_TITLES                                           *
**----------------------------------------------------------------------*
**       returns name and title of all available standard-exits         *
**       every exit in this formpool has to be added to this form       *
**----------------------------------------------------------------------*
**  -->  EXIT_TAB  table with exit-name and exit-titles                 *
**                 structure: NAME(5), PARAM(1), TITEL(60)
**----------------------------------------------------------------------*
*FORM GET_EXIT_TITLES TABLES ETAB.
*
*  DATA: BEGIN OF EXITS OCCURS 50,
*          NAME(5)   TYPE C,
*          PARAM     LIKE C_EXIT_PARAM_NONE,
*          TITLE(60) TYPE C,
*        END OF EXITS.
*
*  EXITS-NAME  = 'U001'.
*  EXITS-PARAM = C_EXIT_PARAM_NONE.
*  EXITS-TITLE = TEXT-001.             "Cost center from CSKS
*  APPEND EXITS.
*
*  EXITS-NAME  = 'U002'.
*  EXITS-PARAM = C_EXIT_PARAM_NONE.
*  EXITS-TITLE = TEXT-001.             "Cost center from CSKS
*  APPEND EXITS.
*
*  EXITS-NAME  = 'U003'.
*  EXITS-PARAM = C_EXIT_PARAM_NONE.
*  EXITS-TITLE = TEXT-001.             "Cost center from CSKS
*  APPEND EXITS.
*
*  EXITS-NAME  = 'U100'.
*  EXITS-PARAM = C_EXIT_PARAM_NONE.
*  EXITS-TITLE = TEXT-100.             "Cost center from CSKS
*  APPEND EXITS.
*
*  EXITS-NAME  = 'U101'.
*  EXITS-PARAM = C_EXIT_PARAM_FIELD.
*  EXITS-TITLE = TEXT-101.             "Cost center from CSKS
*  APPEND EXITS.
*
*  EXITS-NAME  = 'U102'.
*  EXITS-PARAM = C_EXIT_PARAM_CLASS.
*  EXITS-TITLE = TEXT-102.             "Sum is used for the reference.
*  APPEND EXITS.
*
*  EXITS-NAME  = 'U103'.
*  EXITS-PARAM = C_EXIT_PARAM_FIELD.
*  EXITS-TITLE = TEXT-101.             "COMPANY CODE CHANGE
*  APPEND EXITS.
*
*
*  REFRESH ETAB.
*  LOOP AT EXITS.
*    ETAB = EXITS.
*    APPEND ETAB.
*  ENDLOOP.
*
*ENDFORM.
*
*
** eject
**---------------------------------------------------------------------*
**       FORM U100                                                     *
**---------------------------------------------------------------------*
**       Reads the cost-center from the CSKS table .                   *
**---------------------------------------------------------------------*
*FORM U100.
*
*  SELECT * FROM CSKS
*            WHERE KOSTL EQ COBL-KOSTL
*              AND KOKRS EQ COBL-KOKRS.
*    IF CSKS-DATBI >= SY-DATUM AND
*       CSKS-DATAB <= SY-DATUM.
*
*      MOVE CSKS-ABTEI TO COBL-KOSTL.
*
*    ENDIF.
*  ENDSELECT.
*
*ENDFORM.
*
** eject
**---------------------------------------------------------------------*
**       FORM U101                                                     *
**---------------------------------------------------------------------*
**       Reads the cost-center from the CSKS table for accounting      *
**       area '0001'.                                                  *
**       This exit uses a parameter for the cost_center so it can      *
**       be used irrespective of the table used in the callup point.   *
**---------------------------------------------------------------------*
*FORM U101 USING COST_CENTER.
*
*  SELECT * FROM CSKS
*            WHERE KOSTL EQ COST_CENTER
*              AND KOKRS EQ '0001'.
*    IF CSKS-DATBI >= SY-DATUM AND
*       CSKS-DATAB <= SY-DATUM.
*
*      MOVE CSKS-ABTEI TO COST_CENTER .
*
*    ENDIF.
*  ENDSELECT.
*
*ENDFORM.
*
** eject
**---------------------------------------------------------------------*
**       FORM U102                                                     *
**---------------------------------------------------------------------*
**       Inserts the sum of the posting into the reference field.      *
**       This exit can be used in FI for the complete document.        *
**       The complete data is passed in one parameter.                 *
**---------------------------------------------------------------------*
*FORM U102 USING BOOL_DATA TYPE GB002_015.
*DATA: SUM(10) TYPE C.
*
*    LOOP AT BOOL_DATA-BSEG INTO BSEG
*                    WHERE    SHKZG = 'S'.
*       ADD BSEG-DMBTR TO SUM.
*    ENDLOOP.
*
*    BOOL_DATA-BKPF-XBLNR = TEXT-001.
*    REPLACE '&' WITH SUM INTO BOOL_DATA-BKPF-XBLNR.
*
*ENDFORM.
*
*
**-----------------------------------------------------------------------
**     FORM U001
**-----------------------------------------------------------------------
** Customer User Exit.
** - Defines rule for Substitutions.
**-----------------------------------------------------------------------
*FORM U001.
*
** rule applies only if asset is NOT a group asset...
*  IF ( ANLA-XANLGR = SPACE ).
**    ANLB-ANLGR+(5) = ANLA-ANLKL+3(5).       "4.6b + Issue log 902
*    MOVE ANLA-ANLKL+3(5) TO ANLB-ANLGR(5).   "4.6b + Issue log 902
*    ANLB-ANLGR+5 = '9900000'.
*  ENDIF.
*
*ENDFORM.
*
**-----------------------------------------------------------------------
**     FORM U002
**-----------------------------------------------------------------------
** Customer User Exit.
** - Defines rule for Substitutions.
**-----------------------------------------------------------------------
*FORM U002.
*
** rule applies only if asset is NOT a group asset...
*  IF ( ANLA-XANLGR = SPACE ).
**    ANLB-ANLGR+(5) = ANLA-ANLKL+3(5).       "4.6b + Issue log 902
*    MOVE ANLA-ANLKL+3(5) TO ANLB-ANLGR(5).   "4.6b + Issue log 902
*    ANLB-ANLGR+5 = '9800000'.
*  ENDIF.
*
*ENDFORM.
**-----------------------------------------------------------------------
**     FORM U003              Issue log 902
**-----------------------------------------------------------------------
** Customer User Exit.
** - Defines rule for Substitutions.
**-----------------------------------------------------------------------
*FORM U003.
*
** rule applies only if asset is NOT a group asset...
*  IF ( ANLA-XANLGR = SPACE ).
*    MOVE ANLA-ANLKL+3(5) TO ANLB-ANLGR(5).
*    IF ANLA-ANLN1 = '392305000001' AND
*       ( ANLA-ANLN2 = '1993' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900001'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305000002' AND
*       ( ANLA-ANLN2 = '1993' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900001'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305000003' AND
*       ( ANLA-ANLN2 = '1994' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900001'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305000004' AND
*       ( ANLA-ANLN2 = '1995' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900004'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305000005' AND
*       ( ANLA-ANLN2 = '1995' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900005'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305000006' AND
*       ( ANLA-ANLN2 = '1996' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900006'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305000007' AND
*       ( ANLA-ANLN2 = '1996' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900007'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305000008' AND
*       ( ANLA-ANLN2 = '1997' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900008'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305000009' AND
*       ( ANLA-ANLN2 = '1997' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900009'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305002289' AND
*       ( ANLA-ANLN2 = '1999' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900010'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305002290' AND
*       ( ANLA-ANLN2 = '1999' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900010'.
*    ELSE.
*    IF ANLA-ANLN1 = '392305002387' AND
*       ( ANLA-ANLN2 = '2000' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900011'.
*    ELSE.
*    IF ANLA-ANLN1 = '392405004389' AND
*       ( ANLA-ANLN2 = '1994' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900001'.
*    ELSE.
*    IF ANLA-ANLN1 = '392505005141' AND
*       ( ANLA-ANLN2 = '1994' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900001'.
*    ELSE.
*    IF ANLA-ANLN1 = '392505000002' AND
*       ( ANLA-ANLN2 = '1995' OR ANLA-ANLN2 = '0000' ).
*       ANLB-ANLGR+5 = '9900002'.
*    ELSE.
*       MOVE '990'         TO ANLB-ANLGR+5(3).
*       MOVE ANLA-AKTIV(4) TO ANLB-ANLGR+8.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                                         "Issue log 902
**-----------------------------------------------------------------------
**     FORM U103
**-----------------------------------------------------------------------
** Customer User Exit.
** - Defines rule for Substitutions.
**-----------------------------------------------------------------------
*FORM U103.
**  break-point 1.
**  aufk-bukrs = 'UGL' .
**  coas-bukrs = 'UGL' .
*ENDFORM.
