PROGRAM zggbs001 .
*---------------------------------------------------------------------*
* Corrections/ repair
* wms092357 070703 Note 638886: template routines to be used for
*                  workaround to substitute bseg-bewar from bseg-xref1/2
*---------------------------------------------------------------------*
*                                                                     *
*   Substitutions: EXIT-Formpool for Uxxx-Exits                       *
*                                                                     *
*   This formpool is used by SAP for testing purposes only.           *
*                                                                     *
*   Note: If you define a new user exit, you have to enter your       *
*         user exit in the form routine GET_EXIT_TITLES.              *
*                                                                     *
*---------------------------------------------------------------------*
*   Changes:                                                          *
*        By: Mohammad Khan.                                           *
*      Date: August 21, 2001.                                         *
*      Issue Log: 902                                                 *
*      Description: Add user exit U003 to accomodate Empire state     *
*                   historical and regular group asset assignment.    *
*      NOTE: The code related to historical assets may be removed     *
*            after successfully running it in production.             *
*                                                                     *
*    Brian Boundy                                                     *
*    Date: Feb 2013.                                                  *
*    Copy from RGGBS000 merge changes                                 *
*    Add Travel Substitution for BKTXT                                *
*---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 29-DEC-2020  KMB         D30K930791 CHG0201827 Assignment validation *
*                          D30K930834
*&---------------------------------------------------------------------*

INCLUDE fgbbgd00.              "Standard data types


*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
*    PLEASE INCLUDE THE FOLLOWING "TYPE-POOL"  AND "TABLES" COMMANDS  *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM         *
TYPE-POOLS: gb002. " TO BE INCLUDED IN                       "wms092357
TABLES: bkpf,      " ANY SYSTEM THAT                         "wms092357
        bseg,      " HAS 'FI' INSTALLED                      "wms092357
        cobl,                                               "wms092357
        csks,                                               "wms092357
        anlz,                                               "wms092357
        glu1.                                               "wms092357
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*

TABLES: anla,  "Issue log 902
        anlb.  "Issue log 902

*----------------------------------------------------------------------*
*       FORM GET_EXIT_TITLES                                           *
*----------------------------------------------------------------------*
*       returns name and title of all available standard-exits         *
*       every exit in this formpool has to be added to this form.      *
*       You have to specify a parameter type in order to enable the    *
*       code generation program to determine correctly how to          *
*       generate the user exit call, i.e. how many and what kind of    *
*       parameter(s) are used in the user exit.                        *
*       The following parameter types exist:                           *
*                                                                      *
*       TYPE                Description              Usage             *
*    ------------------------------------------------------------      *
*       C_EXIT_PARAM_NONE   Use no parameter         Subst. and Valid. *
*                           except B_RESULT                            *
*       C_EXIT_PARAM_FIELD  Use one field as param.  Only Substitution *
*       C_EXIT_PARAM_CLASS  Use a type as parameter  Subst. and Valid  *
*                                                                      *
*----------------------------------------------------------------------*
*  -->  EXIT_TAB  table with exit-name and exit-titles                 *
*                 structure: NAME(5), PARAM(1), TITEL(60)
*----------------------------------------------------------------------*
FORM get_exit_titles TABLES etab.

  DATA: BEGIN OF exits OCCURS 50,
          name(5)   TYPE c,
          param     LIKE c_exit_param_none,
          title(60) TYPE c,
        END OF exits.

  exits-name  = 'U001'.
  exits-param = c_exit_param_none.
  exits-title = text-001.
  APPEND exits.

  exits-name  = 'U002'.
  exits-param = c_exit_param_none.
  exits-title = text-002.
  APPEND exits.

  exits-name  = 'U003'.
  exits-param = c_exit_param_none.
  exits-title = text-003.
  APPEND exits.

  exits-name  = 'U100'.
  exits-param = c_exit_param_none.
  exits-title = text-100.             "Cost center from CSKS
  APPEND exits.

  exits-name  = 'U101'.
  exits-param = c_exit_param_field.
  exits-title = text-101.             "Cost center from CSKS
  APPEND exits.

  exits-name  = 'U102'.
  exits-param = c_exit_param_class.
  exits-title = text-102.             "Sum is used for the reference.
  APPEND exits.

* begin of insertion                                          "wms092357
  exits-name  = 'U200'.
  exits-param = c_exit_param_field.
  exits-title = text-200.             "Cons. transaction type
  APPEND exits.                       "from xref1/2
* end of insertion                                            "wms092357

  exits-name  = 'UTV1'.
  exits-param = c_exit_param_none.
  exits-title = text-tv1.
  APPEND exits.

* begin of changes by KMB on 29.12.2020 CHG0201827                                         "wms092357
  exits-name  = 'U300'.
  exits-param = c_exit_param_none.
  exits-title = text-300.
  APPEND exits.
* end of changes by KMB on 29.12.2020 CHG0201827


***********************************************************************
** EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
**
** PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
** TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
***********************************************************************
*  INCLUDE rggbs_ps_titles.

  REFRESH etab.
  LOOP AT exits.
    etab = exits.
    APPEND etab.
  ENDLOOP.

ENDFORM.                    "GET_EXIT_TITLES


*-----------------------------------------------------------------------
*     FORM U001
*-----------------------------------------------------------------------
* Customer User Exit.
* - Defines rule for Substitutions.
*-----------------------------------------------------------------------
FORM u001.

* rule applies only if asset is NOT a group asset...
  IF ( anla-xanlgr = space ).
*    ANLB-ANLGR+(5) = ANLA-ANLKL+3(5).       "4.6b + Issue log 902
    MOVE anla-anlkl+3(5) TO anlb-anlgr(5).   "4.6b + Issue log 902
    anlb-anlgr+5 = '9900000'.
  ENDIF.

ENDFORM.                    "U001

*-----------------------------------------------------------------------
*     FORM U002
*-----------------------------------------------------------------------
* Customer User Exit.
* - Defines rule for Substitutions.
*-----------------------------------------------------------------------
FORM u002.

* rule applies only if asset is NOT a group asset...
  IF ( anla-xanlgr = space ).
*    ANLB-ANLGR+(5) = ANLA-ANLKL+3(5).       "4.6b + Issue log 902
    MOVE anla-anlkl+3(5) TO anlb-anlgr(5).   "4.6b + Issue log 902
    anlb-anlgr+5 = '9800000'.
  ENDIF.

ENDFORM.                    "U002
*-----------------------------------------------------------------------
*     FORM U003              Issue log 902
*-----------------------------------------------------------------------
* Customer User Exit.
* - Defines rule for Substitutions.
*-----------------------------------------------------------------------
FORM u003.

* rule applies only if asset is NOT a group asset...
  IF ( anla-xanlgr = space ).
    MOVE anla-anlkl+3(5) TO anlb-anlgr(5).
    IF anla-anln1 = '392305000001' AND
       ( anla-anln2 = '1993' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900001'.
    ELSEIF anla-anln1 = '392305000002' AND
         ( anla-anln2 = '1993' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900001'.
    ELSEIF anla-anln1 = '392305000003' AND
         ( anla-anln2 = '1994' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900001'.
    ELSEIF anla-anln1 = '392305000004' AND
         ( anla-anln2 = '1995' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900004'.
    ELSEIF anla-anln1 = '392305000005' AND
         ( anla-anln2 = '1995' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900005'.
    ELSEIF anla-anln1 = '392305000006' AND
         ( anla-anln2 = '1996' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900006'.
    ELSEIF anla-anln1 = '392305000007' AND
         ( anla-anln2 = '1996' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900007'.
    ELSEIF anla-anln1 = '392305000008' AND
         ( anla-anln2 = '1997' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900008'.
    ELSEIF anla-anln1 = '392305000009' AND
         ( anla-anln2 = '1997' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900009'.
    ELSEIF anla-anln1 = '392305002289' AND
         ( anla-anln2 = '1999' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900010'.
    ELSEIF anla-anln1 = '392305002290' AND
         ( anla-anln2 = '1999' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900010'.
    ELSEIF anla-anln1 = '392305002387' AND
         ( anla-anln2 = '2000' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900011'.
    ELSEIF anla-anln1 = '392405004389' AND
         ( anla-anln2 = '1994' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900001'.
    ELSEIF anla-anln1 = '392505005141' AND
         ( anla-anln2 = '1994' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900001'.
    ELSEIF anla-anln1 = '392505000002' AND
         ( anla-anln2 = '1995' OR anla-anln2 = '0000' ).
      anlb-anlgr+5 = '9900002'.
    ELSE.
      MOVE '990'         TO anlb-anlgr+5(3).
      MOVE anla-aktiv(4) TO anlb-anlgr+8.
    ENDIF.
  ENDIF.
ENDFORM.                                         "Issue log 902

* eject
*---------------------------------------------------------------------*
*       FORM U100                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table .                   *
*---------------------------------------------------------------------*
FORM u100.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
  SELECT * FROM csks
            WHERE kostl EQ cobl-kostl
              AND kokrs EQ cobl-kokrs.
    IF csks-datbi >= sy-datum AND
       csks-datab <= sy-datum.

      MOVE csks-abtei TO cobl-kostl.

    ENDIF.
  ENDSELECT.

ENDFORM.                                                    "U100

* eject
*---------------------------------------------------------------------*
*       FORM U101                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table for accounting      *
*       area '0001'.                                                  *
*       This exit uses a parameter for the cost_center so it can      *
*       be used irrespective of the table used in the callup point.   *
*---------------------------------------------------------------------*
FORM u101 USING cost_center.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
  SELECT * FROM csks
            WHERE kostl EQ cost_center
              AND kokrs EQ '0001'.
    IF csks-datbi >= sy-datum AND
       csks-datab <= sy-datum.

      MOVE csks-abtei TO cost_center .

    ENDIF.
  ENDSELECT.

ENDFORM.                                                    "U101

* eject
*---------------------------------------------------------------------*
*       FORM U102                                                     *
*---------------------------------------------------------------------*
*       Inserts the sum of the posting into the reference field.      *
*       This exit can be used in FI for the complete document.        *
*       The complete data is passed in one parameter.                 *
*---------------------------------------------------------------------*


*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
FORM u102 USING bool_data TYPE gb002_015.
  DATA: sum(10) TYPE c.

  LOOP AT bool_data-bseg INTO bseg
                  WHERE    shkzg = 'S'.
*       BSEG-ZUONR = 'Test'.
*       MODIFY BOOL_DATA-BSEG FROM BSEG.
    ADD bseg-dmbtr TO sum.
  ENDLOOP.

  bkpf-xblnr = text-tot.
  REPLACE '&' WITH sum INTO bkpf-xblnr.

ENDFORM.                    "u102


***********************************************************************
** EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
**
** PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
** TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
***********************************************************************
*INCLUDE rggbs_ps_forms.


*eject
* begin of insertion                                          "wms092357
*&---------------------------------------------------------------------*
*&      Form  u200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM u200 USING e_rmvct TYPE bseg-bewar.
  PERFORM xref_to_rmvct USING bkpf bseg 1 CHANGING e_rmvct.
ENDFORM.                    "u200



*-----------------------------------------------------------------------
*     FORM UTV1
*-----------------------------------------------------------------------
* Customer User Exit.
* - Defines rule for Substitutions.
*-----------------------------------------------------------------------
FORM utv1.
  DATA: lv_name  TYPE pa0001-ename,
        lv_pernr TYPE pa0001-pernr,
        lv_awref TYPE ptrv_rot_awkey-awref.

  lv_awref = bkpf-awkey. "Get travel document number

  SELECT SINGLE pernr
    FROM ptrv_rot_awkey
    INTO lv_pernr
    WHERE awref = lv_awref.

  IF sy-subrc IS INITIAL.
    SELECT SINGLE ename
      FROM pa0001
      INTO lv_name
      WHERE pernr = lv_pernr
        AND begda <= sy-datum
        AND endda >= sy-datum
    .

    IF sy-subrc IS INITIAL.
      bkpf-bktxt = lv_name.
    ENDIF.
  ENDIF.
ENDFORM.                    "utv1

*&---------------------------------------------------------------------*
*&      Form  xref_to_rmvct
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM xref_to_rmvct
     USING    is_bkpf         TYPE bkpf
              is_bseg         TYPE bseg
              i_xref_field    TYPE i
     CHANGING c_rmvct         TYPE rmvct.

  DATA l_msgv TYPE symsgv.
  STATICS st_rmvct TYPE HASHED TABLE OF rmvct WITH UNIQUE DEFAULT KEY.

* either bseg-xref1 or bseg-xref2 must be used as source...
  IF i_xref_field <> 1 AND i_xref_field <> 2.
    MESSAGE x000(gk) WITH 'UNEXPECTED VALUE I_XREF_FIELD ='
      i_xref_field '(MUST BE = 1 OR = 2)' ''.
  ENDIF.
  IF st_rmvct IS INITIAL.
    SELECT trtyp FROM t856 INTO TABLE st_rmvct.
  ENDIF.
  IF i_xref_field = 1.
    c_rmvct = is_bseg-xref1.
  ELSE.
    c_rmvct = is_bseg-xref2.
  ENDIF.
  IF c_rmvct IS INITIAL.
    WRITE i_xref_field TO l_msgv LEFT-JUSTIFIED.
    CONCATENATE text-m00 l_msgv INTO l_msgv SEPARATED BY space.
*   cons. transaction type is not specified => send an error message...
    MESSAGE e123(g3) WITH l_msgv.
*   Bitte geben Sie im Feld &1 eine Konsolidierungsbewegungsart an
  ENDIF.
* c_rmvct <> initial...
  READ TABLE st_rmvct TRANSPORTING NO FIELDS FROM c_rmvct.
  CHECK NOT sy-subrc IS INITIAL.
* cons. transaction type does not exist => send error message...
  WRITE i_xref_field TO l_msgv LEFT-JUSTIFIED.
  CONCATENATE text-m00 l_msgv INTO l_msgv SEPARATED BY space.
  MESSAGE e124(g3) WITH c_rmvct l_msgv.
* KonsBewegungsart &1 ist ungültig (bitte Eingabe im Feld &2 korrigieren
ENDFORM.                    "xref_to_rmvct
* end of insertion                                            "wms092357

* begin of changes by KMB on 29.12.2020 CHG0201827
* eject
*---------------------------------------------------------------------*
*       FORM U300                                                     *
*---------------------------------------------------------------------*
*       "                   *
*---------------------------------------------------------------------*
FORM u300.

  DATA: gv_setid  LIKE sethier-setid,
        gt_values TYPE STANDARD TABLE OF rgsbv,
        gs_values TYPE rgsbv.

  CONSTANTS: gc_set TYPE c LENGTH 17 VALUE 'Z_REF_TRANSACTION'.

  CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
    EXPORTING
      shortname                = gc_set
    IMPORTING
      new_setid                = gv_setid
    EXCEPTIONS
      no_set_found             = 1
      no_set_picked_from_popup = 2
      wrong_class              = 3
      wrong_subclass           = 4
      table_field_not_found    = 5
      fields_dont_match        = 6
      set_is_empty             = 7
      formula_in_set           = 8
      set_is_dynamic           = 9
      OTHERS                   = 10.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'G_SET_FETCH'
      EXPORTING
        setnr           = gv_setid
      TABLES
        set_lines_basic = gt_values
      EXCEPTIONS
        no_authority    = 1
        set_is_broken   = 2
        set_not_found   = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      MESSAGE text-004 TYPE 'E'.
    ENDIF.
  ELSE.
    MESSAGE text-004 TYPE 'E'.
  ENDIF.

  SORT gt_values BY from.
  READ TABLE gt_values INTO gs_values WITH KEY from = bkpf-awtyp BINARY SEARCH.
  IF sy-subrc = 0 OR bseg-zuonr IS INITIAL.
    CONCATENATE 'YY' bkpf-bldat+0(4) INTO bseg-zuonr.
  ENDIF.

ENDFORM.                                                    "U300
* end of changes by KMB on 29.12.2020 CHG0201827
