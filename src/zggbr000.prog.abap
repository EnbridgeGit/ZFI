PROGRAM zggbr000 .
*---------------------------------------------------------------------*
*                                                                     *
*   Regeln: EXIT-Formpool for Uxxx-Exits                              *
*                                                                     *
*   Copied from RGGBR000                                              *
*                                                                     *
*   1999/07/21    J. Sennett             C11K909559/LN# 7076          *
*   1999/07/22    J. Sennett             C11K909583/LN# 7076          *
*   This formpool is a copy from RGGBR001.  I have added 2 new user-  *
*   exit at the end and they are called U200 and U201.  U200 is       *
*   created for CO side while U201 is created for FI side.  These     *
*   user-exits should fix problem where user was able to post to 2    *
*   different cost objects (i.e.  Cost Center and Non-stats Order).   *

*   1999/07/23    J. Sennett             C11K909587/LN# 7076          *
*   - Change user-exit U200 so that it won't check for both CO & PS   *
*     orders.  This check will be in the new user-exit U201 since we  *
*     need a new error message just for this.  U201 was the user-exit *
*     for FI side but found out we don't need it since U200 was able  *
*     to do the job completely.                                       *

*   1999/07/26    J. Sennett             C11K909599/LN# 7076          *
*   - Delete user-exit U201 since only one validation can be active   *
*     at one time.  So re-code user-exit U200.
*
*   2001/08/      G. Langenbacher        C11K911178
*     46C Upgrade.
* BTBOUNDY 2013/11/06 - Add UARI to check if vendor is ariba          *
* AKMADASU 2020/02/20 - Add new selective validation for WBS ending   *
*                       in 2221 and 2222- D30K930450                  *
*---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 29-DEC-2020  KMB         D30K930791 CHG0201827 Assignment validation *
*&---------------------------------------------------------------------*

* Reset INCLUDE to SAP standard for 46C upgrade             "46CUpgrade

INCLUDE fgbbgd00.
* End of change                                             "46CUpgrade
TYPE-POOLS: gb002.
TABLES: bkpf,
        bseg,
        cobl,
        aufk,
        proj,
        prps, "ADDED BY AKMADASU
        glu1.

*----------------------------------------------------------------------*
*       FORM GET_EXIT_TITLES                                           *
*----------------------------------------------------------------------*
*       returns name and title of all available standard-exits         *
*       every exit in this formpool has to be added to this form       *
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

  exits-name  = 'U100'.
  exits-param = c_exit_param_none.
  exits-title = text-100.                 "Posting date check
  APPEND exits.
**--start of changes by akmadasu
  exits-name  = 'U222'.
  exits-param = c_exit_param_none.
  exits-title = text-222.                 "WBS Validation
  APPEND exits.
  exits-name  = 'U223'.
  exits-param = c_exit_param_none.
  exits-title = text-223.                 "
  APPEND exits.
**-- end of chnages by akmadasu

  exits-name  = 'U101'.
  exits-param = c_exit_param_class.       "Complete data used in exit.
  exits-title = text-101.                 "Posting date check
  APPEND exits.

  exits-name  = 'U200'.
  exits-param = c_exit_param_none.
  exits-title = text-200.                 "CC with NonStat Order
  APPEND exits.

  exits-name  = 'UARI'.
  exits-param = c_exit_param_none.
  exits-title = text-ari.                 "Ariba Vendor Check
  APPEND exits.


* begin of changes by KMB on 29.12.2020 CHG0201827                                         "wms092357
  exits-name  = 'U300'.
  exits-param = c_exit_param_none.
  exits-title = text-302.
  APPEND exits.
* end of changes by KMB on 29.12.2020 CHG0201827


  REFRESH etab.
  LOOP AT exits.
    etab = exits.
    APPEND etab.
  ENDLOOP.

ENDFORM.                    "GET_EXIT_TITLES

*eject
*----------------------------------------------------------------------*
*       FORM U100                                                      *
*----------------------------------------------------------------------*
*       Example of an exit for a boolean rule                          *
*       This exit can be used in FI for callup points 1,2 or 3.        *
*----------------------------------------------------------------------*
*  <--  B_RESULT    T = True  F = False                                *
*----------------------------------------------------------------------*
FORM u100  USING b_result.

  IF sy-datum = bkpf-budat.
    b_result  = b_true.
  ELSE.
    b_result  = b_false.
  ENDIF.

ENDFORM.                    "U100
**-- start of changes by akmadasu
FORM u222  USING b_result.
**--START OF CHANGES BY AKMADASU FOR CHG0173962
  TYPES: BEGIN OF lty_xparam,
         paramtype  TYPE zparamtype,
         subtype    TYPE zparamsubtype,
         key1       TYPE zparamkey,
         value1     TYPE zparamvalue,
         END OF lty_xparam.
  DATA:      lt_xparam     TYPE TABLE OF lty_xparam,
             ls_xparam     TYPE lty_xparam,
             lr_key        TYPE RANGE OF zfit_xparam-value1,
             ls_key        LIKE LINE OF lr_key.
  CONSTANTS: lc_zu222      TYPE char5                VALUE 'ZU222',
             lc_valid_user TYPE char10               VALUE 'VALID_USER',
             lc_sign       TYPE sign                 VALUE 'I',
             lc_usr01      TYPE char5                VALUE 'USR01',
             lc_option     TYPE option               VALUE 'EQ'.
  REFRESH : lt_xparam.
  SELECT paramtype subtype key1 value1 FROM zfit_xparam INTO TABLE lt_xparam
                                       WHERE paramtype EQ lc_zu222
                                       AND   subtype   EQ lc_valid_user.
  IF sy-subrc IS INITIAL.
    SORT lt_xparam BY paramtype subtype.
  ENDIF.

  LOOP AT lt_xparam INTO ls_xparam.
    ls_key-low = ls_xparam-value1.
    ls_key-sign   = lc_sign.
    ls_key-option = lc_option.
    APPEND ls_key TO lr_key.
    CLEAR:ls_key.
  ENDLOOP.
**--END OF CHANGES BY AKMADASU FOR CHG0173962
  DATA:lv_posid TYPE prps-posid.
  CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
    EXPORTING
      input  = bseg-projk
    IMPORTING
      output = lv_posid.
  IF sy-subrc IS INITIAL AND
    " lv_posid+10(4) = '2222' " commented by akmadasu CHG0137998
    ( lv_posid+10(4) = '2222' OR lv_posid+10(4) = '2221' ). " added by akmadasu CHG0137998
**--START OF CHANGES BY AKMADASU FOR CHG0173962
    IF sy-uname IN lr_key.
    ELSE.
**--END OF CHANGES BY AKMADASU FOR CHG0173962
      b_result  = b_false.
    ENDIF. " ADDED BY AKMADASU FOR CHG0173962
  ENDIF.

ENDFORM.                    "u222
*&---------------------------------------------------------------------*
*&      Form  u223
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->B_RESULT   text
*----------------------------------------------------------------------*
FORM u223  USING b_result.
  break akmadasu.

  DATA: lv_objnr      TYPE j_objnr,
        lv_prps_objnr TYPE j_objnr,
        jest_tab TYPE TABLE OF jest_upd,
        jest_st  TYPE jest_upd.
  CLEAR: lv_objnr,lv_prps_objnr.
  SELECT SINGLE objnr
           FROM prps
           INTO lv_prps_objnr
          WHERE posid = proj-pspid.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE objnr
             FROM jest
             INTO lv_objnr
            WHERE objnr = lv_prps_objnr
            AND   stat  ='I0002'
            AND   inact = ' '.
    IF sy-subrc IS INITIAL.
      IF proj-plfaz IS NOT INITIAL AND proj-plsez IS NOT INITIAL.
        IF proj-eprog IS INITIAL.
          b_result  = b_false.
        ENDIF.
      ELSE.
        b_result  = b_false.
      ENDIF.
    ELSE.
      CALL FUNCTION 'STATUS_CHANGES_GET'
        TABLES
          t_changes = jest_tab.
      IF sy-subrc IS INITIAL.
        SORT jest_tab BY stat inact.
      ENDIF.
      READ TABLE jest_tab INTO jest_st
                          WITH KEY stat  = 'I0002'
                                   inact = ' '
                          BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF proj-plfaz IS NOT INITIAL.
          IF proj-eprog IS INITIAL.
            b_result  = b_false.
          ENDIF.
        ELSE.
          b_result  = b_false.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "u223
**- end of chnages by akmadasu
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
FORM u101 USING    bool_data TYPE gb002_015
          CHANGING b_result.
  DATA: b_acc_400000_used LIKE d_bool VALUE 'F'.

  b_result = b_true.

* Has account 400000 has been used?
  LOOP AT bool_data-bseg INTO bseg
                 WHERE hkont  = '0000400000'.
    b_acc_400000_used = b_true.
    EXIT.
  ENDLOOP.

* Check that account 400000 has been used.
  CHECK b_acc_400000_used = b_true.

  b_result = b_false.
  LOOP AT bool_data-bseg INTO bseg
                 WHERE hkont  = '0000399999'.
    b_result = b_true.
    EXIT.
  ENDLOOP.

ENDFORM.                    "U101

* eject
*---------------------------------------------------------------------*
*       FORM U200 - User-exit for CC and either CO order or PS order  *
*---------------------------------------------------------------------*
*       Validation check to see if cost center was entered along with *
*       a non-statistical order.                                      *
*---------------------------------------------------------------------*
FORM u200 USING b_result.

  DATA: f_ps_order LIKE proj-pspid.

  b_result = b_true.

  IF cobl-aufnr <> space AND cobl-nplnr <> space.         "CO & PS order
    b_result = b_false.
  ELSEIF cobl-aufnr <> space AND cobl-kostl <> space.     "CC & CO order
    SELECT SINGLE * FROM aufk
                    WHERE aufnr EQ cobl-aufnr.
    IF sy-subrc <> 0 OR aufk-astkz <> 'X'.
      b_result = b_false.
    ENDIF.
  ELSEIF cobl-nplnr <> space AND cobl-kostl <> space.     "CC & PS order
    CLEAR f_ps_order.
    f_ps_order+0(5) = cobl-nplnr+0(5).
    f_ps_order+5(1) = '%'.
    SELECT SINGLE * FROM proj
                    WHERE pspid LIKE f_ps_order.
    IF sy-subrc <> 0 OR proj-xstat <> 'X'.
      b_result = b_false.
    ENDIF.
  ENDIF.

ENDFORM.                    "U200

*---------------------------------------------------------------------*
*       FORM UARI - User-exit for Ariba Vendor Check                  *
*---------------------------------------------------------------------*
*       Validation check to see if Vendor is ariba.                   *
*---------------------------------------------------------------------*
FORM uari USING b_result.
  DATA ls_lfa1 LIKE lfa1.

  SELECT SINGLE lifnr emnfr
    FROM lfa1
    INTO CORRESPONDING FIELDS OF ls_lfa1
    WHERE lifnr = bseg-lifnr
      AND emnfr <> ''.

  IF sy-subrc = 0.
    b_result = b_false.
  ELSE.
    b_result = b_true.
  ENDIF.
ENDFORM.                    "UARI

* begin of changes by KMB on 29.12.2020 CHG0201827
*---------------------------------------------------------------------*
*       FORM U300                                                     *
*---------------------------------------------------------------------*
*       Deferal Validation for Assignment                             *
*---------------------------------------------------------------------*
FORM u300 USING b_result.

  DATA lv_len TYPE n.

  lv_len = strlen( bseg-zuonr ).
  b_result = b_true.
  IF bseg-zuonr IS INITIAL.
    b_result = b_false.
  ELSEIF lv_len > 6  "length is GT 6
         OR
         ( bseg-zuonr+0(2) <> 'YY' ) "first 2 letters not YY
         OR
         ( bseg-zuonr+0(2) = 'YY'    "first 2 letters YY and not in fiscal year range
         AND bseg-zuonr+2(1) CA '0123456789'
         AND bseg-zuonr+3(1) CA '0123456789'
         AND bseg-zuonr+4(1) CA '0123456789'
         AND bseg-zuonr+5(1) CA '0123456789' )
         AND ( bseg-zuonr+2(4) < 1900 OR
               bseg-zuonr+2(4) > 2100 )
         OR
         (   bseg-zuonr+2(1) CN '0123456789' "last 4 digits are not numbers
         OR  bseg-zuonr+3(1) CN '0123456789'
         OR  bseg-zuonr+4(1) CN '0123456789'
         OR  bseg-zuonr+5(1) CN '0123456789' ) .
    b_result = b_false.
  ENDIF.
  CLEAR lv_len.
ENDFORM.                    "U300
* end of changes by KMB on 29.12.2020 CHG0201827
