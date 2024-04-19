*&---------------------------------------------------------------------*
*&  Include           ZFAPR021_IN_HOUSE_CHECK_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPR021_IN_HOUSE_CHECK                       *
*& Include Name       :  ZFAPR021_IN_HOUSE_CHECK_F01                   *
*& Author             :  Babumiya Mohammad                             *
*& Creation Date      :  23-Aug-2011                                   *
*& Object ID          :  F_P2C_AP_003_In_House_Check_(AP) US Instance  *
*& Application Area   :  FI-AP                                         *
*& Description        :  In House Check Printing                       *
*&                       Copy of US program ZFAPF003_IN_HOUSE_CHECK    *
*&                       Forms Include - Subroutines                   *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          : 23-Aug-2011                                          *
* Modified By   : Babumiya Mohammad                                    *
* Correction No : DECK900992                                           *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 2                                                    *
* Date          : 20-Aug-2013                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K922271, D30K923471, D30K924001                   *
* Description   : SDP42671-Convert to UG systems                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 3                                                    *
* Date          : 02-Jan-2015                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K925153, D30K925270, D30K925486                   *
* Description   : SDP42671 1. Add invoice header long text             *
*                          2. Correct payment advice total/subtotal    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 4                                                    *
* Date          : 04-Feb-2016                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K926579                                           *
* Description   : ACR-307  1. Change the address of the Spectra office *
*                             location that is printed on the check    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 5                                                    *
* Date          : 25-Jan-2017                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K927881                                           *
* Description   : ACR-2755 1. Enhancements for Enbridge integration    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 6                                                    *
* Date          : 10-Apr-2017                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K928144                                           *
* Description   : ACR-4084 JPMC In-House Checks - format remittance text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 7                                                    *
* Date          : 28-Mar-2019                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K929884                                           *
* Description   : CHG0148429 - Incorporate TD Bank                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 8                                                    *
* Date          : 10-Jun-2019                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K930133                                           *
* Description   : CHG0148429 - Select Options for House Bank and Accnt *
*                              Add option to override Company Name     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 9                                                    *
* Date          : 20-Sep-2019                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K930167                                           *
* Description   : CHG0160810 - Add US Check Form to SW and UG          *
*                              Add Payment Method P "Post-Dated"       *
*----------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  f_disable_input_fields
*&---------------------------------------------------------------------*
*       Disable the selection-screen inputs
*----------------------------------------------------------------------*
FORM f_disable_input_fields.

  CONSTANTS:
        lc_rzawe         TYPE char7
                         VALUE 'P_RZAWE',
        lc_frmnm         TYPE char7                         "D30K929884
                         VALUE 'P_FRMNM',                   "D30K929884
        lc_voidd         TYPE char7
                         VALUE 'P_VOIDD',
        lc_cashd         TYPE char7
                         VALUE 'P_CASHD',
        lc_pridt         TYPE char7
                         VALUE 'P_PRIDT'.

  LOOP AT SCREEN.

    IF     ( ( screen-name     EQ lc_rzawe ) OR             "D30K929884
             ( screen-name     EQ lc_frmnm )    ).          "D30K929884

      CLEAR                       screen-input.
      MOVE     '0'             TO screen-input.

      MODIFY SCREEN.

    ELSEIF ( ( screen-name     CS lc_voidd ) OR
             ( screen-name     CS lc_cashd ) OR
             ( screen-name     CS lc_pridt )    ).

      CLEAR                       screen-input.
      MOVE     '0'             TO screen-input.
      CLEAR                       screen-invisible.
      MOVE     '1'             TO screen-invisible.

      MODIFY SCREEN.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_disable_input_fields
*eject
*&---------------------------------------------------------------------*
*&      Form  f_f4_help_laufd_laufi
*&---------------------------------------------------------------------*
*       Get the input values for the run date and run ID
*----------------------------------------------------------------------*
FORM f_f4_help_laufd_laufi.

  CONSTANTS:
        lc_f1typ         TYPE char1
                         VALUE 'D',
        lc_f2nme         TYPE char11
                         VALUE 'F110V-LAUFI'.

  CALL FUNCTION 'F4_ZAHLLAUF'
    EXPORTING
      f1typ = lc_f1typ
      f2nme = lc_f2nme
    IMPORTING
      laufd = p_laufd
      laufi = p_laufi.

ENDFORM.                    " f_f4_help_laufd_laufi
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_company_code
*&---------------------------------------------------------------------*
*       Validate the paying company code
*----------------------------------------------------------------------*
FORM f_validate_company_code.

  DATA:    lit_bukrs     TYPE STANDARD TABLE OF bukrs.

  IF     ( s_zbukr[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lit_bukrs[].
  SELECT   bukrs
    INTO   TABLE lit_bukrs
    FROM   t001
   WHERE   bukrs IN s_zbukr.
  IF ( sy-subrc NE 0 ).
    CLEAR  lit_bukrs[].
    MESSAGE  e000 WITH text-201.
  ENDIF.

ENDFORM.                    " f_validate_company_code
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_house_bank_id
*&---------------------------------------------------------------------*
*       Validate the house bank ID
*----------------------------------------------------------------------*
FORM f_validate_house_bank_id.

  DATA:    lit_hbkid     TYPE STANDARD TABLE OF hbkid.

  IF     ( p_hbkid IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lit_hbkid[].
  SELECT   hbkid
    INTO   TABLE lit_hbkid
    FROM   t012
   WHERE   bukrs IN s_zbukr
     AND   hbkid  = p_hbkid.
  IF ( sy-subrc NE 0 ).
    CLEAR  lit_hbkid[].
    MESSAGE  e000 WITH text-202.
  ENDIF.

ENDFORM.                    " f_validate_house_bank_id
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_account_id
*&---------------------------------------------------------------------*
*       Validate the account ID
*----------------------------------------------------------------------*
FORM f_validate_account_id.

  DATA:    lit_hktid     TYPE STANDARD TABLE OF hktid.

  IF     ( p_hktid IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lit_hktid[].
  SELECT   hktid
    INTO   TABLE lit_hktid
    FROM   t012k
   WHERE   bukrs IN s_zbukr
     AND   hbkid  = p_hbkid
     AND   hktid  = p_hktid.
  IF ( sy-subrc NE 0 ).
    CLEAR  lit_hktid[].
    MESSAGE  e000 WITH text-203.
  ENDIF.

ENDFORM.                    " f_validate_account_id
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_form
*&---------------------------------------------------------------------*
*       Validate the SAPScript form
*----------------------------------------------------------------------*
FORM f_validate_form.

  DATA: lv_obj_name      TYPE sobj_name.

  CONSTANTS:
        lc_pgmid_r3tr    TYPE pgmid
                         VALUE 'R3TR',
        lc_object_form   TYPE trobjtype
                         VALUE 'FORM'.

  IF     ( p_frmnm IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lv_obj_name.
  SELECT   SINGLE obj_name
    INTO   lv_obj_name
    FROM   tadir
   WHERE   pgmid      = lc_pgmid_r3tr
     AND   object     = lc_object_form
     AND   obj_name   = p_frmnm.
  IF ( sy-subrc NE 0 ).
    CLEAR  lv_obj_name.
    MESSAGE  e000 WITH text-204.
  ENDIF.

ENDFORM.                    " f_validate_form
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_payment_method                          "D30K929884
*&---------------------------------------------------------------------*
*       Set the payment method
*----------------------------------------------------------------------*
FORM f_set_payment_method                                   "D30K929884
  USING    i_flag TYPE char1.

  IF   ( ( i_flag                        IS INITIAL     ) AND
         ( p_rzawe                       IS NOT INITIAL )     ).
    RETURN.
  ENDIF.

  CLEAR                                     p_rzawe.

  IF     ( rb_bjpmt                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_rzawe_jpmc_ihc             TO p_rzawe.        "D30K930167
  ELSEIF ( rb_btd8c                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_rzawe_td_ihc               TO p_rzawe.        "D30K930167
  ELSEIF ( rb_btd8u                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_rzawe_td_ihc               TO p_rzawe.        "D30K930167
  ELSEIF ( rb_btdpc                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_rzawe_td_pdc               TO p_rzawe.        "D30K930167
  ELSEIF ( rb_btdpu                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_rzawe_td_pdc               TO p_rzawe.        "D30K930167
  ENDIF.                                                    "D30K930167

ENDFORM.                    " f_set_payment_method          "D30K929884
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_check_form                              "D30K929884
*&---------------------------------------------------------------------*
*       Set the check form
*----------------------------------------------------------------------*
FORM f_set_check_form                                       "D30K929884
  USING    i_flag TYPE char1.

  IF   ( ( i_flag                        IS INITIAL     ) AND
         ( p_frmnm                       IS NOT INITIAL )     ).
    RETURN.
  ENDIF.

  CLEAR                                     p_frmnm.

  IF     ( rb_bjpmt                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_frmnm_jpmc_ck2             TO p_frmnm.        "D30K930167
  ELSEIF ( rb_btd8c                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_frmnm_td_ca                TO p_frmnm.        "D30K930167
  ELSEIF ( rb_btd8u                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_frmnm_td_us                TO p_frmnm.        "D30K930167
  ELSEIF ( rb_btdpc                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_frmnm_td_ca                TO p_frmnm.        "D30K930167
  ELSEIF ( rb_btdpu                      IS NOT INITIAL ).  "D30K930167
    MOVE   gc_frmnm_td_us                TO p_frmnm.        "D30K930167
  ENDIF.                                                    "D30K930167

ENDFORM.                    " f_set_check_form              "D30K929884
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  DATA:    lrw_hbkid LIKE LINE OF grt_hbkid,
           lrw_hktid LIKE LINE OF grt_hktid.

  CLEAR    git_xparam[].                                    "D30K929884
  CLEAR    git_reguh[].
  CLEAR    git_regup[].
  CLEAR    git_payr[].
  CLEAR    git_t001[].
  CLEAR    git_adrc[].
  CLEAR    git_our_addr[].                                  "D30K926579
  CLEAR    git_checks[].
  CLEAR    grt_hbkid[].
  CLEAR    grt_hktid[].

  CLEAR    gv_spool.
  CLEAR    gv_nodata.

  IF       ( p_hbkid            IS NOT INITIAL ).
    CLEAR                              lrw_hbkid.
    MOVE     'I'                    TO lrw_hbkid-sign.
    MOVE     'EQ'                   TO lrw_hbkid-option.
    MOVE     p_hbkid                TO lrw_hbkid-low.
    APPEND                             lrw_hbkid
                                    TO grt_hbkid.
  ENDIF.

  IF       ( p_hktid            IS NOT INITIAL ).
    CLEAR                              lrw_hktid.
    MOVE     'I'                    TO lrw_hktid-sign.
    MOVE     'EQ'                   TO lrw_hktid-option.
    MOVE     p_hktid                TO lrw_hktid-low.
    APPEND                             lrw_hktid
                                    TO grt_hktid.
  ENDIF.

  IF       ( p_tstrn                IS INITIAL ).
    CLEAR                              sy-title.
    MOVE     text-rt1               TO sy-title.
  ELSE.
    CLEAR                              sy-title.
    MOVE     text-rt2               TO sy-title.
  ENDIF.

* Select the program control parameters                     "D30K929884
  SELECT   *                                                "D30K929884
    INTO   TABLE git_xparam                                 "D30K929884
    FROM   zfit_xparam                                      "D30K929884
   WHERE   paramtype = gc_paramtype_thba                    "D30K929884
     AND   subtype  IN (gc_subtype_thba, gc_subtype_tcno).  "D30K930133
  IF     ( sy-subrc NE 0 ).                                 "D30K929884
    CLEAR  git_xparam[].                                    "D30K930133
  ENDIF.                                                    "D30K929884

  SORT     git_xparam ASCENDING BY paramtype subtype key1 key2. "929884

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_data
*&---------------------------------------------------------------------*
*       Select the data
*----------------------------------------------------------------------*
FORM f_select_data.

  DATA:    lv_subrc               TYPE sysubrc,             "D30K929884
           lv_tabix               TYPE sytabix,             "D30K929884
           lv_msg_error           TYPE text50.              "D30K929884

  CLEAR    git_reguh[].
  SELECT   laufd  laufi  xvorl  zbukr  lifnr  kunnr  empfg  vblnr "9884
           waers  name1  zanre  znme1  znme2  znme3  znme4  zpstl "9884
           zort1  zstra  zpfac  zland  zregi  zaldt  rzawe  hktid "9884
           hbkid  ubknt  ubnks  ubnkl  valut  rwbtr  zpst2  absbu "9884
           uzawe  zort2  zpfor  ausfd  zadnr                "D30K929884
    INTO   TABLE  git_reguh
    FROM   reguh
   WHERE   laufd  = p_laufd
     AND   laufi  = p_laufi
     AND   xvorl  = SPACE
     AND   zbukr IN s_zbukr
     AND   hktid IN grt_hktid
     AND   hbkid IN grt_hbkid
     AND   rzawe  = p_rzawe.
  IF ( sy-subrc EQ 0 ).
    SORT   git_reguh ASCENDING BY laufd laufi xvorl zbukr
                                  lifnr kunnr empfg vblnr.
  ELSE.
    CLEAR  git_reguh[].
    CLEAR                              gv_nodata.
    MOVE   gc_x                     TO gv_nodata.
    RETURN.
  ENDIF.

  CLEAR    git_regup[].
  SELECT   laufd  laufi  xvorl  zbukr  lifnr  kunnr  empfg  vblnr
           bukrs  belnr  gjahr  buzei  xblnr  bldat  wrbtr  sgtxt "9884
           wskto                                            "D30K929884
    INTO   TABLE  git_regup
    FROM   regup  FOR ALL ENTRIES IN git_reguh
   WHERE   laufd  = git_reguh-laufd
     AND   laufi  = git_reguh-laufi
     AND   xvorl  = git_reguh-xvorl
     AND   zbukr  = git_reguh-zbukr
     AND   lifnr  = git_reguh-lifnr
     AND   kunnr  = git_reguh-kunnr
     AND   empfg  = git_reguh-empfg
     AND   vblnr  = git_reguh-vblnr.
  IF ( sy-subrc EQ 0 ).
    SORT   git_regup ASCENDING BY laufd laufi xvorl zbukr
                                  lifnr kunnr empfg vblnr
                                  bukrs belnr gjahr buzei.
  ELSE.
    CLEAR  git_regup[].
  ENDIF.

*eject
  IF   ( ( p_chect IS INITIAL ) AND ( p_checf IS INITIAL ) ).

    CLEAR    git_payr[].
    SELECT   zbukr  hbkid  hktid  rzawe  chect
             checf  laufd  laufi  lifnr  kunnr
             empfg  vblnr  waers  extrd
      INTO   TABLE  git_payr
      FROM   payr   FOR ALL ENTRIES IN git_reguh
     WHERE   zbukr  = git_reguh-zbukr
       AND   hbkid  = git_reguh-hbkid
       AND   hktid  = git_reguh-hktid
       AND   rzawe  = git_reguh-rzawe
       AND   lifnr  = git_reguh-lifnr
       AND   vblnr  = git_reguh-vblnr.
    IF ( sy-subrc EQ 0 ).
      SORT   git_payr ASCENDING BY zbukr hbkid hktid rzawe lifnr vblnr.
    ELSE.
      CLEAR  git_payr[].
    ENDIF.

  ELSE.

    CLEAR    git_payr[].
    SELECT   zbukr  hbkid  hktid  rzawe  chect
             checf  laufd  laufi  lifnr  kunnr
             empfg  vblnr  waers  extrd
      INTO   TABLE  git_payr
      FROM   payr   FOR ALL ENTRIES IN git_reguh
     WHERE   zbukr  = git_reguh-zbukr
       AND   hbkid  = git_reguh-hbkid
       AND   hktid  = git_reguh-hktid
       AND   rzawe  = git_reguh-rzawe
       AND   chect <= p_chect
       AND   checf >= p_checf
       AND   lifnr  = git_reguh-lifnr
       AND   vblnr  = git_reguh-vblnr.
    IF ( sy-subrc EQ 0 ).
      SORT   git_payr ASCENDING BY zbukr hbkid hktid rzawe lifnr vblnr.
    ELSE.
      CLEAR  git_payr[].
    ENDIF.

  ENDIF.

*eject
  CLEAR    git_t001[].
  CLEAR    git_adrc[].
  SELECT   bukrs  adrnr
    INTO   TABLE  git_t001
    FROM   t001   FOR ALL ENTRIES IN git_reguh
   WHERE   bukrs  = git_reguh-absbu.
  IF ( sy-subrc EQ 0 ).
    SORT   git_t001 ASCENDING BY bukrs.

    SELECT   addrnumber  date_from   nation      date_to    "D30K926579
             name1       city1       post_code1             "D30K926579
             street      country     region                 "D30K926579
      INTO   TABLE git_adrc                                 "D30K926579
      FROM   adrc  FOR ALL ENTRIES IN git_t001              "D30K926579
     WHERE   addrnumber = git_t001-adrnr                    "D30K926579
       AND   date_from <= sy-datum                          "D30K926579
       AND   date_to   >= sy-datum.                         "D30K926579
    IF ( sy-subrc EQ 0 ).
      SORT   git_adrc              BY addrnumber ASCENDING  "D30K926579
                                      date_from  DESCENDING. """K926579
      DELETE ADJACENT DUPLICATES FROM git_adrc              "D30K926579
                            COMPARING addrnumber.           "D30K926579
    ELSE.
      CLEAR  git_adrc[].
    ENDIF.

  ELSE.
    CLEAR  git_t001[].
    CLEAR  git_adrc[].
  ENDIF.

*eject
  CLEAR    git_checks[].

  CLEAR                                gwa_reguh.
  LOOP AT  git_reguh              INTO gwa_reguh.
    lv_tabix = sy-tabix.

    CLEAR    lv_subrc.                                      "D30K929884
    CLEAR    lv_msg_error.                                  "D30K929884

    CLEAR                              gwa_payr.
    READ     TABLE git_payr       INTO gwa_payr
                              WITH KEY zbukr = gwa_reguh-zbukr
                                       hbkid = gwa_reguh-hbkid
                                       hktid = gwa_reguh-hktid
                                       rzawe = gwa_reguh-rzawe
                                       lifnr = gwa_reguh-lifnr
                                       vblnr = gwa_reguh-vblnr
                         BINARY SEARCH.

    IF       ( sy-subrc             IS NOT INITIAL ).       "D30K929884
      MOVE     1                    TO lv_subrc.            "D30K929884
      MOVE     text-231             TO lv_msg_error.        "D30K929884
    ELSEIF   ( gwa_payr-chect       IS     INITIAL ).       "D30K929884
      MOVE     1                    TO lv_subrc.            "D30K929884
      MOVE     text-232             TO lv_msg_error.        "D30K929884
*   ELSEIF   ( gwa_payr-extrd       IS     INITIAL ).       "D30K929884
    ELSEIF   ( gwa_payr-extrd       IS NOT INITIAL ).       "D30K929884
      MOVE     1                    TO lv_subrc.            "D30K929884
      MOVE     text-233             TO lv_msg_error.        "D30K929884
    ELSEIF ( ( rb_btd8c             IS NOT INITIAL ) OR     "D30K930167
             ( rb_btd8u             IS NOT INITIAL ) OR     "D30K930167
             ( rb_btdpc             IS NOT INITIAL ) OR     "D30K930167
             ( rb_btdpu             IS NOT INITIAL )    ).  "D30K930167

      CLEAR                            gwa_xparam.          "D30K929884
      READ   TABLE git_xparam     INTO gwa_xparam           "D30K929884
                              WITH KEY paramtype = gc_paramtype_thba "4
                                       subtype   = gc_subtype_thba "884
                                       key1      = gwa_reguh-ubnkl "884
                                       key2      = gwa_reguh-ubknt "884
                         BINARY SEARCH.                     "D30K929884
      IF     ( sy-subrc EQ 0 ).                             "D30K929884
        CLEAR                          gwa_reguh-chk_frac.  "D30K929884
        MOVE   gwa_xparam-value1    TO gwa_reguh-chk_frac.  "D30K929884
        CLEAR                          gwa_reguh-chk_prot_acnt. "929884
        MOVE   gwa_xparam-value2    TO gwa_reguh-chk_prot_acnt. "929884
        MODIFY git_reguh          FROM gwa_reguh INDEX lv_tabix. "29884
      ELSE.                                                 "D30K929884
        MOVE   1                    TO lv_subrc.            "D30K929884
        MOVE   text-234             TO lv_msg_error.        "D30K929884
      ENDIF.                                                "D30K929884

    ENDIF.                                                  "D30K929884

    IF       ( lv_subrc             IS NOT INITIAL ).       "D30K929884
      CLEAR                            gwa_checks.
      MOVE     gwa_reguh-laufd      TO gwa_checks-laufd.
      MOVE     gwa_reguh-laufi      TO gwa_checks-laufi.
      MOVE     gwa_reguh-zbukr      TO gwa_checks-zbukr.
      MOVE     gwa_reguh-vblnr      TO gwa_checks-vblnr.
      MOVE     gwa_reguh-lifnr      TO gwa_checks-lifnr.
      MOVE     gwa_payr-hbkid       TO gwa_checks-hbkid.
      MOVE     gwa_payr-hktid       TO gwa_checks-hktid.
      MOVE     gwa_payr-rzawe       TO gwa_checks-rzawe.
      MOVE     gwa_payr-chect       TO gwa_checks-chect.
      MOVE     gwa_payr-checf       TO gwa_checks-checf.
      MOVE     gwa_payr-laufd       TO gwa_checks-laufd_p.
      MOVE     gwa_payr-laufi       TO gwa_checks-laufi_p.
      MOVE     gwa_payr-waers       TO gwa_checks-waers.
      MOVE     gwa_payr-extrd       TO gwa_checks-extrd.
      MOVE     lv_msg_error         TO gwa_checks-msg_error. """K935578
      APPEND                           gwa_checks
                                    TO git_checks.
      DELETE   git_reguh         INDEX lv_tabix.
    ENDIF.

    CLEAR  gwa_reguh.
  ENDLOOP.

  SORT   git_reguh ASCENDING BY laufd laufi xvorl zbukr
                                lifnr kunnr empfg vblnr.

*eject
* Get our AP office addresses                               "D30K926579
  CLEAR    git_our_addr[].                                  "D30K926579
                                                            "D30K926579
  PERFORM  f_get_our_office_addresses                       "D30K926579
                              TABLES   git_reguh            "D30K926579
                                       git_t001             "D30K926579
                                       git_adrc             "D30K926579
                                       git_our_addr.        "D30K926579

ENDFORM.                    " f_select_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_form
*&---------------------------------------------------------------------*
*       Open the form
*----------------------------------------------------------------------*
FORM f_open_form.

  DATA:    lwa_options TYPE itcpo.

  IF     ( p_tstrn              IS NOT INITIAL ).
    RETURN.
  ENDIF.

  IF     ( git_reguh[]              IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lwa_options.
  MOVE     1                        TO lwa_options-tdcopies.
  MOVE     gc_x                     TO lwa_options-tdnewid.
  MOVE     gc_title                 TO lwa_options-tdtitle.
  MOVE     p_prntr                  TO lwa_options-tddest.
  MOVE     SPACE                    TO lwa_options-tdgetotf.
  MOVE     text-111                 TO lwa_options-tdcovtitle.
  MOVE     text-111                 TO lwa_options-tdsuffix1.
  MOVE     SPACE                    TO lwa_options-tddelete.
  MOVE     gc_x                     TO lwa_options-tdiexit.

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      DIALOG                            = SPACE
      FORM                              = p_frmnm
      LANGUAGE                          = sy-langu
      OPTIONS                           = lwa_options
*   IMPORTING
*     LANGUAGE                          =
*     NEW_ARCHIVE_PARAMS                =
*     RESULT                            =
    EXCEPTIONS
      CANCELED                          = 1
      DEVICE                            = 2
      FORM                              = 3
      OPTIONS                           = 4
      UNCLOSED                          = 5
      MAIL_OPTIONS                      = 6
      ARCHIVE_ERROR                     = 7
      INVALID_FAX_NUMBER                = 8
      MORE_PARAMS_NEEDED_IN_BATCH       = 9
      SPOOL_ERROR                       = 10
      CODEPAGE                          = 11
      OTHERS                            = 12.

  IF ( sy-subrc NE 0 ).
    MESSAGE  i000 WITH text-311 p_frmnm.
    LEAVE    LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " f_open_form
*eject
*&---------------------------------------------------------------------*
*&      Form  f_close_form
*&---------------------------------------------------------------------*
*       Close the form
*----------------------------------------------------------------------*
FORM f_close_form.

  DATA:    lwa_result TYPE itcpp.

  CLEAR    gv_spool.

  IF     ( p_tstrn              IS NOT INITIAL ).
    RETURN.
  ENDIF.

  IF     ( git_reguh[]              IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lwa_result.

  CALL FUNCTION 'CLOSE_FORM'
    IMPORTING
      RESULT                   = lwa_result
    EXCEPTIONS
      UNOPENED                 = 1
      BAD_PAGEFORMAT_FOR_PRINT = 2
      SEND_ERROR               = 3
      SPOOL_ERROR              = 4
      CODEPAGE                 = 5
      OTHERS                   = 6.

  IF     ( sy-subrc EQ 0 ).
    CLEAR                              gv_spool.
    MOVE     lwa_result-tdspoolid   TO gv_spool.
  ELSEIF ( sy-subrc EQ 4 ).
    CLEAR    gv_spool.
    MESSAGE  i000 WITH text-321.
    LEAVE    LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " f_close_form
*eject
*&---------------------------------------------------------------------*
*&      Form  f_write_form
*&---------------------------------------------------------------------*
*       Write to the form
*----------------------------------------------------------------------*
FORM f_write_form.

  DATA:    lwa_xparam        TYPE ty_wa_xparam.             "D30K930133

  DATA:    lv_subrc          TYPE sysubrc,
           lv_strlen         TYPE syindex,
           lv_ubknt          TYPE char9.

  DATA:    lv_rwbtr          TYPE rwbtr,                    "D30K925153
           lv_text_line(160) TYPE c.

* Begin changes - delete code  JRHARTUNG 10-Apr-17 ACR-4084 "D30K928144
* DATA:   BEGIN OF lwa_text_line,                           "D30K925486
*          text1(24)         TYPE c,                        "D30K925486
*          text2(24)         TYPE c,                        "D30K925486
*          text3(24)         TYPE c,                        "D30K925486
*          text4(24)         TYPE c,                        "D30K925486
*          text5(24)         TYPE c,                        "D30K925486
*          text6(24)         TYPE c,                        "D30K925486
*          text7(24)         TYPE c,                        "D30K925486
*         END   OF lwa_text_line.                           "D30K925486
* End changes   - delete code  JRHARTUNG 10-Apr-17 ACR-4084 "D30K928144

  DATA:    lwa_text_line     TYPE ty_wa_text_line.          "D30K928144

  DATA:    lv_name           TYPE tdobname.                 "D30K928144

  IF     ( git_reguh[]              IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                gwa_reguh.
  LOOP AT  git_reguh              INTO gwa_reguh.

    CLEAR                              lv_rwbtr.            "D30K925153
    MOVE     gwa_reguh-rwbtr        TO lv_rwbtr.            "D30K925153
    IF     ( lv_rwbtr               LT 0 ).                 "D30K925153
      MULTIPLY                           lv_rwbtr BY -1.    "D30K925153
    ENDIF.                                                  "D30K925153

    IF     ( lv_rwbtr               GE 1000000000 ).        "D30K925153
      WRITE: / text-361, gwa_reguh-zbukr, gwa_reguh-vblnr.  "D30K925153
      CLEAR    gwa_reguh.                                   "D30K925153
      CONTINUE.                                             "D30K925153
    ENDIF.                                                  "D30K925153

    CLEAR    gwa_payr.
    CLEAR    gwa_t001.
    CLEAR    gwa_adrc.
    CLEAR    gwa_adrc_legal.
    CLEAR    gwa_regud.
    CLEAR    gwa_spell.

* Ensure that the account number is nine digits; include leading zeros

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = gwa_reguh-ubknt
      IMPORTING
        OUTPUT = gwa_reguh-ubknt.

    lv_strlen = STRLEN( gwa_reguh-ubknt ).

    IF     ( lv_strlen LT 9 ).

      CLEAR                            lv_ubknt.
      MOVE     gwa_reguh-ubknt      TO lv_ubknt.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = lv_ubknt
        IMPORTING
          OUTPUT = lv_ubknt.

      CLEAR                            gwa_reguh-ubknt.
      MOVE     lv_ubknt             TO gwa_reguh-ubknt.

    ENDIF.

*eject
* Read the payment data
    CLEAR                              gwa_payr.
    READ     TABLE git_payr       INTO gwa_payr
                              WITH KEY zbukr = gwa_reguh-zbukr
                                       hbkid = gwa_reguh-hbkid
                                       hktid = gwa_reguh-hktid
                                       rzawe = gwa_reguh-rzawe
                                       lifnr = gwa_reguh-lifnr
                                       vblnr = gwa_reguh-vblnr
                         BINARY SEARCH.
    lv_subrc = sy-subrc.

    IF     ( lv_subrc EQ 0 ).
      CLEAR                            gwa_regud-chect.
      MOVE     gwa_payr-chect       TO gwa_regud-chect.
    ELSE.
      CLEAR    gwa_reguh.
      CONTINUE.
    ENDIF.

    WRITE     gwa_reguh-rwbtr       TO gwa_regud-swnes
                              CURRENCY gwa_reguh-waers.
    TRANSLATE gwa_regud-swnes    USING ' *'.

*eject
* Set the legal address for the company

    CLEAR                              gwa_adrc_legal.
    CLEAR                              gwa_t001.
    READ     TABLE git_t001       INTO gwa_t001
                              WITH KEY bukrs = gwa_reguh-absbu
                         BINARY SEARCH.
    IF     ( sy-subrc EQ 0 ).

      READ   TABLE git_adrc       INTO gwa_adrc_legal
                              WITH KEY addrnumber = gwa_t001-adrnr
                         BINARY SEARCH.
      IF     ( sy-subrc EQ 0 ).
      ELSE.
        CLEAR      gwa_adrc_legal.
      ENDIF.

    ELSE.
      CLEAR        gwa_adrc_legal.
      CLEAR        gwa_t001.
    ENDIF.

* Begin changes - insert code   JRHARTUNG 04-Feb-16 ACR-nnn  D30K926579

    CLEAR      gwa_adrc.

    IF     ( ( gwa_our_addr-absbu   EQ gwa_reguh-absbu ) AND
             ( gwa_our_addr-hbkid   EQ gwa_reguh-hbkid ) AND
             ( gwa_our_addr-hktid   EQ gwa_reguh-hktid )     ).

      MOVE     gwa_our_addr-name1   TO gwa_adrc-name1.
      MOVE     gwa_our_addr-city1   TO gwa_adrc-city1.
      MOVE     gwa_our_addr-post_code1
                                    TO gwa_adrc-post_code1.
      MOVE     gwa_our_addr-street  TO gwa_adrc-street.
      MOVE     gwa_our_addr-country TO gwa_adrc-country.
      MOVE     gwa_our_addr-region  TO gwa_adrc-region.

*eject
    ELSE.

      CLEAR                            gwa_our_addr.
      READ   TABLE git_our_addr   INTO gwa_our_addr
                              WITH KEY absbu = gwa_reguh-absbu
                                       hbkid = gwa_reguh-hbkid
                                       hktid = gwa_reguh-hktid.
      IF     ( sy-subrc EQ 0 ).

        MOVE   gwa_our_addr-name1   TO gwa_adrc-name1.
        MOVE   gwa_our_addr-city1   TO gwa_adrc-city1.
        MOVE   gwa_our_addr-post_code1
                                    TO gwa_adrc-post_code1.
        MOVE   gwa_our_addr-street  TO gwa_adrc-street.
        MOVE   gwa_our_addr-country TO gwa_adrc-country.
        MOVE   gwa_our_addr-region  TO gwa_adrc-region.

      ELSE.
        CLEAR  gwa_our_addr.
      ENDIF.
    ENDIF.

* End changes   - insert code   JRHARTUNG 04-Feb-16 ACR-nnn  D30K926579

* Begin changes - insert code JRHARTUNG 05-Sep-19 CHG0148429 D30K930133

* Check if there is a treasury company name override
    IF     ( ( rb_btd8c             EQ abap_true ) OR       "D30K930167
             ( rb_btd8u             EQ abap_true ) OR       "D30K930167
             ( rb_btdpc             EQ abap_true ) OR       "D30K930167
             ( rb_btdpu             EQ abap_true )    ).    "D30K930167
      CLEAR                            lwa_xparam.
      READ     TABLE git_xparam   INTO lwa_xparam
                              WITH KEY paramtype = gc_paramtype_thba
                                       subtype   = gc_subtype_tcno
                                       key1      = gwa_reguh-zbukr
                         BINARY SEARCH.
      IF     ( sy-subrc EQ 0 ).
        CLEAR                          gwa_adrc-name1.
        MOVE   lwa_xparam-value1    TO gwa_adrc-name1.
      ENDIF.
    ENDIF.

* End changes   - insert code JRHARTUNG 05-Sep-19 CHG0148429 D30K930133

*eject
* Start of form
    IF     ( p_tstrn                IS INITIAL ).

      CALL FUNCTION 'START_FORM'
        EXPORTING
          FORM        = p_frmnm
        EXCEPTIONS
          FORM        = 1
          FORMAT      = 2
          UNENDED     = 3
          UNOPENED    = 4
          UNUSED      = 5
          SPOOL_ERROR = 6
          CODEPAGE    = 7
          OTHERS      = 8.

      IF ( sy-subrc NE 0 ).
        MESSAGE  i000 WITH text-331 p_frmnm.
        LEAVE    LIST-PROCESSING.
      ENDIF.

    ENDIF.

* Print the check details
    IF     ( p_tstrn                IS INITIAL ).

      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          ELEMENT                  = '545'
          WINDOW                   = 'CHECK'
        EXCEPTIONS
          ELEMENT                  = 1
          FUNCTION                 = 2
          TYPE                     = 3
          UNOPENED                 = 4
          UNSTARTED                = 5
          WINDOW                   = 6
          BAD_PAGEFORMAT_FOR_PRINT = 7
          SPOOL_ERROR              = 8
          CODEPAGE                 = 9
          OTHERS                   = 10.

      IF ( sy-subrc NE 0 ).
        MESSAGE  i000 WITH text-332 p_frmnm 'CHECK' '545'.
        LEAVE    LIST-PROCESSING.
      ENDIF.

    ENDIF.

*eject
* Convert the amount from digits to words

    CALL FUNCTION 'SPELL_AMOUNT'
      EXPORTING
        AMOUNT    = gwa_reguh-rwbtr
        CURRENCY  = gwa_reguh-waers
        FILLER    = SPACE
        LANGUAGE  = sy-langu
      IMPORTING
        IN_WORDS  = gwa_spell
      EXCEPTIONS
        NOT_FOUND = 1
        TOO_LARGE = 2
        OTHERS    = 3.

    IF ( sy-subrc NE 0 ).
      MESSAGE  i000 WITH text-341 gwa_reguh-zbukr gwa_reguh-vblnr.
      LEAVE    LIST-PROCESSING.
    ENDIF.

* Print the payment items (invoices) in the main window
    CLEAR    gwa_regud-swrbt.                               "D30K925153
    CLEAR    gwa_regud-swskt.                               "D30K925153
    CLEAR    gwa_regud-swnet.

    CLEAR                              gwa_regup.
    LOOP AT  git_regup            INTO gwa_regup
                                 WHERE laufd = gwa_reguh-laufd
                                   AND laufi = gwa_reguh-laufi
                                   AND xvorl = gwa_reguh-xvorl
                                   AND zbukr = gwa_reguh-zbukr
                                   AND lifnr = gwa_reguh-lifnr
                                   AND kunnr = gwa_reguh-kunnr
                                   AND empfg = gwa_reguh-empfg
                                   AND vblnr = gwa_reguh-vblnr.

      PERFORM  f_string_remove_spcl_char                    "D30K925153
                              CHANGING gwa_regup-xblnr.     "D30K925153

      CLEAR                            gwa_regud-wrbtr.
      MOVE   gwa_regup-wrbtr        TO gwa_regud-wrbtr.
      CLEAR                            gwa_regud-wabzg.
      MOVE   gwa_regup-wskto        TO gwa_regud-wabzg.
      CLEAR                            gwa_regud-wnett.
      MOVE   gwa_regup-wrbtr        TO gwa_regud-wnett.
      SUBTRACT                         gwa_regup-wskto
                                  FROM gwa_regud-wnett.

*eject
* Begin changes - delete code  JRHARTUNG 10-Apr-17 ACR-4084 "D30K928144
*     CLEAR                            lv_text_line.        "D30K925153
*     PERFORM  f_read_remit_long_text                       "D30K925153
*                             CHANGING lv_text_line.        "D30K925153
*
*     PERFORM  f_string_remove_spcl_char                    "D30K925153
*                             CHANGING lv_text_line.        "D30K925153
* End changes   - delete code  JRHARTUNG 10-Apr-17 ACR-4084 "D30K928144

* Begin changes - insert code  JRHARTUNG 10-Apr-17 ACR-4084 "D30K928144

      CLEAR                                 lv_name.
      MOVE     gwa_regup-bukrs+00(04)    TO lv_name+00(04).
      MOVE     gwa_regup-belnr+00(10)    TO lv_name+04(10).
      MOVE     gwa_regup-gjahr+00(04)    TO lv_name+14(04).

      CLEAR                                 lwa_text_line.

      PERFORM  f_format_long_text     USING '0003' lv_name 'BELEG'
                                            160 34
                                   CHANGING lwa_text_line.

* End changes   - insert code  JRHARTUNG 10-Apr-17 ACR-4084 "D30K928144

*     CLEAR                            lwa_text_line.       "D30K928144
*     MOVE     lv_text_line         TO lwa_text_line.       "D30K928144

      CLEAR                            gwa_regud-text1.     "D30K925153
      MOVE     lwa_text_line-text1  TO gwa_regud-text1.     "D30K925153
      CLEAR                            gwa_regud-text2.     "D30K925153
      MOVE     lwa_text_line-text2  TO gwa_regud-text2.     "D30K925153
      CLEAR                            gwa_regud-text3.     "D30K925153
      MOVE     lwa_text_line-text3  TO gwa_regud-text3.     "D30K925153
      CLEAR                            gwa_regud-text4.     "D30K925153
      MOVE     lwa_text_line-text4  TO gwa_regud-text4.     "D30K925153
      CLEAR                            gwa_regud-text5.     "D30K925153
      MOVE     lwa_text_line-text5  TO gwa_regud-text5.     "D30K925153
      CLEAR                            gwa_regud-text6.     "D30K925270
      MOVE     lwa_text_line-text6  TO gwa_regud-text6.     "D30K925270
      CLEAR                            gwa_regud-text7.     "D30K925486
      MOVE     lwa_text_line-text7  TO gwa_regud-text7.     "D30K925486
      CLEAR                            gwa_regud-text8.     "D30K928144
      MOVE     lwa_text_line-text8  TO gwa_regud-text8.     "D30K928144
      CLEAR                            gwa_regud-text9.     "D30K928144
      MOVE     lwa_text_line-text9  TO gwa_regud-text9.     "D30K928144

*eject
      IF     ( p_tstrn              IS INITIAL ).

        CALL FUNCTION 'WRITE_FORM'
          EXPORTING
            ELEMENT                  = '100'
            WINDOW                   = 'MAIN'
          EXCEPTIONS
            ELEMENT                  = 1
            FUNCTION                 = 2
            TYPE                     = 3
            UNOPENED                 = 4
            UNSTARTED                = 5
            WINDOW                   = 6
            BAD_PAGEFORMAT_FOR_PRINT = 7
            SPOOL_ERROR              = 8
            CODEPAGE                 = 9
            OTHERS                   = 10.

        IF ( sy-subrc NE 0 ).
          MESSAGE  i000 WITH text-332 p_frmnm 'MAIN' '100'.
          LEAVE    LIST-PROCESSING.
        ENDIF.

      ENDIF.

      ADD                              gwa_regud-wrbtr      "D30K925153
                                    TO gwa_regud-swrbt.     "D30K925153
      ADD                              gwa_regud-wabzg      "D30K925153
                                    TO gwa_regud-swskt.     "D30K925153
      ADD                              gwa_regud-wnett
                                    TO gwa_regud-swnet.

      CLEAR  gwa_regup.
    ENDLOOP.

    IF     ( p_tstrn                IS INITIAL ).

      CALL FUNCTION 'END_FORM'
        EXCEPTIONS
          UNOPENED                 = 1
          BAD_PAGEFORMAT_FOR_PRINT = 2
          SPOOL_ERROR              = 3
          CODEPAGE                 = 4
          OTHERS                   = 5.

      IF ( sy-subrc NE 0 ).
        MESSAGE  i000 WITH text-333 p_frmnm.
        LEAVE    LIST-PROCESSING.
      ENDIF.

    ENDIF.

*eject
    IF     ( lv_subrc EQ 0 ).
      CLEAR                            gwa_checks.
      MOVE     gwa_reguh-laufd      TO gwa_checks-laufd.
      MOVE     gwa_reguh-laufi      TO gwa_checks-laufi.
      MOVE     gwa_reguh-zbukr      TO gwa_checks-zbukr.
      MOVE     gwa_reguh-vblnr      TO gwa_checks-vblnr.
      MOVE     gwa_reguh-lifnr      TO gwa_checks-lifnr.
      MOVE     gwa_payr-hbkid       TO gwa_checks-hbkid.
      MOVE     gwa_payr-hktid       TO gwa_checks-hktid.
      MOVE     gwa_payr-rzawe       TO gwa_checks-rzawe.
      MOVE     gwa_payr-chect       TO gwa_checks-chect.
      MOVE     gwa_payr-checf       TO gwa_checks-checf.
      MOVE     gwa_payr-laufd       TO gwa_checks-laufd_p.
      MOVE     gwa_payr-laufi       TO gwa_checks-laufi_p.
      MOVE     gwa_payr-waers       TO gwa_checks-waers.
      APPEND                           gwa_checks
                                    TO git_checks.
    ENDIF.

    CLEAR  gwa_reguh.
  ENDLOOP.

ENDFORM.                    " f_write_form
*eject
*&---------------------------------------------------------------------*
*&      Form  f_call_trans_fchx
*&---------------------------------------------------------------------*
*       Update the extraction date
*----------------------------------------------------------------------*
FORM f_call_trans_fchx.

  IF     ( p_tstrn              IS NOT INITIAL ).
    RETURN.
  ENDIF.

  IF     ( git_reguh[]              IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                gwa_checks.
  LOOP AT  git_checks             INTO gwa_checks
                                 WHERE extrd IS INITIAL.

    SUBMIT  rfchke00 WITH par_zbuk = gwa_checks-zbukr
                     WITH par_hbki = gwa_checks-hbkid
                     WITH par_waer = gwa_checks-waers
                     WITH par_xzhl = gc_x
                     WITH zw_laufd = gwa_checks-laufd_p
                     WITH zw_laufi = gwa_checks-laufi_p
                     WITH par_chkf = gwa_checks-checf
                     WITH par_chkt = gwa_checks-chect
                     WITH par_dbup = gc_x
                     AND RETURN.

    CLEAR  gwa_checks.
  ENDLOOP.

ENDFORM.                    " f_call_trans_fchx
*eject
*&---------------------------------------------------------------------*
*&      Form  f_display_spool
*&---------------------------------------------------------------------*
*       Display the output spool
*----------------------------------------------------------------------*
FORM f_display_spool.

  DATA:    lwa_spoolid TYPE sp01r_id,
           lit_spoolid TYPE STANDARD TABLE OF sp01r_id.

  IF     ( gv_nodata IS NOT INITIAL ).
    WRITE: / text-251.
    RETURN.
  ENDIF.

  IF     ( gv_spool  IS NOT INITIAL ).

    CLEAR    lit_spoolid[].

    CLEAR                              lwa_spoolid.
    MOVE     gv_spool               TO lwa_spoolid-id.
    APPEND                             lwa_spoolid
                                    TO lit_spoolid.

    CALL FUNCTION 'RSPO_RID_SPOOLREQ_LIST'
      EXPORTING
        ID_LIST = lit_spoolid
      EXCEPTIONS
        ERROR   = 1
        OTHERS  = 2.

    IF ( sy-subrc NE 0 ).
      MESSAGE  i000 WITH text-351 gv_spool.
    ENDIF.

  ENDIF.

*eject
* List the checks that were printed
  SORT     git_checks ASCENDING BY zbukr hbkid hktid rzawe chect.

  WRITE: / text-411.
  ULINE AT /01(12).
  SKIP     1.
  WRITE: / text-415.
  SKIP     1.

  CLEAR                                gwa_checks.
  LOOP AT  git_checks             INTO gwa_checks
                                 WHERE extrd IS INITIAL.
    WRITE: /001 gwa_checks-zbukr,
            014 gwa_checks-hbkid,
            027 gwa_checks-hktid,
            040 gwa_checks-chect,
            053 gwa_checks-lifnr,
            066 gwa_checks-vblnr,
            079 gwa_checks-laufd,
            092 gwa_checks-laufi.
    CLEAR  gwa_checks.
  ENDLOOP.

  SKIP     3.

* List the checks that were not printed
  WRITE: / text-412.
  ULINE AT /01(53).
  SKIP     1.
  WRITE: / text-415.
  SKIP     1.

  CLEAR                                gwa_checks.
  LOOP AT  git_checks             INTO gwa_checks
                                 WHERE extrd IS NOT INITIAL.
    WRITE: /001 gwa_checks-zbukr,
            014 gwa_checks-hbkid,
            027 gwa_checks-hktid,
            040 gwa_checks-chect,
            053 gwa_checks-lifnr,
            066 gwa_checks-vblnr,
            079 gwa_checks-laufd,
            092 gwa_checks-laufi,
            105 gwa_checks-extrd,                           "D30K929884
           /053 gwa_checks-msg_error.                       "D30K929884
    CLEAR  gwa_checks.
  ENDLOOP.

ENDFORM.                    " f_display_spool
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_long_text                            "D30K928144
*&---------------------------------------------------------------------*
*       Format the remittance long text
*----------------------------------------------------------------------*
FORM f_format_long_text                                     "D30K928144
  USING    iv_id                  TYPE tdid
           iv_name                TYPE tdobname
           iv_object              TYPE tdobject
           iv_nb_char_out_para    TYPE syindex
           iv_nb_char_out_line    TYPE syindex
  CHANGING cwa_text_line          TYPE ty_wa_text_line.

  DATA:    ls_text                TYPE ty_wa_text,
           lt_text                TYPE ty_it_text,
           ls_word                TYPE ty_wa_words,
           lt_words               TYPE ty_it_words,
           ls_line                TYPE ty_wa_lines,
           lt_lines               TYPE ty_it_lines.

  DATA:    lv_ptr_text            TYPE syindex,
           lv_idx_text            TYPE syindex,
           lv_idx_word            TYPE syindex,
           lv_len_word            TYPE syindex,
           lv_i                   TYPE syindex,
           lv_string              TYPE STRING.

  CLEAR    cwa_text_line.

  IF     ( iv_name                  IS INITIAL ).
    RETURN.
  ENDIF.

  IF   ( ( iv_nb_char_out_para      EQ 0                   ) OR
         ( iv_nb_char_out_line      EQ 0                   ) OR
         ( iv_nb_char_out_para      LT iv_nb_char_out_line )    ).
    RETURN.
  ENDIF.

  CLEAR    lt_text[].
  CLEAR    lt_words[].
  CLEAR    lt_lines[].

*eject
* Read the long text
  CLEAR    lv_string.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = iv_id
      LANGUAGE                = 'E'
      NAME                    = iv_name
      OBJECT                  = iv_object
    TABLES
      LINES                   = lt_lines
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  CLEAR                                ls_line.
  LOOP AT  lt_lines               INTO ls_line.
    IF       ( ls_line-tdline       IS INITIAL ).
      CLEAR    ls_line.
      CONTINUE.
    ENDIF.
    CONCATENATE                        lv_string
                                       ls_line-tdline
                                  INTO lv_string
                    SEPARATED BY SPACE.
    CLEAR  ls_line.
  ENDLOOP.

  PERFORM  f_string_remove_spcl_char
                              CHANGING lv_string.

  IF     ( lv_string                IS INITIAL ).
    RETURN.
  ENDIF.

  CONDENSE lv_string.

*eject
* Build the output text lines
  SPLIT    lv_string    AT SPACE INTO TABLE lt_words.

  CLEAR    ls_text.
  CLEAR    ls_word.

  CLEAR    lv_ptr_text.
  CLEAR    lv_idx_text.
  CLEAR    lv_idx_word.
  CLEAR    lv_len_word.

  DO.

    IF       ( sy-index             GT 500 ).
      EXIT.
    ENDIF.

    IF       ( ls_word              IS INITIAL ).

      ADD      1                    TO lv_idx_word.
      CLEAR                            ls_word.
      READ     TABLE lt_words     INTO ls_word INDEX lv_idx_word.
      IF     ( sy-subrc NE 0 ).
        EXIT.
      ENDIF.

      IF     ( ls_word              IS INITIAL ).
        CLEAR  ls_word.
        CONTINUE.
      ENDIF.

    ENDIF.

    lv_len_word = STRLEN( ls_word ).

    lv_i = lv_ptr_text + lv_len_word.

* Append a word to the line of text if there is space available
    IF       ( lv_i                 LE iv_nb_char_out_line ).

      MOVE     ls_word              TO ls_text+lv_ptr_text(lv_len_word).

      lv_ptr_text = lv_ptr_text + lv_len_word + 1.

      CLEAR    ls_word.

      CONTINUE.

    ENDIF.

*eject
* Start a new line of text
    IF       ( ls_text          IS NOT INITIAL ).
      APPEND   ls_text              TO lt_text.
      CLEAR    ls_text.
      CLEAR    lv_ptr_text.
    ENDIF.

* Split the word
    lv_i = iv_nb_char_out_line - lv_ptr_text.

    IF       ( lv_len_word          GT lv_i ).
      MOVE     ls_word+0(lv_i)      TO ls_text+lv_ptr_text(lv_i).
      APPEND   ls_text              TO lt_text.
      CLEAR    ls_text.
      CLEAR    lv_ptr_text.
      SHIFT    ls_word         LEFT BY lv_i PLACES.
    ELSE.
      MOVE     ls_word+0(lv_len_word)
                                    TO ls_text+lv_ptr_text(lv_len_word).
      lv_ptr_text = lv_ptr_text + lv_len_word + 1.
      CLEAR    ls_word.
    ENDIF.

  ENDDO.

  IF       ( ls_text                IS NOT INITIAL ).
    APPEND   ls_text                TO lt_text.
  ENDIF.

*eject
* Move the text to the text structure
  CLEAR    lv_idx_text.

  CLEAR                                ls_text.
  LOOP AT  lt_text                INTO ls_text.

    IF       ( ls_text              IS INITIAL ).
      CLEAR    ls_text.
      CONTINUE.
    ENDIF.

    ADD      1 TO lv_idx_text.

    CASE     lv_idx_text.
      WHEN     1.
        MOVE     ls_text            TO cwa_text_line-text1.
      WHEN     2.
        MOVE     ls_text            TO cwa_text_line-text2.
      WHEN     3.
        MOVE     ls_text            TO cwa_text_line-text3.
      WHEN     4.
        MOVE     ls_text            TO cwa_text_line-text4.
      WHEN     5.
        MOVE     ls_text            TO cwa_text_line-text5.
      WHEN     6.
        MOVE     ls_text            TO cwa_text_line-text6.
      WHEN     7.
        MOVE     ls_text            TO cwa_text_line-text7.
      WHEN     8.
        MOVE     ls_text            TO cwa_text_line-text8.
      WHEN     9.
        MOVE     ls_text            TO cwa_text_line-text9.
      WHEN     OTHERS.
    ENDCASE.

    CLEAR  ls_text.
  ENDLOOP.

ENDFORM.                    " f_format_long_text             "D30K928144
*eject
*&---------------------------------------------------------------------*
*&      f_format_jpmtt_check_micr_var
*----------------------------------------------------------------------*
*       Format the JPMTT check MICR variables
*----------------------------------------------------------------------*
FORM f_format_jpmtt_check_micr_var
  TABLES   iit_in_tab    STRUCTURE itcsy
           cit_out_tab   STRUCTURE itcsy.

  CONSTANTS:
           lc_name_1     TYPE tdtprgname
                         VALUE 'VAR1',
           lc_name_2     TYPE tdtprgname
                         VALUE 'VAR2',
           lc_name_3     TYPE tdtprgname
                         VALUE 'VAR3',
           lc_name_4     TYPE tdtprgname
                         VALUE 'VAR4',
           lc_name_5     TYPE tdtprgname
                         VALUE 'VAR5'.

  DATA:    lv_subrc      TYPE sysubrc,
           lv_tabix      TYPE sytabix,
           lv_var1(10)   TYPE c, "check number
           lv_var2(05)   TYPE c, "bank routing number-part 1
           lv_var3(03)   TYPE c, "bank routing number-part 2
           lv_var4(10)   TYPE c, "bank account number
           lv_var5(12)   TYPE c, "check amount
           lv_char18(18) TYPE c.

  CLEAR    lv_var1.
  CLEAR    lv_var2.
  CLEAR    lv_var3.
  CLEAR    lv_var4.
  CLEAR    lv_var5.

* Process the check number
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 1.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    CLEAR                              lv_var1.
    MOVE     lv_char18+08(10)       TO lv_var1.             "D30K924001
  ENDIF.

* Process the bank routing number
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 2.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    CLEAR                              lv_var2.
    MOVE     lv_char18+13(5)        TO lv_var2.
    CLEAR                              lv_var3.
    MOVE     lv_char18+10(3)        TO lv_var3.
  ENDIF.

* Process the bank account number
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 3.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    CLEAR                              lv_var4.
    MOVE     lv_char18+8(10)        TO lv_var4.
  ENDIF.

* Process the amount
  CLEAR          iit_in_tab.
  READ     TABLE iit_in_tab INDEX 4.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              lv_char18.
    MOVE     iit_in_tab-value       TO lv_char18.
    TRANSLATE                          lv_char18 USING '* . , - '.
    CONDENSE                           lv_char18 NO-GAPS.
    SHIFT    lv_char18           RIGHT DELETING TRAILING SPACE.
    TRANSLATE                          lv_char18 USING ' 0'.
    CLEAR                              lv_var5.
    MOVE     lv_char18+6(12)        TO lv_var5.
  ENDIF.

* Assign the output variables to the output parameter table

* Assign the check number
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_1.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var1                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the bank routing number-part 1
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_2.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var2                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the bank routing number-part 2
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_3.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var3                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the bank account number
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_4.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var4                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

* Assign the check amount
  CLEAR          cit_out_tab.
  READ     TABLE cit_out_tab  WITH KEY name = lc_name_5.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).
    CLEAR                              cit_out_tab-value.
    MOVE     lv_var5                TO cit_out_tab-value.
    MODIFY                             cit_out_tab
                                 INDEX lv_tabix.
  ENDIF.

  CLEAR    iit_in_tab.
  CLEAR    cit_out_tab.

ENDFORM.                    "f_format_jpmtt_check_micr_var
*eject
*&---------------------------------------------------------------------*
*&      Form  f_read_remit_long_text                        "D30K925153
*&---------------------------------------------------------------------*
*       Read the remittance long text
*----------------------------------------------------------------------*
FORM f_read_remit_long_text                                 "D30K925153
  CHANGING cv_text_line  TYPE ANY.

  DATA:    lwa_lines     TYPE tline,
           lit_lines     TYPE STANDARD TABLE OF tline.

  DATA:    lv_id         TYPE tdid,
           lv_name       TYPE tdobname,
           lv_object     TYPE tdobject,
           lv_text1000   TYPE text1000,
           lv_text1000_p TYPE text1000,
           lv_text(300)  TYPE c.

  CLEAR    cv_text_line.

  IF     ( gwa_regup-bukrs          IS INITIAL ).
    RETURN.
  ENDIF.

*eject
  CLEAR                                lv_id.
  MOVE     '0003'                   TO lv_id.
  CLEAR                                lv_name.
  MOVE     gwa_regup-bukrs+00(04)   TO lv_name+00(04).
  MOVE     gwa_regup-belnr+00(10)   TO lv_name+04(10).
  MOVE     gwa_regup-gjahr+00(04)   TO lv_name+14(04).
  CLEAR                                lv_object.
  MOVE     'BELEG'                  TO lv_object.

  CLEAR    lit_lines[].

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = lv_id
      LANGUAGE                = sy-langu
      NAME                    = lv_name
      OBJECT                  = lv_object
    TABLES
      LINES                   = lit_lines
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  IF     ( lit_lines[] IS INITIAL ).
    RETURN.
  ENDIF.

*eject
  CLEAR    lv_text1000.

  DO 20 TIMES.

    CLEAR                              lwa_lines.
    READ     TABLE lit_lines      INTO lwa_lines INDEX 1.
    IF     ( sy-subrc NE 0 ).
      EXIT.
    ENDIF.

    IF     ( lwa_lines-tdline   IS NOT INITIAL ).

      SHIFT  lwa_lines-tdline     LEFT DELETING LEADING SPACE.

      IF   ( lv_text1000            IS INITIAL ).

        CLEAR                          lv_text1000.
        MOVE   lwa_lines-tdline     TO lv_text1000.

      ELSE.

        CLEAR                          lv_text1000_p.
        CONCATENATE                    lv_text1000
                                       lwa_lines-tdline
                                  INTO lv_text1000_p
                    SEPARATED BY SPACE.
        CLEAR                          lv_text1000.
        MOVE   lv_text1000_p        TO lv_text1000.

      ENDIF.

    ENDIF.

    IF     ( STRLEN( lv_text1000 )  GT 400 ).
      EXIT.
    ENDIF.

    DELETE   lit_lines           INDEX 1.

    IF     ( lit_lines[] IS INITIAL ).
      EXIT.
    ENDIF.

  ENDDO.

  CLEAR                                lv_text.
  MOVE     lv_text1000+00(300)      TO lv_text.

  CLEAR                                cv_text_line.
  MOVE     lv_text+00(160)          TO cv_text_line.

ENDFORM.                    " f_read_remit_long_text        "D30K925153
*eject
*&---------------------------------------------------------------------*
*&      Form  f_string_remove_spcl_char                     "D30K925153
*&---------------------------------------------------------------------*
*       Remove special, non-printable, characters from a string
*----------------------------------------------------------------------*
FORM f_string_remove_spcl_char                              "D30K925153
  CHANGING cv_string      TYPE ANY.

  DATA:  lv_i             TYPE syindex,
         lv_j             TYPE syindex,
         lv_k             TYPE syindex,
         lv_char(1)       TYPE c,
         lv_string_i(250) TYPE c,
         lv_string_o(250) TYPE c.

  IF ( cv_string IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lv_string_i.
  MOVE     cv_string                TO lv_string_i.

  CLEAR    lv_string_o.

  lv_i   = STRLEN( lv_string_i ).
  lv_j   = 0.
  lv_k   = 0.

  DO       lv_i TIMES.

    CLEAR                              lv_char.
    MOVE     lv_string_i+lv_k(1)    TO lv_char.
    ADD      1                      TO lv_k.

    IF   ( ( lv_char                GE ' ' ) AND
           ( lv_char                LE '~' )     ).
      MOVE   lv_char                TO lv_string_o+lv_j(1).
      ADD    1                      TO lv_j.
    ENDIF.

  ENDDO.

  CLEAR                                cv_string.
  MOVE     lv_string_o              TO cv_string.

ENDFORM.                    " f_string_remove_spcl_char     "D30K925153
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_our_office_addresses                    "D30K926579
*&---------------------------------------------------------------------*
*       Get our AP office addresses
*----------------------------------------------------------------------*
FORM f_get_our_office_addresses                             "D30K926579
  TABLES   iit_reguh                   TYPE ty_it_reguh
           iit_t001                    TYPE ty_it_t001
           iit_adrc                    TYPE ty_it_adrc
           cit_our_addr                TYPE ty_it_our_addr.

  DATA:    lit_our_addr                TYPE ty_it_our_addr,
           lit_xparam                  TYPE STANDARD TABLE OF
                                            zfit_xparam.

  DATA:    lwa_reguh                   TYPE ty_wa_reguh,
           lwa_t001                    TYPE ty_wa_t001,
           lwa_adrc                    TYPE ty_wa_adrc,
           lwa_our_addr                TYPE ty_wa_our_addr,
           lwa_xparam                  TYPE zfit_xparam.

  DATA:   BEGIN OF lwa_addr_struct,
           score                       TYPE numc3,
           value                       TYPE zparamvalue,
          END   OF lwa_addr_struct.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_index                    TYPE syindex,
           lv_pattern                  TYPE STRING,
           lv_text                     TYPE STRING,
           lv_score                    TYPE numc3,
           lv_absbu                    TYPE absbu,
           lv_hbkid                    TYPE hbkid,
           lv_hktid                    TYPE hktid,
           lv_street                   TYPE STRING,
           lv_city                     TYPE STRING,
           lv_region                   TYPE STRING,
           lv_postal_code              TYPE STRING,
           lv_country                  TYPE STRING.

  CONSTANTS:
           lc_paramtype                TYPE zparamtype
                                       VALUE 'ENHANCEMENT',
           lc_subtype                  TYPE zparamsubtype
                                       VALUE 'F_P2C_AP_021',
           lc_key1                     TYPE zparamkey
                                       VALUE 'OUR_AP_DEPARTMENT'.

  CLEAR    cit_our_addr[].

  IF     ( iit_reguh[]                   IS INITIAL ).
    RETURN.
  ENDIF.

*eject
* Select the program parameters
  CLEAR    lit_xparam[].
  SELECT   *
    INTO   TABLE lit_xparam
    FROM   zfit_xparam
   WHERE   paramtype = lc_paramtype
     AND   subtype   = lc_subtype
     AND   key1      = lc_key1.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lit_xparam[].
  ENDIF.

  CLEAR                                     lwa_reguh.
  LOOP AT    iit_reguh                 INTO lwa_reguh.

    CLEAR                                   lv_absbu.
    MOVE     lwa_reguh-absbu             TO lv_absbu.
    CLEAR                                   lv_hbkid.
    MOVE     lwa_reguh-hbkid             TO lv_hbkid.
    CLEAR                                   lv_hktid.
    MOVE     lwa_reguh-hktid             TO lv_hktid.

* Check if an address has already been created
    CLEAR                                   lwa_our_addr.
    READ     TABLE lit_our_addr        INTO lwa_our_addr
                                   WITH KEY absbu = lv_absbu
                                            hbkid = lv_hbkid
                                            hktid = lv_hktid
                              BINARY SEARCH.
    lv_subrc = sy-subrc.
    lv_tabix = sy-tabix.

    IF         ( lv_subrc EQ 0 ).
      CLEAR      lwa_reguh.
      CONTINUE.
    ENDIF.

*eject
* Find an address from the program parameter table
    CLEAR                                   lwa_addr_struct.
    MOVE     0                           TO lwa_addr_struct-score.

    CLEAR                                   lwa_xparam.
    LOOP AT  lit_xparam                INTO lwa_xparam.

      CLEAR    lv_score.

      DO 4 TIMES.
        lv_index = sy-index.

        CLEAR    lv_pattern.
        CLEAR    lv_text.

        CASE     lv_index.
          WHEN     1.
            MOVE     lwa_xparam-key2     TO lv_pattern.
            MOVE     lv_absbu            TO lv_text.
          WHEN     2.
            MOVE     lwa_xparam-key3     TO lv_pattern.
            MOVE     lv_hbkid            TO lv_text.
          WHEN     3.
            MOVE     lwa_xparam-key4     TO lv_pattern.
            MOVE     lv_hktid            TO lv_text.
          WHEN     4.
*           MOVE     lwa_xparam-key5     TO lv_pattern.
*           MOVE     lv_?????            TO lv_text.
          WHEN     OTHERS.
        ENDCASE.

        IF         ( lv_pattern          EQ SPACE   ).
        ELSEIF     ( lv_pattern          EQ lv_text ).
          ADD        10                  TO lv_score.
        ELSE.
          FIND       REGEX lv_pattern    IN lv_text.
          IF       ( sy-subrc            EQ 0 ).
            ADD      1                   TO lv_score.
          ELSE.
            CLEAR                           lv_score.
            MOVE     0                   TO lv_score.
            EXIT.
          ENDIF.
        ENDIF.

      ENDDO.

*eject
      IF       ( lv_score                EQ 0 ).
      ELSEIF   ( lv_score                GT lwa_addr_struct-score ).
        CLEAR                               lwa_addr_struct.
        MOVE     lv_score                TO lwa_addr_struct-score.
        MOVE     lwa_xparam-value1       TO lwa_addr_struct-value.
      ENDIF.

      CLEAR  lwa_xparam.
    ENDLOOP.

* Read the company name and address
    IF     ( ( lwa_t001-bukrs            IS NOT INITIAL     ) AND
             ( lwa_t001-bukrs            EQ lwa_reguh-absbu ) AND
             ( lwa_adrc-addrnumber       IS NOT INITIAL     ) AND
             ( lwa_adrc-addrnumber       EQ lwa_t001-adrnr  )     ).
    ELSE.
      CLEAR    lwa_t001.
      CLEAR    lwa_adrc.
      READ     TABLE git_t001          INTO lwa_t001
                                   WITH KEY bukrs = lwa_reguh-absbu.
      IF     ( sy-subrc EQ 0 ).
        READ   TABLE git_adrc          INTO lwa_adrc
                                   WITH KEY addrnumber = lwa_t001-adrnr.
        IF   ( sy-subrc NE 0 ).
          CLEAR      lwa_t001.
          CLEAR      lwa_adrc.
        ENDIF.
      ELSE.
        CLEAR        lwa_t001.
        CLEAR        lwa_adrc.
      ENDIF.
    ENDIF.

*eject
* Build our address entry into internal table
    CLEAR                                   lwa_our_addr.
    MOVE   lv_absbu                      TO lwa_our_addr-absbu.
    MOVE   lv_hbkid                      TO lwa_our_addr-hbkid.
    MOVE   lv_hktid                      TO lwa_our_addr-hktid.
    MOVE   lwa_adrc-name1                TO lwa_our_addr-name1.

    IF       ( lwa_addr_struct-score     EQ 0 ).

      MOVE     lwa_adrc-city1            TO lwa_our_addr-city1.
      MOVE     lwa_adrc-post_code1       TO lwa_our_addr-post_code1.
      MOVE     lwa_adrc-street           TO lwa_our_addr-street.
      MOVE     lwa_adrc-country          TO lwa_our_addr-country.
      MOVE     lwa_adrc-region           TO lwa_our_addr-region.

    ELSE.

      CLEAR                                 lv_street.
      CLEAR                                 lv_city.
      CLEAR                                 lv_region.
      CLEAR                                 lv_postal_code.
      CLEAR                                 lv_country.

      SPLIT    lwa_addr_struct-value     AT '|'
                                       INTO lv_street
                                            lv_city
                                            lv_region
                                            lv_postal_code
                                            lv_country.

      SHIFT    lv_street               LEFT DELETING LEADING SPACE.
      SHIFT    lv_city                 LEFT DELETING LEADING SPACE.
      SHIFT    lv_region               LEFT DELETING LEADING SPACE.
      SHIFT    lv_postal_code          LEFT DELETING LEADING SPACE.
      SHIFT    lv_country              LEFT DELETING LEADING SPACE.

      MOVE     lv_city                   TO lwa_our_addr-city1.
      MOVE     lv_postal_code            TO lwa_our_addr-post_code1.
      MOVE     lv_street                 TO lwa_our_addr-street.
      MOVE     lv_country                TO lwa_our_addr-country.
      MOVE     lv_region                 TO lwa_our_addr-region.

    ENDIF.

    INSERT                                  lwa_our_addr
                                       INTO lit_our_addr
                                      INDEX lv_tabix.

    CLEAR    lwa_reguh.
  ENDLOOP.

  SORT     lit_our_addr ASCENDING BY absbu hbkid hktid.

  cit_our_addr[] = lit_our_addr[].

ENDFORM.                    " f_get_our_office_addresses    "D30K926579
