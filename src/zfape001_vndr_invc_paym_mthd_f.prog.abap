*&---------------------------------------------------------------------*
*&  Include           ZFAPE001_VNDR_INVC_PAYM_MTHD_F
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPE001_VNDR_INVC_PAYM_MTHD                      *
*  Include           ZFAPE001_VNDR_INVC_PAYM_MTHD_F                    *
*  Author:           John Hartung                                      *
*  Date:             June 23,2016                                      *
*  Ticket#:          ACR-1118                                          *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      AP Vendor Invoice Payment Method Update Program   *
*                    Forms Include - Subroutines                       *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 06/23/16 JRHARTUNG D30K926970 - ACR-1118 - Initial program           *
*----------------------------------------------------------------------*
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_xparam
*&---------------------------------------------------------------------*
*       Get the program parameter values
*----------------------------------------------------------------------*
FORM f_select_xparam.

  CLEAR    gt_xparam[].

  SELECT   *
    INTO   TABLE gt_xparam
    FROM   zfit_xparam
   WHERE   paramtype = gc_param_out_int
     AND   subtype   = gc_param_obj_id
     AND   key1      = gc_param_program.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_xparam           ASCENDING BY mandt paramtype subtype
                                            key1 key2 key3 key4 key5.
  ELSE.
    CLEAR  gt_xparam[].
  ENDIF.

ENDFORM.                    " f_select_xparam
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_sel_scrn_defaults
*&---------------------------------------------------------------------*
*       Set the selection screen defaults
*----------------------------------------------------------------------*
FORM f_set_sel_scrn_defaults.

  DATA:    ls_xparam                   TYPE ty_wa_xparam,
           ls_bschl                    LIKE LINE OF s_bschl,
           ls_zlsch                    LIKE LINE OF s_zlsch,
           ls_cpairs                   LIKE LINE OF s_cpairs.

  DATA:    lt_sscr_restrict            TYPE sscr_restrict,
           ls_sscr_opt_list            TYPE sscr_opt_list,
           ls_sscr_ass                 TYPE sscr_ass.

  CLEAR                                     ls_xparam.
  LOOP AT  gt_xparam                   INTO ls_xparam.
    IF   ( ( ls_xparam-key2              EQ gc_param_bschl ) AND
           ( ls_xparam-value1            IS NOT INITIAL    )     ).
      CLEAR                                 ls_bschl.
      MOVE   'I'                         TO ls_bschl-sign.
      MOVE   'EQ'                        TO ls_bschl-option.
      MOVE   ls_xparam-value1            TO ls_bschl-low.
      APPEND                                ls_bschl
                                         TO  s_bschl.
    ENDIF.
    IF   ( ( ls_xparam-key2              EQ gc_param_zlsch ) AND
           ( ls_xparam-value1            IS NOT INITIAL    )     ).
      CLEAR                                 ls_zlsch.
      MOVE   'I'                         TO ls_zlsch-sign.
      MOVE   'EQ'                        TO ls_zlsch-option.
      MOVE   ls_xparam-value1            TO ls_zlsch-low.
      APPEND                                ls_zlsch
                                         TO  s_zlsch.
      CLEAR                                 ls_cpairs.
      MOVE   'I'                         TO ls_cpairs-sign.
      MOVE   'BT'                        TO ls_cpairs-option.
      MOVE   ls_xparam-value1            TO ls_cpairs-low.
      MOVE   ls_xparam-value2            TO ls_cpairs-high.
      APPEND                                ls_cpairs
                                         TO  s_cpairs.
    ENDIF.
    CLEAR  ls_xparam.
  ENDLOOP.

  SORT     s_bschl  ASCENDING BY low.
  DELETE   ADJACENT DUPLICATES FROM s_bschl  COMPARING low.
  SORT     s_zlsch  ASCENDING BY low.
  DELETE   ADJACENT DUPLICATES FROM s_zlsch  COMPARING low.
  SORT     s_cpairs ASCENDING BY low.
  DELETE   ADJACENT DUPLICATES FROM s_cpairs COMPARING low.

*eject
* Build the option list for inbound filenames
  CLEAR    lt_sscr_restrict.

  CLEAR                                ls_sscr_opt_list.
  MOVE     'OPT_LST_EQ'             TO ls_sscr_opt_list-name.
  MOVE     'X'                      TO ls_sscr_opt_list-options-eq.
  APPEND   ls_sscr_opt_list         TO lt_sscr_restrict-opt_list_tab.

  CLEAR                                ls_sscr_opt_list.
  MOVE     'OPT_LST_BT'             TO ls_sscr_opt_list-name.
  MOVE     'X'                      TO ls_sscr_opt_list-options-bt.
  APPEND   ls_sscr_opt_list         TO lt_sscr_restrict-opt_list_tab.

* Apply the option list to the selection-screen select-option lists
  CLEAR                                ls_sscr_ass.
  MOVE     'S'                      TO ls_sscr_ass-kind.
  MOVE     'S_BSCHL'                TO ls_sscr_ass-name.
  MOVE     'I'                      TO ls_sscr_ass-sg_main.
  MOVE     'OPT_LST_EQ'             TO ls_sscr_ass-op_main.
  APPEND   ls_sscr_ass              TO lt_sscr_restrict-ass_tab.

  CLEAR                                ls_sscr_ass.
  MOVE     'S'                      TO ls_sscr_ass-kind.
  MOVE     'S_ZLSCH'                TO ls_sscr_ass-name.
  MOVE     'I'                      TO ls_sscr_ass-sg_main.
  MOVE     'OPT_LST_EQ'             TO ls_sscr_ass-op_main.
  APPEND   ls_sscr_ass              TO lt_sscr_restrict-ass_tab.

  CLEAR                                ls_sscr_ass.
  MOVE     'S'                      TO ls_sscr_ass-kind.
  MOVE     'S_CPAIRS'               TO ls_sscr_ass-name.
  MOVE     'I'                      TO ls_sscr_ass-sg_main.
  MOVE     'OPT_LST_BT'             TO ls_sscr_ass-op_main.
  APPEND   ls_sscr_ass              TO lt_sscr_restrict-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      RESTRICTION = lt_sscr_restrict.

ENDFORM.                    " f_set_sel_scrn_defaults
*eject
*&---------------------------------------------------------------------*
*&      Form  f_sel_scrn_output
*&---------------------------------------------------------------------*
*       Selection screen output
*----------------------------------------------------------------------*
FORM f_sel_scrn_output.

  LOOP AT  SCREEN.

* Set screen fields to display only
    IF     ( screen-group1               EQ gc_modif_id_dsp ).

      IF   ( screen-name                 CS 'S_BSCHL'       ) OR
           ( screen-name                 CS 'S_ZLSCH'       ).
        IF ( screen-name                 CS 'OPTI_PUSH'     ) OR
           ( screen-name                 CS 'TO_TEXT'       ) OR
           ( screen-name                 CS 'HIGH'          ).
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY   SCREEN.
        ENDIF.
        IF ( screen-name                 CS 'LOW'           ).
          screen-input     = 0.
          screen-invisible = 0.
          MODIFY   SCREEN.
        ENDIF.
      ENDIF.

      IF   ( screen-name                 CS 'S_CPAIRS'      ).
        IF ( screen-name                 CS 'OPTI_PUSH'     ).
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY   SCREEN.
        ENDIF.
        IF ( screen-name                 CS 'LOW'           ) OR
           ( screen-name                 CS 'HIGH'          ).
          screen-input     = 0.
          screen-invisible = 0.
          MODIFY   SCREEN.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_sel_scrn_output
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
*       Get the data
*----------------------------------------------------------------------*
FORM f_get_data
  TABLES   ct_bsik                     TYPE ty_it_bsik.

  DATA:    ls_bsik                     TYPE ty_wa_bsik,
           lt_bsik                     TYPE ty_it_bsik.

  DATA:    ls_cpairs                   LIKE LINE OF s_cpairs.

  DATA:    lv_fl_append                TYPE flag.

  CLEAR    ct_bsik[].

  CLEAR    lt_bsik[].
  SELECT   bukrs  lifnr  gjahr  belnr  buzei
           budat  bldat  waers  xblnr  blart
           bschl  wrbtr  sgtxt  zlsch  empfb
    INTO   TABLE lt_bsik
    FROM   bsik
   WHERE   bukrs IN s_bukrs
     AND   belnr IN s_belnr
     AND   gjahr IN s_gjahr
     AND   bschl IN s_bschl
     AND   zlsch IN s_zlsch.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lt_bsik[].
    RETURN.
  ENDIF.

  CLEAR                                     ls_bsik.
  LOOP AT  lt_bsik                     INTO ls_bsik.

* Check for special characters
    CLEAR    lv_fl_append.

    PERFORM  f_check_special_char     USING ls_bsik
                                   CHANGING lv_fl_append.

    IF     ( lv_fl_append                IS NOT INITIAL ).
      APPEND                                ls_bsik
                                         TO ct_bsik.
    ENDIF.

    CLEAR  ls_bsik.
  ENDLOOP.

ENDFORM.                    " f_get_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_check_special_char
*&---------------------------------------------------------------------*
*       Check for special characters
*----------------------------------------------------------------------*
FORM f_check_special_char
  USING    is_bsik                     TYPE ty_wa_bsik
  CHANGING cv_fl_append                TYPE flag.

  DATA:    ls_lfa1                     TYPE ty_wa_lfa1,
           ls_adrc                     TYPE ty_wa_adrc,
           lt_adrc                     TYPE ty_it_adrc,
           ls_lines                    TYPE tline,
           lt_lines                    TYPE STANDARD TABLE OF tline.

  DATA:    lv_string                   TYPE STRING,
           lv_i                        TYPE i,
           lv_fl_found                 TYPE flag,
           lv_lifnr                    TYPE lifnr,
           lv_id                       TYPE tdid,
           lv_language                 TYPE spras,
           lv_name                     TYPE tdobname,
           lv_object                   TYPE tdobject.

  CLEAR    cv_fl_append.

  CLEAR    lv_fl_found.

* Search the item text for special characters
  IF     ( is_bsik-sgtxt                 IS NOT INITIAL ).

    CLEAR                                   lv_string.
    MOVE     is_bsik-sgtxt               TO lv_string.

    PERFORM  f_search_special_char    USING lv_string
                                   CHANGING lv_fl_found.

    IF     ( lv_fl_found                 IS NOT INITIAL ).
      cv_fl_append = abap_true.
      RETURN.
    ENDIF.

  ENDIF.

*eject
* Search the vendor name for special characters
  CLEAR    ls_lfa1.

  CLEAR                                     lv_lifnr.
  IF     ( is_bsik-empfb                 IS INITIAL ).
    MOVE   is_bsik-lifnr                 TO lv_lifnr.
  ELSE.
    MOVE   is_bsik-empfb                 TO lv_lifnr.
  ENDIF.

  IF     ( lv_lifnr                      IS NOT INITIAL ).

    SELECT   SINGLE lifnr  name1  name2  adrnr
      INTO   ls_lfa1
      FROM   lfa1
     WHERE   lifnr = lv_lifnr.
    IF     ( sy-subrc EQ 0 ).

      CLEAR                                 lv_string.
      CONCATENATE                           ls_lfa1-name1
                                            ls_lfa1-name2
                                       INTO lv_string.

      PERFORM  f_search_special_char  USING lv_string
                                   CHANGING lv_fl_found.

      IF     ( lv_fl_found               IS NOT INITIAL ).
        cv_fl_append = abap_true.
        RETURN.
      ENDIF.

    ELSE.
      CLEAR    ls_lfa1.
    ENDIF.

  ENDIF.

*eject
* Search the address for special characters
  CLEAR    lt_adrc[].
  CLEAR    ls_adrc.

  IF     ( ls_lfa1-adrnr                 IS NOT INITIAL ).

    SELECT   addrnumber  date_from   nation      date_to
             name1       name2       city1       street      str_suppl2
      INTO   TABLE lt_adrc
      FROM   adrc
     WHERE   addrnumber = ls_lfa1-adrnr.
    IF     ( sy-subrc EQ 0 ).

      SORT     lt_adrc ASCENDING BY date_to.
      CLEAR                                 ls_adrc.
      LOOP AT  lt_adrc                 INTO ls_adrc.
        IF   ( ( ls_adrc-date_from       LE sy-datum ) AND
               ( ls_adrc-date_to         GE sy-datum )     ).
          EXIT.
        ENDIF.
        CLEAR  ls_adrc.
      ENDLOOP.
      IF     ( ls_adrc-addrnumber        IS INITIAL ).
        CLEAR                               ls_adrc.
        READ     TABLE lt_adrc         INTO ls_adrc INDEX 1.
      ENDIF.

      CLEAR                                 lv_string.
      CONCATENATE                           ls_adrc-name1
                                            ls_adrc-name2
                                            ls_adrc-str_suppl2
                                            ls_adrc-street
                                            ls_adrc-city1
                                       INTO lv_string.

      PERFORM  f_search_special_char  USING lv_string
                                   CHANGING lv_fl_found.

      IF     ( lv_fl_found               IS NOT INITIAL ).
        cv_fl_append = abap_true.
        RETURN.
      ENDIF.

    ELSE.
      CLEAR    lt_adrc[].
      CLEAR    ls_adrc.
    ENDIF.

  ENDIF.

*eject
* Search the payment advice long text for special characters
  IF     ( is_bsik-belnr                 IS NOT INITIAL ).

    CLEAR                                   lv_id.
    MOVE     '0003'                      TO lv_id.
    CLEAR                                   lv_language.
    MOVE     sy-langu                    TO lv_language.
    CLEAR                                   lv_name.
    MOVE     is_bsik-bukrs               TO lv_name+00(04).
    MOVE     is_bsik-belnr               TO lv_name+04(10).
    MOVE     is_bsik-gjahr               TO lv_name+14(04).
    CLEAR                                   lv_object.
    MOVE     'BELEG'                     TO lv_object.

    CLEAR    lt_lines[].

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        CLIENT                  = sy-mandt
        ID                      = lv_id
        LANGUAGE                = lv_language
        NAME                    = lv_name
        OBJECT                  = lv_object
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

*eject
    IF     ( sy-subrc EQ 0 ).

      CLEAR    lv_string.
      CLEAR    lv_i.

      CLEAR                                 ls_lines.
      LOOP AT  lt_lines                INTO ls_lines.

        CONCATENATE                         lv_string
                                            ls_lines-tdline
                                       INTO lv_string.

        ADD      1                       TO lv_i.

        IF     ( lv_i                    GE 400 ).

          PERFORM  f_search_special_char
                                      USING lv_string
                                   CHANGING lv_fl_found.

          CLEAR  lv_string.
          CLEAR  lv_i.

          IF     ( lv_fl_found           IS NOT INITIAL ).
            cv_fl_append = abap_true.
            EXIT.
          ENDIF.

        ENDIF.

        CLEAR  ls_lines.
      ENDLOOP.

      IF     ( cv_fl_append              IS INITIAL ).

        IF   ( lv_i                      IS NOT INITIAL ).

          PERFORM  f_search_special_char
                                      USING lv_string
                                   CHANGING lv_fl_found.

          IF     ( lv_fl_found           IS NOT INITIAL ).
            cv_fl_append = abap_true.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_check_special_charr
*eject
*&---------------------------------------------------------------------*
*&      Form  f_search_special_char
*&---------------------------------------------------------------------*
*       Search for special characters
*----------------------------------------------------------------------*
FORM f_search_special_char
  USING    iv_string                   TYPE STRING
  CHANGING cv_fl_found                 TYPE flag.

  DATA:    lv_string(54000)            TYPE c,
           lv_strlen                   TYPE syindex,
           lv_index                    TYPE syindex.

  FIELD-SYMBOLS: <fs>.

  CLEAR    cv_fl_found.

  IF     ( iv_string IS INITIAL ).
    RETURN.
  ENDIF.

  lv_strlen = STRLEN( iv_string ).

  CLEAR                                     lv_string.
  MOVE     iv_string                     TO lv_string.

  DO lv_strlen TIMES.

    lv_index = sy-index - 1.

    ASSIGN    lv_string+lv_index(1) TO <fs>.

    IF     ( <fs> GT '~' ).
      cv_fl_found = abap_true.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.                    " f_search_special_char
*eject
*&---------------------------------------------------------------------*
*&      Form  f_update_vendor_item
*&---------------------------------------------------------------------*
*       Update the vendor item
*----------------------------------------------------------------------*
FORM f_update_vendor_item
  TABLES   it_bsik                     TYPE ty_it_bsik
           ct_report                   TYPE ty_it_report.

  DATA:    ls_bsik                     TYPE ty_wa_bsik,
           ls_cpairs                   LIKE LINE OF s_cpairs,
           ls_report                   TYPE ty_wa_report,
           lt_report                   TYPE ty_it_report.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_bdcmsg_text              TYPE STRING,
           lv_id_doc                   TYPE char24.

  CLEAR    ct_report[].

  IF     ( it_bsik[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_report[].

  CLEAR                                     ls_bsik.
  LOOP AT  it_bsik                     INTO ls_bsik.

    IF     ( ls_bsik-zlsch               IS INITIAL ).
      CLEAR  ls_bsik.
      CONTINUE.
    ENDIF.

    IF     ( ls_cpairs-low               EQ ls_bsik-zlsch ).
    ELSE.
      CLEAR                                 ls_cpairs.
      READ     TABLE s_cpairs          INTO ls_cpairs
                                   WITH KEY low = ls_bsik-zlsch.
      IF     ( sy-subrc NE 0 ).
        CLEAR        ls_cpairs.
      ENDIF.
    ENDIF.

    IF     ( ls_cpairs-high              IS INITIAL ).
      CLEAR  ls_bsik.
      CONTINUE.
    ENDIF.

*eject
* Build the BDC transaction
    CLEAR    gt_bdcdata[].

    PERFORM  f_bdc_dynpro        USING 'SAPMF05L'     '0102'.
    PERFORM  f_bdc_field         USING 'BDC_CURSOR'   'RF05L-BELNR'.
    PERFORM  f_bdc_field         USING 'RF05L-BELNR'  ls_bsik-belnr.
    PERFORM  f_bdc_field         USING 'RF05L-BUKRS'  ls_bsik-bukrs.
    PERFORM  f_bdc_field         USING 'RF05L-GJAHR'  ls_bsik-gjahr.
    PERFORM  f_bdc_field         USING 'RF05L-BUZEI'  ls_bsik-buzei.
    PERFORM  f_bdc_field         USING 'RF05L-XKKRE'  abap_true.
    PERFORM  f_bdc_field         USING 'BDC_OKCODE'   '/00'.

    PERFORM  f_bdc_dynpro        USING 'SAPMF05L'     '0302'.
    PERFORM  f_bdc_field         USING 'BDC_CURSOR'   'BSEG-ZLSCH'.
    PERFORM  f_bdc_field         USING 'BSEG-ZLSCH'
                                       ls_cpairs-high+0(1).
*   PERFORM  f_bdc_field         USING 'BSEG-ZTERM'   record-ZTERM.
*   PERFORM  f_bdc_field         USING 'BSEG-ZFBDT'   record-ZFBDT.
*   PERFORM  f_bdc_field         USING 'BSEG-ZUONR'   record-ZUONR.
    PERFORM  f_bdc_field         USING 'BDC_OKCODE'   '=AE'.

* Call the BDC transaction
    CLEAR    lv_subrc.
    CLEAR    lv_bdcmsg_text.

    PERFORM  f_bdc_transaction       TABLES gt_bdcdata
                                      USING 'FB09'
                                   CHANGING lv_subrc
                                            lv_bdcmsg_text.

*eject
    CLEAR                                   ls_report.

    CLEAR                                   lv_id_doc.
    MOVE     ls_bsik-bukrs               TO lv_id_doc+00(04).
    MOVE     '-'                         TO lv_id_doc+04(01).
    MOVE     ls_bsik-belnr               TO lv_id_doc+05(10).
    MOVE     '-'                         TO lv_id_doc+15(01).
    MOVE     ls_bsik-gjahr               TO lv_id_doc+16(04).
    MOVE     '-'                         TO lv_id_doc+20(01).
    MOVE     ls_bsik-buzei               TO lv_id_doc+21(03).
    MOVE     lv_id_doc                   TO ls_report-id_doc.

    IF     ( ls_bsik-empfb               IS INITIAL ).
      MOVE   ls_bsik-lifnr               TO ls_report-lifnr.
    ELSE.
      MOVE   ls_bsik-empfb               TO ls_report-lifnr.
    ENDIF.

    MOVE     ls_bsik-xblnr               TO ls_report-xblnr.
    MOVE     ls_bsik-budat               TO ls_report-budat.
    MOVE     ls_bsik-bldat               TO ls_report-bldat.
    MOVE     ls_bsik-wrbtr               TO ls_report-wrbtr.
    MOVE     ls_bsik-waers               TO ls_report-waers.
    MOVE     ls_bsik-blart               TO ls_report-blart.
    MOVE     ls_bsik-bschl               TO ls_report-bschl.
    MOVE     ls_bsik-zlsch               TO ls_report-zlsch.

    MOVE     lv_bdcmsg_text              TO ls_report-tx_msg.

    IF     ( p_test                      IS NOT INITIAL ).
      CLEAR                                 ls_report-fl_error.
      MOVE     '3'                       TO ls_report-in_color.
    ELSEIF ( lv_subrc                    IS NOT INITIAL ).
      MOVE     abap_true                 TO ls_report-fl_error.
      MOVE     '6'                       TO ls_report-in_color.
    ELSE.
      CLEAR                                 ls_report-fl_error.
      MOVE     '5'                       TO ls_report-in_color.
    ENDIF.

    APPEND   ls_report                   TO lt_report.

    CLEAR  ls_bsik.
  ENDLOOP.

  ct_report[] = lt_report[].

ENDFORM.                    " f_update_vendor_item
*eject
*&---------------------------------------------------------------------*
*&      Form  f_bdc_dynpro
*&---------------------------------------------------------------------*
*       BDC Dynpro
*----------------------------------------------------------------------*
FORM f_bdc_dynpro
  USING    iv_program   TYPE ANY
           iv_dynpro    TYPE ANY.

  DATA:    ls_bdcdata   TYPE bdcdata.

  CLEAR                                     ls_bdcdata.
  MOVE     iv_program                    TO ls_bdcdata-program.
  MOVE     iv_dynpro                     TO ls_bdcdata-dynpro.
  MOVE     abap_true                     TO ls_bdcdata-dynbegin.
  APPEND                                    ls_bdcdata
                                         TO gt_bdcdata.

ENDFORM.                    "f_bdc_dynpro

*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
*       BDC Field
*----------------------------------------------------------------------*
FORM f_bdc_field
  USING    iv_fnam      TYPE ANY
           iv_fval      TYPE ANY.

  DATA:    ls_bdcdata TYPE bdcdata.

  CLEAR                                     ls_bdcdata.
  MOVE     iv_fnam                       TO ls_bdcdata-fnam.
  MOVE     iv_fval                       TO ls_bdcdata-fval.
  APPEND                                    ls_bdcdata
                                         TO gt_bdcdata.

ENDFORM.                    "f_bdc_field
*eject
*&---------------------------------------------------------------------*
*&      Form  f_bdc_transaction
*&---------------------------------------------------------------------*
*       Call the BDC transaction
*----------------------------------------------------------------------*
FORM f_bdc_transaction
  TABLES   it_bdcdata                  TYPE ty_it_bdcdata
  USING    iv_tcode                    TYPE sytcode
  CHANGING cv_subrc                    TYPE sysubrc
           cv_bdcmsg_text              TYPE STRING.

  DATA:    ls_bdcmsg                   TYPE bdcmsgcoll,
           lt_bdcmsg                   TYPE STANDARD TABLE
                                         OF bdcmsgcoll,
           lwa_opt                     TYPE ctu_params.

  DATA:    lv_subrc1                   TYPE sysubrc,
           lv_subrc2                   TYPE sysubrc,
           lv_bdcmsg_text              TYPE STRING.

  CLEAR    cv_subrc.
  CLEAR    cv_bdcmsg_text.

  IF     ( p_test                        IS NOT INITIAL ).
    MOVE   text-301                      TO cv_bdcmsg_text.
    RETURN.
  ENDIF.

  IF   ( ( it_bdcdata[]                  IS INITIAL ) OR
         ( iv_tcode                      IS INITIAL )    ).
    RETURN.
  ENDIF.

* Update the document
  CLEAR                                     lwa_opt.
  MOVE     'N'                           TO lwa_opt-dismode.
  MOVE     'S'                           TO lwa_opt-updmode.
  MOVE     'X'                           TO lwa_opt-defsize.

  CLEAR    lt_bdcmsg[].
  CLEAR    lv_subrc1.
  CLEAR    lv_subrc2.

  CALL TRANSACTION iv_tcode
             USING it_bdcdata
      OPTIONS FROM lwa_opt
     MESSAGES INTO lt_bdcmsg.

  lv_subrc1 = sy-subrc.

  CLEAR                                     ls_bdcmsg.
  READ     TABLE lt_bdcmsg             INTO ls_bdcmsg
                                   WITH KEY msgid = 'F5'
                                            msgnr = '300'.
  lv_subrc2 = sy-subrc.

*eject
* Successfully updated the documently updated
  IF   ( ( lv_subrc1                     IS INITIAL ) AND
         ( lv_subrc2                     IS INITIAL )     ).

    MOVE   text-302                      TO lv_bdcmsg_text.

* Failed to update the document
  ELSE.

    cv_subrc = 8.

    CLEAR                                   ls_bdcmsg.
    READ     TABLE lt_bdcmsg           INTO ls_bdcmsg
                                   WITH KEY msgtyp = 'E'.
    IF     ( sy-subrc EQ 0 ).

      CLEAR    lv_bdcmsg_text.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          MSGID               = ls_bdcmsg-msgid
          MSGNR               = ls_bdcmsg-msgnr
          MSGV1               = ls_bdcmsg-msgv1
          MSGV2               = ls_bdcmsg-msgv2
          MSGV3               = ls_bdcmsg-msgv3
          MSGV4               = ls_bdcmsg-msgv4
        IMPORTING
          MESSAGE_TEXT_OUTPUT = lv_bdcmsg_text.

    ELSE.

* Candidate document was NOT updated
      MOVE   text-303                    TO lv_bdcmsg_text.

    ENDIF.

  ENDIF.

  cv_bdcmsg_text = lv_bdcmsg_text.

ENDFORM.                    " f_bdc_transaction
*eject
*&---------------------------------------------------------------------*
*&      Form  f_output_report
*&---------------------------------------------------------------------*
*       Output the report
*----------------------------------------------------------------------*
FORM f_output_report
  TABLES   it_report                   TYPE ty_it_report.

  DATA:    ls_report                   TYPE ty_wa_report.

  DATA:    lv_amount                   TYPE char16.

  CLEAR                                     ls_report.
  LOOP AT  it_report                   INTO ls_report.

    CLEAR                                   lv_amount.
    WRITE   ls_report-wrbtr              TO lv_amount
                                   CURRENCY ls_report-waers
                    NO-SIGN RIGHT-JUSTIFIED.


    FORMAT    INTENSIFIED OFF.

    SKIP      1.

    WRITE: /001 ls_report-id_doc,
            027 ls_report-lifnr,
            039 ls_report-xblnr,
            057 ls_report-budat,
            069 ls_report-bldat,
            081 lv_amount,
            099 ls_report-waers,
            106 ls_report-blart,
            110 ls_report-bschl,
            114 ls_report-zlsch.

    WRITE: /003 ls_report-tx_msg(118) COLOR = ls_report-in_color.

    CLEAR  ls_report.
  ENDLOOP.

ENDFORM.                    " f_output_report
*eject
*&---------------------------------------------------------------------*
*&      Form  f_print_report_header
*&---------------------------------------------------------------------*
*       Print the top of page report header
*----------------------------------------------------------------------*
FORM f_print_report_header.

  CLEAR                                     gv_sysid.
  MOVE     sy-sysid                      TO gv_sysid.
  MOVE     sy-mandt                      TO gv_sysid+4(3).
  CLEAR                                     gv_uname.
  MOVE     sy-uname                      TO gv_uname.
  CLEAR                                     gv_pagno.
  MOVE     sy-pagno                      TO gv_pagno.
  CLEAR                                     gv_cprog.
  MOVE     sy-cprog                      TO gv_cprog.
  CLEAR                                     gv_datum.
  WRITE:   sy-datum                      TO gv_datum.
  CLEAR                                     gv_uzeit.
  WRITE:   sy-uzeit                      TO gv_uzeit.

  FORMAT COLOR OFF INTENSIFIED OFF.

  WRITE: /001 text-h11,
          012 gv_sysid,
          040 text-h12,
          052 gv_cprog,
          101 text-h13,
          111 gv_datum.

  WRITE: /001 text-h21,
          012 gv_uname,
          101 text-h23,
          113 gv_uzeit.

  WRITE: /105 text-h33,
          116 gv_pagno.

  SKIP    1.

  IF     ( gt_report[] IS INITIAL ).
    RETURN.
  ENDIF.

  WRITE: /001 text-ch1, "ls_report-id_doc,
          027 text-ch2, "ls_report-lifnr,
          039 text-ch3, "ls_report-xblnr,
          057 text-ch4, "ls_report-budat,
          069 text-ch5, "ls_report-bldat,
          086 text-ch6, "lv_amount,
          099 text-ch7, "ls_report-waers,
          106 text-ch8, "ls_report-blart,
          110 text-ch9, "ls_report-bschl,
          114 text-cha. "ls_report-zlsch.

ENDFORM.                    " f_print_report_header
