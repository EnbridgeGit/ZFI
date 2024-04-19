*&---------------------------------------------------------------------*
*&  Include           ZFAPI008_POWERADVOCATE_F01
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:          ZFAPI008_POWERADVOCATE_F01                        *
*  Program:          ZFAPI008_POWERADVOCATE                            *
*  Author:           John Hartung                                      *
*  Date:             May 19, 2014                                      *
*  Track #:          TR928 Release 1 - Streamline                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      AP PowerAdvocate - Outbound - Forms Include       *
*                                                                      *
*                    Extract cleared vendor docs - Subroutines         *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By           Description                                    *
* -------- ------------ ---------------------------------------------- *
* 05/19/14 JRHARTUNG    D30K924122 - Initial program development       *
*                       D30K924256, D30K924362, D30K924476             *
* 07/26/17 JRHARTUNG    ACR-4710    D30K928284, D30K928296, D30K928308 *
*                       PowerAdvocate Break/Fix Enhancements           *
*                       1. Add Payment Method "N" - E-Payables BOA     *
*                       2. Replace invalid characters with a blank     *
*                       3. Scan Material Description for invalid chars *
*                       4. Copy the PO Number to all items in an MM    *
*                          (MIRO) Invoice (commented out D30K928520 )  *
*----------------------------------------------------------------------*
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 07-03-2019   KMB         D30K929692  CHG0133164 Power Advocate       *
*                                      Additional fields               *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 07-03-2019   KMB         D30K929745  CHG0133164 Power Advocate       *
*                                      Additional fields               *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 15-04-2019   KMB         D30K929768  ENHC0025289 Power Advocate      *
*                                      Additional fields               *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 16-05-2019   KMB         D30K929840  ENHC0025289 Power Advocate      *
*                                      Additional fields               *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 19-06-2019   KMB         D30K929947  CHG0149221 Power Advocate       *
*                                      Network addition                *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 11-07-2019   KMB         D30K930020  CHG0150958 fixing of data       *
*                                      parsing errors in output files  *
*                                      & Eliminate Employee data       *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 27-09-2019   KMB         D30K930187  CHG0161101 DFCT0017698 Data     *
*                                      parsing issue                   *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 14-10-2019   KMB         D30K930212  CHG0161101 DFCT0017698 Data     *
*                                      parsing issue in other fiels    *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 22-10-2019   KMB         D30K930224  CHG0161101 DFCT0017698 Data     *
*                                      parsing issue in other fiels    *
************************************************************************


*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_xparam
*&---------------------------------------------------------------------*
*       Get the program parameter values
*----------------------------------------------------------------------*
FORM f_select_xparam.

  DATA:    lwa_zvar                    TYPE ty_wa_zvar.

  CLEAR    git_zvar[].
  CLEAR    gv_obj_id.
*  CLEAR    p_objid. " COMMNEBTED BY AKMADASU

  CLEAR    gv_obj_title.
  CLEAR    p_objttl.

  SELECT   *
    INTO   TABLE git_zvar
    FROM   zvar
**--START OF CHNAGES BY AKMADASU
*   WHERE   programm = gc_object_id.
    WHERE   programm = p_objid.
**-- END OF CHANGES BY AKMADASU
  IF ( sy-subrc EQ 0 ).
    SORT   git_zvar       ASCENDING BY mandt programm varname varnum.
**--START OF CHNAGES BY AKMADASU
    MOVE   p_objid             TO gv_obj_id.
*    MOVE   gc_object_id             TO gv_obj_id.
*    MOVE   gc_object_id             TO p_objid.
**--END OF CHNAGES BY AKMADASU
  ELSE.
    CLEAR  git_zvar[].
    CLEAR  gv_obj_id.
    CLEAR  p_objid.
  ENDIF.

* Set the report title
  CLEAR                                     lwa_zvar.
  READ     TABLE git_zvar              INTO lwa_zvar
                                   WITH KEY varname = gc_object_title.
  IF     ( sy-subrc EQ 0 ).

    CLEAR                                   gv_obj_title.
    MOVE   lwa_zvar-value1               TO gv_obj_title.

    CLEAR                                   p_objttl.
    MOVE   gv_obj_title                  TO p_objttl.

    CLEAR                                   sy-title.
    MOVE   gv_obj_title                  TO sy-title.

  ENDIF.

ENDFORM.                    " f_select_xparam
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initialization
*&---------------------------------------------------------------------*
*       Initialization
*----------------------------------------------------------------------*
FORM f_initialization.

  DATA:    lwa_zvar                    TYPE ty_wa_zvar,
           lwa_t001u                   TYPE ty_wa_t001u,
           lit_t001u                   TYPE ty_it_t001u,
           lwa_glic                    LIKE LINE OF s_glic.

  DATA:    lrw_blart                   LIKE LINE OF grt_blart,
           lrw_glrt                    LIKE LINE OF s_glrt.

  DATA:    lt_sscr_restrict            TYPE sscr_restrict,
           ls_sscr_opt_list            TYPE sscr_opt_list,
           ls_sscr_ass                 TYPE sscr_ass.

  DATA:    lv_blart                    TYPE blart,
           lv_hkont                    TYPE hkont.

* Build the document type exclusion select-option
  CLEAR                                     s_blartx[].
  CLEAR                                     lwa_zvar.
  LOOP AT  git_zvar                    INTO lwa_zvar
                                      WHERE varname = gc_doc_type_excl.
    CLEAR                                   lv_blart.
    MOVE   lwa_zvar-value1               TO lv_blart.
    CLEAR                                   lrw_blart.
    MOVE   'I'                           TO lrw_blart-sign.
    MOVE   'EQ'                          TO lrw_blart-option.
    MOVE   lv_blart                      TO lrw_blart-low.
    APPEND                                  lrw_blart
                                         TO s_blartx.
    CLEAR  lwa_zvar.
  ENDLOOP.

  SORT     s_blartx ASCENDING            BY low.
  DELETE   ADJACENT DUPLICATES         FROM s_blartx
                                  COMPARING low.

*eject
* Build the retainage account exclusion select-option
  CLEAR                                     s_glrt[].
  CLEAR                                     grt_hkont_ret_v[].
  CLEAR                                     grt_hkont_ret_a[].
  CLEAR                                     lrw_glrt.
  MOVE     'I'                           TO lrw_glrt-sign.
  MOVE     'EQ'                          TO lrw_glrt-option.
  CLEAR                                     lwa_zvar.
  LOOP AT  git_zvar                    INTO lwa_zvar
                                      WHERE varname = gc_ret_acct_excl.
    CLEAR                                   lv_hkont.
    MOVE   lwa_zvar-value1+1(10)         TO lv_hkont.
    CLEAR                                   lrw_glrt-low.
    MOVE   lv_hkont                      TO lrw_glrt-low.
    APPEND                                  lrw_glrt
                                         TO s_glrt.
    IF   ( lwa_zvar-value1+0(1)          EQ 'V' ).
      APPEND                                lrw_glrt
                                         TO grt_hkont_ret_v.
    ELSE.
      APPEND                                lrw_glrt
                                         TO grt_hkont_ret_a.
    ENDIF.
    CLEAR  lwa_zvar.
  ENDLOOP.

  CLEAR                                     lrw_glrt-low.
  MOVE     'ZZZZZZZZZZ'                  TO lrw_glrt-low.
  IF     ( grt_hkont_ret_v[]             IS INITIAL ).
    APPEND                                  lrw_glrt
                                         TO grt_hkont_ret_v.
  ENDIF.
  IF     ( grt_hkont_ret_a[]             IS INITIAL ).
    APPEND                                  lrw_glrt
                                         TO grt_hkont_ret_a.
  ENDIF.

  SORT     s_glrt   ASCENDING            BY low.
  DELETE   ADJACENT DUPLICATES         FROM s_glrt
                                  COMPARING low.
  SORT     grt_hkont_ret_v ASCENDING     BY low.
  DELETE   ADJACENT DUPLICATES         FROM grt_hkont_ret_v
                                  COMPARING low.
  SORT     grt_hkont_ret_a ASCENDING     BY low.
  DELETE   ADJACENT DUPLICATES         FROM grt_hkont_ret_a
                                  COMPARING low.


*eject
* Build option list
  CLEAR    lt_sscr_restrict.

  CLEAR                                ls_sscr_opt_list.
  MOVE     'OPT_LIST'               TO ls_sscr_opt_list-name.
  MOVE     'X'                      TO ls_sscr_opt_list-options-eq.
  APPEND                               ls_sscr_opt_list
                                    TO lt_sscr_restrict-opt_list_tab.

* Apply the option list to the selection-screen select-option lists
  CLEAR                                ls_sscr_ass.
  MOVE     'S'                      TO ls_sscr_ass-kind.
  MOVE     'S_BLARTX'               TO ls_sscr_ass-name.
  MOVE     'I'                      TO ls_sscr_ass-sg_main.
  MOVE     'OPT_LIST'               TO ls_sscr_ass-op_main.
  APPEND                               ls_sscr_ass
                                    TO lt_sscr_restrict-ass_tab.
  CLEAR                                ls_sscr_ass-name.
  MOVE     'S_GLIC'                 TO ls_sscr_ass-name.
  APPEND                               ls_sscr_ass
                                    TO lt_sscr_restrict-ass_tab.
  CLEAR                                ls_sscr_ass-name.
  MOVE     'S_GLRT'                 TO ls_sscr_ass-name.
  APPEND                               ls_sscr_ass
                                    TO lt_sscr_restrict-ass_tab.
**--START OF CHANGES BY AKMADASU
  IF gv_flag IS INITIAL.
    gv_flag = 'X'.
**--END OF CHANGES BY AKMADASU
    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING
        restriction = lt_sscr_restrict.
  ENDIF." ADDED BY AKMADASU
*eject
* Set the G/L intercompany crossbill parameter
  CLEAR    s_glic[].

  CLEAR    lit_t001u[].
  SELECT   *
    INTO   TABLE lit_t001u
    FROM   t001u.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lit_t001u[].
  ENDIF.

  CLEAR                                     lwa_t001u.
  LOOP AT  lit_t001u                   INTO lwa_t001u.
    CLEAR                                   lwa_glic.
    MOVE   'I'                           TO lwa_glic-sign.
    MOVE   'EQ'                          TO lwa_glic-option.
    MOVE   lwa_t001u-konts               TO lwa_glic-low.
    APPEND                                  lwa_glic
                                         TO s_glic.
    CLEAR                                   lwa_glic-low.
    MOVE   lwa_t001u-konth               TO lwa_glic-low.
    APPEND                                  lwa_glic
                                         TO s_glic.
    CLEAR  lwa_t001u.
  ENDLOOP.

  DELETE   s_glic                     WHERE low IS INITIAL.
  SORT     s_glic              ASCENDING BY low.
  DELETE   ADJACENT DUPLICATES         FROM s_glic
                                  COMPARING low.

*eject
* Set the file parameters
  CLEAR    p_fpath1.
  CLEAR    p_fpath2.
  CLEAR    p_fpath3.

  CLEAR                                     lwa_zvar.
  READ     TABLE git_zvar              INTO lwa_zvar
                                   WITH KEY varname = gc_file1.
  IF     ( sy-subrc EQ 0 ).
**--START OF CHANGES BY AKMADASU
    lwa_zvar-value1+20(3) = sy-sysid.
**-- END OF CHANGES BY AKMADASU
    CONCATENATE                             lwa_zvar-value1
                                            lwa_zvar-value2
                                       INTO p_fpath1.
  ENDIF.

  CLEAR                                     lwa_zvar.
  READ     TABLE git_zvar              INTO lwa_zvar
                                   WITH KEY varname = gc_file2.
  IF     ( sy-subrc EQ 0 ).
**--START OF CHANGES BY AKMADASU
    lwa_zvar-value1+20(3) = sy-sysid.
**-- END OF CHANGES BY AKMADASU
    CONCATENATE                             lwa_zvar-value1
                                            lwa_zvar-value2
                                       INTO p_fpath2.
  ENDIF.

  CLEAR                                     lwa_zvar.
  READ     TABLE git_zvar              INTO lwa_zvar
                                   WITH KEY varname = gc_file3.
  IF     ( sy-subrc EQ 0 ).
**--START OF CHANGES BY AKMADASU
    lwa_zvar-value1+20(3) = sy-sysid.
**-- END OF CHANGES BY AKMADASU
    CONCATENATE                             lwa_zvar-value1
                                            lwa_zvar-value2
                                       INTO p_fpath3.
  ENDIF.

ENDFORM.                    " f_initialization
*eject
*&---------------------------------------------------------------------*
*&      Form  f_at_sel_screen_output
*&---------------------------------------------------------------------*
*       AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
FORM f_at_sel_screen_output.

  DATA:    lwa_zvar                    TYPE ty_wa_zvar.

  DATA:    lv_subrc                    TYPE sysubrc.

  LOOP AT SCREEN.

* Set display only select-options
    IF       ( screen-group1 EQ gc_modif_id_dsp   ).
      IF ( ( ( screen-name   CS 'BLARTX'    ) AND
             ( screen-name   CS 'VALU_PUSH' )     ) OR
           ( ( screen-name   CS 'S_GLIC'    ) AND
             ( screen-name   CS 'VALU_PUSH' )     ) OR
           ( ( screen-name   CS 'S_GLRT'    ) AND
             ( screen-name   CS 'VALU_PUSH' )     )    ).
      ELSE.
        screen-input  = 0.
        screen-output = 1.
        MODIFY   SCREEN.
      ENDIF.
    ENDIF.

* If the object ID is not assigned, then disable output file parameters
    IF     ( p_objid       IS INITIAL ).

    ELSEIF ( rb_appl       EQ gc_x ). "output file appl-srvr radiobutton

      IF   ( screen-group1 EQ gc_modif_id_dlm ).
        screen-input = 1.
        MODIFY   SCREEN.
      ENDIF.

    ELSEIF ( rb_pres       EQ gc_x ). "output file pres-srvr radiobutton

      IF   ( screen-group1 EQ gc_modif_id_dlm ).
        screen-active = 0.
        MODIFY   SCREEN.
      ENDIF.

    ENDIF.

  ENDLOOP.

*eject
* Clear the output filepath when switching from appl to pres server
  IF     ( ( rb_pres EQ gc_x )   AND ( gv_flag_pres IS INITIAL ) ).

    MOVE     gc_x                 TO   gv_flag_pres.

    CLEAR    p_fpath1.
    CLEAR    p_fpath2.
    CLEAR    p_fpath3.

* If appl server is selected, set the filepath from ZFIT_XPARAM
  ELSEIF   ( rb_appl EQ gc_x ).

    CLEAR                                   lwa_zvar.
    READ     TABLE git_zvar            INTO lwa_zvar
                                   WITH KEY varname = gc_file1.
    lv_subrc = sy-subrc.

    IF       ( p_fpath1                  IS INITIAL     ).
      IF     ( p_fpa1nd                  IS NOT INITIAL ).
        CLEAR                               p_fpath1.
        MOVE   p_fpa1nd                  TO p_fpath1.
      ELSEIF ( lv_subrc                  EQ 0 ).
        CLEAR                               p_fpath1.
        CONCATENATE                         lwa_zvar-value1
                                            lwa_zvar-value2
                                       INTO p_fpath1.
      ENDIF.
    ENDIF.

    CLEAR                                   lwa_zvar.
    READ     TABLE git_zvar            INTO lwa_zvar
                                   WITH KEY varname = gc_file2.
    lv_subrc = sy-subrc.

    IF       ( p_fpath2                  IS INITIAL     ).
      IF     ( p_fpa2nd                  IS NOT INITIAL ).
        CLEAR                               p_fpath2.
        MOVE   p_fpa2nd                  TO p_fpath2.
      ELSEIF ( lv_subrc                  EQ 0 ).
        CLEAR                               p_fpath2.
        CONCATENATE                         lwa_zvar-value1
                                            lwa_zvar-value2
                                       INTO p_fpath2.
      ENDIF.
    ENDIF.

*eject
    CLEAR                                   lwa_zvar.
    READ     TABLE git_zvar            INTO lwa_zvar
                                   WITH KEY varname = gc_file3.
    lv_subrc = sy-subrc.

    IF       ( p_fpath3                  IS INITIAL     ).
      IF     ( p_fpa3nd                  IS NOT INITIAL ).
        CLEAR                               p_fpath3.
        MOVE   p_fpa3nd                  TO p_fpath3.
      ELSEIF ( lv_subrc                  EQ 0 ).
        CLEAR                               p_fpath3.
        CONCATENATE                         lwa_zvar-value1
                                            lwa_zvar-value2
                                       INTO p_fpath3.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " f_at_sel_screen_output
*eject
*&---------------------------------------------------------------------*
*&      Form  f_at_sel_out_set_lstbx_valus
*&---------------------------------------------------------------------*
*       Set the listbox values
*----------------------------------------------------------------------*
FORM f_at_sel_out_set_lstbx_valus.

  DATA:    lwa_datsrc_values   LIKE LINE OF git_datsrc_values,
           lwa_delim_values    LIKE LINE OF git_delim_values.

* Set the listbox values for the data source
  CLEAR    git_datsrc_values[].

  CLEAR                                     lwa_datsrc_values.
  MOVE     gc_datsrc_us                  TO lwa_datsrc_values-key.
  APPEND                                    lwa_datsrc_values
                                         TO git_datsrc_values.

  CLEAR                                     lwa_datsrc_values.
  MOVE     gc_datsrc_ug                  TO lwa_datsrc_values-key.
  APPEND                                    lwa_datsrc_values
                                         TO git_datsrc_values.

  CLEAR                                     lwa_datsrc_values.
  MOVE     gc_datsrc_sw                  TO lwa_datsrc_values-key.
  APPEND                                    lwa_datsrc_values
                                         TO git_datsrc_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gc_datsrc_id
      values          = git_datsrc_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF     ( sy-subrc NE 0 ).
*   MESSAGE e000(zfi01) WITH text-044.
  ENDIF.

*eject
* Set the listbox values for the delimeter
  CLEAR    git_delim_values[].

  CLEAR                                     lwa_delim_values.
  MOVE     gc_pipe_lit                   TO lwa_delim_values-key.
  APPEND                                    lwa_delim_values
                                         TO git_delim_values.

  CLEAR                                     lwa_delim_values.
  MOVE     gc_comma_lit                  TO lwa_delim_values-key.
  APPEND                                    lwa_delim_values
                                         TO git_delim_values.

  CLEAR                                     lwa_delim_values.
  MOVE     gc_tab_lit                    TO lwa_delim_values-key.
  APPEND                                    lwa_delim_values
                                         TO git_delim_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gc_delim_id
      values          = git_delim_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF     ( sy-subrc NE 0 ).
*   MESSAGE e000(zfi01) WITH text-044.
  ENDIF.

ENDFORM.                    " f_at_sel_out_set_lstbx_valus
*eject
*&---------------------------------------------------------------------*
*&      Form  f_f4_help_pres_file
*&---------------------------------------------------------------------*
*       F4 help - presentation server - file open dialog
*----------------------------------------------------------------------*
FORM f_f4_help_pres_file
  CHANGING cv_fpath                    TYPE localfile.

  DATA:    lwa_file_table              TYPE file_table,
           lit_file_table              TYPE filetable.

  DATA:    lv_rc                       TYPE i,
           lv_window_title             TYPE string,
           lv_initial_directory        TYPE string.

  CLEAR                                     lv_window_title.
  MOVE     text-091                      TO lv_window_title.
  CLEAR                                     lv_initial_directory.
  MOVE     'C:\'                         TO lv_initial_directory.

  CLEAR    lit_file_table[].
  CLEAR    lv_rc.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_window_title
      initial_directory       = lv_initial_directory
      multiselection          = ' '
    CHANGING
      file_table              = lit_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF     ( sy-subrc EQ 0 ).

    CLEAR                                   lwa_file_table.
    READ   TABLE lit_file_table        INTO lwa_file_table
                                      INDEX 1.
    IF ( sy-subrc EQ 0 ).
      CLEAR                                 cv_fpath.
      MOVE       lwa_file_table-filename TO cv_fpath.
    ENDIF.

  ENDIF.

ENDFORM.                    " f_f4_help_pres_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_xparam_entries
*&---------------------------------------------------------------------*
*       Check if the ZFIT_XPARAM entries have been maintained
*----------------------------------------------------------------------*
FORM f_validate_xparam_entries.

  DATA:    lwa_zvar                    TYPE ty_wa_zvar.

* Reset output presentation server flag to control clearing of filepath
  IF       ( rb_appl                     IS NOT INITIAL ).

    IF     ( gv_flag_pres                IS NOT INITIAL ).
      CLEAR                                 p_fpath1.
      CLEAR                                 p_fpath2.
      CLEAR                                 p_fpath3.
    ELSE.

      CLEAR                                 p_fpa1nd.
      MOVE     p_fpath1                  TO p_fpa1nd.
      CLEAR                                 p_fpa2nd.
      MOVE     p_fpath2                  TO p_fpa2nd.
      CLEAR                                 p_fpa3nd.
      MOVE     p_fpath3                  TO p_fpa3nd.
    ENDIF.

    CLEAR    gv_flag_pres.

  ENDIF.

  IF     ( ( rb_pres                     IS NOT INITIAL ) AND
           ( gv_flag_pres                IS INITIAL     )     ).
    CLEAR                                   p_fpa1nd.
    CLEAR                                   p_fpa2nd.
    CLEAR                                   p_fpa3nd.
    IF     ( p_fpath1                    IS NOT INITIAL ).
      MOVE   p_fpath1                    TO p_fpa1nd.
    ENDIF.
    IF     ( p_fpath2                    IS NOT INITIAL ).
      MOVE   p_fpath2                    TO p_fpa2nd.
    ENDIF.
    IF     ( p_fpath3                    IS NOT INITIAL ).
      MOVE   p_fpath3                    TO p_fpa3nd.
    ENDIF.
  ENDIF.

* Check if the program parameter table is maintained
  IF     ( git_zvar[]                    IS INITIAL ).
    MESSAGE  e000(zfi01) WITH text-101.

*eject
* Check if the output filepaths have been maintained
  ELSEIF ( rb_appl                       IS NOT INITIAL ).
    CLEAR                                   lwa_zvar.
    READ     TABLE git_zvar            INTO lwa_zvar
                                   WITH KEY varname = gc_file1.
    IF     ( sy-subrc EQ 0 ).
      IF         ( lwa_zvar-value1       IS INITIAL ).
        MESSAGE  e000(zfi01) WITH text-111.
      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-111.
    ENDIF.
    CLEAR                                   lwa_zvar.
    READ     TABLE git_zvar            INTO lwa_zvar
                                   WITH KEY varname = gc_file2.
    IF     ( sy-subrc EQ 0 ).
      IF         ( lwa_zvar-value1       IS INITIAL ).
        MESSAGE  e000(zfi01) WITH text-112.
      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-112.
    ENDIF.
    CLEAR                                   lwa_zvar.
    READ     TABLE git_zvar            INTO lwa_zvar
                                   WITH KEY varname = gc_file3.
    IF     ( sy-subrc EQ 0 ).
      IF         ( lwa_zvar-value1       IS INITIAL ).
        MESSAGE  e000(zfi01) WITH text-113.
      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-113.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_validate_xparam_entries
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  DATA:    lv_date                     TYPE sydatum,
           lv_time                     TYPE syuzeit.

  DATA:    lrw_bwart                   LIKE LINE OF grt_bwart,
           lwa_tbsl                    TYPE ty_wa_tbsl.

  DATA:    lv_ltext                    TYPE text_bslt.

* Initial the data elements
  CLEAR    grt_blart[].
  CLEAR    grt_bwart[].
  CLEAR    git_t001[].
  CLEAR    git_t001w[].
  CLEAR    git_t003[].
  CLEAR    git_t007s[].
  CLEAR    git_t023t[].
  CLEAR    git_t059t[].
  CLEAR    git_t161t[].
  CLEAR    git_tbsl[].
  CLEAR    git_skat[].
  CLEAR    git_bsak[].
  CLEAR    git_cc_doc[].
  CLEAR    git_bkpf[].
  CLEAR    git_bseg[].
  CLEAR    git_vendor[].
  CLEAR    git_po_data[].
  CLEAR    git_out_item[].
  CLEAR    git_out_po[].
  CLEAR    git_out_doc[].
  CLEAR    git_msg_data[].
  CLEAR    git_msg_proc[].

  CLEAR    gv_cnt_rec_file1.
  CLEAR    gv_cnt_rec_file2.
  CLEAR    gv_cnt_rec_file3.
  CLEAR    gv_count_err_data.
  CLEAR    gv_count_err_proc.
  CLEAR    gv_flag_err_data.
  CLEAR    gv_flag_err_proc.
  CLEAR    gv_flag_err_mstr.
  CLEAR    gv_flag_err_vldt.

* Set the program title
  CLEAR                                     sy-title.
  MOVE     gv_obj_title                  TO sy-title.

*eject
* Set the file delimiter
  CLEAR    gv_delim.

  CASE     p_delim.
    WHEN     gc_pipe_lit.
      MOVE   gc_pipe                     TO gv_delim.
    WHEN     gc_comma_lit.
      MOVE   gc_comma                    TO gv_delim.
    WHEN     gc_tab_lit.
      MOVE   gc_tab                      TO gv_delim.
    WHEN     OTHERS.
      MOVE   gc_comma                    TO gv_delim.
  ENDCASE.

* Set the physical filenames
  CLEAR                                     gv_phy_file1.
  MOVE     p_fpath1                      TO gv_phy_file1.
  CLEAR                                     gv_phy_file2.
  MOVE     p_fpath2                      TO gv_phy_file2.
  CLEAR                                     gv_phy_file3.
  MOVE     p_fpath3                      TO gv_phy_file3.

  CLEAR                                     lv_date.
  MOVE     sy-datum                      TO lv_date.
  CLEAR                                     lv_time.
  MOVE     sy-uzeit                      TO lv_time.

  REPLACE  'YYYYMMDD'                  WITH lv_date
                                       INTO gv_phy_file1.
  REPLACE  'HHMMSS'                    WITH lv_time
                                       INTO gv_phy_file1.
  REPLACE  'YYYYMMDD'                  WITH lv_date
                                       INTO gv_phy_file2.
  REPLACE  'HHMMSS'                    WITH lv_time
                                       INTO gv_phy_file2.
  REPLACE  'YYYYMMDD'                  WITH lv_date
                                       INTO gv_phy_file3.
  REPLACE  'HHMMSS'                    WITH lv_time
                                       INTO gv_phy_file3.

*eject
* Set the document types
  PERFORM  f_set_document_types.

* Select the plant data
  CLEAR    git_t001w[].
  SELECT   werks  ort01  adrnr
    INTO   TABLE  git_t001w
    FROM   t001w.
  IF     ( sy-subrc EQ 0 ).
    SORT   git_t001w ASCENDING BY werks.
  ELSE.
    CLEAR  git_t001w[].
  ENDIF.

* Select the material group descriptions
  CLEAR    git_t023t[].
  SELECT   matkl  wgbez
    INTO   TABLE  git_t023t
    FROM   t023t
   WHERE   spras = 'E'.
  IF     ( sy-subrc EQ 0 ).
    SORT   git_t023t ASCENDING BY matkl.
  ELSE.
    CLEAR  git_t023t[].
  ENDIF.

* Select the minority indicator text
  CLEAR    git_t059t[].
  SELECT   mindk  mtext
    INTO   TABLE  git_t059t
    FROM   t059t
   WHERE   spras = 'E'.
  IF     ( sy-subrc EQ 0 ).
    SORT   git_t059t ASCENDING BY mindk.
  ELSE.
    CLEAR  git_t059t[].
  ENDIF.

* Select the purchasing document type descriptions
  CLEAR    git_t161t[].
  SELECT   bsart  bstyp  batxt
    INTO   TABLE  git_t161t
    FROM   t161t
   WHERE   spras = 'E'.
  IF     ( sy-subrc EQ 0 ).
    SORT   git_t161t ASCENDING BY bsart  bstyp.
  ELSE.
    CLEAR  git_t161t[].
  ENDIF.

*eject
* Set the goods receipt movement type
  CLEAR                                     lrw_bwart.
  MOVE     'I'                           TO lrw_bwart-sign.
  MOVE     'EQ'                          TO lrw_bwart-option.
  MOVE     '101'                         TO lrw_bwart-low.
  APPEND                                    lrw_bwart
                                         TO grt_bwart.

* Select the posting keys
  CLEAR    lwa_tbsl.
  SELECT   bschl  shkzg
    INTO   lwa_tbsl
    FROM   tbsl.

    CLEAR    lv_ltext.
    SELECT   SINGLE ltext
      INTO   lv_ltext
      FROM   tbslt
     WHERE   spras = 'E'
       AND   bschl = lwa_tbsl-bschl
       AND   umskz = ' '.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lv_ltext.
    ENDIF.

    IF     ( lv_ltext                    IS INITIAL ).
      CLEAR                                 lwa_tbsl-ltext.
      CONCATENATE                           lwa_tbsl-bschl '_'
                                            'XXXX'
                                       INTO lwa_tbsl-ltext.
    ELSE.
      CLEAR                                 lwa_tbsl-ltext.
      CONCATENATE                           lwa_tbsl-bschl '_'
                                            lv_ltext
                                       INTO lwa_tbsl-ltext.
    ENDIF.

    APPEND   lwa_tbsl                    TO git_tbsl.

    CLEAR  lwa_tbsl.
  ENDSELECT.

  SORT     git_tbsl            ASCENDING BY bschl.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_document_types
*&---------------------------------------------------------------------*
*       Set the document types
*----------------------------------------------------------------------*
FORM f_set_document_types.

  DATA:    lwa_zvar                    TYPE ty_wa_zvar,
           lrw_blart                   LIKE LINE OF grt_blart,
           lwa_t003                    TYPE ty_wa_t003,
           lwa_t003t                   TYPE t003t,
           lit_t003t                   LIKE STANDARD TABLE OF lwa_t003t.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_blart                    TYPE blart.

  CLEAR    grt_blart[].

* Initial the document type range table
  CLEAR    git_t003[].
  SELECT   blart  brgru
    INTO   TABLE git_t003
    FROM   t003
   WHERE   blart IN s_blart.
  IF     ( sy-subrc EQ 0 ).
    SORT   git_t003  ASCENDING BY blart.
  ELSE.
    CLEAR  git_t003[].
  ENDIF.

  CLEAR    lit_t003t[].
  SELECT   *
    INTO   TABLE lit_t003t
    FROM   t003t
   WHERE   spras = sy-langu.
  IF     ( sy-subrc EQ 0 ).
    SORT   lit_t003t ASCENDING BY blart.
  ELSE.
    CLEAR  lit_t003t[].
  ENDIF.

*eject
  CLEAR                                     lwa_t003.
  LOOP AT  git_t003                    INTO lwa_t003.
    lv_tabix = sy-tabix.

    CLEAR                                   lwa_t003t.
    READ     TABLE lit_t003t           INTO lwa_t003t
                                   WITH KEY blart = lwa_t003-blart
                              BINARY SEARCH.
    IF     ( sy-subrc EQ 0 ).
      CLEAR                                 lwa_t003-ltext.
      MOVE                                  lwa_t003t-ltext
                                         TO lwa_t003-ltext.
      MODIFY                                git_t003
                                       FROM lwa_t003
                                      INDEX lv_tabix
                               TRANSPORTING ltext.
    ENDIF.

    CLEAR                                   lv_blart.
    MOVE     lwa_t003-blart              TO lv_blart.
    IF   ( ( lwa_t003-brgru+0(2)         EQ 'AP'       ) AND
           ( lv_blart                    IN s_blart[]  ) AND
           ( lv_blart                NOT IN s_blartx[] )     ).
      CLEAR                                 lrw_blart.
      MOVE   'I'                         TO lrw_blart-sign.
      MOVE   'EQ'                        TO lrw_blart-option.
      MOVE   lv_blart                    TO lrw_blart-low.
      APPEND                                lrw_blart
                                         TO grt_blart.
    ENDIF.

    CLEAR  lwa_t003.
  ENDLOOP.

* Append the document type inclusions
  CLEAR                                     lwa_zvar.
  LOOP AT  git_zvar                    INTO lwa_zvar
                                      WHERE varname = gc_doc_type_incl.
    CLEAR                                   lv_blart.
    MOVE   lwa_zvar-value1               TO lv_blart.
    IF   ( ( lv_blart                    IN s_blart[]  ) AND
           ( lv_blart                NOT IN s_blartx[] )     ).
      CLEAR                                 lrw_blart.
      MOVE   'I'                         TO lrw_blart-sign.
      MOVE   'EQ'                        TO lrw_blart-option.
      MOVE   lv_blart                    TO lrw_blart-low.
      APPEND                                lrw_blart
                                         TO grt_blart.
    ENDIF.
    CLEAR  lwa_zvar.
  ENDLOOP.

*eject
  SORT     git_t003  ASCENDING           BY blart.
  DELETE   ADJACENT DUPLICATES         FROM git_t003
                                  COMPARING blart.

  SORT     grt_blart ASCENDING           BY low.
  DELETE   ADJACENT DUPLICATES         FROM grt_blart
                                  COMPARING low.

ENDFORM.                    " f_set_document_types
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_select_options
*&---------------------------------------------------------------------*
*       Validate the selection screen select-options
*----------------------------------------------------------------------*
FORM f_validate_select_options
  CHANGING cv_flag_err_vldt            TYPE flag.

  DATA:    lv_flag_err_vldt            TYPE flag.

  CLEAR    cv_flag_err_vldt.
  CLEAR    lv_flag_err_vldt.

* Validate the clearing date and clearing document
  IF   ( ( s_augdt[]                     IS INITIAL ) AND
         ( s_augbl[]                     IS INITIAL )     ).
    MESSAGE  i000 WITH text-121.
    CLEAR                                   lv_flag_err_vldt.
    MOVE     gc_x                        TO lv_flag_err_vldt.
  ENDIF.

* Validate the document type
  IF     ( git_t003[]                    IS INITIAL ).
    MESSAGE  i000 WITH text-122.
    CLEAR                                   lv_flag_err_vldt.
    MOVE     gc_x                        TO lv_flag_err_vldt.
  ENDIF.

  IF     ( grt_blart[]                   IS INITIAL ).
    MESSAGE  i000 WITH text-123.
    CLEAR                                   lv_flag_err_vldt.
    MOVE     gc_x                        TO lv_flag_err_vldt.
  ENDIF.

* Validate the data source
  IF   ( ( p_datsrc                      EQ gc_datsrc_us ) OR
         ( p_datsrc                      EQ gc_datsrc_ug ) OR
         ( p_datsrc                      EQ gc_datsrc_sw )    ).
  ELSE.
    MESSAGE  i000 WITH text-124.
    CLEAR                                   lv_flag_err_vldt.
    MOVE     gc_x                        TO lv_flag_err_vldt.
  ENDIF.

* Validate the delimiter
  IF   ( ( p_delim                       EQ gc_pipe_lit  ) OR
         ( p_delim                       EQ gc_comma_lit ) OR
         ( p_delim                       EQ gc_tab_lit   )    ).
  ELSE.
    MESSAGE  i000 WITH text-125.
    CLEAR                                   lv_flag_err_vldt.
    MOVE     gc_x                        TO lv_flag_err_vldt.
  ENDIF.

  CLEAR                                     cv_flag_err_vldt.
  MOVE     lv_flag_err_vldt              TO cv_flag_err_vldt.

ENDFORM.                    " f_validate_select_options
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_extract_main
*&---------------------------------------------------------------------*
*       Process the extract - main routine
*----------------------------------------------------------------------*
FORM f_process_extract_main.

  DATA:    lit_bsak                    TYPE ty_it_bsak,
           lwa_bkpf                    TYPE ty_wa_bkpf,
           lit_bkpf                    TYPE ty_it_bkpf,
           lit_bkpf_p                  TYPE ty_it_bkpf,
           lit_bseg                    TYPE ty_it_bseg.

  DATA:    lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex,
           lv_buk_s1                   TYPE bukrs,
           lv_bel_s1                   TYPE belnr_d,
           lv_gja_s1                   TYPE gjahr.

  CONSTANTS:
           lc_blocksize                TYPE syindex VALUE 200.

  IF     ( gv_flag_err_proc              IS NOT INITIAL ).
    RETURN.
  ENDIF.

* Select the cleared vendor items - critical to select only
* the accounting document numbers BUKRS&BELNR&GJAHR because
* some documents have multiple vendor numbers; e.g. Doc.Type KL
  CLEAR    git_bsak[].
  SELECT   bukrs  belnr  gjahr  hkont  xzahl
    INTO   TABLE git_bsak
    FROM   bsak
   WHERE   bukrs IN s_bukrs
     AND   lifnr IN s_lifnr
     AND   augdt IN s_augdt
     AND   augbl IN s_augbl
     AND   blart IN grt_blart.
  IF     ( sy-subrc NE 0 ).
    CLEAR  git_bsak[].
    MESSAGE  i000 WITH text-201.
    RETURN.
  ENDIF.

* Delete the payment items
  IF       ( cb_xzahl                    IS NOT INITIAL ).
    DELETE   git_bsak                 WHERE xzahl = gc_x.
  ENDIF.

* Delete items posted to a vendor retainage account
  IF     ( ( cb_glrtd                    IS NOT INITIAL ) AND
           ( grt_hkont_ret_v[]           IS NOT INITIAL )     ).
*   DELETE   git_bsak                 WHERE hkont IN grt_hkont_ret_v.
  ENDIF.

*eject
  IF       ( git_bsak[]                  IS INITIAL ).
    MESSAGE  i000 WITH text-201.
    RETURN.
  ENDIF.

  SORT     git_bsak ASCENDING            BY bukrs belnr gjahr.
  DELETE   ADJACENT DUPLICATES         FROM git_bsak
                                  COMPARING bukrs belnr gjahr.

* Process the cleared items in batches
  DO.

* Calculate the low and high indices for the batch of order numbers
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

*
* Build the batch of order numbers
    CLEAR             lit_bsak[].
    APPEND   LINES OF git_bsak
                 FROM lv_index_lo
                   TO lv_index_hi
                   TO lit_bsak.

    IF     ( lit_bsak[] IS INITIAL ).
      EXIT.
    ENDIF.

    CLEAR    lit_bkpf[].
    CLEAR    lit_bseg[].

* Select the accounting document headers
    PERFORM  f_select_doc_headers  TABLES   lit_bsak
                                            lit_bkpf.

*eject
* Select the accounting document items
    CLEAR    lit_bkpf_p[].
    CLEAR    lit_bseg[].
    CLEAR    lv_buk_s1.
    CLEAR    lv_bel_s1.
    CLEAR    lv_gja_s1.

    CLEAR                                   lwa_bkpf.
    LOOP AT  lit_bkpf                  INTO lwa_bkpf.

      IF   ( ( lwa_bkpf-buk_s1           NE lv_buk_s1 ) OR
             ( lwa_bkpf-bel_s1           NE lv_bel_s1 ) OR
             ( lwa_bkpf-gja_s1           NE lv_gja_s1 )    ).

        CLEAR                               lv_buk_s1.
        MOVE   lwa_bkpf-buk_s1           TO lv_buk_s1.
        CLEAR                               lv_bel_s1.
        MOVE   lwa_bkpf-bel_s1           TO lv_bel_s1.
        CLEAR                               lv_gja_s1.
        MOVE   lwa_bkpf-gja_s1           TO lv_gja_s1.

* Select the accounting document items
        PERFORM  f_select_doc_items  TABLES lit_bkpf_p
                                            lit_bseg.

* Process the extract - items routine
        PERFORM  f_process_extract_items
                                     TABLES lit_bkpf_p
                                            lit_bseg.

        CLEAR    lit_bkpf_p[].
        CLEAR    lit_bseg[].

      ENDIF.

      APPEND   lwa_bkpf                  TO lit_bkpf_p.

*eject
      AT LAST.

* Select the accounting document items
        PERFORM  f_select_doc_items  TABLES lit_bkpf_p
                                            lit_bseg.

* Process the extract - items routine
        PERFORM  f_process_extract_items
                                     TABLES lit_bkpf_p
                                            lit_bseg.

        CLEAR    lit_bkpf_p[].
        CLEAR    lit_bseg[].

      ENDAT.

      CLEAR  lwa_bkpf.
    ENDLOOP.

  ENDDO.

ENDFORM.                    " f_process_extract_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_doc_headers
*&---------------------------------------------------------------------*
*       Select the accounting document headers
*----------------------------------------------------------------------*
FORM f_select_doc_headers
  TABLES   iit_bsak                    TYPE ty_it_bsak
           cit_bkpf                    TYPE ty_it_bkpf.

  DATA:    lwa_bkpf                    TYPE ty_wa_bkpf,
           lit_bkpf                    TYPE ty_it_bkpf.

  DATA:    lv_tabix                    TYPE sytabix.

  CLEAR    cit_bkpf[].

  IF     ( iit_bsak[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lit_bkpf[].
  SELECT   bukrs  belnr  gjahr
           blart  bldat  budat  monat  cpudt  usnam
           tcode  bvorg  xblnr  stblg  stjah  bktxt
           waers  awtyp  awkey  hwaer  ausbk  xreversal
    INTO   TABLE lit_bkpf
    FROM   bkpf FOR ALL ENTRIES IN iit_bsak
   WHERE   bukrs  = iit_bsak-bukrs
     AND   belnr  = iit_bsak-belnr
     AND   gjahr  = iit_bsak-gjahr.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lit_bkpf[].
    RETURN.
  ENDIF.

* Select the related documents
  PERFORM  f_select_related_documents
                                   TABLES   lit_bkpf.

* Filter the accounting doc items per the select options
  CLEAR                                     lwa_bkpf.
  LOOP AT  lit_bkpf                    INTO lwa_bkpf.
    lv_tabix = sy-tabix.

    IF     ( ( lwa_bkpf-bvorg            IS NOT INITIAL ) AND
             ( lwa_bkpf-bukrs        NOT IN s_bukri[]   )     ).
      DELETE   lit_bkpf               INDEX lv_tabix.
      CLEAR    lwa_bkpf.
      CONTINUE.
    ENDIF.

*eject
    IF     ( ( lwa_bkpf-belnr            IS NOT INITIAL ) AND
             ( lwa_bkpf-belnr        NOT IN s_belnr[]   )     ).
      DELETE   lit_bkpf               INDEX lv_tabix.
      CLEAR    lwa_bkpf.
      CONTINUE.
    ENDIF.

    IF     ( ( lwa_bkpf-gjahr            IS NOT INITIAL ) AND
             ( p_gjahr                   IS NOT INITIAL ) AND
             ( lwa_bkpf-gjahr            NE p_gjahr     )     ).
      DELETE   lit_bkpf               INDEX lv_tabix.
      CLEAR    lwa_bkpf.
      CONTINUE.
    ENDIF.

    IF     ( ( lwa_bkpf-monat            IS NOT INITIAL ) AND
             ( lwa_bkpf-monat        NOT IN s_monat[]   )     ).
      DELETE   lit_bkpf               INDEX lv_tabix.
      CLEAR    lwa_bkpf.
      CONTINUE.
    ENDIF.

    IF     ( ( lwa_bkpf-bldat            IS NOT INITIAL ) AND
             ( lwa_bkpf-bldat        NOT IN s_bldat[]   )     ).
      DELETE   lit_bkpf               INDEX lv_tabix.
      CLEAR    lwa_bkpf.
      CONTINUE.
    ENDIF.

    IF     ( ( lwa_bkpf-budat            IS NOT INITIAL ) AND
             ( lwa_bkpf-budat        NOT IN s_budat[]   )     ).
      DELETE   lit_bkpf               INDEX lv_tabix.
      CLEAR    lwa_bkpf.
      CONTINUE.
    ENDIF.

    IF     ( ( lwa_bkpf-usnam            IS NOT INITIAL ) AND
             ( lwa_bkpf-usnam        NOT IN s_usnam[]   )     ).
      DELETE   lit_bkpf               INDEX lv_tabix.
      CLEAR    lwa_bkpf.
      CONTINUE.
    ENDIF.

    CLEAR  lwa_bkpf.
  ENDLOOP.

  IF     ( lit_bkpf[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     lit_bkpf ASCENDING            BY buk_s1 bel_s1 gja_s1
                                            bukrs  belnr  gjahr.

  cit_bkpf[] = lit_bkpf[].

ENDFORM.                    " f_select_doc_headers
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_related_documents
*&---------------------------------------------------------------------*
*       Select the related documents
*----------------------------------------------------------------------*
FORM f_select_related_documents
  TABLES   cit_bkpf                    TYPE ty_it_bkpf.

  DATA:    lwa_bkpf                    TYPE ty_wa_bkpf,
           lwa_bkpf_revl               TYPE ty_wa_bkpf, "reversal"
           lit_bkpf_revl               TYPE ty_it_bkpf, "reversal"
           lwa_bkpf_revd               TYPE ty_wa_bkpf, "reversed"
           lit_bkpf                    TYPE ty_it_bkpf,
           lit_bkpf_p1                 TYPE ty_it_bkpf,
           lit_bkpf_p2                 TYPE ty_it_bkpf.

  IF     ( cit_bkpf[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lit_bkpf[].
  CLEAR    lit_bkpf_p1[].
  CLEAR    lit_bkpf_p2[].

* Sort the documents
  CLEAR                                     lwa_bkpf.
  LOOP AT  cit_bkpf                    INTO lwa_bkpf.

* Process a reversal document, if found, append and continue
    IF     ( lwa_bkpf-xreversal          EQ '2' ).

      CLEAR                                 lit_bkpf_revl[].

      PERFORM  f_process_reversal_doc
                                   TABLES   lit_bkpf_revl
                                   USING    lwa_bkpf.

      IF     ( lit_bkpf_revl[]           IS NOT INITIAL ).
        APPEND                     LINES OF lit_bkpf_revl
                                         TO lit_bkpf_p1.
      ENDIF.

      CLEAR  lwa_bkpf.
      CONTINUE.

    ENDIF.

*eject
* If it is a cross company document, then append and continue -
* - any reversed documents will be processed later
    IF     ( lwa_bkpf-bvorg              IS NOT INITIAL ).

      APPEND                                lwa_bkpf
                                         TO lit_bkpf_p2.
      CLEAR  lwa_bkpf.
      CONTINUE.

    ENDIF.

* Process a reversed document
*   IF     ( lwa_bkpf-xreversal          EQ '1' ).
*
*     CLEAR                                 lwa_bkpf_revd.
*     CLEAR                                 lwa_bkpf_revl.
*
*     PERFORM  f_process_reversed_doc USING lwa_bkpf
*                                  CHANGING lwa_bkpf_revd
*                                           lwa_bkpf_revl.
*
*     IF     ( lwa_bkpf_revd-bukrs       IS NOT INITIAL ).
*       APPEND                              lwa_bkpf_revd
*                                        TO lit_bkpf_p1.
*     ENDIF.
*
*     IF     ( lwa_bkpf_revl-bukrs       IS NOT INITIAL ).
*       APPEND                              lwa_bkpf_revl
*                                        TO lit_bkpf_p1.
*     ENDIF.
*
*     CLEAR  lwa_bkpf.
*     CONTINUE.
*
*   ENDIF.

    CLEAR                                   lwa_bkpf-buk_s1.
    MOVE     lwa_bkpf-bukrs              TO lwa_bkpf-buk_s1.
    CLEAR                                   lwa_bkpf-bel_s1.
    MOVE     lwa_bkpf-belnr              TO lwa_bkpf-bel_s1.
    CLEAR                                   lwa_bkpf-gja_s1.
    MOVE     lwa_bkpf-gjahr              TO lwa_bkpf-gja_s1.

    APPEND                                  lwa_bkpf
                                         TO lit_bkpf_p1.

    CLEAR  lwa_bkpf.
  ENDLOOP.

*eject
* Process the cross company documents
  IF     ( lit_bkpf_p2[]                 IS NOT INITIAL ).

    PERFORM  f_process_intercompany_docs
                                   TABLES   lit_bkpf_p2.

  ENDIF.

  CLEAR                                     lit_bkpf[].
  APPEND   LINES OF                         lit_bkpf_p1
                                         TO lit_bkpf.
  APPEND   LINES OF                         lit_bkpf_p2
                                         TO lit_bkpf.

  CLEAR    cit_bkpf[].

  cit_bkpf[] = lit_bkpf[].

ENDFORM.                    " f_select_related_documents
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_intercompany_docs
*&---------------------------------------------------------------------*
*       Process the intercompany documents
*----------------------------------------------------------------------*
FORM f_process_intercompany_docs
  TABLES   cit_bkpf                    TYPE ty_it_bkpf.

  DATA:    lwa_bkpf                    TYPE ty_wa_bkpf,
           lwa_bkpf_revd               TYPE ty_wa_bkpf,
           lwa_bkpf_revl               TYPE ty_wa_bkpf,
           lwa_bkpf_p                  TYPE ty_wa_bkpf,
           lit_bkpf                    TYPE ty_it_bkpf,
           lit_bkpf_p                  TYPE ty_it_bkpf,
           lwa_bvorg                   TYPE ty_wa_bvorg,
           lit_bvorg                   TYPE ty_it_bvorg,
           lit_bvor                    TYPE ty_it_bvor,
           lwa_msg                     TYPE ty_wa_msg.

  FIELD-SYMBOLS <fs1>                  TYPE ty_wa_bkpf.

  IF     ( cit_bkpf[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lit_bkpf[].
  CLEAR    lit_bkpf_p[].
  CLEAR    lit_bvorg[].
  CLEAR    lit_bvor[].

* Create the key to select intercompany document numbers
  LOOP AT  cit_bkpf               ASSIGNING <fs1>.

    IF     ( <fs1>-bvorg                 IS NOT INITIAL ).
      CLEAR                                 lwa_bvorg.
      MOVE   <fs1>-bvorg                 TO lwa_bvorg-bvorg.
      APPEND                                lwa_bvorg
                                         TO lit_bvorg.
    ENDIF.

  ENDLOOP.

  IF     ( lit_bvorg[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     lit_bvorg ASCENDING           BY bvorg.
  DELETE   ADJACENT DUPLICATES         FROM lit_bvorg
                                  COMPARING bvorg.

*eject
* Select the intercompany document numbers
  CLEAR    lit_bvor[].
  SELECT   *
    INTO   TABLE lit_bvor
    FROM   bvor FOR ALL ENTRIES IN lit_bvorg
   WHERE   bvorg = lit_bvorg-bvorg.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SORT     lit_bvor  ASCENDING           BY bukrs belnr gjahr.

* Select the intercompany accounting documents
  CLEAR    lit_bkpf_p[].
  SELECT   bukrs  belnr  gjahr
           blart  bldat  budat  monat  cpudt  usnam
           tcode  bvorg  xblnr  stblg  stjah  bktxt
           waers  awtyp  awkey  hwaer  ausbk  xreversal
    INTO   TABLE lit_bkpf_p
    FROM   bkpf FOR ALL ENTRIES IN lit_bvor
   WHERE   bukrs  = lit_bvor-bukrs
     AND   belnr  = lit_bvor-belnr
     AND   gjahr  = lit_bvor-gjahr.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SORT     lit_bkpf_p  ASCENDING         BY bvorg ausbk bukrs.

*eject
  CLEAR    lwa_bkpf_p.

  CLEAR                                     lwa_bkpf.
  LOOP AT  lit_bkpf_p                  INTO lwa_bkpf.

    IF       ( lwa_bkpf-bvorg            IS INITIAL ).

* Error - InterCompany error; missing IC Doc Key:
      CLEAR                                 lwa_msg.
      MOVE     gc_e                      TO lwa_msg-msgty.
      MOVE     '211'                     TO lwa_msg-msgno.
      MOVE     text-211                  TO lwa_msg-msgv1.
      MOVE     lwa_bkpf-bukrs            TO lwa_msg-msgv2.
      MOVE     lwa_bkpf-belnr            TO lwa_msg-msgv3.
      MOVE     lwa_bkpf-gjahr            TO lwa_msg-msgv4.

      PERFORM  f_error_in_data        USING lwa_msg.

    ELSE.

      IF     ( lwa_bkpf-bvorg            NE lwa_bkpf_p-bvorg ).
        CLEAR                               lwa_bkpf_p.
        MOVE   lwa_bkpf                  TO lwa_bkpf_p.

        IF   ( lwa_bkpf_p-ausbk          IS NOT INITIAL ).

* Error - InterCompany error; missing Source Doc:
          CLEAR                             lwa_msg.
          MOVE     gc_e                  TO lwa_msg-msgty.
          MOVE     '212'                 TO lwa_msg-msgno.
          MOVE     text-212              TO lwa_msg-msgv1.
          MOVE     lwa_bkpf-bvorg        TO lwa_msg-msgv2.
          MOVE     lwa_bkpf-bukrs        TO lwa_msg-msgv3.
          MOVE     lwa_bkpf-belnr        TO lwa_msg-msgv4.

          PERFORM  f_error_in_data    USING lwa_msg.

        ENDIF.

      ENDIF.

*eject
      IF   ( ( lwa_bkpf_p-bvorg          EQ lwa_bkpf-bvorg ) AND
             ( lwa_bkpf_p-ausbk          IS INITIAL        )     ).

*       IF   ( lwa_bkpf-xreversal        EQ '1' ).
*
*         CLEAR                             lwa_bkpf_revd.
*         CLEAR                             lwa_bkpf_revl.
*
*         PERFORM  f_process_reversed_doc
*                                     USING lwa_bkpf
*                                  CHANGING lwa_bkpf_revd
*                                           lwa_bkpf_revl.
*
*         IF     ( lwa_bkpf_revd-bukrs   IS INITIAL ).
*           CLEAR  lwa_bkpf.
*           CONTINUE.
*         ENDIF.
*
*       ENDIF.

        CLEAR                               lwa_bkpf-buk_s1.
        MOVE   lwa_bkpf_p-bukrs          TO lwa_bkpf-buk_s1.
        CLEAR                               lwa_bkpf-bel_s1.
        MOVE   lwa_bkpf_p-belnr          TO lwa_bkpf-bel_s1.
        CLEAR                               lwa_bkpf-gja_s1.
        MOVE   lwa_bkpf_p-gjahr          TO lwa_bkpf-gja_s1.

        APPEND                              lwa_bkpf
                                         TO lit_bkpf.

      ELSE.

* Error - InterCompany error; unprocessed CC Doc: (CrossCompany)
        CLEAR                               lwa_msg.
        MOVE     gc_e                    TO lwa_msg-msgty.
        MOVE     '213'                   TO lwa_msg-msgno.
        MOVE     text-213                TO lwa_msg-msgv1.
        MOVE     lwa_bkpf-bvorg          TO lwa_msg-msgv2.
        MOVE     lwa_bkpf-bukrs          TO lwa_msg-msgv3.
        MOVE     lwa_bkpf-belnr          TO lwa_msg-msgv4.

        PERFORM  f_error_in_data      USING lwa_msg.

      ENDIF.

    ENDIF.

    CLEAR  lwa_bkpf.
  ENDLOOP.

  CLEAR    cit_bkpf[].

  cit_bkpf[] = lit_bkpf[].

ENDFORM.                    " f_process_intercompany_docs
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_reversal_doc
*&---------------------------------------------------------------------*
*       Process the reversal documents
*----------------------------------------------------------------------*
FORM f_process_reversal_doc
  TABLES   cit_bkpf_revl               TYPE ty_it_bkpf
  USING    iwa_bkpf                    TYPE ty_wa_bkpf.

  DATA:    lwa_bkpf_revd               TYPE ty_wa_bkpf, "reversed"
           lit_bkpf_revd               TYPE ty_it_bkpf, "reversed"
           lwa_bkpf_revl               TYPE ty_wa_bkpf, "reversal"
           lit_bkpf_revl               TYPE ty_it_bkpf, "reversal"
           lit_bkpf_revl_p1            TYPE ty_it_bkpf, "reversal"
           lit_bkpf_revl_p2            TYPE ty_it_bkpf, "reversal"
           lit_bvor                    TYPE ty_it_bvor,
           lwa_doc_prcsd               TYPE ty_wa_doc_prcsd.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix.

  CLEAR    cit_bkpf_revl[].

  CLEAR    lit_bkpf_revl_p1[].
  CLEAR    lit_bkpf_revl_p2[].

  IF     ( iwa_bkpf-bukrs                IS INITIAL ).
    RETURN.
  ENDIF.

* Check if the reversal document has already been processed
  CLEAR                                     lwa_doc_prcsd.
  READ     TABLE git_doc_prcsd         INTO lwa_doc_prcsd
                                   WITH KEY bukrs = iwa_bkpf-bukrs
                                            belnr = iwa_bkpf-belnr
                                            gjahr = iwa_bkpf-gjahr
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF     ( lv_subrc EQ 0 ).
    RETURN.
  ENDIF.

*eject
* Select the reversed document
  CLEAR    lwa_bkpf_revd.
  SELECT   SINGLE bukrs  belnr  gjahr
           blart  bldat  budat  monat  cpudt  usnam
           tcode  bvorg  xblnr  stblg  stjah  bktxt
           waers  awtyp  awkey  hwaer  ausbk  xreversal
    INTO   lwa_bkpf_revd
    FROM   bkpf
   WHERE   bukrs = iwa_bkpf-bukrs
     AND   belnr = iwa_bkpf-stblg
     AND   gjahr = iwa_bkpf-stjah.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  IF     ( lwa_bkpf_revd-bvorg           IS INITIAL ).
    CLEAR                                   lwa_bkpf_revl.
    MOVE   iwa_bkpf                      TO lwa_bkpf_revl.
    MOVE   iwa_bkpf-bukrs                TO lwa_bkpf_revl-buk_s1.
    MOVE   iwa_bkpf-belnr                TO lwa_bkpf_revl-bel_s1.
    MOVE   iwa_bkpf-gjahr                TO lwa_bkpf_revl-gja_s1.
    APPEND                                  lwa_bkpf_revl
                                         TO lit_bkpf_revl_p1.
  ELSE.

* Select the reversed document numbers using the intercompany document
    CLEAR    lit_bvor[].
    SELECT   *
      INTO   TABLE lit_bvor
      FROM   bvor
     WHERE   bvorg = lwa_bkpf_revd-bvorg.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    SORT     lit_bvor  ASCENDING         BY bukrs belnr gjahr.

* Select the reversed documents
    CLEAR    lit_bkpf_revd[].
    SELECT   bukrs  belnr  gjahr
             blart  bldat  budat  monat  cpudt  usnam
             tcode  bvorg  xblnr  stblg  stjah  bktxt
             waers  awtyp  awkey  hwaer  ausbk  xreversal
      INTO   TABLE lit_bkpf_revd
      FROM   bkpf  FOR ALL ENTRIES IN lit_bvor
     WHERE   bukrs = lit_bvor-bukrs
       AND   belnr = lit_bvor-belnr
       AND   gjahr = lit_bvor-gjahr.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

*eject
* Select the reversal documents
    CLEAR    lit_bkpf_revl[].
    SELECT   bukrs  belnr  gjahr
             blart  bldat  budat  monat  cpudt  usnam
             tcode  bvorg  xblnr  stblg  stjah  bktxt
             waers  awtyp  awkey  hwaer  ausbk  xreversal
      INTO   TABLE lit_bkpf_revl
      FROM   bkpf  FOR ALL ENTRIES IN lit_bkpf_revd
     WHERE   bukrs = lit_bkpf_revd-bukrs
       AND   belnr = lit_bkpf_revd-stblg
       AND   gjahr = lit_bkpf_revd-stjah.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    CLEAR                                   lwa_bkpf_revl.
    LOOP AT  lit_bkpf_revl             INTO lwa_bkpf_revl.
      MOVE   iwa_bkpf-bukrs              TO lwa_bkpf_revl-buk_s1.
      MOVE   iwa_bkpf-belnr              TO lwa_bkpf_revl-bel_s1.
      MOVE   iwa_bkpf-gjahr              TO lwa_bkpf_revl-gja_s1.
      APPEND                                lwa_bkpf_revl
                                         TO lit_bkpf_revl_p1.
      CLEAR  lwa_bkpf_revl.
    ENDLOOP.

  ENDIF.

* Insert the reversal doc numbers into the internal table docs processed
  CLEAR                                     lwa_bkpf_revl.
  LOOP AT  lit_bkpf_revl_p1            INTO lwa_bkpf_revl.

    CLEAR                                   lwa_doc_prcsd.
    READ     TABLE git_doc_prcsd       INTO lwa_doc_prcsd
                                   WITH KEY bukrs = lwa_bkpf_revl-bukrs
                                            belnr = lwa_bkpf_revl-belnr
                                            gjahr = lwa_bkpf_revl-gjahr
                              BINARY SEARCH.
    lv_subrc = sy-subrc.
    lv_tabix = sy-tabix.
    IF     ( lv_subrc NE 0 ).
      CLEAR                                 lwa_doc_prcsd.
      MOVE     lwa_bkpf_revl-bukrs       TO lwa_doc_prcsd-bukrs.
      MOVE     lwa_bkpf_revl-belnr       TO lwa_doc_prcsd-belnr.
      MOVE     lwa_bkpf_revl-gjahr       TO lwa_doc_prcsd-gjahr.
      INSERT                                lwa_doc_prcsd
                                       INTO git_doc_prcsd
                                      INDEX lv_tabix.
      APPEND   lwa_bkpf_revl             TO lit_bkpf_revl_p2.
    ENDIF.

    CLEAR  lwa_bkpf_revl.
  ENDLOOP.

  cit_bkpf_revl[] = lit_bkpf_revl_p2[].

ENDFORM.                    " f_process_reversal_doc
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_reversed_doc
*&---------------------------------------------------------------------*
*       Process the reversed documents, returns the reversal documents
*----------------------------------------------------------------------*
FORM f_process_reversed_doc
  USING    iwa_bkpf                    TYPE ty_wa_bkpf
  CHANGING cwa_bkpf_revd               TYPE ty_wa_bkpf
           cwa_bkpf_revl               TYPE ty_wa_bkpf.

  DATA:    lwa_bkpf                    TYPE ty_wa_bkpf,
           lwa_doc_prcsd               TYPE ty_wa_doc_prcsd.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix.

  CLEAR    cwa_bkpf_revd.
  CLEAR    cwa_bkpf_revl.

  IF     ( iwa_bkpf-bukrs                IS INITIAL ).
    RETURN.
  ENDIF.

* Check if the reversed document has already been processed
  CLEAR                                     lwa_doc_prcsd.
  READ     TABLE git_doc_prcsd         INTO lwa_doc_prcsd
                                   WITH KEY bukrs = iwa_bkpf-bukrs
                                            belnr = iwa_bkpf-belnr
                                            gjahr = iwa_bkpf-gjahr
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF     ( sy-subrc NE 0 ).

    CLEAR                                   lwa_bkpf.
    MOVE     iwa_bkpf                    TO lwa_bkpf.

    CLEAR                                   lwa_doc_prcsd.
    MOVE     lwa_bkpf-bukrs              TO lwa_doc_prcsd-bukrs.
    MOVE     lwa_bkpf-belnr              TO lwa_doc_prcsd-belnr.
    MOVE     lwa_bkpf-gjahr              TO lwa_doc_prcsd-gjahr.
    INSERT                                  lwa_doc_prcsd
                                       INTO git_doc_prcsd
                                      INDEX lv_tabix.

    CLEAR                                   lwa_bkpf-buk_s1.
    MOVE     lwa_bkpf-bukrs              TO lwa_bkpf-buk_s1.
    CLEAR                                   lwa_bkpf-bel_s1.
    MOVE     lwa_bkpf-belnr              TO lwa_bkpf-bel_s1.
    CLEAR                                   lwa_bkpf-gja_s1.
    MOVE     lwa_bkpf-gjahr              TO lwa_bkpf-gja_s1.

    CLEAR                                   cwa_bkpf_revd.
    MOVE     lwa_bkpf                    TO cwa_bkpf_revd.

  ENDIF.

*eject
* Check if the reversal document is to be processed
  READ     TABLE git_bsak          WITH KEY bukrs = iwa_bkpf-bukrs
                                            belnr = iwa_bkpf-stblg
                                            gjahr = iwa_bkpf-stjah
                              BINARY SEARCH
                     TRANSPORTING NO FIELDS.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

* Check if the reversal document has already been processed
  CLEAR                                     lwa_doc_prcsd.
  READ     TABLE git_doc_prcsd         INTO lwa_doc_prcsd
                                   WITH KEY bukrs = iwa_bkpf-bukrs
                                            belnr = iwa_bkpf-stblg
                                            gjahr = iwa_bkpf-stjah
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF     ( sy-subrc NE 0 ).

    CLEAR    lwa_bkpf.
    SELECT   SINGLE bukrs  belnr  gjahr
             blart  bldat  budat  monat  cpudt  usnam
             tcode  bvorg  xblnr  stblg  stjah  bktxt
             waers  awtyp  awkey  hwaer  ausbk  xreversal
      INTO   lwa_bkpf
      FROM   bkpf
     WHERE   bukrs = iwa_bkpf-bukrs
       AND   belnr = iwa_bkpf-stblg
       AND   gjahr = iwa_bkpf-stjah.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lwa_bkpf.
      RETURN.
    ENDIF.

    CLEAR                                   lwa_doc_prcsd.
    MOVE     lwa_bkpf-bukrs              TO lwa_doc_prcsd-bukrs.
    MOVE     lwa_bkpf-belnr              TO lwa_doc_prcsd-belnr.
    MOVE     lwa_bkpf-gjahr              TO lwa_doc_prcsd-gjahr.
    INSERT                                  lwa_doc_prcsd
                                       INTO git_doc_prcsd
                                      INDEX lv_tabix.

    CLEAR                                   lwa_bkpf-buk_s1.
    MOVE     lwa_bkpf-bukrs              TO lwa_bkpf-buk_s1.
    CLEAR                                   lwa_bkpf-bel_s1.
    MOVE     lwa_bkpf-belnr              TO lwa_bkpf-bel_s1.
    CLEAR                                   lwa_bkpf-gja_s1.
    MOVE     lwa_bkpf-gjahr              TO lwa_bkpf-gja_s1.

    CLEAR                                   cwa_bkpf_revl.
    MOVE     lwa_bkpf                    TO cwa_bkpf_revl.

  ENDIF.

ENDFORM.                    " f_process_reversed_doc
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_doc_items
*&---------------------------------------------------------------------*
*       Select the accounting document items
*----------------------------------------------------------------------*
FORM f_select_doc_items
  TABLES   iit_bkpf                    TYPE ty_it_bkpf
           cit_bseg                    TYPE ty_it_bseg.

  DATA:    lit_bseg                    TYPE ty_it_bseg.

  DATA:    lv_tabix                    TYPE sytabix.

  FIELD-SYMBOLS:
           <fs_bseg>                   TYPE ty_wa_bseg.

  CLEAR    cit_bseg[].

  IF     ( iit_bkpf[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lit_bseg[].
  SELECT   bukrs  belnr  gjahr  buzei  augdt  augbl
           bschl  koart  shkzg  mwskz  dmbtr  wrbtr
           zuonr  sgtxt  vbund  kostl  aufnr  saknr
           hkont  lifnr  zfbdt  zterm  zbd1t  zbd2t
           zbd3t  sknto  wskto  zlsch  rebzg  menge
           ebeln  ebelp  prctr  nplnr  aufpl  aplzl
           projk  uzawe  xref1  xref2  xref3  auggj
    INTO   TABLE lit_bseg
    FROM   bseg FOR ALL ENTRIES IN iit_bkpf
   WHERE   bukrs  = iit_bkpf-bukrs
     AND   belnr  = iit_bkpf-belnr
     AND   gjahr  = iit_bkpf-gjahr.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lit_bseg[].
    RETURN.
  ENDIF.

*eject
* Filter the accounting doc items per the select options
  LOOP AT  lit_bseg               ASSIGNING <fs_bseg>.
    lv_tabix = sy-tabix.

    IF       ( <fs_bseg>-koart           EQ 'K'         ).
      CONTINUE.
    ENDIF.

    IF     ( ( <fs_bseg>-prctr           IS NOT INITIAL ) AND
             ( <fs_bseg>-prctr       NOT IN s_prctr[]   )     ).
      DELETE   lit_bseg               INDEX lv_tabix.
      CONTINUE.
    ENDIF.

    IF     ( ( <fs_bseg>-kostl           IS NOT INITIAL ) AND
             ( <fs_bseg>-kostl       NOT IN s_kostl[]   )     ).
      DELETE   lit_bseg               INDEX lv_tabix.
      CONTINUE.
    ENDIF.

    IF     ( ( <fs_bseg>-hkont           IS NOT INITIAL ) AND
             ( <fs_bseg>-hkont       NOT IN s_hkont[]   )     ).
      DELETE   lit_bseg               INDEX lv_tabix.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  IF     ( lit_bseg[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     lit_bseg ASCENDING            BY bukrs belnr gjahr buzei.
  DELETE   ADJACENT DUPLICATES         FROM lit_bseg
                                  COMPARING bukrs belnr gjahr buzei.

  cit_bseg[] = lit_bseg[].

ENDFORM.                    " f_select_doc_items
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_extract_items
*&---------------------------------------------------------------------*
*       Process the extract - items routine
*----------------------------------------------------------------------*
FORM f_process_extract_items
  TABLES   iit_bkpf                    TYPE ty_it_bkpf
           iit_bseg                    TYPE ty_it_bseg.

  DATA:    lwa_bkpf                    TYPE ty_wa_bkpf,
           lit_bseg_a                  TYPE ty_it_bseg,
           lit_bseg_v                  TYPE ty_it_bseg,
           lit_int_item                TYPE ty_it_int_item,
           lwa_vendor                  TYPE ty_wa_ven_amt,
           lit_vendor                  TYPE ty_it_ven_amt,
           lwa_zvar                    TYPE ty_wa_zvar.

  DATA:    lv_lines                    TYPE syindex,
           lv_voc_ind                  TYPE char1,
           lv_ret_v                    TYPE flag,
           lv_ret_a                    TYPE flag,
           lv_ret_cr                   TYPE flag,
           lv_ic_je                    TYPE flag,
           lv_ebeln                    TYPE ebeln.

  FIELD-SYMBOLS:
           <fs_bkpf>                   TYPE ty_wa_bkpf,
           <fs_bseg>                   TYPE ty_wa_bseg.

  IF   ( ( iit_bkpf[]               IS INITIAL ) OR
         ( iit_bseg[]               IS INITIAL )    ).
    RETURN.
  ENDIF.

  CLEAR    lwa_bkpf.
  CLEAR    lit_bseg_a[].
  CLEAR    lit_bseg_v[].
  CLEAR    lit_int_item[].
  CLEAR    lit_vendor[].

  CLEAR    lv_lines.
  CLEAR    lv_voc_ind.
  CLEAR    lv_ret_v.
  CLEAR    lv_ret_a.
  CLEAR    lv_ret_cr.
  CLEAR    lv_ic_je.
  CLEAR    lv_ebeln.

*eject
* Select the source document
  LOOP AT    iit_bkpf             ASSIGNING <fs_bkpf>.
    IF   ( ( <fs_bkpf>-bukrs             EQ <fs_bkpf>-buk_s1 ) AND
           ( <fs_bkpf>-belnr             EQ <fs_bkpf>-bel_s1 ) AND
           ( <fs_bkpf>-gjahr             EQ <fs_bkpf>-gja_s1 )     ).
      MOVE   <fs_bkpf>                   TO lwa_bkpf.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF       ( lwa_bkpf-bukrs              IS INITIAL ).
    CLEAR                                   lwa_bkpf.
    READ     TABLE iit_bkpf            INTO lwa_bkpf INDEX 1.
  ENDIF.

* Segregate the vendor items and the accounting items
  LOOP AT  iit_bseg               ASSIGNING <fs_bseg>.

    IF       ( <fs_bseg>-koart           EQ 'K' ).

      APPEND   <fs_bseg>                 TO lit_bseg_v.

      IF     ( <fs_bseg>-hkont           IN grt_hkont_ret_v[] ).
        MOVE   abap_true                 TO lv_ret_v.
      ENDIF.

      CLEAR                                 lwa_vendor.
      MOVE     <fs_bseg>-lifnr           TO lwa_vendor-lifnr.
      COLLECT                               lwa_vendor
                                       INTO lit_vendor.

*eject
    ELSE.   "( <fs_bseg>-koart           NE 'K' ).

      APPEND   <fs_bseg>                 TO lit_bseg_a.

      IF     ( <fs_bseg>-hkont           IN grt_hkont_ret_a[] ).
        MOVE   abap_true                 TO lv_ret_a.

        READ   TABLE git_tbsl      WITH KEY bschl = <fs_bseg>-bschl
                                            shkzg = 'H'
                     TRANSPORTING NO FIELDS.
        IF     ( sy-subrc EQ 0 ).
          MOVE   abap_true               TO lv_ret_cr.
        ENDIF.

      ENDIF.

      IF   ( ( lwa_bkpf-bvorg            IS INITIAL  ) AND
             ( <fs_bseg>-hkont           IN s_glic[] )     ).
        MOVE   abap_true                 TO lv_ic_je.
      ENDIF.

      IF   ( ( lwa_bkpf-awtyp            EQ 'RMRP'      ) AND
             ( <fs_bseg>-ebeln           IS NOT INITIAL ) AND
             ( lv_ebeln                  IS INITIAL     )     ).
        MOVE   <fs_bseg>-ebeln           TO lv_ebeln.
      ENDIF.

    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE lit_vendor           LINES lv_lines.

  CLEAR                                     lv_voc_ind.
  IF     ( lv_lines                      EQ 0 ).
    MOVE   '0'                           TO lv_voc_ind.
  ELSEIF ( lv_lines                      EQ 1 ).
    MOVE   '1'                           TO lv_voc_ind.
  ELSE.
    MOVE   '3'                           TO lv_voc_ind.
  ENDIF.

  CLEAR                                     lwa_zvar.
  READ   TABLE git_zvar                INTO lwa_zvar
                                   WITH KEY varname = gc_doc_type_voc3
                                            value1  = lwa_bkpf-blart.
  IF     ( sy-subrc EQ 0 ).
    CLEAR                                   lv_voc_ind.
    MOVE   '2'                           TO lv_voc_ind.
  ENDIF.

*eject
* Check the retainage accounts
  IF     ( ( cb_glrtd                    IS NOT INITIAL ) AND
           ( lv_ret_a                    IS NOT INITIAL ) AND
           ( lv_ret_cr                   IS NOT INITIAL )     ).
    CLEAR                                   lv_voc_ind.
    MOVE     '2'                         TO lv_voc_ind.
  ENDIF.

* Check if the document is an intercompany journal entry
  IF       ( lv_ic_je                    IS NOT INITIAL ).
    CLEAR                                   lv_voc_ind.
    MOVE     '2'                         TO lv_voc_ind.
  ENDIF.

* Delete vendor items which are not cleared
  IF       ( s_augdt[]                   IS INITIAL ).
    DELETE   lit_bseg_v               WHERE augdt IS INITIAL.
  ELSE.
    DELETE   lit_bseg_v               WHERE augdt IS INITIAL
                                         OR augdt NOT IN s_augdt.
  ENDIF.

* Reassign values when the reference transaction type is an MM invoice
* IF           ( lwa_bkpf-awtyp          EQ 'RMRP'      ).   "D30K928520
*   IF         ( lv_voc_ind              EQ '1'         ).   "D30K928520
*     LOOP AT    lit_bseg_a       ASSIGNING <fs_bseg>.       "D30K928520
*       IF   ( ( <fs_bseg>-ebeln         IS INITIAL     ) AND    "928520
*              ( lv_ebeln                IS NOT INITIAL )     ). "928520
*         MOVE   lv_ebeln                TO <fs_bseg>-ebeln. "D30K928520
*       ENDIF.                                               "D30K928520
*     ENDLOOP.                                               "D30K928520
*   ENDIF.                                                   "D30K928520
*   IF         ( lv_voc_ind              EQ '2'         ).   "D30K928520
*     LOOP AT    lit_bseg_v       ASSIGNING <fs_bseg>.       "D30K928520
*       IF   ( ( <fs_bseg>-ebeln         IS INITIAL     ) AND    "928520
*              ( lv_ebeln                IS NOT INITIAL )     ). "928520
*         MOVE   lv_ebeln                TO <fs_bseg>-ebeln. "D30K928520
*       ENDIF.                                               "D30K928520
*     ENDLOOP.                                               "D30K928520
*   ENDIF.                                                   "D30K928520
* ENDIF.                                                     "D30K928520

*eject
  IF     ( ( lit_bseg_v[]                IS INITIAL ) OR
           ( lit_bseg_a[]                IS INITIAL )    ).
    RETURN.
  ENDIF.

* Process the vendor items
  PERFORM  f_process_vendor_items  TABLES   iit_bkpf
                                            lit_bseg_v
                                            lit_int_item
                                   USING    lwa_bkpf
                                            lv_voc_ind.

* Process the accounting items
  PERFORM  f_process_accntg_items  TABLES   iit_bkpf
                                            lit_bseg_a
                                            lit_int_item
                                   USING    lwa_bkpf
                                            lv_voc_ind.

* Format the output
  PERFORM  f_format_output         TABLES   lit_int_item.

ENDFORM.                    " f_process_extract_items
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_vendor_items
*&---------------------------------------------------------------------*
*       Process the vendor items
*----------------------------------------------------------------------*
FORM f_process_vendor_items
  TABLES   iit_bkpf                    TYPE ty_it_bkpf
           iit_bseg                    TYPE ty_it_bseg
           cit_int_item                TYPE ty_it_int_item
  USING    iwa_bkpf_s                  TYPE ty_wa_bkpf
           iv_voc_ind                  TYPE char1.

  DATA:    lwa_bkpf                    TYPE ty_wa_bkpf,
           lwa_bseg                    TYPE ty_wa_bseg,
           lwa_int_item                TYPE ty_wa_int_item,
           lit_int_item                TYPE ty_it_int_item,
           lwa_ven_amt                 TYPE ty_wa_ven_amt,
           lit_ven_amt                 TYPE ty_it_ven_amt,
           lwa_msg                     TYPE ty_wa_msg.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_dmbtr                    TYPE dmbtr,
           lv_dmbtr_total              TYPE dmbtr,
           lv_sknto                    TYPE sknto,
           lv_sknto_total              TYPE sknto,
           lv_paymeth                  TYPE char8,
           lv_chect                    TYPE chect,
           lv_netdt                    TYPE faedt_fpos,
           lv_approver                 TYPE char12.

  CLEAR    cit_int_item[].

  IF   ( ( iit_bkpf[]               IS INITIAL ) OR
         ( iit_bseg[]               IS INITIAL )    ).
    RETURN.
  ENDIF.

  CLEAR    lwa_bkpf.
  CLEAR    lwa_bseg.
  CLEAR    lwa_int_item.
  CLEAR    lit_int_item[].
  CLEAR    lwa_ven_amt.
  CLEAR    lit_ven_amt[].
  CLEAR    lwa_msg.

  CLEAR    lv_dmbtr_total.

*eject
* Process the vendor items - calculate the document amount total
  IF     ( iv_voc_ind                    EQ '0' ).

* Error - Vendor not found; Accounting Doc:
    CLEAR                                   lwa_msg.
    MOVE     gc_e                        TO lwa_msg-msgty.
    MOVE     '221'                       TO lwa_msg-msgno.
    MOVE     text-221                    TO lwa_msg-msgv1.
    MOVE     iwa_bkpf_s-bukrs            TO lwa_msg-msgv2.
    MOVE     iwa_bkpf_s-belnr            TO lwa_msg-msgv3.
    MOVE     iwa_bkpf_s-gjahr            TO lwa_msg-msgv4.

    PERFORM  f_error_in_data       USING    lwa_msg.

    RETURN.

  ELSEIF ( iv_voc_ind                    EQ '3' ).

* Msg. - multiple Vendors found; Accounting Doc:
    CLEAR                                   lwa_msg.
    MOVE     gc_i                        TO lwa_msg-msgty.
    MOVE     '222'                       TO lwa_msg-msgno.
    MOVE     text-222                    TO lwa_msg-msgv1.
    MOVE     iwa_bkpf_s-bukrs            TO lwa_msg-msgv2.
    MOVE     iwa_bkpf_s-belnr            TO lwa_msg-msgv3.
    MOVE     iwa_bkpf_s-gjahr            TO lwa_msg-msgv4.

    PERFORM  f_error_in_data       USING    lwa_msg.

  ENDIF.

* Build the vendor items
  CLEAR                                     lwa_bseg.
  LOOP AT  iit_bseg                    INTO lwa_bseg.

* Read the accounting document header
    IF   ( ( lwa_bkpf-bukrs              NE lwa_bseg-bukrs ) OR
           ( lwa_bkpf-belnr              NE lwa_bseg-belnr ) OR
           ( lwa_bkpf-gjahr              NE lwa_bseg-gjahr )    ).
      CLEAR                                 lwa_bkpf.
      READ     TABLE iit_bkpf          INTO lwa_bkpf
                                   WITH KEY bukrs = lwa_bseg-bukrs
                                            belnr = lwa_bseg-belnr
                                            gjahr = lwa_bseg-gjahr.
      IF     ( sy-subrc NE 0 ).
        CLEAR    lwa_bkpf.
      ENDIF.
    ENDIF.

*eject
* Get the company data
    IF     ( gwa_t001-bukrs              EQ lwa_bseg-bukrs ).
    ELSE.

      CLEAR                                 gwa_t001.
      PERFORM  f_get_company_data  USING    lwa_bseg-bukrs
                                   CHANGING gwa_t001.

    ENDIF.

* Get the posting key data
    IF     ( gwa_tbsl-bschl              EQ lwa_bseg-bschl ).
    ELSE.

      CLEAR                                 gwa_tbsl.
      PERFORM  f_get_pstkey_data   USING    lwa_bseg-bschl
                                   CHANGING gwa_tbsl.

    ENDIF.

* Get the vendor data
    IF   ( ( gwa_vendor-lifnr            EQ lwa_bseg-lifnr   ) AND
           ( gwa_vendor-bukrs            EQ lwa_bseg-bukrs   )     ).
    ELSE.

      CLEAR                                 gwa_vendor.
      PERFORM  f_get_vendor_data   USING    lwa_bseg-lifnr
                                            lwa_bseg-bukrs
                                   CHANGING gwa_vendor.

    ENDIF.

* Get the purchase order data
    IF     ( lwa_bseg-ebeln              IS INITIAL           ).
      CLEAR                                 gwa_po_data.
    ELSEIF ( lwa_bseg-ebeln              EQ gwa_po_data-ebeln ).
    ELSE.

      CLEAR                                 gwa_po_data.
      PERFORM  f_get_po_data       USING    lwa_bseg-ebeln
                                            00000
                                   CHANGING gwa_po_data.

    ENDIF.

* Set the payment method
    CLEAR    lv_paymeth.
    CLEAR    lv_chect.

    PERFORM  f_set_payment_method  USING    lwa_bkpf
                                            lwa_bseg
                                   CHANGING lv_paymeth
                                            lv_chect.

*eject
* Get the net due date
    CLEAR    lv_netdt.

    PERFORM  f_get_net_due_date    USING    lwa_bseg
                                   CHANGING lv_netdt.

* Get the approver
    CLEAR    lv_approver.

    PERFORM  f_get_approver        USING    lwa_bkpf
                                   CHANGING lv_approver.

* Set the amount in local currency
    CLEAR                                   lv_dmbtr.
    PERFORM  f_determine_amount    USING    lwa_bkpf-budat
                                            iwa_bkpf_s-hwaer
                                            lwa_bkpf-waers
                                            lwa_bkpf-hwaer
                                            lwa_bseg-wrbtr
                                            lwa_bseg-dmbtr
                                   CHANGING lv_dmbtr.

    CLEAR                                   lv_sknto.
    PERFORM  f_determine_amount    USING    lwa_bkpf-budat
                                            iwa_bkpf_s-hwaer
                                            lwa_bkpf-waers
                                            lwa_bkpf-hwaer
                                            lwa_bseg-wskto
                                            lwa_bseg-sknto
                                   CHANGING lv_sknto.

    IF     ( gwa_tbsl-shkzg              EQ 'S' ).
      MULTIPLY                              lv_dmbtr BY -1.
      MULTIPLY                              lv_sknto BY -1.
    ENDIF.
    ADD      lv_dmbtr                    TO lv_dmbtr_total.
    ADD      lv_sknto                    TO lv_sknto_total.

* Collect (total) the amount in local currency per vendor
    CLEAR                                   lwa_ven_amt.
    MOVE     lwa_bseg-lifnr              TO lwa_ven_amt-lifnr.
    MOVE     lv_dmbtr                    TO lwa_ven_amt-dmbtr.
    COLLECT                                 lwa_ven_amt
                                       INTO lit_ven_amt.
    "BOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
    SELECT SINGLE value1
      INTO lv_fg
      FROM zfit_xparam
      WHERE paramtype = lc_param AND
            subtype = lc_doc AND
            key1 = lc_1 AND
            value1 = lwa_bkpf-blart.
    IF sy-subrc = 0.
      CLEAR lwa_bseg-sgtxt.
    ENDIF.
    "EOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
*eject
* Create the lines for vendor items
    CLEAR                                   lwa_int_item.
    MOVE     lwa_bseg-bukrs              TO lwa_int_item-bukrs.
    MOVE     lwa_bseg-belnr              TO lwa_int_item-belnr.
    MOVE     lwa_bseg-gjahr              TO lwa_int_item-gjahr.
    MOVE     lwa_bseg-buzei              TO lwa_int_item-buzei.
    MOVE     lwa_bseg-koart              TO lwa_int_item-koart.
    MOVE     lwa_bseg-lifnr              TO lwa_int_item-lifnr.
    MOVE     gwa_vendor-ktokk            TO lwa_int_item-ktokk.
    MOVE     gwa_vendor-name1            TO lwa_int_item-name1.
    MOVE     gwa_vendor-land1            TO lwa_int_item-land1.
    MOVE     gwa_vendor-stras            TO lwa_int_item-stras.
    MOVE     gwa_vendor-ort01            TO lwa_int_item-ort01.
    MOVE     gwa_vendor-regio            TO lwa_int_item-regio.
    MOVE     gwa_vendor-pstlz            TO lwa_int_item-pstlz.
    MOVE     gwa_vendor-mindk            TO lwa_int_item-mindk.
    MOVE     lwa_bkpf-budat              TO lwa_int_item-budat.
    MOVE     lv_dmbtr                    TO lwa_int_item-dmbtr_total.
    MOVE     lwa_bseg-augdt              TO lwa_int_item-augdt.
    MOVE     lv_paymeth                  TO lwa_int_item-paymeth.
    MOVE     lwa_bkpf-xblnr              TO lwa_int_item-xblnr.
    MOVE     lwa_bkpf-bldat              TO lwa_int_item-bldat.
    MOVE     lwa_bseg-saknr              TO lwa_int_item-saknr.
    MOVE     lwa_bseg-hkont              TO lwa_int_item-hkont.
    MOVE     lwa_bseg-sgtxt              TO lwa_int_item-sgtxt.
    MOVE     lv_dmbtr                    TO lwa_int_item-dmbtr.
    MOVE     lwa_bseg-mwskz              TO lwa_int_item-taxtext.
    MOVE     lwa_bseg-ebeln              TO lwa_int_item-pur_ord.
    MOVE     lwa_bseg-zterm              TO lwa_int_item-zterm.
    MOVE     lv_chect                    TO lwa_int_item-chect.
    MOVE     lwa_bkpf-hwaer              TO lwa_int_item-waers.
    MOVE     p_datsrc                    TO lwa_int_item-datasrce.
    MOVE     lwa_bseg-bschl              TO lwa_int_item-ltext.
    MOVE     lwa_bkpf-bktxt              TO lwa_int_item-bktxt.
    MOVE     lwa_bkpf-blart              TO lwa_int_item-blart.
    MOVE     lwa_bseg-uzawe              TO lwa_int_item-uzawe.
    MOVE     gwa_vendor-erdat            TO lwa_int_item-erdat.
    MOVE     lv_netdt                    TO lwa_int_item-netdt.
    MOVE     lv_sknto                    TO lwa_int_item-sknto_total.
    MOVE     lv_approver                 TO lwa_int_item-approver.
    MOVE     lwa_bkpf-bvorg              TO lwa_int_item-bvorg.
    MOVE     lwa_bkpf-usnam              TO lwa_int_item-usnam.
    MOVE     gwa_po_data-bstyp           TO lwa_int_item-bstyp.
    MOVE     gwa_po_data-bsart           TO lwa_int_item-bsart.
    MOVE     iwa_bkpf_s-buk_s1           TO lwa_int_item-buk_s1.
    MOVE     iwa_bkpf_s-bel_s1           TO lwa_int_item-bel_s1.
    MOVE     iwa_bkpf_s-gja_s1           TO lwa_int_item-gja_s1.
    MOVE     iv_voc_ind                  TO lwa_int_item-voc_ind.

    IF lv_flag IS INITIAL. "Added by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
      APPEND                                  lwa_int_item
                                           TO lit_int_item.
    ENDIF. "Added by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
    CLEAR  lwa_bseg.
  ENDLOOP.

*eject
  IF     ( lines( iit_bseg )             EQ 1 ).
    CLEAR            cit_int_item[].
    cit_int_item[] = lit_int_item[].
    RETURN.
  ENDIF.

* Reassign the total amount if there is more than one vendor item
  CLEAR                                     lwa_int_item.
  LOOP AT  lit_int_item                INTO lwa_int_item.
    lv_tabix = sy-tabix.

    CLEAR                                   lwa_int_item-dmbtr_total.
    MOVE     lv_dmbtr_total              TO lwa_int_item-dmbtr_total.
    CLEAR                                   lwa_int_item-sknto_total.
    MOVE     lv_sknto_total              TO lwa_int_item-sknto_total.

    MODIFY                                  lit_int_item
                                       FROM lwa_int_item
                                      INDEX lv_tabix
                               TRANSPORTING dmbtr_total sknto_total.

    CLEAR  lwa_int_item.
  ENDLOOP.

  CLEAR            cit_int_item[].
  cit_int_item[] = lit_int_item[].

ENDFORM.                    " f_process_vendor_items
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_accntg_items
*&---------------------------------------------------------------------*
*       Process the accounting items
*----------------------------------------------------------------------*
FORM f_process_accntg_items
  TABLES   iit_bkpf                    TYPE ty_it_bkpf
           iit_bseg                    TYPE ty_it_bseg
           cit_int_item                TYPE ty_it_int_item
  USING    iwa_bkpf_s                  TYPE ty_wa_bkpf
           iv_voc_ind                  TYPE char1.

  DATA:    lwa_bkpf                    TYPE ty_wa_bkpf,
           lwa_bseg                    TYPE ty_wa_bseg,
           lwa_int_item                TYPE ty_wa_int_item,
           lwa_int_item_v              TYPE ty_wa_int_item,
           lit_int_item                TYPE ty_it_int_item,
           lwa_goods_recpt             TYPE ty_wa_goods_recpt,
           lwa_msg                     TYPE ty_wa_msg.

  DATA:    lv_posid                    TYPE char24,
           lv_dmbtr                    TYPE dmbtr.

  IF   ( ( iit_bkpf[]               IS INITIAL ) OR
         ( iit_bseg[]               IS INITIAL )    ).
    RETURN.
  ENDIF.

  CLEAR    lwa_bkpf.
  CLEAR    lwa_bseg.
  CLEAR    lwa_int_item.
  CLEAR    lwa_int_item_v.
  CLEAR    lit_int_item[].
  CLEAR    lwa_msg.
  CLEAR    lwa_goods_recpt.

* Read the source vendor accounting document
  CLEAR                                     lwa_int_item_v.
  READ     TABLE cit_int_item          INTO lwa_int_item_v
                                   WITH KEY bukrs = iwa_bkpf_s-bukrs
                                            belnr = iwa_bkpf_s-belnr
                                            gjahr = iwa_bkpf_s-gjahr.
  IF     ( sy-subrc NE 0 ).
    CLEAR                                   lwa_int_item_v.
    READ   TABLE cit_int_item          INTO lwa_int_item_v
                                      INDEX 1.
    IF   ( sy-subrc NE 0 ).
      CLEAR                                 lwa_int_item_v.
    ENDIF.
  ENDIF.

*eject
* Build the accounting items
  CLEAR                                     lwa_bseg.
  LOOP AT  iit_bseg                    INTO lwa_bseg.

* Read the accounting document header
    IF   ( ( lwa_bkpf-bukrs              NE lwa_bseg-bukrs ) OR
           ( lwa_bkpf-belnr              NE lwa_bseg-belnr ) OR
           ( lwa_bkpf-gjahr              NE lwa_bseg-gjahr )    ).
      CLEAR                                 lwa_bkpf.
      READ     TABLE iit_bkpf          INTO lwa_bkpf
                                   WITH KEY bukrs = lwa_bseg-bukrs
                                            belnr = lwa_bseg-belnr
                                            gjahr = lwa_bseg-gjahr.
      IF     ( sy-subrc NE 0 ).
        CLEAR    lwa_bkpf.
      ENDIF.
    ENDIF.

* Get the company data
    IF     ( gwa_t001-bukrs              EQ lwa_bseg-bukrs ).
    ELSE.

      CLEAR                                 gwa_t001.
      PERFORM  f_get_company_data  USING    lwa_bseg-bukrs
                                   CHANGING gwa_t001.

    ENDIF.

* Get the posting key data
    IF     ( gwa_tbsl-bschl              EQ lwa_bseg-bschl ).
    ELSE.

      CLEAR                                 gwa_tbsl.
      PERFORM  f_get_pstkey_data   USING    lwa_bseg-bschl
                                   CHANGING gwa_tbsl.

    ENDIF.

*eject
* Get the purchase order data
    IF   ( ( gwa_po_data-ebeln           EQ lwa_bseg-ebeln ) AND
           ( gwa_po_data-ebelp           EQ lwa_bseg-ebelp )     ).
    ELSE.

      CLEAR                                 gwa_po_data.
      PERFORM  f_get_po_data       USING    lwa_bseg-ebeln
                                            lwa_bseg-ebelp
                                   CHANGING gwa_po_data.

    ENDIF.

* Get the goods receipt data
    CLEAR    lwa_goods_recpt.

    IF ( (   lwa_bseg-ebeln              IS NOT INITIAL ) AND
         (   lwa_bseg-ebelp              IS NOT INITIAL )     ).

      CLEAR                                 lwa_goods_recpt.
      MOVE     lwa_bseg-ebeln            TO lwa_goods_recpt-ebeln.
      MOVE     lwa_bseg-ebelp            TO lwa_goods_recpt-ebelp.
      MOVE     '1'                       TO lwa_goods_recpt-vgabe.
      MOVE     lwa_bseg-menge            TO lwa_goods_recpt-menge.
      MOVE     lwa_bseg-dmbtr            TO lwa_goods_recpt-dmbtr.
      MOVE     lwa_bkpf-xblnr            TO lwa_goods_recpt-xblnr.

      PERFORM  f_get_goods_receipt_data
                                   CHANGING lwa_goods_recpt.

    ENDIF.

* Set the cost object
    CLEAR    lv_posid.

    PERFORM  f_set_cost_object     USING    lwa_bseg
                                   CHANGING lv_posid.

* Set the amount in local currency
    CLEAR                                   lv_dmbtr.
    PERFORM  f_determine_amount    USING    lwa_bkpf-budat
                                            iwa_bkpf_s-hwaer
                                            lwa_bkpf-waers
                                            lwa_bkpf-hwaer
                                            lwa_bseg-wrbtr
                                            lwa_bseg-dmbtr
                                   CHANGING lv_dmbtr.

    IF     ( gwa_tbsl-shkzg              EQ 'H' ).
      MULTIPLY                              lv_dmbtr BY -1.
    ENDIF.

    "BOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
    SELECT SINGLE value1
     INTO lv_fg
     FROM zfit_xparam
     WHERE paramtype = lc_param AND
           subtype = lc_doc AND
           key1 = lc_1 AND
           value1 = lwa_bkpf-blart.
    IF sy-subrc = 0.
      CLEAR lwa_bseg-sgtxt.
    ENDIF.
    "EOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data

*eject
* Create the lines for accounting items
    CLEAR                                   lwa_int_item.
    MOVE     lwa_bseg-bukrs              TO lwa_int_item-bukrs.
    MOVE     lwa_bseg-belnr              TO lwa_int_item-belnr.
    MOVE     lwa_bseg-gjahr              TO lwa_int_item-gjahr.
    MOVE     lwa_bseg-buzei              TO lwa_int_item-buzei.
    MOVE     lwa_bseg-koart              TO lwa_int_item-koart.
    MOVE     lwa_int_item_v-lifnr        TO lwa_int_item-lifnr.
    MOVE     lwa_int_item_v-ktokk        TO lwa_int_item-ktokk.
    MOVE     lwa_int_item_v-name1        TO lwa_int_item-name1.
    MOVE     lwa_int_item_v-land1        TO lwa_int_item-land1.
    MOVE     lwa_int_item_v-stras        TO lwa_int_item-stras.
    MOVE     lwa_int_item_v-ort01        TO lwa_int_item-ort01.
    MOVE     lwa_int_item_v-regio        TO lwa_int_item-regio.
    MOVE     lwa_int_item_v-pstlz        TO lwa_int_item-pstlz.
    MOVE     lwa_int_item_v-mindk        TO lwa_int_item-mindk.
    MOVE     lwa_bkpf-budat              TO lwa_int_item-budat.
    MOVE     lwa_int_item_v-dmbtr_total  TO lwa_int_item-dmbtr_total.
    MOVE     lwa_int_item_v-augdt        TO lwa_int_item-augdt.
    MOVE     lwa_int_item_v-paymeth      TO lwa_int_item-paymeth.
    MOVE     lwa_bkpf-xblnr              TO lwa_int_item-xblnr.
    MOVE     lwa_bkpf-bldat              TO lwa_int_item-bldat.
    MOVE     lwa_int_item_v-saknr        TO lwa_int_item-saknr.
    MOVE     lwa_bseg-hkont              TO lwa_int_item-hkont.

    IF     ( lwa_bseg-sgtxt              IS INITIAL ).
      MOVE   lwa_int_item_v-sgtxt        TO lwa_int_item-sgtxt.
    ELSE.
      MOVE   lwa_bseg-sgtxt              TO lwa_int_item-sgtxt.
    ENDIF.

    MOVE     lv_dmbtr                    TO lwa_int_item-dmbtr.
    MOVE     lv_posid                    TO lwa_int_item-posid.
    MOVE     lwa_bseg-mwskz              TO lwa_int_item-taxtext.
    MOVE     gwa_po_data-ort01           TO lwa_int_item-port01.

    IF     ( lwa_bseg-ebeln              IS NOT INITIAL ).
      MOVE   lwa_bseg-ebeln              TO lwa_int_item-pur_ord.
    ELSEIF ( lwa_bseg-xref1              IS NOT INITIAL ).
      MOVE   lwa_bseg-xref1              TO lwa_int_item-pur_ord.
    ELSEIF ( lwa_int_item_v-pur_ord      IS NOT INITIAL ).
      MOVE   lwa_int_item_v-pur_ord      TO lwa_int_item-pur_ord.
    ENDIF.

*eject
    MOVE     lwa_int_item_v-zterm        TO lwa_int_item-zterm.
    MOVE     lwa_int_item_v-chect        TO lwa_int_item-chect.
    MOVE     lwa_bkpf-hwaer              TO lwa_int_item-waers.
    MOVE     p_datsrc                    TO lwa_int_item-datasrce.
    MOVE     lwa_bseg-bschl              TO lwa_int_item-ltext.
    MOVE     lwa_bkpf-bktxt              TO lwa_int_item-bktxt.
    MOVE     lwa_bkpf-blart              TO lwa_int_item-blart.
    MOVE     lwa_bseg-ebelp              TO lwa_int_item-ebelp.
    MOVE     gwa_po_data-matkl           TO lwa_int_item-matkl.
    MOVE     gwa_po_data-txz01           TO lwa_int_item-matdesc.
    MOVE     lwa_int_item_v-uzawe        TO lwa_int_item-uzawe.
    MOVE     gwa_po_data-konnr           TO lwa_int_item-konnr.
    MOVE     lwa_bseg-kostl              TO lwa_int_item-kostl.
    MOVE     lwa_int_item_v-erdat        TO lwa_int_item-erdat.
    MOVE     lwa_int_item_v-netdt        TO lwa_int_item-netdt.
    MOVE     lwa_int_item_v-sknto_total  TO lwa_int_item-sknto_total.
    MOVE     lwa_int_item_v-approver     TO lwa_int_item-approver.
    MOVE     lwa_bkpf-bvorg              TO lwa_int_item-bvorg.

    MOVE     lwa_bkpf-usnam              TO lwa_int_item-usnam.
    MOVE     gwa_po_data-bstyp           TO lwa_int_item-bstyp.
    MOVE     gwa_po_data-bsart           TO lwa_int_item-bsart.
    MOVE     gwa_po_data-matnr           TO lwa_int_item-matnr.
    MOVE     gwa_po_data-werks           TO lwa_int_item-werks.
    MOVE     lwa_goods_recpt-budat       TO lwa_int_item-grdat.
    MOVE     gwa_po_data-wepos           TO lwa_int_item-wepos.
    MOVE     gwa_po_data-repos           TO lwa_int_item-repos.
    MOVE     gwa_po_data-webre           TO lwa_int_item-webre.
    MOVE     iwa_bkpf_s-buk_s1           TO lwa_int_item-buk_s1.
    MOVE     iwa_bkpf_s-bel_s1           TO lwa_int_item-bel_s1.
    MOVE     iwa_bkpf_s-gja_s1           TO lwa_int_item-gja_s1.
    MOVE     iv_voc_ind                  TO lwa_int_item-voc_ind.
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
    MOVE     gwa_po_data-ktext           TO lwa_int_item-ktext.
    MOVE     gwa_po_data-konty           TO lwa_int_item-konty.
    MOVE     gwa_po_data-recvr           TO lwa_int_item-recvr.
    MOVE     gwa_po_data-iddes           TO lwa_int_item-iddes.
    MOVE     lwa_bseg-dmbtr              TO lwa_int_item-setl_amnt.
    MOVE     gwa_po_data-menge           TO lwa_int_item-ord_qty.
    MOVE     gwa_po_data-meins           TO lwa_int_item-ord_unit.
    MOVE     gwa_po_data-netpr           TO lwa_int_item-net_prc.
    MOVE     gwa_po_data-ekgrp           TO lwa_int_item-pgrp.
    MOVE     gwa_po_data-eknam           TO lwa_int_item-pgrp_desc.
    MOVE     gwa_po_data-werks           TO lwa_int_item-plant_code.
    MOVE     gwa_po_data-name1           TO lwa_int_item-plant_code_desc.
    MOVE     gwa_po_data-aedat           TO lwa_int_item-order_crea_dt.
    MOVE     gwa_po_data-street          TO lwa_int_item-ship_adrc.
    MOVE     gwa_po_data-city1           TO lwa_int_item-ship_city.
    MOVE     gwa_po_data-region          TO lwa_int_item-ship_state.
    MOVE     gwa_po_data-post_code1      TO lwa_int_item-ship_zip.
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
    MOVE     gwa_po_data-set_desc      TO lwa_int_item-set_desc.
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields

    IF lv_flag IS INITIAL. "Added by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
      APPEND                                  lwa_int_item
                                           TO lit_int_item.
    ENDIF. "Added by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
    CLEAR  lwa_bseg.
  ENDLOOP.

  APPEND   LINES                         OF lit_int_item
                                         TO cit_int_item.

ENDFORM.                    " f_process_accntg_items
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_company_data
*&---------------------------------------------------------------------*
*       Get the company data
*----------------------------------------------------------------------*
FORM f_get_company_data
  USING    iv_bukrs                    TYPE bukrs
  CHANGING cwa_t001                    TYPE ty_wa_t001.

  DATA:    lwa_t001                    TYPE ty_wa_t001.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_kalsm                    TYPE kalsm_d.

  CLEAR    cwa_t001.

  IF     ( iv_bukrs                      IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     lwa_t001.
  READ     TABLE git_t001              INTO lwa_t001
                                   WITH KEY bukrs = iv_bukrs
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF     ( lv_subrc NE 0 ).

    CLEAR    lwa_t001.
    SELECT   SINGLE bukrs  land1  waers
      INTO   lwa_t001
      FROM   t001
     WHERE   bukrs = iv_bukrs.
    IF     ( sy-subrc EQ 0 ).

      CLEAR    lv_kalsm.
      SELECT   SINGLE kalsm
        INTO   lv_kalsm
        FROM   t005
       WHERE   land1 = lwa_t001-land1.
      IF     ( sy-subrc EQ 0 ).
        CLEAR                               lwa_t001-kalsm.
        MOVE   lv_kalsm                  TO lwa_t001-kalsm.
      ELSE.
        CLEAR                               lwa_t001-kalsm.
      ENDIF.

    ELSE.
      CLEAR                                 lwa_t001.
      MOVE     iv_bukrs                  TO lwa_t001-bukrs.
    ENDIF.

*eject
    INSERT                                  lwa_t001
                                       INTO git_t001
                                      INDEX lv_tabix.

  ENDIF.

  MOVE     lwa_t001                      TO cwa_t001.

ENDFORM.                    " f_get_company_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_pstkey_data
*&---------------------------------------------------------------------*
*       Get the posting key data
*----------------------------------------------------------------------*
FORM f_get_pstkey_data
  USING    iv_bschl                    TYPE bschl
  CHANGING cwa_tbsl                    TYPE ty_wa_tbsl.

  DATA:    lwa_tbsl                    TYPE ty_wa_tbsl.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_ltext                    TYPE text_bslt.

  CLEAR    cwa_tbsl.

  IF     ( iv_bschl                      IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     lwa_tbsl.
  READ     TABLE git_tbsl              INTO lwa_tbsl
                                   WITH KEY bschl = iv_bschl
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF     ( lv_subrc NE 0 ).

    CLEAR    lwa_tbsl.
    SELECT   SINGLE bschl  shkzg
      INTO   lwa_tbsl
      FROM   tbsl
     WHERE   bschl = iv_bschl.
    IF     ( sy-subrc EQ 0 ).

      CLEAR    lv_ltext.
      SELECT   SINGLE ltext
        INTO   lv_ltext
        FROM   tbslt
       WHERE   spras = 'E'
         AND   bschl = iv_bschl.
      IF     ( sy-subrc NE 0 ).
        CLEAR                               lv_ltext.
        MOVE   'XXXX'                    TO lv_ltext.
      ENDIF.

      CLEAR                                 lwa_tbsl-ltext.
      CONCATENATE                           iv_bschl '_'
                                            lv_ltext
                                       INTO lwa_tbsl-ltext.

*eject
    ELSE.
      CLEAR                                 lwa_tbsl.
      MOVE     iv_bschl                  TO lwa_tbsl-bschl.
      CONCATENATE                           iv_bschl '_'
                                            'XXXX'
                                       INTO lwa_tbsl-ltext.
    ENDIF.

    INSERT                                  lwa_tbsl
                                       INTO git_tbsl
                                      INDEX lv_tabix.

  ENDIF.

  MOVE     lwa_tbsl                      TO cwa_tbsl.

ENDFORM.                    " f_get_pstkey_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_vendor_data
*&---------------------------------------------------------------------*
*       Get the vendor data
*----------------------------------------------------------------------*
FORM f_get_vendor_data
  USING    iv_lifnr                    TYPE lifnr
           iv_bukrs                    TYPE bukrs
  CHANGING cwa_vendor                  TYPE ty_wa_vendor.

  DATA:    lwa_vendor                  TYPE ty_wa_vendor,
           lwa_lfa1                    TYPE ty_wa_lfa1,
           lwa_lfb1                    TYPE ty_wa_lfb1,
           lwa_cdhdr                   TYPE ty_wa_cdhdr,
           lit_cdhdr                   TYPE ty_it_cdhdr.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix.

  CLEAR    cwa_vendor.

  IF   ( ( iv_lifnr                      IS INITIAL ) OR
         ( iv_bukrs                      IS INITIAL )    ).
    RETURN.
  ENDIF.

  CLEAR                                     lwa_vendor.
  READ     TABLE git_vendor            INTO lwa_vendor
                                   WITH KEY lifnr = iv_lifnr
                                            bukrs = iv_bukrs
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF     ( lv_subrc NE 0 ).

    CLEAR    lwa_lfa1.
    CLEAR    lwa_lfb1.
    CLEAR    lv_flag. "Added by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data

    SELECT   SINGLE lifnr  land1  name1  ort01
                    pfach  pstl2  pstlz  regio
                    stras  adrnr  erdat  ktokk
      INTO   lwa_lfa1
      FROM   lfa1
     WHERE   lifnr = iv_lifnr.
    IF     ( sy-subrc EQ 0 ).
      "BOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
     SELECT SINGLE value1
       INTO lv_ven
       FROM zfit_xparam
       WHERE paramtype = lc_param AND
             subtype = lc_vendor AND
             key1 = lc_1 AND
             value1 = lwa_lfa1-ktokk.
      IF sy-subrc = 0.
        lv_flag = gc_x.
      ENDIF.
      "EOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
      SELECT   SINGLE lifnr  bukrs  erdat  mindk
        INTO   lwa_lfb1
        FROM   lfb1
       WHERE   lifnr = iv_lifnr
         AND   bukrs = iv_bukrs.
      IF     ( sy-subrc NE 0 ).
        CLEAR  lwa_lfb1.
      ENDIF.

*eject
      CLEAR    lit_cdhdr[].
      SELECT   objectclas  objectid  changenr  udate
        INTO   TABLE lit_cdhdr
        FROM   cdhdr
       WHERE   objectclas = 'KRED'
         AND   objectid   = iv_lifnr.
      IF     ( sy-subrc EQ 0 ).
        SORT   lit_cdhdr BY udate DESCENDING.
      ELSE.
        CLEAR  lit_cdhdr[].
      ENDIF.

      CLEAR                                 lwa_vendor.
      MOVE     iv_lifnr                  TO lwa_vendor-lifnr.
      MOVE     iv_bukrs                  TO lwa_vendor-bukrs.
      MOVE     lwa_lfa1-name1            TO lwa_vendor-name1.

      IF   ( ( lwa_lfa1-stras            IS INITIAL     ) AND
             ( lwa_lfa1-pfach            IS NOT INITIAL )     ).
        CONCATENATE                         text-182
                                            lwa_lfa1-pfach
                                       INTO lwa_vendor-stras
                         SEPARATED BY space.
        MOVE   lwa_lfa1-pstl2            TO lwa_vendor-pstlz.
      ELSE.
        MOVE   lwa_lfa1-stras            TO lwa_vendor-stras.
        MOVE   lwa_lfa1-pstlz            TO lwa_vendor-pstlz.
      ENDIF.

      MOVE     lwa_lfa1-ort01            TO lwa_vendor-ort01.
      MOVE     lwa_lfa1-regio            TO lwa_vendor-regio.
      MOVE     lwa_lfa1-land1            TO lwa_vendor-land1.
      MOVE     lwa_lfa1-adrnr            TO lwa_vendor-adrnr.
      MOVE     lwa_lfa1-ktokk            TO lwa_vendor-ktokk.
      MOVE     lwa_lfb1-mindk            TO lwa_vendor-mindk.
      MOVE     lwa_lfa1-erdat            TO lwa_vendor-erdat.

      IF     ( lwa_lfb1-erdat            GT lwa_vendor-erdat ).
        CLEAR                               lwa_vendor-erdat.
        MOVE   lwa_lfb1-erdat            TO lwa_vendor-erdat.
      ENDIF.

      CLEAR                                 lwa_cdhdr.
      READ     TABLE lit_cdhdr         INTO lwa_cdhdr INDEX 1.
      IF     ( sy-subrc EQ 0 ).
        IF     ( lwa_cdhdr-udate         GT lwa_vendor-erdat ).
          CLEAR                             lwa_vendor-erdat.
          MOVE   lwa_cdhdr-udate         TO lwa_vendor-erdat.
        ENDIF.
      ENDIF.

*eject
    ELSE.
      CLEAR                                 lwa_vendor.
      MOVE     iv_lifnr                  TO lwa_vendor-lifnr.
      MOVE     iv_bukrs                  TO lwa_vendor-bukrs.
    ENDIF.

    INSERT                                  lwa_vendor
                                       INTO git_vendor
                                      INDEX lv_tabix.

  ENDIF.

  MOVE     lwa_vendor                    TO cwa_vendor.

ENDFORM.                    " f_get_vendor_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_payment_method
*&---------------------------------------------------------------------*
*       Set the payment method
*----------------------------------------------------------------------*
FORM f_set_payment_method
  USING    iwa_bkpf                    TYPE ty_wa_bkpf
           iwa_bseg                    TYPE ty_wa_bseg
  CHANGING cv_paymeth                  TYPE char8
           cv_chect                    TYPE chect.

  DATA:    lv_zlsch                    TYPE dzlsch,
           lv_paymeth                  TYPE char8,
           lv_chect                    TYPE chect,
           lv_flag_cheque              TYPE flag.

  CLEAR    cv_paymeth.
  CLEAR    cv_chect.

  CLEAR    lv_zlsch.
  CLEAR    lv_paymeth.
  CLEAR    lv_chect.
  CLEAR    lv_flag_cheque.

* Set the payment method code
  IF       ( iwa_bseg-augbl              IS INITIAL        ).
    CLEAR    gwa_bsak_c.
  ELSEIF ( ( gwa_bsak_c-bukrs            EQ iwa_bseg-bukrs ) AND
           ( gwa_bsak_c-lifnr            EQ iwa_bseg-lifnr ) AND
           ( gwa_bsak_c-augdt            EQ iwa_bseg-augdt ) AND
           ( gwa_bsak_c-augbl            EQ iwa_bseg-augbl ) AND
           ( gwa_bsak_c-gjahr            EQ iwa_bseg-auggj ) AND
           ( gwa_bsak_c-belnr            EQ iwa_bseg-augbl )     ).
  ELSE.
    CLEAR    gwa_bsak_c.
    SELECT   SINGLE bukrs  lifnr  umsks
             umskz  augdt  augbl  zuonr
             gjahr  belnr  buzei  zlsch
      INTO   gwa_bsak_c
      FROM   bsak
     WHERE   bukrs = iwa_bseg-bukrs
       AND   lifnr = iwa_bseg-lifnr
       AND   augdt = iwa_bseg-augdt
       AND   augbl = iwa_bseg-augbl
       AND   gjahr = iwa_bseg-auggj
       AND   belnr = iwa_bseg-augbl.
    IF ( sy-subrc NE 0 ).
      CLEAR  gwa_bsak_c.
    ENDIF.
  ENDIF.

  IF     ( gwa_bsak_c-zlsch              IS NOT INITIAL ).
    MOVE   gwa_bsak_c-zlsch              TO lv_zlsch.
  ELSEIF ( iwa_bseg-zlsch                IS NOT INITIAL ).
    MOVE   iwa_bseg-zlsch                TO lv_zlsch.
  ENDIF.

*eject
* Determine the payment method text
  IF     ( iwa_bkpf-xreversal            EQ '2'  ).
    MOVE     gc_paymeth_rev              TO lv_paymeth.
  ELSEIF ( p_datsrc                      EQ 'US' ).
    CASE     lv_zlsch.
      WHEN     'A' OR 'X'.
        MOVE   gc_paymeth_ach            TO lv_paymeth.
      WHEN     'B' OR 'S' OR 'T' OR 'U' OR 'V' OR 'W' OR 'Y' OR 'Z'.
        MOVE   gc_paymeth_wir            TO lv_paymeth.
      WHEN     'C' OR 'I' OR 'Q'.
        MOVE   gc_paymeth_chk            TO lv_paymeth.
        MOVE   gc_x                      TO lv_flag_cheque.
      WHEN     'E' OR 'F' OR 'G' OR 'H'.
        MOVE   gc_paymeth_eft            TO lv_paymeth.
      WHEN     'N'.
        MOVE   gc_paymeth_epy            TO lv_paymeth.
      WHEN     'P'.
        MOVE   gc_paymeth_man            TO lv_paymeth.
      WHEN     OTHERS.
        MOVE   'XXXX'                    TO lv_paymeth.
    ENDCASE.
  ELSEIF ( p_datsrc                      EQ 'EAST' ).
    CASE     lv_zlsch.
      WHEN     'D' OR 'I' OR 'J' OR 'Y' OR 'Z'.
        MOVE   gc_paymeth_wir            TO lv_paymeth.
      WHEN     '3' OR 'A' OR 'B' OR 'C' OR 'G' OR 'H' OR
               'K' OR 'L' OR 'N' OR 'O' OR 'P' OR 'Q'.
        MOVE   gc_paymeth_chk            TO lv_paymeth.
        MOVE   gc_x                      TO lv_flag_cheque.
      WHEN     'E' OR 'F' OR 'T' OR 'X'.
        MOVE   gc_paymeth_eft            TO lv_paymeth.
      WHEN     'M'.
        MOVE   gc_paymeth_man            TO lv_paymeth.
      WHEN     'N'.
        MOVE   gc_paymeth_epy            TO lv_paymeth.
      WHEN     OTHERS.
        MOVE   'XXXX'                    TO lv_paymeth.
    ENDCASE.
  ELSEIF ( p_datsrc                      EQ 'WEST' ).
    CASE     lv_zlsch.
      WHEN     'B' OR 'D' OR 'I' OR 'K' OR 'Z'.
        MOVE   gc_paymeth_wir            TO lv_paymeth.
      WHEN     'C' OR 'O' OR 'P' OR 'Q' OR 'S'.
        MOVE   gc_paymeth_chk            TO lv_paymeth.
        MOVE   gc_x                      TO lv_flag_cheque.
      WHEN     'E' OR 'F' OR 'T'.
        MOVE   gc_paymeth_eft            TO lv_paymeth.
      WHEN     'M'.
        MOVE   gc_paymeth_man            TO lv_paymeth.
      WHEN     'N'.
        MOVE   gc_paymeth_epy            TO lv_paymeth.
      WHEN     OTHERS.
        MOVE   'XXXX'                    TO lv_paymeth.
    ENDCASE.
  ENDIF.

*eject
  IF     ( lv_paymeth                    IS INITIAL ).
    MOVE   'XXXX'                        TO lv_paymeth.
  ENDIF.

* Set the check number
  IF       ( iwa_bseg-augbl              IS INITIAL ).
  ELSEIF   ( lv_flag_cheque              IS INITIAL ).
    MOVE     iwa_bseg-augbl              TO lv_chect.
  ELSE.
    CLEAR    lv_chect.
    SELECT   SINGLE chect
      INTO   lv_chect
      FROM   payr
     WHERE   zbukr = iwa_bseg-bukrs
       AND   vblnr = iwa_bseg-augbl
       AND   gjahr = iwa_bseg-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR                                 lv_chect.
      MOVE   iwa_bseg-augbl              TO lv_chect.
    ENDIF.
  ENDIF.

  IF     ( lv_chect                      IS INITIAL ).
    MOVE   'XXXX'                        TO lv_chect.
  ENDIF.

  MOVE     lv_paymeth                    TO cv_paymeth.
  MOVE     lv_chect                      TO cv_chect.

ENDFORM.                    " f_set_payment_method
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_net_due_date
*&---------------------------------------------------------------------*
*       Get the net due date
*----------------------------------------------------------------------*
FORM f_get_net_due_date
  USING    iwa_bseg                    TYPE ty_wa_bseg
  CHANGING cv_netdt                    TYPE faedt_fpos.

  DATA:    lv_netdt                    TYPE faedt_fpos.

  CLEAR    cv_netdt.

  CLEAR    lv_netdt.

  CALL FUNCTION 'NET_DUE_DATE_GET'
    EXPORTING
      i_zfbdt = iwa_bseg-zfbdt
      i_zbd1t = iwa_bseg-zbd1t
      i_zbd2t = iwa_bseg-zbd2t
      i_zbd3t = iwa_bseg-zbd3t
      i_shkzg = iwa_bseg-shkzg
      i_rebzg = iwa_bseg-rebzg
      i_koart = iwa_bseg-koart
    IMPORTING
      e_faedt = lv_netdt.

  MOVE     lv_netdt                      TO cv_netdt.

ENDFORM.                    " f_get_net_due_date
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_approver
*&---------------------------------------------------------------------*
*       Get the approver
*----------------------------------------------------------------------*
FORM f_get_approver
  USING    iwa_bkpf                    TYPE ty_wa_bkpf
  CHANGING cv_approver                 TYPE char12.

  DATA:    lwa_worklist                TYPE swr_wihdr,
           lit_worklist                TYPE STANDARD TABLE OF swr_wihdr,
           lwa_swwwihead               TYPE ty_wa_swwwihead,
           lit_swwwihead               TYPE ty_it_swwwihead.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_objkey                   TYPE swo_typeid.

  CONSTANTS:
           gc_rh_task_ap_wf            TYPE sww_task
                                      VALUE 'WS98300017',
           gc_rh_task_aprvr            TYPE sww_task
                                      VALUE 'TS98300060'.

  CLEAR    cv_approver.

  CLEAR                                     lv_objkey.
  CONCATENATE                               iwa_bkpf-bukrs
                                            iwa_bkpf-belnr
                                            iwa_bkpf-gjahr
                                       INTO lv_objkey.

  CLEAR    lit_worklist[].

  CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
    EXPORTING
      objtype                  = 'FIPP'
      objkey                   = lv_objkey
      top_level_items          = 'X'
      selection_status_variant = 0000
      text                     = 'X'
      output_only_top_level    = 'X'
      language                 = 'E'
      determine_task_filter    = ' '
      removed_objects          = ' '
    IMPORTING
      return_code              = lv_subrc
    TABLES
      worklist                 = lit_worklist.

  DELETE   lit_worklist WHERE wi_rh_task NE gc_rh_task_ap_wf.

  IF     ( lit_worklist[]                IS INITIAL ).
    RETURN.
  ENDIF.

*eject
  SORT     lit_worklist        ASCENDING BY wi_cd wi_ct.

  CLEAR                                     lwa_worklist.
  READ     TABLE lit_worklist          INTO lwa_worklist INDEX 1.

  CLEAR    lit_swwwihead[].
  SELECT   wi_id  wi_type  wi_cd  wi_ct  wi_aagent  wi_rh_task
    INTO   TABLE lit_swwwihead
    FROM   swwwihead
   WHERE   top_wi_id = lwa_worklist-wi_id.
  IF   ( sy-subrc NE 0 ).
    CLEAR  lit_swwwihead[].
    RETURN.
  ENDIF.

  DELETE   lit_swwwihead WHERE NOT
                          ( ( wi_type    EQ 'W'              ) AND
                            ( wi_rh_task EQ gc_rh_task_aprvr )     ).

  IF     ( lit_swwwihead[]               IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     lit_swwwihead                 BY wi_cd DESCENDING
                                            wi_ct DESCENDING.

  CLEAR                                     lwa_swwwihead.
  READ     TABLE lit_swwwihead         INTO lwa_swwwihead INDEX 1.

  MOVE     lwa_swwwihead-wi_aagent       TO cv_approver.

ENDFORM.                 " f_get_approver
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_po_data
*&---------------------------------------------------------------------*
*       Get the purchase order data
*----------------------------------------------------------------------*
FORM f_get_po_data
  USING    iv_ebeln                    TYPE ebeln
           iv_ebelp                    TYPE ebelp
  CHANGING cwa_po_data                 TYPE ty_wa_po_data.

  DATA:    lwa_po_data                 TYPE ty_wa_po_data,
           lwa_t001w                   TYPE ty_wa_t001w.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_werks                    TYPE ewerk,
           lv_ort01                    TYPE ort01,
           lv_city1                    TYPE ad_city1.

*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
  CONSTANTS : lc_koao TYPE c LENGTH 4 VALUE 'KOAO'.
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields

*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
  TYPES : BEGIN OF lty_adrc,
          street TYPE adrc-street,
          city1  TYPE adrc-city1,
          region TYPE adrc-region,
          post_code1 TYPE adrc-post_code1,
          END OF lty_adrc,

          BEGIN OF lty_ekkn,
          ebeln TYPE ebeln,
          ebelp TYPE ebelp,
          aufnr TYPE aufnr,
          ps_psp_pnr TYPE ps_psp_pnr,
          kostl TYPE kostl,
          anln1 TYPE anln1,
*BOC by KMB CHG0149221 ENHC0025832 Add WBS logic in Power advocate report 19.6.2019
          nplnr TYPE nplnr,
*BOC by KMB CHG0149221 ENHC0025832 Add WBS logic in Power advocate report 19.6.2019

          END OF lty_ekkn,

          BEGIN OF lty_aufk,
          aufnr TYPE aufnr,
          ktext TYPE ktext,
          objnr TYPE objnr,
          END OF lty_aufk,

          BEGIN OF lty_prps,
          pspnr TYPE prps-pspnr,
          post1 TYPE prps-post1,
          objnr TYPE objnr,
          END OF lty_prps,

          BEGIN OF lty_csks,
          kostl TYPE kostl,
          objnr TYPE objnr,
          END OF lty_csks,

          BEGIN OF lty_anla,
          anln1 TYPE anln1,
          txt50 TYPE txt50,
          objnr TYPE objnr,
          END OF lty_anla,

          BEGIN OF lty_cobrb,
          objnr TYPE objnr,
          avorg TYPE avorg,
          konty TYPE konty,
          aufnr TYPE aufnr,
          ps_psp_pnr TYPE ps_psp_pnr,
          kostl TYPE kostl,
          anln1 TYPE anln1,
          END OF lty_cobrb.

  DATA : gs_adrc TYPE lty_adrc,
         lv_eknam TYPE eknam,
         lv_name1 TYPE name1,
         lv_ktext TYPE char50,
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
         lv_ltxt  TYPE char50,
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
         lv_ktxt  TYPE char50,
         lv_konty TYPE char10,
         lv_ounit TYPE char10,
         lv_objnr TYPE objnr,
         lv_srcvr TYPE string,
         lt_ekkn TYPE TABLE OF lty_ekkn,
         ls_ekkn TYPE lty_ekkn,
         lt_aufk TYPE TABLE OF lty_aufk,
         ls_aufk TYPE lty_aufk,
         lt_prps TYPE TABLE OF lty_prps,
         ls_prps TYPE lty_prps,
         lt_csks TYPE TABLE OF lty_csks,
         ls_csks TYPE lty_csks,
         lt_anla TYPE TABLE OF lty_anla,
         ls_anla TYPE lty_anla,
         lt_cobrb TYPE TABLE OF lty_cobrb,
         ls_cobrb TYPE lty_cobrb.
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields

  CLEAR    cwa_po_data.

  IF     ( iv_ebeln                      IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lwa_t001w.
  CLEAR    lv_werks.
  CLEAR    lv_ort01.
  CLEAR    lv_city1.

  CLEAR                                     lwa_po_data.
  READ   TABLE git_po_data             INTO lwa_po_data
                                   WITH KEY ebeln = iv_ebeln
                                            ebelp = iv_ebelp
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.

  IF     ( lv_subrc EQ 0 ).
    MOVE     lwa_po_data                 TO cwa_po_data.
    RETURN.
  ENDIF.

  CLEAR      lwa_po_data.

*eject
* Select the PO document header
  IF       ( gwa_ekko-ebeln              EQ iv_ebeln ).
  ELSE.

    CLEAR    gwa_ekko.
    SELECT   SINGLE ebeln  bstyp  bsart
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
                    ekgrp aedat
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
      INTO   gwa_ekko
      FROM   ekko
     WHERE   ebeln = iv_ebeln.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gwa_ekko.
    ENDIF.

  ENDIF.

*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
  CLEAR : lv_ounit.
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields

* Select the PO document item
  IF     ( ( gwa_ekpo-ebeln              EQ iv_ebeln ) AND
           ( gwa_ekpo-ebelp              EQ iv_ebelp )     ).
  ELSE.

    CLEAR    gwa_ekpo.

    IF     ( iv_ebelp                    IS NOT INITIAL ).

      CLEAR    gwa_ekpo.
      SELECT   SINGLE ebeln  ebelp  txz01  matnr
               werks  matkl  wepos  repos  webre
               konnr  adrnr
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
        menge meins netpr
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
        INTO   gwa_ekpo
        FROM   ekpo
       WHERE   ebeln = iv_ebeln
         AND   ebelp = iv_ebelp.
      IF     ( sy-subrc NE 0 ).
        CLEAR  gwa_ekpo.
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields Order unit changes
      ELSEIF sy-subrc = 0.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
          EXPORTING
            input  = gwa_ekpo-meins
          IMPORTING
            output = lv_ounit.
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields order unit changes
      ENDIF.

    ENDIF.

    IF     ( gwa_ekpo-werks              IS INITIAL ).

      CLEAR    lv_werks.
      SELECT   werks
        INTO   lv_werks
        FROM   ekpo UP TO 1 ROWS
       WHERE   ebeln = iv_ebeln
         AND   werks > space.
      ENDSELECT.
      IF     ( sy-subrc EQ 0 ).
        CLEAR                               gwa_ekpo-werks.
        MOVE   lv_werks                  TO gwa_ekpo-werks.
      ENDIF.

    ENDIF.

  ENDIF.

*eject
* Get the city
  CLEAR    lv_ort01.

  IF   ( ( lv_ort01                      IS INITIAL     ) AND
         ( gwa_ekpo-adrnr                IS NOT INITIAL )     ).

    CLEAR    lv_ort01.
    SELECT   SINGLE ort01
      INTO   lv_ort01
      FROM   sadr
     WHERE   adrnr = gwa_ekpo-adrnr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lv_ort01.

      CLEAR    lv_city1.
      SELECT   SINGLE city1
        INTO   lv_city1
        FROM   adrc
       WHERE   addrnumber = gwa_ekpo-adrnr.
      IF     ( sy-subrc EQ 0 ).
        CLEAR                               lv_ort01.
        MOVE   lv_city1                  TO lv_ort01.
      ELSE.
        CLEAR  lv_city1.
      ENDIF.

**BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
      CLEAR : gs_adrc,lv_eknam, lv_name1.
      SELECT   SINGLE street city1 region post_code1
        INTO   gs_adrc
        FROM   adrc
       WHERE   addrnumber = gwa_ekpo-adrnr.
      IF     ( sy-subrc EQ 0 ).
        "Do nothing
      ENDIF.
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields

    ENDIF.

  ENDIF.

**BOC by KMB on 27.03.2019 CHG0133164 Power Advocate Additional fields
  SELECT SINGLE eknam
    FROM t024
    INTO lv_eknam
    WHERE ekgrp = gwa_ekko-ekgrp.
  IF sy-subrc = 0.
    "Do nothing
  ENDIF.

  SELECT SINGLE name1
    FROM t001w
    INTO lv_name1
    WHERE werks = gwa_ekpo-werks.
  IF sy-subrc = 0.
    "Do nothing
  ENDIF.
*EOC by KMB on 27.03.2019 CHG0133164 Power Advocate Additional fields


*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
  CLEAR : ls_ekkn, ls_aufk, lv_objnr, lv_ktxt, ls_prps, ls_csks,
          lv_objnr, lv_ktext, ls_anla, ls_cobrb, lv_srcvr.

*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
  CLEAR : lv_konty, lv_ltxt.
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields

  SELECT SINGLE ebeln ebelp aufnr ps_psp_pnr kostl anln1
*BOC by KMB CHG0149221 ENHC0025832 Add WBS logic in Power advocate report 19.6.2019
                nplnr
*EOC by KMB CHG0149221 ENHC0025832 Add WBS logic in Power advocate report 19.6.2019
    FROM ekkn
    INTO ls_ekkn
    WHERE ebeln = gwa_ekpo-ebeln AND
          ebelp = gwa_ekpo-ebelp.
  IF sy-subrc = 0.
    IF ls_ekkn-aufnr IS NOT INITIAL.
      SELECT SINGLE aufnr ktext objnr
        FROM aufk
        INTO ls_aufk
        WHERE aufnr = ls_ekkn-aufnr.
      IF sy-subrc = 0.
        lv_objnr = ls_aufk-objnr.
        lv_ktxt = ls_aufk-ktext.
      ENDIF.
    ELSEIF ls_ekkn-ps_psp_pnr IS NOT INITIAL.
      SELECT SINGLE pspnr post1 objnr
        FROM prps
        INTO ls_prps
        WHERE pspnr = ls_ekkn-ps_psp_pnr.
      IF sy-subrc = 0.
        lv_objnr = ls_prps-objnr.
        lv_ktxt = ls_prps-post1.
      ENDIF.
    ELSEIF ls_ekkn-kostl IS NOT INITIAL.
      SELECT SINGLE kostl objnr
        FROM csks
        INTO ls_csks
        WHERE kostl = ls_ekkn-kostl.
      IF sy-subrc = 0.
        lv_objnr = ls_csks-objnr.
        SELECT SINGLE ktext "Need ltext field
          FROM cskt
          INTO lv_ktext
          WHERE kostl = ls_csks-kostl.
        IF sy-subrc = 0.
          lv_ktxt = lv_ktext.
        ENDIF.
      ENDIF.
    ELSEIF ls_ekkn-anln1 IS NOT INITIAL.
      SELECT SINGLE anln1 txt50 objnr
        FROM anla
        INTO ls_anla
        WHERE anln1 = ls_ekkn-anln1.
      IF sy-subrc = 0.
        lv_objnr = ls_anla-objnr.
        lv_ktxt = ls_anla-txt50.
      ENDIF.
*BOC by KMB CHG0149221 ENHC0025832 Add WBS logic in Power advocate report 19.6.2019
    ELSEIF ls_ekkn-nplnr IS NOT INITIAL.
      SELECT SINGLE aufnr ktext objnr
        FROM aufk
        INTO ls_aufk
        WHERE aufnr = ls_ekkn-nplnr.
      IF sy-subrc = 0.
        lv_objnr = ls_aufk-objnr.
        lv_ktxt = ls_aufk-ktext.
      ENDIF.
*EOC by KMB CHG0149221 ENHC0025832 Add WBS logic in Power advocate report 19.6.2019
    ENDIF.

    IF lv_objnr IS NOT INITIAL.
      SELECT SINGLE objnr avorg konty aufnr ps_psp_pnr kostl anln1
        FROM cobrb
        INTO ls_cobrb
        WHERE objnr = lv_objnr AND
              avorg = lc_koao.
      IF sy-subrc = 0.
*BOC by kmb on 22.3.2019 for CHG0133164 power advocate field additions C11K935301
        CALL FUNCTION 'CONVERSION_EXIT_OBART_OUTPUT'
          EXPORTING
            input  = ls_cobrb-konty
          IMPORTING
            output = lv_konty.
*BOC by kmb on 22.3.2019 for CHG0133164 power advocate field additions C11K935301
        IF ls_cobrb-aufnr IS NOT INITIAL.
          lv_srcvr = ls_cobrb-aufnr.
*BOC by KMB on 26.04.2019 ENHC0025289 Power Advocate Additional fields
          SELECT SINGLE ktext
            FROM aufk
            INTO lv_ltxt
            WHERE aufnr = ls_cobrb-aufnr.
          IF sy-subrc = 0.
            "do nothing
          ENDIF.
*EOC by KMB on 26.04.2019 ENHC0025289 Power Advocate Additional fields
        ELSEIF ls_cobrb-ps_psp_pnr IS NOT INITIAL.
*BOC by KMB on 16.05.2019 ENHC0025289 Power Advocate Additional fields
*          lv_srcvr = ls_cobrb-ps_psp_pnr.
          CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
            EXPORTING
              input  = ls_cobrb-ps_psp_pnr
            IMPORTING
              output = lv_srcvr.
*EOC by KMB on 16.05.2019 ENHC0025289 Power Advocate Additional fields
*BOC by KMB on 26.04.2019 ENHC0025289 Power Advocate Additional fields
          SELECT SINGLE postu
            FROM prps
            INTO lv_ltxt
            WHERE pspnr = ls_cobrb-ps_psp_pnr.
          IF sy-subrc = 0.
            "do nothing
          ENDIF.
*EOC by KMB on 26.04.2019 ENHC0025289 Power Advocate Additional fields
        ELSEIF ls_cobrb-kostl IS NOT INITIAL.
          lv_srcvr = ls_cobrb-kostl.
*BOC by KMB on 26.04.2019 ENHC0025289 Power Advocate Additional fields
          SELECT ltext
            FROM cskt
            INTO lv_ltxt
            UP TO 1 ROWS
            WHERE kostl = ls_cobrb-kostl AND
                  datbi >= sy-datum AND
                  spras = sy-langu.
          ENDSELECT.
          IF sy-subrc = 0.
            "do nothing
          ENDIF.
*EOC by KMB on 26.04.2019 ENHC0025289 Power Advocate Additional fields
        ELSEIF ls_cobrb-anln1 IS NOT INITIAL.
          lv_srcvr = ls_cobrb-anln1.
*BOC by KMB on 26.04.2019 ENHC0025289 Power Advocate Additional fields
          SELECT txt50
            FROM anla
            INTO lv_ltxt
            UP TO 1 ROWS
            WHERE anln1 = ls_cobrb-anln1.
          ENDSELECT.
          IF sy-subrc = 0.
            "Do nothing
          ENDIF.
*EOC by KMB on 26.04.2019 ENHC0025289 Power Advocate Additional fields
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields


  IF   ( ( lv_ort01                      IS INITIAL     ) AND
         ( gwa_ekpo-werks                IS NOT INITIAL )     ).

    CLEAR                                   lwa_t001w.
    READ   TABLE git_t001w             INTO lwa_t001w
                                   WITH KEY werks = gwa_ekpo-werks
                              BINARY SEARCH.
    IF     ( sy-subrc EQ 0 ).
      CLEAR                                 lv_ort01.
      MOVE       lwa_t001w-ort01         TO lv_ort01.
    ELSE.
      CLEAR      lwa_t001w.
    ENDIF.

  ENDIF.

  IF     ( lv_ort01                      IS INITIAL ).
    CLEAR                                   lv_ort01.
    MOVE   'XXXX'                        TO lv_ort01.
  ENDIF.

*eject
  CLEAR                                     lwa_po_data.
  MOVE     iv_ebeln                      TO lwa_po_data-ebeln.
  MOVE     iv_ebelp                      TO lwa_po_data-ebelp.
  MOVE     gwa_ekko-bstyp                TO lwa_po_data-bstyp.
  MOVE     gwa_ekko-bsart                TO lwa_po_data-bsart.
  MOVE     gwa_ekpo-txz01                TO lwa_po_data-txz01.
  MOVE     gwa_ekpo-matnr                TO lwa_po_data-matnr.
  MOVE     gwa_ekpo-werks                TO lwa_po_data-werks.
  MOVE     gwa_ekpo-matkl                TO lwa_po_data-matkl.
  MOVE     gwa_ekpo-wepos                TO lwa_po_data-wepos.
  MOVE     gwa_ekpo-repos                TO lwa_po_data-repos.
  MOVE     gwa_ekpo-webre                TO lwa_po_data-webre.
  MOVE     gwa_ekpo-konnr                TO lwa_po_data-konnr.
  MOVE     lv_ort01                      TO lwa_po_data-ort01.
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
  MOVE     lv_ktxt                       TO lwa_po_data-ktext.
  MOVE     lv_konty                      TO lwa_po_data-konty.
  MOVE     lv_srcvr                      TO lwa_po_data-recvr.
  MOVE     lv_ktxt                       TO lwa_po_data-iddes.
  MOVE     gwa_ekko-ekgrp                TO lwa_po_data-ekgrp.
  MOVE     gwa_ekko-aedat                TO lwa_po_data-aedat.
  MOVE     gwa_ekpo-werks                TO lwa_po_data-plant.
  MOVE     gwa_ekpo-menge                TO lwa_po_data-menge.
  MOVE     lv_ounit                      TO lwa_po_data-meins.
  MOVE     gwa_ekpo-netpr                TO lwa_po_data-netpr.
  MOVE     gs_adrc-street                TO lwa_po_data-street.
  MOVE     gs_adrc-city1                 TO lwa_po_data-city1.
  MOVE     gs_adrc-region                TO lwa_po_data-region.
  MOVE     gs_adrc-post_code1            TO lwa_po_data-post_code1.
  MOVE     lv_eknam                      TO lwa_po_data-eknam.
  MOVE     lv_name1                      TO lwa_po_data-name1.
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
  MOVE     lv_ltxt                       TO lwa_po_data-set_desc.
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields

  INSERT                                    lwa_po_data
                                       INTO git_po_data
                                      INDEX lv_tabix.

  MOVE     lwa_po_data                   TO cwa_po_data.

ENDFORM.                    " f_get_po_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_goods_receipt_data
*&---------------------------------------------------------------------*
*       Get the goods receipt data
*----------------------------------------------------------------------*
FORM f_get_goods_receipt_data
  CHANGING cwa_goods_recpt             TYPE ty_wa_goods_recpt.

  DATA:    lwa_ekbe                    TYPE ty_wa_ekbe,
           lit_ekbe                    TYPE ty_it_ekbe.

  DATA:    lv_budat                    TYPE budat.

  CLEAR    lv_budat.

  IF   ( ( cwa_goods_recpt-ebeln         IS INITIAL ) OR
         ( cwa_goods_recpt-ebelp         IS INITIAL )    ).
    RETURN.
  ENDIF.

* Select the goods receipts
  CLEAR    lit_ekbe[].
  SELECT   ebeln  ebelp  zekkn  vgabe
           gjahr  belnr  buzei
           budat  menge  dmbtr  xblnr
    INTO   TABLE lit_ekbe
    FROM   ekbe
   WHERE   ebeln  = cwa_goods_recpt-ebeln
     AND   ebelp  = cwa_goods_recpt-ebelp
     AND   vgabe  = cwa_goods_recpt-vgabe
     AND   bwart IN grt_bwart.
  IF     ( sy-subrc EQ 0 ).
    SORT   lit_ekbe DESCENDING BY budat.
  ELSE.
    CLEAR  lit_ekbe[].
    RETURN.
  ENDIF.

*eject
* Read the specific goods receipt
  CLEAR                                lwa_ekbe.
  READ             TABLE lit_ekbe INTO lwa_ekbe
                              WITH KEY menge = cwa_goods_recpt-menge
                                       xblnr = cwa_goods_recpt-xblnr.
  IF     ( sy-subrc EQ 0 ).
    MOVE   lwa_ekbe-budat           TO lv_budat.
  ELSE.
    CLEAR                              lwa_ekbe.
    READ           TABLE lit_ekbe INTO lwa_ekbe
                              WITH KEY dmbtr = cwa_goods_recpt-dmbtr
                                       xblnr = cwa_goods_recpt-xblnr.
    IF     ( sy-subrc EQ 0 ).
      MOVE   lwa_ekbe-budat         TO lv_budat.
    ELSE.
      CLEAR                            lwa_ekbe.
      READ         TABLE lit_ekbe INTO lwa_ekbe
                              WITH KEY menge = cwa_goods_recpt-menge.
      IF     ( sy-subrc EQ 0 ).
        MOVE   lwa_ekbe-budat       TO lv_budat.
      ELSE.
        CLEAR                          lwa_ekbe.
        READ       TABLE lit_ekbe INTO lwa_ekbe
                              WITH KEY dmbtr = cwa_goods_recpt-dmbtr.
        IF     ( sy-subrc EQ 0 ).
          MOVE   lwa_ekbe-budat     TO lv_budat.
        ELSE.
          CLEAR                        lwa_ekbe.
          READ     TABLE lit_ekbe INTO lwa_ekbe INDEX 1.
          IF     ( sy-subrc EQ 0 ).
            MOVE   lwa_ekbe-budat   TO lv_budat.
          ELSE.
            CLEAR                      lv_budat.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR                                     cwa_goods_recpt-budat.
  MOVE     lv_budat                      TO cwa_goods_recpt-budat.

ENDFORM.                    " f_get_goods_receipt_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_cost_object
*&---------------------------------------------------------------------*
*       Set the cost object
*----------------------------------------------------------------------*
FORM f_set_cost_object
  USING    iwa_bseg                    TYPE ty_wa_bseg
  CHANGING cv_posid                    TYPE char24.

  DATA:    lv_posid                    TYPE char24,
           lv_nplnr                    TYPE aufnr,
           lv_vornr                    TYPE vornr.

  CLEAR    cv_posid.

  CLEAR    lv_posid.
  CLEAR    lv_nplnr.
  CLEAR    lv_vornr.

* Internal order
  IF     ( iwa_bseg-aufnr                IS NOT INITIAL ).

    MOVE     iwa_bseg-aufnr              TO lv_posid.

* WBS element
  ELSEIF ( iwa_bseg-projk                IS NOT INITIAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = iwa_bseg-projk
      IMPORTING
        output = lv_posid.

*   IF     ( sy-subrc NE 0 ).
*     CLEAR                                 lv_posid.
*     MOVE     'XXXX'                    TO lv_posid.
*   ENDIF.

*eject
* Network-activity
  ELSEIF ( iwa_bseg-aufpl                IS NOT INITIAL ) AND
         ( iwa_bseg-aplzl                IS NOT INITIAL ).

    CALL FUNCTION 'READ_NETWORK_NPLNR_VORNR'
      EXPORTING
        aplzl     = iwa_bseg-aplzl
        aufpl     = iwa_bseg-aufpl
      IMPORTING
        nplnr     = lv_nplnr
        vornr     = lv_vornr
      EXCEPTIONS
        not_found = 01.

    IF     ( sy-subrc NE 0 ).
      MOVE     'XXXX'                    TO lv_posid.
    ELSE.
      CONCATENATE                           lv_nplnr '_'
                                            lv_vornr
                                       INTO lv_posid.
    ENDIF.

* Network
  ELSEIF ( iwa_bseg-nplnr                IS NOT INITIAL ).

    MOVE     iwa_bseg-nplnr              TO lv_posid.

* Not found
  ELSE.

    MOVE     'XXXX'                      TO lv_posid.

  ENDIF.

  MOVE     lv_posid                      TO cv_posid.

ENDFORM.                    " f_set_cost_object
*eject
*&---------------------------------------------------------------------*
*&      Form  f_determine_amount
*&---------------------------------------------------------------------*
*       Determine the amount
*----------------------------------------------------------------------*
FORM f_determine_amount
  USING    iv_date                     TYPE sydatum
           iv_hwaer_s                  TYPE hwaer
           iv_waers                    TYPE waers
           iv_hwaer                    TYPE hwaer
           iv_wrbtr                    TYPE wrbtr
           iv_dmbtr                    TYPE dmbtr
  CHANGING cv_dmbtr                    TYPE dmbtr.

  DATA:    lv_dmbtr                    TYPE dmbtr.

  CLEAR    cv_dmbtr.

  CLEAR    lv_dmbtr.

  IF     ( iv_hwaer_s                    EQ iv_hwaer ).
    MOVE   iv_dmbtr                      TO lv_dmbtr.
  ELSEIF ( iv_hwaer_s                    EQ iv_waers ).
    MOVE   iv_wrbtr                      TO lv_dmbtr.
  ELSE.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        client           = sy-mandt
        date             = iv_date
        foreign_amount   = iv_dmbtr
        foreign_currency = iv_hwaer
        local_currency   = iv_hwaer_s
      IMPORTING
        local_amount     = lv_dmbtr
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.

    IF     ( sy-subrc NE 0 ).
      MOVE   iv_dmbtr                    TO lv_dmbtr.
    ENDIF.

  ENDIF.

  cv_dmbtr = lv_dmbtr.

ENDFORM.                    " f_determine_amount
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_headings
*&---------------------------------------------------------------------*
*       Format the output column headings
*----------------------------------------------------------------------*
FORM f_format_headings.

  DATA:    lwa_out_item                TYPE ty_wa_out_item,
           lwa_out_doc                 TYPE ty_wa_out_doc.

  DATA:    lv_rec1                     TYPE text2048,
           lv_rec3                     TYPE text1000.

  CLEAR                                     lwa_out_item.
  MOVE     text-401                      TO lwa_out_item-bukrs.     "01
  MOVE     text-402                      TO lwa_out_item-belnr.     "02
  MOVE     text-403                      TO lwa_out_item-lifnr.     "03
  MOVE     text-404                      TO lwa_out_item-ktokk.     "04
  MOVE     text-405                      TO lwa_out_item-name1.     "05
  MOVE     text-406                      TO lwa_out_item-land1.     "06
  MOVE     text-407                      TO lwa_out_item-stras.     "07
  MOVE     text-408                      TO lwa_out_item-ort01.     "08
  MOVE     text-409                      TO lwa_out_item-regio.     "09
  MOVE     text-410                      TO lwa_out_item-pstlz.     "10
  MOVE     text-411                      TO lwa_out_item-mindk.     "11
  MOVE     text-412                      TO lwa_out_item-budat.     "12
  MOVE     text-413                      TO lwa_out_item-dmbtr_total.
  MOVE     text-414                      TO lwa_out_item-augdt.     "14
  MOVE     text-415                      TO lwa_out_item-paymeth.   "15
  MOVE     text-416                      TO lwa_out_item-xblnr.     "16
  MOVE     text-417                      TO lwa_out_item-bldat.     "17
  MOVE     text-418                      TO lwa_out_item-saknr.     "18
  MOVE     text-419                      TO lwa_out_item-hkont.     "19
  MOVE     text-420                      TO lwa_out_item-txt20.     "20
  MOVE     text-421                      TO lwa_out_item-sgtxt.     "21
  MOVE     text-422                      TO lwa_out_item-dmbtr.     "22
  MOVE     text-423                      TO lwa_out_item-posid.     "23
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*  MOVE     text-514                      TO lwa_out_item-ktext. "Need this field at the end
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
  MOVE     text-424                      TO lwa_out_item-taxtext.   "24
  MOVE     text-425                      TO lwa_out_item-filler1.   "25
  MOVE     text-426                      TO lwa_out_item-port01.    "26
  MOVE     text-427                      TO lwa_out_item-pur_ord.   "27
  MOVE     text-428                      TO lwa_out_item-zterm.     "28
  MOVE     text-429                      TO lwa_out_item-chect.     "29
  MOVE     text-430                      TO lwa_out_item-waers.     "30
  MOVE     text-431                      TO lwa_out_item-datasrce.  "31
  MOVE     text-432                      TO lwa_out_item-ltext.     "32
  MOVE     text-433                      TO lwa_out_item-bktxt.     "33
  MOVE     text-434                      TO lwa_out_item-blart.     "34
  MOVE     text-435                      TO lwa_out_item-ebelp.     "35
  MOVE     text-436                      TO lwa_out_item-matkl.     "36
  MOVE     text-437                      TO lwa_out_item-matdesc.   "37
  MOVE     text-438                      TO lwa_out_item-uzawe.     "38
  MOVE     text-439                      TO lwa_out_item-konnr.     "39
  MOVE     text-440                      TO lwa_out_item-kostl.     "40

*eject
  MOVE     text-441                      TO lwa_out_item-erdat.     "41
  MOVE     text-442                      TO lwa_out_item-netdt.     "42
  MOVE     text-443                      TO lwa_out_item-sknto_total.
  MOVE     text-444                      TO lwa_out_item-approver.  "44
  MOVE     text-445                      TO lwa_out_item-doc_key.   "45
  MOVE     text-446                      TO lwa_out_item-bvorg.     "46
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
  MOVE     text-515                      TO lwa_out_item-konty.
  MOVE     text-516                      TO lwa_out_item-recvr.
  MOVE     text-517                      TO lwa_out_item-iddes.
  MOVE     text-518                      TO lwa_out_item-setl_amnt.
  MOVE     text-519                      TO lwa_out_item-ord_qty.
  MOVE     text-520                      TO lwa_out_item-ord_unit.
  MOVE     text-521                      TO lwa_out_item-net_prc.
  MOVE     text-522                      TO lwa_out_item-pgrp.
  MOVE     text-523                      TO lwa_out_item-pgrp_desc.
  MOVE     text-524                      TO lwa_out_item-plant_code.
  MOVE     text-525                      TO lwa_out_item-plant_code_desc.
  MOVE     text-526                      TO lwa_out_item-order_crea_dt.
  MOVE     text-527                      TO lwa_out_item-ship_adrc.
  MOVE     text-528                      TO lwa_out_item-ship_city.
  MOVE     text-529                      TO lwa_out_item-ship_state.
  MOVE     text-530                      TO lwa_out_item-ship_zip.
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
  MOVE     text-514                      TO lwa_out_item-ktext.
  MOVE     text-531                      TO lwa_out_item-set_desc.
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields


  ADD      1                             TO gv_cnt_rec_file1.

  CLEAR                                     lwa_out_doc.
  MOVE     text-402                      TO lwa_out_doc-belnr.      "01
  MOVE     text-434                      TO lwa_out_doc-blart.      "02
  MOVE     text-513                      TO lwa_out_doc-usnam.      "03
  MOVE     text-445                      TO lwa_out_doc-doc_key.    "04
  MOVE     text-446                      TO lwa_out_doc-bvorg.      "05

  ADD      1                             TO gv_cnt_rec_file3.

*eject
  IF     ( rb_appl                       IS INITIAL ).

    APPEND                                  lwa_out_item
                                         TO git_out_item.

    APPEND                                  lwa_out_doc
                                         TO git_out_doc.

  ELSE.

*BOC by KMB on 14.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-name1 WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-stras WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-matkl WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-matdesc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-iddes WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-pgrp_desc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-plant_code_desc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ship_adrc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-set_desc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ltext WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-bktxt WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-txt20 WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ltext WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ktext WITH gc_un.
*EOC by KMB on 14.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
*BOC by KMB on 22.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ship_city WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ship_state WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ship_zip WITH gc_un.
*EOC by KMB on 22.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report

    CLEAR                                   lv_rec1.
    CONCATENATE
               lwa_out_item-bukrs           lwa_out_item-belnr   "01-02
               lwa_out_item-lifnr           lwa_out_item-ktokk   "03-04
               lwa_out_item-name1           lwa_out_item-land1   "05-06
               lwa_out_item-stras           lwa_out_item-ort01   "07-08
               lwa_out_item-regio           lwa_out_item-pstlz   "09-10
               lwa_out_item-mindk           lwa_out_item-budat   "11-12
               lwa_out_item-dmbtr_total     lwa_out_item-augdt   "13-14
               lwa_out_item-paymeth         lwa_out_item-xblnr   "15-16
               lwa_out_item-bldat           lwa_out_item-saknr   "17-18
               lwa_out_item-hkont           lwa_out_item-txt20   "19-20
               lwa_out_item-sgtxt           lwa_out_item-dmbtr   "21-22
               lwa_out_item-posid
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*               lwa_out_item-ktext "Need this field at the end
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
               lwa_out_item-taxtext "23-24
               lwa_out_item-filler1         lwa_out_item-port01  "25-26
               lwa_out_item-pur_ord         lwa_out_item-zterm   "27-28
               lwa_out_item-chect           lwa_out_item-waers   "29-30
               lwa_out_item-datasrce        lwa_out_item-ltext   "31-32
               lwa_out_item-bktxt           lwa_out_item-blart   "33-34
               lwa_out_item-ebelp           lwa_out_item-matkl   "35-36
               lwa_out_item-matdesc         lwa_out_item-uzawe   "37-38
               lwa_out_item-konnr           lwa_out_item-kostl   "39-40
               lwa_out_item-erdat           lwa_out_item-netdt   "41-42
               lwa_out_item-sknto_total     lwa_out_item-approver"43-44
               lwa_out_item-doc_key         lwa_out_item-bvorg   "45-46
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
               lwa_out_item-konty
               lwa_out_item-recvr
               lwa_out_item-iddes
               lwa_out_item-setl_amnt
               lwa_out_item-ord_qty
               lwa_out_item-ord_unit
               lwa_out_item-net_prc
               lwa_out_item-pgrp
               lwa_out_item-pgrp_desc
               lwa_out_item-plant_code
               lwa_out_item-plant_code_desc
               lwa_out_item-order_crea_dt
               lwa_out_item-ship_adrc
               lwa_out_item-ship_city
               lwa_out_item-ship_state
               lwa_out_item-ship_zip
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
               lwa_out_item-ktext
               lwa_out_item-set_desc
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
                                       INTO lv_rec1
                               SEPARATED BY gv_delim.

    TRANSFER                                lv_rec1
                                         TO gv_phy_file1.

*BOC by KMB on 14.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_doc-blart WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_doc-doc_key WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_doc-bvorg WITH gc_un.
*EOC by KMB on 14.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate rep

    CLEAR                                   lv_rec3.
    CONCATENATE                             lwa_out_doc-belnr       "01
                                            lwa_out_doc-blart       "02
                                            lwa_out_doc-usnam       "03
                                            lwa_out_doc-doc_key     "04
                                            lwa_out_doc-bvorg       "05
                                       INTO lv_rec3
                               SEPARATED BY gv_delim.

    TRANSFER                                lv_rec3
                                         TO gv_phy_file3.

  ENDIF.

ENDFORM.                    " f_format_headings
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_output
*&---------------------------------------------------------------------*
*       Format the output
*----------------------------------------------------------------------*
FORM f_format_output
  TABLES   iit_int_item                TYPE ty_it_int_item.

  DATA:    lwa_int_item                TYPE ty_wa_int_item,
           lwa_out_item                TYPE ty_wa_out_item,
           lwa_out_po                  TYPE ty_wa_out_po,
           lwa_out_doc                 TYPE ty_wa_out_doc.

  DATA:    lv_bukrs_old                TYPE bukrs,
           lv_belnr_old                TYPE belnr_d,
           lv_pur_ord_old              TYPE char12,
           lv_ebelp_old                TYPE ebelp,
           lv_rec1                     TYPE text2048,
           lv_rec3                     TYPE text1000,
           lv_name1                    TYPE name1_gp,
           lv_stras                    TYPE stras_gp,
           lv_ort01                    TYPE ort01_gp,
           lv_regio                    TYPE regio,
           lv_mindk                    TYPE char34,
           lv_budat                    TYPE char10,
           lv_dmbtr_total              TYPE char16,
           lv_augdt                    TYPE char10,
           lv_xblnr                    TYPE char17,
           lv_bldat                    TYPE char10,
           lv_txt20                    TYPE txt20_skat,
           lv_sgtxt                    TYPE sgtxt,
           lv_dmbtr                    TYPE char16,
           lv_taxtext                  TYPE char50,
           lv_port01                   TYPE ort01,
           lv_ltext                    TYPE text_bslt,
           lv_bktxt                    TYPE bktxt,
           lv_blart                    TYPE char23,
           lv_matkl                    TYPE char30,
           lv_matdesc                  TYPE char50,
           lv_konnr                    TYPE konnr,
           lv_kostl                    TYPE char11,
           lv_erdat                    TYPE char10,
           lv_netdt                    TYPE char10,
           lv_sknto_total              TYPE char16,
           lv_bvorg                    TYPE char17,
           lv_grdat                    TYPE char10,
           lv_batxt                    TYPE char30,
           lv_wepos                    TYPE char7,
           lv_repos                    TYPE char7,
           lv_webre                    TYPE char12,
           lv_doc_key                  TYPE char20.

*eject
  CLEAR                                     lwa_int_item.
  LOOP AT  iit_int_item                INTO lwa_int_item.

    IF     ( ( cb_glicd                  IS NOT INITIAL ) AND
             ( lwa_int_item-hkont        IN s_glic      )     ).
      CLEAR    lwa_int_item.
      CONTINUE.
    ENDIF.

    CLEAR                                   lv_name1.
    MOVE   lwa_int_item-name1            TO lv_name1.
    REPLACE  ALL OCCURRENCES OF ','      IN lv_name1 WITH '_'.

    CLEAR                                   lv_stras.
    MOVE   lwa_int_item-stras            TO lv_stras.
    REPLACE  ALL OCCURRENCES OF ','      IN lv_stras WITH '_'.

    CLEAR                                   lv_ort01.
    MOVE   lwa_int_item-ort01            TO lv_ort01.
    REPLACE  ALL OCCURRENCES OF ','      IN lv_ort01 WITH '_'.

    CLEAR                                   lv_regio.
    MOVE   lwa_int_item-regio            TO lv_regio.
    REPLACE  ALL OCCURRENCES OF ','      IN lv_regio WITH '_'.

    CLEAR                                   lv_mindk.
    PERFORM  f_format_minority_ind USING    lwa_int_item-mindk
                                   CHANGING lv_mindk.

    CLEAR                                   lv_budat.
    PERFORM  f_format_date         USING    lwa_int_item-budat
                                   CHANGING lv_budat.

    CLEAR                                   lv_dmbtr_total.
    PERFORM  f_format_amount       USING    lwa_int_item-dmbtr_total
                                            lwa_int_item-waers
                                   CHANGING lv_dmbtr_total.

    CLEAR                                   lv_augdt.
    PERFORM  f_format_date         USING    lwa_int_item-augdt
                                   CHANGING lv_augdt.

    CLEAR                                   lv_xblnr.
    CONCATENATE                             lwa_int_item-xblnr '_'
                                       INTO lv_xblnr.
    REPLACE  ALL OCCURRENCES OF ','      IN lv_xblnr WITH '_'.

    CLEAR                                   lv_bldat.
    PERFORM  f_format_date         USING    lwa_int_item-bldat
                                   CHANGING lv_bldat.

    CLEAR                                   lv_txt20.
    PERFORM  f_format_gl_accnt_txt USING    lwa_int_item-hkont
                                   CHANGING lv_txt20.

*eject
    CLEAR                                   lv_sgtxt.
    PERFORM  f_format_item_text    USING    lwa_int_item-buzei
                                            lwa_int_item-koart
                                            lwa_int_item-sgtxt
                                   CHANGING lv_sgtxt.

    IF   ( ( lwa_int_item-voc_ind        EQ '3' ) AND
           ( lwa_int_item-koart          EQ 'K' )     ).
      CLEAR                          lwa_int_item-dmbtr.
    ENDIF.

    CLEAR                                   lv_dmbtr.
    PERFORM  f_format_amount       USING    lwa_int_item-dmbtr
                                            lwa_int_item-waers
                                   CHANGING lv_dmbtr.

    CLEAR                                   lv_taxtext.
    PERFORM  f_format_tax_text     USING    lwa_int_item-bukrs
                                            lwa_int_item-taxtext
                                   CHANGING lv_taxtext.

    CLEAR                                   lv_port01.
    MOVE   lwa_int_item-port01           TO lv_port01.
    REPLACE  ALL OCCURRENCES OF ','      IN lv_port01 WITH '_'.

    CLEAR                                   lv_ltext.
    PERFORM  f_format_post_key_txt USING    lwa_int_item-ltext
                                   CHANGING lv_ltext.

    CLEAR                                   lv_bktxt.
    PERFORM  f_format_header_text  USING    lwa_int_item-bktxt
                                   CHANGING lv_bktxt.

    CLEAR                                   lv_blart.
    PERFORM  f_format_doc_type_txt USING    lwa_int_item-blart
                                   CHANGING lv_blart.

    CLEAR                                   lv_matkl.
    PERFORM  f_format_mat_grp_txt  USING    lwa_int_item-matkl
                                   CHANGING lv_matkl.

    CLEAR                                   lv_matdesc.
    PERFORM  f_format_mat_desc     USING    lwa_int_item-matnr
                                            lwa_int_item-matdesc
                                   CHANGING lv_matdesc.

    CLEAR                                   lv_konnr.
    PERFORM  f_format_ola          USING    lwa_int_item-konnr
                                   CHANGING lv_konnr.

    CLEAR                                   lv_kostl.
    PERFORM  f_format_cost_center  USING    lwa_int_item-kostl
                                   CHANGING lv_kostl.

    CLEAR                                   lv_erdat.
    PERFORM  f_format_date         USING    lwa_int_item-erdat
                                   CHANGING lv_erdat.

*eject
    CLEAR                                   lv_netdt.
    PERFORM  f_format_date         USING    lwa_int_item-netdt
                                   CHANGING lv_netdt.

    CLEAR                                   lv_sknto_total.
    PERFORM  f_format_amount       USING    lwa_int_item-sknto_total
                                            lwa_int_item-waers
                                   CHANGING lv_sknto_total.

    CLEAR                                   lv_doc_key.
    PERFORM  f_format_doc_key      USING    lwa_int_item-bukrs
                                            lwa_int_item-belnr
                                            lwa_int_item-gjahr
                                   CHANGING lv_doc_key.

    CLEAR                                   lv_bvorg.
    PERFORM  f_format_x_comp_doc   USING    lwa_int_item-bvorg
                                   CHANGING lv_bvorg.

    CLEAR                                   lv_grdat.
    PERFORM  f_format_date         USING    lwa_int_item-grdat
                                   CHANGING lv_grdat.

    CLEAR                                   lv_batxt.
    PERFORM  f_format_po_type_text USING    lwa_int_item-bstyp
                                            lwa_int_item-bsart
                                   CHANGING lv_batxt.

    CLEAR                                   lv_wepos.
    CLEAR                                   lv_repos.
    CLEAR                                   lv_webre.
    PERFORM  f_format_gr_ir_ind    USING    lwa_int_item-wepos
                                            lwa_int_item-repos
                                            lwa_int_item-webre
                                   CHANGING lv_wepos
                                            lv_repos
                                            lv_webre.

    "BOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
    SELECT SINGLE value1
      INTO lv_fg
      FROM zfit_xparam
      WHERE paramtype = lc_param AND
            subtype = lc_doc AND
            key1 = lc_1 AND
            value1 = lwa_int_item-blart.
    IF sy-subrc = 0.
      CLEAR lv_sgtxt.
    ENDIF.
    "EOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
*eject
    CLEAR                                   lwa_out_item.
    MOVE   lwa_int_item-buk_s1           TO lwa_out_item-bukrs.     "01
    MOVE   lwa_int_item-bel_s1           TO lwa_out_item-belnr.     "02
    MOVE   lwa_int_item-lifnr            TO lwa_out_item-lifnr.     "03
    MOVE   lwa_int_item-ktokk            TO lwa_out_item-ktokk.     "04
    MOVE   lv_name1                      TO lwa_out_item-name1.     "05
    MOVE   lwa_int_item-land1            TO lwa_out_item-land1.     "06
    MOVE   lv_stras                      TO lwa_out_item-stras.     "07
    MOVE   lv_ort01                      TO lwa_out_item-ort01.     "08
    MOVE   lv_regio                      TO lwa_out_item-regio.     "09
    MOVE   lwa_int_item-pstlz            TO lwa_out_item-pstlz.     "10
    MOVE   lv_mindk                      TO lwa_out_item-mindk.     "11
    MOVE   lv_budat                      TO lwa_out_item-budat.     "12
    MOVE   lv_dmbtr_total                TO lwa_out_item-dmbtr_total.
    MOVE   lv_augdt                      TO lwa_out_item-augdt.     "14
    MOVE   lwa_int_item-paymeth          TO lwa_out_item-paymeth.   "15
    MOVE   lv_xblnr                      TO lwa_out_item-xblnr.     "16
    MOVE   lv_bldat                      TO lwa_out_item-bldat.     "17
    MOVE   lwa_int_item-saknr            TO lwa_out_item-saknr.     "18
    MOVE   lwa_int_item-hkont            TO lwa_out_item-hkont.     "19
    MOVE   lv_txt20                      TO lwa_out_item-txt20.     "20
    MOVE   lv_sgtxt                      TO lwa_out_item-sgtxt.     "21
    MOVE   lv_dmbtr                      TO lwa_out_item-dmbtr.     "22
    MOVE   lwa_int_item-posid            TO lwa_out_item-posid.     "23
    MOVE   lv_taxtext                    TO lwa_out_item-taxtext.   "24
    MOVE   lwa_int_item-filler1          TO lwa_out_item-filler1.   "25
    MOVE   lv_port01                     TO lwa_out_item-port01.    "26
    MOVE   lwa_int_item-pur_ord          TO lwa_out_item-pur_ord.   "27
    MOVE   lwa_int_item-zterm            TO lwa_out_item-zterm.     "28
    MOVE   lwa_int_item-chect            TO lwa_out_item-chect.     "29
    MOVE   lwa_int_item-waers            TO lwa_out_item-waers.     "30
    MOVE   lwa_int_item-datasrce         TO lwa_out_item-datasrce.  "31
    MOVE   lv_ltext                      TO lwa_out_item-ltext.     "32
    MOVE   lv_bktxt                      TO lwa_out_item-bktxt.     "33
    MOVE   lv_blart                      TO lwa_out_item-blart.     "34
    MOVE   lwa_int_item-ebelp            TO lwa_out_item-ebelp.     "35
    MOVE   lv_matkl                      TO lwa_out_item-matkl.     "36
    MOVE   lv_matdesc                    TO lwa_out_item-matdesc.   "37
    MOVE   lwa_int_item-uzawe            TO lwa_out_item-uzawe.     "38
    MOVE   lv_konnr                      TO lwa_out_item-konnr.     "39
    MOVE   lv_kostl                      TO lwa_out_item-kostl.     "40
    MOVE   lv_erdat                      TO lwa_out_item-erdat.     "41
    MOVE   lv_netdt                      TO lwa_out_item-netdt.     "42
    MOVE   lv_sknto_total                TO lwa_out_item-sknto_total.
    MOVE   lwa_int_item-approver         TO lwa_out_item-approver.  "44
    MOVE   lv_doc_key                    TO lwa_out_item-doc_key.   "45
    MOVE   lv_bvorg                      TO lwa_out_item-bvorg.     "46
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
    MOVE   lwa_int_item-ktext            TO lwa_out_item-ktext.
    MOVE   lwa_int_item-konty            TO lwa_out_item-konty.
    MOVE   lwa_int_item-recvr            TO lwa_out_item-recvr.
    MOVE   lwa_int_item-iddes            TO lwa_out_item-iddes.
    MOVE   lwa_int_item-setl_amnt        TO lwa_out_item-setl_amnt.
    MOVE   lwa_int_item-ord_qty          TO lwa_out_item-ord_qty.
    MOVE   lwa_int_item-ord_unit         TO lwa_out_item-ord_unit.
    MOVE   lwa_int_item-net_prc          TO lwa_out_item-net_prc.
    MOVE   lwa_int_item-pgrp             TO lwa_out_item-pgrp.
    MOVE   lwa_int_item-pgrp_desc        TO lwa_out_item-pgrp_desc.
    MOVE   lwa_int_item-plant_code       TO lwa_out_item-plant_code.
    MOVE   lwa_int_item-plant_code_desc  TO lwa_out_item-plant_code_desc.
    MOVE   lwa_int_item-order_crea_dt    TO lwa_out_item-order_crea_dt.
    MOVE   lwa_int_item-ship_adrc        TO lwa_out_item-ship_adrc.
    MOVE   lwa_int_item-ship_city        TO lwa_out_item-ship_city.
    MOVE   lwa_int_item-ship_state       TO lwa_out_item-ship_state.
    MOVE   lwa_int_item-ship_zip         TO lwa_out_item-ship_zip.
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
    MOVE   lwa_int_item-set_desc         TO lwa_out_item-set_desc.
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields

*eject
    CLEAR                                   lwa_out_po.
    MOVE   lwa_int_item-pur_ord          TO lwa_out_po-pur_ord.     "01
    MOVE   lv_grdat                      TO lwa_out_po-grdat.       "02
    MOVE   lwa_int_item-ebelp            TO lwa_out_po-ebelp.       "03
    MOVE   lv_matkl                      TO lwa_out_po-matkl.       "04
    MOVE   lv_batxt                      TO lwa_out_po-batxt.       "05
    MOVE   lv_matdesc                    TO lwa_out_po-matdesc.     "06
    MOVE   lv_wepos                      TO lwa_out_po-wepos.       "07
    MOVE   lv_repos                      TO lwa_out_po-repos.       "08
    MOVE   lv_webre                      TO lwa_out_po-webre.       "09

    CLEAR                                   lwa_out_doc.
    MOVE   lwa_int_item-belnr            TO lwa_out_doc-belnr.      "01
    MOVE   lv_blart                      TO lwa_out_doc-blart.      "02
    MOVE   lwa_int_item-usnam            TO lwa_out_doc-usnam.      "03
    MOVE   lv_doc_key                    TO lwa_out_doc-doc_key.    "04
    MOVE   lv_bvorg                      TO lwa_out_doc-bvorg.      "05

*eject
* Output the item data
    IF   ( ( ( lwa_out_item-bukrs        IS NOT INITIAL ) AND
             ( lwa_int_item-voc_ind      EQ '1'         ) AND
             ( lwa_int_item-koart        NE 'K'         )     ) OR
           ( ( lwa_out_item-bukrs        IS NOT INITIAL ) AND
             ( lwa_int_item-voc_ind      EQ '2'         ) AND
             ( lwa_int_item-koart        EQ 'K'         )     ) OR
           ( ( lwa_out_item-bukrs        IS NOT INITIAL ) AND
             ( lwa_int_item-voc_ind      EQ '3'         )     )    ).

      ADD      1                         TO gv_cnt_rec_file1.

      IF     ( rb_appl                   IS INITIAL ).

        APPEND                              lwa_out_item
                                         TO git_out_item.

      ELSE.

*BOC by KMB on 26.9.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-name1 WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-stras WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-matkl WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-matdesc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-iddes WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-pgrp_desc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-plant_code_desc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ship_adrc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-set_desc WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ltext WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-bktxt WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-txt20 WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ltext WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ktext WITH gc_un.
*EOC by KMB on 26.9.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
*BOC by KMB on 22.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ship_city WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ship_state WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_item-ship_zip WITH gc_un.
*EOC by KMB on 22.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report

        CLEAR                               lv_rec1.
        CONCATENATE
               lwa_out_item-bukrs           lwa_out_item-belnr   "01-02
               lwa_out_item-lifnr           lwa_out_item-ktokk   "03-04
               lwa_out_item-name1           lwa_out_item-land1   "05-06
               lwa_out_item-stras           lwa_out_item-ort01   "07-08
               lwa_out_item-regio           lwa_out_item-pstlz   "09-10
               lwa_out_item-mindk           lwa_out_item-budat   "11-12
               lwa_out_item-dmbtr_total     lwa_out_item-augdt   "13-14
               lwa_out_item-paymeth         lwa_out_item-xblnr   "15-16
               lwa_out_item-bldat           lwa_out_item-saknr   "17-18
               lwa_out_item-hkont           lwa_out_item-txt20   "19-20
               lwa_out_item-sgtxt           lwa_out_item-dmbtr   "21-22
               lwa_out_item-posid
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*               lwa_out_item-ktext "Need this field at the end
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
               lwa_out_item-taxtext "23-24
               lwa_out_item-filler1         lwa_out_item-port01  "25-26
               lwa_out_item-pur_ord         lwa_out_item-zterm   "27-28
               lwa_out_item-chect           lwa_out_item-waers   "29-30
               lwa_out_item-datasrce        lwa_out_item-ltext   "31-32
               lwa_out_item-bktxt           lwa_out_item-blart   "33-34
               lwa_out_item-ebelp           lwa_out_item-matkl   "35-36
               lwa_out_item-matdesc         lwa_out_item-uzawe   "37-38
               lwa_out_item-konnr           lwa_out_item-kostl   "39-40
               lwa_out_item-erdat           lwa_out_item-netdt   "41-42
               lwa_out_item-sknto_total     lwa_out_item-approver"43-44
               lwa_out_item-doc_key         lwa_out_item-bvorg   "45-46
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
               lwa_out_item-konty
               lwa_out_item-recvr
               lwa_out_item-iddes
               lwa_out_item-setl_amnt
               lwa_out_item-ord_qty
               lwa_out_item-ord_unit
               lwa_out_item-net_prc
               lwa_out_item-pgrp
               lwa_out_item-pgrp_desc
               lwa_out_item-plant_code
               lwa_out_item-plant_code_desc
               lwa_out_item-order_crea_dt
               lwa_out_item-ship_adrc
               lwa_out_item-ship_city
               lwa_out_item-ship_state
               lwa_out_item-ship_zip
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
               lwa_out_item-ktext
               lwa_out_item-set_desc
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
                                       INTO lv_rec1
                               SEPARATED BY gv_delim.

        TRANSFER                            lv_rec1
                                         TO gv_phy_file1.

      ENDIF.

    ENDIF.

*eject
* Output the PO document data
    IF     ( ( lwa_int_item-pur_ord      IS INITIAL        ) OR
             ( lwa_int_item-bstyp        IS INITIAL        ) OR
             ( lwa_int_item-bsart        IS INITIAL        )     ).
    ELSEIF ( ( lwa_int_item-pur_ord      EQ lv_pur_ord_old ) AND
             ( lwa_int_item-ebelp        EQ lv_ebelp_old   )     ).
    ELSE.
      CLEAR                                 lv_pur_ord_old.
      MOVE     lwa_int_item-pur_ord      TO lv_pur_ord_old.
      CLEAR                                 lv_ebelp_old.
      MOVE     lwa_int_item-ebelp        TO lv_ebelp_old.

      APPEND                                lwa_out_po
                                         TO git_out_po.

    ENDIF.

* Output the accounting document data
    IF       ( lwa_int_item-belnr        IS INITIAL      ).
    ELSEIF ( ( lwa_int_item-bukrs        EQ lv_bukrs_old ) AND
             ( lwa_int_item-belnr        EQ lv_belnr_old )     ).
    ELSE.
      CLEAR                                 lv_bukrs_old.
      MOVE     lwa_int_item-bukrs        TO lv_bukrs_old.
      CLEAR                                 lv_belnr_old.
      MOVE     lwa_int_item-belnr        TO lv_belnr_old.

      ADD      1                         TO gv_cnt_rec_file3.

      IF     ( rb_appl                   IS INITIAL ).

        APPEND                              lwa_out_doc
                                         TO git_out_doc.

      ELSE.

*BOC by KMB on 14.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_doc-blart WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_doc-doc_key WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_doc-bvorg WITH gc_un.
*EOC by KMB on 14.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate rep

        CLEAR                               lv_rec3.
        CONCATENATE                         lwa_out_doc-belnr       "01
                                            lwa_out_doc-blart       "02
                                            lwa_out_doc-usnam       "03
                                            lwa_out_doc-doc_key     "04
                                            lwa_out_doc-bvorg       "05
                                       INTO lv_rec3
                               SEPARATED BY gv_delim.

        TRANSFER                            lv_rec3
                                         TO gv_phy_file3.

      ENDIF.

    ENDIF.

    CLEAR  lwa_int_item.
  ENDLOOP.

ENDFORM.                    " f_format_output
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_minority_ind
*&---------------------------------------------------------------------*
*       Format the minority indicator
*----------------------------------------------------------------------*
FORM f_format_minority_ind
  USING    iv_mindk                    TYPE mindk
  CHANGING cv_mindk                    TYPE char34.

  DATA:    lv_mindk                    TYPE char34.

  DATA:    lwa_t059t                   TYPE ty_wa_t059t.

  CLEAR    cv_mindk.

  IF     ( iv_mindk                      IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lv_mindk.

  CLEAR                                     lwa_t059t.
  READ     TABLE git_t059t             INTO lwa_t059t
                                    WITH KEY mindk = iv_mindk.
  IF     ( sy-subrc EQ 0 ).
    CONCATENATE                             iv_mindk '_'
                                            lwa_t059t-mtext
                                       INTO lv_mindk.
  ELSE.
    CONCATENATE                             iv_mindk '_'
                                            'XXXX'
                                       INTO lv_mindk.
  ENDIF.

  CLEAR                                     cv_mindk.
  MOVE     lv_mindk                      TO cv_mindk.

ENDFORM.                    " f_format_minority_ind
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_date
*&---------------------------------------------------------------------*
*       Format the date
*----------------------------------------------------------------------*
FORM f_format_date
  USING    iv_date                     TYPE sydatum
  CHANGING cv_date                     TYPE char10.

  DATA:    lv_date                     TYPE char10.

  CLEAR    cv_date.

  CLEAR    lv_date.

  IF   ( ( iv_date                       EQ '        ' ) OR
         ( iv_date                       EQ '00000000' ) OR
         ( iv_date                       EQ '99991231' )    ).
    MOVE   'XX/XX/XXXX'                  TO cv_date.
    RETURN.
  ENDIF.

  MOVE     'mm/dd/yyyy'                  TO lv_date.
  MOVE     iv_date+4(2)                  TO lv_date+0(2).
  MOVE     iv_date+6(2)                  TO lv_date+3(2).
  MOVE     iv_date+0(4)                  TO lv_date+6(4).

  MOVE    lv_date                        TO cv_date.

ENDFORM.                    " f_format_date
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_amount
*&---------------------------------------------------------------------*
*       Format the amount
*----------------------------------------------------------------------*
FORM f_format_amount
  USING    iv_amount                   TYPE dmbtr
           iv_waers                    TYPE waers
  CHANGING cv_amount                   TYPE char16.

  DATA:    lv_amount                   TYPE char16.

  CLEAR    cv_amount.

  CLEAR    lv_amount.

  IF     ( iv_waers                      IS INITIAL ).
    MOVE   '#####.##'                    TO cv_amount.
    RETURN.
  ENDIF.

  WRITE    iv_amount                     TO lv_amount
                                   CURRENCY iv_waers.

  CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
    CHANGING
      value = lv_amount.

  TRANSLATE                                 lv_amount USING ', '.
  CONDENSE                                  lv_amount NO-GAPS.

  MOVE     lv_amount                     TO cv_amount.

ENDFORM.                    " f_format_amount
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_gl_accnt_txt
*&---------------------------------------------------------------------*
*       Format the G/L account text
*----------------------------------------------------------------------*
FORM f_format_gl_accnt_txt
  USING    iv_saknr                    TYPE saknr
  CHANGING cv_txt20                    TYPE txt20_skat.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix.

  CLEAR    cv_txt20.

  IF     ( iv_saknr                      IS INITIAL ).
    MOVE   'XXXX'                        TO cv_txt20.
    RETURN.
  ENDIF.

  IF     ( gwa_skat-saknr                EQ iv_saknr ).
    MOVE   gwa_skat-txt20                TO cv_txt20.
    RETURN.
  ENDIF.

  CLEAR                                     gwa_skat.
  READ     TABLE git_skat              INTO gwa_skat
                                   WITH KEY saknr = iv_saknr
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.

  IF     ( lv_subrc EQ 0 ).
    MOVE   gwa_skat-txt20                TO cv_txt20.
    RETURN.
  ENDIF.

*eject
  CLEAR    gwa_skat.
  SELECT   SINGLE saknr  txt20
    INTO   gwa_skat
    FROM   skat
   WHERE   spras = 'E'
     AND   ktopl = p_ktopl
     AND   saknr = iv_saknr.
  IF     ( sy-subrc NE 0 ).
    CLEAR                                   gwa_skat.
    MOVE   iv_saknr                      TO gwa_skat-saknr.
  ENDIF.

  IF     ( gwa_skat-txt20                IS INITIAL ).
    CLEAR                                   gwa_skat-txt20.
    MOVE   'XXXX'                        TO gwa_skat-txt20.
  ELSE.
    REPLACE  ALL OCCURRENCES OF ','      IN gwa_skat-txt20 WITH '_'.
  ENDIF.

  INSERT                                    gwa_skat
                                       INTO git_skat
                                      INDEX lv_tabix.

  MOVE     gwa_skat-txt20                TO cv_txt20.

ENDFORM.                    " f_format_gl_accnt_txt
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_item_text
*&---------------------------------------------------------------------*
*       Format the item text
*----------------------------------------------------------------------*
FORM f_format_item_text
  USING    iv_buzei                    TYPE buzei
           iv_koart                    TYPE koart
           iv_sgtxt                    TYPE sgtxt
  CHANGING cv_sgtxt                    TYPE sgtxt.

  DATA:    lv_sgtxt                    TYPE sgtxt,
           lv_sgtxt_string             TYPE string.

  CLEAR    cv_sgtxt.

  CLEAR    lv_sgtxt.
  CLEAR    lv_sgtxt_string.

  IF     ( iv_koart                      EQ 'K' ).
    IF   ( iv_sgtxt                      IS INITIAL ).
      CONCATENATE                           iv_buzei '_'
                                            text-181 '_'
                                            'XXXX'
                                       INTO lv_sgtxt.
    ELSE.
      CONCATENATE                           iv_buzei '_'
                                            text-181 '_'
                                            iv_sgtxt
                                       INTO lv_sgtxt.
    ENDIF.
  ELSE.
    IF   ( iv_sgtxt                      IS INITIAL ).
      CONCATENATE                           iv_buzei '_'
                                            'XXXX'
                                       INTO lv_sgtxt.
    ELSE.
      CONCATENATE                           iv_buzei '_'
                                            iv_sgtxt
                                       INTO lv_sgtxt.
    ENDIF.
  ENDIF.

  MOVE     lv_sgtxt                      TO lv_sgtxt_string.
  PERFORM  f_string_remove_spcl_char
                                   CHANGING lv_sgtxt_string.

  CLEAR                                     lv_sgtxt.
  MOVE     lv_sgtxt_string               TO lv_sgtxt.

  REPLACE  ALL OCCURRENCES OF ','        IN lv_sgtxt WITH '_'.
  REPLACE  ALL OCCURRENCES OF '"'        IN lv_sgtxt WITH '_'.

  MOVE     lv_sgtxt                      TO cv_sgtxt.

ENDFORM.                    " f_format_item_text
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_tax_text
*&---------------------------------------------------------------------*
*       Format the tax text
*----------------------------------------------------------------------*
FORM f_format_tax_text
  USING    iv_bukrs                    TYPE bukrs
           iv_taxtext                  TYPE char50
  CHANGING cv_taxtext                  TYPE char50.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_taxtext                  TYPE char50,
           lv_mwskz                    TYPE mwskz.

  CLEAR    cv_taxtext.

  CLEAR    lv_taxtext.
  CLEAR    lv_mwskz.

  IF     ( iv_taxtext                    IS INITIAL ).
    RETURN.
  ENDIF.

  MOVE     iv_taxtext                    TO lv_mwskz.

  IF     ( gwa_t001-bukrs                EQ iv_bukrs ).
  ELSE.

    CLEAR                                   gwa_t001.
    READ   TABLE git_t001              INTO gwa_t001
                                   WITH KEY bukrs = iv_bukrs
                              BINARY SEARCH.
    IF   ( sy-subrc NE 0 ).
      CLEAR      gwa_t001.
    ENDIF.

  ENDIF.

  IF     ( gwa_t001-kalsm                IS INITIAL ).
    CLEAR                                   lv_taxtext.
    CONCATENATE                             lv_mwskz '_'
                                            'XXXX'
                                       INTO lv_taxtext.
    MOVE   lv_taxtext                    TO cv_taxtext.
    RETURN.
  ENDIF.

*eject
  IF   ( ( gwa_t007s-kalsm               EQ gwa_t001-kalsm ) AND
         ( gwa_t007s-mwskz               EQ lv_mwskz       )     ).
  ELSE.

    CLEAR                                   gwa_t007s.
    READ   TABLE git_t007s             INTO gwa_t007s
                                   WITH KEY kalsm = gwa_t001-kalsm
                                            mwskz = lv_mwskz
                              BINARY SEARCH.
    lv_subrc = sy-subrc.
    lv_tabix = sy-tabix.

    IF     ( lv_subrc NE 0 ).

      CLEAR    gwa_t007s.
      SELECT   SINGLE kalsm  mwskz  text1
        INTO   gwa_t007s
        FROM   t007s
       WHERE   spras = 'E'
         AND   kalsm = gwa_t001-kalsm
         AND   mwskz = lv_mwskz.
      IF     ( sy-subrc NE 0 ).
        CLEAR                               gwa_t007s.
        MOVE   gwa_t001-kalsm            TO gwa_t007s-kalsm.
        MOVE   lv_mwskz                  TO gwa_t007s-mwskz.
      ENDIF.

      IF     ( gwa_t007s-text1           IS INITIAL ).
        CLEAR                               gwa_t007s-text1.
        MOVE   'XXXX'                    TO gwa_t007s-text1.
      ELSE.
        REPLACE  ALL OCCURRENCES OF ','  IN gwa_t007s-text1 WITH '_'.
      ENDIF.

      INSERT                                gwa_t007s
                                       INTO git_t007s
                                      INDEX lv_tabix.

    ENDIF.

  ENDIF.

  CLEAR                                     lv_taxtext.
  CONCATENATE                               lv_mwskz '_'
                                            gwa_t007s-text1
                                       INTO lv_taxtext.

  MOVE     lv_taxtext                    TO cv_taxtext.

ENDFORM.                    " f_format_tax_text
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_post_key_txt
*&---------------------------------------------------------------------*
*       Format the posting key text
*----------------------------------------------------------------------*
FORM f_format_post_key_txt
  USING    iv_ltext                    TYPE text_bslt
  CHANGING cv_ltext                    TYPE text_bslt.

  DATA:    lv_bschl                    TYPE bschl.

  CLEAR    cv_ltext.

  CLEAR    lv_bschl.

  IF     ( iv_ltext                      IS INITIAL ).
    RETURN.
  ENDIF.

  MOVE     iv_ltext                      TO lv_bschl.

  IF     ( gwa_tbsl-bschl                EQ lv_bschl ).
  ELSE.

    CLEAR                                   gwa_tbsl.
    READ   TABLE git_tbsl              INTO gwa_tbsl
                                   WITH KEY bschl = lv_bschl
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR      gwa_tbsl.
    ENDIF.

  ENDIF.

  MOVE     gwa_tbsl-ltext                TO cv_ltext.

ENDFORM.                    " f_format_post_key_txt
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_header_text
*&---------------------------------------------------------------------*
*       Format the header text
*----------------------------------------------------------------------*
FORM f_format_header_text
  USING    iv_bktxt                    TYPE bktxt
  CHANGING cv_bktxt                    TYPE bktxt.

  DATA:    lv_bktxt                    TYPE bktxt,
           lv_bktxt_string             TYPE string.

  CLEAR    cv_bktxt.

  CLEAR    lv_bktxt.
  CLEAR    lv_bktxt_string.

  IF     ( iv_bktxt                      IS INITIAL ).
    MOVE   'XXXX'                        TO cv_bktxt.
    RETURN.
  ENDIF.

  MOVE     iv_bktxt                      TO lv_bktxt_string.
  PERFORM  f_string_remove_spcl_char
                                   CHANGING lv_bktxt_string.

  CLEAR                                     lv_bktxt.
  MOVE     lv_bktxt_string               TO lv_bktxt.

  REPLACE  ALL OCCURRENCES OF ','        IN lv_bktxt WITH '_'.
  REPLACE  ALL OCCURRENCES OF '"'        IN lv_bktxt WITH '_'.

  MOVE     lv_bktxt                      TO cv_bktxt.

ENDFORM.                    " f_format_header_text
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_doc_type_txt
*&---------------------------------------------------------------------*
*       Format the document type text
*----------------------------------------------------------------------*
FORM f_format_doc_type_txt
  USING    iv_blart                    TYPE blart
  CHANGING cv_blart                    TYPE char23.

  DATA:    lv_blart                    TYPE char23.

  CLEAR    cv_blart.

  CLEAR    lv_blart.

  IF     ( iv_blart                      IS INITIAL ).
    MOVE   'XXXX'                        TO cv_blart.
    RETURN.
  ENDIF.

  IF     ( gwa_t003-blart                EQ iv_blart ).
  ELSE.

    CLEAR                                   gwa_t003.
    READ   TABLE git_t003              INTO gwa_t003
                                   WITH KEY blart = iv_blart
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR      gwa_t003.
    ENDIF.

  ENDIF.

  IF     ( gwa_t003-ltext                IS INITIAL ).
    CONCATENATE                             iv_blart '_'
                                            'XXXX'
                                       INTO lv_blart.
  ELSE.
    CONCATENATE                             iv_blart '_'
                                            gwa_t003-ltext
                                       INTO lv_blart.
  ENDIF.

  MOVE     lv_blart                      TO cv_blart.

ENDFORM.                    " f_format_doc_type_txt
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_mat_grp_txt
*&---------------------------------------------------------------------*
*       Format the material group text
*----------------------------------------------------------------------*
FORM f_format_mat_grp_txt
  USING    iv_matkl                    TYPE matkl
  CHANGING cv_matkl                    TYPE char30.

  DATA:    lv_matkl                    TYPE char30.

  CLEAR    cv_matkl.

  CLEAR    lv_matkl.

  IF     ( iv_matkl                      IS INITIAL ).
    MOVE   'XXXX'                        TO cv_matkl.
    RETURN.
  ENDIF.

  IF     ( gwa_t023t-matkl               EQ iv_matkl ).
  ELSE.

    CLEAR                                   gwa_t023t.
    READ   TABLE git_t023t             INTO gwa_t023t
                                   WITH KEY matkl = iv_matkl
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR      gwa_t023t.
    ENDIF.

  ENDIF.

  IF     ( gwa_t023t-wgbez               IS INITIAL ).
    CONCATENATE                             iv_matkl '_'
                                            'XXXX'
                                       INTO lv_matkl.
  ELSE.
    CONCATENATE                             iv_matkl '_'
                                            gwa_t023t-wgbez
                                       INTO lv_matkl.
  ENDIF.

  MOVE     lv_matkl                      TO cv_matkl.

ENDFORM.                    " f_format_mat_grp_txt
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_mat_desc
*&---------------------------------------------------------------------*
*       Format the material description
*----------------------------------------------------------------------*
FORM f_format_mat_desc
  USING    iv_matnr                    TYPE matnr
           iv_matdesc                  TYPE char50
  CHANGING cv_matdesc                  TYPE char50.

  DATA:    lv_matnr                    TYPE matnr,
           lv_matdesc                  TYPE char50,
           lv_matdesc_string           TYPE string.

  CLEAR    cv_matdesc.

  CLEAR    lv_matnr.
  CLEAR    lv_matdesc.
  CLEAR    lv_matdesc_string.

  IF   ( ( iv_matnr                      IS INITIAL ) AND
         ( iv_matdesc                    IS INITIAL )     ).
    MOVE     'XXXX_XXXX'                 TO cv_matdesc.
    RETURN.
  ENDIF.

  IF     ( iv_matnr                      IS INITIAL ).
    MOVE     'XXXX'                      TO lv_matnr.
  ELSE.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = iv_matnr
      IMPORTING
        output = lv_matnr.

  ENDIF.

  IF     ( iv_matdesc                    IS INITIAL ).
    MOVE     'XXXX'                      TO lv_matdesc.
  ELSE.
    MOVE     iv_matdesc                  TO lv_matdesc.
  ENDIF.

  CONCATENATE                               lv_matnr '_'
                                            lv_matdesc
                                       INTO lv_matdesc.

*eject
  MOVE     lv_matdesc                    TO lv_matdesc_string.
  PERFORM  f_string_remove_spcl_char
                                   CHANGING lv_matdesc_string.

  CLEAR                                     lv_matdesc.
  MOVE     lv_matdesc_string             TO lv_matdesc.

  REPLACE  ALL OCCURRENCES OF ','        IN lv_matdesc WITH '_'.
  REPLACE  ALL OCCURRENCES OF '"'        IN lv_matdesc WITH '_'.

  MOVE     lv_matdesc                    TO cv_matdesc.

ENDFORM.                    " f_format_mat_desc
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_ola
*&---------------------------------------------------------------------*
*       Format the OLA purchasing agreement
*----------------------------------------------------------------------*
FORM f_format_ola
  USING    iv_konnr                    TYPE konnr
  CHANGING cv_konnr                    TYPE konnr.

  DATA:    lv_konnr                    TYPE konnr.

  CLEAR    cv_konnr.

  CLEAR    lv_konnr.

  IF     ( iv_konnr                      IS INITIAL ).
    MOVE     'XXXX'                      TO cv_konnr.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = iv_konnr
    IMPORTING
      output = lv_konnr.

  MOVE     lv_konnr                      TO cv_konnr.

ENDFORM.                    " f_format_ola
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_cost_center
*&---------------------------------------------------------------------*
*       Format the cost center
*----------------------------------------------------------------------*
FORM f_format_cost_center
  USING    iv_kostl                    TYPE kostl
  CHANGING cv_kostl                    TYPE char11.

  DATA:    lv_kostl                    TYPE kostl.

  CLEAR    cv_kostl.

  CLEAR    lv_kostl.

  IF     ( iv_kostl                      IS INITIAL ).
    MOVE     'XXXX'                      TO cv_kostl.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = iv_kostl
    IMPORTING
      output = lv_kostl.

  MOVE     lv_kostl                      TO cv_kostl.

ENDFORM.                    " f_format_cost_center
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_x_comp_doc
*&---------------------------------------------------------------------*
*       Format the cross company (intercompany) document number
*----------------------------------------------------------------------*
FORM f_format_x_comp_doc
  USING    iv_bvorg                    TYPE bvorg
  CHANGING cv_bvorg                    TYPE char17.

  DATA:    lv_bvorg                    TYPE char17.

  CLEAR    cv_bvorg.

  CLEAR    lv_bvorg.

  IF     ( iv_bvorg                      IS INITIAL ).
    RETURN.
  ENDIF.

  CONCATENATE                               iv_bvorg '_'
                                       INTO lv_bvorg.

  MOVE     lv_bvorg                      TO cv_bvorg.

ENDFORM.                    " f_format_x_comp_doc
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_po_type_text
*&---------------------------------------------------------------------*
*       Format the PO type text
*----------------------------------------------------------------------*
FORM f_format_po_type_text
  USING    iv_bstyp                    TYPE ebstyp
           iv_bsart                    TYPE esart
  CHANGING cv_batxt                    TYPE char30.

  DATA:    lv_batxt                    TYPE char30.

  CLEAR    cv_batxt.

  CLEAR    lv_batxt.

  IF   ( ( iv_bstyp                      IS INITIAL ) OR
         ( iv_bsart                      IS INITIAL )    ).
    MOVE   'XXXX_XXXX'                   TO cv_batxt.
    RETURN.
  ENDIF.

  IF   ( ( gwa_t161t-bstyp               EQ iv_bstyp ) AND
         ( gwa_t161t-bsart               EQ iv_bsart )     ).
  ELSE.

    CLEAR                                   gwa_t161t.
    READ   TABLE git_t161t             INTO gwa_t161t
                                   WITH KEY bstyp = iv_bstyp
                                            bsart = iv_bsart
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR      gwa_t161t.
      MOVE       'XXXX_XXXX'             TO cv_batxt.
      RETURN.
    ENDIF.

  ENDIF.

  IF     ( gwa_t161t-batxt               IS INITIAL ).
    CONCATENATE                             iv_bsart '_'
                                            'XXXX'
                                       INTO lv_batxt.
  ELSE.
    CONCATENATE                             iv_bsart '_'
                                            gwa_t161t-batxt
                                       INTO lv_batxt.
  ENDIF.

  MOVE     lv_batxt                      TO cv_batxt.

ENDFORM.                    " f_format_po_type_text
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_gr_ir_ind
*&---------------------------------------------------------------------*
*       Format the goods receipt and invoice receipt indicators
*----------------------------------------------------------------------*
FORM f_format_gr_ir_ind
  USING    iv_wepos                    TYPE wepos
           iv_repos                    TYPE repos
           iv_webre                    TYPE webre
  CHANGING cv_wepos                    TYPE char7
           cv_repos                    TYPE char7
           cv_webre                    TYPE char12.

  CLEAR    cv_wepos.
  CLEAR    cv_repos.
  CLEAR    cv_webre.

  IF     ( iv_wepos                      IS INITIAL ).
    MOVE   'No'                          TO cv_wepos.
  ELSE.
    MOVE   'Yes'                         TO cv_wepos.
  ENDIF.

  IF     ( iv_repos                      IS INITIAL ).
    MOVE   'No'                          TO cv_repos.
  ELSE.
    MOVE   'Yes'                         TO cv_repos.
  ENDIF.

  IF     ( iv_webre                      IS INITIAL ).
    MOVE   'No'                          TO cv_webre.
  ELSE.
    MOVE   'Yes'                         TO cv_webre.
  ENDIF.

ENDFORM.                    " f_format_gr_ir_ind
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_doc_key
*&---------------------------------------------------------------------*
*       Format the document key
*----------------------------------------------------------------------*
FORM f_format_doc_key
  USING    iv_bukrs                    TYPE bukrs
           iv_belnr                    TYPE belnr_d
           iv_gjahr                    TYPE gjahr
  CHANGING cv_doc_key                  TYPE char20.

  CLEAR    cv_doc_key.

  CONCATENATE                               iv_bukrs '_'
                                            iv_belnr '_'
                                            iv_gjahr
                                       INTO cv_doc_key.

ENDFORM.                    " f_format_doc_key
*eject
*&---------------------------------------------------------------------*
*&      Form  f_string_remove_spcl_char
*&---------------------------------------------------------------------*
*       Remove special characters from string
*----------------------------------------------------------------------*
FORM f_string_remove_spcl_char
  CHANGING cv_string                   TYPE string.

  DATA:  lv_i             TYPE syindex,
         lv_j             TYPE syindex,
         lv_k             TYPE syindex,
         lv_char(1)       TYPE c,
         lv_string_i(250) TYPE c,
         lv_string_o(250) TYPE c.

  IF ( cv_string IS INITIAL ).
    RETURN.
  ENDIF.

* CLEAR                                     lv_string_i.
* MOVE     cv_string                     TO lv_string_i.
*
* CLEAR    lv_string_o.
*
* lv_i   = strlen( lv_string_i ).
* lv_j   = 0.
* lv_k   = 0.
*
* DO       lv_i TIMES.
*
*   CLEAR                                   lv_char.
*   MOVE     lv_string_i+lv_k(1)         TO lv_char.
*   ADD      1                           TO lv_k.
*
*   IF   ( ( lv_char                     GE ' ' ) AND
*          ( lv_char                     LE '~' )     ).
*     MOVE   lv_char                     TO lv_string_o+lv_j(1).
*     ADD    1                           TO lv_j.
*   ENDIF.
*
* ENDDO.

*eject
  CLEAR                                     lv_string_o.
  MOVE     cv_string                     TO lv_string_o.

  lv_i   = strlen( lv_string_o ).

  DO       lv_i TIMES.
    lv_j = sy-index - 1.

    CLEAR                                   lv_char.
    MOVE     lv_string_o+lv_j(1)         TO lv_char.

    IF   ( ( lv_char                     LT ' ' ) OR
           ( lv_char                     GT '~' )    ).
      MOVE   space                       TO lv_string_o+lv_j(1).
    ENDIF.

  ENDDO.

  CLEAR                                     cv_string.
  MOVE     lv_string_o                   TO cv_string.

ENDFORM.                    " f_string_remove_spcl_char
*eject
*&---------------------------------------------------------------------*
*&      Form  f_output_data
*&---------------------------------------------------------------------*
*       Output the data
*----------------------------------------------------------------------*
FORM f_output_data.

  DATA:    lwa_out_po                  TYPE ty_wa_out_po.

  DATA:    lv_filename                 TYPE string,
           lv_rec2                     TYPE text1000.

* Output the item data
  IF     ( rb_appl                       IS INITIAL ).

    CLEAR                                   lv_filename.
    MOVE   p_fpath1                      TO lv_filename.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_filename
        filetype                = 'ASC'
*       APPEND                  = ' '
        write_field_separator   = 'X'
      TABLES
        data_tab                = git_out_item
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF     ( sy-subrc NE 0 ).
* Implement suitable error handling here
    ENDIF.

  ENDIF.

*eject
* Output the PO document data
  SORT     git_out_po ASCENDING.
  DELETE   ADJACENT DUPLICATES FROM git_out_po.

  CLEAR                                     lwa_out_po.
  MOVE     text-427                      TO lwa_out_po-pur_ord.     "01
  MOVE     text-502                      TO lwa_out_po-grdat.       "02
  MOVE     text-435                      TO lwa_out_po-ebelp.       "03
  MOVE     text-436                      TO lwa_out_po-matkl.       "04
  MOVE     text-505                      TO lwa_out_po-batxt.       "05
  MOVE     text-437                      TO lwa_out_po-matdesc.     "06
  MOVE     text-507                      TO lwa_out_po-wepos.       "07
  MOVE     text-508                      TO lwa_out_po-repos.       "08
  MOVE     text-509                      TO lwa_out_po-webre.       "09

  INSERT                                    lwa_out_po
                                       INTO git_out_po
                                      INDEX 1.

  DESCRIBE TABLE git_out_po           LINES gv_cnt_rec_file2.

  IF     ( rb_appl                       IS INITIAL ).

    CLEAR                                   lv_filename.
    MOVE   p_fpath2                      TO lv_filename.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_filename
        filetype                = 'ASC'
*       APPEND                  = ' '
        write_field_separator   = 'X'
      TABLES
        data_tab                = git_out_po
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

*eject
    IF     ( sy-subrc NE 0 ).
* Implement suitable error handling here
    ENDIF.

  ELSE.

    CLEAR                                   lwa_out_po.
    LOOP AT  git_out_po                INTO lwa_out_po.

*BOC by KMB on 14.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_po-matkl  WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_po-batxt WITH gc_un.
        REPLACE ALL OCCURRENCES OF gc_com IN lwa_out_po-matdesc WITH gc_un.
*EOC by KMB on 14.10.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report

      CLEAR                                 lv_rec2.
      CONCATENATE                           lwa_out_po-pur_ord      "01
                                            lwa_out_po-grdat        "02
                                            lwa_out_po-ebelp        "03
                                            lwa_out_po-matkl        "04
                                            lwa_out_po-batxt        "05
                                            lwa_out_po-matdesc      "06
                                            lwa_out_po-wepos        "07
                                            lwa_out_po-repos        "08
                                            lwa_out_po-webre        "09
                                       INTO lv_rec2
                               SEPARATED BY gv_delim.

      TRANSFER                              lv_rec2
                                         TO gv_phy_file2.

      CLEAR  lwa_out_po.
    ENDLOOP.

  ENDIF.

*eject
* Output the accounting document data
  IF     ( rb_appl                       IS INITIAL ).

    CLEAR                                   lv_filename.
    MOVE   p_fpath3                      TO lv_filename.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_filename
        filetype                = 'ASC'
*       APPEND                  = ' '
        write_field_separator   = 'X'
      TABLES
        data_tab                = git_out_doc
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF     ( sy-subrc NE 0 ).
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.                    " f_output_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_write_report
*&---------------------------------------------------------------------*
*       Write the spool report
*----------------------------------------------------------------------*
FORM f_write_report.

  DATA:    lwa_msg                     TYPE ty_wa_msg,
           lrw_blart                   LIKE LINE OF grt_blart.

  DATA:    lv_rept_mode                TYPE char10,
           lv_count                    TYPE numc7,
           lv_blart                    TYPE blart,
           lv_blart_text               TYPE char23.

* Write the control totals section
  NEW-PAGE.
  SKIP    2.
  WRITE: /001 sy-uline(52),
          054 text-r11,
          069 sy-uline(52).

  IF     ( sy-batch                      IS INITIAL ).
    CLEAR                                   lv_rept_mode.
    MOVE   text-r15                      TO lv_rept_mode.
  ELSE.
    CLEAR                                   lv_rept_mode.
    MOVE   text-r16                      TO lv_rept_mode.
  ENDIF.

  lv_count = gv_count_err_data +
             gv_count_err_proc.

  SKIP    1.
  WRITE: /001 text-r21,
          020 lv_rept_mode,
          083 text-r22,
          096 gv_count_err_data,
         /080 text-r23,
          096 gv_count_err_proc,
         /082 text-r24,
          096 lv_count.

*eject
* Output the file names and record counts
  SKIP    1.
  CLEAR                                     lv_count.
  MOVE     gv_cnt_rec_file1              TO lv_count.
  WRITE: /001 text-r31,
          026 gv_phy_file1.
  WRITE: /001 text-r35,
          026 lv_count.

  SKIP    1.
  CLEAR                                     lv_count.
  MOVE     gv_cnt_rec_file2              TO lv_count.
  WRITE: /001 text-r32,
          026 gv_phy_file2.
  WRITE: /001 text-r35,
          026 lv_count.

  SKIP    1.
  CLEAR                                     lv_count.
  MOVE     gv_cnt_rec_file3              TO lv_count.
  WRITE: /001 text-r33,
          026 gv_phy_file3.
  WRITE: /001 text-r35,
          026 lv_count.

* Output the messages
  SKIP    2.
  WRITE: /001 sy-uline(55),
          057 text-r41,
          066 sy-uline(55).
  SKIP  1.

  CLEAR                                     lwa_msg.
  LOOP AT  git_msg_data                INTO lwa_msg.
    WRITE: /001 text-r51,               007 lwa_msg-nbr_file,
            015 text-r52,               019 lwa_msg-rc,
            027 text-r53,               035 lwa_msg-msgid,
            058 text-r54,               068 lwa_msg-msgty,
            072 text-r55,               080 lwa_msg-msgno.
    WRITE: /001 lwa_msg-text(120).
    CLEAR  lwa_msg.
  ENDLOOP.

  CLEAR                                     lwa_msg.
  LOOP AT  git_msg_proc                INTO lwa_msg.
    WRITE: /001 text-r51,               007 lwa_msg-nbr_file,
            015 text-r52,               019 lwa_msg-rc,
            027 text-r53,               035 lwa_msg-msgid,
            058 text-r54,               068 lwa_msg-msgty,
            072 text-r55,               080 lwa_msg-msgno.
    WRITE: /001 lwa_msg-text(120).
    CLEAR  lwa_msg.
  ENDLOOP.

*eject
  SKIP    1.
  WRITE: /001 sy-uline(120).

  NEW-PAGE.
  SKIP    1.
  WRITE: /001 text-r61.
  WRITE: /001 sy-uline(24).
  SKIP    1.

  CLEAR                                     lrw_blart.
  LOOP AT  grt_blart                   INTO lrw_blart.

    CLEAR                                   lv_blart.
    MOVE   lrw_blart-low                 TO lv_blart.

    CLEAR                                   lv_blart_text.
    PERFORM  f_format_doc_type_txt USING    lv_blart
                                   CHANGING lv_blart_text.

    WRITE: /001 lv_blart_text.

    CLEAR  lrw_blart.
  ENDLOOP.

  SKIP    1.
  WRITE: /001 sy-uline(120).

ENDFORM.                    " f_write_report
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_files
*&---------------------------------------------------------------------*
*       Open the files
*----------------------------------------------------------------------*
FORM f_open_files.

  DATA:    lwa_msg                     TYPE ty_wa_msg.

  DATA:    lv_msg                      TYPE text150,
           lv_rc                       TYPE numc5.

  IF     ( rb_appl                       IS INITIAL     ).
    RETURN.
  ENDIF.

  IF     ( gv_flag_err_proc              IS NOT INITIAL ).
    RETURN.
  ENDIF.

  OPEN     DATASET gv_phy_file1
           FOR OUTPUT IN TEXT MODE  MESSAGE lv_msg ENCODING DEFAULT.
  lv_rc = sy-subrc.

  IF     ( lv_rc NE 0 ).

    CLEAR                                   lwa_msg.
    MOVE     1                           TO lwa_msg-nbr_file.
    MOVE     lv_rc                       TO lwa_msg-rc.
    MOVE     gc_e                        TO lwa_msg-msgty.
    MOVE     '301'                       TO lwa_msg-msgno.
    MOVE     text-301                    TO lwa_msg-msgv1.
    MOVE     gv_phy_file1                TO lwa_msg-msgv2.
    MOVE     lv_msg+00(50)               TO lwa_msg-msgv3.
    MOVE     lv_msg+50(50)               TO lwa_msg-msgv4.

    PERFORM  f_error_in_process    USING    lwa_msg.

    CLEAR    gv_phy_file1.

    RETURN.
  ENDIF.

*eject
  OPEN     DATASET gv_phy_file2
           FOR OUTPUT IN TEXT MODE  MESSAGE lv_msg ENCODING DEFAULT.
  lv_rc = sy-subrc.

  IF     ( lv_rc NE 0 ).

    CLEAR                                   lwa_msg.
    MOVE     2                           TO lwa_msg-nbr_file.
    MOVE     lv_rc                       TO lwa_msg-rc.
    MOVE     gc_e                        TO lwa_msg-msgty.
    MOVE     '302'                       TO lwa_msg-msgno.
    MOVE     text-302                    TO lwa_msg-msgv1.
    MOVE     gv_phy_file2                TO lwa_msg-msgv2.
    MOVE     lv_msg+00(50)               TO lwa_msg-msgv3.
    MOVE     lv_msg+50(50)               TO lwa_msg-msgv4.

    PERFORM  f_error_in_process    USING    lwa_msg.

    CLEAR    gv_phy_file2.

    RETURN.
  ENDIF.

  OPEN     DATASET gv_phy_file3
           FOR OUTPUT IN TEXT MODE  MESSAGE lv_msg ENCODING DEFAULT.
  lv_rc = sy-subrc.

  IF     ( lv_rc NE 0 ).

    CLEAR                                   lwa_msg.
    MOVE     3                           TO lwa_msg-nbr_file.
    MOVE     lv_rc                       TO lwa_msg-rc.
    MOVE     gc_e                        TO lwa_msg-msgty.
    MOVE     '303'                       TO lwa_msg-msgno.
    MOVE     text-303                    TO lwa_msg-msgv1.
    MOVE     gv_phy_file3                TO lwa_msg-msgv2.
    MOVE     lv_msg+00(50)               TO lwa_msg-msgv3.
    MOVE     lv_msg+50(50)               TO lwa_msg-msgv4.

    PERFORM  f_error_in_process    USING    lwa_msg.

    CLEAR    gv_phy_file3.

    RETURN.
  ENDIF.

ENDFORM.                    " f_open_files
*eject
*&---------------------------------------------------------------------*
*&      Form  f_close_files
*&---------------------------------------------------------------------*
*       Close the files
*----------------------------------------------------------------------*
FORM f_close_files.

  IF     ( rb_appl                       IS INITIAL     ).
    RETURN.
  ENDIF.

  IF     ( gv_phy_file1                  IS NOT INITIAL ).

    CLOSE    DATASET gv_phy_file1.
    IF     ( sy-subrc NE 0 ).
*
    ELSE.
*     PERFORM  f_archive_file      USING    gv_phy_file1.
    ENDIF.

  ENDIF.

  IF     ( gv_phy_file2                  IS NOT INITIAL ).

    CLOSE    DATASET gv_phy_file2.
    IF     ( sy-subrc NE 0 ).
*
    ELSE.
*     PERFORM  f_archive_file      USING    gv_phy_file2.
    ENDIF.

  ENDIF.

  IF     ( gv_phy_file3                  IS NOT INITIAL ).

    CLOSE    DATASET gv_phy_file3.
    IF     ( sy-subrc NE 0 ).
*
    ELSE.
*     PERFORM  f_archive_file      USING    gv_phy_file3.
    ENDIF.

  ENDIF.

ENDFORM.                    " f_close_files
*eject
*&---------------------------------------------------------------------*
*&      Form  f_archive_file
*&---------------------------------------------------------------------*
*       Archive the file
*----------------------------------------------------------------------*
FORM f_archive_file
  USING    iv_phy_file                 TYPE text255.

  DATA:    lv_phy_file                 TYPE text255.

  DATA:    lwa_zvar                    TYPE ty_wa_zvar,
           lwa_arch_inst               TYPE ty_wa_arch_inst,
           lwa_arch_step               TYPE ty_wa_arch_step,
           lwa_arch_step_p             TYPE ty_wa_arch_step,
           lit_arch_step               TYPE ty_it_arch_step.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_nbr_step                 TYPE numc2,
           lv_key_step                 TYPE zparamkey,
           lv_fpath                    TYPE localfile,
           lv_fname                    TYPE localfile.

  IF     ( rb_appl                       IS INITIAL     ).
    RETURN.
  ENDIF.

  IF     ( gv_flag_err_proc              IS NOT INITIAL ).
    RETURN.
  ENDIF.

  IF     ( iv_phy_file                   IS INITIAL     ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_phy_file.
  MOVE     iv_phy_file                   TO lv_phy_file.

  CLEAR    lv_fpath.
  CLEAR    lv_fname.

* Split the physical file name into file path and file name
  PERFORM  f_split_full_file_name
                                   USING    lv_phy_file
                                   CHANGING lv_fpath
                                            lv_fname.

  CLEAR    lit_arch_step[].

*eject
* Process the archive steps
  CLEAR                                     lwa_arch_step.
  MOVE     0                             TO lwa_arch_step-step_nbr.
  MOVE     'INITIAL'                     TO lwa_arch_step-step_inst.
  MOVE     lv_fpath                      TO lwa_arch_step-fpath_in.
  MOVE     lv_fname                      TO lwa_arch_step-fname_in.
  MOVE     lv_fpath                      TO lwa_arch_step-fpath_out.
  MOVE     lv_fname                      TO lwa_arch_step-fname_out.
  APPEND                                    lwa_arch_step
                                         TO lit_arch_step.

  DO.
    lv_nbr_step = sy-index.

    CLEAR                                   lv_key_step.
    MOVE     lv_nbr_step                 TO lv_key_step.

    CLEAR                                   lwa_zvar.
    READ     TABLE git_zvar            INTO lwa_zvar
                                   WITH KEY varname = 'ARCHIVE'
                                            varnum  = lv_key_step.
    IF     ( sy-subrc NE 0 ).
      EXIT.
    ENDIF.

    CLEAR                                   lwa_arch_inst.
    MOVE     lwa_zvar-value1+00(002)     TO lwa_arch_inst-step_nbr.
    MOVE     lwa_zvar-value1+03(006)     TO lwa_arch_inst-step_cmnd.
    MOVE     lwa_zvar-value1+10(035)     TO lwa_arch_inst-step_inst.

* Read the arch step for the file path and name to be operated upon
    CLEAR                                   lwa_arch_step_p.
    READ     TABLE lit_arch_step       INTO lwa_arch_step_p
                        WITH KEY step_nbr = lwa_arch_inst-step_nbr.
    IF     ( sy-subrc NE 0 ).
*     error
      EXIT.
    ENDIF.

    CLEAR                                   lwa_arch_step.
    MOVE     lv_nbr_step                 TO lwa_arch_step-step_nbr.
    MOVE     lwa_zvar-value1             TO lwa_arch_step-step_inst.
    MOVE     lwa_arch_step_p-fpath_out   TO lwa_arch_step-fpath_in.
    MOVE     lwa_arch_step_p-fname_out   TO lwa_arch_step-fname_in.
    MOVE     lwa_arch_step_p-fpath_out   TO lwa_arch_step-fpath_out.
    MOVE     lwa_arch_step_p-fname_out   TO lwa_arch_step-fname_out.

*eject
    IF     ( lwa_zvar-value2             IS NOT INITIAL ).
      CLEAR                                 lwa_arch_step-fpath_out.
      MOVE   lwa_zvar-value2             TO lwa_arch_step-fpath_out.
    ENDIF.

    IF     ( lwa_arch_inst-step_inst     IS NOT INITIAL ).

      PERFORM  f_change_filename   USING    lwa_arch_inst
                                   CHANGING lwa_arch_step.

    ENDIF.

    CASE     lwa_arch_inst-step_cmnd.
      WHEN     'COPY'.
        PERFORM  f_file_handler    USING    'C'
                                            lwa_arch_step
                                   CHANGING lv_subrc.
      WHEN     'MOVE'.
        PERFORM  f_file_handler    USING    'M'
                                            lwa_arch_step
                                   CHANGING lv_subrc.
      WHEN     'RENAME'.
        PERFORM  f_file_handler    USING    'R'
                                            lwa_arch_step
                                   CHANGING lv_subrc.
      WHEN     OTHERS.
    ENDCASE.

    IF     ( lv_subrc NE 0 ).
      EXIT.
    ENDIF.

    APPEND                                  lwa_arch_step
                                         TO lit_arch_step.

  ENDDO.

ENDFORM.                    " f_archive_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_split_full_file_name
*&---------------------------------------------------------------------*
*       Split the physical file name into file path and file name
*----------------------------------------------------------------------*
FORM f_split_full_file_name
  USING    iv_phy_file                 TYPE text255
  CHANGING cv_fpath                    TYPE localfile
           cv_fname                    TYPE localfile.

  DATA:    lv_ptr1                     TYPE i,
           lv_ptr2                     TYPE i.

  DATA:    lwa_results_tab             TYPE match_result,
           lit_results_tab             TYPE match_result_tab.

  CLEAR    cv_fpath.
  CLEAR    cv_fname.

  IF     ( iv_phy_file IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lit_results_tab[].

  FIND     ALL OCCURRENCES OF '\'        IN iv_phy_file
                                    RESULTS lit_results_tab.

  IF     ( lit_results_tab[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     lit_results_tab    DESCENDING BY offset.

  CLEAR                                     lwa_results_tab.
  READ     TABLE lit_results_tab       INTO lwa_results_tab INDEX 1.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  lv_ptr1 = lwa_results_tab-offset + 1.

  lv_ptr2 = strlen( iv_phy_file )  - lv_ptr1.

  MOVE     iv_phy_file+0(lv_ptr1)        TO cv_fpath.
  MOVE     iv_phy_file+lv_ptr1(lv_ptr2)  TO cv_fname.

ENDFORM.                    " f_split_full_file_name
*eject
*&---------------------------------------------------------------------*
*&      Form  f_change_filename
*&---------------------------------------------------------------------*
*       Change the filename
*----------------------------------------------------------------------*
FORM f_change_filename
  USING    iwa_arch_inst               TYPE ty_wa_arch_inst
  CHANGING cwa_arch_step               TYPE ty_wa_arch_step.

  TYPES: BEGIN OF ty_wa_token,
           token                       TYPE localfile,
         END   OF ty_wa_token.

  TYPES:   ty_it_token                 TYPE STANDARD TABLE
                                         OF ty_wa_token.

  DATA:    lwa_token                   TYPE ty_wa_token,
           lit_token                   TYPE ty_it_token.

  DATA:    lv_ff                       TYPE localfile,
           lv_fn                       TYPE localfile,
           lv_ft                       TYPE localfile.

  IF   ( ( iwa_arch_inst-step_inst       IS INITIAL ) OR
         ( cwa_arch_step-step_inst       IS INITIAL ) OR
         ( cwa_arch_step-fname_in        IS INITIAL )    ).
    RETURN.
  ENDIF.

* Deconstruct the input filename
  CLEAR    lv_ff.
  CLEAR    lv_fn.
  CLEAR    lv_ft.

  MOVE     cwa_arch_step-fname_in        TO lv_ff.

  IF     ( lv_ff                         CA '.' ).
    SPLIT  lv_ff AT '.'                INTO lv_fn lv_ft.
  ELSE.
    MOVE   lv_ff                         TO lv_fn.
    CLEAR                                   lv_ft.
  ENDIF.

  CLEAR    lit_token[].

  SPLIT    iwa_arch_inst-step_inst AT space
                                 INTO TABLE lit_token.

  DELETE   lit_token            WHERE token = space.

  IF     ( lit_token[]             IS INITIAL ).
    RETURN.
  ENDIF.

*eject
* Construct the filename

  CLEAR    lv_ff.

  CLEAR                                     lwa_token.
  LOOP AT  lit_token                   INTO lwa_token.

    SHIFT      lwa_token-token         LEFT DELETING LEADING space.

    IF     ( ( lwa_token-token           EQ 'fn' ) OR
             ( lwa_token-token           EQ 'FN' )    ).

      CLEAR                                 lwa_token-token.
      MOVE   lv_fn                       TO lwa_token-token.

    ELSEIF ( ( lwa_token-token           EQ 'ft' ) OR
             ( lwa_token-token           EQ 'FT' )    ).

      CLEAR                                 lwa_token-token.
      MOVE   lv_ft                       TO lwa_token-token.

    ELSEIF   ( lwa_token-token           CS 'yyyymmdd' ).

      CLEAR                                 lwa_token-token.
      MOVE   sy-datum                    TO lwa_token-token.

    ELSEIF   ( lwa_token-token           CS 'hhmmss' ).

      CLEAR                                 lwa_token-token.
      MOVE   sy-uzeit                    TO lwa_token-token.

    ENDIF.

    IF     ( lwa_token-token             IS NOT INITIAL ).

      CONCATENATE                     lv_ff lwa_token-token INTO lv_ff.

    ENDIF.

    CLEAR  lwa_token.
  ENDLOOP.

  IF     ( lv_ff                         IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     cwa_arch_step-fname_out.
  MOVE     lv_ff                         TO cwa_arch_step-fname_out.

ENDFORM.                    " f_change_filename
*eject
*&---------------------------------------------------------------------*
*&      Form  f_file_handler
*&---------------------------------------------------------------------*
*       File handler
*----------------------------------------------------------------------*
FORM f_file_handler
  USING    iv_command                  TYPE char1
           iwa_arch_step               TYPE ty_wa_arch_step
  CHANGING cv_subrc                    TYPE sysubrc.

  DATA:    lwa_return                  TYPE bapireturn,
           lwa_msg                     TYPE ty_wa_msg.

  DATA:    lv_source_dir               TYPE btcxpgpar,
           lv_target_dir               TYPE btcxpgpar,
           lv_source_fname             TYPE btcxpgpar,
           lv_target_fname             TYPE btcxpgpar.

  CLEAR    cv_subrc.

  IF     ( gv_flag_err_proc              IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_source_dir.
  MOVE     iwa_arch_step-fpath_in        TO lv_source_dir.
  CLEAR                                     lv_target_dir.
  MOVE     iwa_arch_step-fpath_out       TO lv_target_dir.
  CLEAR                                     lv_source_fname.
  MOVE     iwa_arch_step-fname_in        TO lv_source_fname.
  CLEAR                                     lv_target_fname.
  MOVE     iwa_arch_step-fname_out       TO lv_target_fname.

  IF     ( iv_command                    EQ 'R' ).
    CLEAR                                   lv_target_dir.
  ENDIF.

*eject
* Copy, move, or rename the file

  CALL FUNCTION 'ZFI_FILE_HANDLE'
    EXPORTING
      i_source_dir        = lv_source_dir
      i_target_dir        = lv_target_dir
      i_source_fname      = lv_source_fname
      i_target_fname      = lv_target_fname
      i_command           = iv_command
      i_date_time_stamp   = space
      i_rename_arc_to_new = space
    IMPORTING
      e_return            = lwa_return.

  IF   ( lwa_return-type CA 'aAeE' ).

    PERFORM  f_error_in_process    USING    lwa_msg.

    cv_subrc = 8.

    RETURN.
  ENDIF.

  WAIT UP TO 1 SECONDS.

ENDFORM.                    " f_file_handler
*eject
*&---------------------------------------------------------------------*
*&      Form  f_error_in_data
*&---------------------------------------------------------------------*
*       Append an error to the data error table
*----------------------------------------------------------------------*
FORM f_error_in_data
  USING    iwa_msg       TYPE ty_wa_msg.

  DATA:    lv_type_fld   TYPE char1,
           lv_msgno_c    TYPE char3,
           lv_msgno      TYPE symsgno,
           lv_text       TYPE text240.

  DATA:    lwa_msg_data  TYPE ty_wa_msg.

  IF     ( iwa_msg-msgty            CA 'AaEe' ).
    ADD      1                      TO gv_count_err_data.
    CLEAR                              gv_flag_err_data.
    MOVE     gc_x                   TO gv_flag_err_data.
    CLEAR                              gv_flag_err_mstr.
    MOVE     gc_x                   TO gv_flag_err_mstr.
  ENDIF.

  CLEAR    lv_type_fld.
  CLEAR    lv_msgno_c.
  CLEAR    lv_msgno.
  CLEAR    lv_text.

  DESCRIBE FIELD iwa_msg-msgno    TYPE lv_type_fld.

  IF           ( lv_type_fld        CS gc_c ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iwa_msg-msgno
      IMPORTING
        output = lv_msgno_c.

    IF         ( lv_msgno_c         CO '0123456789' ).

      MOVE       lv_msgno_c         TO lv_msgno.

    ENDIF.

  ELSEIF       ( lv_type_fld        CS gc_n ).

    MOVE         iwa_msg-msgno      TO lv_msgno.

  ENDIF.

*eject
  IF   ( iwa_msg-msgid IS INITIAL ).

    CONCATENATE                        iwa_msg-msgv1
                                       iwa_msg-msgv2
                                       iwa_msg-msgv3
                                       iwa_msg-msgv4
                                  INTO lv_text
                          SEPARATED BY space.

  ELSE.

    MESSAGE  ID iwa_msg-msgid     TYPE iwa_msg-msgty
         NUMBER lv_msgno          INTO lv_text
           WITH iwa_msg-msgv1          iwa_msg-msgv2
                iwa_msg-msgv3          iwa_msg-msgv4.

  ENDIF.

  CLEAR                                lwa_msg_data.
  MOVE     iwa_msg-nbr_file         TO lwa_msg_data-nbr_file.
  MOVE     iwa_msg-rc               TO lwa_msg_data-rc.
  MOVE     iwa_msg-msgid            TO lwa_msg_data-msgid.
  MOVE     iwa_msg-msgty            TO lwa_msg_data-msgty.
  MOVE     lv_msgno                 TO lwa_msg_data-msgno.
  MOVE     lv_text                  TO lwa_msg_data-text.
  APPEND   lwa_msg_data             TO git_msg_data.

ENDFORM.                    " f_error_in_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_error_in_process
*&---------------------------------------------------------------------*
*       Append an error to the process error table
*----------------------------------------------------------------------*
FORM f_error_in_process
  USING    iwa_msg       TYPE ty_wa_msg.

  DATA:    lv_type_fld   TYPE char1,
           lv_msgno_c    TYPE char3,
           lv_msgno      TYPE symsgno,
           lv_text       TYPE text240.

  DATA:    lwa_msg_proc  TYPE ty_wa_msg.

  IF     ( iwa_msg-msgty            CA 'AaEe' ).
    ADD      1                      TO gv_count_err_proc.
    CLEAR                              gv_flag_err_proc.
    MOVE     gc_x                   TO gv_flag_err_proc.
    CLEAR                              gv_flag_err_mstr.
    MOVE     gc_x                   TO gv_flag_err_mstr.
  ENDIF.

  CLEAR    lv_type_fld.
  CLEAR    lv_msgno_c.
  CLEAR    lv_msgno.
  CLEAR    lv_text.

  DESCRIBE FIELD iwa_msg-msgno    TYPE lv_type_fld.

  IF           ( lv_type_fld        CS gc_c ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iwa_msg-msgno
      IMPORTING
        output = lv_msgno_c.

    IF         ( lv_msgno_c         CO '0123456789' ).

      MOVE       lv_msgno_c         TO lv_msgno.

    ENDIF.

  ELSEIF       ( lv_type_fld        CS gc_n ).

    MOVE         iwa_msg-msgno      TO lv_msgno.

  ENDIF.

*eject
  IF   ( iwa_msg-msgid IS INITIAL ).

    CONCATENATE                        iwa_msg-msgv1
                                       iwa_msg-msgv2
                                       iwa_msg-msgv3
                                       iwa_msg-msgv4
                                  INTO lv_text
                          SEPARATED BY space.

  ELSE.

    MESSAGE  ID iwa_msg-msgid     TYPE iwa_msg-msgty
         NUMBER lv_msgno          INTO lv_text
           WITH iwa_msg-msgv1          iwa_msg-msgv2
                iwa_msg-msgv3          iwa_msg-msgv4.

  ENDIF.

  CLEAR                                lwa_msg_proc.
  MOVE     iwa_msg-nbr_file         TO lwa_msg_proc-nbr_file.
  MOVE     iwa_msg-rc               TO lwa_msg_proc-rc.
  MOVE     iwa_msg-msgid            TO lwa_msg_proc-msgid.
  MOVE     iwa_msg-msgty            TO lwa_msg_proc-msgty.
  MOVE     lv_msgno                 TO lwa_msg_proc-msgno.
  MOVE     lv_text                  TO lwa_msg_proc-text.
  APPEND   lwa_msg_proc             TO git_msg_proc.

ENDFORM.                    " f_error_in_process
*eject
*&---------------------------------------------------------------------*
*&      Form  f_print_report_header
*&---------------------------------------------------------------------*
*       Print the report header - top-of-page
*----------------------------------------------------------------------*
FORM f_print_report_header.

  CLEAR                                gv_sysid.
  MOVE     sy-sysid                 TO gv_sysid.
  MOVE     sy-mandt                 TO gv_sysid+4(3).
  CLEAR                                gv_uname.
  MOVE     sy-uname                 TO gv_uname.
  CLEAR                                gv_pagno.
  MOVE     sy-pagno                 TO gv_pagno.
  CLEAR                                gv_cprog.
  MOVE     sy-cprog                 TO gv_cprog.
  CLEAR                                gv_datum.
  MOVE     sy-datum                 TO gv_datum.
  CLEAR                                gv_uzeit.
  MOVE     sy-uzeit                 TO gv_uzeit.

  WRITE: /001 text-h11,
          012 gv_sysid,
          051 text-h12,
          062 gv_uname,
          103 text-h13,
          109 gv_pagno.

  WRITE: /001 text-h14,
          013 gv_cprog,
          051 text-h15,
          061 gv_datum,
          103 text-h16,
          113 gv_uzeit.

ENDFORM.                    " f_print_report_header
