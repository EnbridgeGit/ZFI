*&---------------------------------------------------------------------*
*&  Include           ZFAPI130_CIMPL_INV_BDC_F01
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI130_CIMPL_INV_BDC                            *
*  Author:           Shamsiya Shaffe                                   *
*  Date:             August 29, 2019                                   *
*  Track #:                                                            *
*  Application Area: FICO                                              *
*                                                                      *
*  Description:      Cimpl Interface Invoice Inbound BDC               *
*                                                                      *
*                    Read a file in the RFBIBL00 format, translate     *
*                    the records into BDC data, and post the Invoice   *
*                    transactions.                                     *
*                                                                      *
*                    Copy from US Report - ZFAPI017_DATACERT_INV_BDC   *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
*08/29/2019 D30K930117 SHAFFES CHG0153812 - Initial program development*
*10/11/2019 D30K930204 SHAFFES CHG0153812 - Initial program development*
*18/08/2020 D30K930653 AHMADT  CHG0188302 - Disabling ZZREF field      *
*----------------------------------------------------------------------*
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_xparam
*&---------------------------------------------------------------------*
*       Get the program parameter values
*----------------------------------------------------------------------*
FORM f_select_xparam.

  CLEAR    git_xparam[].
  CLEAR    gv_obj_id.
  CLEAR    p_objid.

  CLEAR    git_xparam[].
  SELECT   *
    INTO   TABLE git_xparam
    FROM   zfit_xparam
   WHERE   paramtype = gc_param_in_int
     AND   subtype   = gc_param_obj_id
     AND   key1      = gc_param_post.
  IF ( sy-subrc EQ 0 ).
    SORT   git_xparam     ASCENDING BY mandt paramtype subtype
                                       key1 key2 key3 key4 key5.
    MOVE   gc_param_obj_id          TO gv_obj_id.
    MOVE   gc_param_obj_id          TO p_objid.
  ELSE.
    CLEAR  git_xparam[].
    CLEAR  gv_obj_id.
    CLEAR  p_objid.
  ENDIF.

ENDFORM.                    " f_select_xparam
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_email_id
*&---------------------------------------------------------------------*
*       Set the email ID
*----------------------------------------------------------------------*
FORM f_set_email_id.

  DATA:    lwa_xparam TYPE ty_wa_xparam.

  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_email
                                       key3    = gc_email_id.
  IF ( sy-subrc EQ 0 ).
    CLEAR                              p_email.
    MOVE     lwa_xparam-value1      TO p_email.
  ENDIF.

ENDFORM.                    " f_set_email_id
*eject
*&---------------------------------------------------------------------*
*&      Form  f_toggle_functionality
*&---------------------------------------------------------------------*
*       Selection screen fields to be disabled/hidden
*----------------------------------------------------------------------*
FORM f_toggle_functionality.

  LOOP AT SCREEN.

* Set screen fields to display only
    IF   ( ( screen-group1 EQ gc_modif_id_dsp ) OR
           ( screen-group1 EQ gc_modif_id_dlm )    ).
      screen-input = 0.
      MODIFY   SCREEN.
    ENDIF.

* If the object ID is not assigned, then disable input file parameters
    IF     ( p_objid       IS INITIAL ).

      IF ( ( screen-group1 EQ gc_modif_id_fpt ) OR
           ( screen-group1 EQ gc_modif_id_fnm )    ).
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.

    ELSEIF ( rb_appl       EQ gc_x ). "input file appl-srvr radiobutton

* Disable the input filepath; populate with value from zfit_xparam
      IF   ( screen-group1 EQ gc_modif_id_fpt ).
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.

* Enable the input filename
      IF   ( screen-group1 EQ gc_modif_id_fnm ).
        screen-input = 1.
        MODIFY   SCREEN.
      ENDIF.

    ELSEIF ( rb_pres       EQ gc_x ). "input file pres-srvr radiobutton

* Enable the input filepath
      IF   ( screen-group1 EQ gc_modif_id_fpt ).
        screen-input = 1.
        MODIFY   SCREEN.
      ENDIF.

* Disable the input filename; value to be included with filepath
      IF   ( screen-group1 EQ gc_modif_id_fnm ).
        screen-active = 0.
        MODIFY   SCREEN.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_toggle_functionality
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_sel_scrn_options
*&---------------------------------------------------------------------*
*       Set the selection screen options
*----------------------------------------------------------------------*
FORM f_set_sel_scrn_options.

  DATA: lwa_xparam TYPE ty_wa_xparam.

* Clear the input filepath when switching from appl to pres server
  IF   ( ( rb_pres EQ gc_x )     AND ( gv_flag_pres IS INITIAL ) ).
    MOVE   gc_x                   TO   gv_flag_pres.
    CLEAR  p_fpath.
    CLEAR  p_fname.
  ENDIF.

* If appl server is selected, set the filepath & name from ZFIT_XPARAM
  IF     ( rb_appl EQ gc_x ).

    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_in.
    IF ( sy-subrc EQ 0 ).
      CLEAR                            p_fpath.
      MOVE     lwa_xparam-value1    TO p_fpath.
    ENDIF.
    CLEAR                              lwa_xparam.

  ENDIF.

* Set the object ID description
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key1    = gc_param_post
                                       key2    = space.
  IF ( sy-subrc EQ 0 ).
    IF ( lwa_xparam-value1      IS NOT INITIAL ).
      CLEAR                            p_objdsc.
      MOVE     lwa_xparam-value1    TO p_objdsc.
    ENDIF.
  ENDIF.
  CLEAR                                lwa_xparam.

ENDFORM.                    " f_set_sel_scrn_options
*eject
*&---------------------------------------------------------------------*
*&      Form  f_f4_help_pres_file
*&---------------------------------------------------------------------*
*       F4 help - presentation server - file open dialog
*----------------------------------------------------------------------*
FORM f_f4_help_pres_file
  CHANGING cv_fpath             TYPE localfile.

  CONSTANTS:
           lc_window_title      TYPE string
                                VALUE 'Select File',
           lc_initial_directory TYPE string
                                VALUE 'C:\'.

  DATA:    lv_rc                TYPE i.

  DATA:    lwa_file_table       TYPE file_table,
           lit_file_table       TYPE filetable.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lc_window_title
      initial_directory       = lc_initial_directory
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

  IF ( sy-subrc EQ 0 ).

    CLEAR                                        lwa_file_table.
    READ     TABLE lit_file_table           INTO lwa_file_table
                                           INDEX 1.
    IF ( sy-subrc EQ 0 ).
      CLEAR                                      cv_fpath.
      MOVE         lwa_file_table-filename    TO cv_fpath.
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

  DATA:    lwa_xparam  TYPE zfit_xparam.

* Reset input presentation server flag to control clearing of filepath
  IF     ( rb_appl  IS NOT INITIAL ).
    CLEAR  gv_flag_pres.
  ENDIF.

* Check if the program parameter table is maintained
  IF     ( git_xparam[] IS INITIAL ).
    MESSAGE  e000(zfi01) WITH text-111.
  ENDIF.

  IF     ( rb_appl      EQ gc_x    ).

* Check if the input filepath has been maintained
    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_in.
    IF ( sy-subrc EQ 0 ).
      IF         ( lwa_xparam-value1   IS INITIAL ).
        MESSAGE  e000(zfi01) WITH text-112.
      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-112.
    ENDIF.

* Check if the archive filepath has been maintained
    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_arch.
    IF ( sy-subrc EQ 0 ).
      IF         ( lwa_xparam-value1   IS INITIAL ).
        MESSAGE  e000(zfi01) WITH text-114.
      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-114.
    ENDIF.

*eject
* Check if the error filepath has been maintained
    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_err.
    IF ( sy-subrc EQ 0 ).
      IF         ( lwa_xparam-value1   IS INITIAL ).
        MESSAGE  e000(zfi01) WITH text-115.
      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-115.
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

  DATA:    lwa_xparam   TYPE ty_wa_xparam,
           lwa_ktokk_ot TYPE ty_wa_ktokk_ot.

* Initial the internal tables
  CLEAR    git_ktokk_ot[].
  CLEAR    gr_ktokk_ot[].
  CLEAR    git_file_list[].
  CLEAR    git_file_stats[].
  CLEAR    git_rfbibl00[].
  CLEAR    git_docs_posted[].
  CLEAR    git_errs_data[].
  CLEAR    git_errs_proc[].

* Initial variables
  CLEAR    gv_nbr_file.
  CLEAR    gv_count_err_data.
  CLEAR    gv_count_err_proc.
  CLEAR    gv_flag_err_data.
  CLEAR    gv_flag_err_proc.
  CLEAR    gv_flag_err_mstr.

* Set the batch data control structure
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_bdc_control.
  IF ( sy-subrc NE 0 ).
    CLEAR        lwa_xparam.
  ENDIF.

  CLEAR                                     gwa_bdccntl.

  IF       ( lwa_xparam-value1       IS NOT INITIAL ).
    MOVE     lwa_xparam-value1+00(01)    TO gwa_bdccntl-dismode.
    MOVE     lwa_xparam-value1+01(01)    TO gwa_bdccntl-updmode.
    MOVE     lwa_xparam-value1+02(01)    TO gwa_bdccntl-defsize.
  ENDIF.

*  IF       ( lwa_xparam-value2       IS NOT INITIAL ).
*    MOVE     lwa_xparam-value2+00(12)    TO gwa_bdccntl-sesn_group.
*    MOVE     lwa_xparam-value2+12(08)    TO gwa_bdccntl-sesn_start.
*    MOVE     lwa_xparam-value2+20(01)    TO gwa_bdccntl-sesn_xkeep.
*    MOVE     lwa_xparam-value2+21(12)    TO gwa_bdccntl-sesn_usnam.
*  ENDIF.

  IF       ( lwa_xparam-value2+00(33)       IS NOT INITIAL ).
    MOVE     lwa_xparam-value2+00(12)    TO gwa_bdccntl-sesn_group.
    MOVE     lwa_xparam-value2+12(08)    TO gwa_bdccntl-sesn_start.
    MOVE     lwa_xparam-value2+20(01)    TO gwa_bdccntl-sesn_xkeep.
    MOVE     lwa_xparam-value2+21(12)    TO gwa_bdccntl-sesn_usnam.
  ENDIF.

* Logic for changing session name in case of duplication.
  IF       ( lwa_xparam-value2+34(33)       IS NOT INITIAL ).
    MOVE     lwa_xparam-value2+34(12)    TO gwa_bdccntl_dup-sesn_group.
    MOVE     lwa_xparam-value2+46(08)    TO gwa_bdccntl_dup-sesn_start.
    MOVE     lwa_xparam-value2+54(01)    TO gwa_bdccntl_dup-sesn_xkeep.
    MOVE     lwa_xparam-value2+55(12)    TO gwa_bdccntl_dup-sesn_usnam.
  ENDIF.

*  IF gwa_bdccntl_dup-sesn_group IS NOT INITIAL OR
*     gwa_bdccntl_dup-sesn_start IS NOT INITIAL OR
*     gwa_bdccntl_dup-sesn_xkeep IS NOT INITIAL OR
*     gwa_bdccntl_dup-sesn_usnam IS NOT INITIAL.
*
*    MOVE     gc_x                        TO gwa_bdccntl_dup-sesn_create.
*
*  ENDIF.


  IF     ( ( gwa_bdccntl-dismode         IS INITIAL ) OR
           ( gwa_bdccntl-updmode         IS INITIAL )    ).
    MOVE     gc_x                        TO gwa_bdccntl-sesn_create.
  ENDIF.

*eject
* Select the one-time vendor codes
  SELECT   ktokk
    INTO   TABLE git_ktokk_ot
    FROM   t077k
   WHERE   xcpds = gc_x.
  IF ( sy-subrc NE 0 ).
    CLEAR  git_ktokk_ot[].
  ENDIF.

  CLEAR:   gr_ktokk_ot[].

  CLEAR                                lwa_ktokk_ot.
  LOOP AT  git_ktokk_ot           INTO lwa_ktokk_ot.
    CLEAR                              gwa_ktokk_ot.
    MOVE   'I'                      TO gwa_ktokk_ot-sign.
    MOVE   'EQ'                     TO gwa_ktokk_ot-option.
    MOVE   lwa_ktokk_ot-ktokk       TO gwa_ktokk_ot-low.
    APPEND gwa_ktokk_ot             TO gr_ktokk_ot.
    CLEAR  lwa_ktokk_ot.
  ENDLOOP.
  IF ( sy-subrc NE 0 ).
    CLEAR                              gwa_ktokk_ot.
    MOVE   'I'                      TO gwa_ktokk_ot-sign.
    MOVE   'EQ'                     TO gwa_ktokk_ot-option.
    MOVE   '****'                   TO gwa_ktokk_ot-low.
    APPEND gwa_ktokk_ot             TO gr_ktokk_ot.
  ENDIF.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_build_file_list
*&---------------------------------------------------------------------*
*       Build the list of files to be processed
*----------------------------------------------------------------------*
FORM f_build_file_list
  TABLES   cit_file_list STRUCTURE gwa_file_list.

  DATA:    lwa_file_list TYPE ty_wa_file_list.

  DATA:    lwa_xparam    TYPE ty_wa_xparam.

  DATA:    lv_dir_name   TYPE epsdirnam,
           lv_file_mask  TYPE epsfilnam.

  DATA:    lwa_dir_list  TYPE epsfili.
  DATA:    lit_dir_list  LIKE STANDARD TABLE OF lwa_dir_list.

  DATA:    lv_rc         TYPE numc5,
           lv_string     TYPE string,
           lv_pattern    TYPE string,
           lv_token      TYPE string,
           lv_offset     TYPE i,
           lv_length     TYPE i,
           lv_subrc      TYPE sysubrc.

  CLEAR    cit_file_list[].

  IF     ( gv_flag_err_proc     IS NOT INITIAL ).
    RETURN.
  ENDIF.

  IF     ( rb_pres  EQ gc_x ).

    CLEAR                              lwa_file_list.
    MOVE   p_fpath                  TO lwa_file_list-filename_in.
    APPEND                             lwa_file_list
                                    TO cit_file_list.

  ELSEIF ( rb_appl  EQ gc_x ).

    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_in.
    IF ( sy-subrc EQ 0 ).

      CLEAR                            lv_dir_name.
      MOVE     lwa_xparam-value1    TO lv_dir_name.

      IF     ( p_fname          IS NOT INITIAL ).
        CLEAR                          lv_file_mask.
        MOVE   p_fname              TO lv_file_mask.
      ENDIF.

*eject

      CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
        EXPORTING
          dir_name               = lv_dir_name
          file_mask              = lv_file_mask
        TABLES
          dir_list               = lit_dir_list
        EXCEPTIONS
          invalid_eps_subdir     = 1
          sapgparam_failed       = 2
          build_directory_failed = 3
          no_authorization       = 4
          read_directory_failed  = 5
          too_many_read_errors   = 6
          empty_directory_list   = 7
          OTHERS                 = 8.

      lv_rc = sy-subrc.

      IF ( ( lv_rc NE 0 ) AND ( lv_rc NE 7 ) ).

        CLEAR    lit_dir_list[].

        PERFORM  f_error_in_process
                                 USING 0
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-121
                                       lv_rc
                                       space
                                       space.

        RETURN.
      ENDIF.

      DELETE     lit_dir_list    WHERE name IS INITIAL.

      IF       ( lit_dir_list[]     IS INITIAL ).

        MESSAGE  i000(zfi01) WITH text-122.

        RETURN.
      ENDIF.

*eject
* Evaluate the files in the inbound directory
      CLEAR                            lv_pattern.
      MOVE       lwa_xparam-value2  TO lv_pattern.
      IF       ( lwa_xparam-value2  IS INITIAL ).
        CLEAR                          lv_pattern.
        MOVE     gc_pattern_fn_in   TO lv_pattern.
      ENDIF.

      CLEAR                            lwa_dir_list.
      LOOP AT    lit_dir_list     INTO lwa_dir_list.

        CONDENSE lwa_dir_list-name     NO-GAPS.

        CLEAR                          lv_string.
        MOVE     lwa_dir_list-name  TO lv_string.

* Search the string using a pattern; return offset, length, and token

        PERFORM  f_parse_string_using_pattern
                              USING    lv_string
                                       lv_pattern
                              CHANGING lv_offset
                                       lv_length
                                       lv_token
                                       lv_subrc.

        lv_rc = lv_subrc.

        IF ( lv_rc NE 0 ).
          CLEAR    cit_file_list[].

          PERFORM  f_error_in_process
                                 USING 0
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-125
                                       lv_rc
                                       lwa_dir_list-name
                                       space.

          RETURN.
        ENDIF.

*eject
* If the pattern is found, then save the filename in the file list
        IF   ( ( lv_subrc EQ 0 ) AND ( lv_token IS NOT INITIAL ) ).
          CLEAR                        lwa_file_list.
          CONCATENATE                  lv_dir_name
                                       lwa_dir_list-name
                                  INTO lwa_file_list-filename_in.
          MOVE                         lwa_dir_list-name
                                    TO lwa_file_list-filename_fn.
          APPEND                       lwa_file_list
                                    TO cit_file_list.
        ENDIF.

        CLEAR    lwa_dir_list.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF       ( cit_file_list[] IS INITIAL ).
    MESSAGE  i000(zfi01) WITH text-122.
    RETURN.
  ENDIF.

  SORT     cit_file_list ASCENDING BY filename_in.

ENDFORM.                    " f_build_file_list
*eject
*&---------------------------------------------------------------------*
*&      Form  f_parse_string_using_pattern
*&---------------------------------------------------------------------*
*       Search the string using pattern; return offset, length, & token
*----------------------------------------------------------------------*
FORM f_parse_string_using_pattern
  USING    iv_text    TYPE string
           iv_pattern TYPE string
  CHANGING cv_offset  TYPE i
           cv_length  TYPE i
           cv_token   TYPE string
           cv_subrc   TYPE sysubrc.

  DATA:    lv_text    TYPE string,
           lv_success TYPE char1,
           lv_offset  TYPE i,
           lv_length  TYPE i,
           lv_token   TYPE string.

  DATA:    lr_matcher TYPE REF TO cl_abap_matcher.

  CLEAR    cv_offset.
  CLEAR    cv_length.
  CLEAR    cv_token.

  CHECK  ( cv_subrc   IS INITIAL ).

  IF     ( iv_text    IS INITIAL ).
    cv_subrc = 1. "raise no_text
    RETURN.
  ENDIF.

  IF     ( iv_pattern IS INITIAL ).
    cv_subrc = 2. "raise no_pattern
    RETURN.
  ENDIF.

  CLEAR                  lv_text.
  MOVE     iv_text    TO lv_text.
  TRANSLATE              lv_text TO UPPER CASE.

* Search the string using the pattern
  TRY.

      CALL METHOD cl_abap_matcher=>contains
        EXPORTING
          pattern = iv_pattern
          text    = lv_text
        RECEIVING
          success = lv_success.

    CATCH cx_sy_matcher.
      cv_subrc = 3. "raise search_error

    CATCH cx_sy_regex.
      cv_subrc = 4. "raise pattern_error

  ENDTRY.

*eject
  IF     ( lv_success IS INITIAL ).
    RETURN.
  ENDIF.

* Get the offset and length

  CALL METHOD cl_abap_matcher=>get_object
    RECEIVING
      matcher = lr_matcher.

  TRY.

      lv_offset = lr_matcher->get_offset( ).

      lv_length = lr_matcher->get_length( ).

    CATCH cx_sy_matcher.
      cv_subrc = 5. "raise parse_error

  ENDTRY.

  IF     ( lv_length IS INITIAL ).
    RETURN.
  ENDIF.

* Set the token
  lv_token = iv_text+lv_offset(lv_length).

  IF     ( lv_token IS INITIAL ).
    RETURN.
  ENDIF.

  cv_offset = lv_offset.
  cv_length = lv_length.
  cv_token  = lv_token.

ENDFORM.                    " f_parse_string_using_pattern
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_input_data
*&---------------------------------------------------------------------*
*       Input the delimited data file
*----------------------------------------------------------------------*
FORM f_get_input_data
  TABLES   cit_rfbibl00    TYPE ty_it_rfbibl00
           cit_file_stats  STRUCTURE gwa_file_stats
  USING    iwa_file_list   TYPE ty_wa_file_list.

  DATA:    lv_filename_in  TYPE text1024,
           lv_filename_fn  TYPE text128,
           lv_count        TYPE syindex.

  DATA:    lwa_file_stats  TYPE ty_wa_file_stats.

  CLEAR    cit_rfbibl00[].

  IF     ( gv_flag_err_proc     IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lv_filename_in.
  MOVE     iwa_file_list-filename_in
                                    TO lv_filename_in.

  CLEAR                                lv_filename_fn.
  MOVE     iwa_file_list-filename_fn
                                    TO lv_filename_fn.

  IF     ( rb_appl EQ gc_x ).

* Read data from a file on the IA server

    PERFORM  f_read_data      TABLES   cit_rfbibl00
                              USING    lv_filename_in
                                       lv_filename_fn.

  ELSEIF ( rb_pres EQ gc_x ).

* Upload data from a file on the presentation server

    PERFORM  f_upload_data    TABLES   cit_rfbibl00
                              USING    lv_filename_in.

  ENDIF.

  IF     ( gv_flag_err_proc     IS NOT INITIAL ).
    RETURN.
  ENDIF.

*eject
* Append the file stats
  lv_count = lines( cit_rfbibl00[] ).

  CLEAR                                lwa_file_stats.
  MOVE     gv_nbr_file              TO lwa_file_stats-nbr_file.
  MOVE     lv_filename_in           TO lwa_file_stats-filename_in.
  MOVE     lv_filename_fn           TO lwa_file_stats-filename_fn.
  MOVE     lv_count                 TO lwa_file_stats-cnt_rec_in.
  APPEND                               lwa_file_stats
                                    TO cit_file_stats.

ENDFORM.                    " f_get_input_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_read_data
*&---------------------------------------------------------------------*
*       Read data from a file on the IA server
*----------------------------------------------------------------------*
FORM f_read_data
  TABLES   cit_rfbibl00    TYPE ty_it_rfbibl00
  USING    iv_filename_in  TYPE text1024
           iv_filename_fn  TYPE text128.

  DATA:    lv_rc           TYPE numc5,
           lv_text(3000)   TYPE c.

  CLEAR    cit_rfbibl00[].

* Open the file in input mode
  OPEN     DATASET iv_filename_in
           FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  lv_rc = sy-subrc.

  IF ( lv_rc NE 0 ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-131
                                       iv_filename_in
                                       iv_filename_fn
                                       space.

    RETURN.
  ENDIF.

  DO.

    CLEAR                                   lv_text.
    READ     DATASET iv_filename_in    INTO lv_text.
    IF ( sy-subrc EQ 0 ).
      APPEND                                lv_text
                                         TO cit_rfbibl00.
    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

* Close the file
  CLOSE    DATASET iv_filename_in.

ENDFORM.                    " f_read_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_upload_data
*&---------------------------------------------------------------------*
*       Upload data from a file on the presentation server
*----------------------------------------------------------------------*
FORM f_upload_data
  TABLES   cit_rfbibl00    TYPE ty_it_rfbibl00
  USING    iv_filename_in  TYPE text1024.

  DATA:    lv_rc           TYPE numc5,
           lv_filename     TYPE string.

  CLEAR    cit_rfbibl00[].

  CLEAR    lv_rc.

  IF     ( iv_filename_in IS INITIAL ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-132
                                       space
                                       space
                                       space.

    RETURN.
  ENDIF.

* Upload the file from the presentation server
  CLEAR                                lv_filename.
  MOVE     iv_filename_in           TO lv_filename.

*eject

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = cit_rfbibl00
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  lv_rc = sy-subrc.

  IF ( lv_rc NE 0 ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-132
                                       iv_filename_in
                                       space
                                       space.

    RETURN.
  ENDIF.

ENDFORM.                    " f_upload_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_rfbibl00
*&---------------------------------------------------------------------*
*       Process the RFBIBL00 formatted records
*----------------------------------------------------------------------*
FORM f_process_rfbibl00
  TABLES   iit_rfbibl00    TYPE ty_it_rfbibl00.  "Inv RFBIBL00 Format

  DATA:    lv_flag_toggle  TYPE flag,            "Flag-Toggle
           lv_newbs_old    TYPE newbs,           "Posting Key-Next Line
           lv_newbk_old    TYPE newbk,           "Company Code-Next Line
           lv_newbk        TYPE newbk,           "Company Code-Next Line
           lv_tcode        TYPE tcode,           "Transaction Code
           lv_nbr_btc      TYPE numc5,           "Batch Sequence Number
           lv_nbr_doc      TYPE numc5,           "Doc Sequence Number
           lv_flag_ic      TYPE flag.            "Flag-InterCompany

  DATA:    lwa_rfbibl00    TYPE ty_wa_rfbibl00,  "Inv RFBIBL00 Format
           lwa_bbkpf       TYPE ty_wa_bbkpf,     "Accounting Doc Header
           lwa_bbseg       TYPE ty_wa_bbseg.     "Accounting Doc Segment


  REFRESH: lit_rfbibl00_dup.
  CLEAR  : lwa_rfbibl00_dup.

  IF     ( gv_flag_err_proc     IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lv_flag_toggle.
  MOVE     gc_x                     TO lv_flag_toggle.

  CLEAR    git_bdcdata[].
  CLEAR    gv_flag_err_vldt.
  CLEAR    lv_flag_ic.
  CLEAR    lv_newbs_old.
  CLEAR    lv_newbk_old.
  CLEAR    lv_tcode.
  CLEAR    lv_nbr_btc.
  CLEAR    lv_nbr_doc.

** Logic for Checking if Duplication exists in posting entries.
  lit_rfbibl00_dup[] = iit_rfbibl00[].
  PERFORM f_process_duplication.

* Loop at all records in the file
  CLEAR                                lwa_rfbibl00.
  LOOP AT  iit_rfbibl00           INTO lwa_rfbibl00.

    CASE     lwa_rfbibl00+0(1).

*eject
* Batch session record
      WHEN   '0'.

        IF     ( lv_flag_toggle     IS INITIAL ).

          PERFORM  f_format_bdc_save_doc     USING    lv_tcode
                                                      lv_nbr_btc
                                                      lv_nbr_doc
                                                      lv_flag_ic.

        ENDIF.

        PERFORM  f_close_batch_session       USING    lv_nbr_btc
                                                      lv_nbr_doc.

        CLEAR                          lv_flag_toggle.
        MOVE     gc_x               TO lv_flag_toggle.

        CLEAR    git_bdcdata[].
        CLEAR    gv_flag_err_vldt.
        CLEAR    lv_flag_ic.
        CLEAR    lv_newbs_old.
        CLEAR    lv_newbk_old.
        CLEAR    lv_tcode.

        CLEAR                          gwa_bgr00.
        MOVE     lwa_rfbibl00       TO gwa_bgr00.

        ADD      1 TO lv_nbr_btc.

        PERFORM  f_set_batch_session         CHANGING gwa_bgr00.

        CLEAR :  lc_vendor_wrbtr,
                 lc_reference_xblnr.

*eject
* Document header record
      WHEN   '1'.

        IF     ( lv_flag_toggle     IS INITIAL ).

          PERFORM  f_format_bdc_save_doc     USING    lv_tcode
                                                      lv_nbr_btc
                                                      lv_nbr_doc
                                                      lv_flag_ic.
          CLEAR :  lc_vendor_wrbtr,
                   lc_reference_xblnr.

        ENDIF.

        CLEAR    lv_flag_toggle.

        CLEAR    git_bdcdata[].
        CLEAR    gv_flag_err_vldt.
        CLEAR    lv_flag_ic.
        CLEAR    lv_newbs_old.
        CLEAR    lv_newbk_old.

        CLEAR                          lwa_bbkpf.
        MOVE     lwa_rfbibl00       TO lwa_bbkpf.

        CLEAR                          lv_tcode.
        MOVE     lwa_bbkpf-tcode    TO lv_tcode.

        ADD      1 TO lv_nbr_doc.

        PERFORM  f_format_bdc_next_header    USING    lwa_bbkpf
                                                      lv_nbr_btc
                                                      lv_nbr_doc.

        CLEAR                          lv_newbk_old.
        MOVE     lwa_bbkpf-bukrs    TO lv_newbk_old.

* IMP. Need company code for checking if with holding tax exists for vendor and company code.
        CLEAR : gv_company_code.
        gv_company_code = lwa_bbkpf-bukrs.

        lc_reference_xblnr = lwa_bbkpf-xblnr.

*eject
* Document line item record
      WHEN   '2'.

        CLEAR                          lwa_bbseg.
        MOVE     lwa_rfbibl00       TO lwa_bbseg.

        CLEAR    lv_newbk.

        IF   ( ( lwa_bbseg-newbs    EQ '21' ) OR
               ( lwa_bbseg-newbs    EQ '31' )    ).         "vendor item
        ELSE.

          IF ( ( lwa_bbseg-newbk    IS NOT INITIAL  ) AND
               ( lwa_bbseg-newbk    NE lv_newbk_old )     ).
            CLEAR                      lv_newbk.
            MOVE lwa_bbseg-newbk    TO lv_newbk.
            CLEAR                      lv_newbk_old.
            MOVE lwa_bbseg-newbk    TO lv_newbk_old.
            CLEAR                      lv_flag_ic.
            MOVE gc_x               TO lv_flag_ic.
          ENDIF.

        ENDIF.

        PERFORM  f_format_bdc_next_item      USING    lwa_bbseg-newbs
                                                      lwa_bbseg-hkont
                                                      lv_newbk
                                                      lv_newbs_old.

        IF   ( ( lwa_bbseg-newbs    EQ '21' ) OR
               ( lwa_bbseg-newbs    EQ '31' )    ).         "vendor Item

          lc_vendor_wrbtr = lwa_bbseg-wrbtr.

          PERFORM  f_format_bdc_vendor_item  USING    lwa_bbseg.

        ELSE.                                           "accounting Item

          PERFORM  f_format_bdc_accntng_item USING    lwa_bbseg.

        ENDIF.

        lv_newbs_old = lwa_bbseg-newbs.

*eject
      WHEN   OTHERS.

        PERFORM  f_error_in_process
                                 USING gv_nbr_file
                                       lv_nbr_btc
                                       lv_nbr_doc
                                       0
                                       space
                                       space
                                       0
                                       text-151
                                       text-159
                                       lwa_rfbibl00+0(1)
                                       space.

    ENDCASE.

    IF   ( gv_flag_err_proc     IS NOT INITIAL ).
      CLEAR                            lv_flag_toggle.
      MOVE     gc_x                 TO lv_flag_toggle.
      EXIT.
    ENDIF.

    CLEAR  lwa_rfbibl00.
  ENDLOOP.

  IF     ( lv_flag_toggle     IS INITIAL ).

    PERFORM  f_format_bdc_save_doc           USING    lv_tcode
                                                      lv_nbr_btc
                                                      lv_nbr_doc
                                                      lv_flag_ic.

  ENDIF.

  PERFORM  f_close_batch_session             USING    lv_nbr_btc
                                                      lv_nbr_doc.

ENDFORM.                    " f_process_rfbibl00
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_bdc_next_header
*&---------------------------------------------------------------------*
*       Format BDC - code the next document header into the BDC table
*----------------------------------------------------------------------*
FORM f_format_bdc_next_header
  USING    iwa_bbkpf TYPE ty_wa_bbkpf
           iv_nbr_btc    TYPE numc5
           iv_nbr_doc    TYPE numc5.

  DATA:    lv_bldat_d TYPE sydatum,
           lv_budat_d TYPE sydatum,
           lv_bldat_c TYPE char10,
           lv_budat_c TYPE char10.

  DATA:    lv_rc           TYPE numc5.

  DATA:    lwa_bdcmsg      TYPE ty_wa_bdcmsg.

* Convert the dates to character format
  CLEAR                        lv_bldat_d.
  MOVE     iwa_bbkpf-bldat  TO lv_bldat_d.
  CLEAR                        lv_budat_d.
  MOVE     iwa_bbkpf-budat  TO lv_budat_d.
  CLEAR                        lv_bldat_c.
  WRITE    lv_bldat_d       TO lv_bldat_c  DD/MM/YYYY.
  CLEAR                        lv_budat_c.
  WRITE    lv_budat_d       TO lv_budat_c  DD/MM/YYYY.

* Code the header transaction
  PERFORM  bdc_screen   USING     'SAPMF05A'     '0100'.
  PERFORM  bdc_field    USING     'BKPF-BLDAT'   lv_bldat_c.
  PERFORM  bdc_field    USING     'BKPF-BLART'   iwa_bbkpf-blart.
  PERFORM  bdc_field    USING     'BKPF-BUKRS'   iwa_bbkpf-bukrs.
  PERFORM  bdc_field    USING     'BKPF-BUDAT'   lv_budat_c.
  PERFORM  bdc_field    USING     'BKPF-WAERS'   iwa_bbkpf-waers.
  PERFORM  bdc_field    USING     'BKPF-XBLNR'   iwa_bbkpf-xblnr.
  PERFORM  bdc_field    USING     'BKPF-BKTXT'   iwa_bbkpf-bktxt.

*eject
* Validate the accounting document header company code
  IF     ( iwa_bbkpf-bukrs          IS INITIAL ).

    CLEAR                              gv_flag_err_vldt.
    MOVE     gc_x                   TO gv_flag_err_vldt.

    CLEAR    lv_rc.

    CLEAR                              lwa_bdcmsg.
    MOVE     gc_msgid               TO lwa_bdcmsg-msgid.
    MOVE     gc_e                   TO lwa_bdcmsg-msgtyp.
    MOVE     gc_msgnr               TO lwa_bdcmsg-msgnr.
    MOVE     text-301               TO lwa_bdcmsg-msgv1.

    PERFORM  f_error_in_data     USING gv_nbr_file
                                       iv_nbr_btc
                                       iv_nbr_doc
                                       lv_rc
                                       lwa_bdcmsg-msgid
                                       lwa_bdcmsg-msgtyp
                                       lwa_bdcmsg-msgnr
                                       lwa_bdcmsg-msgv1
                                       lwa_bdcmsg-msgv2
                                       lwa_bdcmsg-msgv3
                                       lwa_bdcmsg-msgv4.

  ENDIF.

ENDFORM.                    " f_format_bdc_next_header
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_bdc_next_item
*&---------------------------------------------------------------------*
*       Format BDC - code the next line item into the BDC table
*----------------------------------------------------------------------*
FORM f_format_bdc_next_item
  USING  iv_newbs     TYPE newbs
         iv_newko     TYPE hkont
         iv_newbk     TYPE newbk
         iv_newbs_old TYPE newbs.

  PERFORM  bdc_field    USING     'RF05A-NEWBS'  iv_newbs.
  PERFORM  bdc_field    USING     'RF05A-NEWKO'  iv_newko.
  PERFORM  bdc_field    USING     'RF05A-NEWBK'  iv_newbk.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   '/00'.


* logic for With Holding Tax Pop-up issue
  IF ( ( iv_newbs_old = '21'  ) OR
       ( iv_newbs_old = '31'  )  ).
    IF gv_vendor_tax_chk IS NOT INITIAL AND
       gv_company_code   IS NOT INITIAL.

      CLEAR : gwa_lfbw_chk.
      SELECT SINGLE *
        FROM lfbw
        INTO gwa_lfbw_chk
        WHERE lifnr = gv_vendor_tax_chk AND
              bukrs = gv_company_code   AND
              wt_subjct = 'X'.
      IF sy-subrc = 0.
        CLEAR:  gv_vendor_tax_chk,
                gv_company_code.
        PERFORM  bdc_screen USING     'SAPLFWTD'     '0100'.
        PERFORM  bdc_field  USING     'BDC_CURSOR'   'WITH_ITEM-WT_WITHCD(01)'.
        PERFORM  bdc_field  USING     'BDC_OKCODE'   '=GO'.

      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR : gwa_lfbw_chk.

* The coding block will appear after a G/L accounting line is entered
  IF ( ( iv_newbs_old NE '21'  ) AND
       ( iv_newbs_old NE '31'  ) AND
       ( iv_newbs_old NE space )     ).

    PERFORM  bdc_screen USING     'SAPLKACB'     '0002'.
    PERFORM  bdc_field  USING     'BDC_OKCODE'   '/8'.

  ENDIF.

ENDFORM.                    " f_format_bdc_next_item
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_bdc_vendor_item
*&---------------------------------------------------------------------*
*       Format BDC - code a vendor account line item into the BDC table
*----------------------------------------------------------------------*
FORM f_format_bdc_vendor_item
  USING    iwa_bbseg TYPE ty_wa_bbseg.

  DATA:    lv_lifnr  TYPE lifnr,
           lv_ktokk  TYPE ktokk.

* Select the vendor account group.
  CLEAR                                lv_lifnr.
  MOVE     iwa_bbseg-hkont          TO lv_lifnr.

* Pass vendor no for with holding tax check.
  CLEAR : gv_vendor_tax_chk.
  gv_vendor_tax_chk = iwa_bbseg-hkont.

  CLEAR    lv_ktokk.
  SELECT   SINGLE ktokk
    INTO   lv_ktokk
    FROM   lfa1
   WHERE   lifnr = lv_lifnr.
  IF ( sy-subrc NE 0 ).
    CLEAR  lv_ktokk.
  ENDIF.

  IF   ( lv_ktokk IN gr_ktokk_ot ).

    PERFORM  bdc_screen USING     'SAPLFCPD'     '0100'.
    PERFORM  bdc_field  USING     'BSEC-NAME1'   iwa_bbseg-name1.
    PERFORM  bdc_field  USING     'BSEC-NAME2'   iwa_bbseg-name2.
    PERFORM  bdc_field  USING     'BSEC-STRAS'   iwa_bbseg-stras.
    PERFORM  bdc_field  USING     'BSEC-ORT01'   iwa_bbseg-ort01.
    PERFORM  bdc_field  USING     'BSEC-PSTLZ'   iwa_bbseg-pstlz.

  ENDIF.

* Create vendor item screen
  PERFORM  bdc_screen   USING     'SAPMF05A'     '0302'.
  PERFORM  bdc_field    USING     'BSEG-WRBTR'   iwa_bbseg-wrbtr.
  PERFORM  bdc_field    USING     'BSEG-MWSKZ'   iwa_bbseg-mwskz.
  PERFORM  bdc_field    USING     'BSEG-ZTERM'   iwa_bbseg-zterm.
  PERFORM  bdc_field    USING     'BSEG-ZUONR'   iwa_bbseg-zuonr.
  PERFORM  bdc_field    USING     'BSEG-SGTXT'   iwa_bbseg-sgtxt.
  PERFORM  bdc_field    USING     'BSEG-ZLSCH'   iwa_bbseg-zlsch. "Add by RBHATT 04-APR-2012 TR DECK905448

* Enter the route code
  IF   ( iwa_bbseg-xref3 IS NOT INITIAL ).

    PERFORM  bdc_field  USING     'BDC_OKCODE'   '=ZK'.

    PERFORM  bdc_screen USING     'SAPMF05A'     '0332'.
    PERFORM  bdc_field  USING     'BSEG-XREF3'   iwa_bbseg-xref3.

  ENDIF.

ENDFORM.                    " f_format_bdc_vendor_item
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_bdc_accntng_item
*&---------------------------------------------------------------------*
*       Format BDC - code a G/L account line item into the BDC table
*----------------------------------------------------------------------*
FORM f_format_bdc_accntng_item
  USING    iwa_bbseg TYPE ty_wa_bbseg.

* Screen: Create G/L account item
  PERFORM  bdc_screen   USING     'SAPMF05A'     '0300'.
  PERFORM  bdc_field    USING     'BSEG-WRBTR'   iwa_bbseg-wrbtr.
  PERFORM  bdc_field    USING     'BSEG-MWSKZ'   iwa_bbseg-mwskz.
  PERFORM  bdc_field    USING     'BSEG-SGTXT'   iwa_bbseg-sgtxt.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   'ZK'.

* coding block screen
  PERFORM  bdc_screen   USING     'SAPLKACB'     '0002'.
  PERFORM  bdc_field    USING     'COBL-KOSTL'   iwa_bbseg-kostl.
  PERFORM  bdc_field    USING     'COBL-AUFNR'   iwa_bbseg-aufnr.
*  Start of changes by AHAMDT for CHG0188302
*  PERFORM  bdc_field    USING     'COBL-ZZREF'   iwa_bbseg-zzref.  "Add by RBHATT 04-APR-2012 TR DECK905448
*  End of changes by AHAMDT for CHG0188302
  PERFORM  bdc_field    USING     'COBL-PS_PSP_PNR'
                                                 iwa_bbseg-projk.
  PERFORM  bdc_field    USING     'COBL-NPLNR'   iwa_bbseg-nplnr.
  PERFORM  bdc_field    USING     'COBL-VORNR'   iwa_bbseg-vornr.
  PERFORM  bdc_field    USING     'COBL-ZZLOC'   iwa_bbseg-zzloc.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   '=ENTE'.

  PERFORM  bdc_screen   USING     'SAPMF05A'     '0330'.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   'ZK'.

  PERFORM  bdc_screen   USING     'SAPMF05A'     '0300'.

ENDFORM.                    " f_format_bdc_accntng_item
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_bdc_save_doc
*&---------------------------------------------------------------------*
*       Format BDC - save the accounting document
*----------------------------------------------------------------------*
FORM f_format_bdc_save_doc
  USING    iv_tcode      TYPE tcode
           iv_nbr_btc    TYPE numc5
           iv_nbr_doc    TYPE numc5
           iv_flag_ic    TYPE flag.

  IF   ( git_bdcdata[]              IS INITIAL ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       iv_nbr_btc
                                       iv_nbr_doc
                                       0
                                       space
                                       space
                                       0
                                       text-151
                                       text-152
                                       space
                                       space.

    RETURN.
  ENDIF.

  IF   ( iv_tcode                   IS INITIAL ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       iv_nbr_btc
                                       iv_nbr_doc
                                       0
                                       space
                                       space
                                       0
                                       text-151
                                       text-153
                                       space
                                       space.

    RETURN.
  ENDIF.

*eject
* Complete the BDC data for the document

  PERFORM  bdc_field    USING     'BDC_OKCODE'   '=AB'.

  PERFORM  bdc_screen   USING     'SAPLKACB'     '0002'.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   '/8'.

  IF     ( iv_flag_ic               IS INITIAL ).

    PERFORM  bdc_screen USING     'SAPMF05A'     '0700'.
    PERFORM  bdc_field  USING     'BDC_OKCODE'   'BU'.

  ELSE.

    PERFORM  bdc_screen USING     'SAPMF05A'     '0701'.
    PERFORM  bdc_field  USING     'BDC_OKCODE'   'BU'.

  ENDIF.

  IF     ( gv_flag_err_vldt     IS NOT INITIAL ).
    RETURN.
  ENDIF.

  IF lc_reference_xblnr IS NOT INITIAL.
    READ TABLE lit_dup_entries INTO lwa_dup_entries
                               WITH KEY xblnr = lc_reference_xblnr
                                        wrbtr = lc_vendor_wrbtr.
    IF sy-subrc = 0.
      lc_dup_flag =  gc_x.
    ELSE.
      CLEAR lc_dup_flag.
    ENDIF.
  ENDIF.

  IF lc_dup_flag NE gc_x.
    IF     ( gwa_bdccntl-sesn_create  IS INITIAL ).

* Call transaction

      PERFORM  f_call_transaction  USING iv_tcode
                                         iv_nbr_btc
                                         iv_nbr_doc.

    ELSE.

* Insert data into the BDC batch session

      PERFORM  f_insert_bdcdata    USING iv_tcode
                                         iv_nbr_btc
                                         iv_nbr_doc.

    ENDIF.

  ELSE.

    ADD      1                        TO gv_count_err_data.
    CLEAR                                gv_flag_err_data.
    MOVE     gc_x                     TO gv_flag_err_data.
    CLEAR                                gv_flag_err_mstr.
    MOVE     gc_x                     TO gv_flag_err_mstr.

    CLEAR : lwa_error_dup.
    lwa_error_dup-msgv1 = 'Duplication exists for same Reference No and Amount'(e01).

    CLEAR                                lwa_errs_data_dup.
    MOVE     gv_nbr_file              TO lwa_errs_data_dup-nbr_file.
    MOVE     iv_nbr_btc               TO lwa_errs_data_dup-nbr_btc.
    MOVE     iv_nbr_doc               TO lwa_errs_data_dup-nbr_doc.
    MOVE     '8'                      TO lwa_errs_data_dup-rc.
    MOVE     ''                       TO lwa_errs_data_dup-msgid.
    MOVE     lwa_error_dup-msgtyp     TO lwa_errs_data_dup-msgty.
    MOVE     '0'                      TO lwa_errs_data_dup-msgno.
    MOVE     lwa_error_dup-msgv1      TO lwa_errs_data_dup-text.
    APPEND   lwa_errs_data_dup            TO git_errs_data.

    CLEAR : lwa_error_dup,
            lwa_errs_data_dup.

  ENDIF.

ENDFORM.                    " f_format_bdc_save_doc
*eject
*&---------------------------------------------------------------------*
*&      FORM  bdc_screen
*&---------------------------------------------------------------------*
*       Format BDC - add an entry to table BDCDATA with screen info
*-----------------------------------------------------------------------
FORM bdc_screen
  USING    iv_program TYPE any
           iv_dynpro  TYPE any.

  CLEAR                                gwa_bdcdata.
  MOVE     iv_program               TO gwa_bdcdata-program.
  MOVE     iv_dynpro                TO gwa_bdcdata-dynpro.
  MOVE     gc_x                     TO gwa_bdcdata-dynbegin.
  APPEND   gwa_bdcdata              TO git_bdcdata.

ENDFORM.                    " bdc_screen
*eject
*&---------------------------------------------------------------------*
*&      FORM  bdc_field
*&---------------------------------------------------------------------*
*       Format BDC - add an entry to table BDCDATA with field info
*-----------------------------------------------------------------------
FORM  bdc_field
  USING    iv_fnam    TYPE any
           iv_fval    TYPE any.

  IF     ( iv_fnam NE 'BDC_OKCODE' ).

    IF ( ( iv_fval EQ space ) OR
         ( iv_fval EQ gc_fs )    ).
      RETURN.
    ENDIF.

  ENDIF.

  CLEAR                                gwa_bdcdata.
  MOVE     iv_fnam                  TO gwa_bdcdata-fnam.
  MOVE     iv_fval                  TO gwa_bdcdata-fval.
  APPEND   gwa_bdcdata              TO git_bdcdata.

ENDFORM.                    "  bdc_field
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_batch_session
*&---------------------------------------------------------------------*
*       Set the batch session input structure data
*----------------------------------------------------------------------*
FORM f_set_batch_session
  CHANGING cwa_bgr00     TYPE ty_wa_bgr00.

* Set the group name
  IF     ( gwa_bdccntl-sesn_group   IS NOT INITIAL ).
    CLEAR                              cwa_bgr00-group.
    MOVE   gwa_bdccntl-sesn_group   TO cwa_bgr00-group.
  ENDIF.

  IF   ( ( cwa_bgr00-group(1)       EQ gc_fs ) OR
         ( cwa_bgr00-group          EQ space )    ).
    CLEAR                              cwa_bgr00-group.
    MOVE   gc_sesn_group            TO cwa_bgr00-group.
  ENDIF.

* Set the start date
  IF     ( gwa_bdccntl-sesn_start   IS NOT INITIAL ).
    CLEAR                              cwa_bgr00-start.
    MOVE   gwa_bdccntl-sesn_start   TO cwa_bgr00-start.
  ELSE.
    CLEAR                              cwa_bgr00-start.
    cwa_bgr00-start = sy-datum - 1.
  ENDIF.

* Set the keep flag
  IF     ( gwa_bdccntl-sesn_xkeep   CS gc_y ).
    CLEAR                              cwa_bgr00-xkeep.
    MOVE   gc_x                     TO cwa_bgr00-xkeep.
  ELSEIF ( gwa_bdccntl-sesn_xkeep   CS gc_n ).
    CLEAR                              cwa_bgr00-xkeep.
  ENDIF.

  IF   ( ( cwa_bgr00-xkeep          NE gc_x  ) OR
         ( cwa_bgr00-xkeep          NE space )    ).
    CLEAR                              cwa_bgr00-xkeep.
    MOVE   gc_x                     TO cwa_bgr00-xkeep.
  ENDIF.

* Set the user name
  IF     ( gwa_bdccntl-sesn_usnam   IS NOT INITIAL ).
    CLEAR                              cwa_bgr00-usnam.
    MOVE   gwa_bdccntl-sesn_usnam   TO cwa_bgr00-usnam.
  ENDIF.

  IF   ( ( cwa_bgr00-usnam(1)       EQ gc_fs ) OR
         ( cwa_bgr00-usnam          EQ space )    ).
    CLEAR                              cwa_bgr00-usnam.
    MOVE   sy-uname                 TO cwa_bgr00-usnam.
  ENDIF.

ENDFORM.                    " f_set_batch_session
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_batch_session
*&---------------------------------------------------------------------*
*       Open a BDC batch session
*----------------------------------------------------------------------*
FORM f_open_batch_session
  USING    iv_nbr_btc    TYPE numc5
           iv_nbr_doc    TYPE numc5.

  DATA:    lv_rc         TYPE numc5.

  CLEAR    lv_rc.

  IF     ( gwa_bdccntl-sesn_open    IS NOT INITIAL ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       iv_nbr_btc
                                       iv_nbr_doc
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-151
                                       text-155
                                       space
                                       space.

    RETURN.
  ENDIF.

  IF     ( cb_test              IS NOT INITIAL ).
    CLEAR                              gwa_bdccntl-sesn_open.
    MOVE     gc_x                   TO gwa_bdccntl-sesn_open.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client              = sy-mandt
      group               = gwa_bgr00-group
      holddate            = gwa_bgr00-start
      keep                = gwa_bgr00-xkeep
      user                = gwa_bgr00-usnam
    EXCEPTIONS
      client_invalid      = 1
      destination_invalid = 2
      group_invalid       = 3
      group_is_locked     = 4
      holddate_invalid    = 5
      internal_error      = 6
      queue_error         = 7
      running             = 8
      system_lock_error   = 9
      user_invalid        = 10
      OTHERS              = 11.

  lv_rc = sy-subrc.

*eject
  IF ( lv_rc EQ 0 ).
    CLEAR                              gwa_bdccntl-sesn_open.
    MOVE     gc_x                   TO gwa_bdccntl-sesn_open.
  ELSE.

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       iv_nbr_btc
                                       iv_nbr_doc
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-151
                                       text-156
                                       lv_rc
                                       gwa_bgr00.

    RETURN.
  ENDIF.

ENDFORM.                    " f_open_batch_session
*eject
*&---------------------------------------------------------------------*
*&      Form  f_close_batch_session
*&---------------------------------------------------------------------*
*       Close a BDC batch session
*----------------------------------------------------------------------*
FORM f_close_batch_session
  USING    iv_nbr_btc    TYPE numc5
           iv_nbr_doc    TYPE numc5.

  DATA:    lv_rc         TYPE numc5.

  CLEAR    lv_rc.

  IF   ( gwa_bdccntl-sesn_open      IS INITIAL ).
    RETURN.
  ENDIF.

  IF     ( cb_test              IS NOT INITIAL ).
    CLEAR                              gwa_bdccntl-sesn_open.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1
      queue_error = 2
      OTHERS      = 3.

  lv_rc = sy-subrc.

  IF ( lv_rc EQ 0 ).
    CLEAR                              gwa_bdccntl-sesn_open.
  ELSE.

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       iv_nbr_btc
                                       iv_nbr_doc
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-151
                                       text-157
                                       lv_rc
                                       space.

    RETURN.
  ENDIF.

ENDFORM.                    " f_close_batch_session
*eject
*&---------------------------------------------------------------------*
*&      Form  f_insert_bdcdata
*&---------------------------------------------------------------------*
*       Insert data into the BDC batch session
*----------------------------------------------------------------------*
FORM f_insert_bdcdata
  USING    iv_tcode      TYPE tcode
           iv_nbr_btc    TYPE numc5
           iv_nbr_doc    TYPE numc5.

  DATA:    lv_rc         TYPE numc5.

  IF       ( gwa_bdccntl-sesn_open  IS INITIAL ).

    PERFORM  f_open_batch_session
                                 USING iv_nbr_btc
                                       iv_nbr_doc.

  ENDIF.

  IF     ( cb_test              IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode            = iv_tcode
    TABLES
      dynprotab        = git_bdcdata
    EXCEPTIONS
      internal_error   = 1
      not_open         = 2
      queue_error      = 3
      tcode_invalid    = 4
      printing_invalid = 5
      posting_invalid  = 6
      OTHERS           = 7.

  lv_rc = sy-subrc.

  IF ( lv_rc NE 0 ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       iv_nbr_btc
                                       iv_nbr_doc
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-151
                                       text-158
                                       lv_rc
                                       space.

    RETURN.
  ENDIF.

ENDFORM.                    " f_insert_bdcdata
*eject
*&---------------------------------------------------------------------*
*&      Form  f_call_transaction
*&---------------------------------------------------------------------*
*       Call the transaction
*----------------------------------------------------------------------*
FORM f_call_transaction
  USING    iv_tcode        TYPE tcode
           iv_nbr_btc      TYPE numc5
           iv_nbr_doc      TYPE numc5.

  DATA:    lv_rc           TYPE numc5,
           lv_msgtyp       TYPE char1.

  DATA:    lwa_bdcopts     TYPE ctu_params,
           lwa_bdcmsg      TYPE ty_wa_bdcmsg,
           lwa_docs_posted TYPE ty_wa_msgs.

  IF     ( cb_test              IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lwa_bdcopts.
  MOVE     gwa_bdccntl-dismode      TO lwa_bdcopts-dismode.
  MOVE     gwa_bdccntl-updmode      TO lwa_bdcopts-updmode.
  MOVE     gwa_bdccntl-defsize      TO lwa_bdcopts-defsize.

* Call the transaction
  CLEAR    git_bdcmsg[].

  CALL     TRANSACTION iv_tcode
                 USING git_bdcdata
          OPTIONS FROM lwa_bdcopts
         MESSAGES INTO git_bdcmsg.

  lv_rc = sy-subrc.

*eject
  CLEAR                                lv_msgtyp.

  CLEAR                                lwa_bdcmsg.
  LOOP AT  git_bdcmsg             INTO lwa_bdcmsg
                                 WHERE msgtyp CA 'aAeE'.

    CLEAR                              lv_msgtyp.
    MOVE     lwa_bdcmsg-msgtyp      TO lv_msgtyp.

    PERFORM  f_error_in_data     USING gv_nbr_file
                                       iv_nbr_btc
                                       iv_nbr_doc
                                       lv_rc
                                       lwa_bdcmsg-msgid
                                       lwa_bdcmsg-msgtyp
                                       lwa_bdcmsg-msgnr
                                       lwa_bdcmsg-msgv1
                                       lwa_bdcmsg-msgv2
                                       lwa_bdcmsg-msgv3
                                       lwa_bdcmsg-msgv4.

    CLEAR  lwa_bdcmsg.
  ENDLOOP.

  IF ( ( lv_rc NE 0 ) AND ( lv_msgtyp IS INITIAL ) ).

    CLEAR                              lwa_bdcmsg.
    LOOP AT  git_bdcmsg           INTO lwa_bdcmsg.

      CLEAR                            lv_msgtyp.
      MOVE     gc_e                 TO lv_msgtyp.

      PERFORM  f_error_in_data   USING gv_nbr_file
                                       iv_nbr_btc
                                       iv_nbr_doc
                                       lv_rc
                                       lwa_bdcmsg-msgid
                                       lwa_bdcmsg-msgtyp
                                       lwa_bdcmsg-msgnr
                                       lwa_bdcmsg-msgv1
                                       lwa_bdcmsg-msgv2
                                       lwa_bdcmsg-msgv3
                                       lwa_bdcmsg-msgv4.

      CLEAR  lwa_bdcmsg.
    ENDLOOP.

  ENDIF.

*eject
  IF ( ( lv_rc EQ 0 ) AND ( lv_msgtyp IS INITIAL ) ).

    CLEAR                              lwa_docs_posted.
    MOVE     gv_nbr_file            TO lwa_docs_posted-nbr_file.
    MOVE     iv_nbr_btc             TO lwa_docs_posted-nbr_btc.
    MOVE     iv_nbr_doc             TO lwa_docs_posted-nbr_doc.
    MOVE     text-r36               TO lwa_docs_posted-text.
    APPEND   lwa_docs_posted        TO git_docs_posted.

  ELSE.

    PERFORM  f_insert_bdcdata    USING iv_tcode
                                       iv_nbr_btc
                                       iv_nbr_doc.

  ENDIF.

ENDFORM.                    " f_call_transaction
*eject
*&---------------------------------------------------------------------*
*&      Form  f_archive_file
*&---------------------------------------------------------------------*
*        Archive the input file
*----------------------------------------------------------------------*
FORM f_archive_file
  TABLES   cit_file_stats     STRUCTURE gwa_file_stats.

  DATA:    lwa_file_stats     TYPE ty_wa_file_stats,
           lwa_xparam         TYPE ty_wa_xparam.

  DATA:    lwa_return         TYPE bapireturn.

  DATA:    lv_source_dir      TYPE btcxpgpar,
           lv_target_dir      TYPE btcxpgpar,
           lv_source_fname    TYPE btcxpgpar,
           lv_target_fname    TYPE btcxpgpar,
           lv_filename_ff_in  TYPE text1024, "filename in - path + name
           lv_filename_fn_in  TYPE text128,  "filename in - name
           lv_filepath_out    TYPE text1024, "filepath out
           lv_filename_ff_out TYPE text1024, "filename out - path + name
           lv_filename_fn_out TYPE text128,  "filename out - name
           lv_text25          TYPE text25,
           lv_key2            TYPE zparamkey,
           lv_apnd            TYPE char4,
           lv_source          TYPE text1024,
           lv_target          TYPE text1024,
           lv_data            TYPE string,
           lv_rc              TYPE numc5.

  IF   ( ( cb_test IS NOT INITIAL ) OR
         ( rb_appl     IS INITIAL )    ).
    RETURN.
  ENDIF.

* Read the file stats
  CLEAR                                lwa_file_stats.
  READ     TABLE cit_file_stats   INTO lwa_file_stats
                              WITH KEY nbr_file = gv_nbr_file.
  IF ( sy-subrc NE 0 ).
    CLEAR        lwa_file_stats.

    IF ( gv_flag_err_proc           IS INITIAL ).

      PERFORM  f_error_in_process
                                 USING gv_nbr_file
                                       0
                                       0
                                       0
                                       space
                                       space
                                       0
                                       text-171
                                       gv_nbr_file
                                       'Archive'
                                       space.

    ENDIF.

    RETURN.
  ENDIF.

*eject
* Read the input filepath
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_in.
  IF ( sy-subrc NE 0 ).
    CLEAR        lwa_xparam.
  ENDIF.

* Set the source directory and filename
  CLEAR                                lv_source_dir.
  MOVE     lwa_xparam-value1        TO lv_source_dir.

  CLEAR                                lv_source_fname.
  MOVE     lwa_file_stats-filename_fn
                                    TO lv_source_fname.

* Read either the archive or error filepath
  CLEAR                                lv_key2.
  CLEAR                                lv_apnd.
  IF   ( ( gv_flag_err_data         IS INITIAL ) AND
         ( gv_flag_err_proc         IS INITIAL )     ).
    MOVE   gc_filepath_arch         TO lv_key2.
    MOVE   gc_extension_arch        TO lv_apnd.
  ELSE.
    MOVE   gc_filepath_err          TO lv_key2.
    MOVE   gc_extension_err         TO lv_apnd.
  ENDIF.

  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = lv_key2.
  IF ( sy-subrc NE 0 ).
    CLEAR        lwa_xparam.
  ENDIF.

*eject
* Set the archive filename
  CLEAR                                lv_filename_ff_in.
  CLEAR                                lv_filename_fn_in.
  MOVE     lv_source_fname          TO lv_filename_fn_in.
  CLEAR                                lv_filename_ff_out.
  CLEAR                                lv_filename_fn_out.

  CLEAR                                lv_filepath_out.
  IF     ( lwa_xparam-value1    IS NOT INITIAL ).
    MOVE   lwa_xparam-value1        TO lv_filepath_out.
  ELSE.
    MOVE   lv_source_dir            TO lv_filepath_out.
  ENDIF.

  CLEAR                                lv_text25.
  IF     ( lwa_xparam-value2    IS NOT INITIAL ).
    MOVE   lwa_xparam-value2        TO lv_text25.
  ELSE.
    MOVE   lv_apnd                  TO lv_text25.
  ENDIF.

  PERFORM  f_generate_filename
                              USING    lv_filename_ff_in
                                       lv_filename_fn_in
                                       lv_filepath_out
                                       gc_x   "delete existing extension
                                       lv_text25            "append text
                              CHANGING lv_filename_ff_out
                                       lv_filename_fn_out.

* Set the target directory and filename
  CLEAR                                lv_target_dir.
  MOVE     lv_filepath_out          TO lv_target_dir.

  CLEAR                                lv_target_fname.
  MOVE     lv_filename_fn_out       TO lv_target_fname.

  IF   ( ( lv_source_dir            IS INITIAL ) OR
         ( lv_target_dir            IS INITIAL ) OR
         ( lv_source_fname          IS INITIAL ) OR
         ( lv_target_fname          IS INITIAL )    ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       0
                                       space
                                       space
                                       0
                                       text-181
                                       lv_target_dir
                                       lv_target_fname
                                       space.

    RETURN.
  ENDIF.

* Archive the input file
* Source path - Directory + file name
  CLEAR: lv_source.
  CONCATENATE lv_source_dir
              lv_source_fname
         INTO lv_source.

* Target Path - Directory + file name
  CLEAR: lv_target.
  CONCATENATE lv_target_dir
              lv_target_fname
         INTO lv_target.

  OPEN DATASET lv_source FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc IS NOT INITIAL.
    lv_rc = sy-subrc.
    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-181
                                       lv_target_dir
                                       lv_target_fname
                                       space.
    RETURN.
  ENDIF.

  OPEN DATASET lv_target FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc IS NOT INITIAL.
    lv_rc = sy-subrc.
    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-181
                                       lv_target_dir
                                       lv_target_fname
                                       space.
    RETURN.
  ENDIF.

* copy the file from source to destination
  DO.
    READ DATASET lv_source INTO lv_data.
    IF sy-subrc EQ 0.
      TRANSFER lv_data TO lv_target.
      CLEAR lv_data.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

* Delete the source file after copy
  DELETE DATASET lv_source.

* Close all open dataset
  CLOSE DATASET: lv_source,
                 lv_target.
**eject
** Archive the data via a move file
*
*  CALL FUNCTION 'ZFI_FILE_HANDLE'
*    EXPORTING
*      i_source_dir        = lv_source_dir
*      i_target_dir        = lv_target_dir
*      i_source_fname      = lv_source_fname
*      i_target_fname      = lv_target_fname
*      i_command           = 'M'
*      i_date_time_stamp   = space
*      i_rename_arc_to_new = space
*    IMPORTING
*      e_return            = lwa_return.
*
*  IF   ( lwa_return-type CA 'aAeE' ).
*
*    PERFORM  f_error_in_process  USING gv_nbr_file
*                                       0
*                                       0
*                                       0
*                                       space
*                                       space
*                                       0
*                                       lwa_return-message
*                                       'Archive'
*                                       space
*                                       space.
*
*    RETURN.
*  ENDIF.

ENDFORM.                    " f_archive_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_generate_filename
*&---------------------------------------------------------------------*
*       Generate a new filename from an old filename
*----------------------------------------------------------------------*
FORM f_generate_filename
  USING    iv_filename_ff_in  TYPE text1024
           iv_filename_fn_in  TYPE text128
           iv_filepath_out    TYPE text1024
           iv_flag_del_extn   TYPE flag
           iv_filename_extn   TYPE any
  CHANGING cv_filename_ff_out TYPE text1024
           cv_filename_fn_out TYPE text128.

  DATA:    lv_filename_ff     TYPE text1024,
           lv_filename_fn     TYPE text128,
           lv_filename_fn_in  TYPE text1024, "assigned full filename
           lv_fdpos           TYPE syfdpos.

  CLEAR    cv_filename_ff_out.
  CLEAR    cv_filename_fn_out.

  CLEAR                                lv_filename_fn_in.
  MOVE       iv_filename_fn_in      TO lv_filename_fn_in.

* If the input filename is not specified, extract it from the full file
  IF       ( iv_filename_fn_in      IS INITIAL ).

    IF     ( iv_filename_ff_in      IS INITIAL ).
      RETURN.
    ENDIF.

    CLEAR                              lv_filename_fn_in.
    MOVE     iv_filename_ff_in      TO lv_filename_fn_in.

    DO       12 TIMES.
      IF   ( lv_filename_fn_in      CS '\' ).
        lv_fdpos = sy-fdpos.
        ADD     1 TO lv_fdpos.
        SHIFT   lv_filename_fn_in LEFT BY lv_fdpos PLACES.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDIF.

*eject
  CLEAR                                lv_filename_fn.
  MOVE       lv_filename_fn_in      TO lv_filename_fn.

* Delete the file extension if the flag is set
  IF       ( iv_flag_del_extn   IS NOT INITIAL ).
    IF     ( lv_filename_fn_in      CS '.' ).
      lv_fdpos = sy-fdpos.
      CLEAR                            lv_filename_fn.
      MOVE   lv_filename_fn_in(lv_fdpos)
                                    TO lv_filename_fn.
    ENDIF.
  ENDIF.

* Append the new file extension if it is given
  IF     ( iv_filename_extn     IS NOT INITIAL ).
    CLEAR                              cv_filename_fn_out.
    CONCATENATE                        lv_filename_fn
                                       iv_filename_extn
                                  INTO cv_filename_fn_out.
  ELSE.
    CLEAR                              cv_filename_fn_out.
    MOVE       lv_filename_fn       TO cv_filename_fn_out.
  ENDIF.

* If a new filepath is specified, then determine the full filename
  IF       ( iv_filepath_out    IS NOT INITIAL ).
    CLEAR                              cv_filename_ff_out.
    CONCATENATE                        iv_filepath_out
                                       cv_filename_fn_out
                                  INTO cv_filename_ff_out.
    RETURN.
  ENDIF.

* If the old full filename is specified, use the old filepath
  IF       ( iv_filename_ff_in  IS NOT INITIAL ).
    IF     ( iv_filename_ff_in      CS lv_filename_fn_in ).
      lv_fdpos = sy-fdpos.
      IF   ( lv_fdpos               IS INITIAL ).
        CLEAR                          cv_filename_ff_out.
        RETURN.
      ENDIF.
      CLEAR                            lv_filename_ff.
      MOVE   iv_filename_ff_in(lv_fdpos)
                                    TO lv_filename_ff.
      CLEAR                            cv_filename_ff_out.
      CONCATENATE                      lv_filename_ff
                                       cv_filename_fn_out
                                  INTO cv_filename_ff_out.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_generate_filename
*eject
*&---------------------------------------------------------------------*
*&      Form  f_write_report
*&---------------------------------------------------------------------*
*       Write the report
*----------------------------------------------------------------------*
FORM f_write_report
  TABLES   iit_file_stats  STRUCTURE gwa_file_stats.

  DATA:    lwa_file_stats  TYPE ty_wa_file_stats,
           lwa_errs_data   TYPE ty_wa_msgs,
           lwa_errs_proc   TYPE ty_wa_msgs,
           lwa_docs_posted TYPE ty_wa_msgs.

  DATA:    lv_rept_mode    TYPE char4,
           lv_count        TYPE numc5.

* Read the file stats
  CLEAR                                lwa_file_stats.
  READ     TABLE iit_file_stats   INTO lwa_file_stats
                              WITH KEY nbr_file = gv_nbr_file.
  IF ( sy-subrc NE 0 ).
    CLEAR        lwa_file_stats.
  ENDIF.

* Write the control totals section
  NEW-PAGE.
  SKIP    2.
  WRITE: /001 sy-uline(52),
          054 text-r11,
          069 sy-uline(52).

* Report mode and error counts
  CLEAR                                lv_rept_mode.
  IF       ( cb_test IS INITIAL ).
    MOVE     text-rmp               TO lv_rept_mode.
  ELSE.
    MOVE     text-rmt               TO lv_rept_mode.
  ENDIF.

  lv_count = gv_count_err_data +
             gv_count_err_proc.

  SKIP    1.
  WRITE: /001 text-r21,
          020 lv_rept_mode,
          054 text-r22,
          068 lv_count,
          083 text-r23,
          096 gv_count_err_data,
         /080 text-r24,
          096 gv_count_err_proc.

*eject
* Input file
  SKIP    1.
  WRITE: /001 text-r31,
          013 lwa_file_stats-filename_in.

* Input record count
  CLEAR                                lv_count.
  MOVE     lwa_file_stats-cnt_rec_in
                                    TO lv_count.

  WRITE: /001 text-r32,
          016 lv_count.

* Write documents posted
  IF     ( git_docs_posted[]    IS NOT INITIAL ).

    SKIP  1.
    WRITE: /001 text-r35.
    SKIP  1.

    CLEAR                              lwa_docs_posted.
    LOOP AT  git_docs_posted      INTO lwa_docs_posted.
      WRITE: /001 text-r51,        007 lwa_docs_posted-nbr_file,
              015 text-r52,        022 lwa_docs_posted-nbr_btc,
              030 text-r53,        040 lwa_docs_posted-nbr_doc,
              048 text-r36.
      CLEAR  lwa_docs_posted.
    ENDLOOP.

  ENDIF.

*eject
* Write the errors section
  IF   ( ( gv_flag_err_data         IS INITIAL ) AND
         ( gv_flag_err_proc         IS INITIAL )     ).
    RETURN.
  ENDIF.

  SKIP    2.
  WRITE: /001 sy-uline(56),
          058 text-r41,
          065 sy-uline(56).

  IF     ( gv_flag_err_data     IS NOT INITIAL ).

    SKIP  1.
    WRITE: /001 text-r42.
    SKIP  1.

    CLEAR                              lwa_errs_data.
    LOOP AT  git_errs_data        INTO lwa_errs_data.
      WRITE: /001 text-r51,        007 lwa_errs_data-nbr_file,
              015 text-r52,        022 lwa_errs_data-nbr_btc,
              030 text-r53,        040 lwa_errs_data-nbr_doc,
              048 text-r54,        052 lwa_errs_data-rc,
              060 text-r55,        068 lwa_errs_data-msgid,
              091 text-r56,        099 lwa_errs_data-msgty,
              103 text-r57,        111 lwa_errs_data-msgno.
      WRITE: /001 lwa_errs_data-text(120).
      CLEAR  lwa_errs_data.
    ENDLOOP.

  ENDIF.

  IF     ( gv_flag_err_proc     IS NOT INITIAL ).

    SKIP  1.
    WRITE: /001 text-r43.
    SKIP  1.

    CLEAR                              lwa_errs_proc.
    LOOP AT  git_errs_proc        INTO lwa_errs_proc.
      WRITE: /001 text-r51,        007 lwa_errs_proc-nbr_file,
              015 text-r52,        022 lwa_errs_proc-nbr_btc,
              030 text-r53,        040 lwa_errs_proc-nbr_doc,
              048 text-r54,        052 lwa_errs_proc-rc,
              060 text-r55,        068 lwa_errs_proc-msgid,
              091 text-r56,        099 lwa_errs_proc-msgty,
              103 text-r57,        111 lwa_errs_proc-msgno.
      WRITE: /001 lwa_errs_proc-text(120).
      CLEAR  lwa_errs_proc.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " f_write_report
*eject
*&---------------------------------------------------------------------*
*&      Form  f_send_email
*&---------------------------------------------------------------------*
*       Send email containing run details
*----------------------------------------------------------------------*
FORM f_send_email.

  DATA:    lwa_xparam         TYPE ty_wa_xparam,
           lwa_errs           TYPE ty_wa_msgs.

  DATA:    lwa_document_data  TYPE sodocchgi1,
           lwa_packing_list   TYPE sopcklsti1,
           lit_packing_list   TYPE TABLE OF sopcklsti1,
           lwa_contents_txt   TYPE solisti1,
           lit_contents_txt   TYPE TABLE OF solisti1,
           lwa_receivers      TYPE somlreci1,
           lit_receivers      TYPE TABLE OF somlreci1.

  DATA:    lv_obj_descr       TYPE so_obj_des,
           lv_name            TYPE tdobname,
           lv_lines           TYPE syindex,
           lv_rc              TYPE numc5.

  CONSTANTS:
           lc_u               TYPE char1 VALUE 'U',
           lc_int             TYPE char3 VALUE 'INT',
           lc_o               TYPE char1 VALUE 'O',
           lc_raw             TYPE char3 VALUE 'RAW'.

  IF     ( gv_flag_err_mstr         IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR:   lit_packing_list[],         lwa_packing_list.
  CLEAR:   lit_contents_txt[],         lwa_contents_txt.
  CLEAR:   lit_receivers[],            lwa_receivers.
  CLEAR    lwa_document_data.
  CLEAR    lv_obj_descr.

* Set the email content
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY key2 = gc_email
                                       key3 = gc_email_std_txt.
  IF ( sy-subrc EQ 0 ).

    CLEAR                              lv_name.
    IF     ( gv_flag_err_mstr       IS INITIAL ).
      MOVE   lwa_xparam-value1      TO lv_name.
    ELSE. "( gv_flag_err_mstr   IS NOT INITIAL ).
      MOVE   lwa_xparam-value2      TO lv_name.
    ENDIF.

    PERFORM  f_read_text      TABLES   lit_contents_txt
                              USING    lv_name
                              CHANGING lv_obj_descr.

  ENDIF.

*eject
* Set the document data
  IF     ( lv_obj_descr             IS INITIAL ).
    MOVE   text-mre                 TO lv_obj_descr.
  ENDIF.

  CLEAR                                lwa_document_data.
  MOVE     gv_obj_id(12)            TO lwa_document_data-obj_name.
  MOVE     sy-langu                 TO lwa_document_data-obj_langu.
  MOVE     lc_o                     TO lwa_document_data-sensitivty.
  MOVE     lv_obj_descr             TO lwa_document_data-obj_descr.

  lwa_document_data-obj_expdat   = sy-datum + 30.

* Set the receiver list
  CLEAR:   lit_receivers[],            lwa_receivers.
  MOVE     p_email                  TO lwa_receivers-receiver.
  MOVE     lc_u                     TO lwa_receivers-rec_type.
  MOVE     lc_int                   TO lwa_receivers-com_type.
  APPEND   lwa_receivers            TO lit_receivers.

* Append the process errors to the email content
  IF       ( gv_flag_err_proc   IS NOT INITIAL ).
    CLEAR                              lwa_contents_txt.
    APPEND   lwa_contents_txt       TO lit_contents_txt.
    MOVE     text-mpe               TO lwa_contents_txt-line.
    APPEND   lwa_contents_txt       TO lit_contents_txt.
    CLEAR                              lwa_contents_txt.
    APPEND   lwa_contents_txt       TO lit_contents_txt.
    CLEAR                              lwa_errs.
    LOOP AT  git_errs_proc        INTO lwa_errs.
      CLEAR                            lwa_contents_txt.
      MOVE   lwa_errs-text          TO lwa_contents_txt-line.
      APPEND lwa_contents_txt       TO lit_contents_txt.
      CLEAR  lwa_errs.
    ENDLOOP.
  ENDIF.

* Set the packing list
  lv_lines = lines( lit_contents_txt[] ).

  CLEAR:   lit_packing_list[],         lwa_packing_list.
  MOVE     1                        TO lwa_packing_list-head_start.
  MOVE     0                        TO lwa_packing_list-head_num.
  MOVE     1                        TO lwa_packing_list-body_start.
  MOVE     lv_lines                 TO lwa_packing_list-body_num.
  MOVE     lc_raw                   TO lwa_packing_list-doc_type.
  APPEND   lwa_packing_list         TO lit_packing_list.

*eject

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = lwa_document_data
      put_in_outbox              = gc_x
      commit_work                = gc_x
    TABLES
      packing_list               = lit_packing_list
      contents_txt               = lit_contents_txt
      receivers                  = lit_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  lv_rc = sy-subrc.

  IF ( lv_rc NE 0 ).

    PERFORM  f_error_in_process  USING 0
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-192
                                       lv_rc
                                       lv_name
                                       space.

    RETURN.
  ENDIF.

ENDFORM.                    " f_send_email
*eject
*&---------------------------------------------------------------------*
*&      Form  f_read_text
*&---------------------------------------------------------------------*
*       Read standard text for the email content
*----------------------------------------------------------------------*
FORM f_read_text
  TABLES   cit_contents_txt   TYPE ty_it_list_text
  USING    iv_name            TYPE tdobname
  CHANGING cv_obj_descr       TYPE so_obj_des.

  DATA:    lwa_contents_txt   TYPE solisti1.

  DATA:    lwa_lines          TYPE tline,
           lit_lines          TYPE TABLE OF tline.

  DATA:    lv_rc              TYPE numc5.

  CONSTANTS:
           lc_id              TYPE tdid
                              VALUE 'ST',
           lc_language        TYPE spras
                              VALUE 'E',
           lc_object          TYPE tdobject
                              VALUE 'TEXT'.

  CLEAR    cit_contents_txt[].
  CLEAR    cv_obj_descr.

  CHECK  ( iv_name              IS NOT INITIAL ).

  CLEAR:   lit_lines[],                lwa_lines.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      client                  = sy-mandt
      id                      = lc_id
      language                = lc_language
      name                    = iv_name
      object                  = lc_object
    TABLES
      lines                   = lit_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  lv_rc = sy-subrc.

*eject
  IF ( lv_rc NE 0 ).

    PERFORM  f_error_in_process  USING 0
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-191
                                       lv_rc
                                       iv_name
                                       space.

    RETURN.
  ENDIF.

* Set the email object content
  CLEAR                      lwa_lines.
  LOOP AT  lit_lines    INTO lwa_lines.

    IF ( sy-tabix EQ 1 ).
      CLEAR                            cv_obj_descr.
      MOVE     lwa_lines-tdline     TO cv_obj_descr.
    ELSE.
      CLEAR                            lwa_contents_txt.
      MOVE     lwa_lines-tdline     TO lwa_contents_txt-line.
      APPEND   lwa_contents_txt     TO cit_contents_txt.
    ENDIF.

    CLEAR  lwa_lines.
  ENDLOOP.

ENDFORM.                    " f_read_text
*eject
*&---------------------------------------------------------------------*
*&      Form  f_error_in_data
*&---------------------------------------------------------------------*
*       Append an error to the data error table
*----------------------------------------------------------------------*
FORM f_error_in_data
  USING    iv_nbr_file   TYPE numc5
           iv_nbr_btc    TYPE numc5
           iv_nbr_doc    TYPE numc5
           iv_rc         TYPE numc5
           iv_msgid      TYPE symsgid
           iv_msgty      TYPE symsgty
           iv_msgno      TYPE any
           iv_msgv1      TYPE any
           iv_msgv2      TYPE any
           iv_msgv3      TYPE any
           iv_msgv4      TYPE any.

  DATA:    lv_type_fld   TYPE char1,
           lv_msgno_c    TYPE char3,
           lv_msgno      TYPE symsgno,
           lv_text       TYPE text240.

  DATA:    lwa_errs_data TYPE ty_wa_msgs.

  ADD      1                        TO gv_count_err_data.
  CLEAR                                gv_flag_err_data.
  MOVE     gc_x                     TO gv_flag_err_data.
  CLEAR                                gv_flag_err_mstr.
  MOVE     gc_x                     TO gv_flag_err_mstr.

  CLEAR    lv_type_fld.
  CLEAR    lv_msgno_c.
  CLEAR    lv_msgno.
  CLEAR    lv_text.

  DESCRIBE FIELD iv_msgno         TYPE lv_type_fld.

  IF           ( lv_type_fld        CS gc_c ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_msgno
      IMPORTING
        output = lv_msgno_c.

    IF         ( lv_msgno_c         CO '0123456789' ).

      MOVE       lv_msgno_c         TO lv_msgno.

    ENDIF.

  ELSEIF       ( lv_type_fld        CS gc_n ).

    MOVE         iv_msgno           TO lv_msgno.

  ENDIF.

*eject
  IF   ( iv_msgid IS INITIAL ).

    CONCATENATE                        iv_msgv1
                                       iv_msgv2
                                       iv_msgv3
                                       iv_msgv4
                                  INTO lv_text
                          SEPARATED BY space.

  ELSE.

    MESSAGE  ID iv_msgid          TYPE iv_msgty
         NUMBER lv_msgno          INTO lv_text
           WITH iv_msgv1               iv_msgv2
                iv_msgv3               iv_msgv4.

  ENDIF.

  CLEAR                                lwa_errs_data.
  MOVE     iv_nbr_file              TO lwa_errs_data-nbr_file.
  MOVE     iv_nbr_btc               TO lwa_errs_data-nbr_btc.
  MOVE     iv_nbr_doc               TO lwa_errs_data-nbr_doc.
  MOVE     iv_rc                    TO lwa_errs_data-rc.
  MOVE     iv_msgid                 TO lwa_errs_data-msgid.
  MOVE     iv_msgty                 TO lwa_errs_data-msgty.
  MOVE     lv_msgno                 TO lwa_errs_data-msgno.
  MOVE     lv_text                  TO lwa_errs_data-text.
  APPEND   lwa_errs_data            TO git_errs_data.

ENDFORM.                    " f_error_in_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_error_in_process
*&---------------------------------------------------------------------*
*       Append an error to the process error table
*----------------------------------------------------------------------*
FORM f_error_in_process
  USING    iv_nbr_file   TYPE numc5
           iv_nbr_btc    TYPE numc5
           iv_nbr_doc    TYPE numc5
           iv_rc         TYPE numc5
           iv_msgid      TYPE symsgid
           iv_msgty      TYPE symsgty
           iv_msgno      TYPE any
           iv_msgv1      TYPE any
           iv_msgv2      TYPE any
           iv_msgv3      TYPE any
           iv_msgv4      TYPE any.

  DATA:    lv_type_fld   TYPE char1,
           lv_msgno_c    TYPE char3,
           lv_msgno      TYPE symsgno,
           lv_text       TYPE text240.

  DATA:    lwa_errs_proc TYPE ty_wa_msgs.

  ADD      1                        TO gv_count_err_proc.
  CLEAR                                gv_flag_err_proc.
  MOVE     gc_x                     TO gv_flag_err_proc.
  CLEAR                                gv_flag_err_mstr.
  MOVE     gc_x                     TO gv_flag_err_mstr.

  CLEAR    lv_type_fld.
  CLEAR    lv_msgno_c.
  CLEAR    lv_msgno.
  CLEAR    lv_text.

  DESCRIBE FIELD iv_msgno         TYPE lv_type_fld.

  IF           ( lv_type_fld        CS gc_c ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_msgno
      IMPORTING
        output = lv_msgno_c.

    IF         ( lv_msgno_c         CO '0123456789' ).

      MOVE       lv_msgno_c         TO lv_msgno.

    ENDIF.

  ELSEIF       ( lv_type_fld        CS gc_n ).

    MOVE         iv_msgno           TO lv_msgno.

  ENDIF.

*eject
  IF   ( iv_msgid IS INITIAL ).

    CONCATENATE                        iv_msgv1
                                       iv_msgv2
                                       iv_msgv3
                                       iv_msgv4
                                  INTO lv_text
                          SEPARATED BY space.

  ELSE.

    MESSAGE  ID iv_msgid          TYPE iv_msgty
         NUMBER lv_msgno          INTO lv_text
           WITH iv_msgv1               iv_msgv2
                iv_msgv3               iv_msgv4.

  ENDIF.

  CLEAR                                lwa_errs_proc.
  MOVE     iv_nbr_file              TO lwa_errs_proc-nbr_file.
  MOVE     iv_nbr_btc               TO lwa_errs_proc-nbr_btc.
  MOVE     iv_nbr_doc               TO lwa_errs_proc-nbr_doc.
  MOVE     iv_rc                    TO lwa_errs_proc-rc.
  MOVE     iv_msgid                 TO lwa_errs_proc-msgid.
  MOVE     iv_msgty                 TO lwa_errs_proc-msgty.
  MOVE     lv_msgno                 TO lwa_errs_proc-msgno.
  MOVE     lv_text                  TO lwa_errs_proc-text.
  APPEND   lwa_errs_proc            TO git_errs_proc.

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
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_DUPLICATION
*&---------------------------------------------------------------------*
* Checking if duplication exists for Posting documents
*----------------------------------------------------------------------*
FORM f_process_duplication .

  DATA:     lv_flag_toggle  TYPE flag,            "Flag-Toggle
            lv_newbs_old    TYPE newbs,           "Posting Key-Next Line
            lv_newbk_old    TYPE newbk,           "Company Code-Next Line
            lv_newbk        TYPE newbk,           "Company Code-Next Line
            lv_tcode        TYPE tcode,           "Transaction Code
            lv_nbr_btc      TYPE numc5,           "Batch Sequence Number
            lv_nbr_doc      TYPE numc5,           "Doc Sequence Number
            lv_flag_ic      TYPE flag.            "Flag-InterCompany

  DATA:    "lwa_rfbibl00    TYPE ty_wa_rfbibl00,  "Inv RFBIBL00 Format
           lwa_bbkpf       TYPE ty_wa_bbkpf,     "Accounting Doc Header
           lwa_bbseg       TYPE ty_wa_bbseg.     "Accounting Doc Segment

  CLEAR                                lv_flag_toggle.
  MOVE     gc_x                     TO lv_flag_toggle.

  CLEAR    git_bdcdata[].
  CLEAR    gv_flag_err_vldt.
  CLEAR    lv_flag_ic.
  CLEAR    lv_newbs_old.
  CLEAR    lv_newbk_old.
  CLEAR    lv_tcode.
  CLEAR    lv_nbr_btc.
  CLEAR    lv_nbr_doc.


  LOOP AT lit_rfbibl00_dup INTO lwa_rfbibl00_dup.

    CASE     lwa_rfbibl00_dup+0(1).

*eject
* Batch session record
      WHEN   '0'.

        IF     ( lv_flag_toggle     IS INITIAL ).

          PERFORM  f_format_bdc_save_doc_dup USING    lv_tcode
                                                      lv_nbr_btc
                                                      lv_nbr_doc
                                                      lv_flag_ic.

        ENDIF.

        PERFORM  f_close_batch_session_dup   USING    lv_nbr_btc
                                                      lv_nbr_doc.

        CLEAR                          lv_flag_toggle.
        MOVE     gc_x               TO lv_flag_toggle.

        CLEAR    git_bdcdata[].
        CLEAR    gv_flag_err_vldt.
        CLEAR    lv_flag_ic.
        CLEAR    lv_newbs_old.
        CLEAR    lv_newbk_old.
        CLEAR    lv_tcode.

        CLEAR                          gwa_bgr00.
        MOVE     lwa_rfbibl00_dup   TO gwa_bgr00.

        ADD      1 TO lv_nbr_btc.

        CLEAR : gwa_bgr00-group.

        PERFORM  f_set_batch_session_dup        CHANGING gwa_bgr00.

        CLEAR :  lc_vendor_wrbtr,
                 lc_reference_xblnr.

*eject
* Document header record
      WHEN   '1'.

        IF     ( lv_flag_toggle     IS INITIAL ).

          PERFORM  f_format_bdc_save_doc_dup USING    lv_tcode
                                                      lv_nbr_btc
                                                      lv_nbr_doc
                                                      lv_flag_ic.
          CLEAR :  lc_vendor_wrbtr,
                   lc_reference_xblnr.

        ENDIF.

        CLEAR    lv_flag_toggle.

        CLEAR    git_bdcdata[].
        CLEAR    gv_flag_err_vldt.
        CLEAR    lv_flag_ic.
        CLEAR    lv_newbs_old.
        CLEAR    lv_newbk_old.

        CLEAR                          lwa_bbkpf.
        MOVE     lwa_rfbibl00_dup   TO lwa_bbkpf.

        CLEAR                          lv_tcode.
        MOVE     lwa_bbkpf-tcode    TO lv_tcode.

        ADD      1 TO lv_nbr_doc.

        PERFORM  f_format_bdc_next_header_dup USING    lwa_bbkpf
                                                       lv_nbr_btc
                                                       lv_nbr_doc.

        CLEAR                          lv_newbk_old.
        MOVE     lwa_bbkpf-bukrs    TO lv_newbk_old.

* IMP. Need company code for checking if with holding tax exists for vendor and company code.
        CLEAR : gv_company_code.
        gv_company_code = lwa_bbkpf-bukrs.

        lc_reference_xblnr = lwa_bbkpf-xblnr.

*eject
* Document line item record
      WHEN   '2'.

        CLEAR                          lwa_bbseg.
        MOVE     lwa_rfbibl00_dup   TO lwa_bbseg.

        CLEAR    lv_newbk.

        IF   ( ( lwa_bbseg-newbs    EQ '21' ) OR
               ( lwa_bbseg-newbs    EQ '31' )    ).         "vendor item
        ELSE.

          IF ( ( lwa_bbseg-newbk    IS NOT INITIAL  ) AND
               ( lwa_bbseg-newbk    NE lv_newbk_old )     ).
            CLEAR                      lv_newbk.
            MOVE lwa_bbseg-newbk    TO lv_newbk.
            CLEAR                      lv_newbk_old.
            MOVE lwa_bbseg-newbk    TO lv_newbk_old.
            CLEAR                      lv_flag_ic.
            MOVE gc_x               TO lv_flag_ic.
          ENDIF.

        ENDIF.

        PERFORM  f_format_bdc_next_item_dup  USING    lwa_bbseg-newbs
                                                      lwa_bbseg-hkont
                                                      lv_newbk
                                                      lv_newbs_old.

        IF   ( ( lwa_bbseg-newbs    EQ '21' ) OR
               ( lwa_bbseg-newbs    EQ '31' )    ).         "vendor Item

          lc_vendor_wrbtr = lwa_bbseg-wrbtr.

          PERFORM  f_format_bdc_vendor_item_dup   USING    lwa_bbseg.

        ELSE.                                           "accounting Item

          PERFORM  f_format_bdc_accntng_item_dup  USING    lwa_bbseg.

        ENDIF.

        lv_newbs_old = lwa_bbseg-newbs.

*eject
      WHEN   OTHERS.
        RETURN.

    ENDCASE.

    IF   ( gv_flag_err_proc     IS NOT INITIAL ).
      CLEAR                            lv_flag_toggle.
      MOVE     gc_x                 TO lv_flag_toggle.
      EXIT.
    ENDIF.

    CLEAR  lwa_rfbibl00_dup.

  ENDLOOP.

  IF     ( lv_flag_toggle     IS INITIAL ).

    PERFORM  f_format_bdc_save_doc_dup       USING    lv_tcode
                                                      lv_nbr_btc
                                                      lv_nbr_doc
                                                      lv_flag_ic.

  ENDIF.

  PERFORM  f_close_batch_session_dup         USING    lv_nbr_btc
                                                      lv_nbr_doc.


** End process for duplication so that the logic of earlier program is not changed
  CLEAR                                lv_flag_toggle.
  MOVE     gc_x                     TO lv_flag_toggle.

  CLEAR    git_bdcdata[].
  CLEAR    gv_flag_err_vldt.
  CLEAR    lv_flag_ic.
  CLEAR    lv_newbs_old.
  CLEAR    lv_newbk_old.
  CLEAR    lv_tcode.
  CLEAR    lv_nbr_btc.
  CLEAR    lv_nbr_doc.

  SORT lit_dup_entries.
  DELETE ADJACENT DUPLICATES FROM lit_dup_entries COMPARING ALL FIELDS.

ENDFORM.                    " F_PROCESS_DUPLICATION
*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_BDC_SAVE_DOC_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TCODE  text
*      -->P_LV_NBR_BTC  text
*      -->P_LV_NBR_DOC  text
*      -->P_LV_FLAG_IC  text
*----------------------------------------------------------------------*
FORM f_format_bdc_save_doc_dup
                              USING    iv_tcode      TYPE tcode
                                       iv_nbr_btc    TYPE numc5
                                       iv_nbr_doc    TYPE numc5
                                       iv_flag_ic    TYPE flag.

  IF   ( git_bdcdata[]              IS INITIAL ).

    RETURN.
  ENDIF.

  IF   ( iv_tcode                   IS INITIAL ).

    RETURN.
  ENDIF.

*eject
* Complete the BDC data for the document

  PERFORM  bdc_field    USING     'BDC_OKCODE'   '=AB'.

  PERFORM  bdc_screen   USING     'SAPLKACB'     '0002'.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   '/8'.

  IF     ( iv_flag_ic               IS INITIAL ).

    PERFORM  bdc_screen USING     'SAPMF05A'     '0700'.
    PERFORM  bdc_field  USING     'BDC_OKCODE'   'BU'.

  ELSE.

    PERFORM  bdc_screen USING     'SAPMF05A'     '0701'.
    PERFORM  bdc_field  USING     'BDC_OKCODE'   'BU'.

  ENDIF.

  IF     ( gv_flag_err_vldt     IS NOT INITIAL ).
    RETURN.
  ENDIF.

*** Check here if duplication exsists or not.
  IF lc_reference_xblnr IS NOT INITIAL.

    SELECT bukrs belnr gjahr
       FROM  bkpf
       INTO TABLE lit_bkpf_dup
       WHERE xblnr = lc_reference_xblnr .
    IF sy-subrc = 0 AND lit_bkpf_dup[] IS NOT INITIAL.

      SELECT bukrs belnr gjahr buzei
         FROM bseg
         INTO TABLE lit_bseg_dup
         FOR ALL ENTRIES IN lit_bkpf_dup
         WHERE bukrs = lit_bkpf_dup-bukrs  AND
               belnr = lit_bkpf_dup-belnr  AND
               gjahr = lit_bkpf_dup-gjahr  AND
               wrbtr = lc_vendor_wrbtr     AND
               shkzg = 'H'.                 " co_h.

      IF sy-subrc = 0 AND lit_bseg_dup[] IS NOT INITIAL.

        lwa_dup_entries-xblnr   =  lc_reference_xblnr.
        lwa_dup_entries-wrbtr   =  lc_vendor_wrbtr.
        APPEND  lwa_dup_entries TO lit_dup_entries.
        CLEAR : lwa_dup_entries.

        PERFORM  f_insert_bdcdata_dup    USING iv_tcode
                                               iv_nbr_btc
                                               iv_nbr_doc.
      ELSE.

        SELECT ausbk belnr gjahr bzkey bukrs
           FROM vbsegk
           INTO TABLE lit_vbsegk_dup
           FOR ALL ENTRIES IN lit_bkpf_dup
           WHERE bukrs = lit_bkpf_dup-bukrs  AND
                 belnr = lit_bkpf_dup-belnr  AND
                 gjahr = lit_bkpf_dup-gjahr  AND
                 wrbtr = lc_vendor_wrbtr     AND
               ( shkzg = 'H' OR
                 shkzg = 'S' ).  " Added SHKZG = 'S' so that parking document can be checked for duplication.

        IF sy-subrc = 0 AND lit_vbsegk_dup[] IS NOT INITIAL.

          lwa_dup_entries-xblnr   =  lc_reference_xblnr.
          lwa_dup_entries-wrbtr   =  lc_vendor_wrbtr.
          APPEND  lwa_dup_entries TO lit_dup_entries.
          CLEAR : lwa_dup_entries.

          PERFORM  f_insert_bdcdata_dup    USING iv_tcode
                                                 iv_nbr_btc
                                                 iv_nbr_doc.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_FORMAT_BDC_SAVE_DOC_DUP
*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_BDC_NEXT_HEADER_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_BBKPF  text
*      -->P_LV_NBR_BTC  text
*      -->P_LV_NBR_DOC  text
*----------------------------------------------------------------------*
FORM f_format_bdc_next_header_dup
                                  USING    iwa_bbkpf     TYPE ty_wa_bbkpf
                                           iv_nbr_btc    TYPE numc5
                                           iv_nbr_doc    TYPE numc5.

  DATA:    lv_bldat_d TYPE sydatum,
           lv_budat_d TYPE sydatum,
           lv_bldat_c TYPE char10,
           lv_budat_c TYPE char10.

  DATA:    lv_rc           TYPE numc5.

  DATA:    lwa_bdcmsg      TYPE ty_wa_bdcmsg.

* Convert the dates to character format
  CLEAR                        lv_bldat_d.
  MOVE     iwa_bbkpf-bldat  TO lv_bldat_d.
  CLEAR                        lv_budat_d.
  MOVE     iwa_bbkpf-budat  TO lv_budat_d.
  CLEAR                        lv_bldat_c.
  WRITE    lv_bldat_d       TO lv_bldat_c  DD/MM/YYYY.
  CLEAR                        lv_budat_c.
  WRITE    lv_budat_d       TO lv_budat_c  DD/MM/YYYY.

* Code the header transaction
  PERFORM  bdc_screen   USING     'SAPMF05A'     '0100'.
  PERFORM  bdc_field    USING     'BKPF-BLDAT'   lv_bldat_c.
  PERFORM  bdc_field    USING     'BKPF-BLART'   iwa_bbkpf-blart.
  PERFORM  bdc_field    USING     'BKPF-BUKRS'   iwa_bbkpf-bukrs.
  PERFORM  bdc_field    USING     'BKPF-BUDAT'   lv_budat_c.
  PERFORM  bdc_field    USING     'BKPF-WAERS'   iwa_bbkpf-waers.
  PERFORM  bdc_field    USING     'BKPF-XBLNR'   iwa_bbkpf-xblnr.
  PERFORM  bdc_field    USING     'BKPF-BKTXT'   iwa_bbkpf-bktxt.



ENDFORM.                    " F_FORMAT_BDC_NEXT_HEADER_DUP
*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_BDC_NEXT_ITEM_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_BBSEG_NEWBS  text
*      -->P_LWA_BBSEG_HKONT  text
*      -->P_LV_NEWBK  text
*      -->P_LV_NEWBS_OLD  text
*----------------------------------------------------------------------*
FORM f_format_bdc_next_item_dup
                                USING  iv_newbs     TYPE newbs
                                       iv_newko     TYPE hkont
                                       iv_newbk     TYPE newbk
                                       iv_newbs_old TYPE newbs.

  PERFORM  bdc_field    USING     'RF05A-NEWBS'  iv_newbs.
  PERFORM  bdc_field    USING     'RF05A-NEWKO'  iv_newko.
  PERFORM  bdc_field    USING     'RF05A-NEWBK'  iv_newbk.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   '/00'.

* logic for With Holding Tax Pop-up issue
  IF ( ( iv_newbs_old = '21'  ) OR
       ( iv_newbs_old = '31'  )  ).
    IF gv_vendor_tax_chk IS NOT INITIAL AND
       gv_company_code   IS NOT INITIAL.

      CLEAR : gwa_lfbw_chk.
      SELECT SINGLE *
        FROM lfbw
        INTO gwa_lfbw_chk
        WHERE lifnr = gv_vendor_tax_chk AND
              bukrs = gv_company_code   AND
              wt_subjct = 'X'.
      IF sy-subrc = 0.
        CLEAR:  gv_vendor_tax_chk,
                gv_company_code.
        PERFORM  bdc_screen USING     'SAPLFWTD'     '0100'.
        PERFORM  bdc_field  USING     'BDC_CURSOR'   'WITH_ITEM-WT_WITHCD(01)'.
        PERFORM  bdc_field  USING     'BDC_OKCODE'   '=GO'.

      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR : gwa_lfbw_chk.

* The coding block will appear after a G/L accounting line is entered
  IF ( ( iv_newbs_old NE '21'  ) AND
       ( iv_newbs_old NE '31'  ) AND
       ( iv_newbs_old NE space )     ).

    PERFORM  bdc_screen USING     'SAPLKACB'     '0002'.
    PERFORM  bdc_field  USING     'BDC_OKCODE'   '/8'.

  ENDIF.

ENDFORM.                    " F_FORMAT_BDC_NEXT_ITEM_DUP
*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_BDC_VENDOR_ITEM_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_BBSEG  text
*----------------------------------------------------------------------*
FORM f_format_bdc_vendor_item_dup
                                  USING    iwa_bbseg TYPE ty_wa_bbseg.

  DATA:    lv_lifnr  TYPE lifnr,
           lv_ktokk  TYPE ktokk.

* Select the vendor account group.
  CLEAR                                lv_lifnr.
  MOVE     iwa_bbseg-hkont          TO lv_lifnr.

* Pass vendor no for with holding tax check.
  CLEAR : gv_vendor_tax_chk.
  gv_vendor_tax_chk = iwa_bbseg-hkont.

  CLEAR    lv_ktokk.
  SELECT   SINGLE ktokk
    INTO   lv_ktokk
    FROM   lfa1
   WHERE   lifnr = lv_lifnr.
  IF ( sy-subrc NE 0 ).
    CLEAR  lv_ktokk.
  ENDIF.

  IF   ( lv_ktokk IN gr_ktokk_ot ).

    PERFORM  bdc_screen USING     'SAPLFCPD'     '0100'.
    PERFORM  bdc_field  USING     'BSEC-NAME1'   iwa_bbseg-name1.
    PERFORM  bdc_field  USING     'BSEC-NAME2'   iwa_bbseg-name2.
    PERFORM  bdc_field  USING     'BSEC-STRAS'   iwa_bbseg-stras.
    PERFORM  bdc_field  USING     'BSEC-ORT01'   iwa_bbseg-ort01.
    PERFORM  bdc_field  USING     'BSEC-PSTLZ'   iwa_bbseg-pstlz.

  ENDIF.

* Create vendor item screen
  PERFORM  bdc_screen   USING     'SAPMF05A'     '0302'.
  PERFORM  bdc_field    USING     'BSEG-WRBTR'   iwa_bbseg-wrbtr.
  PERFORM  bdc_field    USING     'BSEG-MWSKZ'   iwa_bbseg-mwskz.
  PERFORM  bdc_field    USING     'BSEG-ZTERM'   iwa_bbseg-zterm.
  PERFORM  bdc_field    USING     'BSEG-ZUONR'   iwa_bbseg-zuonr.
  PERFORM  bdc_field    USING     'BSEG-SGTXT'   iwa_bbseg-sgtxt.
  PERFORM  bdc_field    USING     'BSEG-ZLSCH'   iwa_bbseg-zlsch. "Add by RBHATT 04-APR-2012 TR DECK905448

* Enter the route code
  IF   ( iwa_bbseg-xref3 IS NOT INITIAL ).

    PERFORM  bdc_field  USING     'BDC_OKCODE'   '=ZK'.

    PERFORM  bdc_screen USING     'SAPMF05A'     '0332'.
    PERFORM  bdc_field  USING     'BSEG-XREF3'   iwa_bbseg-xref3.

  ENDIF.

ENDFORM.                    " F_FORMAT_BDC_VENDOR_ITEM_DUP
*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_BDC_ACCNTNG_ITEM_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_BBSEG  text
*----------------------------------------------------------------------*
FORM f_format_bdc_accntng_item_dup
                                  USING    iwa_bbseg TYPE ty_wa_bbseg.

* Screen: Create G/L account item
  PERFORM  bdc_screen   USING     'SAPMF05A'     '0300'.
  PERFORM  bdc_field    USING     'BSEG-WRBTR'   iwa_bbseg-wrbtr.
  PERFORM  bdc_field    USING     'BSEG-MWSKZ'   iwa_bbseg-mwskz.
  PERFORM  bdc_field    USING     'BSEG-SGTXT'   iwa_bbseg-sgtxt.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   'ZK'.

* coding block screen
  PERFORM  bdc_screen   USING     'SAPLKACB'     '0002'.
  PERFORM  bdc_field    USING     'COBL-KOSTL'   iwa_bbseg-kostl.
  PERFORM  bdc_field    USING     'COBL-AUFNR'   iwa_bbseg-aufnr.
* Start of changes by AHAMDT for CHG0188302
*  PERFORM  bdc_field    USING     'COBL-ZZREF'   iwa_bbseg-zzref.  "Add by RBHATT 04-APR-2012 TR DECK905448
* End of changes by AHAMDT for CHG0188302
  PERFORM  bdc_field    USING     'COBL-PS_PSP_PNR'
                                                 iwa_bbseg-projk.
  PERFORM  bdc_field    USING     'COBL-NPLNR'   iwa_bbseg-nplnr.
  PERFORM  bdc_field    USING     'COBL-VORNR'   iwa_bbseg-vornr.
  PERFORM  bdc_field    USING     'COBL-ZZLOC'   iwa_bbseg-zzloc.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   '=ENTE'.

  PERFORM  bdc_screen   USING     'SAPMF05A'     '0330'.
  PERFORM  bdc_field    USING     'BDC_OKCODE'   'ZK'.

  PERFORM  bdc_screen   USING     'SAPMF05A'     '0300'.


ENDFORM.                    " F_FORMAT_BDC_ACCNTNG_ITEM_DUP
*&---------------------------------------------------------------------*
*&      Form  F_CLOSE_BATCH_SESSION_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_NBR_BTC  text
*      -->P_LV_NBR_DOC  text
*----------------------------------------------------------------------*
FORM f_close_batch_session_dup
                              USING    iv_nbr_btc    TYPE numc5
                                       iv_nbr_doc    TYPE numc5.

  DATA:    lv_rc         TYPE numc5.

  CLEAR    lv_rc.

  IF   ( gwa_bdccntl_dup-sesn_open      IS INITIAL ).
    RETURN.
  ENDIF.

  IF     ( cb_test              IS NOT INITIAL ).
    CLEAR                              gwa_bdccntl_dup-sesn_open.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1
      queue_error = 2
      OTHERS      = 3.

  lv_rc = sy-subrc.

  IF ( lv_rc EQ 0 ).
    CLEAR                              gwa_bdccntl_dup-sesn_open.
  ELSE.

    RETURN.
  ENDIF.

ENDFORM.                    " F_CLOSE_BATCH_SESSION_DUP
*&---------------------------------------------------------------------*
*&      Form  F_SET_BATCH_SESSION_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GWA_BGR00  text
*----------------------------------------------------------------------*
FORM f_set_batch_session_dup
                            CHANGING cwa_bgr00     TYPE ty_wa_bgr00.

* Set the group name
  IF     ( gwa_bdccntl_dup-sesn_group   IS NOT INITIAL ).
    CLEAR                              cwa_bgr00-group.
    MOVE   gwa_bdccntl_dup-sesn_group   TO cwa_bgr00-group.
  ENDIF.

  IF   ( ( cwa_bgr00-group(1)       EQ gc_fs ) OR
         ( cwa_bgr00-group          EQ space )    ).
    CLEAR                              cwa_bgr00-group.
    MOVE   gc_sesn_group_dup        TO cwa_bgr00-group.
  ENDIF.

* Set the start date
  IF     ( gwa_bdccntl_dup-sesn_start   IS NOT INITIAL ).
    CLEAR                              cwa_bgr00-start.
    MOVE   gwa_bdccntl_dup-sesn_start   TO cwa_bgr00-start.
  ELSE.
    CLEAR                              cwa_bgr00-start.
    cwa_bgr00-start = sy-datum - 1.
  ENDIF.

* Set the keep flag
  IF     ( gwa_bdccntl_dup-sesn_xkeep   CS gc_y ).
    CLEAR                              cwa_bgr00-xkeep.
    MOVE   gc_x                     TO cwa_bgr00-xkeep.
  ELSEIF ( gwa_bdccntl_dup-sesn_xkeep   CS gc_n ).
    CLEAR                              cwa_bgr00-xkeep.
  ENDIF.

  IF   ( ( cwa_bgr00-xkeep          NE gc_x  ) OR
         ( cwa_bgr00-xkeep          NE space )    ).
    CLEAR                              cwa_bgr00-xkeep.
    MOVE   gc_x                     TO cwa_bgr00-xkeep.
  ENDIF.

* Set the user name
  IF     ( gwa_bdccntl_dup-sesn_usnam   IS NOT INITIAL ).
    CLEAR                               cwa_bgr00-usnam.
    MOVE   gwa_bdccntl_dup-sesn_usnam   TO cwa_bgr00-usnam.
  ENDIF.

  IF   ( ( cwa_bgr00-usnam(1)       EQ gc_fs ) OR
         ( cwa_bgr00-usnam          EQ space )    ).
    CLEAR                              cwa_bgr00-usnam.
    MOVE   sy-uname                 TO cwa_bgr00-usnam.
  ENDIF.


ENDFORM.                    " F_SET_BATCH_SESSION_DUP
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_BDCDATA_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_TCODE  text
*      -->P_IV_NBR_BTC  text
*      -->P_IV_NBR_DOC  text
*----------------------------------------------------------------------*
FORM f_insert_bdcdata_dup
                         USING    iv_tcode      TYPE tcode
                                  iv_nbr_btc    TYPE numc5
                                  iv_nbr_doc    TYPE numc5.

  DATA:    lv_rc         TYPE numc5.

  IF       ( gwa_bdccntl_dup-sesn_open  IS INITIAL ).

    PERFORM  f_open_batch_session_dup
                                 USING iv_nbr_btc
                                       iv_nbr_doc.

  ENDIF.

  IF     ( cb_test              IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode            = iv_tcode
    TABLES
      dynprotab        = git_bdcdata
    EXCEPTIONS
      internal_error   = 1
      not_open         = 2
      queue_error      = 3
      tcode_invalid    = 4
      printing_invalid = 5
      posting_invalid  = 6
      OTHERS           = 7.

  lv_rc = sy-subrc.

  IF ( lv_rc NE 0 ).

    RETURN.
  ENDIF.

ENDFORM.                    " F_INSERT_BDCDATA_DUP
*&---------------------------------------------------------------------*
*&      Form  F_OPEN_BATCH_SESSION_DUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_NBR_BTC  text
*      -->P_IV_NBR_DOC  text
*----------------------------------------------------------------------*
FORM f_open_batch_session_dup
                             USING    iv_nbr_btc    TYPE numc5
                                      iv_nbr_doc    TYPE numc5.

  DATA:    lv_rc         TYPE numc5.

  CLEAR    lv_rc.

  IF     ( gwa_bdccntl_dup-sesn_open    IS NOT INITIAL ).

    RETURN.
  ENDIF.

  IF     ( cb_test              IS NOT INITIAL ).
    CLEAR                              gwa_bdccntl_dup-sesn_open.
    MOVE     gc_x                   TO gwa_bdccntl_dup-sesn_open.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client              = sy-mandt
      group               = gwa_bgr00-group
      holddate            = gwa_bgr00-start
      keep                = gwa_bgr00-xkeep
      user                = gwa_bgr00-usnam
    EXCEPTIONS
      client_invalid      = 1
      destination_invalid = 2
      group_invalid       = 3
      group_is_locked     = 4
      holddate_invalid    = 5
      internal_error      = 6
      queue_error         = 7
      running             = 8
      system_lock_error   = 9
      user_invalid        = 10
      OTHERS              = 11.

  lv_rc = sy-subrc.

*eject
  IF ( lv_rc EQ 0 ).
    CLEAR                              gwa_bdccntl_dup-sesn_open.
    MOVE     gc_x                   TO gwa_bdccntl_dup-sesn_open.
  ELSE.

    RETURN.
  ENDIF.

ENDFORM.                    " F_OPEN_BATCH_SESSION_DUP
