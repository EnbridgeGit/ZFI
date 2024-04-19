*&---------------------------------------------------------------------*
*&  Include           ZFAPI130_CIMPL_INV_FRMT_F01
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI130_CIMPL_INV_FRMT                           *
*  Author:           Shamsiya Shaffe                                   *
*  Date:             August 29, 2019                                   *
*  Track #:                                                            *
*  Application Area: FICO                                              *
*                                                                      *
*  Description:      Cimpl Interface Invoice Inbound Format            *
*                                                                      *
*                    Create a file in the RFBIBL00 format using        *
*                    data from the inbound interface file              *
*                                                                      *
*                    Copy of US Report ZFAPI017_DATACERT_INV_FRMT with *
*                    change in document type to CM                     *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
*08/29/2019 D30K930117 SHAFFES CHG0153812 - Initial program development*
*10/11/2019 D30K930204 SHAFFES CHG0153812 - Initial program development*
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
     AND   key1      = gc_param_format.
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
           ( screen-group1 EQ gc_modif_id_dl1 ) OR
           ( screen-group1 EQ gc_modif_id_dl2 )    ).
      screen-input = 0.
      MODIFY   SCREEN.
    ENDIF.

* If the object ID is not assigned, then disable input file parameters
    IF     ( p_objid       IS INITIAL ).

      IF ( ( screen-group1 EQ gc_modif_id_fp1 ) OR
           ( screen-group1 EQ gc_modif_id_fn1 )    ).
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.

    ELSEIF ( rb_appl       EQ gc_x ). "input file appl-srvr radiobutton

* Disable the input filepath; populate with value from zfit_xparam
      IF   ( screen-group1 EQ gc_modif_id_fp1 ).
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.

* Enable the input filename
      IF   ( screen-group1 EQ gc_modif_id_fn1 ).
        screen-input = 1.
        MODIFY   SCREEN.
      ENDIF.

    ELSEIF ( rb_pres       EQ gc_x ). "input file pres-srvr radiobutton

* Enable the input filepath
      IF   ( screen-group1 EQ gc_modif_id_fp1 ).
        screen-input = 1.
        MODIFY   SCREEN.
      ENDIF.

* Disable the input filename; value to be included with filepath
      IF   ( screen-group1 EQ gc_modif_id_fn1 ).
        screen-active = 0.
        MODIFY   SCREEN.
      ENDIF.

    ENDIF.

*eject
* If the object ID is not assigned, then disable output file parameters
    IF     ( p_objid       IS INITIAL ).

      IF ( ( screen-group1 EQ gc_modif_id_fp2 ) OR
           ( screen-group1 EQ gc_modif_id_fn2 )    ).
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.

    ELSEIF ( rb_appl       EQ gc_x ). "output file appl-srvr radiobutton

* Disable the output filepath; populate with value from zfit_xparam
      IF   ( screen-group1 EQ gc_modif_id_fp2 ).
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.

* Disable the output filename; value to be included with filepath
      IF   ( screen-group1 EQ gc_modif_id_fn2 ).
        screen-active = 0.
        MODIFY   SCREEN.
      ENDIF.

    ELSEIF ( rb_pres       EQ gc_x ). "output file pres-srvr radiobutton

* Enable the output filepath
      IF   ( screen-group1 EQ gc_modif_id_fp2 ).
        screen-input = 1.
        MODIFY   SCREEN.
      ENDIF.

* Disable the output filename; value to be included with filepath
      IF   ( screen-group1 EQ gc_modif_id_fn2 ).
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
  IF   ( ( rb_pres  EQ gc_x )    AND ( gv_flag_pres1 IS INITIAL ) ).
    MOVE   gc_x                   TO   gv_flag_pres1.
    CLEAR  p_fpath1.
  ENDIF.

* If appl server is selected, set the filepath & name from ZFIT_XPARAM
  IF     ( rb_appl  EQ gc_x ).
    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_in.
    IF ( sy-subrc EQ 0 ).
      CLEAR                            p_fpath1.
      MOVE     lwa_xparam-value1    TO p_fpath1.
    ENDIF.
    CLEAR                              lwa_xparam.
  ENDIF.

* Clear the output filepath when switching from appl to pres server
  IF   ( ( rb_pres  EQ gc_x )    AND ( gv_flag_pres2 IS INITIAL ) ).
    MOVE   gc_x                   TO   gv_flag_pres2.
    CLEAR  p_fpath2.
  ENDIF.

* If appl server is selected, set the filepath & name from ZFIT_XPARAM
  IF     ( rb_appl  EQ gc_x ).
    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_out.
    IF ( sy-subrc EQ 0 ).
      CLEAR                            p_fpath2.
      MOVE     lwa_xparam-value1    TO p_fpath2.
    ENDIF.
    CLEAR                              lwa_xparam.
  ENDIF.

*eject
* Set the object ID description
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key1    = gc_param_format
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
    CLEAR  gv_flag_pres1.
  ENDIF.

* Reset output presentation server flag to control clearing of filepath
  IF     ( rb_appl  IS NOT INITIAL ).
    CLEAR  gv_flag_pres2.
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

* Check if the output filepath has been maintained
    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_out.
    IF ( sy-subrc EQ 0 ).
      IF         ( lwa_xparam-value1   IS INITIAL ).
        MESSAGE  e000(zfi01) WITH text-113.
      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-113.
    ENDIF.

*eject
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

* Initial the internal tables
  CLEAR:   git_dd03l[].
  CLEAR:   git_file_list[].
  CLEAR:   git_file_stats[].
  CLEAR:   git_inv_item[].
  CLEAR:   git_rfbibl00[].
  CLEAR    git_errs_data[].
  CLEAR    git_errs_proc[].

* Initial variables
  CLEAR    gv_nbr_file.
  CLEAR    gv_count_err_data.
  CLEAR    gv_count_err_proc.
  CLEAR    gv_flag_err_data.
  CLEAR    gv_flag_err_proc.
  CLEAR    gv_flag_err_mstr.

* Initial the table fields internal table
  SELECT   *
    INTO   TABLE git_dd03l
    FROM   dd03l
   WHERE   tabname IN ('BGR00', 'BBKPF', 'BBSEG').
  IF ( sy-subrc EQ 0 ).
    SORT   git_dd03l ASCENDING BY tabname position.
  ELSE.
    CLEAR  git_dd03l[].
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
    MOVE   p_fpath1                 TO lwa_file_list-filename_in.
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

      IF     ( p_fname1         IS NOT INITIAL ).
        CLEAR                          lv_file_mask.
        MOVE   p_fname1             TO lv_file_mask.
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

  IF     ( cv_subrc   IS NOT INITIAL ).
    RETURN.
  ENDIF.
  IF     ( iv_text        IS INITIAL ).
    cv_subrc = 1. "raise no_text
    RETURN.
  ENDIF.
  IF     ( iv_pattern     IS INITIAL ).
    cv_subrc = 2. "raise no_pattern
    RETURN.
  ENDIF.

  CLEAR                      lv_text.
  MOVE     iv_text        TO lv_text.
  TRANSLATE                  lv_text TO UPPER CASE.

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
  TABLES   cit_inv_item       STRUCTURE gwa_inv_item
           cit_file_stats     STRUCTURE gwa_file_stats
  USING    iwa_file_list      TYPE ty_wa_file_list.

  DATA:    lv_filename_ff_in  TYPE text1024, "filename in - path + name
           lv_filename_fn_in  TYPE text128,  "filename in - name
           lv_filepath_out    TYPE text1024, "filepath out
           lv_filename_ff_out TYPE text1024, "filename out - path + name
           lv_filename_fn_out TYPE text128.  "filename out - name

  DATA:    lv_count           TYPE syindex,
           lv_text25          TYPE text25.

  DATA:    lwa_xparam         TYPE ty_wa_xparam,
           lwa_file_stats     TYPE ty_wa_file_stats.

  DATA:    lit_data_tab       TYPE ty_it_data_tab.

  CLEAR    cit_inv_item[].

  CLEAR    lit_data_tab[].

  IF     ( gv_flag_err_proc     IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lv_filename_ff_in.
  MOVE     iwa_file_list-filename_in
                                    TO lv_filename_ff_in.

  CLEAR                                lv_filename_fn_in.
  MOVE     iwa_file_list-filename_fn
                                    TO lv_filename_fn_in.

*eject
  IF     ( rb_appl EQ gc_x ).

* Read data from a file on the IA server

    PERFORM  f_read_data      TABLES   lit_data_tab
                              USING    lv_filename_ff_in
                                       lv_filename_fn_in.

  ELSEIF ( rb_pres EQ gc_x ).

* Upload data from a file on the presentation server

    PERFORM  f_upload_data    TABLES   lit_data_tab
                              USING    lv_filename_ff_in.

  ENDIF.

  IF     ( gv_flag_err_proc     IS NOT INITIAL ).
    RETURN.
  ENDIF.

* Format the input records - invoice items

  PERFORM  f_format_input_rec   TABLES lit_data_tab
                                       cit_inv_item.

  CLEAR    lit_data_tab[].

* Read the program parameter table for the output filepath and file extn
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key2    = gc_filepath_out.
  IF ( sy-subrc NE 0 ).
    CLEAR        lwa_xparam.
  ENDIF.

  CLEAR                                lv_filepath_out.
  IF     ( lwa_xparam-value1    IS NOT INITIAL ).
    MOVE   lwa_xparam-value1        TO lv_filepath_out.
  ENDIF.

  CLEAR                                lv_text25.
  MOVE     lwa_xparam-value2        TO lv_text25.
  IF     ( lv_text25                IS INITIAL ).
    MOVE   gc_rfbibl00              TO lv_text25.
  ENDIF.

*eject
* Set the output full filename - filepath and filename
  IF     ( ( rb_pres            IS NOT INITIAL ) AND
           ( p_fpath2           IS NOT INITIAL )     ).

    CLEAR                              lv_filename_ff_out.
    MOVE   p_fpath2                 TO lv_filename_ff_out.

  ELSEIF ( ( rb_pres            IS NOT INITIAL ) AND
           ( p_fpath2               IS INITIAL )     ).

    CLEAR                              lv_filepath_out.

    PERFORM  f_generate_filename
                              USING    lv_filename_ff_in
                                       lv_filename_fn_in
                                       lv_filepath_out
                                       gc_x   "delete existing extension
                                       lv_text25            "append text
                              CHANGING lv_filename_ff_out
                                       lv_filename_fn_out.

  ELSE.

    PERFORM  f_generate_filename
                              USING    lv_filename_ff_in
                                       lv_filename_fn_in
                                       lv_filepath_out
                                       gc_x   "delete existing extension
                                       lv_text25            "append text
                              CHANGING lv_filename_ff_out
                                       lv_filename_fn_out.

  ENDIF.

* Append the file stats
  lv_count = lines( lit_data_tab[] ).

  CLEAR                                lwa_file_stats.
  MOVE     gv_nbr_file              TO lwa_file_stats-nbr_file.
  MOVE     lv_filename_ff_in        TO lwa_file_stats-filename_in.
  MOVE     lv_filename_fn_in        TO lwa_file_stats-filename_fn.
  MOVE     lv_count                 TO lwa_file_stats-cnt_rec_in.
  MOVE     lv_filename_ff_out       TO lwa_file_stats-filename_out.
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
  TABLES   cit_data_tab    TYPE ty_it_data_tab
  USING    iv_filename_in  TYPE text1024
           iv_filename_fn  TYPE text128.

  DATA:    lv_rc           TYPE numc5,
           lv_text         TYPE text1000.

  CLEAR    cit_data_tab[].

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

    CLEAR                                lv_text.
    READ     DATASET iv_filename_in INTO lv_text.
    IF ( sy-subrc EQ 0 ).
      APPEND                             lv_text
                                      TO cit_data_tab.
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
  TABLES   cit_data_tab    TYPE ty_it_data_tab
  USING    iv_filename_in  TYPE text1024.

  DATA:    lv_rc           TYPE numc5,
           lv_filename     TYPE string.

  CLEAR    cit_data_tab[].

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
      data_tab                = cit_data_tab
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
*&      Form  f_format_input_rec
*&---------------------------------------------------------------------*
*       Format the input records - invoice items
*----------------------------------------------------------------------*
FORM f_format_input_rec
  TABLES   iit_data_tab TYPE ty_it_data_tab
           cit_inv_item STRUCTURE gwa_inv_item.

  DATA:    lv_text      TYPE text1000,
           lv_tabix     TYPE sytabix.

  DATA:    lwa_inv_item TYPE ty_wa_inv_item.

  CLEAR    cit_inv_item[].

* Split the input data records into the fields of an invoice
  CLEAR                      lv_text.
  LOOP AT  iit_data_tab INTO lv_text.
    lv_tabix = sy-tabix.

    IF     ( lv_tabix     LT 2       ).
    ELSEIF ( lv_text      IS INITIAL ).
    ELSEIF ( lv_text+0(7) EQ '|||||||' ).
    ELSEIF ( lv_text+0(1) EQ 'I'       ).
    ELSE.

      SPLIT  lv_text      AT gc_pipe
                        INTO lwa_inv_item-bldat  lwa_inv_item-bukrs
                             lwa_inv_item-waers  lwa_inv_item-xblnr
                             lwa_inv_item-bktxt  lwa_inv_item-bschl
                             lwa_inv_item-lifnr  lwa_inv_item-newbk
                             lwa_inv_item-wrbtr  lwa_inv_item-sgtxt
                             "lwa_inv_item-zlsch           "Add by RBHATT 04-APR-2012 TR DECK905448
                             lwa_inv_item-saknr  lwa_inv_item-kostl
                             "lwa_inv_item-zzref           "Add by RBHATT 04-APR-2012 TR DECK905448
                             lwa_inv_item-aufnr  lwa_inv_item-nplnr
                             lwa_inv_item-vornr  lwa_inv_item-projk
                             lwa_inv_item-zuonr  lwa_inv_item-name1
                             lwa_inv_item-stras  lwa_inv_item-ort01
                             lwa_inv_item-pstlz  lwa_inv_item-xref3
                             lwa_inv_item-mwskz  lwa_inv_item-zzloc
                             lwa_inv_item-zlsch           "Add by SKAPSE 21-MAY-2012 TR DECK905448
                             lwa_inv_item-zzref .          "Add by RBHATT 21-MAY-2012 TR DECK905448.

      APPEND lwa_inv_item  TO cit_inv_item.

    ENDIF.

    CLEAR  lv_text.
  ENDLOOP.

ENDFORM.                    " f_format_input_rec
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_rfbibl00
*&---------------------------------------------------------------------*
*       Format the invoice items into the rfbibl00 layout
*----------------------------------------------------------------------*
FORM f_format_rfbibl00
  TABLES   iit_inv_item    STRUCTURE gwa_inv_item
           cit_rfbibl00    TYPE ty_it_rfbibl00
           cit_file_stats  STRUCTURE gwa_file_stats.

  DATA:    lv_subrc        TYPE sysubrc,
           lv_tabix        TYPE sytabix,
           lv_flag_doc_1st TYPE flag,
           lv_rec_cnt      TYPE syindex,
           lv_rec_cnt_t0   TYPE syindex,
           lv_rec_cnt_t1   TYPE syindex,
           lv_rec_cnt_t2   TYPE syindex.

  DATA:    lwa_inv_item    TYPE ty_wa_inv_item,
           lwa_rfbibl00    TYPE ty_wa_rfbibl00,
           lwa_file_stats  TYPE ty_wa_file_stats,
           lwa_bgr00       TYPE ty_wa_bgr00,
           lwa_bbkpf       TYPE ty_wa_bbkpf,
           lwa_bbseg       TYPE ty_wa_bbseg.

  CLEAR    cit_rfbibl00[].

  IF     ( gv_flag_err_proc     IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                lv_flag_doc_1st.
  MOVE     gc_x                     TO lv_flag_doc_1st.
  CLEAR                                lv_rec_cnt.
  CLEAR                                lv_rec_cnt_t0.
  CLEAR                                lv_rec_cnt_t1.
  CLEAR                                lv_rec_cnt_t2.

  CLEAR                                lwa_inv_item.
  LOOP AT  iit_inv_item           INTO lwa_inv_item.

    CASE     lwa_inv_item-bldat(1).

      WHEN     ' '.

* Initial the data structure - bbseg - G/L account item
        CLEAR                          lwa_bbseg.

        PERFORM  f_set_structure_bbseg
                              USING    lwa_inv_item
                                       gc_koart_gl
                              CHANGING lwa_bbseg.

        CLEAR                          lwa_rfbibl00.
        MOVE     lwa_bbseg          TO lwa_rfbibl00.
        APPEND   lwa_rfbibl00       TO cit_rfbibl00.
        ADD      1                  TO lv_rec_cnt_t2.

*eject
      WHEN     OTHERS.

        IF       ( lv_flag_doc_1st  EQ gc_x ).
          CLEAR    lv_flag_doc_1st.

* Initial the data structure - bgr00
          CLEAR                        lwa_bgr00.

          PERFORM  f_set_structure_bgr00
                              CHANGING lwa_bgr00.

          CLEAR                        lwa_rfbibl00.
          MOVE     lwa_bgr00        TO lwa_rfbibl00.
          APPEND   lwa_rfbibl00     TO cit_rfbibl00.
          ADD      1                TO lv_rec_cnt_t0.

        ENDIF.

* Initial the data structure - bbkpf
        CLEAR                          lwa_bbkpf.

        PERFORM  f_set_structure_bbkpf
                              USING    lwa_inv_item
                              CHANGING lwa_bbkpf.

        CLEAR                          lwa_rfbibl00.
        MOVE     lwa_bbkpf          TO lwa_rfbibl00.
        APPEND   lwa_rfbibl00       TO cit_rfbibl00.
        ADD      1                  TO lv_rec_cnt_t1.

* Initial the data structure - bbseg - vendor account item
        CLEAR                          lwa_bbseg.

        PERFORM  f_set_structure_bbseg
                              USING    lwa_inv_item
                                       gc_koart_vendor
                              CHANGING lwa_bbseg.

*       TRANSFER z_zbseg   TO p_fileot LENGTH 2373.
        CLEAR                          lwa_rfbibl00.
        MOVE     lwa_bbseg          TO lwa_rfbibl00.
        APPEND   lwa_rfbibl00       TO cit_rfbibl00.
        ADD      1                  TO lv_rec_cnt_t2.

    ENDCASE.

    CLEAR  lwa_inv_item.
  ENDLOOP.

*eject
* Update the file stats
  lv_rec_cnt = lines( cit_rfbibl00[] ).

  CLEAR                                lwa_file_stats.
  READ     TABLE cit_file_stats   INTO lwa_file_stats
                              WITH KEY nbr_file = gv_nbr_file.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF ( lv_subrc EQ 0 ).

    CLEAR                              lwa_file_stats-cnt_rec_out.
    MOVE     lv_rec_cnt             TO lwa_file_stats-cnt_rec_out.
    CLEAR                              lwa_file_stats-cnt_rec_out_t0.
    MOVE     lv_rec_cnt_t0          TO lwa_file_stats-cnt_rec_out_t0.
    CLEAR                              lwa_file_stats-cnt_rec_out_t1.
    MOVE     lv_rec_cnt_t1          TO lwa_file_stats-cnt_rec_out_t1.
    CLEAR                              lwa_file_stats-cnt_rec_out_t2.
    MOVE     lv_rec_cnt_t2          TO lwa_file_stats-cnt_rec_out_t2.

    MODIFY                             cit_file_stats
                                  FROM lwa_file_stats INDEX lv_tabix.

  ELSE.

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       0
                                       space
                                       space
                                       0
                                       text-151
                                       gv_nbr_file
                                       'Format'
                                       space.

    RETURN.
  ENDIF.

ENDFORM.                    " f_format_rfbibl00
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_structure_bgr00
*&---------------------------------------------------------------------*
*       Set values in the data structure - bgr00
*----------------------------------------------------------------------*
FORM f_set_structure_bgr00
  CHANGING cwa_bgr00    TYPE ty_wa_bgr00.

  PERFORM  f_initial_structure
                              USING    'BGR00'.

  MOVE     '0'                      TO cwa_bgr00-stype.
  MOVE     text-t01                 TO cwa_bgr00-group.
  MOVE     sy-mandt                 TO cwa_bgr00-mandt.
  MOVE     sy-uname                 TO cwa_bgr00-usnam.

ENDFORM.                    " f_set_structure_bgr00
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_structure_bbkpf
*&---------------------------------------------------------------------*
*       Set values in the data structure - bbkpf
*----------------------------------------------------------------------*
FORM f_set_structure_bbkpf
  USING    iwa_inv_item TYPE ty_wa_inv_item
  CHANGING cwa_bbkpf    TYPE ty_wa_bbkpf.

  PERFORM  f_initial_structure
                              USING    'BBKPF'.

  MOVE     '1'                      TO cwa_bbkpf-stype.
  MOVE     p_tcode                  TO cwa_bbkpf-tcode.
  MOVE     sy-datum                 TO cwa_bbkpf-budat.
  MOVE     p_blart                  TO cwa_bbkpf-blart.
  MOVE     iwa_inv_item-bukrs       TO cwa_bbkpf-bukrs.
  MOVE     sy-datum+04(02)          TO cwa_bbkpf-monat.
  MOVE     iwa_inv_item-waers       TO cwa_bbkpf-waers.
  MOVE     iwa_inv_item-xblnr       TO cwa_bbkpf-xblnr.
  MOVE     iwa_inv_item-bktxt       TO cwa_bbkpf-bktxt.

  CONCATENATE
           iwa_inv_item-bldat+00(04)
           iwa_inv_item-bldat+05(02)
           iwa_inv_item-bldat+08(02)
                                  INTO cwa_bbkpf-bldat.

ENDFORM.                    " f_set_structure_bbkpf
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_structure_bbseg
*&---------------------------------------------------------------------*
*       Set values in the data structure - bbseg - G/L & Vendor items
*----------------------------------------------------------------------*
FORM f_set_structure_bbseg
  USING    iwa_inv_item TYPE ty_wa_inv_item
           iv_koart     TYPE koart "K=Vendor, S=G/L
  CHANGING cwa_bbseg    TYPE ty_wa_bbseg.

  DATA:    lv_bukrs     TYPE bukrs,
           lv_lifnr     TYPE lifnr,
           lv_lifnr_p   TYPE lifnr.

  PERFORM  f_initial_structure
                              USING    'BBSEG'.

  MOVE     '2'                      TO cwa_bbseg-stype.
  MOVE     'BBSEG'                  TO cwa_bbseg-tbnam.
  MOVE     iwa_inv_item-bschl       TO cwa_bbseg-newbs.
  MOVE     iwa_inv_item-wrbtr       TO cwa_bbseg-wrbtr.
  MOVE     iwa_inv_item-zlsch       TO cwa_bbseg-zlsch. "Add by RBHATT 04-APR-2012 TR DECK905448
  MOVE     iwa_inv_item-zzref       TO cwa_bbseg-zzref. "Add by RBHATT 04-APR-2012 TR DECK905448
  MOVE     iwa_inv_item-kostl       TO cwa_bbseg-kostl.
  MOVE     iwa_inv_item-aufnr       TO cwa_bbseg-aufnr.
  MOVE     iwa_inv_item-projk       TO cwa_bbseg-projk.
  MOVE     iwa_inv_item-nplnr       TO cwa_bbseg-nplnr.
  MOVE     iwa_inv_item-vornr       TO cwa_bbseg-vornr.
  MOVE     iwa_inv_item-newbk       TO cwa_bbseg-newbk.

  CONCATENATE                          text-t04  "changed from t02 (CIMPL) to t04
*                                                 (Ebill) by AHMADT for CHG0175529
           iwa_inv_item-zuonr          text-t03
           iwa_inv_item-sgtxt     INTO cwa_bbseg-sgtxt.

* Begin changes - delete code   JRHARTUNG  10/10/11  TR0928  DECK902584
* IF     ( iwa_inv_item-saknr(06)   EQ p_saknr+04(06) ).
*   IF   ( iv_koart                 EQ gc_koart_gl    ).
*     CLEAR                            cwa_bbseg-mwskz.
*   ENDIF.
* ELSEIF ( iwa_inv_item-mwskz       IS INITIAL        ).
*   MOVE   p_mwskz                  TO cwa_bbseg-mwskz.
* ELSE.
*   MOVE   iwa_inv_item-mwskz       TO cwa_bbseg-mwskz.
* ENDIF.
* End changes   - delete code   JRHARTUNG  10/10/11  TR0928  DECK902584

  IF     ( iv_koart                 EQ gc_koart_gl    ).

* Begin changes - insert code   JRHARTUNG  10/10/11  TR0928  DECK902584

    IF   ( iwa_inv_item-mwskz   IS NOT INITIAL        ).
      CLEAR                            cwa_bbseg-mwskz.
      MOVE iwa_inv_item-mwskz       TO cwa_bbseg-mwskz.
    ENDIF.

* End changes   - insert code   JRHARTUNG  10/10/11  TR0928  DECK902584

    MOVE   iwa_inv_item-saknr       TO cwa_bbseg-hkont.

    IF   ( iwa_inv_item-zzloc   IS NOT INITIAL        ).
      MOVE iwa_inv_item-zzloc       TO cwa_bbseg-zzloc.
    ENDIF.

    RETURN.

  ENDIF.

*eject
* This code is associated with the vendor line of the accounting doc.
  CLEAR                                lv_bukrs.
  MOVE     iwa_inv_item-bukrs       TO lv_bukrs.

  CLEAR                                lv_lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = iwa_inv_item-lifnr
    IMPORTING
      output = lv_lifnr.

  CLEAR                                lv_lifnr_p.
  MOVE         iwa_inv_item-lifnr   TO lv_lifnr_p.
  IF         ( lv_lifnr_p           NA 'E' ).
    SHIFT      lv_lifnr_p        RIGHT DELETING TRAILING space.
    TRANSLATE  lv_lifnr_p        USING ' 0'.
  ENDIF.

  IF     ( lv_lifnr_p               EQ gc_lifnr_trisk ).

    MOVE   gc_lifnr_trisk+04(06)    TO cwa_bbseg-hkont.
    MOVE   iwa_inv_item-name1       TO cwa_bbseg-name1.
    MOVE   iwa_inv_item-stras       TO cwa_bbseg-stras.
    MOVE   iwa_inv_item-ort01       TO cwa_bbseg-ort01.
    MOVE   iwa_inv_item-pstlz       TO cwa_bbseg-pstlz.
    MOVE   iwa_inv_item-xref3       TO cwa_bbseg-xref3.

    CONCATENATE                        text-t04 "changed from t02 (CIMPL) to t04
*                                                (Ebill) by AHMADT for CHG0175529
           iwa_inv_item-zuonr     INTO cwa_bbseg-zuonr.

  ELSE.

    IF       ( gwa_lfa1-lifnr       EQ lv_lifnr ).
    ELSE.

      CLEAR    gwa_lfa1.
      SELECT   SINGLE lifnr name1 name2 stras ort01 pstlz
        INTO   gwa_lfa1
        FROM   lfa1
       WHERE   lifnr = lv_lifnr.
      IF ( sy-subrc NE 0 ).
        CLEAR  gwa_lfa1.
      ENDIF.

    ENDIF.

    IF       ( gwa_lfa1-lifnr       EQ lv_lifnr ).

      MOVE     iwa_inv_item-lifnr   TO cwa_bbseg-hkont.
      MOVE     gwa_lfa1-name1       TO cwa_bbseg-name1.
      MOVE     gwa_lfa1-name2       TO cwa_bbseg-name2.
      MOVE     gwa_lfa1-stras       TO cwa_bbseg-stras.
      MOVE     gwa_lfa1-ort01       TO cwa_bbseg-ort01.
      MOVE     gwa_lfa1-pstlz       TO cwa_bbseg-pstlz.
      MOVE     iwa_inv_item-xref3   TO cwa_bbseg-xref3.

    ENDIF.

*eject
    IF     ( ( gwa_lfb1-lifnr       EQ lv_lifnr ) AND
             ( gwa_lfb1-bukrs       EQ lv_bukrs )     ).
    ELSE.

      CLEAR    gwa_lfb1.
      SELECT   SINGLE lifnr bukrs zterm
        INTO   gwa_lfb1
        FROM   lfb1
       WHERE   lifnr = lv_lifnr
         AND   bukrs = lv_bukrs.
      IF ( sy-subrc NE 0 ).
        CLEAR  gwa_lfb1.
      ENDIF.

    ENDIF.

    IF     ( ( gwa_lfb1-lifnr       EQ lv_lifnr ) AND
             ( gwa_lfb1-bukrs       EQ lv_bukrs )     ).

      MOVE     gwa_lfb1-zterm       TO cwa_bbseg-zterm.

    ENDIF.

  ENDIF.

ENDFORM.                    " f_set_structure_bbseg
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_structure
*&---------------------------------------------------------------------*
*       Initial the data structures; bbseg, bgr00, and bbkpf
*----------------------------------------------------------------------*
FORM f_initial_structure
  USING    iv_tabname   TYPE tabname.

  DATA:    lv_fieldname TYPE fieldname.

  DATA:    lwa_dd03l    TYPE ty_wa_dd03l.

  FIELD-SYMBOLS: <lfs>  TYPE any.

  CLEAR                                lwa_dd03l.
  LOOP AT  git_dd03l              INTO lwa_dd03l
                                 WHERE tabname = iv_tabname.

    CLEAR                              lv_fieldname.
    MOVE     'GWA_'                 TO lv_fieldname+00(04).
    MOVE     iv_tabname             TO lv_fieldname+04(05).
    MOVE     '-'                    TO lv_fieldname+09(01).
    MOVE     lwa_dd03l-fieldname(20)
                                    TO lv_fieldname+10(20).
    ASSIGN  (lv_fieldname)          TO <lfs>.
    IF ( sy-subrc EQ 0 ).
      CLEAR                            <lfs>.
      MOVE   gc_char_nodata         TO <lfs>.
    ENDIF.

    CLEAR  lwa_dd03l.
  ENDLOOP.

ENDFORM.                    " f_initial_structure
*eject
*&---------------------------------------------------------------------*
*&      Form  f_put_output_data
*&---------------------------------------------------------------------*
*       Output the formatted data file
*----------------------------------------------------------------------*
FORM f_put_output_data
  TABLES   iit_rfbibl00    TYPE ty_it_rfbibl00
           cit_file_stats  STRUCTURE gwa_file_stats.

  DATA:    lv_filename     TYPE text1024.

  DATA:    lwa_file_stats  TYPE ty_wa_file_stats.

  IF     ( gv_flag_err_proc     IS NOT INITIAL ).
    RETURN.
  ENDIF.

* Read the file stats
  CLEAR                                lwa_file_stats.
  READ     TABLE cit_file_stats   INTO lwa_file_stats
                              WITH KEY nbr_file = gv_nbr_file.
  IF ( sy-subrc NE 0 ).
    CLEAR        lwa_file_stats.

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       0
                                       space
                                       space
                                       0
                                       text-151
                                       gv_nbr_file
                                       'Output'
                                       space.

    RETURN.
  ENDIF.

  CLEAR                                lv_filename.
  MOVE       lwa_file_stats-filename_out
                                    TO lv_filename.

  IF       ( rb_appl EQ gc_x )  AND  ( cb_test IS INITIAL ).

    PERFORM  f_transfer_data  TABLES   iit_rfbibl00
                              USING    lv_filename.

  ELSEIF   ( rb_pres EQ gc_x ).

    PERFORM  f_download_data  TABLES   iit_rfbibl00
                              USING    lv_filename.

  ENDIF.

ENDFORM.                    " f_put_output_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_transfer_data
*&---------------------------------------------------------------------*
*       Output data to the application server
*----------------------------------------------------------------------*
FORM f_transfer_data
  TABLES   iit_rfbibl00 TYPE ty_it_rfbibl00
  USING    iv_filename  TYPE text1024.

  DATA:    lv_rc        TYPE numc5,
           lv_msg       TYPE text100.

  DATA:    lwa_rfbibl00 TYPE ty_wa_rfbibl00.

* Open the file in output mode
  OPEN     DATASET iv_filename
           FOR OUTPUT IN TEXT MODE MESSAGE lv_msg ENCODING DEFAULT.

  lv_rc = sy-subrc.

  IF ( lv_rc NE 0 ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-135
                                       iv_filename
                                       lv_msg
                                       space.

    RETURN.
  ENDIF.

  CLEAR                                lwa_rfbibl00.
  LOOP AT  iit_rfbibl00           INTO lwa_rfbibl00.

    TRANSFER                           lwa_rfbibl00
                                    TO iv_filename.

    CLEAR  lwa_rfbibl00.
  ENDLOOP.

* Close the file
  CLOSE    DATASET iv_filename.

ENDFORM.                    " f_transfer_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_download_data
*&---------------------------------------------------------------------*
*       Output data to the presentation server
*----------------------------------------------------------------------*
FORM f_download_data
  TABLES   iit_rfbibl00 TYPE ty_it_rfbibl00
  USING    iv_filename  TYPE text1024.

  DATA:    lv_rc        TYPE numc5,
           lv_filename  TYPE string.

  CLEAR                                lv_filename.
  MOVE     iv_filename              TO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE              =
      filename                  = lv_filename
*     FILETYPE                  = 'ASC'
*     APPEND                    = ' '
*     TRUNC_TRAILING_BLANKS     = ' '
*     WRITE_LF                  = 'X'
*     DAT_MODE                  = ' '
*     CONFIRM_OVERWRITE         = ' '
*     NO_AUTH_CHECK             = ' '
*     TRUNC_TRAILING_BLANKS_EOL = 'X'
    TABLES
      data_tab                  = iit_rfbibl00
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      OTHERS                    = 22.

  lv_rc = sy-subrc.

*eject
  IF ( lv_rc NE 0 ).

    PERFORM  f_error_in_process  USING gv_nbr_file
                                       0
                                       0
                                       lv_rc
                                       space
                                       space
                                       0
                                       text-136
                                       iv_filename
                                       space
                                       space.

    RETURN.
  ENDIF.

ENDFORM.                    " f_download_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_archive_file
*&---------------------------------------------------------------------*
*       Archive the input file
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
                                       text-151
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
                                       text-141
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
                                       text-141
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
                                       text-141
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
           lwa_errs_data   TYPE ty_wa_errs,
           lwa_errs_proc   TYPE ty_wa_errs.

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

* Report mode and error count
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

* Output file
  SKIP    1.
  WRITE: /001 text-r35,
          013 lwa_file_stats-filename_out.

* Output record count
  CLEAR                                lv_count.
  MOVE     lwa_file_stats-cnt_rec_out
                                    TO lv_count.

  WRITE: /001 text-r36,
          016 lv_count.

* BGR00 record count
  CLEAR                                lv_count.
  MOVE     lwa_file_stats-cnt_rec_out_t0
                                    TO lv_count.

  SKIP    1.
  WRITE: /001 text-r37,
          007 text-r36,
          023 lv_count.

* BBKPF record count
  CLEAR                                lv_count.
  MOVE     lwa_file_stats-cnt_rec_out_t1
                                    TO lv_count.

  WRITE: /001 text-r38,
          007 text-r36,
          023 lv_count.

* BBSEG record count
  CLEAR                                lv_count.
  MOVE     lwa_file_stats-cnt_rec_out_t2
                                    TO lv_count.

  WRITE: /001 text-r39,
          007 text-r36,
          023 lv_count.

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
           lwa_errs           TYPE ty_wa_errs.

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

  IF     ( iv_name                  IS INITIAL ).
    RETURN.
  ENDIF.

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

  DATA:    lwa_errs_proc TYPE ty_wa_errs.

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
