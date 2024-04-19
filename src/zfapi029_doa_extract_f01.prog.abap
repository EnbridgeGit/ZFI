*&---------------------------------------------------------------------*
*&  Include           ZFAPI029_DOA_EXTRACT_F01
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI029_DOA_EXTRACT                              *
*  Include:          ZFAPI029_DOA_EXTRACT_F01                          *
*  Author:           MNagarathina                                      *
*  Date:             August 09, 2012                                   *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      AP DOA and User Roles Extract                     *
*                                                                      *                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date      By           Transport   Description                      *
* ---------- ------------ ----------  -------------------------------- *
* 08/09/2012 MNagarathina DECK901428  Initial program development      *
*                                     Ticket#19069
*----------------------------------------------------------------------*

************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Module  pbo_9000  OUTPUT
*&---------------------------------------------------------------------*
*       Screen 9000 Process Before Output Module.  Performs code
*       to create the GUI ALV grid and display the summary report.
*----------------------------------------------------------------------*
MODULE pbo_9000 OUTPUT.
* Set the GUI status and title bar
  SET PF-STATUS 'MAIN9000'.
  SET TITLEBAR  'MAIN9000'.

* Create the ALV grid
  PERFORM  f_create_alv_grid.

ENDMODULE.                 " pbo_9000  OUTPUT
*eject
*&---------------------------------------------------------------------*
*&      Module  pai_9000  INPUT
*&---------------------------------------------------------------------*
*       Screen 9000 Process After Input Module.  Performs code
*       to exit the screen and free space used by the summary report.
*----------------------------------------------------------------------*
MODULE pai_9000 INPUT.
* Process the screen OK code
  CLEAR                      gv_ucomm.
  MOVE     gv_ok_code     TO gv_ucomm.
  CLEAR    gv_ok_code.
  CASE     gv_ucomm.
    WHEN     'BACK' OR 'EXIT' OR 'CANC'.

      PERFORM  f_exit_alv_screen USING gv_ucomm.

  ENDCASE.

ENDMODULE.                 " pai_9000  INPUT
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
   WHERE   paramtype = gc_param_out_int
     AND   subtype   = gc_param_obj_id.
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
                                       key1    = gc_email
                                       key2    = gc_email_id.
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

* If the object ID is not assigned, then disable output file parameters
    IF     ( p_objid       IS INITIAL ).

      IF ( ( screen-group1 EQ gc_modif_id_fpt ) OR
           ( screen-group1 EQ gc_modif_id_ofp ) OR
           ( screen-group1 EQ gc_modif_id_fnm )    ).
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.

    ELSEIF ( rb_appl       EQ gc_x ). "output file appl-srvr radiobutton

* Disable the output filepath & filename; populate with zfit_xparam vals
      IF ( ( screen-group1 EQ gc_modif_id_fpt ) OR
           ( screen-group1 EQ gc_modif_id_ofp ) OR
           ( screen-group1 EQ gc_modif_id_fnm )    ).
        screen-input = 0.
        MODIFY   SCREEN.
      ENDIF.
      %_p_fpath_%_app_%-text = 'Archive Filepath'.
    ELSEIF ( rb_pres       EQ gc_x ). "output file pres-srvr radiobutton
      IF ( screen-name EQ 'P_OFPATH' ).
        %_p_fpath_%_app_%-text = 'Filepath'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
* Enable the output filepath
      IF   ( screen-group1 EQ gc_modif_id_fpt ).
        screen-input = 1.
        MODIFY   SCREEN.
      ENDIF.

* Disable the output filename; value to be included with filepath
      IF   ( screen-group1 EQ gc_modif_id_fnm )
      OR   ( screen-group1 EQ gc_modif_id_ofp ).
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

* Clear the output filepath when switching from appl to pres server
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
                                       key1    = gc_filepath_arch.
    IF ( sy-subrc EQ 0 ).
      CLEAR                            p_fpath.
      MOVE     lwa_xparam-value1    TO p_fpath.
      CLEAR                            p_fname.
      MOVE     lwa_xparam-value2    TO p_fname.
    ENDIF.
    CLEAR                              lwa_xparam.

    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key1    = gc_filepath_out.
    IF ( sy-subrc EQ 0 ).
      CLEAR                            p_ofpath.
      MOVE     lwa_xparam-value1    TO p_ofpath.
    ENDIF.
    CLEAR                              lwa_xparam.
  ENDIF.

* Set the object ID description
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key1    = space
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

* Reset output presentation server flag to control clearing of filepath
  IF     ( rb_appl  IS NOT INITIAL ).
    CLEAR  gv_flag_pres.
  ENDIF.

* Check if the program parameter table is maintained
  IF     ( git_xparam[] IS INITIAL ).
    MESSAGE  e000(zfi01) WITH text-111.
  ENDIF.

  IF     ( rb_appl      EQ gc_x    ).

* Check if the output filepath has been maintained
    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key1    = gc_filepath_out.
    IF ( sy-subrc EQ 0 ).
      IF         ( lwa_xparam-value1   IS INITIAL ).
        MESSAGE  e000(zfi01) WITH text-113.
      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-113.
    ENDIF.

* Check if the archive filepath has been maintained
    CLEAR                              lwa_xparam.
    READ     TABLE git_xparam     INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key1    = gc_filepath_arch.
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
                                       key1    = gc_filepath_err.
    IF ( sy-subrc EQ 0 ).
      IF         ( lwa_xparam-value1   IS INITIAL ).
        MESSAGE  e000(zfi01) WITH text-115.
      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-115.
    ENDIF.

  ENDIF.

* Check if the file delimiter has been maintained
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key1    = gc_file_delimit
                                       key2    = p_delim.
  IF ( sy-subrc EQ 0 ).
    IF         ( lwa_xparam-value1  IS INITIAL ).
      MESSAGE  e000(zfi01) WITH text-116.
    ENDIF.
  ELSE.
    MESSAGE  e000(zfi01) WITH text-116.
  ENDIF.

ENDFORM.                    " f_validate_xparam_entries
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  DATA:    lv_fdpos   TYPE syfdpos.

* Initial the internal tables
  CLEAR    git_msgs[].
  CLEAR    git_errs[].

* Initial the global variables
  CLEAR    gv_filename_ff.
  CLEAR    gv_filename_fn.
  CLEAR    gv_count_recs.
  CLEAR    gv_count_errs.
  CLEAR    gv_flag_error.

* Set the filename
  IF       ( rb_appl            IS NOT INITIAL ).

    IF     ( p_fname            IS NOT INITIAL ).
      CLEAR                            gv_filename_fn.
      MOVE   p_fname                TO gv_filename_fn.
    ELSE.
      CLEAR                            gv_filename_fn.
      MOVE   gc_filename            TO gv_filename_fn.
    ENDIF.

    IF     ( gv_filename_fn         CS 'yyyymmdd' ).
      lv_fdpos = sy-fdpos.
      MOVE   sy-datum               TO gv_filename_fn+lv_fdpos(8).
    ENDIF.

    IF     ( gv_filename_fn         CS 'hhmmss'   ).
      lv_fdpos = sy-fdpos.
      MOVE   sy-uzeit               TO gv_filename_fn+lv_fdpos(6).
    ENDIF.

    IF     ( p_fpath           IS NOT INITIAL ) AND
           ( gv_filename_fn     IS NOT INITIAL ).
      CONCATENATE                      p_fpath
                                       gv_filename_fn
                                  INTO gv_filename_ff.
    ENDIF.

    IF     ( gv_filename_ff         IS INITIAL ).
      MESSAGE  e000(zfi01) WITH text-121.
      LEAVE    LIST-PROCESSING.
    ENDIF.

*eject
  ELSEIF   ( rb_pres IS NOT INITIAL ).

    CLEAR                              gv_filename_ff.
    MOVE     p_fpath                TO gv_filename_ff.

    IF     ( gv_filename_ff         IS INITIAL ).
      MESSAGE  e000(zfi01) WITH text-122.
      LEAVE    LIST-PROCESSING.
    ENDIF.

  ENDIF.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_archive_file
*&---------------------------------------------------------------------*
*       Archive the file
*----------------------------------------------------------------------*
FORM f_archive_file
  USING    iv_filename_ff  TYPE text1024.

  DATA:    iv_filename_fn     TYPE text128.
  DATA:    lwa_xparam         TYPE ty_wa_xparam,
           lwa_errs           TYPE ty_wa_errs,
           lwa_return         TYPE bapireturn.

  DATA:    lv_source_dir      TYPE btcxpgpar,
           lv_target_dir      TYPE btcxpgpar,
           lv_source_fname    TYPE btcxpgpar,
           lv_target_fname    TYPE btcxpgpar,
           lv_command         TYPE char1,
           lv_filename_ff_in  TYPE text1024, "filename in - path + name
           lv_filename_fn_in  TYPE text128,  "filename in - name
           lv_filepath_out    TYPE text1024, "filepath out
           lv_filename_ff_out TYPE text1024, "filename out - path + name
           lv_filename_fn_out TYPE text128,  "filename out - name
           lv_text25          TYPE text25,
           lv_key1            TYPE zparamkey.


  DATA:patt             TYPE string VALUE '\',
       result_tab       TYPE match_result_tab.
  DATA:lv_strlen        TYPE int4.
  DATA:lv_strlen1       TYPE int4.
  DATA:lv_offset        TYPE int4.
  DATA:lv_length        TYPE int4.
  DATA:lv_filename      TYPE string.
  DATA:lv_filename1     TYPE string.
  FIELD-SYMBOLS:<match> LIKE LINE OF result_tab.

  CLEAR:lv_strlen1,lv_strlen.
  CLEAR:lv_filename,lv_filename1.

  MOVE:iv_filename_ff              TO lv_filename1.

  FIND ALL OCCURRENCES OF patt IN lv_filename1
       RESULTS result_tab.

  lv_strlen1 = strlen( lv_filename1 ).

  SORT:result_tab BY offset DESCENDING.
  CLEAR:lv_offset,lv_length.

  LOOP AT result_tab ASSIGNING <match>.
    lv_strlen = lv_strlen1 - <match>-offset.
    lv_length = lv_strlen - 1.
    lv_offset = <match>-offset + 1.
    iv_filename_fn =  lv_filename1+lv_offset(lv_length).
    EXIT.
  ENDLOOP.

* Set the source directory path and filename
  CLEAR    lv_source_dir.
  CLEAR    lv_source_fname.
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key1    = gc_filepath_arch.
  IF ( sy-subrc EQ 0 ).
    MOVE     lwa_xparam-value1      TO lv_source_dir.
    MOVE     iv_filename_fn         TO lv_source_fname.
  ENDIF.

* Set the target directory path and filename
  CLEAR    lv_target_dir.
  CLEAR    lv_target_fname.
  CLEAR                                lv_key1.
  CLEAR                                lv_command.
  IF     ( gv_flag_error            IS INITIAL ).
    MOVE   gc_filepath_out          TO lv_key1.
    MOVE   gc_c                     TO lv_command.
  ELSE.
    MOVE   gc_filepath_err          TO lv_key1.
    MOVE   gc_m                     TO lv_command.
  ENDIF.
  CLEAR                                lwa_xparam.
  READ     TABLE git_xparam       INTO lwa_xparam
                              WITH KEY subtype = gv_obj_id
                                       key1    = lv_key1.
  IF ( sy-subrc EQ 0 ).
    MOVE     lwa_xparam-value1      TO lv_target_dir.
    MOVE     iv_filename_fn         TO lv_target_fname.
  ENDIF.

*eject
* Copy the file to the main filepath
  IF   ( ( lv_source_dir            IS INITIAL ) OR
         ( lv_target_dir            IS INITIAL ) OR
         ( lv_source_fname          IS INITIAL ) OR
         ( lv_target_fname          IS INITIAL )    ).
    MESSAGE  i000(zfi01) WITH text-141 lv_target_dir lv_target_fname.
    CLEAR                              gv_flag_error.
    MOVE     gc_x                   TO gv_flag_error.
    ADD      1                      TO gv_count_errs.
    CLEAR                              lwa_errs.
    CONCATENATE                        text-141
                                       lv_target_dir
                                       lv_target_fname
                                  INTO lwa_errs
                          SEPARATED BY space.
    APPEND   lwa_errs               TO git_errs.
    RETURN.
  ENDIF.

  WAIT     UP TO 1 SECONDS.
  CLEAR    lwa_return.

  CALL FUNCTION 'ZFI_FILE_HANDLE'
    EXPORTING
      i_source_dir        = lv_source_dir
      i_target_dir        = lv_target_dir
      i_source_fname      = lv_source_fname
      i_target_fname      = lv_target_fname
      i_command           = lv_command
*     I_DATE_TIME_STAMP   = SPACE
*     I_RENAME_ARC_TO_NEW = SPACE
    IMPORTING
      e_return            = lwa_return.

  IF   ( lwa_return-type CS 'aAeE' ).
    MESSAGE  i000(zfi01) WITH lwa_return-message.
    CLEAR                              gv_flag_error.
    MOVE     gc_x                   TO gv_flag_error.
    ADD      1                      TO gv_count_errs.
    CLEAR                              lwa_errs.
    MOVE     lwa_return-message     TO lwa_errs.
    APPEND   lwa_errs               TO git_errs.
    RETURN.
  ENDIF.

  CLEAR:gv_ff_ofname.
  CONCATENATE:lv_target_dir lv_target_fname INTO
         gv_ff_ofname.
*eject
* Set the archive filename
  CLEAR                                lv_filename_ff_in.
  CLEAR                                lv_filename_fn_in.
  MOVE     lv_source_fname          TO lv_filename_fn_in.
  CLEAR                                lv_filename_ff_out.
  CLEAR                                lv_filename_fn_out.

  IF     ( gv_flag_error            IS INITIAL ).
    CLEAR                              lv_filepath_out.
    MOVE   lv_source_dir            TO lv_filepath_out.
    CLEAR                              lv_text25.
    MOVE   gc_extension_arch        TO lv_text25.
  ELSE.
    CLEAR                              lv_source_dir.
    MOVE   lv_target_dir            TO lv_source_dir.
    CLEAR                              lv_filepath_out.
    MOVE   lv_source_dir            TO lv_filepath_out.
    CLEAR                              lv_text25.
    MOVE   gc_extension_err         TO lv_text25.
  ENDIF.

  PERFORM  f_generate_filename
                              USING    lv_filename_ff_in
                                       lv_filename_fn_in
                                       lv_filepath_out
                                       gc_x   "delete existing extension
                                       lv_text25            "append text
                              CHANGING lv_filename_ff_out
                                       lv_filename_fn_out.

* Rename the archive file
  CLEAR                                lv_target_dir.
  CLEAR                                lv_target_fname.
  MOVE     lv_filename_fn_out       TO lv_target_fname.

  WAIT     UP TO 1 SECONDS.
  CLEAR    lwa_return.

  CALL FUNCTION 'ZFI_FILE_HANDLE'
    EXPORTING
      i_source_dir        = lv_source_dir
*     I_TARGET_DIR        = lv_target_dir
      i_source_fname      = lv_source_fname
      i_target_fname      = lv_target_fname
      i_command           = gc_r
*     I_DATE_TIME_STAMP   = SPACE
*     I_RENAME_ARC_TO_NEW = SPACE
    IMPORTING
      e_return            = lwa_return.

*eject
  IF   ( lwa_return-type CS 'aAeE' ).
    MESSAGE  i000(zfi01) WITH lwa_return-message.
    CLEAR                              gv_flag_error.
    MOVE     gc_x                   TO gv_flag_error.
    ADD      1                      TO gv_count_errs.
    CLEAR                              lwa_errs.
    MOVE     lwa_return-message     TO lwa_errs.
    APPEND   lwa_errs               TO git_errs.
    RETURN.
  ENDIF.

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

  DATA:    lv_dt_ts           TYPE string.

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

  CLEAR:lv_dt_ts.
  CONCATENATE:sy-datum sy-uzeit INTO lv_dt_ts.
  CONDENSE:lv_dt_ts.
* Append the new file extension if it is given
  IF     ( iv_filename_extn     IS NOT INITIAL ).
    CLEAR                              cv_filename_fn_out.
    CONCATENATE                        lv_filename_fn
                                       lv_dt_ts
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
*&      Form  f_prepare_messages
*&---------------------------------------------------------------------*
*       Prepare the messages
*----------------------------------------------------------------------*
FORM f_prepare_messages.
  CONSTANTS:lc_one   TYPE numc2 VALUE '01'.

  DATA:    lv_count TYPE numc5.

  DATA:    lwa_msgs TYPE ty_wa_msgs,
           lwa_errs TYPE ty_wa_errs.

* Prepare the header messages
  CLEAR                                gv_sysid.
  MOVE     sy-sysid                 TO gv_sysid.
  MOVE     sy-mandt                 TO gv_sysid+4(3).

  CLEAR    lwa_msgs.
  APPEND   lwa_msgs                  TO git_msgs.

  MOVE     text-h11                  TO lwa_msgs+000(010).
  MOVE     gv_sysid                  TO lwa_msgs+011(008).
  MOVE     text-h12                  TO lwa_msgs+050(010).
  MOVE     sy-uname                  TO lwa_msgs+061(012).
  MOVE     text-h13                  TO lwa_msgs+102(005).
  WRITE    lc_one                    TO lwa_msgs+108(010).
  APPEND   lwa_msgs                  TO git_msgs.

  CLEAR                                 lwa_msgs.
  MOVE     text-h14                  TO lwa_msgs+000(011).
  MOVE     sy-cprog                  TO lwa_msgs+012(008).
  MOVE     text-h15                  TO lwa_msgs+050(009).
  WRITE    sy-datum                  TO lwa_msgs+060(012).
  MOVE     text-h16                  TO lwa_msgs+102(009).
  WRITE    sy-uzeit                  TO lwa_msgs+112(010).
  APPEND   lwa_msgs                  TO git_msgs.

*eject
* Prepare the control messages
  CLEAR                                 lwa_msgs.
  APPEND   lwa_msgs                  TO git_msgs.
  APPEND   lwa_msgs                  TO git_msgs.
  WRITE    sy-uline(52)              TO lwa_msgs+000(052).
  MOVE     text-r11                  TO lwa_msgs+053(014).
  WRITE    sy-uline(52)              TO lwa_msgs+068(052).
  APPEND   lwa_msgs                  TO git_msgs.

  CLEAR                                 lv_count.
  MOVE     gv_count_errs             TO lv_count.

  CLEAR                                 lwa_msgs.
  APPEND   lwa_msgs                  TO git_msgs.
  MOVE     text-r21                  TO lwa_msgs+000(018).
  IF     ( cb_test IS INITIAL ).
    MOVE   text-rmp                  TO lwa_msgs+019(004).
  ELSE.
    MOVE   text-rmt                  TO lwa_msgs+019(004).
  ENDIF.

  MOVE     text-r22                  TO lwa_msgs+053(007).
  WRITE    lv_count                  TO lwa_msgs+061(005).
  APPEND   lwa_msgs                  TO git_msgs.

  IF     ( cb_test IS INITIAL ).

    CLEAR                               lv_count.
    lv_count = lv_count + lines( git_zfit_doa[] ).
    lv_count = lv_count + lines( git_agr_users[] ).

    CLEAR                               lwa_msgs.
    APPEND   lwa_msgs                TO git_msgs.
    MOVE     text-r25                TO lwa_msgs+000(012).
    MOVE     gv_ff_doctype           TO lwa_msgs+013.
    APPEND   lwa_msgs                TO git_msgs.
    CLEAR                               lwa_msgs.
    MOVE     text-r26                TO lwa_msgs+000(015).
    CLEAR:lv_count.
    lv_count = lines( git_t003t[] ).
    WRITE    lv_count                TO lwa_msgs+016(005).
    APPEND   lwa_msgs                TO git_msgs.

    CLEAR                               lwa_msgs.
    APPEND   lwa_msgs                TO git_msgs.
    MOVE     text-r25                TO lwa_msgs+000(012).
    MOVE     gv_ff_doctype_ug        TO lwa_msgs+013.
    APPEND   lwa_msgs                TO git_msgs.
    CLEAR                               lwa_msgs.
    MOVE     text-r26                TO lwa_msgs+000(015).
    CLEAR:lv_count.
    lv_count = lines( git_t003t_ug[] ).
    WRITE    lv_count                TO lwa_msgs+016(005).
    APPEND   lwa_msgs                TO git_msgs.

    CLEAR                               lwa_msgs.
    APPEND   lwa_msgs                TO git_msgs.
    MOVE     text-r25                TO lwa_msgs+000(012).
    MOVE     gv_ff_doctype_sw        TO lwa_msgs+013.
    APPEND   lwa_msgs                TO git_msgs.
    CLEAR                               lwa_msgs.
    MOVE     text-r26                TO lwa_msgs+000(015).
    CLEAR:lv_count.
    lv_count = lines( git_t003t_sw[] ).
    WRITE    lv_count                TO lwa_msgs+016(005).
    APPEND   lwa_msgs                TO git_msgs.

    CLEAR                               lwa_msgs.
    APPEND   lwa_msgs                TO git_msgs.
    MOVE     text-r25                TO lwa_msgs+000(012).
    MOVE     gv_ff_doa               TO lwa_msgs+013.
    APPEND   lwa_msgs                TO git_msgs.
    CLEAR                               lwa_msgs.
    MOVE     text-r26                TO lwa_msgs+000(015).
    CLEAR:lv_count.
    lv_count = lines( git_zfit_doa[] ).
    WRITE    lv_count                TO lwa_msgs+016(005).
    APPEND   lwa_msgs                TO git_msgs.

    CLEAR                               lwa_msgs.
    APPEND   lwa_msgs                TO git_msgs.
    MOVE     text-r25                TO lwa_msgs+000(012).
    MOVE     gv_ff_agrusers          TO lwa_msgs+013.
    APPEND   lwa_msgs                TO git_msgs.
    CLEAR                               lwa_msgs.
    MOVE     text-r26                TO lwa_msgs+000(015).
    CLEAR:lv_count.
    lv_count = lines( git_agr_users[] ).
    WRITE    lv_count                TO lwa_msgs+016(005).
    APPEND   lwa_msgs                TO git_msgs.
  ENDIF.

*eject
* Prepare the error messages
  IF     ( gv_count_errs             GT 0 ).

    CLEAR                               lwa_msgs.
    APPEND   lwa_msgs                TO git_msgs.
    APPEND   lwa_msgs                TO git_msgs.
    WRITE    sy-uline(56)            TO lwa_msgs+000(056).
    MOVE     text-r31                TO lwa_msgs+057(006).
    WRITE    sy-uline(56)            TO lwa_msgs+064(056).
    APPEND   lwa_msgs                TO git_msgs.

    CLEAR                               lwa_msgs.
    APPEND   lwa_msgs                TO git_msgs.

    CLEAR                               lwa_errs.
    LOOP AT  git_errs              INTO lwa_errs.
      CLEAR                             lwa_msgs.
      MOVE   lwa_errs                TO lwa_msgs.
      APPEND lwa_msgs                TO git_msgs.
      CLEAR  lwa_errs.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " f_prepare_messages
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

  IF     ( gv_flag_error            IS INITIAL ).
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
                              WITH KEY key1 = gc_email
                                       key2 = gc_email_std_txt.
  IF ( sy-subrc EQ 0 ).

    CLEAR                              lv_name.
    IF     ( gv_flag_error          IS INITIAL ).
      MOVE   lwa_xparam-value1      TO lv_name.
    ELSE. "( gv_flag_error      IS NOT INITIAL ).
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
  IF       ( gv_flag_error      IS NOT INITIAL ).
    CLEAR                              lwa_contents_txt.
    APPEND   lwa_contents_txt       TO lit_contents_txt.
    MOVE     text-mpe               TO lwa_contents_txt-line.
    APPEND   lwa_contents_txt       TO lit_contents_txt.
    CLEAR                              lwa_contents_txt.
    APPEND   lwa_contents_txt       TO lit_contents_txt.
    CLEAR                              lwa_errs.
    LOOP AT  git_errs             INTO lwa_errs.
      CLEAR                            lwa_contents_txt.
      MOVE   lwa_errs-line          TO lwa_contents_txt-line.
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
    MESSAGE  i000(zfi01) WITH text-192 lv_rc.
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

  IF ( lv_rc NE 0 ).
    MESSAGE  i000(zfi01) WITH text-191 lv_rc.
  ENDIF.

*eject
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
*&      Form  f_display_results
*&---------------------------------------------------------------------*
*       Display the results
*----------------------------------------------------------------------*
FORM f_display_results.

  IF   ( git_msgs[] IS NOT INITIAL ).

    CALL SCREEN 9000.

  ENDIF.

ENDFORM.                    " f_display_results
*eject
*&---------------------------------------------------------------------*
*&      Form  f_exit_alv_screen
*&---------------------------------------------------------------------*
*       Exit the ALV screen and return
*----------------------------------------------------------------------*
FORM f_exit_alv_screen
  USING    iv_ucomm TYPE syucomm.

  IF ( ( iv_ucomm EQ 'BACK' ) OR
       ( iv_ucomm EQ 'EXIT' ) OR
       ( iv_ucomm EQ 'CANC' )    ).

* Return to list processing
    SET   SCREEN 0.
    LEAVE SCREEN.

  ENDIF.

ENDFORM.                    " f_exit_alv_screen
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_alv_grid
*&---------------------------------------------------------------------*
*       Create the ALV grid
*----------------------------------------------------------------------*
FORM f_create_alv_grid.

  CONSTANTS:
           lc_container_name
                         TYPE scrfname VALUE 'GS_CUSTOM_CONTAINER'.

  DATA:    lr_container TYPE REF TO cl_gui_custom_container,
           lr_cont1     TYPE REF TO cl_gui_container,
           lr_cont2     TYPE REF TO cl_gui_container,
           lr_split     TYPE REF TO cl_gui_splitter_container.

  DATA:    lv_count     TYPE syindex.

  CLEAR         lv_count.
  IF     ( git_msgs[] IS NOT INITIAL ).
    ADD    1 TO lv_count.
  ENDIF.

* Create the container, provided execution is not in the background
  IF   ( cl_salv_table=>is_offline( ) EQ if_salv_c_bool_sap=>false ).

    CREATE OBJECT lr_container
      EXPORTING
        container_name = lc_container_name.

    CREATE OBJECT lr_split
      EXPORTING
        parent  = lr_container
        rows    = lv_count
        columns = 1.

    IF ( git_msgs[] IS NOT INITIAL ).

      CALL METHOD lr_split->get_container
        EXPORTING
          row       = lv_count
          column    = 1
        RECEIVING
          container = lr_cont1.

      SUBTRACT 1 FROM lv_count.

* Display messages - foreground

      PERFORM f_display_output USING git_msgs[]
                                     lr_cont1
                                     'MESSAGES'.

    ENDIF.

  ELSE.

* Display messages - background

    PERFORM f_display_output USING git_msgs[]
                                   lr_cont1
                                   'MESSAGES'.

  ENDIF.

ENDFORM.                    " f_create_alv_grid
*eject
*&---------------------------------------------------------------------*
*&      Form  f_display_output
*&---------------------------------------------------------------------*
*       Display the output
*----------------------------------------------------------------------*
FORM f_display_output
  USING    iit_outtab    TYPE table
           ir_container  TYPE REF TO cl_gui_container
           iv_text       TYPE lvc_title.

  DATA:    lv_griddsp    TYPE REF TO cl_salv_display_settings.

  DATA:    lcl_functions TYPE REF TO cl_salv_functions_list.

  TRY.

      CALL METHOD cl_salv_table=>factory
        EXPORTING
          r_container  = ir_container
          list_display = if_salv_c_bool_sap=>false
        IMPORTING
          r_salv_table = gv_table
        CHANGING
          t_table      = iit_outtab.

* Get Default Functions
      lcl_functions = gv_table->get_functions( ).
      lcl_functions->set_all( abap_true ).

* Set Column Optimize
      gv_columns = gv_table->get_columns( ).
      CALL METHOD gv_columns->set_optimize
        EXPORTING
          value = if_salv_c_bool_sap=>true.

* ALV Title
      lv_griddsp = gv_table->get_display_settings( ).
      lv_griddsp->set_striped_pattern( gc_x ).
      lv_griddsp->set_list_header( iv_text ).

* Display ALV
      CALL METHOD gv_table->display.

    CATCH cx_salv_msg .                                 "#EC NO_HANDLER

  ENDTRY.

ENDFORM.                    " f_display_output
*&---------------------------------------------------------------------*
*&      Form  F_SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_select_data.
  DATA: lv_rfc TYPE TB_RFCDEST.
  DATA: T_APPR TYPE TABLE OF AGR_USERS WITH HEADER LINE.

  CLEAR:git_t003t[].
  CLEAR:git_zfit_doa[],git_agr_users[].

  SELECT *
    FROM t003t
    INTO CORRESPONDING FIELDS OF TABLE git_t003t
   WHERE spras EQ 'E'.

* Begin of Changes by <Chaya>
*  SELECT *
*    FROM zfit_doa
*    INTO CORRESPONDING FIELDS OF TABLE git_zfit_doa.
  SELECT *
    FROM zfit_doa_new
    INTO CORRESPONDING FIELDS OF TABLE git_zfit_doa.
* End of Changes by <Chaya>

  SELECT *
    FROM agr_users
    INTO CORRESPONDING FIELDS OF TABLE git_agr_users
   WHERE agr_name LIKE 'ZEC:FI%AP%IA%'.

  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      IMP_PARAMTYPE = 'ECCUG'
    IMPORTING
      EXP_RFCDEST   = lv_rfc.

  IF NOT LV_RFC IS INITIAL.
    CALL FUNCTION 'ZFI_DOCTYP_INVAPPR' DESTINATION lv_rfc
      TABLES
        APPROVERS = T_APPR
        DOCTYPES  = git_t003t_UG.

    APPEND LINES OF T_APPR[] to git_agr_users[].
  ENDIF.

  CLEAR: LV_RFC.

* Begin of Changes by <Chaya>

*  CALL FUNCTION 'ZFI_GET_RFC_DEST'
*    EXPORTING
*      IMP_PARAMTYPE = 'ECCSW'
*    IMPORTING
*      EXP_RFCDEST   = lv_rfc.
*
*  IF NOT LV_RFC IS INITIAL.
*    CALL FUNCTION 'ZFI_DOCTYP_INVAPPR' DESTINATION lv_rfc
*      TABLES
*        APPROVERS = T_APPR
*        DOCTYPES  = git_t003t_SW.
*
*    APPEND LINES OF T_APPR[] to git_agr_users[].
*  ENDIF.

* End of Changes by <Chaya>

ENDFORM.                    " F_SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_generate_output.
  DATA:lwa_t003t         TYPE ty_wa_t003t,
       lwa_zfit_doa      TYPE ty_wa_zfit_doa,
       lwa_agr_users     TYPE ty_wa_agr_users,
       lwa_output        TYPE ty_wa_output.

  DATA:lit_output        TYPE STANDARD TABLE OF ty_wa_output.
  DATA:lv_amount         TYPE string.
  DATA:lv_filename       TYPE text1024.
  DATA:lwa_errs          TYPE ty_wa_errs.
  DATA:lv_string         TYPE string.

*doc type entries
  CLEAR:lit_output[].
  CLEAR:gv_count_recs.
  CLEAR:lwa_output.
  CLEAR:gv_ff_doctype,gv_ff_doa,gv_ff_agrusers.

  CONCATENATE text-c01 text-c02 text-c03 INTO lwa_output-record
    SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

  APPEND:lwa_output               TO lit_output.
  ADD:1                           TO gv_count_recs.

  CLEAR:lwa_t003t.
  LOOP AT  git_t003t               INTO lwa_t003t.
    CLEAR:lwa_output.
    CONCATENATE:lwa_t003t-blart lwa_t003t-ltext INTO lwa_output-record
      SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    APPEND:lwa_output TO lit_output.
    ADD:1             TO gv_count_recs.
  ENDLOOP.

  CLEAR:lv_string.
  MOVE:'_doctype'        TO lv_string.
  MOVE:p_fpath           TO lv_filename.

  IF rb_appl            EQ gc_x.
    MOVE:gv_filename_ff TO lv_filename.
  ENDIF.

  CLEAR:gv_ff_ofname.
  PERFORM f_get_filename USING  lv_string  CHANGING lv_filename.
  PERFORM f_trasfer_data TABLES lit_output USING    lv_filename.
  MOVE:lv_filename      TO gv_ff_doctype.
*  MOVE:gv_ff_ofname     TO gv_ff_doctype.

* UG doc type entries
  CLEAR:lit_output[].
  CLEAR:gv_count_recs.
  CLEAR:lwa_output.
  CLEAR:gv_ff_doctype_ug.

  CONCATENATE text-c01 text-c02 text-c03 INTO lwa_output-record
    SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

  APPEND:lwa_output               TO lit_output.
  ADD:1                           TO gv_count_recs.

  CLEAR:lwa_t003t.
  LOOP AT  git_t003t_ug               INTO lwa_t003t.
    CLEAR:lwa_output.
    CONCATENATE:lwa_t003t-blart lwa_t003t-ltext INTO lwa_output-record
      SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    APPEND:lwa_output TO lit_output.
    ADD:1             TO gv_count_recs.
  ENDLOOP.

  CLEAR:lv_string.
  MOVE:'_doctypeUG'        TO lv_string.
  MOVE:p_fpath           TO lv_filename.

  IF rb_appl            EQ gc_x.
    MOVE:gv_filename_ff TO lv_filename.
  ENDIF.

  CLEAR:gv_ff_ofname.
  PERFORM f_get_filename USING  lv_string  CHANGING lv_filename.
  PERFORM f_trasfer_data TABLES lit_output USING    lv_filename.
  MOVE:lv_filename      TO gv_ff_doctype_ug.
*  MOVE:gv_ff_ofname     TO gv_ff_doctype_ug.

* SW doc type entries
  CLEAR:lit_output[].
  CLEAR:gv_count_recs.
  CLEAR:lwa_output.
  CLEAR:gv_ff_doctype_sw.

  CONCATENATE text-c01 text-c02 text-c03 INTO lwa_output-record
    SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

  APPEND:lwa_output               TO lit_output.
  ADD:1                           TO gv_count_recs.

  CLEAR:lwa_t003t.
  LOOP AT  git_t003t_sw               INTO lwa_t003t.
    CLEAR:lwa_output.
    CONCATENATE:lwa_t003t-blart lwa_t003t-ltext INTO lwa_output-record
      SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    APPEND:lwa_output TO lit_output.
    ADD:1             TO gv_count_recs.
  ENDLOOP.

  CLEAR:lv_string.
  MOVE:'_doctypeSW'        TO lv_string.
  MOVE:p_fpath           TO lv_filename.

  IF rb_appl            EQ gc_x.
    MOVE:gv_filename_ff TO lv_filename.
  ENDIF.

  CLEAR:gv_ff_ofname.
  PERFORM f_get_filename USING  lv_string  CHANGING lv_filename.
  PERFORM f_trasfer_data TABLES lit_output USING    lv_filename.
  MOVE:lv_filename      TO gv_ff_doctype_sw.
*  MOVE:gv_ff_ofname     TO gv_ff_doctype_sw.

*zfit_doa entries
  CLEAR:lit_output[].
  CLEAR:gv_count_recs.
  CLEAR :lwa_output.

  CONCATENATE:text-c11 text-c12 text-c13 text-c14 text-c15
         INTO lwa_output-record
    SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

  APPEND:lwa_output               TO lit_output.
  ADD:1                           TO gv_count_recs.

  CLEAR:lwa_zfit_doa.
  LOOP AT  git_zfit_doa               INTO lwa_zfit_doa.
    CLEAR:lwa_output.
    lv_amount = lwa_zfit_doa-amount.
    CONCATENATE:lwa_zfit_doa-lookup_key lwa_zfit_doa-lookup_key_type
                lwa_zfit_doa-tcode      lwa_zfit_doa-doc_type
                lv_amount
          INTO lwa_output-record
     SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    APPEND:lwa_output TO lit_output.
    ADD:1             TO gv_count_recs.
  ENDLOOP.

  CLEAR:lv_string.
  MOVE:'_doa'            TO lv_string.
  MOVE:p_fpath           TO lv_filename.
  IF rb_appl  EQ gc_x.
    MOVE:gv_filename_ff TO lv_filename.
  ENDIF.

  CLEAR:gv_ff_ofname.
  PERFORM f_get_filename USING  lv_string  CHANGING lv_filename.
  PERFORM f_trasfer_data TABLES lit_output USING    lv_filename.
  MOVE:lv_filename      TO gv_ff_doa.
*  MOVE:gv_ff_ofname     TO gv_ff_doa.

*agr_users entries
  CLEAR:lit_output[].
  CLEAR:gv_count_recs.
  CLEAR:lwa_output.
  CONCATENATE text-c21 text-c22 INTO lwa_output-record
    SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

  APPEND:lwa_output               TO lit_output.
  ADD:1                           TO gv_count_recs.

  CLEAR:lwa_zfit_doa.
  LOOP AT  git_agr_users               INTO lwa_agr_users.
    CLEAR:lwa_output.
    CONCATENATE:lwa_agr_users-agr_name lwa_agr_users-uname
           INTO lwa_output-record
      SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    APPEND:lwa_output TO lit_output.
    ADD:1             TO gv_count_recs.
  ENDLOOP.

  CLEAR:lv_string.
  MOVE:'_agr'            TO lv_string.
  MOVE:p_fpath           TO lv_filename.
  IF rb_appl  EQ gc_x.
    MOVE:gv_filename_ff TO lv_filename.
  ENDIF.

  CLEAR:gv_ff_ofname.
  PERFORM f_get_filename USING  lv_string  CHANGING lv_filename.
  PERFORM f_trasfer_data TABLES lit_output USING    lv_filename.
  MOVE:lv_filename      TO gv_ff_agrusers.
*  MOVE:gv_ff_ofname     TO gv_ff_agrusers.

  PERFORM f_prepare_messages.

ENDFORM.                    " F_GENERATE_OUTPUT_EXTRACT
*&---------------------------------------------------------------------*
*&      Form  F_TRANSFER_DATA_EXTRACT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_transfer_data_extract
       TABLES iit_output     STRUCTURE gwa_output
       USING  iv_filename_ff TYPE      text1024.

  DATA:lv_msg         TYPE text100.
  DATA:lwa_output     TYPE ty_wa_output,
       lwa_errs       TYPE ty_wa_errs.

* Open the file in output mode
  OPEN DATASET iv_filename_ff
   FOR OUTPUT IN TEXT MODE MESSAGE lv_msg ENCODING DEFAULT.

  IF ( sy-subrc NE 0 ).

    MESSAGE  i000(zfi01) WITH text-135 iv_filename_ff lv_msg.
    CLEAR                              gv_flag_error.
    MOVE     gc_x                   TO gv_flag_error.
    ADD      1                      TO gv_count_errs.
    CLEAR                              lwa_errs.
    CONCATENATE                        text-135
                                       iv_filename_ff
                                       lv_msg
                                  INTO lwa_errs
                          SEPARATED BY space.
    APPEND   lwa_errs               TO git_errs.
    RETURN.
  ENDIF.

  CLEAR                                lwa_output.
  LOOP AT  iit_output             INTO lwa_output.
    TRANSFER lwa_output-record    TO iv_filename_ff.
    CLEAR  lwa_output.
  ENDLOOP.

* Close the file
  CLOSE    DATASET iv_filename_ff.

* Archive the file

  PERFORM  f_archive_file        USING iv_filename_ff.

ENDFORM.                    " F_TRANSFER_DATA_EXTRACT
*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_DATA_EXTRACT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_download_data_extract
     TABLES iit_output  STRUCTURE gwa_output
     USING iv_filename TYPE text1024.

  DATA:lv_filename  TYPE string.
  DATA:lwa_errs       TYPE ty_wa_errs.

  CLEAR:lv_filename.
  MOVE:iv_filename              TO lv_filename.


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
      data_tab                  = iit_output
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

*eject
  IF ( sy-subrc NE 0 ).
    MESSAGE  i000(zfi01) WITH text-136 iv_filename.
    CLEAR                              gv_flag_error.
    MOVE     gc_x                   TO gv_flag_error.
    ADD      1                      TO gv_count_errs.
    CLEAR                              lwa_errs.
    CONCATENATE                        text-136
                                       iv_filename
                                  INTO lwa_errs
                          SEPARATED BY space.
    APPEND   lwa_errs               TO git_errs.
    RETURN.
  ENDIF.


ENDFORM.                    " F_DOWNLOAD_DATA_EXTRACT
*&---------------------------------------------------------------------*
*&      Form  F_TRASFER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_OUTPUT  text
*      -->P_GV_FILENAME_FF  text
*----------------------------------------------------------------------*
FORM f_trasfer_data  TABLES   p_lit_output     STRUCTURE gwa_output
                      USING   p_gv_filename_ff TYPE      text1024.

  IF       ( rb_appl EQ gc_x )  AND  ( cb_test IS INITIAL ).
    PERFORM  f_transfer_data_extract TABLES p_lit_output
                                     USING  p_gv_filename_ff.
  ELSEIF   ( rb_pres EQ gc_x ).
    PERFORM  f_download_data_extract TABLES p_lit_output
                                     USING  p_gv_filename_ff.
  ENDIF.

ENDFORM.                    " F_TRASFER_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text.
*----------------------------------------------------------------------*
FORM f_get_filename USING    p_string   TYPE string
                    CHANGING p_filename TYPE text1024.

  DATA:patt             TYPE string VALUE '.',
       result_tab       TYPE match_result_tab.
  DATA:lv_strlen        TYPE int4.
  DATA:lv_strlen1       TYPE int4.
  DATA:lv_filename      TYPE string.
  DATA:lv_filename1     TYPE string.
  FIELD-SYMBOLS:<match> LIKE LINE OF result_tab.

  CLEAR:lv_strlen1,lv_strlen.
  CLEAR:lv_filename,lv_filename1.

  MOVE:p_filename              TO lv_filename1.

  FIND ALL OCCURRENCES OF patt IN lv_filename1
       RESULTS result_tab.

  lv_strlen1 = strlen( lv_filename1 ).
  SORT:result_tab BY offset DESCENDING.

  LOOP AT result_tab ASSIGNING <match>.
    lv_strlen = lv_strlen1 - <match>-offset.
    CONCATENATE:lv_filename1+0(<match>-offset) p_string lv_filename1+<match>-offset(lv_strlen)
           INTO lv_filename.
    CONDENSE:lv_filename.
    MOVE:lv_filename TO p_filename.
    EXIT.
  ENDLOOP.

ENDFORM.                    " GET_FILENAME

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
* Set the GUI status and title bar
  SET PF-STATUS 'MAIN9000'.
  SET TITLEBAR  'MAIN9000'.

* Create the ALV grid
  PERFORM  f_create_alv_grid.

ENDMODULE.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

* Process the screen OK code
  CLEAR                      gv_ucomm.
  MOVE     gv_ok_code     TO gv_ucomm.
  CLEAR    gv_ok_code.
  CASE     gv_ucomm.
    WHEN     'BACK' OR 'EXIT' OR 'CANC'.

      PERFORM  f_exit_alv_screen USING gv_ucomm.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
