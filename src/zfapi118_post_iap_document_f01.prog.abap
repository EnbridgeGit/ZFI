*&---------------------------------------------------------------------*
*&  Include           ZFAPI118_POST_IAP_DOCUMENT_F01
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI118_POST_IAP_DOCUMENT                        *
*  Include:          ZFAPI118_POST_IAP_DOCUMENT_F01                    *
*  Author:           Vijay Rajaputra ( Copy of US System )             *
*  Date:             Aug, 01 2018                                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      Post IAP Documents for PO and Non-PO Invoices     *
*                    and Credit Memos                                  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 08/01/18 VRAJAPUTRA D30K928896 - CHG0109670 - Initial program        *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  f_initialization
*&---------------------------------------------------------------------*
*       Initialization
*----------------------------------------------------------------------*
FORM f_initialization.

  DATA:    lt_sscr_restrict            TYPE sscr_restrict,
           ls_sscr_opt_list            TYPE sscr_opt_list,
           ls_sscr_ass                 TYPE sscr_ass.

  MOVE   gc_fpth1a                       TO p_fpth1a.
  MOVE   gc_fnam1a                       TO p_fnam1a.
  MOVE   gc_fregxa                       TO p_fregxa.
  MOVE   gc_farc1a                       TO p_farc1a.
  MOVE   gc_fpth2a                       TO p_fpth2a.
  MOVE   gc_fnam2a                       TO p_fnam2a.
  MOVE   gc_farc2a                       TO p_farc2a.
  MOVE   gc_fpth3a                       TO p_fpth3a.
  MOVE   gc_fnam3a                       TO p_fnam3a.

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
  MOVE     'S_EMAIL'                TO ls_sscr_ass-name.
  MOVE     'I'                      TO ls_sscr_ass-sg_main.
  MOVE     'OPT_LIST'               TO ls_sscr_ass-op_main.
  APPEND                               ls_sscr_ass
                                    TO lt_sscr_restrict-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      RESTRICTION = lt_sscr_restrict.

ENDFORM.                    " f_initialization
*eject
*&---------------------------------------------------------------------*
*&      Form  f_sel_scrn_output
*&---------------------------------------------------------------------*
*       Selection screen output
*----------------------------------------------------------------------*
FORM f_sel_scrn_output.

  LOOP AT SCREEN.

* Set the screen fields to display only
    IF     ( screen-group1 EQ gc_modif_id_dsp ).
      screen-input  = 0.
      MODIFY   SCREEN.
    ENDIF.

    IF     ( rb_fsapl      IS NOT INITIAL ).

* Enable the application server file parameters
      IF   ( screen-group1 EQ gc_modif_id_fas ).
        screen-input  = 1.
        MODIFY SCREEN.
      ENDIF.

* Disable the presentation server file parameters
      IF   ( screen-group1 EQ gc_modif_id_fps ).
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

    ELSEIF ( rb_fsprs      IS NOT INITIAL ).

* Disable the application server file parameters
      IF   ( screen-group1 EQ gc_modif_id_fas ).
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

* Enable the presentation server file parameters
      IF   ( screen-group1 EQ gc_modif_id_fps ).
        screen-input  = 1.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_sel_scrn_output
*eject
*&---------------------------------------------------------------------*
*&      Form  f_f4_help_pres_file
*&---------------------------------------------------------------------*
*       F4 help - presentation server - file open dialog
*----------------------------------------------------------------------*
FORM f_f4_help_pres_file
  CHANGING cv_fname                    TYPE text255.

  DATA:    ls_file_table               TYPE file_table,
           lt_file_table               TYPE filetable.

  DATA:    lv_window_title             TYPE string,
           lv_initial_directory        TYPE string,
           lv_rc                       TYPE i.

  CLEAR                                     lv_window_title.
  MOVE     text-c01                      TO lv_window_title.
  CLEAR                                     lv_initial_directory.
  MOVE     text-c02                      TO lv_initial_directory.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_window_title
      initial_directory       = lv_initial_directory
      multiselection          = ' '
    CHANGING
      file_table              = lt_file_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF   ( ( sy-subrc EQ 0 ) AND ( lv_rc GT 0 ) ).

    CLEAR                                   ls_file_table.
    READ   TABLE lt_file_table         INTO ls_file_table
                                      INDEX 1.
    IF ( sy-subrc EQ 0 ).
      CLEAR                                 cv_fname.
      MOVE       ls_file_table-filename  TO cv_fname.
    ENDIF.

  ENDIF.

ENDFORM.                    " f_f4_help_pres_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_selection_screen
*&---------------------------------------------------------------------*
*       Validate the selection screen
*----------------------------------------------------------------------*
FORM f_validate_selection_screen.

  IF     ( p_erpid  IS INITIAL ).
    macro_msg                       'B' 'A' text-101.
  ENDIF.

  IF     ( rb_fsapl IS NOT INITIAL ).

    IF   ( p_fpth1a IS INITIAL ).
      macro_msg                     'B' 'A' text-111.
    ENDIF.
    IF   ( p_fnam1a IS INITIAL ).
      macro_msg                     'B' 'A' text-112.
    ENDIF.
    IF   ( p_fregxa IS INITIAL ).
      macro_msg                     'B' 'A' text-113.
    ENDIF.
    IF   ( p_farc1a IS INITIAL ).
      macro_msg                     'B' 'A' text-114.
    ENDIF.
    IF   ( p_fpth2a IS INITIAL ).
      macro_msg                     'B' 'A' text-121.
    ENDIF.
    IF   ( p_fnam2a IS INITIAL ).
      macro_msg                     'B' 'A' text-122.
    ENDIF.
    IF   ( p_farc2a IS INITIAL ).
      macro_msg                     'B' 'A' text-123.
    ENDIF.
    IF   ( p_fpth3a IS INITIAL ).
      macro_msg                     'B' 'A' text-131.
    ENDIF.
    IF   ( p_fnam3a IS INITIAL ).
      macro_msg                     'B' 'A' text-132.
    ENDIF.

  ELSEIF ( rb_fsprs IS NOT INITIAL ).

    IF   ( p_fnam1p IS INITIAL ).
      macro_msg                     'B' 'A' text-112.
    ENDIF.
    IF   ( p_fnam2p IS INITIAL ).
      macro_msg                     'B' 'A' text-122.
    ENDIF.
    IF   ( p_fnam3p IS INITIAL ).
      macro_msg                     'B' 'A' text-132.
    ENDIF.

  ENDIF.

ENDFORM.                    " f_validate_selection_screen
*eject
*&---------------------------------------------------------------------*
*&      Form  f_build_file_list
*&---------------------------------------------------------------------*
*       Build the list of files to be processed
*----------------------------------------------------------------------*
FORM f_build_file_list.

  DATA:    ls_file_list                TYPE gty_file_list,
           ls_dir_list                 TYPE gty_dir_list,
           lt_dir_list                 TYPE gtt_dir_list.

  DATA:    lv_dir_name                 TYPE epsdirnam,
           lv_file_mask                TYPE epsfilnam,
           lv_rc                       TYPE numc5,
           lv_text                     TYPE string,
           lv_pattern                  TYPE string,
           lv_token                    TYPE string,
           lv_offset                   TYPE i,
           lv_length                   TYPE i,
           lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix.

  FIELD-SYMBOLS: <fs_file_list>        TYPE gty_file_list.

  CLEAR    gt_file_list[].

  IF     ( gv_fl_errr_prcs IS NOT INITIAL ).
    RETURN.
  ENDIF.

  IF     ( rb_fsprs        IS NOT INITIAL ).

    CLEAR                                   ls_file_list.
    MOVE     abap_true                   TO ls_file_list-fsprs.
    MOVE     p_fnam1p                    TO ls_file_list-fn_in_main.
    APPEND   ls_file_list                TO gt_file_list.

*eject
  ELSEIF ( rb_fsapl IS NOT INITIAL ).

    CLEAR                                   lv_dir_name.
    MOVE     p_fpth1a                    TO lv_dir_name.

    CLEAR                                   lv_file_mask.
    MOVE     p_fnam1a                    TO lv_file_mask.

    CLEAR    lt_dir_list[].

    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name               = lv_dir_name
        file_mask              = lv_file_mask
      TABLES
        dir_list               = lt_dir_list
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

    IF   ( ( lv_rc NE 0 ) AND ( lv_rc NE 7 ) ).
      CLEAR    lt_dir_list[].
      CLEAR                                 lv_text.
      CONCATENATE                           text-231 text-232
                                            lv_rc lv_dir_name
                                       INTO lv_text
                               SEPARATED BY space.
      macro_msg                     'B' 'A' lv_text.
      RETURN.
    ENDIF.

    DELETE   lt_dir_list              WHERE name IS INITIAL.

    IF     ( lt_dir_list[]               IS INITIAL ).
      CLEAR                                 lv_text.
      MOVE     text-233                  TO lv_text.
      macro_msg                     'B' 'I' lv_text.
      RETURN.
    ENDIF.

*eject
* Evaluate the files in the inbound application directory
    IF     ( p_fregxa                    IS NOT INITIAL ).

      CLEAR                                 ls_dir_list.
      LOOP AT    lt_dir_list           INTO ls_dir_list.

* Search the string using a pattern; return offset, length, and token
        CLEAR                               lv_text.
        MOVE     ls_dir_list-name        TO lv_text.

        CLEAR                               lv_pattern.
        MOVE     p_fregxa                TO lv_pattern.

        CLEAR    lv_offset.
        CLEAR    lv_length.
        CLEAR    lv_token.
        CLEAR    lv_subrc.

        CALL FUNCTION 'ZFI_PARSE_STRING_USING_PATTERN'
          EXPORTING
            iv_text       = lv_text
            iv_pattern    = lv_pattern
          IMPORTING
            cv_offset     = lv_offset
            cv_length     = lv_length
            cv_token      = lv_token
            cv_subrc      = lv_subrc
          EXCEPTIONS
            no_text       = 1
            no_pattern    = 2
            search_error  = 3
            pattern_error = 4
            parse_error   = 5
            OTHERS        = 6.

        lv_rc = sy-subrc.

        IF     ( lv_rc NE 0 ).
          CLEAR    gt_file_list[].
          CLEAR                             lv_text.
          CONCATENATE                       text-235 text-232
                                            lv_rc lv_pattern
                                       INTO lv_text
                               SEPARATED BY space.
          macro_msg                 'B' 'A' lv_text.
          RETURN.
        ENDIF.

*eject
* If the pattern is found, then save the filename in the file list
        IF     ( ( lv_subrc EQ 0 ) AND ( lv_token IS NOT INITIAL ) ).
          CLEAR                             ls_file_list.
          MOVE     abap_true             TO ls_file_list-fsapl.
          MOVE     p_fpth1a              TO ls_file_list-fp_in_main.
          MOVE     ls_dir_list-name      TO ls_file_list-fn_in_main.
          APPEND   ls_file_list          TO gt_file_list.
        ENDIF.

        CLEAR  ls_dir_list.
      ENDLOOP.

    ELSE.

      CLEAR                                 ls_dir_list.
      LOOP AT      lt_dir_list         INTO ls_dir_list.
        CLEAR                               ls_file_list.
        MOVE       abap_true             TO ls_file_list-fsapl.
        MOVE       p_fpth1a              TO ls_file_list-fp_in_main.
        MOVE       ls_dir_list-name      TO ls_file_list-fn_in_main.
        APPEND     ls_file_list          TO gt_file_list.
        CLEAR      ls_dir_list.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF     ( gt_file_list[]                IS INITIAL ).
    CLEAR                                   lv_text.
    MOVE     text-233                    TO lv_text.
    macro_msg                       'B' 'I' lv_text.
    RETURN.
  ENDIF.

  SORT     gt_file_list        ASCENDING BY fp_in_main fn_in_main.

  LOOP AT  gt_file_list           ASSIGNING <fs_file_list>.
    lv_tabix = sy-tabix.

    MOVE     lv_tabix                    TO <fs_file_list>-file_nbr.

  ENDLOOP.

ENDFORM.                    " f_build_file_list
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_main
*&---------------------------------------------------------------------*
*       Main process
*----------------------------------------------------------------------*
FORM f_process_main.

  DATA:    lv_tabix                    TYPE sytabix.

  FIELD-SYMBOLS: <fs_file_list>        TYPE gty_file_list.

  IF     ( gv_fl_errr_prcs IS NOT INITIAL ).
    RETURN.
  ENDIF.

* Loop at the file list
  LOOP AT  gt_file_list           ASSIGNING <fs_file_list>.
    lv_tabix = sy-tabix.

    IF     ( lv_tabix GT 1 ).
      WAIT UP TO 2 SECONDS.
    ENDIF.

* Initial the data elements
    PERFORM  f_initial_data_elements.

* Generate the filepaths and filenames
    PERFORM  f_generate_filenames  CHANGING <fs_file_list>.

* Open the log file
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_open_file            USING <fs_file_list>
                                            'L' "log
                                   CHANGING gv_filename_log
                                            gv_filename_lgx.
    ENDIF.

* Open the inbound file
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_open_file            USING <fs_file_list>
                                            'I' "inbound
                                   CHANGING gv_filename_in
                                            gv_filename_inx.
    ENDIF.

* Read the data
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_read_data         CHANGING <fs_file_list>.
    ENDIF.

* Close the inbound file
    PERFORM  f_close_file             USING 'I' "inbound
                                   CHANGING gv_filename_in
                                            gv_filename_inx.

*eject
* Open the outbound file
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_open_file            USING <fs_file_list>
                                            'O' "outbound
                                   CHANGING gv_filename_out
                                            gv_filename_otx.
    ENDIF.

* Process the data
    IF       ( gv_fl_errr_prcs           IS INITIAL ).
      PERFORM  f_process_data.
    ENDIF.

* Close the outbound file
    PERFORM  f_close_file             USING 'O' "outbound
                                   CHANGING gv_filename_out
                                            gv_filename_otx.

* Dispatch the files
    IF       ( rb_fsapl                  IS NOT INITIAL ).
      PERFORM  f_dispatch_files       USING <fs_file_list>.
    ENDIF.

* Close the log file
    PERFORM  f_close_file             USING 'L' "log
                                   CHANGING gv_filename_log
                                            gv_filename_lgx.

  ENDLOOP.

* Download files to the presentation server
  IF       ( rb_fsprs                    IS NOT INITIAL ).
    PERFORM  f_download_files         USING <fs_file_list>.
  ENDIF.

ENDFORM.                    " f_process_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  CLEAR    gv_fl_errr_prcs.
  CLEAR    gv_cnt_doc_trns.
  CLEAR    gv_cnt_doc_errr.
  CLEAR    gv_cnt_doc_wrng.
  CLEAR    gv_cnt_doc_pass.
  CLEAR    gv_filename_in.
  CLEAR    gv_filename_inx.
  CLEAR    gv_filename_out.
  CLEAR    gv_filename_otx.
  CLEAR    gv_filename_log.
  CLEAR    gv_filename_lgx.

  CLEAR    gt_out_data[].
  CLEAR    gt_log_data[].
  CLEAR    gt_doc_data[].
  CLEAR    gt_doc_hdr[].
  CLEAR    gt_doc_item[].
  CLEAR    gt_doc_actg[].

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_generate_filenames
*&---------------------------------------------------------------------*
*       Generate the filepaths and filenames
*----------------------------------------------------------------------*
FORM f_generate_filenames
  CHANGING cs_file_list                TYPE gty_file_list.

  DATA:    lv_filepath                 TYPE text128,
           lv_filename                 TYPE text128.

* Inbound archive
  CLEAR                                     lv_filename.
  MOVE       cs_file_list-fn_in_main     TO lv_filename.
  TRANSLATE  lv_filename              USING '._'.
  CONCATENATE                               lv_filename '_'
                                            sy-datum    '_'
                                            sy-uzeit    text-c21
                                       INTO lv_filename.

  MOVE     p_farc1a                      TO cs_file_list-fp_in_arch.
  MOVE     lv_filename                   TO cs_file_list-fn_in_arch.

* Outbound temporary
  CLEAR                                     lv_filepath.
  MOVE     p_fpth1a                      TO lv_filepath.
  CONCATENATE                               lv_filepath text-c05 gc_slash
                                       INTO lv_filepath.

  CLEAR                                     lv_filename.
  MOVE     p_fnam2a                      TO lv_filename.
  REPLACE  'ERPID'                       IN lv_filename WITH p_erpid.
  REPLACE  'YYYYMMDD'                    IN lv_filename WITH sy-datum.
  REPLACE  'HHMMSS'                      IN lv_filename WITH sy-uzeit.

  MOVE     lv_filepath                   TO cs_file_list-fp_out_temp.
  MOVE     lv_filename                   TO cs_file_list-fn_out_temp.

* Outbound archive
  CLEAR                                     lv_filename.
  MOVE       cs_file_list-fn_out_temp    TO lv_filename.

  IF     ( cb_test IS INITIAL ).
    TRANSLATE  lv_filename            USING '._'.
    CONCATENATE                             lv_filename text-c21
                                       INTO lv_filename.
  ELSE.
    TRANSLATE  lv_filename            USING '._'.
    CONCATENATE                             lv_filename text-c22
                                       INTO lv_filename.
  ENDIF.

  MOVE     p_farc2a                      TO cs_file_list-fp_out_arch.
  MOVE     lv_filename                   TO cs_file_list-fn_out_arch.

*eject
* Outbound main
  IF     ( rb_fsapl                      IS NOT INITIAL ).
    MOVE   p_fpth2a                      TO cs_file_list-fp_out_main.
    MOVE   cs_file_list-fn_out_temp      TO cs_file_list-fn_out_main.
  ELSE.
    MOVE   p_fnam2p                      TO cs_file_list-fn_out_main.
  ENDIF.

* Log main
  CLEAR                                     lv_filename.
  MOVE     p_fnam3a                      TO lv_filename.
  REPLACE  'ERPID'                       IN lv_filename WITH p_erpid.
  REPLACE  'YYYYMMDD'                    IN lv_filename WITH sy-datum.
  REPLACE  'HHMMSS'                      IN lv_filename WITH sy-uzeit.

  IF     ( cb_test IS INITIAL ).
  ELSE.
    TRANSLATE  lv_filename            USING '._'.
    CONCATENATE                             lv_filename text-c22
                                       INTO lv_filename.
  ENDIF.

  IF     ( rb_fsapl                      IS NOT INITIAL ).
    MOVE   p_fpth3a                      TO cs_file_list-fp_log_main.
    MOVE   lv_filename                   TO cs_file_list-fn_log_main.
  ELSE.
    MOVE   p_fnam3p                      TO cs_file_list-fn_log_main.
  ENDIF.

ENDFORM.                    " f_generate_filenames
*eject
*&---------------------------------------------------------------------*
*&      Form  f_read_data
*&---------------------------------------------------------------------*
*       Read the data
*----------------------------------------------------------------------*
FORM f_read_data
  CHANGING cs_file_list                TYPE gty_file_list.

  DATA:    ls_data_tab                 TYPE string,
           lt_data_tab                 TYPE STANDARD TABLE OF string,
           ls_data_fld                 TYPE string,
           lt_data_fld                 TYPE STANDARD TABLE OF string.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_lines                    TYPE syindex,
           lv_filename                 TYPE string,
           lv_string                   TYPE string,
           lv_rc                       TYPE numc5,
           lv_text                     TYPE string.

  DATA:    lr_codepage_init            TYPE REF TO
                                       cx_sy_codepage_converter_init,
           lr_codepage_conv            TYPE REF TO
                                       cx_sy_conversion_codepage,
           lr_file_io                  TYPE REF TO cx_sy_file_io.

  DATA:    lcl_doc_data                TYPE REF TO data.

  FIELD-SYMBOLS: <lfs_doc_data>        TYPE gty_doc_data,
                 <lfs_data_fld>        TYPE any.

  IF     ( gv_fl_errr_prcs IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_data_tab[].

*eject
* Read from the presentation server
  IF     ( rb_fsprs                      IS NOT INITIAL ).

    CLEAR                                   lv_filename.
    MOVE     cs_file_list-fn_in_main     TO lv_filename.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        FILENAME                      = lv_filename
*       FILETYPE                      = 'ASC'
*     IMPORTING
*       FILELENGTH                    =
*       HEADER                        =
      TABLES
        DATA_TAB                      = lt_data_tab
*     CHANGING
*       ISSCANPERFORMED               = ' '
      EXCEPTIONS
        FILE_OPEN_ERROR               = 1
        FILE_READ_ERROR               = 2
        NO_BATCH                      = 3
        GUI_REFUSE_FILETRANSFER       = 4
        INVALID_TYPE                  = 5
        NO_AUTHORITY                  = 6
        UNKNOWN_ERROR                 = 7
        BAD_DATA_FORMAT               = 8
        HEADER_NOT_ALLOWED            = 9
        SEPARATOR_NOT_ALLOWED         = 10
        HEADER_TOO_LONG               = 11
        UNKNOWN_DP_ERROR              = 12
        ACCESS_DENIED                 = 13
        DP_OUT_OF_MEMORY              = 14
        DISK_FULL                     = 15
        DP_TIMEOUT                    = 16
        OTHERS                        = 17.

    lv_rc = sy-subrc.

    IF     ( lv_rc NE 0 ).
      CLEAR                                 lv_text.
      CONCATENATE                           text-246 text-232
                                            lv_rc lv_filename
                                       INTO lv_text
                               SEPARATED BY space.
      macro_msg                     'B' 'A' lv_text.
      RETURN.
    ENDIF.

*eject
* Read from the application server
  ELSEIF ( rb_fsapl                      IS NOT INITIAL ).

* Read the file
    CLEAR    lv_subrc.
    CLEAR    lv_string.

    DO.

      CLEAR    ls_data_tab.

      TRY.
          READ   DATASET gv_filename_in     INTO ls_data_tab.
        CATCH cx_sy_codepage_converter_init INTO lr_codepage_init.
          lv_subrc  = 1.
          lv_string = lr_codepage_init->get_text( ).
        CATCH  cx_sy_conversion_codepage    INTO lr_codepage_conv.
          lv_subrc  = 2.
          lv_string = lr_codepage_conv->get_text( ).
        CATCH  cx_sy_file_io                INTO lr_file_io.
          lv_subrc  = 3.
          lv_string = lr_file_io->get_text( ).
      ENDTRY.

      IF     ( sy-subrc NE 0 ).
        EXIT.
      ELSEIF ( lv_subrc NE 0 ).
        EXIT.
      ENDIF.

      APPEND   ls_data_tab               TO lt_data_tab.

    ENDDO.

    IF     ( lv_subrc NE 0 ).

      lv_rc = lv_subrc.

      CLEAR                                 lv_text.
      CONCATENATE                           text-245 text-232
                                            lv_rc lv_filename
                                       INTO lv_text
                               SEPARATED BY space.
      macro_msg                     'B' 'A' lv_text.
      macro_msg                     'B' 'A' lv_string.
      RETURN.
    ENDIF.

  ENDIF.

*eject
* Parse the data
  CREATE   DATA lcl_doc_data           TYPE gty_doc_data.

  CLEAR                                     ls_data_tab.
  LOOP AT  lt_data_tab                 INTO ls_data_tab.

    ASSIGN   lcl_doc_data->*             TO <lfs_doc_data>.

    CLEAR                                   lt_data_fld[].
    SPLIT    ls_data_tab                 AT gc_delim
                                 INTO TABLE lt_data_fld[].

    CLEAR                                   ls_data_fld.
    LOOP AT  lt_data_fld               INTO ls_data_fld.
      lv_tabix = sy-tabix.
      ASSIGN COMPONENT lv_tabix OF STRUCTURE <lfs_doc_data>
                                          TO <lfs_data_fld>.
      CLEAR                                  <lfs_data_fld>.
      MOVE     ls_data_fld                TO <lfs_data_fld>.
      CLEAR    ls_data_fld.
    ENDLOOP.

    APPEND   <lfs_doc_data>              TO gt_doc_data.

    CLEAR  ls_data_tab.
  ENDLOOP.

  CLEAR                                     lv_lines.
  DESCRIBE TABLE lt_data_tab          LINES lv_lines.

  CLEAR                                     cs_file_list-cn_recs.
  MOVE     lv_lines                      TO cs_file_list-cn_recs.

* Inbound File Record Count
  CLEAR                                   lv_text.
  CONCATENATE       text-l21              cs_file_list-cn_recs
                                     INTO lv_text.
  macro_log                             0 lv_text.

ENDFORM.                    " f_read_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_data
*&---------------------------------------------------------------------*
*       Process the data
*----------------------------------------------------------------------*
FORM f_process_data.

  DATA:    lv_seqn3                    TYPE numc3.

  IF     ( gv_fl_errr_prcs IS NOT INITIAL ).
    RETURN.
  ENDIF.

  IF     ( gt_doc_data[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     gt_doc_data ASCENDING BY hdrseqn itemseqn actgseqn.

  CLEAR                                     gs_doc_data.
  LOOP AT  gt_doc_data                 INTO gs_doc_data.

    AT NEW hdrseqn.
      CLEAR    gt_doc_hdr[].
      CLEAR    gt_doc_item[].
      CLEAR    gt_doc_actg[].
      CLEAR    gs_doc_hdr.
      CLEAR    gs_doc_item.
      CLEAR    gs_doc_actg.
    ENDAT.

    CLEAR                                   lv_seqn3.
    IF       ( gs_doc_data-itemseqn      IS INITIAL ).
      MOVE     '000'                     TO lv_seqn3.
    ELSE.
      MOVE     gs_doc_data-itemseqn      TO lv_seqn3.
    ENDIF.

    CLEAR                                   gs_doc_data-itemseqn.
    MOVE       lv_seqn3                  TO gs_doc_data-itemseqn.

    CLEAR                                   lv_seqn3.
    IF       ( gs_doc_data-actgseqn      IS INITIAL ).
      MOVE     '000'                     TO lv_seqn3.
    ELSE.
      MOVE     gs_doc_data-actgseqn      TO lv_seqn3.
    ENDIF.

    CLEAR                                   gs_doc_data-actgseqn.
    MOVE       lv_seqn3                  TO gs_doc_data-actgseqn.

*eject
* IAP document header fields
    IF       ( gs_doc_data-hdrseqn       NE gs_doc_hdr-hdrseqn  ).
      CLEAR                                 gs_doc_hdr.
      MOVE     gs_doc_data-hdrseqn       TO gs_doc_hdr-hdrseqn.
      MOVE     gs_doc_data-systid        TO gs_doc_hdr-systid.
      MOVE     gs_doc_data-doccatg       TO gs_doc_hdr-doccatg.
      MOVE     gs_doc_data-ritm          TO gs_doc_hdr-ritm.
      MOVE     gs_doc_data-created       TO gs_doc_hdr-created.
      MOVE     gs_doc_data-compcode      TO gs_doc_hdr-compcode.
      MOVE     gs_doc_data-supplier      TO gs_doc_hdr-supplier.
      MOVE     gs_doc_data-invoice       TO gs_doc_hdr-invoice.
      MOVE     gs_doc_data-doctype       TO gs_doc_hdr-doctype.
      MOVE     gs_doc_data-invcdate      TO gs_doc_hdr-invcdate.
      MOVE     gs_doc_data-postdate      TO gs_doc_hdr-postdate.
      MOVE     gs_doc_data-period        TO gs_doc_hdr-period.
      MOVE     gs_doc_data-hdrtext       TO gs_doc_hdr-hdrtext.
      MOVE     gs_doc_data-sglind        TO gs_doc_hdr-sglind.
      MOVE     gs_doc_data-invcamnt      TO gs_doc_hdr-invcamnt.
      MOVE     gs_doc_data-currcode      TO gs_doc_hdr-currcode.
      MOVE     gs_doc_data-calctax       TO gs_doc_hdr-calctax.
      MOVE     gs_doc_data-taxamnt       TO gs_doc_hdr-taxamnt.
      MOVE     gs_doc_data-taxcode       TO gs_doc_hdr-taxcode.
      MOVE     gs_doc_data-blinedate     TO gs_doc_hdr-blinedate.
      MOVE     gs_doc_data-paymterms     TO gs_doc_hdr-paymterms.
      MOVE     gs_doc_data-paymmthd      TO gs_doc_hdr-paymmthd.
      MOVE     gs_doc_data-paymmthdsplm  TO gs_doc_hdr-paymmthdsplm.
      MOVE     gs_doc_data-paymblock     TO gs_doc_hdr-paymblock.
      MOVE     gs_doc_data-payee         TO gs_doc_hdr-payee.
      MOVE     gs_doc_data-partnrbnk     TO gs_doc_hdr-partnrbnk.
      MOVE     gs_doc_data-housebnk      TO gs_doc_hdr-housebnk.
      MOVE     gs_doc_data-wthtaxtype    TO gs_doc_hdr-wthtaxtype.
      MOVE     gs_doc_data-wthtaxcode    TO gs_doc_hdr-wthtaxcode.
      MOVE     gs_doc_data-wthbasamnt    TO gs_doc_hdr-wthbasamnt.
      MOVE     gs_doc_data-wthtaxamnt    TO gs_doc_hdr-wthtaxamnt.
      MOVE     gs_doc_data-wthtaxtype2   TO gs_doc_hdr-wthtaxtype2.
      MOVE     gs_doc_data-wthtaxcode2   TO gs_doc_hdr-wthtaxcode2.
      MOVE     gs_doc_data-wthbasamnt2   TO gs_doc_hdr-wthbasamnt2.
      MOVE     gs_doc_data-wthtaxamnt2   TO gs_doc_hdr-wthtaxamnt2.
      MOVE     gs_doc_data-routecode     TO gs_doc_hdr-routecode.
      MOVE     gs_doc_data-unpldelvcosts TO gs_doc_hdr-unpldelvcosts.
      MOVE     gs_doc_data-cashdiscamnt  TO gs_doc_hdr-cashdiscamnt.
      MOVE     gs_doc_data-itemdesc      TO gs_doc_hdr-itemdesc.
      APPEND   gs_doc_hdr                TO gt_doc_hdr.
    ENDIF.

*eject
* IAP document item fields
    IF         ( gs_doc_data-itemseqn    GT 0       ).
      IF     ( ( gs_doc_data-hdrseqn     NE gs_doc_item-hdrseqn  ) OR
               ( gs_doc_data-itemseqn    NE gs_doc_item-itemseqn ) ).
        CLEAR                               gs_doc_item.
        MOVE     gs_doc_data-hdrseqn     TO gs_doc_item-hdrseqn.
        MOVE     gs_doc_data-itemseqn    TO gs_doc_item-itemseqn.
        MOVE     gs_doc_data-pohdr       TO gs_doc_item-pohdr.
        MOVE     gs_doc_data-poitem      TO gs_doc_item-poitem.
        MOVE     gs_doc_data-refdoc      TO gs_doc_item-refdoc.
        MOVE     gs_doc_data-refdocyear  TO gs_doc_item-refdocyear.
        MOVE     gs_doc_data-refdocitem  TO gs_doc_item-refdocitem.
        MOVE     gs_doc_data-dbtcrd      TO gs_doc_item-dbtcrd.
        MOVE     gs_doc_data-taxcode_i   TO gs_doc_item-taxcode.
        MOVE     gs_doc_data-amount      TO gs_doc_item-amount.
        MOVE     gs_doc_data-quantity    TO gs_doc_item-quantity.
        MOVE     gs_doc_data-pouom       TO gs_doc_item-pouom.
        MOVE     gs_doc_data-sheetno     TO gs_doc_item-sheetno.
        MOVE     gs_doc_data-grirclear   TO gs_doc_item-grirclear.
        MOVE     gs_doc_data-itemdesc_i  TO gs_doc_item-itemdesc.
        APPEND   gs_doc_item             TO gt_doc_item.
      ENDIF.
    ENDIF.

*eject
* IAP document accounting fields
    IF         ( gs_doc_data-actgseqn    GT 0       ).
      IF     ( ( gs_doc_data-hdrseqn     NE gs_doc_actg-hdrseqn  ) OR
               ( gs_doc_data-itemseqn    NE gs_doc_actg-itemseqn ) OR
               ( gs_doc_data-actgseqn    NE gs_doc_actg-actgseqn ) ).
        CLEAR                               gs_doc_actg.
        MOVE     gs_doc_data-hdrseqn     TO gs_doc_actg-hdrseqn.
        MOVE     gs_doc_data-itemseqn    TO gs_doc_actg-itemseqn.
        MOVE     gs_doc_data-actgseqn    TO gs_doc_actg-actgseqn.
        MOVE     gs_doc_data-acctasgn    TO gs_doc_actg-acctasgn.
        MOVE     gs_doc_data-dbtcrd_a    TO gs_doc_actg-dbtcrd.
        MOVE     gs_doc_data-taxcode_a   TO gs_doc_actg-taxcode.
        MOVE     gs_doc_data-amount_a    TO gs_doc_actg-amount.
        MOVE     gs_doc_data-quantity_a  TO gs_doc_actg-quantity.
        MOVE     gs_doc_data-pouom_a     TO gs_doc_actg-pouom.
        MOVE     gs_doc_data-compcode_a  TO gs_doc_actg-compcode.
        MOVE     gs_doc_data-glacct      TO gs_doc_actg-glacct.
        MOVE     gs_doc_data-costcntr    TO gs_doc_actg-costcntr.
        MOVE     gs_doc_data-intlordr    TO gs_doc_actg-intlordr.
        MOVE     gs_doc_data-wbselem     TO gs_doc_actg-wbselem.
        MOVE     gs_doc_data-network     TO gs_doc_actg-network.
        MOVE     gs_doc_data-activity    TO gs_doc_actg-activity.
        MOVE     gs_doc_data-prftsegmno  TO gs_doc_actg-prftsegmno.
        MOVE     gs_doc_data-lctncode    TO gs_doc_actg-lctncode.
        MOVE     gs_doc_data-cfattr      TO gs_doc_actg-cfattr.
        MOVE     gs_doc_data-itemdesc_a  TO gs_doc_actg-itemdesc.
        APPEND   gs_doc_actg             TO gt_doc_actg.
      ENDIF.
    ENDIF.

    AT END OF hdrseqn.

      PERFORM  f_process_document.

    ENDAT.

    CLEAR  gs_doc_data.
  ENDLOOP.

ENDFORM.                    " f_process_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_document
*&---------------------------------------------------------------------*
*       Process the accounting document
*----------------------------------------------------------------------*
FORM f_process_document.

  DATA:    ls_doc_hdr                  TYPE gty_doc_hdr,
           ls_iap_xref_ad              TYPE gty_iap_xref_ad.

  DATA:    lt_return                   TYPE STANDARD TABLE OF bapiret2,
           ls_return                   TYPE bapiret2.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_msgty                    TYPE symsgty,
           lv_check                    TYPE xflag,
           lv_cnt_hdr                  TYPE numc3,
           lv_cnt_item                 TYPE numc3,
           lv_cnt_actg                 TYPE numc3,
           lv_fl_type_e                TYPE xflag,
           lv_fl_type_w                TYPE xflag,
           lv_document                 TYPE char20,
           lv_text                     TYPE string,
           lv_text_p                   TYPE string,
           lv_text_c120                TYPE text120.

  FIELD-SYMBOLS: <fs_return>           TYPE bapiret2.

  CLEAR                                     ls_doc_hdr.
  READ     TABLE gt_doc_hdr            INTO ls_doc_hdr INDEX 1.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  CLEAR    lt_return[].

  CLEAR    lv_msgty.
  CLEAR    lv_check.
  CLEAR    lv_cnt_hdr.
  CLEAR    lv_cnt_item.
  CLEAR    lv_cnt_actg.
  CLEAR    lv_fl_type_e.
  CLEAR    lv_fl_type_w.

*eject
* IAP - AP Document
  DESCRIBE TABLE gt_doc_hdr           LINES lv_cnt_hdr.
  DESCRIBE TABLE gt_doc_item          LINES lv_cnt_item.
  DESCRIBE TABLE gt_doc_actg          LINES lv_cnt_actg.

  CLEAR                                     lv_text_c120.
  MOVE                text-l31           TO lv_text_c120.
  MOVE                ls_doc_hdr-ritm    TO lv_text_c120+25.
  MOVE                text-l32           TO lv_text_c120+50.
  MOVE                lv_cnt_hdr         TO lv_text_c120+62.
  MOVE                text-l33           TO lv_text_c120+70.
  MOVE                lv_cnt_item        TO lv_text_c120+80.
  MOVE                text-l34           TO lv_text_c120+90.
  MOVE                lv_cnt_actg        TO lv_text_c120+106.
  macro_log                               2 lv_text_c120.

* Check if the document has been previously posted
  CLEAR    ls_iap_xref_ad.
  SELECT   SINGLE *
    INTO   ls_iap_xref_ad
    FROM   zapt_iap_xref_ad
   WHERE   zz_iap_ritm_doc = ls_doc_hdr-ritm.
  IF     ( sy-subrc EQ 0 ).

    CLEAR                                   lv_document.
    IF     ( ls_iap_xref_ad-awtyp        CS 'BKPF' ).
      MOVE   ls_iap_xref_ad-bukrs        TO lv_document+00(04).
      MOVE   '-'                         TO lv_document+04(01).
      MOVE   ls_iap_xref_ad-belnr        TO lv_document+05(10).
      MOVE   '-'                         TO lv_document+15(01).
      MOVE   ls_iap_xref_ad-gjahr        TO lv_document+16(04).
    ELSEIF ( ls_iap_xref_ad-awtyp        EQ 'RMRP' ).
      MOVE   ls_iap_xref_ad-belnr        TO lv_document+00(10).
      MOVE   '-'                         TO lv_document+10(01).
      MOVE   ls_iap_xref_ad-gjahr        TO lv_document+11(04).
    ENDIF.

    CLEAR                                   lv_text.
    CONCATENATE       text-501              lv_document
                                       INTO lv_text.

    CLEAR                                   ls_return.
    MOVE     'W'                         TO ls_return-type.
    MOVE     'ZFI01-000'                 TO ls_return-id.
    MOVE     '501'                       TO ls_return-number.
    MOVE     lv_text                     TO ls_return-message.
    APPEND   ls_return                   TO lt_return.

*eject
* Post the accounting document
  ELSE.

    IF     ( cb_test IS NOT INITIAL ).
      lv_check = abap_true.
    ENDIF.

    CALL FUNCTION 'ZAP_IAP_INVOICE_POST'
      EXPORTING
        CHECK    = lv_check
      TABLES
        IAP_HDR  = gt_doc_hdr
        IAP_ITEM = gt_doc_item
        IAP_ACTG = gt_doc_actg
        RETURN   = lt_return.

  ENDIF.

  LOOP AT  lt_return              ASSIGNING <fs_return>.
    IF   ( <fs_return>-type              CA 'AaEe' ).
      lv_fl_type_e = abap_true.
    ENDIF.
    IF   ( <fs_return>-type              CA 'Ww'   ).
      lv_fl_type_w = abap_true.
    ENDIF.
    CLEAR                                   lv_text_c120.
    MOVE   <fs_return>-type              TO lv_text_c120+02(01).
    MOVE   <fs_return>-id                TO lv_text_c120+05(10).
    MOVE   <fs_return>-number            TO lv_text_c120+17(03).
    MOVE   <fs_return>-message           TO lv_text_c120+22(98).
    macro_log                             0 lv_text_c120.
  ENDLOOP.

  IF     ( lv_fl_type_e                  IS NOT INITIAL ).
    MOVE     'E'                         TO lv_msgty.
    DELETE   lt_return                WHERE type CA 'SsIi'.
  ELSEIF ( lv_fl_type_w                  IS NOT INITIAL ).
    MOVE     'W'                         TO lv_msgty.
  ELSE.
    MOVE     'S'                         TO lv_msgty.
  ENDIF.

*eject
  CLEAR    lv_text.
  CLEAR    lv_text_p.

  LOOP AT  lt_return              ASSIGNING <fs_return>.
    lv_tabix = sy-tabix.
    CLEAR                                   lv_text.
    CONCATENATE                             <fs_return>-number '('
                                            <fs_return>-id     ')-'
                                            <fs_return>-message
                                       INTO lv_text.
    IF     ( lv_tabix EQ 1 ).
      CONCATENATE                           ls_doc_hdr-ritm gc_delim
                                            lv_msgty        gc_delim
                                            lv_text
                                       INTO lv_text_p.
    ELSE.
      CONCATENATE                           lv_text_p '*|*'
                                            lv_text
                                       INTO lv_text_p
                               SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  IF       ( rb_fsapl IS NOT INITIAL ).

    TRANSFER lv_text_p                   TO gv_filename_out.

  ELSE.

    CLEAR                                   gs_out_data.
    MOVE     lv_text_p                   TO gs_out_data.
    APPEND   gs_out_data                 TO gt_out_data.

  ENDIF.

  ADD      1                             TO gv_cnt_doc_trns.
  IF     ( lv_fl_type_e                  IS NOT INITIAL ).
    ADD    1                             TO gv_cnt_doc_errr.
  ELSEIF ( lv_fl_type_w                  IS NOT INITIAL ).
    ADD    1                             TO gv_cnt_doc_wrng.
  ELSE.
    ADD    1                             TO gv_cnt_doc_pass.
  ENDIF.

ENDFORM.                    " f_process_document
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_file
*&---------------------------------------------------------------------*
*       Open a file
*----------------------------------------------------------------------*
FORM f_open_file
  USING    is_file_list                TYPE gty_file_list
           iv_file_funct               TYPE char1
  CHANGING cv_filename                 TYPE text255
           cv_filename_x               TYPE xflag.

  DATA:    lv_filename                 TYPE text255,
           lv_subrc                    TYPE sysubrc,
           lv_string                   TYPE string,
           lv_rc                       TYPE numc5,
           lv_text                     TYPE string,
           lv_date_time                TYPE char20.

  DATA:    lr_file_open                TYPE REF TO
                                       cx_sy_file_open,
           lr_file_authority           TYPE REF TO
                                       cx_sy_file_authority,
           lr_codepage_init            TYPE REF TO
                                       cx_sy_codepage_converter_init,
           lr_codepage_conv            TYPE REF TO
                                       cx_sy_conversion_codepage.

  CLEAR    cv_filename.
  CLEAR    cv_filename_x.

  CLEAR    lv_filename.
  CLEAR    lv_subrc.
  CLEAR    lv_string.
  CLEAR    lv_rc.
  CLEAR    lv_text.
  CLEAR    lv_date_time.

  IF     ( rb_fsapl IS NOT INITIAL ).

    CASE     iv_file_funct.
      WHEN     'I'.
        CONCATENATE is_file_list-fp_in_main is_file_list-fn_in_main
                                       INTO lv_filename.
        MOVE   text-241                  TO lv_text.
      WHEN     'O'.
        CONCATENATE is_file_list-fp_out_temp is_file_list-fn_out_temp
                                       INTO lv_filename.
        MOVE   text-242                  TO lv_text.
      WHEN     'L'.
        CONCATENATE is_file_list-fp_log_main is_file_list-fn_log_main
                                       INTO lv_filename.
        MOVE   text-243                  TO lv_text.
      WHEN     OTHERS.
    ENDCASE.

*eject
    CLEAR    lv_subrc.
    CLEAR    lv_string.

    TRY.

        IF     ( iv_file_funct                EQ 'I' ).
          OPEN   DATASET lv_filename         FOR INPUT
                                              IN TEXT MODE
                                        ENCODING DEFAULT.
        ELSE.
          OPEN   DATASET lv_filename         FOR OUTPUT
                                              IN TEXT MODE
                                        ENCODING DEFAULT.
        ENDIF.

      CATCH  cx_sy_file_open                INTO lr_file_open.
        lv_subrc  = 1.
        lv_string = lr_file_open->get_text( ).
      CATCH  cx_sy_file_authority           INTO lr_file_authority.
        lv_subrc  = 2.
        lv_string = lr_file_authority->get_text( ).
      CATCH  cx_sy_codepage_converter_init  INTO lr_codepage_init.
        lv_subrc  = 3.
        lv_string = lr_codepage_init->get_text( ).
      CATCH  cx_sy_conversion_codepage      INTO lr_codepage_conv.
        lv_subrc  = 4.
        lv_string = lr_codepage_conv->get_text( ).

    ENDTRY.

    IF   ( ( sy-subrc EQ 0 ) AND ( lv_subrc EQ 0 ) ).

      cv_filename   = lv_filename.
      cv_filename_x = abap_true.

    ELSE.

      cv_filename   = lv_filename.
      cv_filename_x = abap_false.

      lv_rc = lv_subrc.

      CONCATENATE                           lv_text text-232
                                            lv_rc   lv_filename
                                       INTO lv_text
                               SEPARATED BY space.
      macro_msg                     'B' 'A' lv_text.
      macro_msg                     'B' 'A' lv_string.
      macro_log                           1 lv_text.
      RETURN.
    ENDIF.

  ENDIF.

*eject
* Write the selection screen parameters to the log file

  IF     ( iv_file_funct EQ 'L' ).

* Run Date and Time
    WRITE             sy-datum           TO lv_date_time.
    WRITE             sy-uzeit           TO lv_date_time+11.
    CLEAR                                   lv_text.
    CONCATENATE       text-l01              lv_date_time
                                       INTO lv_text.
    macro_log                             2 lv_text.

* File Number
    CLEAR                                   lv_text.
    CONCATENATE       text-l02              is_file_list-file_nbr
                                       INTO lv_text.
    macro_log                             0 lv_text.

* ERP ID
    CLEAR                                   lv_text.
    CONCATENATE       text-l03 p_erpid INTO lv_text.
    macro_log                             0 lv_text.

* Test Run / Post Run
    CLEAR                                   lv_text.
    IF     ( cb_test                     IS NOT INITIAL ).
      CONCATENATE     text-l04 'Yes'   INTO lv_text.
    ELSE.
      CONCATENATE     text-l05 'Yes'   INTO lv_text.
    ENDIF.
    macro_log                             0 lv_text.

* Inbound File
    CLEAR                                   lv_text.
    CONCATENATE       text-l11              is_file_list-fp_in_main
                                            is_file_list-fn_in_main
                                       INTO lv_text.
    macro_log                             1 lv_text.

* Inbound Archive File
    CLEAR                                   lv_text.
    CONCATENATE       text-l12              is_file_list-fp_in_arch
                                            is_file_list-fn_in_arch
                                       INTO lv_text.
    IF     ( cb_test IS INITIAL ).
      macro_log                           0 lv_text.
    ENDIF.

*eject
* Outbound File
    CLEAR                                   lv_text.
    CONCATENATE       text-l13              is_file_list-fp_out_main
                                            is_file_list-fn_out_main
                                       INTO lv_text.
    IF     ( cb_test IS INITIAL ).
      macro_log                           0 lv_text.
    ENDIF.

* Outbound Archive File
    CLEAR                                   lv_text.
    CONCATENATE       text-l14              is_file_list-fp_out_arch
                                            is_file_list-fn_out_arch
                                       INTO lv_text.
    macro_log                             0 lv_text.

* Log File
    CLEAR                                   lv_text.
    CONCATENATE       text-l15              is_file_list-fp_log_main
                                            is_file_list-fn_log_main
                                       INTO lv_text.
    macro_log                             0 lv_text.

  ENDIF.

ENDFORM.                    " f_open_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_close_file
*&---------------------------------------------------------------------*
*       Close a file
*----------------------------------------------------------------------*
FORM f_close_file
  USING    iv_file_funct               TYPE char1
  CHANGING cv_filename                 TYPE text255
           cv_filename_x               TYPE xflag.

  DATA:    lv_text                     TYPE string.

* Write the transaction counts to the log file

  IF     ( iv_file_funct EQ 'O' ).

    CLEAR                                   lv_text.
    CONCATENATE       text-l51              gv_cnt_doc_trns
                                       INTO lv_text.
    macro_log                             2 lv_text.
    CLEAR                                   lv_text.

    CLEAR                                   lv_text.
    CONCATENATE       text-l52              gv_cnt_doc_errr
                                       INTO lv_text.
    macro_log                             0 lv_text.

    CLEAR                                   lv_text.
    CLEAR                                   lv_text.
    CONCATENATE       text-l53              gv_cnt_doc_wrng
                                       INTO lv_text.
    macro_log                             0 lv_text.

    CLEAR                                   lv_text.
    CLEAR                                   lv_text.
    CONCATENATE       text-l54              gv_cnt_doc_pass
                                       INTO lv_text.
    macro_log                             0 lv_text.

  ENDIF.

* Close the file

  IF     ( rb_fsapl      IS NOT INITIAL ).
    IF   ( cv_filename_x IS NOT INITIAL ).
      CLOSE    DATASET cv_filename.
*     CLEAR            cv_filename. "*** do not uncomment ***
      CLEAR            cv_filename_x.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_close_file
*eject
*&---------------------------------------------------------------------*
*&      Form  f_download_files
*&---------------------------------------------------------------------*
*       Download files to the presentation server
*----------------------------------------------------------------------*
FORM f_download_files
  USING    is_file_list                TYPE gty_file_list.

  DATA:    lv_filename                 TYPE string.

  IF     ( rb_fsprs IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_filename.
  MOVE     is_file_list-fn_out_main      TO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = gt_out_data
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
*
  ENDIF.

*eject
  CLEAR                                     lv_filename.
  MOVE     is_file_list-fn_log_main      TO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = gt_log_data
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
*
  ENDIF.

ENDFORM.                    " f_download_files
*eject
*&---------------------------------------------------------------------*
*&      Form  f_dispatch_files
*&---------------------------------------------------------------------*
*       Move the files
*----------------------------------------------------------------------*
FORM f_dispatch_files
  USING    is_file_list                TYPE gty_file_list.

  DATA:    lv_filename                 TYPE text128,
           lv_source_dir               TYPE btcxpgpar,
           lv_target_dir               TYPE btcxpgpar,
           lv_source_fname             TYPE btcxpgpar,
           lv_target_fname             TYPE btcxpgpar,
           ls_return                   TYPE bapireturn,
           lv_text_c120                TYPE text120.

  IF     ( rb_fsapl IS INITIAL ).
    RETURN.
  ENDIF.

*eject

*** Dispatch the inbound file (only in post mode

  IF     ( cb_test IS INITIAL ).

* Rename the input file
    CLEAR                                   lv_filename.
    MOVE     is_file_list-fn_in_arch     TO lv_filename.
    IF     ( gv_fl_errr_prcs             IS NOT INITIAL ).
      TRANSLATE                             lv_filename USING '._'.
      CONCATENATE                           lv_filename text-c23
                                       INTO lv_filename.
    ENDIF.

    CLEAR                                   lv_source_dir.
    MOVE     is_file_list-fp_in_main     TO lv_source_dir.
    CLEAR                                   lv_source_fname.
    MOVE     is_file_list-fn_in_main     TO lv_source_fname.
    CLEAR                                   lv_target_dir.
    MOVE     is_file_list-fp_in_main     TO lv_target_dir.
    CLEAR                                   lv_target_fname.
    MOVE     lv_filename                 TO lv_target_fname.

    CLEAR                                   lv_text_c120.
    MOVE     text-c13                    TO lv_text_c120+04.
    MOVE     text-c15                    TO lv_text_c120+12.
    MOVE     lv_source_dir               TO lv_text_c120+20.
    macro_log                             2 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_source_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     text-c16                    TO lv_text_c120+12.
    MOVE     lv_target_dir               TO lv_text_c120+20.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_target_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.

    CLEAR    ls_return.

    CALL FUNCTION 'ZFI_FILE_HANDLE'
      EXPORTING
        I_SOURCE_DIR   = lv_source_dir
        I_SOURCE_FNAME = lv_source_fname
        I_TARGET_FNAME = lv_target_fname
        I_COMMAND      = 'R'
      IMPORTING
        E_RETURN       = ls_return.

    IF     ( ls_return-type EQ 'E' ).
      macro_msg  'B' 'A' text-253.
      RETURN.
    ENDIF.

*eject
* Move the input file to the inbound archive directory
    CLEAR                                   lv_source_dir.
    MOVE     is_file_list-fp_in_main     TO lv_source_dir.
    CLEAR                                   lv_source_fname.
    MOVE     lv_filename                 TO lv_source_fname.
    CLEAR                                   lv_target_dir.
    MOVE     is_file_list-fp_in_arch     TO lv_target_dir.
    CLEAR                                   lv_target_fname.
    MOVE     lv_filename                 TO lv_target_fname.

    CLEAR                                   lv_text_c120.
    MOVE     text-c12                    TO lv_text_c120+04.
    MOVE     text-c15                    TO lv_text_c120+12.
    MOVE     lv_source_dir               TO lv_text_c120+20.
    macro_log                             1 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_source_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     text-c16                    TO lv_text_c120+12.
    MOVE     lv_target_dir               TO lv_text_c120+20.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_target_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.

    CLEAR    ls_return.

    CALL FUNCTION 'ZFI_FILE_HANDLE'
      EXPORTING
        I_SOURCE_DIR   = lv_source_dir
        I_TARGET_DIR   = lv_target_dir
        I_SOURCE_FNAME = lv_source_fname
        I_TARGET_FNAME = lv_target_fname
        I_COMMAND      = 'M'
      IMPORTING
        E_RETURN       = ls_return.

    IF     ( ls_return-type EQ 'E' ).
      macro_msg  'B' 'A' text-252.
      RETURN.
    ENDIF.

  ENDIF.

*eject

*** Dispatch the outbound file

* Copy the output file to the outbound archive directory
  CLEAR                                     lv_filename.
  MOVE     is_file_list-fn_out_arch      TO lv_filename.
  IF     ( gv_fl_errr_prcs               IS NOT INITIAL ).
    TRANSLATE                               lv_filename USING '._'.
    CONCATENATE                             lv_filename text-c23
                                       INTO lv_filename.
  ENDIF.

  CLEAR                                     lv_source_dir.
  MOVE     is_file_list-fp_out_temp      TO lv_source_dir.
  CLEAR                                     lv_source_fname.
  MOVE     is_file_list-fn_out_temp      TO lv_source_fname.
  CLEAR                                     lv_target_dir.
  MOVE     is_file_list-fp_out_arch      TO lv_target_dir.
  CLEAR                                     lv_target_fname.
  MOVE     lv_filename                   TO lv_target_fname.

  CLEAR                                     lv_text_c120.
  MOVE     text-c11                      TO lv_text_c120+04.
  MOVE     text-c15                      TO lv_text_c120+12.
  MOVE     lv_source_dir                 TO lv_text_c120+20.
  macro_log                               1 lv_text_c120.
  CLEAR                                     lv_text_c120.
  MOVE     lv_source_fname               TO lv_text_c120+25.
  macro_log                               0 lv_text_c120.
  CLEAR                                     lv_text_c120.
  MOVE     text-c16                      TO lv_text_c120+12.
  MOVE     lv_target_dir                 TO lv_text_c120+20.
  macro_log                               0 lv_text_c120.
  CLEAR                                     lv_text_c120.
  MOVE     lv_target_fname               TO lv_text_c120+25.
  macro_log                               0 lv_text_c120.

  CLEAR    ls_return.

  CALL FUNCTION 'ZFI_FILE_HANDLE'
    EXPORTING
      I_SOURCE_DIR   = lv_source_dir
      I_TARGET_DIR   = lv_target_dir
      I_SOURCE_FNAME = lv_source_fname
      I_TARGET_FNAME = lv_target_fname
      I_COMMAND      = 'C'
    IMPORTING
      E_RETURN       = ls_return.

  IF     ( ls_return-type EQ 'E' ).
    macro_msg  'B' 'A' text-251.
    RETURN.
  ENDIF.

*eject
* If this is a test run or there is a process error, then delete
  IF   ( ( cb_test                       IS NOT INITIAL ) OR
         ( gv_fl_errr_prcs               IS NOT INITIAL )    ).

    CLEAR                                   lv_source_dir.
    MOVE     is_file_list-fp_out_temp    TO lv_source_dir.
    CLEAR                                   lv_source_fname.
    MOVE     is_file_list-fn_out_temp    TO lv_source_fname.

    CLEAR                                   lv_text_c120.
    MOVE     text-c14                    TO lv_text_c120+04.
    MOVE     text-c15                    TO lv_text_c120+12.
    MOVE     lv_source_dir               TO lv_text_c120+20.
    macro_log                             1 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_source_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.

    IF   ( gv_filename_out               IS NOT INITIAL ).
      CATCH SYSTEM-EXCEPTIONS FILE_ACCESS_ERRORS = 1.
        DELETE   DATASET gv_filename_out.
      ENDCATCH.
    ENDIF.

  ELSE.

*eject
* Move the output file to the outbound directory
    CLEAR                                   lv_source_dir.
    MOVE     is_file_list-fp_out_temp    TO lv_source_dir.
    CLEAR                                   lv_source_fname.
    MOVE     is_file_list-fn_out_temp    TO lv_source_fname.
    CLEAR                                   lv_target_dir.
    MOVE     is_file_list-fp_out_main    TO lv_target_dir.
    CLEAR                                   lv_target_fname.
    MOVE     is_file_list-fn_out_main    TO lv_target_fname.

    CLEAR                                   lv_text_c120.
    MOVE     text-c12                    TO lv_text_c120+04.
    MOVE     text-c15                    TO lv_text_c120+12.
    MOVE     lv_source_dir               TO lv_text_c120+20.
    macro_log                             1 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_source_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     text-c16                    TO lv_text_c120+12.
    MOVE     lv_target_dir               TO lv_text_c120+20.
    macro_log                             0 lv_text_c120.
    CLEAR                                   lv_text_c120.
    MOVE     lv_target_fname             TO lv_text_c120+25.
    macro_log                             0 lv_text_c120.

    CLEAR    ls_return.

    CALL FUNCTION 'ZFI_FILE_HANDLE'
      EXPORTING
        I_SOURCE_DIR   = lv_source_dir
        I_TARGET_DIR   = lv_target_dir
        I_SOURCE_FNAME = lv_source_fname
        I_TARGET_FNAME = lv_target_fname
        I_COMMAND      = 'M'
      IMPORTING
        E_RETURN       = ls_return.

    IF     ( ls_return-type EQ 'E' ).
      macro_msg  'B' 'A' text-252.
      RETURN.
    ENDIF.

  ENDIF.

ENDFORM.                    " f_dispatch_files
*eject
*&---------------------------------------------------------------------*
*&      Form  f_send_email
*&---------------------------------------------------------------------*
*       Send a notification that there was a process error
*----------------------------------------------------------------------*
FORM f_send_email.

  DATA:    lo_send_request             TYPE REF TO cl_bcs
                                       VALUE IS INITIAL.

  CLASS    cl_bcs DEFINITION LOAD.

  DATA:    lo_document                 TYPE REF TO cl_document_bcs
                                       VALUE IS INITIAL,
           lo_sender                   TYPE REF TO if_sender_bcs
                                       VALUE IS INITIAL,
           lo_recipient                TYPE REF TO if_recipient_bcs
                                       VALUE IS INITIAL,
           lx_document_bcs             TYPE REF TO cx_document_bcs,
           lx_send_req_bcs             TYPE REF TO cx_send_req_bcs,
           lx_address_bcs              TYPE REF TO cx_address_bcs.

  DATA:    lt_text                     TYPE soli_tab,
           ls_text                     TYPE soli,
           lv_text                     TYPE so_text255,
           lv_subject                  TYPE text50,
           ls_email                    LIKE LINE OF s_email.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_text_msg                 TYPE string,
           lv_rc                       TYPE numc5.

  IF   ( ( gv_fl_errr_prcs               IS     INITIAL ) OR
         ( rb_fsapl                      IS     INITIAL ) OR
         ( cb_test                       IS NOT INITIAL ) OR
         ( s_email[]                     IS     INITIAL )    ).
    RETURN.
  ENDIF.

  CLEAR    lv_subrc.
  CLEAR    lv_text_msg.

*eject
* Build the email body
  CLEAR                                     lv_subject.
  MOVE     text-ebs                      TO lv_subject.

  CLEAR    lt_text[].

  CLEAR                                     ls_text.
  APPEND   ls_text                       TO lt_text.

  MOVE     text-eb1                      TO ls_text-line.
  APPEND   ls_text                       TO lt_text.

  CLEAR                                     ls_text.
  APPEND   ls_text                       TO lt_text.

  CLEAR                                     lv_text.
  CONCATENATE  text-eb2  sy-sysid  sy-mandt
               text-eb3  sy-datum  sy-uzeit sy-zonlo
                                       INTO lv_text
                               SEPARATED BY space.
  MOVE     lv_text                       TO ls_text-line.
  APPEND   ls_text                       TO lt_text.

  CLEAR                                     ls_text.
  APPEND   ls_text                       TO lt_text.

* MOVE     gv_tx_errr_prcs               TO ls_text-line.
  APPEND   ls_text                       TO lt_text.

  TRY.

      lo_send_request = cl_bcs=>create_persistent( ).

      lo_document = cl_document_bcs=>create_document(
          i_type    = 'RAW'
          i_text    = lt_text
          i_subject = lv_subject ).

      lo_send_request->set_document( lo_document ).

*eject
* Set the sender of the email
      lo_sender = cl_sapuser_bcs=>create( sy-uname ).

      lo_send_request->set_sender(
        EXPORTING
          i_sender = lo_sender ).

* Set the recipients of the email
      CLEAR                                 ls_email.
      LOOP AT  s_email                 INTO ls_email.

        lo_recipient = cl_cam_address_bcs=>create_internet_address(
          ls_email-low ).

        lo_send_request->add_recipient(
          EXPORTING
            i_recipient = lo_recipient ).

        CLEAR  ls_email.
      ENDLOOP.

* Send the email
      CALL METHOD lo_send_request->set_send_immediately
        EXPORTING
          i_send_immediately = space.

      lo_send_request->send(
        EXPORTING
          i_with_error_screen = space ).

    CATCH    cx_document_bcs           INTO lx_document_bcs.
      lv_subrc    = 1.
      lv_text_msg = lx_document_bcs->get_text( ).
    CATCH    cx_send_req_bcs           INTO lx_send_req_bcs.
      lv_subrc    = 2.
      lv_text_msg = lx_send_req_bcs->get_text( ).
    CATCH    cx_address_bcs            INTO lx_address_bcs.
      lv_subrc    = 3.
      lv_text_msg = lx_address_bcs->get_text( ).

  ENDTRY.

*eject
  IF     ( lv_subrc IS INITIAL ).

    COMMIT WORK.

  ELSE.

    lv_rc = lv_subrc.

    CLEAR                                   lv_text.
    CONCATENATE                             text-261 text-232
                                            lv_rc lv_text_msg
                                       INTO lv_text
                               SEPARATED BY space.
    macro_msg                       'B' 'A' lv_text.

  ENDIF.

ENDFORM.                    " f_send_email
