*&---------------------------------------------------------------------*
*&  Include           ZLPSI017_RECON_PLANT_SUB
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_F4_HELP
*&---------------------------------------------------------------------*
*       F4 Help for Input and Output File
*----------------------------------------------------------------------*
FORM f_f4_help  USING    p_p_opath TYPE any.

  DATA: lit_filetable    TYPE filetable,
        lwa_filetable    TYPE LINE OF filetable,
        lv_subrc         TYPE i VALUE 1.

  CONSTANTS: lc_title     TYPE string VALUE 'SELECT FILE',
             lc_dir       TYPE string VALUE 'C:\'.

  IF rb_pres = gc_x.

* F4 Help - Presentation Server
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = lc_title
        initial_directory       = lc_dir
        multiselection          = ' '
      CHANGING
        file_table              = lit_filetable
        rc                      = lv_subrc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    IF sy-subrc = 0.
      READ TABLE lit_filetable INTO lwa_filetable
                 INDEX 1.
      IF sy-subrc = 0.
        p_p_opath = lwa_filetable-filename.
      ENDIF.
    ENDIF.

* F4 Help - Application Server
  ELSEIF rb_appl = gc_x.

    DATA: lv_ap_op_path(250)  TYPE c.

    SUBMIT rs_get_f4_dir_from_applserv AND RETURN.

    IMPORT path_name TO lv_ap_op_path FROM MEMORY ID 'PATH_NAME_SDL'.

    p_p_opath = lv_ap_op_path.

  ENDIF.

ENDFORM.                                                    " F_F4_HELP

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
* Fetch the Totals from FAGLFLEXT table
*----------------------------------------------------------------------*
FORM f_get_data .

  SELECT ryear
         objnr00
         objnr01
         objnr02
         objnr03
         objnr04
         objnr05
         objnr06
         objnr07
         objnr08
         drcrk
         rpmax
         rldnr
         racct
         rbukrs
         hslvt
         hsl01
         hsl02
         hsl03
         hsl04
         hsl05
         hsl06
         hsl07
         hsl08
         hsl09
         hsl10
         hsl11
         hsl12
         hsl13
         hsl14
         hsl15
         hsl16
         FROM faglflext
         INTO TABLE git_faglflext
         WHERE ryear  =  p_year   AND
               rbukrs IN s_bukrs  AND
               racct  IN s_racct  AND
               rldnr  IN s_ledger AND
               rrcty = '0'."        AND
*               awtyp = 'BKPF'.

  SORT git_faglflext BY rbukrs racct rldnr ryear drcrk.

  LOOP AT git_faglflext INTO gwa_faglflext.
    gwa_data-rbukrs = gwa_faglflext-rbukrs.
    gwa_data-racct  = gwa_faglflext-racct.
    gwa_data-rldnr  = gwa_faglflext-rldnr.
    gwa_data-ryear  = gwa_faglflext-ryear.
    ADD gwa_faglflext-hsl01 FROM 1 TO p_perd GIVING gwa_data-amount.

*Add the Balance carried forward to the cumulative total
    gwa_data-amount = gwa_data-amount + gwa_faglflext-hslvt.
    COLLECT gwa_data INTO git_data.
    CLEAR: gwa_faglflext,
           gwa_data.
  ENDLOOP.

ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE
*&---------------------------------------------------------------------*
* Validate the Screen entries
*----------------------------------------------------------------------*

FORM f_validate .

  IF p_perd > 16 OR p_perd < 01.
    gv_err_flag = gc_x.
    MESSAGE i208(00) WITH 'Period should be between 01 and 16'(003).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_bukrs[] IS INITIAL.
**    gv_err_flag = gc_x.
**    MESSAGE i208(00) WITH 'Enter atleast ONE Company Code'(004).
**    LEAVE LIST-PROCESSING.
  ENDIF.

  IF s_racct[] IS INITIAL.
**    gv_err_flag = gc_x.
**    MESSAGE i208(00) WITH 'Enter atleast ONE GL Account'(005).
**    LEAVE LIST-PROCESSING.
  ENDIF.


  IF s_ledger[] IS INITIAL.
    gv_err_flag = gc_x.
    MESSAGE i208(00) WITH 'Enter The Ledger'(006).
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_opath IS INITIAL.
    gv_err_flag = gc_x.
    MESSAGE i208(00) WITH 'Enter The Ouput Filepath'(007).
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " F_VALIDATE

*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_FILE
*&---------------------------------------------------------------------*
* Generate the Output file at Presentation Server/Application Server
*----------------------------------------------------------------------*
FORM f_generate_file .

  DATA: lwa_file      TYPE ty_file,
        lv_record     TYPE string,
*        lv_delim(1)   TYPE c VALUE '|',
        lc_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
        lv_amount(26) TYPE c,
        lv_period(6)  TYPE c,
        lv_ledger(4) TYPE c.

  IF git_data[] IS NOT INITIAL.
    LOOP AT git_data INTO gwa_data.
*replace Ledger code with description For 0L -> #GAAP#, For P1 -> 'FERC'
      IF gwa_data-rldnr = '0L'.
        lv_ledger = 'GAAP'.
      ELSEIF gwa_data-rldnr = 'P1'.
        lv_ledger = 'FERC'.
      ENDIF.
      WRITE gwa_data-amount TO lv_amount CURRENCY ''.
      CONDENSE lv_amount.

      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_amount.

      CONCATENATE gwa_data-ryear
                  p_perd
                  INTO lv_period.
      CONCATENATE lv_period
                  gwa_data-rbukrs
                  gwa_data-racct
                  lv_ledger
                  lv_amount
                  INTO lv_record
                  SEPARATED BY lc_tab.

      lwa_file-record = lv_record.
      APPEND lwa_file TO git_file.
      CLEAR: lv_record,
             lv_period,
             lwa_file,
             lv_ledger.
    ENDLOOP.

* Create Output File
    PERFORM f_create_output_file USING p_opath
                                       git_file.
  ENDIF.

ENDFORM.                    " F_GENERATE_FILE

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_OUTPUT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_create_output_file  USING    p_p_opath TYPE rlgrap-filename
                                    p_git_file TYPE tt_file.

  DATA: lv_file  TYPE string,
        lwa_file TYPE ty_file.


  CHECK p_p_opath IS NOT INITIAL.

  lv_file = p_p_opath.

  IF rb_pres = gc_x.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_file
      TABLES
        data_tab                = p_git_file
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

    IF sy-subrc <> 0.
      WRITE:/ 'Error while downloading the Output file'(008), lv_file.
    ELSE.
      SKIP 1.
      WRITE:/ 'Output file was downloaded at'(009), lv_file.
    ENDIF.

  ELSE.
    CLEAR gv_fname.
    CONCATENATE 'Account_recon'(017)
*                sy-datum
*                sy-uzeit
                '.txt'(018)
                INTO gv_fname.

    CONCATENATE lv_file
                gv_fname
                INTO
                lv_file.


* Open File in Application Server
    OPEN DATASET lv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

* Write Success/Error Records to File
    IF sy-subrc = 0.
      LOOP AT p_git_file INTO lwa_file.
        TRANSFER lwa_file-record TO lv_file.
      ENDLOOP.

* Close File
      CLOSE DATASET lv_file.
      WRITE:/ 'Output file was downloaded at'(009), lv_file.

      PERFORM f_archive_file.

    ELSE.
      WRITE:/ 'Error while downloading the Output file'(008), lv_file.
    ENDIF.

  ENDIF.
ENDFORM.                    " F_CREATE_OUTPUT_FILE

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
* Display the Result on the Output screen
*----------------------------------------------------------------------*
FORM f_display_result .

  DATA: lv_bukrs  TYPE string,
        lv_ledger TYPE string,
        lv_racct  TYPE string.

  WRITE:/ 'Year            :'(012), p_year.
  SKIP 1.
  WRITE:/ 'Month           :'(013), p_perd.
  SKIP 1.
  LOOP AT s_bukrs.
    CONCATENATE s_bukrs-low
                lv_bukrs
                INTO lv_bukrs
                SEPARATED BY space.
  ENDLOOP.
  LOOP AT s_ledger.
    CONCATENATE s_ledger-low
                lv_ledger
                INTO lv_ledger
                SEPARATED BY space.
  ENDLOOP.
  LOOP AT s_racct.
    CONCATENATE s_racct-low
                s_racct-high
                lv_racct
                INTO lv_racct
                SEPARATED BY space.
  ENDLOOP.

  WRITE:/ 'Company Code(s) :'(014), lv_bukrs.
  SKIP 1.
  WRITE:/ 'GL Account(s)   :'(015), lv_racct.
  SKIP 1.
  WRITE:/ 'Ledger Group(s) :'(016), lv_ledger.
  SKIP 1.

ENDFORM.                    " F_DISPLAY_RESULT

*&---------------------------------------------------------------------*
*&      Form  F_SELECT_XPARAM
*&---------------------------------------------------------------------*
*      Get ZFIT_XPARAM Entries
*----------------------------------------------------------------------*
FORM f_select_xparam .

  SELECT *
         FROM zfit_xparam
         INTO TABLE git_xparam
         WHERE paramtype = gc_paramtype AND
               subtype   = gc_subtype.
  IF sy-subrc = 0.
    SORT git_xparam BY paramtype subtype key1.
  ENDIF.

* Check if Archive Filepath has been maintained in ZFIT_XPARAM
  READ TABLE git_xparam INTO gwa_xparam WITH KEY paramtype = gc_paramtype
                                                 subtype   = gc_subtype
                                                 key1      = gc_outfile
                                                 BINARY SEARCH.
  IF sy-subrc = 0.
    IF rb_appl = gc_x.
      p_opath = gwa_xparam-value1.
    ENDIF.
  ELSE.
    gv_err_flag = gc_x.
    MESSAGE i208(00) WITH 'Archive Filepath not maintained in ZFIT_XPARAM'(019).
    EXIT.
  ENDIF.

  CLEAR gwa_xparam.

ENDFORM.                    " F_SELECT_XPARAM

*&---------------------------------------------------------------------*
*&      Form  F_ARCHIVE_FILE
*&---------------------------------------------------------------------*
*       File Archive
*----------------------------------------------------------------------*
FORM f_archive_file .

  DATA: lwa_return        TYPE bapireturn,
        lv_arc_fpath      TYPE zparamvalue,
        lv_sourcefpath    TYPE zparamvalue,
        lv_source_fname   TYPE zparamvalue,
        lv_target_fname   TYPE zparamvalue.

  DATA: lv_message TYPE string.

  READ TABLE git_xparam INTO gwa_xparam WITH KEY paramtype = gc_paramtype
                                                 subtype   = gc_subtype
                                                 key1      = gc_archive
                                                 BINARY SEARCH.
  IF sy-subrc = 0.
    lv_sourcefpath  = p_opath.
    lv_arc_fpath    = gwa_xparam-value1.
    lv_source_fname = gv_fname.
    lv_target_fname = gv_fname.
  ENDIF.

  CALL FUNCTION 'ZFI_FILE_HANDLE'
    EXPORTING
      i_source_dir        = lv_sourcefpath
      i_target_dir        = lv_arc_fpath
      i_source_fname      = lv_source_fname
      i_target_fname      = lv_target_fname
      i_command           = gc_c
*     i_date_time_stamp   = gc_x
*     i_rename_arc_to_new = gc_x
    IMPORTING
      e_return            = lwa_return.


  IF lwa_return-type = gc_e.
    SKIP 1.
    WRITE:/ 'Error in Archiving file'(010).
  ELSE.
    SKIP 1.
    CONDENSE gv_fname.
    CONCATENATE gv_fname
     'File was Archived successfully'(011)
     INTO lv_message SEPARATED BY space.
    WRITE:/ lv_message.
  ENDIF.

  CLEAR: lv_source_fname,
         lv_target_fname,
         lwa_return.
ENDFORM.                    " F_ARCHIVE_FILE
