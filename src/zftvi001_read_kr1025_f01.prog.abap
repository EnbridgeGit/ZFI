*----------------------------------------------------------------------*
*   INCLUDE ZPMO_CCC010_F01                                            *
*----------------------------------------------------------------------*

***********************************************************************
*                         Input Formroutines                          *
***********************************************************************


*---------------------------------------------------------------------*
*                          Form  READ_DATASET
*---------------------------------------------------------------------*
*  Purpose: Read Dataset from Workstation or Server
*---------------------------------------------------------------------*
FORM read_dataset USING    p_op    TYPE c
                           p_subrc LIKE sy-subrc
                           p_msg   TYPE string.

  IF file_inl IS INITIAL.
* Lokale Datei lesen
    PERFORM datenfile_lesen USING p_op p_subrc.
  ELSE.
* Datei vom Server lesen.
    IF file_nin IS INITIAL.
      p_subrc = 1.
      p_op = 'R'.
      p_msg = text-e08.
    ELSE.
*     Begin of MAWH1510789
      CALL METHOD cl_hrpay99_file_check=>check_name
        EXPORTING
          iv_logical_filename  = cv_logical_filename
          iv_parameter_1       = firmennr
        CHANGING
          cv_physical_filename = file_nin
        EXCEPTIONS
          error_occurred       = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid
              TYPE sy-msgty
            NUMBER sy-msgno
              WITH sy-msgv1
                   sy-msgv2
                   sy-msgv3
                   sy-msgv4.
      ENDIF.
*     End of MAWH1510789
      OPEN DATASET file_nin IN TEXT MODE ENCODING UTF-8
                            FOR INPUT MESSAGE p_msg.
      IF sy-subrc = 0.
        DO.
          READ DATASET file_nin INTO t_info_ax_asc.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          APPEND t_info_ax_asc.
        ENDDO.
      ELSE.
        p_op = 'R'.
        p_subrc = sy-subrc.
      ENDIF.

      CLOSE DATASET file_nin.   "VKAMATH - 14OCT11
    ENDIF.
  ENDIF.
*
ENDFORM.                    " READ_DATASET
*-------------------    End of form Read_Dataset   --------------------







*---------------------------------------------------------------------*
*                           FORM DATENFILE_LESEN
*---------------------------------------------------------------------*
* Purpose: Das sequentielle File wird eingelesen (I.Tab: T_Info_AX_Asc)
*---------------------------------------------------------------------*
FORM datenfile_lesen USING p_op    TYPE c
                           p_subrc LIKE sy-subrc.
*
  CLASS cl_gui_frontend_services DEFINITION LOAD.

  DATA: l_filefilter TYPE string,
        l_filename TYPE string,
        l_filetable TYPE filetable,
        l_rc TYPE i,
        l_user_action TYPE i,
        l_window_title TYPE string.

  IF NOT file_nin IS INITIAL.
    l_filename = file_nin.
  ELSE.
    CONCATENATE text-f01 ' (*.TXT)|*.TXT|' INTO l_filefilter.
    CONCATENATE l_filefilter
                cl_gui_frontend_services=>filetype_all
            INTO l_filefilter.

    l_window_title = text-f02.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = l_window_title
        default_extension       = 'TXT'
*       DEFAULT_FILENAME        =
        file_filter             = l_filefilter
        initial_directory       = 'C:\'
*       MULTISELECTION          =
      CHANGING
        file_table              = l_filetable
        rc                      = l_rc
        user_action             = l_user_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      p_op = 'G'.
      p_subrc = sy-subrc.
    ENDIF.

* dialog not canceled
    CHECK l_user_action <> cl_gui_frontend_services=>action_cancel.

    READ TABLE l_filetable INTO l_filename INDEX 1.
    CHECK sy-subrc = 0.
    CHECK NOT l_filename IS INITIAL.

  ENDIF.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_filename
      filetype                = 'ASC'
    CHANGING
      data_tab                = t_info_ax_asc[]
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
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    p_subrc = sy-subrc.
    p_op = 'U'.
  ENDIF.


ENDFORM.                    "datenfile_lesen
*----------------   End of Form  Datenfile_Lesen   --------------------




***********************************************************************
*                         Output Formroutines                         *
***********************************************************************

FORM speichern_output USING  p_op    TYPE c
                             p_subrc LIKE sy-subrc
                             p_msg   TYPE string.

  p_subrc = 0.
*  CHECK testlauf NE 'X'.
  p_subrc = 1.

  IF fil_onl IS INITIAL.
* Lokale Output Datei
    PERFORM lokales_output_speichern USING 'S' p_op p_subrc.
  ELSE.
* Output Datei liegt auf dem Server.
    PERFORM server_output_speichern USING 'S' p_subrc p_msg.
  ENDIF.
ENDFORM.                    "speichern_output
*-------------------   End of form Speichern_Output   -----------------

FORM save_errors USING  p_op         TYPE c
                        p_subrc      LIKE sy-subrc
                        p_msg        TYPE string.
*
  READ TABLE t_errors INDEX 1.
  IF sy-subrc = 4.
    p_subrc = 0.
  ELSE.
    t_errors = header_ax.
    INSERT t_errors INDEX 1.
    IF fil_fnl IS INITIAL.
      PERFORM lokales_output_speichern USING 'E' p_op p_subrc.
    ELSE.
      PERFORM server_output_speichern USING 'E' p_subrc p_msg.
    ENDIF.
  ENDIF.
ENDFORM.                    "save_errors

*---------------------------------------------------------------------*
*      Form  LOKALES_OUTPUT_SPEICHERN
*---------------------------------------------------------------------*
*       Speichert Datei lokal (Workstation / NW Shared Disks)         *
*---------------------------------------------------------------------*
FORM lokales_output_speichern  USING info    TYPE c
                                     p_op    TYPE c
                                     p_subrc LIKE sy-subrc.
*
  CLASS cl_gui_frontend_services DEFINITION LOAD.

  DATA: l_filefilter TYPE string,
        l_filename TYPE string,
        l_filetable TYPE filetable,
        l_rc TYPE i,
        l_user_action TYPE i,
        l_window_title TYPE string.
  DATA: l_default_extension TYPE string.
*
  CLEAR p_subrc.
  IF info = 'E'.
    l_filename = fil_nf.
    IF l_filename IS INITIAL.
      CONCATENATE text-f01 ' (*.TXT)|*.TXT|' INTO l_filefilter.
      CONCATENATE l_filefilter
                  cl_gui_frontend_services=>filetype_all
              INTO l_filefilter.
      l_window_title = text-f04.
      l_default_extension = 'TXT'.
      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          window_title            = l_window_title
          default_extension       = l_default_extension
*         DEFAULT_FILENAME        =
          file_filter             = l_filefilter
          initial_directory       = 'C:\'
*         MULTISELECTION          =
        CHANGING
          file_table              = l_filetable
          rc                      = l_rc
          user_action             = l_user_action
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.

      IF sy-subrc NE 0.
        p_op = 'G'.
        p_subrc = sy-subrc.
        EXIT.
      ELSE.
        READ TABLE l_filetable INTO l_filename INDEX 1.
        IF sy-subrc NE 0 OR l_filename IS INITIAL.
          p_op = 'G'.
          p_subrc = sy-subrc.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = l_filename
        filetype                = 'ASC'
      CHANGING
        data_tab                = t_errors[]
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
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
  ELSE.
    l_filename = fil_nout.
    IF l_filename IS INITIAL.
      CONCATENATE text-f01 ' (*.CCD)|*.CCD|' INTO l_filefilter.
      CONCATENATE l_filefilter
                  cl_gui_frontend_services=>filetype_all
              INTO l_filefilter.
      l_window_title = text-f03.
      l_default_extension = 'CCD'.
      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          window_title            = l_window_title
          default_extension       = l_default_extension
*         DEFAULT_FILENAME        =
          file_filter             = l_filefilter
          initial_directory       = 'C:\'
*         MULTISELECTION          =
        CHANGING
          file_table              = l_filetable
          rc                      = l_rc
          user_action             = l_user_action
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.

      IF sy-subrc NE 0.     "Fehlerbehandlung des WS_FILENAME_GET
        p_op = 'G'.
        p_subrc = sy-subrc.
        EXIT.
      ELSE.
        READ TABLE l_filetable INTO l_filename INDEX 1.
        IF sy-subrc NE 0 OR l_filename IS INITIAL.
          p_op = 'G'.
          p_subrc = sy-subrc.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = l_filename
        filetype                = 'ASC'
      CHANGING
        data_tab                = t_info_sap_asc[]
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
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.

  ENDIF.
ENDFORM.                    "lokales_output_speichern
*--------------   End of form  Lokales_Output_Speichern   -------------



*---------------------------------------------------------------------*
*      Form  SERVER_OUTPUT_SPEICHERN
*---------------------------------------------------------------------*
*       Speichert Datei auf den Server                                *
*---------------------------------------------------------------------*
FORM server_output_speichern USING info    TYPE c
                                   p_subrc LIKE sy-subrc
                                   p_msg   TYPE string.
*
  DATA: popup_answer.
  DATA: file_for_save LIKE fil_nout.
*
  IF info = 'E'.
    file_for_save = fil_nf.
  ELSE.
    file_for_save = fil_nout.
  ENDIF.
  IF file_for_save IS INITIAL.
    p_msg = text-e10.
    p_subrc = 1.
    EXIT.
  ENDIF.
* Begin of MAWH1510789
  CALL METHOD cl_hrpay99_file_check=>check_name
    EXPORTING
      iv_logical_filename  = cv_logical_filename
      iv_parameter_1       = firmennr
    CHANGING
      cv_physical_filename = file_for_save
    EXCEPTIONS
      error_occurred       = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1
               sy-msgv2
               sy-msgv3
               sy-msgv4.
  ENDIF.
* End of MAWH1510789
  OPEN DATASET file_for_save IN TEXT MODE ENCODING NON-UNICODE
                             FOR INPUT MESSAGE p_msg.
  IF sy-subrc = 0.
    CLOSE DATASET file_for_save.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = text-p01
        text_question         = text-p02
        text_button_1         = 'Ja'(001)
        text_button_2         = 'Nein'(002)
        default_button        = '1'
        display_cancel_button = ' '
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = popup_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.
    IF sy-subrc <> 0.
      p_msg = text-e98.
      EXIT.
    ENDIF.
    IF popup_answer NE '1'.
      p_msg = text-e11.
      EXIT.
    ENDIF.
  ENDIF.
*
* Begin of MAWH1510789
  CALL METHOD cl_hrpay99_file_check=>check_name
    EXPORTING
      iv_logical_filename  = cv_logical_filename
      iv_parameter_1       = firmennr
    CHANGING
      cv_physical_filename = file_for_save
    EXCEPTIONS
      error_occurred       = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1
               sy-msgv2
               sy-msgv3
               sy-msgv4.
  ENDIF.
* End of MAWH1510789
  OPEN DATASET file_for_save IN TEXT MODE ENCODING NON-UNICODE
                             FOR OUTPUT MESSAGE p_msg.
  IF sy-subrc = 0.
    IF info = 'E'.
      LOOP AT t_errors.
        TRANSFER t_errors TO file_for_save.
      ENDLOOP.
    ELSE.
      LOOP AT t_info_sap_asc.
        TRANSFER t_info_sap_asc TO file_for_save.
      ENDLOOP.
    ENDIF.

    CLOSE DATASET file_for_save.
    IF sy-subrc = 0.
      p_subrc = 0.
    ENDIF.

  ELSE.
    p_subrc = sy-subrc.
  ENDIF.
ENDFORM.                    "server_output_speichern
*--------------   End of form  Server_Output_Speichern   -------------

* BOI - VKAMATH - 13DEC11

FORM f_select_xparam.

  SELECT *
         FROM zfit_xparam
         INTO TABLE git_xparam
         WHERE paramtype = gc_paramtype AND
               subtype   = gc_subtype.

ENDFORM.                    "f_select_xparam

*&---------------------------------------------------------------------*
*&      Form  f_archive_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_archive_file.

  DATA: lwa_xparam       TYPE zfit_xparam,
        lwa_return       TYPE bapireturn,
        lv_arc_fpath     TYPE zparamvalue,
        lv_file          TYPE localfile,
        lv_s_dir         TYPE sxpgcolist-parameters,
        lv_s_file        TYPE sxpgcolist-parameters,
        lv_source_fpath  TYPE sxpgcolist-parameters,
        lv_source_fname  TYPE zparamvalue,
        lv_target_fname  TYPE zparamvalue.

  CONSTANTS: lc_arcfile  TYPE zparamkey VALUE 'ARCHIVE_FILEPATH',
             lc_slash(3) TYPE c VALUE '*\*',
             lc_sep(1)   TYPE c VALUE '_',
             lc_arc(3)   TYPE c VALUE 'ARC',
             lc_m(1)     TYPE c VALUE 'M',
             lc_x(1)     TYPE c VALUE 'X',
             lc_e(1)     TYPE c VALUE 'E'.

* Get Archive Filepath
  READ TABLE git_xparam INTO lwa_xparam
             WITH KEY key1 = lc_arcfile.

  IF sy-subrc = 0.
    lv_arc_fpath = lwa_xparam-value1.
    CLEAR lwa_xparam.
  ENDIF.

* Split Filepath and Filename
  lv_file = file_nin.

  IF lv_file CP lc_slash.
    DO.
      IF lv_file CP lc_slash.
        CONCATENATE lv_s_dir
                    lv_file+0(1)
                    INTO lv_s_dir.
        SHIFT lv_file LEFT BY 1 PLACES.
      ELSE.
        lv_s_file = lv_file.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.

* Source Filepath & name
  lv_source_fpath = lv_s_dir.
  lv_source_fname = lv_s_file.

* Target Filename
  CONCATENATE lv_source_fname
              lc_arc
              INTO lv_target_fname
              SEPARATED BY lc_sep.

  CALL FUNCTION 'ZFI_FILE_HANDLE'
    EXPORTING
      i_source_dir        = lv_source_fpath
      i_target_dir        = lv_arc_fpath
      i_source_fname      = lv_source_fname
      i_target_fname      = lv_target_fname
      i_command           = lc_m
      i_date_time_stamp   = lc_x
      i_rename_arc_to_new = space
    IMPORTING
      e_return            = lwa_return.

  IF lwa_return-type = lc_e.
    MESSAGE i000(zfi01) WITH lwa_return-message.
  ENDIF.

ENDFORM.                    "f_archive_file

* EOI - VKAMATH - 13DEC11
