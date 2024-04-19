*&---------------------------------------------------------------------*
*& Report  ZFGLI020_FXUPDDLY
*&
*&---------------------------------------------------------------------*
REPORT  zfgli020_fxupddly MESSAGE-ID zfi01.
*&---------------------------------------------------------------------*
*& Program Name       :  ZFGLI020_FXUPDDLY                             *
*& Author             :  Har Mayank Shrivastava                        *
*& Creation Date      :  02-Aug-2011                                   *
*& Object ID          :  I_P2C_GL_020 - Daily Exchange Upload          *
*& Application Area   :  FI-GL                                         *
*& Transport Request  :  DECK901134                                    *
*& Description        :  Interface Program for Converting Bloomberg    *
*&                       Foreign Exchange Data Feed to SAP Format Ready*
*&                       for Transaction TBDM                          *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 02-Aug-2011                                          *
* Modified By   : Har Mayank Shrivastava                               *
* Transport #   : DECK901134                                           *
* Description   : Initial Version                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 02-Sep-2011                                          *
* Modified By   : Har Mayank Shrivastava                               *
* Transport #   : DECK901845                                           *
* Description   : Changes for the Rate Type D and M                    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 3.0                                                  *
* Date          : 08-Sep-2011                                          *
* Modified By   : Har Mayank Shrivastava                               *
* Transport #   : DECK901931                                           *
* Description   : Changes for the Rate Type D and M                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               TYPES
*----------------------------------------------------------------------*
TYPES : BEGIN OF ty_dwnflfrmt,
         data_class TYPE char2,                   " Data Class
         frmcur_cky TYPE char20,                  " From Currency
         tocur_cky  TYPE char20,                  " To Currency
         market     TYPE char15,                  " Market
         date       TYPE char8,                   " Date
         time       TYPE char6,                   " Time
         value      TYPE char20,                  " Value
         currency   TYPE char20,                  " Currency
         from_curr  TYPE char7,                   " From Currency
         to_curr    TYPE char7,                   " To Currency
         pricenot   TYPE char5,                   " Price notification
         status     TYPE char2,                   " Status
         error_msg  TYPE char80,                  " Error Message
       END OF ty_dwnflfrmt,

       BEGIN OF ty_upflfrmt,
         filestrng  TYPE string,                  " File String
       END OF ty_upflfrmt.

TYPES: BEGIN OF ty_email_data,
       filename      TYPE sdokpath-pathname,      " Filepath with Name
       log           TYPE string,                 " Error Log
       END OF ty_email_data.

*----------------------------------------------------------------------*
*                             CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS: gc_x             TYPE char01           VALUE 'X',
           gc_01            TYPE char02           VALUE '01',
           gc_1             TYPE char01           VALUE '1',
           gc_usd           TYPE char03           VALUE 'USD',
           gc_m             TYPE char01           VALUE 'M',
           gc_d             TYPE char01           VALUE 'D',
           gc_e	            TYPE char01           VALUE 'E',
           gc_a             TYPE char01           VALUE 'A',
           gc_pres          TYPE char04           VALUE 'PRES',
           gc_app           TYPE char03           VALUE 'APP',
           gc_sep           TYPE char01           VALUE '|',
           gc_paramtype     TYPE zparamtype       VALUE 'BLOOMFEX',
           gc_subtype       TYPE zparamtype       VALUE 'I_P2C_GL_020',
           gc_appflnm       TYPE char11           VALUE 'APPFILENAME',
           gc_dwna          TYPE char11           VALUE 'APPDOWNLOAD',
           gc_erremail      TYPE char11           VALUE 'ERREMAIL',
           gc_archive       TYPE char07           VALUE 'ARCHIVE'.

*----------------------------------------------------------------------*
*                             VARIABLES                                *
*----------------------------------------------------------------------*
DATA : gv_fullfilenm     TYPE string,
       gv_filetyp        TYPE string,
       gv_filenm         TYPE string,
       gv_dummyflnm      TYPE string,
       gv_source         TYPE zparamkey,
       gv_archv          TYPE zparamkey,
       gv_filbla         TYPE zparamvalue.

*----------------------------------------------------------------------*
*                         INTERNAL TABLES
*----------------------------------------------------------------------*
DATA : git_upflfrmt     TYPE STANDARD TABLE OF ty_upflfrmt,
       git_dwnflfrmt    TYPE STANDARD TABLE OF ty_dwnflfrmt,
       git_dwnflfrmt_em TYPE STANDARD TABLE OF ty_dwnflfrmt,
       git_email_data   TYPE STANDARD TABLE OF ty_email_data,
       git_xparam       TYPE STANDARD TABLE OF zfit_xparam.

*----------------------------------------------------------------------*
*                         SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.

PARAMETERS: p_applbl      RADIOBUTTON GROUP grp1,
            p_filbla      TYPE rlgrap-filename,
            p_presbl      RADIOBUTTON GROUP grp1,
            p_filblp      TYPE string.

SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.

PARAMETERS: p_appldw      RADIOBUTTON GROUP grp2,
            p_fildwa      TYPE rlgrap-filename,
            p_presdw      RADIOBUTTON GROUP grp2,
            p_fildwp      TYPE string.
SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-003.
PARAMETERS: p_errml       TYPE string OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk3.

*----------------------------------------------------------------------*
*             At Selection-Screen on Value Request                     *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fildwp.
* F4 Help for File Path
  PERFORM f_f4_help_presentation CHANGING gv_dummyflnm
                                          p_fildwp.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filblp.
* F4 Help for FileName
  PERFORM f_f4_help_sel_pres CHANGING p_filblp.

AT SELECTION-SCREEN ON  VALUE-REQUEST FOR p_filbla.
* F4 help from Application  server
  PERFORM f_f4_help_sel_app.

*----------------------------------------------------------------------*
*                       At Selection-Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Check if the filename is empty
  PERFORM f_check_fileempty_bl.
* Check if the filepath is empty
  PERFORM f_check_fpempty_dw.

*----------------------------------------------------------------------*
*                         Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
* Fetch the ZFITXPARAM Table Data.
  PERFORM f_select_xparam.
* Populate the Selection Screen Field Values from Parameter Table.
  PERFORM f_populate_filename.

*----------------------------------------------------------------------*
*                         Start-of-Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Read File from Presentation / Application Server
  PERFORM f_upload_file.

* Read File Content from file and Create Internal Table from the File Read
  IF NOT git_upflfrmt IS INITIAL.
    PERFORM f_read_file_make_it.
  ENDIF.

*----------------------------------------------------------------------*
*                         End-of-Selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* If the presentation Server Option is selected.
  IF p_presdw EQ gc_x.
*   Split the filename from the full filepath and use the filename for downloading new file.
    PERFORM f_splitfilename USING p_filbla
                            CHANGING gv_filenm.

    CONCATENATE p_fildwp
                gv_filenm INTO
                gv_fullfilenm.

    gv_filetyp = gc_pres.

* Download File to Presentation Server
    PERFORM f_download_file USING gv_fullfilenm
                                  gv_filetyp.
* If the Application Server Option is selected.
  ELSE.
*   Split the filename from the full filepath and use the filename for downloading new file.
    PERFORM f_splitfilename USING p_filbla
                            CHANGING gv_filenm.

    CONCATENATE p_fildwa
              gv_filenm INTO
              gv_fullfilenm.

    gv_filetyp = gc_app.
* Download File to Application Server
    PERFORM f_download_file USING gv_fullfilenm
                                  gv_filetyp.
  ENDIF.

* Send Email containing Run Details
  IF NOT git_email_data IS INITIAL.
    PERFORM f_send_email.
  ENDIF.

*&---------------------------------------------------------------------*
*&                       Form  F_UPLOAD_FILE
*&---------------------------------------------------------------------*
*                            Upload File
*----------------------------------------------------------------------*
FORM f_upload_file .
  IF p_presbl EQ gc_x.
    PERFORM f_upload_presentation.
  ELSE.
    PERFORM f_upload_application.
  ENDIF.
ENDFORM.                    " F_UPLOAD_FILE

*&---------------------------------------------------------------------*
*&                      Form  F_UPLOAD_PRESENTATION
*&---------------------------------------------------------------------*
*                      Upload File From Presentation Server
*----------------------------------------------------------------------*
FORM f_upload_presentation.

  DATA : lwa_email_data   TYPE ty_email_data.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = p_filblp
    TABLES
      data_tab                = git_upflfrmt
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
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    lwa_email_data-filename = p_filblp.
    lwa_email_data-log = text-014.
    APPEND lwa_email_data TO git_email_data.
  ENDIF.

ENDFORM.                    " F_UPLOAD_PRESENTATION

*&---------------------------------------------------------------------*
*&      Form  f_f4_help_presentation
*&---------------------------------------------------------------------*
*       F4 Help - Presentation
*----------------------------------------------------------------------*
FORM f_f4_help_presentation CHANGING p_filename TYPE string
                                     p_fpath    TYPE string.

  CONSTANTS: lc_title TYPE string VALUE 'SELECT FILE'.


* F4 Help - Presentation Server
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = lc_title
      default_file_name    = p_filename
      prompt_on_overwrite  = gc_x
    CHANGING
      filename             = p_filename
      path                 = p_fpath
      fullpath             = p_fpath
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "f_f4_help_presentation
*&---------------------------------------------------------------------*
*&      Form  F_F4_HELP_SEL_PRES
*&---------------------------------------------------------------------*
*       F4 Help for Presentation Server Filename.
*----------------------------------------------------------------------*
FORM f_f4_help_sel_pres  CHANGING p_p_filblp TYPE any.

  CONSTANTS : lc_title   TYPE string VALUE 'SELECT FILE',
              lc_dir     TYPE string VALUE 'C:\',
              lc_defext  TYPE string VALUE '*.csv',
              lc_filter  TYPE string VALUE '*.csv'.

  DATA:  lit_filetable  TYPE filetable,
         lwa_filetable  TYPE LINE OF filetable,
         lv_subrc       TYPE i VALUE 1.

* F4 Help - Presentation Server
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lc_title
      initial_directory       = lc_dir
      default_extension       = lc_defext
      file_filter             = lc_filter
    CHANGING
      file_table              = lit_filetable
      rc                      = lv_subrc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE lit_filetable INTO lwa_filetable INDEX 1.
    IF sy-subrc = 0.
      p_p_filblp = lwa_filetable-filename.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_F4_HELP_SEL_PRES
*&---------------------------------------------------------------------*
*&                      Form  F_READ_FILE_DATA
*&---------------------------------------------------------------------*
*               Read File Data and Move to internal table
*----------------------------------------------------------------------*
FORM f_read_file_make_it.

  CONSTANTS : lc_4      TYPE char01 VALUE '4',
              lc_6      TYPE char01 VALUE '6',
              lc_7      TYPE char01 VALUE '7'.

  DATA :      lwa_upflfrmt   TYPE ty_upflfrmt,
              lv_fieldval    TYPE string,
              lv_frmcrcy     TYPE char20,
              lv_contridt    TYPE char10,
              lv_val(16)     TYPE p DECIMALS 5.

* Read the First Record from the file that contains the Feed
  READ TABLE git_upflfrmt INTO lwa_upflfrmt INDEX 1.

* Loop at the File String and Capture individual Field Value
  DO.
    SPLIT lwa_upflfrmt-filestrng
          AT gc_sep
          INTO lv_fieldval
               lwa_upflfrmt-filestrng.

*   Capture the 4th, 6th and 7th Field which is required for the conversion
    CASE sy-index.
      WHEN lc_4.
        lv_frmcrcy = lv_fieldval.
      WHEN lc_6.
        lv_contridt = lv_fieldval.
      WHEN lc_7.
        lv_val = lv_fieldval.
      WHEN OTHERS.
        IF sy-index GE '8'.
          EXIT.
        ENDIF.
    ENDCASE.
  ENDDO.

* Prepare Internal table from the Data of the Feed.
  PERFORM f_convert_file USING lv_frmcrcy
                               lv_contridt
                               lv_val.
ENDFORM.                    " F_READ_FILE_DATA
*&---------------------------------------------------------------------*
*&                      Form  F_CONVERT_FILE
*&---------------------------------------------------------------------*
*               Prepare Internal Table from the file string
*----------------------------------------------------------------------*
FORM f_convert_file  USING    p_lv_frmcrcy TYPE any
                              p_lv_contridt TYPE any        "#EC NEEDED
                              p_lv_val TYPE any.

  CONSTANTS : lc_daily TYPE char05 VALUE 'DAILY',
              lc_eom   TYPE char03 VALUE 'EOM',
              lc_aveg  TYPE char07 VALUE 'AVERAGE'.

  DATA: lwa_dwnflfrmt     TYPE ty_dwnflfrmt,
        lwa_dwnflfrmtem   TYPE ty_dwnflfrmt,
        lv_filename       TYPE string.

  lwa_dwnflfrmt-data_class  = gc_01.
  lwa_dwnflfrmt-frmcur_cky  = p_lv_frmcrcy.
  lwa_dwnflfrmt-tocur_cky   = gc_usd.
  IF p_applbl EQ gc_x.
    lv_filename = p_filbla.
  ELSEIF p_presbl EQ gc_x.
    lv_filename = p_filblp.
  ENDIF.
  TRANSLATE lv_filename TO UPPER CASE.
  IF lv_filename CS lc_daily.
    lwa_dwnflfrmt-market = gc_m.
  ELSEIF lv_filename CS lc_eom.
    lwa_dwnflfrmt-market = gc_e.
  ELSEIF lv_filename CS lc_aveg.
    lwa_dwnflfrmt-market = gc_a.
  ENDIF.
  CONCATENATE p_lv_contridt+3(2)
              p_lv_contridt+0(2)
              p_lv_contridt+6(4)
               INTO lwa_dwnflfrmt-date.
*  lwa_dwnflfrmt-date        = sy-datum.
  lwa_dwnflfrmt-time        = sy-uzeit.
  lwa_dwnflfrmt-value       = p_lv_val.
  lwa_dwnflfrmt-from_curr   = gc_1.
  lwa_dwnflfrmt-to_curr     = gc_1.

  IF lv_filename CS lc_eom.
    APPEND lwa_dwnflfrmt TO git_dwnflfrmt.
    MOVE lwa_dwnflfrmt TO lwa_dwnflfrmtem.
    lwa_dwnflfrmtem-market = gc_m.
    APPEND lwa_dwnflfrmtem TO git_dwnflfrmt.
  ELSE.
    APPEND lwa_dwnflfrmt TO git_dwnflfrmt.
  ENDIF.

ENDFORM.                    " F_CONVERT_FILE
*&---------------------------------------------------------------------*
*&                      Form  F_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*                           Download File
*----------------------------------------------------------------------*
FORM f_download_file USING p_filename TYPE any
                           p_filetyp TYPE any.
  DATA : lwa_dwnflfrmt    TYPE ty_dwnflfrmt,
         lv_record        TYPE string,
         lwa_email_data   TYPE ty_email_data.

  CHECK NOT git_dwnflfrmt IS INITIAL.

  IF p_filetyp = gc_pres.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = p_filename
      CHANGING
        data_tab                = git_dwnflfrmt
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
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

      lwa_email_data-filename = p_filename.
      lwa_email_data-log = text-015.
      APPEND lwa_email_data TO git_email_data.

    ELSE.
      WRITE:/ text-004, p_filename.
    ENDIF.
  ELSE.
*  Opening the File
    OPEN DATASET p_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    IF sy-subrc <> 0.

      WRITE:/ text-005.
      lwa_email_data-filename = p_filename.
      lwa_email_data-log = text-015.
      APPEND lwa_email_data TO git_email_data.

    ELSE.

* Transferring Data
      LOOP AT git_dwnflfrmt INTO lwa_dwnflfrmt.
        CONCATENATE lwa_dwnflfrmt-data_class
                    lwa_dwnflfrmt-frmcur_cky
                    lwa_dwnflfrmt-tocur_cky
                    lwa_dwnflfrmt-market
                    lwa_dwnflfrmt-date
                    lwa_dwnflfrmt-time
                    lwa_dwnflfrmt-value
                    lwa_dwnflfrmt-currency
                    lwa_dwnflfrmt-from_curr
                    lwa_dwnflfrmt-to_curr
                    lwa_dwnflfrmt-pricenot
                    lwa_dwnflfrmt-status
                    lwa_dwnflfrmt-error_msg
                    INTO lv_record RESPECTING BLANKS.
        TRANSFER lv_record TO p_filename.
      ENDLOOP.
      CLOSE DATASET p_filename.
      WRITE:/ text-004, p_filename.
* Archive the file once the whole process ended successfully.
      PERFORM f_archive_file USING p_filbla
                             gc_m.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&                        Form  F_UPLOAD_APPLICATION
*&---------------------------------------------------------------------*
*                       Upload File from Application Server
*----------------------------------------------------------------------*
FORM f_upload_application.
  DATA :      lwa_upflfrmt   TYPE ty_upflfrmt,
              lv_record_line TYPE char512,
              lv_record_read TYPE i,
              lv_eof         TYPE c,
              lwa_email_data TYPE ty_email_data.

* Open the Input File in Read Mode
  OPEN DATASET p_filbla FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.

    DO.

      CLEAR lv_record_line.

* Read File Contents
      READ DATASET p_filbla INTO lv_record_line.

      IF sy-subrc <> 0.
        lv_eof = gc_x.
      ELSE.
        lv_record_read = lv_record_read + 1.
      ENDIF.

      IF lv_eof <> gc_x.
        lwa_upflfrmt-filestrng = lv_record_line.
        APPEND lwa_upflfrmt TO git_upflfrmt.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
* Close the File
    CLOSE DATASET p_filbla.

  ELSE.

    MESSAGE s000(zfi01) WITH text-005.
    lwa_email_data-filename = p_filbla.
    lwa_email_data-log = text-014.
    APPEND lwa_email_data TO git_email_data.

  ENDIF.
ENDFORM.                    " F_UPLOAD_APPLICATION

*&---------------------------------------------------------------------*
*&      Form  f_send_email
*&---------------------------------------------------------------------*
*       Send Email with Run Details
*----------------------------------------------------------------------*
FORM f_send_email.

  DATA: lt_objtxt    TYPE TABLE OF solisti1,
        lt_objpack   TYPE TABLE OF sopcklsti1,
        lt_reclist   TYPE TABLE OF somlreci1,
        lt_objhead   TYPE soli_tab.

  DATA: lv_lines     TYPE i,
        lv_string    TYPE string,
        lwa_objpack  TYPE sopcklsti1,
        lwa_objtxt   TYPE solisti1,
        lwa_doc_chng TYPE sodocchgi1,
        lwa_reclist  TYPE somlreci1.

  CONSTANTS: lc_f(1)      TYPE c VALUE 'F',
             lc_u(1)      TYPE c VALUE 'U',
             lc_int(3)    TYPE c VALUE 'INT',
             lc_htm(3)    TYPE c VALUE 'HTM',
             lc_hyphen(1) TYPE c VALUE '-',
             lc_log(3)    TYPE c VALUE 'LOG'.

* Prepare Email Content
  PERFORM f_build_mail_content CHANGING lt_objtxt.

* Object with main text of the mail.
  lwa_objtxt = ''.
  APPEND lwa_objtxt TO lt_objtxt.
  CLEAR lwa_objtxt.

  DESCRIBE TABLE lt_objtxt LINES lv_lines.

  CONCATENATE text-006
              lc_hyphen
              lc_log
              INTO lv_string
              SEPARATED BY space.

  lwa_doc_chng-obj_descr  = lv_string.
  lwa_doc_chng-sensitivty = lc_f.
  lwa_doc_chng-doc_size   = lv_lines * 255.

* Pack to main body.
  lwa_objpack-head_start = 1.
  lwa_objpack-head_num   = 0.
  lwa_objpack-body_start = 1.
  lwa_objpack-body_num   = lv_lines.
  lwa_objpack-doc_type   = lc_htm.
  APPEND lwa_objpack TO lt_objpack.
  CLEAR lwa_objpack.

  lwa_reclist-copy = gc_x.

* Map Email ID(s)
  lwa_reclist-receiver   = p_errml.
  lwa_reclist-rec_type   = lc_u.
  lwa_reclist-com_type   = lc_int.
  lwa_reclist-notif_del  = gc_x.
  lwa_reclist-notif_ndel = gc_x.
  lwa_reclist-copy       = space.
  APPEND lwa_reclist TO lt_reclist.

* Funcion module for sending email.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = lwa_doc_chng
      put_in_outbox              = gc_x
      commit_work                = gc_x
    TABLES
      packing_list               = lt_objpack
      object_header              = lt_objhead
      contents_txt               = lt_objtxt
      receivers                  = lt_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc <> 0.
    MESSAGE i000(zfi01) WITH text-025.
  ELSE.

*Call program to push mail from SAP mail outbox
    SUBMIT rsconn01 WITH mode = lc_int
                    WITH output = space
                    AND RETURN.
  ENDIF.


ENDFORM.                    "f_send_email

*&---------------------------------------------------------------------*
*&      Form  f_build_mail_content
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJTXT  text
*----------------------------------------------------------------------*
FORM f_build_mail_content CHANGING p_lt_objtxt TYPE table.

  DATA: lwa_objtxt       TYPE solisti1,
        lwa_email_data   TYPE ty_email_data.

*Prepare HTML mail header
  CONCATENATE '<html>'
              '<body>'
              '<h4 style="font-family:arial"><caption><b><u>'
              text-007
              '</u></b><caption></h4>'
              '<ul>'
              '</ul>'
         INTO lwa_objtxt
         SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE '<table border="2" width="50%">'
              '<tr> <td>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE text-016
              '</td> <td>'
              text-018
              '</td> </tr>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE '<tr> <td>'
              text-008
              '</td> <td>'
              text-006
              '</td> </tr> </table>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE '<br> </br> <b> <u>'
              text-009
              '</u> </b>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  LOOP AT git_email_data INTO lwa_email_data.

* Prepare Table
    CONCATENATE '<br> <table border="2" width="50%">'
                '<tr> <td>'
                INTO lwa_objtxt
                SEPARATED BY space.

    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.

    CONCATENATE text-010
                '</td> <td>'
                lwa_email_data-filename
                '</td> </tr>'
                INTO lwa_objtxt
                SEPARATED BY space.

    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.

    CONCATENATE '<tr> <td>'
                text-011
                '</td> <td>'
                lwa_email_data-log
                '</td> </tr>'
                INTO lwa_objtxt
                SEPARATED BY space.

    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.

    AT LAST.

      MOVE: '</table> <br>' TO lwa_objtxt.
      APPEND: lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDAT.

  ENDLOOP.

*End of HTML text here
  CONCATENATE '</body>'
              '</html>'
         INTO lwa_objtxt
         SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

ENDFORM.                    "f_build_mail_content

*&---------------------------------------------------------------------*
*&      Form  F_SELECT_XPARAM
*&---------------------------------------------------------------------*
*      Get ZFIT_XPARAM Entries
*----------------------------------------------------------------------*
FORM f_select_xparam.

  SELECT *  FROM zfit_xparam
            INTO TABLE git_xparam
            WHERE paramtype = gc_paramtype AND
                  subtype   = gc_subtype.

  IF sy-subrc = 0.
    SORT git_xparam BY paramtype subtype key1.
  ENDIF.

ENDFORM.                    " F_SELECT_XPARAM


*&---------------------------------------------------------------------*
*&      Form  F_POPULATE_FILENAME
*&---------------------------------------------------------------------*
*       Prepare filename for application server
*----------------------------------------------------------------------*
FORM f_populate_filename.
  DATA : lwa_xparam   TYPE zfit_xparam.

  LOOP AT git_xparam INTO lwa_xparam WHERE paramtype = gc_paramtype AND
                                           subtype   = gc_subtype.
    CASE lwa_xparam-key1.
      WHEN gc_appflnm.
        p_filbla = lwa_xparam-value1.
      WHEN gc_dwna.
        p_fildwa = lwa_xparam-value1.
      WHEN gc_erremail.
        p_errml = lwa_xparam-value1.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " F_POPULATE_FILENAME

*&---------------------------------------------------------------------*
*&                      Form  F_F4_HELP_SEL_APP
*&---------------------------------------------------------------------*
*                   F4 Help for Application Server File
*----------------------------------------------------------------------*
FORM f_f4_help_sel_app.

  CONSTANTS: lc_app     TYPE dxfields-location VALUE 'A'.


  DATA : lv_path        TYPE dxfields-longpath,
         lwa_xparam     TYPE zfit_xparam.

*  Read the filepath From ZFIT_XPARAM
  READ TABLE git_xparam INTO lwa_xparam
             WITH KEY subtype = gc_subtype
                      key1    = gc_appflnm.

  lv_path = lwa_xparam-value1.

  CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
    EXPORTING
      i_location_flag = lc_app
      i_server        = space
      i_path          = lv_path
    IMPORTING
      o_path          = lv_path
    EXCEPTIONS
      rfc_error       = 1
      error_with_gui  = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.
    p_filbla = lv_path.
  ENDIF.

ENDFORM.                    "f_f4_help_selpath_app

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_FILEEMPTY_bl
*&---------------------------------------------------------------------*
*       Check if the filename is empty or not
*----------------------------------------------------------------------*
FORM f_check_fileempty_bl.

  IF p_applbl = gc_x.
    CHECK p_filbla IS INITIAL.
    MESSAGE e000(zfi01) WITH text-021.
    LEAVE LIST-PROCESSING.
  ELSE.
    CHECK p_filblp IS INITIAL.
    MESSAGE e000(zfi01) WITH text-022.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    " F_CHECK_FILEEMPTY
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_FPEMPTY_DW
*&---------------------------------------------------------------------*
*       Check if the download filepath is empty or not
*----------------------------------------------------------------------*
FORM f_check_fpempty_dw.

  IF p_appldw = gc_x.
    CHECK p_fildwa IS INITIAL.
    MESSAGE e000(zfi01) WITH text-023.
    LEAVE LIST-PROCESSING.
  ELSE.
    CHECK p_fildwp IS INITIAL.
    MESSAGE e000(zfi01) WITH text-024.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " f_check_fpempty_dw

*&---------------------------------------------------------------------*
*&      Form  F_ARCHIVE_FILE
*&---------------------------------------------------------------------*
*       File Archive
*----------------------------------------------------------------------*
FORM f_archive_file USING p_filename TYPE localfile
                          p_command  TYPE char01.

  CONSTANTS : lc_e        TYPE char01 VALUE 'E'.

  DATA: lwa_return        TYPE bapireturn,
        lv_arc_fpath      TYPE zparamvalue,
        lv_sourcefpath    TYPE zparamvalue,
        lv_source_fname   TYPE zparamvalue,
        lv_target_fname   TYPE zparamvalue,
        lwa_xparam        TYPE zfit_xparam,
        lv_filename       TYPE dsvasdocid.

* Split the filepath and filename
  lv_filename = p_filename.
  CALL FUNCTION 'DSVAS_DOC_FILENAME_SPLIT'
    EXPORTING
      pf_docid     = lv_filename
    IMPORTING
      pf_directory = lv_sourcefpath
      pf_filename  = lv_source_fname.

* Get the Target File Path for archiving
  READ TABLE git_xparam INTO lwa_xparam WITH KEY paramtype = gc_paramtype
                                                 subtype   = gc_subtype
                                                 key1      = gc_archive
                                                 BINARY SEARCH.
  IF sy-subrc = 0.
    lv_arc_fpath    = lwa_xparam-value1.
    lv_source_fname = lv_source_fname.
    lv_target_fname = lv_source_fname.
  ENDIF.

* Call the FM for Archiving.
  CALL FUNCTION 'ZFI_FILE_HANDLE'
    EXPORTING
      i_source_dir   = lv_sourcefpath
      i_target_dir   = lv_arc_fpath
      i_source_fname = lv_source_fname
      i_target_fname = lv_target_fname
      i_command      = p_command
    IMPORTING
      e_return       = lwa_return.

* Display Messages if the Archiving was successful/failed
  IF lwa_return-type = lc_e.
    WRITE:/ text-026.
  ELSE.
    WRITE:/ text-027.
  ENDIF.

  CLEAR: lv_source_fname,
         lv_target_fname,
         lwa_return.
ENDFORM.                    " F_ARCHIVE_FILE
*&---------------------------------------------------------------------*
*&                        Form  F_SPLITFILENAME
*&---------------------------------------------------------------------*
*                       Split Filename and Filepath
*----------------------------------------------------------------------*
FORM f_splitfilename  USING    p_p_filbla  TYPE LOCALFILE
                      CHANGING p_gv_filenm TYPE STRING.

  DATA : lv_fullfilename       TYPE dsvasdocid,
         lv_filename           TYPE dsvasdocid.

* Split the filepath and filename
  lv_fullfilename = p_p_filbla.
  CALL FUNCTION 'DSVAS_DOC_FILENAME_SPLIT'
    EXPORTING
      pf_docid     = lv_fullfilename
    IMPORTING
      pf_filename  = lv_filename.

  p_gv_filenm = lv_filename.

ENDFORM.                    " F_SPLITFILENAME
