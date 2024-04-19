*&---------------------------------------------------------------------*
*& Report  ZFARC001_ST_CURRENCY_SPLT
*&---------------------------------------------------------------------*
* AUTHOR:      Sajjad Ahmad                                            *
* DATE:        September 2011.                                         *
* PROJECT:     FI Receiable                                            *
* ISSUE LOG:   TR804                                                   *
* DESCRIPTION:                                                         *
* This program will receive the Accounting file from Contrax S & T and *
* split into CAD/USD files.                                            *
*&---------------------------------------------------------------------*
REPORT  zfarc001_st_currency_splt.

DATA: BEGIN OF typ_output,
        item_type(10),
        applied_year(4),
        applied_month(2),
        pty_id(8),
        rate_class(12),
        st_code(6),
        sc_code(2),
        seasonal_class(4),
        rate_type(4),
        charge_type(4),
        sr_usage(4),
        st_sub_type(6),
        non_rate_item_type(8),
        cycled_ind(1),
        tier_step_level(2),
        amount(17),
        amount_uom(8),
        volume(15),
        volume_uom(8),
        item_class(8),
        exchange_rate(20),
        ssegid(10),
        srsubtype(10),
        srusersup(10),
        contrref(15),
        shrtlnterm(10),
    END OF typ_output.

DATA:BEGIN OF gt_tab OCCURS 0,         "Text file format
           text1(208),
     END OF gt_tab.
DATA: BEGIN OF gt_msg OCCURS 0,
           text1(100),
      END OF gt_msg.

DATA: gt_cad LIKE TABLE OF typ_output,
      wa_cad LIKE typ_output,
      gt_usd LIKE TABLE OF typ_output,
      wa_usd LIKE typ_output,
      gt_output LIKE TABLE OF typ_output,
      wa_output LIKE typ_output.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: r_server RADIOBUTTON GROUP rad2,
            r_pc RADIOBUTTON GROUP rad2.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS: p_file TYPE rfpdo-lboxfile DEFAULT '/usr/sap/interfaces/d22/drco0078/zcontraxstacc.dat' OBLIGATORY,
            p_cadf TYPE rfpdo-lboxfile DEFAULT '/usr/sap/interfaces/d22/drco0078/zcontraxstacccad.dat' OBLIGATORY,
            p_usdf TYPE rfpdo-lboxfile DEFAULT '/usr/sap/interfaces/d22/drco0078/zcontraxstaccusd.dat' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.
***************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',DAT File,*.dat'
      static    = 'X'
    CHANGING
      file_name = p_file.
*****************************Start of Selection
START-OF-SELECTION.
*Uploading of text file from Application server.
  CLEAR: gt_msg,
         gt_tab,
         gt_output.
  IF r_server = 'X'. "application server
    PERFORM uploading_text_file.
    IF gt_msg[] IS INITIAL AND  "internal table for error message
       gt_output[] IS NOT INITIAL. " internal table for data
      PERFORM process_input_file.
*Downloading of text file to Application server.
      PERFORM downloading_text_file.
    ELSE.
      PERFORM error_messages.
    ENDIF.
  ELSE.  "Presentation Server
    PERFORM upload_from_pc.
    IF gt_msg[] IS INITIAL AND
       gt_output IS NOT INITIAL.
      PERFORM process_input_file.
      PERFORM pc_download_text_file.
    ELSE.
      PERFORM error_messages.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  uploading_text_file
*&---------------------------------------------------------------------*
*      Uploading text file from application server
*----------------------------------------------------------------------*
FORM uploading_text_file .

  DATA: fl_string TYPE string.

  OPEN DATASET p_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    gt_msg-text1 = 'Can not open text file for process, Please check text file, security or path'.
    APPEND gt_msg.
  ENDIF.
  DO.

    CLEAR: wa_output,
           gt_tab.
    READ DATASET p_file INTO gt_tab-text1. "fl_string.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    PERFORM split_data USING gt_tab
                             wa_output.
*    SPLIT fl_string AT cl_abap_char_utilities=>horizontal_tab
*    INTO wa_output-item_type
*        wa_output-applied_year
*        wa_output-applied_month
*        wa_output-pty_id
*        wa_output-rate_class
*        wa_output-st_code
*        wa_output-sc_code
*        wa_output-seasonal_class
*        wa_output-rate_type
*        wa_output-charge_type
*        wa_output-sr_usage
*        wa_output-st_sub_type
*        wa_output-non_rate_item_type
*        wa_output-cycled_ind
*        wa_output-tier_step_level
*        wa_output-amount
*        wa_output-amount_uom
*        wa_output-volume
*        wa_output-volume_uom
*        wa_output-item_class
*        wa_output-exchange_rate
*        wa_output-ssegid
*        wa_output-srsubtype
*        wa_output-srusersup
*        wa_output-contrref
*        wa_output-shrtlnterm.

    APPEND wa_output TO gt_output.
  ENDDO.
  CLOSE DATASET p_file.
ENDFORM.                    " uploading_text_file
*&---------------------------------------------------------------------*
*&      Form  DOWNLOADING_TEXT_FILE
*&---------------------------------------------------------------------*
*       download text file to application server
*----------------------------------------------------------------------*
FORM downloading_text_file .
*Writing CDN file
  OPEN DATASET p_cadf FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open $CDN text file for output.'.
  ELSE.
    LOOP AT gt_cad INTO wa_cad.
      TRANSFER wa_cad TO p_cadf.
    ENDLOOP.
    CLOSE DATASET p_cadf.
  ENDIF.
*****Writing USD file
  OPEN DATASET p_usdf FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open $USD text file for output.'.
  ELSE.
    LOOP AT gt_usd INTO wa_usd.
      TRANSFER wa_usd TO p_cadf.
    ENDLOOP.
    CLOSE DATASET p_cadf.
  ENDIF.
ENDFORM.                    " DOWNLOADING_TEXT_FILE
*&---------------------------------------------------------------------*
*&      Form  ERROR_MESSAGES;
*&---------------------------------------------------------------------*
*       Display Error Messages
*----------------------------------------------------------------------*
FORM error_messages.
  IF gt_tab[] IS INITIAL.
    WRITE: / 'No lines for processing.'..
  ENDIF.
  IF gt_msg[] IS NOT INITIAL.
    WRITE: / 'Following errors occured during processing.'.
    LOOP AT gt_msg.
      WRITE: / '->', gt_msg-text1.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " ERROR_MESSAGES;
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FROM_PC
*&---------------------------------------------------------------------*
*      Upload text file from PC
*----------------------------------------------------------------------*
FORM upload_from_pc .
  DATA:   lt_auszug TYPE STANDARD TABLE OF string.
  DATA:   lv_auszug_file TYPE string.

  REFRESH lt_auszug[].
  lv_auszug_file = p_file.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename        = lv_auszug_file
*      HAS_FIELD_SEPARATOR = 'X'
      filetype        = 'ASC'
    CHANGING
      data_tab        = lt_auszug
    EXCEPTIONS
      file_open_error = 1
      file_read_error = 2
      OTHERS          = 18.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE e503(fv) WITH p_file+2 p_file+0(2) INTO gt_msg-text1.
      APPEND gt_msg.
    WHEN 2.
      MESSAGE e503(fv) WITH p_file+2 p_file+0(2) INTO gt_msg-text1.
      APPEND gt_msg.
    WHEN OTHERS.
  ENDCASE.
  gt_tab[] = lt_auszug[].
*text file is tab delimited, following routine reads it.
  PERFORM convert_data.
ENDFORM.                    " UPLOAD_FROM_PC
*&---------------------------------------------------------------------*
*&      Form  PC_DOWNLOAD_TEXT_FILE
*&---------------------------------------------------------------------*
*      Download at PC
*----------------------------------------------------------------------*
FORM pc_download_text_file .
  DATA: lv_filename TYPE string.
*Download Canadian file
  MOVE p_cadf TO lv_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
      trunc_trailing_blanks_eol = space "'X'
    CHANGING
      data_tab                  = gt_cad
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
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24.
  IF sy-subrc <> 0.
    WRITE: / 'Error with downloading $CDN text file at PC ', sy-subrc.
  ELSE.
    WRITE: / '$CDN successfully created at ', p_cadf.
  ENDIF.
*Download US file
  MOVE p_usdf TO lv_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
      trunc_trailing_blanks_eol = space "'X'
    CHANGING
      data_tab                  = gt_usd
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
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24.
  IF sy-subrc <> 0.
    WRITE: / 'Error with downloading $USD text file at PC ', sy-subrc.
  ELSE.
    WRITE: / '$USD successfully created at ', p_usdf.
  ENDIF.

ENDFORM.                    " PC_DOWNLOAD_TEXT_FILE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_INPUT_FILE
*&---------------------------------------------------------------------*
*       Process input file and create internal table for output file
*----------------------------------------------------------------------*
FORM process_input_file .
  DATA: fl_amount_uom(8).

  LOOP AT gt_output INTO wa_output.
    fl_amount_uom = wa_output-amount_uom.
    CONDENSE fl_amount_uom NO-GAPS.
    IF fl_amount_uom = '$CDN'.
      wa_cad = wa_output.
      APPEND wa_cad TO gt_cad.
    ELSEIF fl_amount_uom = '$US'.
      wa_usd = wa_output.
      APPEND wa_usd TO gt_usd.
    ENDIF.
    CLEAR: wa_cad,
           wa_usd.
  ENDLOOP.
ENDFORM.                    " PROCESS_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATA
*&---------------------------------------------------------------------*
*       Use to split tab delimited data
*----------------------------------------------------------------------*
FORM convert_data .
  LOOP AT gt_tab.
    CLEAR: wa_output.
    PERFORM split_data USING gt_tab
                           wa_output.
    APPEND wa_output TO gt_output.

  ENDLOOP.
ENDFORM.                    " CONVERT_DATA
*&---------------------------------------------------------------------*
*&      Form  SPLIT_DATA
*&---------------------------------------------------------------------*
*       Split TAB Delimited data
*----------------------------------------------------------------------*
FORM split_data  USING    p_gt_tab STRUCTURE gt_tab
                          p_output STRUCTURE typ_output.
  SPLIT p_gt_tab AT cl_abap_char_utilities=>horizontal_tab
      INTO p_output-item_type
          p_output-applied_year
          p_output-applied_month
          p_output-pty_id
          p_output-rate_class
          p_output-st_code
          p_output-sc_code
          p_output-seasonal_class
          p_output-rate_type
          p_output-charge_type
          p_output-sr_usage
          p_output-st_sub_type
          p_output-non_rate_item_type
          p_output-cycled_ind
          p_output-tier_step_level
          p_output-amount
          p_output-amount_uom
          p_output-volume
          p_output-volume_uom
          p_output-item_class
          p_output-exchange_rate
          p_output-ssegid
          p_output-srsubtype
          p_output-srusersup
          p_output-contrref
          p_output-shrtlnterm.

ENDFORM.                    " SPLIT_DATA
