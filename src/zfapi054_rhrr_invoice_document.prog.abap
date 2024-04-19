*&---------------------------------------------------------------------*
*& Report  ZFAPI054_RHRR_INVOICE_DOCUMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapi054_rhrr_invoice_document.
TABLES: reguhm.
TYPES: BEGIN OF ty_output,
        account(7),
        comma(1),
        premise(7),
       END OF ty_output.
DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_zbukr  TYPE reguhm-zbukr DEFAULT 'UGL',
            p_laufdm TYPE reguhm-laufd_m DEFAULT sy-datum,
            p_rzawe  TYPE reguhm-rzawe DEFAULT 'O',
            p_lifnr  TYPE reguhm-lifnr DEFAULT 'OTRHRR'.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X'  USER-COMMAND cmd,
            p_sfile   LIKE        rfpdo-rfbifile MODIF ID srv,
            r_local   RADIOBUTTON GROUP rad1,
            p_lfile   TYPE        rfpdo-rfbifile DEFAULT 'H:\' MODIF ID lcl.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM  f_toggle_functionality.

START-OF-SELECTION.

  CLEAR: gt_output,
         gs_output.

  IF r_local IS NOT INITIAL.
    CONCATENATE p_lfile 'RHRR' '_' sy-datum '_' sy-uzeit '.CSV'  INTO p_lfile.
    "'.XLS'  INTO p_lfile.
  ELSE.
    CONCATENATE p_sfile 'RHRR' '_' sy-datum '_' sy-uzeit '.CSV' INTO p_sfile.
  ENDIF.
  PERFORM get_data.
*PERFORM dummy_data.
  IF gt_output[] IS INITIAL.
    WRITE : / 'no data to output.'.
  ELSE.
    PERFORM output_data.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: lv_koart TYPE koart VALUE 'K',
        lt_reguhm TYPE TABLE OF reguhm,
        ls_reguhm TYPE reguhm,
        lt_bse_clr TYPE TABLE OF bse_clr,
        lt_bseg TYPE TABLE OF bseg,
        ls_bseg TYPE bseg,
        lv_banner(10),
        lv_gjahr TYPE bse_clr-gjahr.

  lv_gjahr = p_laufdm(4).

  SELECT * FROM reguhm INTO TABLE lt_reguhm
    WHERE zbukr   = p_zbukr
      AND lifnr   = p_lifnr
      AND laufd_m = p_laufdm.
  "AND lifnr   = p_lifnr.
  IF lt_reguhm[] IS INITIAL.
    WRITE : / 'No data to process (REGUHM)..'.
    STOP.
  ENDIF.
  SELECT * FROM bse_clr INTO TABLE lt_bse_clr
    FOR ALL ENTRIES IN lt_reguhm
    WHERE bukrs_clr = lt_reguhm-zbukr
      AND belnr_clr = lt_reguhm-vblnr
      AND gjahr_clr = lv_gjahr.

*  BUKRS
*BELNR
*GJAHR
  IF lt_bse_clr[] IS INITIAL.
    WRITE : / 'No data to output (BSE_CLR)'.
    STOP.
  ENDIF.
  SELECT * FROM bseg INTO TABLE lt_bseg
    FOR ALL ENTRIES IN lt_bse_clr
    WHERE bukrs = lt_bse_clr-bukrs
      AND belnr = lt_bse_clr-belnr
      AND gjahr = lt_bse_clr-gjahr.
  LOOP AT lt_bseg INTO ls_bseg WHERE koart = lv_koart.
    CLEAR gs_output.
    SPLIT ls_bseg-sgtxt AT space INTO gs_output-account gs_output-premise lv_banner.
    gs_output-comma = ','.
    APPEND gs_output TO gt_output.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_data .
  IF r_server = 'X'.
    PERFORM download_to_server.
  ELSE.
    PERFORM download_to_pc.
  ENDIF.
ENDFORM.                    " OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_toggle_functionality .

  LOOP AT SCREEN.
* Set the screen fields to display only
    IF  screen-group1 EQ gc_modif_id_dsp.
      screen-input = 0.
    ENDIF.
    IF r_local = 'X'.
      IF screen-group1 = 'LCL'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'LCL'.
        screen-input = 0.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.
ENDFORM.                    " F_TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TO_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_to_server .
*Writing CDN file
  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open file for output.'.
  ELSE.
    LOOP AT gt_output INTO gs_output.
      TRANSFER gs_output TO p_sfile.
    ENDLOOP.
    CLOSE DATASET p_sfile.
    write: / 'File is downloaded', p_Sfile.
  ENDIF.
ENDFORM.                    " DOWNLOAD_TO_SERVER
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TO_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_to_pc .
  DATA: lv_filename TYPE string.
*Download Canadian file
  MOVE p_lfile TO lv_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
      write_field_separator     = 'X'
      trunc_trailing_blanks_eol = space "'X'
    CHANGING
      data_tab                  = gt_output
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
    WRITE: / 'Error with downloading file at PC ', sy-subrc.
  ELSE.
    WRITE: / 'Successfully created at ', p_lfile.
  ENDIF.
ENDFORM.                    " DOWNLOAD_TO_PC
*&---------------------------------------------------------------------*
*&      Form  DUMMY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dummy_data .
  gs_output-account = 'acct1'.
  gs_output-premise = 'prem1'.
  APPEND gs_output TO gt_output.
  gs_output-account = 'acct2'.
  gs_output-premise = 'prem2'.
  APPEND gs_output TO gt_output.
ENDFORM.                    " DUMMY_DATA
