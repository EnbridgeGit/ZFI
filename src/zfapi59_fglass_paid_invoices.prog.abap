*&---------------------------------------------------------------------*
*& Report  ZFAPI59_FGLASS_PAID_INVOICES
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 03-Dec-2018  JRHARTUNG   D30K929327  CHG0131446  Include Reference   *
*                                      Documents that start with ENB*  *
*&---------------------------------------------------------------------*

REPORT  zfapi59_fglass_paid_invoices.

TABLES: reguhm,
        bkpf,
        bsak.
TYPES: BEGIN OF ty_output,
       invoice_id TYPE string, "(16),
       sp1(1),
       paid_date TYPE string, "(8),
       sp2(1),
       payment_ref TYPE string, "(10),
       sp3(1),
       paid_amount TYPE string, "(13),
       sp4(1),
       pay_doc_cc TYPE string, "(4),
       sp5(1),
       pay_doc_year TYPE string, "(4),
       sp6(1),
       inv_doc TYPE string, "(24),
       sp7(1),
       ecc_id TYPE string, "(2),
       END OF ty_output.
DATA: gt_output TYPE TABLE OF ty_output,
      gt_output1 TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR bsak-bukrs DEFAULT '01'.
PARAMETERS: "p_zbukr  TYPE reguhm-zbukr DEFAULT '01',
            "p_laufdm TYPE reguhm-laufd_m DEFAULT sy-datum,
            p_augdt TYPE bsak-augdt,
            p_eccid TYPE char02 DEFAULT 'CE'.
SELECT-OPTIONS: s_lifnr  FOR reguhm-lifnr NO INTERVALS OBLIGATORY,
            s_laufi FOR reguhm-laufi NO-DISPLAY,
*           s_xblnr FOR bkpf-xblnr NO-DISPLAY.
            s_xblnr FOR bkpf-xblnr.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND cmd,
            p_sfile   LIKE  rfpdo-rfbifile MODIF ID srv,
            r_local   RADIOBUTTON GROUP rad1,
            p_lfile   TYPE  rfpdo-rfbifile DEFAULT 'H:\' MODIF ID lcl.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM  f_toggle_functionality.

INITIALIZATION.
  s_laufi-sign = 'I'.
  s_laufi-option = 'CP'.
  s_laufi-low = '0*'.
  APPEND s_laufi.

  s_xblnr-sign = 'I'.
  s_xblnr-option = 'CP'.
  s_xblnr-low = 'SPEN*'.
  APPEND s_xblnr.

  s_xblnr-sign = 'I'.
  s_xblnr-option = 'CP'.
  s_xblnr-low = 'ENB*'.
  APPEND s_xblnr.

START-OF-SELECTION.

  IF r_server IS NOT INITIAL AND
     p_sfile IS INITIAL.
    WRITE : / 'Enter Application Server Path..'.
    STOP.
  ENDIF.
  IF r_local IS NOT INITIAL AND
     p_lfile IS INITIAL.
    WRITE : / 'Enter Local PC Path..'.
    STOP.
  ENDIF.
  CLEAR: gt_output,
         gs_output,
         gt_output1.

  IF r_local IS NOT INITIAL.
    CONCATENATE p_lfile 'FG_PAIDINV' '_' p_eccid '_' sy-datum '_' sy-uzeit
                '.TXT' INTO p_lfile.
  ELSE.
    CONCATENATE p_sfile 'FG_PAIDINV' '_' p_eccid '_' sy-datum '_' sy-uzeit
                '.TXT' INTO p_sfile.
  ENDIF.
  PERFORM print_header.
  PERFORM get_data_new.
*PERFORM dummy_data.
  IF gt_output1[] IS INITIAL.
    WRITE : / 'no data to output.'.
  ELSE.
    PERFORM output_data.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_data .

  APPEND LINES OF gt_output1 TO gt_output.

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
  DATA:  lv_string TYPE string,
         lv_crlf TYPE char2.

  lv_crlf = CL_ABAP_CHAR_UTILITIES=>CR_LF.
*   lv_crlf = CL_ABAP_CHAR_UTILITIES=>NEWLINE.

  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open file for output.'.
    STOP.
  ELSE.

    clear lv_crlf.
    LOOP AT gt_output INTO gs_output.
      CONCATENATE gs_output-invoice_id
                  gs_output-sp1
                  gs_output-paid_date
                  gs_output-sp2
                  gs_output-payment_ref
                  gs_output-sp3
                  gs_output-paid_amount
                  gs_output-sp4
                  gs_output-pay_doc_cc
                  gs_output-sp5
                  gs_output-pay_doc_year
                  gs_output-sp6
                  gs_output-inv_doc
                  gs_output-sp7
                  gs_output-ecc_id
                  lv_crlf
     INTO lv_string.
      "SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
      TRANSFER lv_string TO p_sfile.
*      TRANSFER gs_output TO p_sfile.
    ENDLOOP.
    CLOSE DATASET p_sfile.
    WRITE: / 'File is downloaded', p_sfile.
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
      write_field_separator     = space "'X'
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
*&      Form  PRINT_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_header .
  CLEAR gs_output.
  gs_output-invoice_id = 'Type=Paid Invoice Upload'.
  APPEND gs_output TO gt_output.
  gs_output-invoice_id = 'Transaction=False'.
  APPEND gs_output TO gt_output.
  gs_output-invoice_id = 'Date Format=YYYY-MM-DD'.
  APPEND gs_output TO gt_output.
  gs_output-invoice_id = space.
  APPEND gs_output TO gt_output.
  gs_output-sp1 = '~'.
  gs_output-sp2 = '~'.
  gs_output-sp3 = '~'.
  gs_output-sp4 = '~'.
  gs_output-sp5 = '~'.
  gs_output-sp6 = '~'.
  gs_output-sp7 = '~'.
  gs_output-invoice_id = 'Invoice ID'.
  gs_output-paid_date  = 'Date paid'.
  gs_output-payment_ref = 'Check Number'.
  gs_output-paid_amount = 'Amount'.
  gs_output-pay_doc_cc  = '[c] Payment Doc CompCd'.
  gs_output-pay_doc_year = '[c] Payment Doc FisYr'.
  gs_output-inv_doc = '[c] Invoice Doc Number'.
  gs_output-ecc_id =  '[c] ECC'.
  APPEND gs_output TO gt_output.
  CLEAR gs_output.

ENDFORM.                    " PRINT_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA_NEW .
  DATA: lv_append TYPE xfeld,
        ls_bkpf TYPE bkpf,
        lt_bsak TYPE TABLE OF bsak,
        ls_bsak TYPE bsak,
        lv_gjahr TYPE bse_clr-gjahr.

  SELECT * FROM bsak INTO TABLE lt_bsak
    WHERE bukrs   in s_bukrs
      AND lifnr   IN s_lifnr
      AND augdt = p_augdt "laufdm
      AND xblnr in s_xblnr.

  IF lt_bsak[] IS INITIAL.
    WRITE : / 'No data to process (BSAK)..'.
    STOP.
  ENDIF.
  LOOP AT lt_bsak INTO ls_bsak.
    gs_output-sp1 = '~'.
    gs_output-sp2 = '~'.
    gs_output-sp3 = '~'.
    gs_output-sp4 = '~'.
    gs_output-sp5 = '~'.
    gs_output-sp6 = '~'.
    gs_output-sp7 = '~'.
    "gs_output-paid_date  = ls_reguhm-zaldt.
    CONCATENATE ls_bsak-augdt(4) '-' ls_bsak-augdt+4(2) '-'
                ls_bsak-augdt+6(2) INTO gs_output-paid_date.
    gs_output-ecc_id = p_eccid.
    lv_gjahr = ls_bsak-augdt(4).
    CONDENSE ls_bsak-xblnr.
    CONDENSE ls_bsak-belnr.
    CONDENSE ls_bsak-bukrs.
    CONDENSE ls_bsak-gjahr.
    gs_output-invoice_id = ls_bsak-xblnr.
    gs_output-payment_ref = ls_bsak-augbl.
    gs_output-paid_amount = ls_bsak-wrbtr.
    gs_output-pay_doc_cc  = ls_bsak-bukrs.
    gs_output-pay_doc_year = ls_bsak-gjahr.
    CONDENSE gs_output-paid_amount.
    CONCATENATE ls_bsak-bukrs '|' ls_bsak-belnr '|'
                ls_bsak-gjahr INTO gs_output-inv_doc
                SEPARATED BY space.
    APPEND gs_output TO gt_output1.

  ENDLOOP.
  SORT gt_output1 BY payment_ref invoice_id.
ENDFORM.                    " GET_DATA_NEW
