*----------------------------------------------------------------------*
* Report Name: ZFAPI61_GUARDIAN_PAID_INV_CPY                           *
* Author:	     KBANERJEE-Kaushiki Banerjee                             *
* Date:	       January 8th,2019                                        *
* Logical Database: NA                                                 *
* SAPScript name:   NA                                                 *
* Application Area: FI                                                 *
* Description:  This program is a copy of ZFAPI61_GUARDIAN_PAID_INV    *
*               A new field "Year end adjustment is added".This field  *
*               is used to derive the records from PAYR for the        *
*               scenario in which there is a mismatch in fiscal year   *
*               between BSAK and PAYR.This will solve the issue with   *
*               the Guardian interface where the system is unable to   *
*               send payment confirmations for the documents with      *
*               payment date 01.01.2019.                               *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 04-DEC-2018  KBANERJEE    D30K929464 PRB0045534 -Initial Development *
*                           D30K929466                                 *
*                                                                      *
************************************************************************

REPORT  zfapi61_guardian_paid_inv_cpy.

TABLES: reguhm,
        bkpf,
        bsak.
TYPES: BEGIN OF ty_output,
       col1(50), "Batch id
       col2(25), "Processed Date
       "col3(25), "Tax Amount
       "col4(25), "Payment Amount
       col5(50), " Payment ID
       col6(25), "Cheque number
       col7(25), "Payment Document number
       col8(25), "Cheque Date
       col9(25), "Payment Amount
       col10(25), "Actual tax paid
       END OF ty_output.

DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR bsak-bukrs OBLIGATORY DEFAULT 'UGL',
                s_lifnr FOR reguhm-lifnr NO INTERVALS DEFAULT 'OTGUAR',
                s_xblnr FOR bsak-xblnr DEFAULT 'GUAR*' NO INTERVALS. " MODIF ID dsp.
PARAMETERS: p_augdt TYPE bsak-augdt DEFAULT sy-datum OBLIGATORY,
            p_blart TYPE bsak-blart DEFAULT 'GD' OBLIGATORY.
PARAMETERS: p_adyr  TYPE gjahr OBLIGATORY.
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

  s_xblnr-sign = 'I'.
  s_xblnr-option = 'CP'.
  s_xblnr-low = 'GUAR*'.
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
         gs_output.

  IF r_local IS NOT INITIAL.
    CONCATENATE p_lfile 'GUARDIAN_PAIDINV' '_' sy-datum '_' sy-uzeit
                '.TXT' INTO p_lfile.
  ELSE.
    CONCATENATE p_sfile 'GUARDIAN_PAIDINV' '_' sy-datum '_' sy-uzeit
                '.TXT' INTO p_sfile.
  ENDIF.
*  PERFORM print_header.
  PERFORM get_data.
  IF gt_output[] IS INITIAL.
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
  DATA:  lv_output(150),
         lv_crlf TYPE char2,
         lv_tab(1) TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.
  lv_crlf = cl_abap_char_utilities=>cr_lf.

  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open file for output.'.
    STOP.
  ELSE.
    LOOP AT gt_output INTO gs_output.

      CONCATENATE gs_output-col1
                  gs_output-col2
                  "gs_output-col3
                  "gs_output-col4
                  gs_output-col5
                  gs_output-col6
                  gs_output-col7
                  gs_output-col8
                  gs_output-col9
                  gs_output-col10
     INTO lv_output SEPARATED BY lv_tab.
      TRANSFER lv_output TO p_sfile.
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
*&      Form  PRINT_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_header .
  CLEAR gs_output.

  gs_output-col1 = 'Batch ID'.
  gs_output-col2 = 'Processed Date'.
  "gs_output-col3 = 'Tax Amount'.
  "gs_output-col4 = 'Payment Amount'.
  gs_output-col5 = 'Payment ID'.
  gs_output-col6 = 'Cheque Number'.
  gs_output-col7 = 'Payment Document Number'.
  gs_output-col8 = 'Cheque Date'.
  gs_output-col9 = 'Payment Amount'.
  gs_output-col10 = 'Tax Amount'.

  APPEND gs_output TO gt_output.

  CLEAR gs_output.

ENDFORM.                    " PRINT_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data.

  DATA: lv_append TYPE xfeld,
        lv_tabix TYPE sy-tabix,
        lv_payment TYPE payr-rwbtr,
        lv_docno TYPE i,
        lv_paymentid(18),
        lv_payee TYPE bsak-sgtxt,
        lv_wrbtr TYPE bseg-wrbtr,
        lt_bkpf TYPE TABLE OF bkpf,
        ls_bkpf TYPE bkpf,
        lt_bsak TYPE TABLE OF bsak,
        ls_bsak TYPE bsak,
        lt_payr TYPE TABLE OF payr,
        ls_payr TYPE payr,
        lv_gjahr TYPE bse_clr-gjahr,
        lt_bseg TYPE TABLE OF bseg,
        ls_bseg TYPE bseg,
        lt_bseg_v TYPE TABLE OF bseg,
        ls_bseg_v TYPE bseg.
  DATA:lv_adyr TYPE gjahr.
  CONSTANTS: lc_hkont TYPE bseg-hkont VALUE '0000256950'.

  lv_adyr = p_adyr.
  SELECT * FROM bsak INTO TABLE lt_bsak
    WHERE bukrs   IN s_bukrs
      AND lifnr   IN s_lifnr
      AND augdt = p_augdt
      AND blart = p_blart
      AND xblnr IN s_xblnr.

  IF lt_bsak[] IS INITIAL.
    WRITE : / 'No data to process (BSAK)..'.
    STOP.
  ENDIF.
  SELECT * FROM payr INTO TABLE lt_payr
    FOR ALL ENTRIES IN lt_bsak
    WHERE zbukr = lt_bsak-bukrs
      AND vblnr = lt_bsak-augbl
      AND gjahr = lv_adyr
      AND lifnr = lt_bsak-lifnr.
  IF lt_payr[] IS INITIAL.
    WRITE: / 'No Cheque data available for this selection criteria..'.
    STOP.
  ENDIF.
  SELECT * FROM bkpf INTO TABLE lt_bkpf
    FOR ALL ENTRIES IN lt_bsak
    WHERE bukrs = lt_bsak-bukrs
      AND belnr = lt_bsak-augbl
      AND gjahr = lv_adyr.
  SELECT * FROM bseg INTO TABLE lt_bseg
    FOR ALL ENTRIES IN lt_bsak
    WHERE bukrs = lt_bsak-bukrs
      AND belnr = lt_bsak-belnr
      AND gjahr = lt_bsak-gjahr.
  lt_bseg_v[] = lt_bseg[].
  DELETE lt_bseg_v WHERE bschl <> '31'
                     AND lifnr IS INITIAL.
  SORT lt_bseg_v BY bukrs belnr gjahr lifnr.
  SORT lt_payr BY zbukr vblnr gjahr lifnr.
  LOOP AT lt_bsak INTO ls_bsak.
    CLEAR: lv_append,
           lv_tabix,
           gs_output,
           ls_bkpf,
           ls_bseg,
           lv_wrbtr,
           lv_tabix.
    READ TABLE lt_bseg_v WITH KEY bukrs = ls_bsak-bukrs
                                  belnr = ls_bsak-belnr
                                  gjahr = ls_bsak-gjahr
                                  lifnr = ls_bsak-lifnr
                                  TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_bseg_v INTO ls_bseg_v FROM lv_tabix.
      IF ls_bseg_v-bukrs <> ls_bsak-bukrs OR
         ls_bseg_v-belnr <> ls_bsak-belnr OR
         ls_bseg_v-gjahr <> ls_bsak-gjahr OR
         ls_bseg_v-lifnr <> ls_bsak-lifnr.
        EXIT.
      ENDIF.
      IF ls_bseg_v-shkzg = 'H'. "credit
        lv_wrbtr = lv_wrbtr + ls_bseg_v-wrbtr.
      ELSE.
        lv_wrbtr = lv_wrbtr - ls_bseg_v-wrbtr.
      ENDIF.
    ENDLOOP.
    READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = ls_bsak-bukrs
                                             belnr = ls_bsak-augbl "belnr
                                             gjahr = lv_adyr.
    READ TABLE lt_bseg INTO ls_bseg WITH KEY bukrs = ls_bsak-bukrs
                                             belnr = ls_bsak-belnr
                                             gjahr = ls_bsak-gjahr
                                             hkont = lc_hkont.  "Tax GL
    IF ls_bseg-wrbtr < 0.
      ls_bseg-wrbtr = ls_bseg-wrbtr * -1.
    ENDIF.
    WRITE ls_bseg-wrbtr TO gs_output-col10 DECIMALS 2.
    REPLACE ALL OCCURRENCES OF ',' IN gs_output-col10 WITH space.
    CONDENSE gs_output-col10 NO-GAPS.
    gs_output-col1 = ls_bkpf-bktxt.
    SHIFT gs_output-col1 RIGHT DELETING TRAILING ' '.
    SHIFT gs_output-col1 LEFT DELETING LEADING ' '.
    gs_output-col2 = ls_bsak-bldat.
    "WRITE ls_bsak-wmwst TO gs_output-col3 DECIMALS 2.
    "WRITE ls_bsak-WRBTR TO gs_output-col4 DECIMALS 2.
    "gs_output-col5 = ls_bsak-sgtxt.
    CLEAR: lv_paymentid, lv_payee.
    SPLIT ls_bsak-sgtxt AT '-' INTO lv_paymentid lv_payee.
    gs_output-col5 = lv_paymentid.
    SHIFT gs_output-col5 RIGHT DELETING TRAILING ' '.
    SHIFT gs_output-col5 LEFT DELETING LEADING ' '.
    gs_output-col7 = ls_bsak-augbl.
    CLEAR lv_tabix.
    READ TABLE lt_payr WITH KEY zbukr = ls_bsak-bukrs
                                vblnr = ls_bsak-augbl
                                gjahr = lv_adyr
                                lifnr = ls_bsak-lifnr
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_payr INTO ls_payr FROM lv_tabix.
      IF ls_payr-zbukr <> ls_bsak-bukrs OR
         ls_payr-vblnr <> ls_bsak-augbl OR
         ls_payr-gjahr <> lv_adyr       OR
         ls_payr-lifnr <> ls_bsak-lifnr.
        EXIT.
      ENDIF.
      gs_output-col6 = ls_payr-chect.
      gs_output-col8 = ls_payr-zaldt.
      SHIFT gs_output-col6 RIGHT DELETING TRAILING ' '.
      SHIFT gs_output-col6 LEFT DELETING LEADING ' '.
*      IF ls_payr-rwbtr < 0.
*        ls_payr-rwbtr = ls_payr-rwbtr * -1.
*      ENDIF.
*      WRITE  ls_payr-rwbtr TO gs_output-col9 DECIMALS 2.
      "output invoice amount as cheque amount
      WRITE  lv_wrbtr TO gs_output-col9 DECIMALS 2.
      REPLACE ALL OCCURRENCES OF ',' IN gs_output-col9 WITH space.
      CONDENSE gs_output-col9 NO-GAPS.
      "-----------------------
      APPEND gs_output TO gt_output.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GET_DATA
