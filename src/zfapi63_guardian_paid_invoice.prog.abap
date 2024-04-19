*&---------------------------------------------------------------------*
*& Report  ZFAPI61_GUARDIAN_PAID_INVOICE
*&
*&---------------------------------------------------------------------*
*&  Modification Log(Latest Version on Top)                            *
*&---------------------------------------------------------------------*
*& Version No    : 1.0                                                 *
*& Date          : 12-NOV-2019                                         *
*& Modified By   : Tawfeeq Ahmad (AHMADT)                              *
*& Correction No : D30K930282 CHG0165588                               *
*& Description   : Modified report to fetch data from tables BKPF and  *
*&                 BSEG using filtered data from PAYR rather than BSAK *
*&                 Added PAYR-PRIDT on selection screen.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 09-Dec-2019  JOOKONTR    D30K930326 CHG0168941 Changes for current   *
*                          Year cheques printing                       *
*                          D30K930328                                  *
*                          D30K930332                                  *
* 16-Jan-2020  JOOKONTR    D30K930332 CHG0171648   Change logic for the*
*                          ROWAMPS payment  confirmation Tax Issue     *
*                          D30K930419 CHG0171648   Change logic for the*
*                          ROWAMPS payment  confirmation Tax Issue     *
*                          tables Sequence changed to fetch accurate   *
*                          data                                        *
* 26-Mar-2021 KOTTAPAN    D30K930927 CHG0208608 Add logic to fetch     *
*                         data from REGUHM                             *
* 07-Jun-2021 KOTTAPAN    D30K931016 CHG0216879 Add logic to delete    *
*                         OTLAND vendors from REGUHM                   *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

REPORT  zfapi63_guardian_paid_invoice.

TABLES: reguhm,
        bkpf,
        bsak,
* Start of Changes by AHMADT for CHG0165588 D30K930282
        payr,
        bseg.
* End of Changes by AHMADT for CHG0165588 D30K930282
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
*& Begin of changes by JOOKONTR for CHG0171648
TYPES: BEGIN OF ty_payr,
        zbukr TYPE payr-zbukr,
        chect TYPE payr-chect,
        lifnr TYPE payr-lifnr,
        vblnr TYPE payr-vblnr,
        gjahr TYPE payr-gjahr,
        zaldt TYPE payr-zaldt,
        pridt TYPE payr-pridt,
       END OF ty_payr,
       BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         bktxt TYPE bkpf-bktxt,
       END OF ty_bkpf,
       BEGIN OF ty_bsak,
         bukrs TYPE bsak-bukrs,
         lifnr TYPE bsak-lifnr,
         augbl TYPE bsak-augbl,
         gjahr TYPE bsak-gjahr,
         belnr TYPE bsak-belnr,
         bldat TYPE bsak-bldat,
         xblnr TYPE bsak-xblnr,
         blart TYPE bsak-blart,
         sgtxt TYPE bsak-sgtxt,
       END OF ty_bsak,
       BEGIN OF ty_bseg,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         bschl TYPE bseg-bschl,
         shkzg TYPE bseg-shkzg,
         wrbtr TYPE bseg-wrbtr,
         hkont TYPE bseg-hkont,
         lifnr TYPE bseg-lifnr,
       END OF ty_bseg.

*& End of changes by JOOKONTR for CHG0171648

" Start of Add by KOTTAPAN for CHG0208608
TYPES : BEGIN OF ty_reguhm,
          zbukr TYPE reguhm-zbukr,
          lifnr TYPE reguhm-lifnr,
          vblnr TYPE reguhm-vblnr,
          zaldt TYPE reguhm-zaldt,
          laufd TYPE reguhm-laufd,
        END OF  ty_reguhm.
" End of Add by KOTTAPAN for CHG0208608

DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR bsak-bukrs OBLIGATORY DEFAULT 'UGL',
                s_lifnr FOR reguhm-lifnr NO INTERVALS DEFAULT 'OTGUAR',
                s_xblnr FOR bsak-xblnr DEFAULT 'GUAR*' NO INTERVALS. " MODIF ID dsp.
* Start of Changes by AHMADT for CHG0165588 D30K930282
SELECT-OPTIONS: "S_AUGDT FOR BSAK-AUGDT DEFAULT SY-DATUM OBLIGATORY, "Commented for CHG0171648 for revert changes
                s_pridt FOR payr-pridt. "Print Date
*                S_AUGCP FOR BSEG-AUGCP. "Clearing Entry Date  "Commented for CHG0171648 for revert changes
PARAMETERS:     p_blart TYPE bsak-blart DEFAULT 'GD' OBLIGATORY,
                p_gjahr TYPE gjahr.   "Added by JOOKONTR for CHG0171648
*PARAMETERS: p_augdt TYPE bsak-augdt DEFAULT sy-datum OBLIGATORY,
*            p_blart TYPE bsak-blart DEFAULT 'GD' OBLIGATORY.
* End of Changes by AHMADT for CHG0165588 D30K930282
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
  p_gjahr = sy-datum+0(4).
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
    CONCATENATE p_lfile 'LANDCHQPROD' "'_' sy-datum '_' sy-uzeit
                '.txt' INTO p_lfile.
  ELSE.
    CONCATENATE p_sfile  'LANDCHQPROD' "'GUARDIAN_PAIDINV' '_' sy-datum '_' sy-uzeit
                '.txt' INTO p_sfile.
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
*Start of code Commented by JOOKONTR for CHG0171648
*        LT_BKPF TYPE TABLE OF BKPF,
*        LS_BKPF TYPE BKPF,
*        LT_BSAK TYPE TABLE OF BSAK,
*        LS_BSAK TYPE BSAK,
*        LT_PAYR TYPE TABLE OF PAYR,
*        LS_PAYR TYPE PAYR,
*End of code Commented by JOOKONTR for CHG0171648
        lv_gjahr TYPE bse_clr-gjahr,
*Start of code Commented by JOOKONTR for CHG0171648
*        LT_BSEG TYPE TABLE OF BSEG,
*        LS_BSEG TYPE BSEG,
*        LT_BSEG_V TYPE TABLE OF BSEG,
*        LS_BSEG_V TYPE BSEG,
*End of code Commented by JOOKONTR for CHG0171648
*Start of code added by JOOKONTR for CHG0171648
        lt_bkpf TYPE TABLE OF ty_bkpf,
        ls_bkpf TYPE ty_bkpf,
        lt_bsak TYPE TABLE OF ty_bsak,
        ls_bsak TYPE ty_bsak,
        lt_payr TYPE TABLE OF ty_payr,
        ls_payr TYPE ty_payr,
        lt_bseg TYPE TABLE OF ty_bseg,
        ls_bseg TYPE ty_bseg,
        lt_bseg_v TYPE TABLE OF ty_bseg,
        ls_bseg_v TYPE ty_bseg.

*End of code added by JOOKONTR for CHG0171648

  DATA :  lt_reguhm TYPE TABLE OF ty_reguhm, " Added by KOTTAPAN for CHG0208608 on 26.03.21
          ls_reguhm TYPE ty_reguhm.

  CONSTANTS: lc_hkont  TYPE bseg-hkont VALUE '0000256950',
             lc_otland TYPE lifnr VALUE 'OTLAND'. " Added by KOTTAPAN for CHG0216879
*&--Begin of code commented by JOOKONTR for CHG0171648
*  SELECT * FROM BSAK INTO TABLE LT_BSAK
*    WHERE BUKRS   IN S_BUKRS
*      AND LIFNR   IN S_LIFNR
** Start of Changes by AHMADT for CHG0165588 D30K930282
*       AND AUGDT IN S_AUGDT
**      AND augdt = p_augdt
** End of Changes by AHMADT for CHG0165588 D30K930282
*      AND BLART = P_BLART
*      AND XBLNR IN S_XBLNR.
*
*  IF LT_BSAK[] IS INITIAL.
*    WRITE : / 'No data to process (BSAK)..'.
*    STOP.
*  ENDIF.
*  SELECT * FROM PAYR INTO TABLE LT_PAYR
*    FOR ALL ENTRIES IN LT_BSAK
*    WHERE ZBUKR = LT_BSAK-BUKRS
*      AND VBLNR = LT_BSAK-AUGBL
**      AND GJAHR = LT_BSAK-GJAHR  "Code commented by JOOKONTR for CHG0168941
**      AND  GJAHR = SY-DATUM+0(4)  "Code added by JOOKONTR for CHG0168941 commented for CHG0171648
*      AND  GJAHR = P_GJAHR  "Code added by JOOKONTR for CHG0171648
*      AND LIFNR = LT_BSAK-LIFNR
** Start of Changes by AHMADT for CHG0165588 D30K930282
*      AND PRIDT IN S_PRIDT.
** End of Changes by AHMADT for CHG0165588 D30K930282
*
*  IF LT_PAYR[] IS INITIAL.
*    WRITE: / 'No Cheque data available for this selection criteria..'.
*    STOP.
*  ENDIF.
** Start of Changes by AHMADT for CHG0165588 D30K930282
**  SELECT * FROM bkpf INTO TABLE lt_bkpf
**    FOR ALL ENTRIES IN lt_bsak
**    WHERE bukrs = lt_bsak-bukrs
**      AND belnr = lt_bsak-augbl
**      AND gjahr = lt_bsak-gjahr.
**  SELECT * FROM bseg INTO TABLE lt_bseg
**    FOR ALL ENTRIES IN lt_bsak
**    WHERE bukrs = lt_bsak-bukrs
**      AND belnr = lt_bsak-belnr
**      AND gjahr = lt_bsak-gjahr.
*  IF LT_PAYR IS NOT INITIAL.
*    SELECT * FROM BKPF INTO TABLE LT_BKPF
*      FOR ALL ENTRIES IN LT_PAYR
*      WHERE BUKRS = LT_PAYR-ZBUKR
*        AND BELNR = LT_PAYR-VBLNR
*        AND GJAHR = LT_PAYR-GJAHR.
*  ENDIF.
*  IF LT_BSAK IS NOT INITIAL.
*    SELECT * FROM BSEG INTO TABLE LT_BSEG
*    FOR ALL ENTRIES IN LT_BSAK
*    WHERE BUKRS = LT_BSAK-BUKRS
*      AND BELNR = LT_BSAK-BELNR
**      AND GJAHR = LT_BSAK-GJAHR   "Commented by JOOKONTR for CHG0168941
**      AND ( GJAHR = LT_BSAK-GJAHR OR GJAHR = SY-DATUM+0(4) ) "Code added by JOOKONTR for CHG0168941 commented for CHG0171648
*      AND ( GJAHR = LT_BSAK-GJAHR OR GJAHR = P_GJAHR ) "Code added by JOOKONTR for CHG0171648
*      AND AUGCP IN S_AUGCP.
*  ENDIF.
* End of Changes by AHMADT for CHG0165588 D30K930282
*&--End of code commented by JOOKONTR for CHG0171648

*&-Start of code added by JOOKONTR for CHG0171648
  SELECT zbukr
         chect
         lifnr
         vblnr
         gjahr
         zaldt
         pridt FROM payr
    INTO TABLE lt_payr
    WHERE zbukr IN s_bukrs
    AND lifnr IN s_lifnr
    AND gjahr EQ p_gjahr
    AND pridt IN s_pridt.
*  IF sy-subrc EQ 0. " Commented by KOTTAPAN for CHG0208608

  " Start of Added by KOTTAPAN for CHG0208608 on 26.03.21
  SELECT zbukr
         lifnr
         vblnr
         zaldt
         laufd
    FROM reguhm
  INTO TABLE lt_reguhm
  WHERE laufd IN s_pridt.
  IF sy-subrc = 0.
    DELETE lt_reguhm WHERE lifnr = lc_otland. " Added by KOTTAPAN for CHG0216879
    SORT lt_reguhm BY vblnr.
    DELETE ADJACENT DUPLICATES FROM lt_reguhm COMPARING vblnr.
  ENDIF.

  LOOP AT lt_reguhm INTO ls_reguhm.
    ls_payr-zbukr = ls_reguhm-zbukr.
    ls_payr-lifnr = ls_reguhm-lifnr.
    ls_payr-vblnr = ls_reguhm-vblnr.
    ls_payr-gjahr = ls_reguhm-laufd+0(4).
    ls_payr-zaldt = ls_reguhm-zaldt.
    ls_payr-pridt = ls_reguhm-laufd.
    APPEND ls_payr TO lt_payr.
    CLEAR ls_payr.
  ENDLOOP.

  SORT lt_payr BY lifnr vblnr.
  DELETE ADJACENT DUPLICATES FROM lt_payr COMPARING lifnr vblnr.
  " End of Add by KOTTAPAN for CHG0208608on 26.03.21

  IF lt_payr IS NOT INITIAL. " Added by KOTTAPAN for CHG0208608

    SELECT bukrs
           belnr
           gjahr
           bktxt FROM bkpf
      INTO TABLE lt_bkpf
     FOR ALL ENTRIES IN lt_payr
      WHERE bukrs EQ lt_payr-zbukr
      AND belnr EQ lt_payr-vblnr
      AND gjahr EQ lt_payr-gjahr.
    IF sy-subrc EQ 0.
      SORT lt_bkpf BY bukrs belnr gjahr.
    ENDIF.
    SELECT bukrs
           lifnr
           augbl
           gjahr
           belnr
           bldat
           xblnr
           blart
           sgtxt FROM bsak
      INTO TABLE lt_bsak
      FOR ALL ENTRIES IN lt_payr
      WHERE bukrs EQ lt_payr-zbukr
      AND augbl EQ lt_payr-vblnr
      AND lifnr IN s_lifnr
      AND xblnr IN s_xblnr
      AND blart EQ p_blart.
    IF sy-subrc EQ 0.
      SORT lt_bsak BY bukrs lifnr augbl gjahr belnr.
      SELECT bukrs
             belnr
             gjahr
             bschl
             shkzg
             wrbtr
             hkont
             lifnr FROM bseg
        INTO TABLE lt_bseg
        FOR ALL ENTRIES IN lt_bsak
        WHERE bukrs EQ lt_bsak-bukrs
        AND belnr EQ lt_bsak-belnr
        AND gjahr EQ lt_bsak-gjahr.
      IF sy-subrc EQ 0.
        SORT lt_bseg BY bukrs belnr gjahr hkont.
      ENDIF.
    ELSE.
      WRITE : / text-003.
      STOP.
    ENDIF.
  ELSE.
    WRITE: / text-004.
    STOP.
  ENDIF.
*&-End of code added by JOOKONTR for CHG0171648
  lt_bseg_v[] = lt_bseg[].
  DELETE lt_bseg_v WHERE bschl <> '31'
                     AND lifnr IS INITIAL.

  SORT lt_bseg_v BY bukrs belnr gjahr lifnr.
  SORT lt_payr BY zbukr vblnr gjahr lifnr.
  SORT lt_bkpf BY bukrs belnr gjahr.
  LOOP AT lt_bsak INTO ls_bsak.   "Added by JOOKONTR for CHG0165588 D30K930282
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
*&--Start of code added by JOOKONTR for CHG0165588 D30K930282
    CLEAR ls_payr.
    READ TABLE lt_payr INTO ls_payr WITH KEY zbukr = ls_bsak-bukrs
                                             vblnr = ls_bsak-augbl
*                                            GJAHR = LS_BSAK-GJAHR   "Commented by JOOKONTR for CHG0168941
*                                            GJAHR = SY-DATUM+0(4)   "Code added by JOOKONTR for CHG0168941 Commented for CHG0171648
                                             gjahr = p_gjahr         "Code added by JOOKONTR for CHG0171648
                                             lifnr = ls_bsak-lifnr
                                             BINARY SEARCH.          "Code added by JOOKONTR for CHG0171648
    IF sy-subrc EQ 0.
      READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = ls_payr-zbukr
                                          belnr = ls_payr-vblnr "belnr
                                          gjahr = ls_payr-gjahr
**--strat of changes by JOOKONTR FOR  CHG0171648
                                          BINARY SEARCH.
**--END of changes by JOOKONTR FOR  CHG0171648
      IF sy-subrc EQ 0.
        gs_output-col1 = ls_bkpf-bktxt.
      ENDIF.
    ENDIF.
    SHIFT gs_output-col1 RIGHT DELETING TRAILING ' '.
    SHIFT gs_output-col1 LEFT DELETING LEADING ' '.
*    READ TABLE LT_BKPF INTO LS_BKPF WITH KEY BUKRS = LS_BSAK-BUKRS
*                                             BELNR = LS_BSAK-AUGBL "belnr
*                                             GJAHR = LS_BSAK-GJAHR.
*&--End of code added by JOOKONTR for CHG0165588 D30K930282

    READ TABLE lt_bseg INTO ls_bseg WITH KEY bukrs = ls_bsak-bukrs
                                             belnr = ls_bsak-belnr
                                             gjahr = ls_bsak-gjahr
                                             hkont = lc_hkont
                                             BINARY SEARCH.  "Tax GL "Code added by JOOKONTR for CHG0171648
    IF ls_bseg-wrbtr < 0.
      ls_bseg-wrbtr = ls_bseg-wrbtr * -1.
    ENDIF.
    WRITE ls_bseg-wrbtr TO gs_output-col10 DECIMALS 2.
    REPLACE ALL OCCURRENCES OF ',' IN gs_output-col10 WITH space.
    CONDENSE gs_output-col10 NO-GAPS.
*Start of Code commented by JOOKONTR for CHG0165588 D30K930282
*    GS_OUTPUT-COL1 = LS_BKPF-BKTXT.
*    SHIFT GS_OUTPUT-COL1 RIGHT DELETING TRAILING ' '.
*    SHIFT GS_OUTPUT-COL1 LEFT DELETING LEADING ' '.
*End of code commented by JOOKONTR for CHG0165588 D30K930282
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
*                                GJAHR = LS_BSAK-GJAHR  "Commented by JOOKONTR for CHG0168941
*                                GJAHR = SY-DATUM+0(4)   "Code added by JOOKONTR for CHG0168941 " Commented for CHG0171648
                                gjahr = p_gjahr         "Code added by JOOKONTR for CHG0171648
                                lifnr = ls_bsak-lifnr
                                TRANSPORTING NO FIELDS
                                BINARY SEARCH.          "Code added by JOOKONTR for CHG0171648
    lv_tabix = sy-tabix.
    LOOP AT lt_payr INTO ls_payr FROM lv_tabix.
      IF ls_payr-zbukr <> ls_bsak-bukrs OR
         ls_payr-vblnr <> ls_bsak-augbl OR
*         LS_PAYR-GJAHR <> LS_BSAK-GJAHR OR   "Commented by JOOKONTR for CHG0168941
*         LS_PAYR-GJAHR <> SY-DATUM+0(4) OR    "Code added by JOOKONTR for CHG0168941 "Commented for CHG0171648
         ls_payr-gjahr <> p_gjahr OR    "Code added by JOOKONTR for CHG0171648
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
      WRITE  lv_wrbtr TO gs_output-col9 DECIMALS 2.
      REPLACE ALL OCCURRENCES OF ',' IN gs_output-col9 WITH space.
      CONDENSE gs_output-col9 NO-GAPS.
      "-----------------------
      APPEND gs_output TO gt_output.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GET_DATA
