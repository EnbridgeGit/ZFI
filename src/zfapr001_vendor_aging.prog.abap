*&---------------------------------------------------------------------*
*& Report  ZFAPR001_VENDOR_AGING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapr001_vendor_aging.

INCLUDE zlapretop.

*&---------------------------------------------------------------------*
*&       Class LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*       Double Click Event Handler
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
                                 IMPORTING e_row e_column es_row_no,
             handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
                                 IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
"LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_handler
*&---------------------------------------------------------------------*
*        Double Click Event Handler Implementation
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row e_column
                                      es_row_no.
  ENDMETHOD.                    "handle_double_click
  METHOD  handle_hotspot_click.
    PERFORM  handle_double_click USING e_row_id e_column_id
                                       es_row_no.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.               "lcl_event_handler
TYPES: BEGIN OF typ_output,
           bukrs      TYPE  bukrs,
           lifnr      TYPE  lifnr,
           blart      TYPE  blart,
           zxblnr     TYPE  xblnr1,
           gjahr      TYPE gjahr,
           belnr      TYPE  belnr_d,
           zbudat     TYPE  budat,
           zshkzg     TYPE  shkzg,
           dmbtr      TYPE  dmbtr,
           wrbtr      TYPE  wrbtr,
           a0_30      TYPE  rr_dmshb,
           a31_60     TYPE rr_dmshb,
           a61_90     TYPE rr_dmshb,
           a91_120    TYPE rr_dmshb,
           a121_150   TYPE rr_dmshb,
           a151_180   TYPE rr_dmshb,
           a181_210   TYPE rr_dmshb,
           a211_above TYPE rr_dmshb,
       END OF typ_output.

DATA: callback      LIKE ldbcb    OCCURS 0 WITH HEADER LINE,
      seltab    LIKE rsparams OCCURS 0 WITH HEADER LINE.

DATA: it_output TYPE TABLE OF typ_output,
      wa_output LIKE LINE OF it_output,
      it_bsikext TYPE TABLE OF bsikext,
      wa_bsikext LIKE LINE OF it_bsikext.
DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE ok_code.
DATA: gr_container TYPE REF TO cl_gui_custom_container,
      gr_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_layout    TYPE lvc_s_layo.
DATA: gr_event_handler TYPE REF TO lcl_event_handler.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: kd_bukrs  FOR  bkpf-bukrs OBLIGATORY,
                kd_lifnr  FOR  lfa1-lifnr, " OBLIGATORY,
                kd_akont  FOR  lfb1-akont.
PARAMETERS:      p_vari TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:  kd_stida TYPE allgstid DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_variant.

START-OF-SELECTION.

  break sahmad.

  IF kd_lifnr IS INITIAL AND
     kd_akont IS INITIAL.
    WRITE : / ' Blank Vendor and Reconcialiation code are not allowed. Enter one of them.'.
    STOP.
  ENDIF.

  PERFORM ldb_processing.
  PERFORM data_processing.
  PERFORM perform_alv.
*&--------------------------------------------------------------------*
*&      Form  CB_GET_LFA1                                             *
*&--------------------------------------------------------------------*
FORM cb_get_lfa1 USING cb_node LIKE ldbn-ldbnode
                       cb_lfa1 LIKE lfa1
                       cb_mode
                       cb_selected.
*BREAK SAHMAD.
* fill workarea LFA1
  lfa1 = cb_lfa1.

  IF gs_progress-readrec > gs_progress-recrd.
    gs_progress-recrd = gs_progress-readrec.
    gs_progress-text  = text-014.
    REPLACE '&' WITH gs_progress-recrd
                INTO gs_progress-text.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lfa1-lifnr
      IMPORTING
        output = gs_progress-lifnr.

    REPLACE '&' WITH gs_progress-lifnr
                INTO gs_progress-text.
    CONDENSE gs_progress-text.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 0
        text       = gs_progress-text.
    gs_progress-recrd = gs_progress-recrd + g_progress_portion.
  ENDIF.

ENDFORM.                    " CB_GET_LFA1
*&--------------------------------------------------------------------*
*&      Form  CB_GET_BSIKEXT                                          *
*&--------------------------------------------------------------------*
FORM cb_get_bsikext USING cb_node LIKE ldbn-ldbnode
                          cb_bsik LIKE bsikext
                          cb_mode
                          cb_selected.
*  BREAK SAHMAD.
*addition of program
  MOVE-CORRESPONDING cb_bsik TO wa_bsikext.
  APPEND wa_bsikext TO it_bsikext.
*End of additon
* local data declaration
  DATA: l_vtage TYPE p.

*.. add read records
  gs_progress-readrec = gs_progress-readrec + 1.

  IF t001-bukrs <> cb_bsik-bukrs.
    SELECT SINGLE * FROM t001 WHERE bukrs = cb_bsik-bukrs.
  ENDIF.

*.. check the invoice reference
  IF NOT ( cb_bsik-rebzt IS INITIAL OR
           cb_bsik-rebzt = 'V' OR
           cb_bsik-rebzt = 'P' ).
    PERFORM invoice_reference_check CHANGING cb_bsik.
  ENDIF.

  CLEAR: gt_re_ex20.
  IF gs_flg-lfa1_fields_used = c_on.
    MOVE-CORRESPONDING lfa1  TO gt_re_ex20.
  ENDIF.
  IF gs_flg-lfb1_fields_used = c_on.
    MOVE-CORRESPONDING lfb1  TO gt_re_ex20.
  ENDIF.

  MOVE-CORRESPONDING cb_bsik TO gt_re_ex20.
*Sahmad - Additional fields data
  gt_re_ex20-zxblnr = cb_bsik-xblnr.   "PO Number
  gt_re_ex20-zbudat = cb_bsik-budat.   "Posting Date
  gt_re_ex20-zshkzg = cb_bsik-shkzg.   "Debit / Credit Indicator
*end of additional fields data
  gt_re_ex20-waers = t001-waers.
  gt_re_ex20-stida = g_stida.
  gt_re_ex20-koart = c_acct_type_kred.
*.. calculate days in arrear
  IF g_stida GE cb_bsik-netdt.
    gt_re_ex20-due_type = '1'.
    l_vtage = g_stida - cb_bsik-netdt.
    UNPACK l_vtage TO gt_re_ex20-vtage_net.
  ELSE.
    CLEAR: gt_re_ex20-vtage_net WITH NULL.
  ENDIF.

  IF g_stida GE cb_bsik-sk1dt.
    gt_re_ex20-due_type = '1'.
    l_vtage = g_stida - cb_bsik-sk1dt.
    UNPACK l_vtage TO gt_re_ex20-vtage_sk1.
  ELSE.
    CLEAR: gt_re_ex20-vtage_sk1 WITH NULL.
  ENDIF.

  IF g_stida GE cb_bsik-sk2dt.
    gt_re_ex20-due_type = '1'.
    l_vtage = g_stida - cb_bsik-sk2dt.
    UNPACK l_vtage TO gt_re_ex20-vtage_sk2.
  ELSE.
    CLEAR: gt_re_ex20-vtage_sk2 WITH NULL.
  ENDIF.

  IF gt_re_ex20-due_type = '1'.
    COLLECT gt_re_ex20.
  ENDIF.
*.. calculate days of not due items
  CLEAR: gt_re_ex20-vtage_net,
         gt_re_ex20-vtage_sk1,
         gt_re_ex20-vtage_sk2,
         gt_re_ex20-due_type.

  IF g_stida LT cb_bsik-netdt.
    gt_re_ex20-due_type = '2'.
    l_vtage = cb_bsik-netdt - g_stida.
    UNPACK l_vtage TO gt_re_ex20-vtage_net.
  ELSE.
    CLEAR: gt_re_ex20-vtage_net WITH NULL.
  ENDIF.

  IF g_stida LT cb_bsik-sk1dt.
    gt_re_ex20-due_type = '2'.
    l_vtage = cb_bsik-sk1dt - g_stida.
    UNPACK l_vtage TO gt_re_ex20-vtage_sk1.
  ELSE.
    CLEAR: gt_re_ex20-vtage_sk1 WITH NULL.
  ENDIF.

  IF g_stida LT cb_bsik-sk2dt.
    gt_re_ex20-due_type = '2'.
    l_vtage = cb_bsik-sk2dt - g_stida.
    UNPACK l_vtage TO gt_re_ex20-vtage_sk2.
  ELSE.
    CLEAR: gt_re_ex20-vtage_sk2 WITH NULL.
  ENDIF.

  IF gt_re_ex20-due_type = '2'.
    COLLECT gt_re_ex20.
  ENDIF.

ENDFORM.                    " CB_GET_BSIK
*&--------------------------------------------------------------------*
*&      Form  INVOICE_REFERENCE_CHECK                                 *
*&--------------------------------------------------------------------*
FORM invoice_reference_check
              CHANGING value(ps_bsik) STRUCTURE bsikext.
* local data declaration
  DATA: ls_bkpf  LIKE bkpf,
        ls_bseg  LIKE bseg,
        ls_faede LIKE faede.

  IF NOT ( ps_bsik-rebzg IS INITIAL AND
           ps_bsik-rebzj IS INITIAL AND
           ps_bsik-rebzz IS INITIAL ).
    SELECT SINGLE * FROM bkpf INTO ls_bkpf      "because of 'BLDAT'
                   WHERE bukrs = ps_bsik-bukrs
                     AND belnr = ps_bsik-rebzg
                     AND gjahr = ps_bsik-rebzj.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM bseg INTO ls_bseg
                     WHERE bukrs = ps_bsik-bukrs
                       AND belnr = ps_bsik-rebzg
                       AND gjahr = ps_bsik-rebzj
                       AND buzei = ps_bsik-rebzz.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_bkpf TO ls_faede.
        MOVE-CORRESPONDING ls_bseg TO ls_faede.
        CALL FUNCTION 'DETERMINE_DUE_DATE'
          EXPORTING
            i_faede = ls_faede
          IMPORTING
            e_faede = ls_faede
          EXCEPTIONS
            OTHERS  = 1.
*...... takeover due date from invoice
        ps_bsik-netdt = ls_faede-netdt.
        ps_bsik-sk1dt = ls_faede-sk1dt.
        ps_bsik-sk2dt = ls_faede-sk2dt.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " INVOICE_REFERENCE_CHECK
*&---------------------------------------------------------------------*
*&      Form  LDB_PROCESSING
*&---------------------------------------------------------------------*
*       LDB Extract and process
*----------------------------------------------------------------------*
FORM ldb_processing .
  g_stida = kd_stida.      "'20111027'.
*Call back routine after logical database return data.
  callback-ldbnode     = 'LFA1'.
  callback-get         = 'X'.
  callback-get_late    = ' '.
  callback-cb_prog     = sy-repid.
  callback-cb_form     = 'CB_GET_LFA1'.
  APPEND callback.

  callback-ldbnode     = 'BSIKEXT'.
  callback-get         = 'X'.
  callback-get_late    = ' '.
  callback-cb_prog     = sy-repid.
  callback-cb_form     = 'CB_GET_BSIKEXT'.
  APPEND callback.
***********************
**Move basic selection criteria for logical database processing.
  MOVE: 'I'      TO seltab-sign,
        'EQ'     TO seltab-option,
        'S'      TO seltab-kind,
        'DUE_TYPE' TO seltab-selname,
        '1'     TO seltab-low.
  APPEND seltab.

  MOVE: 'I'      TO seltab-sign,
        'EQ'     TO seltab-option,
        'S'      TO seltab-kind,
        'DUE_TYPE' TO seltab-selname,
        '2'     TO seltab-low.
  APPEND seltab.

  MOVE: 'KOART' TO seltab-selname,
        'S'      TO seltab-kind,
        'I'      TO seltab-sign,
        'EQ'     TO seltab-option,
        'K'     TO seltab-low.
*	      TO SELTAB-HIGH.
  APPEND seltab.

  MOVE: 'KD_STIDA' TO seltab-selname,
        'P'      TO seltab-kind,
        'I'      TO seltab-sign,
        'EQ'     TO seltab-option,
*      '20111027'     TO SELTAB-LOW.
         kd_stida TO seltab-low.
*	      TO SELTAB-HIGH.
  APPEND seltab.

  LOOP AT kd_bukrs.
    MOVE: 'KD_BUKRS' TO seltab-selname,
       'S'      TO seltab-kind,
       kd_bukrs-sign      TO seltab-sign,
       kd_bukrs-option TO seltab-option,
*        'UGL'     TO SELTAB-LOW.
       kd_bukrs-low     TO seltab-low,
       kd_bukrs-high     TO seltab-high.
*  	      TO SELTAB-HIGH.
    APPEND seltab.
  ENDLOOP.
  LOOP AT kd_lifnr.
    MOVE: 'KD_LIFNR' TO seltab-selname,
          'S'      TO seltab-kind,
          kd_lifnr-sign     TO seltab-sign,
          kd_lifnr-option TO seltab-option,
*          '0000023926'     TO SELTAB-LOW.
          kd_lifnr-low TO seltab-low,
          kd_lifnr-high TO seltab-high.
*    	      TO SELTAB-HIGH.
    APPEND seltab.
  ENDLOOP.
  LOOP AT kd_akont.
    MOVE: 'KD_AKONT' TO seltab-selname,
          'S'      TO seltab-kind,
          kd_akont-sign     TO seltab-sign,
          kd_akont-option TO seltab-option,
*          '0000023926'     TO SELTAB-LOW.
          kd_akont-low TO seltab-low,
          kd_akont-high TO seltab-high.
*    	      TO SELTAB-HIGH.
    APPEND seltab.
  ENDLOOP.
*KD_BUDAT
  CALL FUNCTION 'LDB_PROCESS'
    EXPORTING
      ldbname                     = 'KDF'
*     VARIANT                     =
      expressions                 = gt_dynsel
      field_selection             = gt_select_fields
    TABLES
      callback                    = callback
      selections                  = seltab "GT_SELECTIONS
    EXCEPTIONS
      ldb_not_reentrant           = 1
      ldb_incorrect               = 2
      ldb_already_running         = 3
      ldb_error                   = 4
      ldb_selections_error        = 5
      ldb_selections_not_accepted = 6
      variant_not_existent        = 7
      variant_obsolete            = 8
      variant_error               = 9
      free_selections_error       = 10
      callback_no_event           = 11
      callback_node_duplicate     = 12
      OTHERS                      = 13.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " LDB_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM data_processing .

  DATA: lv_vnet LIKE gt_re_ex20-vtage_net.

  CLEAR lv_vnet WITH NULL.
  LOOP AT gt_re_ex20.
*Null value is due to Key date is less than due date
    IF gt_re_ex20-vtage_net = lv_vnet.
*      gt_re_ex20-vtage_net = 0.
       CONTINUE.
    ENDIF.
    CLEAR wa_output.
    MOVE-CORRESPONDING gt_re_ex20 TO wa_output.
    IF gt_re_ex20-vtage_net <= 30.
      wa_output-a0_30 = gt_re_ex20-dmshb.
    ENDIF.
    IF gt_re_ex20-vtage_net > 30 AND
       gt_re_ex20-vtage_net <= 60.
      wa_output-a31_60 = gt_re_ex20-dmshb.
    ENDIF.
    IF gt_re_ex20-vtage_net > 60 AND
       gt_re_ex20-vtage_net <= 90.
      wa_output-a61_90 = gt_re_ex20-dmshb.
    ENDIF.
    IF gt_re_ex20-vtage_net > 90 AND
       gt_re_ex20-vtage_net <= 120.
      wa_output-a91_120 = gt_re_ex20-dmshb.
    ENDIF.
    IF gt_re_ex20-vtage_net > 120 AND
       gt_re_ex20-vtage_net <= 150.
      wa_output-a121_150 = gt_re_ex20-dmshb.
    ENDIF.
    IF gt_re_ex20-vtage_net > 150 AND
       gt_re_ex20-vtage_net <= 180.
      wa_output-a151_180 = gt_re_ex20-dmshb.
    ENDIF.
    IF gt_re_ex20-vtage_net > 180 AND
       gt_re_ex20-vtage_net <= 210.
      wa_output-a181_210 = gt_re_ex20-dmshb.
    ENDIF.
    IF gt_re_ex20-vtage_net > 210.
      wa_output-a211_above = gt_re_ex20-dmshb.
    ENDIF.
    APPEND wa_output TO it_output.
  ENDLOOP.
  LOOP AT it_output INTO wa_output. "it_BSIKEXT into wa_BSIKEXT.
    CLEAR wa_bsikext.
    READ TABLE it_bsikext INTO wa_bsikext WITH KEY belnr = wa_output-belnr
                                             blart = wa_output-blart
                                             xblnr = wa_output-zxblnr.
    CHECK sy-subrc = 0.
    wa_output-dmbtr = wa_bsikext-dmbtr.
    wa_output-wrbtr = wa_bsikext-wrbtr.
    MODIFY it_output FROM wa_output TRANSPORTING dmbtr wrbtr.
  ENDLOOP.
*  if kd_xblnr is not initial.
*     delete it_output where zxblnr not in kd_xblnr.
*  endif.
ENDFORM.                    " DATA_PROCESSING
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_double_click  USING    p_e_row TYPE lvc_s_row
                                   p_e_column TYPE lvc_s_col
                                   p_es_row_no TYPE lvc_s_roid.
  DATA: ls_output LIKE LINE OF it_output,
        ls_ekpo TYPE ekpo,
        txt2(50).
  break sahmad.
  READ TABLE it_output INTO ls_output INDEX p_es_row_no-row_id.
  CHECK sy-subrc = 0.
  IF p_e_column-fieldname = 'BELNR'.
    SET PARAMETER ID 'BLN' FIELD ls_output-belnr.
    SET PARAMETER ID 'BUK' FIELD ls_output-bukrs.
    SET PARAMETER ID 'GJR' FIELD ls_output-gjahr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ELSEIF p_e_column-fieldname = 'ZXBLNR'.
    SELECT SINGLE * INTO ls_ekpo FROM ekpo WHERE ebeln = ls_output-zxblnr.
    IF sy-subrc <> 0.
      CONCATENATE 'Please check PO' ls_output-zxblnr INTO txt2 SEPARATED BY space.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Purchase Order'
          txt1  = 'PO as reference document is not available'
          txt2  = txt2
*         TXT3  = ' '
*         TXT4  = ' '
        .
    ELSE.
      SET PARAMETER ID 'BES' FIELD ls_output-zxblnr.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
ENDFORM.                            " HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  PERFORM_ALV
*&---------------------------------------------------------------------*
*       ALV Display
*----------------------------------------------------------------------*
FORM perform_alv .

  DATA: ls_variant TYPE disvariant.

  CREATE OBJECT gr_container
    EXPORTING
*    parent                      =
      container_name              = 'ALV_CONTAINER1'
*    style                       =
*    lifetime                    = lifetime_default
*    repid                       =
*    dynnr                       =
*    no_autodef_progid_dynnr     =
*  EXCEPTIONS
*    cntl_error                  = 1
*    cntl_system_error           = 2
*    create_error                = 3
*    lifetime_error              = 4
*    lifetime_dynpro_dynpro_link = 5
*    others                      = 6
      .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CREATE OBJECT gr_alvgrid
    EXPORTING
*    i_shellstyle      = 0
*    i_lifetime        =
      i_parent          = gr_container
*    i_appl_events     = space
*    i_parentdbg       =
*    i_applogparent    =
*    i_graphicsparent  =
*    i_name            =
*    i_fcat_complete   = space
*  EXCEPTIONS
*    error_cntl_create = 1
*    error_cntl_init   = 2
*    error_cntl_link   = 3
*    error_dp_create   = 4
*    others            = 5
      .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
****Create and register double click event
  CREATE OBJECT gr_event_handler.
  SET HANDLER gr_event_handler->handle_double_click
              gr_event_handler->handle_hotspot_click
              FOR gr_alvgrid.
**********
  PERFORM field_catalog.
  PERFORM prepare_layout.

*endif.
  CALL METHOD gr_alvgrid->set_gridtitle
    EXPORTING
      i_gridtitle = 'Vendor Aging Report'.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  ls_variant-report = sy-repid.
  ls_variant-variant = p_vari.
  CALL METHOD gr_alvgrid->set_table_for_first_display
    EXPORTING
*     I_BUFFER_ACTIVE               =
*     I_BYPASSING_BUFFER            =
*     I_CONSISTENCY_CHECK           =
*     I_STRUCTURE_NAME              =
      is_variant                    = ls_variant
      i_save                        = 'A'
      i_default                     = 'X'
      is_layout                     = gs_layout
*     IS_PRINT                      =
*     IT_SPECIAL_GROUPS             =
*     IT_TOOLBAR_EXCLUDING          =
*     IT_HYPERLINK                  =
*     IT_ALV_GRAPHICS               =
*     IT_EXCEPT_QINFO               =
*     IR_SALV_ADAPTER               =
    CHANGING
      it_outtab                     = it_output[]
      it_fieldcatalog               = gt_fieldcat
*     IT_SORT                       =
*     IT_FILTER                     =
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL SCREEN 100.

ENDFORM.                    " PERFORM_ALV
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STAT_01'.
  SET TITLEBAR 'T01'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  break sahmad.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
*       Build Fields Catalog
*----------------------------------------------------------------------*
FORM field_catalog .

  DATA ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = 'BUKRS'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'BUKRS'.
  ls_fcat-outputlen = '10'.
*  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = ''.
*  ls_fcat-seltext = ''.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'LIFNR'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'LIFNR'.
  ls_fcat-outputlen = '10'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'BLART'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'BLART'.
  ls_fcat-outputlen = '4'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ZXBLNR'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'ZXBLNR'.
  ls_fcat-outputlen = '14'.
  ls_fcat-coltext = 'PO Number'.
  ls_fcat-seltext = 'PO Number'.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'GJAHR'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'GJAHR'.
  ls_fcat-outputlen = '6'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'BELNR'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'BELNR'.
  ls_fcat-outputlen = '10'.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ZBUDAT'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'ZBUDAT'.
  ls_fcat-outputlen = '10'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ZSHKZG'.
  ls_fcat-ref_table = 'BSIKEXT'.
  ls_fcat-ref_field = 'SHKZG'.
  ls_fcat-outputlen = '4'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'DMBTR'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'DMBTR'.
  ls_fcat-outputlen = '15'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WRBTR'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'WRBTR'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = 'Amount in DC'.
  ls_fcat-seltext = 'Amount in Document Currency'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'A0_30'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'DMSHB'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = '0 - 30'.
  ls_fcat-seltext = '0 - 30'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'A31_60'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'DMSHB'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = '31 - 60'.
  ls_fcat-seltext = '31 - 60'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'A61_90'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'DMSHB'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = '61 - 90'.
  ls_fcat-seltext = '61 - 90'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'A91_120'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'DMSHB'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = '91 - 120'.
  ls_fcat-seltext = '91 - 120'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'A121_150'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'DMSHB'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = '121 - 150'.
  ls_fcat-seltext = '121 - 150'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'A151_180'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'DMSHB'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = '151 - 180'.
  ls_fcat-seltext = '151 - 180'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'A181_210'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'DMSHB'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = '181 - 210'.
  ls_fcat-seltext = '181 - 210'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'A211_ABOVE'.
  ls_fcat-ref_table = 'RFRRK20'.
  ls_fcat-ref_field = 'DMSHB'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = '211 - Above'.
  ls_fcat-seltext = '211 - Above'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

ENDFORM.                    " FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM prepare_layout .

  DATA: fl_title(50),
        fl_date(10).

*write kd_stida to fl_date .
  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = kd_stida
    IMPORTING
      output = fl_date.

  CONCATENATE 'Vendor Aging Report, Key Date : ' fl_date
                        INTO fl_title RESPECTING BLANKS.

  gs_layout-zebra  = 'X'.
  gs_layout-smalltitle = 'X'.
  gs_layout-sel_mode = 'A'.
  gs_layout-grid_title = fl_title.

ENDFORM.                    " PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F4_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_variant.

  DATA: ls_variant TYPE disvariant,
        ls_dis_var TYPE disvariant,
        g_exit TYPE c.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = ls_variant
*     I_TABNAME_HEADER    =
*     I_TABNAME_ITEM      =
*     it_default_fieldcat = lt_fieldcat
      i_save              = 'A'
*     I_DISPLAY_VIA_GRID  = ' '
    IMPORTING
      e_exit              = g_exit
      es_variant          = ls_dis_var
    EXCEPTIONS
      not_found           = 1
      program_error       = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  p_vari = ls_dis_var-variant.

ENDFORM.                    " F4_VARIANT
