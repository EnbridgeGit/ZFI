*&---------------------------------------------------------------------*
*&  Include           ZTAX_VALIDATION_CLASS                            *
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
    handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

    handle_menu_button
        FOR EVENT menu_button OF cl_gui_alv_grid
            IMPORTING e_object e_ucomm,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,

    respond_hotspot_click FOR EVENT
             hotspot_click OF
             cl_gui_alv_grid IMPORTING  e_row_id
                                        e_column_id
                                        es_row_no.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.

    CLEAR gs_toolbar.
    MOVE 3 TO gs_toolbar-butn_type.
    APPEND gs_toolbar TO e_object->mt_toolbar.

    CLEAR gs_toolbar.
    MOVE 'TO_FIDOC' TO gs_toolbar-function.
* --> This function code is evaluated in 'handle_menu_button'
    MOVE icon_detail TO gs_toolbar-icon.
    MOVE 'Display Detail' TO gs_toolbar-quickinfo.
    MOVE 1 TO gs_toolbar-butn_type.
    MOVE space TO gs_toolbar-disabled.
    APPEND gs_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.                    "handle_toolbar
*--------------------------------------------------------------------
  METHOD handle_menu_button.
    IF e_ucomm = 'TO_FIDOC'.
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TO_FIDOC'
          text  = text-100. "FI
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TO_LIVDOC'
          text  = text-200. "LIV
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TO_PODOC'
          text  = text-300. "PO
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TO_VEND'
          text  = text-400. "Vendor
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TO_KOSTL'
          text  = text-500. "Cost Center
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TO_SAKNR'
          text  = text-600. "GL Account
      CALL METHOD e_object->add_function
        EXPORTING
          fcode = 'TO_ASSET'
          text  = text-700. "ASSET

    ENDIF.
  ENDMETHOD.                    "handle_menu_button
*---------------------------------------------------------------------
  METHOD handle_user_command.
    DATA: lt_rows TYPE lvc_t_row.
    DATA: selected TYPE lvc_s_row.

* get selected row
    g_repid = sy-repid.
    CALL METHOD grid1->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.
    CALL METHOD cl_gui_cfw=>flush.
    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = g_repid
          txt2  = sy-subrc
          txt1  = 'Error in Flush'(500).
    ENDIF.

    READ TABLE lt_rows INTO selected INDEX 1.
    READ TABLE alv_grid INTO ztax_validate1 INDEX selected-index.
    REFRESH bdc_tab.
    CASE e_ucomm.
      WHEN 'TO_FIDOC'.
        AUTHORITY-CHECK OBJECT 'S_TCODE'
                 ID 'TCD' FIELD 'FB03'.
        IF sy-subrc NE 0.
          MESSAGE s000(ztax) WITH 'Missing Authorization'.
        ELSE.
          SET PARAMETER ID 'BLN' FIELD ztax_validate1-belnr.
          SET PARAMETER ID 'BUK' FIELD ztax_validate1-bukrs.
          SET PARAMETER ID 'GJR' FIELD ztax_validate1-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'TO_LIVDOC'.
        IF NOT ztax_validate1-belnri IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'MIR4'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            SET PARAMETER ID 'RBN' FIELD ztax_validate1-belnri.
            SET PARAMETER ID 'GJR' FIELD ztax_validate1-gjahr.
            CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'TO_PODOC'.
        IF NOT ztax_validate1-ebeln IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'ME23N'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            SET PARAMETER ID 'BES' FIELD ztax_validate1-ebeln.
            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'TO_VEND'.
        IF NOT ztax_validate1-lifnr IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'XK03'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.

            PERFORM insert_row_in_bdc_tab TABLES bdc_tab USING:
                 'X' 'SAPMF02K'             '0101',
                 ' ' 'RF02K-LIFNR'          ztax_validate1-lifnr,
                 ' ' 'RF02K-D0110'          'X',
                 ' ' 'RF02K-D0120'          'X',
                 ' ' 'BDC_OKCODE'             'ENT1',
                 'X' 'SAPMFK02K'            '0111'.
            CALL TRANSACTION 'XK03' USING bdc_tab MODE 'E'.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'TO_KOSTL'.
        IF NOT ztax_validate1-kostl IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'KS03'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            SET PARAMETER ID 'CAC' FIELD ztax_validate1-kokrs.
            SET PARAMETER ID 'KOS' FIELD ztax_validate1-kostl.
            CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'TO_SAKNR'.
        IF NOT ztax_validate1-saknr IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'FS00'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            PERFORM insert_row_in_bdc_tab TABLES bdc_tab USING:
                 'X' 'SAPLGL_ACCOUNT_MASTER_MAINTAIN'  '2001',
                 ' ' 'GLACCOUNT_SCREEN_KEY-SAKNR' ztax_validate1-saknr,
                 ' ' 'GLACCOUNT_SCREEN_KEY-BUKRS' ztax_validate1-bukrs,
                 ' ' 'BDC_OKCODE'      'ACC_SHOW',
                 'X' 'SAPLGL_ACCOUNT_MASTER_MAINTAIN'  '2001'.
            CALL TRANSACTION 'FS00' USING bdc_tab MODE 'E'.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'TO_ASSET'.
        IF NOT ztax_validate1-anln1 IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'AB03'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            SET PARAMETER ID 'BUK' FIELD ztax_validate1-bukrs.
            SET PARAMETER ID 'AN1' FIELD ztax_validate1-anln1.
            SET PARAMETER ID 'AN2' FIELD ztax_validate1-anln2.
            SET PARAMETER ID 'GJR' FIELD ztax_validate1-gjahr.
            CALL TRANSACTION 'AB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                           "handle_user_command

  METHOD respond_hotspot_click.
**Callaway
    DATA: BEGIN OF wflkey,
     bukrs TYPE bukrs,
     belnr TYPE bkpf-belnr,
     gjahr TYPE bkpf-gjahr,
    END OF wflkey.
    DATA: BEGIN OF wflpos,
     bukrs TYPE bkpf-bukrs,
     belnr TYPE bkpf-belnr,
     gjahr TYPE bkpf-gjahr,
     buzei TYPE bseg-buzei,
    END OF wflpos.

    REFRESH bdc_tab.
    READ TABLE alv_grid INTO ztax_validate1 INDEX e_row_id-index.
    CASE e_column_id.
      WHEN 'BELNR'.
        AUTHORITY-CHECK OBJECT 'S_TCODE'
                 ID 'TCD' FIELD 'FB03'.
        IF sy-subrc NE 0.
          MESSAGE s000(ztax) WITH 'Missing Authorization'.
        ELSE.
          SET PARAMETER ID 'BLN' FIELD ztax_validate1-belnr.
          SET PARAMETER ID 'BUK' FIELD ztax_validate1-bukrs.
          SET PARAMETER ID 'GJR' FIELD ztax_validate1-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'BELNRI'.
        IF NOT ztax_validate1-belnri IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'MIR4'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            SET PARAMETER ID 'RBN' FIELD ztax_validate1-belnri.
            SET PARAMETER ID 'GJR' FIELD ztax_validate1-gjahr.
            CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'EBELN'.
        IF NOT ztax_validate1-ebeln IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'ME23N'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            SET PARAMETER ID 'BES' FIELD ztax_validate1-ebeln.
            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'LIFNR'.
        IF NOT ztax_validate1-lifnr IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'XK03'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            PERFORM insert_row_in_bdc_tab TABLES bdc_tab USING:
                 'X' 'SAPMF02K'             '0101',
                 ' ' 'RF02K-LIFNR'          ztax_validate1-lifnr,
                 ' ' 'RF02K-D0110'          'X',
                 ' ' 'RF02K-D0120'          'X',
                 ' ' 'BDC_OKCODE'             'ENT1',
                 'X' 'SAPMFK02K'            '0111'.
            CALL TRANSACTION 'XK03' USING bdc_tab MODE 'E'.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'KOSTL'.
        IF NOT ztax_validate1-kostl IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'KS03'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            SET PARAMETER ID 'CAC' FIELD ztax_validate1-kokrs.
            SET PARAMETER ID 'KOS' FIELD ztax_validate1-kostl.
            CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'VBELN'.
        IF NOT ztax_validate1-vbeln IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'VA03'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            SET PARAMETER ID 'AUN' FIELD ztax_validate1-vbeln.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'ANLN1'.
        IF NOT ztax_validate1-anln1 IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'AB03'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            SET PARAMETER ID 'BUK' FIELD ztax_validate1-bukrs.
            SET PARAMETER ID 'AN1' FIELD ztax_validate1-anln1.
            SET PARAMETER ID 'AN2' FIELD ztax_validate1-anln2.
            SET PARAMETER ID 'GJR' FIELD ztax_validate1-gjahr.
            CALL TRANSACTION 'AB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'SAKNR'.
        IF NOT ztax_validate1-saknr IS INITIAL.
          AUTHORITY-CHECK OBJECT 'S_TCODE'
                   ID 'TCD' FIELD 'FS00'.
          IF sy-subrc NE 0.
            MESSAGE s000(ztax) WITH 'Missing Authorization'.
          ELSE.
            PERFORM insert_row_in_bdc_tab TABLES bdc_tab USING:
                 'X' 'SAPLGL_ACCOUNT_MASTER_MAINTAIN'  '2001',
                 ' ' 'GLACCOUNT_SCREEN_KEY-SAKNR' ztax_validate1-saknr,
                 ' ' 'GLACCOUNT_SCREEN_KEY-BUKRS' ztax_validate1-bukrs,
                 ' ' 'BDC_OKCODE'      'ACC_SHOW',
                 'X' 'SAPLGL_ACCOUNT_MASTER_MAINTAIN'  '2001'.
            CALL TRANSACTION 'FS00' USING bdc_tab MODE 'E'.
          ENDIF.
        ELSE.
          MOVE 'No value for Navigation.' TO info_msg.
          MESSAGE s000(ztax) WITH info_msg.
        ENDIF.
      WHEN 'ARC_DOC_ID'.
        IF NOT ztax_validate1-arc_doc_id IS INITIAL.
** Callaway archive display
          MOVE-CORRESPONDING ztax_validate1 TO: wflkey, wflpos.
          PERFORM org_beleg_zeigen IN PROGRAM sapfs006 USING wflkey wflpos.

*          CALL FUNCTION 'ARCHIVOBJECT_DISPLAY'
*            EXPORTING
*              archiv_doc_id            = ztax_validate1-arc_doc_id
*              archiv_id                = ztax_validate1-archiv_id
*              objecttype               = ztax_validate1-sap_object
*              object_id                = ztax_validate1-object_id
*              ar_object                = ztax_validate1-ar_object
*            EXCEPTIONS
*              error_archiv             = 1
*              error_communicationtable = 2
*              error_kernel             = 3
*              OTHERS                   = 4.
*          IF sy-subrc <> 0.
*            MESSAGE i101.
*          ENDIF.

        ENDIF.
      WHEN 'GRDOC'.
        AUTHORITY-CHECK OBJECT 'S_TCODE'
                 ID 'TCD' FIELD 'MB03'.
        IF sy-subrc NE 0.
          MESSAGE s000(ztax) WITH 'Missing Authorization'.
        ELSE.
          SET PARAMETER ID 'MBN' FIELD ztax_validate1-grdoc.
          SET PARAMETER ID 'MJA' FIELD ztax_validate1-grjah.
          CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "RESPOND_HOTSPOT_CLICK


ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
