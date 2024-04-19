*&---------------------------------------------------------------------*
*& Report  ZFAPR002_VEN_INV_TREN
*&
*&---------------------------------------------------------------------*
*&R_P2C_AP_002# Vendor Invoice Approval Trend Report.
*&---------------------------------------------------------------------*
************************************************************************
*  Author:      Sajjad Ahmad                                          *
*  Date:        Oct 18, 2011.                                          *
*  Issue Log:                                                      *
*  Description:                                                        *
*     - Vendor Invoice Approval Trend Report
************************************************************************
*CHANGES:                                                              *
************************************************************************
REPORT  zfapr002_ven_inv_tren.
*&---------------------------------------------------------------------*
*&       Class LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*       Double Click Event Handler
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid
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
  METHOD handle_hotspot_click.
    PERFORM handle_double_click USING e_row_id e_column_id
                                      es_row_no.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.               "lcl_event_handler
TABLES: bkpf,
        lfa1,
        bsik,
        vbsegk.
TYPES: BEGIN OF ty_output,
         bukrs TYPE bsik-bukrs,
         lifnr TYPE bsik-lifnr,
         name1 TYPE lfa1-name1,
         ebeln TYPE bsik-ebeln, "PO
         xblnr TYPE bsik-xblnr, "Vendor Inv
         blart TYPE bsik-blart,
         gjahr TYPE bsik-gjahr,   "not in spec
         belnr TYPE bsik-belnr,
         mminv TYPE bsik-belnr,
         wrbtr TYPE bsik-wrbtr,  "not in spec
         augbl TYPE bsik-augbl,  "not in spec
         awkey TYPE vbkpf-awkey,
         awtyp TYPE vbkpf-awtyp,
         wi_id TYPE swr_wihdr-wi_id, "Work item
         wi_cd TYPE swr_wihdr-wi_cd, "Work Item Creation Date
         wi_creator TYPE swr_wihdr-wi_creator, " Work item creator / Document Parked by
         route_owner(35),
         wi_led TYPE swr_widtl-wi_led, "Document Approval Date
         wi_aagent TYPE swr_widtl-wi_aagent, " Document approved by
         wi_days TYPE i, "difference b/w WI creation and completion date
         wi_stat TYPE swr_widtl-wi_stat, "Work Item Status
         wi_item_stat(35),
       END OF ty_output.
TYPES: BEGIN OF typ_bkpf,
           awkey TYPE bkpf-awkey,
           belnr TYPE bkpf-belnr,
           gjahr TYPE bkpf-gjahr,
           bukrs TYPE bkpf-bukrs,
        END OF typ_bkpf.

DATA: it_output TYPE TABLE OF ty_output,
      wa_output LIKE LINE OF it_output,
      it_workflow TYPE TABLE OF ty_output.
DATA: lt_bsik TYPE TABLE OF bsik,
      wa_bsik LIKE LINE OF lt_bsik,
      lt_bsik1 TYPE TABLE OF bsik,
      lt_typ_bkpf TYPE TABLE OF typ_bkpf,
      wa_typ_bkpf LIKE LINE OF lt_typ_bkpf,
      it_lfa1 TYPE TABLE OF lfa1,
      wa_lfa1 LIKE LINE OF it_lfa1.
DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE ok_code.
DATA: gr_container TYPE REF TO cl_gui_custom_container,
      gr_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_layout    TYPE lvc_s_layo.
DATA: gr_event_handler TYPE REF TO lcl_event_handler.
*************SELECTION SCREEN************************************
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_bukrs  FOR  bkpf-bukrs OBLIGATORY,
                s_lifnr  FOR  lfa1-lifnr OBLIGATORY,
*                s_BELNR  for  bkpf-BELNR,
                s_budat  FOR  bkpf-budat,
                s_blart  FOR  bkpf-blart,
*                s_xblnr  FOR  bkpf-xblnr.
                s_ebeln  FOR  bsik-ebeln,    "for PO
                s_xref3  FOR  vbsegk-xref3.  "for NON PO
SELECTION-SCREEN END OF BLOCK box1.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETERS: s_all  RADIOBUTTON GROUP grp1 DEFAULT 'X',
            s_comp RADIOBUTTON GROUP grp1,
            s_rco  RADIOBUTTON GROUP grp1,
            s_dao  RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK box2.
**************END OF SELECTION SCREEN*******************************
INITIALIZATION.

AT SELECTION-SCREEN ON BLOCK box1.
  IF s_ebeln IS NOT INITIAL AND
     s_xref3 IS NOT INITIAL.
    MESSAGE e000(zfi01) WITH 'PO and Reference3 are not allowed together'. "'&' '&' '&'.
  ENDIF.

START-OF-SELECTION.

*write: / 'Under development'.
*stop.
  CLEAR: it_output,
         wa_output,
         it_workflow,
         lt_bsik,
         wa_bsik,
         lt_bsik1,
         lt_typ_bkpf,
         wa_typ_bkpf,
         it_lfa1,
         wa_lfa1.

  SELECT * INTO TABLE it_lfa1 FROM lfa1
                        WHERE lifnr IN s_lifnr.
************
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 5
      text       = 'Extracting Open / Cleared Items'.
  PERFORM get_data_open_cleared.
*************
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 25
      text       = 'Extracting Parked Invoices'.
*No need for this because when PO base Invoice parked it is saved in VBKPF table, when cleared it is gone to clear items (BSIK)
*  IF s_xref3 IS INITIAL.
*     PERFORM get_po_invoices. "
*  ENDIF.
***************
  PERFORM get_parked_documents.
*****************
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = 'Final Processing of data and preparing for output'.
  PERFORM process_data.
  PERFORM perform_alv.
*&---------------------------------------------------------------------*
*&      Form  PERFORM_ALV
*&---------------------------------------------------------------------*
FORM perform_alv .
*if gr_container is initial.
  CREATE OBJECT gr_container
    EXPORTING
*     PARENT                      =
      container_name              = 'ALV_CONTAINER1'
*     STYLE                       =
*     LIFETIME                    = lifetime_default
*     REPID                       =
*     DYNNR                       =
*     NO_AUTODEF_PROGID_DYNNR     =
*     EXCEPTIONS
*     CNTL_ERROR                  = 1
*     CNTL_SYSTEM_ERROR           = 2
*     CREATE_ERROR                = 3
*     LIFETIME_ERROR              = 4
*     LIFETIME_DYNPRO_DYNPRO_LINK = 5
*     others                      = 6
      .
  IF sy-subrc <> 0.
*                                MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                                           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CREATE OBJECT gr_alvgrid
    EXPORTING
*    I_SHELLSTYLE      = 0
*    I_LIFETIME        =
      i_parent          = gr_container
*    I_APPL_EVENTS     = space
*    I_PARENTDBG       =
*    I_APPLOGPARENT    =
*    I_GRAPHICSPARENT  =
*    I_NAME            =
*    I_FCAT_COMPLETE   = SPACE
*  EXCEPTIONS
*    ERROR_CNTL_CREATE = 1
*    ERROR_CNTL_INIT   = 2
*    ERROR_CNTL_LINK   = 3
*    ERROR_DP_CREATE   = 4
*    others            = 5
      .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
****Create and register double click event
  CREATE OBJECT gr_event_handler.
  SET HANDLER: gr_event_handler->handle_double_click
               gr_event_handler->handle_hotspot_click
               FOR gr_alvgrid.
**********
  PERFORM field_catalog.
  PERFORM prepare_layout.

*endif.
  CALL METHOD gr_alvgrid->set_gridtitle
    EXPORTING
      i_gridtitle = 'Vendor Invoice Trend'.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL METHOD gr_alvgrid->set_table_for_first_display
    EXPORTING
*    I_BUFFER_ACTIVE               =
*    I_BYPASSING_BUFFER            =
*    I_CONSISTENCY_CHECK           =
*    I_STRUCTURE_NAME              =
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
      is_layout                     = gs_layout
*    IS_PRINT                      =
*    IT_SPECIAL_GROUPS             =
*    IT_TOOLBAR_EXCLUDING          =
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
*    IT_EXCEPT_QINFO               =
*    IR_SALV_ADAPTER               =
    CHANGING
      it_outtab                     = it_output[]
      it_fieldcatalog               = gt_fieldcat
*    IT_SORT                       =
*    IT_FILTER                     =
*  EXCEPTIONS
*    INVALID_PARAMETER_COMBINATION = 1
*    PROGRAM_ERROR                 = 2
*    TOO_MANY_LINES                = 3
*    others                        = 4
          .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL SCREEN 100.
ENDFORM.                    " PERFORM_ALV
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STAT_01'.
  SET TITLEBAR 'T01'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
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
*&      Form  PREPARE_LAYOUT
*&---------------------------------------------------------------------*
FORM prepare_layout .

  gs_layout-zebra  = 'X'.
  gs_layout-smalltitle = 'X'.
  gs_layout-sel_mode = 'A'.
*  gs_layout-GRID_TITLE = 'ALV Report'.

ENDFORM.                    " PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
FORM field_catalog .

  DATA ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = 'BUKRS'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'BUKRS'.
  ls_fcat-outputlen = '10'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'LIFNR'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'LIFNR'.
  ls_fcat-outputlen = '10'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'NAME1'.
  ls_fcat-ref_table = 'LFA1'.
  ls_fcat-ref_field = 'NAME1'.
  ls_fcat-outputlen = '35'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'EBELN'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'EBELN'.
  ls_fcat-outputlen = '10'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'BLART'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'BLART'.
  ls_fcat-outputlen = '3'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'BELNR'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'BELNR'.
  ls_fcat-outputlen = '10'.
*  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = ''.
*  ls_fcat-seltext = ''.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'XBLNR'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'XBLNR'.
  ls_fcat-outputlen = '10'.
  ls_fcat-coltext = 'Vendor Invoice'.
  ls_fcat-seltext = 'Vendor Invoice'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'MMINV'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'BELNR'.
  ls_fcat-outputlen = '10'.
  ls_fcat-coltext = 'MM Invoice'.
  ls_fcat-seltext = 'MM Invoice'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WRBTR'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'WRBTR'.
  ls_fcat-outputlen = '13'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'AUGBL'.
  ls_fcat-ref_table = 'BSIK'.
  ls_fcat-ref_field = 'AUGBL'.
  ls_fcat-outputlen = '10'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ROUTE_OWNER'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = 'RC Owner'.
  ls_fcat-seltext = 'Route Code Owner'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_ID'.
  ls_fcat-ref_table = 'SWR_WIHDR'.
  ls_fcat-ref_field = 'WI_ID'.
  ls_fcat-coltext = 'Work item ID'.
  ls_fcat-seltext = 'Work item ID'.
  ls_fcat-outputlen = '12'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_CD'.
  ls_fcat-ref_table = 'SWR_WIHDR'.
  ls_fcat-ref_field = 'WI_CD'.
  ls_fcat-coltext = 'WI Creation Date'.
  ls_fcat-seltext = 'WI Creation Date'.
  ls_fcat-outputlen = '12'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_CREATOR'.
  ls_fcat-ref_table = 'SWR_WIHDR'.
  ls_fcat-ref_field = 'WI_CREATOR'.
  ls_fcat-coltext = 'Doc Parked By'.
  ls_fcat-seltext = 'Document Parked By'.
  ls_fcat-outputlen = '15'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_LED'.
  ls_fcat-ref_table = 'SWR_WIDTL'.
  ls_fcat-ref_field = 'WI_LED'.
  ls_fcat-coltext = 'Doc. Approval Date'.
  ls_fcat-seltext = 'Doc. Approval Date'.
  ls_fcat-outputlen = '10'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_AAGENT'.
  ls_fcat-ref_table = 'SWR_WIDTL'.
  ls_fcat-ref_field = 'WI_AAGENT'.
  ls_fcat-coltext = 'Doc. Approved By'.
  ls_fcat-seltext = 'Doc. Approved By'.
  ls_fcat-outputlen = '12'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_DAYS'.
  ls_fcat-outputlen = '10'.
  ls_fcat-coltext = '# days for approval'.
  ls_fcat-seltext = 'No of days for approval'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

*  ls_fcat-fieldname = 'WI_STAT'.
*  ls_fcat-ref_table = 'SWR_WIDTL'.
*  ls_fcat-ref_field = 'WI_STAT'.
*  ls_fcat-outputlen = '12'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_ITEM_STAT'.
*ls_fcat-ref_table = ''.
*  ls_fcat-ref_field = 'WI_STAT'.
  ls_fcat-outputlen = '20'.
  ls_fcat-coltext = 'Status'.
  ls_fcat-seltext = 'Status'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

ENDFORM.                    " FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM handle_double_click  USING    p_e_row TYPE lvc_s_row
                                   p_e_column TYPE lvc_s_col
                                   p_es_row_no TYPE lvc_s_roid.
  DATA: ls_output LIKE LINE OF it_output,
        ls_ekpo TYPE ekpo,
        txt2(50).
*  break sahmad.
  READ TABLE it_output INTO ls_output INDEX p_es_row_no-row_id.
  CHECK sy-subrc = 0.
  CASE p_e_column-fieldname.
    WHEN 'BELNR'.
*         IF p_e_column-fieldname = 'BELNR'.
      IF  ls_output-mminv IS INITIAL OR
          ls_output-mminv <> ls_output-belnr.
        SET PARAMETER ID 'BLN' FIELD ls_output-belnr.
        SET PARAMETER ID 'BUK' FIELD ls_output-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_output-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSE.
        SET PARAMETER ID 'RBN' FIELD ls_output-belnr.
        SET PARAMETER ID 'BUK' FIELD ls_output-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_output-gjahr.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'EBELN'.
*  ELSEIF p_e_column-fieldname = 'EBELN'.
      SELECT SINGLE * INTO ls_ekpo FROM ekpo WHERE ebeln = ls_output-ebeln.
      IF sy-subrc <> 0.
        CONCATENATE 'Please check PO' ls_output-ebeln INTO txt2 SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel         = 'Purchase Order'
            txt1          = 'PO as reference document is not available'
            txt2          = txt2
*                    TXT3          = ' '
*                    TXT4          = ' '
                  .
      ELSE.
        SET PARAMETER ID 'BES' FIELD ls_output-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'MMINV'.
      IF ls_output-mminv IS INITIAL.
        MESSAGE i000(zfi01) WITH 'MM Invoice is blank' '' '' ''.
      ELSE.
        SET PARAMETER ID 'RBN' FIELD ls_output-mminv.
        SET PARAMETER ID 'BUK' FIELD ls_output-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_output-gjahr.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'AUGBL'.
*         IF p_e_column-fieldname = 'BELNR'.
      IF ls_output-augbl IS INITIAL.
        MESSAGE i000(zfi01) WITH 'Clearing document is blank' '' '' ''.
      ELSE.
        SET PARAMETER ID 'BLN' FIELD ls_output-augbl.
        SET PARAMETER ID 'BUK' FIELD ls_output-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_output-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.
*  ENDIF.
ENDFORM.                    " handle_user_command
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_OPEN_CLEARED
*&---------------------------------------------------------------------*
*       Open and cleared Item Data
*----------------------------------------------------------------------*
FORM get_data_open_cleared .



  DATA: fl_objkey  TYPE swotobjid-objkey,
        fl_objtype TYPE swotobjid-objtype,
        lt_swr_wihdr TYPE TABLE OF swr_wihdr,
        wa_swr_wihdr LIKE LINE OF lt_swr_wihdr,
        wa_swr_widtl TYPE swr_widtl,
        lt_swr_cont TYPE TABLE OF swr_cont,
        wa_swr_cont LIKE LINE OF lt_swr_cont,
        lt_task_wihdr TYPE TABLE OF swr_wihdr,
        wa_task_wihdr TYPE swr_wihdr.
  DATA: fl_ebeln TYPE rbdrseg-ebeln,
        fl_belnr TYPE bsik-belnr,
        fl_bukrs TYPE bsik-bukrs,
        fl_gjahr TYPE bsik-gjahr,
        fl_wi_rh_task TYPE swr_wihdr-wi_rh_task,
        fl_flag,
        fl_usnam TYPE rbkp-usnam.

*break sahmad.
*Vendor Open items
*  IF rg_open = 'X' OR
*     rg_all  = 'X'.
  SELECT * APPENDING TABLE lt_bsik FROM bsik
                              WHERE bukrs IN s_bukrs
                                AND lifnr IN s_lifnr
                                AND blart IN s_blart
                                AND budat IN s_budat.
*                                AND xblnr IN s_ebeln "PO
*                                AND xref3 IN s_xref3. "Non PO Reference 3
*  ENDIF.
*Vendor Cleared Items
*  IF rg_clear = 'X' OR
*     rg_all   = 'X'.
  SELECT * APPENDING TABLE lt_bsik FROM bsak
                              WHERE bukrs IN s_bukrs
                                AND lifnr IN s_lifnr
                                AND blart IN s_blart
                                AND budat IN s_budat.
*                                AND xblnr IN s_ebeln
*                                AND xref3 IN s_xref3.
*  ENDIF.
*  IF lt_bsik IS INITIAL.
*    WRITE: / 'No data for output for given selection criteria.'.
*    STOP.
*  ENDIF.
  lt_bsik1[] = lt_bsik[].
  DELETE ADJACENT DUPLICATES FROM lt_bsik1 COMPARING belnr.

  IF lt_bsik1 IS NOT INITIAL.
    SELECT belnr awkey gjahr bukrs FROM bkpf INTO CORRESPONDING FIELDS OF TABLE lt_typ_bkpf
                                   FOR ALL ENTRIES IN lt_bsik1 WHERE bukrs = lt_bsik1-bukrs
                                                                 AND belnr = lt_bsik1-belnr
                                                                 AND gjahr = lt_bsik1-gjahr.
  ENDIF.
  LOOP AT lt_bsik INTO wa_bsik.
*    break sahmad.
    CLEAR: wa_typ_bkpf,
           fl_ebeln,
           wa_output,
           fl_usnam.
    READ TABLE lt_typ_bkpf INTO wa_typ_bkpf WITH KEY belnr = wa_bsik-belnr
                                                     gjahr = wa_bsik-gjahr
                                                     bukrs = wa_bsik-bukrs.
    IF sy-subrc = 0.
      SELECT SINGLE usnam INTO fl_usnam FROM rbkp
        WHERE belnr = wa_typ_bkpf-awkey(10).
      IF sy-subrc = 0.
        SELECT SINGLE ebeln INTO fl_ebeln FROM rseg
        WHERE belnr = wa_typ_bkpf-awkey(10).
        "for PO base Invoice
        fl_objtype = 'BUS2081'.
        MOVE wa_typ_bkpf-awkey TO fl_objkey.
        fl_wi_rh_task = 'TS02000011'. "New one I got from Workflow " 'TS90000099'.
        fl_flag = 'I'.
      ELSE.
        "non PO invoice (Parked Documents)
        fl_objtype = 'FIPP'.
        fl_belnr = wa_typ_bkpf-awkey(10).
        fl_bukrs = wa_typ_bkpf-awkey+10(4).
        fl_gjahr = wa_typ_bkpf-awkey+14(4).
        CONCATENATE fl_bukrs fl_belnr fl_gjahr INTO fl_objkey RESPECTING BLANKS.
        fl_wi_rh_task = 'TS02000014'.
        fl_flag = 'C'.
      ENDIF.
    ENDIF.

    CLEAR: lt_swr_wihdr,
           lt_task_wihdr.
    PERFORM get_workflow  TABLES lt_swr_wihdr
                                 lt_task_wihdr
                          USING  fl_objtype
                                 fl_objkey
                                 fl_wi_rh_task.
    wa_output-bukrs = wa_bsik-bukrs.
    wa_output-lifnr = wa_bsik-lifnr.
    wa_output-ebeln = fl_ebeln. "wa_bsik-ebeln. PO
    IF fl_ebeln IS INITIAL.
      wa_output-xblnr = wa_bsik-xblnr.  "Vendor invoice refernce for NON PO
    ELSE.
      wa_output-mminv = wa_typ_bkpf-awkey(10). " MM Invoice number
    ENDIF.
    wa_output-blart = wa_bsik-blart.
    wa_output-belnr = wa_bsik-belnr.
    wa_output-wrbtr = wa_bsik-wrbtr.
    wa_output-augbl = wa_bsik-augbl.
    wa_output-gjahr = wa_bsik-gjahr.
    IF lt_swr_wihdr IS INITIAL.
      APPEND wa_output TO it_output.
    ENDIF.
*********************
    LOOP AT lt_swr_wihdr  INTO wa_swr_wihdr.

      CLEAR: wa_swr_widtl,
             wa_output-route_owner.
      PERFORM get_workitem USING wa_swr_wihdr-wi_id
                                 wa_swr_widtl.
      PERFORM get_container USING wa_swr_wihdr-wi_id
                                  wa_output-route_owner.
      PERFORM get_item_appr TABLES lt_task_wihdr
                             USING wa_output-wi_led
                                   wa_output-wi_aagent
                                   wa_output-wi_item_stat
                                   fl_flag
                                   wa_output-route_owner.
      IF fl_usnam IS NOT INITIAL.
        wa_output-wi_aagent = fl_usnam.
      ENDIF.
      wa_output-bukrs = wa_bsik-bukrs.
      wa_output-lifnr = wa_bsik-lifnr.
      wa_output-ebeln = fl_ebeln. "wa_bsik-ebeln.
      IF fl_ebeln IS INITIAL.
        wa_output-xblnr = wa_bsik-xblnr.  "Vendor invoice refernce for NON PO
      ELSE.
        wa_output-mminv = wa_typ_bkpf-awkey(10). " MM Invoice number
      ENDIF.
      wa_output-blart = wa_bsik-blart.
      wa_output-belnr = wa_typ_bkpf-awkey(10). "wa_bsik-belnr.
      wa_output-wrbtr = wa_bsik-wrbtr.
      wa_output-augbl = wa_bsik-augbl.
      wa_output-gjahr = wa_bsik-gjahr.
      wa_output-wi_id = wa_swr_wihdr-wi_id.
      wa_output-wi_cd = wa_swr_wihdr-wi_cd.
      wa_output-wi_creator = wa_swr_wihdr-wi_creator.
*      wa_output-wi_led = wa_swr_widtl-wi_led.
*      wa_output-wi_aagent = wa_swr_widtl-wi_aagent.
      IF wa_output-wi_led IS NOT INITIAL.
        wa_output-wi_days = wa_output-wi_led - wa_output-wi_cd.
      ELSE.
        wa_output-wi_days = space.
      ENDIF.
      wa_output-wi_stat = wa_swr_widtl-wi_stat.
*      APPEND wa_output TO it_workflow.
      APPEND wa_output TO it_output.
    ENDLOOP.
  ENDLOOP.
**********IF PO / REF3 used as selection criteria
  IF s_ebeln IS NOT INITIAL.
    DELETE it_output WHERE ebeln NOT IN s_ebeln.
  ENDIF.
  IF s_xref3 IS NOT INITIAL.
    DELETE it_output WHERE xblnr NOT IN s_xref3.
  ENDIF.
ENDFORM.                    " GET_DATA_OPEN_CLEARED
*&---------------------------------------------------------------------*
*&      Form  GET_PO_INVOICES
*&---------------------------------------------------------------------*
*       Get Data for PO based Invoices
*----------------------------------------------------------------------*
FORM get_po_invoices .

  DATA: fl_objkey  TYPE swotobjid-objkey,
        fl_objtype TYPE swotobjid-objtype,
        lt_swr_wihdr TYPE TABLE OF swr_wihdr,
        wa_swr_wihdr LIKE LINE OF lt_swr_wihdr,
        wa_swr_widtl TYPE swr_widtl,
        lt_task_wihdr TYPE TABLE OF swr_wihdr.
  DATA: lt_rbkp TYPE TABLE OF ty_output,     "PO base Invoice Data
        wa_rbkp LIKE LINE OF lt_rbkp, " rbkp,
        lt_vbkpf TYPE TABLE OF ty_output, "vbkpf,  "non PO base parked document
        wa_vbkpf LIKE LINE OF lt_vbkpf,
        fl_wi_rh_task TYPE swr_wihdr-wi_rh_task,
        lt_bkpf TYPE TABLE OF bkpf,
        fl_awkey TYPE bkpf-awkey,
        fl_flag.

*  break sahmad.
*Invoice verification (MM Invoice) data, PO Base Invoices
  SELECT a~bukrs b~lifnr b~ebeln a~blart a~gjahr a~belnr b~wrbtr INTO CORRESPONDING FIELDS OF TABLE
         lt_rbkp FROM rbkp AS a INNER JOIN rseg AS b ON a~belnr = b~belnr
                                                    AND a~gjahr = b~gjahr
                                WHERE a~bukrs IN s_bukrs
                                  AND b~lifnr IN s_lifnr
                                  AND a~blart IN s_blart
                                  AND a~budat IN s_budat
                                  AND b~ebeln IN s_ebeln.
  fl_objtype = 'BUS2081'. "'FIPP'.
  fl_wi_rh_task = 'TS02000011'.
  LOOP AT lt_rbkp INTO wa_rbkp.
    CLEAR wa_output.
*IF there is open / cleared item, don't include this document in output.
    CONCATENATE wa_rbkp-belnr wa_rbkp-gjahr INTO fl_awkey RESPECTING BLANKS.
    READ TABLE lt_typ_bkpf WITH KEY  awkey = fl_awkey
                                     bukrs = wa_rbkp-bukrs TRANSPORTING NO FIELDS.
*    awkey(10) = wa_rbkp-belnr
*    bukrs = wa_rbkp-bukrs
*    gjahr = wa_rbkp-gjahr TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    CLEAR lt_bkpf.
*    SELECT * INTO TABLE lt_bkpf FROM bkpf WHERE awkey = fl_awkey
*                                            AND bukrs = wa_rbkp-bukrs.
*    IF sy-subrc = 0.
*      fl_flag = 'C'. "completed
*    ELSE.
*     fl_flag = 'I'. "
*    ENDIF.
    CONCATENATE wa_rbkp-belnr wa_rbkp-gjahr INTO fl_objkey RESPECTING BLANKS.
    CLEAR: lt_swr_wihdr,
           lt_task_wihdr.
    PERFORM get_workflow  TABLES lt_swr_wihdr
                                 lt_task_wihdr
                           USING  fl_objtype
                                  fl_objkey
                                  fl_wi_rh_task.
    wa_output-bukrs = wa_rbkp-bukrs.
    wa_output-lifnr = wa_rbkp-lifnr.
    wa_output-ebeln = wa_rbkp-ebeln. "PO
    wa_output-blart = wa_rbkp-blart.
    wa_output-belnr = wa_rbkp-belnr.
    wa_output-mminv = wa_rbkp-belnr.
    wa_output-wrbtr = wa_rbkp-wrbtr.
    wa_output-gjahr = wa_rbkp-gjahr.
    IF lt_swr_wihdr IS INITIAL.
      APPEND wa_output TO it_output.
    ENDIF.
    LOOP AT lt_swr_wihdr  INTO wa_swr_wihdr.

      CLEAR: wa_swr_widtl,
             wa_output-route_owner.
      PERFORM get_workitem USING wa_swr_wihdr-wi_id
                                 wa_swr_widtl.
      PERFORM get_container USING wa_swr_wihdr-wi_id
                                  wa_output-route_owner.
      PERFORM get_item_appr TABLES lt_task_wihdr
                             USING wa_output-wi_led
                                   wa_output-wi_aagent
                                   wa_output-wi_item_stat
                                   'I' "fl_flag
                                   wa_output-route_owner.
      wa_output-bukrs = wa_rbkp-bukrs.
      wa_output-lifnr = wa_rbkp-lifnr.
      wa_output-ebeln = wa_rbkp-ebeln. "PO
      wa_output-blart = wa_rbkp-blart.
      wa_output-belnr = wa_rbkp-belnr.
      wa_output-mminv = wa_rbkp-belnr.
      wa_output-wrbtr = wa_rbkp-wrbtr.
      wa_output-gjahr = wa_rbkp-gjahr.
      wa_output-wi_id = wa_swr_wihdr-wi_id.
      wa_output-wi_cd = wa_swr_wihdr-wi_cd.
      wa_output-wi_creator = wa_swr_wihdr-wi_creator.
*      wa_output-wi_led = wa_swr_widtl-wi_led.
*      wa_output-wi_aagent = wa_swr_widtl-wi_aagent.
      IF wa_output-wi_led IS NOT INITIAL.
        wa_output-wi_days = wa_output-wi_led - wa_output-wi_cd.
      ELSE.
        wa_output-wi_days = space.
      ENDIF.
      wa_output-wi_stat = wa_swr_widtl-wi_stat.
*      APPEND wa_output TO it_workflow.
      APPEND wa_output TO it_output.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " GET_PO_NONPO_INVOICES
*&---------------------------------------------------------------------*
*&      Form  GET_CONTAINER
*&---------------------------------------------------------------------*
*       Get Route code owner from Workflow
*----------------------------------------------------------------------*
FORM get_container  USING    p_wi_id TYPE swr_wihdr-wi_id
                             p_route_owner.

  DATA: lt_swr_cont TYPE TABLE OF swr_cont,
        ls_swr_cont LIKE LINE OF lt_swr_cont.

  CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
       EXPORTING
         workitem_id                    =  p_wi_id
*           LANGUAGE                       = SY-LANGU
*           USER                           = SY-UNAME
*         IMPORTING
*           RETURN_CODE                    =
*           IFS_XML_CONTAINER              =
*           IFS_XML_CONTAINER_SCHEMA       =
      TABLES
        simple_container               = lt_swr_cont
*           MESSAGE_LINES                  =
*           MESSAGE_STRUCT                 =
*           SUBCONTAINER_BOR_OBJECTS       =
*           SUBCONTAINER_ALL_OBJECTS       =
               .

  LOOP AT lt_swr_cont INTO ls_swr_cont
                      WHERE element = 'ROUTECODEOWNER'. "Workflow Route Owner, while step route owner element is RCOWNER
    p_route_owner = ls_swr_cont-value.
  ENDLOOP.
ENDFORM.                    " GET_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  GET_WORKITEM
*&---------------------------------------------------------------------*
*       Get Workflow Item Details
*----------------------------------------------------------------------*
FORM get_workitem  USING    p_wi_id TYPE swr_wihdr-wi_id
                            p_swr_widtl TYPE swr_widtl.
*  break sahmad.
  CALL FUNCTION 'SAP_WAPI_GET_WORKITEM_DETAIL'
          EXPORTING
            workitem_id           = p_wi_id
*                USER                  = SY-UNAME
*                LANGUAGE              = SY-LANGU
         IMPORTING
            workitem_detail       = p_swr_widtl
*                RETURN_CODE           =
*              TABLES
*                MESSAGE_LINES         =
*                MESSAGE_STRUCT        =
                  .

ENDFORM.                    " GET_WORKITEM
*&---------------------------------------------------------------------*
*&      Form  GET_WORKFLOW
*&---------------------------------------------------------------------*
*       Get Workflow Data
*----------------------------------------------------------------------*
FORM get_workflow  TABLES   p_lt_swr_wihdr  STRUCTURE swr_wihdr
                            p_lt_task_wihdr STRUCTURE swr_wihdr
                   USING    p_fl_objtype TYPE swotobjid-objtype
                            p_fl_objkey  TYPE swotobjid-objkey
                            p_task_id    TYPE swr_wihdr-wi_rh_task.

  DATA: lv_wi_creator TYPE swr_wihdr-wi_creator VALUE 'FILENET'.

  break sahmad.
  CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
       EXPORTING
*           OBJECT_POR                     =
          objtype                        = p_fl_objtype
          objkey                         = p_fl_objkey
           top_level_items                = 'X'
             selection_status_variant       = 0000        "All instances of workflow
*           TIME                           =
*           TEXT                           = 'X'
*           OUTPUT_ONLY_TOP_LEVEL          = ' '
*           LANGUAGE                       = SY-LANGU
*           DETERMINE_TASK_FILTER          = 'X'
*           REMOVED_OBJECTS                = ' '
*         IMPORTING
*           RETURN_CODE                    =
        TABLES
*           TASK_FILTER                    =
          worklist                       = p_lt_swr_wihdr
*           MESSAGE_LINES                  =
*           MESSAGE_STRUCT                 =
                .
  CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
       EXPORTING
*           OBJECT_POR                     =
          objtype                        = p_fl_objtype
          objkey                         = p_fl_objkey
           top_level_items                = ' '
             selection_status_variant       = 0000        "All instances of workflow
*           TIME                           =
*           TEXT                           = 'X'
*           OUTPUT_ONLY_TOP_LEVEL          = ' '
*           LANGUAGE                       = SY-LANGU
*           DETERMINE_TASK_FILTER          = 'X'
*           REMOVED_OBJECTS                = ' '
*         IMPORTING
*           RETURN_CODE                    =
        TABLES
*           TASK_FILTER                    =
          worklist                       = p_lt_task_wihdr
*           MESSAGE_LINES                  =
*           MESSAGE_STRUCT                 =
                .
  DELETE p_lt_task_wihdr WHERE wi_rh_task <> p_task_id.
  DELETE p_lt_swr_wihdr  WHERE wi_creator = lv_wi_creator OR
                               wi_rh_task CP 'TS*'.

ENDFORM.                    " GET_WORKFLOW
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       process collected data
*----------------------------------------------------------------------*
FORM process_data .

  IF s_comp = 'X'.
    DELETE it_output WHERE wi_item_stat <> 'COMPLETED'.
  ENDIF.
  IF s_rco = 'X'.
    DELETE it_output WHERE wi_item_stat <> 'ROUTE_CODE_OWNER'.
  ENDIF.
  IF s_dao = 'X'.
    DELETE it_output WHERE wi_item_stat <> 'DOA_APPROVER'.
  ENDIF.
  IF s_ebeln IS NOT INITIAL.
    DELETE it_output WHERE ebeln NOT IN s_ebeln.
  ENDIF.
  LOOP AT it_output INTO wa_output.
    CLEAR: wa_lfa1.
    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_output-lifnr.
    CHECK sy-subrc = 0.
    wa_output-name1 = wa_lfa1-name1.
    MODIFY it_output FROM wa_output TRANSPORTING name1.
  ENDLOOP.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ITEM_APPR
*&---------------------------------------------------------------------*
*       Get document approval data, approved by and item status
*----------------------------------------------------------------------*
FORM get_item_appr  TABLES   p_lt_task_wihdr STRUCTURE swr_wihdr
                    USING    p_wi_led TYPE swr_widtl-wi_led
                             p_wi_aagent TYPE swr_widtl-wi_aagent
                             p_wi_item_stat
                             p_flag
                             p_route_owner.

  DATA: wa_task_wihdr TYPE swr_wihdr.

*break sahmad.
  SORT p_lt_task_wihdr BY wi_cd wi_ct ASCENDING.

  LOOP AT p_lt_task_wihdr INTO wa_task_wihdr.
  ENDLOOP.
  p_wi_aagent = wa_task_wihdr-wi_aagent.
  p_wi_led    = wa_task_wihdr-wi_cd.
*Non PO Base
  IF p_flag = 'C' AND                    "Posted
     wa_task_wihdr-wi_stat = 'COMPLETED'.
    p_wi_item_stat = 'COMPLETED'.
  ELSEIF p_flag = 'C' AND
          wa_task_wihdr-wi_stat <> 'COMPLETED'.
    IF p_route_owner = wa_task_wihdr-wi_aagent.
      p_wi_item_stat = 'ROUTE_CODE_OWNER'.
    ELSE.
      p_wi_item_stat = 'DOA_APPROVER'.
    ENDIF.
  ENDIF.
****************PO base Invoice
  IF p_flag = 'I'.
    p_wi_item_stat = wa_task_wihdr-wi_stat.
  ENDIF.
ENDFORM.                    " GET_ITEM_APPR
*&---------------------------------------------------------------------*
*&      Form  GET_PARKED_DOCUMENTS
*&---------------------------------------------------------------------*
*       Get Parked Documents
*----------------------------------------------------------------------*
FORM get_parked_documents .
  DATA: fl_objkey  TYPE swotobjid-objkey,
           fl_objtype TYPE swotobjid-objtype,
           lt_swr_wihdr TYPE TABLE OF swr_wihdr,
           wa_swr_wihdr LIKE LINE OF lt_swr_wihdr,
           wa_swr_widtl TYPE swr_widtl,
           lt_task_wihdr TYPE TABLE OF swr_wihdr.
  DATA: lt_rbkp TYPE TABLE OF ty_output, "rbkp,    "PO base Invoice Data
        wa_rbkp LIKE LINE OF lt_rbkp, " rbkp,
        lt_vbkpf TYPE TABLE OF ty_output, "vbkpf,  "non PO base parked document
        wa_vbkpf LIKE LINE OF lt_vbkpf,
        fl_wi_rh_task TYPE swr_wihdr-wi_rh_task,
        fl_belnr TYPE vbkpf-belnr,
        fl_gjahr TYPE vbkpf-gjahr,
        fl_flag,
        fl_usnam TYPE rbkp-usnam.

*  break sahmad.
*Parked Documents (Non PO Base Invoices)
  SELECT a~bukrs b~lifnr a~blart a~gjahr a~belnr b~wrbtr a~xblnr a~awkey a~awtyp
        INTO CORRESPONDING FIELDS OF TABLE lt_vbkpf FROM vbkpf AS a
             INNER JOIN vbsegk AS b ON a~belnr = b~belnr
                                   AND a~gjahr = b~gjahr
                                 WHERE a~bukrs IN s_bukrs
                                   AND b~lifnr IN s_lifnr
                                   AND a~blart IN s_blart
                                   AND a~budat IN s_budat
                                   AND b~xref3 IN s_xref3.

  LOOP AT lt_vbkpf INTO wa_vbkpf.
    CLEAR: wa_output,
           fl_usnam.
    fl_belnr = wa_vbkpf-awkey(10).
    fl_gjahr = wa_vbkpf-awkey+10(4).

    SELECT SINGLE usnam INTO fl_usnam FROM rbkp
        WHERE belnr = fl_belnr
          AND gjahr = fl_gjahr.
*    if wa_vbkpf-awtyp = 'RMRP'.
    IF sy-subrc = 0.
      SELECT SINGLE ebeln INTO wa_output-ebeln FROM rseg WHERE belnr = fl_belnr
                                                           AND gjahr = fl_gjahr.
      fl_objtype = 'BUS2081'.
      fl_wi_rh_task = 'TS02000011'.
*        CONCATENATE fl_belnr fl_gjahr INTO fl_objkey RESPECTING BLANKS.
      fl_objkey =  wa_vbkpf-awkey.
      wa_output-mminv = fl_belnr.
      fl_flag = 'I'.
    ELSE.
      fl_objtype = 'FIPP'.
      fl_wi_rh_task = 'TS02000014'.
      CONCATENATE wa_vbkpf-bukrs wa_vbkpf-belnr wa_vbkpf-gjahr INTO fl_objkey RESPECTING BLANKS.
      fl_flag = 'C'.
    ENDIF.
    CLEAR: lt_swr_wihdr,
            lt_task_wihdr.

    PERFORM get_workflow  TABLES lt_swr_wihdr
                                 lt_task_wihdr
                          USING  fl_objtype
                                 fl_objkey
                                 fl_wi_rh_task.
    wa_output-bukrs = wa_vbkpf-bukrs.
    wa_output-lifnr = wa_vbkpf-lifnr.
    wa_output-xblnr = wa_vbkpf-xblnr.  "Vendor invoice refernce for NON PO
    wa_output-blart = wa_vbkpf-blart.
    wa_output-belnr = wa_vbkpf-belnr.
    wa_output-wrbtr = wa_vbkpf-wrbtr.
    wa_output-gjahr = wa_vbkpf-gjahr.
    IF lt_swr_wihdr IS INITIAL.
      APPEND wa_output TO it_output.
    ENDIF.
    LOOP AT lt_swr_wihdr  INTO wa_swr_wihdr.

      CLEAR: wa_swr_widtl,
             wa_output-route_owner.
      PERFORM get_workitem USING wa_swr_wihdr-wi_id
                                 wa_swr_widtl.
      PERFORM get_container USING wa_swr_wihdr-wi_id
                                  wa_output-route_owner.
      PERFORM get_item_appr TABLES lt_task_wihdr
                             USING wa_output-wi_led
                                   wa_output-wi_aagent
                                   wa_output-wi_item_stat
                                   fl_flag "'C'
                                   wa_output-route_owner.
      IF fl_usnam IS NOT INITIAL.
        wa_output-wi_aagent = fl_usnam.
      ENDIF.
*      wa_output-bukrs = wa_vbkpf-bukrs.
*      wa_output-lifnr = wa_vbkpf-lifnr.
*      wa_output-xblnr = wa_vbkpf-xblnr.  "Vendor invoice refernce for NON PO
*      wa_output-blart = wa_vbkpf-blart.
*      wa_output-belnr = wa_vbkpf-belnr.
*      wa_output-wrbtr = wa_vbkpf-wrbtr.
*      wa_output-gjahr = wa_vbkpf-gjahr.
      wa_output-wi_id = wa_swr_wihdr-wi_id.
      wa_output-wi_cd = wa_swr_wihdr-wi_cd.
      wa_output-wi_creator = wa_swr_wihdr-wi_creator.
*      wa_output-wi_led = wa_swr_widtl-wi_led.
*      wa_output-wi_aagent = wa_swr_widtl-wi_aagent.
      IF wa_output-wi_led IS NOT INITIAL.
        wa_output-wi_days = wa_output-wi_led - wa_output-wi_cd.
      ELSE.
        wa_output-wi_days = space.
      ENDIF.
      wa_output-wi_stat = wa_swr_widtl-wi_stat.
*      APPEND wa_output TO it_workflow.
      APPEND wa_output TO it_output.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " GET_PARKED_DOCUMENTS
