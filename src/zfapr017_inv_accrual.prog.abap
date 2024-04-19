*&---------------------------------------------------------------------*
*& Report  ZFAPR017_INV_ACCRUAL
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
************************************************************************
*  Author:      Sajjad Ahmad                                           *
*  Date:        March 19, 2012.                                        *
*  Issue Log:                                                          *
*  Description:                                                        *
*     -
************************************************************************
*CHANGES:                                                              *
************************************************************************
REPORT  zfapr017_inv_accrual.
*&---------------------------------------------------------------------*
*&       Class LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*       Double Click Event Handler
*----------------------------------------------------------------------*
*CLASS lcl_event_handler DEFINITION.
*  PUBLIC SECTION.
*    METHODS: handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid
*                                  IMPORTING e_row e_column es_row_no,
*             handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
*                                  IMPORTING e_row_id e_column_id es_row_no.
*ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*"LCL_EVENT_HANDLER
**&---------------------------------------------------------------------*
**&       Class (Implementation)  lcl_event_handler
**&---------------------------------------------------------------------*
**        Double Click Event Handler Implementation
**----------------------------------------------------------------------*
*CLASS lcl_event_handler IMPLEMENTATION.
*  METHOD handle_double_click.
*    PERFORM handle_double_click USING e_row e_column
*                                      es_row_no.
*  ENDMETHOD.                    "handle_double_click
*  METHOD handle_hotspot_click.
*    PERFORM handle_double_click USING e_row_id e_column_id
*                                      es_row_no.
*  ENDMETHOD.                    "handle_hotspot_click
*ENDCLASS.               "lcl_event_handler
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: hotspot_click  FOR EVENT link_click OF cl_salv_events_table
                         IMPORTING row column.
ENDCLASS.               "LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_handler
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD hotspot_click.
    PERFORM handle_click USING row column.
  ENDMETHOD.                    "hotspot_click1
ENDCLASS.               "lcl_event_handler
TABLES: vbkpf,
        lfa1,
        vbsegs,
        vbsegk,
        rseg,
        ekko,
        bsik,
        butp_tp.
TYPES: BEGIN OF ty_output,
         bukrs TYPE vbkpf-ausbk,
         lifnr TYPE bsik-lifnr,
         name1 TYPE lfa1-name1,
         ebeln TYPE rseg-ebeln, "PO
         zzariba_approver TYPE ekko-zzariba_approver,
         xblnr TYPE vbkpf-xblnr, "Vendor Inv
         blart TYPE vbkpf-blart,
         gjahr TYPE vbkpf-gjahr,
         belnr TYPE vbkpf-belnr,
         mminv TYPE vbkpf-belnr,
         ekgrp TYPE ekko-ekgrp,
         werks TYPE rseg-werks,
         wrbtr TYPE vbsegk-wrbtr,
         shkzg  TYPE vbsegk-shkzg,
         venamt TYPE vbsegk-wrbtr,
         waers TYPE vbkpf-waers,
         awkey TYPE vbkpf-awkey,
         awtyp TYPE vbkpf-awtyp,
         nplnr TYPE vbsegs-nplnr,
         vornr TYPE vbsegs-vornr,
         kostl TYPE vbsegs-kostl,
         saknr TYPE vbsegs-saknr,
         projk TYPE vbsegs-ps_psp_pnr,
         aufnr TYPE vbsegs-aufnr,
         mwskz TYPE vbsegs-mwskz,
         xref3 TYPE vbsegk-xref3,
         wi_id TYPE swr_wihdr-wi_id, "Work item
         wi_idc(12) TYPE c, "Work item for display
         wi_cd TYPE swr_wihdr-wi_cd, "Work Item Creation Date
*Work item creator / Document Parked by
         wi_creator TYPE swr_wihdr-wi_creator, "
         route_owner(35),
         finalapp(35),
         wi_led TYPE swr_widtl-wi_led, "Document Approval Date
         wi_aagent TYPE swr_widtl-wi_aagent,
         wi_forw_by TYPE swr_widtl-wi_forw_by, "last forwarder of work item
         wi_days TYPE i, "difference b/w WI creation and compl date
         wi_stat TYPE swr_widtl-wi_stat, "Work Item Status
         wi_item_stat(35),
         tnotes  TYPE i,
         cronam  TYPE so_cro_nam,
         recnam  TYPE tdline,
         note1   TYPE tdline,
         note2   TYPE tdline,

       END OF ty_output.
TYPES: BEGIN OF ty_notes_output,
       bukrs TYPE vbkpf-ausbk,
       gjahr TYPE vbkpf-gjahr,
       belnr TYPE vbkpf-belnr,
       blart TYPE vbkpf-blart,
       recnam TYPE  tdline,
       cronam TYPE  so_cro_nam,
       crdat  TYPE  so_dat_cr,
       crtim  TYPE  so_tim_cr,
       note1  TYPE  tdline,
       note2  TYPE  tdline,
       END OF ty_notes_output.
TYPES: BEGIN OF ty_rcowner,
        owner TYPE  username,
        route_code  TYPE  xref3,
        bukrs TYPE  bukrs,
       END OF ty_rcowner.
DATA: it_output TYPE TABLE OF ty_output,
      wa_output LIKE LINE  OF it_output,
      it_output1 LIKE it_output WITH HEADER LINE,
      gv_log_dest TYPE tb_rfcdest,
      gt_rc_list TYPE TABLE OF zfi_rcowner_list,
      gt_rcowner TYPE TABLE OF ty_rcowner,
      gt_notes TYPE zso_notes_tab,
      gt_notes_output TYPE TABLE OF ty_notes_output,
      gt_notes_output1 TYPE TABLE OF ty_notes_output,
      gs_notes_output TYPE ty_notes_output,
      gt_output_notes1 LIKE gt_notes_output WITH HEADER LINE,
      gv_flag,
      gv_check_po,
      gv_check_nonpo,
      gt_comp_tab TYPE TABLE OF rstrucinfo WITH HEADER LINE,
      gt_comp_tab1 TYPE TABLE OF rstrucinfo WITH HEADER LINE.
*      it_workflow TYPE TABLE OF ty_output.
DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE ok_code.
DATA: gr_container TYPE REF TO cl_gui_custom_container,
      gr_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_layout    TYPE lvc_s_layo.
DATA: gr_event_handler TYPE REF TO lcl_event_handler.
DATA: gv_vari TYPE disvariant.

*************SELECTION SCREEN************************************
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_bukrs  FOR  vbkpf-ausbk OBLIGATORY,
                s_budat  FOR  vbkpf-budat,
                s_blart  FOR  vbkpf-blart,
                s_lifnr  FOR  bsik-lifnr.
SELECTION-SCREEN END OF BLOCK box1.
SELECTION-SCREEN BEGIN OF BLOCK box5 WITH FRAME TITLE text-005.
SELECT-OPTIONS:  s_ebeln  FOR  rseg-ebeln,    "for PO
                 s_ekgrp  FOR  ekko-ekgrp,
                 s_werks  FOR  rseg-werks.
SELECTION-SCREEN END OF BLOCK box5.
SELECTION-SCREEN BEGIN OF BLOCK box6 WITH FRAME TITLE text-006.
SELECT-OPTIONS:  s_xref3  FOR  vbsegk-xref3,  "for NON PO
                 s_rcown  FOR  butp_tp-intbankacc.
SELECTION-SCREEN END OF BLOCK box6.
SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-003.
PARAMETERS: r_allcs  RADIOBUTTON GROUP grp2 DEFAULT 'X' USER-COMMAND cmd,
            r_kostl  RADIOBUTTON GROUP grp2 ,
            r_nplnr  RADIOBUTTON GROUP grp2 ,
            r_projk  RADIOBUTTON GROUP grp2 ,
            r_aufnr  RADIOBUTTON GROUP grp2 ,
            r_saknr  RADIOBUTTON GROUP grp2 .
SELECTION-SCREEN END OF BLOCK box3.
SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-004.
SELECT-OPTIONS: s_kostl  FOR  vbsegs-kostl MODIF ID kst,
                s_nplnr  FOR  vbsegs-nplnr MODIF ID npl,
                s_vornr  FOR  vbsegs-vornr MODIF ID npl,
                s_projk  FOR  vbsegs-ps_psp_pnr MODIF ID prj,
                s_aufnr  FOR  vbsegs-aufnr MODIF ID auf,
                s_saknr  FOR  vbsegs-saknr MODIF ID sak.
SELECTION-SCREEN END OF BLOCK box4.
*SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
*PARAMETERS: s_all  RADIOBUTTON GROUP grp1 DEFAULT 'X',
*            s_comp RADIOBUTTON GROUP grp1,
*            s_rco  RADIOBUTTON GROUP grp1,
*            s_dao  RADIOBUTTON GROUP grp1.
*SELECTION-SCREEN END OF BLOCK box2.
SELECTION-SCREEN BEGIN OF BLOCK box7 WITH FRAME TITLE text-007.
PARAMETERS: p_vari TYPE disvariant-variant DEFAULT '/DEFAULT'.
SELECTION-SCREEN END OF BLOCK box7.
*SELECTION-SCREEN BEGIN OF BLOCK box5 WITH FRAME TITLE text-005.
*PARAMETERS: p_VARI type DISVARIANT default '/DEFAULT'.
*SELECTION-SCREEN END OF BLOCK box5.
**************END OF SELECTION SCREEN*******************************
INITIALIZATION.

*Get Logical Destination
  CALL FUNCTION 'ZFI_GET_LOGICAL_DEST'
    EXPORTING
      imp_paramtype = 'ECCUS'
    IMPORTING
      exp_rfcdest   = gv_log_dest.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_xref3-low.
  IF  gv_log_dest IS NOT INITIAL.
    PERFORM get_rc_xref.
    PERFORM f4_output USING '1'.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_xref3-high.
  IF  gv_log_dest IS NOT INITIAL.
    PERFORM get_rc_xref.
    PERFORM f4_output USING '2'.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_rcown-low.
  IF  gv_log_dest IS NOT INITIAL.
    PERFORM get_rc_xref.
    PERFORM f4_output USING '3'.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_rcown-high.
  IF  gv_log_dest IS NOT INITIAL.
    PERFORM get_rc_xref.
    PERFORM f4_output USING '4'.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  PERFORM disable_enable_cost_objects.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM variant_help.

START-OF-SELECTION.

  CLEAR: it_output,
         wa_output,
         gv_flag,
         gv_check_po,
         gv_check_nonpo,
         gt_notes_output,
         gs_notes_output.

  IF s_ebeln IS NOT INITIAL OR
     s_ekgrp IS NOT INITIAL OR
     s_werks IS NOT INITIAL.
    gv_flag = 'I'.
    gv_check_po = 'X'.
  ENDIF.
  IF s_xref3 IS NOT INITIAL OR
     s_rcown IS NOT INITIAL.
    gv_flag = 'C'.
    gv_check_nonpo = 'X'.
  ENDIF.
  IF gv_check_po IS NOT INITIAL AND
     gv_check_nonpo IS NOT INITIAL.
    WRITE : / 'PO and Non-PO parameters are not allowed together'.
    STOP.
  ENDIF.
**************Get Components
  PERFORM get_components.
***************
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 15
      text       = 'Data extraction is in progress.'.
  PERFORM get_parked_documents.
*****************
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 80
      text       = 'Final Processing of data and preparing for output'.
  PERFORM process_data.
*  PERFORM perform_alv.
  PERFORM perform_salv.
*&---------------------------------------------------------------------*
*&      Form  PERFORM_ALV
*&---------------------------------------------------------------------*
FORM perform_alv .

*  CREATE OBJECT gr_alvgrid
*    EXPORTING
*      i_parent = cl_gui_container=>screen0. "gr_container
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*****Create and register double click event
*  CREATE OBJECT gr_event_handler.
*  SET HANDLER: gr_event_handler->handle_double_click
*               gr_event_handler->handle_hotspot_click
*               FOR gr_alvgrid.
***********
*  PERFORM field_catalog.
*  PERFORM prepare_layout.
*
*  gv_vari-variant = p_vari.
*  gv_vari-report  = sy-repid.
*  CALL METHOD gr_alvgrid->set_table_for_first_display
*    EXPORTING
**     I_BUFFER_ACTIVE               =
**     I_BYPASSING_BUFFER            =
**     I_CONSISTENCY_CHECK           =
**     I_STRUCTURE_NAME              =
*      is_variant                    = gv_vari
*      i_save                        = 'A'
**     I_DEFAULT                     = 'X'
*      is_layout                     = gs_layout
**     IS_PRINT                      =
**     IT_SPECIAL_GROUPS             =
**     IT_TOOLBAR_EXCLUDING          =
**     IT_HYPERLINK                  =
**     IT_ALV_GRAPHICS               =
**     IT_EXCEPT_QINFO               =
**     IR_SALV_ADAPTER               =
*    CHANGING
*      it_outtab                     = it_output[]
*      it_fieldcatalog               = gt_fieldcat
**     IT_SORT                       =
**     IT_FILTER                     =
*    EXCEPTIONS
*      invalid_parameter_combination = 1
*      program_error                 = 2
*      too_many_lines                = 3
*      OTHERS                        = 4.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*  CALL SCREEN 100.
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
  ls_fcat-ref_table = 'VBKPF'.
  ls_fcat-ref_field = 'AUSBK'.
  ls_fcat-outputlen = '4'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'LIFNR'.
  ls_fcat-ref_table = 'VBSEGK'.
  ls_fcat-ref_field = 'LIFNR'.
  ls_fcat-outputlen = '5'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'NAME1'.
  ls_fcat-ref_table = 'LFA1'.
  ls_fcat-ref_field = 'NAME1'.
  ls_fcat-outputlen = '35'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'EBELN'.
  ls_fcat-ref_table = 'RSEG'.
  ls_fcat-ref_field = 'EBELN'.
  ls_fcat-outputlen = '10'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ZZARIBA_APPROVER'.
  ls_fcat-ref_table = 'EKKO'.
  ls_fcat-ref_field = 'ZZARIBA_APPROVER'.
  ls_fcat-outputlen = '20'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'BLART'.
  ls_fcat-ref_table = 'VBKPF'.
  ls_fcat-ref_field = 'BLART'.
  ls_fcat-coltext = 'DocTy'.
  ls_fcat-seltext = 'DocTy'.
  ls_fcat-outputlen = '3'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'BELNR'.
  ls_fcat-ref_table = 'VBKPF'.
  ls_fcat-ref_field = 'BELNR'.
  ls_fcat-outputlen = '10'.
*  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = ''.
*  ls_fcat-seltext = ''.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'XBLNR'.
  ls_fcat-ref_table = 'VBKPF'.
  ls_fcat-ref_field = 'XBLNR'.
  ls_fcat-outputlen = '10'.
  ls_fcat-coltext = 'Reference'.
  ls_fcat-seltext = 'Reference'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'MMINV'.
  ls_fcat-ref_table = 'VBKPF'.
  ls_fcat-ref_field = 'BELNR'.
  ls_fcat-outputlen = '10'.
  ls_fcat-coltext = 'MM Invoice'.
  ls_fcat-seltext = 'MM Invoice'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'MWSKZ'.
  ls_fcat-ref_table = 'VBSEGS'.
  ls_fcat-ref_field = 'MWSKZ'.
  ls_fcat-outputlen = '4'.
*  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'SAKNR'.
  ls_fcat-ref_table = 'VBSEGS'.
  ls_fcat-ref_field = 'SAKNR'.
  ls_fcat-outputlen = '10'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'KOSTL'.
  ls_fcat-ref_table = 'RBCO'.
  ls_fcat-ref_field = 'KOSTL'.
  ls_fcat-outputlen = '10'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'NPLNR'.
  ls_fcat-ref_table = 'RBCO'.
  ls_fcat-ref_field = 'NPLNR'.
  ls_fcat-outputlen = '12'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'VORNR'.
  ls_fcat-ref_table = 'RBCO'.
  ls_fcat-ref_field = 'VORNR'.
  ls_fcat-outputlen = '4'.
*  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'PROJK'.
  ls_fcat-ref_table = 'VBSEGS'.
  ls_fcat-ref_field = 'PS_PSP_PNR'.
  ls_fcat-outputlen = '14'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'AUFNR'.
  ls_fcat-ref_table = 'VBSEGS'.
  ls_fcat-ref_field = 'AUFNR'.
  ls_fcat-outputlen = '12'.
  ls_fcat-hotspot   = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'EKGRP'.
  ls_fcat-ref_table = 'EKKO'.
  ls_fcat-ref_field = 'EKGRP'.
  ls_fcat-outputlen = '4'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WERKS'.
  ls_fcat-ref_table = 'RSEG'.
  ls_fcat-ref_field = 'WERKS'.
  ls_fcat-outputlen = '4'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ROUTE_OWNER'.
  ls_fcat-outputlen = '15'.
  ls_fcat-coltext = 'RC Owner'.
  ls_fcat-seltext = 'Route Code Owner'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.
*added by rajamans
  ls_fcat-fieldname = 'FINALAPP'.
  ls_fcat-outputlen = '15'.
  ls_fcat-key_sel = 'X'.
  ls_fcat-coltext = 'Final Approver'.
  ls_fcat-seltext = 'Final Approver'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.
*additiion ends here
*  ls_fcat-fieldname = 'WI_ID'.
*  ls_fcat-ref_table = 'SWR_WIHDR'.
*  ls_fcat-ref_field = 'WI_ID'.
*  ls_fcat-coltext = 'Workflow ID'.
*  ls_fcat-seltext = 'Workflow ID'.
*  ls_fcat-outputlen = '12'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_IDC'.
  ls_fcat-inttype = 'C'.
  ls_fcat-intlen = '12'.
  ls_fcat-coltext = 'Workflow ID'.
  ls_fcat-seltext = 'Workflow ID'.
  ls_fcat-outputlen = '12'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_CREATOR'.
  ls_fcat-ref_table = 'SWR_WIHDR'.
  ls_fcat-ref_field = 'WI_CREATOR'.
  ls_fcat-coltext = 'Parked By'.
  ls_fcat-seltext = 'Parked By'.
  ls_fcat-outputlen = '12'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_AAGENT'.
  ls_fcat-ref_table = 'SWR_WIDTL'.
  ls_fcat-ref_field = 'WI_AAGENT'.
  ls_fcat-coltext = 'Act Agent of current Task'. "'Approved By'.
  ls_fcat-seltext = 'Act Agent of current Task'.
  ls_fcat-outputlen = '12'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_FORW_BY'.
  ls_fcat-ref_table = 'SWR_WIDTL'.
  ls_fcat-ref_field = 'WI_FORW_BY'.
*  ls_fcat-coltext = ''.
* ls_fcat-seltext = ''.
  ls_fcat-outputlen = '12'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_ITEM_STAT'.
*ls_fcat-ref_table = ''.
*  ls_fcat-ref_field = 'WI_STAT'.
  ls_fcat-outputlen = '20'.
  ls_fcat-coltext = 'Status'.
  ls_fcat-seltext = 'Status'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WRBTR'.
  ls_fcat-ref_table = 'VBSEGK'.
  ls_fcat-ref_field = 'WRBTR'.
  ls_fcat-outputlen = '13'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'SHKZG'.
  ls_fcat-ref_table = 'VBSEGS'.
  ls_fcat-ref_field = 'SHKZG'.
  ls_fcat-outputlen = '3'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'VENAMT'.
  ls_fcat-ref_table = 'VBSEGK'.
  ls_fcat-ref_field = 'WRBTR'.
  ls_fcat-coltext = 'Vendor Amount'.
  ls_fcat-seltext = 'Vendor Amount'.
  ls_fcat-outputlen = '13'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WAERS'.
  ls_fcat-ref_table = 'VBKPF'.
  ls_fcat-ref_field = 'WAERS'.
  ls_fcat-outputlen = '4'.
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

  ls_fcat-fieldname = 'WI_LED'.
  ls_fcat-ref_table = 'SWR_WIDTL'.
  ls_fcat-ref_field = 'WI_LED'.
  ls_fcat-coltext = 'Doc. Approval Date'.
  ls_fcat-seltext = 'Doc. Approval Date'.
  ls_fcat-outputlen = '10'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'WI_DAYS'.
  ls_fcat-outputlen = '10'.
  ls_fcat-coltext = '# days for approval'.
  ls_fcat-seltext = 'No of days for approval'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'TNOTES'.
  ls_fcat-coltext = 'Tot. Notes'.
  ls_fcat-seltext = 'Total Notes'.
  ls_fcat-hotspot = 'X'.
  ls_fcat-outputlen = '3'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'CRONAM'.
  ls_fcat-ref_field = 'SO_CRO_NAM'.
  ls_fcat-outputlen = '12'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'RECNAM'.
  ls_fcat-coltext = 'Notes Reciever(s)'.
  ls_fcat-seltext = 'Notes Reciever(s)'.
  ls_fcat-hotspot = 'X'.
  ls_fcat-outputlen = '50'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'NOTE1'.
  ls_fcat-coltext = 'NOTES1'.
  ls_fcat-seltext = 'NOTES1'.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'NOTE2'.
  ls_fcat-coltext = 'NOTES2'.
  ls_fcat-seltext = 'NOTES2'.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.
*  ls_fcat-fieldname = 'WI_STAT'.
*  ls_fcat-ref_table = 'SWR_WIDTL'.
*  ls_fcat-ref_field = 'WI_STAT'.
*  ls_fcat-outputlen = '12'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.

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
  DATA: projk_out(24) TYPE c.
  DATA: bdcdata_wa  TYPE bdcdata,
        bdcdata_tab TYPE TABLE OF bdcdata.
  DATA opt TYPE ctu_params.

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
      SELECT SINGLE * INTO ls_ekpo FROM ekpo
                WHERE ebeln = ls_output-ebeln.
      IF sy-subrc <> 0.
        CONCATENATE 'Please check PO' ls_output-ebeln INTO txt2 SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Purchase Order'
            txt1  = 'PO as reference document is not available'
            txt2  = txt2
*           TXT3  = ' '
*           TXT4  = ' '
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
    WHEN 'NPLNR'.
      SET PARAMETER ID 'ANR' FIELD ls_output-nplnr.
      CALL TRANSACTION 'CN23' AND SKIP FIRST SCREEN.
    WHEN 'KOSTL'.
      SET PARAMETER ID 'KOS' FIELD ls_output-kostl.
      CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
    WHEN 'SAKNR'.

      CLEAR bdcdata_wa.
      bdcdata_wa-program  = 'SAPLGL_ACCOUNT_MASTER_MAINTAIN'.
      bdcdata_wa-dynpro   = '2001'.
      bdcdata_wa-dynbegin = 'X'.
      APPEND bdcdata_wa TO bdcdata_tab.
      CLEAR bdcdata_wa.

      bdcdata_wa-fnam = 'GLACCOUNT_SCREEN_KEY-SAKNR'.
      bdcdata_wa-fval = ls_output-saknr.
      APPEND bdcdata_wa TO bdcdata_tab.

      CLEAR bdcdata_wa.
      bdcdata_wa-fnam = 'GLACCOUNT_SCREEN_KEY-BUKRS'.
      bdcdata_wa-fval = ls_output-bukrs.
      APPEND bdcdata_wa TO bdcdata_tab.

      CLEAR bdcdata_wa.
      bdcdata_wa-fnam = 'BDC_OKCODE'.
      bdcdata_wa-fval = 'ACC_SHOW'.
      APPEND bdcdata_wa TO bdcdata_tab.

      CALL TRANSACTION 'FS00' USING bdcdata_tab  MODE 'E'.

    WHEN 'PROJK'.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = ls_output-projk
        IMPORTING
          output = projk_out.

      SET PARAMETER ID 'PRO' FIELD projk_out.
      CALL TRANSACTION 'CJ03' AND SKIP FIRST SCREEN.
    WHEN 'AUFNR'.
      SET PARAMETER ID 'ANR' FIELD ls_output-aufnr.
      CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.
    WHEN 'TNOTES' OR 'RECNAM' OR 'NOTE1' OR 'NOTE2'.
      PERFORM display_notes USING ls_output.
  ENDCASE.
*  ENDIF.
ENDFORM.                    " handle_user_command
*&---------------------------------------------------------------------*
*&      Form  GET_CONTAINER
*&---------------------------------------------------------------------*
*       Get Route code owner from Workflow
*----------------------------------------------------------------------*
FORM get_container  USING    p_wi_id TYPE swr_wihdr-wi_id
                             p_route_owner
                             p_finalapp.

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
    p_route_owner = ls_swr_cont-value+2(35). "ls_swr_cont-value.
  ENDLOOP.


*  added by rajamans
  clear p_finalapp.
  LOOP AT lt_swr_cont INTO ls_swr_cont
                       WHERE element = 'FINALAPPROVER' .
    p_finalapp = ls_swr_cont-value+2(35).
  ENDLOOP.
  LOOP AT lt_swr_cont INTO ls_swr_cont
                      WHERE  element = 'APPROVER' or element = 'SRC' or element = 'ACTUALAPPROVER'.
    if p_finalapp is INITIAL.
      if  ls_swr_cont-element = 'APPROVER' and ls_swr_cont-value+2(35) is not INITIAL..
        p_finalapp = ls_swr_cont-value+2(35).
        exit.
      elseif   ls_swr_cont-element = 'SRC' and ls_swr_cont-value+2(35) is not INITIAL.
        p_finalapp = ls_swr_cont-value+2(35).
        exit.
      elseif    ls_swr_cont-element = 'ACTUALAPPROVER' and ls_swr_cont-value+2(35) is not INITIAL.
        p_finalapp = ls_swr_cont-value+2(35).
        exit.
      endif.
    endif.
  ENDLOOP.
*  additin enda here
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

*  break sahmad.
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
*  DELETE p_lt_task_wihdr WHERE wi_rh_task <> p_task_id.
  DELETE p_lt_task_wihdr WHERE ( wi_type <> 'W' AND wi_type <> 'Q' ).
  DELETE p_lt_swr_wihdr  WHERE wi_creator = lv_wi_creator OR
                               wi_rh_task CP 'TS*'.

ENDFORM.                    " GET_WORKFLOW
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       process collected data
*----------------------------------------------------------------------*
FORM process_data .
  FIELD-SYMBOLS: <output> TYPE ty_output.
*  IF s_comp = 'X'.
*    DELETE it_output WHERE wi_item_stat <> 'COMPLETED'.
*  ENDIF.
*  IF s_rco = 'X'.
*    DELETE it_output WHERE wi_item_stat <> 'ROUTE_CODE_OWNER'.
*  ENDIF.
*  IF s_dao = 'X'.
*    DELETE it_output WHERE wi_item_stat <> 'DOA_APPROVER'.
*  ENDIF.

  IF s_kostl[] IS NOT INITIAL.
    DELETE it_output WHERE kostl NOT IN s_kostl.
  ENDIF.
  IF s_nplnr[] IS NOT INITIAL.
    DELETE it_output WHERE nplnr NOT IN s_nplnr.
  ENDIF.
  IF s_vornr[] IS NOT INITIAL.
    DELETE it_output WHERE vornr NOT IN s_vornr.
  ENDIF.
  IF s_projk[] IS NOT INITIAL.
    DELETE it_output WHERE projk NOT IN s_projk.
  ENDIF.
  IF s_aufnr[] IS NOT INITIAL.
    DELETE it_output WHERE aufnr NOT IN s_aufnr.
  ENDIF.
  IF s_saknr[] IS NOT INITIAL.
    DELETE it_output WHERE saknr NOT IN s_saknr.
  ENDIF.
  IF s_ebeln[] IS NOT INITIAL.
    DELETE it_output WHERE ebeln NOT IN s_ebeln.
  ENDIF.
  IF s_xref3[] IS NOT INITIAL.
    DELETE it_output WHERE xref3 NOT IN s_xref3.
  ENDIF.
  IF s_rcown[] IS NOT INITIAL.
    DELETE it_output WHERE route_owner NOT IN s_rcown.
  ENDIF.
  IF s_ekgrp[] IS NOT INITIAL.
    DELETE it_output WHERE ekgrp NOT IN s_ekgrp.
  ENDIF.
  IF s_werks[] IS NOT INITIAL.
    DELETE it_output WHERE werks NOT IN s_werks.
  ENDIF.
*Begin of changes by rajamans

  loop at it_output ASSIGNING <output>.
    if <output>-WI_AAGENT is INITIAL.
      if <output>-finalapp is not INITIAL.
        <output>-WI_AAGENT = <output>-finalapp.
      else.
        <output>-WI_AAGENT = <output>-ROUTE_OWNER.
      endif.
    endif.
  endloop.
*changes end here
ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ITEM_APPR
*&---------------------------------------------------------------------*
*       Get document approval data, approved by and item status
*----------------------------------------------------------------------*
FORM get_item_appr  TABLES   p_lt_task_wihdr STRUCTURE swr_wihdr
                    USING    p_wi_led TYPE swr_widtl-wi_led
                             p_wi_aagent TYPE swr_widtl-wi_aagent
                             p_wi_forw_by TYPE swr_widtl-wi_forw_by
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
  p_wi_forw_by = wa_task_wihdr-wi_forw_by.
  p_wi_item_stat = wa_task_wihdr-wi_stat.
**Non PO Base
*  IF p_flag = 'C' AND                    "Posted
*     wa_task_wihdr-wi_stat = 'COMPLETED'.
*    p_wi_item_stat = 'COMPLETED'.
*  ELSEIF p_flag = 'C' AND
*          wa_task_wihdr-wi_stat <> 'COMPLETED'.
*    IF p_route_owner = wa_task_wihdr-wi_aagent.
*      p_wi_item_stat = 'ROUTE_CODE_OWNER'.
*    ELSE.
*      p_wi_item_stat = 'DOA_APPROVER'.
*    ENDIF.
*  ENDIF.
****************PO base Invoice
*  IF p_flag = 'I'.
*    p_wi_item_stat = wa_task_wihdr-wi_stat.
*  ENDIF.
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
  DATA: lv_tabix TYPE sy-tabix,
        lt_rbkp TYPE TABLE OF ty_output, "rbkp,    "PO base Invoice Data
        lt_vbkpf TYPE TABLE OF vbkpf,
        wa_vbkpf TYPE vbkpf,
        lt_vbsegk TYPE TABLE OF vbsegk,
        ls_vbsegk TYPE vbsegk,
        lt_vbsegs TYPE TABLE OF vbsegs,
        ls_vbsegs TYPE vbsegs,
        lt_rbco TYPE TABLE OF rbco,
        ls_rbco TYPE rbco,
        fl_wi_rh_task TYPE swr_wihdr-wi_rh_task,
        fl_belnr TYPE vbkpf-belnr,
        fl_gjahr TYPE vbkpf-gjahr,
        fl_flag,
        fl_usnam TYPE rbkp-usnam,
        fl_ebelp TYPE rseg-ebelp,
        lt_lfa1 TYPE TABLE OF lfa1,
        ls_lfa1 TYPE lfa1,
        ls_notes TYPE zso_notes.

*  break sahmad.
*Parked Documents
  SELECT * FROM vbkpf INTO TABLE lt_vbkpf
    WHERE  ausbk IN s_bukrs
      AND  blart IN s_blart
      AND  budat IN s_budat.

  IF lt_vbkpf IS INITIAL.
    WRITE: / 'No parked document for given selection criteria'.
    STOP.
  ENDIF.
*Get parked document line items
*for Non PO based invoice, we can get cost/network/activity
*from this table.
*for PO base invoice, we have to extract AUFPL APLZL from RBCO
*(Invoice account assignment)table. Using these fields, extract
*Activity from AFVC (Operations within order) table. While
*Network and Cost center will be extracted from RBCO table.
*************
*GL data for Non-PO based invoice
  SELECT * FROM vbsegs INTO TABLE lt_vbsegs
    FOR ALL ENTRIES IN lt_vbkpf
    WHERE ausbk = lt_vbkpf-ausbk
      AND belnr = lt_vbkpf-belnr
      AND gjahr = lt_vbkpf-gjahr
      AND bukrs = lt_vbkpf-bukrs.
*Vendor line items for parked document
  SELECT * FROM vbsegk INTO TABLE lt_vbsegk
    FOR ALL ENTRIES IN lt_vbkpf
    WHERE ausbk = lt_vbkpf-ausbk
      AND belnr = lt_vbkpf-belnr
      AND gjahr = lt_vbkpf-gjahr
      AND bukrs = lt_vbkpf-bukrs
      AND lifnr IN s_lifnr.
  IF lt_vbsegk IS NOT INITIAL.
    SELECT * FROM lfa1 INTO TABLE lt_lfa1
      FOR ALL ENTRIES IN lt_vbsegk
      WHERE lifnr = lt_vbsegk-lifnr.
  ENDIF.
****
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 40
      text       = 'Data extraction relevant to workflow is in progress.'.
****
  SORT lt_vbsegs BY ausbk gjahr belnr bukrs.
  SORT lt_vbsegk BY ausbk gjahr belnr bukrs.
  LOOP AT lt_vbkpf INTO wa_vbkpf.
    CLEAR: wa_output,
           fl_usnam,
           fl_ebelp,
           ls_vbsegk,
           ls_lfa1,
           lt_rbco.

    READ TABLE lt_vbsegk INTO ls_vbsegk WITH KEY
                         ausbk = wa_vbkpf-ausbk
                         belnr = wa_vbkpf-belnr
                         gjahr = wa_vbkpf-gjahr
                         bukrs = wa_vbkpf-bukrs.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY
                         lifnr = ls_vbsegk-lifnr.

    fl_belnr = wa_vbkpf-awkey(10).
    fl_gjahr = wa_vbkpf-awkey+10(4).

    SELECT SINGLE usnam INTO fl_usnam FROM rbkp
        WHERE belnr = fl_belnr
          AND gjahr = fl_gjahr.
    IF sy-subrc = 0.  "PO Based
**If Non-PO Invoice parameters are entered, no need to execute PO logic.
      IF gv_flag IS NOT INITIAL.
        IF gv_flag <> 'I'.
          CONTINUE.
        ENDIF.
      ENDIF.
******
      SELECT SINGLE ebeln werks ebelp INTO
             (wa_output-ebeln , wa_output-werks , fl_ebelp)
             FROM rseg WHERE belnr = fl_belnr
                         AND gjahr = fl_gjahr.
      IF s_werks IS NOT INITIAL.  "Plant from selection screen
        IF wa_output-werks NOT IN s_werks.
          CONTINUE.
        ENDIF.
      ENDIF.
      SELECT SINGLE ekgrp zzariba_approver INTO
            (wa_output-ekgrp , wa_output-zzariba_approver)
            FROM ekko  WHERE ebeln = wa_output-ebeln.
      IF s_ekgrp IS NOT INITIAL.  "Pur Group from selection screen
        IF wa_output-ekgrp NOT IN s_ekgrp.
          CONTINUE.
        ENDIF.
      ENDIF.
      SELECT * FROM rbco INTO TABLE lt_rbco
                      WHERE belnr = fl_belnr
                        AND gjahr = fl_gjahr.

      fl_objtype = 'BUS2081'.
      fl_wi_rh_task = 'TS02000053'. "'TS90000104'.
      fl_objkey =  wa_vbkpf-awkey.
      wa_output-mminv = fl_belnr.
      fl_flag = 'I'.
    ELSE.  "Non-PO Base
**If PO Invoice parameters are entered, no need to execute Non-PO logic.
      IF gv_flag IS NOT INITIAL.
        IF gv_flag <> 'C'.
          CONTINUE.
        ENDIF.
      ENDIF.
******
      fl_objtype = 'FIPP'.
      fl_wi_rh_task = 'TS02000013'. "'TS90000089'.
      CONCATENATE wa_vbkpf-ausbk wa_vbkpf-belnr wa_vbkpf-gjahr
                              INTO fl_objkey RESPECTING BLANKS.
      fl_flag = 'C'.
    ENDIF.
****check PO or Non-PO
    IF gv_flag IS NOT INITIAL.
      IF gv_flag <> fl_flag.
        CONTINUE.
      ENDIF.
    ENDIF.
****
    CLEAR: lt_swr_wihdr,
           lt_task_wihdr,
           ls_notes.

    PERFORM get_workflow  TABLES lt_swr_wihdr
                                 lt_task_wihdr
                          USING  fl_objtype
                                 fl_objkey
                                 fl_wi_rh_task.
    PERFORM get_notes USING fl_objtype
                            fl_objkey
                            ls_notes
                            wa_vbkpf-ausbk
                            wa_vbkpf-belnr
                            wa_vbkpf-gjahr
                            wa_vbkpf-blart
                            wa_output-tnotes.
    wa_output-cronam = ls_notes-cronam.
    wa_output-recnam = ls_notes-recnam.
    wa_output-note1  = ls_notes-note1.
    wa_output-note2  = ls_notes-note2.
    IF fl_flag = 'C'. "Non-PO base invoice
      CLEAR lv_tabix.
      READ TABLE lt_vbsegs WITH KEY ausbk = wa_vbkpf-ausbk
                                    gjahr = wa_vbkpf-gjahr
                                    belnr = wa_vbkpf-belnr
                                    bukrs = wa_vbkpf-bukrs
                                    TRANSPORTING NO FIELDS.
      IF sy-subrc  = 0.
        lv_tabix = sy-tabix.
        LOOP AT lt_vbsegs INTO ls_vbsegs FROM lv_tabix.

          IF ls_vbsegs-ausbk <> wa_vbkpf-ausbk OR
             ls_vbsegs-gjahr <> wa_vbkpf-gjahr OR
             ls_vbsegs-belnr <> wa_vbkpf-belnr OR
             ls_vbsegs-bukrs <> wa_vbkpf-bukrs.
            EXIT.
          ENDIF.
          wa_output-name1 = ls_lfa1-name1.
          wa_output-xref3 = ls_vbsegk-xref3.
          wa_output-bukrs = wa_vbkpf-ausbk. "bukrs.
          wa_output-lifnr = ls_vbsegk-lifnr.
          wa_output-venamt = ls_vbsegk-wrbtr.
          "Vendor invoice refernce for NON PO
          wa_output-xblnr = wa_vbkpf-xblnr.
          wa_output-blart = wa_vbkpf-blart.
          wa_output-belnr = wa_vbkpf-belnr.
          wa_output-wrbtr = ls_vbsegs-wrbtr.
          wa_output-shkzg  = ls_vbsegs-shkzg.
          wa_output-waers = wa_vbkpf-waers.
          wa_output-gjahr = wa_vbkpf-gjahr.
          wa_output-nplnr = ls_vbsegs-nplnr.
          wa_output-kostl = ls_vbsegs-kostl.
          wa_output-vornr = ls_vbsegs-vornr.
          wa_output-saknr = ls_vbsegs-saknr.
          wa_output-projk = ls_vbsegs-ps_psp_pnr.
          wa_output-aufnr = ls_vbsegs-aufnr.
          wa_output-mwskz = ls_vbsegs-mwskz.
          IF lt_swr_wihdr IS INITIAL.
            wa_output-wi_idc = 'No WF'.
            APPEND wa_output TO it_output.
          ENDIF.
          LOOP AT lt_swr_wihdr  INTO wa_swr_wihdr.

            CLEAR: wa_swr_widtl,
                   wa_output-route_owner.
            PERFORM get_workitem USING wa_swr_wihdr-wi_id
                                       wa_swr_widtl.
            PERFORM get_container USING wa_swr_wihdr-wi_id
                                        wa_output-route_owner
                                        wa_output-finalapp.  " added by rajamns
            PERFORM get_item_appr TABLES lt_task_wihdr
                                   USING wa_output-wi_led
                                         wa_output-wi_aagent
                                         wa_output-wi_forw_by
                                         wa_output-wi_item_stat
                                         fl_flag "'C'
                                         wa_output-route_owner.
*              IF fl_usnam IS NOT INITIAL.
*                wa_output-wi_aagent = fl_usnam.
*              ENDIF.
            wa_output-wi_id = wa_swr_wihdr-wi_id.
            wa_output-wi_idc = wa_swr_wihdr-wi_id.
            wa_output-wi_cd = wa_swr_wihdr-wi_cd.
            wa_output-wi_creator = wa_swr_wihdr-wi_creator+2(35).
            IF wa_output-wi_led IS NOT INITIAL.
              wa_output-wi_days = wa_output-wi_led - wa_output-wi_cd.
            ELSE.
              wa_output-wi_days = space.
            ENDIF.
            wa_output-wi_stat = wa_swr_widtl-wi_stat.
            APPEND wa_output TO it_output.
          ENDLOOP.
        ENDLOOP.
****
      ELSE. "No GL Segement
****
        wa_output-name1 = ls_lfa1-name1.
        wa_output-xref3 = ls_vbsegk-xref3.
        wa_output-bukrs = wa_vbkpf-ausbk. "bukrs.
        wa_output-lifnr = ls_vbsegk-lifnr.
        wa_output-venamt = ls_vbsegk-wrbtr.
        "Vendor invoice refernce for NON PO
        wa_output-xblnr = wa_vbkpf-xblnr.
        wa_output-blart = wa_vbkpf-blart.
        wa_output-belnr = wa_vbkpf-belnr.
        wa_output-wrbtr = space.
        wa_output-shkzg  = space.
        wa_output-waers = wa_vbkpf-waers.
        wa_output-gjahr = wa_vbkpf-gjahr.
        wa_output-nplnr = space.
        wa_output-kostl = space.
        wa_output-vornr = space.
        wa_output-saknr = space.
        wa_output-projk = space.
        wa_output-aufnr = space.
        wa_output-mwskz = space.
        IF lt_swr_wihdr IS INITIAL.
          wa_output-wi_idc = 'No WF'.
          APPEND wa_output TO it_output.
        ENDIF.
        LOOP AT lt_swr_wihdr  INTO wa_swr_wihdr.
          CLEAR: wa_swr_widtl,
                 wa_output-route_owner.
          PERFORM get_workitem USING wa_swr_wihdr-wi_id
                                     wa_swr_widtl.
          PERFORM get_container USING wa_swr_wihdr-wi_id
                                      wa_output-route_owner
                                      wa_output-finalapp.  "added by rajamans
          PERFORM get_item_appr TABLES lt_task_wihdr
                                 USING wa_output-wi_led
                                       wa_output-wi_aagent
                                       wa_output-wi_forw_by
                                       wa_output-wi_item_stat
                                       fl_flag "'C'
                                       wa_output-route_owner.
*          IF fl_usnam IS NOT INITIAL.
*            wa_output-wi_aagent = fl_usnam.
*          ENDIF.
          wa_output-wi_id = wa_swr_wihdr-wi_id.
          wa_output-wi_idc = wa_swr_wihdr-wi_id.
          wa_output-wi_cd = wa_swr_wihdr-wi_cd.
          wa_output-wi_creator = wa_swr_wihdr-wi_creator+2(35).
          IF wa_output-wi_led IS NOT INITIAL.
            wa_output-wi_days = wa_output-wi_led - wa_output-wi_cd.
          ELSE.
            wa_output-wi_days = space.
          ENDIF.
          wa_output-wi_stat = wa_swr_widtl-wi_stat.
          APPEND wa_output TO it_output.
        ENDLOOP.
      ENDIF.
    ELSE. "PO base invoice
      IF lt_rbco IS NOT INITIAL.
        LOOP AT lt_rbco INTO ls_rbco.
          wa_output-vornr = space.
          SELECT SINGLE vornr INTO wa_output-vornr FROM afvc
                                 WHERE aufpl = ls_rbco-aufpl
                                   AND aplzl = ls_rbco-aplzl.
          wa_output-name1 = ls_lfa1-name1.
          wa_output-xref3 = ls_vbsegk-xref3.
          wa_output-bukrs = wa_vbkpf-ausbk. "bukrs.
          wa_output-lifnr = ls_vbsegk-lifnr.
          wa_output-venamt = ls_vbsegk-wrbtr.
          "Vendor invoice refernce for NON PO
          wa_output-xblnr = wa_vbkpf-xblnr.
          wa_output-blart = wa_vbkpf-blart.
          wa_output-belnr = wa_vbkpf-belnr.
          wa_output-wrbtr = ls_rbco-wrbtr.
          wa_output-waers = wa_vbkpf-waers.
          wa_output-gjahr = wa_vbkpf-gjahr.
          wa_output-nplnr = ls_rbco-nplnr.
          wa_output-kostl = ls_rbco-kostl.
          wa_output-saknr = ls_rbco-saknr.
          wa_output-projk = ls_rbco-ps_psp_pnr.
          wa_output-aufnr = ls_rbco-aufnr.
          wa_output-mwskz = ls_rbco-mwskz.
          IF lt_swr_wihdr IS INITIAL.
            wa_output-wi_idc = 'No WF'.
            APPEND wa_output TO it_output.
          ENDIF.
          LOOP AT lt_swr_wihdr  INTO wa_swr_wihdr.

            CLEAR: wa_swr_widtl,
                   wa_output-route_owner.
            PERFORM get_workitem USING wa_swr_wihdr-wi_id
                                       wa_swr_widtl.
            PERFORM get_container USING wa_swr_wihdr-wi_id
                                        wa_output-route_owner
                                        wa_output-finalapp. " Added by rajamans
            PERFORM get_item_appr TABLES lt_task_wihdr
                                   USING wa_output-wi_led
                                         wa_output-wi_aagent
                                         wa_output-wi_forw_by
                                         wa_output-wi_item_stat
                                         fl_flag "'C'
                                         wa_output-route_owner.
*             IF fl_usnam IS NOT INITIAL.
*               wa_output-wi_aagent = fl_usnam.
*             ENDIF.
            wa_output-wi_id = wa_swr_wihdr-wi_id.
            wa_output-wi_idc = wa_swr_wihdr-wi_id.
            wa_output-wi_cd = wa_swr_wihdr-wi_cd.
            wa_output-wi_creator = wa_swr_wihdr-wi_creator+2(35).
            IF wa_output-wi_led IS NOT INITIAL.
              wa_output-wi_days = wa_output-wi_led - wa_output-wi_cd.
            ELSE.
              wa_output-wi_days = space.
            ENDIF.
            wa_output-wi_stat = wa_swr_widtl-wi_stat.
            APPEND wa_output TO it_output.
          ENDLOOP.
        ENDLOOP.
      ELSE. "no lt_rbco
        wa_output-name1 = ls_lfa1-name1.
        wa_output-xref3 = ls_vbsegk-xref3.
        wa_output-bukrs = wa_vbkpf-ausbk. "bukrs.
        wa_output-lifnr = ls_vbsegk-lifnr.
        wa_output-venamt = ls_vbsegk-wrbtr.
        "Vendor invoice refernce for NON PO
        wa_output-xblnr = wa_vbkpf-xblnr.
        wa_output-blart = wa_vbkpf-blart.
        wa_output-belnr = wa_vbkpf-belnr.
        wa_output-wrbtr = space.
        wa_output-waers = wa_vbkpf-waers.
        wa_output-gjahr = wa_vbkpf-gjahr.
        wa_output-nplnr = space.
        wa_output-kostl = space.
        wa_output-saknr = space.
        wa_output-projk = space.
        wa_output-aufnr = space.
        wa_output-mwskz = space.
        IF lt_swr_wihdr IS INITIAL.
          wa_output-wi_idc = 'No WF'.
          APPEND wa_output TO it_output.
        ENDIF.
        LOOP AT lt_swr_wihdr  INTO wa_swr_wihdr.
          CLEAR: wa_swr_widtl,
                 wa_output-route_owner.
          PERFORM get_workitem USING wa_swr_wihdr-wi_id
                                     wa_swr_widtl.
          PERFORM get_container USING wa_swr_wihdr-wi_id
                                      wa_output-route_owner
                                      wa_output-finalapp.
          PERFORM get_item_appr TABLES lt_task_wihdr
                                 USING wa_output-wi_led
                                       wa_output-wi_aagent
                                       wa_output-wi_forw_by
                                       wa_output-wi_item_stat
                                       fl_flag "'C'
                                       wa_output-route_owner.
          wa_output-wi_id = wa_swr_wihdr-wi_id.
          wa_output-wi_idc = wa_swr_wihdr-wi_id.
          wa_output-wi_cd = wa_swr_wihdr-wi_cd.
          wa_output-wi_creator = wa_swr_wihdr-wi_creator+2(35).
          IF wa_output-wi_led IS NOT INITIAL.
            wa_output-wi_days = wa_output-wi_led - wa_output-wi_cd.
          ELSE.
            wa_output-wi_days = space.
          ENDIF.
          wa_output-wi_stat = wa_swr_widtl-wi_stat.
          APPEND wa_output TO it_output.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_notes_output BY  bukrs gjahr belnr
                           crdat crtim cronam.
ENDFORM.                    " GET_PARKED_DOCUMENTS
*&---------------------------------------------------------------------*
*&      Form  DISABLE_ENABLE_COST_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disable_enable_cost_objects .

  CONSTANTS: lc_cst TYPE char3 VALUE 'CST'.

  LOOP AT SCREEN.
    IF r_allcs = 'X'.
      REFRESH: s_kostl[],
               s_nplnr[],
               s_vornr[],
               s_projk[],
               s_saknr[].
      IF screen-group1 = 'KST' OR
         screen-group1 = 'NPL' OR
         screen-group1 = 'PRJ' OR
         screen-group1 = 'AUF' OR
         screen-group1 = 'SAK'.
        screen-input = 0. " Disable the input
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF r_kostl = 'X'.
      REFRESH: s_nplnr[],
               s_vornr[],
               s_projk[],
               s_aufnr[],
               s_saknr[].
      IF screen-group1 = 'KST'.
        screen-input = 1. " enable the input
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'NPL' OR
             screen-group1 = 'PRJ' OR
             screen-group1 = 'AUF' OR
             screen-group1 = 'SAK'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF r_nplnr = 'X'.
      REFRESH: s_kostl[],
               s_vornr[],
               s_projk[],
               s_aufnr[],
               s_saknr[].
      IF screen-group1 = 'NPN'.
        screen-input = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'KST' OR
             screen-group1 = 'PRJ' OR
             screen-group1 = 'AUF' OR
             screen-group1 = 'SAK'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF r_projk = 'X'.
      REFRESH: s_kostl[],
               s_nplnr[],
               s_vornr[],
               s_aufnr[],
               s_saknr[].
      IF screen-group1 = 'PRJ'.
        screen-input = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'KST' OR
             screen-group1 = 'NPL' OR
             screen-group1 = 'AUF' OR
             screen-group1 = 'SAK'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF r_aufnr = 'X'.
      REFRESH: s_kostl[],
               s_nplnr[],
               s_vornr[],
               s_projk[],
               s_saknr[].
      IF screen-group1 = 'AUF'.
        screen-input = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'KST' OR
             screen-group1 = 'NPL' OR
             screen-group1 = 'PRJ' OR
             screen-group1 = 'SAK'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF r_saknr = 'X'.
      REFRESH: s_kostl[],
               s_nplnr[],
               s_vornr[],
               s_projk[],
               s_aufnr[].
      IF screen-group1 = 'SAK'.
        screen-input = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 = 'KST' OR
             screen-group1 = 'NPL' OR
             screen-group1 = 'PRJ' OR
             screen-group1 = 'AUF'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DISABLE_ENABLE_COST_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  VARIANT_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM variant_help .
  gv_vari-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = gv_vari
*     I_TABNAME_HEADER    =
*     I_TABNAME_ITEM      =
*     IT_DEFAULT_FIELDCAT =
*     I_SAVE              = ' '
*     I_DISPLAY_VIA_GRID  = ' '
    IMPORTING
*     E_EXIT              =
      es_variant          = gv_vari
    EXCEPTIONS
      not_found           = 1
      program_error       = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  p_vari = gv_vari-variant.
ENDFORM.                    " VARIANT_HELP
*&---------------------------------------------------------------------*
*&      Form  GET_RC_XREF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_rc_xref.
  DATA: ls_rc_list LIKE LINE OF gt_rc_list,
        ls_rcowner LIKE LINE OF gt_rcowner.

  CLEAR: gt_rc_list,
         gt_rcowner.
*  CALL FUNCTION 'ZFI_GET_ROUTCODE_XREF3' DESTINATION gv_log_dest  "MZH01
  CALL FUNCTION 'ZFI_GET_ROUTCODE_XREF3'  "MZH01
    TABLES
      rc_owner_list = gt_rc_list.

  LOOP AT gt_rc_list INTO ls_rc_list.
    MOVE-CORRESPONDING ls_rc_list TO ls_rcowner.
    APPEND ls_rcowner TO gt_rcowner.
  ENDLOOP.

ENDFORM.                    " GET_RC_XREF
*&---------------------------------------------------------------------*
*&      Form  F4_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_output  USING iv_check.

  DATA: lt_return TYPE STANDARD TABLE OF ddshretval WITH HEADER LINE,
        lv_retfield TYPE  dfies-fieldname,
        lv_dynprofield TYPE help_info-dynprofld.

  IF iv_check = '1'.
    lv_retfield	    = 'ROUTE_CODE'.
    lv_dynprofield  = 'S_XREF3-LOW'.
  ENDIF.
  IF iv_check = '2'.
    lv_retfield	    = 'ROUTE_CODE'.
    lv_dynprofield  = 'S_XREF3-HIGH'.
  ENDIF.
  IF iv_check = '3'.
    lv_retfield	    = 'OWNER'.
    lv_dynprofield  = 'S_RCOWN-LOW'.
  ENDIF.
  IF iv_check = '4'.
    lv_retfield	    = 'OWNER'.
    lv_dynprofield  = 'S_RCOWN-HIGH'.
  ENDIF.
  IF iv_check = '1' OR iv_check = '2'.
    SORT gt_rcowner BY route_code.
  ELSE.
    SORT gt_rcowner BY owner.
  ENDIF.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = lv_retfield
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = lv_dynprofield
      value_org       = 'S'
    TABLES
      value_tab       = gt_rcowner
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " F4_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_NOTES
*&---------------------------------------------------------------------*
*       Get Send Notes related to the document
*----------------------------------------------------------------------*
FORM get_notes  USING    p_objtype  TYPE swotobjid-objtype
                         p_objkey   TYPE swotobjid-objkey
                         p_ls_notes TYPE zso_notes
                         p_ausbk    TYPE vbkpf-ausbk
                         p_belnr    TYPE vbkpf-belnr
                         p_gjahr    TYPE vbkpf-gjahr
                         p_blart    TYPE vbkpf-blart
                         p_tnotes   TYPE i.
  DATA: lv_instid TYPE sibfboriid,
        lv_typeid TYPE sibftypeid,
        lt_sood TYPE ztab_sood,
        lt_soos TYPE ztab_soos,
        lt_notes TYPE zso_notes_tab,
        ls_notes TYPE zso_notes.
*In Case of FIPP, I_INSTID_A = Company code, document + year i.e. 01  01200000092011
*In case of BUS2081, Document + Year i.e. 52000000092011
  lv_instid = p_objkey.
  lv_typeid = p_objtype.
  TRY.
      CALL METHOD z_read_object_outbox=>read_object_outbox
        EXPORTING
          i_instid_a    = lv_instid
          i_typeid_a    = lv_typeid
*         i_catid_a     = 'BO'
        IMPORTING
          etab_sood     = lt_sood
          etab_soos     = lt_soos
          etab_so_notes = lt_notes.
    CATCH cx_obl_parameter_error .
  ENDTRY.
  IF lt_notes[] IS NOT INITIAL.
    READ TABLE lt_notes INTO p_ls_notes INDEX 1.
    LOOP AT lt_notes INTO ls_notes.
      gs_notes_output-belnr = p_belnr.
      gs_notes_output-bukrs = p_ausbk.
      gs_notes_output-gjahr = p_gjahr.
      gs_notes_output-blart = p_blart.
      gs_notes_output-recnam = ls_notes-recnam.
      gs_notes_output-cronam = ls_notes-cronam.
      gs_notes_output-crdat  = ls_notes-crdat.
      gs_notes_output-crtim  = ls_notes-crtim.
      gs_notes_output-note1  = ls_notes-note1.
      gs_notes_output-note2  = ls_notes-note2.
      APPEND gs_notes_output TO gt_notes_output.
      p_tnotes = p_tnotes + 1.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_NOTES
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_NOTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_notes USING ls_output TYPE ty_output.

  DATA: lv_tabix         TYPE sy-tabix,
          lv_title2         TYPE sytitle,
          lo_table2         TYPE REF TO cl_salv_table,
          lo_functions2     TYPE REF TO cl_salv_functions,
          lo_display2       TYPE REF TO cl_salv_display_settings,
          lo_columns2       TYPE REF TO cl_salv_columns_table,
          lo_column2        TYPE REF TO cl_salv_column_table, "#EC NEEDED
          lo_content2       TYPE REF TO cl_salv_form_element,
          lo_grid2          TYPE REF TO cl_salv_form_layout_grid,
          lo_events_salv2   TYPE REF TO cl_salv_events_table,
*        lo_event         TYPE REF TO lcl_event_handler,
          lt_notes_output TYPE TABLE OF ty_notes_output,
          ls_notes_output TYPE ty_notes_output.

  SORT gt_notes_output BY bukrs gjahr belnr.
  READ TABLE gt_notes_output WITH KEY bukrs = ls_output-bukrs
                                      gjahr = ls_output-gjahr
                                      belnr = ls_output-belnr
                                      TRANSPORTING NO FIELDS.
  lv_tabix = sy-tabix.
  LOOP AT gt_notes_output INTO ls_notes_output FROM lv_tabix.
    IF ls_notes_output-bukrs <> ls_output-bukrs OR
       ls_notes_output-gjahr <> ls_output-gjahr OR
       ls_notes_output-belnr <> ls_output-belnr.
      EXIT.
    ENDIF.
    APPEND ls_notes_output TO lt_notes_output.
  ENDLOOP.
  IF lt_notes_output IS INITIAL.
    MESSAGE i000(zfi01) WITH 'No Notes Data to output' '' '' '' DISPLAY LIKE 'I'.
*   & & & & &

  ELSE.
    SORT lt_notes_output BY  bukrs gjahr belnr
                             crdat crtim cronam.
    CONCATENATE 'SAP Object OutBox Notes for ' ls_output-bukrs
                ls_output-gjahr ls_output-belnr
                INTO lv_title2 SEPARATED BY space.
    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_table2
          CHANGING
            t_table      = lt_notes_output.
      CATCH cx_salv_msg .
    ENDTRY.
*Function settings
    lo_functions2 = lo_table2->get_functions( ).
    lo_functions2->set_all( abap_true ).
*Event
*  lo_events_salv = lo_table->get_event( ).
*  CREATE OBJECT lo_event.
*  SET HANDLER: lo_event->hotspot_click1
*               FOR lo_events_salv.
*Display Setting
    lo_display2 = lo_table2->get_display_settings( ).

    lo_display2->set_striped_pattern( cl_salv_display_settings=>true ).

*Get columns
    CALL METHOD lo_table2->get_columns
      RECEIVING
        value = lo_columns2.
    lo_columns2->set_optimize( 'X' ).
******Change ALV Fields  - title etc.
    PERFORM alv_fields1 USING lo_columns2 lo_column2.

******Set ALV Header
*... Create top_of_list contents.
    CREATE OBJECT lo_grid2.
    lo_grid2->create_label(
          row    = 1
          column = 1
          text   = lv_title2 ).

    lo_content2 = lo_grid2.
    lo_table2->set_top_of_list( lo_content2 ).
******Display ALV
    CALL METHOD lo_table2->display.
  ENDIF.
ENDFORM.                    " DISPLAY_NOTES
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDS1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LO_COLUMNS  text
*      -->P_LO_COLUMN  text
*----------------------------------------------------------------------*
FORM alv_fields1  USING   io_columns TYPE REF TO cl_salv_columns_table
                          io_column  TYPE REF TO cl_salv_column_table.

  DATA: lv_column     TYPE lvc_fname,
        lv_long_text  TYPE scrtext_l,
        lv_short_text TYPE scrtext_s,
        lv_med_text   TYPE scrtext_m.

  LOOP AT gt_comp_tab1.
    CLEAR: lv_long_text,
           lv_short_text,
           lv_med_text.
    lv_column = gt_comp_tab1-compname.
    CASE lv_column.
      WHEN 'RECNAM'.
        lv_long_text  = 'Receiver Name(s)'.
        lv_short_text = 'Rcv. Name(s)'.
        lv_med_text   = 'Rcv. Name(s)'.
      WHEN 'NOTE1'.
        lv_long_text  = 'Notes 1'.
        lv_short_text = 'Notes 1'.
        lv_med_text   = 'Notes 1'.
      WHEN 'NOTE2'.
        lv_long_text  = 'Notes 2'.
        lv_short_text = 'Notes 2'.
        lv_med_text   = 'Notes 2'.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    TRY.
        io_column ?= io_columns->get_column( lv_column ).
      CATCH cx_salv_not_found .
    ENDTRY.
    CALL METHOD io_column->set_long_text
      EXPORTING
        value = lv_long_text.
    CALL METHOD io_column->set_short_text
      EXPORTING
        value = lv_short_text.
    CALL METHOD io_column->set_medium_text
      EXPORTING
        value = lv_med_text.
  ENDLOOP.

*****hot spot on product category
*     TRY.
*        io_column ?= io_columns->get_column( 'BELNR' ).
*      CATCH cx_salv_not_found .
*    ENDTRY.
*    CALL METHOD io_column->set_cell_type
*      EXPORTING
*        value = if_salv_c_cell_type=>hotspot.
ENDFORM.                    " ALV_FIELDS1
*&---------------------------------------------------------------------*
*&      Form  GET_COMPONENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_components .

  REFRESH gt_comp_tab.

  CALL FUNCTION 'GET_COMPONENT_LIST'
    EXPORTING
      program    = sy-repid
      fieldname  = 'GT_OUTPUT_NOTES1'
    TABLES
      components = gt_comp_tab1[].
  CALL FUNCTION 'GET_COMPONENT_LIST'
    EXPORTING
      program    = sy-repid
      fieldname  = 'IT_OUTPUT1'
    TABLES
      components = gt_comp_tab[].
ENDFORM.                    " GET_COMPONENTS
*&---------------------------------------------------------------------*
*&      Form  PERFORM_SALV
*&---------------------------------------------------------------------*
*       Object ALV
*----------------------------------------------------------------------*
FORM perform_salv .
  DATA: lv_label       TYPE string,
          lv_title       TYPE sytitle,
          ls_key         TYPE salv_s_layout_key,
          lo_table       TYPE REF TO cl_salv_table,
          lo_layout      TYPE REF TO cl_salv_layout,
          lo_functions   TYPE REF TO cl_salv_functions,
          lo_display     TYPE REF TO cl_salv_display_settings,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column_table,  "#EC NEEDED
          lo_content     TYPE REF TO cl_salv_form_element,
          lo_grid        TYPE REF TO cl_salv_form_layout_grid,
          lo_events_salv TYPE REF TO cl_salv_events_table,
          lo_event       TYPE REF TO lcl_event_handler.

  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*          r_container    = lr_con1
*          container_name = 'ALV_CON1'
        IMPORTING
          r_salv_table   = lo_table
        CHANGING
          t_table        = it_output.
    CATCH cx_salv_msg .
  ENDTRY.
*Function settings
  lo_functions = lo_table->get_functions( ).
  lo_functions->set_all( abap_true ).
*Display Setting
  lo_display = lo_table->get_display_settings( ).

  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
*  CONCATENATE 'Create Date: ' s_erdat-sign s_erdat-option s_erdat-low s_erdat-high
*                            INTO lv_msg separated by space.
*
*  lo_display->set_list_header( lv_msg ).
*Event
  lo_events_salv = lo_table->get_event( ).
  CREATE OBJECT lo_event.
  SET HANDLER: lo_event->hotspot_click
               FOR lo_events_salv.
*Set layout
  lo_layout = lo_table->get_layout( ).
  ls_key-report = sy-repid.
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  CALL METHOD lo_layout->set_initial_layout
    EXPORTING
      value = p_vari.
*Get columns
  CALL METHOD lo_table->get_columns
    RECEIVING
      value = lo_columns.
  lo_columns->set_optimize( 'X' ).
*  lo_columns->clear( 'FINALAPP' ).
******Change ALV Fields  - title etc.
  PERFORM alv_fields USING lo_columns lo_column.
******Display ALV
  CALL METHOD lo_table->display.
ENDFORM.                    " PERFORM_SALV
*&---------------------------------------------------------------------*
*&      Form  HANDLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ROW  text
*      -->P_COLUMN  text
*----------------------------------------------------------------------*
FORM handle_click  USING iv_row TYPE salv_de_row
                          iv_column TYPE salv_de_column.

  DATA: ls_output LIKE LINE OF it_output,
        ls_ekpo TYPE ekpo,
        txt2(50).
  DATA: projk_out(24) TYPE c.
  DATA: bdcdata_wa  TYPE bdcdata,
        bdcdata_tab TYPE TABLE OF bdcdata.
  DATA opt TYPE ctu_params.

  READ TABLE it_output INTO ls_output INDEX iv_row.
  CHECK sy-subrc = 0.
  CASE iv_column.
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
      SELECT SINGLE * INTO ls_ekpo FROM ekpo
                WHERE ebeln = ls_output-ebeln.
      IF sy-subrc <> 0.
        CONCATENATE 'Please check PO' ls_output-ebeln INTO txt2 SEPARATED BY space.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Purchase Order'
            txt1  = 'PO as reference document is not available'
            txt2  = txt2
*           TXT3  = ' '
*           TXT4  = ' '
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
    WHEN 'NPLNR'.
      SET PARAMETER ID 'ANR' FIELD ls_output-nplnr.
      CALL TRANSACTION 'CN23' AND SKIP FIRST SCREEN.
    WHEN 'KOSTL'.
      SET PARAMETER ID 'KOS' FIELD ls_output-kostl.
      CALL TRANSACTION 'KS03' AND SKIP FIRST SCREEN.
    WHEN 'SAKNR'.

      CLEAR bdcdata_wa.
      bdcdata_wa-program  = 'SAPLGL_ACCOUNT_MASTER_MAINTAIN'.
      bdcdata_wa-dynpro   = '2001'.
      bdcdata_wa-dynbegin = 'X'.
      APPEND bdcdata_wa TO bdcdata_tab.
      CLEAR bdcdata_wa.

      bdcdata_wa-fnam = 'GLACCOUNT_SCREEN_KEY-SAKNR'.
      bdcdata_wa-fval = ls_output-saknr.
      APPEND bdcdata_wa TO bdcdata_tab.

      CLEAR bdcdata_wa.
      bdcdata_wa-fnam = 'GLACCOUNT_SCREEN_KEY-BUKRS'.
      bdcdata_wa-fval = ls_output-bukrs.
      APPEND bdcdata_wa TO bdcdata_tab.

      CLEAR bdcdata_wa.
      bdcdata_wa-fnam = 'BDC_OKCODE'.
      bdcdata_wa-fval = 'ACC_SHOW'.
      APPEND bdcdata_wa TO bdcdata_tab.

      CALL TRANSACTION 'FS00' USING bdcdata_tab  MODE 'E'.

    WHEN 'PROJK'.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = ls_output-projk
        IMPORTING
          output = projk_out.

      SET PARAMETER ID 'PRO' FIELD projk_out.
      CALL TRANSACTION 'CJ03' AND SKIP FIRST SCREEN.
    WHEN 'AUFNR'.
      SET PARAMETER ID 'ANR' FIELD ls_output-aufnr.
      CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.
    WHEN 'TNOTES' OR 'RECNAM' OR 'NOTE1' OR 'NOTE2'.
      PERFORM display_notes USING ls_output.
  ENDCASE.
ENDFORM.                " HANDLE_CLICK1
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LO_COLUMNS  text
*      -->P_LO_COLUMN  text
*----------------------------------------------------------------------*
FORM alv_fields  USING    io_columns TYPE REF TO cl_salv_columns_table
                          io_column  TYPE REF TO cl_salv_column_table.


  DATA: lv_column     TYPE lvc_fname,
        lv_long_text  TYPE scrtext_l,
        lv_short_text TYPE scrtext_s,
        lv_med_text   TYPE scrtext_m,
        lv_hotspot TYPE xfeld.

  LOOP AT gt_comp_tab.
    CLEAR: lv_long_text,
           lv_short_text,
           lv_med_text,
           lv_hotspot.
    lv_column = gt_comp_tab-compname.
    CASE lv_column.
      WHEN 'XBLNR'.
        lv_long_text  = 'Reference'.
        lv_med_text   = 'Reference'.
        lv_short_text = 'Reference'.
      WHEN 'MMINV'.
        lv_long_text  = 'MM Invoice'.
        lv_med_text   = 'MM Invoice'.
        lv_short_text = 'MM Invoice'.
        lv_hotspot = 'X'.
      WHEN 'EBELN'.
        lv_hotspot = 'X'.

      WHEN 'BELNR'.
        lv_hotspot = 'X'.
      WHEN 'SAKNR'.
        lv_hotspot = 'X'.
      WHEN 'KOSTL'.
        lv_hotspot = 'X'.
      WHEN 'NPLNR'.
        lv_hotspot = 'X'.
      WHEN 'PROJK'.
        lv_hotspot = 'X'.
      WHEN 'AUFNR'.
        lv_hotspot = 'X'.
      WHEN 'ROUTE_OWNER'.
        lv_long_text  = 'Route Code Owner'.
        lv_med_text   = 'Route Code Owner'.
        lv_short_text = 'RC Owner'.
      WHEN 'WI_IDC'.
        lv_long_text  = 'Workflow ID'.
        lv_med_text   = 'Workflow ID'.
        lv_short_text = 'Workflow ID'.
      WHEN 'WI_CREATOR'.
        lv_long_text  = 'Parked By'.
        lv_med_text   = 'Parked By'.
        lv_short_text = 'Parked By'.
      WHEN 'WI_AAGENT'.
        lv_long_text  = 'Act Agent of current Task'.
        lv_med_text   = 'Act Agent of current Task'.
        lv_short_text = 'Act.Agent'.
      WHEN 'WI_ITEM_STAT'.
        lv_long_text  = 'Status'.
        lv_med_text   = 'Status'.
        lv_short_text = 'Status'.
      WHEN 'VENAMT'.
        lv_long_text  = 'Vendor Amount'.
        lv_med_text   = 'Vendor Amount'.
        lv_short_text = 'Vendor Amount'.
      WHEN 'WI_CD'.
        lv_long_text  = 'WI Creation Date'.
        lv_med_text   = 'WI Creation Date'.
        lv_short_text = 'WI Cr.Date'.
      WHEN 'WI_LED'.
        lv_long_text  = 'Doc. Approval Date'.
        lv_med_text   = 'Doc. Approval Date'.
        lv_short_text = 'Doc.App.Date'.
      WHEN 'WI_DAYS'.
        lv_long_text  = 'No days for approval'.
        lv_med_text   = '# days for approval'.
        lv_short_text = '# days for approval'.
      WHEN 'TNOTES'.
        lv_long_text  = 'Total Notes'.
        lv_med_text   = 'Total Notes' .
        lv_short_text = 'Total Notes'.
        lv_hotspot = 'X'.
      WHEN 'RECNAM'.
        lv_long_text  = 'Notes Reciever(s)'.
        lv_med_text   = 'Notes Reciever(s)'.
        lv_short_text = 'N.Reciever(s)'.
        lv_hotspot = 'X'.
      WHEN 'NOTE1'.
        lv_long_text  = 'Notes1'.
        lv_med_text   = 'Notes1'.
        lv_short_text = 'Notes1'.
        lv_hotspot = 'X'.
      WHEN 'NOTE2'.
        lv_long_text  = 'Notes2'.
        lv_med_text   = 'Notes2'.
        lv_short_text = 'Notes2'.
      WHEN 'FINALAPP'.
        lv_long_text  = 'Final Approver'.
        lv_med_text   = 'Final App'.
        lv_short_text = 'FinalApp'.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    TRY.
        io_column ?= io_columns->get_column( lv_column ).
        IF lv_long_text IS NOT INITIAL.
          CALL METHOD io_column->set_long_text
            EXPORTING
              value = lv_long_text.
          CALL METHOD io_column->set_short_text
            EXPORTING
              value = lv_short_text.
          CALL METHOD io_column->set_medium_text
            EXPORTING
              value = lv_med_text.
        ENDIF.
*addition by rajamans
        if lv_long_text = 'Final Approver'.

          CALL METHOD IO_COLUMN->SET_VISIBLE
            EXPORTING
              VALUE = IF_SALV_C_BOOL_SAP=>FALSE.
        endif.
* additon ends here
        IF lv_hotspot IS NOT INITIAL.
          CALL METHOD io_column->set_cell_type
            EXPORTING
              value = if_salv_c_cell_type=>hotspot.
        ENDIF.
      CATCH cx_salv_not_found .
    ENDTRY.

  ENDLOOP.

ENDFORM.                    "alv_fields
