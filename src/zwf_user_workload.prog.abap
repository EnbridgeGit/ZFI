*&---------------------------------------------------------------------*
*& Report  ZWF_USER_WORKLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zwf_user_workload MESSAGE-ID zfi_workflow.

TABLES: usr01.

TYPE-POOLS: slis.                                 "ALV Declarations
*Data Declaration
TYPES: BEGIN OF t_out,
  bname TYPE xubname,
  wi_id TYPE sww_wiid,
  desc TYPE sww_witext,
  stat TYPE sww_wistat,
  action TYPE char12,
  sub TYPE xubname,
  crdt TYPE char10,
  exedt TYPE char10,
  task TYPE sww_task,
  wf_initiator TYPE xubname,
 END OF t_out.

DATA: it_out TYPE STANDARD TABLE OF t_out INITIAL SIZE 0,
      wa_out TYPE t_out,
      gt_simp_cont TYPE TABLE OF swr_cont,
      gs_simp_cont TYPE swr_cont.
DATA: it_exe TYPE uwlitemlist,
      it_forw TYPE uwlitemlist.
DATA: wa_exe TYPE uwlitem,
      wa_forw TYPE uwlitem.
DATA: it_wihead TYPE TABLE OF swwwihead WITH HEADER LINE.

DATA: w_bname TYPE xubname,
      w_index TYPE i,
      gv_top_wi_id TYPE sww_wiid,
      gv_resubmit TYPE xfeld,
      gv_rcconfirmd TYPE xfeld,
      gv_iniconfirmd TYPE xfeld.

*ALV data declarations
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_tab_group TYPE slis_t_sp_group_alv,
      gd_layout    TYPE slis_layout_alv,
      gd_repid     LIKE sy-repid.

SELECT-OPTIONS: s_user FOR usr01-bname,
                s_date FOR syst-datum.

AT SELECTION-SCREEN.
  IF s_user[] IS INITIAL.
    MESSAGE e000 WITH 'Specify User ID'.
  ENDIF.

  IF s_date[] IS INITIAL.
    MESSAGE e000 WITH 'Specify Date Range'.
  ENDIF.

*Start-of-selection.
START-OF-SELECTION.

  SELECT * FROM swwwihead INTO TABLE it_wihead
    WHERE wi_type = 'F' AND
          wi_cd IN s_date AND
          wi_cruser IN s_user AND
          wi_rh_task IN ('WS02000002').
  "Workflow template in US instance ('WS98300001', 'WS98300017').

  LOOP AT it_wihead.
    CLEAR wa_out.
    wa_out-bname = it_wihead-wi_cruser.
    wa_out-wi_id = it_wihead-wi_id.
    wa_out-action = 'Triggered'.
    wa_out-desc = it_wihead-wi_text.
    wa_out-stat = it_wihead-wi_stat.
    wa_out-task = it_wihead-wi_rh_task.
    WRITE it_wihead-wi_cd TO wa_out-crdt.
    CLEAR: gt_simp_cont.
    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id                    = wa_out-wi_id
*       LANGUAGE                       = SY-LANGU
*       USER                           = SY-UNAME
*       BUFFERED_ACCESS                = 'X'
*     IMPORTING
*       RETURN_CODE                    =
*       IFS_XML_CONTAINER              =
*       IFS_XML_CONTAINER_SCHEMA       =
     TABLES
       simple_container               = gt_simp_cont
*       MESSAGE_LINES                  =
*       MESSAGE_STRUCT                 =
*       SUBCONTAINER_BOR_OBJECTS       =
*       SUBCONTAINER_ALL_OBJECTS       =
              .
    LOOP AT gt_simp_cont INTO gs_simp_cont.
      CASE gs_simp_cont-element.
        WHEN '_WF_INITIATOR'.
          wa_out-wf_initiator = gs_simp_cont-value+2(12).
          EXIT.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
    APPEND wa_out TO it_out.
  ENDLOOP.

  CLEAR it_wihead.
  REFRESH it_wihead.

  SELECT * FROM swwwihead INTO TABLE it_wihead
    WHERE wi_stat NE 'CANCELLED' AND
          wi_cd IN s_date AND
          wi_aagent IN s_user AND
          wi_rh_task IN ('TS02000013').
  "Tasks in US Instance ('TS98300005', 'TS98300054').

  LOOP AT it_wihead.
    CLEAR: wa_out,
           gt_simp_cont,
           gv_resubmit,
           gv_rcconfirmd,
           gv_iniconfirmd.
    wa_out-wi_id = it_wihead-wi_id.
    "we need to get resubmit data. Same task is used for final approver
    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id                    = wa_out-wi_id
*       LANGUAGE                       = SY-LANGU
*       USER                           = SY-UNAME
*       BUFFERED_ACCESS                = 'X'
*     IMPORTING
*       RETURN_CODE                    =
*       IFS_XML_CONTAINER              =
*       IFS_XML_CONTAINER_SCHEMA       =
     TABLES
       simple_container               = gt_simp_cont
*       MESSAGE_LINES                  =
*       MESSAGE_STRUCT                 =
*       SUBCONTAINER_BOR_OBJECTS       =
*       SUBCONTAINER_ALL_OBJECTS       =
              .
    LOOP AT gt_simp_cont INTO gs_simp_cont.
      CASE gs_simp_cont-element.
        WHEN '_WI_COMP_EVENT_NAME'.
          IF gs_simp_cont-value(10) = 'RCREJECTED'.
            gv_resubmit = 'X'.
          ENDIF.
          IF gs_simp_cont-value(11) = 'RCCONFIRMED'.
            gv_rcconfirmd = 'X'.
          ENDIF.
          IF gs_simp_cont-value(21) = 'INITIATORCONFIRMATION'.
            gv_iniconfirmd = 'X'.
          ENDIF.
          EXIT.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
    IF gv_resubmit IS INITIAL AND
       gv_rcconfirmd IS INITIAL AND
       gv_iniconfirmd IS INITIAL.
      CONTINUE.
    ENDIF.
*    CHECK gv_resubmit IS NOT INITIAL.
    gv_top_wi_id = it_wihead-top_wi_id.
    wa_out-bname = it_wihead-wi_aagent.
    wa_out-wi_id = it_wihead-wi_id.
    IF gv_rcconfirmd IS NOT INITIAL.
      wa_out-action = 'RCConfirmed'.
    ELSEIF gv_resubmit IS NOT INITIAL.
      wa_out-action = 'ReSubmitted'.
    ELSEIF gv_iniconfirmd IS NOT INITIAL.
      wa_out-action = 'InitConfirm'.
    ENDIF.
    wa_out-desc = it_wihead-wi_text.
    wa_out-stat = it_wihead-wi_stat.
    wa_out-task = it_wihead-wi_rh_task.
    WRITE it_wihead-wi_cd TO wa_out-crdt.
    WRITE it_wihead-wi_aed TO wa_out-exedt.
    CLEAR: gt_simp_cont.
    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id                    = gv_top_wi_id "wa_out-wi_id
*       LANGUAGE                       = SY-LANGU
*       USER                           = SY-UNAME
*       BUFFERED_ACCESS                = 'X'
*     IMPORTING
*       RETURN_CODE                    =
*       IFS_XML_CONTAINER              =
*       IFS_XML_CONTAINER_SCHEMA       =
     TABLES
       simple_container               = gt_simp_cont
*       MESSAGE_LINES                  =
*       MESSAGE_STRUCT                 =
*       SUBCONTAINER_BOR_OBJECTS       =
*       SUBCONTAINER_ALL_OBJECTS       =
              .
    LOOP AT gt_simp_cont INTO gs_simp_cont.
      CASE gs_simp_cont-element.
        WHEN '_WF_INITIATOR'.
          wa_out-wf_initiator = gs_simp_cont-value+2(12).
          EXIT.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
    APPEND wa_out TO it_out.
  ENDLOOP.

  SORT it_out BY wi_id ASCENDING.

  PERFORM build_fieldcatalog.
  PERFORM build_layout.
  PERFORM display_report.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .

  PERFORM field_cat USING 'BNAME' 'User' .
  PERFORM field_cat USING 'WI_ID' 'Workitem ID' .
  PERFORM field_cat USING 'DESC' 'Description' .
  PERFORM field_cat USING 'STAT' 'Status' .
  PERFORM field_cat USING 'ACTION' 'Action' .
  PERFORM field_cat USING 'SUB' 'Substituted For' .
  PERFORM field_cat USING 'CRDT' 'Creation Date' .
  PERFORM field_cat USING 'EXEDT' 'Execution Date' .
  PERFORM field_cat USING 'TASK' 'Task ID' .
  PERFORM field_cat USING 'WF_INITIATOR' 'WF Initiator' .
ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .
  gd_layout-no_input          = 'X'.
  gd_layout-colwidth_optimize = 'X'.
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_report .
  gd_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = gd_repid
*     i_callback_top_of_page  = 'TOP-OF-PAGE' "see FORM
      i_callback_user_command = 'USER_COMMAND'
      is_layout               = gd_layout
      it_fieldcat             = fieldcatalog[]
      i_save                  = 'X'
    TABLES
      t_outtab                = it_out
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
  ENDIF.
ENDFORM.                    " DISPLAY_REPORT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0036   text
*      -->P_0037   text
*----------------------------------------------------------------------*
FORM field_cat  USING p_field p_label.

  fieldcatalog-fieldname   = p_field.
  fieldcatalog-seltext_m   = p_label.
  IF p_field = 'SUB'.
    fieldcatalog-no_out = 'X'.
  ENDIF.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

ENDFORM.                    " FIELD_CAT
************************************************************************
* User commands
************************************************************************
FORM user_command USING rf_ucomm LIKE sy-ucomm
                       rs       TYPE slis_selfield.

  DATA: z_id TYPE sww_wiid,
        w_bukrs TYPE bukrs,
        w_doc TYPE belnr_d,
        w_yr TYPE gjahr.
* User command
  CASE rf_ucomm.
    WHEN '&IC1'.
      IF rs-fieldname = 'WI_ID'.
        z_id = rs-value.
        CALL FUNCTION 'SAP_WAPI_DISPLAY_WORKITEM'
          EXPORTING
            workitem_id = z_id.

      ELSEIF rs-fieldname = 'DESC'.
        w_bukrs = rs-value+0(4).
        w_doc = rs-value+5(10).
        w_yr = rs-value+16(4).
        SET PARAMETER ID 'BLN' FIELD w_doc.
        SET PARAMETER ID 'BUK' FIELD w_bukrs.
        SET PARAMETER ID 'GJR' FIELD w_yr.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSE.
        MESSAGE w000 WITH 'Select WI ID or Description Only'.
      ENDIF.
  ENDCASE.
  CLEAR rf_ucomm.
ENDFORM.                    "user_command
