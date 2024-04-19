*&---------------------------------------------------------------------*
*& Report  ZWF_USER_TURNAROUND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  ZWF_USER_TURNAROUND  MESSAGE-ID ZFI_WORKFLOW.

tables: usr01.

type-pools: slis.                                 "ALV Declarations
*Data Declaration
TYPES: BEGIN OF t_out,
  bname TYPE xubname,
  wi_id TYPE SWW_WIID,
  desc TYPE SWW_WITEXT,
  crdt TYPE char10,
  exedt TYPE char10,
  task TYPE sww_task,
  atime TYPE SWI_T_MEAN,
  stime TYPE sytabix,
 END OF t_out.

DATA: W_aed TYPE SWW_AED,
      W_aet TYPE SWW_AET.

DATA: it_out TYPE STANDARD TABLE OF t_out INITIAL SIZE 0,
      wa_out TYPE t_out.

DATA: it_wihead TYPE TABLE OF SWWWIHEAD WITH HEADER LINE.

*ALV data declarations
data: fieldcatalog type slis_t_fieldcat_alv with header line,
      gd_tab_group type slis_t_sp_group_alv,
      gd_layout    type slis_layout_alv,
      gd_repid     like sy-repid.
DATA: T_header   TYPE slis_t_listheader.
DATA: gs_line TYPE slis_listheader.
data: z_text(60).

Select-OPTIONS: s_user for usr01-bname,
                s_date for syst-datum.

AT SELECTION-SCREEN.
  if S_user[] is INITIAL.
    message e000 with 'Specify User ID'.
  endif.

  if S_date[] is INITIAL.
    message e000 with 'Specify Date Range'.
  endif.

*Start-of-selection.
START-OF-SELECTION.

  select * from SWWWIHEAD into table it_wihead
    where wi_TYPE = 'W' and
          wi_aed in S_DATE and
          WI_AAGENT in S_USER.

  loop at it_wihead.
    clear wa_out.
    wa_out-bname = it_wihead-WI_AAGENT.
    wa_out-wi_id = it_wihead-wi_id.
    wa_out-desc = it_wihead-wi_text.
    wa_out-task = it_wihead-WI_RH_TASK.
    write it_wihead-wi_cd to wa_out-crdt.
    write it_wihead-wi_aed to wa_out-exedt.

    CALL FUNCTION 'SWI_GET_ACTUAL_ENDING_DATA'
      EXPORTING
        WI_ID       = it_wihead-wi_id
      IMPORTING
        WI_AED      = w_aed
        WI_AET      = w_aet
      EXCEPTIONS
        READ_FAILED = 1
        OTHERS      = 2.

    IF NOT W_AED IS INITIAL AND NOT W_AET IS INITIAL.
      CALL FUNCTION 'SWI_DURATION_DETERMINE'
        EXPORTING
          START_DATE = it_wihead-wi_cd
          END_DATE   = w_aed
          START_TIME = it_wihead-wi_ct
          END_TIME   = w_aet
        IMPORTING
          DURATION   = wa_out-stime.

      perform convert_time(rswimean)
              using wa_out-stime wa_out-atime.
    ENDIF.

    append wa_out to it_out.
  endloop.

  sort it_out by wi_id ASCENDING.

  perform build_fieldcatalog.
  perform build_layout.
  perform display_report.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .

  Perform field_cat using 'BNAME' 'User' .
  Perform field_cat using 'WI_ID' 'Workitem ID' .
  Perform field_cat using 'DESC' 'Description' .
  Perform field_cat using 'CRDT' 'Creation Date' .
  Perform field_cat using 'EXEDT' 'Execution Date' .
  Perform field_cat using 'TASK' 'Task ID' .
  Perform field_cat using 'ATIME' 'Processing Time' .
  Perform field_cat using 'STIME' 'Time in Seconds' .

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT .
  gd_layout-no_input          = 'X'.
  gd_layout-colwidth_optimize = 'X'.
  gd_layout-detail_popup = 'X'.
  gd_layout-numc_sum = 'X'.
  gd_layout-get_selinfos = 'X'.
  gd_layout-confirmation_prompt = 'X'.
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_REPORT .
  gd_repid = sy-repid.

  PERFORM build_header.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = gd_repid
      i_callback_top_of_page  = 'TOP-OF-PAGE' "see FORM
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      is_layout               = gd_layout
      it_fieldcat             = fieldcatalog[]
      i_save                  = 'X'
    TABLES
      t_outtab                = it_out
    EXCEPTIONS
      program_error           = 1
      others                  = 2.
  if sy-subrc <> 0.
  endif.
ENDFORM.                    " DISPLAY_REPORT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0191   text
*      -->P_0192   text
*----------------------------------------------------------------------*
FORM FIELD_CAT  USING p_field p_label.

  fieldcatalog-fieldname   = p_field.
  fieldcatalog-seltext_m   = p_label.
*  IF p_field = 'SUB'.
*    fieldcatalog-no_out = 'X'.
*  ENDIF.
  append fieldcatalog to fieldcatalog.
  clear  fieldcatalog.
ENDFORM.                    " FIELD_CAT

************************************************************************
* User commands
************************************************************************
FORM user_command USING rf_ucomm LIKE sy-ucomm
                       rs       TYPE slis_selfield.

  DATA: Z_ID TYPE SWW_WIID.

* User command
  CASE rf_ucomm.
    when '&IC1'.
      If rs-fieldname = 'WI_ID'.
        Z_ID = rs-value.
        CALL FUNCTION 'SAP_WAPI_DISPLAY_WORKITEM'
          EXPORTING
            WORKITEM_ID = Z_ID.
      else.
        message w000 with 'Select Workitem ID Only'.
      endif.
  ENDCASE.
  CLEAR rf_ucomm.
ENDFORM.                    "user_command

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP-OF-PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header.
ENDFORM.                    "TOP-OF-PAGE
*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_HEADER .

  CLEAR gs_line.
  gs_line-typ  = 'H'.
  gs_line-info = 'Workitem Turnaround Time Report'.
  APPEND gs_line TO t_header.

  IF NOT S_USER[] is initial.
    IF NOT S_USER-HIGH IS INITIAL.
      concatenate 'User :' 'From ' S_USER-low ' To ' S_USER-HIGH
      into z_text separated by space.
    ELSE.
      concatenate 'User :' S_USER-low
      into z_text separated by space.
    ENDIF.

    CLEAR gs_line.
    gs_line-typ  = 'S'.
    gs_line-info = z_text.
    APPEND gs_line TO t_header.
    Clear z_text.
  ENDIF.

  IF NOT S_DATE[] is initial.
    IF NOT S_DATE-HIGH IS INITIAL.
      concatenate 'Execution Date :' 'From ' S_DATE-low ' To ' S_DATE-HIGH
      into z_text separated by space.
    ELSE.
      concatenate 'Execution Date  :' S_DATE-low
      into z_text separated by space.
    ENDIF.

    CLEAR gs_line.
    gs_line-typ  = 'S'.
    gs_line-info = z_text.
    APPEND gs_line TO t_header.
    Clear z_text.
  ENDIF.
ENDFORM.                    " BUILD_HEADER
