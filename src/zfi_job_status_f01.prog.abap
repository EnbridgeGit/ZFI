*&---------------------------------------------------------------------*
*&  Include           ZFI_JOB_STATUS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA : lv_xparam TYPE zparamvalue.
  CONSTANTS : lc_user TYPE zparamsubtype VALUE 'USER'.
  CLEAR : gs_job_info.

  SELECT SINGLE value1 FROM zfit_xparam INTO lv_xparam
          WHERE paramtype = sy-repid AND
                subtype = lc_user.

  SELECT SINGLE * FROM tbtco INTO gs_job_info
     WHERE jobname IN p_job AND
           sdlstrtdt EQ sy-datum AND
           sdluname EQ lv_xparam AND
           status IN gr_status.

  IF sy-subrc = 0.

    DO.
      CLEAR gv_job_read_jobhead.
      CALL FUNCTION 'BP_JOB_READ' " BP_JOBLOG_READ
      EXPORTING
      job_read_jobcount     = gs_job_info-jobcount
      job_read_jobname      = gs_job_info-jobname
      job_read_opcode       = gc_read_jobhead_only
      IMPORTING
      job_read_jobhead      = gv_job_read_jobhead
      EXCEPTIONS
      invalid_opcode        = 1
      job_doesnt_exist      = 2
      job_doesnt_have_steps = 3
      OTHERS = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CASE gv_job_read_jobhead-status.
        WHEN 'P'. "'Scheduled'.
        WHEN 'S'. "'Released'.
        WHEN 'R'. "'Active'.
        WHEN 'Y'. "'Ready'
        WHEN 'F'. "'Completed'.

          CLEAR: gv_msg_text.
          CONCATENATE 'Job Completed'(026)
          gs_job_info-jobname
          INTO gv_msg_text SEPARATED BY ''.
          WRITE : / gv_msg_text.

*          PERFORM f_update_job_log USING
*          gs_job_info-jobcount
*          gs_job_info-jobname.
          EXIT.
        WHEN 'A'. "'Cancelled'.

          CLEAR: gv_msg_text.
          CONCATENATE 'Job Cancelled'(027)
          gs_job_info-jobname
          INTO gv_msg_text SEPARATED BY ''.
          WRITE : / gv_msg_text.

*          PERFORM f_update_job_log USING
*          gs_job_info-jobcount
*          gs_job_info-jobname.
          EXIT.

      ENDCASE.
    ENDDO.
  ELSE.
    MESSAGE text-t01 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  f_update_job_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_JOB_NUMBER  text
*      -->P_JOB_NAME    text
*----------------------------------------------------------------------*
FORM f_update_job_log USING p_job_number TYPE tbtcjob-jobcount
                            p_job_name TYPE tbtcjob-jobname.

  DATA: lt_job_log TYPE TABLE OF tbtc5,
        ls_job_log TYPE tbtc5.

  CLEAR: lt_job_log[],ls_job_log.

  CALL FUNCTION 'BP_JOBLOG_READ'
    EXPORTING
*     CLIENT                = SY-MANDT
      jobcount              = p_job_number
*     JOBLOG                = ' '
      jobname               = p_job_name
*     LINES                 =
*     DIRECTION             =
    TABLES
      joblogtbl             = lt_job_log
    EXCEPTIONS
      cant_read_joblog      = 1
      jobcount_missing      = 2
      joblog_does_not_exist = 3
      joblog_is_empty       = 4
      joblog_name_missing   = 5
      jobname_missing       = 6
      job_does_not_exist    = 7
      OTHERS                = 8.
  IF sy-subrc = 0 AND lt_job_log[] IS NOT INITIAL.

    MESSAGE s000(zrar_messages) WITH p_job_name '--' p_job_number .

    LOOP AT lt_job_log INTO ls_job_log.
      MESSAGE s000(zrar_messages) WITH ls_job_log-text.
    ENDLOOP.

  ENDIF.

ENDFORM.                    "f_update_job_log
*&---------------------------------------------------------------------*
*&      Form  INITIALISE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialise .

  CLEAR : gr_status,gs_status.

  gs_status-sign = 'I'.
  gs_status-option = 'EQ'.
  gs_status-low = 'S'.
  APPEND gs_status TO gr_status.
  CLEAR : gs_status.

  gs_status-sign = 'I'.
  gs_status-option = 'EQ'.
  gs_status-low = 'R'.
  APPEND gs_status TO gr_status.
  CLEAR : gs_status.

  gs_status-sign = 'I'.
  gs_status-option = 'EQ'.
  gs_status-low = 'P'.
  APPEND gs_status TO gr_status.
  CLEAR : gs_status.

ENDFORM.                    " INITIALISE
