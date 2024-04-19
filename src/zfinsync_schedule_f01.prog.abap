*&---------------------------------------------------------------------*
*&  Include           ZFINSYNC_SCHEDULE_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFINSYNC_SCHEDULE                             *
* Include            :   ZFINSYNC_SCHEDULE_F01                         *
* Author             :   AKMADASU                                      *
* Date               :   Jan 23, 2022                                  *
* Technical Contact  :   Ashok Madasu                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  To Schedule Finance Sync IDOC Process, PGI AMD *
*                    :  Billing Jobs paralelly in BACKGROUND           *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 23-Jan-2022  AKMADASU    D30K931986-Initial development              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SERVER .
*--Getting the application server details

  CLEAR: lt_instances[],i_server_tb[].
  CALL FUNCTION 'SMLG_GET_DEFINED_SERVERS'
    EXPORTING
      grouptype          = 'S'         " RZLLITAB-GROUPTYPE    Group Type (' '= Logon, 'S'= Server)
      groupname          = p_sergrp    " RZLLITAB-CLASSNAME   (Group Name (' '= All Instances))
    TABLES
      instances          = lt_instances
    EXCEPTIONS
      invalid_group_type = 1
      no_instances_found = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    CLEAR: lv_msg_text.
    CONCATENATE 'No instances found for'(030)
                p_sergrp
                INTO lv_msg_text SEPARATED BY ''.
    MESSAGE lv_msg_text TYPE 'E'.

  ELSE.
*--*Get host name
    CALL FUNCTION 'TH_SERVER_LIST'
      TABLES
        list   = sys_tabl
      EXCEPTIONS
        OTHERS = 99.
    IF sy-subrc <> 0.
      CLEAR: lv_msg_text.
      CONCATENATE 'No server list found for'(031)
                   p_sergrp
                  INTO lv_msg_text SEPARATED BY ''.
      MESSAGE lv_msg_text TYPE 'E'.

    ENDIF.
    LOOP AT lt_instances INTO lst_instances.
      CLEAR: lst_server_tb.
      READ TABLE sys_tabl WITH KEY name = lst_instances-applserver.
      IF sy-subrc EQ 0.
        v_host = sys_tabl-host.
        v_server_name = lst_instances-applserver.

        lst_server_tb-host = sys_tabl-host.
        lst_server_tb-serv = lst_instances-applserver.

        APPEND lst_server_tb TO i_server_tb.
*        EXIT. " exit loop
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF i_server_tb[] IS INITIAL.
    CLEAR: lv_msg_text.
    CONCATENATE 'No server list found for'(031)
                 p_sergrp
                INTO lv_msg_text SEPARATED BY ''.
    MESSAGE lv_msg_text TYPE 'E'.
  ENDIF.

ENDFORM.                    " GET_SERVER
*&---------------------------------------------------------------------*
*&      Form  GET_PROCESS_IDOCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PROCESS_IDOCS .
  data: lv_num type i.
  SELECT docnum
         FROM edidc
         INTO TABLE it_edidc
         WHERE
*    ( status = c_status_in_ready_post OR
*           status = c_idoc_status_postponed )
*      AND
          status IN status
      AND docnum IN docnum
      AND mestyp IN mestyp
      AND mescod IN mescod
      AND mesfct IN mesfct
      AND sndprt IN sndprt
      AND sndprn IN sndprn
      AND sndpfc IN sndpfc
      AND credat IN credat
      AND cretim IN cretim.
*      AND test   IN test.
  IF sy-subrc = 0.
    sort it_edidc by docnum.
    DESCRIBE TABLE it_edidc LINES lv_lines.
    lv_jobs = p_totjob.
    IF lv_jobs GE lv_lines.
      lv_jobs = lv_lines.
    ENDIF.
    lv_packet1 = lv_lines / lv_jobs.
    lv_packet = lv_packet1.
*    lv_packet = lv_lines / lv_jobs.
    lv_to = lv_packet.
    clear:lv_num.
    DO lv_jobs TIMES.
      lv_num = lv_num + 1.
      REFRESH: it_edidc_t,lr_docnum.
      CLEAR: wa_edidc.
      APPEND LINES OF it_edidc FROM lv_from TO lv_to TO it_edidc_t.
      IF it_edidc_t is INITIAL.
        EXIT.
      ENDIF.
      lv_from = lv_to + 1.
      if lv_num eq 9.
        lv_to = lv_lines.
      else.
        lv_to = lv_to + lv_packet.
      endif.
      CLEAR:lv_low,lv_high.
      LOOP AT it_edidc_t INTO wa_edidc.
        if sy-tabix = 1.
          lv_low = wa_edidc-docnum.
        endif.
        wa_docnum-sign = 'I'.
        wa_docnum-option = 'EQ'.
        wa_docnum-low = wa_edidc-docnum.
        APPEND wa_docnum TO lr_docnum.
        CLEAR wa_docnum.
        lv_high = wa_edidc-docnum.
      ENDLOOP.
      CLEAR: lv_job_name,lv_job_number.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = lv_low
        IMPORTING
          OUTPUT = lv_low.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = lv_high
        IMPORTING
          OUTPUT = lv_high.

      CONCATENATE lv_Low '_' lv_high into lv_count1.
      CONDENSE LV_COUNT1 NO-GAPS.
      CONCATENATE lc_job '_'   " 13
*                  sy-datum   '_'   " 9
*                  sy-uzeit   '_'   " 7
                  lv_count1         " 2
                  INTO lv_job_name .
      CONDENSE LV_JOB_NAME NO-GAPS.
      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = lv_job_name
        IMPORTING
          jobcount         = lv_job_number
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.
      IF sy-subrc = 0 and lr_docnum is not INITIAL.
        SUBMIT ZFI_ORD_IDOC_PROCESS WITH docnum IN lr_docnum
                        WITH credat IN credat
                        WITH cretim IN cretim
                        WITH status IN status
                        WITH mestyp IN mestyp
                        WITH mescod IN mescod
                        WITH mesfct IN mesfct
                        WITH sndprt IN sndprt
                        WITH sndprn IN sndprn
                        WITH sndpfc IN sndpfc
                        WITH p_pcksiz = p_pcksiz
                        WITH test   IN test
                        WITH obj_type IN obj_type
                        WITH p_output = p_output
                        VIA JOB lv_job_name NUMBER lv_job_number
                                          AND RETURN.
        CLEAR: lst_job_info.
        lst_job_info-job_name = lv_job_name.
        lst_job_info-job_number = lv_job_number.
        APPEND lst_job_info TO lt_job_info.
      else.
      ENDIF.
    ENDDO.
    LOOP AT lt_job_info ASSIGNING <lfs_job_info>.
*--       Closing the Job
      DO.  " *--       Closing the Job
        CLEAR: lv_job_closed.

        DO.
          LOOP AT i_server_tb INTO lst_server_tb.        "#EC CI_NESTED
            v_server_name = lst_server_tb-serv.
            v_host = lst_server_tb-host.

            CLEAR: lv_total_btc, lv_free_btc,lv_percentage.
            CALL FUNCTION 'TH_COUNT_WPS'
              EXPORTING
                server       = v_server_name
              IMPORTING
                btc_wps      = lv_total_btc
                free_btc_wps = lv_free_btc
              EXCEPTIONS
                failed       = 1
                OTHERS       = 2.
            IF sy-subrc = 0.
*--*Calculate availability percentage
              lv_percentage = lv_free_btc / lv_total_btc * 100.
            ENDIF.

            IF lv_percentage GT v_serv_per..
              EXIT. " loop
            ENDIF.
          ENDLOOP.
          IF lv_percentage IS NOT INITIAL.
            IF lv_percentage GT v_serv_per..
              EXIT. " Do
            ENDIF.
          ENDIF.
        ENDDO.

*--*If availability is more than mainatined in constnat table
        IF lv_percentage GT v_serv_per.

          lv_job_number = <lfs_job_info>-job_number.
          lv_job_name = <lfs_job_info>-job_name.

          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              jobcount             = lv_job_number   "Job number
              jobname              = lv_job_name     "Job name
              strtimmed            = abap_true       "Start immediately
              targetsystem         = v_host
*             TARGETGROUP          = ' '
            EXCEPTIONS
              cant_start_immediate = 1
              invalid_startdate    = 2
              jobname_missing      = 3
              job_close_failed     = 4
              job_nosteps          = 5
              job_notex            = 6
              lock_failed          = 7
              OTHERS               = 8.
          IF sy-subrc = 0.

            lv_job_closed = abap_true.

            MESSAGE s001(za) WITH lv_job_name
                                             '->'
                                             lv_job_number
                                             'Submitted'(028).
          ELSE.
            lv_job_closed = abap_true.  " but some error

            MESSAGE ID sy-msgid
                  TYPE sy-msgty
                NUMBER sy-msgno
                  WITH sy-msgv1
                       sy-msgv2
                       sy-msgv3
                       sy-msgv4.

          ENDIF.
        ENDIF.
**              ENDIF.
        IF lv_job_closed = abap_true.
          EXIT.  " exit do enddo
        ENDIF.
      ENDDO.  " *--       Closing the Job
    ENDLOOP.

  ENDIF.
ENDFORM.                    " GET__PROCESS_IDOCS
