*&---------------------------------------------------------------------*
*&  Include           ZFI_MAIL_POST_SPOOL_ERR_MAIN
*&---------------------------------------------------------------------*
*& Report Name          : ZFI_MAIL_POST_SPOOL_ERR                        *
*& Author               : KMB                                            *
*& Creation Date        : 30-Jan-2019                                    *
*& Object ID            : CHG0136225                                     *
*& Application Area     : FI                                             *
*& Description          : Mail Posting spool errors                      *
*&-----------------------------------------------------------------------*
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  -------------------------------   *
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 17-07-2019   KMB         D30K930039  CHG0152154 Issue with the program *
*                          D30K930226                                    *
**************************************************************************

START-OF-SELECTION.

  lv_date = sy-datum - 10.

  CLEAR : lv_spool.
  REFRESH : lt_tbtcp.
  SELECT listident sdldate
    FROM tbtcp
    INTO TABLE lt_tbtcp
    WHERE jobname = p_job
          AND sdldate >= lv_date.
  IF sy-subrc = 0.
    SORT lt_tbtcp DESCENDING BY sdldate.
    READ TABLE lt_tbtcp INTO ls_tbtcp INDEX 1.
    IF sy-subrc = 0.
      IF ls_tbtcp-listident IS NOT INITIAL.
        lv_spool =  ls_tbtcp-listident.
        CLEAR ls_tbtcp.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lv_spool IS NOT INITIAL.
    ##FM_SUBRC_OK
    CALL FUNCTION 'RSPO_RETURN_SPOOLJOB'
      EXPORTING
        rqident              = lv_spool
        desired_type         = 'RAW'
      IMPORTING
        real_type            = lv_doc_type
      TABLES
        buffer               = lt_tab
      EXCEPTIONS
        no_such_job          = 1
        job_contains_no_data = 2
        selection_empty      = 3
        no_permission        = 4
        can_not_access       = 5
        read_error           = 6
        type_no_match        = 7
        OTHERS               = 8.
    IF sy-subrc = 0.
      ls_str-sign   = 'I'.
      ls_str-option = 'CP'.
      ls_str-low    = 'E RW609 Error in document:*'.
      APPEND ls_str TO lr_str.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*      CLEAR lr_str.
      CLEAR ls_str.

      LOOP AT lt_tab INTO ls_tab WHERE line IN lr_str.
        CLEAR : lv_index, ls_tab1, lv_string1, lv_string2, lv_awref, lv_string, ls_ptrv_doc_it.
        CALL FUNCTION 'TEXT_SPLIT'
          EXPORTING
            length = 33
            text   = ls_tab-line
          IMPORTING
            line   = lv_string1
            rest   = lv_string2.
        SPLIT lv_string2 AT space INTO lv_awref lv_string. "Get document number from spool line
        IF lv_awref IS NOT INITIAL.
          lv_fiscal = sy-datum+0(4). "Current fiscal year
          SELECT *
            FROM ptrv_doc_it
            INTO ls_ptrv_doc_it
            UP TO 1 ROWS
            WHERE awref = lv_awref.
          ENDSELECT.
          IF sy-subrc = 0 AND
             ls_ptrv_doc_it-datv1+0(4) = lv_fiscal AND "Get entries of current fiscal year only
             ls_ptrv_doc_it-datb1+0(4) = lv_fiscal.
            ls_trip-lv_awref = ls_ptrv_doc_it-awref.
            ls_trip-lv_pernr = ls_ptrv_doc_it-pernr.
            ls_trip-lv_kostl = ls_ptrv_doc_it-kostl.
            ls_trip-lv_exbel = ls_ptrv_doc_it-exbel.
            ls_trip-lv_bukrs = ls_ptrv_doc_it-bukrs.
            APPEND ls_trip TO lt_trip.
            CLEAR ls_trip.
          ENDIF.
        ENDIF.
        CLEAR ls_tab.
      ENDLOOP.
      CLEAR lv_lines.
      SORT lt_trip BY lv_exbel lv_pernr. "Sort the entries by assignment and pernr
      DESCRIBE TABLE lt_trip LINES lv_lines.
      LOOP AT lt_trip INTO ls_trip.
        CLEAR: ls_str, lr_str, lv_index1, lv_flag, lv_line, lv_value, lv_str1, lv_str2, ls_trip1.
        lv_index1 = sy-tabix.
        lv_index1 = lv_index1 + 1.
        IF lv_index1 > lv_lines.
          lv_flag = abap_true. "Flag to indicate that the last entry reached
        ENDIF.

        ls_str-sign   = lc_i.
        ls_str-option = lc_cp.
        CONCATENATE text-003 ls_trip-lv_awref lc_s INTO ls_str-low.
        APPEND ls_str TO lr_str. "To only loop entries in error status
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019

        LOOP AT lt_tab INTO ls_tab WHERE line IN lr_str.
          CLEAR : lv_index, ls_tab1.
          lv_index = sy-tabix.
          lv_index = lv_index + 1.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*        CALL FUNCTION 'TEXT_SPLIT'
*          EXPORTING
*            length = 33
*            text   = ls_tab-line
*          IMPORTING
*            line   = lv_string1
*            rest   = lv_string2.
*        SPLIT lv_string2 AT space INTO lv_awref lv_string.
*        IF lv_awref IS NOT INITIAL.
*          SELECT SINGLE *
*            FROM ptrv_doc_it
*            INTO ls_ptrv_doc_it
*            WHERE awref = lv_awref.
*          IF sy-subrc = 0.
*            SELECT begda usrid_long
*              FROM pa0105
*              INTO TABLE lt_pa0105
*              WHERE pernr = ls_ptrv_doc_it-pernr AND
*                    subty = '0010'.
*            IF sy-subrc = 0.
*              SORT lt_pa0105 DESCENDING BY begda.
*              READ TABLE lt_pa0105 INTO ls_pa0105 INDEX 1.
*              IF sy-subrc = 0.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
          READ TABLE lt_tab INTO ls_tab1 INDEX lv_index.
          IF sy-subrc = 0.
            IF ls_tab1-line CS text-028. "Changed by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
              lv_line = 'WBS'.
              lv_value = '1'.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                    PERFORM send_mail USING ls_pa0105-usrid_long ls_ptrv_doc_it-exbel lv_line lv_value.
              PERFORM f_collect USING lv_line lv_value.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            ELSEIF ls_tab1-line CS text-029. "Changed by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
              lv_line = 'COST'.
              lv_value = ls_ptrv_doc_it-kostl.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                    PERFORM send_mail USING ls_pa0105-usrid_long ls_ptrv_doc_it-exbel lv_line lv_value.
              PERFORM f_collect USING lv_line lv_value.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            ELSEIF ls_tab1-line CS text-030. "Changed by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
              lv_line = 'CC1051'.
              lv_value = ls_ptrv_doc_it-bukrs.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                    PERFORM send_mail USING ls_pa0105-usrid_long ls_ptrv_doc_it-exbel lv_line lv_value.
              PERFORM f_collect USING lv_line lv_value.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            ELSEIF ls_tab1-line CS text-031. "Changed by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
              lv_line = 'CC1099'.
              lv_value = ls_ptrv_doc_it-bukrs.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                    PERFORM send_mail USING ls_pa0105-usrid_long ls_ptrv_doc_it-exbel lv_line lv_value.
              PERFORM f_collect USING lv_line lv_value.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            ELSEIF ls_tab1-line CS text-032. "Changed by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
              lv_line = 'CC4015'.
              lv_value = ls_ptrv_doc_it-bukrs.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                    PERFORM send_mail USING ls_pa0105-usrid_long ls_ptrv_doc_it-exbel lv_line lv_value.
              PERFORM f_collect USING lv_line lv_value.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            ELSEIF ls_tab1-line CS text-033. "Changed by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
              lv_line = 'CC1004'.
              lv_value = ls_ptrv_doc_it-bukrs.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                    PERFORM send_mail USING ls_pa0105-usrid_long ls_ptrv_doc_it-exbel lv_line lv_value.
              PERFORM f_collect USING lv_line lv_value.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            ELSEIF ls_tab1-line CS text-034.               "Changed by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
              lv_line = 'JV'.
              lv_value = '2'.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                    PERFORM send_mail USING ls_pa0105-usrid_long ls_ptrv_doc_it-exbel lv_line lv_value.
              PERFORM f_collect USING lv_line lv_value.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            ELSEIF ls_tab1-line CS text-035.    "Changed by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
              lv_line = 'CC'.
              lv_value = ls_ptrv_doc_it-bukrs.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                    PERFORM send_mail USING ls_pa0105-usrid_long ls_ptrv_doc_it-exbel lv_line lv_value.
              PERFORM f_collect USING lv_line lv_value.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            ELSE.
              lv_line = 'OTHERS'.
              CALL FUNCTION 'TEXT_SPLIT'
                EXPORTING
                  length = 8
                  text   = ls_tab1-line
                IMPORTING
                  line   = lv_str1
                  rest   = lv_str2.
              lv_value = lv_str2.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                    PERFORM send_mail USING ls_pa0105-usrid_long ls_ptrv_doc_it-exbel lv_line lv_value.
              PERFORM f_collect USING lv_line lv_value.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
            ENDIF.
          ENDIF.
          CLEAR : ls_pa0105.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
          CLEAR ls_tab.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
        ENDLOOP.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
        READ TABLE lt_trip INTO ls_trip1 INDEX lv_index1.
        IF lv_flag = abap_true OR ( sy-subrc = 0 AND ( ls_trip1-lv_exbel <> ls_trip-lv_exbel OR ls_trip1-lv_pernr <> ls_trip-lv_pernr ) ).
          CLEAR : lv_mail, lt_pa0105[], ls_pa0105.
          SELECT begda usrid_long
            FROM pa0105
            INTO TABLE lt_pa0105
            WHERE pernr = ls_trip-lv_pernr AND
                  subty = lc_10.
          IF sy-subrc = 0.
            SORT lt_pa0105 DESCENDING BY begda.
            READ TABLE lt_pa0105 INTO ls_pa0105 INDEX 1.
            IF sy-subrc = 0.
              lv_mail = ls_pa0105-usrid_long.
              PERFORM send_mail USING lv_mail ls_trip-lv_exbel ls_trip-lv_pernr.

              CONCATENATE text-036 ls_trip-lv_exbel text-037 ls_trip-lv_pernr INTO ls_mailtxt SEPARATED BY space.
              APPEND ls_mailtxt TO lt_collect.
              CLEAR ls_mailtxt.
              APPEND LINES OF lt_mailtxt TO lt_collect.
              CLEAR ls_mailtxt.
              APPEND ls_mailtxt TO lt_collect.

              CLEAR : lt_mailsubject, lt_mailrecipients, lt_mailtxt.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR ls_trip.
      ENDLOOP.
      IF lt_collect IS NOT INITIAL AND s_email IS NOT INITIAL.
        PERFORM f_send_col.
        CLEAR : lt_collect.
      ENDIF.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_PA0105_USRID_LONG  text
*      -->P_LS_TAB1_LINE  text
*----------------------------------------------------------------------*
FORM send_mail  USING    p_usrid_long TYPE comm_id_long
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*                         p_exbel TYPE ptrv_doc_it-exbel
*                         p_line TYPE string
*                         p_value TYPE string.
                         p_exbel TYPE ptrv_doc_it-exbel
                         p_pernr TYPE ptrv_doc_it-pernr.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019

*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
* Data Declarations
*  DATA: lt_mailsubject     TYPE sodocchgi1.
*  DATA: lt_mailrecipients  TYPE STANDARD TABLE OF somlrec90.
*  DATA: ls_mailrecipients  TYPE somlrec90.
*  DATA: lt_mailtxt         TYPE STANDARD TABLE OF soli.
*  DATA: ls_mailtxt         TYPE soli.
*  DATA : lv_sub TYPE string.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019

* Recipients
  ls_mailrecipients-rec_type = lc_u.
  ls_mailrecipients-receiver = p_usrid_long.
  APPEND ls_mailrecipients TO lt_mailrecipients.
  CLEAR ls_mailrecipients.

* Subject.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
  CONCATENATE text-000 p_exbel text-002 INTO lt_mailsubject-obj_descr SEPARATED BY space.
*  lt_mailsubject-obj_name = text-000.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
  lt_mailsubject-obj_langu = sy-langu.
*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*  lt_mailsubject-obj_descr = lt_mailsubject-obj_name.
*EOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019

*BOC by KMB on CHG0152154 DFCT0017426 Issue with program 17.7.2019
*body
*  IF p_line = 'WBS'.
*    CONCATENATE text-011 text-012 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-007 text-008  INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    ls_mailtxt = text-009.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-015 text-016  INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*  ELSEIF p_line = 'COST'.
*    ls_mailtxt = text-011.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-012 text-013 p_value '.' INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-014 text-015 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    ls_mailtxt = text-016.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*  ELSEIF p_line = 'CC1051'.
*    CONCATENATE text-011 text-012 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    ls_mailtxt = text-017.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-018 text-019 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*  ELSEIF p_line = 'CC1099'.
*    CONCATENATE text-011 text-012 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    ls_mailtxt = text-022.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-023 text-024 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*  ELSEIF p_line = 'CC4015'.
*    CONCATENATE text-011 text-012 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    ls_mailtxt = text-025.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-026 text-027 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*  ELSEIF p_line = 'CC1004'.
*    CONCATENATE text-011 text-012 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-028 text-029 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-030 text-031 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*  ELSEIF p_line = 'JV'.
*    CONCATENATE text-011 text-012 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    ls_mailtxt = text-032.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-033 text-034 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*  ELSEIF p_line = 'CC'.
*    CONCATENATE text-011 text-012 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-035 p_value '.' INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-023 text-024 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*  ELSEIF p_line = 'OTHERS'.
*    CONCATENATE text-011 text-012 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    ls_mailtxt = p_value.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    ls_mailtxt = text-020.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
*    APPEND ls_mailtxt TO lt_mailtxt.
*    CLEAR ls_mailtxt.
*  ENDIF.

  IF lt_mailsubject IS NOT INITIAL AND lt_mailtxt IS NOT INITIAL AND lt_mailrecipients IS NOT INITIAL.
* Send Mail
    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = lt_mailsubject
        put_in_outbox              = lc_x
      TABLES
        object_content             = lt_mailtxt
        receivers                  = lt_mailrecipients
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc EQ 0.
      COMMIT WORK.
*   Push mail out from SAP outbox
*    SUBMIT rsconn01 WITH mode = 'INT' AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  F_COLLECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_LINE  text
*      -->P_LV_VALUE  text
*----------------------------------------------------------------------*
FORM f_collect  USING    p_line
                         p_value.
*body of email
  IF p_line = text-028.
    CONCATENATE text-011 text-013 text-014 text-012 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-015 text-016  INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
*BOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fixing
*  ELSEIF p_line = text-029.
  ELSEIF p_line = text-038.
*EOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fixing
    ls_mailtxt = text-011.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-026 text-027 p_value '.' INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
*BOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
*  ELSEIF p_line = text-030.
  ELSEIF p_line = text-039.
*EOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
    CONCATENATE text-011 text-017 text-012 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
*BOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
*  ELSEIF p_line = text-031.
  ELSEIF p_line = text-040.
*EOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
    CONCATENATE text-011 text-018 text-019 text-012 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
*BOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
*  ELSEIF p_line = text-032.
  ELSEIF p_line = text-041.
*EOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
    CONCATENATE text-011 text-020 text-021 text-012 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
*BOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
*  ELSEIF p_line = text-033.
  ELSEIF p_line = text-042.
*EOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
    CONCATENATE text-011 text-012 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-028 text-029 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-030 text-031 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
  ELSEIF p_line = text-034.
    CONCATENATE text-011 text-023 text-024 text-012 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
*BOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
*  ELSEIF p_line = text-035.
  ELSEIF p_line = text-043.
*EOC by KMB on 22.10.2019 CHG0152154 DFCT0017426 FUT issue fix
    CONCATENATE text-011 text-022 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
  ELSEIF p_line = 'OTHERS'.
    ls_mailtxt = text-011.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    ls_mailtxt = p_value.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-025 text-012 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
    CONCATENATE text-015 text-016 INTO ls_mailtxt SEPARATED BY space.
    APPEND ls_mailtxt TO lt_mailtxt.
    CLEAR ls_mailtxt.
  ENDIF.
  CLEAR ls_mailtxt.
  APPEND ls_mailtxt TO lt_mailtxt.

ENDFORM.                    " F_COLLECT
*&---------------------------------------------------------------------*
*&      Form  F_SEND_COL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_send_col.

* Recipients
  LOOP AT s_email.
    ls_mailrecipients-rec_type = lc_u.
    ls_mailrecipients-receiver = s_email-low.
    APPEND ls_mailrecipients TO lt_mailrecipients.
    CLEAR ls_mailrecipients.
  ENDLOOP.

* Subject.
  lt_mailsubject-obj_name = text-001.
  lt_mailsubject-obj_langu = sy-langu.
  lt_mailsubject-obj_descr = text-001.

* Send collective mail
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = lt_mailsubject
      put_in_outbox              = lc_x
    TABLES
      object_content             = lt_collect
      receivers                  = lt_mailrecipients
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " F_SEND_COL
