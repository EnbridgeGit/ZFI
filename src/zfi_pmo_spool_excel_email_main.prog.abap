*&---------------------------------------------------------------------*
*&  Include           ZFI_PMO_SPOOL_EXCEL_EMAIL_MAIN
*&---------------------------------------------------------------------*
*&-----------------------------------------------------------------------*
*& Report Name          : ZFI_PMO_SPOOL_EXCEL_EMAIL                      *
*& Author               : KMB                                            *
*& Creation Date        : 23-May-2019                                    *
*& Transport no.        : D30K929860                                     *
*& Object ID            : CHG0147746                                     *
*& Application Area     : FI                                             *
*& Description          : Mail excel with job spool data                 *
*&-----------------------------------------------------------------------*
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 07-06-2019   KMB         D30K929919  CHG0147746 ENHC0025526 Excl       *
*                                      attachement with spool 7.6.2019   *
**************************************************************************
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 24-06-2019   KMB         D30K929963  CHG0153285 ENHC0025526 Excl       *
*                                      attachement with spool 24.6.2019  *
**************************************************************************
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 24-06-2019   KMB         D30K929981  CHG0147746 ENHC0025526 Excl       *
*                                      attachement with spool 24.6.2019  *
**************************************************************************
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 24-06-2019   KMB         D30K930025  CHG0153285 DFCT0017463 date field *
*                                      in selection screen    12.7.2019  *
**************************************************************************
**************************************************************************
*                           Modification Log                             *
* Changed On   Changed By  CTS         Description                       *
* -----------  ----------  ----------  ----------------------------------*
* DD-MMM-YYYY  User ID     TR#         Change Description                *
* 15-07-2019   KMB         D30K930029  CHG0153285 DFCT0017463 multiple   *
*                                      email in selection screen 15.7.19 *
**************************************************************************

INITIALIZATION.
  PERFORM f_build_range.

AT SELECTION-SCREEN.
  PERFORM f_email_val.   "Changes by KMB CHG0147746 ENHC0025526 Excl attachement with spool 7.6.2019

START-OF-SELECTION.
  PERFORM f_data_fetch.  "Changes by KMB CHG0147746 ENHC0025526 Excl attachement with spool 7.6.2019

END-OF-SELECTION.
  PERFORM f_data_arrange. "Changes by KMB CHG0147746 ENHC0025526 Excl attachement with spool 7.6.2019

*&---------------------------------------------------------------------*
*&      Form  F_SENDMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_sendmail.
  TRY.
      CLEAR : gv_string_n, gt_main_text[], gv_string_n1, gv_attachment_name, gv_email.
      "to create persistent send request
      gcl_send_request = cl_bcs=>create_persistent( ).

*     to create and set document with attachment
*     create document object from internal table with text
      CONCATENATE text-013 sy-datum sy-uzeit text-014 INTO gv_string_n SEPARATED BY space.
      APPEND gv_string_n TO gt_main_text.

      CLEAR gv_string_n.
      CONCATENATE text-013 sy-datum sy-uzeit INTO gv_string_n SEPARATED BY space.
      gv_string_n1 = gv_string_n.

      gcl_document = cl_document_bcs=>create_document(
        i_type    = gc_raw
        i_text    = gt_main_text    "emial body
        i_subject = gv_string_n1 ). "email subject

      CONCATENATE text-015 sy-datum gc_un sy-uzeit INTO gv_attachment_name. "Attachment name

*     add the spread sheet as attachment to document object
      gcl_document->add_attachment(
        i_attachment_type    = 'xls'
        i_attachment_subject = gv_attachment_name   "Name of Excel File
        i_attachment_size    = gv_size
        i_att_content_hex    = gt_binary_content ).

*     add document object to send request
      gcl_send_request->set_document( gcl_document ).

*     add recipient (e-mail address)
*     create recipient object
*BOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 15.7.2019
*      gv_email = p_email.
      LOOP AT s_email.
      gv_email = s_email-low.
*EOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 15.7.2019
      gif_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
      gcl_send_request->add_recipient( gif_recipient ).
      ENDLOOP. "Added by kmb on CHG0153285 DFCT0017463 Date field selection screen 15.7.2019

*     send document
      gv_sent_to_all = gcl_send_request->send( i_with_error_screen = 'X' ).

      COMMIT WORK.

      IF gv_sent_to_all IS INITIAL.
        MESSAGE text-018 TYPE gc_e.
      ELSE.
        MESSAGE text-016 TYPE gc_i.
      ENDIF.

* exception handling
    CATCH cx_bcs INTO gex_bcs_exception.
      MESSAGE i865(so) WITH gex_bcs_exception->error_type.
  ENDTRY.

ENDFORM.                    " F_SENDMAIL
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_build_range.

   "Build range to delete all unwanted lines from spool data
  gs_str-sign   = gc_i.
  gs_str-option = gc_cp.
  gs_str-low    = text-020.
  APPEND gs_str TO gt_str.
  CLEAR gs_str.

  gs_str-sign   = gc_i.
  gs_str-option = gc_cp.
  gs_str-low    = text-021.
  APPEND gs_str TO gt_str.
  CLEAR gs_str.

  gs_str-sign   = gc_i.
  gs_str-option = gc_cp.
  gs_str-low    = text-022.
  APPEND gs_str TO gt_str.
  CLEAR gs_str.

  gs_str-sign   = gc_i.
  gs_str-option = gc_cp.
  gs_str-low    = text-023.
  APPEND gs_str TO gt_str.
  CLEAR gs_str.

  gs_str-sign   = gc_i.
  gs_str-option = gc_cp.
  gs_str-low    = gc_st.
  APPEND gs_str TO gt_str.
  CLEAR gs_str.

  gs_str-sign   = gc_i.
  gs_str-option = gc_cp.
  gs_str-low    = gc_ln.
  APPEND gs_str TO gt_str.
  CLEAR gs_str.

  gs_str-sign   = gc_i.
  gs_str-option = gc_cp.
  gs_str-low    = gc_doc.
  APPEND gs_str TO gt_str.
  CLEAR gs_str.

  gs_str-sign   = gc_i.
  gs_str-option = gc_cp.
  gs_str-low    = gc_ln1.
  APPEND gs_str TO gt_str.
  CLEAR gs_str.
ENDFORM.                    " F_BUILD_RANGE
*&---------------------------------------------------------------------*
*&      Form  F_DATA_FETCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_fetch .

  CLEAR : gv_spool, gv_listident.

*BOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 12.7.2019
*BOC by KMB on CHG0147746 ENHC0025526 Excl attachement with spool 3.7.2019
*  IF p_spool IS NOT INITIAL.
  IF p_spool IS NOT INITIAL AND p_date IS INITIAL.
*EOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 12.7.2019
    SELECT listident
    FROM tbtcp
    INTO gv_listident
    UP TO 1 ROWS
    WHERE jobname = text-000 AND
          sdldate = sy-datum AND
          listident = p_spool.
    ENDSELECT.
*BOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 12.7.2019
*  ELSE.
  ELSEIF p_spool IS INITIAL AND p_date IS INITIAL.
*EOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 12.7.2019
*EOC by KMB on CHG0147746 ENHC0025526 Excl attachement with spool 3.7.2019
    "Get spool number for job BSMSRKPEP003DP
    SELECT listident
      FROM tbtcp
      INTO gv_listident
      UP TO 1 ROWS
      WHERE jobname = text-000 AND
            sdldate = sy-datum AND
            listident IS NOT NULL. "ADDED by KMB on 24.6.2019 CHG0147746
    ENDSELECT.
*BOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 12.7.2019
  ELSEIF p_date IS NOT INITIAL and p_spool IS INITIAL.
     SELECT listident
      FROM tbtcp
      INTO gv_listident
      UP TO 1 ROWS
      WHERE jobname = text-000 AND
            sdldate = p_date AND
            listident IS NOT NULL. "ADDED by KMB on 24.6.2019 CHG0147746
    ENDSELECT.
*EOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 12.7.2019
  ENDIF."Added by KMB on CHG0147746 ENHC0025526 Excl attachement with spool 3.7.2019
  IF sy-subrc = 0.
    gv_spool =  gv_listident.
  ENDIF.
ENDFORM.                    " F_DATA_FETCH
*&---------------------------------------------------------------------*
*&      Form  F_DATA_ARRANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_data_arrange .
  IF gv_spool IS NOT INITIAL.
    CLEAR : gt_tab[], gv_doc_type.
    "Get spool data for the spool number
    ##FM_SUBRC_OK
    CALL FUNCTION 'RSPO_RETURN_SPOOLJOB'
      EXPORTING
        rqident              = gv_spool
        desired_type         = gc_raw
      IMPORTING
        real_type            = gv_doc_type
      TABLES
        buffer               = gt_tab
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

      "Delete all unwanted lines from spool data
      DELETE gt_tab FROM 1 TO 70.
      DELETE gt_tab WHERE line IN gt_str.
      CLEAR: gs_str, gt_str[].

      IF gt_tab IS NOT INITIAL.
*        "Excel header
        "columns are separated by GC_TAB
        " line ends with GC_CRLF
        CONCATENATE text-002 text-001 text-003 text-004 text-005
        text-006 text-007 text-008 text-009 text-010 text-011 text-012
        INTO gv_string SEPARATED BY gc_tab.
        CONCATENATE gv_string gc_crlf INTO gv_string.

        "Excel data
        LOOP AT gt_tab INTO gs_tab.
          CLEAR: gt_itab[], gt_itab2[], gv_index, gs_itab.
          SPLIT gs_tab AT '|' INTO TABLE gt_itab.
          gt_itab2[] = gt_itab[].
          LOOP AT gt_itab ASSIGNING <fs_itab>.
            IF <fs_itab> IS ASSIGNED.
              CASE sy-tabix.
                WHEN 2.
                  "Swap Document number and project definition columns
                  gv_index = sy-tabix + 1.
                  READ TABLE gt_itab2 INTO gs_itab INDEX gv_index.
                  IF sy-subrc = 0.
                    <fs_itab> = gs_itab."Project definition column
                  ENDIF.
                WHEN 3.
                  "Swap Document number and project definition columns
                  gv_index = sy-tabix - 1.
                  READ TABLE gt_itab2 INTO gs_itab INDEX gv_index.
                  IF sy-subrc = 0.
                    <fs_itab> = gs_itab."Document number column
                  ENDIF.
                WHEN 11.
                  "Val/COArea Crcy Current display : 324.00- Proposed display: -324.00
                  IF <fs_itab> CA '-'.
                    CONDENSE <fs_itab>.
                    REPLACE ALL OCCURRENCES OF '-' IN <fs_itab> WITH space.
                    CONCATENATE '-' <fs_itab> INTO <fs_itab>.
                  ENDIF.
                WHEN OTHERS.
*                  "do nothing
              ENDCASE.
              CONCATENATE gs_tab1 <fs_itab> '|'  INTO gs_tab1.
              CLEAR : gs_tab.
            ENDIF.
          ENDLOOP.
          "giving tab and line end
          REPLACE ALL OCCURRENCES OF gc_ln2 IN gs_tab1 WITH gc_tab.
          REPLACE ALL OCCURRENCES OF gc_tab IN gs_tab1+0(1) WITH space.
          CONDENSE gs_tab1.
          CONCATENATE gv_string gs_tab1 gc_crlf INTO gv_string.
          CLEAR gs_tab1.
        ENDLOOP.

        TRY.
            cl_bcs_convert=>string_to_solix(
              EXPORTING
                iv_string   = gv_string
                iv_codepage = '4103'  "suitable for MS Excel, leave empty
                iv_add_bom  = 'X'     "for other doc types
              IMPORTING
                et_solix  = gt_binary_content
                ev_size   = gv_size ).
          CATCH cx_bcs.
            MESSAGE e445(so).
        ENDTRY.

        "Send mail with attachment
        PERFORM f_sendmail.
      ENDIF.
    ELSE.
      MESSAGE text-017 TYPE gc_i DISPLAY LIKE gc_e.
    ENDIF.
  ELSE.
    MESSAGE text-017 TYPE gc_i DISPLAY LIKE gc_e.
  ENDIF.
ENDFORM.                    " F_DATA_ARRANGE
*&---------------------------------------------------------------------*
*&      Form  F_EMAIL_VAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_email_val .
  "Validation for valid email id
** If @ is present, more than once. Error out
  CLEAR: gv_count, gv_badpattern.
*BOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 15.7.2019
*  FIND ALL OCCURRENCES OF '@' IN p_email MATCH COUNT gv_count.
*  IF gv_count > 1.
*    gv_badpattern = 1.
*  ENDIF.
*
*** If , is present, once, Error out
*  CLEAR gv_count.
*  FIND ALL OCCURRENCES OF ',' IN p_email MATCH COUNT gv_count.
*  IF gv_count > 0.
*    gv_badpattern = gv_badpattern + 1.
*  ENDIF.
*
*  FIND REGEX gc_mailpattern  IN p_email IGNORING CASE .
*  IF sy-subrc <> 0 OR gv_badpattern > 0.
*    MESSAGE text-019 TYPE gc_e.
*  ENDIF.

 LOOP AT s_email.
  FIND ALL OCCURRENCES OF '@' IN s_email-low MATCH COUNT gv_count.
  IF gv_count > 1.
    gv_badpattern = 1.
  ENDIF.

** If , is present, once, Error out
  CLEAR gv_count.
  FIND ALL OCCURRENCES OF ',' IN s_email-low MATCH COUNT gv_count.
  IF gv_count > 0.
    gv_badpattern = gv_badpattern + 1.
  ENDIF.

  FIND REGEX text-024 IN s_email-low IGNORING CASE .
  IF sy-subrc <> 0 OR gv_badpattern > 0.
    CLEAR : s_email[].
    MESSAGE text-019 TYPE gc_e.
  ENDIF.
 ENDLOOP.
*EOC by kmb on CHG0153285 DFCT0017463 Date field selection screen 15.7.2019
ENDFORM.                    " F_EMAIL_VAL
