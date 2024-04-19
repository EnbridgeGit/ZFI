*&---------------------------------------------------------------------*
*&  Include           ZFI_AP_ERROR_WI_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_AP_ERROR_WI_F01                           *
*& Author             :  Kavya M B                                     *
*& Creation Date      :  14/11/2019                                    *
*& Object ID          :  CHG0153857                                    *
*& Application Area   :  FICO                                          *
*& Description        :  AP CI-SAP - automate job to locate invoice    *
*                        documents in workflow error status            *
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 14-11-2019   KMB         DECK920334  AP CI-SAP automate job to locate*
*                                      invoice documents in workflow   *
*                                      error status                    *
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .
  "get all errored work items for AP workflow
  SELECT wi_id wi_cd wi_ct wi_rh_task
  FROM swwwihead
  INTO TABLE gt_swwwihead
  WHERE wi_stat = 'ERROR'
    AND wi_cd IN s_date
    AND wi_rh_task = p_task.
  IF sy-subrc <> 0.
    CLEAR gt_swwwihead.
  ENDIF.
ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_SENDMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_sendmail .

  TRY.
      CLEAR :  gt_main_text[], gv_attachment_name, gv_email, gv_string_n, gv_string_n1.
      "to create persistent send request
      gcl_send_request = cl_bcs=>create_persistent( ).

*     to create and set document with attachment
*     create document object from internal table with text
      IF gt_swwwihead IS NOT INITIAL.
        CONCATENATE text-001 sy-sysid 'from' s_date-low 'to' s_date-high '.' INTO gv_string_n SEPARATED BY space.
        APPEND gv_string_n TO gt_main_text.
        CLEAR gv_string_n.

        gv_string_n = text-002.
        APPEND gv_string_n TO gt_main_text.
        CLEAR gv_string_n.
      ELSE.
        CONCATENATE text-033 s_date-low 'to' s_date-high '.' INTO gv_string_n SEPARATED BY space.
        APPEND gv_string_n TO gt_main_text.
        CLEAR gv_string_n.

        gv_string_n = text-034.
        APPEND gv_string_n TO gt_main_text.
        CLEAR gv_string_n.
      ENDIF.

      CLEAR gv_string_n.
      gv_string_n = text-000.
      gv_string_n1 = gv_string_n.

      gcl_document = cl_document_bcs=>create_document(
        i_type    = gc_raw
        i_text    = gt_main_text    "emial body
        i_subject = gv_string_n1 ). "email subject


      IF gt_binary_content IS NOT INITIAL.

        CONCATENATE text-003 sy-datum INTO gv_attachment_name. "Attachment name

*     add the spread sheet as attachment to document object
        gcl_document->add_attachment(
          i_attachment_type    = 'xls'
          i_attachment_subject = gv_attachment_name   "Name of Excel File
          i_attachment_size    = gv_size
          i_att_content_hex    = gt_binary_content ).

      ENDIF.

*     add document object to send request
      gcl_send_request->set_document( gcl_document ).



*     add recipient (e-mail address)
*     create recipient object
      IF p_email IS NOT INITIAL.
        gv_email = p_email.
        gif_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
        gcl_send_request->add_recipient( gif_recipient ).
      ENDIF.

*send mail to distribution list
      gif_recipient = cl_distributionlist_bcs=>getu_persistent( i_dliname = 'ZFARI004_PAY' i_private = ' ').
      CALL METHOD gcl_send_request->add_recipient
        EXPORTING
          i_recipient = gif_recipient
          i_express   = 'X'.

*     send document
      gv_sent_to_all = gcl_send_request->send( i_with_error_screen = 'X' ).

      COMMIT WORK.

      IF gv_sent_to_all IS INITIAL.
        MESSAGE text-028 TYPE gc_e.
      ELSE.
        MESSAGE text-029 TYPE gc_i.
      ENDIF.

* exception handling
    CATCH cx_bcs INTO gex_bcs_exception.
      MESSAGE i865(so) WITH gex_bcs_exception->error_type.
  ENDTRY.

ENDFORM.                    " F_SENDMAIL
*&---------------------------------------------------------------------*
*&      Form  F_FORM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_form_data .
  IF gt_swwwihead IS NOT INITIAL.

    CLEAR: gv_string1, gv_string, gs_swwwihead.
    "fill header for excel file
    CONCATENATE text-004 text-005 text-006 text-007 text-008 text-009 text-010
                text-011 text-012 text-013 text-014 text-015 text-016 text-017 text-018 text-019
    INTO gv_string1 SEPARATED BY gc_tab.

    CONCATENATE gv_string1 gc_crlf INTO gv_string.


    LOOP AT gt_swwwihead INTO gs_swwwihead.

      CLEAR gs_workflow.

      gs_workflow-wi_id  = gs_swwwihead-wi_id. "WI id
      gs_workflow-startd = gs_swwwihead-wi_cd. "Creation date
      gs_workflow-startt = gs_swwwihead-wi_ct. "Creation Time

      CLEAR: gt_container, gv_return.
      CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
        EXPORTING
          workitem_id      = gs_swwwihead-wi_id
        IMPORTING
          return_code      = gv_return
        TABLES
          simple_container = gt_container.
      IF gv_return = 0.
        CLEAR gs_container.
        READ TABLE gt_container INTO gs_container WITH KEY element = '_WF_INITIATOR'.
        IF sy-subrc = 0.
          gs_workflow-wf_init = gs_container-value+2. "WF initiator
        ENDIF.
        CLEAR gs_container.
        READ TABLE gt_container INTO gs_container WITH KEY element = 'AMOUNT'.
        IF sy-subrc = 0.
          gs_workflow-amount = gs_container-value.    "Amount
        ENDIF.
        CLEAR gs_container.
        READ TABLE gt_container INTO gs_container WITH KEY element = 'RCO'.
        IF sy-subrc = 0.
          gs_workflow-rco = gs_container-value+20(12). "RCO
        ENDIF.
        CLEAR gs_container.
        READ TABLE gt_container INTO gs_container WITH KEY element = 'FIPP'.
        IF sy-subrc = 0.
          gs_workflow-cocode  = gs_container-value+20(4). "Company code
          gs_workflow-docnum  = gs_container-value+24(10). "Document number
          gs_workflow-year    = gs_container-value+34(4).  "Year
        ENDIF.
      ENDIF.
      APPEND gs_workflow TO gt_workflow.
    ENDLOOP.

    IF gt_swwwihead IS NOT INITIAL.
      "Last work item details
      SELECT wi_id wi_stat wi_cd wi_ct wi_aagent wi_rh_task top_wi_id
        INTO TABLE gt_lastitem
        FROM swwwihead
        FOR ALL ENTRIES IN gt_swwwihead
        WHERE top_wi_id = gt_swwwihead-wi_id
          AND wi_type = gc_w
          AND wi_stat <> 'COMPLETED'
          AND wi_stat <> 'CANCELLED'.
    ENDIF.

    IF gt_workflow IS NOT INITIAL.
      "Vendor
      SELECT ausbk bukrs belnr gjahr bstat
        FROM vbkpf
        INTO TABLE gt_vbkpf
        FOR ALL ENTRIES IN gt_workflow
        WHERE ausbk = gt_workflow-cocode
          AND bukrs = gt_workflow-cocode
          AND belnr = gt_workflow-docnum
          AND gjahr = gt_workflow-year.
      IF sy-subrc = 0 AND gt_vbkpf IS NOT INITIAL.
        SELECT ausbk bukrs belnr gjahr lifnr
          FROM vbsegk
          INTO TABLE gt_vbsegk
          FOR ALL ENTRIES IN gt_vbkpf
          WHERE ausbk = gt_vbkpf-bukrs
            AND bukrs = gt_vbkpf-bukrs
            AND belnr = gt_vbkpf-belnr
            AND gjahr = gt_vbkpf-gjahr.
      ENDIF.

      "amount
      SELECT ausbk bukrs belnr gjahr wrbtr
        INTO TABLE gt_wrbtr
        FROM vbsegd
        FOR ALL ENTRIES IN gt_workflow
        WHERE ausbk = gt_workflow-cocode
        AND   bukrs = gt_workflow-cocode
        AND   belnr = gt_workflow-docnum
        AND   gjahr = gt_workflow-year.
      IF sy-subrc IS NOT INITIAL.
        SELECT ausbk bukrs belnr gjahr wrbtr
          INTO TABLE gt_wrbtr
          FROM vbsegk
          FOR ALL ENTRIES IN gt_workflow
          WHERE ausbk = gt_workflow-cocode
          AND   bukrs = gt_workflow-cocode
          AND   belnr = gt_workflow-docnum
          AND   gjahr = gt_workflow-year
          AND   umskz NE 'U'.
        IF sy-subrc IS NOT INITIAL.
          SELECT ausbk bukrs belnr gjahr wrbtr
            INTO TABLE gt_wrbtr
            FROM vbsegs
            FOR ALL ENTRIES IN gt_workflow
            WHERE ausbk = gt_workflow-cocode
            AND   bukrs = gt_workflow-cocode
            AND   belnr = gt_workflow-docnum
            AND   gjahr = gt_workflow-year.
        ENDIF.
      ENDIF.
    ENDIF.


    SORT: gt_workflow, gt_wrbtr.
    LOOP AT gt_workflow ASSIGNING <gfs_workflow>.

      "Amount
      IF <gfs_workflow>-amount IS INITIAL.
        READ TABLE gt_wrbtr INTO gs_wrbtr WITH KEY ausbk = <gfs_workflow>-cocode
                                                   bukrs = <gfs_workflow>-cocode
                                                   belnr = <gfs_workflow>-docnum
                                                   gjahr = <gfs_workflow>-year BINARY SEARCH.
        IF sy-subrc = 0.
          gv_index = sy-tabix.
          LOOP AT gt_wrbtr INTO gs_wrbtr FROM gv_index.
            IF gs_wrbtr-ausbk <> <gfs_workflow>-cocode AND
               gs_wrbtr-bukrs <> <gfs_workflow>-cocode  AND
               gs_wrbtr-belnr <> <gfs_workflow>-docnum AND
               gs_wrbtr-gjahr <> <gfs_workflow>-year.
              EXIT.
            ELSE.
              <gfs_workflow>-amount = <gfs_workflow>-amount + gs_wrbtr-wrbtr.
            ENDIF.
            CLEAR gs_wrbtr.
          ENDLOOP.

        ENDIF.
      ENDIF.

      gv_amount = <gfs_workflow>-amount.

      "Last item
      READ TABLE gt_lastitem INTO gs_lastitem WITH KEY top_wi_id = <gfs_workflow>-wi_id.
      IF sy-subrc = 0.
        <gfs_workflow>-agent  = gs_lastitem-wi_aagent.    "WF last agent
        <gfs_workflow>-wi_cd  = gs_lastitem-wi_cd.        "Last WI date
        <gfs_workflow>-wi_ct  = gs_lastitem-wi_ct.        "Last WI time

        IF gs_lastitem-wi_stat = 'STARTED'.
          <gfs_workflow>-status = text-021.
        ELSEIF gs_lastitem-wi_stat = 'READY'.
          <gfs_workflow>-status = text-022.
        ELSE.
          <gfs_workflow>-status = gs_lastitem-wi_stat.
        ENDIF.

        IF <gfs_workflow>-agent IS INITIAL.
          CLEAR: gv_return, gt_recipient.
          CALL FUNCTION 'SAP_WAPI_WORKITEM_RECIPIENTS'
            EXPORTING
              workitem_id = gs_lastitem-wi_id
            IMPORTING
              return_code = gv_return
            TABLES
              recipients  = gt_recipient.

          IF gv_return = 0.
            DELETE gt_recipient WHERE objid IS INITIAL.
            READ TABLE gt_recipient INTO gs_recipient INDEX 1.
            IF sy-subrc = 0.
              <gfs_workflow>-agent = gs_recipient-objid.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <gfs_workflow>-agent IS INITIAL.
          <gfs_workflow>-agtype = text-023.   "Agent type
        ELSEIF gs_lastitem-wi_rh_task = 'TS98300052'.
          <gfs_workflow>-agtype = text-024.
        ELSEIF gs_lastitem-wi_rh_task = 'TS98300060'.
          <gfs_workflow>-agtype = text-025.
          <gfs_workflow>-approver = <gfs_workflow>-agent.
        ELSEIF <gfs_workflow>-agent = <gfs_workflow>-wf_init.
          <gfs_workflow>-agtype = text-026.
        ELSE.
          <gfs_workflow>-agtype = text-027.
        ENDIF.

      ENDIF.

      "Vendor
      IF <gfs_workflow>-vendor IS INITIAL.
        READ TABLE gt_vbkpf INTO gs_vbkpf WITH KEY ausbk = <gfs_workflow>-cocode
                                                   bukrs = <gfs_workflow>-cocode
                                                   belnr = <gfs_workflow>-docnum
                                                   gjahr = <gfs_workflow>-year.
        IF sy-subrc = 0.
          READ TABLE gt_vbsegk INTO gs_vbsegk WITH KEY ausbk = gs_vbkpf-bukrs
                                                       bukrs = gs_vbkpf-bukrs
                                                       belnr = gs_vbkpf-belnr
                                                       gjahr = gs_vbkpf-gjahr.
          IF sy-subrc = 0.
            <gfs_workflow>-vendor = gs_vbsegk-lifnr.   "vendor

            SHIFT <gfs_workflow>-vendor LEFT DELETING LEADING '0'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = <gfs_workflow>-vendor
              IMPORTING
                output = <gfs_workflow>-vendor.
          ENDIF.
        ENDIF.
      ENDIF.

      REPLACE ALL OCCURRENCES OF gc_h IN <gfs_workflow>-wf_init WITH space.
      REPLACE ALL OCCURRENCES OF gc_h IN <gfs_workflow>-rco WITH space.
      REPLACE ALL OCCURRENCES OF gc_h IN <gfs_workflow>-approver WITH space.
      REPLACE ALL OCCURRENCES OF gc_h IN <gfs_workflow>-agent WITH space.
      REPLACE ALL OCCURRENCES OF gc_h IN <gfs_workflow>-agtype WITH space.
      REPLACE ALL OCCURRENCES OF gc_h IN <gfs_workflow>-status WITH space.

      CONCATENATE <gfs_workflow>-wi_id
                  <gfs_workflow>-startd
                  <gfs_workflow>-startt
                  <gfs_workflow>-year
                  <gfs_workflow>-docnum
                  <gfs_workflow>-cocode
                  <gfs_workflow>-vendor
                  gv_amount
                  <gfs_workflow>-wf_init
                  <gfs_workflow>-rco
                  <gfs_workflow>-approver
                  <gfs_workflow>-agent
                  <gfs_workflow>-agtype
                  <gfs_workflow>-status
                  <gfs_workflow>-wi_cd
                  <gfs_workflow>-wi_ct INTO gv_string1 SEPARATED BY gc_tab.

      CONCATENATE gv_string gv_string1 gc_crlf INTO gv_string.

      CLEAR <gfs_workflow>.
      CLEAR: gv_amount, gv_index, gv_string1.

    ENDLOOP.

    TRY.
        cl_bcs_convert=>string_to_solix(
          EXPORTING
            iv_string   = gv_string
            iv_codepage = '4103'  "suitable for MS Excel, leave empty for other doc types
            iv_add_bom  = 'X'
          IMPORTING
            et_solix  = gt_binary_content
            ev_size   = gv_size ).
      CATCH cx_bcs.
        MESSAGE e445(so).
    ENDTRY.

  ENDIF.
ENDFORM.                    " F_FORM_DATA
*&---------------------------------------------------------------------*
*&      Form  F_SCREEN_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_screen_fields .
  LOOP AT SCREEN.
    IF screen-name = 'P_TASK'.
      screen-input = '0'.
      screen-output = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " F_SCREEN_FIELDS
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_validation .
  IF s_coco IS NOT INITIAL.
    SELECT bukrs
      FROM t001
      INTO TABLE gt_bukrs
      WHERE bukrs IN s_coco.
    IF sy-subrc <> 0.
      MESSAGE text-032 TYPE 'E'.
    ENDIF.
  ENDIF.

  IF p_email IS NOT INITIAL.

    CLEAR: gv_count, gv_badpattern.
    " If is present more than once, Error out
    FIND ALL OCCURRENCES OF '@' IN p_email MATCH COUNT gv_count.
    IF gv_count > 1.
      gv_badpattern = 1.
    ENDIF.

    CLEAR gv_count.
    " If , is present, once, Error out
    FIND ALL OCCURRENCES OF ',' IN p_email MATCH COUNT gv_count.
    IF gv_count > 0.
      gv_badpattern = gv_badpattern + 1.
    ENDIF.

    FIND REGEX text-030  IN p_email IGNORING CASE .
    IF sy-subrc <> 0 OR gv_badpattern > 0.
      CLEAR : p_email.
      MESSAGE text-031 TYPE 'E'.
    ENDIF.

  ENDIF.
ENDFORM.                    " F_VALIDATION
