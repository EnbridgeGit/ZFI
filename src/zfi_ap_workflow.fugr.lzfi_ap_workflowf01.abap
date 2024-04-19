*----------------------------------------------------------------------*
***INCLUDE LZFI_AP_WORKFLOWF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Function Module    :                                                *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  19-Sep-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :                                                *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    : MZH01                                                  *
* Date          : Feb 14, 2023                                                     *
* Modified By   : Mohammed Hossain                                                 *
* Correction No :                                                      *
* Description   : DOA migration changes                                                      *
*----------------------------------------------------------------------*


FORM user_command_9001 .

  DATA: ltp_event TYPE swetypecou-event,
        ltp_wfuser TYPE wfsyst-initiator,
        ltp_tcode  TYPE tstc-tcode,
        ltp_confirmation TYPE flag.

  DATA: ltp_log_dest TYPE tb_rfcdest.
  DATA: ltp_amount TYPE zfi_doa_amount,
        ltp_document_complete TYPE flag.

  DATA: w_post LIKE boole-boole.

  DATA: lv_fatca TYPE boole_d.   "MZH01
  DATA: lv_ans   TYPE xfeld.     "MZH01

  SET PARAMETER ID 'BUK' FIELD gtp_bukrs.
  SET PARAMETER ID 'BLN' FIELD gtp_belnr.
  SET PARAMETER ID 'BLP' FIELD gtp_belnr.
  SET PARAMETER ID 'GJR' FIELD gtp_gjahr.

  CALL FUNCTION 'ZFI_GET_LOGICAL_DEST'
    EXPORTING
      imp_paramtype = 'ECCUS'
    IMPORTING
      exp_rfcdest   = ltp_log_dest.

  CASE gtp_okcode.

    WHEN 'FSUPER' OR 'FORWARD'.
      CLEAR: ltp_wfuser, ltp_document_complete.

      "Get User Details where document need to forward
      PERFORM get_wfuser_name  CHANGING ltp_wfuser.

      IF gtp_user_decision = 'RCOD'.
        PERFORM is_document_status_complete CHANGING ltp_document_complete.
        IF ltp_document_complete = 'X'.
          PERFORM check_user_is_approver  USING ltp_wfuser ltp_log_dest
                                          CHANGING ltp_confirmation.
        ENDIF.
      ELSEIF gtp_user_decision = 'POST'.
        PERFORM is_document_status_complete CHANGING ltp_document_complete.
        IF ltp_document_complete = 'X'.
          PERFORM check_user_is_rcowner USING ltp_wfuser ltp_log_dest
                                        CHANGING ltp_confirmation.
        ENDIF.
      ENDIF.

      PERFORM f_set_comments.

* Begin of changes MZH01
* If FATCA flag is on then trigger event RCRejected or ParkRejected based on
* decision. By triggering event new work item is created for AP clerks
      PERFORM check_fatca CHANGING lv_fatca.
      IF lv_fatca IS NOT INITIAL.
        IF gtp_user_decision = 'RCOD'.
          ltp_event = 'RCRejected'.
        ELSEIF gtp_user_decision = 'POST'.
          ltp_event = 'ParkRejected'.
        ENDIF.
        PERFORM raise_event USING ltp_event.
        SET SCREEN 0.
        LEAVE SCREEN.
      ENDIF.
* End of changes MZH01

      IF ltp_confirmation <> 'X'.
        PERFORM forward_workitem USING ltp_wfuser .

        IF gtp_user_decision = 'RCOD'.
          PERFORM send_notification USING ltp_wfuser 'CODE'.
        ELSEIF gtp_user_decision = 'POST'.
          PERFORM send_notification USING ltp_wfuser 'APPR'.
        ENDIF.
      ENDIF.

      SET SCREEN 0.
      LEAVE SCREEN.

*   Action on User Decision Post
    WHEN zif_fi_constants=>gc_ucom_post .
*     Validate DOA
*     Get Amount and Currency
      PERFORM get_cost_and_currency CHANGING ltp_amount.

      PERFORM validate_doa USING ltp_amount ltp_log_dest.
      IF gtp_user_decision = 'POST'.
*  Begin of MZH01
        PERFORM generate_fatca_popup CHANGING lv_ans.
        IF lv_ans EQ '1'.
          ltp_event = 'RCRejected'.
          PERFORM raise_event USING ltp_event.
          SET SCREEN 0.
          LEAVE SCREEN.
        ENDIF.
*  End of MZH01

        PERFORM check_duplicates USING ltp_amount CHANGING w_post.
        IF w_post = 'X'.
          PERFORM post_document.
        ENDIF.
        "PERFORM send_notification USING ltp_wfuser 'COAP'.
      ENDIF.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'CONFIRM'.
      PERFORM raise_event USING 'InitiatorConfirmation'.
      PERFORM f_set_comments.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'COMPLETE'.
      "call transaction to display the document

      "Determine which function to call.

*      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
*        EXPORTING
*          tcode  = 'FBV0'
*        EXCEPTIONS
*          ok     = 1
*          not_ok = 2
*          OTHERS = 3.
*
*      IF sy-subrc = 1.
*        "If the user has FBV0, let them use it.
*        CALL TRANSACTION 'FBV0' AND SKIP FIRST SCREEN.
*      ELSE.
      "Try ZFI_FBV0
      "If they don't have it they will get an error message.
      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
        EXPORTING
          tcode  = 'ZFI_FBV0'
        EXCEPTIONS
          ok     = 1
          not_ok = 2
          OTHERS = 3.
      IF sy-subrc = 1.
        CALL TRANSACTION 'ZFI_FBV0' AND SKIP FIRST SCREEN.

* Begin of Changes MZH01
        PERFORM check_fatca CHANGING lv_fatca.
        IF lv_fatca IS NOT INITIAL.
          PERFORM set_fatca_commnets USING gtp_okcode.
        ENDIF.
* End of changes MZH01

      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
*      ENDIF.

*   Action on User Decision View
    WHEN zif_fi_constants=>gc_ucom_view.

**      IF sy-dynnr = '9003'.
**        ltp_tcode = 'FBV2'.
**      ELSE.
**        ltp_tcode = 'FBV3'.
**      ENDIF.

      "call transaction to display the document
      CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
        EXPORTING
          tcode  = 'FBV3'
        EXCEPTIONS
          ok     = 1
          not_ok = 2
          OTHERS = 3.
      IF sy-subrc = 1.
        CALL TRANSACTION 'FBV3' AND SKIP FIRST SCREEN.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

* Action on User Decision Reject
    WHEN zif_fi_constants=>gc_ucom_reject.

      IF gtp_comment IS INITIAL.
        MESSAGE e006(zfi_workflow).
      ELSE.

        PERFORM f_set_comments.

        IF gtp_user_decision = 'RCOD'.
          ltp_event = 'RCRejected'.
        ELSEIF gtp_user_decision = 'POST'.
          ltp_event = 'ParkRejected'.
        ENDIF.
        PERFORM raise_event USING ltp_event.
        "PERFORM send_notification USING ltp_wfuser 'CORE'.
        SET SCREEN 0.
        LEAVE SCREEN.
      ENDIF.

*   Action on Other User Decision
    WHEN zif_fi_constants=>gc_ucom_exit
      OR zif_fi_constants=>gc_ucom_cancel
      OR zif_fi_constants=>gc_ucom_back
      OR zif_fi_constants=>gc_ucom_return.

      "If document complete and the RCO is exiting, warn to forward item.
      IF gtp_user_decision = 'RCOD'.

        DATA: lst_vbkpf TYPE vbkpf.
        "Get document status from VBKPF Table
        SELECT SINGLE * FROM vbkpf INTO lst_vbkpf
                WHERE bukrs = gtp_bukrs
                  AND belnr = gtp_belnr
                  AND gjahr = gtp_gjahr.
        IF sy-subrc EQ 0 AND lst_vbkpf-bstat EQ 'V' AND lst_vbkpf-xprfg EQ 'X'.
          MESSAGE w045(zfi_workflow).
        ENDIF.
      ENDIF.

      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.

  ENDCASE.
ENDFORM.                    "USER_COMMAND_9001
*&---------------------------------------------------------------------*
*&      Form  GET_WFUSER_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LTP_WFUSER  text
*----------------------------------------------------------------------*
FORM get_wfuser_name  CHANGING ctp_wfuser TYPE wfsyst-initiator.

  DATA: lv_username TYPE xubname.

  IF gtp_okcode = 'FORWARD'.
    IF gtp_username IS INITIAL.
      MESSAGE e011(zfi_workflow).
    ELSE.
      CONCATENATE 'US' gtp_username INTO ctp_wfuser.
    ENDIF.
  ELSE.
    CONCATENATE 'US' sy-uname INTO ctp_wfuser.
    CALL FUNCTION 'ZFI_GETAPPROVERS'
      EXPORTING
        imp_username = ctp_wfuser
      IMPORTING
        exp_manager  = ctp_wfuser
      EXCEPTIONS
        nobody_found = 1
        OTHERS       = 2.
    IF sy-subrc <> 0 OR ctp_wfuser+2 IS INITIAL.
      MESSAGE e013(zfi_workflow) WITH sy-uname .
    ENDIF.
  ENDIF.


  lv_username = ctp_wfuser+2(12).

  "Make sure user exists.
  CALL FUNCTION 'ZBBC_CHECK_VALID_USER'
    EXPORTING
      imp_uname      = lv_username
    EXCEPTIONS
      user_not_valid = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE e060(zfi_workflow) WITH ctp_wfuser+2(12).
  ENDIF.
ENDFORM.                    " GET_WFUSER_NAME
*&---------------------------------------------------------------------*
*&      Form  IS_DOCUMENT_STATUS_COMPLETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LTP_DOCUMENT_COMPLETE  text
*----------------------------------------------------------------------*
FORM is_document_status_complete  CHANGING ctp_document_complete TYPE flag.

  DATA: lst_vbkpf TYPE vbkpf.
* Get document status from VBKPF Table
  SELECT SINGLE * FROM vbkpf INTO lst_vbkpf
          WHERE bukrs = gtp_bukrs
            AND belnr = gtp_belnr
            AND gjahr = gtp_gjahr.
  IF sy-subrc EQ 0 AND lst_vbkpf-bstat EQ 'V' AND lst_vbkpf-xprfg EQ 'X'.
    ctp_document_complete = 'X'.
  ELSE.
    CLEAR : ctp_document_complete .
    MESSAGE w044(zfi_workflow).
  ENDIF.

ENDFORM.                    " IS_DOCUMENT_STATUS_COMPLETE
*&---------------------------------------------------------------------*
*&      Form  CHECK_USER_IS_APPROVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_WFUSER  text
*      -->P_LTP_LOG_DEST  text
*      <--P_LTP_CONFIRMATION  text
*----------------------------------------------------------------------*
FORM check_user_is_approver  USING    itp_wfuser    TYPE wfsyst-initiator
                                      itp_log_dest  TYPE tb_rfcdest
                             CHANGING ctp_confirmed TYPE flag.

  DATA: ltp_usnam TYPE usnam,
        ltp_userid TYPE username,
        ltp_pernr TYPE pernr_d,
        ltp_lookup_key TYPE zfi_lookup_key.
  CONSTANTS : c_tcode TYPE sy-tcode VALUE 'FV60'.

  DATA : lt_message TYPE STANDARD TABLE OF  bapiret2,
         ls_message TYPE bapiret2.
  DATA : ltp_uname  TYPE bname,
         lv_amount  TYPE zfi_doa_amount,
         lv_doa     TYPE zfi_doa_validity.


  ltp_userid = itp_wfuser+2.

  "Validate the approver has at least 1 dollar DOA
  lv_amount = 1.
  CALL FUNCTION 'ZFI_DOA_VALIDATION' DESTINATION itp_log_dest
    EXPORTING
      imp_system     = 'UG'
      imp_lookup_key = ltp_userid
      imp_tcode      = c_tcode
      imp_amount     = lv_amount
    IMPORTING
      exp_doa        = lv_doa
    EXCEPTIONS
      bad_lookup_key = 1
      no_doa_found   = 2
      no_grade       = 3
      OTHERS         = 4.

  IF sy-subrc IS NOT INITIAL OR lv_doa = 'N'.
    MESSAGE e005(zfi_workflow) WITH ltp_userid.
  ELSE.
    "Check user is forwarding document to himself
    IF ltp_userid = sy-uname.
      MESSAGE e033(zfi_workflow).
    ENDIF.
  ENDIF.

  TRANSLATE ltp_userid TO UPPER CASE.
  ltp_usnam = ltp_userid.
  SELECT SINGLE usnam
    FROM bkpf
    INTO ltp_userid
    WHERE bukrs = gtp_bukrs
      AND belnr = gtp_belnr
      AND gjahr = gtp_gjahr
  .
  IF sy-subrc IS INITIAL AND ltp_usnam = ltp_userid.
    MESSAGE e032(zfi_workflow).
  ENDIF.
  gtp_username = ltp_usnam.
  PERFORM raise_event USING 'RCConfirmed'.
  ctp_confirmed = 'X'.
ENDFORM.                    " CHECK_USER_IS_APPROVER
*&---------------------------------------------------------------------*
*&      Form  RAISE_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0340   text
*----------------------------------------------------------------------*
FORM raise_event  USING   itp_event TYPE swetypecou-event.

  DATA: BEGIN OF lst_doc_key,
          companycode    TYPE bkpf-bukrs,
          documentnumber TYPE bkpf-belnr,
          fiscalyear     TYPE bkpf-gjahr,
        END OF lst_doc_key.
  DATA: ltp_object_key TYPE sweinstcou-objkey,
        lta_container TYPE STANDARD TABLE OF swcont,
        ltp_wfuser  TYPE wfsyst-initiator.

  CONCATENATE 'US' gtp_username INTO ltp_wfuser.

  IF itp_event = 'RCConfirmed'.
    CALL FUNCTION 'SWC_ELEMENT_SET'
      EXPORTING
        element       = 'FinalApprover'
        field         = ltp_wfuser
      TABLES
        container     = lta_container
      EXCEPTIONS
        type_conflict = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING type_conflict.
    ENDIF.
  ENDIF.

* Set variables with values
  lst_doc_key-companycode    = gtp_bukrs.
  lst_doc_key-documentnumber = gtp_belnr.
  lst_doc_key-fiscalyear     = gtp_gjahr.
  ltp_object_key = lst_doc_key .

* Raise Rejection Event
  CALL FUNCTION 'SWE_EVENT_CREATE'
    EXPORTING
      objtype           = zif_fi_constants=>gc_objtype_fipp
      objkey            = ltp_object_key
      event             = itp_event
    TABLES
      event_container   = lta_container
    EXCEPTIONS
      objtype_not_found = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          RAISING objtype_not_found.
  ENDIF.
ENDFORM.                    " RAISE_EVENT
*&---------------------------------------------------------------------*
*&      Form  SEND_NOTIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_WFUSER  text
*      -->P_0108   text
*----------------------------------------------------------------------*
FORM send_notification  USING    itp_wfuser TYPE wfsyst-initiator
                                 itp_user_decision TYPE zfis_ap_workflow-ap_user_action.

  CALL FUNCTION 'ZFI_AP_NOTIFICATION'
    EXPORTING
      imp_bukrs       = gtp_bukrs
      imp_belnr       = gtp_belnr
      imp_gjahr       = gtp_gjahr
      imp_username    = itp_wfuser
      imp_user_action = itp_user_decision
      imp_comments    = gtp_comment.
ENDFORM.                    " SEND_NOTIFICATION
*&---------------------------------------------------------------------*
*&      Form  FORWARD_WORKITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_WFUSER  text
*----------------------------------------------------------------------*
FORM forward_workitem  USING    itp_wfuser TYPE wfsyst-initiator.

  DATA: ltp_return          TYPE sysubrc,
        ltp_object_fwd      TYPE sweinstcou-objkey,
        ltp_new_status      TYPE swr_wistat,
        lta_message_lines   TYPE sapi_msg_lines,
        lst_message_lines   TYPE swr_messag,
        lta_message_struct  TYPE swr_msgtab,
        lta_user_ids        TYPE swrtagent.

  DATA: BEGIN OF lst_forward,
          workitem TYPE zfis_forward_workitem-workitem,
          username TYPE zfis_forward_workitem-username,
        END OF lst_forward.

  CALL FUNCTION 'SAP_WAPI_FORWARD_WORKITEM'
    EXPORTING
      workitem_id    = gtp_workitem_id
      user_id        = itp_wfuser+2
      language       = sy-langu
      do_commit      = 'X'
      current_user   = sy-uname
    IMPORTING
      return_code    = ltp_return
      new_status     = ltp_new_status
    TABLES
      message_lines  = lta_message_lines
      message_struct = lta_message_struct
      user_ids       = lta_user_ids.

  READ TABLE lta_message_lines WITH KEY msg_type = 'E' INTO lst_message_lines.
  IF sy-subrc IS INITIAL.
    MESSAGE e001(zfi_workflow) WITH lst_message_lines-line.
  ELSE.
    MESSAGE s012(zfi_workflow) WITH itp_wfuser+2.
    lst_forward-workitem = gtp_workitem_id.
    lst_forward-username = itp_wfuser+2.
    ltp_object_fwd = lst_forward .
*         Raise Event to Forward and remove from current inbox
    CALL FUNCTION 'SWE_EVENT_CREATE'
      EXPORTING
        objtype           = 'ZFI_FWORD'
        objkey            = ltp_object_fwd
        event             = 'Forward'
      EXCEPTIONS
        objtype_not_found = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            RAISING objtype_not_found.
    ENDIF.
  ENDIF.

ENDFORM.                    " FORWARD_WORKITEM
*&---------------------------------------------------------------------*
*&      Form  GET_COST_AND_CURRENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LTP_AMOUNT  text
*----------------------------------------------------------------------*
FORM get_cost_and_currency  CHANGING ctp_amount TYPE zfi_doa_amount.

  TYPES: BEGIN OF ty_wrbtr,
             wrbtr TYPE wrbtr_d,
           END OF ty_wrbtr.

  DATA: lta_wrbtr TYPE STANDARD TABLE OF ty_wrbtr,
        lst_wrbtr TYPE ty_wrbtr.

  SELECT wrbtr INTO TABLE lta_wrbtr FROM vbsegd WHERE bukrs = gtp_bukrs
                            AND   belnr = gtp_belnr
                            AND   gjahr = gtp_gjahr.
  IF sy-subrc IS NOT INITIAL.
    SELECT wrbtr INTO TABLE lta_wrbtr FROM vbsegk WHERE bukrs = gtp_bukrs
                          AND   belnr = gtp_belnr
                          AND   gjahr = gtp_gjahr.
    IF sy-subrc IS NOT INITIAL.
      SELECT wrbtr INTO TABLE lta_wrbtr FROM vbsegs WHERE bukrs = gtp_bukrs
                        AND   belnr = gtp_belnr
                        AND   gjahr = gtp_gjahr.
    ENDIF.
  ENDIF.

  CLEAR: ctp_amount.
  LOOP AT lta_wrbtr INTO lst_wrbtr.
    ctp_amount = ctp_amount + lst_wrbtr-wrbtr.
    CLEAR lst_wrbtr.
  ENDLOOP.
ENDFORM.                    " GET_COST_AND_CURRENCY
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DOA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_AMOUNT  text
*      -->P_LTP_LOG_DEST  text
*----------------------------------------------------------------------*
FORM validate_doa  USING  itp_amount TYPE zfi_doa_amount
                          itp_log_dest TYPE tb_rfcdest.

  DATA: ltp_doa   TYPE zfi_doa_validity,
        ltp_lookup_key TYPE zfi_lookup_key,
        ltp_blart TYPE blart,
        ltp_doc_type TYPE zfi_doc_typ.


  DATA: result TYPE p LENGTH 8 DECIMALS 2,
        oref   TYPE REF TO cx_root,
        text   TYPE string.

  CONSTANTS : c_tcode TYPE sy-tcode VALUE 'FV60'.


***Added for Passing Document Type in DOA FM
*   Find Document Type from BKPF Tbale
  SELECT SINGLE blart
    FROM bkpf
    INTO ltp_blart
    WHERE bukrs = gtp_bukrs
      AND belnr = gtp_belnr
      AND gjahr = gtp_gjahr.
*   Validate DOA
  ltp_doc_type = ltp_blart.

  ltp_lookup_key = sy-uname.

  TRY.
      CALL FUNCTION 'ZFI_DOA_VALIDATION' DESTINATION itp_log_dest
        EXPORTING
          imp_system     = 'UG'
          imp_lookup_key = ltp_lookup_key
          imp_tcode      = c_tcode
          imp_doc_type   = ltp_doc_type
          imp_amount     = itp_amount
          imp_waers      = 'CAD'
        IMPORTING
          exp_doa        = ltp_doa
        EXCEPTIONS
          bad_lookup_key = 1
          no_doa_found   = 2
          no_grade       = 3
          OTHERS         = 4.

      IF ltp_doa = 'N'.
        MESSAGE e027(zfi_workflow) WITH sy-uname .
      ENDIF.
    CATCH cx_root INTO oref.
      text = oref->get_text( ).
  ENDTRY.
ENDFORM.                    " VALIDATE_DOA
*&---------------------------------------------------------------------*
*&      Form  POST_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_document .
  DATA: lta_vbkpf TYPE STANDARD TABLE OF vbkpf,
        lst_vbkpf TYPE vbkpf,
        lta_message TYPE STANDARD TABLE OF fimsg1,
        lst_message TYPE fimsg1.

* Set table entries for posting
  lst_vbkpf-ausbk = gtp_bukrs.
  lst_vbkpf-bukrs = gtp_bukrs.
  lst_vbkpf-belnr = gtp_belnr.
  lst_vbkpf-gjahr = gtp_gjahr.
  INSERT lst_vbkpf INTO TABLE lta_vbkpf.

* Post Document
  CALL FUNCTION 'PRELIMINARY_POSTING_POST_ALL'
    EXPORTING
      nomsg   = 'X'
      synch   = 'X'
    TABLES
      t_vbkpf = lta_vbkpf
      t_msg   = lta_message.

  "Check for errors.
  IF lta_message IS NOT INITIAL.
    LOOP AT lta_message INTO lst_message.
      MESSAGE ID lst_message-msgid TYPE lst_message-msgty NUMBER lst_message-msgno
          WITH lst_message-msgv1 lst_message-msgv2 lst_message-msgv3 lst_message-msgv4.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " POST_DOCUMENT

*&---------------------------------------------------------------------*
*&      Form  CHECK_USER_IS_RCOWNER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_WFUSER  text
*      -->P_LTP_LOG_DEST  text
*      <--P_LTP_CONFIRMATION  text
*----------------------------------------------------------------------*
FORM check_user_is_rcowner  USING     itp_wfuser    TYPE wfsyst-initiator
                                      itp_log_dest  TYPE tb_rfcdest
                             CHANGING ctp_confirmed TYPE flag.

  DATA :  ltp_xref3   TYPE bseg-xref3,
          ltp_rc_owner   TYPE wfsyst-initiator,"username,
          ltp_log_dest   TYPE tb_rfcdest,
          ltp_user TYPE username.
  CLEAR ltp_user.
  SELECT SINGLE xref3 FROM vbsegk INTO ltp_xref3
      WHERE ausbk = gtp_bukrs
        AND belnr = gtp_belnr
        AND gjahr = gtp_gjahr.

  IF sy-subrc IS INITIAL.

**Get Logical Destination
    CALL FUNCTION 'ZFI_GET_LOGICAL_DEST'
      EXPORTING
        imp_paramtype = 'ECCUS'
      IMPORTING
        exp_rfcdest   = ltp_log_dest.

    IF ltp_xref3 IS NOT INITIAL.

*      CALL FUNCTION 'ZFI_GET_ROUTCODE' DESTINATION LTP_LOG_DEST "'DECCLNT200' "MZH01
      CALL FUNCTION 'ZFI_GET_ROUTCODE'    "MZH01
      EXPORTING
        imp_route_code = ltp_xref3
      IMPORTING
        exp_rc_owner = ltp_rc_owner
      EXCEPTIONS
        nobody_found.

      ltp_user = ltp_rc_owner+2.

      IF ltp_user = gtp_username.
        MESSAGE e034(zfi_workflow).
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_USER_IS_RCOWNER
*&---------------------------------------------------------------------*
*&      Form  F_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_grid .
  PERFORM f_get_data TABLES lta_user_comment.
  PERFORM f_display_data USING lta_user_comment.
ENDFORM.                    " F_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTA_USER_COMMENT  text
*----------------------------------------------------------------------*
FORM f_get_data  TABLES lta_user_comment.

  DATA: ltp_objkey TYPE swo_typeid.

  CLEAR ltp_objkey.
  CONCATENATE gtp_bukrs gtp_belnr gtp_gjahr INTO ltp_objkey.
  REFRESH lta_user_comment.

  CALL FUNCTION 'ZFI_GET_APPROVER_COMMENT'
    EXPORTING
      imp_workitem_id = gtp_workitem_top
      imp_objectkey   = ltp_objkey
    TABLES
      user_comment    = lta_user_comment.
ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTA_USER_COMMENT  text
*----------------------------------------------------------------------*
FORM f_display_data  USING    p_lta_user_comment.
  REFRESH gtp_fcat.

  CLEAR gwa_fcat.
  gwa_fcat-fieldname = 'USERNAME'.
  gwa_fcat-tabname =  'LTA_USER_COMMENT'.
  gwa_fcat-coltext = 'User Name'.

  APPEND gwa_fcat TO gtp_fcat.

  CLEAR gwa_fcat.
  gwa_fcat-fieldname = 'COMMENTS'.
  gwa_fcat-tabname =  'LTA_USER_COMMENT'.
  gwa_fcat-coltext = 'Comments'.
  APPEND gwa_fcat TO gtp_fcat.

  IF gr_obj_alv_grid IS NOT BOUND.
    PERFORM f_alv_initialze.
*    Create a ALV Grid
    FREE gr_obj_alv_grid.
    CREATE OBJECT gr_obj_alv_grid
      EXPORTING
        i_parent = obj_alv_container.

    CALL METHOD gr_obj_alv_grid->set_table_for_first_display
      CHANGING
        it_fieldcatalog               = gtp_fcat
        it_outtab                     = lta_user_comment[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE e000(zmsg) WITH 'Invalid Parameter'(006).
      WHEN 2.
        MESSAGE e000(zmsg) WITH 'Program Error'(007).
      WHEN 3.
        MESSAGE e000(zmsg) WITH 'Too many lines'(008).
      WHEN 4.
        MESSAGE e000(zmsg) WITH 'Other Error'(009).
    ENDCASE.

    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        control = gr_obj_alv_grid.
    CALL METHOD cl_gui_cfw=>flush.

  ELSE.
    CALL METHOD gr_obj_alv_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  F_ALV_INITIALZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_initialze .
  FREE obj_alv_container.
  CREATE OBJECT obj_alv_container
    EXPORTING
      container_name              = g_data_container
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  CASE sy-subrc.
    WHEN 1.
      MESSAGE e000(zmsg) WITH 'Control Error'(001).
    WHEN 2.
      MESSAGE e000(zmsg) WITH 'Control System Error'(002).
    WHEN 3.
      MESSAGE e000(zmsg) WITH 'Control creation Error'(003).
    WHEN 4.
      MESSAGE e000(zmsg) WITH 'Lifetime Error'(004).
    WHEN 5.
      MESSAGE e000(zmsg) WITH 'Lifetime Dynpro Error'(005).
  ENDCASE.
ENDFORM.                    " F_ALV_INITIALZE
*&---------------------------------------------------------------------*
*&      Form  CHECK_DUPLICATES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_AMOUNT  text
*----------------------------------------------------------------------*
FORM check_duplicates  USING    p_amount CHANGING p_post.
  DATA: w_xblnr TYPE bkpf-xblnr,
        w_wrbtr TYPE bseg-wrbtr.

  w_wrbtr = p_amount.

  SELECT SINGLE xblnr
     FROM bkpf
     INTO w_xblnr
     WHERE bukrs = gtp_bukrs
       AND belnr = gtp_belnr
       AND gjahr = gtp_gjahr.

  CALL FUNCTION 'ZFI_DUPL_CHECK_WF'
    EXPORTING
      i_bukrs = gtp_bukrs
      i_xblnr = w_xblnr
      i_wrbtr = w_wrbtr
      i_belnr = gtp_belnr
      i_gjahr = gtp_gjahr
    IMPORTING
      e_post  = p_post.

ENDFORM.                    " CHECK_DUPLICATES

*&---------------------------------------------------------------------*
*&      Form  GENERATE_FATCA_POPUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_fatca_popup CHANGING p_ans TYPE xfeld.

  DATA:  r_witht      TYPE RANGE OF lfbw-witht,
         r_witht_line LIKE LINE  OF r_witht,
         ltp_objtype  TYPE swo_objtyp,
         ltp_return   TYPE sysubrc,
         lta_worklist TYPE swrtwihdr,
         lwa_worklist TYPE swr_wihdr,
         lt_cont      TYPE TABLE OF swr_cont,
         lwa_cont     TYPE swr_cont,
         ltp_objkey   TYPE swotobjid-objkey,
         lv_qsshb     TYPE wt_bs1,
         lv_ans       TYPE xfeld.

  CLEAR p_ans.

  REFRESH r_witht.
  r_witht_line-sign = 'I'.
  r_witht_line-option = 'EQ'.

  r_witht_line-low = '03'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '04'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '05'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '42'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '43'.
  APPEND r_witht_line TO r_witht.
  r_witht_line-low = '44'.
  APPEND r_witht_line TO r_witht.

  SELECT SINGLE wt_qsshb FROM with_item INTO lv_qsshb
    WHERE bukrs = gtp_bukrs AND
          belnr = gtp_belnr AND
          gjahr = gtp_gjahr AND
          witht IN r_witht.

  IF sy-subrc = 0 AND lv_qsshb IS INITIAL.
    CALL FUNCTION 'Z_POPUP_WITH_2_OPTIONS'
      EXPORTING
        textline1    = 'Was all or part of the services on this'
        textline2    = 'invoice performed in the continental U.S.?'
        textline3    = '(Note: If AP has already processed this invoice'
        textline4    = 'for US Withholding Tax, answer No)'
        text_option1 = 'Yes'
        text_option2 = 'No'
        title        = 'FATCA Requirement'
      IMPORTING
        answer       = lv_ans.

    PERFORM upd_wf_container USING lv_ans.
    PERFORM set_fatca_commnets USING gtp_okcode.
    IF lv_ans = '1'.
      MESSAGE e000(zfi_workflow) WITH 'Please Reject Invoice Back to AP for US WH Tax' DISPLAY LIKE 'I'.
      p_ans  = '1'.
    ENDIF.
  ENDIF.

ENDFORM.                    " GENERATE_FATCA_POPUP

*&---------------------------------------------------------------------*
*&      Form  UPD_WF_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upd_wf_container USING p_ans TYPE any.

  DATA: lt_cont  TYPE TABLE OF swr_cont,
        lwa_cont LIKE LINE OF lt_cont.

  REFRESH lt_cont.

  lwa_cont-element = 'RCOFATCA'.

  IF p_ans EQ '1'.
    lwa_cont-value   = 'X'.
  ELSE.
    lwa_cont-value   = ' '.
  ENDIF.

  APPEND lwa_cont TO lt_cont.

  CALL FUNCTION 'SAP_WAPI_WRITE_CONTAINER'
    EXPORTING
      workitem_id      = gtp_workitem_top
    TABLES
      simple_container = lt_cont.

ENDFORM.                    " UPD_WF_CONTAINER
