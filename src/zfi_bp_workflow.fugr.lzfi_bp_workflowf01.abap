*----------------------------------------------------------------------*
***INCLUDE LZFI_BP_WORKFLOWF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_all .

  DATA: ltp_wfuser TYPE wfsyst-initiator,
        ltp_event  TYPE swetypecou-event.

  SET PARAMETER ID 'BPA' FIELD gtp_businesspartner.

  CASE gtp_okcode.

    WHEN zif_fi_constants=>gc_ucom_view.
      PERFORM validate_transaction_authority USING 'BP'.
*     Call BP Transaction
      CALL TRANSACTION 'BP' AND SKIP FIRST SCREEN.

    WHEN 'RELEASE'.
      PERFORM validate_transaction_authority USING 'BP'.
*     Check for the Release procedure
      CALL FUNCTION 'ZFI_BP_CHECK_RELEASE_PROCEDURE'
        EXPORTING
          imp_business_partner = gtp_businesspartner
        EXCEPTIONS
          not_released         = 1
          rejected             = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        MESSAGE s020(zfi_workflow) WITH gtp_businesspartner sy-uname.
      ELSE.
        ltp_event = 'Released'.
        PERFORM raise_event USING ltp_event.
*       Notification Mail of the Rejection
        PERFORM send_notification USING 'COAP'.
        PERFORM leave_screen.
      ENDIF.


    WHEN zif_fi_constants=>gc_ucom_reject.
*     Check if comments field is not initial
      IF gtp_comment IS INITIAL.
        MESSAGE e009(zfi_workflow).
      ELSE.
*       Raise Workflow Event Rejected
        PERFORM raise_event USING 'Reject'.
        MESSAGE s008(zfi_workflow) WITH gtp_businesspartner sy-uname.
*       Send Rejection Notification to the Users
        PERFORM send_notification USING 'CORE'.
      ENDIF.
      PERFORM leave_screen.

    WHEN 'FORWARD' OR 'FSUPER'.
*     Get User Details where document need to forward
      PERFORM get_wfuser_name  CHANGING ltp_wfuser.
      PERFORM forward_workitem USING ltp_wfuser .
      PERFORM send_notification USING 'FORW'.
      PERFORM leave_screen.

    WHEN zif_fi_constants=>gc_ucom_return
      OR zif_fi_constants=>gc_ucom_back
      OR zif_fi_constants=>gc_ucom_cancel
      OR zif_fi_constants=>gc_ucom_exit.
      PERFORM leave_screen.
  ENDCASE.

  CLEAR gtp_okcode.

ENDFORM.                    " USER_COMMAND_ALL

*&---------------------------------------------------------------------*
*&      Form  validate_transaction_authority
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITP_TCODE  text
*----------------------------------------------------------------------*
FORM validate_transaction_authority  USING itp_tcode TYPE tstc-tcode.

* Check authority for the transaction
  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = itp_tcode
    EXCEPTIONS
      ok     = 0
      not_ok = 2
      OTHERS = 3.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " VALIDATE_TRANSACTION_AUTHORITY
*&---------------------------------------------------------------------*
*&      Form  raise_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ITP_EVENT  text
*----------------------------------------------------------------------*
FORM raise_event  USING   itp_event TYPE swetypecou-event.

  DATA: BEGIN OF lst_doc_key,
            businesspartner TYPE but000-partner,
          END OF lst_doc_key.

  DATA: ltp_object_key TYPE sweinstcou-objkey,
        ltp_object_fwd TYPE sweinstcou-objkey,
        ltp_wfuser     TYPE wfsyst-initiator.

  CONCATENATE 'US' gtp_username INTO ltp_wfuser.

* Set variables with values
  lst_doc_key-businesspartner = gtp_businesspartner.
  ltp_object_key = lst_doc_key .

* Raise Rejection Event
  CALL FUNCTION 'SWE_EVENT_CREATE'
    EXPORTING
      objtype           = zif_fi_constants=>gc_objtype_bus1006
      objkey            = ltp_object_key
      event             = itp_event
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
*      -->P_0180   text
*----------------------------------------------------------------------*
FORM send_notification  USING  itp_decision TYPE zfis_tv_workflow-conf_type.

  DATA: ltp_wfuser TYPE wfsyst-initiator.

  CONCATENATE 'US' sy-uname INTO ltp_wfuser.
  CALL FUNCTION 'ZFI_BP_NOTIFICATION'
    EXPORTING
      imp_business_partner = gtp_businesspartner
      imp_decision_type    = itp_decision
      imp_manager          = ltp_wfuser
      imp_comment          = gtp_comment.



ENDFORM.                    " SEND_NOTIFICATION
*&---------------------------------------------------------------------*
*&      Form  GET_WFUSER_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_CRUSER  text
*      <--P_LTP_WFUSER  text
*----------------------------------------------------------------------*
FORM get_wfuser_name CHANGING ctp_wfuser TYPE wfsyst-initiator.

  IF gtp_okcode = 'FORWARD'.
    IF gtp_username IS INITIAL.
      MESSAGE e011(zfi_workflow).
    ELSE.
      CONCATENATE 'US' gtp_username INTO ctp_wfuser.
    ENDIF.
  ELSEIF gtp_okcode = 'FSUPER'.
    CONCATENATE 'US' sy-uname INTO ctp_wfuser.
    CALL FUNCTION 'ZFI_GET_MANAGER'
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
ENDFORM.                    " GET_WFUSER_NAME
*&---------------------------------------------------------------------*
*&      Form  FORWARD_WORKITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTP_WFUSER  text
*----------------------------------------------------------------------*
FORM forward_workitem  USING itp_wfuser TYPE wfsyst-initiator.

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
      workitem_id    = gtp_workflow_id
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
    lst_forward-workitem    = gtp_workflow_id.
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
*&      Form  LEAVE_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM leave_screen .
  SET SCREEN 0.
  LEAVE SCREEN.
ENDFORM.                    " LEAVE_SCREEN
