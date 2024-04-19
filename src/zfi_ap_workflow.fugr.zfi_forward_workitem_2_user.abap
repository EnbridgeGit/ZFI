FUNCTION zfi_forward_workitem_2_user.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_WORKITEM_ID) TYPE  SWR_STRUCT-WORKITEMID
*"     REFERENCE(IMP_USERNAME) TYPE  SY-UNAME
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_FORWARD_WORKITEM                          *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :                                                *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Forward Workitem                              *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*
  DATA: ltp_return TYPE sysubrc,
      ltp_object_fwd TYPE sweinstcou-objkey,
      ltp_new_status TYPE swr_wistat,
      lta_message_lines TYPE sapi_msg_lines,
      lst_message_lines TYPE swr_messag,
      lta_message_struct TYPE swr_msgtab,
      lta_user_ids TYPE swrtagent.
  DATA: BEGIN OF lst_forward,
          workitem    TYPE zfis_forward_workitem-workitem,
          username TYPE zfis_forward_workitem-username,
        END OF lst_forward.

  CALL FUNCTION 'SAP_WAPI_FORWARD_WORKITEM'
    EXPORTING
      workitem_id    = imp_workitem_id
      user_id        = imp_username
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
    MESSAGE s001(zfi_workflow) WITH lst_message_lines-line DISPLAY LIKE 'E'.
    RAISE error.
  ELSE.
    MESSAGE s012(zfi_workflow) WITH imp_username."change
    lst_forward-workitem    = imp_workitem_id.
    lst_forward-username = imp_username.
    ltp_object_fwd = lst_forward .
*   Raise Event to Forward and remove from current inbox
    CALL FUNCTION 'SWE_EVENT_CREATE'
      EXPORTING
        objtype           = 'ZFI_FWORD'
        objkey            = ltp_object_fwd
        event             = 'Forward'
      EXCEPTIONS
        objtype_not_found = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*            RAISING objtype_not_found.
    ENDIF.
  ENDIF.




ENDFUNCTION.
