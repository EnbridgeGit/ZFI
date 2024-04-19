FUNCTION ZFI_AP_NOTIFICATION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_BUKRS) TYPE  BKPF-BUKRS
*"     REFERENCE(IMP_BELNR) TYPE  BKPF-BELNR
*"     REFERENCE(IMP_GJAHR) TYPE  BKPF-GJAHR
*"     REFERENCE(IMP_USERNAME) TYPE  WFSYST-INITIATOR OPTIONAL
*"     REFERENCE(IMP_USER_ACTION) TYPE  ZFIS_AP_WORKFLOW-AP_USER_ACTION
*"     REFERENCE(IMP_COMMENTS) TYPE  ZFIS_AP_COMMENTS-COMMENTS OPTIONAL
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_AP_NOTIFICATION                           *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  19-Sep-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Notification                                  *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :       1.0                                            *
* Date          :       02-Dec 2019                                    *
* Modified By   :       Tawfeeq Ahmad (AHMADT)                         *
* Correction No :       CHG0163977  D30K930299                         *
* Description   :       Modify Email content with necessary links for  *
*                       approvers to approve pending work items in     *
*                       their inbox                                    *
*----------------------------------------------------------------------*
* Set Global Parameter
  gtp_bukrs = imp_bukrs.
  gtp_belnr = imp_belnr.
  gtp_gjahr = imp_gjahr.
  gtp_user  = imp_username+2.
  gtp_comment = imp_comments.
  gtp_decision_type = imp_user_action.


  REFRESH: gta_objpack, gta_objbin, gta_objtxt, gta_reclist.

  PERFORM get_recipent_list.

  PERFORM set_mail_body.

  PERFORM set_mailing_parameters.

* Sending the document to recipients
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gst_doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = gta_objpack
      contents_bin               = gta_objbin
      contents_txt               = gta_objtxt
      receivers                  = gta_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.





ENDFUNCTION.
