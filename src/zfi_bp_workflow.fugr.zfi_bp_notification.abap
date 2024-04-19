FUNCTION zfi_bp_notification .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_BUSINESS_PARTNER) TYPE  BUT000-PARTNER
*"     REFERENCE(IMP_DECISION_TYPE) TYPE  ZFIS_TV_WORKFLOW-CONF_TYPE
*"     REFERENCE(IMP_MANAGER) TYPE  WFSYST-INITIATOR OPTIONAL
*"     REFERENCE(IMP_COMMENT) TYPE  ZFIS_COMMENTS-COMMENTS OPTIONAL
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_BP_NOTIFICATION                           *
*& Author             :  Harshada Patil                                *
*& Creation Date      :  1-Nov-2011                                    *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  BP notification                               *
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

* Set Global Parameter
  gtp_businesspartner = imp_business_partner.
  gtp_decision_type   = imp_decision_type.
  gtp_manager = imp_manager+2.
  gtp_comment = imp_comment.
  REFRESH : gta_objpack,gta_objbin, gta_objtxt,gta_reclist.
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
