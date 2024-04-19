FUNCTION zfi_ap_post_invoice.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_BUKRS) TYPE  BKPF-BUKRS
*"     VALUE(IMP_BELNR) TYPE  BKPF-BELNR
*"     VALUE(IMP_GJAHR) TYPE  BKPF-GJAHR
*"     VALUE(IMP_DECISION_TYPE) TYPE  ZFIS_AP_WORKFLOW-DECISION_TYPE
*"     VALUE(IMP_WORKITEM_ID) TYPE  SWR_STRUCT-WORKITEMID
*"     VALUE(IMP_WORKITEM_TOP) TYPE  SWR_STRUCT-WORKITEMID OPTIONAL
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_AP_POST_INVOICE                           *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  19-Sep-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Post invoice                                  *
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

  DATA:ltp_objkey TYPE swo_typeid.

  gtp_bukrs = imp_bukrs .
  gtp_belnr = imp_belnr .
  gtp_gjahr = imp_gjahr .
  gtp_workitem_id = imp_workitem_id.
  gtp_user_decision = imp_decision_type.
  gtp_workitem_top = imp_workitem_top.

  CONCATENATE gtp_bukrs gtp_belnr gtp_gjahr INTO ltp_objkey.

  CALL FUNCTION 'ZFI_AP_UPDATE_WI_AND_KEY'
    EXPORTING
      imp_objkey = ltp_objkey
      imp_wi     = gtp_workitem_top.


  IF gtp_user_decision = 'POST'.
    CALL SCREEN 9002.
  ELSEIF gtp_user_decision = 'RCOD'.
    CALL SCREEN 9001.
  ELSEIF gtp_user_decision = 'INIT'.
    CALL SCREEN 9003.
  ENDIF.






ENDFUNCTION.
