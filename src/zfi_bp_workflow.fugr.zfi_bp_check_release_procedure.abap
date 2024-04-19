FUNCTION zfi_bp_check_release_procedure.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_BUSINESS_PARTNER) TYPE  BUT000-PARTNER
*"  EXCEPTIONS
*"      NOT_RELEASED
*"      REJECTED
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_BP_CHECK_RELEASE_PROCEDURE                *
*& Author             :  Harshada Patil                                *
*& Creation Date      :  1-Nov-2011                                    *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Check released                                *
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


  DATA: ltp_not_released TYPE bu_xnot_released,
        ltp_skey TYPE vzfgd-skey1,
        ltp_userdecision TYPE c.

  ltp_skey = imp_business_partner.
  CALL FUNCTION 'DYNPRO_CONFIRMATION'
    EXPORTING
      i_sfgobj         = 'ZGP'
      i_skey1          = ltp_skey
      i_mode           = 'APPROVE'
    IMPORTING
      e_useraction     = ltp_userdecision
    EXCEPTIONS
      object_not_found = 1
      OTHERS           = 2.
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.


  if ltp_userdecision  = '4'.     "REJECT
      RAISE rejected.

  elseif ltp_userdecision  = '8'.     "PUTBACK
*    Put Back Workitem
      CALL FUNCTION 'FVZLWF_PUT_BACK_WORKITEM_SNT'
        STARTING NEW TASK 'PUT_BACK'
        EXPORTING
          workitem_id = gtp_workflow_id
        EXCEPTIONS
          OTHERS      = 00.
*      CALL FUNCTION 'FVZLWF_ENQUEUE_AFTER_PUTBACK'
*        EXPORTING
*          i_sfgobj = 'ZGP'
*          i_skey1  = ltp_skey.
      RAISE not_released.

    elseif ltp_userdecision  = 'A'.     "CANCEL
*    Put Back Workitem
      CALL FUNCTION 'FVZLWF_PUT_BACK_WORKITEM_SNT'
        STARTING NEW TASK 'PUT_BACK'
        EXPORTING
          workitem_id = gtp_workflow_id
        EXCEPTIONS
          OTHERS      = 00.
*      CALL FUNCTION 'FVZLWF_ENQUEUE_AFTER_PUTBACK'
*        EXPORTING
*          i_sfgobj = 'ZGP'
*          i_skey1  = ltp_skey.
      RAISE not_released.

  ENDif.


ENDFUNCTION.
