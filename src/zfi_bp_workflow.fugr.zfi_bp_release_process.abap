FUNCTION zfi_bp_release_process.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_BUSINESSPARTNER) TYPE  BUT000-PARTNER
*"     REFERENCE(IMP_WORKFLOW_ID) TYPE  SWR_STRUCT-WORKITEMID
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_BP_RELEASE_PROCESS                        *
*& Author             :  Harshada Patil                                *
*& Creation Date      :  1-Nov-2011                                    *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Release BP                                    *
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


  gtp_businesspartner = imp_businesspartner.
  gtp_workflow_id     = imp_workflow_id.

  DATA: ltp_objkey TYPE swo_typeid.
* Prepare the key
 ltp_objkey = gtp_businesspartner.

* Update FM to update Table
 CALL FUNCTION 'ZFI_UPDATE_WI_AND_KEY'
   EXPORTING
     imp_objkey       = ltp_objkey
     imp_wi           = gtp_workflow_id
           .

  CALL SCREEN 9003.

* Commit Work and Wait
  COMMIT WORK AND WAIT.

ENDFUNCTION.
