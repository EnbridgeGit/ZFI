FUNCTION ZFI_AP_UPDATE_WI_AND_KEY.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_OBJKEY) TYPE  SWO_TYPEID
*"     REFERENCE(IMP_WI) TYPE  SWW_WIID
*"--------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_AP_UPDATE_WI_AND_KEY                      *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  31-Jan-2012                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Update workitem and key                       *
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


DATA: lwa_zfit_wi_key TYPE zfit_ap_wi_key.

lwa_zfit_wi_key-objkey = imp_objkey.
lwa_zfit_wi_key-wi = imp_wi.

MODIFY zfit_ap_wi_key FROM lwa_zfit_wi_key.

if sy-subrc is INITIAL.

ENDIF.

ENDFUNCTION.
