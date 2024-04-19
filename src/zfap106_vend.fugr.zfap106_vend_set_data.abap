*----------------------------------------------------------------------*
* Func Group Name    :   ZFAP106_VEND                                  *
* Function Module    :   ZFAP106_VEND_SET_DATA                         *
* Author             :                                                 *
* Date               :   Apr 11, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Vendor LFA1 Table Enhancements                *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 11-Apr-2018  CPALYAM     D30K928783  CHG0108326-Initial development  *
*                          D30K928787                                  *
*----------------------------------------------------------------------*
FUNCTION zfap106_vend_set_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_ZZ_IAP_VENDOR_ID) TYPE  Z_IAP_VENDOR_ID OPTIONAL
*"     REFERENCE(IV_ZZ_IAP_RITM_ID) TYPE  Z_IAP_RITM_ID OPTIONAL
*"     REFERENCE(IV_ZZ_IAP_CHANGE_DT) TYPE  Z_IAP_CHANGE_DT OPTIONAL
*"----------------------------------------------------------------------

  lfa1-zz_iap_vendor_id = iv_zz_iap_vendor_id.
  lfa1-zz_iap_ritm_id   = iv_zz_iap_ritm_id.
  lfa1-zz_iap_change_dt = iv_zz_iap_change_dt.

ENDFUNCTION.
