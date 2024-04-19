class ZCL_IM_E102_VEND_ADD_DATACS definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_E102_VEND_ADD_DATACS
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA_CS .
protected section.
*"* protected components of class ZCL_IM_E102_VEND_ADD_DATACS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_E102_VEND_ADD_DATACS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_E102_VEND_ADD_DATACS IMPLEMENTATION.


method IF_EX_VENDOR_ADD_DATA_CS~GET_DATA.

endmethod.


method IF_EX_VENDOR_ADD_DATA_CS~GET_FIELDNAME_FOR_CHANGEDOC.

endmethod.


method IF_EX_VENDOR_ADD_DATA_CS~GET_TAXI_SCREEN.
*----------------------------------------------------------------------*
* Class Name         :   ZFAPE102_VEND_ADD_DATACS                      *
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
*----------------------------------------------------------------------*

  IF i_taxi_fcode = 'ZIAP'.
    e_program = 'SAPLZFAP106_VEND'.
    e_screen = '9001'.
  ENDIF.

endmethod.


method IF_EX_VENDOR_ADD_DATA_CS~SET_DATA.
*----------------------------------------------------------------------*
* Class Name         :   ZFAPE102_VEND_ADD_DATACS                      *
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
*                          D30K928783                                  *
*----------------------------------------------------------------------*

  CALL FUNCTION 'ZFAP106_VEND_SET_DATA'
    EXPORTING
      iv_zz_iap_vendor_id = i_lfa1-zz_iap_vendor_id
      iv_zz_iap_ritm_id   = i_lfa1-zz_iap_ritm_id
      iv_zz_iap_change_dt = i_lfa1-zz_iap_change_dt.

endmethod.


method IF_EX_VENDOR_ADD_DATA_CS~SET_FCODE.

endmethod.


method IF_EX_VENDOR_ADD_DATA_CS~SUPPRESS_TAXI_TABSTRIPS.

endmethod.
ENDCLASS.
