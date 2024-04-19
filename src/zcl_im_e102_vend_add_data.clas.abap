class ZCL_IM_E102_VEND_ADD_DATA definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_E102_VEND_ADD_DATA
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA .
protected section.
*"* protected components of class ZCL_IM_E102_VEND_ADD_DATA
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_E102_VEND_ADD_DATA
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_E102_VEND_ADD_DATA IMPLEMENTATION.


method IF_EX_VENDOR_ADD_DATA~BUILD_TEXT_FOR_CHANGE_DETAIL.

endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_ACCOUNT_NUMBER.

endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_ADD_ON_ACTIVE.
*----------------------------------------------------------------------*
* Class Name         :   ZFAPE102_VEND_ADD_DATA                        *
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

  IF i_screen_group = 'ZI'.
    e_add_on_active = 'X'.
  ENDIF.

endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_ALL_DATA.

endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_DATA_CHANGED.

endmethod.


method IF_EX_VENDOR_ADD_DATA~GET_CHANGEDOCS_FOR_OWN_TABLES.

endmethod.


method IF_EX_VENDOR_ADD_DATA~INITIALIZE_ADD_ON_DATA.

endmethod.


method IF_EX_VENDOR_ADD_DATA~MODIFY_ACCOUNT_NUMBER.

endmethod.


method IF_EX_VENDOR_ADD_DATA~PRESET_VALUES_CCODE.

endmethod.


method IF_EX_VENDOR_ADD_DATA~PRESET_VALUES_PORG.

endmethod.


method IF_EX_VENDOR_ADD_DATA~PRESET_VALUES_PORG_ALTERNATIVE.

endmethod.


method IF_EX_VENDOR_ADD_DATA~READ_ADD_ON_DATA.

endmethod.


method IF_EX_VENDOR_ADD_DATA~SAVE_DATA.

endmethod.


method IF_EX_VENDOR_ADD_DATA~SET_USER_INPUTS.

endmethod.
ENDCLASS.
