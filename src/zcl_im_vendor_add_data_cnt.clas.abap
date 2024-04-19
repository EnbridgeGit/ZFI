class ZCL_IM_VENDOR_ADD_DATA_CNT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_VENDOR_ADD_DATA_CNT
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA .
*"* protected components of class ZCL_IM_VENDOR_ADD_DATA_CNT
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_VENDOR_ADD_DATA_CNT
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_VENDOR_ADD_DATA_CNT IMPLEMENTATION.


method IF_EX_VENDOR_ADD_DATA~BUILD_TEXT_FOR_CHANGE_DETAIL.
endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_ACCOUNT_NUMBER.
endmethod.


method IF_EX_VENDOR_ADD_DATA~CHECK_ADD_ON_ACTIVE.
  IF i_screen_group = 'Z2'.
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
