class ZCL_IM_VENDORADDDATACS_CNT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_VENDORADDDATACS_CNT
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA_CS .
*"* protected components of class ZCL_IM_VENDORADDDATACS_CNT
*"* do not include other source files here!!!
protected section.
*"* private components of class ZCL_IM_VENDORADDDATACS_CNT
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_IM_VENDORADDDATACS_CNT IMPLEMENTATION.


METHOD if_ex_vendor_add_data_cs~get_data.
  CALL FUNCTION 'ZFI_AP_VENDORCONTACT_GET_DATA'
    TABLES
      t_knvk = t_knvk.
ENDMETHOD.


method IF_EX_VENDOR_ADD_DATA_CS~GET_FIELDNAME_FOR_CHANGEDOC.
endmethod.


METHOD if_ex_vendor_add_data_cs~get_taxi_screen.
  IF i_taxi_fcode = 'ZVCNT'.
    e_program = 'SAPLZFI_AP_VENDORCONTACT'.
    e_screen = '9001'.
  ENDIF.
ENDMETHOD.


METHOD if_ex_vendor_add_data_cs~set_data.
  CALL FUNCTION 'ZFI_AP_VENDORCONTACT_SET_DATA'
    EXPORTING
      i_lfa1  = i_lfa1
      i_aktyp = i_activity
  .
ENDMETHOD.


method IF_EX_VENDOR_ADD_DATA_CS~SET_FCODE.
endmethod.


method IF_EX_VENDOR_ADD_DATA_CS~SUPPRESS_TAXI_TABSTRIPS.
endmethod.
ENDCLASS.
