class ZCL_IM_FI_ACC_DOCUMENT definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_FI_ACC_DOCUMENT
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ACC_DOCUMENT .
protected section.
*"* protected components of class ZCL_IM_FI_ACC_DOCUMENT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_FI_ACC_DOCUMENT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_FI_ACC_DOCUMENT IMPLEMENTATION.


method IF_EX_ACC_DOCUMENT~CHANGE.
  BREAK sahmad.
  data: lv_posnr type ACCIT-posnr,
      ls_extension2 TYPE LINE OF BAPIPAREX_TAB_AC,
      ls_accit type LINE OF accit_tab.

LOOP AT c_extension2 INTO ls_extension2 WHERE structure = 'ZFFII041_PERNR'.
  lv_posnr = ls_extension2-valuepart1.
  READ TABLE c_accit INTO ls_accit WITH KEY POSNR = lv_posnr.
  CHECK sy-subrc = 0.
  ls_accit-pernr = ls_extension2-valuepart2.
  MODIFY c_accit FROM ls_accit TRANSPORTING pernr WHERE posnr = lv_posnr.
ENDLOOP.

endmethod.


method IF_EX_ACC_DOCUMENT~FILL_ACCIT.
endmethod.
ENDCLASS.
