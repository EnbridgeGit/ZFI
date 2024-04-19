class ZCL_IM_AP_BADI_FDCB_SUBB05 definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_AP_BADI_FDCB_SUBB05
*"* do not include other source files here!!!

  interfaces IF_EX_BADI_FDCB_SUBBAS05 .
protected section.
*"* protected components of class ZCL_IM_AP_BADI_FDCB_SUBB05
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_AP_BADI_FDCB_SUBB05
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_AP_BADI_FDCB_SUBB05 IMPLEMENTATION.


method IF_EX_BADI_FDCB_SUBBAS05~GET_DATA_FROM_SCREEN_OBJECT.

* fill export parameters from interface attributes

  ex_invfo = me->if_ex_badi_fdcb_subbas05~invfo.

endmethod.


method IF_EX_BADI_FDCB_SUBBAS05~PUT_DATA_TO_SCREEN_OBJECT.

* fill interface attributes from importing parameters

  me->if_ex_badi_fdcb_subbas05~invfo = im_invfo.

endmethod.
ENDCLASS.
