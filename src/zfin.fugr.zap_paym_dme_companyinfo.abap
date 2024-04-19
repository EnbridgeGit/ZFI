FUNCTION zap_paym_dme_companyinfo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_TREE_TYPE) TYPE  DMEE_TREETYPE_ABA
*"     VALUE(I_TREE_ID) TYPE  DMEE_TREEID_ABA
*"     VALUE(I_ITEM)
*"     VALUE(I_PARAM)
*"     VALUE(I_UPARAM)
*"  EXPORTING
*"     REFERENCE(O_VALUE)
*"     REFERENCE(C_VALUE)
*"     REFERENCE(N_VALUE)
*"     REFERENCE(P_VALUE)
*"  TABLES
*"      I_TAB
*"----------------------------------------------------------------------

  DATA: ls_fpayh    TYPE fpayh,
        ls_item     TYPE dmee_paym_if_type,
        lv_bukrs    TYPE bapi0002_2-comp_code,
        ls_detail   TYPE bapi0002_2,
        ls_address  TYPE bapi0002_3,
        ls_return   TYPE bapireturn,
        lv_address  TYPE char140.


  CLEAR    c_value.
  ls_item = i_item.
  MOVE     ls_item-fpayh      TO ls_fpayh.
  lv_bukrs = ls_fpayh-zbukr.
  CALL FUNCTION 'BAPI_COMPANYCODE_GETDETAIL'
    EXPORTING
      companycodeid       = lv_bukrs
    IMPORTING
      companycode_detail  = ls_detail
      companycode_address = ls_address
      return              = ls_return.

  CONCATENATE ls_detail-comp_name ls_address-street ls_address-city
              ls_address-region ls_address-postl_cod1
              INTO lv_address SEPARATED BY space.
  c_value = lv_address.

ENDFUNCTION.
