FUNCTION zap_paym_dme_grossamt.
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

  DATA: ls_fpayp TYPE fpayp,
        ls_item  TYPE dmee_paym_if_type,
        lv_netamount TYPE fpayp-wrbtr,
        lv_amt type char13.
break sahmad.
  CLEAR:    p_value.

  ls_item = i_item.
  MOVE     ls_item-fpayp      TO ls_fpayp.
  lv_netamount = ls_fpayp-wrbtr - ls_fpayp-wskto.
  lv_amt = lv_netamount.
  p_value = lv_amt.

ENDFUNCTION.
