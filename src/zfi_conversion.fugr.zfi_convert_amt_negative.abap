FUNCTION zfi_convert_amt_negative.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_WAERS) TYPE  WAERS
*"  EXPORTING
*"     REFERENCE(E_AMOUNT) TYPE  STRING
*"  CHANGING
*"     REFERENCE(C_WRBTR) TYPE  ACBTR
*"----------------------------------------------------------------------
  DATA: lv_wrbtr        TYPE  c LENGTH 15.
  CONSTANTS lc_neg_sign TYPE flag VALUE '-'.
  IF c_wrbtr IS NOT INITIAL.
*This us used to get the thousand separator and decimal notation for amount
    WRITE : c_wrbtr TO lv_wrbtr CURRENCY i_waers NO-GROUPING.
    IF lv_wrbtr CS lc_neg_sign.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = lv_wrbtr.
      CONDENSE lv_wrbtr NO-GAPS.
    ELSE.
      CONDENSE lv_wrbtr NO-GAPS.
    ENDIF.
    e_amount = lv_wrbtr.
  ENDIF.
ENDFUNCTION.
