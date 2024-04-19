FUNCTION zfi_ap_get_route_code_owner.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_BUKRS) TYPE  BKPF-BUKRS
*"     REFERENCE(IMP_BELNR) TYPE  BKPF-BELNR
*"     REFERENCE(IMP_GJAHR) TYPE  BKPF-GJAHR
*"  EXPORTING
*"     REFERENCE(EXP_RC_OWNER) TYPE  WFSYST-INITIATOR
*"     REFERENCE(EXP_VENDOR) TYPE  LFA1-LIFNR
*"     REFERENCE(EXP_VNAME) TYPE  LFA1-NAME1
*"     REFERENCE(EXP_AMOUNT) TYPE  VBSEGK-DMBTR
*"     REFERENCE(EXP_PDATE) TYPE  VBKPF-BLDAT
*"  EXCEPTIONS
*"      NOBODY_FOUND
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_AP_GET_ROUTE_CODE_OWNER                   *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  19-Sep-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Get Ap Route code owner                       *
*&---------------------------------------------------------------------*
*ADDITIONAL DATA ON THE INVOICE
  DATA: lv_zfbdt TYPE bsid-zfbdt,
        lv_zbd1t TYPE bsid-zbd1t,
        lv_zbd2t TYPE bsid-zbd2t,
        lv_zbd3t TYPE bsid-zbd3t,
        lv_shkzg TYPE vbsegk-shkzg.

  "GET VENDOR
  SELECT SINGLE lifnr
    FROM vbsegk
    INTO exp_vendor
    WHERE belnr = imp_belnr
    AND bukrs = imp_bukrs
    AND gjahr = imp_gjahr
  .

  "GET VENDOR NAME
  SELECT SINGLE name1
    FROM lfa1
    INTO exp_vname
    WHERE lifnr = exp_vendor
  .

  "Get Invoice Data for Due Date and Amount
  SELECT SINGLE zfbdt zbd1t zbd2t zbd3t shkzg dmbtr
    FROM vbsegk
    INTO (lv_zfbdt, lv_zbd1t, lv_zbd2t, lv_zbd3t, lv_shkzg, exp_amount)
    WHERE belnr = imp_belnr
      AND bukrs = imp_bukrs
      AND gjahr = imp_gjahr
  .

  CALL FUNCTION 'NET_DUE_DATE_GET'
    EXPORTING
      i_zfbdt = lv_zfbdt
      i_zbd1t = lv_zbd1t
      i_zbd2t = lv_zbd2t
      i_zbd3t = lv_zbd3t
      i_shkzg = lv_shkzg
      i_rebzg = imp_belnr
      i_koart = 'D'
    IMPORTING
      e_faedt = exp_pdate.



*ROUTE CODE OWNER
  DATA: ltp_route_code TYPE xref3,
        ltp_rc_owner   TYPE wfsyst-initiator,
        ltp_log_dest   TYPE tb_rfcdest.

* Call FM to get Route Code of the invoice
  CALL FUNCTION 'ZFI_AP_GET_ROUTE_CODE'
    EXPORTING
      imp_bukrs      = imp_bukrs
      imp_belnr      = imp_belnr
      imp_gjahr      = imp_gjahr
    IMPORTING
      exp_route_code = ltp_route_code.

**Get Logical Destination
* Begin Changes by <Chaya>
* DOA Changes
*  CALL FUNCTION 'ZFI_GET_LOGICAL_DEST'
*    EXPORTING
*imp_paramtype = 'ECCUS'
*    IMPORTING
*      exp_rfcdest   = ltp_log_dest.
* End Changes by <Chaya>

  IF ltp_route_code IS NOT INITIAL.
* Begin Changes by <Chaya>
*    CALL FUNCTION 'ZFI_GET_ROUTCODE' DESTINATION ltp_log_dest
       CALL FUNCTION 'ZFI_GET_ROUTCODE'
* End Changes by <Chaya>
    EXPORTING
      imp_route_code = ltp_route_code
    IMPORTING
      exp_rc_owner = ltp_rc_owner
    EXCEPTIONS
      nobody_found.
*     Return owner as agent of this rule
    MOVE ltp_rc_owner TO exp_rc_owner.

  ENDIF.




ENDFUNCTION.
