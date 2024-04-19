FUNCTION zap_epaym_dme_drcr.
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

  DATA: ls_fpayp  TYPE fpayp,
        ls_fpayh  TYPE fpayh,
        ls_item   TYPE dmee_paym_if_type.
*        lv_wrbtr  TYPE fpayp-wrbtr,
*        lv_amt    TYPE char13,
*        lv_doc2r  TYPE doc1r_fpm,
*        lv_doc2t  TYPE doc1t_fpm,
*        lv_gjahr2 TYPE gjahr,
*        lv_belnr2 TYPE belnr_d,
*        lv_bukrs2 TYPE bukrs,
*        lv_buzei2 TYPE buzei,
*        lv_doc1r  TYPE doc1r_fpm,
*        lv_doc1t  TYPE doc1t_fpm,
*        lv_gjahr1 TYPE gjahr,
*        lv_belnr1 TYPE belnr_d,
*        lv_bukrs1 TYPE bukrs,
*        lv_buzei1 TYPE buzei,
*        lv_shkzg  TYPE regup-shkzg,
*        lv_lifnr  TYPE regup-lifnr.

  break sahmad.
*  CLEAR:    p_value.
  CLEAR: C_VALUE.
  ls_item = i_item.
  MOVE ls_item-fpayp TO ls_fpayp.
  MOVE ls_item-fpayh TO ls_fpayh.
**  CONDENSE ls_fpayp-gpa2r NO-GAPS.
*  CONDENSE ls_fpayh-gpa1r NO-GAPS.
*  lv_lifnr = ls_fpayh-GPA1R. "ls_fpayp-gpa2r.
*  lv_doc2r = ls_fpayp-doc2r.
*  lv_doc2t = ls_fpayp-doc2t.
*  CALL FUNCTION 'FI_REF_DOCUMENT_INTERPRET'
*    EXPORTING
*      im_doc1r = lv_doc2r
*      im_doc1t = lv_doc2t
*    IMPORTING
*      ex_bukrs = lv_bukrs2
*      ex_belnr = lv_belnr2
*      ex_gjahr = lv_gjahr2
*      ex_buzei = lv_buzei2.
*
*  lv_doc1r = ls_fpayh-doc1r.
*  lv_doc1t = ls_fpayh-doc1t.
*  CALL FUNCTION 'FI_REF_DOCUMENT_INTERPRET'
*    EXPORTING
*      im_doc1r = lv_doc1r
*      im_doc1t = lv_doc1t
*    IMPORTING
*      ex_bukrs = lv_bukrs1
*      ex_belnr = lv_belnr1
*      ex_gjahr = lv_gjahr1
*      ex_buzei = lv_buzei1.
*
*  IF lv_belnr1 IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_belnr1
*      IMPORTING
*        output = lv_belnr1.
*  ENDIF.
*  IF lv_belnr2 IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_belnr2
*      IMPORTING
*        output = lv_belnr2.
*  ENDIF.
*  IF lv_lifnr IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_lifnr
*      IMPORTING
*        output = lv_lifnr.
*  ENDIF.
*  SELECT SINGLE shkzg INTO lv_shkzg FROM regup
*    WHERE laufd = ls_fpayp-laufd
*      AND laufi = ls_fpayp-laufi
*      AND xvorl <> 'X'
*      AND zbukr = ls_fpayh-zbukr
*      AND lifnr = lv_lifnr
*      AND vblnr = lv_belnr1
*      AND bukrs = lv_bukrs2
*      AND belnr = lv_belnr2
*      AND gjahr = lv_gjahr2
*      AND xblnr = ls_fpayp-xblnr
*      AND dmbtr = ls_fpayp-dmbtr
*      AND wrbtr = ls_fpayp-wrbtr
*      AND waers = ls_fpayp-waers.
  IF ls_fpayp-wrbtr < 0. "lv_shkzg = 'S'.  "Debit
*    lv_wrbtr = ls_fpayp-wrbtr * -1.
     C_VALUE = 'CR'.
  ELSE.
*    lv_wrbtr = ls_fpayp-wrbtr.
     C_VALUE = space.
  ENDIF.
*  lv_amt = lv_wrbtr.
*  p_value = lv_amt.

ENDFUNCTION.
