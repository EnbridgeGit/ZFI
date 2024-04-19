FUNCTION zap_epaym_dme_po.
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
        lv_doc2r TYPE doc1r_fpm,
        lv_doc2t TYPE doc1t_fpm,
        lv_ebeln TYPE bseg-ebeln,
        lv_gjahr TYPE gjahr,
        lv_belnr TYPE belnr_d,
        lv_bukrs TYPE bukrs,
        lv_BUZEI type BUZEI.

  break sahmad.

  CLEAR: c_value.

  ls_item = i_item.
  MOVE ls_item-fpayp TO ls_fpayp.
  lv_doc2r = ls_fpayp-doc2r.
  lv_doc2t = ls_fpayp-doc2t.

  CALL FUNCTION 'FI_REF_DOCUMENT_INTERPRET'
    EXPORTING
      im_doc1r = lv_doc2r
      im_doc1t = lv_doc2t
    IMPORTING
      ex_bukrs = lv_bukrs
      ex_belnr = lv_belnr
      ex_gjahr = lv_gjahr
      EX_BUZEI = lv_BUZEI
*     EX_KEYNO =
*     EX_PYORD =
*     EX_PERNR =
*     EX_SEQNR =
*     EX_BTZNR =
    .

  IF lv_belnr IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_belnr
      IMPORTING
        output = lv_belnr.
*    select SINGLE shkzg INTO lv_shkzg FROM regup
*      WHERE LAUFD = ls_fpayp-
*        And LAUFI = ls_fpayp-
*        and XVORL = space
*        AND ZBUKR =
*        and LIFNR = ls_fpayp-
*        and bukrs =
*        and VBLNR = ls_fpayp-
*        and BUKRS = lv_bukrs
*        and belnr = lv_belnr
*        and gjahr = lv_gjahr
*        and buzei = lv_buzei
    SELECT SINGLE ebeln INTO lv_ebeln FROM bseg
      WHERE bukrs = lv_bukrs
        AND belnr = lv_belnr
        AND gjahr = lv_gjahr
        AND ebeln <> space.
  ENDIF.

  c_value = lv_ebeln.

ENDFUNCTION.
