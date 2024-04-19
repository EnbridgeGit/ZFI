FUNCTION ZFI_CONTRAX_00001430.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BKDF) TYPE  BKDF OPTIONAL
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_BSEC STRUCTURE  BSEC OPTIONAL
*"      T_BKPFSUB STRUCTURE  BKPF_SUBST
*"      T_BSEGSUB STRUCTURE  BSEG_SUBST
*"  CHANGING
*"     REFERENCE(I_BKDFSUB) TYPE  BKDF_SUBST OPTIONAL
*"----------------------------------------------------------------------

  BREAK rajamans.
  FIELD-SYMBOLS: <f_bseg> TYPE bseg.
  data: tbsegsub TYPE bseg_subst,
        lv_agreement TYPE dzuonr,
        lv_customer TYPE kunnr.

  LOOP AT t_bseg ASSIGNING <f_bseg>.
   IF <f_bseg>-bschl NE '50'.
    lv_agreement = <f_bseg>-ZUONR.
    lv_customer = <f_bseg>-KUNNR.
   ENDIF.
   IF <f_bseg>-bschl = '50'.
*   READ TABLE t_bsegsub INTO tbsegsub INDEX sy-tabix.
*    IF sy-subrc = 0.
        <f_bseg>-ZUONR = lv_agreement.
        <f_bseg>-sgtxt = lv_customer.
*    modify t_bsegsub FROM tbsegsub INDEX sy-tabix.
    clear: lv_agreement,
           lv_customer.
    ENDIF.
*   ENDIF.
  ENDLOOP.


ENDFUNCTION.
