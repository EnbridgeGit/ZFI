FUNCTION zfi_sample_process_00001120.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BKDF) TYPE  BKDF OPTIONAL
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_BKPFSUB STRUCTURE  BKPF_SUBST
*"      T_BSEGSUB STRUCTURE  BSEG_SUBST
*"      T_BSEC STRUCTURE  BSEC OPTIONAL
*"  CHANGING
*"     REFERENCE(I_BKDFSUB) TYPE  BKDF_SUBST OPTIONAL
*"----------------------------------------------------------------------

  READ TABLE t_bkpf INDEX 1.
  IF t_bkpf-blart = 'SP' AND
     t_bkpf-awtyp = 'HRPAY'.
    LOOP AT t_bsegsub.
      t_bsegsub-valut = t_bkpf-budat.
      t_bsegsub-fdtag = t_bkpf-budat.
      MODIFY t_bsegsub.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
