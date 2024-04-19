FUNCTION ZSAMPLE_INTERFACE_00002214.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_SUBRC) LIKE  SY-SUBRC
*"  TABLES
*"      T_VBKPF STRUCTURE  FVBKPF
*"      T_XVBSEG STRUCTURE  FVBSEG
*"      T_YVBSEG STRUCTURE  FVBSEG
*"----------------------------------------------------------------------

  DATA: key LIKE  swr_struct-object_key,
         lv_comp.

  READ TABLE t_vbkpf INDEX 1.
  IF t_vbkpf-blart = 'SA' OR t_vbkpf-blart = 'SV' OR t_vbkpf-blart = 'IR' OR t_vbkpf-blart = 'PY'.

*    CONCATENATE t_vbkpf-bukrs t_vbkpf-belnr t_vbkpf-gjahr INTO key.
*    CONDENSE key NO-GAPS.
    key+0(4) = t_vbkpf-bukrs.
    key+4(10) = t_vbkpf-belnr.
    key+14(4) = t_vbkpf-gjahr.

*.. Get the complete indicator as it is on the database at present

    SELECT SINGLE xprfg FROM  vbkpf INTO lv_comp
           WHERE  ausbk  = t_vbkpf-bukrs
           AND    belnr  = t_vbkpf-belnr
           AND    gjahr  = t_vbkpf-gjahr.

    IF t_vbkpf-xprfg = 'X'.
      CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
        EXPORTING
          object_type = 'FIPP'
          object_key  = key
          event       = 'COMPLETED'
          commit_work = 'X'.
    ELSEIF SY-TCODE NE 'SBWP'.
      CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
        EXPORTING
          object_type = 'FIPP'
          object_key  = key
          event       = 'CHANGED'
          commit_work = 'X'.
    ENDIF.
  ENDIF.



ENDFUNCTION.
