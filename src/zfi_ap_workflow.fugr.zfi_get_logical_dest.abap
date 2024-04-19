FUNCTION zfi_get_logical_dest.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_PARAMTYPE) TYPE  Z_PARAMTYPE DEFAULT 'ECCUS'
*"  EXPORTING
*"     REFERENCE(EXP_RFCDEST) TYPE  TB_RFCDEST
*"----------------------------------------------------------------------

*OBSOLETE
*OBSOLETE, USE ZFI_GET_RFC_DEST
*OBSOLETE

  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      imp_paramtype = imp_paramtype
    IMPORTING
      exp_rfcdest   = exp_rfcdest.
ENDFUNCTION.
