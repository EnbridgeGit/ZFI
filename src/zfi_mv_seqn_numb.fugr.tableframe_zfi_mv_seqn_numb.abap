*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFI_MV_SEQN_NUMB
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFI_MV_SEQN_NUMB   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
