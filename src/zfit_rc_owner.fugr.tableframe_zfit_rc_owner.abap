*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFIT_RC_OWNER
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFIT_RC_OWNER      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
