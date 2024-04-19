*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZVGR
*   generation date: 12/30/1997 at 15:35:08 by user SAPDEVE
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZVGR               .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
