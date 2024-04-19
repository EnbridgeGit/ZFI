*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_RC_OWNER...................................*
DATA:  BEGIN OF STATUS_ZFIT_RC_OWNER                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_RC_OWNER                 .
CONTROLS: TCTRL_ZFIT_RC_OWNER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_RC_OWNER                 .
TABLES: ZFIT_RC_OWNER                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
