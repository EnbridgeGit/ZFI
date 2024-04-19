*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_CNTRXICMAP.................................*
DATA:  BEGIN OF STATUS_ZFIT_CNTRXICMAP               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_CNTRXICMAP               .
CONTROLS: TCTRL_ZFIT_CNTRXICMAP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_CNTRXICMAP               .
TABLES: ZFIT_CNTRXICMAP                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
