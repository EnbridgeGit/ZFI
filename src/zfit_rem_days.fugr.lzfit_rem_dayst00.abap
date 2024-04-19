*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_REM_DAYS...................................*
DATA:  BEGIN OF STATUS_ZFIT_REM_DAYS                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_REM_DAYS                 .
CONTROLS: TCTRL_ZFIT_REM_DAYS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_REM_DAYS                 .
TABLES: ZFIT_REM_DAYS                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
