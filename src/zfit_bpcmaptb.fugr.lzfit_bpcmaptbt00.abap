*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_BPCMAPTB...................................*
DATA:  BEGIN OF STATUS_ZFIT_BPCMAPTB                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_BPCMAPTB                 .
CONTROLS: TCTRL_ZFIT_BPCMAPTB
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_BPCMAPTB                 .
TABLES: ZFIT_BPCMAPTB                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
