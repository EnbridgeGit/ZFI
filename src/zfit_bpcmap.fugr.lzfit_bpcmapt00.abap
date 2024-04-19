*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_BPCMAP.....................................*
DATA:  BEGIN OF STATUS_ZFIT_BPCMAP                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_BPCMAP                   .
CONTROLS: TCTRL_ZFIT_BPCMAP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_BPCMAP                   .
TABLES: ZFIT_BPCMAP                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
