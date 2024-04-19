*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_BPCMAPCOG..................................*
DATA:  BEGIN OF STATUS_ZFIT_BPCMAPCOG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_BPCMAPCOG                .
CONTROLS: TCTRL_ZFIT_BPCMAPCOG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_BPCMAPCOG                .
TABLES: ZFIT_BPCMAPCOG                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
