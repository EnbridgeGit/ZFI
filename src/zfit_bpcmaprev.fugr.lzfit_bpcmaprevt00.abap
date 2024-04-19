*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_BPCMAPREV..................................*
DATA:  BEGIN OF STATUS_ZFIT_BPCMAPREV                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_BPCMAPREV                .
CONTROLS: TCTRL_ZFIT_BPCMAPREV
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_BPCMAPREV                .
TABLES: ZFIT_BPCMAPREV                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
