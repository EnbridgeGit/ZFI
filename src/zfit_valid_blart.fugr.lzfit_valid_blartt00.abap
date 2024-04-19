*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_VALID_BLART................................*
DATA:  BEGIN OF STATUS_ZFIT_VALID_BLART              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_VALID_BLART              .
CONTROLS: TCTRL_ZFIT_VALID_BLART
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_VALID_BLART              .
TABLES: ZFIT_VALID_BLART               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
