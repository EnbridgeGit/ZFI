*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_PS_TYPE....................................*
DATA:  BEGIN OF STATUS_ZFIT_PS_TYPE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_PS_TYPE                  .
CONTROLS: TCTRL_ZFIT_PS_TYPE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_PS_TYPE                  .
TABLES: ZFIT_PS_TYPE                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
