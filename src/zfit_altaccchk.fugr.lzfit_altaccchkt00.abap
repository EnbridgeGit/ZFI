*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_ALTACCCHK..................................*
DATA:  BEGIN OF STATUS_ZFIT_ALTACCCHK                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_ALTACCCHK                .
CONTROLS: TCTRL_ZFIT_ALTACCCHK
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_ALTACCCHK                .
TABLES: ZFIT_ALTACCCHK                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
