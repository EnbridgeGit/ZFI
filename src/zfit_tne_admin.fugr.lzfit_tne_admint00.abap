*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_TNE_ADMIN..................................*
DATA:  BEGIN OF STATUS_ZFIT_TNE_ADMIN                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_TNE_ADMIN                .
CONTROLS: TCTRL_ZFIT_TNE_ADMIN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_TNE_ADMIN                .
TABLES: ZFIT_TNE_ADMIN                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
