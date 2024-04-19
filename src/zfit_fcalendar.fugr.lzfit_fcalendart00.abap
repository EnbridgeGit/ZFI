*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_FCALENDAR..................................*
DATA:  BEGIN OF STATUS_ZFIT_FCALENDAR                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_FCALENDAR                .
CONTROLS: TCTRL_ZFIT_FCALENDAR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_FCALENDAR                .
TABLES: ZFIT_FCALENDAR                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
