*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_SEQN_NUMB..................................*
DATA:  BEGIN OF STATUS_ZFIT_SEQN_NUMB                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_SEQN_NUMB                .
CONTROLS: TCTRL_ZFIT_SEQN_NUMB
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_SEQN_NUMB                .
TABLES: ZFIT_SEQN_NUMB                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
