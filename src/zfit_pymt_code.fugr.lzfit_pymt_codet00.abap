*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_PYMT_CODE..................................*
DATA:  BEGIN OF STATUS_ZFIT_PYMT_CODE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_PYMT_CODE                .
CONTROLS: TCTRL_ZFIT_PYMT_CODE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIT_PYMT_CODE                .
TABLES: ZFIT_PYMT_CODE                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
