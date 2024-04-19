*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIT_EFSCC......................................*
DATA:  BEGIN OF STATUS_ZFIT_EFSCC                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_EFSCC                    .
CONTROLS: TCTRL_ZFIT_EFSCC
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZFIT_EFSCS......................................*
DATA:  BEGIN OF STATUS_ZFIT_EFSCS                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_EFSCS                    .
CONTROLS: TCTRL_ZFIT_EFSCS
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZFIT_EFSFILTER..................................*
DATA:  BEGIN OF STATUS_ZFIT_EFSFILTER                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_EFSFILTER                .
CONTROLS: TCTRL_ZFIT_EFSFILTER
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZFIT_EFSILOB....................................*
DATA:  BEGIN OF STATUS_ZFIT_EFSILOB                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_EFSILOB                  .
CONTROLS: TCTRL_ZFIT_EFSILOB
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZFIT_EFSLOB.....................................*
DATA:  BEGIN OF STATUS_ZFIT_EFSLOB                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_EFSLOB                   .
CONTROLS: TCTRL_ZFIT_EFSLOB
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZFIT_EFSSUB.....................................*
DATA:  BEGIN OF STATUS_ZFIT_EFSSUB                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_EFSSUB                   .
CONTROLS: TCTRL_ZFIT_EFSSUB
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZFIT_EGDTIMESTMP................................*
DATA:  BEGIN OF STATUS_ZFIT_EGDTIMESTMP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIT_EGDTIMESTMP              .
CONTROLS: TCTRL_ZFIT_EGDTIMESTMP
            TYPE TABLEVIEW USING SCREEN '0007'.
*.........table declarations:.................................*
TABLES: *ZFIT_EFSCC                    .
TABLES: *ZFIT_EFSCS                    .
TABLES: *ZFIT_EFSFILTER                .
TABLES: *ZFIT_EFSILOB                  .
TABLES: *ZFIT_EFSLOB                   .
TABLES: *ZFIT_EFSSUB                   .
TABLES: *ZFIT_EGDTIMESTMP              .
TABLES: ZFIT_EFSCC                     .
TABLES: ZFIT_EFSCS                     .
TABLES: ZFIT_EFSFILTER                 .
TABLES: ZFIT_EFSILOB                   .
TABLES: ZFIT_EFSLOB                    .
TABLES: ZFIT_EFSSUB                    .
TABLES: ZFIT_EGDTIMESTMP               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
