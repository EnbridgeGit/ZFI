*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPSACCT.........................................*
DATA:  BEGIN OF STATUS_ZPSACCT                       .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPSACCT                       .
CONTROLS: TCTRL_ZPSACCT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPSACCT                       .
TABLES: ZPSACCT                        .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
