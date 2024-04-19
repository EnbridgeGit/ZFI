*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZAPT_LOCATION...................................*
DATA:  BEGIN OF STATUS_ZAPT_LOCATION                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAPT_LOCATION                 .
CONTROLS: TCTRL_ZAPT_LOCATION
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZLSDBN004.......................................*
DATA:  BEGIN OF STATUS_ZLSDBN004                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLSDBN004                     .
CONTROLS: TCTRL_ZLSDBN004
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAPT_LOCATION                 .
TABLES: *ZLSDBN004                     .
TABLES: ZAPT_LOCATION                  .
TABLES: ZLSDBN004                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
