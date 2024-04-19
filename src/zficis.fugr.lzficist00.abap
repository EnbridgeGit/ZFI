*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLFICIS01.......................................*
DATA:  BEGIN OF STATUS_ZLFICIS01                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLFICIS01                     .
CONTROLS: TCTRL_ZLFICIS01
            TYPE TABLEVIEW USING SCREEN '0150'.
*.........table declarations:.................................*
TABLES: *ZLFICIS01                     .
TABLES: ZLFICIS01                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
