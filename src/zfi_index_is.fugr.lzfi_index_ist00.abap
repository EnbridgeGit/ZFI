*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_INDEX_IS....................................*
DATA:  BEGIN OF STATUS_ZFI_INDEX_IS                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_INDEX_IS                  .
CONTROLS: TCTRL_ZFI_INDEX_IS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_INDEX_IS                  .
TABLES: ZFI_INDEX_IS                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
