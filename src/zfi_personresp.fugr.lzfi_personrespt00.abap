*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_PERSONRESP..................................*
DATA:  BEGIN OF STATUS_ZFI_PERSONRESP                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_PERSONRESP                .
CONTROLS: TCTRL_ZFI_PERSONRESP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_PERSONRESP                .
TABLES: ZFI_PERSONRESP                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
