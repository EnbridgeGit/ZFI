*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_PCARD_TAX...................................*
DATA:  BEGIN OF STATUS_ZFI_PCARD_TAX                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_PCARD_TAX                 .
CONTROLS: TCTRL_ZFI_PCARD_TAX
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZFI_PCARD_TAX                 .
TABLES: ZFI_PCARD_TAX                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
