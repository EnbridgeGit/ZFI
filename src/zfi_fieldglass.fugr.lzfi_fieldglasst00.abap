*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_FGLASS_TAX..................................*
DATA:  BEGIN OF STATUS_ZFI_FGLASS_TAX                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_FGLASS_TAX                .
CONTROLS: TCTRL_ZFI_FGLASS_TAX
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZFI_FGLASS_TRANS................................*
DATA:  BEGIN OF STATUS_ZFI_FGLASS_TRANS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_FGLASS_TRANS              .
CONTROLS: TCTRL_ZFI_FGLASS_TRANS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_FGLASS_TAX                .
TABLES: *ZFI_FGLASS_TRANS              .
TABLES: ZFI_FGLASS_TAX                 .
TABLES: ZFI_FGLASS_TRANS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
