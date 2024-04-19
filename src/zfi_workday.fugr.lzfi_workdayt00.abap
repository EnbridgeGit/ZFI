*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_WD_PAYFILE..................................*
DATA:  BEGIN OF STATUS_ZFI_WD_PAYFILE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_WD_PAYFILE                .
CONTROLS: TCTRL_ZFI_WD_PAYFILE
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZFI_WD_SUSCCENTR................................*
DATA:  BEGIN OF STATUS_ZFI_WD_SUSCCENTR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_WD_SUSCCENTR              .
CONTROLS: TCTRL_ZFI_WD_SUSCCENTR
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZFI_WD_PAYFILE                .
TABLES: *ZFI_WD_SUSCCENTR              .
TABLES: ZFI_WD_PAYFILE                 .
TABLES: ZFI_WD_SUSCCENTR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
