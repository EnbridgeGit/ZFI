*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_TRIAL_BAL...................................*
DATA:  BEGIN OF STATUS_ZFI_TRIAL_BAL                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_TRIAL_BAL                 .
CONTROLS: TCTRL_ZFI_TRIAL_BAL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_TRIAL_BAL                 .
TABLES: ZFI_TRIAL_BAL                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
