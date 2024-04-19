*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_IC_ACCT_XREF................................*
DATA:  BEGIN OF STATUS_ZFI_IC_ACCT_XREF              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_IC_ACCT_XREF              .
CONTROLS: TCTRL_ZFI_IC_ACCT_XREF
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZFTV_CA_TAX_XREF................................*
DATA:  BEGIN OF STATUS_ZFTV_CA_TAX_XREF              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFTV_CA_TAX_XREF              .
CONTROLS: TCTRL_ZFTV_CA_TAX_XREF
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZFTV_TEEXPCODE..................................*
DATA:  BEGIN OF STATUS_ZFTV_TEEXPCODE                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFTV_TEEXPCODE                .
CONTROLS: TCTRL_ZFTV_TEEXPCODE
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZTET_MCC........................................*
DATA:  BEGIN OF STATUS_ZTET_MCC                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTET_MCC                      .
CONTROLS: TCTRL_ZTET_MCC
            TYPE TABLEVIEW USING SCREEN '0006'.
*.........table declarations:.................................*
TABLES: *ZFI_IC_ACCT_XREF              .
TABLES: *ZFTV_CA_TAX_XREF              .
TABLES: *ZFTV_TEEXPCODE                .
TABLES: *ZTET_MCC                      .
TABLES: ZFI_IC_ACCT_XREF               .
TABLES: ZFTV_CA_TAX_XREF               .
TABLES: ZFTV_TEEXPCODE                 .
TABLES: ZTET_MCC                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
