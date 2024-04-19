*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIMV_TNE_ADMIN.................................*
TABLES: ZFIMV_TNE_ADMIN, *ZFIMV_TNE_ADMIN. "view work areas
CONTROLS: TCTRL_ZFIMV_TNE_ADMIN
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIMV_TNE_ADMIN. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIMV_TNE_ADMIN.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIMV_TNE_ADMIN_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_TNE_ADMIN.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_TNE_ADMIN_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIMV_TNE_ADMIN_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_TNE_ADMIN.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_TNE_ADMIN_TOTAL.

*.........table declarations:.................................*
TABLES: USR02                          .
TABLES: ZFIT_TNE_ADMIN                 .
