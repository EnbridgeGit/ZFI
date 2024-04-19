*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIMV_REM_DAYS..................................*
TABLES: ZFIMV_REM_DAYS, *ZFIMV_REM_DAYS. "view work areas
CONTROLS: TCTRL_ZFIMV_REM_DAYS
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIMV_REM_DAYS. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIMV_REM_DAYS.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIMV_REM_DAYS_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_REM_DAYS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_REM_DAYS_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIMV_REM_DAYS_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_REM_DAYS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_REM_DAYS_TOTAL.

*.........table declarations:.................................*
TABLES: ZFIT_REM_DAYS                  .
