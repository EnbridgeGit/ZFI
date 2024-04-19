*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIMV_ACT_EXIT..................................*
TABLES: ZFIMV_ACT_EXIT, *ZFIMV_ACT_EXIT. "view work areas
CONTROLS: TCTRL_ZFIMV_ACT_EXIT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIMV_ACT_EXIT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIMV_ACT_EXIT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIMV_ACT_EXIT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_ACT_EXIT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_ACT_EXIT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIMV_ACT_EXIT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_ACT_EXIT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_ACT_EXIT_TOTAL.

*.........table declarations:.................................*
TABLES: ZFIT_ACTIVE_EXIT               .
