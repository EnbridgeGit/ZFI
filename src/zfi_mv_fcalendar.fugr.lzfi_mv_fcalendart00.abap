*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIMV_FCALENDAR.................................*
TABLES: ZFIMV_FCALENDAR, *ZFIMV_FCALENDAR. "view work areas
CONTROLS: TCTRL_ZFIMV_FCALENDAR
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIMV_FCALENDAR. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIMV_FCALENDAR.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIMV_FCALENDAR_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_FCALENDAR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_FCALENDAR_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIMV_FCALENDAR_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_FCALENDAR.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_FCALENDAR_TOTAL.

*.........table declarations:.................................*
TABLES: TFACD                          .
TABLES: TFACT                          .
TABLES: ZFIT_FCALENDAR                 .
