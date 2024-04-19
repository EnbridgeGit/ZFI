*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIMV_PS_TYPE...................................*
TABLES: ZFIMV_PS_TYPE, *ZFIMV_PS_TYPE. "view work areas
CONTROLS: TCTRL_ZFIMV_PS_TYPE
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIMV_PS_TYPE. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIMV_PS_TYPE.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIMV_PS_TYPE_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_PS_TYPE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_PS_TYPE_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIMV_PS_TYPE_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_PS_TYPE.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_PS_TYPE_TOTAL.

*.........table declarations:.................................*
TABLES: T000                           .
TABLES: TCJ1                           .
TABLES: TCJ1T                          .
TABLES: ZFIT_PS_TYPE                   .
