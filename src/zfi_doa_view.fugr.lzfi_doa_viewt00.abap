*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIMV_DOA.......................................*
TABLES: ZFIMV_DOA, *ZFIMV_DOA. "view work areas
CONTROLS: TCTRL_ZFIMV_DOA
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIMV_DOA. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIMV_DOA.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIMV_DOA_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_DOA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_DOA_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIMV_DOA_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_DOA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_DOA_TOTAL.

*.........table declarations:.................................*
TABLES: T000                           .
TABLES: TSTC                           .
TABLES: TSTCT                          .
TABLES: ZFIT_DOA                       .
