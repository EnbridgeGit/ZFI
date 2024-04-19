*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIMV_DOA_NEW...................................*
TABLES: ZFIMV_DOA_NEW, *ZFIMV_DOA_NEW. "view work areas
CONTROLS: TCTRL_ZFIMV_DOA_NEW
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIMV_DOA_NEW. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIMV_DOA_NEW.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIMV_DOA_NEW_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_DOA_NEW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_DOA_NEW_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIMV_DOA_NEW_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_DOA_NEW.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_DOA_NEW_TOTAL.

*.........table declarations:.................................*
TABLES: T000                           .
TABLES: TSTC                           .
TABLES: TSTCT                          .
TABLES: ZFIT_DOA_NEW                   .
