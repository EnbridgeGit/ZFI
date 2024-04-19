*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIMV_VAL_BLART.................................*
TABLES: ZFIMV_VAL_BLART, *ZFIMV_VAL_BLART. "view work areas
CONTROLS: TCTRL_ZFIMV_VAL_BLART
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIMV_VAL_BLART. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIMV_VAL_BLART.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIMV_VAL_BLART_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_VAL_BLART.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_VAL_BLART_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIMV_VAL_BLART_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_VAL_BLART.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_VAL_BLART_TOTAL.

*.........table declarations:.................................*
TABLES: T003                           .
TABLES: T003T                          .
TABLES: ZFIT_VALID_BLART               .
