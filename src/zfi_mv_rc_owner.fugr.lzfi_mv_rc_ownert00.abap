*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIMV_RC_OWNER..................................*
TABLES: ZFIMV_RC_OWNER, *ZFIMV_RC_OWNER. "view work areas
CONTROLS: TCTRL_ZFIMV_RC_OWNER
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZFIMV_RC_OWNER. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFIMV_RC_OWNER.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFIMV_RC_OWNER_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_RC_OWNER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_RC_OWNER_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFIMV_RC_OWNER_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFIMV_RC_OWNER.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFIMV_RC_OWNER_TOTAL.

*.........table declarations:.................................*
TABLES: T000                           .
TABLES: ZFIT_RC_OWNER                  .
