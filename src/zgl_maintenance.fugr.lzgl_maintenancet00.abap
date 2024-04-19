*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL_POSTINGS....................................*
DATA:  BEGIN OF STATUS_ZGL_POSTINGS                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL_POSTINGS                  .
CONTROLS: TCTRL_ZGL_POSTINGS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL_POSTINGS                  .
TABLES: ZGL_POSTINGS                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
