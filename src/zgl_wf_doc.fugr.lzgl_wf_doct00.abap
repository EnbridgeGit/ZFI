*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZGL_WF_DOC......................................*
DATA:  BEGIN OF STATUS_ZGL_WF_DOC                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZGL_WF_DOC                    .
CONTROLS: TCTRL_ZGL_WF_DOC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZGL_WF_DOC                    .
TABLES: ZGL_WF_DOC                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
