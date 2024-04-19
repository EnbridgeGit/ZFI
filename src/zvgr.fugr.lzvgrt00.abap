*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2001/11/14 at 09:51:06 by user DSARTOR
*---------------------------------------------------------------------*
*...processing: Z9000...........................................*
DATA:  BEGIN OF STATUS_Z9000                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z9000                         .
CONTROLS: TCTRL_Z9000
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZFUE1...........................................*
DATA:  BEGIN OF STATUS_ZFUE1                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFUE1                         .
CONTROLS: TCTRL_ZFUE1
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZFUE6...........................................*
DATA:  BEGIN OF STATUS_ZFUE6                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFUE6                         .
CONTROLS: TCTRL_ZFUE6
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZOHAG...........................................*
DATA:  BEGIN OF STATUS_ZOHAG                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZOHAG                         .
CONTROLS: TCTRL_ZOHAG
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZOHDP...........................................*
DATA:  BEGIN OF STATUS_ZOHDP                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZOHDP                         .
CONTROLS: TCTRL_ZOHDP
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZOHGS...........................................*
DATA:  BEGIN OF STATUS_ZOHGS                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZOHGS                         .
CONTROLS: TCTRL_ZOHGS
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZOHNP...........................................*
DATA:  BEGIN OF STATUS_ZOHNP                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZOHNP                         .
CONTROLS: TCTRL_ZOHNP
            TYPE TABLEVIEW USING SCREEN '0005'.
*.........table declarations:.................................*
TABLES: *Z9000                         .
TABLES: *ZFUE1                         .
TABLES: *ZFUE6                         .
TABLES: *ZOHAG                         .
TABLES: *ZOHDP                         .
TABLES: *ZOHGS                         .
TABLES: *ZOHNP                         .
TABLES: Z9000                          .
TABLES: ZFUE1                          .
TABLES: ZFUE6                          .
TABLES: ZOHAG                          .
TABLES: ZOHDP                          .
TABLES: ZOHGS                          .
TABLES: ZOHNP                          .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
