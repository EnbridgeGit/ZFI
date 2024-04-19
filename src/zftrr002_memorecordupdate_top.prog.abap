*&---------------------------------------------------------------------*
*&  Include           ZFTRP002_MEMORECORDUPDATE_TOP
*&---------------------------------------------------------------------*

TABLES: fdes.


**** Start of type Definition

TYPES : BEGIN OF  ty_fdes,
         bukrs     TYPE fdes-bukrs,
         bnkko     TYPE fdes-bnkko,
         ebene     TYPE fdes-ebene,
         datum     TYPE fdes-datum,
         idenr     TYPE fdes-idenr,
         dmshb     TYPE fdes-dmshb,
         wrshb     TYPE fdes-wrshb,
         avdat     TYPE fdes-avdat,
         gvalt     TYPE fdes-gvalt,
         status    TYPE string,
        END OF ty_fdes.

TYPES : BEGIN OF ty_fdes_flag_table.
        INCLUDE STRUCTURE fdes.
TYPES:   flag_change(1),
        END OF ty_fdes_flag_table.
**** End of type Definition

**** Start of data type Definition
DATA : gv_ebene_validation   TYPE  t036-ebene,
       gwa_layout            TYPE  slis_layout_alv,
       gwa_alv_print         TYPE  slis_print_alv,
       gwa_extab             TYPE  slis_extab,
       gv_rep                TYPE  sy-repid.

**** End of data type Definition

**** Start of internal table Definition
DATA : git_fdes              TYPE  STANDARD TABLE OF fdes,
       git_final_output      TYPE  STANDARD TABLE OF ty_fdes,
       git_fdes_flag_table   TYPE  STANDARD TABLE OF ty_fdes_flag_table,
       git_extab             TYPE  slis_t_extab,
       git_fieldcat_display  TYPE slis_t_fieldcat_alv.

**** End of internal table Definition

**** Start of Work Area Definition
DATA : gwa_fdes              TYPE  fdes,
       gwa_final_output      TYPE  ty_fdes,
       gwa_fdes_flag_table   TYPE  ty_fdes_flag_table,
       gwa_fieldcat_display  TYPE  slis_fieldcat_alv.
**** End of Work Area Definition

CONSTANTS : gc_x(1)              TYPE c    VALUE  'X',
            gc_fieldcat_name(4)  TYPE c    VALUE  'FDES'.


TYPES : BEGIN OF ty_fdesdist,
          herku    TYPE fdesdist-herku,
          bukrs    TYPE fdesdist-bukrs,
          idenr    TYPE fdesdist-idenr,
        END OF ty_fdesdist.

TYPES : BEGIN OF ty_t001,
          bukrs    TYPE t001-bukrs,
        END OF ty_t001.

TYPES : BEGIN OF ty_t037,
          dsart    TYPE t037-dsart,
          ebene    TYPE t037-ebene,
        END OF ty_t037.

TYPES : BEGIN OF ty_t036,
          ebene    TYPE t036-ebene,
          orign    TYPE t036-orign,
        END OF ty_t036.

TYPES : BEGIN OF ty_t039,
          orign    TYPE t039-orign,
          xtfst    TYPE t039-xtfst,
        END OF ty_t039.

DATA:  subsystem             TYPE  fdesdist-herku,                  " Same name is used to avoid confusion when debugging FF63.
       aktyp(02)             TYPE  c,                               " Same name is used to avoid confusion when debugging FF63.
       gv_tabix              TYPE  sy-tabix,
       gc_error_text         TYPE  string,
       gc_exp_date(10)       TYPE  c.



* Internal table
DATA : git_fdesdist          TYPE  STANDARD TABLE OF ty_fdesdist,
       git_t001              TYPE  STANDARD TABLE OF ty_t001,
       git_t037              TYPE  STANDARD TABLE OF ty_t037,
       git_t036              TYPE  STANDARD TABLE OF ty_t036,
       git_t039              TYPE  STANDARD TABLE OF ty_t039,
       git_bdcdata           TYPE  STANDARD TABLE OF bdcdata,
       git_bdc_msg_error     TYPE  STANDARD TABLE OF bdcmsgcoll.

* Work area
DATA : gwa_fdesdist          TYPE  ty_fdesdist,
       gwa_t001              TYPE  ty_t001,
       gwa_t037              TYPE  ty_t037,
       gwa_t036              TYPE  ty_t036,
       gwa_t039              TYPE  ty_t039,
       gwa_bdcdata           TYPE  bdcdata,
       gwa_bdc_msg_error     TYPE  bdcmsgcoll.

CONSTANTS: gco_bdc_tcode(4)     TYPE c    VALUE  'FF63',
           gco_bdc_mode(1)      TYPE c    VALUE  'N',
           gco_bdc_option(1)    TYPE c    VALUE  'S'.


*****************************************************************************************
*** SELECTION SCREEN                                                                  ***
*****************************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_ebene  FOR    fdes-ebene  OBLIGATORY,
                 s_datum  FOR    fdes-datum  OBLIGATORY.
PARAMETERS:      cb_test  TYPE   c AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK blk1.
