*&---------------------------------------------------------------------*
*&  Include           ZFAPE101_VENDOR_CONTACT_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:   sscrfields.

*=======================================================================
* GLOBAL CONSTANTS
*=======================================================================
CONSTANTS:
    gc_fslash       TYPE char1            VALUE '\',
    gc_ucomm_onli   TYPE sscrfields-ucomm VALUE 'ONLI',
    gc_en           TYPE spras            VALUE 'E',
    gc_0009         TYPE abtnr_pa         VALUE '0009',
    gc_13           TYPE pafkt            VALUE '13',
    gc_0002         TYPE abtnr_pa         VALUE '0002',
    gc_12           TYPE pafkt            VALUE '12'.

*=======================================================================
* GLOBAL TYPES
*=======================================================================
TYPES: BEGIN OF gty_t002t,
       sprsl TYPE spras,
       sptxt TYPE sptxt,
       END OF   gty_t002t,

       BEGIN OF gty_t005t,
       land1 TYPE land1,
       landx TYPE landx,
       END OF   gty_t005t,

       BEGIN OF gty_t077y,
       ktokk TYPE ktokk,
       txt30 TYPE txt30_077t,
       END OF   gty_t077y,

       BEGIN OF gty_lfa1,
       lifnr TYPE lifnr,
       land1 TYPE land1_gp,
       name1 TYPE name1_gp,
       adrnr TYPE adrnr,
       ktokk TYPE ktokk,
       stcd1 TYPE stcd1,
       stcd2 TYPE stcd2,
       END OF   gty_lfa1,

       BEGIN OF gty_knvk,
       parnr TYPE parnr,
       abtnr TYPE abtnr_pa,
       pafkt TYPE pafkt,
       lifnr TYPE lifnr,
       prsnr TYPE ad_persnum,
       END OF   gty_knvk,

       BEGIN OF gty_adrp,
       persnumber TYPE ad_persnum,
       name_first TYPE ad_namefir,
       name_last  TYPE ad_namelas,
       langu      TYPE spras,
       END OF   gty_adrp,

       BEGIN OF gty_adr2,
       addrnumber TYPE ad_addrnum,
       persnumber TYPE ad_persnum,
       country    TYPE ad_comctry,
       tel_number TYPE ad_tlnmbr,
       END OF   gty_adr2,

       BEGIN OF gty_adr6,
       addrnumber TYPE ad_addrnum,
       persnumber TYPE ad_persnum,
       smtp_addr  TYPE ad_smtpadr,
       END OF   gty_adr6,

       BEGIN OF gty_output,
       erpid(5)   TYPE c,
       lifnr TYPE lifnr,
       land1 TYPE land1_gp,
       name1 TYPE name1_gp,
       ktokk(40) TYPE c, "Vendor Account Group and Description
       stcd1 TYPE stcd1,
       dept(2)    TYPE c,
       func(20)   TYPE c,
       smtp_addr  TYPE ad_smtpadr,
       name_first TYPE ad_namefir,
       name_last  TYPE ad_namelas,
       tel_number TYPE ad_tlnmbr,
       landx TYPE landx,
       sptxt TYPE sptxt,
       END OF   gty_output,

       BEGIN OF gty_error,
       err_txt    TYPE ufdescr,
       END OF gty_error.

*=======================================================================
* GLOBAL VARIABLES
*=======================================================================
DATA: gv_lifnr  TYPE lifnr,
      gv_ktokk  TYPE ktokk,
      gv_filename1      TYPE string,    " File name
      gv_fname1         TYPE string,
      gv_fname2         TYPE string,
      gv_fold1          TYPE string,
      gv_fold2          TYPE string.

*=======================================================================
* GLOBAL WORKAREAS
*=======================================================================
DATA:
  gs_output         TYPE gty_output,
  gs_error          TYPE gty_error,
  gs_tab_data       TYPE string,
  gs_t002t          TYPE gty_t002t,
  gs_t005t          TYPE gty_t005t,
  gs_t077y          TYPE gty_t077y,
  gs_lfa1           TYPE gty_lfa1,
  gs_knvk           TYPE gty_knvk,
  gs_adrp           TYPE gty_adrp,
  gs_adr2           TYPE gty_adr2,
  gs_adr6           TYPE gty_adr6.

*=======================================================================
* GLOBAL INTERNAL TABLES
*=======================================================================
DATA:
  gt_tab_data       TYPE STANDARD TABLE OF string,
  gt_output         TYPE STANDARD TABLE OF gty_output,
  gt_error          TYPE STANDARD TABLE OF gty_error,
  gt_t002t          TYPE STANDARD TABLE OF gty_t002t,
  gt_t005t          TYPE STANDARD TABLE OF gty_t005t,
  gt_t077y          TYPE STANDARD TABLE OF gty_t077y,
  gt_lfa1           TYPE STANDARD TABLE OF gty_lfa1,
  gt_knvk           TYPE STANDARD TABLE OF gty_knvk,
  gt_adrp           TYPE STANDARD TABLE OF gty_adrp,
  gt_adr2           TYPE STANDARD TABLE OF gty_adr2,
  gt_adr6           TYPE STANDARD TABLE OF gty_adr6.

******************************************************************************************************
* REFERRENCE VARIABLES
******************************************************************************************************
* Reference for Interface monitor
DATA : gref_util TYPE REF TO zcl_iap_interface_util.
