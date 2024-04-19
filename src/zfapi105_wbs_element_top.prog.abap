*&---------------------------------------------------------------------*
*&  Include           ZFAPI105_WBS_ELEMENT_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPI105_WBS_ELEMENT                          *
* Author             :                                                 *
* Date               :   Jan 10, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   For WBS data through interface for IAP        *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 10-Jan-2018  CPALYAM     D30K928578  CHG0100809-Initial Development  *                                                                     *
*                          D30K928836, D30K928894                      *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:   sscrfields.

*=======================================================================
* GLOBAL TYPES
*=======================================================================
TYPES: BEGIN OF gty_prps,
       posid      TYPE ps_posid,
       post1      TYPE ps_post1,
       objnr      TYPE j_objnr,
       pbukr      TYPE ps_pbukr,
       verna      TYPE ps_verna,
       loevm      TYPE loevm,
       END OF gty_prps,

       BEGIN OF gty_output,
       erpid(5)   TYPE c,
       posid      TYPE ps_posid,
       post1      TYPE ps_post1,
       erp_comp(10) TYPE c,
       pbukr      TYPE ps_pbukr,
*       verna      TYPE ps_verna,
       status(1)  TYPE c,
       END OF gty_output.

*=======================================================================
* GLOBAL CONSTANTS
*=======================================================================
CONSTANTS:
    gc_x            TYPE c                VALUE 'X',
    gc_fslash       TYPE char1            VALUE '/',
    gc_ucomm_onli   TYPE sscrfields-ucomm VALUE 'ONLI',
    gc_filename     TYPE fileintern
                     VALUE 'SSSSS_Wbs_Elmnt_YYMMDDHHMM_NNN.CSV',
    gc_delim        TYPE char2 VALUE '|~'.

*=======================================================================
* GLOBAL VARIABLES
*=======================================================================
DATA: gv_pathsplit      TYPE string,
      gv_path           TYPE string,
      gv_filename1      TYPE string,    " File name
      gv_fname1         TYPE string,
      gv_fold1          TYPE string,
      gv_flag           TYPE c,
      gv_aedat TYPE updat,
      gv_posid TYPE ps_posid,

      gv_cn_recs_total TYPE i,
      gv_cn_recs_max   TYPE i,
      gv_cn_recs_file  TYPE i,
      gv_cn_files      TYPE i,
      gv_filename      TYPE text256,
      gv_filename_p    TYPE text256.


*=======================================================================
* GLOBAL WORKAREAS
*=======================================================================
DATA:
  gs_prps           TYPE gty_prps,
  gs_output         TYPE gty_output,
  gs_tab_data       TYPE string.
*=======================================================================
* GLOBAL INTERNAL TABLES
*=======================================================================
DATA:
  gt_pathsplit      TYPE STANDARD TABLE OF string,
  gt_tab_data       TYPE STANDARD TABLE OF string,
  gt_output         TYPE STANDARD TABLE OF gty_output,
  gt_prps           TYPE STANDARD TABLE OF gty_prps.

******************************************************************************************************
* REFERRENCE VARIABLES
******************************************************************************************************
* Reference for Interface monitor
DATA : gref_util TYPE REF TO zcl_iap_interface_util.
