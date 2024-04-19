*&---------------------------------------------------------------------*
*&  Include           ZFAPI120_BANK_KEY_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPI120_BANK_KEY                             *
* Author             :                                                 *
* Date               :   Mar 22, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Bank Key Extract Interface                    *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  CPALYAM     D30K928630  CHG0106152-Initial development  *
*                          D30K928713, D30K928842, D30K928902          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:   bnka, sscrfields.

*=======================================================================
* GLOBAL TYPES
*=======================================================================
TYPES: BEGIN OF gty_bnka,
        banks           TYPE banks,
        bankl           TYPE bankk,
        loevm           TYPE loevm,
       END   OF gty_bnka,

       BEGIN OF gty_output,
        erpid           TYPE char5,
        banks           TYPE banks,
        bankl           TYPE bankk,
        status          TYPE char1,
       END   OF gty_output.

*=======================================================================
* GLOBAL CONSTANTS
*=======================================================================
CONSTANTS:
      gc_x              TYPE c                VALUE 'X',
      gc_fslash         TYPE char1            VALUE '/',
      gc_ucomm_onli     TYPE sscrfields-ucomm VALUE 'ONLI',
      gc_filename       TYPE fileintern
                     VALUE 'SSSSS_Bank_Key_YYMMDDHHMM_NNN.CSV',
    gc_delim            TYPE char2 VALUE '|~'.

*=======================================================================
* GLOBAL VARIABLES
*=======================================================================
DATA: gv_pathsplit      TYPE string,
      gv_path           TYPE string,
      gv_filename1      TYPE string,
      gv_fname1         TYPE string,
      gv_fold1          TYPE string,
      gv_flag           TYPE c,
      gv_updat          TYPE sydatum,
      gv_rec_count      TYPE i,

      gv_cn_recs_total  TYPE i,
      gv_cn_recs_max    TYPE i,
      gv_cn_recs_file   TYPE i,
      gv_cn_files       TYPE i,
      gv_filename       TYPE text256,
      gv_filename_p     TYPE text256.

*=======================================================================
* GLOBAL WORKAREAS
*=======================================================================
DATA: gs_tab_data       TYPE string,
      gs_output         TYPE gty_output,
      gs_bnka           TYPE gty_bnka.

*=======================================================================
* GLOBAL INTERNAL TABLES
*=======================================================================
DATA: gt_pathsplit      TYPE STANDARD TABLE OF string,
      gt_tab_data       TYPE STANDARD TABLE OF string,
      gt_output         TYPE STANDARD TABLE OF gty_output,
      gt_bnka           TYPE STANDARD TABLE OF gty_bnka.

******************************************************************************************************
* REFERRENCE VARIABLES
******************************************************************************************************
* Reference for Interface monitor
DATA: gref_util         TYPE REF TO zcl_iap_interface_util.
