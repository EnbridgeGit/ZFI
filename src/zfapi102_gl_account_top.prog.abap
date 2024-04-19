*&---------------------------------------------------------------------*
*&  Include           ZFAPI102_GL_ACCOUNT_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI102_GL_ACCOUNT                           *
* Author             :                                                 *
* Date               :   16-Feb-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  G/L Accounts Extraction  for IAP               *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 16-Feb-2018  VRAJAPUTRA  D30K928566 CHG0100806 # Initial development *
*                          D30K928825, D30K928874, D30K928888          *
*&---------------------------------------------------------------------*

* Local variables Declarations

TABLES : ska1,t001,sscrfields.

TYPES: BEGIN OF gty_ska1,
       ktopl TYPE ktopl,
       saknr TYPE saknr,
       xspeb TYPE xspeb,
       END OF gty_ska1,

       BEGIN OF gty_skb1,
       bukrs TYPE bukrs,
       saknr TYPE saknr,
       FSTAG TYPE FSTAG,
       xspeb TYPE xspeb,
       xintb TYPE xintb,
       END OF gty_skb1,

       BEGIN OF gty_cskb,
       kstar TYPE kstar,  " Controlling Area
       END OF gty_cskb,


       BEGIN OF gty_skat,
       KTOPL TYPE KTOPL,
       SAKNR TYPE saknr,
       TXT20 TYPE TXT20_SKAT,
       END OF gty_skat,

       BEGIN OF gty_data,
       col1 TYPE c LENGTH 5,  " System
       col2 TYPE c LENGTH 4,  " Company Code
       col3 TYPE c LENGTH 10, " GL_Account
       col4 TYPE c LENGTH 20, " Description
       col5 TYPE c LENGTH 1,  " Cost Element Flag
       col6 TYPE c LENGTH 1,  " Status
       col7 TYPE c LENGTH 4,  " Field Status Group
       END OF gty_data.

DATA : gt_ska1 TYPE TABLE OF gty_ska1,
       gt_skb1 TYPE TABLE OF gty_skb1,
       gt_skat TYPE TABLE OF gty_skat,
       gt_cskb TYPE TABLE OF gty_cskb,
       gt_data TYPE TABLE OF gty_data,

       gv_cn_recs_total TYPE i,
       gv_cn_recs_max   TYPE i,
       gv_cn_recs_file  TYPE i,
       gv_cn_files      TYPE i,
       gv_filename      TYPE text256,
       gv_filename_p    TYPE text256.

CONSTANTS : gc_x           TYPE c VALUE 'X',
            gc_ucomm_onli  TYPE sscrfields-ucomm VALUE 'ONLI',
            gc_filename    TYPE fileintern
                         VALUE 'SSSSS_GL_Accnt_YYMMDDHHMM_NNN.CSV',
            gc_delim         TYPE char2 VALUE '|~'.
