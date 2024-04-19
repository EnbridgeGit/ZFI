*&---------------------------------------------------------------------*
*& Report  ZFAPI114_PAYMENT_VOIDS_TOP                                  *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZFAPI114_PAYMENT_VOIDS                         *
* Include            :  ZFAPI114_PAYMENT_VOIDS_TOP                     *
* Author             :  Vijay Rajaputra                                *
* Date               :  05-Mar-2018                                    *
* Technical Contact  :  Vijay Rajaputra                                *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  ATCAT Payment Voids Extract Interface          *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 05-Mar-2018  VRAJAPUTRA    D30K928605 CHG0100819 Initial Development *
*                            D30K928780                                *
*&---------------------------------------------------------------------*

* Local variables Declarations

TABLES : sscrfields.

TYPES: BEGIN OF gty_object,
        objectid  TYPE cdobjectv,
       END OF gty_object,

       BEGIN OF gty_cdhdr,
        bukrs     TYPE bukrs,
        augbl     TYPE augbl,
        gjahr     TYPE gjahr,
        lifnr     TYPE lifnr,
        ldate     TYPE sydatum,
        hdate     TYPE sydatum,
       END OF   gty_cdhdr,

       BEGIN OF gty_reguh,
        zbukr     TYPE bukrs,
        vblnr     TYPE vblnr,
        zaldt     TYPE dzaldt_zhl,
       END OF   gty_reguh,

       BEGIN OF gty_data,
*       erpid     TYPE c LENGTH 5,   " System
        col1      TYPE c LENGTH 30,  " ChequeULID
        col2      TYPE c LENGTH 10,  " PaymentNumber
       END OF   gty_data.

DATA : gt_object  TYPE STANDARD TABLE OF gty_object,
       gt_cdhdr   TYPE STANDARD TABLE OF gty_cdhdr,
       gt_reguh   TYPE STANDARD TABLE OF gty_reguh,
       gt_data    TYPE STANDARD TABLE OF gty_data.

DATA : gv_cn_recs TYPE i.

CONSTANTS : gc_x           TYPE c VALUE 'X',
            gc_ucomm_onli  TYPE sscrfields-ucomm VALUE 'ONLI'.
