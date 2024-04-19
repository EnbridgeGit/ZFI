*&---------------------------------------------------------------------*
*&  Include           ZFAPI110_FIELD_STATUS_TOP
*&---------------------------------------------------------------------*

TABLES     : sscrfields,t004f.
TYPE-POOLS : vrm.

CONSTANTS:
        gc_ucomm_onli  TYPE sscrfields-ucomm VALUE 'ONLI'.

TYPES: BEGIN OF gty_final,
        erpid          TYPE char05,      " ERP ID
        fstag          TYPE t004f-fstag, " Field Status Group
        bukrs          TYPE fstva,       " Field Status Variant
        c_center       TYPE char01,      " Cost Center Ind.
        i_order        TYPE char01,      " Internal Order Ind.
        wbs_element    TYPE char01,      " WBS Element Ind.
        network        TYPE char01,      " Network Ind.
        activity       TYPE char01,      " Activity Ind.
       END   OF gty_final.

DATA : gt_t004f        TYPE STANDARD TABLE OF t004f,
       gs_t004f        TYPE t004f,
       gt_final        TYPE STANDARD TABLE OF gty_final,
       gs_final        TYPE gty_final,
       gt_data_tab     TYPE STANDARD TABLE OF string,
       gs_data_tab     TYPE string.

DATA : gv_cn_recs      TYPE i.
