*&---------------------------------------------------------------------*
*&  Include           ZFAPI104_INTERNAL_ORDER_TOP
*&---------------------------------------------------------------------*
************************************************************************
*                            Enbridge Energy                           *
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI104_INTERNAL_ORDER                       *
*& Author             :  Kalinga Keshari Rout                          *
*& Creation Date      :  January 09, 2018                              *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Program extracts master data for both cases   *
*&                       full load and delta load and file created in  *
*&                       application server  .Frequency of data upload *
*&                        weekly                                       *
*&---------------------------------------------------------------------*
*                         Modification Log                             *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* -------------------------------------------------------------------- *
* 19-Jan-2018  KROUT       D30K928564  CHG0100808  Initial Development *
*                          D30K928834, D30K928890                      *
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_aufk,
      aufnr TYPE aufk-aufnr,
      ktext TYPE aufk-ktext,
      bukrs TYPE aufk-bukrs,
      phas3 TYPE aufk-phas3,
      loekz TYPE aufk-loekz,
      objnr TYPE aufk-objnr,
       END OF ty_aufk .

TYPES: BEGIN OF ty_final,
      erpid(5) TYPE c,
*      erp_comp(10) TYPE c,
      bukrs TYPE aufk-bukrs,
      aufnr TYPE aufk-aufnr,
      ktext TYPE aufk-ktext,
      status TYPE c ,
      END OF ty_final.

DATA : gt_aufk TYPE STANDARD TABLE OF ty_aufk,
       gs_aufk TYPE ty_aufk ,
       gt_final TYPE STANDARD TABLE OF ty_final ,
       gs_final TYPE ty_final,
       gv_chk,
       gv_autyp TYPE auftyp,

       gv_cn_recs_total TYPE i,
       gv_cn_recs_max   TYPE i,
       gv_cn_recs_file  TYPE i,
       gv_cn_files      TYPE i,
       gv_filename      TYPE text256,
       gv_filename_p    TYPE text256.

CONSTANTS :  gc_x            TYPE c                VALUE 'X' ,
           gc_ucomm_onli   TYPE sscrfields-ucomm VALUE 'ONLI',
           gc_filename    TYPE fileintern
                            VALUE 'SSSSS_Int_Order_YYMMDDHHMM_NNN.CSV',
           gc_delim      TYPE char2 VALUE '|~'.
