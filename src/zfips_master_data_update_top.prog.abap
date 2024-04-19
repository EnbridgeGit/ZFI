*&---------------------------------------------------------------------*
*&  Include           ZFIPS_MASTER_DATA_UPDATE_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFIPS_MASTER_DATA_UPDATE                      *
*& Author             :  KMB                                           *
*& Creation Date      :  19/11/2020                                    *
*& Object ID          :  CHG0203062                                    *
*& Application Area   :  FICO                                          *
*& Description        :  Optimization of PS master data update         *
*&-------------------------------------------------------------------- *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 19-03-2020   KMB         D30K930800  CHG020306  Initial              *
************************************************************************

*Type declarations
  TYPES: BEGIN OF gty_record_c55,
          pspid(024),
          posid(024),
          post1(040),
          usr00(020),
        END OF gty_record_c55,

        BEGIN OF gty_record_sf,
          pspid(024),
          post1(040),
          plfaz(010),
          plsez(010),
          eprog(010),
        END OF gty_record_sf,

        BEGIN OF gty_record_est,
           pspid(024),
           posid(024),
           post1(040),
           usr08(010),
           usr09(010),
        END OF gty_record_est,


  gty_truxs_t_text_data(4096) TYPE c OCCURS 0.

*Internal tables and work area declarations
  DATA : gt_record_c55 TYPE TABLE OF gty_record_c55,
         gs_record_c55 TYPE gty_record_c55,
         gt_record_sf TYPE TABLE OF gty_record_sf,
         gs_record_sf TYPE gty_record_sf,
         gt_record_est TYPE TABLE OF gty_record_est,
         gs_record_est TYPE gty_record_est,
         gt_msgtab TYPE TABLE OF bdcmsgcoll, "Internal Table for messages during the BDC
         gs_msgtab TYPE bdcmsgcoll,
         gt_bdcdata TYPE TABLE OF bdcdata, "Internal table for BDC transaction data
         gv_len TYPE i,
         gv_str_msg(255)    TYPE  c,
         gs_bdcdata TYPE bdcdata,
         gt_bapiret2 TYPE STANDARD TABLE OF bapiret2,
         gt_raw TYPE gty_truxs_t_text_data.

*Global variable
  DATA: gv_dismode TYPE ctu_params-dismode.

*Constants
  DATA: gc_e TYPE bdc_mart VALUE 'E',
        gc_s TYPE bdc_mart VALUE 'S',
        gc_x TYPE c VALUE 'X',
        gc_cj07 TYPE c LENGTH 4 VALUE 'CJ07',
        gc_cj02 TYPE c LENGTH 4 VALUE 'CJ02'.
