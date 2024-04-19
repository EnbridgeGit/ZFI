*&---------------------------------------------------------------------*
*&  Include           ZFI_MASS_VOID_TOP
*&---------------------------------------------------------------------*
*&-----------------------------------------------------------------------*
*& Report Name          : ZFI_MASS_VOID_TOP                              *
*& Author               : KMB                                            *
*& Creation Date        : 12-Jun-2019                                    *
*& Transport no.        : D30K929933                                     *
*& Object ID            : CHG0147638                                     *
*& Application Area     : FI                                             *
*& Description          : Mass voiding of cheques                        *
*&-----------------------------------------------------------------------*

*Type declarations
  TYPES: BEGIN OF gty_record,
          zbukr_001(004), "data element: DZBUKR
          hbkid_002(005), "data element: HBKID
          hktid_003(005), "data element: HKTID
          chect_004(013), "data element: CHECT
          voidr_005(002), "data element: VOIDR
          stgrd_006(002), "data element: STGRD
          monat_008(002), "data element: MONAT
        END OF gty_record,

        gty_truxs_t_text_data(4096) TYPE c OCCURS 0.

*Internal tables and work area declarations
  DATA : gt_record TYPE TABLE OF gty_record,
         gs_record TYPE gty_record,
         gt_msgtab TYPE TABLE OF bdcmsgcoll, "Internal Table for messages during the BDC
         gs_msgtab TYPE bdcmsgcoll,
         gt_bdcdata TYPE TABLE OF bdcdata, "Internal table for BDC transaction data
         gs_bdcdata TYPE bdcdata,
         gt_bapiret2 TYPE STANDARD TABLE OF bapiret2,
         gt_raw TYPE gty_truxs_t_text_data.

*Variables
  DATA:  gv_str_msg(255) TYPE c,
         gv_len TYPE i,
         gv_dismode TYPE ctu_params-dismode,
         gv_budat_007(010).

*Constants
  DATA: gc_e TYPE bdc_mart VALUE 'E',
        gc_s TYPE bdc_mart VALUE 'S',
        gc_x TYPE c VALUE 'X',
        gc_fch8 TYPE c LENGTH 4 VALUE 'FCH8',
        gc_fch9 TYPE c LENGTH 4 VALUE 'FCH9'.
