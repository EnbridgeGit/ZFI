*&---------------------------------------------------------------------*
*&  Include           ZFI_AP_ERROR_WI_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_AP_ERROR_WI_TOP                           *
*& Author             :  Kavya M B                                     *
*& Creation Date      :  14/11/2019                                    *
*& Object ID          :  CHG0153857                                    *
*& Application Area   :  FICO                                          *
*& Description        :  AP CI-SAP - automate job to locate invoice    *
*                        documents in workflow error status            *
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 14-11-2019   KMB         DECK920334  AP CI-SAP automate job to locate*
*                                      invoice documents in workflow   *
*                                      error status                    *
************************************************************************

"Type declaration
TYPES:  BEGIN OF gty_nonpo,
          wi_id       TYPE sww_contob-wi_id,
          startd      TYPE swwwihead-wi_cd,
          startt      TYPE swwwihead-wi_ct,
          year        TYPE vbkpf-gjahr,
          docnum      TYPE vbkpf-belnr,
          cocode      TYPE vbkpf-ausbk,
          vendor      TYPE lifnr,
          amount      TYPE dmbtr,
          wf_init     TYPE swp_initia,
          rco         TYPE swp_initia,
          approver    TYPE swp_initia,
          agent       TYPE swwwihead-wi_aagent,
          agtype      TYPE string,
          status      TYPE swwwihead-wi_stat,
          wi_cd       TYPE swwwihead-wi_cd,
          wi_ct       TYPE swwwihead-wi_ct,
        END OF gty_nonpo,

        BEGIN OF gty_wrbtr,
          ausbk TYPE vbsegs-ausbk,
          bukrs TYPE vbsegs-bukrs,
          belnr TYPE vbsegs-belnr,
          gjahr TYPE vbsegs-gjahr,
          wrbtr TYPE wrbtr_d,
        END OF gty_wrbtr,

        BEGIN OF gty_wf,
         wi_id TYPE swwwihead-wi_id,
         wi_cd TYPE swwwihead-wi_cd,
         wi_ct TYPE swwwihead-wi_ct,
         wi_rh_task TYPE swwwihead-wi_rh_task,
        END OF gty_wf,

        BEGIN OF gty_last,
         wi_id      TYPE swwwihead-wi_id,
         wi_stat    TYPE swwwihead-wi_stat,
         wi_cd      TYPE swwwihead-wi_cd,
         wi_ct      TYPE swwwihead-wi_ct,
         wi_aagent  TYPE swwwihead-wi_aagent,
         wi_rh_task TYPE swwwihead-wi_rh_task,
         top_wi_id  TYPE swwwihead-top_wi_id,
        END OF gty_last,

        BEGIN OF gty_vbkpf,
          ausbk TYPE vbkpf-ausbk,
          bukrs TYPE vbkpf-bukrs,
          belnr TYPE vbkpf-belnr,
          gjahr TYPE vbkpf-gjahr,
          bstat TYPE vbkpf-bstat,
        END OF gty_vbkpf,

        BEGIN OF gty_vbsegk,
          ausbk TYPE vbsegk-ausbk,
          bukrs TYPE vbsegk-bukrs,
          belnr TYPE vbsegk-belnr,
          gjahr TYPE vbsegk-gjahr,
          lifnr TYPE vbsegk-lifnr,
        END OF gty_vbsegk,

        BEGIN OF gty_bukrs,
          bukrs TYPE t001-bukrs,
        END OF gty_bukrs.

"Variable declaration
DATA : gv_string          TYPE string,
       gv_string1         TYPE string,
       gv_amount          TYPE string,
       gv_count           TYPE n,
       gv_badpattern      TYPE n,
       gv_attachment_name TYPE sood-objdes,
       gv_string_n1       TYPE so_obj_des,
       gv_email           TYPE ad_smtpadr,
       gv_string_n        TYPE string,
       gv_size            TYPE so_obj_len,
       gv_sent_to_all     TYPE os_boolean,
       gv_index           TYPE sy-tabix,
       gv_s_date          TYPE swwwihead-wi_cd,
       gv_s_coco          TYPE bseg-bukrs,
       gv_return          TYPE integer.

"Constants declaration
CONSTANTS: gc_crlf TYPE c VALUE cl_bcs_convert=>gc_crlf,
           gc_tab  TYPE c VALUE cl_bcs_convert=>gc_tab,
           gc_raw  TYPE c LENGTH 3 VALUE 'RAW',
           gc_i    TYPE c VALUE 'I',
           gc_h    TYPE c VALUE '#',
           gc_w    TYPE c VALUE 'W',
           gc_e    TYPE c VALUE 'E'.

"Field symbols
FIELD-SYMBOLS : <gfs_workflow> TYPE gty_nonpo.

"Internal table Work area declaration
DATA:   gs_workflow       TYPE gty_nonpo,
        gt_workflow       TYPE TABLE OF gty_nonpo,
        gs_container      TYPE swr_cont,
        gt_container      LIKE TABLE OF gs_container,
        gt_binary_content TYPE solix_tab,
        gs_vbkpf          TYPE gty_vbkpf,
        gt_vbkpf          TYPE TABLE OF gty_vbkpf,
        gs_vbsegk         TYPE gty_vbsegk,
        gt_vbsegk         TYPE TABLE OF gty_vbsegk,
        gcl_send_request  TYPE REF TO cl_bcs,
        gcl_document      TYPE REF TO cl_document_bcs,
        gif_recipient     TYPE REF TO if_recipient_bcs,
        gex_bcs_exception TYPE REF TO cx_bcs,
        gt_main_text      TYPE bcsy_text,
        gt_bukrs          TYPE TABLE OF gty_bukrs,
        gs_swwwihead      TYPE gty_wf,
        gs_lastitem       TYPE gty_last,
        gt_lastitem       TYPE TABLE OF gty_last,
        gt_swwwihead      TYPE TABLE OF gty_wf,
        gs_recipient      TYPE swragent,
        gt_recipient      LIKE TABLE OF gs_recipient,
        gt_wrbtr          TYPE STANDARD TABLE OF gty_wrbtr,
        gs_wrbtr          TYPE gty_wrbtr.
