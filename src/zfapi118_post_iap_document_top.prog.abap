*&---------------------------------------------------------------------*
*&  Include           ZFAPI118_POST_IAP_DOCUMENT_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI118_POST_IAP_DOCUMENT                        *
*  Include:          ZFAPI118_POST_IAP_DOCUMENT_TOP                    *
*  Author:           Vijay Rajaputra ( Copy of US System )             *
*  Date:             Aug, 01 2018                                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      Post IAP Documents for PO and Non-PO Invoices     *
*                    and Credit Memos                                  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 08/01/18 VRAJAPUTRA D30K928896 - CHG0109670 - Initial program        *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************

*eject
TABLES: adr6.

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  gty_dir_list     TYPE epsfili,           "Directory List       "

        gtt_dir_list     TYPE STANDARD TABLE OF gty_dir_list.

TYPES:  gty_out_data     TYPE string,            "Out Data             "

        gtt_out_data     TYPE STANDARD TABLE OF gty_out_data.

TYPES:  gty_log_data     TYPE string,            "Log Data             "

        gtt_log_data     TYPE STANDARD TABLE OF gty_log_data.

TYPES: BEGIN OF gty_file_list,                   "File List            "
        file_nbr         TYPE numc3,             "Number-File          "
        fsprs            TYPE xflag,             "File Server - Presentn
        fsapl            TYPE xflag,             "File Server - Applictn
        fp_in_main       TYPE text128,           "Filepath In Main     "
        fn_in_main       TYPE text255,           "Filename In Main     "
        fp_in_arch       TYPE text128,           "Filepath In Archive  "
        fn_in_arch       TYPE text128,           "Filename In Archive  "
        fp_out_temp      TYPE text128,           "Filepath Out Temp    "
        fn_out_temp      TYPE text128,           "Filename Out Temp    "
        fp_out_arch      TYPE text128,           "Filepath Out Archive "
        fn_out_arch      TYPE text128,           "Filename Out Archive "
        fp_out_main      TYPE text128,           "Filepath Out Main    "
        fn_out_main      TYPE text255,           "Filename Out Main    "
        fp_log_main      TYPE text128,           "Filepath Log Main    "
        fn_log_main      TYPE text255,           "Filename Log Main    "
        cn_recs          TYPE numc10,            "Count of the Records "
       END   OF gty_file_list,

        gtt_file_list    TYPE STANDARD TABLE OF gty_file_list.

*eject
TYPES:  gty_doc_data     TYPE zaps_iap_doc,      "IAP Document Data    "

        gtt_doc_data     TYPE STANDARD TABLE OF gty_doc_data.

TYPES:  gty_doc_hdr      TYPE zaps_iap_doc_hdr,  "IAP Document Header  "

        gtt_doc_hdr      TYPE STANDARD TABLE OF gty_doc_hdr.

TYPES:  gty_doc_item     TYPE zaps_iap_doc_item, "IAP Document Item    "

        gtt_doc_item     TYPE STANDARD TABLE OF gty_doc_item.

TYPES:  gty_doc_actg     TYPE zaps_iap_doc_actg, "IAP Document Accountng

        gtt_doc_actg     TYPE STANDARD TABLE OF gty_doc_actg.

TYPES:  gty_iap_xref_ad  TYPE zapt_iap_xref_ad.  "Log Data             "

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_fpth1a        TYPE text128            "Filepath - Inbound   "
                         VALUE
           '\\SAPFileShare.gtna.gt.ds\FI\DEV\In\I_P2C_AP_118\',
        gc_fnam1a        TYPE text128            "Filename - Inbound   "
                         VALUE '*',
        gc_fregxa        TYPE text128            "File REGEX - Inbound "
                         VALUE '^SAPUS_INVOICE_\d{8}\_\d{6}\.CSV$',
        gc_farc1a        TYPE text128            "Filepath - In Archive"
                         VALUE
           '\\SAPFileShare.gtna.gt.ds\FI\DEV\In\I_P2C_AP_118\Arch\',
        gc_fpth2a        TYPE text128            "Filepath - Outbound  "
                         VALUE
           '\\SAPFileShare.gtna.gt.ds\FI\DEV\Out\I_P2C_AP_118\',
        gc_fnam2a        TYPE text128            "Filename - Outbound  "
                         VALUE
           'ERPID_INVOICE_MSGS_YYYYMMDD_HHMMSS.CSV',
        gc_farc2a        TYPE text128            "Filepath - Out Archive
                         VALUE
           '\\SAPFileShare.gtna.gt.ds\FI\DEV\Out\I_P2C_AP_118\Arch\',
        gc_fpth3a        TYPE text128            "Filepath - Log       "
                         VALUE
           '\\SAPFileShare.gtna.gt.ds\FI\DEV\In\I_P2C_AP_118\Log\',
        gc_fnam3a        TYPE text128            "Filename - Log       "
                         VALUE
           'ERPID_INVOICE_LOG_YYYYMMDD_HHMMSS.TXT',
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP',
        gc_modif_id_fas  TYPE char3              "ModifID-File Appl.Srvr
                         VALUE 'FAS',
        gc_modif_id_fps  TYPE char3              "ModifID-File Pres.Srvr
                         VALUE 'FPS',
        gc_slash         TYPE char1              "Slash                "
                         VALUE '\',
        gc_delim         TYPE char2              "Delimiter            "
                         VALUE '|~'.

*eject
************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_fl_errr_prcs  TYPE xflag,             "Error - Process      "
        gv_fl_errr_mstr  TYPE xflag,             "Error - Master       "
        gv_cnt_doc_trns  TYPE numc06,            "Count - Transactions "
        gv_cnt_doc_errr  TYPE numc06,            "Count - Docs In Error"
        gv_cnt_doc_wrng  TYPE numc06,            "Count - Docs W/ Warnin
        gv_cnt_doc_pass  TYPE numc06,            "Count - Docs Passed  "
        gv_filename_in   TYPE text255,           "Filename - Inbound   "
        gv_filename_inx  TYPE xflag,             "Filename - Inbound   "
        gv_filename_out  TYPE text255,           "Filename - Outbound  "
        gv_filename_otx  TYPE xflag,             "Filename - Outbound  "
        gv_filename_log  TYPE text255,           "Filename - Log       "
        gv_filename_lgx  TYPE xflag.             "Filename - Log       "

************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gs_out_data      TYPE gty_out_data,      "Out Data             "
        gs_log_data      TYPE gty_log_data,      "Log Data             "
        gs_doc_data      TYPE gty_doc_data,      "IAP Document Data    "
        gs_doc_hdr       TYPE gty_doc_hdr,       "IAP Document Header  "
        gs_doc_item      TYPE gty_doc_item,      "IAP Document Item    "
        gs_doc_actg      TYPE gty_doc_actg.      "IAP Document Accountng

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_file_list     TYPE gtt_file_list,     "File List            "
        gt_out_data      TYPE gtt_out_data,      "Out Data             "
        gt_log_data      TYPE gtt_log_data,      "Log Data             "
        gt_doc_data      TYPE gtt_doc_data,      "IAP Document Data    "
        gt_doc_hdr       TYPE gtt_doc_hdr,       "IAP Document Header  "
        gt_doc_item      TYPE gtt_doc_item,      "IAP Document Item    "
        gt_doc_actg      TYPE gtt_doc_actg.      "IAP Document Accountng

*eject
************************************************************************
*                                Macros                                *
************************************************************************
DEFINE macro_msg.                                "Macro - Message      "
  IF         ( &1   EQ 'B' ).
    SKIP        1.
    WRITE:   / &3.
    IF       ( &2   EQ 'S' ).
      MESSAGE  &3 TYPE 'S'.
    ELSE.
      MESSAGE  &3 TYPE 'I'.
    ENDIF.
  ELSEIF     ( &1   EQ 'W' ).
    SKIP        1.
    WRITE:   / &3.
  ELSEIF     ( &1   EQ 'M' ).
    IF       ( &2   EQ 'S' ).
      MESSAGE  &3 TYPE 'S'.
    ELSE.
      MESSAGE  &3 TYPE 'I'.
    ENDIF.
  ENDIF.
  IF         ( &2   EQ 'A' ).
    gv_fl_errr_prcs  = 'X'.
    gv_fl_errr_mstr  = 'X'.
  ENDIF.
END-OF-DEFINITION.

DEFINE macro_log.                                "Macro - Write Log    "
  IF       ( rb_fsprs          IS NOT INITIAL ).
    CLEAR                         gs_log_data.
    IF     ( &1                GT 0 ).
      DO     &1 TIMES.
        APPEND   gs_log_data   TO gt_log_data.
      ENDDO.
    ENDIF.
    MOVE     &2                TO gs_log_data.
    APPEND   gs_log_data       TO gt_log_data.
  ELSEIF   ( gv_filename_lgx   IS NOT INITIAL ).
    IF     ( &1                GT 0 ).
      DO     &1 TIMES.
        TRANSFER space         TO gv_filename_log.
      ENDDO.
    ENDIF.
    TRANSFER &2                TO gv_filename_log.
  ENDIF.
  IF       ( &1                GT 0 ).
    SKIP     &1.
  ENDIF.
  WRITE:   / &2.
END-OF-DEFINITION.
