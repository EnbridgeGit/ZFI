*&---------------------------------------------------------------------*
*&  Include           ZFAPI017_CIMPL_INV_BDC_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI130_CIMPL_INV_BDC                            *
*  Author:           Shamsiya Shaffe                                   *
*  Date:             August 29, 2019                                   *
*  Track #:                                                            *
*  Application Area: FICO                                              *
*                                                                      *
*  Description:      Cimpl Interface Invoice Inbound BDC               *
*                                                                      *
*                    Read a file in the RFBIBL00 format, translate     *
*                    the records into BDC data, and post the Invoice   *
*                    transactions.                                     *
*                                                                      *
*                    Copy form US Report - ZFAPI017_DATACERT_INV_BDC   *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
*08/29/2019 D30K930117 SHAFFES CHG0153812 - Initial program development*
*10/11/2019 D30K930204 SHAFFES CHG0153812 - Initial program development*
*15/10/2020 D30K930715 AKMADASU CHG0191529- Team connect Interface tax *
*           D30K930718                      code changes               *
*----------------------------------------------------------------------*
************************************************************************

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  ty_wa_xparam     TYPE zfit_xparam.       "Parameter Master

TYPES:  ty_it_xparam     TYPE STANDARD TABLE OF ty_wa_xparam.

TYPES: BEGIN OF ty_wa_ktokk_ot,                  "One-Time Ven Accnt Grp
        ktokk            TYPE ktokk,             "Vendor Account Group
       END   OF ty_wa_ktokk_ot.

TYPES:  ty_it_ktokk_ot   TYPE STANDARD TABLE OF ty_wa_ktokk_ot.

TYPES: BEGIN OF ty_wa_file_list,                 "File List
        filename_in      TYPE text1024,          "Input Filename
        filename_fn      TYPE text128,           "Filename w/o Path
       END   OF ty_wa_file_list.

TYPES:  ty_it_file_list  TYPE STANDARD TABLE OF ty_wa_file_list.

*eject
TYPES: BEGIN OF ty_wa_file_stats,                "File Stats
        nbr_file         TYPE numc5,             "File Sequence Number
        filename_in      TYPE text1024,          "Input Filename
        filename_fn      TYPE text128,           "Filename w/o Path
        cnt_rec_in       TYPE syindex,           "Input Record Count
        fl_err_p         TYPE flag,              "Error Flag-Process Err
        fl_err_d         TYPE flag,              "Error Flag-Data Err
        cnt_err          TYPE syindex,           "Error Count
       END   OF ty_wa_file_stats.

TYPES:  ty_it_file_stats TYPE STANDARD TABLE OF ty_wa_file_stats.

TYPES:  ty_wa_rfbibl00(3000)                     "Inv RFBIBL00 Format
                         TYPE c.

TYPES:  ty_it_rfbibl00   TYPE STANDARD TABLE OF ty_wa_rfbibl00.

TYPES: BEGIN OF ty_wa_msgs,                      "Messages
        nbr_file         TYPE numc5,             "File Sequence Number
        nbr_btc          TYPE numc5,             "Batch Sequence Number
*        nbr_doc          TYPE numc5,             "Doc Sequence Number " Added BY AKMADASU FOR CHG0191529
        nbr_doc          TYPE char10,             "Doc Sequence Number " Added BY AKMADASU FOR CHG0191529
        rc               TYPE numc5,             "Return Code
        msgid            TYPE symsgid,           "Message Type
        msgty            TYPE symsgty,           "Message ID
        msgno            TYPE symsgno,           "Message Number
        text             TYPE text240,           "Text
       END   OF ty_wa_msgs.

TYPES:  ty_it_msgs       TYPE STANDARD TABLE OF ty_wa_msgs.

TYPES: BEGIN OF ty_wa_bdccntl.                   "Batch Data Control
INCLUDE                  TYPE ctu_params.        "Params For Call Trans
TYPES:  sesn_group       TYPE apq_grpn,          "Group Name
        sesn_start       TYPE apq_stda,          "Queue Start Date
        sesn_xkeep       TYPE xkeep_bi,          "Keep Batch Session
        sesn_usnam       TYPE apq_mapn,          "Queue User Id
        sesn_create      TYPE char1,             "Create Batch Session
        sesn_open        TYPE char1,             "Session Open
       END   OF ty_wa_bdccntl.                   "Batch Data Control

TYPES:  ty_wa_bdcdata    TYPE bdcdata.           "Batch Input

TYPES:  ty_it_bdcdata    TYPE STANDARD TABLE OF ty_wa_bdcdata.

TYPES:  ty_wa_bdcmsg     TYPE bdcmsgcoll.        "Batch Message

TYPES:  ty_it_bdcmsg     TYPE STANDARD TABLE OF ty_wa_bdcmsg.

TYPES:  ty_wa_bgr00      TYPE bgr00.             "Batch Input Structure

TYPES:  ty_wa_bbkpf      TYPE bbkpf.             "Accounting Doc Header

TYPES:  ty_wa_bbseg      TYPE bbseg.             "Accounting Doc Segment

TYPES:  ty_wa_list_text  TYPE solisti1.          "Single List Text

TYPES:  ty_it_list_text  TYPE STANDARD TABLE OF ty_wa_list_text.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_c             TYPE char1              "C / Character
                         VALUE 'C',
        gc_e             TYPE char1              "E / Error
                         VALUE 'E',
        gc_n             TYPE char1              "N / No-disply / Numerc
                         VALUE 'N',
        gc_x             TYPE char1              "X / Yes / True
                         VALUE 'X',
        gc_y             TYPE char1              "Y / Yes
                         VALUE 'Y',
        gc_fs            TYPE char1              "Forward Slash
                         VALUE '/',
        gc_na            TYPE char8              "Not Applicable
                         VALUE 'Not/Appl',
        gc_sesn_group    TYPE apq_grpn           "Batch Session Grp Name
                         VALUE 'ZAPLEGAL',       "Changed from 'ZAPCIMPL' to 'ZAPLEGAL'
*                                                 by AHMADT for CHG0175529
        gc_pattern_fn_in TYPE string VALUE       "Pattern-Filename In
                         '^SAPUGL_\d{8}_\d{6}\_RFBIBL00$',
        gc_extension_arch                        "File Extension-Archive
                         TYPE char4
                         VALUE '.ARC',
        gc_extension_err TYPE char4              "File Extension-Error
                         VALUE '.ERR',
        gc_msgid         TYPE bdc_mid            "Message ID
                         VALUE 'ZFI01',
        gc_msgnr         TYPE bdc_mnr            "Message Number
                         VALUE '000'.

CONSTANTS:
        gc_param_in_int  TYPE zparamkey          "Inbound Interface
                         VALUE 'INBOUND_INTERFACE',
        gc_param_obj_id  TYPE zparamkey          "Object ID
                         VALUE 'I_P2C_AP_017',   "Changed to *017 from *130 by AHMADT for CHG0175529
        gc_param_post    TYPE zparamkey          "Post
                         VALUE 'POST',
        gc_filepath_in   TYPE zparamkey          "Input Filepath
                         VALUE 'INPUT_FILEPATH',
        gc_filepath_arch TYPE zparamkey          "Archive Filepath
                         VALUE 'ARCHIVE_FILEPATH',
        gc_filepath_err  TYPE zparamkey          "Error Filepath
                         VALUE 'ERROR_FILEPATH',
        gc_bdc_control   TYPE zparamkey          "Batch Data Control
                         VALUE 'BDC_CONTROL',
        gc_email         TYPE zparamkey          "Email
                         VALUE 'EMAIL',
        gc_email_id      TYPE zparamkey          "Email ID
                         VALUE 'EMAIL_ID',
        gc_email_std_txt TYPE zparamkey          "Email Standard Text
                         VALUE 'STANDARD_TEXT'.

*eject
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only
                         VALUE 'DSP',
        gc_modif_id_dlm  TYPE char3              "ModifID-Input Delimite
                         VALUE 'DLM',
        gc_modif_id_fpt  TYPE char3              "ModifID-Input Filepath
                         VALUE 'FPT',
        gc_modif_id_fnm  TYPE char3              "ModifID-Input Filename
                         VALUE 'FNM'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_obj_id        TYPE char14,            "Object ID
        gv_flag_pres     TYPE flag.              "Flag-Presentatn Server

DATA:   gv_nbr_file      TYPE numc5,             "File Sequence Number
        gv_count_err_data                        "Count-Errors-Data
                         TYPE numc5,
        gv_count_err_proc                        "Count-Errors-Process
                         TYPE numc5,
        gv_flag_err_data TYPE flag,              "Flag-Errors-Data
        gv_flag_err_proc TYPE flag,              "Flag-Errors-Process
        gv_flag_err_mstr TYPE flag,              "Flag-Errors-Master
        gv_flag_err_vldt TYPE flag,              "Flag-Errors-Validation
        gv_xblnr         TYPE XBLNR," Added BY AKMADASU FOR CHG0191529
        gv_bukrs         TYPE bukrs." Added BY AKMADASU FOR CHG0191529

DATA:   gv_sysid         TYPE sysysid,           "Name Of The SAP System
        gv_uname         TYPE syuname,           "User Name
        gv_pagno         TYPE sypagno,           "Current List Page
        gv_cprog         TYPE sycprog,           "Calling Program
        gv_datum         TYPE sydatum,           "Current Date of Applic
        gv_uzeit         TYPE syuzeit.           "Current Time of Applic

************************************************************************
*                                Ranges                                *
************************************************************************
DATA:   gr_ktokk_ot      TYPE RANGE OF           "Ven Accnt Grp-One Time
                         t077k-ktokk.

*eject
************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gwa_ktokk_ot     LIKE LINE OF            "Ven Accnt Grp-One Time
                         gr_ktokk_ot.

DATA:   gwa_file_list    TYPE ty_wa_file_list.   "File List

DATA:   gwa_file_stats   TYPE ty_wa_file_stats.  "File Stats

DATA:   gwa_bgr00        TYPE ty_wa_bgr00.       "Batch Input Structure

DATA:   gwa_bdccntl      TYPE ty_wa_bdccntl.     "Batch Data Control

DATA:   gwa_bdcdata      TYPE ty_wa_bdcdata.     "Batch Input

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   git_xparam       TYPE ty_it_xparam.      "Parameter Master

DATA:   git_ktokk_ot     TYPE ty_it_ktokk_ot.    "One-Time Ven Accnt Grp

DATA:   git_file_list    TYPE ty_it_file_list.   "File List

DATA:   git_file_stats   TYPE ty_it_file_stats.  "File Stats

DATA:   git_rfbibl00     TYPE ty_it_rfbibl00.    "Inv RFBIBL00 Format

DATA:   git_docs_posted  TYPE ty_it_msgs.        "Documents Posted

DATA:   git_errs_data    TYPE ty_it_msgs.        "Errors-Data

DATA:   git_errs_proc    TYPE ty_it_msgs.        "Errors-Process

DATA:   git_bdcdata      TYPE ty_it_bdcdata.     "Batch Input

DATA:   git_bdcmsg       TYPE ty_it_bdcmsg.      "Batch Message


TYPES : BEGIN OF ty_bkpf_dup,
         bukrs     TYPE bkpf-bukrs,
         belnr     TYPE bkpf-belnr,
         gjahr     TYPE bkpf-gjahr,
        END OF ty_bkpf_dup.

TYPES : BEGIN OF ty_bseg_dup,
         bukrs     TYPE bseg-bukrs,
         belnr     TYPE bseg-belnr,
         gjahr     TYPE bseg-gjahr,
         buzei     TYPE bseg-buzei,
        END OF ty_bseg_dup.

TYPES : BEGIN OF ty_dup_entries,
         xblnr     TYPE bkpf-xblnr,
         wrbtr     TYPE bseg-wrbtr,
        END OF ty_dup_entries.

TYPES : BEGIN OF ty_vbsegk_dup,
         ausbk     TYPE vbsegk-ausbk,
         belnr     TYPE vbsegk-belnr,
         gjahr     TYPE vbsegk-gjahr,
         bzkey     TYPE vbsegk-bzkey,
         bukrs     TYPE vbsegk-bukrs,
        END OF ty_vbsegk_dup.

* Data definition
DATA:   gwa_bdccntl_dup     TYPE ty_wa_bdccntl,     "Batch Data Control for Duplication
        lit_rfbibl00_dup    TYPE ty_it_rfbibl00,
        lwa_rfbibl00_dup    TYPE ty_wa_rfbibl00,
        lc_vendor_wrbtr     TYPE bseg-wrbtr,
        lc_reference_xblnr  TYPE bkpf-xblnr,
        lit_bkpf_dup        TYPE STANDARD TABLE OF ty_bkpf_dup,
        lit_bseg_dup        TYPE STANDARD TABLE OF ty_bseg_dup,
        lit_dup_entries     TYPE STANDARD TABLE OF ty_dup_entries,
        lwa_dup_entries     TYPE ty_dup_entries,
        lit_vbsegk_dup      TYPE STANDARD TABLE OF ty_vbsegk_dup,
        lwa_vbsegk_dup      TYPE ty_vbsegk_dup,
        lc_dup_flag(1)      TYPE c,
        lwa_error_dup       TYPE bdcmsgcoll,        "Message in case of duplication found.
        lwa_errs_data_dup   TYPE ty_wa_msgs,
        gv_company_code     TYPE bkpf-bukrs,
        gv_vendor_tax_chk   TYPE lfbw-lifnr,
        gwa_lfbw_chk        TYPE lfbw.

CONSTANTS : gc_sesn_group_dup    TYPE apq_grpn           "Batch Session Grp Name for duplication of Posting
                                 VALUE 'ZAPLEGALDUP'.    "Changed from 'ZAPCIMPLDUP' to 'ZAPLEGALDUP'
*                                                         by AHMADT for CHG0175529
*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************

* Run Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_objid  TYPE zparamkey        "Object ID            "
                           VISIBLE LENGTH 12
                           OBLIGATORY
                           MODIF ID dsp.
SELECTION-SCREEN: COMMENT 50(45) p_objdsc.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_email  TYPE so_recname.      "Email ID             "
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       cb_test  TYPE c                "Test Mode            "
                           AS CHECKBOX
                           DEFAULT gc_x
                           USER-COMMAND cmd.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       rb_appl  RADIOBUTTON GROUP rbg1 "Application Server  "
                           DEFAULT 'X'
                           USER-COMMAND cmd.
PARAMETERS:       rb_pres  RADIOBUTTON GROUP rbg1."Presentation Server "

* Input File
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_delim  TYPE zparamkey        "File Delimiter-Input "
                           AS LISTBOX VISIBLE LENGTH 12
                           DEFAULT gc_na
                           MODIF ID dlm.
PARAMETERS:       p_fpath  TYPE localfile        "Filepath-Input       "
                           MODIF ID fpt.
PARAMETERS:       p_fname  TYPE localfile        "Filename-Input       "
                           MODIF ID fnm.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb2.

SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb1.
