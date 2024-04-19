*&---------------------------------------------------------------------*
*&  Include           ZFAPI130_CIMPL_INV_FRMT_TOP
*&---------------------------------------------------------------------*
************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI130_CIMPL_INV_FRMT                           *
*  Author:           Shamsiya Shaffe                                   *
*  Date:             August 29, 2019                                   *
*  Track #:                                                            *
*  Application Area: FICO                                              *
*                                                                      *
*  Description:      Cimpl Interface Invoice Inbound Format            *
*                                                                      *
*                    Create a file in the RFBIBL00 format using        *
*                    data from the inbound interface file              *
*                                                                      *
*                    Copy of US Report ZFAPI017_DATACERT_INV_FRMT with *
*                    change in document type to CM                     *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
*08/29/2019 D30K930117 SHAFFES CHG0153812 - Initial program development*
*10/11/2019 D30K930204 SHAFFES CHG0153812 - Initial program development*
*----------------------------------------------------------------------*
************************************************************************

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  ty_wa_xparam     TYPE zfit_xparam.       "Parameter Master

TYPES:  ty_it_xparam     TYPE STANDARD TABLE OF ty_wa_xparam.

TYPES:  ty_wa_dd03l      TYPE dd03l.             "Table Fields

TYPES:  ty_it_dd03l      TYPE STANDARD TABLE OF ty_wa_dd03l.

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
        filename_out     TYPE text1024,          "Output Filename
        cnt_rec_out      TYPE syindex,           "Output Record Count
        cnt_rec_out_t0   TYPE syindex,           "Output Rec Count BGR00
        cnt_rec_out_t1   TYPE syindex,           "Output Rec Count BBKPF
        cnt_rec_out_t2   TYPE syindex,           "Output Rec Count BBSEG
        fl_err_p         TYPE flag,              "Error Flag-Process Err
        fl_err_d         TYPE flag,              "Error Flag-Data Err
        cnt_err          TYPE syindex,           "Error Count
       END   OF ty_wa_file_stats.

TYPES:  ty_it_file_stats TYPE STANDARD TABLE OF ty_wa_file_stats.

TYPES:  ty_it_data_tab   TYPE STANDARD TABLE OF string. "Data Table

TYPES: BEGIN OF ty_wa_inv_item,                  "Invoice Item
        bldat            TYPE char10,            "Document Date
        bukrs            TYPE bukrs,             "Company Code
        waers            TYPE waers,             "Currency Key
        xblnr            TYPE xblnr1,            "Reference Doc.Number
        bktxt            TYPE bktxt,             "Document Header Text
        bschl            TYPE bschl,             "Posting Key
        lifnr            TYPE lifnr,             "Account No.Of Vendor
        newbk            TYPE bukrs,             "Business Area
        wrbtr            TYPE char16,            "Amount In Doc.Currency
        sgtxt            TYPE sgtxt,             "Item Text
       " zlsch            TYPE schzw_bseg,        "Payment Method  "Add by RBHATT 04-APR-2012 TR DECK905448
        saknr            TYPE saknr,             "G/L Account Number
        kostl            TYPE kostl,             "Cost Center
     "   zzref            TYPE zzref,             "CF Attribute    "Add by RBHATT 04-APR-2012 TR DECK905448
        aufnr            TYPE aufnr,             "Order Number
        nplnr            TYPE nplnr,             "Network Number
        vornr            TYPE vornr,             "Operation/Activity Num
        projk            TYPE ps_posid,          "WBS Number (External
        zuonr            TYPE dzuonr,            "Assignment Number
        name1            TYPE name1_gp,          "Name
        stras            TYPE stras_gp,          "House Number & Street
        ort01            TYPE ort01_gp,          "City
        pstlz            TYPE pstlz,             "Postal Code
        xref3            TYPE xref3,             "Line Ref Key-Source Cd
        mwskz            TYPE mwskz,             "Sales Tax Code
        zzloc            TYPE zzloc,             "Location
        zlsch            TYPE schzw_bseg,        "Payment Method  "Add by SKAPSE 05-MAy-2012 TR DECK905448
        zzref            TYPE zzref,             "CF Attribute    "Add by SKAPSE 05-MAy-2012 TR DECK905448
       END   OF ty_wa_inv_item.

TYPES:  ty_it_inv_item   TYPE STANDARD TABLE OF ty_wa_inv_item.

TYPES:  ty_wa_rfbibl00(3000)                     "Inv RFBIBL00 Format
                         TYPE c.

TYPES:  ty_it_rfbibl00   TYPE STANDARD TABLE OF ty_wa_rfbibl00.

TYPES:  ty_wa_list_text  TYPE solisti1.          "Single List Text

TYPES:  ty_it_list_text  TYPE STANDARD TABLE OF ty_wa_list_text.

*eject
TYPES: BEGIN OF ty_wa_errs,                      "Errors
        nbr_file         TYPE numc5,             "File Sequence Number
        nbr_btc          TYPE numc5,             "Batch Sequence Number
        nbr_doc          TYPE numc5,             "Doc Sequence Number
        rc               TYPE numc5,             "Return Code
        msgid            TYPE symsgid,           "Message Type
        msgty            TYPE symsgty,           "Message ID
        msgno            TYPE symsgno,           "Message Number
        text             TYPE text240,           "Text
       END   OF ty_wa_errs.

TYPES:  ty_it_errs       TYPE STANDARD TABLE OF ty_wa_errs.

TYPES:  ty_wa_bgr00      TYPE bgr00.             "Batch Input Structure

TYPES:  ty_wa_bbkpf      TYPE bbkpf.             "Accounting Doc Header

TYPES:  ty_wa_bbseg      TYPE bbseg.             "Accounting Doc Segment

TYPES: BEGIN OF ty_wa_lfa1,                      "Vendor Master
        lifnr            TYPE lifnr,             "Vendor Account Number
        name1            TYPE name1_gp,          "Name 1               "
        name2            TYPE name2_gp,          "Name 2               "
        stras            TYPE stras_gp,          "House Number & Street
        ort01            TYPE ort01_gp,          "City
        pstlz            TYPE pstlz,             "Postal Code
       END   OF ty_wa_lfa1.

TYPES: BEGIN OF ty_wa_lfb1,                      "Vendor Master Company
        lifnr            TYPE lifnr,             "Vendor Account Number
        bukrs            TYPE bukrs,             "Company Code         "
        zterm            TYPE dzterm,            "Payment Terms        "
       END   OF ty_wa_lfb1.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_c             TYPE char1              "C / Character
                         VALUE 'C',
        gc_n             TYPE char1              "N / No-disply / Numerc
                         VALUE 'N',
        gc_x             TYPE char1              "X / Yes / True
                         VALUE 'X',
        gc_tcode         TYPE tcode              "Transaction Code
                         VALUE 'F-43',
        gc_blart         TYPE blart              "Document Type
                         VALUE 'CM',
        gc_mwskz         TYPE mwskz              "Tax Code
                         VALUE 'I0',
        gc_na            TYPE char8              "Not Applicable
                         VALUE 'Not/Appl',
        gc_pipelit       TYPE char4              "Delimiter-Pipe-Literal
                         VALUE 'PIPE',
        gc_pipe          TYPE char1              "Delimiter-Pipe
                         VALUE '|',
        gc_char_nodata   TYPE char1              "No-Data Character
                         VALUE '/',
        gc_lifnr_trisk   TYPE lifnr              "Vendor ID - TRISK
                         VALUE '00000TRISK',
        gc_koart_gl      TYPE koart              "Account Type - G/L
                         VALUE 'S',
        gc_koart_vendor  TYPE koart              "Account Type - Vendor
                         VALUE 'K',
        gc_pattern_fn_in TYPE string             "Pattern-Filename In
                         VALUE '^SAPUGL_\d{8}_\d{6}\.CSV$',
        gc_rfbibl00      TYPE text25             "RFBIBL00             "
                         VALUE '_RFBIBL00',
        gc_extension_arch                        "File Extension-Archive
                         TYPE char4
                         VALUE '.ARC',
        gc_extension_err TYPE char4              "File Extension-Error
                         VALUE '.ERR'.

*eject
CONSTANTS:
        gc_param_in_int  TYPE zparamkey          "Inbound Interface
                         VALUE 'INBOUND_INTERFACE',
        gc_param_obj_id  TYPE zparamkey          "Object ID
                         VALUE 'I_P2C_AP_130',
        gc_param_format  TYPE zparamkey          "Format
                         VALUE 'FORMAT',
        gc_filepath_in   TYPE zparamkey          "Input Filepath
                         VALUE 'INPUT_FILEPATH',
        gc_filepath_out  TYPE zparamkey          "Output Filepath
                         VALUE 'OUTPUT_FILEPATH',
        gc_filepath_arch TYPE zparamkey          "Archive Filepath
                         VALUE 'ARCHIVE_FILEPATH',
        gc_filepath_err  TYPE zparamkey          "Error Filepath
                         VALUE 'ERROR_FILEPATH',
        gc_email         TYPE zparamkey          "Email
                         VALUE 'EMAIL',
        gc_email_id      TYPE zparamkey          "Email ID
                         VALUE 'EMAIL_ID',
        gc_email_std_txt TYPE zparamkey          "Email Standard Text
                         VALUE 'STANDARD_TEXT'.

CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only
                         VALUE 'DSP',
        gc_modif_id_dl1  TYPE char3              "ModifID-Input Delimite
                         VALUE 'DL1',
        gc_modif_id_fp1  TYPE char3              "ModifID-Input Filepath
                         VALUE 'FP1',
        gc_modif_id_fn1  TYPE char3              "ModifID-Input Filename
                         VALUE 'FN1',
        gc_modif_id_dl2  TYPE char3              "ModifID-Output Delimit
                         VALUE 'DL2',
        gc_modif_id_fp2  TYPE char3              "ModifID-Output Filepat
                         VALUE 'FP2',
        gc_modif_id_fn2  TYPE char3              "ModifID-Output Filenam
                         VALUE 'FN2'.

*eject
************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_obj_id        TYPE char14,            "Object ID
        gv_flag_pres1    TYPE flag,              "Flag-PresServer-Input
        gv_flag_pres2    TYPE flag.              "Flag-PresServer-Output

DATA:   gv_nbr_file      TYPE numc5,             "File Sequence Number
        gv_count_err_data                        "Count-Errors-Data
                         TYPE numc5,
        gv_count_err_proc                        "Count-Errors-Process
                         TYPE numc5,
        gv_flag_err_data TYPE flag,              "Flag-Errors-Data
        gv_flag_err_proc TYPE flag,              "Flag-Errors-Process
        gv_flag_err_mstr TYPE flag.              "Flag-Errors-Master

DATA:   gv_sysid         TYPE sysysid,           "Name Of The SAP System
        gv_uname         TYPE syuname,           "User Name
        gv_pagno         TYPE sypagno,           "Current List Page
        gv_cprog         TYPE sycprog,           "Calling Program
        gv_datum         TYPE sydatum,           "Current Date of Applic
        gv_uzeit         TYPE syuzeit.           "Current Time of Applic

************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gwa_file_list    TYPE ty_wa_file_list.   "File List

DATA:   gwa_file_stats   TYPE ty_wa_file_stats.  "File Stats

DATA:   gwa_inv_item     TYPE ty_wa_inv_item.    "invoice Item

DATA:   gwa_lfa1         TYPE ty_wa_lfa1.        "Vendor Master

DATA:   gwa_lfb1         TYPE ty_wa_lfb1.        "Vendor Master

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   git_xparam       TYPE ty_it_xparam.      "Parameter Master

DATA:   git_dd03l        TYPE ty_it_dd03l.       "Table Fields

DATA:   git_file_list    TYPE ty_it_file_list.   "File List

DATA:   git_file_stats   TYPE ty_it_file_stats.  "File Stats

DATA:   git_inv_item     TYPE ty_it_inv_item.    "Invoice Item

DATA:   git_rfbibl00     TYPE ty_it_rfbibl00.    "Inv RFBIBL00 Format

DATA:   git_errs_data    TYPE ty_it_errs.        "Errors-Data

DATA:   git_errs_proc    TYPE ty_it_errs.        "Errors-Process

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
PARAMETERS:       p_tcode  TYPE tcode            "Transaction Code     "
                           DEFAULT gc_tcode
                           OBLIGATORY
                           MODIF ID dsp.
PARAMETERS:       p_blart  TYPE blart            "Document Type        "
                           DEFAULT gc_blart
                           OBLIGATORY
                           MODIF ID dsp.
* Begin changes - delete code   JRHARTUNG  10/10/11  TR0928  DECK902584
*ARAMETERS:       p_mwskz  TYPE mwskz            "Tax Code             "
*                          DEFAULT gc_mwskz
*                          OBLIGATORY.
*ARAMETERS:       p_saknr  TYPE saknr            "G/L Account          "
*                          OBLIGATORY.
* End changes   - delete code   JRHARTUNG  10/10/11  TR0928  DECK902584
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
PARAMETERS:       p_delim1 TYPE zparamkey        "File Delimiter-Input "
                           AS LISTBOX VISIBLE LENGTH 12
                           DEFAULT gc_pipelit
                           MODIF ID dl1.
PARAMETERS:       p_fpath1 TYPE localfile        "Filepath-Input       "
                           MODIF ID fp1.
PARAMETERS:       p_fname1 TYPE localfile        "Filename-Input       "
                           MODIF ID fn1.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb2.

*eject
* Output File
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb3 WITH FRAME TITLE text-sb3.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_delim2 TYPE zparamkey        "File Delimiter-Output"
                           AS LISTBOX VISIBLE LENGTH 12
                           DEFAULT gc_na
                           MODIF ID dl2.
PARAMETERS:       p_fpath2 TYPE localfile        "Filepath-Output      "
                           MODIF ID fp2.
PARAMETERS:       p_fname2 TYPE localfile        "Filename-Output      "
                           MODIF ID fn2.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb3.

SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb1.
