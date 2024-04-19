*&---------------------------------------------------------------------*
*&  Include           ZFAPE001_VNDR_INVC_PAYM_MTHD_T
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPE001_VNDR_INVC_PAYM_MTHD                      *
*  Include           ZFAPE001_VNDR_INVC_PAYM_MTHD_T                    *
*  Author:           John Hartung                                      *
*  Date:             June 23,2016                                      *
*  Ticket#:          ACR-1118                                          *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      AP Vendor Invoice Payment Method Update Program   *
*                    Top Include - Data Declarations                   *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 06/23/16 JRHARTUNG D30K926970 - ACR-1118 - Initial program           *
*----------------------------------------------------------------------*
************************************************************************

*eject
TABLES: bsik.

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  ty_wa_xparam     TYPE zfit_xparam,       "Program Parameters   "

        ty_it_xparam     TYPE STANDARD TABLE OF ty_wa_xparam.

TYPES: BEGIN OF ty_wa_bsik,                      "Vendor Open Items    "
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        buzei            TYPE buzei,             "Accounting Doc Item  "
        budat            TYPE budat,             "Posting Date         "
        bldat            TYPE bldat,             "Document Date        "
        waers            TYPE waers,             "Document Currency Key"
        xblnr            TYPE xblnr1,            "Reference Doc Number "
        blart            TYPE blart,             "Document Type        "
        bschl            TYPE bschl,             "Posting Key          "
        wrbtr            TYPE wrbtr,             "Amount In Doc Currency
        sgtxt            TYPE sgtxt,             "Item Text            "
        zlsch            TYPE dzlsch,            "Payment Method       "
        empfb            TYPE empfb,             "Payee/Payer          "
       END   OF ty_wa_bsik,

        ty_it_bsik       TYPE STANDARD TABLE OF ty_wa_bsik.

TYPES: BEGIN OF ty_wa_lfa1,                      "Vendor Master (General
        lifnr            TYPE lifnr,             "Vendor Account Number"
        name1            TYPE name1_gp,          "Name 1               "
        name2            TYPE name2_gp,          "Name 2               "
        adrnr            TYPE adrnr,             "Address Number       "
       END   OF ty_wa_lfa1,

        ty_it_lfa1       TYPE STANDARD TABLE OF ty_wa_lfa1.

TYPES: BEGIN OF ty_wa_adrc,                      "Addresses            "
        addrnumber       TYPE ad_addrnum,        "Address Number       "
        date_from        TYPE ad_date_fr,        "Valid-From Date      "
        nation           TYPE ad_nation,         "International Address"
        date_to          TYPE ad_date_to,        "Valid-To Date        "
        name1            TYPE ad_name1,          "Name 1               "
        name2            TYPE ad_name2,          "Name 2               "
        city1            TYPE ad_city1,          "City                 "
        street           TYPE ad_street,         "Street               "
        str_suppl2       TYPE ad_strspp2,        "Attention Of         "
       END   OF ty_wa_adrc,

        ty_it_adrc       TYPE STANDARD TABLE OF ty_wa_adrc.

TYPES:  ty_wa_bdcdata    TYPE bdcdata,           "BDC Data             "

        ty_it_bdcdata    TYPE STANDARD TABLE OF ty_wa_bdcdata.

*eject
TYPES: BEGIN OF ty_wa_report,                    "Report               "
        id_doc           TYPE char24,            "Document ID          "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        xblnr            TYPE xblnr1,            "Reference Doc Number "
        budat            TYPE budat,             "Posting Date         "
        bldat            TYPE bldat,             "Document Date        "
        wrbtr            TYPE wrbtr,             "Amount In Doc Currency
        waers            TYPE waers,             "Document Currency Key"
        blart            TYPE blart,             "Document Type        "
        bschl            TYPE bschl,             "Posting Key          "
        zlsch            TYPE dzlsch,            "Payment Method       "
        tx_msg           TYPE text120,           "Message Text         "
        fl_error         TYPE flag,              "Error Flag           "
        in_color         TYPE char1,             "Color Indicator      "
       END   OF ty_wa_report,

        ty_it_report     TYPE STANDARD TABLE OF ty_wa_report.

************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_param_out_int TYPE zparamkey          "Outbound Interface   "
                         VALUE 'OUTBOUND_INTERFACE',
        gc_param_obj_id  TYPE zparamkey          "Object ID            "
                         VALUE 'I_P2C_TR_003',
        gc_param_program TYPE zparamkey          "Program              "
                         VALUE 'VNDR_INVC_PAYM_MTHD',
        gc_param_bschl   TYPE zparamkey          "Posting Key          "
                         VALUE 'POSTING_KEY',
        gc_param_zlsch   TYPE zparamkey          "Payment Method       "
                         VALUE 'PAYMENT_METHOD',
        gc_modif_id_dsp  TYPE char3              "MODIF ID-Display     "
                         VALUE 'DSP'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_sysid         TYPE sysysid,           "Name Of The SAP System
        gv_uname         TYPE syuname,           "User Name            "
        gv_pagno         TYPE sypagno,           "Current List Page    "
        gv_cprog         TYPE sycprog,           "Calling Program      "
        gv_datum         TYPE char10,            "Current Date of Applic
        gv_uzeit         TYPE char08.            "Current Time of Applic

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_xparam        TYPE ty_it_xparam.      "Program Parameters   "

DATA:   gt_bsik          TYPE ty_it_bsik.        "Vendor Open Items    "

DATA:   gt_bdcdata       TYPE ty_it_bdcdata.     "BDC Data             "

DATA:   gt_report        TYPE ty_it_report.      "Report               "

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************

* Select Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECT-OPTIONS:   s_bukrs  FOR bsik-bukrs.       "Company Code         "
SELECT-OPTIONS:   s_belnr  FOR bsik-belnr.       "Accounting Doc Number"
SELECT-OPTIONS:   s_gjahr  FOR bsik-gjahr.       "Fiscal Year          "
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_bschl  FOR bsik-bschl        "Posting Key          "
                           MODIF ID dsp.
SELECT-OPTIONS:   s_zlsch  FOR bsik-zlsch        "Payment Method       "
                           MODIF ID dsp.
SELECTION-SCREEN: END   OF BLOCK ssb1.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
PARAMETERS:       p_test   AS CHECKBOX           "Company Code         "
                           DEFAULT 'X'.
SELECT-OPTIONS:   s_cpairs FOR bsik-zlsch        "Conversion Pairs For "
                           MODIF ID dsp.         "Payment Method       "
SELECTION-SCREEN: END   OF BLOCK ssb2.
