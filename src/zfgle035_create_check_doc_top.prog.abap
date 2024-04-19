*&---------------------------------------------------------------------*
*&  Include           ZFGLE035_CREATE_CHECK_DOC_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFGLE035_CREATE_CHECK_DOC                         *
*  Include:          ZFGLE035_CREATE_CHECK_DOC_TOP                     *
*  Author:           John Hartung                                      *
*  Date:             December 11, 2013                                 *
*  Application Area: FICO GL                                           *
*                                                                      *
*  Description:      GL Create Check Document from G/L Posting         *
*                                                                      *
*                    FI G/L documents are queried and found items      *
*                    are used to create Check Documents.  These        *
*                    Check documents are required in order for         *
*                    FEBAN processing to automatically clear the       *
*                    postings once the actual check is cashed.         *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By           Transport  Description                         *
* -------- ------------ ---------- ----------------------------------- *
* 10/23/19 JRHARTUNG    D30K930228 CHG0160254 - Initial Program        *
*                                  Development                         *
* 10/31/19 JRHARTUNG    D30K930244 CHG0160254 - Do not adjust check #  *
*----------------------------------------------------------------------*
************************************************************************

************************************************************************
*                                Tables                                *
************************************************************************
TABLES: bkpf,                      ""Accounting Document Header        "
        bseg.                      ""Accounting Document Item          "

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  ty_wa_xparam     TYPE zfit_xparam.       "Program Parameters   "

TYPES:  ty_it_xparam     TYPE STANDARD TABLE OF ty_wa_xparam.

TYPES: BEGIN OF ty_wa_bkpf,                      "Accounting Doc Header"
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        blart            TYPE blart,             "Document Type        "
        bldat            TYPE bldat,             "Document Date        "
        budat            TYPE budat,             "Posting Date         "
        monat            TYPE monat,             "Fiscal Period        "
        cpudt            TYPE cpudt,             "Date Of Entry        "
        cputm            TYPE cputm,             "Time Of Entry        "
        aedat            TYPE aedat_bkpf,        "Last Doc Change Date "
        upddt            TYPE upddt,             "Last Update Date     "
        wwert            TYPE wwert_d,           "Translation Date     "
        usnam            TYPE usnam,             "User Name            "
        tcode            TYPE tcode,             "Transaction Code     "
        bvorg            TYPE bvorg,             "Cross-Company Doc Nbr"
        xblnr            TYPE xblnr1,            "Reference Doc Number "
        stblg            TYPE stblg,             "Reverse Doc Number   "
        stjah            TYPE stjah,             "Reverse Doc Fiscal Yr"
        bktxt            TYPE bktxt,             "Document Header Text "
        waers            TYPE waers,             "Currency Key         "
        bstat            TYPE bstat_d,           "Document Status      "
        xnetb            TYPE xnetb,             "Indictr:Doc Posted Net
        glvor            TYPE glvor,             "Business Transaction "
        grpid            TYPE grpid_bkpf,        "Batch Input Sessn Name
        awtyp            TYPE awtyp,             "Reference Transaction"
        awkey            TYPE awkey,             "Reference Key        "
        hwaer            TYPE hwaer,             "Local Currency       "
        hwae2            TYPE hwae2,             "Group Currency       "
        xstov            TYPE xstov,             "Flagged For Reversal "
        stodt            TYPE stodt,             "Planned Dt For Rev Pst
        xmwst            TYPE xmwst,             "Calc tax automatically
        ausbk            TYPE ausbk,             "Source Company Code  "
        awsys            TYPE logsystem,         "Logical System       "
        stgrd            TYPE stgrd,             "Reason For Reversal  "
        ppnam            TYPE ppnam,             "User Who Parked Doc  "
        xreversal        TYPE xreversal,         "Reversal Indicator   "
        rldnr            TYPE fagl_rldnr,        "Ledger In G/L Accntng"
        ldgrp            TYPE fagl_ldgrp,        "Ledger Group         "
        xsplit           TYPE split_posting,     "Origin From Split Post
       END   OF ty_wa_bkpf.

TYPES:  ty_it_bkpf       TYPE STANDARD TABLE OF ty_wa_bkpf.

*eject
TYPES: BEGIN OF ty_wa_bseg_key,                  "Accounting Doc Itm Key
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
       END   OF ty_wa_bseg_key.                  "Accounting Doc Itm Key

TYPES:  ty_it_bseg_key   TYPE STANDARD TABLE OF ty_wa_bseg_key.

TYPES: BEGIN OF ty_wa_bseg,                      "Accounting Doc Item  "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Item Line Number     "
        augdt            TYPE augdt,             "Clearing Doc Date    "
        augbl            TYPE augbl,             "Clearing Doc Number  "
        bschl            TYPE bschl,             "Posting Key          "
        koart            TYPE koart,             "Account Type         "
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        dmbtr            TYPE dmbtr,             "Amnt in Local Currncy"
        wrbtr            TYPE wrbtr,             "Amnt in Dcmnt Currncy"
        zuonr            TYPE dzuonr,            "Assignment Number    "
        sgtxt            TYPE sgtxt,             "Item Text            "
        vbund            TYPE rassc,             "Trading Partner Compny
        kokrs            TYPE kokrs,             "Controlling Area     "
        kostl            TYPE kostl,             "Cost Center          "
        aufnr            TYPE aufnr,             "Order Number         "
        xauto            TYPE xauto,             "Itm Automatcly Creatd"
        hkont            TYPE hkont,             "General Ledgr Account"
        lifnr            TYPE lifnr,             "Vendor Account Number"
        xref1            TYPE xref1,             "Business Partner Ref 1
        xref2            TYPE xref2,             "Business Partner Ref 2
        xref3            TYPE xref3,             "Business Partner Ref 3
        flag_delete      TYPE flag,              "Delete Flag          "
       END   OF ty_wa_bseg.

TYPES:  ty_it_bseg       TYPE STANDARD TABLE OF ty_wa_bseg.

TYPES: BEGIN OF ty_wa_bsak,                      "Clearing Douments    "
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        umsks            TYPE umsks,             "Spcl G/L Transaction "
        umskz            TYPE umskz,             "Spcl G/L Indicator   "
        augdt            TYPE augdt,             "Clearing Doc Date    "
        augbl            TYPE augbl,             "Clearing Doc Number  "
        zuonr            TYPE dzuonr,            "Assignment Number    "
        gjahr            TYPE gjahr,             "Fiscal Year          "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        buzei            TYPE buzei,             "Accounting Doc Item  "
        xblnr            TYPE xblnr1,            "Reference Doc Number "
        blart            TYPE blart,             "Document Type        "
        bschl            TYPE bschl,             "Posting Key          "
        sgtxt            TYPE sgtxt,             "Document Item Text   "
       END   OF ty_wa_bsak.

TYPES:  ty_it_bsak       TYPE STANDARD TABLE OF ty_wa_bsak.

*eject
TYPES: BEGIN OF ty_wa_skb1,                      "G/L Company Segment  "
        bukrs            TYPE bukrs,             "Company Code         "
        saknr            TYPE saknr,             "G/L Account Number   "
        hbkid            TYPE hbkid,             "House Bank Short Key "
        hktid            TYPE hktid,             "Bank Account Short Key
       END   OF ty_wa_skb1.

TYPES:  ty_it_skb1       TYPE STANDARD TABLE OF ty_wa_skb1.

TYPES: BEGIN OF ty_wa_tbsl,                      "Posting Key          "
        bschl            TYPE bschl,             "Posting Key          "
        shkzg            TYPE tbsl-shkzg,        "Debit/Credit Indicator
        koart            TYPE tbsl-koart,        "Account Type         "
        xumsw            TYPE tbsl-xumsw,        "Sales Related Indicatr
       END   OF ty_wa_tbsl.

TYPES:  ty_it_tbsl       TYPE STANDARD TABLE OF ty_wa_tbsl.

TYPES: BEGIN OF ty_wa_item,                      "Item Data            "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Item Line Number     "
        blart            TYPE blart,             "Document Type        "
        budat            TYPE budat,             "Posting Date         "
        cpudt            TYPE cpudt,             "Date Of Entry        "
        xblnr            TYPE xblnr1,            "Reference Doc Number "
        waers            TYPE waers,             "Currency Key         "
        koart            TYPE koart,             "Account Type         "
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        dmbtr            TYPE dmbtr,             "Amnt in Local Currncy"
        wrbtr            TYPE wrbtr,             "Amnt in Dcmnt Currncy"
        hkont            TYPE hkont,             "General Ledgr Account"
        sgtxt            TYPE sgtxt,             "Item Text            "
        hbkid            TYPE hbkid,             "House Bank Short Key "
        hktid            TYPE hktid,             "Accnt Details Short ID
        zort1            TYPE dzort1,            "City Of The Payee    "
        zland            TYPE dzland,            "Country Key          "
       END   OF ty_wa_item.

TYPES:  ty_it_item       TYPE STANDARD TABLE OF ty_wa_item.

TYPES:  ty_wa_crt_chk_rpt                        "Create Check Report  "
                         TYPE zgls_create_check_alv.

TYPES   ty_it_crt_chk_rpt
                         TYPE STANDARD TABLE OF ty_wa_crt_chk_rpt.

TYPES:  ty_wa_bdcdata    TYPE bdcdata.           "Batch Input          "

TYPES:  ty_it_bdcdata    TYPE STANDARD TABLE OF ty_wa_bdcdata.

TYPES:  ty_wa_bdcmsg     TYPE bdcmsgcoll.        "Batch Message        "

TYPES:  ty_it_bdcmsg     TYPE STANDARD TABLE OF ty_wa_bdcmsg.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_paramtype     TYPE zparamtype         "Parameter Type       "
                         VALUE 'ENHANCEMENT',
        gc_subtype       TYPE zparamsubtype      "Parameter Subtype    "
                         VALUE 'I_P2C_GL_035',
        gc_key1          TYPE zparamkey          "Parameter Key 1      "
                         VALUE 'RETRIEVE_INVOICE',
        gc_a             TYPE char1              "A, Abnormal Terminatn"
                         VALUE 'A',
        gc_e             TYPE char1              "E, Error             "
                         VALUE 'E',
        gc_x             TYPE char1              "X, True, Yes         "
                         VALUE 'X',
        gc_blocksize     TYPE syindex            "Blocksize            "
                         VALUE 50,
        gc_hbkid         TYPE hbkid              "House Bank Short Key "
                         VALUE 'JPMCD',
        gc_hktid         TYPE hktid              "Accnt Details Short ID
                         VALUE 'ROWCK',
        gc_zort1         TYPE dzort1             "City Of The Payee    "
                         VALUE 'Houston',
        gc_zland         TYPE dzland             "Country Key          "
                         VALUE 'US',
        gc_koart_vendor  TYPE koart              "Vendor Account Type  "
                         VALUE 'K',
        gc_tcode         TYPE sytcode            "Transaction Code     "
                         VALUE 'FCH5',
        gc_color_errr    TYPE char4              "Color-Error          "
                         VALUE 'C600'.

CONSTANTS:
        gc_alv_cntn_rpt1 TYPE scrfname           "ALV Container-Report 1
                         VALUE 'CUSTOM_CONTAINER_RPT1',
        gc_alv_stru_rpt1 TYPE tabname            "ALV Structure-Report 1
                         VALUE 'ZGLS_CREATE_CHECK_ALV'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_repid         TYPE syrepid,           "Report ID            "
        gv_ucomm         TYPE syucomm,           "Screen Function Code "
        gv_ok_code       TYPE syucomm,           "Screen Function Code "
        gv_flag_exit     TYPE flag.              "Flag-Exit            "

DATA:   gs_variant       TYPE disvariant,        "LVC Display Variant  "
        gs_variant_p     TYPE disvariant,        "LVC Display Variant-Pr
        gs_layout        TYPE lvc_s_layo,        "LVC Layout Structure "
        gs_print         TYPE lvc_s_prnt.        "LVC Print Settings   "

*eject
*----------------------------------------------------------------------*
* Range Tables                                                         *
*----------------------------------------------------------------------*

DATA:   grt_tbsl_vendor  TYPE RANGE OF bschl.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   git_xparam       TYPE ty_it_xparam.      "Program Parameters   "

DATA:   git_skb1         TYPE ty_it_skb1.        "G/L Company Segment  "

DATA:   git_tbsl_vendor  TYPE ty_it_tbsl.        "Vendor Posting Keys  "

DATA:   git_item         TYPE ty_it_item.        "Item Data            "

DATA:   git_fld_cat_rpt1 TYPE lvc_t_fcat.        "LVC Field Catalog RPT1

DATA:   git_crt_chk_rpt  TYPE ty_it_crt_chk_rpt. "Create Check Report  "

DATA:   git_bdcdata      TYPE ty_it_bdcdata.     "Batch Input          "

*eject
************************************************************************
* Predefine a local class for event handling to allow the              *
* declaration of a reference variable.                                 *
************************************************************************
CLASS   lcl_event_receiver   DEFINITION DEFERRED.

************************************************************************
*                               Objects                                *
************************************************************************
DATA:   go_custom_cntn_rpt1  TYPE REF TO cl_gui_custom_container,"Reprt1
        go_grid_rpt1         TYPE REF TO cl_gui_alv_grid,        "Reprt1
        go_event_receiver    TYPE REF TO lcl_event_receiver.     "Events

************************************************************************
*                           Selection Screen                           *
************************************************************************

* Select Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECT-OPTIONS:   s_bukrs  FOR bkpf-bukrs        "Company Code         "
                           OBLIGATORY.
SELECT-OPTIONS:   s_belnr  FOR bkpf-belnr.       "Accounting Doc Number"
SELECT-OPTIONS:   s_gjahr  FOR bkpf-gjahr.       "Fiscal Year          "
SELECT-OPTIONS:   s_budat  FOR bkpf-budat.       "Posting Date         "
SELECT-OPTIONS:   s_cpudt  FOR bkpf-cpudt.       "Date Of Entry        "
SELECT-OPTIONS:   s_blart  FOR bkpf-blart        "Document Type        "
                           OBLIGATORY.
SELECT-OPTIONS:   s_hkont  FOR bseg-hkont        "General Ledger Account
                           OBLIGATORY.
SELECTION-SCREEN: END   OF BLOCK ssb1.

* Run Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
PARAMETERS:       p_dspvar TYPE slis_vari.       "Display Layout       "
PARAMETERS:       cb_tstrn AS CHECKBOX           "Test Run             "
                           DEFAULT gc_x.
SELECTION-SCREEN: END   OF BLOCK ssb2.
