*&---------------------------------------------------------------------*
*&  Include           ZFFIC001_PWC_AUDIT_EXTRACT_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFFIC001_PWC_AUDIT_EXTRACT                        *
*  Include:          ZFFIC001_PWC_AUDIT_EXTRACT_TOP                    *
*  Author:           John Hartung                                      *
*  Date:             August 24, 2017                                   *
*  Application Area: FICO                                              *
*                                                                      *
*  Description:      FI PWC Audit Extract Top (Data Definitions)       *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 24Aug17  JRHARTUNG ACR-4863 DECK917823, DECK917837, DECK917853       *
*                    PWC Audit Extract - initial program creation      *
*----------------------------------------------------------------------*
************************************************************************

TABLES: bkpf.                                    "Accounting Doc.Header"

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF ty_wa_t001,                      "Company Codes        "
        bukrs            TYPE bukrs,             "Company Code         "
        butxt            TYPE butxt,             "Company Name         "
        land1            TYPE land1,             "Country Key          "
        waers            TYPE waers,             "Currency Key         "
        ktopl            TYPE ktopl,             "Chart-Of-Accounts    "
       END   OF ty_wa_t001,

        ty_it_t001       TYPE STANDARD TABLE OF ty_wa_t001.

TYPES: BEGIN OF ty_wa_doc_key,                   "Accounting Doc.Key   "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc.Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
       END   OF ty_wa_doc_key.

TYPES: BEGIN OF ty_wa_bkpf_key,                  "Accounting Doc.Key   "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc.Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        monat            TYPE monat,             "Fiscal Period        "
       END   OF ty_wa_bkpf_key,

        ty_it_bkpf_key   TYPE STANDARD TABLE OF ty_wa_bkpf_key.

TYPES:  ty_wa_bkpf       TYPE bkpf,              "Accounting Doc.Header"
        ty_it_bkpf       TYPE STANDARD TABLE OF ty_wa_bkpf.

TYPES:  ty_wa_bseg       TYPE bseg,              "Accounting Doc.Item  "
        ty_it_bseg       TYPE STANDARD TABLE OF ty_wa_bseg.

TYPES: BEGIN OF ty_wa_journal_id,                "Journal ID           "
        monat            TYPE monat,             "Fiscal Period        "
        comma_1          TYPE char1,             "Comma                "
        gjahr            TYPE gjahr,             "Fiscal Year          "
        comma_2          TYPE char1,             "Comma                "
        bukrs            TYPE bukrs,             "Company Code         "
        comma_3          TYPE char1,             "Comma                "
        belnr            TYPE belnr_d,           "Accounting Doc.Number"
       END   OF ty_wa_journal_id.

*eject
TYPES: BEGIN OF ty_wa_out_gl_detl,               "G/L Detail Output    "
        journal_id       TYPE STRING,            "Journal ID           "
        buzei            TYPE STRING,            "Journal ID Item Number
        bktxt            TYPE STRING,            "Document Header Text "
        sgtxt            TYPE STRING,            "Document Item Text   "
        source           TYPE STRING,            "Bus./Ref.Transaction "
        xauto            TYPE STRING,            "Automatically Created"
        lifnr            TYPE STRING,            "Vendor               "
        ebeln            TYPE STRING,            "Purchasing Doc.Number"
        bschl            TYPE STRING,            "Posting Key          "
        bukrs            TYPE STRING,            "Company Code         "
        gjahr            TYPE STRING,            "Fiscal Year          "
        monat            TYPE STRING,            "Fiscal Period        "
        budat            TYPE STRING,            "Posting Date         "
        cpudt            TYPE STRING,            "Entry Date           "
        userid           TYPE STRING,            "User ID              "
        tcode            TYPE STRING,            "Transaction Code     "
        hkont            TYPE STRING,            "G/L Account Number   "
        dmbtr            TYPE STRING,            "Amount in Local Curr."
        shkzg            TYPE STRING,            "Debit/Credit Indicator
        waers            TYPE STRING,            "Document Currency Key"
        upddt            TYPE STRING,            "Last Modified Date   "
        mod_by           TYPE STRING,            "Last Modified By     "
        appr_dt          TYPE STRING,            "Approved Date        "
        appr_by          TYPE STRING,            "Approved By          "
        cputm            TYPE STRING,            "Entry Time           "
        wrbtr            TYPE STRING,            "Amount in Doc.Currency
        rpt_curr         TYPE STRING,            "Amt.in Reporting Curr.
        pswbt            TYPE STRING,            "Amt. in G/L Currency "
        hwaer            TYPE STRING,            "Local Currency Key   "
        xnegp            TYPE STRING,            "Reversal Indicator   "
        stblg            TYPE STRING,            "Reverse Doc.Number   "
        mandt            TYPE STRING,            "Client               "
        bstat            TYPE STRING,            "Document Status      "
        blart            TYPE STRING,            "Document Type        "
        aedat            TYPE STRING,            "Last Change Date     "
        dbblg            TYPE STRING,            "Recurring Doc.Number "
        stjah            TYPE STRING,            "Reverse Doc.Fiscal Yr"
        brnch            TYPE STRING,            "Branch               "
        kursf            TYPE STRING,            "Exchange Rate        "
        xrueb            TYPE STRING,            "Back-Posted Document "
        xstov            TYPE STRING,            "Reversal Flag        "
        ausbk            TYPE STRING,            "Source Company Code  "
        xnetb            TYPE STRING,            "Net Document Type    "
        xblnr            TYPE STRING,            "Reference Doc.Number "
        hwae2            TYPE STRING,            "Second Local Currency"
        bldat            TYPE STRING,            "Document Date        "
        wwert            TYPE STRING,            "Translation Date     "
        grpid            TYPE STRING,            "Batch Input Session  "
        bvorg            TYPE STRING,            "Cross-Company Doc.   "

*eject
        buzid            TYPE STRING,            "Line Item Identificatn
        augdt            TYPE STRING,            "Clearing Date        "
        augcp            TYPE STRING,            "Clearing Entry Date  "
        augbl            TYPE STRING,            "Clearing Doc.Number  "
        koart            TYPE STRING,            "Account Type         "
        bewar            TYPE STRING,            "Transaction Type     "
        altkt            TYPE STRING,            "Group Account Number "
        vorgn            TYPE STRING,            "G/L Transaction Type "
        dmbe2            TYPE STRING,            "Amount in 2nd Local C"
        kostl            TYPE STRING,            "Cost Center          "
        saknr            TYPE STRING,            "G/L Account Number   "
        anfbu            TYPE STRING,            "Bill-Of-Exchange CoCd"
        ktosl            TYPE STRING,            "Transaction Key      "
        vbeln            TYPE STRING,            "Billing Document     "
        prctr            TYPE STRING,            "Profit Center        "
        pswsl            TYPE STRING,            "G/L Amount Currency Ky
        kokrs            TYPE STRING,            "Controlling Area     "
        gsber            TYPE STRING,            "Business Area        "
        vname            TYPE STRING,            "Joint Venture        "
        umskz            TYPE STRING,            "Special G/L Indicator"
        zuonr            TYPE STRING,            "Assignment Number    "
        kunnr            TYPE STRING,            "Customer Number      "
        matnr            TYPE STRING,            "Material Number      "
        werks            TYPE STRING,            "Plant                "
        menge            TYPE STRING,            "Quantity             "
        meins            TYPE STRING,            "Base Unit-Of-Measure "
        anbwa            TYPE STRING,            "Asset Transaction Type
        belnr            TYPE STRING,            "Accounting Doc.Number"
        glvor            TYPE STRING,            "Business Transaction "
        awtyp            TYPE STRING,            "Reference Transaction"
        usnam            TYPE STRING,            "User Name            "
        ppnam            TYPE STRING,            "Parked By            "
       END   OF ty_wa_out_gl_detl.

*eject
TYPES: BEGIN OF ty_wa_out_bkpf,                  "G/L Detail Output    "
        mandt            TYPE STRING,            "Client               "
        bukrs            TYPE STRING,            "Company Code         "
        monat            TYPE STRING,            "Fiscal Period        "
        gjahr            TYPE STRING,            "Fiscal Year          "
        belnr            TYPE STRING,            "Accounting Doc.Number"
        bktxt            TYPE STRING,            "Document Header Text "
        tcode            TYPE STRING,            "Transaction Code     "
        glvor            TYPE STRING,            "Business Transaction "
        awtyp            TYPE STRING,            "Reference Transaction"
        budat            TYPE STRING,            "Posting Date         "
        cpudt            TYPE STRING,            "Entry Date           "
        usnam            TYPE STRING,            "User Name            "
        waers            TYPE STRING,            "Document Currency Key"
        upddt            TYPE STRING,            "Last Modified Date   "
        ppnam            TYPE STRING,            "Parked By            "
        cputm            TYPE STRING,            "Entry Time           "
        hwaer            TYPE STRING,            "Local Currency Key   "
        stblg            TYPE STRING,            "Reverse Doc.Number   "
        bstat            TYPE STRING,            "Document Status      "
        blart            TYPE STRING,            "Document Type        "
        aedat            TYPE STRING,            "Last Change Date     "
        dbblg            TYPE STRING,            "Recurring Doc.Number "
        stjah            TYPE STRING,            "Reverse Doc.Fiscal Yr"
        brnch            TYPE STRING,            "Branch               "
        kursf            TYPE STRING,            "Exchange Rate        "
        xrueb            TYPE STRING,            "Back-Posted Document "
        xstov            TYPE STRING,            "Reversal Flag        "
        ausbk            TYPE STRING,            "Source Company Code  "
        xnetb            TYPE STRING,            "Net Document Type    "
        xblnr            TYPE STRING,            "Reference Doc.Number "
        hwae2            TYPE STRING,            "Second Local Currency"
        bldat            TYPE STRING,            "Document Date        "
        wwert            TYPE STRING,            "Translation Date     "
        grpid            TYPE STRING,            "Batch Input Session  "
        bvorg            TYPE STRING,            "Cross-Company Doc.   "
       END   OF ty_wa_out_bkpf.

*eject
TYPES: BEGIN OF ty_wa_out_bseg,                  "G/L Detail Output    "
        mandt            TYPE STRING,            "Client               "
        bukrs            TYPE STRING,            "Company Code         "
        belnr            TYPE STRING,            "Accounting Doc.Number"
        buzei            TYPE STRING,            "Journal ID Item Number
        sgtxt            TYPE STRING,            "Document Item Text   "
        gjahr            TYPE STRING,            "Fiscal Year          "
        hkont            TYPE STRING,            "G/L Account Number   "
        dmbtr            TYPE STRING,            "Amount in Local Curr."
        shkzg            TYPE STRING,            "Debit/Credit Indicator
        wrbtr            TYPE STRING,            "Amount in Doc.Currency
        pswbt            TYPE STRING,            "Amt. in G/L Currency "
        xnegp            TYPE STRING,            "Reversal Indicator   "
        buzid            TYPE STRING,            "Line Item Identificatn
        xauto            TYPE STRING,            "Automatically Created"
        ebeln            TYPE STRING,            "Purchasing Doc.Number"
        augdt            TYPE STRING,            "Clearing Date        "
        augcp            TYPE STRING,            "Clearing Entry Date  "
        augbl            TYPE STRING,            "Clearing Doc.Number  "
        bschl            TYPE STRING,            "Posting Key          "
        koart            TYPE STRING,            "Account Type         "
        bewar            TYPE STRING,            "Transaction Type     "
        altkt            TYPE STRING,            "Group Account Number "
        vorgn            TYPE STRING,            "G/L Transaction Type "
        dmbe2            TYPE STRING,            "Amount in 2nd Local C"
        kostl            TYPE STRING,            "Cost Center          "
        saknr            TYPE STRING,            "G/L Account Number   "
        anfbu            TYPE STRING,            "Bill-Of-Exchange CoCd"
        ktosl            TYPE STRING,            "Transaction Key      "
        vbeln            TYPE STRING,            "Billing Document     "
        prctr            TYPE STRING,            "Profit Center        "
        pswsl            TYPE STRING,            "G/L Amount Currency Ky
        kokrs            TYPE STRING,            "Controlling Area     "
        gsber            TYPE STRING,            "Business Area        "
        vname            TYPE STRING,            "Joint Venture        "
        umskz            TYPE STRING,            "Special G/L Indicator"
        zuonr            TYPE STRING,            "Assignment Number    "
        kunnr            TYPE STRING,            "Customer Number      "
        lifnr            TYPE STRING,            "Vendor               "
        matnr            TYPE STRING,            "Material Number      "
        werks            TYPE STRING,            "Plant                "
        menge            TYPE STRING,            "Quantity             "
        meins            TYPE STRING,            "Base Unit-Of-Measure "
        anbwa            TYPE STRING,            "Asset Transaction Type
       END   OF ty_wa_out_bseg.

*eject
************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_datum_run     TYPE sydatum,           "Run Date             "
        gv_uzeit_run     TYPE syuzeit,           "Run Time             "
        gv_fdlm1         TYPE char1,             "File Delimiter       "
        gv_file_seqn     TYPE numc3,             "File Sequence        "
        gv_file_size     TYPE syindex,           "File Size            "
        gv_file_size_brk TYPE syindex,           "File Size-ControlBreak
        gv_file_name     TYPE text256,           "File Name            "
        gv_file_name_hdr TYPE text256,           "File Name            "
        gv_file_name_itm TYPE text256,           "File Name            "
        gv_file_name_trn TYPE text256,           "File Name-Transfer   "
        gv_doc_count     TYPE syindex.           "Document Count       "

************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gs_t001          TYPE ty_wa_t001,        "Company Codes        "
        gs_bkpf_key      TYPE ty_wa_bkpf_key,    "Accounting Doc.Key   "
        gs_bkpf          TYPE ty_wa_bkpf,        "Accounting Doc.Header"
        gs_bseg          TYPE ty_wa_bseg.        "Accounting Doc.Item  "

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_t001          TYPE ty_it_t001,        "Company Codes        "
        gt_bkpf_key      TYPE ty_it_bkpf_key,    "Accounting Doc.Key   "
        gt_bkpf          TYPE ty_it_bkpf,        "Accounting Doc.Header"
        gt_bseg          TYPE ty_it_bseg.        "Accounting Doc.Item  "

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************
* Select Options
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       rb_g1ex1 RADIOBUTTON            "G/L Detail Data     "
                           GROUP rbg1
                           DEFAULT 'X'.
SELECTION-SCREEN: BEGIN OF BLOCK sb10 WITH FRAME TITLE text-s10.
SELECTION-SCREEN: BEGIN OF BLOCK sb11 WITH FRAME TITLE text-s11.
SELECT-OPTIONS:   s_bukrs1 FOR  bkpf-bukrs.      "Company Code         "
SELECT-OPTIONS:   s_belnr1 FOR  bkpf-belnr.      "Accounting Doc.Number"
SELECT-OPTIONS:   s_gjahr1 FOR  bkpf-gjahr.      "Fiscal Year          "
SELECT-OPTIONS:   s_monat1 FOR  bkpf-monat.      "Fiscal Period        "
SELECTION-SCREEN: END   OF BLOCK sb11.
SELECTION-SCREEN: BEGIN OF BLOCK sb12 WITH FRAME TITLE text-s12.
PARAMETERS:       p_fpth1  TYPE text128          "File Path            "
                           LOWER CASE.
PARAMETERS:       p_fnam1  TYPE text128          "File Name            "
                           LOWER CASE.
PARAMETERS:       p_fext1  TYPE char3            "File Extension       "
                           LOWER CASE.
PARAMETERS:       p_fdlm1  TYPE char1            "File Delimiter       "
                           DEFAULT '|'.
PARAMETERS:       p_fbrk1  TYPE numc4            "File Break In Mbyte  "
                           DEFAULT 100.
PARAMETERS:       p_dpbc1  TYPE numc5            "Docs Per Batch Count "
                           DEFAULT 2500.
PARAMETERS:       p_chdr1  AS CHECKBOX.          "Column Header Record "
PARAMETERS:       rb_g2fm1 RADIOBUTTON
                           GROUP rbg2
                           DEFAULT 'X'.
PARAMETERS:       rb_g2fm2 RADIOBUTTON
                           GROUP rbg2.
SELECTION-SCREEN: END   OF BLOCK sb12.
SELECTION-SCREEN: END   OF BLOCK sb10.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       rb_g1ex2 RADIOBUTTON            "Miscellaneous Coding"
                           GROUP rbg1.
SELECTION-SCREEN: BEGIN OF BLOCK sb20 WITH FRAME TITLE text-s20.
SELECTION-SCREEN: END   OF BLOCK sb20.
