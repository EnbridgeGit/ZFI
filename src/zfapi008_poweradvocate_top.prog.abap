*&---------------------------------------------------------------------*
*&  Include           ZFAPI008_POWERADVOCATE_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Include:          ZFAPI008_POWERADVOCATE_TOP                        *
*  Program:          ZFAPI008_POWERADVOCATE                            *
*  Author:           John Hartung                                      *
*  Date:             May 19, 2014                                      *
*  Track #:          TR928 Release 1 - Streamline                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      AP PowerAdvocate - Outbound - TOP Include         *
*                                                                      *
*                    Extract cleared vendor docs - Data Definitions    *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By           Description                                    *
* -------- ------------ ---------------------------------------------- *
* 05/19/14 JRHARTUNG    D30K924122 - Initial program development       *
*                       D30K924256, D30K924362, D30K924476             *
* 07/26/17 JRHARTUNG    ACR-4710    D30K928284, D30K928296, D30K928308 *
*                       PowerAdvocate Break/Fix Enhancements           *
*                       1. Add Payment Method "N" - E-Payables BOA     *
*                       2. Replace invalid characters with a blank     *
*                       3. Scan Material Description for invalid chars *
*                       4. Copy the PO Number to all items in an MM    *
*                          (MIRO) Invoice (commented out D30K928520 )  *
*----------------------------------------------------------------------*
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 07-03-2019   KMB         D30K929692  CHG0133164 Power Advocate       *
*                                      Additional fields               *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 15-04-2019   KMB         D30K929768  ENHC0025289 Power Advocate      *
*                                      Additional fields               *
**********************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 16-05-2019   KMB         D30K929840  ENHC0025289 Power Advocate      *
*                                      Additional fields               *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 11-07-2019   KMB         D30K930020  CHG0150958 fixing of data       *
*                                      parsing errors in output files  *
*                                      & Eliminate Employee data       *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 27-09-2019   KMB         D30K930187  CHG0161101 DFCT0017698 Data     *
*                                      parsing issue                   *
************************************************************************

TABLES: bsak,                     "Secondary Index - Vendor Cleared Docs
        bkpf,                     "Accounting Document Header          "
        bseg.                     "Accounting Document Segment         "

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  ty_wa_zvar       TYPE zvar,              "Program Variables    "

        ty_it_zvar       TYPE STANDARD TABLE OF ty_wa_zvar.

TYPES: BEGIN OF ty_wa_t001,                      "Company              "
        bukrs            TYPE bukrs,             "Company Code         "
        land1            TYPE land1,             "Country Key          "
        waers            TYPE waers,             "Currency Key         "
        kalsm            TYPE kalsm_d,           "Tax Procedure        "
       END   OF ty_wa_t001,

        ty_it_t001       TYPE STANDARD TABLE OF ty_wa_t001.

TYPES:  ty_wa_t001u      TYPE t001u,             "Company Clearing     "

        ty_it_t001u      TYPE STANDARD TABLE OF ty_wa_t001u.

TYPES: BEGIN OF ty_wa_t001w,                     "Plants/Branches      "
        werks            TYPE ewerk,             "Plant                "
        ort01            TYPE ort01,             "City                 "
        adrnr            TYPE adrnr,             "Address Number       "
       END   OF ty_wa_t001w,

        ty_it_t001w      TYPE STANDARD TABLE OF ty_wa_t001w.

TYPES: BEGIN OF ty_wa_t003,                      "Document Types       "
        blart            TYPE blart,             "Document Type        "
        brgru            TYPE brgru,             "Authorization Group  "
        ltext            TYPE ltext_003t,        "Doc.Type Description "
       END   OF ty_wa_t003,

        ty_it_t003       TYPE STANDARD TABLE OF ty_wa_t003.

TYPES: BEGIN OF ty_wa_t007s,                     "Tax Code Names       "
        kalsm            TYPE kalsm_d,           "Tax Procedure        "
        mwskz            TYPE mwskz,             "Tax Code             "
        text1            TYPE text1_007s,        "Tax Code Name        "
       END   OF ty_wa_t007s,

        ty_it_t007s      TYPE STANDARD TABLE OF ty_wa_t007s.

TYPES: BEGIN OF ty_wa_t023t,                     "Material Group Descrpt
        matkl            TYPE matkl,             "Material Group       "
        wgbez            TYPE wgbez,             "Material Group Descrpt
       END   OF ty_wa_t023t,

        ty_it_t023t      TYPE STANDARD TABLE OF ty_wa_t023t.

*eject
TYPES: BEGIN OF ty_wa_t059t,                     "Minority Indicator Txt
        mindk            TYPE mindk,             "Minority Indicator   "
        mtext            TYPE txt30,             "Minority Indicator Txt
       END   OF ty_wa_t059t,

        ty_it_t059t      TYPE STANDARD TABLE OF ty_wa_t059t.

TYPES: BEGIN OF ty_wa_t161t,                     "Purch.Doc.Type Descrpt
        bsart            TYPE esart,             "Purchasing Doc.Type  "
        bstyp            TYPE bstyp,             "Purchasing Doc.Categry
        batxt            TYPE batxt,             "Purch.Doc.Type Descrpt
       END   OF ty_wa_t161t,

        ty_it_t161t      TYPE STANDARD TABLE OF ty_wa_t161t.

TYPES: BEGIN OF ty_wa_tbsl,                      "Posting Key          "
        bschl            TYPE bschl,             "Posting Key          "
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        ltext            TYPE text_bslt,         "Posting Key Text     "
       END   OF ty_wa_tbsl,

        ty_it_tbsl       TYPE STANDARD TABLE OF ty_wa_tbsl.

TYPES: BEGIN OF ty_wa_skat,                      "G/L Account Text     "
        saknr            TYPE saknr,             "G/L Account          "
        txt20            TYPE txt20_skat,        "G/L Account Short Text
       END   OF ty_wa_skat,

        ty_it_skat       TYPE STANDARD TABLE OF ty_wa_skat.

TYPES: BEGIN OF ty_wa_bsak,                      "Sec.Index-Vendor Clrd"
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        hkont            TYPE hkont,             "G/L Account Code     "
        xzahl            TYPE xzahl,             "PstgKey Used In Paymnt
       END   OF ty_wa_bsak,

        ty_it_bsak       TYPE STANDARD TABLE OF ty_wa_bsak.

TYPES: BEGIN OF ty_wa_bsak_c,                    "Sec.Index-Vendor Clrd"
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        umsks            TYPE umsks,             "Spec.G/L Transctn Type
        umskz            TYPE umskz,             "Special G/L Indicator"
        augdt            TYPE augdt,             "Clearing Date        "
        augbl            TYPE augbl,             "Clearing Document    "
        zuonr            TYPE dzuonr,            "Assignment Number    "
        gjahr            TYPE gjahr,             "Fiscal Year          "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        buzei            TYPE buzei,             "Accounting Doc Item  "
        zlsch            TYPE dzlsch,            "Payment Method       "
       END   OF ty_wa_bsak_c.

*eject
TYPES: BEGIN OF ty_wa_doc_prcsd,                 "Processed Documents  "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
       END   OF ty_wa_doc_prcsd,

        ty_it_doc_prcsd  TYPE STANDARD TABLE OF ty_wa_doc_prcsd.

TYPES: BEGIN OF ty_wa_bkpf,                      "Accounting Doc Headers
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        blart            TYPE blart,             "Document Type        "
        bldat            TYPE bldat,             "Document Date        "
        budat            TYPE budat,             "Posting Date         "
        monat            TYPE monat,             "Fiscal Period        "
        cpudt            TYPE cpudt,             "Document Entry Date  "
        usnam            TYPE usnam,             "User Name            "
        tcode            TYPE tcode,             "Transaction Code     "
        bvorg            TYPE bvorg,             "Cross-Company Code Pst
        xblnr            TYPE xblnr1,            "Reference Doc Number "
        stblg            TYPE stblg,             "Reverse Doc Number   "
        stjah            TYPE stjah,             "Reverse Doc Fiscl Year
        bktxt            TYPE bktxt,             "Document Header Text "
        waers            TYPE waers,             "Currency Key         "
        awtyp            TYPE awtyp,             "Reference Transaction"
        awkey            TYPE awkey,             "Reference Transaction"
        hwaer            TYPE hwaer,             "Local Currency       "
        ausbk            TYPE ausbk,             "Source Company Code  "
        xreversal        TYPE xreversal,         "Reversed/Reveresal Ind
        buk_s1           TYPE bukrs,             "Company Code     Sort"
        bel_s1           TYPE belnr_d,           "Accounting Doc   Sort"
        gja_s1           TYPE gjahr,             "Fiscal Year      Sort"
        fl_found_rev     TYPE flag,              "Flag-Reversal Found  "
       END   OF ty_wa_bkpf,

        ty_it_bkpf       TYPE STANDARD TABLE OF ty_wa_bkpf.

*eject
TYPES: BEGIN OF ty_wa_bseg,                      "Accounting Doc Items "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Accounting Doc Item  "
        augdt            TYPE augdt,             "Clearing Date        "
        augbl            TYPE augbl,             "Clearing Document    "
        bschl            TYPE bschl,             "Posting Key          "
        koart            TYPE koart,             "Account Type         "
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        mwskz            TYPE mwskz,             "Tax Code             "
        dmbtr            TYPE dmbtr,             "Amount In Local Curr "
        wrbtr            TYPE wrbtr,             "Amount In Doc.  Curr "
        zuonr            TYPE dzuonr,            "Assignment Number    "
        sgtxt            TYPE sgtxt,             "Item Text            "
        vbund            TYPE rassc,             "Trading Partner Compny
        kostl            TYPE kostl,             "Cost Center          "
        aufnr            TYPE aufnr,             "Order Number         "
        saknr            TYPE saknr,             "G/L Account Code     "
        hkont            TYPE hkont,             "G/L Account Code     "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        zfbdt            TYPE dzfbdt,            "Base Line Date-Due Dat
        zterm            TYPE dzterm,            "Terms Of Payment Key "
        zbd1t            TYPE dzbd1t,            "Cash Discount Days 1 "
        zbd2t            TYPE dzbd2t,            "Cash Discount Days 2 "
        zbd3t            TYPE dzbd3t,            "Net Payment Terms Perd
        sknto            TYPE sknto,             "Cash Discount Amount "
        wskto            TYPE wskto,             "Cash Discount Amount "
        zlsch            TYPE schzw_bseg,        "Payment Method       "
        rebzg            TYPE rebzg,             "Invoice Number Ref   "
        menge            TYPE menge_d,           "Quantity             "
        ebeln            TYPE ebeln,             "Purchasing Doc.Number"
        ebelp            TYPE ebelp,             "Purchasing Doc.Item  "
        prctr            TYPE prctr,             "Profit Center        "
        nplnr            TYPE nplnr,             "Network Number       "
        aufpl            TYPE aufpl_ch,          "Operation In Order   "
        aplzl            TYPE aplzl_ch,          "Counter for Order    "
        projk            TYPE ps_psp_pnr,        "WBS Element          "
        uzawe            TYPE uzawe,             "Payment Method Suplmnt
        xref1            TYPE xref1,             "Busnss Partner Ref.Key
        xref2            TYPE xref2,             "Busnss Partner Ref.Key
        xref3            TYPE xref3,             "Reference Key For Item
        auggj            TYPE auggj,             "Clearg Doc Fiscal Year
        buk_s1           TYPE bukrs,             "Company Code     Sort"
        bel_s1           TYPE belnr_d,           "Accounting Doc   Sort"
        gja_s1           TYPE gjahr,             "Fiscal Year      Sort"
       END   OF ty_wa_bseg,

        ty_it_bseg       TYPE STANDARD TABLE OF ty_wa_bseg.

*eject
TYPES: BEGIN OF ty_wa_bvorg,                     "Cross-Comp Postng Trns
        bvorg            TYPE bvorg,             "Cross-Comp Postng Trns
       END   OF ty_wa_bvorg,

        ty_it_bvorg      TYPE STANDARD TABLE OF ty_wa_bvorg.

TYPES:  ty_wa_bvor       TYPE bvor,              "InterCompany Postings"

        ty_it_bvor       TYPE STANDARD TABLE OF ty_wa_bvor.

TYPES: BEGIN OF ty_wa_lfa1,                      "Vendor Master-General"
        lifnr            TYPE lifnr,             "Vendor Account Number"
        land1            TYPE land1_gp,          "Country              "
        name1            TYPE name1_gp,          "Vendor Name 1        "
        ort01            TYPE ort01_gp,          "City                 "
        pfach            TYPE pfach,             "PO Box               "
        pstl2            TYPE pstl2,             "PO Box Postal Code   "
        pstlz            TYPE pstlz,             "Postal Code          "
        regio            TYPE regio,             "Region/State/Prov.   "
        stras            TYPE stras_gp,          "Street               "
        adrnr            TYPE adrnr,             "Address Number       "
        erdat            TYPE erdat_rf,          "Create Date          "
        ktokk            TYPE ktokk,             "Vendor Account Type  "
       END   OF ty_wa_lfa1.

TYPES: BEGIN OF ty_wa_lfb1,                      "Vendor Master-Company"
        lifnr            TYPE lifnr,             "Vendor Account Number"
        bukrs            TYPE bukrs,             "Company Number       "
        erdat            TYPE erdat_rf,          "Create Date          "
        mindk            TYPE mindk,             "Minority Indicator   "
       END   OF ty_wa_lfb1.

TYPES: BEGIN OF ty_wa_cdhdr,                     "Change Doc.Header    "
        objectclas       TYPE cdobjectcl,        "Object Class         "
        objectid         TYPE cdobjectv,         "Object ID            "
        changenr         TYPE cdchangenr,        "Change Number        "
        udate            TYPE cddatum,           "Creation Date Of Chng"
       END   OF ty_wa_cdhdr,

        ty_it_cdhdr      TYPE STANDARD TABLE OF ty_wa_cdhdr.

TYPES: BEGIN OF ty_wa_vendor,                    "Vendor               "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        bukrs            TYPE bukrs,             "Company Number       "
        name1            TYPE name1_gp,          "Vendor Name 1        "
        stras            TYPE stras_gp,          "Street               "
        ort01            TYPE ort01_gp,          "City                 "
        regio            TYPE regio,             "Region/State/Prov.   "
        pstlz            TYPE pstlz,             "Postal Code          "
        land1            TYPE land1_gp,          "Country              "
        adrnr            TYPE adrnr,             "Address Number       "
        ktokk            TYPE ktokk,             "Vendor Account Type  "
        mindk            TYPE mindk,             "Minority Indicator   "
        erdat            TYPE erdat_rf,          "Create Date          "
       END   OF ty_wa_vendor,

        ty_it_vendor     TYPE STANDARD TABLE OF ty_wa_vendor.

*eject
TYPES: BEGIN OF ty_wa_ekko,                      "Purchasing Document  "
        ebeln            TYPE ebeln,             "Purchasing Doc.Number"
        bstyp            TYPE ebstyp,            "Purchasing Doc.Categry
        bsart            TYPE esart,             "Purchasing Doc.Type  "
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
        ekgrp            TYPE ekgrp,
        aedat            TYPE aedat,
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
       END   OF ty_wa_ekko.

TYPES: BEGIN OF ty_wa_ekpo,                      "Purchasing Doc.Item  "
        ebeln            TYPE ebeln,             "Purchasing Doc.Number"
        ebelp            TYPE ebelp,             "Purchasing Doc.Item  "
        txz01            TYPE txz01,             "PO Item Text         "
        matnr            TYPE matnr,             "Material Number      "
        werks            TYPE ewerk,             "Plant                "
        matkl            TYPE matkl,             "Material Group       "
        wepos            TYPE wepos,             "Goods Receipt Ind    "
        repos            TYPE repos,             "Invoice Receipt Ind  "
        webre            TYPE webre,             "GR-Based Inv Verif Ind
        konnr            TYPE konnr,             "Princpl Purch.Agreemnt
        adrnr            TYPE adrnr_mm,          "Manual Address Number"
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
        menge            TYPE ekpo-menge,
        meins            TYPE ekpo-meins,
        netpr            TYPE ekpo-netpr,
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
       END   OF ty_wa_ekpo.

TYPES: BEGIN OF ty_wa_ekbe,                      "Purchasing Doc.History
        ebeln            TYPE ebeln,             "Purchasing Doc.Number"
        ebelp            TYPE ebelp,             "Purchasing Doc.Item  "
        zekkn            TYPE dzekkn,            "Account Assignment No.
        vgabe            TYPE vgabe,             "Transaction Type     "
        gjahr            TYPE mjahr,             "Material Doc.Year    "
        belnr            TYPE mblnr,             "Material Doc.Number  "
        buzei            TYPE mblpo,             "Material Doc.Item    "
        budat            TYPE budat,             "Posting Date         "
        menge            TYPE menge_d,           "Quantity             "
        dmbtr            TYPE dmbtr,             "Amount In Local Curr "
        xblnr            TYPE xblnr1,            "Reference Doc Number "
       END   OF ty_wa_ekbe,

        ty_it_ekbe       TYPE STANDARD TABLE OF ty_wa_ekbe.

*eject
TYPES: BEGIN OF ty_wa_po_data,                   "Purchase Order Data  "
        ebeln            TYPE ebeln,             "Purchasing Doc.Number"
        ebelp            TYPE ebelp,             "Purchasing Doc.Item  "
        bstyp            TYPE ebstyp,            "Purchasing Doc.Categry
        bsart            TYPE esart,             "Purchasing Doc.Type  "
        txz01            TYPE txz01,             "PO Item Text         "
        matnr            TYPE matnr,             "Material Number      "
        werks            TYPE ewerk,             "Plant                "
        matkl            TYPE matkl,             "Material Group       "
        wepos            TYPE wepos,             "Goods Receipt Indicatr
        repos            TYPE repos,             "Invoice Receipt Ind  "
        webre            TYPE webre,             "GR-Based Inv Verif Ind
        konnr            TYPE konnr,             "Princpl Purch.Agreemnt
        ort01            TYPE ort01,             "City                 "
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
        ktext            TYPE char50,
        konty            TYPE char10,
*BOC by KMB on 16.05.2019 ENHC0025289 Power Advocate Additional fields
*        recvr            TYPE char12,
        recvr            TYPE char20,
*EOC by KMB on 16.05.2019 ENHC0025289 Power Advocate Additional fields
        iddes            TYPE char50,
        ekgrp            TYPE ekgrp,
        aedat            TYPE aedat,
        plant            TYPE werks,
        menge            TYPE ekpo-menge,
        meins            TYPE char10,
        netpr            TYPE ekpo-netpr,
        street           TYPE adrc-street,
        city1            TYPE adrc-city1,
        region           TYPE adrc-region,
        post_code1       TYPE adrc-post_code1,
        eknam            TYPE eknam,
        name1            TYPE name1,
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
        set_desc         TYPE char50,
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
       END   OF ty_wa_po_data,

        ty_it_po_data    TYPE STANDARD TABLE OF ty_wa_po_data.

TYPES: BEGIN OF ty_wa_goods_recpt,               "Goods Receipt Data   "
        ebeln            TYPE ebeln,             "Purchasing Doc.Number"
        ebelp            TYPE ebelp,             "Purchasing Doc.Item  "
        zekkn            TYPE dzekkn,            "Account Assignment No.
        vgabe            TYPE vgabe,             "Transaction Type     "
        gjahr            TYPE mjahr,             "Material Doc.Year    "
        belnr            TYPE mblnr,             "Material Doc.Number  "
        buzei            TYPE mblpo,             "Material Doc.Item    "
        budat            TYPE budat,             "Posting Date         "
        menge            TYPE menge_d,           "Quantity             "
        dmbtr            TYPE dmbtr,             "Amount In Local Curr "
        xblnr            TYPE xblnr1,            "Reference Doc Number "
       END   OF ty_wa_goods_recpt.

TYPES: BEGIN OF ty_wa_ven_amt,                   "Vendor Amount        "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        dmbtr            TYPE dmbtr,             "Amount In Local Curr "
       END   OF ty_wa_ven_amt,

        ty_it_ven_amt    TYPE STANDARD TABLE OF ty_wa_ven_amt.

TYPES: BEGIN OF ty_wa_swwwihead,                 "Workflow Header      "
        wi_id            TYPE sww_wiid,          "Work Item ID         "
        wi_type          TYPE sww_witype,        "Work Item Type       "
        wi_cd            TYPE sww_cd,            "Creation Date of WI  "
        wi_ct            TYPE sww_ct,            "Creation Time of WI  "
        wi_aagent        TYPE sww_aagent,        "Actual Agent of WI   "
        wi_rh_task       TYPE sww_task,          "Task ID              "
       END   OF ty_wa_swwwihead,

       ty_it_swwwihead   TYPE STANDARD TABLE OF ty_wa_swwwihead.

*eject
TYPES: BEGIN OF ty_wa_int_item,                  "Item Internal Data   "
        bukrs            TYPE bukrs,             "01-Company Number    "
        belnr            TYPE belnr_d,           "02-Accnt Doc.Number  "
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Accounting Doc Item  "
        koart            TYPE koart,             "Account Type         "
        lifnr            TYPE lifnr,             "03-Vendor Number     "
        ktokk            TYPE ktokk,             "04-Vendor Accnt Group"
        name1            TYPE name1_gp,          "05-Vendor Name 1     "
        land1            TYPE land1_gp,          "06-Country           "
        stras            TYPE stras_gp,          "07-Street            "
        ort01            TYPE ort01_gp,          "08-City              "
        regio            TYPE regio,             "09-Region/State/Prov."
        pstlz            TYPE pstlz,             "10-Postal Code       "
        mindk            TYPE mindk,             "11-Minority Indicator"
        budat            TYPE budat,             "12-Posting Date      "
        dmbtr_total      TYPE dmbtr,             "13-Amount Total      "
        augdt            TYPE augdt,             "14-Clearing Date     "
        paymeth          TYPE char8,             "15-Payment Method    "
        xblnr            TYPE xblnr1,            "16-Ref.Doc.Number    "
        bldat            TYPE bldat,             "17-Document Date     "
        saknr            TYPE saknr,             "18-G/L Account       "
        hkont            TYPE hkont,             "19-Line Item Account "
        txt20            TYPE txt20_skat,        "20-G/L Accnt Descrptn"
        sgtxt            TYPE sgtxt,             "21-Line Item Text    "
        dmbtr            TYPE dmbtr,             "22-Amount Line Item  "
        posid            TYPE char24,            "23-WBS Element       "
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*        ktext            TYPE anla-txt50, "Need this field at the end
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
        taxtext          TYPE char50,            "24-Tax Code + Desc.  "
        filler1          TYPE char3,             "25-Filler 1          "
        port01           TYPE ort01,             "26-Ship-To Destinatn "
        pur_ord          TYPE char12,            "27-Purchase Order    "
        zterm            TYPE dzterm,            "28-Payment Method    "
        chect            TYPE chect,             "29-Check or EFT Numbr"
        waers            TYPE waers,             "30-Currency Code     "
        datasrce         TYPE char4,             "31-Data Src.East/West"
        ltext            TYPE text_bslt,         "32-PstKey Code + Desc"
        bktxt            TYPE bktxt,             "33-Doc.Header Text   "
        blart            TYPE blart,             "34-Document Type     "
        ebelp            TYPE ebelp,             "35-Pur.Ord. Item Nmbr"
        matkl            TYPE matkl,             "36-Material Group    "
        matdesc          TYPE char50,            "37-Material Descriptn"
        uzawe            TYPE uzawe,             "38-Paymnt Meth Suplmnt
        konnr            TYPE konnr,             "39-Purchase Agreement"
        kostl            TYPE kostl,             "40-Cost Center       "
        erdat            TYPE erdat_rf,          "41-Vendor Change Date"
        netdt            TYPE faedt_fpos,        "42-Net Due Date      "
        sknto_total      TYPE sknto,             "Cash Discount Amount "
        approver         TYPE char12,            "44-Non-PO Invc Approvr
        bvorg            TYPE bvorg,             "45-Cross-Company Code"
        usnam            TYPE usnam,             "User Name            "
        bstyp            TYPE ebstyp,            "Purchasing Doc.Categry
        bsart            TYPE esart,             "Purchasing Doc.Type  "
        matnr            TYPE matnr,             "Material Number      "
        werks            TYPE ewerk,             "Plant                "
        grdat            TYPE datum,             "Goods Receipt Date   "
        wepos            TYPE wepos,             "Goods Receipt Indicatr
        repos            TYPE repos,             "Invoice Receipt Ind  "
        webre            TYPE webre,             "GR-IR Invoice Verif  "
        buk_s1           TYPE bukrs,             "Company Code     Sort"
        bel_s1           TYPE belnr_d,           "Accounting Doc   Sort"
        gja_s1           TYPE gjahr,             "Fiscal Year      Sort"
        voc_ind          TYPE char1,             "Vendor Output Control"
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
        konty            TYPE char10,
*BOC by KMB on 16.05.2019 ENHC0025289 Power Advocate Additional fields
*        recvr            TYPE char12,
        recvr            TYPE char20,
*EOC by KMB on 16.05.2019 ENHC0025289 Power Advocate Additional fields
        iddes            TYPE char50,
        setl_amnt        TYPE dmbtr,
        ord_qty          TYPE ekpo-menge,
        ord_unit         TYPE char10,
        net_prc          TYPE ekpo-netpr,
        pgrp             TYPE ekgrp,
        pgrp_desc        TYPE eknam,
        plant_code       TYPE werks,
        plant_code_desc  TYPE name1,
        order_crea_dt    TYPE aedat,
        ship_adrc        TYPE adrc-street,
        ship_city        TYPE adrc-city1,
        ship_state       TYPE adrc-region,
        ship_zip         TYPE adrc-post_code1,
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
        ktext            TYPE anla-txt50,
        set_desc         TYPE char50,
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
       END   OF ty_wa_int_item,

        ty_it_int_item   TYPE STANDARD TABLE OF ty_wa_int_item.

*eject
TYPES: BEGIN OF ty_wa_out_item,                  "Item Output Data     "
        bukrs            TYPE char7,             "01 Company Number    "
        belnr            TYPE belnr_d,           "02-Accnt Doc.Number  "
        lifnr            TYPE lifnr,             "03-Vendor Number     "
        ktokk            TYPE char10,            "04-Vendor Accnt Group"
        name1            TYPE name1_gp,          "05-Vendor Name 1     "
        land1            TYPE char5,             "06-Country           "
        stras            TYPE stras_gp,          "07-Street            "
        ort01            TYPE ort01_gp,          "08-City              "
        regio            TYPE char9,             "09-Region/State/Prov."
        pstlz            TYPE char11,            "10-Postal Code       "
        mindk            TYPE char34,            "11-Minority Indicator"
        budat            TYPE char10,            "12-Posting Date      "
        dmbtr_total      TYPE char16,            "13-Amount Total      "
        augdt            TYPE char13,            "14-Clearing Date     "
        paymeth          TYPE char9,             "15-Payment Method    "
        xblnr            TYPE char17,            "16-Ref.Doc.Number    "
        bldat            TYPE char10,            "17-Document Date     "
        saknr            TYPE saknr,             "18-G/L Account       "
        hkont            TYPE hkont,             "19-Line Item Account "
        txt20            TYPE txt20_skat,        "20-G/L Accnt Descrptn"
        sgtxt            TYPE sgtxt,             "21-Line Item Text    "
        dmbtr            TYPE char16,            "22-Amount Line Item  "
        posid            TYPE char24,            "23-WBS Element       "
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*        ktext            TYPE anla-txt50, "Need this field at the end
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
        taxtext          TYPE char50,            "24-Tax Code + Desc.  "
        filler1          TYPE char3,             "25-Filler 1          "
        port01           TYPE ort01,             "26-Ship-To Destinatn "
        pur_ord          TYPE char12,            "27-Purchase Order    "
        zterm            TYPE char9,             "28-Payment Method    "
        chect            TYPE chect,             "29-Check or EFT Numbr"
        waers            TYPE char8,             "30-Currency Code     "
        datasrce         TYPE char11,            "31-Data Src.East/West"
        ltext            TYPE text_bslt,         "32-PstKey Code + Desc"
        bktxt            TYPE bktxt,             "33-Doc.Header Text   "
        blart            TYPE char23,            "34-Document Type     "
        ebelp            TYPE char11,            "35-Pur.Ord. Item Nmbr"
        matkl            TYPE char30,            "36-Material Group    "
        matdesc          TYPE char50,            "37-Material Descriptn"
        uzawe            TYPE char15,            "38-Paymnt Meth Suplmnt
        konnr            TYPE konnr,             "39-OLA Purch Agreement
        kostl            TYPE char11,            "40-Cost Center       "
        erdat            TYPE char18,            "41-Vendor Change Date"
        netdt            TYPE char12,            "42-Net Due Date      "
        sknto_total      TYPE char16,            "Cash Discount Amount "
        approver         TYPE char16,            "44-Non-PO Invc Approvr
        doc_key          TYPE char20,            "45-Accnt Doc.Key     "
        bvorg            TYPE char17,            "46-Cross-Company Code"
*BOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
        konty            TYPE char16,
*BOC by KMB on 16.05.2019 ENHC0025289 Power Advocate Additional fields
*        recvr            TYPE char16,
        recvr            TYPE char20,
*EOC by KMB on 16.05.2019 ENHC0025289 Power Advocate Additional fields
        iddes            TYPE char50,
        setl_amnt        TYPE char16,
        ord_qty          TYPE char16,
        ord_unit         TYPE char10,
        net_prc          TYPE char16,
        pgrp             TYPE char6,
        pgrp_desc        TYPE char18,
        plant_code       TYPE char10,
        plant_code_desc  TYPE char30,
        order_crea_dt    TYPE char20,
        ship_adrc        TYPE stras_gp,
        ship_city        TYPE ort01_gp,
        ship_state       TYPE char13,
        ship_zip         TYPE char11,
*BOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
        ktext            TYPE anla-txt50,
        set_desc         TYPE char50,
*EOC by KMB on 15.04.2019 ENHC0025289 Power Advocate Additional fields
*EOC by KMB on 07.03.2019 CHG0133164 Power Advocate Additional fields
       END   OF ty_wa_out_item,

        ty_it_out_item   TYPE STANDARD TABLE OF ty_wa_out_item.

*eject
TYPES: BEGIN OF ty_wa_out_po,                    "PurchOrd Output Data "
        pur_ord          TYPE char12,            "01-Purchase Order    "
        grdat            TYPE char10,            "02-Goods Receipt Date"
        ebelp            TYPE char11,            "03-Pur.Ord. Item Nmbr"
        matkl            TYPE char30,            "04-Material Grp Desc."
        batxt            TYPE char30,            "05-PO Type Descriptn "
        matdesc          TYPE char50,            "06-Material Descriptn"
        wepos            TYPE char7,             "07-Goods Receipt Ind "
        repos            TYPE char7,             "08-Invoice Receipt Ind
        webre            TYPE char12,            "09-GR-IR Invoice Verif
       END   OF ty_wa_out_po,

        ty_it_out_po     TYPE STANDARD TABLE OF ty_wa_out_po.

TYPES: BEGIN OF ty_wa_out_doc,                   "Document Output Data "
        belnr            TYPE belnr_d,           "01-Accnt Doc.Number  "
        blart            TYPE char23,            "02-Document Type     "
        usnam            TYPE usnam,             "03-User Name         "
        doc_key          TYPE char20,            "04-Accnt Doc.Key     "
        bvorg            TYPE char17,            "05-Cross-Company Code"
       END   OF ty_wa_out_doc,

        ty_it_out_doc    TYPE STANDARD TABLE OF ty_wa_out_doc.

TYPES: BEGIN OF ty_wa_arch_step,                 "Archive Step         "
        step_nbr         TYPE numc2,             "Step Number          "
        step_inst        TYPE z_varvaluel,       "Step Instruction     "
        fpath_in         TYPE localfile,         "File Path In         "
        fname_in         TYPE localfile,         "File Name In         "
        fpath_out        TYPE localfile,         "File Path Out        "
        fname_out        TYPE localfile,         "File Name Out        "
       END   OF ty_wa_arch_step,

        ty_it_arch_step  TYPE STANDARD TABLE OF ty_wa_arch_step.

TYPES: BEGIN OF ty_wa_arch_inst,                 "Archive Instruction  "
        step_nbr         TYPE numc2,             "Step Nbr-Source File "
        step_cmnd        TYPE char10,            "Step Type            "
        step_inst        TYPE z_varvaluel,       "Step Instruction     "
       END   OF ty_wa_arch_inst.

*eject
TYPES: BEGIN OF ty_wa_msg,                       "Messages             "
        nbr_file         TYPE numc5,             "File Sequence Number "
        rc               TYPE numc5,             "Return Code          "
        msgid            TYPE symsgid,           "Message Class        "
        msgty            TYPE symsgty,           "Message Type         "
        msgno            TYPE symsgno,           "Message Number       "
        msgv1            TYPE symsgv,            "Message Variable 1   "
        msgv2            TYPE symsgv,            "Message Variable 2   "
        msgv3            TYPE symsgv,            "Message Variable 3   "
        msgv4            TYPE symsgv,            "Message Variable 4   "
        text             TYPE text240,           "Text                 "
       END   OF ty_wa_msg.

TYPES:  ty_it_msg        TYPE STANDARD TABLE OF ty_wa_msg.

TYPES:  ty_rt_blart      TYPE RANGE OF blart.    "Document Type        "

TYPES:  ty_rt_hkont      TYPE RANGE OF hkont.    "G/L Account          "

TYPES:  ty_rt_bwart      TYPE RANGE OF bwart.    "Movement Type        "

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_c             TYPE char1              "C / Character        "
                         VALUE 'C',
        gc_e             TYPE char1              "E / Error            "
                         VALUE 'E',
        gc_i             TYPE char1              "I / Informational    "
                         VALUE 'I',
        gc_n             TYPE char1              "N / Numerc           "
                         VALUE 'N',
        gc_x             TYPE char1              "X / Yes / True       "
                         VALUE 'X',
        gc_object_id     TYPE programm           "Object ID            "
                         VALUE 'ZFAPI008_POWERADVOCATE',
        gc_object_title  TYPE z_varname          "Object Title         "
                         VALUE 'OBJECT_TITLE',
        gc_doc_type_incl TYPE z_varname          "Document Type Inclusn"
                         VALUE 'DOCUMENT_TYPE_INCLUSION',
        gc_doc_type_excl TYPE z_varname          "Document Type Exclusn"
                         VALUE 'DOCUMENT_TYPE_EXCLUSION',
        gc_doc_type_voc3 TYPE z_varname          "Document Type Ven Out'
                         VALUE 'DOCUMENT_TYPE_VENDOR_OUT',
        gc_ret_acct_excl TYPE z_varname          "Retainage Acct Exclusn
                         VALUE 'RETAINAGE_ACCOUNT_EXCL',
        gc_file1         TYPE z_varname          "File 1               "
                         VALUE 'FILE_1',
        gc_file2         TYPE z_varname          "File 2               "
                         VALUE 'FILE_2',
        gc_file3         TYPE z_varname          "File 3               "
                         VALUE 'FILE_3',
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP',
        gc_modif_id_dlm  TYPE char3              "ModifID-Delimiter    "
                         VALUE 'DLM',
        gc_modif_id_fpt  TYPE char3              "ModifID-Input Filepath
                         VALUE 'FPT'.

*eject
CONSTANTS:
        gc_datsrc_id     TYPE vrm_id             "Data Source Parameter"
                         VALUE 'P_DATSRC',
        gc_datsrc_us     TYPE char4              "Data Source-US       "
                         VALUE 'US',
        gc_datsrc_ug     TYPE char4              "Data Source-UG       "
                         VALUE 'EAST',
        gc_datsrc_sw     TYPE char4              "Data Source-SW       "
                         VALUE 'WEST',
        gc_delim_id      TYPE vrm_id             "Delimiter Parameter  "
                         VALUE 'P_DELIM',
        gc_pipe          TYPE char1              "Pipe "|"             "
                         VALUE '|',
        gc_pipe_lit      TYPE char4              "Delimiter-Pipe-Literal
                         VALUE 'PIPE',
        gc_comma         TYPE char1              "Comma ","            "
                         VALUE ',',
        gc_comma_lit     TYPE char5              "Delimiter-Comma-Literl
                         VALUE 'COMMA',
        gc_tab           TYPE char1              "Tab Character        "
                         VALUE cl_abap_char_utilities=>horizontal_tab,
        gc_tab_lit       TYPE char5              "Delimiter-TAB-Literal"
                         VALUE 'TAB',
        gc_paymeth_ach   TYPE char8              "Payment Method-ACH   "
                         VALUE 'ACH',
        gc_paymeth_wir   TYPE char8              "Payment Method-Wire  "
                         VALUE 'WIRE',
        gc_paymeth_chk   TYPE char8              "Payment Method-Check "
                         VALUE 'CHEQUE',
        gc_paymeth_eft   TYPE char8              "Payment Method-EFT   "
                         VALUE 'EFT',
        gc_paymeth_epy   TYPE char8              "Payment method-EPaybls
                         VALUE 'EPAYABLS',
        gc_paymeth_man   TYPE char8              "Payment Method-Manual"
                         VALUE 'MANUAL',
        gc_paymeth_rev   TYPE char8              "Payment Method-Reversl
                         VALUE 'REVERSAL'.

*eject
************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_delim         TYPE char1,             "Delimiter            "
        gv_obj_id        TYPE programm,          "Object ID            "
        gv_obj_title     TYPE sytitle,           "Object Title         "
        gv_flag_pres     TYPE flag,              "Flag-PresServer-Output
        gv_phy_file1     TYPE text255,           "Physical File 1      "
        gv_phy_file2     TYPE text255,           "Physical File 2      "
        gv_phy_file3     TYPE text255.           "Physical File 3      "

DATA:   gv_cnt_rec_file1 TYPE numc7,             "Count-Records-File 1 "
        gv_cnt_rec_file2 TYPE numc7,             "Count-Records-File 2 "
        gv_cnt_rec_file3 TYPE numc7,             "Count-Records-File 3 "
        gv_count_err_data                        "Count-Errors-Data    "
                         TYPE numc7,
        gv_count_err_proc                        "Count-Errors-Process "
                         TYPE numc7,
        gv_flag_err_data TYPE flag,              "Flag-Errors-Data     "
        gv_flag_err_proc TYPE flag,              "Flag-Errors-Process  "
        gv_flag_err_mstr TYPE flag,              "Flag-Errors-Master   "
        gv_flag_err_vldt TYPE flag.              "Flag-Errors-Validatn "

DATA:   gv_sysid         TYPE sysysid,           "Name Of The SAP System
        gv_uname         TYPE syuname,           "User Name            "
        gv_pagno         TYPE sypagno,           "Current List Page    "
        gv_cprog         TYPE sycprog,           "Calling Program      "
        gv_datum         TYPE sydatum,           "Current Date of Applic
        gv_uzeit         TYPE syuzeit.           "Current Time of Applic

*eject
************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gwa_t001         TYPE ty_wa_t001.        "Company              "

DATA:   gwa_t003         TYPE ty_wa_t003.        "Document Types       "

DATA:   gwa_t007s        TYPE ty_wa_t007s.       "Tax Code Names       "

DATA:   gwa_t023t        TYPE ty_wa_t023t.       "Material Group Descrpt

DATA:   gwa_t161t        TYPE ty_wa_t161t.       "Purch.Doc.Type Descrpt

DATA:   gwa_tbsl         TYPE ty_wa_tbsl.        "Posting Key          "

DATA:   gwa_skat         TYPE ty_wa_skat.        "G/L Account Text     "

DATA:   gwa_bsak_c       TYPE ty_wa_bsak_c.      "Sec.Index-Vendor Clrd"

DATA:   gwa_vendor       TYPE ty_wa_vendor.      "Vendor               "

DATA:   gwa_ekko         TYPE ty_wa_ekko.        "Purchasing Document  "

DATA:   gwa_ekpo         TYPE ty_wa_ekpo.        "Purchasing Doc.Item  "

DATA:   gwa_po_data      TYPE ty_wa_po_data.     "Purchase Order Data  "

************************************************************************
*                             Range Tables                             *
************************************************************************
DATA:   grt_blart        TYPE ty_rt_blart.       "Document Type        "

DATA:   grt_hkont_ret_v  TYPE ty_rt_hkont.       "G/L Retainage-Vendor "

DATA:   grt_hkont_ret_a  TYPE ty_rt_hkont.       "G/L Retainage-Account"

DATA:   grt_bwart        TYPE ty_rt_bwart.       "Movement Type        "

*eject
************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   git_zvar         TYPE ty_it_zvar.        "Program Variables    "

DATA:   git_t001         TYPE ty_it_t001.        "Company              "

DATA:   git_t001w        TYPE ty_it_t001w.       "Plants/Branches      "

DATA:   git_t003         TYPE ty_it_t003.        "Document Types       "

DATA:   git_t007s        TYPE ty_it_t007s.       "Tax Code Names       "

DATA:   git_t023t        TYPE ty_it_t023t.       "Material Group Descrpt

DATA:   git_t059t        TYPE ty_it_t059t.       "Minority Indicator Txt

DATA:   git_t161t        TYPE ty_it_t161t.       "Purch.Doc.Type Descrpt

DATA:   git_tbsl         TYPE ty_it_tbsl.        "Posting Key          "

DATA:   git_skat         TYPE ty_it_skat.        "G/L Account Text     "

DATA:   git_bsak         TYPE ty_it_bsak.        "Sec.Index-Vendor Clrd"

DATA:   git_doc_prcsd    TYPE ty_it_doc_prcsd.   "Processed Documents  "

DATA:   git_cc_doc       TYPE ty_it_doc_prcsd.   "Cross-Company (InterCo

DATA:   git_bkpf         TYPE ty_it_bkpf.        "Accounting Doc Headers

DATA:   git_bseg         TYPE ty_it_bseg.        "Accounting Doc Items "

DATA:   git_vendor       TYPE ty_it_vendor.      "Vendor               "

DATA:   git_po_data      TYPE ty_it_po_data.     "Purchase Order Data  "

DATA:   git_out_item     TYPE ty_it_out_item.    "Item Output Data     "

DATA:   git_out_po       TYPE ty_it_out_po.      "PurchOrd Output Data "

DATA:   git_out_doc      TYPE ty_it_out_doc.     "Document Output Data "

DATA:   git_msg_data     TYPE ty_it_msg.         "Messages-Data        "

DATA:   git_msg_proc     TYPE ty_it_msg.         "Messages-Process     "

DATA:   git_datsrc_values                        "Data Source Values   "
                         TYPE vrm_values.

DATA:   git_delim_values TYPE vrm_values.        "Delimiter Values     "

**- START OF CHANGES BY AKMADASU
DATA:gv_flag TYPE flag.
**-- END OF CHANGES BY AKMADASU

"BOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data
DATA : lv_flag TYPE flag,
       lv_fg TYPE blart,
       lv_ven TYPE ktokk.
CONSTANTS : lc_param TYPE c LENGTH 17 VALUE 'ZFI_POWERADVOCATE',
            lc_doc TYPE c LENGTH 8 VALUE 'DOC_TYPE',
            lc_1 TYPE c VALUE '1',
            lc_vendor TYPE c LENGTH 10 VALUE 'VENDOR_GRP'.
"EOC by kmb on 11.7.2019 CHG0150958 fixing of  data parsing errors in the output files & Eliminate Employee data

*BOC by KMB on 26.9.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report
CONSTANTS : gc_com TYPE c VALUE ',',
            gc_un  TYPE c VALUE '_'.
*EOC by KMB on 26.9.2019 CHG0161101 DFCT0017698 - PEC : Data parsing issue with Power Advocate report

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************

* Select Options
*ELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-010.
SELECT-OPTIONS:   s_bukrs  FOR bsak-bukrs        "Company Code         "
                           MEMORY ID buk
                           OBLIGATORY.
SELECT-OPTIONS:   s_lifnr  FOR bsak-lifnr.       "Vendor Number        "
SELECT-OPTIONS:   s_augdt  FOR bsak-augdt.       "Clearing Date        "
SELECT-OPTIONS:   s_augbl  FOR bsak-augbl.       "Clearing Document    "
SELECT-OPTIONS:   s_blart  FOR bkpf-blart.       "Document Type        "
SELECT-OPTIONS:   s_blartx FOR bsak-blart        "Document Type Excluded
                           MODIF ID dsp.
PARAMETERS:       cb_xzahl AS CHECKBOX           "PstgKey Used In Paymnt
                           DEFAULT gc_x
                           MODIF ID dsp.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_bukri  FOR bsak-bukrs.       "InterCompany Code    "
SELECT-OPTIONS:   s_belnr  FOR bkpf-belnr.       "Document Number      "
PARAMETERS:       p_gjahr  TYPE gjahr.           "Fiscal Year          "
SELECT-OPTIONS:   s_monat  FOR bkpf-monat.       "Fiscal Period        "
SELECT-OPTIONS:   s_bldat  FOR bkpf-bldat.       "Document Date        "
SELECT-OPTIONS:   s_budat  FOR bkpf-budat.       "Posting Date         "
SELECT-OPTIONS:   s_usnam  FOR bkpf-usnam        "User Name            "
                           NO INTERVALS.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_prctr  FOR bseg-prctr.       "Profit Center        "
SELECT-OPTIONS:   s_kostl  FOR bseg-kostl.       "Cost Center          "
PARAMETERS:       p_ktopl  TYPE ktopl            "Chart-Of-Accounts    "
                           MEMORY ID kpl
                           OBLIGATORY.
SELECT-OPTIONS:   s_hkont  FOR bseg-hkont.       "G/L Account          "
SELECTION-SCREEN: END   OF BLOCK ssb1.

*eject
* Run Options
*ELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-020.
PARAMETERS:       p_datsrc TYPE char4            "US / EAST / WEST     "
                           AS LISTBOX VISIBLE LENGTH 12
                           DEFAULT gc_datsrc_ug
                           MODIF ID dsp.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 01.
PARAMETERS:       cb_glicd AS CHECKBOX           "Delete IC Crssbll Itms
                              DEFAULT gc_x.
SELECTION-SCREEN: COMMENT  03(25) text-022.
SELECTION-SCREEN: POSITION 30.
SELECT-OPTIONS:   s_glic   FOR bseg-hkont        "G/L-IC Crossbill Items
                           MODIF ID dsp.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 01.
PARAMETERS:       cb_glrtd AS CHECKBOX           "Delete Retainage Items
                              DEFAULT gc_x.
SELECTION-SCREEN: COMMENT  03(22) text-023.
SELECTION-SCREEN: POSITION 30.
SELECT-OPTIONS:   s_glrt   FOR bseg-hkont        "G/L-Retainage Items  "
                           MODIF ID dsp.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: END   OF BLOCK ssb2.

*eject
* File Options
*ELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb3 WITH FRAME TITLE text-030.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  01(09) text-031.
SELECTION-SCREEN: POSITION 33.
**-- START OF CHANGES BY AKMADASU FOR
*PARAMETERS:       p_objid  TYPE programm         "Object ID            "
*                           VISIBLE LENGTH 22
*                           DEFAULT gc_object_id
*                           MODIF ID dsp.

PARAMETERS:       p_objid  TYPE programm         "Object ID            "
                           VISIBLE LENGTH 22
                           DEFAULT gc_object_id OBLIGATORY.
**-- END OF CHANGES BY AKMADASU FOR
SELECTION-SCREEN: POSITION 58.
PARAMETERS:       p_objttl TYPE sytitle          "Object Title         "
                           MODIF ID dsp.
SELECTION-SCREEN: END   OF LINE.
PARAMETERS:       rb_appl  RADIOBUTTON GROUP rbg1  "Application Server "
                           DEFAULT 'X'
                           USER-COMMAND cmd.
PARAMETERS:       rb_pres  RADIOBUTTON GROUP rbg1. "Presentation Server"
PARAMETERS:       p_delim  TYPE zparamkey        "File Delimiter
                           AS LISTBOX VISIBLE LENGTH 12
                           DEFAULT gc_comma_lit
                           MODIF ID dlm.
PARAMETERS:       p_fpath1 TYPE localfile        "Filepath-Master Data "
                           MODIF ID fpt.
PARAMETERS:       p_fpa1nd TYPE localfile        "Filename-Master Data "
                           NO-DISPLAY.
PARAMETERS:       p_fpath2 TYPE localfile        "Filepath-PO Data     "
                           MODIF ID fpt.
PARAMETERS:       p_fpa2nd TYPE localfile        "Filename-PO Data     "
                           NO-DISPLAY.
PARAMETERS:       p_fpath3 TYPE localfile        "Filepath-Doc Data    "
                           MODIF ID fpt.
PARAMETERS:       p_fpa3nd TYPE localfile        "Filename-Doc Data    "
                           NO-DISPLAY.
SELECTION-SCREEN: END   OF BLOCK ssb3.
