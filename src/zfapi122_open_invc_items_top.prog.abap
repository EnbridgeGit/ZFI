*&---------------------------------------------------------------------*
*&  Include           ZFAPI122_OPEN_INVC_ITEMS_TOP
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI122_OPEN_INVC_ITEMS                       *
* Include Program    :  ZFAPI122_OPEN_INVC_ITEMS_TOP                   *
* Author             :  Paul Karunakar                                 *
* Creation Date      :  16-Apr-2018                                    *
* Application Area   :  FICO                                           *
* Description        :  Open Invoice data from each of the three SAP   *
*                       instances will be extracted in a delimited     *
*                       file and sent to IAP.                          *
*                                                                      *
*&---------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 16-Apr-2018  KPAL        D30K928646  CHG0108943  Initial Development *
*                          D30K928913, D30K929061                      *
*----------------------------------------------------------------------*

TYPE-POOLS: vrm.

TABLES: sscrfields, bsik, lfa1.

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF gty_bsik_key,               "BSIK Key                  "
        bukrs            TYPE bukrs,        "Company Code              "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        gjahr            TYPE gjahr,        "Fiscal Year               "
       END   OF gty_bsik_key.

TYPES:  gtt_bsik_key     TYPE STANDARD TABLE OF gty_bsik_key.

TYPES: BEGIN OF gty_bvorg,                  "Cross Company Document Key"
        bvorg            TYPE bvorg,        "Cross Company Document Key"
       END   OF gty_bvorg.

TYPES:  gtt_bvorg        TYPE STANDARD TABLE OF gty_bvorg.

TYPES: BEGIN OF gty_bkpf,                   "BKPF                      "
        bukrs            TYPE bukrs,        "Company Code              "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        gjahr            TYPE gjahr,        "Fiscal Year               "
        blart            TYPE blart,        "Document Type             "
        bldat            TYPE bldat,        "Document Date             "
        budat            TYPE budat,        "Posting Date              "
        monat            TYPE monat,        "Fiscal Period             "
        cpudt            TYPE cpudt,        "Entry Date                "
        usnam            TYPE usnam,        "User Name                 "
        tcode            TYPE tcode,        "Transaction Code          "
        bvorg            TYPE bvorg,        "Cross-Company Doc. Number "
        xblnr            TYPE xblnr1,       "Reference Document Number "
        bktxt            TYPE bktxt,        "Document Header Text      "
        waers            TYPE waers,        "Currency Key              "
        kursf            TYPE kursf,        "Exchange Rate             "
        bstat            TYPE bstat_d,      "Document Status           "
        awtyp            TYPE awtyp,        "Reference Transaction     "
        awkey            TYPE awkey,        "Reference Key             "
        xmwst            TYPE xmwst,        "Calculate Tax             "
        xreversal        TYPE xreversal,    "Reversed/Reversal Indicator
*        ritm             TYPE z_iap_ritm_doc, "IAP RITM Document ID    "
       END   OF gty_bkpf.

TYPES:  gtt_bkpf         TYPE STANDARD TABLE OF gty_bkpf.

*eject
TYPES: BEGIN OF gty_bseg,                   "BSEG                      "
        bukrs            TYPE bukrs,        "Company Code              "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        gjahr            TYPE gjahr,        "Fiscal Year               "
        buzei            TYPE buzei,        "Accounting Doc. Item Number
        bschl            TYPE bschl,        "Posting Key               "
        koart            TYPE koart,        "Account Type              "
        umskz            TYPE umskz,        "Special G/L Indicator     "
        shkzg            TYPE shkzg,        "Debit/Credit Indicator    "
        mwskz            TYPE mwskz,        "Tax Code                  "
        wrbtr            TYPE wrbtr,        "Amount in Document Currency
        wmwst            TYPE wmwst,        "Tax Amount in Doc. Currency
        qsshb            TYPE qsshb,        "Withhldng Tax Base Amount "
        sgtxt            TYPE sgtxt,        "Item Text                 "
        saknr            TYPE saknr,        "G/L Account               "
        lifnr            TYPE lifnr,        "Vendor Account Number     "
        zfbdt            TYPE dzfbdt,       "Baseline Date             "
        zterm            TYPE dzterm,       "Payment Terms             "
        zbd1t            TYPE dzbd1t,       "Cash Discount Days 1      "
        zbd2t            TYPE dzbd2t,       "Cash Discount Days 2      "
        zbd3t            TYPE dzbd3t,       "Net Payment Terms Period  "
        skfbt            TYPE skfbt,        "Cash Discount Base Amount "
        wskto            TYPE wskto,        "Cash Discount Amount in Doc
        zlsch            TYPE schzw_bseg,   "Payment Method            "
        zlspr            TYPE dzlspr,       "Payment Block             "
        hbkid            TYPE hbkid,        "House Bank                "
        bvtyp            TYPE bvtyp,        "Partner Bank Type         "
        qbshb            TYPE qbshb,        "Withhldng Tax Amount in Doc
        uzawe            TYPE uzawe,        "Payment Method Supplement "
        empfb            TYPE empfb,        "Payee/Payer               "
       END   OF gty_bseg.

TYPES:  gtt_bseg         TYPE STANDARD TABLE OF gty_bseg.

*eject
TYPES: BEGIN OF gty_vbsegk_key,             "VBSEGK Key                "
        ausbk            TYPE ausbk,        "Source Company Code       "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        gjahr            TYPE gjahr,        "Fiscal Year               "
      END   OF gty_vbsegk_key.

TYPES:  gtt_vbsegk_key   TYPE STANDARD TABLE OF gty_vbsegk_key.

TYPES: BEGIN OF gty_vbkpf,                  "VBKPF                     "
        ausbk            TYPE ausbk,        "Source Company Code       "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        gjahr            TYPE gjahr,        "Fiscal Year               "
        bukrs            TYPE bukrs,        "Company Code              "
        bstat            TYPE bstat_d,      "Document Status           "
        blart            TYPE blart,        "Document Type             "
        bldat            TYPE bldat,        "Document Date             "
        budat            TYPE budat,        "Posting Date              "
        monat            TYPE monat,        "Fiscal Period             "
        cpudt            TYPE cpudt,        "Entry Date                "
        usnam            TYPE usnm_vbkpf,   "User Name                 "
        tcode            TYPE tcode,        "Transaction Code          "
        bvorg            TYPE bvorg,        "Cross-Company Document No."
        xblnr            TYPE xblnr1,       "Reference Document Number "
        bktxt            TYPE bktxt,        "Document Header Text      "
        waers            TYPE waers,        "Currency Key              "
        kursf            TYPE kursf,        "Exchange Rate             "
        xmwst            TYPE xmwst,        "Calculate Tax             "
        awtyp            TYPE awtyp,        "Reference Transaction     "
        awkey            TYPE awkey,        "Reference Key             "
       END   OF gty_vbkpf.

TYPES:  gtt_vbkpf        TYPE STANDARD TABLE OF gty_vbkpf.

*eject
TYPES: BEGIN OF gty_vbsegk,                 "VBSEGK                    "
        ausbk            TYPE ausbk,        "Source Company Code       "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        gjahr            TYPE gjahr,        "Fiscal Year               "
        bzkey            TYPE buzei,        "Accounting Doc. Item Number
        bukrs            TYPE bukrs,        "Company Code              "
        buzei            TYPE buzei,        "Accounting Doc. Item Number
        bschl            TYPE bschl,        "Posting Key               "
        umskz            TYPE umskz,        "Special G/L Indicator     "
        shkzg            TYPE shkzg,        "Debit/Credit Indicator    "
        mwskz            TYPE mwskz,        "Tax Code                  "
        wrbtr            TYPE wrbtr,        "Amount in Document Currency
        wmwst            TYPE wmwst,        "Tax Amount in Doc. Currency
        qsshb            TYPE qsshb,        "Withhldng Tax Base Amount "
        qbshb            TYPE qbshb,        "Withhldng Tax Amount in Doc
        sgtxt            TYPE sgtxt,        "Item Text                 "
        hkont            TYPE hkont,        "G/L Account               "
        lifnr            TYPE lifnr,        "Vendor Account Number     "
        zfbdt            TYPE dzfbdt,       "Baseline Date             "
        zterm            TYPE dzterm,       "Payment Terms             "
        zbd1t            TYPE dzbd1t,       "Cash Discount Days 1      "
        zbd2t            TYPE dzbd2t,       "Cash Discount Days 2      "
        zbd3t            TYPE dzbd3t,       "Net Payment Terms Period  "
        skfbt            TYPE skfbt,        "Cash Discount Base Amount "
        wskto            TYPE wskto,        "Cash Discount Amount in Doc
        zlsch            TYPE schzw_bseg,   "Payment Method            "
        zlspr            TYPE dzlspr,       "Payment Block             "
        uzawe            TYPE uzawe,        "Payment Method Supplement "
        hbkid            TYPE hbkid,        "House Bank                "
        bvtyp            TYPE bvtyp,        "Partner Bank Type         "
        empfb            TYPE empfb,        "Payee/Payer               "
       END   OF gty_vbsegk.

TYPES:  gtt_vbsegk       TYPE STANDARD TABLE OF gty_vbsegk.

*eject
TYPES: BEGIN OF gty_wtht,                   "Withholding Tax Item      "
        bukrs            TYPE bukrs,        "Company Code              "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        gjahr            TYPE gjahr,        "Fiscal Year               "
        buzei            TYPE buzei,        "Accounting Doc. Item Number
        witht            TYPE witht,        "Withholding Tax Type      "
        wt_withcd        TYPE wt_withcd,    "Withholding Tax Code      "
        wt_qsshb         TYPE wt_bs1,       "Withhldng Base in Doc. Curr
        wt_qbshb         TYPE wt_wt1,       "Withhldng Tax in Doc. Curr.
       END   OF gty_wtht.

TYPES:  gtt_wtht         TYPE STANDARD TABLE OF gty_wtht.

TYPES: BEGIN OF gty_wht,                    "Withholding Tax           "
        witht            TYPE witht,        "Withholding Tax Type      "
        wt_withcd        TYPE wt_withcd,    "Withholding Tax Code      "
        text40           TYPE text40,       "Text                      "
        wt_qsshb         TYPE wt_bs1,       "Withhldng Base in Doc. Curr
        wt_qbshb         TYPE wt_wt1,       "Withhldng Tax in Doc. Curr.
        wt_qbshb_abs     TYPE wt_wt1,       "Withhldng Tax in Doc. Curr.
       END   OF gty_wht.

TYPES:  gtt_wht          TYPE STANDARD TABLE OF gty_wht.

TYPES: BEGIN OF gty_t001,                   "T001                      "
        bukrs            TYPE bukrs,        "Company Code              "
        land1            TYPE land1,        "Country Key               "
       END   OF gty_t001.

TYPES:  gtt_t001         TYPE STANDARD TABLE OF gty_t001.

TYPES: BEGIN OF gty_t059u,                  "T059U                     "
        land1            TYPE land1,        "Country Key               "
        witht            TYPE witht,        "Withholding Tax Type      "
        text40           TYPE text40,       "Text                      "
       END   OF gty_t059u.

TYPES:  gtt_t059u        TYPE STANDARD TABLE OF gty_t059u.

*eject
TYPES: BEGIN OF gty_header,                 "Header Data               "
        ausbk            TYPE ausbk,        "Source Company Code       "
        bukrs            TYPE bukrs,        "Company Code              "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        gjahr            TYPE gjahr,        "Fiscal Year               "
        blart            TYPE blart,        "Document Type             "
        bldat            TYPE bldat,        "Document Date             "
        budat            TYPE budat,        "Posting Date              "
        monat            TYPE monat,        "Fiscal Period             "
        cpudt            TYPE cpudt,        "Entry Date                "
        usnam            TYPE usnam,        "User Name                 "
        tcode            TYPE tcode,        "Transaction Code          "
        bvorg            TYPE bvorg,        "Cross-Company Doc. Number "
        xblnr            TYPE xblnr1,       "Reference Document Number "
        bktxt            TYPE bktxt,        "Document Header Text      "
        waers            TYPE waers,        "Currency Code             "
        kursf            TYPE kursf,        "Exchange Rate             "
        bstat            TYPE bstat_d,      "Document Status           "
        awtyp            TYPE awtyp,        "Reference Transaction     "
        awkey            TYPE awkey,        "Reference Key             "
        xmwst            TYPE xmwst,        "Calculate Tax Automatically
        xreversal        TYPE xreversal,    "Reversed/Reversal Indicator
*        ritm             TYPE z_iap_ritm_doc, "IAP RITM Document ID    "
       END   OF gty_header.

*eject
TYPES: BEGIN OF gty_vendor,                 "Vendor Data               "
        ausbk            TYPE ausbk,        "Source Company Code       "
        bukrs            TYPE bukrs,        "Company Code              "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        gjahr            TYPE gjahr,        "Fiscal Year               "
        buzei            TYPE buzei,        "Accoutning Document Item  "
        bschl            TYPE bschl,        "Posting Key               "
        koart            TYPE koart,        "Account Type              "
        umskz            TYPE umskz,        "Special G/L Indicator     "
        shkzg            TYPE shkzg,        "Debit/Credit Indicator    "
        mwskz            TYPE mwskz,        "Tax Code                  "
        wrbtr            TYPE wrbtr,        "Amount in Document Currency
        wmwst            TYPE wmwst,        "Tax Amount in Doc. Currency
        qsshb            TYPE qsshb,        "Withholding Tax Base Amount
        sgtxt            TYPE sgtxt,        "Item Text                 "
        saknr            TYPE saknr,        "G/L Account Number        "
        lifnr            TYPE lifnr,        "Vendor Account Number     "
        zfbdt            TYPE dzfbdt,       "Baseline Date             "
        zterm            TYPE dzterm,       "Payment Terms             "
        zbd1t            TYPE dzbd1t,       "Cash Discount Days 1      "
        zbd2t            TYPE dzbd2t,       "Cash Discount Days 2      "
        zbd3t            TYPE dzbd3t,       "Net Payment Terms Period  "
        skfbt            TYPE skfbt,        "Eligible Cash Disc. in Doc.
        wskto            TYPE wskto,        "Cash Discount in Doc. Curr.
        zlsch            TYPE schzw_bseg,   "Payment Method            "
        zlspr            TYPE dzlspr,       "Payment Block Key         "
        hbkid            TYPE hbkid,        "House Bank                "
        bvtyp            TYPE bvtyp,        "Partner Bank Type         "
        qbshb            TYPE qbshb,        "Withhldng Tax Amt. in Doc.
        uzawe            TYPE uzawe,        "Payment Method Supplement "
        empfb            TYPE empfb,        "Payee/Payer               "
       END   OF gty_vendor.

*eject
TYPES: BEGIN OF gty_rec,                    "Record                    "
        erpid            TYPE char05,       "ERP_Id                    "
        inv_id           TYPE char20,       "Invoice_Id                "
        cc_inv_id        TYPE char20,       "Cross_Company_Invoice_Id  "
        ritm             TYPE char15,       "RITM                      "
        bukrs            TYPE bukrs,        "Bill_To_Company           "
        lifnr            TYPE lifnr,        "Supplier                  "
        umskz            TYPE umskz,        "Special_GL_Indicator      "
        blart            TYPE blart,        "Document_Type             "
        tcode            TYPE tcode,        "Transaction               "
        xblnr            TYPE xblnr1,       "Invoice_Number            "
        bldat            TYPE char10,       "Invoice_Date              "
        budat            TYPE char10,       "Posting_Date              "
        monat            TYPE char2,        "Period                    "
        pay_date         TYPE char1,        "Payment_Date              "
        last_update      TYPE char1,        "Last_Update_In_ERP        "
        belnr            TYPE belnr_d,      "ERP_Document_Number       "
        wrbtr            TYPE char16,       "Invoice_Gross_Amount      "
        waers            TYPE waers,        "Currency_Code             "
        due_date         TYPE char10,       "Due_Date                  "
        zfbdt            TYPE char10,       "Baseline_Date             "
        bktxt            TYPE bktxt,        "Header_Text               "
        bstat            TYPE bstat_d,      "ERP_Status                "
        reversed         TYPE char1,        "Reversed                  "
        xmwst            TYPE xmwst,        "Calculate_Tax             "
        wmwst            TYPE char16,       "Tax_Amount                "
        mwskz            TYPE mwskz,        "Tax_Code                  "
        wht_name_1       TYPE text40,       "WHT_Name_1                "
        wht_type_1       TYPE witht,        "WHT_Type_1                "
        wht_code_1       TYPE wt_withcd,    "WHT_Code_1                "
        wht_base_1       TYPE char16,       "WHT_Base_1                "
        wht_tax_1        TYPE char16,       "WHT_Tax_1                 "
        wht_name_2       TYPE text40,       "WHT_Name_2                "
        wht_type_2       TYPE witht,        "WHT_Type_2                "
        wht_code_2       TYPE wt_withcd,    "WHT_Code_2                "
        wht_base_2       TYPE char16,       "WHT_Base_2                "
        wht_tax_2        TYPE char16,       "WHT_Tax_2                 "
        empfb            TYPE empfb,        "Payee                     "
        bvtyp            TYPE bvtyp,        "Partner_Bank              "
        hbkid            TYPE hbkid,        "House_Bank                "
        zterm            TYPE dzterm,       "Payment_Terms             "
        wskto            TYPE char16,       "Cash_Discount             "
        skfbt            TYPE char16,       "Discount_Base             "
        zlsch            TYPE dzlsch,       "Payment_Method            "
        uzawe            TYPE uzawe,        "Payment_Method_Supplement "
        zlspr            TYPE dzlspr,       "Payment_Block             "
        unplan_dc        TYPE char1,        "Unplanned_Delivery_Costs  "
        adv_text         TYPE string,       "Payment_Advice_Text       "
        sgtxt_v          TYPE sgtxt,        "Vendor_Item_Description   "
       END   OF gty_rec.

TYPES:  gtt_rec          TYPE STANDARD TABLE OF gty_rec.

*eject
TYPES:  gty_output       TYPE string,       "Output                    "

        gtt_output       TYPE STANDARD TABLE OF gty_output.

************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_delim         TYPE char2 VALUE '|~',
        gc_ucomm_onli    TYPE sscrfields-ucomm VALUE 'ONLI',
        gc_lgcfilpth     TYPE pathintern
                         VALUE 'ZFAPI122_OPEN_INVC_ITEMS',
        gc_filename      TYPE fileintern
                         VALUE 'SSSSS_Open_Items_YYMMDDHHMM_NNN.CSV'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_cn_recs_total TYPE i,
        gv_cn_recs_max   TYPE i,
        gv_cn_recs_file  TYPE i,
        gv_cn_files      TYPE i,
        gv_filename      TYPE text256,
        gv_filename_p    TYPE text256.

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_bsik_key      TYPE gtt_bsik_key,      "BSIK Key             "
        gt_bvorg         TYPE gtt_bvorg,         "Cross Company Doc.Key"
        gt_bkpf          TYPE gtt_bkpf,          "BKPF                 "
        gt_bseg          TYPE gtt_bseg,          "BSEG                 "
        gt_vbsegk_key    TYPE gtt_vbsegk_key,    "VBSEGK Key           "
        gt_vbkpf         TYPE gtt_vbkpf,         "VBKPF                "
        gt_vbsegk        TYPE gtt_vbsegk,        "VBSEGK               "
        gt_wtht          TYPE gtt_wtht,          "WITH_ITEM            "
        gt_t001          TYPE gtt_t001,          "T001                 "
        gt_t059u         TYPE gtt_t059u,         "T059U                "
        gt_output        TYPE gtt_output.        "Output               "
