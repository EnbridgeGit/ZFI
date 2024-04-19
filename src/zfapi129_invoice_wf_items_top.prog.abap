*&---------------------------------------------------------------------*
*&  Include           ZFAPI129_INVOICE_WF_ITEMS_TOP
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI129_INVOICE_WF_ITEMS                      *
* Include Program    :  ZFAPI129_INVOICE_WF_ITEMS_TOP                  *
* Author             :  Vijay Rajaputra                                *
* Creation Date      :  05-Nov-2018                                    *
* Application Area   :  FICO                                           *
* Technical Contact  :  Vijay Rajaputra                                *
*                                                                      *
* Purpose            :  Global Data Elements Include Program           *
*                                                                      *
*----------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 05-Nov-2018  VRAJAPUTRA  D30K929291  CHG0130803  Initial development *
*                          D30K929374                                  *
* 22-Jan-2019  VRAJAPUTRA  D30K929505  CHG0135339  Duplicate checks    *
*                          D30K929531                                  *
* 31-Jan-2019  VRAJAPUTRA  D30K929554  CHG0136094  Duplicate checks    *
*&---------------------------------------------------------------------*

TABLES: bsik, lfa1, toapr, sscrfields.

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  gty_xparam       TYPE zfit_xparam.       "Program Parameters   "

TYPES:  gtt_xparam       TYPE STANDARD TABLE OF gty_xparam.

TYPES: BEGIN OF gty_vbsegk_key,                  "Parked Invoice Key   "
        ausbk            TYPE ausbk,             "Source Company Code  "
        belnr            TYPE belnr_d,           "Accounting Doc. Number
        gjahr            TYPE gjahr,             "Fiscal Year          "
        lifnr            TYPE lifnr,             "Vendor Account Number"
       END   OF gty_vbsegk_key.

TYPES:  gtt_vbsegk_key   TYPE STANDARD TABLE OF gty_vbsegk_key.

TYPES: BEGIN OF gty_vbsegk,                      "Parked Invoice Data  "
        ausbk            TYPE ausbk,             "Source Company Code  "
        belnr            TYPE belnr_d,           "Accounting Doc. Number
        gjahr            TYPE gjahr,             "Fiscal Year          "
        bzkey            TYPE buzei,             "Accounting Doc. Item "
        bukrs            TYPE bukrs,             "Company Code         "
        buzei            TYPE buzei,             "Accounting Doc. Item "
        bschl            TYPE bschl,             "Posting Key          "
        umskz            TYPE umskz,             "Special G/L Indicator"
        umsks            TYPE umsks,             "Special G/L Trans Type
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        mwskz            TYPE mwskz,             "Tax Code             "
        dmbtr            TYPE dmbtr,             "Amount In Local Curr."
        wrbtr            TYPE wrbtr,             "Amount In Doc. Curr. "
        zuonr            TYPE dzuonr,            "Assignment Number    "
        sgtxt            TYPE sgtxt,             "Item Text            "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        zfbdt            TYPE dzfbdt,            "Baseline Date        "
        zterm            TYPE dzterm,            "Payment Terms        "
        zbd1t            TYPE dzbd1t,            "Cash Discount Days 1 "
        zbd2t            TYPE dzbd2t,            "Cash Discount Days 2 "
        zbd3t            TYPE dzbd3t,            "Net Paym. Terms Period
        skfbt            TYPE skfbt,             "Amt Eligible For Disc.
        sknto            TYPE sknto,             "Cash Discount Amnt LC"
        wskto            TYPE wskto,             "Cash Discount Amnt DC"
        zlsch            TYPE schzw_bseg,        "Payment Method       "
        zlspr            TYPE dzlspr,            "Payment Block        "
        xref3            TYPE xref3,             "Reference Key / RCO  "
       END   OF gty_vbsegk.

TYPES:  gtt_vbsegk       TYPE STANDARD TABLE OF gty_vbsegk.

*eject
TYPES: BEGIN OF gty_vbkpf,                       "Parked Accounting Doc"
        ausbk            TYPE ausbk,             "Source Company Code  "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc. Number
        gjahr            TYPE gjahr,             "Fiscal Year          "
        bstat            TYPE bstat_d,           "Document Status      "
        blart            TYPE blart,             "Document Type        "
        bldat            TYPE bldat,             "Document Date        "
        budat            TYPE budat,             "Posting Date         "
        monat            TYPE monat,             "Fiscal Period        "
        cpudt            TYPE cpudt,             "Entry Date           "
        cputm            TYPE cputm,             "Entry Time           "
        aedat            TYPE datum,             "Last Change Date     "
        usnam            TYPE usnam,             "User Name            "
        xblnr            TYPE xblnr1,            "Reference Doc. Number"
        waers            TYPE waers,             "Document Currency Key"
        hwaer            TYPE hwaer,             "Local Currency       "
        kursf            TYPE kursf,             "Exchange Rate        "
        awtyp            TYPE awtyp,             "Reference Transaction"
        awkey            TYPE awkey,             "Reference Key        "
       END   OF gty_vbkpf.

TYPES:  gtt_vbkpf        TYPE STANDARD TABLE OF gty_vbkpf.

TYPES: BEGIN OF gty_bsak_key,                    "Cleared Invoice Key  "
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        augdt            TYPE augdt,             "Clearing Date        "
        augbl            TYPE augbl,             "Clearing Doc. Number "
        gjahr            TYPE gjahr,             "Fiscal Year          "
        belnr            TYPE belnr_d,           "Accounting Doc. Number
       END   OF gty_bsak_key.

TYPES:  gtt_bsak_key     TYPE STANDARD TABLE OF gty_bsak_key.

*eject
TYPES: BEGIN OF gty_bsak,                        "Cleared Invoice Data "
        bukrs            TYPE bukrs,             "Company Code         "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        umsks            TYPE umsks,             "Special G/L Trans Type
        umskz            TYPE umskz,             "Special G/L Indicator"
        augdt            TYPE augdt,             "Clearing Date        "
        augbl            TYPE augbl,             "Clearing Doc. Number "
        zuonr            TYPE dzuonr,            "Assignment Number    "
        gjahr            TYPE gjahr,             "Fiscal Year          "
        belnr            TYPE belnr_d,           "Accounting Doc. Number
        buzei            TYPE buzei,             "Accounting Doc. Item "
        budat            TYPE budat,             "Posting Date         "
        bldat            TYPE bldat,             "Document Date        "
        cpudt            TYPE cpudt,             "Entry Date           "
        waers            TYPE waers,             "Document Currency Key"
        xblnr            TYPE xblnr1,            "Reference Doc. Number"
        blart            TYPE blart,             "Document Type        "
        bschl            TYPE bschl,             "Posting Key          "
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        mwskz            TYPE mwskz,             "Tax Code             "
        dmbtr            TYPE dmbtr,             "Amount in Local Curr."
        wrbtr            TYPE wrbtr,             "Amount In Doc. Curr. "
        sgtxt            TYPE sgtxt,             "Item Text            "
        zfbdt            TYPE dzfbdt,            "Baseline Date        "
        zterm            TYPE dzterm,            "Payment Terms        "
        zbd1t            TYPE dzbd1t,            "Cash Discount Days 1 "
        zbd2t            TYPE dzbd2t,            "Cash Discount Days 2 "
        zbd3t            TYPE dzbd3t,            "Net Paym. Terms Period
        skfbt            TYPE skfbt,             "Amt Eligible For Disc.
        sknto            TYPE sknto,             "Cash Discount Amnt LC"
        wskto            TYPE wskto,             "Cash Discount Amnt DC"
        zlsch            TYPE dzlsch,            "Payment Method       "
        zlspr            TYPE dzlspr,            "Payment Block        "
        bstat            TYPE bstat_d,           "Document Status      "
        xref3            TYPE xref3,             "Reference Key / RCO  "
       END   OF gty_bsak.

TYPES:  gtt_bsak         TYPE STANDARD TABLE OF gty_bsak.

*eject
TYPES: BEGIN OF gty_bseg,                        "Accounting Doc Items "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc. Number
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Accounting Doc. Item "
        augdt            TYPE augdt,             "Clearing Date        "
        augbl            TYPE augbl,             "Clearing Doc. Number "
        bschl            TYPE bschl,             "Posting Key          "
        koart            TYPE koart,             "Account Type         "
        umskz            TYPE umskz,             "Special G/L Indicator"
        umsks            TYPE umsks,             "Special G/L Trans Type
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        mwskz            TYPE mwskz,             "Tax Code             "
        dmbtr            TYPE dmbtr,             "Amount in Local Curr."
        wrbtr            TYPE wrbtr,             "Amount In Doc. Curr. "
        zuonr            TYPE dzuonr,            "Assignment Number    "
        sgtxt            TYPE sgtxt,             "Item Text            "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        zfbdt            TYPE dzfbdt,            "Baseline Date        "
        zterm            TYPE dzterm,            "Payment Terms        "
        zbd1t            TYPE dzbd1t,            "Cash Discount Days 1 "
        zbd2t            TYPE dzbd2t,            "Cash Discount Days 2 "
        zbd3t            TYPE dzbd3t,            "Net Paym. Terms Period
        skfbt            TYPE skfbt,             "Amt Eligible For Disc.
        sknto            TYPE sknto,             "Cash Discount Amnt LC"
        wskto            TYPE wskto,             "Cash Discount Amnt DC"
        zlsch            TYPE dzlsch,            "Payment Method       "
        zlspr            TYPE dzlspr,            "Payment Block        "
        xref3            TYPE xref3,             "Reference Key / RCO  "
       END   OF gty_bseg.

TYPES:  gtt_bseg         TYPE STANDARD TABLE OF gty_bseg.

*eject
TYPES: BEGIN OF gty_bkpf,                        "Accounting Document  "
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc. Number
        gjahr            TYPE gjahr,             "Fiscal Year          "
        blart            TYPE blart,             "Document Type        "
        bldat            TYPE bldat,             "Document Date        "
        budat            TYPE budat,             "Posting Date         "
        monat            TYPE monat,             "Fiscal Period        "
        cpudt            TYPE cpudt,             "Entry Date           "
        cputm            TYPE cputm,             "Entry Time           "
        aedat            TYPE datum,             "Last Change Date     "
        usnam            TYPE usnam,             "User Name            "
        bvorg            TYPE bvorg,             "Cross Company Document
        xblnr            TYPE xblnr1,            "Reference Doc. Number"
        waers            TYPE waers,             "Document Currency Key"
        kursf            TYPE kursf,             "Exchange Rate        "
        bstat            TYPE bstat_d,           "Document Status      "
        awtyp            TYPE awtyp,             "Reference Transaction"
        awkey            TYPE awkey,             "Reference Key        "
        hwaer            TYPE hwaer,             "Local Currency       "
        ausbk            TYPE ausbk,             "Source Company Code  "
       END   OF gty_bkpf.

TYPES:  gtt_bkpf         TYPE STANDARD TABLE OF gty_bkpf.

*eject
TYPES: BEGIN OF gty_invc_data,
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc. Number
        gjahr            TYPE gjahr,             "Fiscal Year          "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        monat            TYPE monat,             "Fiscal Period        "
        blart            TYPE blart,             "Document Type        "
        bldat            TYPE bldat,             "Document Date        "
        budat            TYPE budat,             "Posting Date         "
        cpudt            TYPE cpudt,             "Entry Date           "
        cputm            TYPE cputm,             "Entry Time           "
        usnam            TYPE usnam,             "User Name            "
        xblnr            TYPE xblnr1,            "Reference Doc. Number"
        waers            TYPE waers,             "Document Currency Key"
        hwaer            TYPE hwaer,             "Local Currency       "
        kursf            TYPE kursf,             "Exchange Rate        "
        bstat            TYPE bstat_d,           "Document Status      "
        awtyp            TYPE awtyp,             "Reference Transaction"
        awkey            TYPE awkey,             "Reference Key        "
        buzei            TYPE buzei,             "Accounting Doc. Item "
        augdt            TYPE augdt,             "Clearing Date        "
        augbl            TYPE augbl,             "Clearing Doc. Number "
        bschl            TYPE bschl,             "Posting Key          "
        koart            TYPE koart,             "Account Type         "
        umskz            TYPE umskz,             "Special G/L Indicator"
        umsks            TYPE umsks,             "Special G/L Trans Type
        shkzg            TYPE shkzg,             "Debit/Credit Indicator
        mwskz            TYPE mwskz,             "Tax Code             "
        dmbtr            TYPE dmbtr,             "Amount in Local Curr."
        wrbtr            TYPE wrbtr,             "Amount In Doc. Curr. "
        zuonr            TYPE dzuonr,            "Assignment Number    "
        sgtxt            TYPE sgtxt,             "Item Text            "
        zfbdt            TYPE dzfbdt,            "Baseline Date        "
        zterm            TYPE dzterm,            "Payment Terms        "
        zbd1t            TYPE dzbd1t,            "Cash Discount Days 1 "
        zbd2t            TYPE dzbd2t,            "Cash Discount Days 2 "
        zbd3t            TYPE dzbd3t,            "Net Paym. Terms Period
        skfbt            TYPE skfbt,             "Amt Eligible For Disc.
        sknto            TYPE sknto,             "Cash Discount Amnt LC"
        wskto            TYPE wskto,             "Cash Discount Amnt DC"
        zlsch            TYPE schzw_bseg,        "Payment Method       "
        zlspr            TYPE dzlspr,            "Payment Block        "
        xref3            TYPE xref3,             "Reference Key / RCO  "
        disc_prcnt(6)    TYPE p DECIMALS 3,      "Discount Percent     "
        due_date         TYPE sydatum,           "Net Due Date         "
        rprtg_amt        TYPE wrbtr,             "Amnt In Reporting Curr
        rprtg_fx         TYPE kursf,             "Exchange_Rate        "
        stage            TYPE char1,             "Stage Prkd/Open/Clrd "
        instid           TYPE sibfboriid,        "Object Id            "
       END   OF gty_invc_data.

TYPES:  gtt_invc_data    TYPE STANDARD TABLE OF gty_invc_data.

*eject
TYPES: BEGIN OF gty_lfa1_key,                    "Vendor Key           "
        lifnr            TYPE lifnr,             "Vendor Account Number"
       END   OF gty_lfa1_key.

TYPES:  gtt_lfa1_key     TYPE STANDARD TABLE OF gty_lfa1_key.

TYPES: BEGIN OF gty_object,                      "Object               "
        object_id        TYPE char32,            "Object Id            "
       END   OF gty_object.

TYPES:  gtt_object       TYPE STANDARD TABLE OF gty_object.

TYPES: BEGIN OF gty_wi_id,                       "Work Item ID         "
        wi_id            TYPE sww_wiid,          "Work Item ID         "
       END   OF gty_wi_id.

TYPES:  gtt_wi_id        TYPE STANDARD TABLE OF gty_wi_id.

TYPES: BEGIN OF gty_top_wi_id,                   "Top Work Item ID     "
        top_wi_id        TYPE swfrtwiid,         "Top Work Item ID     "
       END   OF gty_top_wi_id.

TYPES:  gtt_top_wi_id    TYPE STANDARD TABLE OF gty_top_wi_id.

TYPES: BEGIN OF gty_top_wi,                      "Top Work Item        "
        wi_id            TYPE sww_wiid,          "Work Item ID         "
        wi_type          TYPE sww_witype,        "Work Item Type       "
        wi_rh_task       TYPE sww_task,          "Task ID              "
       END   OF gty_top_wi.

TYPES:  gtt_top_wi       TYPE STANDARD TABLE OF gty_top_wi.

TYPES: BEGIN OF gty_wi2obj_key,                  "Work Item to Objct Key
        wi_id            TYPE sww_wiid,          "Work Item ID         "
        instid           TYPE sibfboriid,        "BOR Object ID        "
        typeid           TYPE sibftypeid,        "BOR Type ID          "
        removed          TYPE sww_relrmv,        "Relation Deleted     "
       END   OF gty_wi2obj_key.

TYPES:  gtt_wi2obj_key   TYPE STANDARD TABLE OF gty_wi2obj_key.

TYPES:  gty_swwwihead    TYPE swwwihead.         "Work Items           "
TYPES:  gtt_swwwihead    TYPE STANDARD TABLE OF gty_swwwihead.

TYPES:  gty_wi2obj       TYPE sww_wi2obj.        "Work Item to Object  "
TYPES:  gtt_wi2obj       TYPE STANDARD TABLE OF gty_wi2obj.

TYPES:  gty_wicont       TYPE sww_cont.          "Work Item Container  "
TYPES:  gtt_wicont       TYPE STANDARD TABLE OF gty_wicont.

TYPES:  gty_toa01        TYPE toa01.             "ArchiveLink Table    "
TYPES:  gtt_toa01        TYPE STANDARD TABLE OF gty_toa01.

TYPES:  gty_cont         TYPE swr_cont.          "Workflow Container   "
TYPES:  gtt_cont         TYPE STANDARD TABLE OF gty_cont.

*eject
TYPES: BEGIN OF gty_rec,                    "Output Record Layout      "
        erpid            TYPE char5,        "ERP_Id                    "
        invc_ulid        TYPE char40,       "Invoice_ULID              "
        invc_status      TYPE char1,        "Invoice_Status            "
        title_mwfi       TYPE text120,      "Title_Of_Main_WF_Item     "
        title_rwfi       TYPE text120,      "Title_Of_RCO_Or_Appr_WF_Itm
        crnt_agent       TYPE char12,       "Current_Agent             "
        wf_status        TYPE char12,       "WF_Status                 "
        image_sdt        TYPE char10,       "Image_Start_Date          "
        image_stm        TYPE char8,        "Image_Start_Time          "
        zterm            TYPE char4,        "Payment_Terms             "
        disc_prcnt       TYPE char6,        "Discount_Percent          "
        wskto            TYPE char16,       "Discount_Amount           "
        paym_status      TYPE char12,       "Payment_Status            "
        ap_sdt           TYPE char10,       "AP_Clerk_Start_Date       "
        ap_cdt           TYPE char10,       "AP_Clerk_Complete_Date    "
        days_paym_out    TYPE char5,        "Days_Payment_Outstanding  "
        invc_type        TYPE char3,        "Invoice_Type              "
        paym_due_dt      TYPE char10,       "Payment_Due_Date          "
        mm_invc          TYPE belnr_d,      "MM_Invoice_Number         "
        bukrs            TYPE bukrs,        "Company_Code              "
        belnr            TYPE belnr_d,      "Document_Number           "
        gjahr            TYPE char4,        "Fiscal_Year               "
        bldat            TYPE char10,       "Invoice_Date              "
        budat            TYPE char10,       "Posting_Date              "
        wrbtr            TYPE char16,       "Invoice_Amount            "
        waers            TYPE waers,        "Invoice_Currency          "
        xref3            TYPE xref3,        "RCO                       "
        prkd_date        TYPE char10,       "Parked_Date               "
        prkd_time        TYPE char8,        "Parked_Time               "
        prkd_by          TYPE sww_aagent,   "Parked_By                 "
        rco_rdt          TYPE char10,       "RCO_Receipt_Date          "
        rco_rtm          TYPE char8,        "RCO_Receipt_Time          "
        rco_cdt          TYPE char10,       "RCO_Completed_Date        "
        rco_ctm          TYPE char8,        "RCO_Completed_time        "
        aprvl_sdt        TYPE char10,       "Approval_Start_Date       "
        aprvl_stm        TYPE char8,        "Approval_Start_Time       "
        entrd_dt         TYPE char10,       "Entered_Date              "
        entrd_tm         TYPE char8,        "Entered_Time              "
        entrd_by         TYPE sww_aagent,   "Entered_By                "
        lifnr            TYPE lifnr,        "Vendor_Number             "
        zuonr            TYPE dzuonr,       "Assignment                "
        monat            TYPE char2,        "Period                    "
        blart            TYPE blart,        "Document_Type             "
        dmbtr            TYPE char16,       "Amt_In_Local_Curr         "
        hwaer            TYPE hwaer,        "Local_Currency            "
        sgtxt            TYPE sgtxt,        "Vendor_Line_Item_Text     "
        augbl            TYPE augbl,        "Clearing_Document         "
        augdt            TYPE char10,       "Clearing_Date             "
        zlsch            TYPE dzlsch,       "Payment_Method            "
        zlspr            TYPE dzlspr,       "Payment_Block             "
        xblnr            TYPE xblnr1,       "Reference_Document        "
        mwskz            TYPE mwskz,        "Tax_Code                  "
        title_image      TYPE sww_witext,   "Image_Title               "
        image_status     TYPE sww_wistat,   "Image_Status              "
        image_priority   TYPE char1,        "Image_Priority            "
        image_agent      TYPE sww_cruser,   "Image_Agent               "
        image_atchm      TYPE sww_noteex,   "Image_Attachments         "
        image_conf       TYPE sww_wiconf,   "Image_Confirm             "
        image_wi_id      TYPE char12,       "Image_Work_Item_Id        "
        image_task_text  TYPE sww_rhtext,   "Image_Task_Text           "
        title_ap_clerk   TYPE sww_witext,   "AP_Clerk_Title            "
        ap_stm           TYPE char8,        "AP_Clerk_Start_Time       "
        wf_wi_id         TYPE char12,       "Workflow_Work_Item_Id     "
        image_wi_type    TYPE sww_witype,   "Image_Work_Item_Type      "
        image_task       TYPE sww_task,     "Image_Task                "
        ap_ddln          TYPE sww_deadfl,   "AP_Clerk_Deadline         "
        ap_frwrd_by      TYPE sww_forwby,   "AP_Clerk_Forwarded_By     "
        ap_priority      TYPE char1,        "Ap_Clerk_Priority         "
        ap_ddln_status   TYPE char4,        "AP_Clerk_Deadline_Status  "
        ap_text          TYPE sww_witext,   "AP_Clerk_Text             "
        rprtg_curr       TYPE char5,        "Reporting_Currency        "
        rprtg_amt        TYPE char16,       "Amt_In_Reporting_Curr     "
        rprtg_fx         TYPE char12,       "Exchange_Rate             "
        image_guid       TYPE char40,       "Image_GUID                "
       END   OF gty_rec.

TYPES:  gty_output       TYPE string.       "Output String             "

TYPES:  gtt_output       TYPE STANDARD TABLE OF gty_output.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_delim         TYPE char2              "Delimiter            "
                         VALUE '|~',
        gc_ucomm_onli    TYPE sscrfields-ucomm   "Online Function Cmmnd"
                         VALUE 'ONLI',
        gc_lgcfilpth     TYPE pathintern         "Logical File Path    "
                         VALUE 'ZFAPI129_INVOICE_WF_ITEMS'.
*       gc_filename      TYPE fileintern         "Filename             "
*                        VALUE 'InvAging-SSSSS_MMDDYYYYHHMMSS_NNN.CSV'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_fl_sys_us     TYPE xflag,             "US System Flag       "
        gv_fl_sys_ca     TYPE xflag,             "Canadian System Flag "
        gv_cn_recs_total TYPE i,                 "Total Record Count   "
        gv_cn_recs_max   TYPE i,                 "Maximum Record Count "
        gv_cn_recs_file  TYPE i,                 "File Record Count    "
        gv_cn_files      TYPE i,                 "File Count           "
        gv_filename      TYPE text256,           "Filename             "
        gv_filename_p    TYPE text256.           "Filename             "

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_xparam        TYPE gtt_xparam,        "Program Parameters   "
        gt_object        TYPE gtt_object,        "Object               #
        gt_top_wi_id     TYPE gtt_top_wi_id,     "Top Work Item ID     "
        gt_wi_top        TYPE gtt_top_wi,        "Top Work Item        "
        gt_wi2obj_key    TYPE gtt_wi2obj_key,    "Work Item to Objct Key
        gt_output        TYPE gtt_output.        "Output               "

************************************************************************
*                             Range Tables                             *
************************************************************************
DATA:   gr_crea_tmp      TYPE RANGE OF swfrcrets, "Creation Time Stamp "
        gs_crea_tmp      LIKE LINE  OF gr_crea_tmp.
