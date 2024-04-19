*&---------------------------------------------------------------------*
*&  Include           ZFAPI123_PAYMENT_HISTORY_TOP
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI123_PAYMENT_HISTORY                       *
* Include Program    :  ZFAPI123_PAYMENT_HISTORY_TOP                   *
* Author             :  Paul Karunakar                                 *
* Creation Date      :  02-May-2018                                    *
* Application Area   :  FICO                                           *
* Description        :  Payment History data from each of the three    *
*                       SAP instances will be extracted in a delimited *
*                       file and sent to IAP.                          *
*                                                                      *
*&---------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  --------------------------------*
* 02-May-2018  KPAL        D30K928709  CHG0108944  Initial Development *
*----------------------------------------------------------------------*

TYPE-POOLS: vrm.

TABLES: sscrfields, bsak, lfa1.

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF gty_bsak_key,               "BSAK Key                  "
        bukrs            TYPE bukrs,        "Company Code              "
        lifnr            TYPE lifnr,        "Vendor Account Number     "
        augdt            TYPE augdt,        "Clearing Date             "
        augbl            TYPE augbl,        "Clearing Document Number  "
      END   OF gty_bsak_key.

TYPES:  gtt_bsak_key     TYPE STANDARD TABLE OF gty_bsak_key.

TYPES: BEGIN OF gty_bsak,                   "BSAK                      "
        bukrs            TYPE bukrs,        "Company Code              "
        lifnr            TYPE lifnr,        "Vendor Account Number     "
        umsks            TYPE umsks,        "Special G/L Transaction Typ
        umskz            TYPE umskz,        "Special G/L Indicator     "
        augdt            TYPE augdt,        "Clearing Date             "
        augbl            TYPE augbl,        "Clearing Document Number  "
        zuonr            TYPE dzuonr,       "Assignment Number         "
        gjahr            TYPE gjahr,        "Fiscal Year               "
        belnr            TYPE belnr_d,      "Accounting Document Number"
        buzei            TYPE buzei,        "Accounting Doc. Item Number
        waers            TYPE waers,        "Currency Key              "
        xblnr            TYPE xblnr1,       "Reference Document Number "
        blart            TYPE blart,        "Document Type             "
        bschl            TYPE bschl,        "Posting Key               "
        shkzg            TYPE shkzg,        "Debit/Credit Indicator    "
        dmbtr            TYPE dmbtr,        "Amount in Local Currency  "
        wrbtr            TYPE wrbtr,        "Amount in Document Currency
        zlsch            TYPE dzlsch,       "Payment Method            "
        empfb            TYPE empfb,        "Payee/Payer               "
      END   OF gty_bsak.

TYPES:  gtt_bsak         TYPE STANDARD TABLE OF gty_bsak.

TYPES: BEGIN OF gty_objectid,
        objectid         TYPE cdobjectv,
        udate            TYPE cddatum,
        utime            TYPE cduzeit,
       END   OF gty_objectid.

TYPES:  gtt_objectid     TYPE STANDARD TABLE OF gty_objectid.

TYPES: BEGIN OF gty_cdhdr,
        bukrs            TYPE bukrs,
        augbl            TYPE augbl,
        gjahr            TYPE gjahr,
        udate            TYPE cddatum,
        utime            TYPE cduzeit,
        lifnr            TYPE lifnr,
        ldate            TYPE sydatum,
        hdate            TYPE sydatum,
       END   OF gty_cdhdr.

TYPES:  gtt_cdhdr        TYPE STANDARD TABLE OF gty_cdhdr.

*eject
TYPES: BEGIN OF gty_reguh,
        laufd            TYPE laufd,
        laufi            TYPE laufi,
        xvorl            TYPE xvorl,
        zbukr            TYPE dzbukr,
        lifnr            TYPE lifnr,
        kunnr            TYPE kunnr,
        empfg            TYPE empfg,
        vblnr            TYPE vblnr,
        waers            TYPE waers,
        zaldt            TYPE dzaldt_zhl,
        rzawe            TYPE rzawe,
        valut            TYPE valut,
        rwbtr            TYPE rwbtr,
       END   OF gty_reguh.

TYPES:  gtt_reguh        TYPE STANDARD TABLE OF gty_reguh.

TYPES: BEGIN OF gty_regup,
        laufd            TYPE laufd,
        laufi            TYPE laufi,
        xvorl            TYPE xvorl,
        zbukr            TYPE dzbukr,
        lifnr            TYPE lifnr,
        kunnr            TYPE kunnr,
        empfg            TYPE empfg,
        vblnr            TYPE vblnr,
        bukrs            TYPE bukrs,
        belnr            TYPE belnr_d,
        gjahr            TYPE gjahr,
        buzei            TYPE buzei,
        xblnr            TYPE xblnr1,
        shkzg            TYPE shkzg,
        wrbtr            TYPE wrbtr,
       END   OF gty_regup.

TYPES:  gtt_regup        TYPE STANDARD TABLE OF gty_regup.

TYPES: BEGIN OF gty_payr,
        zbukr            TYPE dzbukr,
        hbkid            TYPE hbkid,
        hktid            TYPE hktid,
        rzawe            TYPE dzlsch,
        chect            TYPE chect,
        lifnr            TYPE lifnr,
        vblnr            TYPE vblnr,
        gjahr            TYPE gjahr,
        bancd            TYPE bancd,
       END   OF gty_payr.

TYPES:  gtt_payr         TYPE STANDARD TABLE OF gty_payr.

*eject
TYPES: BEGIN OF gty_final,                  "Final Layout
        erpid            TYPE char05,       "ERP_Id
        ph_ulid          TYPE char40,       "Payment_History_ULID
        ch_ulid          TYPE char20,       "Cheque_ULID
        bukrs            TYPE bukrs,        "Company_Code
        augbl            TYPE augbl,        "Payment_Number
        chect            TYPE chect,        "Check_Number
        p_voided         TYPE char01,       "Payment_Voided
        p_total          TYPE char16,       "Payment_Total
        waers            TYPE waers,        "Payment_Currency
        augdt            TYPE char10,       "Payment_Date
        bancd            TYPE char10,       "Encashment_Date
        zlsch            TYPE dzlsch,       "Payment_Method
        i_ulid           TYPE char20,       "Invoice_ULID
        inv_id           TYPE char20,       "Invoice_Id
        xblnr            TYPE xblnr1,       "Invoice_Number
        i_total          TYPE string,       "Invoice_Total
        i_paid           TYPE string,       "Invoice_Amount_Paid
        belnr            TYPE belnr,        "ERP_Document_Number
        v_ulid           TYPE char20,       "Vendor_ULID
        lifnr            TYPE lifnr,        "Vendor_Number
        empfb            TYPE empfb,        "Payee
        gjahr            TYPE gjahr,        "Fiscal_Year
        lc_date          TYPE char14,       "Last_Change_DateTime
       END OF gty_final.

TYPES:  gty_output       TYPE string.       "Output                    "

TYPES:  gtt_output       TYPE STANDARD TABLE OF gty_output.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_delim         TYPE char2 VALUE '|~',
        gc_ucomm_onli    TYPE sscrfields-ucomm VALUE 'ONLI',
        gc_lgcfilpth     TYPE pathintern
                         VALUE 'ZFAPI123_PAYMENT_HISTORY',
        gc_filename      TYPE fileintern
                         VALUE 'SSSSS_Paym_Hist_YYMMDDHHMM_NNN.CSV'.

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
DATA:   gt_bsak_key      TYPE gtt_bsak_key,      "BSAK Key             "
        gt_bsak          TYPE gtt_bsak,          "BSAK                 "
        gt_output        TYPE gtt_output.        "Output               "
