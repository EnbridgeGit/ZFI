*&---------------------------------------------------------------------*
*&  Include           ZFAPR021_IN_HOUSE_CHECK_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPR021_IN_HOUSE_CHECK                       *
*& Include Name       :  ZFAPR021_IN_HOUSE_CHECK_TOP                   *
*& Author             :  Babumiya Mohammad                             *
*& Creation Date      :  23-Aug-2011                                   *
*& Object ID          :  F_P2C_AP_003_In_House_Check_(AP) US Instance  *
*& Application Area   :  FI-AP                                         *
*& Description        :  In House Check Printing                       *
*&                       Copy of US program ZFAPF003_IN_HOUSE_CHECK    *
*&                       TOP Include - Data Definitions                *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
*----------------------------------------------------------------------*
* Version No    : 1                                                    *
* Date          : 23-Aug-2011                                          *
* Modified By   : Babumiya Mohammad                                    *
* Correction No : DECK900992                                           *
* Description   : Initial program development                          *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 2                                                    *
* Date          : 20-Aug-2013                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K922271, D30K923471, D30K924001                   *
* Description   : SDP42671-Convert to UG systems                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 3                                                    *
* Date          : 02-Jan-2015                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K925153, D30K925270, D30K925486                   *
* Description   : SDP42671 1. Add invoice header long text             *
*                          2. Correct payment advice total/subtotal    *
*                 SDP86117 D30K925924 - retransport to correct         *
*                          import errors (wrong order)import errors    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 4                                                    *
* Date          : 04-Feb-2016                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K926579                                           *
* Description   : ACR-307  1. Change the address of the Spectra office *
*                             location that is printed on the check    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 5                                                    *
* Date          : 25-Jan-2017                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K927881                                           *
* Description   : ACR-2755 1. Enhancements for Enbridge integration    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 6                                                    *
* Date          : 10-Apr-2017                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K928144                                           *
* Description   : ACR-4084 JPMC In-House Checks - format remittance text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 7                                                    *
* Date          : 28-Mar-2019                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K929884                                           *
* Description   : CHG0148429 - Incorporate TD Bank                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 8                                                    *
* Date          : 10-Jun-2019                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K930133                                           *
* Description   : CHG0148429 - Select Options for House Bank and Accnt *
*                              Add option to override Company Name     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 9                                                    *
* Date          : 20-Sep-2019                                          *
* Modified By   : John Hartung                                         *
* Correction No : D30K930167                                           *
* Description   : CHG0160810 - Add US Check Form to SW and UG          *
*                              Add Payment Method P "Post-Dated"       *
*----------------------------------------------------------------------*

TABLES: reguh.               "Settlement Data From Payment Program     "

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  ty_wa_xparam     TYPE zfit_xparam.       "Program Control Params

TYPES:  ty_it_xparam     TYPE STANDARD TABLE OF ty_wa_xparam.

TYPES: BEGIN OF ty_wa_reguh,                     "Payment Settlemnt Data
        laufd            TYPE laufd,             "Settlement Run Date  "
        laufi            TYPE laufi,             "Settlement Run ID    "
        xvorl            TYPE xvorl,             "Flag-Settlmnt Proposal
        zbukr            TYPE dzbukr,            "Paying Company Code  "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        kunnr            TYPE kunnr,             "Customer Number      "
        empfg            TYPE empfg,             "Payee Code           "
        vblnr            TYPE vblnr,             "Payment Documnt Number
        waers            TYPE waers,             "Currency Key         "
        name1            TYPE name1_gp,          "Name 1               "
        zanre            TYPE dzanre,            "Payee Title          "
        znme1            TYPE dznme1,            "Payee Name 1         "
        znme2            TYPE dznme1,            "Payee Name 2         "
        znme3            TYPE dznme1,            "Payee Name 3         "
        znme4            TYPE dznme1,            "Payee Name 4         "
        zpstl            TYPE dzpstl,            "Payee Postal Code    "
        zort1            TYPE dzort1,            "Payee City           "
        zstra            TYPE dzstra,            "Payee Street         "
        zpfac            TYPE dzpfac,            "Payee P.O. Box       "
        zland            TYPE dzland,            "Payee Country Key    "
        zregi            TYPE dzregi,            "Payee Region Code    "
        zaldt            TYPE dzaldt_zhl,        "Paymnt Doc Posting Dat
        rzawe            TYPE rzawe,             "Payment Method       "
        hktid            TYPE hktid,             "Account ID           "
        hbkid            TYPE hbkid,             "House Bank ID        "
        ubknt            TYPE ubknt,             "Our Account Number   "
        ubnks            TYPE banks,             "Our Bank Country     "
        ubnkl            TYPE ubnkl,             "Our Bank Number      "
        valut            TYPE valut,             "Value Date           "
        rwbtr            TYPE rwbtr,             "Amount Paid Paymnt Cur
        zpst2            TYPE dzpst2,            "P.O. Box Postal Code "
        absbu            TYPE absbu,             "Sending Company Code "
        uzawe            TYPE uzawe,             "Payment Method Spplmnt
        zort2            TYPE ort02_z,           "Payee Address Location
        zpfor            TYPE pfort_z,           "Payee PO Box City    "
        ausfd            TYPE ausfd,             "Due Date Of Paid Items
        zadnr            TYPE adrnr_z,           "Payee Address Number "
        chk_frac         TYPE char15,            "Check Fraction       "
        chk_prot_acnt    TYPE char5,             "Check Protected Accnt"
       END   OF ty_wa_reguh.

TYPES:  ty_it_reguh      TYPE STANDARD TABLE OF ty_wa_reguh.

*eject
TYPES: BEGIN OF ty_wa_regup,                     "Payment Items        "
        laufd            TYPE laufd,             "Settlement Run Date  "
        laufi            TYPE laufi,             "Settlement Run ID    "
        xvorl            TYPE xvorl,             "Flag-Settlmnt Proposal
        zbukr            TYPE dzbukr,            "Paying Company Code  "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        kunnr            TYPE kunnr,             "Customer Number      "
        empfg            TYPE empfg,             "Payee Code           "
        vblnr            TYPE vblnr,             "Payment Documnt Number
        bukrs            TYPE bukrs,             "Company Code         "
        belnr            TYPE belnr_d,           "Accounting Doc Number"
        gjahr            TYPE gjahr,             "Fiscal Year          "
        buzei            TYPE buzei,             "Accounting Doc Item  "
        xblnr            TYPE xblnr1,            "Reference Doc Number "
        bldat            TYPE bldat,             "Document Date        "
        wrbtr            TYPE wrbtr,             "Amount In Doc Currency
        sgtxt            TYPE sgtxt,             "Vendor Item Text     "
        wskto            TYPE wskto,             "Cash Discnt In Doc Cur
       END   OF ty_wa_regup.

TYPES:  ty_it_regup      TYPE STANDARD TABLE OF ty_wa_regup.

TYPES: BEGIN OF ty_wa_payr,                      "Payment Medium File  "
        zbukr            TYPE dzbukr,            "Paying Company Code  "
        hbkid            TYPE hbkid,             "House Bank ID        "
        hktid            TYPE hktid,             "Account ID           "
        rzawe            TYPE dzlsch,            "Payment Method       "
        chect            TYPE chect,             "Check Number - To    "
        checf            TYPE checf,             "Check Number - From  "
        laufd            TYPE laufd,             "Settlement Run Date  "
        laufi            TYPE laufi,             "Settlement Run ID    "
        lifnr            TYPE lifnr,             "Vendor Account Number"
        kunnr            TYPE kunnr,             "Customer Number      "
        empfg            TYPE empfg,             "Payee Code           "
        vblnr            TYPE vblnr,             "Payment Documnt Number
        waers            TYPE waers,             "Currency Key         "
        extrd            TYPE extrd,             "Date of Extract Creat"
       END   OF ty_wa_payr.

TYPES:  ty_it_payr       TYPE STANDARD TABLE OF ty_wa_payr.

TYPES: BEGIN OF ty_wa_t001,                      "Company Codes        "
        bukrs            TYPE bukrs,             "Company Code         "
        adrnr            TYPE adrnr,             "Address Number       "
       END   OF ty_wa_t001.

TYPES:  ty_it_t001       TYPE STANDARD TABLE OF ty_wa_t001.

*eject
TYPES: BEGIN OF ty_wa_adrc,                      "Addresses            "
        addrnumber       TYPE ad_addrnum,        "Address Number       "
        date_from        TYPE ad_date_fr,        "Date From            "
        nation           TYPE ad_nation,         "Address Version ID   "
        date_to          TYPE ad_date_to,        "Date To              "
        name1            TYPE ad_name1,          "Name 1               "
        city1            TYPE ad_city1,          "City                 "
        post_code1       TYPE ad_pstcd1,         "Postal Code          "
        street           TYPE ad_street,         "Street               "
        country          TYPE land1,             "Country Key          "
        region           TYPE regio,             "Region               "
       END   OF ty_wa_adrc.

TYPES:  ty_it_adrc       TYPE STANDARD TABLE OF ty_wa_adrc.

TYPES: BEGIN OF ty_wa_our_addr,                  "Our AP Office Addresss
        absbu            TYPE absbu,             "Sending Company Code "
        hbkid            TYPE hbkid,             "House Bank ID        "
        hktid            TYPE hktid,             "Account ID           "
        name1            TYPE ad_name1,          "Name 1               "
        city1            TYPE ad_city1,          "City                 "
        post_code1       TYPE ad_pstcd1,         "Postal Code          "
        street           TYPE ad_street,         "Street               "
        country          TYPE land1,             "Country Key          "
        region           TYPE regio,             "Region               "
       END   OF ty_wa_our_addr.

TYPES:  ty_it_our_addr   TYPE STANDARD TABLE OF ty_wa_our_addr.

TYPES: BEGIN OF ty_wa_checks,                    "Checks Issued        "
        laufd            TYPE laufd,             "Settlement Run Date  "
        laufi            TYPE laufi,             "Settlement Run ID    "
        zbukr            TYPE dzbukr,            "Paying Company Code  "
        vblnr            TYPE vblnr,             "Payment Documnt Number
        lifnr            TYPE lifnr,             "Vendor Account Number"
        hbkid            TYPE hbkid,             "House Bank ID        "
        hktid            TYPE hktid,             "Account ID           "
        rzawe            TYPE dzlsch,            "Payment Method       "
        chect            TYPE chect,             "Check Number - To    "
        checf            TYPE checf,             "Check Number - From  "
        laufd_p          TYPE laufd,             "Settlement Run Date  "
        laufi_p          TYPE laufi,             "Settlement Run ID    "
        waers            TYPE waers,             "Currency Key         "
        extrd            TYPE extrd,             "Date of Extract Creat"
        msg_error        TYPE text50,            "Error Message        "
       END   OF ty_wa_checks.

TYPES:  ty_it_checks     TYPE STANDARD TABLE OF ty_wa_checks.

TYPES:  ty_rt_hbkid      TYPE RANGE OF hbkid.    "Range Table-House Bank

TYPES:  ty_rt_hktid      TYPE RANGE OF hktid.    "Range Table-Bank Accnt

*eject
TYPES: BEGIN OF ty_wa_text_line,                            "D30K928144
        text1            TYPE STRING,                       "D30K928144
        text2            TYPE STRING,                       "D30K928144
        text3            TYPE STRING,                       "D30K928144
        text4            TYPE STRING,                       "D30K928144
        text5            TYPE STRING,                       "D30K928144
        text6            TYPE STRING,                       "D30K928144
        text7            TYPE STRING,                       "D30K928144
        text8            TYPE STRING,                       "D30K928144
        text9            TYPE STRING,                       "D30K928144
       END   OF ty_wa_text_line.                            "D30K928144

TYPES:  ty_wa_text       TYPE trffo,                        "D30K928144
        ty_it_text       TYPE STANDARD TABLE OF ty_wa_text. "D30K928144

TYPES:  ty_wa_lines      TYPE tline,                        "D30K928144
        ty_it_lines      TYPE STANDARD TABLE OF ty_wa_lines. "D30K928144

TYPES:  ty_wa_words      TYPE STRING,                       "D30K928144
        ty_it_words      TYPE STANDARD TABLE OF ty_wa_words. "D30K928144

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_x             TYPE char1              "X / Yes / True       "
                         VALUE 'X',
        gc_title         TYPE char7              "Title                "
                         VALUE 'TITLE',
        gc_paramtype_thba                        "Configuration        "
                         TYPE zparamtype
                         VALUE 'CONFIGURATION',
        gc_subtype_thba  TYPE zparamsubtype      "TR House Bank Account"
                         VALUE 'TREASURY_HOUSE_BANK_ACCNT',
        gc_subtype_tcno  TYPE zparamsubtype      "TR Company Name Ovrrd"
                         VALUE 'TREASURY_COMPANY_NAME_OVR',
        gc_rzawe_jpmc_ihc                        "Paym Mth-JPMC In-House
                         TYPE rzawe
                         VALUE 'Q',
        gc_rzawe_td_ihc  TYPE rzawe              "Paym Mth-TD In-House "
                         VALUE '8',
        gc_rzawe_td_pdc  TYPE rzawe              "Paym Mth-TD Post-Dated
                         VALUE 'P',
        gc_frmnm_jpmc_ck2                        "Form Name - JPMC CK2 "
                         TYPE sobj_name
                         VALUE 'YF110_PRENUM_CK2',
        gc_frmnm_td_ca   TYPE sobj_name          "Form Name - TD Bank CA
                         VALUE 'ZFAPF021_TD_CA',
        gc_frmnm_td_us   TYPE sobj_name          "Form Name - TD Bank US
                         VALUE 'ZFAPF021_TD_US'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_spool         TYPE char10,            "Spool                "
        gv_nodata        TYPE flag.              "Flag-No Data Found   "

************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gwa_xparam       TYPE ty_wa_xparam,      "Program Control Params
        gwa_reguh        TYPE ty_wa_reguh,       "Payment Settlemnt Data
        gwa_regup        TYPE ty_wa_regup,       "Payment Items        "
        gwa_payr         TYPE ty_wa_payr,        "Payment Medium File  "
        gwa_t001         TYPE ty_wa_t001,        "Company Codes        "
        gwa_adrc         TYPE ty_wa_adrc,        "Addresses            "
        gwa_adrc_legal   TYPE ty_wa_adrc,        "Addresses - Legal Lctn
        gwa_our_addr     TYPE ty_wa_our_addr,    "Our AP Office Addresss
        gwa_checks       TYPE ty_wa_checks.      "Checks Issued        "

DATA:   gwa_regud        TYPE regud,             "Form Print Transfr Dat
        gwa_spell        TYPE spell.             "Form Print Transfr Wrd

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   git_xparam       TYPE ty_it_xparam.      "Program Control Params

DATA:   git_reguh        TYPE ty_it_reguh.       "Payment Settlemnt Data

DATA:   git_regup        TYPE ty_it_regup.       "Payment Items        "

DATA:   git_payr         TYPE ty_it_payr.        "Payment Medium File  "

DATA:   git_t001         TYPE ty_it_t001.        "Company Codes        "

DATA:   git_adrc         TYPE ty_it_adrc.        "Addresses            "

DATA:   git_our_addr     TYPE ty_it_our_addr.    "Our AP Office Addresss

DATA:   git_checks       TYPE ty_it_checks.      "Checks Issued        "

DATA:   grt_hbkid        TYPE ty_rt_hbkid.       "Range Table-House Bank

DATA:   grt_hktid        TYPE ty_rt_hktid.       "Range Table-Bank Accnt

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************

* Select Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
PARAMETERS:       p_laufd  TYPE laufd                 "Run Date        "
                           OBLIGATORY.
PARAMETERS:       p_laufi  TYPE laufi                 "Run ID          "
                           OBLIGATORY.
SELECT-OPTIONS:   s_zbukr  FOR reguh-zbukr.           "Paying Company  "
PARAMETERS:       p_hbkid  TYPE hbkid.                "House Bank ID   "
PARAMETERS:       p_hktid  TYPE hktid.                "Account ID      "
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  01(12) text-s11
                           FOR FIELD p_checf,
                           POSITION POS_LOW.
PARAMETERS:       p_checf  TYPE checf.                "Check Number-From
SELECTION-SCREEN: COMMENT  52(05) text-s12
                           FOR FIELD p_checf,
                           POSITION POS_HIGH.
PARAMETERS:       p_chect  TYPE chect.                "Check Number-To "
SELECTION-SCREEN: END   OF LINE.
PARAMETERS:       p_voidd  TYPE voidd.                "Void Date       "
PARAMETERS:       p_cashd  TYPE bancd.                "Encashment Date "
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       rb_bjpmt RADIOBUTTON GROUP rbgb
                           USER-COMMAND RBGBCMD
                           DEFAULT 'X',
                  rb_btd8c RADIOBUTTON GROUP rbgb,
                  rb_btd8u RADIOBUTTON GROUP rbgb,
                  rb_btdpc RADIOBUTTON GROUP rbgb,
                  rb_btdpu RADIOBUTTON GROUP rbgb.
SELECTION-SCREEN: END   OF BLOCK ssb1.

* Run Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
PARAMETERS:       p_rzawe  TYPE rzawe                 "Payment Method  "
                           OBLIGATORY.                "Payment Method  "
PARAMETERS:       p_frmnm  TYPE sobj_name             "SAPScript Form  "
                           OBLIGATORY.
PARAMETERS:       p_prntr  TYPE fordpriz.             "Printer         "
PARAMETERS:       p_pridt  TYPE pridt.                "Print Date      "
PARAMETERS:       p_tstrn  AS CHECKBOX                "Test Run        "
                           DEFAULT 'X'.
SELECTION-SCREEN: END   OF BLOCK ssb2.
