*&---------------------------------------------------------------------*
*& Report  ZFAPI115_VENDOR_MASTER_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI115_VENDOR_MASTER                        *
* Include            :   ZFAPI115_VENDOR_MASTER_TOP                    *
* Author             :   Vijay Rajaputra                               *
* Date               :   03-Mar-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Vendor Master Extraction for IAP               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 03-Mar-2018  VRAJAPUTRA  D30K928789 CHG0105961 - Initial development *
*                          D30K928848                                  *
*&---------------------------------------------------------------------*

TABLES: lfa1, lfb1, lfm1, sscrfields.

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF gty_lifnr,
        lifnr            TYPE lifnr,
       END   OF gty_lifnr,

        gtt_lifnr        TYPE STANDARD TABLE OF gty_lifnr.

TYPES: BEGIN OF gty_lfa1_key,
        lifnr            TYPE lifnr,
        ktokk            TYPE ktokk,
        loevm            TYPE loevm_x,
        sperr            TYPE sperb_x,
*        zz_iap_vendor_id TYPE z_iap_vendor_id,
*        zz_iap_ritm_id   TYPE z_iap_ritm_id,
        chngind          TYPE cdchngind,
       END   OF gty_lfa1_key,

        gtt_lfa1_key     TYPE STANDARD TABLE OF gty_lfa1_key.

TYPES: BEGIN OF gty_lfa1,
        lifnr            TYPE lifnr,
        land1            TYPE land1_gp,
        name1            TYPE name1_gp,
        name2            TYPE name2_gp,
        adrnr            TYPE adrnr,
        mcod1            TYPE mcdk1,
        mcod2            TYPE mcdk2,
        bahns            TYPE bahns,
        begru            TYPE brgru,
        konzs            TYPE konzs,
        ktokk            TYPE ktokk,
        lnrza            TYPE lnrza,
        loevm            TYPE loevm_x,
        sperr            TYPE sperb_x,
        sperm            TYPE sperm_x,
        stcd1            TYPE stcd1,
        stcd2            TYPE stcd2,
        telf1            TYPE telf1,
        telfx            TYPE telfx,
        sperz            TYPE sperz,
        xlfza            TYPE xlfza,
*        zz_iap_vendor_id TYPE z_iap_vendor_id,
       END   OF gty_lfa1,

        gtt_lfa1         TYPE STANDARD TABLE OF gty_lfa1.

*eject
TYPES: BEGIN OF gty_lfbk_key,
        lifnr            TYPE lifnr,
        banks            TYPE banks,
        bankl            TYPE bankk,
        bankn            TYPE bankn,
        chngind          TYPE cdchngind,
       END   OF gty_lfbk_key,

        gtt_lfbk_key     TYPE STANDARD TABLE OF gty_lfbk_key.

TYPES: BEGIN OF gty_lfbk,
        lifnr            TYPE lifnr,
        banks            TYPE banks,
        bankl            TYPE bankk,
        bankn            TYPE bankn,
        bkont            TYPE bkont,
        bvtyp            TYPE bvtyp,
        bkref            TYPE bkref,
        chngind          TYPE cdchngind,
       END   OF gty_lfbk,

        gtt_lfbk         TYPE STANDARD TABLE OF gty_lfbk.

TYPES: BEGIN OF gty_tbchain21_key,
        banksrec         TYPE banksrec,
        bankkrec         TYPE bankkrec,
        banknrec         TYPE banknrec,
        waers            TYPE waers,
        bankssnd         TYPE bankssnd,
        bankksnd         TYPE bankksnd,
        chngind          TYPE cdchngind,
       END   OF gty_tbchain21_key,

        gtt_tbchain21_key
                         TYPE STANDARD TABLE OF gty_tbchain21_key.

TYPES:  gty_tbchain21    TYPE tbchain21,

        gtt_tbchain21    TYPE STANDARD TABLE OF gty_tbchain21.

*eject
TYPES: BEGIN OF gty_lfb1_key,
        lifnr            TYPE lifnr,
        bukrs            TYPE bukrs,
        chngind          TYPE cdchngind,
       END   OF gty_lfb1_key,

        gtt_lfb1_key     TYPE STANDARD TABLE OF gty_lfb1_key.

TYPES: BEGIN OF gty_lfb1,
        lifnr            TYPE lifnr,
        bukrs            TYPE bukrs,
        sperr            TYPE sperb_b,
        loevm            TYPE loevm_b,
        zuawa            TYPE dzuawa,
        akont            TYPE akont,
        zwels            TYPE dzwels,
        zahls            TYPE dzahls,
        zterm            TYPE dzterm,
        fdgrv            TYPE fdgrv,
        busab            TYPE busab,
        lnrzb            TYPE lnrzb,
        zindt            TYPE dzindt,
        reprf            TYPE reprf,
        hbkid            TYPE hbkid,
        xpore            TYPE xpore,
        qsskz            TYPE qsskz,
        mindk            TYPE mindk,
        uzawe            TYPE uzawe,
        qsrec            TYPE qsrec,
        qland            TYPE qland,
        xlfzb            TYPE xlfzb,
        cerdt            TYPE cerdt,
       END   OF gty_lfb1,

        gtt_lfb1         TYPE STANDARD TABLE OF gty_lfb1.

*eject
TYPES: BEGIN OF gty_lfza_key,
        lifnr            TYPE lifnr,
        bukrs            TYPE bukrs,
        empfk            TYPE empfk,
        chngind          TYPE cdchngind,
       END   OF gty_lfza_key,

        gtt_lfza_key     TYPE STANDARD TABLE OF gty_lfza_key.

TYPES: BEGIN OF gty_lfza,
        lifnr            TYPE lifnr,
        bukrs            TYPE bukrs,
        empfk            TYPE empfk,
        chngind          TYPE cdchngind,
       END   OF gty_lfza,

        gtt_lfza         TYPE STANDARD TABLE OF gty_lfza.

TYPES: BEGIN OF gty_lfbw_key,
        lifnr            TYPE lifnr,
        bukrs            TYPE bukrs,
        witht            TYPE witht,
        chngind          TYPE cdchngind,
       END   OF gty_lfbw_key,

        gtt_lfbw_key     TYPE STANDARD TABLE OF gty_lfbw_key.

TYPES: BEGIN OF gty_lfbw,
        lifnr            TYPE lifnr,
        bukrs            TYPE bukrs,
        witht            TYPE witht,
        wt_subjct        TYPE wt_subjct,
        qsrec            TYPE wt_qsrec,
        wt_wtstcd        TYPE wt_wtstcd,
        wt_withcd        TYPE wt_withcd,
        chngind          TYPE cdchngind,
       END   OF gty_lfbw,

        gtt_lfbw         TYPE STANDARD TABLE OF gty_lfbw.

*eject
TYPES: BEGIN OF gty_lfm1_key,
        lifnr            TYPE lifnr,
        ekorg            TYPE ekorg,
        chngind          TYPE cdchngind,
       END   OF gty_lfm1_key,

        gtt_lfm1_key     TYPE STANDARD TABLE OF gty_lfm1_key.

TYPES: BEGIN OF gty_lfm1,
        lifnr            TYPE elifn,
        ekorg            TYPE ekorg,
        sperm            TYPE sperm_m,
        loevm            TYPE loevm_m,
        waers            TYPE bstwa,
        zterm            TYPE dzterm,
        inco1            TYPE inco1,
        inco2            TYPE inco2,
        webre            TYPE webre,
        kalsk            TYPE kalsk,
        kzaut            TYPE kzaut,
        meprf            TYPE meprf,
        eikto            TYPE eikto_m,
       END   OF gty_lfm1,

        gtt_lfm1         TYPE STANDARD TABLE OF gty_lfm1.

TYPES: BEGIN OF gty_wyt3_key,
        lifnr            TYPE lifnr,
        ekorg            TYPE ekorg,
        parvw            TYPE parvw,
        parza            TYPE parza,
        chngind          TYPE cdchngind,
       END OF gty_wyt3_key,

        gtt_wyt3_key     TYPE STANDARD TABLE OF gty_wyt3_key.

TYPES: BEGIN OF gty_wyt3,
        lifnr            TYPE lifnr,
        ekorg            TYPE ekorg,
        parvw            TYPE parvw,
        parza            TYPE parza,
        lifn2            TYPE lifn2,
        defpa            TYPE defpa,
        chngind          TYPE cdchngind,
       END OF gty_wyt3,

        gtt_wyt3         TYPE STANDARD TABLE OF gty_wyt3.

*eject
TYPES: BEGIN OF gty_knvk,
        parnr            TYPE parnr,
        namev            TYPE namev_vp,
        name1            TYPE name1_gp,
        abtnr            TYPE abtnr_pa,
        telf1            TYPE telf1,
        pafkt            TYPE pafkt,
        lifnr            TYPE lifnr,
        prsnr            TYPE ad_persnum,
       END   OF gty_knvk,

        gtt_knvk         TYPE STANDARD TABLE OF gty_knvk.

TYPES: BEGIN OF gty_adrc,
        addrnumber       TYPE ad_addrnum,
        name1            TYPE ad_name1,
        city1            TYPE ad_city1,
        post_code1       TYPE ad_pstcd1,
        street           TYPE ad_street,
        str_suppl2       TYPE ad_strspp2,
        country          TYPE land1,
        region           TYPE regio,
       END   OF gty_adrc,

        gtt_adrc         TYPE STANDARD TABLE OF gty_adrc.

TYPES: BEGIN OF gty_adr2,
        addrnumber       TYPE ad_addrnum,
        persnumber       TYPE ad_persnum,
        tel_number       TYPE ad_tlnmbr,
        tel_extens       TYPE ad_tlxtns,
       END   OF gty_adr2,

        gtt_adr2         TYPE STANDARD TABLE OF gty_adr2.

TYPES: BEGIN OF gty_adr3,
        addrnumber       TYPE ad_addrnum,
        persnumber       TYPE ad_persnum,
        fax_number       TYPE ad_fxnmbr,
        fax_extens       TYPE ad_fxxtns,
        faxnr_long       TYPE ad_fxnrlng,
       END   OF gty_adr3,

        gtt_adr3         TYPE STANDARD TABLE OF gty_adr3.

TYPES: BEGIN OF gty_adr6,
        addrnumber       TYPE ad_addrnum,
        persnumber       TYPE ad_persnum,
        smtp_addr        TYPE ad_smtpadr,
       END   OF gty_adr6,

        gtt_adr6         TYPE STANDARD TABLE OF gty_adr6.

*eject
TYPES: BEGIN OF gty_data_comp,              " Vendor Composite
* LFA1 - 01
        iap_ritm_id      TYPE c LENGTH 15,  " RITM
        erp_id           TYPE c LENGTH  5,  " ERP_ID
        rec_type         TYPE c LENGTH  2,  " RECORD_TYPE
        iap_vendor_id    TYPE c LENGTH 15,  " IAP_VENDOR_NUMBER
        lifnr            TYPE c LENGTH 10,  " VENDOR_NUMBER
        comp_name        TYPE c LENGTH 35,  " COMPANY_REPORTING_NAME
        name1            TYPE c LENGTH 35,  " VENDOR_NAME
        name2            TYPE c LENGTH 35,  " VENDOR_NAME_2
        mcod1            TYPE c LENGTH 25,  " SEARCH_TERM_1
        mcod2            TYPE c LENGTH 25,  " SEARCH_TERM_2
        ktokk            TYPE c LENGTH  4,  " VENDOR_ACCOUNT_GROUP
        begru            TYPE c LENGTH  4,  " AUTHORIZATION_GROUP
        konzs            TYPE c LENGTH 10,  " CORPORATE_GROUP
        bahns            TYPE c LENGTH 25,  " SAP_SOURCING_VENDOR_NUMBER
        land1            TYPE c LENGTH  3,  " COUNTRY
        parnr_ap         TYPE c LENGTH 10,  " CONTACT_PERSON_ID
        name1_ap         TYPE c LENGTH 35,  " LAST_NAME
        namev_ap         TYPE c LENGTH 35,  " FIRST_NAME
        abtnr_ap         TYPE c LENGTH  4,  " DEPARTMENT
        pafkt_ap         TYPE c LENGTH  2,  " FUNCTION
        telf1_ap         TYPE c LENGTH 16,  " TELEPHONE
        fax_ap           TYPE c LENGTH 30,  " FAX
        email_ap         TYPE c LENGTH 241, " EMAIL_ADDRESS
        parnr_sc         TYPE c LENGTH 10,  " CONTACT_PERSON_PURCHASING
        name1_sc         TYPE c LENGTH 35,  " LAST_NAME_PURCHASING
        namev_sc         TYPE c LENGTH 35,  " FIRST_NAME_PURCHASING
        abtnr_sc         TYPE c LENGTH  4,  " DEPARTMENT_PURCHASING
        pafkt_sc         TYPE c LENGTH  2,  " FUNCTION_PURCHASING
        telf1_sc         TYPE c LENGTH 16,  " TELEPHONE_PURCHASING
        fax_sc           TYPE c LENGTH 30,  " FAX_PURCHASING
        email_sc         TYPE c LENGTH 241, " EMAIL_ADDRESS_PURCHASING
        telf1            TYPE c LENGTH 30,  " PHONE_NUMBER_1
        extn_tel         TYPE c LENGTH 10,  " EXTENSION_TEL_1
        telfx            TYPE c LENGTH 30,  " FAX_NUMBER
        extn_fax         TYPE c LENGTH 10,  " EXTENSION_FAX
        stcd1            TYPE c LENGTH 16,  " TAX_NUMBER_1
        stcd2            TYPE c LENGTH 11,  " TAX_NUMBER_2
        loevm            TYPE c LENGTH  1,  " CENTRAL_DELETION_FLAG
        sperr            TYPE c LENGTH  1,  " CENTRAL_POSTING_BLOCK
        sperm            TYPE c LENGTH  1,  " CENTRAL_PURCHASING_BLOCK
        sperz            TYPE c LENGTH  1,  " CENTRAL_PAYMENT_BLOCK
        name1_adrc       TYPE c LENGTH 40,  " LOCATION_NAME
        street           TYPE c LENGTH 60,  " STREET_ADDRESS
        str_suppl2       TYPE c LENGTH 40,  " STREET_ADDRESS_3
        city1            TYPE c LENGTH 40,  " CITY
        region           TYPE c LENGTH  3,  " STATE
        post_code1       TYPE c LENGTH 10,  " POSTAL_CODE
        country          TYPE c LENGTH  3,  " COUNTRY
        lnrza            TYPE c LENGTH 10,  " ALTERNATE_PAYEE_V
        xlfza            TYPE c LENGTH  1,  " PERMITTED_PAYEE_ALLOWED_V
*eject
* LFBK - 02
        bvtyp            TYPE c LENGTH  4,  " PARTNER_BANK_TYPE
        banks            TYPE c LENGTH  3,  " BANK_COUNTRY_KEY
        bankl            TYPE c LENGTH 15,  " BANK_KEY
        bankn            TYPE c LENGTH 18,  " BANK_ACCOUNT_NUMBER
        bkref            TYPE c LENGTH 20,  " REFERENCE
        bkont            TYPE c LENGTH  2,  " BANK_CONTROL_KEY
        waers_lfbk       TYPE c LENGTH  5,  " BANK_CHAIN_CURRENCY
        bankssnd         TYPE c LENGTH  3,  " BANK_COUNTRY_SENDER
        bankksnd         TYPE c LENGTH 15,  " BANK_KEY_SENDER
        chainno          TYPE c LENGTH  2,  " BANK_SEQN_CORRESPOND
        chainbankt       TYPE c LENGTH  1,  " BANK_TYPE_CORRESPOND
        chainbanks       TYPE c LENGTH  3,  " BANK_COUNTRY_CORRESPOND
        chainbankk       TYPE c LENGTH 15,  " BANK_KEY_CORRESPOND
        chainbankn       TYPE c LENGTH 35,  " BANK_ACCOUNT_CORRESPOND
        chngind_lfbk     TYPE c LENGTH  1,  " REMTACC_ACTIVE_INACTIVE
* LFB1 - 03
        bukrs            TYPE c LENGTH  4,  " COMPANY_CODE
        zterm_lfb1       TYPE c LENGTH  4,  " PAYMENT_TERM
        zwels            TYPE c LENGTH 10,  " PAYMENT_METHOD
        loevm_lfb1       TYPE c LENGTH  1,  " DELETION_FLAG_C
        sperr_lfb1       TYPE c LENGTH  1,  " POSTING_BLOCK
        zahls            TYPE c LENGTH  1,  " PAYMENT_BLOCK
        busab            TYPE c LENGTH  2,  " ACCOUNTING_CLERK
        akont            TYPE c LENGTH 10,  " RECON_ACCOUNT
        zuawa            TYPE c LENGTH  3,  " SORT_KEY
        fdgrv            TYPE c LENGTH 10,  " CASH_MGMT_GROUP
        mindk            TYPE c LENGTH  3,  " MINORITY_INDICATOR
        cerdt            TYPE c LENGTH  8,  " CERTIFICATION_DATE
        zindt            TYPE c LENGTH  8,  " LAST_KEY_DATE
        reprf            TYPE c LENGTH  1,  " CHECK_DOUBLE_INVOICES
        lnrzb            TYPE c LENGTH 10,  " ALTERNATE_PAYEE_C
        xlfzb            TYPE c LENGTH  1,  " PERMITTED_PAYEE_ALLOWED_C
        hbkid            TYPE c LENGTH  5,  " HOUSE_BANK
        xpore            TYPE c LENGTH  1,  " INDIVIDUAL_PAYMENT
        uzawe            TYPE c LENGTH  2,  " PAYMENT_METHOD_SUPP
        qland            TYPE c LENGTH  3,  " WHT_COUNTRY
* LFZA - 04
        empfk            TYPE c LENGTH 10,  " PERMITTED_PAYEE
        chngind_lfza     TYPE c LENGTH  1,  " PRMPAYE_ACTIVE_INACTIVE
* LFBW - 05
        witht            TYPE c LENGTH  2,  " WHT_TAX_TYPE
        wt_withcd        TYPE c LENGTH  2,  " WHT_TAX_CODE
        wt_subjct        TYPE c LENGTH  1,  " LIABLE_WITHHOLDING_TAX
        qsrec            TYPE c LENGTH  2,  " TYPE_OF_RECIPIENT
        wt_wtstcd        TYPE c LENGTH 16,  " WHT_TAX_IDENTIFICATION_NUM
        chngind_lfbw     TYPE c LENGTH  1,  " WITHHLD_ACTIVE_INACTIVE
*eject
* LFM1 - 06
        ekorg            TYPE c LENGTH  4,  " PURCHASING_ORG
        sperm_lfm1       TYPE c LENGTH  1,  " PURCHASING_BLOCK
        loevm_lfm1       TYPE c LENGTH  1,  " DELETION_FLAG_P
        waers_lfm1       TYPE c LENGTH  5,  " PO_CURRENCY
        zterm_lfm1       TYPE c LENGTH  4,  " PURCHASE_ORG_PAYMENT_TERM
        inco1            TYPE c LENGTH  3,  " INCOTERMS_PART_1
        inco2            TYPE c LENGTH 28,  " INCOTERMS_PART_2
        kalsk            TYPE c LENGTH  2,  " SCHEMA_GROUP
        meprf            TYPE c LENGTH  1,  " PRICING_DATE_CONTROL
        eikto            TYPE c LENGTH 12,  " ACC_VENDOR
        webre            TYPE c LENGTH  1,  " GR_INV_VERIFICATION
        kzaut            TYPE c LENGTH  1,  " AUTO_PURCHASE_ORDER
* WYT3 - 07
        parvw            TYPE c LENGTH  2,  " PARTNER_FUNCTION
        parza            TYPE c LENGTH  3,  " PARTNER_COUNTER
        lifn2            TYPE c LENGTH 10,  " LEGAL_ENTITY_REFERENCE
        defpa            TYPE c LENGTH  1,  " DEFAULT_PARTNER
        chngind_wyt3     TYPE c LENGTH  1,  " PARTNER_ACTIVE_INACTIVE
       END   OF gty_data_comp.

TYPES:  gty_output       TYPE string,

        gtt_output       TYPE STANDARD TABLE OF gty_output.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_delim         TYPE char2 VALUE '|~',
        gc_ucomm_onli    TYPE sscrfields-ucomm VALUE 'ONLI',
        gc_lgcfilpth     TYPE pathintern
                         VALUE 'ZFAPI115_VENDOR_MASTER',
        gc_filename      TYPE fileintern
                         VALUE 'SSSSS_Vndr_Mstr_YYMMDDHHMM_NNN.CSV'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_cn_recs_01    TYPE i,
        gv_cn_recs_02    TYPE i,
        gv_cn_recs_03    TYPE i,
        gv_cn_recs_04    TYPE i,
        gv_cn_recs_05    TYPE i,
        gv_cn_recs_06    TYPE i,
        gv_cn_recs_07    TYPE i,
        gv_cn_recs_total TYPE i,
        gv_cn_recs_max   TYPE i,
        gv_cn_recs_file  TYPE i,
        gv_cn_files      TYPE i,
        gv_filename      TYPE text256,
        gv_filename_p    TYPE text256.

*eject
************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gs_lifnr         TYPE gty_lifnr,
        gs_lfa1_key      TYPE gty_lfa1_key,
        gs_lfa1          TYPE gty_lfa1,
        gs_lfbk_key      TYPE gty_lfbk_key,
        gs_lfbk          TYPE gty_lfbk,
        gs_tbchain21_key TYPE gty_tbchain21_key,
        gs_lfb1_key      TYPE gty_lfb1_key,
        gs_lfb1          TYPE gty_lfb1,
        gs_lfza_key      TYPE gty_lfza_key,
        gs_lfza          TYPE gty_lfza,
        gs_lfbw_key      TYPE gty_lfbw_key,
        gs_lfbw          TYPE gty_lfbw,
        gs_lfm1_key      TYPE gty_lfm1_key,
        gs_lfm1          TYPE gty_lfm1,
        gs_wyt3_key      TYPE gty_wyt3_key,
        gs_wyt3          TYPE gty_wyt3,
        gs_knvk          TYPE gty_knvk,
        gs_adrc          TYPE gty_adrc,
        gs_adr2          TYPE gty_adr2,
        gs_adr3          TYPE gty_adr3,
        gs_adr6          TYPE gty_adr6.

*eject
************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_lifnr         TYPE gtt_lifnr,
        gt_lfa1_key      TYPE gtt_lfa1_key,
        gt_lfa1          TYPE gtt_lfa1,
        gt_lfbk_key      TYPE gtt_lfbk_key,
        gt_lfbk          TYPE gtt_lfbk,
        gt_tbchain21_key TYPE gtt_tbchain21_key,
        gt_tbchain21     TYPE gtt_tbchain21,
        gt_lfb1_key      TYPE gtt_lfb1_key,
        gt_lfb1          TYPE gtt_lfb1,
        gt_lfza_key      TYPE gtt_lfza_key,
        gt_lfza          TYPE gtt_lfza,
        gt_lfbw_key      TYPE gtt_lfbw_key,
        gt_lfbw          TYPE gtt_lfbw,
        gt_lfm1_key      TYPE gtt_lfm1_key,
        gt_lfm1          TYPE gtt_lfm1,
        gt_wyt3_key      TYPE gtt_wyt3_key,
        gt_wyt3          TYPE gtt_wyt3,
        gt_knvk          TYPE gtt_knvk,
        gt_adrc          TYPE gtt_adrc,
        gt_adr2          TYPE gtt_adr2,
        gt_adr3          TYPE gtt_adr3,
        gt_adr6          TYPE gtt_adr6,
        gt_output        TYPE gtt_output.
