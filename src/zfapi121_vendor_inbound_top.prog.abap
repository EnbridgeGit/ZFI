*&---------------------------------------------------------------------*
*&  Include           ZFAPI121_VENDOR_INBOUND_TOP
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFAPI121_VENDOR_INBOUND                       *
* Program Include    :   ZFAPI121_VENDOR_INBOUND_TOP                   *
* Author             :                                                 *
* Date               :   Apr 15, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Vendor Inbound Interface                      *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 15-Apr-2018  CPALYAM     D30K929071-Initial development              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:   sscrfields.

*eject
*=======================================================================
* GLOBAL TYPES
*=======================================================================
TYPES: BEGIN OF gty_data_comp,              " Vendor Composite
* LFA1 - 01 - SAP Vendor Legal Entity
        zz_iap_ritm_id   TYPE c LENGTH 15,  " RITM
        erpid            TYPE c LENGTH  5,  " ERP_ID
        recty            TYPE c LENGTH  2,  " RECORD_TYPE
        zz_iap_vendor_id TYPE c LENGTH 15,  " IAP_VENDOR_NUMBER
        lifnr            TYPE c LENGTH 10,  " VENDOR_NUMBER
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
        smtp_ap          TYPE c LENGTH 241, " EMAIL_ADDRESS

        parnr_sc         TYPE c LENGTH 10,  " CONTACT_PERSON_PURCHASING
        name1_sc         TYPE c LENGTH 35,  " LAST_NAME_PURCHASING
        namev_sc         TYPE c LENGTH 35,  " FIRST_NAME_PURCHASING
        abtnr_sc         TYPE c LENGTH  4,  " DEPARTMENT_PURCHASING
        pafkt_sc         TYPE c LENGTH  2,  " FUNCTION_PURCHASING
        telf1_sc         TYPE c LENGTH 16,  " TELEPHONE_PURCHASING
        fax_sc           TYPE c LENGTH 30,  " FAX_PURCHASING
        smtp_sc          TYPE c LENGTH 241, " EMAIL_ADDRESS_PURCHASING

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
* LFBK - 02 - SAP LE Remittance Accounts
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
* LFB1 - 03 - SAP Company Code Data
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
* LFZA - 04 -  SAP Permitted Alternative Payee
        empfk            TYPE c LENGTH 10,  " PERMITTED_PAYEE
        chngind_lfza     TYPE c LENGTH  1,  " PRMPAYE_ACTIVE_INACTIVE
* LFBW - 05 - SAP Company Unit WHT
        witht            TYPE c LENGTH  2,  " WHT_TAX_TYPE
        wt_withcd        TYPE c LENGTH  2,  " WHT_TAX_CODE
        wt_subjct        TYPE c LENGTH  1,  " LIABLE_WITHHOLDING_TAX
        qsrec            TYPE c LENGTH  2,  " TYPE_OF_RECIPIENT
        wt_wtstcd        TYPE c LENGTH 16,  " WHT_TAX_IDENTIFICATION_NUM
        chngind_lfbw     TYPE c LENGTH  1,  " WITHHLD_ACTIVE_INACTIVE
*eject
* LFM1 - 06 - SAP Purchasing Org Data
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
* WYT3 - 07 -  SAP Supplier LE Relationship
        parvw            TYPE c LENGTH  2,  " PARTNER_FUNCTION
        parza            TYPE c LENGTH  3,  " PARTNER_COUNTER
        lifn2            TYPE c LENGTH 10,  " LEGAL_ENTITY_REFERENCE
        defpa            TYPE c LENGTH  1,  " DEFAULT_PARTNER
        chngind_wyt3     TYPE c LENGTH  1,  " PARTNER_ACTIVE_INACTIVE
       END   OF gty_data_comp.

*eject
TYPES: BEGIN OF gty_vle,
        zz_iap_ritm_id   TYPE z_iap_ritm_id,
        erpid(5)         TYPE c,
        recty(2)         TYPE c,
        zz_iap_vendor_id TYPE z_iap_vendor_id,
        lifnr            TYPE lifnr,
        name1            TYPE name1_gp,
        name2            TYPE name2_gp,
        mcod1            TYPE mcdk1,
        mcod2            TYPE mcdk2,
        ktokk            TYPE ktokk,
        begru            TYPE brgru,
        konzs            TYPE konzs,
        bahns            TYPE bahns,
        land1            TYPE land1_gp,
* Contact Data 1 - AP Remittance Contact
        parnr_ap         TYPE parnr,
        name1_ap         TYPE name1_gp,
        namev_ap         TYPE namev_vp,
        abtnr_ap         TYPE abtnr_pa,
        pafkt_ap         TYPE pafkt,
        telf1_ap         TYPE telf1,
        fax_ap           TYPE ad_fxnmbr,
        smtp_ap          TYPE ad_smtpadr,
* Contact Data 1 - Supply Chain Contact
        parnr_sc         TYPE parnr,
        name1_sc         TYPE name1_gp,
        namev_sc         TYPE namev_vp,
        abtnr_sc         TYPE abtnr_pa,
        pafkt_sc         TYPE pafkt,
        telf1_sc         TYPE telf1,
        fax_sc           TYPE ad_fxnmbr,
        smtp_sc          TYPE ad_smtpadr,
        telf1            TYPE telf1,
        extn_tel         TYPE c,
        telfx            TYPE telfx,
        extn_fax         TYPE c,
        stcd1            TYPE stcd1,
        stcd2            TYPE stcd2,
        loevm            TYPE loevm_x,
        sperr            TYPE sperb_x,
        sperm            TYPE sperm_x,
        sperz            TYPE sperz,
        name1_adrc       TYPE ad_name1,
        street           TYPE ad_street,
        str_suppl2       TYPE ad_strspp2,
        city1            TYPE ad_city1,
        region           TYPE regio,
        post_code1       TYPE ad_pstcd1,
        country          TYPE land1,
        lnrza            TYPE lnrza,
        xlfza            TYPE xlfza,
       END   OF gty_vle,

        gtt_vle          TYPE STANDARD TABLE OF gty_vle.

*eject
TYPES: BEGIN OF gty_ler,
        zz_iap_ritm_id   TYPE z_iap_ritm_id,
        erpid(5)         TYPE c,
        recty(2)         TYPE c,
        zz_iap_vendor_id TYPE z_iap_vendor_id,
        lifnr            TYPE lifnr,
        bvtyp            TYPE bvtyp,
        banks            TYPE banks,
        bankl            TYPE bankk,
        bankn            TYPE bankn,
        bkref            TYPE bkref,
        bkont            TYPE bkont,
        waers            TYPE waers,
        bankssnd         TYPE bankssnd,
        bankksnd         TYPE bankksnd,
        chainno          TYPE chainno,
        chainbankt       TYPE chainbankt,
        chainbanks       TYPE chainbanks,
        chainbankk       TYPE chainbankk,
        chainbankn       TYPE chainbankn,
        chngind_lfbk     TYPE char1,
       END   OF gty_ler,

        gtt_ler          TYPE STANDARD TABLE OF gty_ler.

TYPES: BEGIN OF gty_ccd,
        zz_iap_ritm_id   TYPE z_iap_ritm_id,
        erpid(5)         TYPE c,
        recty(2)         TYPE c,
        zz_iap_vendor_id TYPE z_iap_vendor_id,
        lifnr            TYPE lifnr,
        bukrs            TYPE bukrs,
        zterm_lfb1       TYPE dzterm,
        zwels            TYPE dzwels,
        loevm_lfb1       TYPE loevm_b,
        sperr_lfb1       TYPE sperb_b,
        zahls            TYPE dzahls,
        busab            TYPE busab,
        akont            TYPE akont,
        zuawa            TYPE dzuawa,
        fdgrv            TYPE fdgrv,
        mindk            TYPE mindk,
        cerdt            TYPE cerdt,
        zindt            TYPE dzindt,
        reprf            TYPE reprf,
        lnrzb            TYPE lnrzb,
        xlfzb            TYPE xlfzb,
        hbkid            TYPE hbkid,
        xpore            TYPE xpore,
        uzawe            TYPE uzawe,
        qland            TYPE qland,
       END   OF gty_ccd,

        gtt_ccd          TYPE STANDARD TABLE OF gty_ccd.

*eject
TYPES: BEGIN OF gty_pap,
        zz_iap_ritm_id   TYPE z_iap_ritm_id,
        erpid(5)         TYPE c,
        recty(2)         TYPE c,
        zz_iap_vendor_id TYPE z_iap_vendor_id,
        lifnr            TYPE lifnr,
        bukrs            TYPE bukrs,
        empfk            TYPE empfk,
        chngind_lfza     TYPE char1,
       END   OF gty_pap,

        gtt_pap          TYPE STANDARD TABLE OF gty_pap.

TYPES: BEGIN OF gty_cuw,
        zz_iap_ritm_id   TYPE z_iap_ritm_id,
        erpid(5)         TYPE c,
        recty(2)         TYPE c,
        zz_iap_vendor_id TYPE z_iap_vendor_id,
        lifnr            TYPE lifnr,
        bukrs            TYPE bukrs,
        witht            TYPE witht,
        wt_withcd        TYPE wt_withcd,
        wt_subjct        TYPE wt_subjct,
        qsrec            TYPE wt_qsrec,
        wt_wtstcd        TYPE wt_wtstcd,
        chngind_lfbw     TYPE char1,
       END OF gty_cuw,

        gtt_cuw          TYPE STANDARD TABLE OF gty_cuw.

TYPES: BEGIN OF gty_pod,
        zz_iap_ritm_id   TYPE z_iap_ritm_id,
        erpid(5)         TYPE c,
        recty(2)         TYPE c,
        zz_iap_vendor_id TYPE z_iap_vendor_id,
        lifnr            TYPE lifnr,
        ekorg            TYPE ekorg,
        sperm_lfm1       TYPE sperm_m,
        loevm_lfm1       TYPE loevm_m,
        waers_lfm1       TYPE bstwa,
        zterm_lfm1       TYPE dzterm,
        inco1            TYPE inco1,
        inco2            TYPE inco2,
        kalsk            TYPE kalsk,
        meprf            TYPE meprf,
        eikto            TYPE eikto_m,
        webre            TYPE webre,
        kzaut            TYPE kzaut,
       END   OF gty_pod,

        gtt_pod          TYPE STANDARD TABLE OF gty_pod.

*eject
TYPES: BEGIN OF gty_sler,
        zz_iap_ritm_id   TYPE z_iap_ritm_id,
        erpid(5)         TYPE c,
        recty(2)         TYPE c,
        zz_iap_vendor_id TYPE z_iap_vendor_id,
        lifnr            TYPE lifnr,
        ekorg            TYPE ekorg,
        parvw            TYPE parvw,
        parza            TYPE parza,
        lifn2            TYPE lifn2,
        defpa            TYPE defpa,
        chngind_wyt3     TYPE char1,
       END   OF gty_sler,

        gtt_sler         TYPE STANDARD TABLE OF gty_sler.

TYPES: BEGIN OF gty_kdata,                       "Key Data
        zz_iap_vendor_id TYPE z_iap_vendor_id,
        lifnr            TYPE lifnr,
       END   OF gty_kdata,

        gtt_kdata        TYPE STANDARD TABLE OF gty_kdata.

TYPES: BEGIN OF gty_output,
        erpid(5)         TYPE c,
        zz_iap_vendor_id TYPE z_iap_vendor_id,
        lifnr            TYPE lifnr,
        name1            TYPE name1_gp,
        status(1)        TYPE c,
        msg(255)         TYPE c,
       END   OF gty_output,

        gtt_output       TYPE STANDARD TABLE OF gty_output.

TYPES: BEGIN OF gty_lfza,
        empfk            TYPE empfk,
       END   OF gty_lfza,

        gtt_lfza         TYPE STANDARD TABLE OF gty_lfza.

TYPES:  gty_dir_list     TYPE epsfili,           "Directory List
        gtt_dir_list     TYPE STANDARD TABLE OF gty_dir_list.

TYPES:  gty_out_data     TYPE string,            "Out Data
        gtt_out_data     TYPE STANDARD TABLE OF gty_out_data.

TYPES:  gty_log_data     TYPE string,            "Log Data
        gtt_log_data     TYPE STANDARD TABLE OF gty_log_data.

*eject
TYPES: BEGIN OF gty_file_list,                   "File List            "
        file_nbr         TYPE numc3,             "Number-File          "
        fsprs            TYPE xflag,             "File Server - Presentn
        fsapl            TYPE xflag,             "File Server - Applictn
        fp_in_main       TYPE text128,           "Filepath In Main     "
        fn_in_main       TYPE text255,           "Filename In Main     "
        fp_in_arch       TYPE text128,           "Filepath In Archive  "
        fn_in_arch       TYPE text128,           "Filename In Archive  "
        fp_out_temp      TYPE text128,           "Filepath Out Temp    "
        fn_out_temp      TYPE text128,           "Filename Out Temp    "
        fp_out_arch      TYPE text128,           "Filepath Out Archive "
        fn_out_arch      TYPE text128,           "Filename Out Archive "
        fp_out_main      TYPE text128,           "Filepath Out Main    "
        fn_out_main      TYPE text255,           "Filename Out Main    "
        fp_log_main      TYPE text128,           "Filepath Log Main    "
        fn_log_main      TYPE text255,           "Filename Log Main    "
        cn_recs          TYPE numc10,            "Count of the Records "
       END   OF gty_file_list,

        gtt_file_list    TYPE STANDARD TABLE OF gty_file_list.

*eject
*=======================================================================
* GLOBAL CONSTANTS
*=======================================================================
CONSTANTS:
        gc_x             TYPE c                  "True/False; Yes/No   "
                         VALUE 'X',
        gc_fpth1a        TYPE text128            "Filepath - Inbound   "
                         VALUE
          '/usr/sap/interfaces/D30/IAP_BPO/I_P2C_AP_121/',
        gc_fnam1a        TYPE text128            "Filename - Inbound   "
                         VALUE '*',
        gc_fregxa        TYPE text128            "File REGEX - Inbound "
                         VALUE '^SAPUG_VENDOR_\d{8}\_\d{6}\.CSV$',
        gc_farc1a        TYPE text128            "Filepath - In Archive"
                         VALUE
          '/usr/sap/interfaces/D30/IAP_BPO/I_P2C_AP_121/Arch/',
        gc_fpth2a        TYPE text128            "Filepath - Outbound  "
                         VALUE
          '/usr/sap/interfaces/D30/IAP_BPO/I_P2C_AP_121/',
        gc_fnam2a        TYPE text128            "Filename - Outbound  "
                         VALUE
          'ERPID_VENDOR_MSGS_YYYYMMDD_HHMMSS.CSV',
        gc_farc2a        TYPE text128            "Filepath - Out Archive
                         VALUE
          '/usr/sap/interfaces/D30/IAP_BPO/I_P2C_AP_121/Arch/',
        gc_fpth3a        TYPE text128            "Filepath - Log       "
                         VALUE
          '/usr/sap/interfaces/D30/IAP_BPO/I_P2C_AP_121/Log/',
        gc_fnam3a        TYPE text128            "Filename - Log       "
                         VALUE
          'ERPID_VENDOR_LOG_YYYYMMDD_HHMMSS.TXT',
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP',
        gc_modif_id_fas  TYPE char3              "ModifID-File Appl.Srvr
                         VALUE 'FAS',
        gc_modif_id_fps  TYPE char3              "ModifID-File Pres.Srvr
                         VALUE 'FPS',
        gc_slash         TYPE char1              "Slash                "
                         VALUE '/',
        gc_delim         TYPE char2              "Delimiter            "
                         VALUE '|~'.

*eject
*=======================================================================
* GLOBAL VARIABLES
*=======================================================================
DATA:   gv_fl_errr_prcs  TYPE xflag,             "Error - Process      "
        gv_fl_errr_mstr  TYPE xflag,             "Error - Master       "
        gv_cnt_doc_trns  TYPE numc06,            "Count - Transactions "
        gv_cnt_doc_errr  TYPE numc06,            "Count - Docs In Error"
        gv_cnt_doc_wrng  TYPE numc06,            "Count - Docs W/ Warnin
        gv_cnt_doc_pass  TYPE numc06,            "Count - Docs Passed  "
        gv_filename_in   TYPE text255,           "Filename - Inbound   "
        gv_filename_inx  TYPE xflag,             "Filename - Inbound   "
        gv_filename_out  TYPE text255,           "Filename - Outbound  "
        gv_filename_otx  TYPE xflag,             "Filename - Outbound  "
        gv_filename_log  TYPE text255,           "Filename - Log       "
        gv_filename_lgx  TYPE xflag,             "Filename - Log       "
        gv_smtp_addr     TYPE ad_smtpadr,        "Email Address        "
        gv_trns          TYPE xflag,             "Transactions Flag    "
        gv_errr          TYPE xflag,             "Errors Flag          "
        gv_wrng          TYPE xflag,             "Warnings Flag        "
        gv_pass          TYPE xflag.             "Success Flag         "

*=======================================================================
* GLOBAL WORKAREAS
*=======================================================================
DATA:   gs_data          TYPE string,
        gs_tab_data      TYPE string,
        gs_vle           TYPE gty_vle,
        gs_ler           TYPE gty_ler,
        gs_ccd           TYPE gty_ccd,
        gs_pap           TYPE gty_pap,
        gs_cuw           TYPE gty_cuw,
        gs_pod           TYPE gty_pod,
        gs_sler          TYPE gty_sler,
        gs_kdata         TYPE gty_kdata,
        gs_bdcdata       TYPE bdcdata,
        gs_out_data      TYPE gty_out_data,      "Out Data             "
        gs_log_data      TYPE gty_log_data.      "Log Data             "

*eject
*=======================================================================
* GLOBAL INTERNAL TABLES
*=======================================================================
DATA:   gt_data          TYPE STANDARD TABLE OF string,
        gt_tab_data      TYPE STANDARD TABLE OF string,
        gt_vle           TYPE gtt_vle,
        gt_ler           TYPE gtt_ler,
        gt_ccd           TYPE gtt_ccd,
        gt_pap           TYPE gtt_pap,
        gt_pap_d         TYPE gtt_pap,
        gt_pap_c         TYPE gtt_pap,
        gt_pap_cd        TYPE gtt_pap,
        gt_cuw           TYPE gtt_cuw,
        gt_pod           TYPE gtt_pod,
        gt_sler          TYPE gtt_sler,
        gt_lfza          TYPE gtt_lfza,
        gt_kdata         TYPE gtt_kdata,
        gt_bdcdata       TYPE STANDARD TABLE OF bdcdata,
        gt_messages      TYPE STANDARD TABLE OF bdcmsgcoll,
        gt_file_list     TYPE gtt_file_list,     "File List            "
        gt_out_data      TYPE gtt_out_data,      "Out Data             "
        gt_log_data      TYPE gtt_log_data.      "Log Data             "

************************************************************************
* REFERRENCE VARIABLES
************************************************************************
* Reference for Interface monitor
*DATA: gref_util         TYPE REF TO zcl_iap_interface_util.

*eject
************************************************************************
*                                Macros                                *
************************************************************************
DEFINE macro_msg.                                "Macro - Message      "
  if         ( &1   eq 'B' ).
    skip        1.
    write:   / &3.
    if       ( &2   eq 'S' ).
      message  &3 type 'S'.
    else.
      message  &3 type 'I'.
    endif.
  elseif     ( &1   eq 'W' ).
    skip        1.
    write:   / &3.
  elseif     ( &1   eq 'M' ).
    if       ( &2   eq 'S' ).
      message  &3 type 'S'.
    else.
      message  &3 type 'I'.
    endif.
  endif.
  if         ( &2   eq 'A' ).
    gv_fl_errr_prcs  = 'X'.
    gv_fl_errr_mstr  = 'X'.
  endif.
END-OF-DEFINITION.

DEFINE macro_log.                                "Macro - Write Log    "
  if       ( rb_fsprs          is not initial ).
    clear                         gs_log_data.
    if     ( &1                gt 0 ).
      do     &1 times.
        append   gs_log_data   to gt_log_data.
      enddo.
    endif.
    move     &2                to gs_log_data.
    append   gs_log_data       to gt_log_data.
  elseif   ( gv_filename_lgx   is not initial ).
    if     ( &1                gt 0 ).
      do     &1 times.
        transfer space         to gv_filename_log.
      enddo.
    endif.
    transfer &2                to gv_filename_log.
  endif.
  if       ( &1                gt 0 ).
    skip     &1.
  endif.
  write:   / &2.
END-OF-DEFINITION.
