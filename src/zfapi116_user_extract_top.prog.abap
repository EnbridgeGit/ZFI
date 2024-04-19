*&---------------------------------------------------------------------*
*&  Include           ZFAPI116_USER_EXTRACT_TOP
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
*& Program Name       :  ZFAPI116_USER_EXTRACT                         *
*& Include            :  ZFAPI116_USER_EXTRACT_TOP                     *
*& Author             :  Kalinga Keshari Rout / Paul Karunakar         *
*& Creation Date      :  08-Mar-2018                                   *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  User data from each of the three SAP          *
*&                       instances will be extracted in a delimited    *
*&                       file and sent to IAP.                         *
*&                                                                     *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 08-Mar-2018  KROUT       D30K928695, CHG0105965  Initial development *
*                          D30K928697, D30K928840, D30K929003,         *
*                          D30K929075, D30K929081, D30K929083          *
************************************************************************

TYPE-POOLS: vrm.

TABLES: sscrfields, adrp, adrc, usr02, zapt_iap_user.

*eject
************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES:  gty_xparam     TYPE zfit_xparam,    "Parameter Master Table    "

        gtt_xparam     TYPE STANDARD TABLE OF gty_xparam.

TYPES:  gty_iap_user   TYPE zapt_iap_user,  "IAP User Change Table     "

        gtt_iap_user   TYPE STANDARD TABLE OF gty_iap_user.

TYPES:  gty_doa_data   TYPE zfis_doa_us,    "Deligation Of Authority   "

        gtt_doa_data   TYPE STANDARD TABLE OF gty_doa_data.

TYPES: BEGIN OF gty_valid_doc_types,        "Valid Document Types      "
        blart          TYPE blart,
        brgru          TYPE brgru,
       END   OF gty_valid_doc_types.

TYPES:  gty_user_names TYPE zaps_user_names. "User Names               "

TYPES:  gty_user_data  TYPE zaps_user_data. "User Data                 "

TYPES: BEGIN OF gty_usr02,                  "Logon Data                "
        uname          TYPE xubname,
        gltgv          TYPE xugltgv,
        gltgb          TYPE xugltgb,
       END   OF gty_usr02.

TYPES: BEGIN OF gty_usr21,                  "User Address Keys         "
        bname          TYPE xubname,
        persnumber     TYPE ad_persnum,
        addrnumber     TYPE ad_addrnum,
       END   OF gty_usr21.

TYPES: BEGIN OF gty_agr_users,              "Assignment Of Roles       "
        agr_name       TYPE agr_name,
        uname          TYPE xubname,
        from_dat       TYPE agr_fdate,
        to_dat         TYPE agr_tdate,
        status_resp    TYPE char1,
       END   OF gty_agr_users.

TYPES: BEGIN OF gty_xparam_resp,            "Parameters-Responsibilities
        paramtype      TYPE zparamtype,
        subtype        TYPE zparamsubtype,
        key1           TYPE zparamkey,
        key2           TYPE zparamkey,
        key3           TYPE zparamkey,
        key4           TYPE zparamkey,
        key5           TYPE zparamkey,
        value1         TYPE agr_name,
        value2         TYPE zparamvalue,
       END   OF gty_xparam_resp.

*eject
TYPES: BEGIN OF gty_adrp,                   "Persons                   "
        persnumber     TYPE ad_persnum,
        name_first     TYPE ad_namefir,
        name_last      TYPE ad_namelas,
       END   OF gty_adrp.

TYPES: BEGIN OF gty_adrc,                   "Addresses                 "
        addrnumber     TYPE ad_addrnum,
        country        TYPE land1,
       END   OF gty_adrc.

TYPES: BEGIN OF gty_adr6,                   "Email Addresses           "
        addrnumber     TYPE ad_addrnum,
        persnumber     TYPE ad_persnum,
        smtp_addr      TYPE ad_smtpadr,
       END   OF gty_adr6.

TYPES: BEGIN OF gty_final,                  "Final Layout              "
        erpid          TYPE zerpid,         "ERP_Id                    "
        fname          TYPE pad_vorna,      "First_Name                "
        lname          TYPE pad_nachn,      "Family_Name               "
        uname          TYPE xubname,        "User_Name                 "
        pernr          TYPE persno,         "Personnel_Number          "
        resp           TYPE zparamkey,      "Responsibility            "
        sp_limit       TYPE char21,         "Spending_Limit            "
        sp_type        TYPE char2,          "Spending_Limit_Type       "
        blart          TYPE blart,          "Document_Type             "
        land1          TYPE land1,          "Org_Id                    "
        addr           TYPE ad_smtpadr,     "Email_Address             "
        uname_mgr      TYPE zuname_mgr,     "Manager_User_Name         "
        pernr_mgr      TYPE zpernr_mgr,     "Manager_Personnel_Number  "
        company        TYPE char8,          "Company                   "
        status_user    TYPE zstatus_user,   "User_Status               "
        status_resp    TYPE zstatus_resp,   "Responsibility_Status     "
       END   OF gty_final.

TYPES: BEGIN OF gty_output,
        erpid          TYPE char5,          "ERP_Id                    "
        fill01         TYPE char3,          "Filler                    "
        uname          TYPE char12,         "User_Name                 "
        fill02         TYPE char3,          "Filler                    "
        status_user    TYPE char1,          "User_Status               "
        fill03         TYPE char3,          "Filler                    "
        resp           TYPE char16,         "Responsibility            "
        fill04         TYPE char3,          "Filler                    "
        status_resp    TYPE char1,          "Responsibility_Status     "
        fill05         TYPE char3,          "Filler                    "
        blart          TYPE char2,          "Document_Type             "
        fill06         TYPE char3,          "Filler                    "
        sp_limit       TYPE char21,         "Spending_Limit            "
        fill07         TYPE char3,          "Filler                    "
        land1          TYPE char3,          "Country                   "
        fill08         TYPE char3,          "Filler                    "
        uname_mgr      TYPE char12,         "Manager_User_Name         "
       END   OF gty_output.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_x             TYPE c             VALUE 'X',
        gc_a             TYPE c             VALUE 'A',
        gc_i             TYPE c             VALUE 'I',
        gc_e             TYPE c             VALUE 'E',
        gc_g             TYPE c             VALUE 'G',
        gc_rfcname_us    TYPE zparamtype    VALUE 'ECCUS',
        gc_rfcname_hr    TYPE zparamtype    VALUE 'HR',
        gc_ucomm_onli    TYPE sscrfields-ucomm VALUE 'ONLI',
        gc_tcode_fv60    TYPE char4         VALUE 'FV60',
        gc_coder         TYPE char5         VALUE 'CODER',
        gc_general       TYPE char7         VALUE 'GENERAL'.

************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_filepath      TYPE STRING,
        gv_records       TYPE STRING,
        gv_count_recs    TYPE i,
        gv_full_extract  TYPE c,
        gv_rfcdest_us    TYPE tb_rfcdest,
        gv_rfcdest_hr    TYPE tb_rfcdest,
        gv_doa_limit     TYPE zfi_doa_amount,
        gv_chk           TYPE c,
        gv_flag_found    TYPE xflag.

************************************************************************
*                              Structures                              *
************************************************************************
DATA:   gs_xparam            TYPE gty_xparam,
        gs_iap_user          TYPE gty_iap_user,
        gs_iap_user_current  TYPE gty_iap_user,
        gs_iap_user_history  TYPE gty_iap_user,
        gs_iap_user_changes  TYPE gty_iap_user,
        gs_doa_data          TYPE gty_doa_data,
        gs_doa_data_p        TYPE gty_doa_data,
        gs_valid_doc_types   TYPE gty_valid_doc_types,
        gs_user_names        TYPE gty_user_names,
        gs_user_names_p      TYPE gty_user_names,
        gs_user_data         TYPE gty_user_data,
        gs_usr02             TYPE gty_usr02,
        gs_usr21             TYPE gty_usr21,
        gs_agr_users         TYPE gty_agr_users,
        gs_xparam_resp       TYPE gty_xparam_resp,
        gs_adrp              TYPE gty_adrp,
        gs_adrc              TYPE gty_adrc,
        gs_adr6              TYPE gty_adr6,
        gs_final             TYPE gty_final,
        gs_output            TYPE gty_output.

*eject
************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_xparam            TYPE STANDARD TABLE OF gty_xparam,
        gt_iap_user          TYPE STANDARD TABLE OF gty_iap_user,
        gt_iap_user_current  TYPE STANDARD TABLE OF gty_iap_user,
        gt_iap_user_history  TYPE STANDARD TABLE OF gty_iap_user,
        gt_iap_user_changes  TYPE STANDARD TABLE OF gty_iap_user,
        gt_doa_data          TYPE STANDARD TABLE OF gty_doa_data,
        gt_doa_data_p        TYPE STANDARD TABLE OF gty_doa_data,
        gt_valid_doc_types   TYPE STANDARD TABLE OF gty_valid_doc_types,
        gt_user_names        TYPE STANDARD TABLE OF gty_user_names,
        gt_user_names_p      TYPE STANDARD TABLE OF gty_user_names,
        gt_user_data         TYPE STANDARD TABLE OF gty_user_data,
        gt_usr02             TYPE STANDARD TABLE OF gty_usr02,
        gt_usr21             TYPE STANDARD TABLE OF gty_usr21,
        gt_agr_users         TYPE STANDARD TABLE OF gty_agr_users,
        gt_xparam_resp       TYPE STANDARD TABLE OF gty_xparam_resp,
        gt_adrp              TYPE STANDARD TABLE OF gty_adrp,
        gt_adrc              TYPE STANDARD TABLE OF gty_adrc,
        gt_adr6              TYPE STANDARD TABLE OF gty_adr6,
        gt_final             TYPE STANDARD TABLE OF gty_final.
