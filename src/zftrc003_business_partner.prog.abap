REPORT zftrc003_business_partner
       MESSAGE-ID zfi01.

*----------------------------------------------------------------------*
*Report Name         : ZFTRC003_BUSINESS_PARTNER
*Author              : Krishna Kancherla
*Date                : 07/12/2011
*Logical Database    : NA
*SAPScript name      : NA
*Application Area    : FI-TR
*Description         : This interface will upload the Business Partner
*                      Address, Bank, Company Code, Payment, authorizations
*                      data through FTB_BUPA_DARK_MAINTAIN_INTERN
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Modification details
* Version No    Date         Modified by       Correction No
* 1.0           07/12/2011  KRISHNA KANCHERLA  DECK901054
*Description: Initial Program creation
* 2.0           09/15/2011  KRISHNA KANCHERLA  DECK901998
*Description: Removed old code and changed the process from BDC to
*             FM logic FTB_BUPA_DARK_MAINTAIN_INTERN as per new file format
*----------------------------------------------------------------------*

TYPES:  BEGIN OF ty_bp_entries,
          col1(2)  TYPE c,      " Record Type
          col2     TYPE string, " Activity Type/Bank Details ID/Change Category/Change Category/Change Category/Change category
          col3     TYPE string, " Commit Work/Change Category/Validity Start/Currency Key/Contract Type/Currency Key
          col4     TYPE string, " Create Chg docs/Bank Ctr Key/Validity End/Iden of Pay Details/Prod Category/Contract Type
          col5     TYPE string, " BP Number/Bank Key/Co Name/Partner Bk Details/Product Type/Product Category
          col6     TYPE string, " BP in External/Bk Ac Number/City/Short Key for House Bk/Financial Transaction Type/Product Type
          col7     TYPE string, " BP Category/Bank Control Key/District/ID for Acc Details/Auth Indicator/Financial Transaction Type
          col8     TYPE string, " BP Type/Ref specifications for Bank/City Postal Code/Payment Method/Direction of flow
          col9     TYPE string, " BP Grouping/Act Holder Name/City Postal Code/Payment Method Suppl/Identification of Payment Details
          col10    TYPE string, " BP Role Category/Bank Details ID/PO Box/Payment Transaction/
          col11    TYPE string, " BP Role Category/Ind Collec Auth/Flag PO Box No/Payer/Payee
          col12    TYPE string, " BP Role/Name of Bank Ac/PO Box City/Generate Payment Req
          col13    TYPE string, " BP Role/Target Details for Chg Bank/City PO Box code/Individual Payment
          col14    TYPE string, " Valid from Date of BP Role/Validity Start Dt/Region for PO Box/Deter Grouping def
          col15    TYPE string, " Valid from Date of BP Role/Validity End Dt/PO Box Cty/Same Direction necessary for Joint Payment
          col16    TYPE string, " Valid To Date of BP Role/Date of Move/Post Delivery District/List of Pay methods to considered
          col17    TYPE string, " Valid from Date of BP Role/Bank Details/Street
          col18    TYPE string, " Valid To Date of BP Role//House Number
          col19    TYPE string, " Company code//Building
          col20    TYPE string, " Search Term//Country Key
          col21    TYPE string, " Name1//Language Key
          col22    TYPE string, " Name2//Region
          col23    TYPE string, " Name3//Search Term1
          col24    TYPE string,                             " Name4
        END OF ty_bp_entries,

        BEGIN OF ty_bp_upload,
*Header Data
         i_partner      TYPE but000-partner,        " Business Partner Number
         i_bukrs        TYPE t001-bukrs,            " Company Code
         i_aktyp        TYPE tbz0k-aktyp,           " Activity Type
         i_xcommit      TYPE boole-boole,           " Commit Work
         i_xchdoc       TYPE boole-boole,           " Create Change Documents
         i_bpext        TYPE but000-bpext,          " Business Partner Number in External System
         i_type         TYPE but000-type,           " Business partner category
         i_bpkind       TYPE but000-bpkind,         " Business Partner Type
         i_group        TYPE but000-bu_group,       " Business Partner Grouping
         i_rltyp        TYPE bu_partnerrolecat,     " BP Role Category
         i_rltyp2       TYPE bu_partnerrolecat,     " BP Role Category
         i_role1        TYPE bu_partnerrole,        " BP Role
         i_role2        TYPE bu_partnerrole,        " BP Role
         i_valid_from_1 TYPE bu_role_valid_from_di, " Valid From Date of a BP Role (YYYYMMDD)
         i_valid_to_1   TYPE bu_role_valid_to_di,   " Valid To Date of a BP Role (YYYYMMDD)
         i_valid_from_2 TYPE bu_role_valid_from_di, " Valid From Date of a BP Role (YYYYMMDD)
         i_valid_to_2   TYPE bu_role_valid_to_di,   " Valid To Date of a BP Role (YYYYMMDD)
         i_valdt        TYPE bus0di2-valdt,         " Validity Date of Changes (Direct Input)
         bu_sort1       TYPE bu_sort1,              " Search term 1 for business partner
         name_org1      TYPE bu_nameor1,            " Name 1 of organization
         name_org2      TYPE bu_nameor2,            " Name 2 of organization
         name_org3      TYPE bu_nameor3,            " Name 3 of organization
         name_org4      TYPE bu_nameor4,            " Name 4 of organization
*Bank details (direct input)
         bkvid          TYPE bu_bkvid,              " Bank details ID
         chind_bank     TYPE bu_chind,              " Change category
         banks          TYPE banks,                 " Bank country key
         bankl          TYPE bankk,                 " Bank Keys
         bankn          TYPE bankn,                 " Bank account number
         bkont          TYPE bkont,                 " Bank Control Key
         bkref          TYPE bkref,                 " Reference specifications for bank details
         koinh          TYPE bu_koinh,              " Account Holder Name
         bkext          TYPE bu_bkext,              " Bank details ID in external system
         xezer          TYPE bu_xezer,              " Indicator: Collection Authorization
         accname        TYPE bu_bankaccname,        " Name of Bank Account
         move_bkvid     TYPE bu_move_bkvid,         " Target Details ID for Change of Bank Details (BP)
         bk_date_from   TYPE bu_valid_date_from_di, " Validity Start (DI)
         bk_date_to     TYPE bu_valid_date_to_di,   " Validity End (DI)
         bk_move_date   TYPE bu_move_date_di,       " Date of Move (DI)
         iban           TYPE iban,                  " IBAN (International Bank Account Number)
*Address data transfer structure (direct input)
         chind_addr     TYPE bu_chind,              " Change category
         addr_date_from TYPE bu_valid_date_from_di, " Validity Start (DI)
         addr_date_to   TYPE bu_valid_date_to_di,   " Validity End (DI)
         name_co        TYPE ad_name_co,            " c/o name
         city1          TYPE ad_city1,              " City
         city2          TYPE ad_city2,              " District
         post_code1     TYPE ad_pstcd1,             " City postal code
         pcode1_ext     TYPE ad_pst1xt,             " (Not Supported)City Postal Code Extension, e.g. ZIP+4+2 Code
         po_box         TYPE ad_pobx,               " PO Box
         po_box_num     TYPE ad_pobxnum,            " Flag: PO Box without number
         po_box_loc     TYPE ad_pobxloc,            " PO Box city
         city_code2     TYPE ad_cit2num,            " City PO box code (City file)
         po_box_reg     TYPE ad_pobxreg,            " Region for PO Box (Country, State, Province, ...)
         po_box_cty     TYPE ad_pobxcty,            " PO box country
         postalarea     TYPE ad_pstlar,             " (Not Supported) Post Delivery District
         street         TYPE ad_street,             " Street
         house_num1     TYPE ad_hsnm1,              " House Number
         building       TYPE ad_bldng,              " Building (Number or Code)
         country        TYPE land1,                 " Country Key
         langu          TYPE spras,                 " Language Key
         region         TYPE regio,                 " Region (State, Province, County)
         sort1          TYPE ad_sort1ul,            " Search Term 1
*Payment Details
         chind_sc1      TYPE bu_chind,              " Change category
         waers_c1       TYPE bp_waers_di,           " Currency Key
         zahlvid_c1     TYPE tb_zahlvid,            " Identification of Payment Details (Standing Instructions)
         bvtyp          TYPE tb_rpbank,             " Partner bank details
         hbkid_c1       TYPE hbkid,                 " Short Key for a House Bank
         hktid          TYPE hktid,                 " ID for Account Details
         zlsch          TYPE dzlsch,                " Payment Method
         uzawe_c1       TYPE uzawe,                 " Payment Method Supplement
         szart          TYPE tb_szart,              " Payment transaction
         rpzahl         TYPE tb_rpzahl_new,         " Payer/payee
         spayrq         TYPE tb_spayrqk,            " Generate payment request
         sprsng         TYPE tb_sprsngk,            " Individual payment
         sprgrd         TYPE tb_sprgrd,             " Determine grouping definition
         scspay         TYPE tb_scspay,             " Same direction necessary for joint payment?
         zwels_c1       TYPE dzwels,                " List of the Payment Methods to be Considered
*Treasury Attribute SI Authorizations
         chind_si3      TYPE bu_chind,              " Change category
         rantyp_a3      TYPE rantyp,                " Contract Type
         sanlf_a3       TYPE sanlf_di,              " Product Category
         sgsart_a3      TYPE vvsart,                " Product Type
         sfhaart_a3     TYPE tb_sfhaart,            " Financial Transaction Type
         author         TYPE tb_author,             " Authorization indicator for Treasury product
*Assignment of Payment Details as SI
         chind_si1      TYPE bu_chind,              " Change category
         waers_a1       TYPE bp_waers_di,           " Currency Key
         rantyp_a1      TYPE rantyp,                " Contract Type
         sanlf_a1       TYPE sanlf_di,              " Product Category
         sgsart_a1      TYPE vvsart,                " Product Type
         sfhaart_a1     TYPE tb_sfhaart,            " Financial Transaction Type
         ssign          TYPE tb_ssign,              " Direction of flow
         zahlvid        TYPE tb_zahlvid,            " Identification of Payment Details (Standing Instructions)
  END OF ty_bp_upload,

      BEGIN OF ty_output,
         i_partner      TYPE but000-partner,
         i_bukrs        TYPE t001-bukrs,
         msgtyp(1)      TYPE c,
         msg            TYPE string,
      END OF ty_output.

DATA : gwa_output   TYPE ty_output.

DATA : git_upload     TYPE TABLE OF string,
       git_bp_upload  TYPE TABLE OF ty_bp_upload,
       git_output     TYPE TABLE OF ty_output,
       git_bp_entries TYPE TABLE OF ty_bp_entries.

DATA : gv_filename   TYPE string,
       gv_err_flg(1) TYPE c,
       gv_total      TYPE i,
       gv_success    TYPE i,
       gv_fail       TYPE i.

CONSTANTS: gc_x(1)  TYPE c VALUE 'X',
           gc_h(1)  TYPE c VALUE 'H',
           gc_bk(2) TYPE c VALUE 'BK',
           gc_ad(2) TYPE c VALUE 'AD',
           gc_py(2) TYPE c VALUE 'PY',
           gc_pt(2) TYPE c VALUE 'PT',
           gc_ps(2) TYPE c VALUE 'PS'.

*----------------------------------------------------------------------*
*                 Selection Screen.                                    *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-001.
PARAMETERS: rb_prs RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND cmd,
            rb_app RADIOBUTTON GROUP grp1 .

PARAMETERS: p_sep(1)    TYPE c  OBLIGATORY DEFAULT '|'.
PARAMETERS: p_pfile TYPE rlgrap-filename MODIF ID sc1,
            p_afile TYPE string MODIF ID sc2.
SELECTION-SCREEN END OF BLOCK blk3.

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*

INITIALIZATION.

  PERFORM f_clear_data.

*----------------------------------------------------------------------*
*                 At Selection-Screen validation                       *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM f_toggle_fields.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pfile.

* F4 Help for File Path
  PERFORM f_f4_help_presentation.

*----------------------------------------------------------------------*
* Start-of-Selection                                                   *
*----------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM f_upload_file.

  PERFORM f_build_data.

  PERFORM f_call_bapi.

*----------------------------------------------------------------------*
* End-of-Selection                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM f_display_output.

*&---------------------------------------------------------------------*
*&      Form  F_TOGGLE_FIELDS
*&---------------------------------------------------------------------*
*       Toggle the fields depending upon the radio button selection
*----------------------------------------------------------------------*
FORM f_toggle_fields .

  DATA: lc_sc1(3)     TYPE c VALUE 'SC1',
        lc_sc2(3)     TYPE c VALUE 'SC2',
        lc_zero(1)    TYPE c VALUE '0'.

  IF rb_prs EQ gc_x.
    LOOP AT SCREEN.
* Hide application server field
      IF  screen-group1 = lc_sc2.
        screen-active = lc_zero.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
*Hide Presentation server field
      IF  screen-group1 = lc_sc1.
        screen-active = lc_zero.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_TOGGLE_FIELDS

*&---------------------------------------------------------------------*
*&      Form  F_F4_HELP_PRESENTATION
*&---------------------------------------------------------------------*
*       F4 Help - Presentation server
*----------------------------------------------------------------------*
FORM f_f4_help_presentation .

  DATA:  lit_filetable  TYPE filetable,
         lwa_filetable  TYPE LINE OF filetable,
         lv_subrc       TYPE i VALUE 1.

  CONSTANTS:lv_title TYPE string VALUE 'SELECT FILE',
            lv_direc TYPE string VALUE 'C:\'.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_title
      initial_directory       = lv_direc
      multiselection          = ' '
    CHANGING
      file_table              = lit_filetable
      rc                      = lv_subrc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0.

    READ TABLE lit_filetable INTO lwa_filetable INDEX 1.
    IF sy-subrc = 0.
      p_pfile = lwa_filetable-filename.
    ENDIF.

  ENDIF.
ENDFORM.                    " F_F4_HELP_PRESENTATION

*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_FILE
*&---------------------------------------------------------------------*
*       Upload file from Presentation / Application Server
*----------------------------------------------------------------------*
FORM f_upload_file .

  IF rb_prs = gc_x.
    PERFORM f_upload_presentation.
  ELSEIF rb_app = gc_x.
    PERFORM f_read_application.
  ENDIF.

ENDFORM.                    " F_UPLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_PRESENTATION
*&---------------------------------------------------------------------*
*       Upload file from Presentation Server
*----------------------------------------------------------------------*
FORM f_upload_presentation .

  DATA: lwa_bp_entries TYPE ty_bp_entries,
        lwa_upload     TYPE string.

  IF NOT p_pfile IS INITIAL.

    gv_filename = p_pfile.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = gv_filename
      TABLES
        data_tab                = git_upload
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.

    IF sy-subrc <> 0.
      MESSAGE i000 WITH 'Error in uploading the file'(003).
      gv_err_flg = gc_x.
      LEAVE TO LIST-PROCESSING.
    ENDIF.
  ELSE.
    MESSAGE i000 WITH 'Input file should not be blank'(004).
    gv_err_flg = gc_x.
    LEAVE TO LIST-PROCESSING.
  ENDIF.

  LOOP AT git_upload INTO lwa_upload.

    SPLIT lwa_upload AT p_sep INTO  lwa_bp_entries-col1
                                    lwa_bp_entries-col2
                                    lwa_bp_entries-col3
                                    lwa_bp_entries-col4
                                    lwa_bp_entries-col5
                                    lwa_bp_entries-col6
                                    lwa_bp_entries-col7
                                    lwa_bp_entries-col8
                                    lwa_bp_entries-col9
                                    lwa_bp_entries-col10
                                    lwa_bp_entries-col11
                                    lwa_bp_entries-col12
                                    lwa_bp_entries-col13
                                    lwa_bp_entries-col14
                                    lwa_bp_entries-col15
                                    lwa_bp_entries-col16
                                    lwa_bp_entries-col17
                                    lwa_bp_entries-col18
                                    lwa_bp_entries-col19
                                    lwa_bp_entries-col20
                                    lwa_bp_entries-col21
                                    lwa_bp_entries-col22
                                    lwa_bp_entries-col23
                                    lwa_bp_entries-col24.

    APPEND lwa_bp_entries TO git_bp_entries.
    CLEAR  lwa_bp_entries.

  ENDLOOP.

ENDFORM.                    " F_UPLOAD_PRESENTATION

*&---------------------------------------------------------------------*
*&      Form  F_READ_APPLICATION
*&---------------------------------------------------------------------*
*       Read File from Application Server
*----------------------------------------------------------------------*
FORM f_read_application .

  DATA: lv_record_line TYPE char512,
        lwa_bp_entries TYPE ty_bp_entries.

* Open the File in Application Server
  OPEN DATASET p_afile FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  IF sy-subrc = 0.
    DO.
      CLEAR lv_record_line.
* Read File Contents
      READ DATASET p_afile INTO lv_record_line.
      IF sy-subrc = 0.
        SPLIT lv_record_line AT p_sep INTO  lwa_bp_entries-col1
                                            lwa_bp_entries-col2
                                            lwa_bp_entries-col3
                                            lwa_bp_entries-col4
                                            lwa_bp_entries-col5
                                            lwa_bp_entries-col6
                                            lwa_bp_entries-col7
                                            lwa_bp_entries-col8
                                            lwa_bp_entries-col9
                                            lwa_bp_entries-col10
                                            lwa_bp_entries-col11
                                            lwa_bp_entries-col12
                                            lwa_bp_entries-col13
                                            lwa_bp_entries-col14
                                            lwa_bp_entries-col15
                                            lwa_bp_entries-col16
                                            lwa_bp_entries-col17
                                            lwa_bp_entries-col18
                                            lwa_bp_entries-col19
                                            lwa_bp_entries-col20
                                            lwa_bp_entries-col21
                                            lwa_bp_entries-col22
                                            lwa_bp_entries-col23
                                            lwa_bp_entries-col24.

        APPEND lwa_bp_entries TO git_bp_entries.
        CLEAR: lwa_bp_entries,
               lv_record_line.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
* Close the File
    CLOSE DATASET p_afile.

  ENDIF.

ENDFORM.                    " F_READ_APPLICATION

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
* Display Output on the screen
*----------------------------------------------------------------------*
FORM f_display_output .

  IF gv_err_flg IS INITIAL.

    IF rb_prs = gc_x.
      WRITE:/ 'File Name      :'(010), p_pfile.
    ELSE.
      WRITE:/ 'File Name      :'(010), p_afile.
    ENDIF.

    WRITE:/ 'File Separator :'(011), p_sep.

    SKIP 1.
    WRITE:/ 'Total No. of Records         :'(013), gv_total.
    WRITE:/ 'Total No. of Success Records :'(014), gv_success.
    WRITE:/ 'Total No. of Fail  Records   :'(015), gv_fail.

    SKIP 2.
    WRITE:/1(12)  'BP Number'(016),
           15(15) 'Company Code'(021),
           33(7)  'Status'(019),
           40     'Description'(020).
    SKIP 1.

    LOOP AT git_output INTO gwa_output.
      WRITE:/1(10) gwa_output-i_partner,
            15(15) gwa_output-i_bukrs,
            33(7)  gwa_output-msgtyp,
            40     gwa_output-msg.
      CLEAR gwa_output.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " F_DISPLAY_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_CALL_BAPI
*&---------------------------------------------------------------------*
* Call FM FTB_BUPA_DARK_MAINTAIN_INTERN and create the Business Partner
*----------------------------------------------------------------------*
FORM f_call_bapi .

  DATA: lv_error TYPE bus000flds-char1,
        lv_tabix TYPE sy-tabix,
        lv_date  TYPE bus0di2-valdt,
        lv_x(1)  TYPE c.

  DATA: lwa_bus000_di     TYPE bus000_di,
        lwa_bus020_di     TYPE bus020_di,
        lwa_bus0bk_di     TYPE bus0bk_di,
        lwa_bpavtbsta1_di TYPE bpavtbsta1_di,
        lwa_bpavtbstc1_di TYPE bpavtbstc1_di,
        lwa_bpavtbsta3_di TYPE bpavtbsta3_di,
        lwa_bp_upload     TYPE ty_bp_upload,
        lwa_bus0msg       TYPE bus0msg1.

  DATA: lit_bus020_di     TYPE TABLE OF bus020_di,
        lit_bus0bk_di     TYPE TABLE OF bus0bk_di,
        lit_bpavtbsta1_di TYPE TABLE OF bpavtbsta1_di,
        lit_bpavtbstc1_di TYPE TABLE OF bpavtbstc1_di,
        lit_bpavtbsta3_di TYPE TABLE OF bpavtbsta3_di,
        lit_bus0msg1      TYPE TABLE OF bus0msg1.

  lv_date = sy-datum.

  LOOP AT git_bp_upload INTO lwa_bp_upload.

    lv_tabix = sy-tabix.

    CLEAR lv_error.

    lwa_bus000_di-name_org1 = lwa_bp_upload-name_org1.
    lwa_bus000_di-name_org2 = lwa_bp_upload-name_org2.
    lwa_bus000_di-name_org3 = lwa_bp_upload-name_org3.
    lwa_bus000_di-name_org4 = lwa_bp_upload-name_org4.
    lwa_bus000_di-bu_sort1  = lwa_bp_upload-bu_sort1.

*Population of Address Tab details
    lwa_bus020_di-chind_addr      = lwa_bp_upload-chind_addr.
    lwa_bus020_di-addr_date_from  = lwa_bp_upload-addr_date_from.
    lwa_bus020_di-addr_date_to    = lwa_bp_upload-addr_date_to.
    lwa_bus020_di-name_co         = lwa_bp_upload-name_co.
    lwa_bus020_di-city1           = lwa_bp_upload-city1.
    lwa_bus020_di-city2           = lwa_bp_upload-city2.
    lwa_bus020_di-post_code1      = lwa_bp_upload-post_code1.
    lwa_bus020_di-pcode1_ext      = lwa_bp_upload-pcode1_ext.
    lwa_bus020_di-po_box          = lwa_bp_upload-po_box.
    lwa_bus020_di-po_box_num      = lwa_bp_upload-po_box_num.
    lwa_bus020_di-po_box_loc      = lwa_bp_upload-po_box_loc.
    lwa_bus020_di-city_code2      = lwa_bp_upload-city_code2.
    lwa_bus020_di-po_box_reg      = lwa_bp_upload-po_box_reg.
    lwa_bus020_di-po_box_cty      = lwa_bp_upload-po_box_cty.
    lwa_bus020_di-postalarea      = lwa_bp_upload-postalarea.
    lwa_bus020_di-street          = lwa_bp_upload-street.
    lwa_bus020_di-house_num1      = lwa_bp_upload-house_num1.
    lwa_bus020_di-building        = lwa_bp_upload-building.
    lwa_bus020_di-country         = lwa_bp_upload-country.
    lwa_bus020_di-langu           = lwa_bp_upload-langu.
    lwa_bus020_di-region          = lwa_bp_upload-region.
    lwa_bus020_di-sort1           = lwa_bp_upload-sort1.

    IF NOT lwa_bus020_di IS INITIAL.
      APPEND lwa_bus020_di TO lit_bus020_di.
      CLEAR  lwa_bus020_di.
    ENDIF.

*Population of Bank Tab details
    lwa_bus0bk_di-bkvid         = lwa_bp_upload-bkvid.
    lwa_bus0bk_di-chind_bank    = lwa_bp_upload-chind_bank.
    lwa_bus0bk_di-banks         = lwa_bp_upload-banks.
    lwa_bus0bk_di-bankl         = lwa_bp_upload-bankl.
    lwa_bus0bk_di-bankn         = lwa_bp_upload-bankn.
    lwa_bus0bk_di-bkont         = lwa_bp_upload-bkont.
    lwa_bus0bk_di-bkref         = lwa_bp_upload-bkref.
    lwa_bus0bk_di-koinh	        = lwa_bp_upload-koinh.
    lwa_bus0bk_di-bkext	        = lwa_bp_upload-bkext.
    lwa_bus0bk_di-xezer	        = lwa_bp_upload-xezer.
    lwa_bus0bk_di-accname	      = lwa_bp_upload-accname.
    lwa_bus0bk_di-move_bkvid    = lwa_bp_upload-move_bkvid.
    lwa_bus0bk_di-bk_date_from  = lwa_bp_upload-bk_date_from.
    lwa_bus0bk_di-bk_date_to    = lwa_bp_upload-bk_date_to.
    lwa_bus0bk_di-bk_move_date  = lwa_bp_upload-bk_move_date.
    lwa_bus0bk_di-iban          = lwa_bp_upload-iban.

    IF NOT lwa_bus0bk_di IS INITIAL.
      APPEND lwa_bus0bk_di TO lit_bus0bk_di.
      CLEAR  lwa_bus0bk_di.
    ENDIF.

*Population of Payment Details Tab details
    lwa_bpavtbstc1_di-chind_sc1  = lwa_bp_upload-chind_sc1.
    lwa_bpavtbstc1_di-waers_c1   = lwa_bp_upload-waers_c1.
    lwa_bpavtbstc1_di-zahlvid_c1 = lwa_bp_upload-zahlvid_c1.
    lwa_bpavtbstc1_di-bvtyp      = lwa_bp_upload-bvtyp.
    lwa_bpavtbstc1_di-hbkid_c1   = lwa_bp_upload-hbkid_c1.
    lwa_bpavtbstc1_di-hktid      = lwa_bp_upload-hktid.
    lwa_bpavtbstc1_di-zlsch      = lwa_bp_upload-zlsch.
    lwa_bpavtbstc1_di-uzawe_c1   = lwa_bp_upload-uzawe_c1.
    lwa_bpavtbstc1_di-szart	     = lwa_bp_upload-szart.
    lwa_bpavtbstc1_di-rpzahl     = lwa_bp_upload-rpzahl.
    lwa_bpavtbstc1_di-spayrq     = lwa_bp_upload-spayrq.
    lwa_bpavtbstc1_di-sprsng     = lwa_bp_upload-sprsng.
    lwa_bpavtbstc1_di-sprgrd     = lwa_bp_upload-sprgrd.
    lwa_bpavtbstc1_di-scspay     = lwa_bp_upload-scspay.
    lwa_bpavtbstc1_di-zwels_c1   = lwa_bp_upload-zwels_c1.

    IF NOT lwa_bpavtbstc1_di IS INITIAL.
      APPEND lwa_bpavtbstc1_di TO lit_bpavtbstc1_di.
      CLEAR  lwa_bpavtbstc1_di.
    ENDIF.

*Population of Authroizations tab details
    lwa_bpavtbsta3_di-chind_si3	  = lwa_bp_upload-chind_si3.
    lwa_bpavtbsta3_di-rantyp_a3	  = lwa_bp_upload-rantyp_a3.
    lwa_bpavtbsta3_di-sanlf_a3    = lwa_bp_upload-sanlf_a3.
    lwa_bpavtbsta3_di-sgsart_a3   = lwa_bp_upload-sgsart_a3.
    lwa_bpavtbsta3_di-sfhaart_a3  = lwa_bp_upload-sfhaart_a3.
    lwa_bpavtbsta3_di-author      = lwa_bp_upload-author.

    IF NOT lwa_bpavtbsta3_di IS INITIAL.
      APPEND lwa_bpavtbsta3_di TO lit_bpavtbsta3_di.
      CLEAR  lwa_bpavtbsta3_di.
    ENDIF.

*Assignment of Payment Details as SI
    lwa_bpavtbsta1_di-chind_si1	  = lwa_bp_upload-chind_si1.
    lwa_bpavtbsta1_di-waers_a1    = lwa_bp_upload-waers_a1.
    lwa_bpavtbsta1_di-rantyp_a1	  = lwa_bp_upload-rantyp_a1.
    lwa_bpavtbsta1_di-sanlf_a1    = lwa_bp_upload-sanlf_a1.
    lwa_bpavtbsta1_di-sgsart_a1	  = lwa_bp_upload-sgsart_a1.
    lwa_bpavtbsta1_di-sfhaart_a1  = lwa_bp_upload-sfhaart_a1.
    lwa_bpavtbsta1_di-ssign	      = lwa_bp_upload-ssign.
    lwa_bpavtbsta1_di-zahlvid    	= lwa_bp_upload-zahlvid.
    IF NOT lwa_bpavtbsta1_di IS INITIAL.
      APPEND lwa_bpavtbsta1_di TO lit_bpavtbsta1_di.
      CLEAR  lwa_bpavtbsta1_di.
    ENDIF.

    AT END OF i_bukrs.

      READ TABLE git_bp_upload INTO lwa_bp_upload INDEX lv_tabix.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_bp_upload-i_bpkind
        IMPORTING
          output = lwa_bp_upload-i_bpkind.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lwa_bp_upload-i_aktyp
        IMPORTING
          output = lwa_bp_upload-i_aktyp.

      CALL FUNCTION 'FTB_BUPA_DARK_MAINTAIN_INTERN'
        EXPORTING
          i_aktyp        = lwa_bp_upload-i_aktyp
          i_xcommit      = lwa_bp_upload-i_xcommit
          i_xchdoc       = lwa_bp_upload-i_xchdoc
          i_partner      = lwa_bp_upload-i_partner
          i_bpext        = lwa_bp_upload-i_bpext
          i_type         = lwa_bp_upload-i_type
          i_bpkind       = lwa_bp_upload-i_bpkind
          i_group        = lwa_bp_upload-i_group
          i_rltyp        = lwa_bp_upload-i_rltyp
          i_rltyp2       = lwa_bp_upload-i_rltyp2
          i_role1        = lwa_bp_upload-i_role1
          i_role2        = lwa_bp_upload-i_role2
          i_valid_from_1 = lwa_bp_upload-i_valid_from_1
          i_valid_to_1   = lwa_bp_upload-i_valid_to_1
          i_valid_from_2 = lwa_bp_upload-i_valid_from_2
          i_valid_to_2   = lwa_bp_upload-i_valid_to_2
          i_valdt        = lv_date
          i_bukrs        = lwa_bp_upload-i_bukrs
          i_but000       = lwa_bus000_di
        IMPORTING
          e_xerror       = lv_error
        TABLES
          t_but0bk       = lit_bus0bk_di
          t_but020       = lit_bus020_di
          t_vtbsta1      = lit_bpavtbsta1_di
          t_vtbstc1      = lit_bpavtbstc1_di
          t_vtbsta3      = lit_bpavtbsta3_di
          t_message      = lit_bus0msg1.

      gv_total = gv_total + 1.

      IF lv_error = gc_x.

        gv_fail = gv_fail + 1.
        gwa_output-i_partner = lwa_bp_upload-i_partner.
        gwa_output-i_bukrs   = lwa_bp_upload-i_bukrs.
        gwa_output-msgtyp    = 'E'.
        LOOP AT lit_bus0msg1 INTO lwa_bus0msg WHERE msgty = 'E'.
          CONCATENATE lwa_bus0msg-text
                      gwa_output-msg
                      INTO
                      gwa_output-msg
                      SEPARATED BY space.
        ENDLOOP.
        APPEND gwa_output TO git_output.
        CLEAR gwa_output.

      ELSE.

        gv_success = gv_success + 1.
        gwa_output-i_partner = lwa_bp_upload-i_partner.
        gwa_output-i_bukrs   = lwa_bp_upload-i_bukrs.
        gwa_output-msgtyp    = 'S'.
        LOOP AT lit_bus0msg1 INTO lwa_bus0msg WHERE msgty = 'S'.
          CONCATENATE lwa_bus0msg-text
                      gwa_output-msg
                      INTO
                      gwa_output-msg
                      SEPARATED BY space.
        ENDLOOP.
        APPEND gwa_output TO git_output.
        CLEAR gwa_output.

      ENDIF.
      WAIT UP TO 5 SECONDS.
      CLEAR: lwa_bp_upload,
             lwa_bus000_di,
             lit_bus0msg1[],
             lit_bus0bk_di[],
             lit_bus020_di[],
             lit_bpavtbsta1_di[],
             lit_bpavtbstc1_di[],
             lit_bpavtbsta3_di[].
    ENDAT.

  ENDLOOP.

ENDFORM.                    " F_CALL_BAPI

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_DATA
*&---------------------------------------------------------------------*
* Arrange the data in the final format
*----------------------------------------------------------------------*
FORM f_build_data .

  TYPES: BEGIN OF ty_bp_upload_hd,
*Header Data
          i_partner      TYPE but000-partner,
          i_aktyp        TYPE tbz0k-aktyp,
          i_xcommit      TYPE boole-boole,
          i_xchdoc       TYPE boole-boole,
          i_bpext        TYPE but000-bpext,
          i_type         TYPE but000-type,
          i_bpkind       TYPE but000-bpkind,
          i_group        TYPE but000-bu_group,
          i_rltyp        TYPE bu_partnerrolecat,
          i_rltyp2       TYPE bu_partnerrolecat,
          i_role1        TYPE bu_partnerrole,
          i_role2        TYPE bu_partnerrole,
          i_valid_from_1 TYPE bu_role_valid_from_di,
          i_valid_to_1   TYPE bu_role_valid_to_di,
          i_valid_from_2 TYPE bu_role_valid_from_di,
          i_valid_to_2   TYPE bu_role_valid_to_di,
          i_valdt        TYPE bus0di2-valdt,
          i_bukrs        TYPE t001-bukrs,
          bu_sort1       TYPE bu_sort1,
          name_org1      TYPE bu_nameor1,
          name_org2      TYPE bu_nameor2,
          name_org3      TYPE bu_nameor3,
          name_org4      TYPE bu_nameor4,
        END OF ty_bp_upload_hd.

  DATA: lwa_bp_entries    TYPE ty_bp_entries,
        lwa_bp_upload_hd  TYPE ty_bp_upload_hd,
        lwa_bp_upload     TYPE ty_bp_upload.

  LOOP AT git_bp_entries INTO lwa_bp_entries.

* Header Data
    IF lwa_bp_entries-col1 = gc_h.
      lwa_bp_upload_hd-i_aktyp        = lwa_bp_entries-col2.
      lwa_bp_upload_hd-i_xcommit      = lwa_bp_entries-col3.
      lwa_bp_upload_hd-i_xchdoc       = lwa_bp_entries-col4.
      lwa_bp_upload_hd-i_partner      = lwa_bp_entries-col5.
      lwa_bp_upload_hd-i_bpext        = lwa_bp_entries-col6.
      lwa_bp_upload_hd-i_type         = lwa_bp_entries-col7.
      lwa_bp_upload_hd-i_bpkind       = lwa_bp_entries-col8.
      lwa_bp_upload_hd-i_group        = lwa_bp_entries-col9.
      lwa_bp_upload_hd-i_rltyp        = lwa_bp_entries-col10.
      lwa_bp_upload_hd-i_rltyp2       = lwa_bp_entries-col11.
      lwa_bp_upload_hd-i_role1        = lwa_bp_entries-col12.
      lwa_bp_upload_hd-i_role2        = lwa_bp_entries-col13.
      lwa_bp_upload_hd-i_valid_from_1 = lwa_bp_entries-col14.
      lwa_bp_upload_hd-i_valid_to_1   = lwa_bp_entries-col15.
      lwa_bp_upload_hd-i_valid_from_2 = lwa_bp_entries-col16.
      lwa_bp_upload_hd-i_valid_to_2   = lwa_bp_entries-col17.
      lwa_bp_upload_hd-i_valdt        = lwa_bp_entries-col18.
      lwa_bp_upload_hd-i_bukrs        = lwa_bp_entries-col19.
      lwa_bp_upload_hd-bu_sort1       = lwa_bp_entries-col20.
      lwa_bp_upload_hd-name_org1      = lwa_bp_entries-col21.
      lwa_bp_upload_hd-name_org2      = lwa_bp_entries-col22.
      lwa_bp_upload_hd-name_org3      = lwa_bp_entries-col23.
      lwa_bp_upload_hd-name_org4      = lwa_bp_entries-col24.
    ENDIF.

* Bank Data
    IF lwa_bp_entries-col1 = gc_bk.
      lwa_bp_upload-bkvid         = lwa_bp_entries-col2.
      lwa_bp_upload-chind_bank    = lwa_bp_entries-col3.
      lwa_bp_upload-banks         = lwa_bp_entries-col4.
      lwa_bp_upload-bankl         = lwa_bp_entries-col5.
      lwa_bp_upload-bankn         = lwa_bp_entries-col6.
      lwa_bp_upload-bkont         = lwa_bp_entries-col7.
      lwa_bp_upload-bkref         = lwa_bp_entries-col8.
      lwa_bp_upload-koinh         = lwa_bp_entries-col9.
      lwa_bp_upload-bkext         = lwa_bp_entries-col10.
      lwa_bp_upload-xezer         = lwa_bp_entries-col11.
      lwa_bp_upload-accname       = lwa_bp_entries-col12.
      lwa_bp_upload-move_bkvid    = lwa_bp_entries-col13.
      lwa_bp_upload-bk_date_from  = lwa_bp_entries-col14.
      lwa_bp_upload-bk_date_to    = lwa_bp_entries-col15.
      lwa_bp_upload-bk_move_date  = lwa_bp_entries-col16.
      lwa_bp_upload-iban          = lwa_bp_entries-col17.
    ENDIF.

*Address Data
    IF lwa_bp_entries-col1 = gc_ad.
      lwa_bp_upload-chind_addr      = lwa_bp_entries-col2.
      lwa_bp_upload-addr_date_from  = lwa_bp_entries-col3.
      lwa_bp_upload-addr_date_to    = lwa_bp_entries-col4.
      lwa_bp_upload-name_co         = lwa_bp_entries-col5.
      lwa_bp_upload-city1           = lwa_bp_entries-col6.
      lwa_bp_upload-city2           = lwa_bp_entries-col7.
      lwa_bp_upload-post_code1      = lwa_bp_entries-col8.
      lwa_bp_upload-pcode1_ext      = lwa_bp_entries-col9.
      lwa_bp_upload-po_box          = lwa_bp_entries-col10.
      lwa_bp_upload-po_box_num      = lwa_bp_entries-col11.
      lwa_bp_upload-po_box_loc      = lwa_bp_entries-col12.
      lwa_bp_upload-city_code2      = lwa_bp_entries-col13.
      lwa_bp_upload-po_box_reg      = lwa_bp_entries-col14.
      lwa_bp_upload-po_box_cty      = lwa_bp_entries-col15.
      lwa_bp_upload-postalarea      = lwa_bp_entries-col16.
      lwa_bp_upload-street          = lwa_bp_entries-col17.
      lwa_bp_upload-house_num1      = lwa_bp_entries-col18.
      lwa_bp_upload-building        = lwa_bp_entries-col19.
      lwa_bp_upload-country         = lwa_bp_entries-col20.
      lwa_bp_upload-langu           = lwa_bp_entries-col21.
      lwa_bp_upload-region          = lwa_bp_entries-col22.
      lwa_bp_upload-sort1           = lwa_bp_entries-col23.
    ENDIF.

*Payment Data
    IF lwa_bp_entries-col1 = gc_py.
      lwa_bp_upload-chind_sc1   = lwa_bp_entries-col2.
      lwa_bp_upload-waers_c1    = lwa_bp_entries-col3.
      lwa_bp_upload-zahlvid_c1  = lwa_bp_entries-col4.
      lwa_bp_upload-bvtyp       = lwa_bp_entries-col5.
      lwa_bp_upload-hbkid_c1    = lwa_bp_entries-col6.
      lwa_bp_upload-hktid       = lwa_bp_entries-col7.
      lwa_bp_upload-zlsch       = lwa_bp_entries-col8.
      lwa_bp_upload-uzawe_c1    = lwa_bp_entries-col9.
      lwa_bp_upload-szart       = lwa_bp_entries-col10.
      lwa_bp_upload-rpzahl      = lwa_bp_entries-col11.
      lwa_bp_upload-spayrq      = lwa_bp_entries-col12.
      lwa_bp_upload-sprsng      = lwa_bp_entries-col13.
      lwa_bp_upload-sprgrd      = lwa_bp_entries-col14.
      lwa_bp_upload-scspay      = lwa_bp_entries-col15.
      lwa_bp_upload-zwels_c1    = lwa_bp_entries-col16.
    ENDIF.

*Treasury Attribute SI Authorizations
    IF lwa_bp_entries-col1      = gc_pt.
      lwa_bp_upload-chind_si3   = lwa_bp_entries-col2.
      lwa_bp_upload-rantyp_a3   = lwa_bp_entries-col3.
      lwa_bp_upload-sanlf_a3    = lwa_bp_entries-col4.
      lwa_bp_upload-sgsart_a3   = lwa_bp_entries-col5.
      lwa_bp_upload-sfhaart_a3  = lwa_bp_entries-col6.
      lwa_bp_upload-author      = lwa_bp_entries-col7.
    ENDIF.

* Assignment of Payment Details as SI
    IF lwa_bp_entries-col1      = gc_ps.
      lwa_bp_upload-chind_si1   = lwa_bp_entries-col2.
      lwa_bp_upload-waers_a1    = lwa_bp_entries-col3.
      lwa_bp_upload-rantyp_a1   = lwa_bp_entries-col4.
      lwa_bp_upload-sanlf_a1    = lwa_bp_entries-col5.
      lwa_bp_upload-sgsart_a1   = lwa_bp_entries-col6.
      lwa_bp_upload-sfhaart_a1  = lwa_bp_entries-col7.
      lwa_bp_upload-ssign       = lwa_bp_entries-col8.
      lwa_bp_upload-zahlvid     = lwa_bp_entries-col9.
    ENDIF.

    MOVE-CORRESPONDING lwa_bp_upload_hd TO lwa_bp_upload.

    APPEND lwa_bp_upload TO git_bp_upload.
    CLEAR: lwa_bp_upload.

  ENDLOOP.

ENDFORM.                    " F_BUILD_DATA

*&---------------------------------------------------------------------*
*&      Form  F_CLEAR_DATA
*&---------------------------------------------------------------------*
*       clear global data
*----------------------------------------------------------------------*
FORM f_clear_data .

  CLEAR: gwa_output.

  CLEAR: git_upload[],
         git_bp_upload[],
         git_output[],
         git_bp_entries[].

  CLEAR: gv_filename,
         gv_err_flg,
         gv_total,
         gv_success,
         gv_fail.

ENDFORM.                    " F_CLEAR_DATA
