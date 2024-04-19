REPORT zfci_config_data_extract_erp LINE-SIZE 256.

*----------------------------------------------------------------------*
* This program extracts selected configuration data from SAP system and
* download it into corresponding csv data files.
*----------------------------------------------------------------------*

TABLES sscrfields.

CONSTANTS: c1(1) VALUE '"',
           c2(3) VALUE '","',
           c3(2) VALUE '""',
           c_comma(1) VALUE ',',
           c_space(1) VALUE ' ',
           c_unknown(10) VALUE 'Unknown',
           c_usd TYPE waers_curc VALUE 'USD',
           c_comp_code(15) VALUE 'Company Code',
           c_compcode(15) VALUE 'CompanyCode',
           c_plant(10) VALUE 'Plant'.

DATA: lv_data(100) TYPE c ,
      lv_data2(100) TYPE c,
      lv_data3(100) TYPE c,
      lv_timestamp(20) TYPE c,
      lv_filename LIKE rlgrap-filename.

* To use in the new OO method
DATA: lv_fname TYPE string,
      lv_ul_path  TYPE string,
      lv_dl_path  TYPE string.
TYPES: filetab_line TYPE file_table.
DATA:
*      wa_filetab   TYPE file_table,
      filetable TYPE STANDARD TABLE OF filetab_line.

DATA: BEGIN OF lt_data OCCURS 0,
      rec(1024),
      END OF lt_data.

DATA: BEGIN OF lt_table OCCURS 0,
      f01(160),
      f02(160),
      f03(160),
      f04(160),
      f05(160),
      f06(160),
      f07(160),
      f08(160),
      f09(160),
      f10(160),
      f11(160),
      f12(160),
      f13(160),
      f14(160),
      f15(160),
      f16(160),
      f17(160),
      f18(160),
      f19(160),
      f20(160),
      f21(160),
      f22(160),
      f23(160),
      f24(160),
      f25(160),
      f26(160),
      f27(160),
      f28(160),
      f29(160),
      f30(160),
      f31(160),
      f32(160),
      f33(160),
      f34(160),
      f35(160),
      END OF lt_table.

* Language
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_langu(20) DEFAULT 'Language'.
SELECTION-SCREEN POSITION 35.
PARAMETERS: p_langu TYPE spras DEFAULT 'EN' OBLIGATORY.
SELECTION-SCREEN END OF LINE .

SELECTION-SCREEN ULINE.

* File directories
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_r1 RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_t1(20) DEFAULT 'PC File Directory'.
SELECTION-SCREEN POSITION 35.
PARAMETERS: p_pc LIKE rlgrap-filename LOWER CASE .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_r2 RADIOBUTTON GROUP g1.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_t2(20) DEFAULT 'App Server File Dir'.
SELECTION-SCREEN POSITION 35.
PARAMETERS: p_app LIKE rlgrap-filename LOWER CASE DEFAULT
 '/usr/sap/tmp/'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.

* Select all / Deselect all function keys
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
* Information Function Key
SELECTION-SCREEN FUNCTION KEY 4.


SELECTION-SCREEN ULINE.
* default configuration

SELECTION-SCREEN COMMENT /1(72) comm1
      FOR FIELD p_parcom
      MODIF ID mg1.

SELECTION-SCREEN COMMENT /1(72) comm2
      FOR FIELD p_parcom
      MODIF ID mg1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_parcom(20) DEFAULT 'Parent Company ID'.
SELECTION-SCREEN POSITION 35.
PARAMETERS: p_parcom(100) TYPE c LOWER CASE DEFAULT 'SAP-FCI'.
SELECTION-SCREEN END OF LINE .

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_srmail(20) DEFAULT 'Source Email'.
SELECTION-SCREEN POSITION 35.
PARAMETERS: p_srmail TYPE vvemail LOWER CASE DEFAULT
'no_property_file@sap.com'.
SELECTION-SCREEN END OF LINE .

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_admail(20) DEFAULT 'Admin Email'.
SELECTION-SCREEN POSITION 35.
PARAMETERS: p_admail TYPE vvemail LOWER CASE DEFAULT
'no_property_file@sap.com'.
SELECTION-SCREEN END OF LINE .


SELECTION-SCREEN ULINE.


* Currency
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name01(20) DEFAULT 'Currency'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f011(40) DEFAULT '01_Currency_sap_fci_<TimeStamp>' ,
    h_ext011(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Country
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c2 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name02(20) DEFAULT 'Country'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f021(40) DEFAULT '02_Country_sap_fci_<TimeStamp>',
    h_ext021(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* FCI: Location (from company)
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c3 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name03(20) DEFAULT 'FCI: Location'.
SELECTION-SCREEN POSITION 35.

PARAMETERS:
    h_f031(40) DEFAULT '03_LocationCompany_sap_fci_<TimeStamp>',
    h_ext031(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Company
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c4 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name04(20) DEFAULT 'Company'.
SELECTION-SCREEN POSITION 35.

PARAMETERS:
    h_f041(40) DEFAULT '04_Company_sap_fci_<TimeStamp>',
    h_ext041(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Region
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c5 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name05(20) DEFAULT 'Region'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f051(40) DEFAULT '05_Region_sap_fci_<TimeStamp>',
    h_ext051(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* FCI Organizational Unit : Company Code
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c6 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name06(20) DEFAULT 'FCI Company Code'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f061(40) DEFAULT '06_CompanyCode_sap_fci_<TimeStamp>',
    h_ext061(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* FCI : Location from Plant
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c7 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name07(20) DEFAULT 'FCI Location'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f071(40) DEFAULT '07_LocationPlant_sap_fci_<TimeStamp>',
    h_ext071(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* FCI Organizational Unit : Plant
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c8 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name08(20) DEFAULT 'FCI Plant Org Unit'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f081(40) DEFAULT '08_PlantOrgUnit_sap_fci_<TimeStamp>',
    h_ext081(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* FCI : Plant
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c9 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name09(20) DEFAULT 'FCI Plant'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f091(40) DEFAULT '09_Plant_sap_fci_<TimeStamp>',
    h_ext091(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Internal Category
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c10 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name10(20) DEFAULT 'Internal Category'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f101(40) DEFAULT '10_MaterialGroup_sap_fci_<TimeStamp>',
    h_ext101(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Unit of Measure Types
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c11 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name11(20) DEFAULT 'UOM Type'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f111(40) DEFAULT '11_UOMType_sap_fci_<TimeStamp>',
    h_ext111(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Unit of Measure
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c12 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name12(20) DEFAULT 'Unit of Measure'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f121(40) DEFAULT '12_UnitofMeasure_sap_fci_<TimeStamp>',
    h_ext121(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Material Type
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c13 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name13(20) DEFAULT 'Material Type'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f131(40) DEFAULT '13_MaterialType_sap_fci_<TimeStamp>',
    h_ext131(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Material Status
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c14 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name14(20) DEFAULT 'Material Status'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f143(40) DEFAULT '14_MaterialStatus_sap_fci_<TimeStamp>',
    h_ext143(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Group
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c15 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name15(20) DEFAULT 'Group'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f151(40) DEFAULT '15_PurchasingGroup_sap_fci_<TimeStamp>',
    h_ext151(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Purhasing Org
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c17 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name17(20) DEFAULT 'Purhasing Org'.
SELECTION-SCREEN POSITION 35.

PARAMETERS:
    h_f171(40) DEFAULT '16_PurchaseOrg_sap_fci_<TimeStamp>',
    h_ext171(4) DEFAULT '.xml'.
SELECTION-SCREEN END OF LINE.

* Accounting Group
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c18 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name18(20) DEFAULT 'Accounting Group'.
SELECTION-SCREEN POSITION 35.
PARAMETERS: h_f181(40) DEFAULT '17_AcctGroup_sap46c_fci_<TimeStamp>',
            h_ext181(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Region Group
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c19 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name19(20) DEFAULT 'Region Group'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f191(40) DEFAULT '18_RegionGroup_sap_fci_<TimeStamp>',
    h_ext191(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Terms of Payment
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c20 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name20(20) DEFAULT 'Terms of Payment'.
SELECTION-SCREEN POSITION 35.
PARAMETERS: h_f201(40) DEFAULT '19_PaymentTerms_sap_fci_<TimeStamp>',
            h_ext201(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Vendor GL Account
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c21 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name21(20) DEFAULT 'Vendor GL Account'.
SELECTION-SCREEN POSITION 35.
PARAMETERS: h_f211(40) DEFAULT '20_GLAccount_sap_fci_<TimeStamp>',
            h_ext211(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

* Delivery Term
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_c22 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN POSITION 10.
PARAMETERS: h_name22(20) DEFAULT 'Delivery Term'.
SELECTION-SCREEN POSITION 35.
PARAMETERS:
    h_f221(40) DEFAULT '21_DeliveryTerm_sap_fci_<TimeStamp>',
    h_ext221(4) DEFAULT '.csv'.
SELECTION-SCREEN END OF LINE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pc.

* file selector
  PERFORM get_filename CHANGING p_pc.

INITIALIZATION.
  MOVE 'Select All' TO sscrfields-functxt_02.
  MOVE 'Deselect All' TO sscrfields-functxt_03.
* Information to the user
  MOVE 'Info' TO sscrfields-functxt_04.

  CALL METHOD cl_gui_frontend_services=>get_upload_download_path
    CHANGING
      upload_path                 = lv_ul_path
      download_path               = lv_dl_path
    EXCEPTIONS
      cntl_error                  = 1
      error_no_gui                = 2
      not_supported_by_gui        = 3
      gui_upload_download_path    = 4
      upload_download_path_failed = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  p_pc = lv_dl_path .

AT SELECTION-SCREEN.
  PERFORM user_action.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

START-OF-SELECTION.
  IF NOT p_c1 IS INITIAL.
    PERFORM p_c1_currency.
  ENDIF.

  IF NOT p_c2 IS INITIAL.
    PERFORM p_c2_country.
  ENDIF.

  IF NOT p_c3 IS INITIAL.
    PERFORM p_c3_fci_location.
  ENDIF.

  IF NOT p_c4 IS INITIAL.
    PERFORM p_c4_company.
  ENDIF.

  IF NOT p_c5 IS INITIAL.
    PERFORM p_c5_region.
  ENDIF.

  IF NOT p_c6 IS INITIAL.
    PERFORM p_c6_company_code.
  ENDIF.

  IF NOT p_c7 IS INITIAL.
    PERFORM p_c7_location.
  ENDIF.

  IF NOT p_c8 IS INITIAL.
    PERFORM p_c8_plantorgunit.
  ENDIF.

  IF NOT p_c9 IS INITIAL.
    PERFORM p_c9_plant.
  ENDIF.

  IF NOT p_c10 IS INITIAL.
    PERFORM p_c10_matgroup.
  ENDIF.

  IF NOT p_c11 IS INITIAL.
    PERFORM p_c11_uomtype.
  ENDIF.

  IF NOT p_c12 IS INITIAL.
    PERFORM p_c12_unitofmeasure.
  ENDIF.

  IF NOT p_c13 IS INITIAL.
    PERFORM p_c13_mattype.
  ENDIF.

  IF NOT p_c14 IS INITIAL.
    PERFORM p_c14_matstatus.
  ENDIF.

  IF NOT p_c15 IS INITIAL.
    PERFORM p_c15_group.
  ENDIF.

  IF NOT p_c17 IS INITIAL.
    PERFORM p_c17_purhasingorg.
  ENDIF.

  IF NOT p_c18 IS INITIAL.
    PERFORM p_c18_accountinggroup.
  ENDIF.

  IF NOT p_c19 IS INITIAL.
    PERFORM p_c19_regiongroup.
  ENDIF.

  IF NOT p_c20 IS INITIAL.
    PERFORM p_c20_termsofpayment.
  ENDIF.

  IF NOT p_c21 IS INITIAL.
    PERFORM p_c21_glaccount.
  ENDIF.

  IF NOT p_c22 IS INITIAL.
    PERFORM p_c22_deliveryterm.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.
  LOOP AT SCREEN.
* Comments for default configuration
    comm1 =
'Please maintain Parent Company similar to the entry corresponding to'.

    comm2 =
'BUS_UNIT_CTXT in tab SCRIPTS in file integration_data_workbook.xls.'.

    IF screen-name+0(1) = 'H'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  user_action
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------*
FORM user_action.
  IF sscrfields-ucomm = 'FC02'.
    p_c1 = 'X'.
    p_c2 = 'X'.
    p_c3 = 'X'.
    p_c4 = 'X'.
    p_c5 = 'X'.
    p_c6 = 'X'.
    p_c7 = 'X'.
    p_c8 = 'X'.
    p_c9 = 'X'.
    p_c10 = 'X'.
    p_c11 = 'X'.
    p_c12 = 'X'.
    p_c13 = 'X'.
    p_c14 = 'X'.
    p_c15 = 'X'.
    p_c17 = 'X'.
    p_c18 = 'X'.
    p_c19 = 'X'.
    p_c20 = 'X'.
    p_c21 = 'X'.
    p_c22 = 'X'.
  ELSEIF sscrfields-ucomm = 'FC03'.
    CLEAR: p_c1, p_c2, p_c3, p_c4, p_c5, p_c6, p_c7, p_c8, p_c9, p_c10,
           p_c11, p_c12, p_c13, p_c14, p_c15, p_c17, p_c18,
           p_c19, p_c20, p_c21, p_c22.
* For the information
  ELSEIF sscrfields-ucomm = 'FC04'.

    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
        titel        = 'Config Data Extract : Information'
        textline1    = 'This report downloads config. master data from ERP for E-Sourcing'
        textline2    = 'Enter Parent Company name, source and admin email id before executing'
        start_column = 25
        start_row    = 6.

  ENDIF.
ENDFORM.                    " user_action
*&---------------------------------------------------------------------*
*&      Form  get_filename
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_PC  text
*----------------------------------------------------------------------*
FORM get_filename CHANGING p_p_pc.
  DATA: lv_patt(1) VALUE '\',
        lv_offset  LIKE sy-index VALUE 1.

  DATA: lt_files TYPE filetable,
        l_file TYPE file_table,
        l_subrc TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      file_filter             = ',*.txt.'
    CHANGING
      file_table              = lt_files
      rc                      = l_subrc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sy-subrc = 0.
    LOOP AT lt_files INTO l_file.
      p_pc = l_file.
      EXIT.
    ENDLOOP.
  ENDIF.

  WHILE sy-subrc = 0.
    SEARCH p_pc FOR lv_patt STARTING AT lv_offset.
    IF sy-subrc = 0.
      lv_offset = lv_offset + sy-fdpos + 1.
    ENDIF.
  ENDWHILE.

  IF lv_offset > 1.
    lv_offset = lv_offset - 1.
    p_pc = p_pc+0(lv_offset).
  ENDIF.

ENDFORM.                    " get_filename
*&---------------------------------------------------------------------*
*&      Form  file_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_download.
  SHIFT lv_filename LEFT DELETING LEADING space.

  IF NOT p_r1 IS INITIAL.
    CONCATENATE p_pc lv_filename INTO lv_filename.
  ELSEIF NOT p_r2 IS INITIAL.
    CONCATENATE p_app lv_filename INTO lv_filename.
  ENDIF.

  IF lt_data[] IS INITIAL.
    CONCATENATE lv_filename ': no data selected'
    INTO lv_filename SEPARATED BY space.
  ELSE.
* PC file download
    IF NOT p_r1 IS INITIAL.

      filetable[] = lt_data[] .
      lv_fname = lv_filename.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = lv_fname
          filetype                = 'ASC'
        CHANGING
          data_tab                = filetable
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.

      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF sy-subrc <> 0.
        CONCATENATE lv_filename ': download failed'
        INTO lv_filename SEPARATED BY space.
      ELSE.
        CONCATENATE lv_filename ': download successful'
        INTO lv_filename SEPARATED BY space.
      ENDIF.
    ELSEIF NOT p_r2 IS INITIAL.
* Application server file download
      OPEN DATASET lv_filename FOR OUTPUT
*       IN BINARY MODE .
      IN TEXT MODE ENCODING DEFAULT.

      IF sy-subrc <> 0.
        CONCATENATE lv_filename ': error open file'
        INTO lv_filename SEPARATED BY space.
      ELSE.
        LOOP AT lt_data.
          TRANSFER lt_data TO lv_filename.
        ENDLOOP.
        CLOSE DATASET lv_filename.

        CONCATENATE lv_filename ': download successful'
        INTO lv_filename SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDIF.

  WRITE:/ lv_filename.
ENDFORM.                    " file_download
*&---------------------------------------------------------------------*
*&      Form  p_c3_fci_location
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c3_fci_location.

  REFRESH lt_data .

  REFRESH lt_table .
  lt_table-f01 =  'DISPLAY_NAME' .
  lt_table-f02 =  'DOCUMENT_DESCRIPTION' .
  lt_table-f03 =  'ADDRESS_1' .
  lt_table-f04 =  'ADDRESS_2' .
  lt_table-f05 =  'ADDRESS_3' .
  lt_table-f06 =  'CITY' .
  lt_table-f07 =  'COUNTRY' .
  lt_table-f08 =  'COUNTY' .
  lt_table-f09 =  'FAX_1' .
  lt_table-f10 =  'FAX_2' .
  lt_table-f11 =  'FAX_3' .
  lt_table-f12 =  'POSTAL_CODE' .
  lt_table-f13 =  'GEOGRAPHY' .
  lt_table-f14 =  'STATE_PROVINCE' .
  lt_table-f15 =  'TAX_JURISDICTION' .
  lt_table-f16 =  'TELEPHONE_1' .
  lt_table-f17 =  'TELEPHONE_2' .
  lt_table-f18 =  'TELEPHONE_3' .

* CSV Header line - HDR
  APPEND '#DataType[masterdata.location]' TO lt_data.

* CSV Header line - Column Names
  CONCATENATE
            c1 lt_table-f01
            c2 lt_table-f02
            c2 lt_table-f03
            c2 lt_table-f04
            c2 lt_table-f05
            c2 lt_table-f06
            c2 lt_table-f07
            c2 lt_table-f08
            c2 lt_table-f09
            c2 lt_table-f10
            c2 lt_table-f11
            c2 lt_table-f12
            c2 lt_table-f13
            c2 lt_table-f14
            c2 lt_table-f15
            c2 lt_table-f16
            c2 lt_table-f17
            c2 lt_table-f18
            c1 INTO lt_data.

  APPEND lt_data.

  SELECT city cntry INTO TABLE lt_table FROM t880
   WHERE langu = p_langu.

  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.

  CONCATENATE h_f031 lv_timestamp h_ext031 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

    IF NOT lt_table-f01 IS INITIAL.

    ELSE .
      lt_table-f01 = c_unknown .
    ENDIF.

    CONCATENATE lt_table-f01 lt_table-f02
    INTO lv_data
    SEPARATED BY '_' .

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.
    PERFORM p_doublequote CHANGING lv_data.
    CONCATENATE
                   c1 lv_data
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 lt_table-f01
                   c2 lt_table-f02
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c2 c_space
                   c1 INTO lt_data.

    APPEND lt_data.

  ENDLOOP.
  PERFORM file_download.
ENDFORM.                    " p_c3_fci_location
*&---------------------------------------------------------------------*
*&      Form  p_c4_company
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c4_company.

  REFRESH lt_data .

  REFRESH lt_table .
  lt_table-f01 =  'DISPLAY_NAME' .
  lt_table-f02 =  'SHORT_NAME' .
  lt_table-f03 =  'EXTERNAL_ID' .
  lt_table-f04 =  'KEYWORD' .
  lt_table-f05 =  'URL' .
  lt_table-f06 =  'PARENT' .
  lt_table-f07 =  'LOCATION' .
  lt_table-f08 =  'CURRENCY' .
  lt_table-f09 =  'ADDRESS_1' .
  lt_table-f10 =  'ADDRESS_2' .
  lt_table-f11 =  'ADDRESS_3' .
  lt_table-f12 =  'CITY' .
  lt_table-f13 =  'COUNTRY' .
  lt_table-f14 =  'COUNTY' .
  lt_table-f15 =  'FAX_1' .
  lt_table-f16 =  'FAX_2' .
  lt_table-f17 =  'FAX_3' .
  lt_table-f18 =  'POSTAL_CODE' .
  lt_table-f19 =  'REGION' .
  lt_table-f20 =  'STATE_PROVINCE' .
  lt_table-f21 =  'STATE_JURISDICTION' .
  lt_table-f22 =  'TELEPHONE_1' .
  lt_table-f23 =  'TELEPHONE_2' .
  lt_table-f24 =  'TELEPHONE_3' .
  lt_table-f25 =  'TECH_SUPPORT_EMAIL' .
  lt_table-f26 =  'TECH_SUPPORT_URL' .
  lt_table-f27 =  'TECH_SUPPORT_PHONE' .
  lt_table-f28 =  'TECH_SUPPORT_NAME' .
  lt_table-f29 =  'SOURCE_EMAIL' .
  lt_table-f30 =  'BOUND_CTX' .
  lt_table-f31 =  'ADMIN_EMAIL' .
  lt_table-f32 =  'INTERNAL_CATEGORY' .
  lt_table-f33 =  'EXERNAL_CATEGORY' .
  lt_table-f34 =  'TIME_ZONE' .
  lt_table-f35 =  'DATE_FORMAT' .

* CSV Header line - HDR
  APPEND  '#DataType[masterdata.company]' TO lt_data.

* CSV Header line - Column Names
  CONCATENATE
            lt_table-f01 c_comma
            lt_table-f02 c_comma
            lt_table-f03 c_comma
            lt_table-f04 c_comma
            lt_table-f05 c_comma
            lt_table-f06 c_comma
            lt_table-f07 c_comma
            lt_table-f08 c_comma
            lt_table-f09 c_comma
            lt_table-f10 c_comma
            lt_table-f11 c_comma
            lt_table-f12 c_comma
            lt_table-f13 c_comma
            lt_table-f14 c_comma
            lt_table-f15 c_comma
            lt_table-f16 c_comma
            lt_table-f17 c_comma
            lt_table-f18 c_comma
            lt_table-f19 c_comma
            lt_table-f20 c_comma
            lt_table-f21 c_comma
            lt_table-f22 c_comma
            lt_table-f23 c_comma
            lt_table-f24 c_comma
            lt_table-f25 c_comma
            lt_table-f26 c_comma
            lt_table-f27 c_comma
            lt_table-f28 c_comma
            lt_table-f29 c_comma
            lt_table-f30 c_comma
            lt_table-f31 c_comma
            lt_table-f32 c_comma
            lt_table-f33 c_comma
            lt_table-f34 c_comma
            lt_table-f35
            INTO lt_data.
  PERFORM p_doublequote CHANGING lt_table-f01.
  PERFORM p_doublequote CHANGING lt_table-f02.
  PERFORM p_doublequote CHANGING lt_table-f03.
  PERFORM p_doublequote CHANGING lt_table-f04.
  PERFORM p_doublequote CHANGING lt_table-f05.
  PERFORM p_doublequote CHANGING lt_table-f06.
  PERFORM p_doublequote CHANGING lt_table-f07.
  PERFORM p_doublequote CHANGING lt_table-f08.
  PERFORM p_doublequote CHANGING lt_table-f09.
  PERFORM p_doublequote CHANGING lt_table-f10.
  PERFORM p_doublequote CHANGING lt_table-f11.
  PERFORM p_doublequote CHANGING lt_table-f12.
  PERFORM p_doublequote CHANGING lt_table-f13.
  PERFORM p_doublequote CHANGING lt_table-f14.
  PERFORM p_doublequote CHANGING lt_table-f15.
  PERFORM p_doublequote CHANGING lt_table-f16.
  PERFORM p_doublequote CHANGING lt_table-f17.
  PERFORM p_doublequote CHANGING lt_table-f18.
  PERFORM p_doublequote CHANGING lt_table-f19.
  PERFORM p_doublequote CHANGING lt_table-f20.
  PERFORM p_doublequote CHANGING lt_table-f21.
  PERFORM p_doublequote CHANGING lt_table-f22.
  PERFORM p_doublequote CHANGING lt_table-f23.
  PERFORM p_doublequote CHANGING lt_table-f24.
  PERFORM p_doublequote CHANGING lt_table-f25.
  PERFORM p_doublequote CHANGING lt_table-f26.
  PERFORM p_doublequote CHANGING lt_table-f27.
  PERFORM p_doublequote CHANGING lt_table-f28.
  PERFORM p_doublequote CHANGING lt_table-f29.
  PERFORM p_doublequote CHANGING lt_table-f30.
  PERFORM p_doublequote CHANGING lt_table-f31.
  PERFORM p_doublequote CHANGING lt_table-f32.
  PERFORM p_doublequote CHANGING lt_table-f33.
  PERFORM p_doublequote CHANGING lt_table-f34.
  PERFORM p_doublequote CHANGING lt_table-f35.

  CONCATENATE
            c1 lt_table-f01
            c2 lt_table-f02
            c2 lt_table-f03
            c2 lt_table-f04
            c2 lt_table-f05
            c2 lt_table-f06
            c2 lt_table-f07
            c2 lt_table-f08
            c2 lt_table-f09
            c2 lt_table-f10
            c2 lt_table-f11
            c2 lt_table-f12
            c2 lt_table-f13
            c2 lt_table-f14
            c2 lt_table-f15
            c2 lt_table-f16
            c2 lt_table-f17
            c2 lt_table-f18
            c2 lt_table-f19
            c2 lt_table-f20
            c2 lt_table-f21
            c2 lt_table-f22
            c2 lt_table-f23
            c2 lt_table-f24
            c2 lt_table-f25
            c2 lt_table-f26
            c2 lt_table-f27
            c2 lt_table-f28
            c2 lt_table-f29
            c2 lt_table-f30
            c2 lt_table-f31
            c2 lt_table-f32
            c2 lt_table-f33
            c2 lt_table-f34
            c2 lt_table-f35
            c1 INTO lt_data.


  APPEND lt_data.

  SELECT rcomp name1 cntry stret pstlc city curr
  INTO TABLE lt_table FROM t880
  WHERE langu = p_langu.

  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f041 lv_timestamp h_ext041 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

* if city = empty then use constant "Unknown"
    IF NOT lt_table-f06 IS INITIAL.
    ELSE .

      lt_table-f06 = c_unknown .
    ENDIF.

* Location = city + country
    CONCATENATE lt_table-f06 lt_table-f03
    INTO lv_data
    SEPARATED BY '_' .

* if currency = empty then use constant "USD"
    IF lt_table-f07 IS INITIAL.
      lt_table-f07 = c_usd .
    ENDIF.

* if stret = empty then use constant "Unknown"
    IF lt_table-f04 IS INITIAL.
      lt_table-f04 = c_unknown .
    ENDIF.

* if Postal Code = empty then use constant "Unknown"
    IF lt_table-f05 IS INITIAL.
      lt_table-f05 = c_unknown .
    ENDIF.

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.
    PERFORM p_doublequote CHANGING lt_table-f03.
    PERFORM p_doublequote CHANGING lt_table-f04.
    PERFORM p_doublequote CHANGING lt_table-f05.
    PERFORM p_doublequote CHANGING lt_table-f06.
    PERFORM p_doublequote CHANGING lt_table-f07.
    PERFORM p_doublequote CHANGING lv_data.

    CONCATENATE
                c1 lt_table-f02
                c2 c_space
                c2 lt_table-f01
                c2 c_space
                c2 c_space
                c2 c_space  " parent
                c2 lv_data  " Location
                c2 lt_table-f07
                c2 lt_table-f04
                c2 c_space
                c2 c_space
                c2 lt_table-f06
                c2 lt_table-f03
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 lt_table-f05
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 p_srmail
                c2 c_space
                c2 p_admail
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c1 INTO lt_data.


    APPEND lt_data.
  ENDLOOP.

  PERFORM file_download.

ENDFORM.                    " p_c4_company
*&---------------------------------------------------------------------*
*&      Form  p_c6_company_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c6_company_code.

  REFRESH lt_data .

  REFRESH lt_table .
  lt_table-f01 =  'DISPLAY_NAME' .
  lt_table-f02 =  'DOCUMENT_DESCRIPTION' .
  lt_table-f03 =  'PARENT' .
  lt_table-f04 =  'EXTERNAL_ID' .
  lt_table-f05 =  'EXTERNAL_CATEGORIES' .
  lt_table-f06 =  'INTERNAL_CATEGORIES' .
  lt_table-f07 =  'ORG_UNIT_TYPE' .
  lt_table-f08 =  'SAPCURRENCY' .
  lt_table-f09 =  'SAPGLOBALCC' .

* CSV Header line - HDR
  APPEND '#DataType[masterdata.BusinessUnit]' TO lt_data.

* CSV Header line - Column Names

  CONCATENATE
           c1 lt_table-f01
           c2 lt_table-f02
           c2 lt_table-f03
           c2 lt_table-f04
           c2 lt_table-f05
           c2 lt_table-f06
           c2 lt_table-f07
           c2 lt_table-f08
           c2 lt_table-f09
           c1 INTO lt_data.

  APPEND lt_data.

  SELECT bukrs butxt waers rcomp bukrs_glob
  INTO TABLE lt_table FROM t001
   WHERE spras = p_langu
   .
  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f061 lv_timestamp h_ext061 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

* DISPLAY_NAME
    IF  NOT lt_table-f02 IS INITIAL.
*      DISPLAY_NAME = BUTXT.
      lv_data = lt_table-f02 .
    ELSE.
*      DISPLAY_NAME = BUKRS.
      lv_data = lt_table-f01 .
    ENDIF.

* Parent Company
    IF lt_table-f04 IS INITIAL.
*    if city = empty then use parameter p_parcom
      lt_table-f04 = p_parcom .
    ENDIF.

* EXTERNAL_ID
    CONCATENATE c_compcode lt_table-f01
    INTO lv_data2
    SEPARATED BY '_' .

    PERFORM p_doublequote CHANGING lv_data.
    PERFORM p_doublequote CHANGING lv_data2.
    PERFORM p_doublequote CHANGING lt_table-f03.
    PERFORM p_doublequote CHANGING lt_table-f04.
    PERFORM p_doublequote CHANGING lt_table-f05.
    CONCATENATE
                c1 lv_data
                c2 c_space
                c2 lt_table-f04
                c2 lv_data2  " external_id
                c2 c_space
                c2 c_space
                c2 c_comp_code  " org_unit
                c2 lt_table-f03
                c2 lt_table-f05
                c1 INTO lt_data.
    APPEND lt_data.
  ENDLOOP.
  PERFORM file_download.
ENDFORM.                    " p_c6_company_code
*&---------------------------------------------------------------------*
*&      Form  p_c7_location
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c7_location.

  REFRESH lt_data .

  REFRESH lt_table .
  lt_table-f01 =  'DISPLAY_NAME' .
  lt_table-f02 =  'DOCUMENT_DESCRIPTION' .
  lt_table-f03 =  'ADDRESS_1' .
  lt_table-f04 =  'ADDRESS_2' .
  lt_table-f05 =  'ADDRESS_3' .
  lt_table-f06 =  'CITY' .
  lt_table-f07 =  'COUNTRY' .
  lt_table-f08 =  'COUNTY' .
  lt_table-f09 =  'FAX_1' .
  lt_table-f10 =  'FAX_2' .
  lt_table-f11 =  'FAX_3' .
  lt_table-f12 =  'POSTAL_CODE' .
  lt_table-f13 =  'GEOGRAPHY' .
  lt_table-f14 =  'STATE_PROVINCE' .
  lt_table-f15 =  'TAX_JURISDICTION' .
  lt_table-f16 =  'TELEPHONE_1' .
  lt_table-f17 =  'TELEPHONE_2' .
  lt_table-f18 =  'TELEPHONE_3' .

* CSV Header line - HDR
  APPEND '#DataType[masterdata.location]' TO lt_data.

* CSV Header line - Column Names
  CONCATENATE
            c1 lt_table-f01
            c2 lt_table-f02
            c2 lt_table-f03
            c2 lt_table-f04
            c2 lt_table-f05
            c2 lt_table-f06
            c2 lt_table-f07
            c2 lt_table-f08
            c2 lt_table-f09
            c2 lt_table-f10
            c2 lt_table-f11
            c2 lt_table-f12
            c2 lt_table-f13
            c2 lt_table-f14
            c2 lt_table-f15
            c2 lt_table-f16
            c2 lt_table-f17
            c2 lt_table-f18
            c1 INTO lt_data.

  APPEND lt_data.

  SELECT ort01 land1 regio
  INTO TABLE lt_table FROM t001w
   WHERE spras = p_langu.

  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f071 lv_timestamp h_ext071  INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

    IF  lt_table-f01 IS INITIAL.
      lt_table-f01 = c_unknown .
    ENDIF.

    CONCATENATE lt_table-f01  lt_table-f03 lt_table-f02
        INTO lv_data
        SEPARATED BY '_' .

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.
    PERFORM p_doublequote CHANGING lt_table-f03.
    PERFORM p_doublequote CHANGING lv_data.
    CONCATENATE
                c1 lv_data
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 lt_table-f01  " City
                c2 lt_table-f02  " Country
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c2 lt_table-f03
                c2 c_space
                c2 c_space
                c2 c_space
                c2 c_space
                c1 INTO lt_data.
    APPEND lt_data.
  ENDLOOP.
  PERFORM file_download.
ENDFORM.                    " p_c7_location
*&---------------------------------------------------------------------*
*&      Form  p_c8_plantorgunit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c8_plantorgunit.
  REFRESH lt_data .

  REFRESH lt_table .
  lt_table-f01 =  'DISPLAY_NAME' .
  lt_table-f02 =  'DOCUMENT_DESCRIPTION' .
  lt_table-f03 =  'PARENT' .
  lt_table-f04 =  'EXTERNAL_ID' .
  lt_table-f05 =  'EXTERNAL_CATEGORIES' .
  lt_table-f06 =  'INTERNAL_CATEGORIES' .
  lt_table-f07 =  'ORG_UNIT_TYPE' .

* CSV Header line - HDR
  APPEND '#DataType[masterdata.BusinessUnit]' TO lt_data.

* CSV Header line - Column Names

  CONCATENATE
            c1 lt_table-f01
            c2 lt_table-f02
            c2 lt_table-f03
            c2 lt_table-f04
            c2 lt_table-f05
            c2 lt_table-f06
            c2 lt_table-f07
            c1 INTO lt_data.

  APPEND lt_data.

  SELECT a~bwkey a~bukrs b~werks b~name1 INTO TABLE lt_table FROM
    ( t001k AS a INNER JOIN t001w AS b ON a~bwkey = b~bwkey )
  WHERE b~spras = p_langu.

* Filename
  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f081 lv_timestamp h_ext081 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

* DISPLAY_NAME = *T001W-NAME1(If T001W-NAME1 is blank, use WERKS)

    IF NOT lt_table-f04 IS INITIAL.
      lv_data = lt_table-f04 .
    ELSEIF  NOT lt_table-f03 IS INITIAL.
      lv_data = lt_table-f03 .
    ENDIF.
* PARENT = CompanyCode _ T001K-BUKRS .

    IF NOT lt_table-f02 IS INITIAL.
      CONCATENATE c_compcode lt_table-f02
          INTO lv_data2 SEPARATED BY '_' .
    ENDIF.

*  EXTERNAL_ID = "Plant" _ T001W-WERKS
    IF NOT lt_table-f03 IS INITIAL.
      CONCATENATE c_plant lt_table-f03
            INTO lv_data3 SEPARATED BY '_' .
    ELSE.
*     Dont know what to do!
    ENDIF.

    PERFORM p_doublequote CHANGING lv_data.
    PERFORM p_doublequote CHANGING lv_data2.
    PERFORM p_doublequote CHANGING lv_data3.

    CONCATENATE
                c1 lv_data
                c2 c_space
                c2 lv_data2
                c2 lv_data3
                c2 c_space
                c2 c_space
                c2 c_plant
                c1 INTO lt_data.
    APPEND lt_data.
  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c8_plantorgunit
*&---------------------------------------------------------------------*
*&      Form  p_c9_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c9_plant.
  REFRESH lt_data .

  REFRESH lt_table .
  lt_table-f01 =  'DISPLAY_NAME' .
  lt_table-f02 =  'DOCUMENT_DESCRIPTION' .
  lt_table-f03 =  'EXTERNAL_ID' .
  lt_table-f04 =  'LOCATION' .
  lt_table-f05 =  'COMPANY' .
  lt_table-f06 =  'COLLN_BUSINESS_UNITS' .
  lt_table-f07 =  'COLLN_INTERNAL_CATS' .
  lt_table-f08 =  'COLLN_EXTERNAL_CATS' .

* CSV Header line - HDR
  APPEND '#DataType[masterdata.Plant]' TO lt_data.

* CSV Header line - Column Names
  CONCATENATE
            c1 lt_table-f01
            c2 lt_table-f02
            c2 lt_table-f03
            c2 lt_table-f04
            c2 lt_table-f05
            c2 lt_table-f06
            c2 lt_table-f07
            c2 lt_table-f08
            c1 INTO lt_data.

  APPEND lt_data.

  SELECT a~bwkey a~bukrs b~werks b~name1 b~ort01 b~land1 b~regio c~rcomp
                                                     INTO TABLE lt_table
                                                     FROM ( ( t001k AS a
                            INNER JOIN t001w AS b ON b~bwkey = a~bwkey )
                             INNER JOIN t001 AS c ON c~bukrs = a~bukrs )
                          WHERE b~spras = p_langu.

* Filename
  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f091 lv_timestamp h_ext091 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

* DISPLAY_NAME = *T001W-NAME1(If T001W-NAME1 is blank, use WERKS)

    IF NOT lt_table-f04 IS INITIAL.
      lv_data = lt_table-f04 .
    ELSEIF  NOT lt_table-f03 IS INITIAL.
      lv_data = lt_table-f03 .
    ENDIF.

* LOCATION = Concatanate T001W-ORT01,T001W-REGIO,T001W-LAND1.
    IF lt_table-f05 IS INITIAL.
      lt_table-f05 = c_unknown .
    ENDIF.
    CONCATENATE lt_table-f05 lt_table-f07 lt_table-f06
        INTO lv_data2 SEPARATED BY '_' .

*  COMPANY = T001-RCOMP or Default/user-given
    IF NOT lt_table-f08 IS INITIAL.
      lv_data3 = lt_table-f08 .
    ELSE.
*   if empty then use parameter p_parcom
      lv_data3 = p_parcom .
    ENDIF.

    PERFORM p_doublequote CHANGING lv_data.
    PERFORM p_doublequote CHANGING lv_data2.
    PERFORM p_doublequote CHANGING lv_data3.
    PERFORM p_doublequote CHANGING lt_table-f03.

    CONCATENATE
               c1 lv_data
               c2 c_space
               c2 lt_table-f03  " EXTERNAL_ID
               c2 lv_data2      " Location
               c2 lv_data3      " Company
               c2 c_space
               c2 c_space
               c2 c_space
               c1 INTO lt_data.

    APPEND lt_data.
  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c9_plant
*&---------------------------------------------------------------------*
*&      Form  p_c10_matgroup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c10_matgroup.
  TABLES: t023t.
  DATA: BEGIN OF lt_t023t OCCURS 0,
        matkl LIKE t023t-matkl,
        wgbez LIKE t023t-wgbez,
        wgbez60 LIKE t023t-wgbez60,
        END OF lt_t023t.

  SELECT matkl wgbez wgbez60 INTO TABLE lt_t023t
  FROM t023t WHERE spras = p_langu.
  REFRESH: lt_data,
           lt_table.
  CLEAR : lt_table.

  CONCATENATE h_f101 sy-datum '-' sy-uzeit  h_ext101 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

* e-sourcing column names
  lt_data = '#DataType[masterdata.InternalCat]'.
  APPEND lt_data.

  lt_table-f01 = 'DISPLAY_NAME'.
  lt_table-f02 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f03 = 'CATEGORY_ID'.
  lt_table-f04 = 'PARENT'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c1 INTO lt_data.
  APPEND lt_data.

  REFRESH lt_table.
  CLEAR lt_table.

  LOOP AT lt_t023t.
    lt_table-f01 =  lt_t023t-wgbez.

    lt_table-f02 =  lt_t023t-wgbez60.
    lt_table-f03 =  lt_t023t-matkl.
    lt_table-f04 =  space.

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.
    PERFORM p_doublequote CHANGING lt_table-f03.
    PERFORM p_doublequote CHANGING lt_table-f04.

    CONCATENATE c1 lt_table-f01
                c2 lt_table-f02
                c2 lt_table-f03
                c2 lt_table-f04
                c1 INTO lt_data.
    APPEND lt_data.
    CLEAR lt_table.
  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c10_matgroup
*&---------------------------------------------------------------------*
*&      Form  p_c11_uomtype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c11_uomtype.
  TABLES: t006t.

  DATA: BEGIN OF lt_t006t OCCURS 0,
        dimid LIKE t006t-dimid,
        txdim LIKE t006t-txdim,
        END OF lt_t006t.

  SELECT dimid txdim INTO TABLE lt_t006t FROM t006t WHERE spras =
p_langu.
  REFRESH: lt_data,
           lt_table.
  CLEAR lt_table.
  CONCATENATE h_f111 sy-datum '-' sy-uzeit h_ext111 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

* e-sourcing column names
  lt_data = '#DataType[masterdata.ValueListValue]'.
  APPEND lt_data.

  lt_table-f01 = 'DISPLAY_NAME_ID'.
  lt_table-f02 = 'DISPLAY_NAME'.
  lt_table-f03 = 'PARENT'.
  lt_table-f04 = 'ALTERNATE_NAME'.
  lt_table-f05 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f06 = 'DESCRIPTION_ID'.


  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c1 INTO lt_data.
  APPEND lt_data.

  LOOP AT lt_t006t.

    lt_table-f01 = space.
    lt_table-f02 = lt_t006t-dimid.
    lt_table-f03 = 'unit_category'.
    lt_table-f04 = lt_t006t-txdim.

    lt_table-f05 = space.
    lt_table-f06 = space.


    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.
    PERFORM p_doublequote CHANGING lt_table-f03.
    PERFORM p_doublequote CHANGING lt_table-f04.
    PERFORM p_doublequote CHANGING lt_table-f05.
    PERFORM p_doublequote CHANGING lt_table-f06.

    CONCATENATE c1 lt_table-f01
                c2 lt_table-f02
                c2 lt_table-f03
                c2 lt_table-f04
                c2 lt_table-f05
                c2 lt_table-f06
                c1 INTO lt_data.

    APPEND lt_data.
    CLEAR lt_table.
  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c11_uomtype
*&---------------------------------------------------------------------*
*&      Form  p_c12_unitofmeasure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c12_unitofmeasure.

  TABLES: t006, t006a, t006d.

  DATA: BEGIN OF lt_t006 OCCURS 0,
        msehi LIKE t006-msehi,
        andec LIKE t006-andec,
        dimid(12),
        isocode(6),
        END OF lt_t006.
  DATA: lv_andec(5).

  DATA: BEGIN OF lt_t006a OCCURS 0,
        msehi LIKE t006a-msehi,
        mseh3 LIKE t006a-mseh3,
        mseht LIKE t006a-mseht,
        END OF lt_t006a.

  DATA: BEGIN OF lt_t006d OCCURS 0,
        dimid LIKE t006d-dimid,
        mssie LIKE t006d-mssie,
        END OF lt_t006d.

  DATA :lv_string TYPE string.

  SELECT msehi andec dimid isocode INTO TABLE lt_t006 FROM t006.

  SELECT t006~msehi t006a~mseh3 t006a~mseht INTO TABLE lt_t006a FROM
t006a
        INNER JOIN t006 ON t006a~msehi = t006~msehi
        WHERE spras = p_langu.
  SORT lt_t006a BY msehi.

  SELECT t006d~dimid t006d~mssie INTO TABLE lt_t006d FROM t006d
         INNER JOIN t006 ON t006d~dimid = t006~dimid AND
                            t006d~mssie = t006~msehi.
  SORT lt_t006d BY dimid mssie.

  REFRESH: lt_data, lt_table.
  CLEAR lt_table.
  CONCATENATE h_f121 sy-datum '-' sy-uzeit h_ext121 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  lt_data = '#DataType[masterdata.UnitsOfMeasure]'.
  APPEND lt_data.

  lt_table-f01 = 'DISPLAY_NAME'.
  lt_table-f02 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f03 = 'CATEGORY'.
  lt_table-f04 = 'CONVERSION'.
  lt_table-f05 = 'CONVERSION_SCALE'.
  lt_table-f06 = 'UNIT_PLUGIN'.
  lt_table-f07 = 'SYNONYMS'.
  lt_table-f08 = 'PRECISION'.
  lt_table-f09 = 'SCALE'.
  lt_table-f10 = 'PRIMARY_UNIT'.
  lt_table-f11 = 'SAPINTERNALUM'.
  lt_table-f12 = 'SAPCOMMUM'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c2 lt_table-f07
              c2 lt_table-f08
              c2 lt_table-f09
              c2 lt_table-f10
              c2 lt_table-f11
              c2 lt_table-f12
              c1 INTO lt_data.
  APPEND lt_data.

  REFRESH lt_table.
  CLEAR lt_table.

  LOOP AT lt_t006.
    IF lt_t006-isocode NE space.
      lt_table-f01 = lt_t006-isocode.
    ELSE.
      lt_table-f01 = lt_t006-msehi.
    ENDIF.

    READ TABLE lt_t006a WITH KEY msehi = lt_t006-msehi
                             BINARY SEARCH.
    IF lt_t006a-mseh3 NE space.
      lt_table-f12 = lt_t006a-mseh3.
    ELSE.
      lt_table-f12 = space.
    ENDIF.
    IF lt_t006a-mseht NE space.
      lt_table-f02 = lt_t006a-mseht.
    ENDIF.

    lt_table-f03 = lt_t006-dimid.
    lt_table-f04 = '1'.
    lt_table-f05 = '0'.
    lt_table-f06 = space.
    lt_table-f07 = space.
    lv_string    = lt_t006-andec.

    lt_table-f08 = lv_string.
    lt_table-f09 = lv_string.

    READ TABLE lt_t006d WITH KEY dimid = lt_t006-dimid
                                 mssie = lt_t006-msehi
                            BINARY SEARCH.
    IF sy-subrc = 0.
      lt_table-f10 = 'TRUE'.
    ELSE.
      lt_table-f10 = 'FALSE'.
    ENDIF.

    lt_table-f11 = lt_t006-msehi.

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.
    PERFORM p_doublequote CHANGING lt_table-f03.
    PERFORM p_doublequote CHANGING lt_table-f04.
    PERFORM p_doublequote CHANGING lt_table-f05.
    PERFORM p_doublequote CHANGING lt_table-f06.
    PERFORM p_doublequote CHANGING lt_table-f07.
    PERFORM p_doublequote CHANGING lt_table-f08.
    PERFORM p_doublequote CHANGING lt_table-f09.
    PERFORM p_doublequote CHANGING lt_table-f10.
    PERFORM p_doublequote CHANGING lt_table-f11.
    PERFORM p_doublequote CHANGING lt_table-f12.


    CONCATENATE c1 lt_table-f01
                c2 lt_table-f02
                c2 lt_table-f03
                c2 lt_table-f04
                c2 lt_table-f05
                c2 lt_table-f06
                c2 lt_table-f07
                c2 lt_table-f08
                c2 lt_table-f09
                c2 lt_table-f10
                c2 lt_table-f11
                c2 lt_table-f12
                c1 INTO lt_data.
    APPEND lt_data.
    CLEAR: lt_table,
           lt_t006.
  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c12_unitofmeasure
*&---------------------------------------------------------------------*
*&      Form  p_c13_mattype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c13_mattype.
  TABLES: t134t.
  DATA: BEGIN OF lt_t134t OCCURS 0,
        mtart LIKE t134t-mtart,
        mtbez LIKE t134t-mtbez,
        END OF lt_t134t.

  SELECT mtart mtbez INTO TABLE lt_t134t FROM t134t
  WHERE spras = p_langu.
  REFRESH: lt_data, lt_table.
  CLEAR : lt_table.
  CONCATENATE h_f131 sy-datum '-' sy-uzeit h_ext131 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

* e-sourcing column names
  lt_data = '#DataType[masterdata.ValueListValue]'.
  APPEND lt_data.

  lt_table-f01 = 'DISPLAY_NAME_ID'.
  lt_table-f02 = 'DISPLAY_NAME'.
  lt_table-f03 = 'PARENT'.
  lt_table-f04 = 'ALTERNATE_NAME'.
  lt_table-f05 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f06 = 'DESCRIPTION_ID'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c1 INTO lt_data.
  APPEND lt_data.

  REFRESH lt_table.
  CLEAR lt_table.

  LOOP AT lt_t134t.

    lt_table-f01 = space.
    lt_table-f02 = lt_t134t-mtart.
    lt_table-f03 = 'material_value_list_1'.
    lt_table-f04 = lt_t134t-mtbez.

    lt_table-f05 = space.
    lt_table-f06 = space.

    CONCATENATE c1 lt_table-f01
                c2 lt_table-f02
                c2 lt_table-f03
                c2 lt_table-f04
                c2 lt_table-f05
                c2 lt_table-f06
                c1 INTO lt_data.
    APPEND lt_data.
    CLEAR: lt_table,
           lt_t134t.
  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c13_mattype
*&---------------------------------------------------------------------*
*&      Form  p_c15_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c15_group.
  TABLES: t024.

  DATA: BEGIN OF lt_t024 OCCURS 0,
        ekgrp LIKE t024-ekgrp,
        eknam LIKE t024-eknam,
        ektel LIKE t024-ektel,
        END OF lt_t024.

  SELECT ekgrp eknam ektel INTO TABLE lt_t024 FROM t024.

  CONCATENATE h_f151 sy-datum '-' sy-uzeit h_ext151 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  REFRESH: lt_data,
           lt_table.
  CLEAR : lt_table.

* e-sourcing column names
  lt_data = '#DataType[masterdata.PurchasingGroup]'.
  APPEND lt_data.

  lt_table-f01 = 'DISPLAY_NAME'.
  lt_table-f02 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f03 = 'BOUND_CTX'.
  lt_table-f04 = 'PARENT'.
  lt_table-f05 = 'EXTERNAL_ID'.
  lt_table-f06 = 'MANAGER'.
  lt_table-f07 = 'ROLES'.
  lt_table-f08 = 'SAPPGROUPFLAG'.
*
  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c2 lt_table-f07
              c2 lt_table-f08
              c1 INTO lt_data.
  APPEND lt_data.

  REFRESH lt_table.
  CLEAR lt_table.

  LOOP AT lt_t024.

    lt_table-f01 = lt_t024-eknam.
    lt_table-f02 = lt_t024-ektel.

    lt_table-f03 = space.
    lt_table-f04 = space.
    lt_table-f05 = lt_t024-ekgrp.
    lt_table-f06 = space.
    lt_table-f07 = space.
    lt_table-f08 = 'TRUE'.

    CONCATENATE c1 lt_table-f01
                c2 lt_table-f02
                c2 lt_table-f03
                c2 lt_table-f04
                c2 lt_table-f05
                c2 lt_table-f06
                c2 lt_table-f07
                c2 lt_table-f08
                c1 INTO lt_data.

    APPEND lt_data.
    CLEAR lt_table.
  ENDLOOP.
  PERFORM file_download.
ENDFORM.                    " p_c15_group
*&---------------------------------------------------------------------*
*&      Form  p_c17_purhasingorg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c17_purhasingorg.
  TABLES: t024e, t024w, t024z.

* Internal tables
  DATA:
    BEGIN OF lt_porg OCCURS 0,
      ekorg LIKE t024e-ekorg,
      ekotx LIKE t024e-ekotx,
      bukrs LIKE t024e-bukrs,
    END OF lt_porg,

    BEGIN OF lt_porg_plants OCCURS 0,
      ekorg LIKE t024w-ekorg,
      werks LIKE t024w-werks,
    END OF lt_porg_plants,

    BEGIN OF lt_porg_t024z OCCURS 0,
      ekorg LIKE t024z-ekorg,
      ekorz LIKE t024z-ekorz,
    END OF lt_porg_t024z,

    BEGIN OF lt_xml OCCURS 0,
      line(256),
    END OF lt_xml ,

    ls_xml TYPE string.


  CONSTANTS:
  lc_xml_header(100)  TYPE c    VALUE
  '<?xml version="1.0" encoding="utf-8" ?>',
  lc_xml_strt(50)     TYPE c    VALUE '<MT_FCI_PURORG>',
  lc_xml_end(60)          TYPE c    VALUE
  '</objects></fcidataimport></MT_FCI_PURORG>',
  lc_xml_fci(100)          TYPE c    VALUE
 '<fcidataimport importer="com.frictionless.sap.integration.Importer">',
  lc_objects(15)          TYPE c    VALUE '<objects>',
  lc_st_object(60)        TYPE c    VALUE
  '<object classname="analysis.CustomMD4">',
  lc_et_object(15)        TYPE c    VALUE '</object>',
  lc_st_field(15)         TYPE c    VALUE '<fields>',
  lc_et_field(15)         TYPE c    VALUE '</fields>',
  lc_st_row(15)           TYPE c    VALUE '<row>',
  lc_et_row(15)           TYPE c    VALUE '</row>'
  .

  CLEAR lt_xml.

  CONCATENATE lc_xml_header lc_xml_strt lc_xml_fci lc_objects INTO
  lt_xml-line.
  APPEND lt_xml.


* Select from ERP DB Purchasing Organizations Basic Data
  SELECT ekorg ekotx bukrs
      INTO TABLE lt_porg
      FROM t024e
    .

  IF sy-subrc EQ 0 .
    SORT lt_porg BY ekorg.
    LOOP AT lt_porg.

      CONCATENATE lc_st_object lc_st_field '<DISPLAY_NAME>' lt_porg-ekorg
                                       '</DISPLAY_NAME>' INTO lt_xml-line.
      APPEND lt_xml.

* Have to replace c_comma with space from document_description
      lv_data2 = lt_porg-ekotx .

      CONCATENATE '<DOCUMENT_DESCRIPTION>'  lv_data2
    '</DOCUMENT_DESCRIPTION>' INTO lt_xml-line.
      APPEND lt_xml.

      lt_xml-line = '<INACTIVE>FALSE</INACTIVE>' .
      APPEND lt_xml.


      lt_xml-line =  '<PARENT/>' .
      APPEND lt_xml.

      CONCATENATE '<EXTERNAL_ID>' lt_porg-ekorg '</EXTERNAL_ID>'
        lc_et_field INTO lt_xml-line.
      APPEND lt_xml.

      lt_xml-line = '<extensions>'.
      APPEND lt_xml.

      IF NOT lt_porg-bukrs IS INITIAL .
        CONCATENATE c_compcode lt_porg-bukrs INTO lv_data
        SEPARATED BY '_' .

        CONCATENATE lc_st_field '<SAPCCODE>' lv_data '</SAPCCODE>'
  '</fields>' INTO lv_data .
      ELSE.
        lv_data = '<fields><SAPCCODE/></fields>' .
      ENDIF.

      lt_xml-line = lv_data .
      APPEND lt_xml.


      lt_xml-line = '<collection name="PLANTS">' .
      APPEND lt_xml.

* Plants from T024W
      REFRESH lt_porg_plants .
      SELECT ekorg werks INTO TABLE lt_porg_plants FROM t024w
        WHERE ekorg = lt_porg-ekorg
        .

      IF sy-subrc EQ 0 .


        LOOP AT lt_porg_plants .

          IF NOT lt_porg_plants-werks IS INITIAL .

            CONCATENATE  lc_st_row lc_st_field '<SAPPLANT>'
              lt_porg_plants-werks '</SAPPLANT></fields></row>'
              INTO lv_data2 .
          ELSE.
            lv_data2 = '<row><fields><SAPPLANT/></fields></row>'.
          ENDIF.
          lt_xml-line = lv_data2 .
          APPEND lt_xml.

        ENDLOOP.
        lt_xml-line = '</collection>'.
        APPEND lt_xml.


      ELSE .
        lv_data2 = '<row><fields><SAPPLANT/></fields></row></collection>'.
        lt_xml-line = lv_data2 .
        APPEND lt_xml.

      ENDIF.

* Porgs from T024Z


      REFRESH lt_porg_t024z .
      SELECT ekorg ekorz INTO TABLE lt_porg_t024z FROM t024z
        WHERE ekorz = lt_porg-ekorg .

      IF sy-subrc EQ 0 .

        lt_xml-line = '<collection name="PORGS">' .

        APPEND lt_xml.

        LOOP AT lt_porg_t024z .

          IF NOT lt_porg_t024z-ekorg IS INITIAL .

            CONCATENATE lc_st_row lc_st_field '<SAPPORG>'
  lt_porg_t024z-ekorg '</SAPPORG></fields></row>' INTO lv_data2 .
          ELSE.
            lv_data2 = '<row><fields><SAPPORG/></fields></row>'.
          ENDIF.
          lt_xml-line = lv_data2 .
          APPEND lt_xml.

        ENDLOOP.
        lt_xml-line = '</collection>'.
        APPEND lt_xml.

      ELSE .
        lt_xml-line =  '<collection name="PORGS"/>' .

        APPEND lt_xml.

      ENDIF.

      lt_xml-line = '</extensions>'.
      APPEND lt_xml.

      lt_xml-line = '</object>'.
      APPEND lt_xml.


    ENDLOOP.

  ENDIF.
* The last tag must be the root closing tag *
  lt_xml-line = '</objects></fcidataimport></MT_FCI_PURORG>'.
  APPEND lt_xml.

  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f171 lv_timestamp h_ext171 INTO lv_filename .
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

* Download XML File
  lt_data[] = lt_xml[] .
  PERFORM file_download.

ENDFORM.                    " p_c17_purhasingorg
*&---------------------------------------------------------------------*
*&      Form  p_c18_accountinggroup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c18_accountinggroup.
  TABLES: t077k, t077y, nriv.

  DATA: BEGIN OF lt_t077k OCCURS 0,
        ktokk LIKE t077k-ktokk,
        numkr LIKE t077k-numkr,
        END OF lt_t077k.

  DATA: BEGIN OF lt_t077y OCCURS 0,
        ktokk LIKE t077y-ktokk,
        txt30 LIKE t077y-txt30,
        END OF lt_t077y.

  DATA: BEGIN OF lt_nriv OCCURS 0,
        nrrangenr LIKE nriv-nrrangenr,
        fromnumber LIKE nriv-fromnumber,
        tonumber LIKE nriv-tonumber,
        externind LIKE nriv-externind,
        END OF lt_nriv.
  DATA: lv_externind LIKE nriv-externind.

  SELECT ktokk numkr INTO TABLE lt_t077k FROM t077k
  WHERE numkr <> space.

  SELECT ktokk txt30 INTO TABLE lt_t077y FROM t077y
                     WHERE spras = p_langu.
  SORT lt_t077y BY ktokk.

  SELECT nrrangenr fromnumber tonumber externind INTO TABLE lt_nriv
  FROM nriv WHERE object = 'KREDITOR'.
  SORT lt_nriv BY nrrangenr.

  REFRESH: lt_data,
           lt_table.
  CLEAR lt_table.
  CONCATENATE h_f181 sy-datum '-' sy-uzeit h_ext181 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

* e-sourcing column names
  lt_data = '#DataType[masterdata.ValueListValue]'.
  APPEND lt_data.

  lt_table-f01 = 'DISPLAY_NAME_ID'.
  lt_table-f02 = 'DISPLAY_NAME'.
  lt_table-f03 = 'PARENT'.
  lt_table-f04 = 'ALTERNATE_NAME'.
  lt_table-f05 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f06 = 'DESCRIPTION_ID'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c1 INTO lt_data.
  APPEND lt_data.
  REFRESH lt_table.
  CLEAR lt_table.
  LOOP AT lt_t077k.
    lt_table-f01 = space.
    lt_table-f02 = lt_t077k-ktokk.
    lt_table-f03 = 'Accounting Group'.
    READ TABLE lt_t077y WITH KEY ktokk = lt_t077k-ktokk
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      lt_table-f04 = lt_t077y-txt30.
    ENDIF.

    READ TABLE lt_nriv WITH KEY nrrangenr = lt_t077k-numkr
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      IF lt_nriv-externind = 'X' .
        lv_externind = 'X'.
      ELSE.
        lv_externind = 'I'.
      ENDIF.
      CONCATENATE lt_nriv-nrrangenr '_'
                  lv_externind       '_'
                  lt_nriv-fromnumber '_to_'
                  lt_nriv-tonumber INTO lt_table-f05.
    ELSE.
      lt_table-f05 = space.
    ENDIF.
    lt_table-f06 = space.

    CONCATENATE c1 lt_table-f01
                c2 lt_table-f02
                c2 lt_table-f03
                c2 lt_table-f04
                c2 lt_table-f05
                c2 lt_table-f06
                c1 INTO lt_data.
    APPEND lt_data.
    CLEAR lt_table.
  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c18_accountinggroup
*&---------------------------------------------------------------------*
*&      Form  p_c20_termsofpayment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c20_termsofpayment.
  TABLES: t052, tvzbt.

  DATA: BEGIN OF lt_t052 OCCURS 0,
        zterm LIKE t052-zterm,
        zschf LIKE t052-zschf,
        END OF lt_t052.

  DATA: BEGIN OF lt_tvzbt OCCURS 0,
        zterm LIKE tvzbt-zterm,
        vtext LIKE tvzbt-vtext,
        END OF lt_tvzbt.

  SELECT zterm zschf INTO TABLE lt_t052 FROM t052.
  SORT lt_t052 BY zterm zschf.
  DELETE ADJACENT DUPLICATES FROM lt_t052.

  SELECT tvzbt~zterm tvzbt~vtext INTO TABLE lt_tvzbt
                 FROM tvzbt INNER JOIN t052
                 ON tvzbt~zterm = t052~zterm
                 WHERE tvzbt~spras = p_langu.
  SORT lt_tvzbt BY zterm.
  REFRESH: lt_data,
           lt_table.
  CLEAR lt_table.
  CONCATENATE h_f201 sy-datum '-' sy-uzeit h_ext201 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

* e-sourcing column names
  lt_data = '#DataType[masterdata.PaymentTerm]'.
  APPEND lt_data.

  lt_table-f01 = 'EXTERNAL_ID'.
  lt_table-f02 = 'DISPLAY_NAME'.
  lt_table-f03 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f04 = 'PAY_DAY'.
  lt_table-f05 = 'DISCOUNT'.
  lt_table-f06 = 'NET'.
  lt_table-f07 = 'INACTIVE'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c2 lt_table-f07
              c1 INTO lt_data.
  APPEND lt_data.

  REFRESH lt_table.
  CLEAR lt_table.
  LOOP AT lt_t052.

    lt_table-f01 = lt_t052-zterm.
    lt_table-f02 = lt_t052-zterm.

    READ TABLE lt_tvzbt WITH KEY zterm = lt_t052-zterm BINARY SEARCH.
    IF lt_tvzbt-vtext EQ space.
      lt_table-f03 = lt_t052-zterm.
    ELSE.
      lt_table-f03 = lt_tvzbt-vtext.
    ENDIF.
    lt_table-f04 = '0'.
    lt_table-f05 = '0'.
    lt_table-f06 = '0'.
    IF lt_t052-zschf = space.
      lt_table-f07 = 'FALSE'.
    ELSE.
      lt_table-f07 = 'TRUE'.
    ENDIF.

    CONCATENATE   c1  lt_table-f01
                  c2 lt_table-f02
                  c2 lt_table-f03
                  c2 lt_table-f04
                  c2 lt_table-f05
                  c2 lt_table-f06
                  c2 lt_table-f07
                  c1 INTO lt_data.

    APPEND lt_data.
    CLEAR lt_table.
  ENDLOOP.
  PERFORM file_download.


ENDFORM.                    " p_c20_termsofpayment
*&---------------------------------------------------------------------*
*&      Form  p_c21_glaccount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c21_glaccount.
  TABLES: skb1, ska1.

  DATA: BEGIN OF lt_skb1 OCCURS 0,
        bukrs LIKE skb1-bukrs,
        saknr LIKE skb1-saknr,
        mitkz LIKE skb1-mitkz,
        xloeb LIKE skb1-xloeb,
        xspeb LIKE skb1-xspeb,
        END OF lt_skb1.

  DATA: BEGIN OF lt_ska1 OCCURS 0,
        ktopl LIKE ska1-ktopl,
        saknr LIKE ska1-saknr,
        xbilk LIKE ska1-xbilk,
        txt20 LIKE skat-txt20,
        txt50 LIKE skat-txt50,
        END OF lt_ska1.

  DATA: BEGIN OF lt_tmp1 OCCURS 0,
        ktopl LIKE t001-ktopl,
        END OF lt_tmp1.

  DATA: BEGIN OF lt_t001 OCCURS 0,
        bukrs LIKE t001-bukrs,
        ktopl LIKE t001-ktopl,
        END OF lt_t001.

  DATA: BEGIN OF lt_tmp2 OCCURS 0,
        bukrs LIKE skb1-bukrs,
        END OF lt_tmp2.

  DATA lv_data(160).

  SELECT bukrs saknr mitkz xloeb xspeb INTO TABLE lt_skb1 FROM skb1
  WHERE mitkz = 'K'.
  SORT lt_skb1 BY bukrs saknr.

  SELECT ktopl INTO TABLE lt_tmp1 FROM t001.
  SORT lt_tmp1 BY ktopl.
  DELETE ADJACENT DUPLICATES FROM lt_tmp1.

  SELECT a~ktopl a~saknr a~xbilk b~txt20 b~txt50 INTO TABLE lt_ska1 FROM
     ( ska1 AS a INNER JOIN skat AS b ON a~ktopl = b~ktopl AND a~saknr =
                            b~saknr AND b~spras = p_langu )
  FOR ALL ENTRIES IN lt_tmp1 WHERE a~ktopl = lt_tmp1-ktopl AND a~xbilk =
                           'X'.
  SORT lt_ska1 BY ktopl saknr.

  SELECT bukrs INTO TABLE lt_tmp2 FROM skb1.
  SORT lt_tmp2 BY bukrs.
  DELETE ADJACENT DUPLICATES FROM lt_tmp2.

  SELECT bukrs ktopl INTO TABLE lt_t001 FROM t001
  FOR ALL ENTRIES IN lt_tmp2 WHERE bukrs = lt_tmp2-bukrs.

  SORT lt_t001 BY bukrs.

  REFRESH: lt_data.
  CONCATENATE h_f211 sy-datum '-' sy-uzeit h_ext211 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

* e-sourcing column names
  lt_data = '#DataType[masterdata.GLAccount]'.
  APPEND lt_data.

  lt_table-f01 = 'GL_ACCOUNT_ID'.
  lt_table-f02 = 'EXTERNAL_ID'.
  lt_table-f03 = 'DISPLAY_NAME'.
  lt_table-f04 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f05 = 'SAPCCODE'.
  lt_table-f06 = 'SAPRECONID'.
  lt_table-f07 = 'SAPACCTYPE'.
  lt_table-f08 = 'SAPBLOCKFLAG'.
  lt_table-f09 = 'INACTIVE'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c2 lt_table-f07
              c2 lt_table-f08
              c2 lt_table-f09
              c1 INTO lt_data.
  APPEND lt_data.

  LOOP AT lt_skb1.
    lt_table-f01 = lt_skb1-saknr.
    CONCATENATE lt_skb1-bukrs
                lt_skb1-saknr INTO lt_table-f02
                SEPARATED BY '_'.
    READ TABLE lt_t001 WITH KEY bukrs = lt_skb1-bukrs
                            BINARY SEARCH.
    IF sy-subrc = 0.
      READ TABLE lt_ska1 WITH KEY ktopl = lt_t001-ktopl
                                  saknr = lt_skb1-saknr
                                  BINARY SEARCH.
      IF lt_ska1-txt20 NE space.
        lt_table-f03 = lt_ska1-txt20.
        lt_table-f04 = lt_ska1-txt50.
        lv_data = lt_ska1-xbilk.
      ELSE.
        lt_table-f03 = lt_table-f02.
        lt_table-f04 = lt_table-f02.
        lv_data = lt_table-f02.
      ENDIF.
    ENDIF.

    CONCATENATE 'CompanyCode' lt_skb1-bukrs INTO lt_table-f05
                SEPARATED BY '_'.
    lt_table-f06 = lt_skb1-mitkz.

    IF lv_data = 'X'.
      lt_table-f07 = 'Balance'.
    ELSE.
      lt_table-f07 = 'P & L'.
    ENDIF.

    IF lt_skb1-xspeb = 'X'.
      lt_table-f08 = 'TRUE'.
    ELSE.
      lt_table-f08 = 'FALSE'.
    ENDIF.

    IF lt_skb1-xloeb = 'X'.
      lt_table-f09 = 'TRUE'.
    ELSE.
      lt_table-f09 = 'FALSE'.
    ENDIF.

    CONCATENATE c1 lt_table-f01
                c2 lt_table-f02
                c2 lt_table-f03
                c2 lt_table-f04
                c2 lt_table-f05
                c2 lt_table-f06
                c2 lt_table-f07
                c2 lt_table-f08
                c2 lt_table-f09
                c1 INTO lt_data.
    APPEND lt_data.
    CLEAR: lv_data,
           lt_skb1.

  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c21_glaccount
*&---------------------------------------------------------------------*
*&      Form  p_c1_currency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c1_currency.
  TABLES: tcurc, tcurt, tcurx.
  DATA: BEGIN OF lt_tcurc OCCURS 0,
        isocd LIKE tcurc-isocd,
        ltext LIKE tcurt-ltext,
        END OF lt_tcurc.

  DATA: BEGIN OF lt_tcurx OCCURS 0,
        currkey LIKE tcurx-currkey,
        currdec LIKE tcurx-currdec,
        END OF lt_tcurx.

  DATA: t_currkey(10),
        t_currdec(3).

  REFRESH lt_data.
* Data Type
  lt_data = '#DataType[masterdata.Currency]'.
  APPEND lt_data.
  CLEAR lt_data.

* Header Record Structure
  lt_table-f01 = 'DISPLAY_NAME'.
  lt_table-f02 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f03 = 'IN_EURO'.
  lt_table-f04 = 'DISPLAY_PRECISION'.
  lt_table-f05 = 'STORAGE_PRECISION'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c1 INTO lt_data.
  APPEND lt_data.
  CLEAR :lt_data, lt_table.

* Table Data

  SELECT isocd ltext INTO TABLE lt_tcurc
         FROM ( tcurc AS a INNER JOIN tcurt AS b
                            ON a~waers = b~waers )
         WHERE a~isocd <> space AND spras = p_langu.

  SELECT * FROM tcurx INTO TABLE lt_tcurx.


  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f011 lv_timestamp h_ext011 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_tcurc.
    CLEAR lt_table.
    lt_table-f01 = lt_tcurc-isocd.
    lt_table-f02 = lt_tcurc-ltext.

    READ TABLE lt_tcurx WITH KEY currkey = lt_table-f01.
    IF sy-subrc = 0.
      lt_table-f04 = lt_tcurx-currdec.
      lt_table-f05 = lt_tcurx-currdec.
    ELSE.
      lt_table-f04 = '2'.
      lt_table-f05 = '2'.
    ENDIF.
    CONDENSE lt_table-f04.
    CONDENSE lt_table-f05.

    APPEND lt_table.
  ENDLOOP.


  LOOP AT lt_table.

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.
    PERFORM p_doublequote CHANGING lt_table-f04.
    PERFORM p_doublequote CHANGING lt_table-f05.


    CONCATENATE c1 lt_table-f01
                c2 lt_table-f02
                c2 c_space
                c2 lt_table-f04
                c2 lt_table-f05
                c1 INTO lt_data.

    APPEND lt_data.
    CLEAR lt_data.
  ENDLOOP.

  PERFORM file_download.

ENDFORM.                    " p_c1_currency
*&---------------------------------------------------------------------*
*&      Form  p_c2_country
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c2_country.
  TABLES: t005t.

  DATA: BEGIN OF lt_t005 OCCURS 0,
        land1 LIKE t005t-land1,
        landx LIKE t005t-landx,
        END OF lt_t005.

  REFRESH lt_data.
* Data Type
  lt_data = '#DataType[masterdata.ValueListValue]'.
  APPEND lt_data.
  CLEAR lt_data.

* Header Record Structure
  lt_table-f01 = 'DISPLAY_NAME_ID'.
  lt_table-f02 = 'DISPLAY_NAME'.
  lt_table-f03 = 'PARENT'.
  lt_table-f04 = 'ALTERNATE_NAME'.
  lt_table-f05 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f06 = 'DESCRIPTION_ID'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c1 INTO lt_data .

  APPEND lt_data.
  CLEAR lt_data.

* Table Data
  SELECT land1 INTO TABLE lt_t005 FROM t005.

  REFRESH: lt_table.
  LOOP AT lt_t005.
    lt_table-f01 = lt_t005-land1.
    SELECT SINGLE landx INTO lt_table-f02 FROM t005t WHERE
    land1 = lt_t005-land1 AND spras = p_langu.
    APPEND lt_table.
  ENDLOOP.


  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f021 lv_timestamp h_ext021 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.
    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.

    CONCATENATE c1 c_space
                c2 lt_table-f01
                c2 'country'
                c2 lt_table-f02
                c2 c_space
                c2 c_space
                c1 INTO lt_data.

    APPEND lt_data.
  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c2_country
*&---------------------------------------------------------------------
*&      Form  p_c5_region
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c5_region.
  TABLES: t005s.

  DATA: BEGIN OF lt_t005s OCCURS 0,
        land1 LIKE t005s-land1,
        bland LIKE t005s-bland,
        bezei LIKE t005u-bezei,
        END OF lt_t005s.
  REFRESH: lt_data.
* Data Type
  lt_data = '#DataType[masterdata.ValueListValue]'.
  APPEND lt_data.
  CLEAR lt_data.

* Header Record Structure
  lt_table-f01 = 'DISPLAY_NAME_ID'.
  lt_table-f02 = 'DISPLAY_NAME'.
  lt_table-f03 = 'PARENT'.
  lt_table-f04 = 'ALTERNATE_NAME'.
  lt_table-f05 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f06 = 'DESCRIPTION_ID'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c1 INTO lt_data .

  APPEND lt_data.
  CLEAR lt_data.

* Table Data

  SELECT a~land1 a~bland b~bezei
  INTO TABLE lt_table FROM ( t005s AS a INNER JOIN t005u AS b ON
  b~spras = p_langu AND
  a~land1 = b~land1 AND
  a~bland = b~bland ).

  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f051 lv_timestamp h_ext051 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

    CONCATENATE lt_table-f02 '_' lt_table-f01 INTO lt_table-f01.

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f03.


    CONCATENATE c1 c_space
                c2 lt_table-f01
                c2 'Region'
                c2 c_space
                c2 lt_table-f03
                c2 c_space
                c1 INTO lt_data.

    APPEND lt_data.
  ENDLOOP.
  PERFORM file_download.

ENDFORM.                    " p_c5_region
*&---------------------------------------------------------------------*
*&      Form  p_c14_matstatus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c14_matstatus.
  TABLES: t141, dd07v.

  DATA: BEGIN OF lt_t141 OCCURS 0,
        mmsta LIKE t141-mmsta,
        deink LIKE t141-deink,
        mtstb LIKE t141t-mtstb,
        END OF lt_t141.

  DATA: BEGIN OF lt_dd07v OCCURS 0,
        valpos LIKE dd07l-valpos,
        domvalue_l LIKE dd07l-domvalue_l,
        ddtext LIKE dd07t-ddtext,
        END OF lt_dd07v.

  REFRESH lt_data.
* Data Type
  lt_data = '#DataType[masterdata.ValueListValue]'.
  APPEND lt_data.
  CLEAR lt_data.

* Header Record Structure
  lt_table-f01 = 'DISPLAY_NAME_ID'.
  lt_table-f02 = 'DISPLAY_NAME'.
  lt_table-f03 = 'PARENT'.
  lt_table-f04 = 'ALTERNATE_NAME'.
  lt_table-f05 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f06 = 'DESCRIPTION_ID'.

  CONCATENATE c1 lt_table-f01
              c2 lt_table-f02
              c2 lt_table-f03
              c2 lt_table-f04
              c2 lt_table-f05
              c2 lt_table-f06
              c1 INTO lt_data .


  APPEND lt_data.
  CLEAR lt_data.

* Table Data
  SELECT a~mmsta a~deink b~mtstb INTO TABLE lt_t141 FROM
  ( t141 AS a INNER JOIN t141t AS b ON a~mmsta = b~mmsta )
  WHERE b~spras = p_langu.

  SELECT valpos domvalue_l ddtext INTO TABLE lt_dd07v FROM dd07v
  WHERE domname = 'FEDIA' AND ddlanguage = p_langu.

  LOOP AT lt_t141.
    CLEAR lt_table.
    lt_table-f01 = lt_t141-mmsta.

    READ TABLE lt_dd07v WITH KEY domvalue_l = lt_t141-deink.
    IF sy-subrc = 0.
      CONCATENATE lt_t141-deink '_' lt_dd07v-ddtext INTO   lt_table-f02.
    ENDIF.

    lt_table-f03 = lt_t141-mtstb.
    APPEND lt_table.

  ENDLOOP.

  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f143 lv_timestamp h_ext143 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.
    PERFORM p_doublequote CHANGING lt_table-f03.

    CONCATENATE c1 c_space
                c2 lt_table-f01
                c2 'material_value_list_2'
                c2  lt_table-f02
                c2  lt_table-f03
                c1 INTO lt_data.

    APPEND lt_data.
  ENDLOOP.
  PERFORM file_download.
ENDFORM.                    " p_c14_matstatus
*&---------------------------------------------------------------------*
*&      Form  p_c19_regiongroup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p_c19_regiongroup.
  TABLES: adrreggrpt.

  DATA: BEGIN OF lt_adrreggrp OCCURS 0,
        regiogroup LIKE adrreggrpt-regiogroup,
        descript LIKE adrreggrpt-descript,
        END OF lt_adrreggrp.

  REFRESH: lt_data.
* Data Type
  lt_data = '#DataType[masterdata.ValueListValue]'.
  APPEND lt_data.
  CLEAR lt_data.

* Header Record Structure
  lt_table-f01 = 'DISPLAY_NAME_ID'.
  lt_table-f02 = 'DISPLAY_NAME'.
  lt_table-f03 = 'PARENT'.
  lt_table-f04 = 'ALTERNATE_NAME'.
  lt_table-f05 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f06 = 'DESCRIPTION_ID'.

  CONCATENATE
            c1 lt_table-f01
            c2 lt_table-f02
            c2 lt_table-f03
            c2 lt_table-f04
            c2 lt_table-f05
            c2 lt_table-f06
            c1 INTO lt_data .

  APPEND lt_data.
  CLEAR lt_data.

* Table Data

  SELECT regiogroup descript INTO TABLE lt_table FROM adrreggrpt
  WHERE langu = p_langu.

  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f191 lv_timestamp h_ext191 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.


    CONCATENATE c1 c_space
                c2 lt_table-f01
                c2 'Region Group'
                c2 lt_table-f02
                c2 c_space
                c1 INTO lt_data.

    APPEND lt_data.
  ENDLOOP.
  PERFORM file_download.
ENDFORM.                    " p_c19_regiongroup

*&---------------------------------------------------------------------*
*&      Form  p_c22_deliveryterm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------- --------------------------------------------------*
FORM p_c22_deliveryterm.
  TABLES: tinct.

  DATA: BEGIN OF lt_tinct OCCURS 0,
        inco1 LIKE tinct-inco1,
        bezei LIKE tinct-bezei,
        END OF lt_tinct.

  REFRESH: lt_data.
* Data Type
  lt_data = '#DataType[masterdata.ValueListValue]'.
  APPEND lt_data.
  CLEAR lt_data.

* Header Record Structure
  lt_table-f01 = 'DISPLAY_NAME_ID'.
  lt_table-f02 = 'DISPLAY_NAME'.
  lt_table-f03 = 'PARENT'.
  lt_table-f04 = 'ALTERNATE_NAME'.
  lt_table-f05 = 'DOCUMENT_DESCRIPTION'.
  lt_table-f06 = 'DESCRIPTION_ID'.

  CONCATENATE
   c1 lt_table-f01
   c2 lt_table-f02
   c2 lt_table-f03
   c2 lt_table-f04
   c2 lt_table-f05
   c2 lt_table-f06
   c1 INTO lt_data.
  APPEND lt_data.
  CLEAR lt_data.

* Table Data
  SELECT inco1 bezei INTO TABLE lt_table FROM tinct
  WHERE spras = p_langu.

  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp SEPARATED BY '-'.
  CONCATENATE h_f221 lv_timestamp h_ext221 INTO lv_filename.
  REPLACE '<TimeStamp>' WITH c_space INTO lv_filename.
  CONDENSE lv_filename NO-GAPS.

  LOOP AT lt_table.

    PERFORM p_doublequote CHANGING lt_table-f01.
    PERFORM p_doublequote CHANGING lt_table-f02.


    CONCATENATE c1 c_space
                c2 lt_table-f01
                c2 'delivery_term'
                c2 c_space
                c2 lt_table-f02
                c2 c_space
                c1 INTO lt_data.
    APPEND lt_data.
    CLEAR lt_data.
  ENDLOOP.
  PERFORM file_download.
ENDFORM.                    " p_c22_delivery
*&---------------------------------------------------------------------*
*&      Form  p_doublequote
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*------------------- --------------------------------------------------*
FORM p_doublequote CHANGING p_field.

  SEARCH p_field FOR c1 .
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.
  REPLACE ALL OCCURRENCES OF c1 IN p_field WITH c3 .

ENDFORM.                    " p_doublequote
*------------------- --------------------------------------------------*
