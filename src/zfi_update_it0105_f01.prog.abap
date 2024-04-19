*&---------------------------------------------------------------------*
*&  Include           ZFI_UPDATE_IT0105_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  POPULATE_BDCDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM populate_bdcdata .
  CONCATENATE 'MC' ls_field-card_account_number INTO ls_field-card_account_number.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external = ls_field-card_create_date
    IMPORTING
      date_internal = lv_date1.

  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = lv_date1
    IMPORTING
      output = lv_begda.

  PERFORM:
  fill_bdc_data USING 'SAPMP50A' '1000' 'X'  ' '  ' ',
  fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE' '=INS',               " Insert.
  fill_bdc_data USING  ''  ''  ''   'RP50G-PERNR' ls_field-payee_id,   " Employee ID.
  fill_bdc_data USING  ''  ''  ''   'RP50G-TIMR6' 'X',
  fill_bdc_data USING  ''  ''  ''   'RP50G-CHOIC' '0105',   " Infotype.
  fill_bdc_data USING  ''  ''  ''   'RP50G-SUBTY' '0011',   " Subtype.
  fill_bdc_data USING 'MP010500' '2000' 'X'  ' '  ' ',
  fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE' '=UPD',
  fill_bdc_data USING  ''  ''  ''   'P0105-BEGDA' lv_begda,
  fill_bdc_data USING  ''  ''  ''   'P0105-ENDDA' '12319999',
  fill_bdc_data USING  ''  ''  ''   'P0105-USRID' ls_field-card_account_number.

ENDFORM.                    " POPULATE_BDCDATA
*&---------------------------------------------------------------------*
*&      Form  POPULATE_BDCDATA_EDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM populate_bdcdata_edit .
  CONCATENATE 'MC' ls_field-card_account_number INTO ls_field-card_account_number.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = ls_field-card_create_date
*     ACCEPT_INITIAL_DATE      =
    IMPORTING
      date_internal            = lv_date1
    EXCEPTIONS
      DATE_EXTERNAL_IS_INVALID = 1
      OTHERS                   = 2.
  IF sy-subrc eq 0.
* Implement suitable error handling here
    lv_endda1 = lv_date1 - 1.
    lv_begda = lv_date1.
  ENDIF.


  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = lv_endda1
    IMPORTING
      output = lv_endda.
  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
    EXPORTING
      input  = lv_begda
    IMPORTING
      output = lv_begda.

  PERFORM:

  fill_bdc_data USING 'SAPMP50A' '1000' 'X'  ' '  ' ',
  fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE' '=MOD',
  fill_bdc_data USING  ''  ''  ''   'RP50G-PERNR' ls_field-payee_id,   " Employee ID.
  fill_bdc_data USING  ''  ''  ''   'RP50G-TIMR6' 'X',
  fill_bdc_data USING  ''  ''  ''   'RP50G-CHOIC' '0105',   " Infotype.
  fill_bdc_data USING  ''  ''  ''   'RP50G-SUBTY' '0011',   " Subtype.
  fill_bdc_data USING 'MP010500' '2000' 'X'  ' '  ' ',
  fill_bdc_data USING  ''  ''  ''   'BDC_CURSOR' 'P0105-ENDDA',
  fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE' '=UPD',
  fill_bdc_data USING  ''  ''  ''   'P0105-ENDDA' lv_endda.
  lv_flag = 'X'.
  PERFORM insert_data.
  REFRESH lt_bdcdata.
  PERFORM:
    fill_bdc_data USING 'SAPMP50A' '1000' 'X'  ' '  ' ',
    fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE' '=INS',               " Insert.
    fill_bdc_data USING  ''  ''  ''   'RP50G-PERNR' ls_field-payee_id,   " Employee ID.
    fill_bdc_data USING  ''  ''  ''   'RP50G-TIMR6' 'X',
    fill_bdc_data USING  ''  ''  ''   'RP50G-CHOIC' '0105',   " Infotype.
    fill_bdc_data USING  ''  ''  ''   'RP50G-SUBTY' '0011',   " Subtype.
    fill_bdc_data USING 'MP010500' '2000' 'X'  ' '  ' ',
    fill_bdc_data USING  ''  ''  ''   'BDC_OKCODE' '=UPD',
    fill_bdc_data USING  ''  ''  ''   'P0105-BEGDA' lv_begda,
    fill_bdc_data USING  ''  ''  ''   'P0105-ENDDA' '12319999',
    fill_bdc_data USING  ''  ''  ''   'P0105-USRID' ls_field-card_account_number.
  PERFORM insert_data.
ENDFORM.                    " POPULATE_BDCDATA_EDIT
*&---------------------------------------------------------------------*
*&      Form  INSERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_data .
*Data decleration for Error Message
  DATA:
       lt_msg TYPE TABLE OF bdcmsgcoll,   " Collecting Error messages
       ls_msg TYPE bdcmsgcoll,
       ls_msg1(51).

* Call transaction 'PA30'
  CALL TRANSACTION 'PA30' USING lt_bdcdata
    MODE lv_mode
    UPDATE 'S'
    MESSAGES INTO lt_msg.

  IF sy-subrc EQ 0.
    IF lv_flag EQ 'X'.
      ls_field-status = 'Delimited existing record'.
    ELSE.
      ls_field-status = 'Record  Successfully created in IT0105'.
    ENDIF.
    APPEND ls_field TO lt_alv.
    CLEAR lv_flag.
  ELSE.
    ls_field-status = 'Record creation failed'.
    APPEND ls_field TO lt_alv.
  ENDIF.
ENDFORM.                    " INSERT_DATA

*&---------------------------------------------------------------------*
*&      Form  fill_bdc_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE         text
*      -->(P_PROGRAM)   text
*      -->VALUE         text
*      -->(P_DYNPRO)    text
*      -->VALUE         text
*      -->(P_DYNBEGIN)  text
*      -->VALUE         text
*      -->(P_FNAM)      text
*      -->VALUE         text
*      -->(P_FVAL)      text
*----------------------------------------------------------------------*
FORM fill_bdc_data USING value(p_program)
                      value(p_dynpro)
                      value(p_dynbegin)
                      value(p_fnam)
                      value(p_fval).
  CLEAR ls_bdcdata .
  IF p_dynbegin = 'X' .
    ls_bdcdata-program = p_program .
    ls_bdcdata-dynpro  = p_dynpro .
    ls_bdcdata-dynbegin = p_dynbegin .
    APPEND ls_bdcdata TO lt_bdcdata.
  ELSE.
    ls_bdcdata-fnam = p_fnam.
    ls_bdcdata-fval = p_fval.
    CONDENSE ls_bdcdata-fval.
    APPEND ls_bdcdata TO lt_bdcdata.
  ENDIF.                               " IF p_dynbeg..

ENDFORM .                              " Fill_entry
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_alv .
  CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
    TABLES
      table    = lt_alv
    EXCEPTIONS
      FB_ERROR = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " CREATE_ALV
FORM UPLOAD_DATA .
  lv_filename_string = p_file.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename_string
      filetype                = 'ASC'
      has_field_separator     = 'X'
      dat_mode                = ''
    TABLES
      data_tab                = lt_text_data
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
  IF sy-subrc = 0.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_field_seperator    = 'X'
      i_line_header        = 'X'
      i_tab_raw_data       = lt_text_data
      i_filename           = p_file
    TABLES
      i_tab_converted_data = lt_field
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  ENDIF.
ENDFORM.                    " UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA .
  LOOP AT lt_field INTO ls_field WHERE status EQ 'Active'.
    REFRESH: lt_bdcdata.
    CLEAR ls_bdcdata.
    IF ls_field-payee_id IS NOT INITIAL.
      CALL FUNCTION 'BAPI_EMPLOYEE_CHECKEXISTENCE'
        EXPORTING
          number = ls_field-payee_id
        IMPORTING
          return = lv_check.

      IF lv_check IS INITIAL.
        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
            pernr           = ls_field-payee_id
            infty           = '0105'
            begda           = sy-datum
            endda           = sy-datum
          IMPORTING
            subrc           = lv_subrc
          TABLES
            infty_tab       = lt_p0105
          EXCEPTIONS
            infty_not_found = 1
            OTHERS          = 2.
        if sy-subrc is INITIAL.
          sort lt_p0105 by subty endda.
          READ TABLE lt_p0105 INTO ls_p0105 WITH KEY subty = '0011'
                                                     endda = '99991231' BINARY SEARCH.
          IF sy-subrc = 0.
            PERFORM populate_bdcdata_edit.
          ELSE.
            PERFORM populate_bdcdata.
            PERFORM insert_data.
          ENDIF.
        endif.
      ELSE.
*    Employee not exist in the system.
        ls_field-status = 'Employee not exist in the system'.
        APPEND ls_field TO lt_alv.
      ENDIF.

    ELSE.
* Employee number is blank in the input file
      ls_field-status = 'Employee number is blank in the input file'.
      APPEND ls_field TO lt_alv.
    ENDIF.
    clear:ls_field.
  ENDLOOP.
ENDFORM.                    " PROCESS_DATA
