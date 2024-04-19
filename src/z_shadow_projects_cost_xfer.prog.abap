*&---------------------------------------------------------------------*
*& Report  Z_SHADOW_PROJECTS_COST_XFER                                 *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  Z_SHADOW_PROJECTS_COST_XFER                    *
* Author             :  GERGESB1                                       *
* Date               :  15-Jan-2021                                    *
* Technical Contact  :  GERGESB1                                       *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Shadow Proejcts Cost Transfer                  *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 15-Jan-2021  GERGESB1    D30K930824 CHG0209448 - Initial development *
*                                     Shadow Proejcts Cost Transfer    *

*D30K930925 raghu

*25-Aug-2021  DADIM       D30K931501 CHG0224865 - Add posting and      *
*                         D30K931598 document date to input screen     *
*&---------------------------------------------------------------------*
REPORT z_shadow_projects_cost_xfer.

*-----------DATA DECLARATIONS-----------*
TYPES: BEGIN OF type_input,
         segment   TYPE char10,
         projnum   TYPE char10,
         tasknum   TYPE char24,  " Changed BY Raghu
*         taskid      TYPE char10,
         taskname  TYPE char40,
         expend_type TYPE char40,
         reason    TYPE char100,
         amount    TYPE char20,
         pa_period TYPE char20,
         exp_type  TYPE char10,
       END OF type_input,

       BEGIN OF type_output,
         col1  TYPE string,
         col2  TYPE string,
         col3  TYPE string,
         col4  TYPE string,
         col5  TYPE string,
         col6  TYPE string,
         col7  TYPE string,
         col8  TYPE string,
         col9  TYPE string,
         col10 TYPE string,
         col11 TYPE string,
         col12 TYPE string,
         col13 TYPE string,
         col14 TYPE string,
         col15 TYPE string,
         col16 TYPE string,
       END OF type_output.

DATA: gt_input  TYPE STANDARD TABLE OF type_input,
      gt_output TYPE STANDARD TABLE OF type_output,
      gt_return TYPE STANDARD TABLE OF bapiret2.
DATA: lc_file         TYPE string,
      lv_file_content TYPE string_table.

*-----------SELECTION SCREEN-------------*

SELECTION-SCREEN BEGIN OF BLOCK b3.
PARAMETERS: r_conv RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND uc,
            r_post RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
* file path
PARAMETERS: p_ulfile LIKE rlgrap-filename.
PARAMETERS: p_dlfile LIKE rlgrap-filename.

PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY DEFAULT '1101',
            p_blart TYPE blart OBLIGATORY DEFAULT 'ZR',
            p_waers TYPE waers OBLIGATORY DEFAULT 'USD',
            p_xblnr TYPE xblnr,
            p_bktxt TYPE bktxt,
*            p_sname TYPE char40 OBLIGATORY DEFAULT 'CIS-GL'.
*Start of change for CHG0224865 by DADIM
            p_bldat TYPE bldat DEFAULT sy-datum,
            p_budat TYPE budat DEFAULT sy-datum.
*End of change for CHG0224865 by DADIM
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
* file path
PARAMETERS: p_jefile LIKE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b2.

*----------- At Selection-Screen output----------*
AT SELECTION-SCREEN OUTPUT.
  IF r_conv = 'X' .
    LOOP AT SCREEN.
      IF screen-name = 'P_ULFILE'
        OR screen-name = 'P_DLFILE'
        OR screen-name = 'P_BUKRS'
        OR screen-name = 'P_BLART'
        OR screen-name = 'P_WAERS'
        OR screen-name = 'P_XBLNR'
        OR screen-name = 'P_BKTXT'
        OR screen-name = 'P_SNAME'
*Start of change for CHG0224865 by DADIM
        OR screen-name = 'P_BLDAT'
        OR screen-name = 'P_BUDAT'.
*End of change for CHG0224865 by DADIM
        screen-input = 1.
      ENDIF.
      IF screen-name = 'P_JEFILE'.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSEIF r_post = 'X'.
    LOOP AT SCREEN.
      IF screen-name = 'P_ULFILE'
        OR screen-name = 'P_DLFILE'
        OR screen-name = 'P_BUKRS'
        OR screen-name = 'P_BLART'
        OR screen-name = 'P_WAERS'
        OR screen-name = 'P_XBLNR'
        OR screen-name = 'P_BKTXT'
        OR screen-name = 'P_SNAME'
*Start of change for CHG0224865 by DADIM
        OR screen-name = 'P_BLDAT'
        OR screen-name = 'P_BUDAT'.
*End of change for CHG0224865 by DADIM
        screen-input = 0.
      ENDIF.
      IF screen-name = 'P_JEFILE'.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ulfile.
  DATA : lv_fname             TYPE ibipparms-path.
  CLEAR : lv_fname.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = lv_fname.

  p_ulfile = lv_fname.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dlfile.
  DATA : lv_fname             TYPE ibipparms-path.
  CLEAR : lv_fname.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = lv_fname.

  p_dlfile = lv_fname.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_jefile.
  DATA : lv_fname             TYPE ibipparms-path.
  CLEAR : lv_fname.

  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = lv_fname.

  p_jefile = lv_fname.

*------------START-OF-SELECTION------------*
START-OF-SELECTION.


  IF r_conv = 'X'.
    PERFORM get_filedata USING p_ulfile  gt_input.
    IF gt_input IS NOT INITIAL.
      PERFORM conv_inputfile USING gt_input CHANGING gt_output.

      PERFORM dload_inputfile USING p_dlfile gt_output.
    ELSE.
      MESSAGE 'No Data Found' TYPE 'I'.
    ENDIF.
  ELSEIF r_post = 'X'.
    PERFORM get_jedata USING p_jefile CHANGING gt_output.
    IF gt_output IS NOT INITIAL.
      PERFORM post_je USING gt_output CHANGING gt_return.
      IF gt_return IS NOT INITIAL.
        PERFORM display_alv USING gt_return.
      ENDIF.
    ELSE.
      MESSAGE 'No Data Found' TYPE 'I'.
    ENDIF.
  ENDIF.

*****************SUB ROUTINES****************

*&---------------------------------------------------------------------*
*&      Form  GET_FILEDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ULFILE  text
*      <--P_GT_INPUT  text
*----------------------------------------------------------------------*
FORM get_filedata  USING    p_file
                   CHANGING ft_data LIKE gt_input.

  TYPES: BEGIN OF type_tab_str,
           rec(1000) TYPE c,
         END OF type_tab_str.
  DATA: lt_tab_str  TYPE STANDARD TABLE OF type_tab_str,
        ls_tab_str  TYPE type_tab_str,
        lv_filename TYPE string,
        ls_input    TYPE type_input.

  lv_filename = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = lt_tab_str
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
  IF sy-subrc EQ 0.
    LOOP AT lt_tab_str INTO ls_tab_str.
      IF  sy-tabix = '1'.
        CONTINUE. " skip header
      ENDIF.

*      REPLACE ALL OCCURRENCES OF '"' IN ls_tab_str WITH space.
*      CONDENSE ls_tab_str.

      SPLIT ls_tab_str AT '~' INTO ls_input-segment
                                   ls_input-projnum
                                   ls_input-tasknum
*                                   ls_input-taskid
                                   ls_input-taskname
                                   ls_input-expend_type
                                   ls_input-reason
                                   ls_input-amount
                                   ls_input-pa_period
                                   ls_input-exp_type.

      APPEND ls_input TO ft_data.

    ENDLOOP.
  ENDIF.
ENDFORM.                    "get_filedata
*&---------------------------------------------------------------------*
*&      Form  CONV_INPUTFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_INPUT  text
*      <--P_GT_OUTPUT  text
*----------------------------------------------------------------------*
FORM conv_inputfile  USING    ft_data LIKE gt_input
                     CHANGING ft_output LIKE gt_output.

  DATA: lt_output       TYPE STANDARD TABLE OF type_output,
        ls_output       TYPE type_output,
        lv_counter      TYPE i,
        lv_pk(2)        TYPE c,
        lv_fiscyear     TYPE  bapi0002_4-fiscal_year,
        lv_fiscperiod   TYPE bapi0002_4-fiscal_period,
        lv_return       TYPE bapireturn1,
        lv_compcode(10) TYPE c.
*Start of change for CHG0224865 by DADIM
  DATA : lv_key TYPE bschl.
*End of change for CHG0224865 by DADIM


  ls_output-col1 = 'SAP DOC Header'.
  ls_output-col2 = 'Document No.'.
  ls_output-col3 = 'Comp.Code'.
  ls_output-col4 = 'Fiscal Year'.
  ls_output-col5 = 'Doc. Date'.
  ls_output-col6 = 'Posting Date'.
  ls_output-col7 = 'Period'.
  ls_output-col8 = 'Reference'.
  ls_output-col9 = 'Currency'.
  ls_output-col10 =  'Doc.Type'.
  ls_output-col11 = 'Doc.Header Text'.

  APPEND ls_output TO lt_output.


  CLEAR: lv_fiscyear, lv_fiscperiod.
  CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
    EXPORTING
      companycodeid = p_bukrs
      posting_date  = sy-datum
    IMPORTING
      fiscal_year   = lv_fiscyear
      fiscal_period = lv_fiscperiod
      return        = lv_return.

  CLEAR ls_output.

  CLEAR lv_compcode.
  CONCATENATE '*' p_bukrs '*' INTO lv_compcode.

  MOVE:    '' TO ls_output-col1,
           '' TO ls_output-col2,
*           p_bukrs TO ls_output-col3,
           lv_compcode TO ls_output-col3,
           lv_fiscyear TO ls_output-col4,
*Start of change for CHG0224865 by DADIM
*           sy-datum TO ls_output-col5,
*           sy-datum TO ls_output-col6,
           p_bldat TO ls_output-col5,
           p_budat TO ls_output-col6,
*End of change for CHG0224865 by DADIM
           lv_fiscperiod TO ls_output-col7,
           p_xblnr TO ls_output-col8,
           p_waers TO ls_output-col9,
           p_blart TO ls_output-col10,
           p_bktxt  TO ls_output-col11.

  APPEND ls_output TO lt_output.
  CLEAR ls_output.

  MOVE:    'SAP DOC Item' TO ls_output-col1,
           'Item Number' TO ls_output-col2,
           'Posting Key' TO ls_output-col3,
           'GL Account' TO ls_output-col4,
           'Short Text' TO ls_output-col5,
           'Network' TO ls_output-col6,
           'OPAC' TO ls_output-col7,
           'Currency' TO ls_output-col8,
           'Assignment' TO ls_output-col9,
           'Amount' TO ls_output-col10,
           'Long Text' TO ls_output-col11,
           'Cost Center' TO ls_output-col12,
           'WBS' TO ls_output-col13,
           'Expense Type' TO ls_output-col14,
           'Debit/Credit' TO ls_output-col15,
           'Trade Partner' TO ls_output-col16.   " ++ insert by  Raghu

  APPEND ls_output TO lt_output.

  DATA: ls_gl_test LIKE zgl_postings,
        lt_gl_test TYPE STANDARD TABLE OF zgl_postings.

  SELECT * FROM zgl_postings INTO TABLE lt_gl_test WHERE bukrs = p_bukrs.

  IF sy-subrc NE 0.
    REFRESH lt_gl_test.
  ENDIF.

  DATA: ls_data LIKE LINE OF ft_data.

  LOOP AT ft_data INTO ls_data.

    TRANSLATE ls_data-exp_type TO UPPER CASE.

*    LOOP AT lt_gl_test INTO ls_gl_test.   --  deleted by Raghu
    LOOP AT lt_gl_test INTO ls_gl_test WHERE orcl_code = ls_data-segment. " ++ Insert By raghu
      lv_counter = lv_counter + 1.
      CLEAR ls_output.

      MOVE:    '' TO ls_output-col1,
               lv_counter TO ls_output-col2,
               ls_gl_test-posting_key TO ls_output-col3,
               ls_gl_test-gl_acct TO ls_output-col4,
               ls_data-taskname TO ls_output-col5,
               p_waers TO ls_output-col8,
*               'Assignment' TO ls_output-col9,
               '' TO ls_output-col9,
               ls_data-amount TO ls_output-col10,
               ls_data-reason TO ls_output-col11,
*               'Cost Center' TO ls_output-col12,
               '' TO ls_output-col12,
               ls_data-projnum TO ls_output-col13,
               '' TO ls_output-col13,
               '' TO ls_output-col6, " Insert By Raghu
               ls_data-exp_type TO ls_output-col14,
               ls_gl_test-dr_flag TO ls_output-col15,
               ls_gl_test-trade_id TO ls_output-col16.  " ++ Insert  by Raghu


      IF ls_gl_test-dr_flag = 'D'.  " Change by Raghu
*          MOVE ls_data-tasknum TO ls_output-col6.
        MOVE ls_data-tasknum TO ls_output-col13. " Insert by Raghu
*          MOVE 'OPAC' TO ls_output-col7.
      ELSE.  " Insert by Raghu
        CLEAR: ls_output-col13, ls_output-col7. " --  Changed by Raghu
      ENDIF. " Change by Raghu
*Start of change for CHG0224865 by DADIM
      IF ls_data-amount < 0.
        CLEAR : ls_output-col3,lv_key.
        IF ls_gl_test-dr_flag = 'D'.
          SELECT SINGLE posting_key FROM zgl_postings
            INTO lv_key
            WHERE orcl_code = ls_gl_test-orcl_code AND
                  bukrs     = ls_gl_test-bukrs AND
                  dr_flag   = 'C'.
        ELSEIF ls_gl_test-dr_flag = 'C'.
          SELECT SINGLE posting_key FROM zgl_postings
            INTO lv_key
            WHERE orcl_code = ls_gl_test-orcl_code AND
                  bukrs     = ls_gl_test-bukrs AND
                  dr_flag   = 'D'.
        ENDIF.
        ls_output-col3 = lv_key.
        ls_output-col10 = ls_output-col10 * -1.
      ENDIF.
*End of change for CHG0224865 by DADIM


      APPEND ls_output TO lt_output.
    ENDLOOP.
  ENDLOOP.

  ft_output[] = lt_output.

ENDFORM.                    "conv_inputfile
*&---------------------------------------------------------------------*
*&      Form  DLOAD_INPUTFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DLFILE  text
*      -->P_GT_OUTPUT  text
*----------------------------------------------------------------------*
FORM dload_inputfile  USING    p_file
                               ft_output LIKE gt_output.

  DATA: lv_file TYPE string.
  lv_file = p_file.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE            =
      filename                = lv_file
      filetype                = 'ASC'
*     APPEND                  = ' '
      write_field_separator   = 'X'
    TABLES
      data_tab                = ft_output
*     FIELDNAMES              =
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
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    "dload_inputfile
*&---------------------------------------------------------------------*
*&      Form  POST_JE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUTPUT  text
*----------------------------------------------------------------------*
FORM post_je  USING    ft_output LIKE gt_output CHANGING ft_return LIKE gt_return.
  DATA: ls_docheader TYPE bapiache09,
        lt_glacc     TYPE STANDARD TABLE OF bapiacgl09,
        ls_glacc     TYPE bapiacgl09,
        lt_curramt   TYPE STANDARD TABLE OF bapiaccr09,
        ls_curramt   TYPE bapiaccr09,
        lt_return    TYPE STANDARD TABLE OF bapiret2,
        lv_amount    TYPE wrbtr,
        lv_hkont     TYPE hkont,
        lv_compcode  TYPE char10.

  DATA: ls_data LIKE LINE OF gt_output.

  LOOP AT gt_output INTO ls_data.

    IF sy-tabix = 2." second row- header data

      CLEAR lv_compcode.
      lv_compcode = ls_data-col3.
      REPLACE ALL OCCURRENCES OF '*' IN lv_compcode WITH space.
      CONDENSE lv_compcode.

*      ls_docheader-comp_code = ls_data-col3.
      ls_docheader-comp_code = lv_compcode.
      ls_docheader-fisc_year = ls_data-col4.
      ls_docheader-doc_date = sy-datum.
      ls_docheader-pstng_date = sy-datum.
      ls_docheader-username = sy-uname.
      ls_docheader-doc_type = ls_data-col10.
      ls_docheader-ref_doc_no = ls_data-col8.
      ls_docheader-header_txt = ls_data-col11.
    ELSEIF sy-tabix > 3. " Item data

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_data-col4
        IMPORTING
          output = lv_hkont.

      ls_glacc-itemno_acc = ls_data-col2.
      ls_glacc-gl_account = lv_hkont.
      ls_glacc-item_text = ls_data-col11.
      ls_glacc-network = ls_data-col6.
      ls_glacc-activity = ls_data-col7.
      ls_glacc-wbs_element = ls_data-col13.
      ls_glacc-trade_id = ls_data-col16.  " ++ Insert by Raghu
      APPEND ls_glacc TO lt_glacc.
      CLEAR ls_glacc.

      lv_amount = ls_data-col10.
*      IF ls_data-col15 = 'D'.  " Debit  " Raghu -- deleted changed from D to C - bug Fix
*      IF ls_data-col15 = 'C'.  " Credit   " Raghu ++ Insert ""Commented by DADIM for CHG0224865
      IF ls_data-col3 = '50'.   "Added by DADIM for CHG0224865
        lv_amount = lv_amount * -1.
      ENDIF.

      ls_curramt-itemno_acc =  ls_data-col2.
      ls_curramt-amt_doccur =  lv_amount.
      ls_curramt-currency =  ls_data-col8.
      APPEND ls_curramt TO lt_curramt.
      CLEAR ls_curramt.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = ls_docheader
    TABLES
      accountgl      = lt_glacc
      currencyamount = lt_curramt
      return         = lt_return.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*     EXPORTING
*       WAIT          =
*     IMPORTING
*       RETURN        =
              .

    ft_return[] = lt_return[].
  ENDIF.



ENDFORM.                    "post_je
*&---------------------------------------------------------------------*
*&      Form  GET_JEDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_JEFILE  text
*      <--P_GT_OUTPUT  text
*----------------------------------------------------------------------*
FORM get_jedata  USING    p_jefile
                 CHANGING ft_output LIKE gt_output.

  DATA: lv_rawdata  TYPE truxs_t_text_data,
        lv_filename TYPE  rlgrap-filename.

  lv_filename = p_jefile.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
*     I_LINE_HEADER        =
      i_tab_raw_data       = lv_rawdata
      i_filename           = lv_filename
    TABLES
      i_tab_converted_data = ft_output
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    "get_jedata
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_RETURN  text
*----------------------------------------------------------------------*
FORM display_alv  USING    ft_return LIKE gt_return.

  DATA : lo_alv       TYPE REF TO cl_salv_table,        " ALV Class
         lo_columns   TYPE REF TO cl_salv_columns_table,
         lo_column    TYPE REF TO cl_salv_column_table,
         lo_functions TYPE REF TO cl_salv_functions_list,
         lo_header    TYPE REF TO cl_salv_form_layout_grid,
         lo_flow      TYPE REF TO cl_salv_form_layout_flow,
         lo_exception TYPE REF TO cx_root,
         lc_text      TYPE string,
         li_records   TYPE i. " Store number of records

  DATA: lv_no_recs  TYPE char10.
************************************************************************
* Display ALV                                                          *
************************************************************************

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = ft_return.

    CATCH cx_salv_msg INTO lo_exception.

*        MESSAGE s004 WITH lo_exception->get_text( ) DISPLAY LIKE cc_e. "'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.

* Activate ALV generic Functions
  lo_functions = lo_alv->get_functions( ).
  lo_functions->set_all( abap_true ).
  lo_alv->display( ).

ENDFORM.                    "display_alv
