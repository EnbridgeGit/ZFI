*&---------------------------------------------------------------------*
*& Report  ZFAPI052_HOME_RENOV_REBATE_INV
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapi053_home_renov_rebate_inv.

TABLES: rfpdo.
TYPES: BEGIN OF ty_msg,
        msgrp TYPE char01,
        msgid TYPE symsgid,
        msgty TYPE symsgty,
        msgno TYPE symsgno,
        msg_text(250),
       END OF ty_msg.
DATA:BEGIN OF gt_tab OCCURS 0,         "Text file format
           text1(208),
     END OF gt_tab.
TYPES: BEGIN OF ty_data,
       reference(16) TYPE c, " Reference
       rebate_amt TYPE bapiwrbtr, "Rebate Amount
       pce(6)  TYPE c, "Primary cost Element
       internal_ord(10) TYPE c, "Internal Order
       specl_inst  TYPE string,  " DSM Instruction, no need for BAPI
       name_payee(35) TYPE c, "Payee
       street(35) TYPE c, "Address
       city(35) TYPE c, "City
       province(2)  TYPE c, "Province
       postal_code(7)  TYPE c, "Postal code
       banner_act(7) TYPE c, "Banner Account
       banner_premises(7) TYPE c, "Banner Premises
       END OF ty_data.
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.
DATA: gv_lines TYPE i,
      gv_mail   TYPE string,
      gv_name   TYPE string,
      gv_domain TYPE string,
      gt_id_msg TYPE TABLE OF ty_msg,
      gs_msg TYPE ty_msg,
      gt_bapi_msg TYPE TABLE OF ty_msg,
      gs_bapi_msg TYPE ty_msg,
      gt_bapi_no_error_msg TYPE TABLE OF ty_msg,
      gv_flag_err_proc TYPE xfeld,
      gs_data TYPE ty_data,
      gt_data TYPE TABLE OF ty_data,
      gt_bapiret2 TYPE TABLE OF bapiret2,
      gs_bapiret2 TYPE bapiret2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X'  USER-COMMAND cmd,
            p_sfile   LIKE        rfpdo-rfbifile MODIF ID srv,
            r_local   RADIOBUTTON GROUP rad1,
            p_lfile   TYPE        rfpdo-rfbifile DEFAULT 'H:\' MODIF ID lcl,
            p_errml   TYPE string OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM  f_toggle_functionality.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lfile.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',CSV File,*.csv'
      static    = 'X'
    CHANGING
      file_name = p_lfile.

AT SELECTION-SCREEN ON p_errml.

  gv_mail = p_errml.
  FIND REGEX '^([a-z](?:\w|-|\.)*)@((\w|-|(\.?!\.)))+\.[[:alpha:]]{2,4}$'
  IN gv_mail
  IGNORING CASE
  SUBMATCHES gv_name gv_domain.

  IF sy-subrc <> 0.
    MESSAGE e000(zfi01) WITH
     'Enter correct email address for errors' '' '' ''.
    STOP.
  ENDIF.

START-OF-SELECTION.

  CLEAR: gt_id_msg,
         gt_bapi_msg,
         gt_bapi_no_error_msg,
         gv_flag_err_proc,
         gs_msg,
         gv_lines,
         gt_bapiret2,
         gt_data,
         gs_data.

  IF r_server IS NOT INITIAL.
    IF p_sfile IS INITIAL.
      WRITE: / 'Input Server File path and name.'.
      STOP.
    ELSE.
      PERFORM upload_server_file.
    ENDIF.
  ELSE.
    IF p_lfile IS INITIAL.
      WRITE : / 'Input PC file path and name.'.
      STOP.
    ELSE.
      PERFORM upload_pc_file.
    ENDIF.
  ENDIF.
*  stop.
  IF gv_flag_err_proc IS INITIAL.
    DESCRIBE TABLE gt_data LINES gv_lines.
    "first row of input file is only header
    IF gt_data[] IS INITIAL. " OR
      " gv_lines <= 1.
      gs_msg-msgrp = 'G'.
      gs_msg-msgid = 'ZFI01'.
      gs_msg-msgty = 'E'.
      gs_msg-msgno = '000'.
      MOVE text-013 TO gs_msg-msg_text.
      APPEND gs_msg TO gt_id_msg.
      WRITE : / 'No Data to Process'.
    ELSE.
      PERFORM post_data.
      IF gt_bapi_msg[] IS NOT INITIAL.
        PERFORM email_bapi_error.
        PERFORM print_on_screen.
      ELSE.
        PERFORM print_on_screen_no_error.
      ENDIF.
    ENDIF.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  F_TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_toggle_functionality .
  LOOP AT SCREEN.

* Set the screen fields to display only
    IF  screen-group1 EQ gc_modif_id_dsp.
      screen-input = 0.
    ENDIF.
    IF r_local = 'X'.
      IF screen-group1 = 'LCL'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'LCL'.
        screen-input = 0.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.
ENDFORM.                    " F_TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_SERVER_FILE
*&---------------------------------------------------------------------*
*       Upload Server File
*----------------------------------------------------------------------*
FORM upload_server_file .
 DATA: lv_cnt TYPE i,
        ls_msg TYPE ty_msg.

  OPEN DATASET p_sfile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'E'.
    ls_msg-msgno = '000'.
    MOVE text-011 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    CONCATENATE 'Input File: ' p_sfile INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    gv_flag_err_proc = 'X'.
    EXIT.
  ENDIF.
  DO.
    CLEAR: gs_data,
           gt_tab.
    READ DATASET p_sfile INTO gt_tab-text1. "fl_string.
    IF sy-subrc <> 0.
*      WRITE: / 'Unable to read File data..'.
      EXIT.
    ENDIF.
     "first line is columns header, ignore it.
    lv_cnt = lv_cnt + 1.
    IF lv_cnt = 1.
      CONTINUE.
    ENDIF.
    PERFORM split_data USING gt_tab
                             gs_data.
    APPEND gs_data TO gt_data.
  ENDDO.
  CLOSE DATASET p_sfile.
  IF sy-subrc <> 0.
*    gt_msg-text1 = 'Unable to close text file for process, Please check.'.
*    APPEND gt_msg.
*    WRITE: / 'Unable to close text file for process, Please check.'.
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'E'.
    ls_msg-msgno = '000'.
    MOVE text-002 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    CONCATENATE 'Input File: ' p_sfile INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    gv_flag_err_proc = 'X'.
    EXIT.
  ENDIF.
ENDFORM.                    " UPLOAD_SERVER_FILE
*&---------------------------------------------------------------------*
*&      Form  SPLIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM split_data  USING   p_gt_tab STRUCTURE gt_tab
                         p_output STRUCTURE gs_data.
  DATA: lv_sep(1) VALUE ',',
        lv_amnt1(13),
        lv_amnt2 TYPE p LENGTH 13 DECIMALS 2.

  SPLIT p_gt_tab AT lv_sep
        INTO p_output-reference
             lv_amnt1
             p_output-pce
             p_output-internal_ord
             p_output-specl_inst
             p_output-name_payee
             p_output-street
             p_output-city
             p_output-province
             p_output-postal_code
             p_output-banner_act
             p_output-banner_premises.

*WRITE lv_amnt1 to lv_amnt2 DECIMALS 2.
  p_output-rebate_amt = lv_amnt1.

ENDFORM.                    " SPLIT_DATA
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_pc_file .

  DATA:   lt_auszug TYPE STANDARD TABLE OF string,
          ls_auszug TYPE string,
          lv_auszug_file TYPE string,
          ls_msg TYPE ty_msg.

  REFRESH lt_auszug[].
  lv_auszug_file = p_lfile.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename            = lv_auszug_file
*     has_field_separator = 'X'
      filetype            = 'ASC'
    CHANGING
      data_tab            = lt_auszug
    EXCEPTIONS
      file_open_error     = 1
      file_read_error     = 2
      OTHERS              = 18.
  CASE sy-subrc.
    WHEN 1.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      CONCATENATE 'File Open error in file' p_lfile
            INTO ls_msg-msg_text
            SEPARATED BY space.
      APPEND ls_msg TO gt_id_msg.
      gv_flag_err_proc = 'X'.
    WHEN 2.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      CONCATENATE 'Read error in file' p_lfile
      INTO ls_msg-msg_text
      SEPARATED BY space.
      APPEND ls_msg TO gt_id_msg.
      gv_flag_err_proc = 'X'.
    WHEN 18.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      CONCATENATE 'Read error in file' p_lfile
      INTO ls_msg-msg_text
      SEPARATED BY space.
      APPEND ls_msg TO gt_id_msg.
      gv_flag_err_proc = 'X'.
  ENDCASE.
  LOOP AT lt_auszug INTO ls_auszug.
    "first line is columns header, ignore it.
    IF sy-tabix = 1.
      CONTINUE.
    ENDIF.
    gt_tab-text1 = ls_auszug.
    PERFORM split_data USING gt_tab
                             gs_data.
    APPEND gs_data TO gt_data.
  ENDLOOP.
ENDFORM.                    " UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM post_data .
  DATA: lv_check TYPE xfeld,
        lv_length TYPE i,
        lv_postal_code(7),
        lv_pcode1(3),
        lv_pcode2(3),
        lv_error TYPE xfeld,
        ls_documentheader TYPE bapiache03,
        ls_customercpd  TYPE bapiacpa00,
        lt_accountpayable TYPE TABLE OF bapiacap03,
        ls_accountpayable TYPE bapiacap03,
        lt_accountgl TYPE TABLE OF bapiacgl03,
        ls_accountgl TYPE bapiacgl03,
        lt_currencyamount TYPE TABLE OF bapiaccr01,
        ls_currencyamount TYPE bapiaccr01,
        lt_accounttax TYPE TABLE OF bapiactx01.
  CONSTANTS: lc_bukrs TYPE bapiache03-comp_code VALUE 'UGL',
             lc_vendor TYPE bapiacap03-vendor_no VALUE 'OTRHRR',
             lc_pterms TYPE bapiacap03-pmnttrms VALUE 'N00',
             lc_pmethod_sup TYPE bapiacap03-pmtmthsupl VALUE '02',
             lc_pmethod TYPE bapiacap03-pymt_meth VALUE 'O'.
  LOOP AT gt_data INTO gs_data.

    CLEAR: ls_documentheader,
           ls_customercpd,
           ls_accountpayable,
           ls_currencyamount,
           ls_accountgl,
           lt_accountpayable,
           lt_currencyamount,
           lt_accountgl,

           gt_bapiret2.

    "-----------Header
    ls_documentheader-username = sy-uname.
    ls_documentheader-header_txt = 'Home Reno Rebate'.
    ls_documentheader-comp_code = lc_bukrs.
    ls_documentheader-doc_date = sy-datum.
    ls_documentheader-pstng_date = sy-datum.
    ls_documentheader-doc_type = 'KN'.
    ls_documentheader-ref_doc_no = gs_data-reference.
    "------------One time Customer
    lv_length = strlen( gs_data-postal_code ).
    IF lv_length < 7.
      lv_pcode1 = gs_data-postal_code(3).
      lv_pcode2 = gs_data-postal_code+3(3).
      CONCATENATE lv_pcode1 lv_pcode2
                  INTO gs_data-postal_code
                  SEPARATED BY space.
    ENDIF.
    ls_customercpd-name = gs_data-name_payee.
    ls_customercpd-postl_code = gs_data-postal_code.
    ls_customercpd-city = gs_data-city.
    ls_customercpd-country = 'CA'.
    ls_customercpd-street = gs_data-street.
    ls_customercpd-region = 'ON'. "gs_data-province.
    "-----------Accounts Payable
    ls_accountpayable-itemno_acc = '0000000001'.
    ls_accountpayable-vendor_no = lc_vendor.
    ls_accountpayable-pmnttrms = lc_pterms.
    ls_accountpayable-pmtmthsupl = lc_pmethod_sup.
    ls_accountpayable-pymt_meth = lc_pmethod.
    CONCATENATE gs_data-banner_act gs_data-banner_premises 'Banner'
                INTO ls_accountpayable-item_text SEPARATED BY space.
    APPEND ls_accountpayable TO lt_accountpayable.
    "--------Accounts GL
    ls_accountgl-itemno_acc = '0000000002'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_data-pce
      IMPORTING
        output = ls_accountgl-gl_account.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_data-internal_ord
      IMPORTING
        output = ls_accountgl-orderid.
*    ls_accountgl-gl_account = gs_data-pce.
*    ls_accountgl-orderid = gs_data-internal_ord.
    CONCATENATE gs_data-name_payee gs_data-reference INTO
               ls_accountgl-item_text SEPARATED BY space.
    APPEND ls_accountgl TO lt_accountgl.
    "--------Currency Amount
    "Credit Entry
    ls_currencyamount-itemno_acc = '0000000001'.
    ls_currencyamount-currency = 'CAD'.
    ls_currencyamount-amt_doccur = gs_data-rebate_amt * -1.
    ls_currencyamount-amt_base = gs_data-rebate_amt * -1 .
    ls_currencyamount-disc_base = gs_data-rebate_amt * -1.
    APPEND ls_currencyamount TO lt_currencyamount.
    "Debit Entry
    ls_currencyamount-itemno_acc = '0000000002'.
    ls_currencyamount-currency = 'CAD'.
    ls_currencyamount-amt_doccur = gs_data-rebate_amt.
    ls_currencyamount-amt_base = gs_data-rebate_amt.
    ls_currencyamount-disc_base = gs_data-rebate_amt.
    APPEND ls_currencyamount TO lt_currencyamount.

    CALL FUNCTION 'BAPI_ACC_INVOICE_RECEIPT_POST'
      EXPORTING
        documentheader       = ls_documentheader
        customercpd          = ls_customercpd
* IMPORTING
*   OBJ_TYPE             =
*   OBJ_KEY              =
*   OBJ_SYS              =
      TABLES
        accountpayable       = lt_accountpayable
        accountgl            = lt_accountgl
        accounttax           = lt_accounttax
        currencyamount       = lt_currencyamount
*   PURCHASEORDER        =
*   PURCHASEAMOUNT       =
        return               = gt_bapiret2
*   CRITERIA             =
*   VALUEFIELD           =
*   EXTENSION1           =
              .
    CLEAR: lv_check,
           lv_error.
    LOOP AT gt_bapiret2 INTO gs_bapiret2.
      CLEAR: gs_bapi_msg.
      IF gs_bapiret2-type = 'E' OR
         gs_bapiret2-type = 'A'.
        lv_error = 'X'.
        IF lv_check IS INITIAL.
          CONCATENATE '***' gs_data-name_payee
                      gs_data-reference
                      gs_data-postal_code
                      gs_data-city
                      gs_data-pce
                      gs_data-internal_ord INTO gs_bapi_msg-msg_text
                      SEPARATED BY space.
          APPEND gs_bapi_msg TO gt_bapi_msg.
          lv_check = 'X'.
        ENDIF.
        gs_bapi_msg-msgrp = gs_bapiret2-type.
        gs_bapi_msg-msgid = gs_bapiret2-id.
        gs_bapi_msg-msgty = gs_bapiret2-type.
        gs_bapi_msg-msgno = gs_bapiret2-number.
        gs_bapi_msg-msg_text = gs_bapiret2-message .
        APPEND gs_bapi_msg TO gt_bapi_msg.
      ELSE.
        IF gs_bapiret2-type = 'W'.
          CONTINUE.
        ELSE.
          gs_bapi_msg-msgrp = gs_bapiret2-type.
          gs_bapi_msg-msgid = gs_bapiret2-id.
          gs_bapi_msg-msgty = gs_bapiret2-type.
          gs_bapi_msg-msgno = gs_bapiret2-number.
          gs_bapi_msg-msg_text = gs_bapiret2-message .
          APPEND gs_bapi_msg TO gt_bapi_no_error_msg.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lv_error IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*         EXPORTING
*           WAIT          =
*         IMPORTING
*           RETURN        =
                .
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*         IMPORTING
*           RETURN        =
                .

    ENDIF.
  ENDLOOP.
ENDFORM.                    " POST_DATA
*&---------------------------------------------------------------------*
*&      Form  EMAIL_BAPI_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM email_bapi_error .
  DATA: lt_objtxt    TYPE TABLE OF solisti1,
            lt_objpack   TYPE TABLE OF sopcklsti1,
            lt_reclist   TYPE TABLE OF somlreci1,
            lt_objhead   TYPE soli_tab.

  DATA: lv_lines     TYPE i,
        lv_string    TYPE string,
        lwa_objpack  TYPE sopcklsti1,
        lwa_objtxt   TYPE solisti1,
        lwa_doc_chng TYPE sodocchgi1,
        lwa_reclist  TYPE somlreci1,
        lv_space     TYPE char01  VALUE ' '.


  CONSTANTS: lc_f(1)      TYPE c VALUE 'F',
             lc_u(1)      TYPE c VALUE 'U',
             lc_int(3)    TYPE c VALUE 'INT',
             lc_htm(3)    TYPE c VALUE 'HTM',
             lc_hyphen(1) TYPE c VALUE '-',
             lc_log(3)    TYPE c VALUE 'LOG',
             lc_x(1)      TYPE c VALUE 'X'.

* Prepare Email Content
  PERFORM f_build_mail_content CHANGING lt_objtxt.

* Object with main text of the mail.
  lwa_objtxt = lv_space.
  APPEND lwa_objtxt TO lt_objtxt.
  CLEAR lwa_objtxt.

  DESCRIBE TABLE lt_objtxt LINES lv_lines.

  CONCATENATE text-005
              lc_hyphen
              lc_log
              INTO lv_string
              SEPARATED BY space.

  lwa_doc_chng-obj_descr  = lv_string.
  lwa_doc_chng-sensitivty = lc_f.
  lwa_doc_chng-doc_size   = lv_lines * 255.

* Pack to main body.
  lwa_objpack-head_start = 1.
  lwa_objpack-head_num   = 0.
  lwa_objpack-body_start = 1.
  lwa_objpack-body_num   = lv_lines.
  lwa_objpack-doc_type   = lc_htm.
  APPEND lwa_objpack TO lt_objpack.
  CLEAR lwa_objpack.

  lwa_reclist-copy = lc_x.

* Map Email ID(s)
  lwa_reclist-receiver   = p_errml.
  lwa_reclist-rec_type   = lc_u.
  lwa_reclist-com_type   = lc_int.
  lwa_reclist-notif_del  = lc_x.
  lwa_reclist-notif_ndel = lc_x.
  lwa_reclist-copy       = space.
  APPEND lwa_reclist TO lt_reclist.

* Function module for sending email.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = lwa_doc_chng
      put_in_outbox              = lc_x
      commit_work                = lc_x
    TABLES
      packing_list               = lt_objpack
      object_header              = lt_objhead
      contents_txt               = lt_objtxt
      receivers                  = lt_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc <> 0.
    MESSAGE i000(zfi01) WITH text-006.
  ELSE.

**Call program to push mail from SAP mail outbox
*    SUBMIT rsconn01 WITH mode = lc_int
*                    WITH output = space
*                    AND RETURN.
    WRITE: /'Error log is emailed to ', p_errml.

  ENDIF.
ENDFORM.                    " EMAIL_BAPI_ERROR
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_OBJTXT  text
*----------------------------------------------------------------------*
FORM f_build_mail_content  CHANGING p_lt_objtxt TYPE table.

  DATA: lwa_objtxt       TYPE solisti1.

*Prepare HTML mail header
  CONCATENATE '<html>'
              '<body>'
              '<h4 style="font-family:arial"><caption><b><u>'
              text-007
              '</u></b><caption></h4>'
              '<ul>'
              '</ul>'
         INTO lwa_objtxt
         SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE text-014 '</td><td>'
              text-012 '</td><td>'
              text-015 '</td><td>'
              text-010 '</td></tr>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  LOOP AT gt_bapi_msg INTO gs_bapi_msg.

    CONCATENATE  '<tr> <td>'
                gs_bapi_msg-msgid '</td><td>'
                gs_bapi_msg-msgty '</td><td>'
                gs_bapi_msg-msgno '</td><td>'
                gs_bapi_msg-msg_text '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.

    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.


  ENDLOOP.

  MOVE: '</table> <br>' TO lwa_objtxt.
  APPEND: lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
*End of HTML text here
  CONCATENATE '</body>'
              '</html>'
         INTO lwa_objtxt
         SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
ENDFORM.                    " F_BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*&      Form  PRINT_ON_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_on_screen .

*  WRITE: / 'Email is sent to ', p_errml.
  SKIP 2.
  LOOP AT gt_bapi_msg INTO gs_bapi_msg.

    WRITE: /  gs_bapi_msg-msgid,
              gs_bapi_msg-msgty,
              gs_bapi_msg-msgno,
              gs_bapi_msg-msg_text.

  ENDLOOP.
  WRITE: / '********** End of Messages **********'.

ENDFORM.                    " PRINT_ON_SCREEN
*&---------------------------------------------------------------------*
*&      Form  PRINT_ON_SCREEN_NO_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_on_screen_no_error .

  WRITE: / 'No error email is generated.'.
  SKIP 2.
  LOOP AT gt_bapi_no_error_msg INTO gs_bapi_msg.

    WRITE: /  gs_bapi_msg-msgid,
              gs_bapi_msg-msgty,
              gs_bapi_msg-msgno,
              gs_bapi_msg-msg_text.

  ENDLOOP.
  WRITE: / '********** End of Messages **********'.
ENDFORM.                    " PRINT_ON_SCREEN_NO_ERROR
