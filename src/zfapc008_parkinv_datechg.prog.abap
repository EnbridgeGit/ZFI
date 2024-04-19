REPORT zfapc008_parkinv_datechg MESSAGE-ID f4 LINE-SIZE 150
       LINE-COUNT 65 NO STANDARD PAGE HEADING.
************************************************************************
* Author      : Glenn Ymana                                            *
* Date Created: March 29, 2012                                         *
*----------------------------------------------------------------------*
* Description :                                                        *
* This program was copied from a US program ZPSAP2_B. It will select   *
* one or many parked documents created by transaction FV60 * FBV1 and  *
* change the posting date on VBKPF.                                    *
* This was done because parked invoices get created but not approved   *
* before month end, the FI period will close and the parked doc will   *
* error when the next user tries to change, complete, or approve it.   *
************************************************************************
* Date        Developer      Request #  Description                    *
************************************************************************
* 2012/03/22  MNagarathina   DECK905295 Change code not to update/     *
* Program ZPSAP2_B Change               clear the 'Document complete'  *
*                                       flag when the date is updated  *
* 2012/04/02  GYMANA                    Re-written program to process  *
*                                       in batch mode                  *
* 2012/06/12  BTBOUNDY                  Added T-Code to Selection      *
*                                      Screen, removed hard coded from *
*                                       SQL select                     *
* 2012/06/03  SAHMAD                   MIRO Invoice Posting Date Change
* 2012/07/20  SAHMAD                   Email notification about errors
*                                      and processed items
* 2012/08/14  SAHMAD                   BAPI is used for MIRO invoices
*----------------------------------------------------------------------*

TABLES:  vbkpf,
         t020.
DATA:    no_output    TYPE c,
         w_mandt(5)    TYPE c,
         ln_cntr       TYPE i VALUE 99.
DATA:   BEGIN OF tbkpf OCCURS 5.
        INCLUDE STRUCTURE vbkpf.
DATA:   END   OF tbkpf.
DATA:   BEGIN OF it020 OCCURS 0.
        INCLUDE STRUCTURE t020.
DATA:   END   OF it020.
DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
DATA: BEGIN OF messtab OCCURS 10.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF messtab.
DATA: gv_msg TYPE t100-text.
TYPES: BEGIN OF ty_email_data,
       filename      TYPE sdokpath-pathname,
       log           TYPE string,
       END OF ty_email_data.
DATA: ge_mail   TYPE string,
      ge_name   TYPE string,
      ge_domain TYPE string,
      gv_error,
      gv_conf,
      GV_counter type i.

SELECTION-SCREEN SKIP 2.
SELECT-OPTIONS:
         bukrs     FOR  vbkpf-bukrs MEMORY ID buk,
         belnr     FOR  vbkpf-belnr,
         gjahr     FOR  vbkpf-gjahr MEMORY ID gjr,
         budat     FOR  vbkpf-budat,
         bldat     FOR  vbkpf-bldat,
         blart     FOR  vbkpf-blart,
         xblnr     FOR  vbkpf-xblnr,
         bktxt     FOR  vbkpf-bktxt,
         usnam     FOR  vbkpf-usnam DEFAULT sy-uname,
         tcode     FOR  vbkpf-tcode,
         xwffr     FOR  vbkpf-xwffr,
         xprfg     FOR  vbkpf-xprfg,
         xfrge     FOR  vbkpf-xfrge.

SELECTION-SCREEN SKIP.
PARAMETERS: p_email       TYPE string DEFAULT
                          'dbossy@spectraenergy.com' OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETER: pdate    LIKE bkpf-budat DEFAULT sy-datlo.
PARAMETER: p_testrn AS CHECKBOX DEFAULT 'X'.   "Test Run Only

AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON p_email.
  ge_mail = p_email.
  FIND REGEX '^([a-z](?:\w|-|\.)*)@((\w|-|(\.?!\.)))+\.[[:alpha:]]{2,4}$'
  IN ge_mail
  IGNORING CASE
  SUBMATCHES ge_name ge_domain.
  IF sy-subrc <> 0.
    WRITE: / 'Enter correct email address for download'.
    STOP.
  ENDIF.


START-OF-SELECTION.

  SELECT * INTO tbkpf FROM vbkpf
          WHERE bukrs IN bukrs
            AND   ausbk IN bukrs
            AND   belnr IN belnr
            AND   gjahr IN gjahr
            AND   budat IN budat
            AND   bldat IN bldat
            AND   blart IN blart
            AND   bktxt IN bktxt
            AND   xblnr IN xblnr
            AND   usnam IN usnam
            AND   bstat EQ 'V'
            AND   tcode IN tcode
            AND   xwffr IN xwffr
            AND   xfrge IN xfrge
            AND   xprfg IN xprfg.
    CHECK tbkpf-bukrs EQ tbkpf-ausbk.
    APPEND tbkpf.
  ENDSELECT.
  IF sy-subrc <> 0.
    PERFORM generate_detail_rpt_header.
    WRITE: 20 '*** No data was selected. ',
           46 'Please verify input parameters ***'.
    EXIT.
  ENDIF.
  PERFORM fbv4_bdc_miro_bapi.
  CHECK  p_testrn IS INITIAL.
  IF p_email IS NOT INITIAL.
    CLEAR: gv_error,
           gv_conf,
           gv_counter.
    PERFORM send_error_email.
    clear: gv_counter.
    PERFORM send_processed_email.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  fbv4_bdc_Miro_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fbv4_bdc_miro_bapi.
  REFRESH messtab.

  SELECT * FROM t020
        INTO TABLE it020
        ORDER BY PRIMARY KEY.
  LOOP AT tbkpf.
    CLEAR: gv_msg.
    IF p_testrn = ' '.
*MIRO/MIR7 are Sap Enjoy transaction.
*They can't use for batch input using BDC
*That is why using BAPI
      IF tbkpf-tcode = 'MIRO' OR
         tbkpf-tcode = 'MIR7' OR
         tbkpf-tcode = 'MIR4'.
        PERFORM miro_bapi_call.
      ELSE.
        PERFORM fbv4_bdc1.
      ENDIF.
      PERFORM write_detail_report.
    ELSE.
      MOVE 'Test Run Only (No Update)' TO messtab.
      PERFORM write_detail_report.
    ENDIF.
  ENDLOOP.
ENDFORM.                                            "fbv4_bdc_Miro_bapi
*&---------------------------------------------------------------------*
*&      Form  FBV4_BDC1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_GJAHR    text
*      -->I_BELNR    text
*      -->I_BUKRS    text
*      -->I_TCODE    text
*      -->I_PDATE    text
*----------------------------------------------------------------------*
FORM fbv4_bdc1.

  DATA: xdate(10) TYPE c.
  REFRESH bdcdata.
  PERFORM bdc_dynpro      USING 'SAPMF05V' '0100'.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05V-GJAHR'.
  PERFORM bdc_field       USING 'RF05V-BUKRS'
                                tbkpf-bukrs.
  PERFORM bdc_field       USING 'RF05V-BELNR'
                                tbkpf-belnr.
  PERFORM bdc_field       USING 'RF05V-GJAHR'
                                tbkpf-gjahr.

  WRITE pdate TO xdate(000010)
           DD/MM/YYYY.
  CLEAR it020.
  READ TABLE it020 WITH KEY tcode = tbkpf-tcode BINARY SEARCH.


  IF it020-gener <> '1'.

    PERFORM bdc_dynpro      USING 'SAPLF040' '0600'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BKPF-BUDAT'.
    PERFORM bdc_field       USING 'BKPF-BUDAT'
                                  xdate.

  ELSE.
    IF     it020-koart = 'D'.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '1200'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                               'INVFO-BUDAT'.
      PERFORM bdc_field       USING 'INVFO-BUDAT'
                               xdate.
    ELSEIF it020-koart = 'K'.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '1100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                               'INVFO-BUDAT'.
      PERFORM bdc_field       USING 'INVFO-BUDAT'
                               xdate.
    ELSEIF it020-koart = 'S'.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '1001'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                               'ACGL_HEAD-BUDAT'.
      PERFORM bdc_field       USING 'ACGL_HEAD-BUDAT'
                               xdate.
    ELSEIF it020-koart = space AND
           ( tbkpf-tcode = 'MIRO' OR
             tbkpf-tcode = 'MIR7' OR
             tbkpf-tcode = 'MIR4' ).
*      PERFORM bdc_dynpro      USING 'SAPLMR1M' '6000'.
*      PERFORM bdc_field       USING 'BDC_OKCODE'
*                                    '/EPPCH'.
*      PERFORM bdc_dynpro      USING 'SAPLMR1M' '6000'.
*      PERFORM bdc_field       USING 'BDC_CURSOR'
*                                    'INVFO-BUDAT'.
*      PERFORM bdc_field       USING 'INVFO-BUDAT'
*                                     xdate.
    ENDIF.
  ENDIF.

  CLEAR:sy-subrc.                                           "DECK905295

  IF NOT tbkpf-xprfg IS INITIAL AND                         "DECK905295
         tbkpf-tcode = 'FV60'.
    PERFORM bdc_field       USING 'BDC_OKCODE'              "DECK905295
                                  '=PBBP'.                  "DECK905295
  ELSEIF tbkpf-tcode = 'MIRO' OR
         tbkpf-tcode = 'MIR7' OR
         tbkpf-tcode = 'MIR4'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=PARK'.
  ELSE.                                                     "DECK905295
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  'BP'.
  ENDIF.                                                    "DECK905295

  CALL TRANSACTION 'FBV4' USING bdcdata
                   MODE   'N'
                   UPDATE 'S'.
*                       MESSAGES INTO lt_MESSTAB.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = sy-msgid
      msgnr               = sy-msgno
      msgv1               = sy-msgv1
      msgv2               = sy-msgv2
      msgv3               = sy-msgv3
      msgv4               = sy-msgv4
    IMPORTING
      message_text_output = gv_msg "messtab
    EXCEPTIONS
      OTHERS              = 4.

  FIND tbkpf-belnr IN gv_msg IGNORING CASE.
  IF sy-subrc <> 0.
    CONCATENATE tbkpf-belnr tbkpf-gjahr tbkpf-bukrs
                gv_msg INTO gv_msg SEPARATED BY space.
  ENDIF.
  messtab-msgv1 = gv_msg.
  messtab-msgtyp = sy-msgty.
  APPEND messtab.

ENDFORM.                                                    "fbv4_bdc1
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  generate_detail_rpt_header
*&---------------------------------------------------------------------*

FORM generate_detail_rpt_header.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 40 text-002.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /1 sy-sysid, 5 w_mandt, 39 text-003.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.

  WRITE: /3    text-004,                 "Document
          15   text-005,                 "Fiscal
          22   text-006,                 "Comp.
          30   text-007,                 "Doc.
          36   text-008,                 "Posting
          71   text-011,                 "Trans
          77   text-012,                 "Release
          86   text-013.                 "Doc.
  WRITE: /     text-04a UNDER text-004,  "Number
               text-05a UNDER text-005,  "Year
               text-06a UNDER text-006,  "Code
               text-07a UNDER text-007,  "Type
               text-08a UNDER text-008,  "Date
          44   text-009,                 "Currency
          53   text-010,                 "Reference
               text-11a UNDER text-011,  "Code
               text-12a UNDER text-012,  "Necc
               text-13a UNDER text-013,  "Compl
          92   text-014,                 "Released
          103   text-015.                "Status Message
  SKIP 2.
  MOVE '5' TO ln_cntr.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "generate_detail_rpt_header

*&---------------------------------------------------------------------*
*&      Form  write_detail_report
*&---------------------------------------------------------------------*
FORM write_detail_report.

  IF ln_cntr >= 55.
    PERFORM generate_detail_rpt_header.
  ENDIF.

  WRITE tbkpf-belnr UNDER text-04a.
  WRITE tbkpf-gjahr UNDER text-05a.
  WRITE tbkpf-bukrs UNDER text-06a.
  WRITE tbkpf-blart UNDER text-07a.
  WRITE tbkpf-budat UNDER text-08a DD/MM/YY.
  WRITE tbkpf-waers UNDER text-009.
  WRITE tbkpf-xblnr UNDER text-010.
  WRITE tbkpf-tcode UNDER text-11a.
  WRITE tbkpf-xwffr UNDER text-12a.
  WRITE tbkpf-xprfg UNDER text-13a.
  WRITE tbkpf-xfrge UNDER text-014.
*  WRITE messtab UNDER text-015.
  WRITE gv_msg UNDER text-015.
  SKIP.
  ADD +2 TO ln_cntr.

ENDFORM.                    "write_detail_report
*&---------------------------------------------------------------------*
*&      Form  SEND_ERROR_EMAIL
*&---------------------------------------------------------------------*
*       Send Error Log
*----------------------------------------------------------------------*
FORM send_error_email .
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
  PERFORM f_build_mail_content USING 'E'
                               CHANGING lt_objtxt.
  CHECK gv_error = 'X'.
  check gv_counter > 0.
* Object with main text of the mail.
  lwa_objtxt = lv_space.
  APPEND lwa_objtxt TO lt_objtxt.
  CLEAR lwa_objtxt.

  DESCRIBE TABLE lt_objtxt LINES lv_lines.

  CONCATENATE text-016
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
  lwa_reclist-receiver   = p_email.
  lwa_reclist-rec_type   = lc_u.
  lwa_reclist-com_type   = lc_int.
  lwa_reclist-notif_del  = lc_x.
  lwa_reclist-notif_ndel = lc_x.
  lwa_reclist-copy       = space.
  APPEND lwa_reclist TO lt_reclist.

* Funcion module for sending email.
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
    WRITE: /'Error log is emailed.'.

  ENDIF.
ENDFORM.                    " SEND_ERROR_EMAIL
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_build_mail_content  USING p_flag TYPE c
                           CHANGING p_lt_objtxt TYPE table.

  DATA: lwa_objtxt       TYPE solisti1,
        lwa_email_data   TYPE ty_email_data,
        lv_msg(400) TYPE c,
        lv_header(40) TYPE c.

*Prepare HTML mail header
  IF p_flag = 'E'.
    lv_header = 'Error Log'.
     gv_error = 'X'.
  ELSE.
    lv_header = 'Changed Documents Log'.
    gv_conf = 'X'.
  ENDIF.
  CONCATENATE '<html>'
              '<body>'
              '<h4 style="font-family:arial"><caption><b><u>'
              lv_header
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

  CONCATENATE text-017 '</td><td>'
              text-018 '</td></tr>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  LOOP AT messtab.
    IF p_flag = 'E'.
      CHECK messtab-msgtyp = 'E' OR
            messtab-msgtyp = 'A'.
    ELSE.
      IF messtab-msgtyp = 'E' OR
         messtab-msgtyp = 'A'.
        CONTINUE.
      ENDIF.
    ENDIF.
    gv_counter = gv_counter + 1.
*    concatenate messtab-msgv1 messtab-msgv2
*                messtab-msgv3 messtab-msgv4
*                into lv_msg separated by ','.
    lv_msg = messtab-msgv1.
    CONCATENATE  '<tr> <td>'
                messtab-msgtyp '</td><td>'
                lv_msg '</td></tr>'
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
*&      Form  SEND_PROCESSED_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_processed_email .
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
  PERFORM f_build_mail_content USING 'P'
                               CHANGING lt_objtxt.
  CHECK gv_conf = 'X'.
  check gv_counter > 0.
* Object with main text of the mail.
  lwa_objtxt = lv_space.
  APPEND lwa_objtxt TO lt_objtxt.
  CLEAR lwa_objtxt.

  DESCRIBE TABLE lt_objtxt LINES lv_lines.

  CONCATENATE text-019
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
  lwa_reclist-receiver   = p_email.
  lwa_reclist-rec_type   = lc_u.
  lwa_reclist-com_type   = lc_int.
  lwa_reclist-notif_del  = lc_x.
  lwa_reclist-notif_ndel = lc_x.
  lwa_reclist-copy       = space.
  APPEND lwa_reclist TO lt_reclist.

* Funcion module for sending email.
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
    WRITE: /'Changed documents log is emailed.'.

  ENDIF.
ENDFORM.                    " SEND_PROCESSED_EMAIL
*&---------------------------------------------------------------------*
*&      Form  MIRO_BAPI_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM miro_bapi_call .

  DATA: lv_belnr   TYPE bapi_incinv_fld-inv_doc_no,
        lv_gjahr   TYPE bapi_incinv_fld-fisc_year,
        ls_header  TYPE bapi_incinv_chng_header,
        ls_headerx TYPE bapi_incinv_chng_headerx,
        lt_return TYPE TABLE OF bapiret2,
        ls_return TYPE bapiret2,
        lv_check,
        lv_cdate type c LENGTH 10,
        lv_pdate type c LENGTH 10.

*VBKPF-AWKEY(10) = MM (MIRO) document and VBKPF-AWKEY+10(4) is year
  MOVE: tbkpf-awkey(10)   TO lv_belnr,
        tbkpf-awkey+10(4) TO lv_gjahr.

  ls_header-pstng_date = pdate.
  ls_headerx-pstng_date = 'X'.

  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CHANGE'
    EXPORTING
      invoicedocnumber           = lv_belnr
      fiscalyear                 = lv_gjahr
*   INVOICE_DOC_STATUS         = ' '
*   TABLE_CHANGE               =
     headerdata_change          = ls_header
     headerdata_changex         = ls_headerx
*   ADRESSDATA_CHANGE          =
*   ADRESSDATA_CHANGEX         =
* IMPORTING
*   INVOICEDOCNUMBER_NEW       =
*   FISCALYEAR_NEW             =
    TABLES
*   ITEMDATA                   =
*   ACCOUNTINGDATA             =
*   GLACCOUNTDATA              =
*   MATERIALDATA               =
*   TAXDATA                    =
*   WITHTAXDATA                =
*   VENDORITEMSPLITDATA        =
      return                     = lt_return.
  LOOP AT lt_return INTO ls_return.
    IF ls_return-type = 'A' OR
       ls_return-type = 'E'.
       lv_check = 'X'.
       messtab-msgv1  = ls_return-message.
       messtab-msgtyp = ls_return-type.
       FIND tbkpf-belnr IN messtab-msgv1 IGNORING CASE.
       IF sy-subrc <> 0.
          CONCATENATE tbkpf-belnr tbkpf-gjahr tbkpf-bukrs
                      messtab-msgv1 INTO messtab-msgv1 SEPARATED BY space.
       ENDIF.
       APPEND messtab.
       gv_msg = ls_return-message.
    ENDIF.
  ENDLOOP.
  CONCATENATE tbkpf-budat(4) '/' tbkpf-budat+4(2)
                '/' tbkpf-budat+6(2) into lv_cdate.
  CONCATENATE pdate(4) '/' pdate+4(2)
                '/' pdate+6(2) into lv_pdate.
  IF lv_check IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*     IMPORTING
*       RETURN        = .
    CONCATENATE tbkpf-belnr tbkpf-gjahr tbkpf-bukrs
                lv_cdate 'Now ' lv_pdate INTO messtab-msgv1
                SEPARATED BY space.
    messtab-msgtyp = 'I'.
    gv_msg = messtab-msgv1.
    APPEND messtab.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    CONCATENATE 'ERROR ' tbkpf-belnr tbkpf-gjahr tbkpf-bukrs
                'Unable to change date ' INTO messtab-msgv1
                                         SEPARATED BY space.
    messtab-msgtyp = 'E'.
    gv_msg = messtab-msgv1.
    APPEND messtab.
  ENDIF.

ENDFORM.                    " MIRO_BAPI_CALL
