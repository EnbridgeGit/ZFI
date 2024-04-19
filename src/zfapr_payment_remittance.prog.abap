*&---------------------------------------------------------------------*
*& Report  ZFAPR_PAYMENT_REMITTANCE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFAPR_PAYMENT_REMITTANCE.


TABLES: regup.
TYPES: BEGIN OF ty_no_email,
       instance(10) TYPE c,
       bukrs TYPE bukrs,
       lifnr TYPE lfa1-lifnr,
       name1 TYPE lfa1-name1,
       phone TYPE ad_tlnmbr1,
       text(100),
       END OF ty_no_email.
TYPES: BEGIN OF ty_mail,
        emrec TYPE swotobjid,    " Email recipient ID
        adrnr TYPE adrnr,        " Address number
       END OF ty_mail.
TYPES: BEGIN OF ty_email,
       email TYPE adr6-smtp_addr,
       END OF ty_email.
TYPES: BEGIN OF ty_fax,
       fax_num TYPE adrc-fax_number,
       END OF ty_fax.
TYPES: BEGIN OF ty_email_data,
       filename      TYPE sdokpath-pathname,      " Filepath with Name
       log           TYPE string,                 " Error Log
       END OF ty_email_data.


DATA: gt_no_email TYPE TABLE OF ty_no_email,
      gs_no_email LIKE LINE OF gt_no_email,
      gt_yes_email TYPE TABLE OF ty_no_email,
      gs_yes_email LIKE LINE OF gt_yes_email,
      gt_email TYPE TABLE OF ty_email,
      gs_email LIKE LINE OF gt_email,
      gt_fax   TYPE TABLE OF ty_fax,
      gs_fax LIKE LINE OF gt_fax.
DATA: gt_regup TYPE TABLE OF regup,
      gs_regup LIKE LINE OF gt_regup,
      gs_adrc  TYPE adrc,
      gs_lfa1 TYPE lfa1,
      gt_lfa1 TYPE TABLE OF lfa1,
      gs_alt_payee TYPE lfa1,
      gv_alt_payee TYPE lfa1-lifnr,
      gv_adrnr TYPE t001-adrnr,
      gv_waers TYPE waers,
      gv_vblnr TYPE vblnr.

DATA: gv_email TYPE swotobjid,
      gv_fm_name TYPE rs38l_fnam,
      gv_mail TYPE ty_mail.
DATA: ge_mail   TYPE string,
      ge_name   TYPE string,
      ge_domain TYPE string.
*Adobeform
DATA: gv_fpname           TYPE FPNAME,
      gv_FUNCNAME         type FUNCNAME,
      gv_fPINTERFACETYPE  TYPE fPINTERFACETYPE,
      gv_fUNCNAME_inbound type fUNCNAME,
      gs_docparams        type sfpdocparams,
      gs_outputparams     type sfpoutputparams.

CONSTANTS: c_abtnr TYPE knvk-abtnr VALUE '0009', "financial deparment
           c_pafkt TYPE knvk-pafkt VALUE '13'. "function

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_zbukr TYPE regup-zbukr OBLIGATORY,
            p_laufd TYPE regup-laufd OBLIGATORY.
SELECT-OPTIONS: s_lifnr FOR regup-lifnr,
                s_zlsch FOR regup-zlsch,
                s_hbkid FOR regup-hbkid.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_downl RADIOBUTTON GROUP grp1 DEFAULT 'X' USER-COMMAND cmd,
            p_email RADIOBUTTON GROUP grp1.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
PARAMETERS: p_downe  TYPE string MODIF ID dn1.
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_errml       TYPE string DEFAULT
                          'APCAEastinquiry@spectraenergy.com' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.

*AT SELECTION-SCREEN ON BLOCK b2.
AT SELECTION-SCREEN OUTPUT.
  PERFORM disable_enable_fields.

AT SELECTION-SCREEN ON p_errml.

  ge_mail = p_errml.
  FIND REGEX '^([a-z](?:\w|-|\.)*)@((\w|-|(\.?!\.)))+\.[[:alpha:]]{2,4}$'
  IN ge_mail
  IGNORING CASE
  SUBMATCHES ge_name ge_domain.

  IF sy-subrc <> 0.
    MESSAGE e000(zfi01) WITH
     'Enter correct email address for errors' '' '' ''.
    STOP.
  ENDIF.
*  IF p_errml IS INITIAL OR
*     p_errml NA '@'.
*    MESSAGE e000(zfi01) WITH 'Enter correct email address for errors' '' '' ''.
*    STOP.
*  ENDIF.

*AT SELECTION-SCREEN ON p_downe.
*  IF p_downl = 'X'.
*    ge_mail = p_downe.
*    FIND REGEX '^([a-z](?:\w|-|\.)*)@((\w|-|(\.?!\.)))+\.[[:alpha:]]{2,4}$'
*    IN ge_mail
*    IGNORING CASE
*    SUBMATCHES ge_name ge_domain.
*    IF sy-subrc <> 0.
*      MESSAGE e000(zfi01) WITH
*       'Enter correct email address for download' '' '' ''.
*      STOP.
*    ENDIF.
*    IF p_downe IS INITIAL. " OR
**       p_downe NA '@'.
*      MESSAGE e000(zfi01) WITH 'Enter correct email address for download' '' '' ''.
*      STOP.
*    ENDIF.
*  ENDIF.

START-OF-SELECTION.

  IF p_downl = 'X'.
    ge_mail = p_downe.
    FIND REGEX '^([a-z](?:\w|-|\.)*)@((\w|-|(\.?!\.)))+\.[[:alpha:]]{2,4}$'
    IN ge_mail
    IGNORING CASE
    SUBMATCHES ge_name ge_domain.
    IF sy-subrc <> 0.
      MESSAGE e000(zfi01) WITH
       'Enter correct email address for download' '' '' ''.
      STOP.
    ENDIF.
    IF p_downe IS INITIAL. " OR
*       p_downe NA '@'.
      MESSAGE e000(zfi01) WITH 'Enter correct email address for download' '' '' ''.
      STOP.
    ENDIF.
  ENDIF.
  IF p_email IS NOT INITIAL.
    CLEAR p_downe.
  ENDIF.

  CLEAR: gt_regup,
         gt_lfa1,
         gt_no_email,
         gs_no_email,
         gt_yes_email,
         gs_yes_email,
         gt_email,
         gs_email,
         gs_regup,
         gs_adrc,
         gs_lfa1,
         gt_lfa1,
         gv_adrnr,
         gv_waers,
         gv_vblnr,
         gv_email,
         gv_fm_name,
         gv_mail.

  SET COUNTRY 'CA'. "'US'.

  PERFORM get_data.
  PERFORM get_fm_name.
  PERFORM process_data_payee.
  PERFORM process_data_vendor.
  PERFORM email_log.

  SET COUNTRY space.
*No need to release email, in prod it is scheduled
*to release every 30 minutes.
*  PERFORM release_emails.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_reguh TYPE TABLE OF reguh.

  SELECT SINGLE adrnr INTO gv_adrnr FROM t001 WHERE bukrs = p_zbukr.
  IF sy-subrc <> 0.
    WRITE : / 'Enter correct company code...'.
    STOP.
  ENDIF.
  SELECT SINGLE * FROM adrc INTO gs_adrc WHERE addrnumber = gv_adrnr.
  IF sy-subrc <> 0.
    WRITE : / 'Company Code address data is not maintained...'.
    STOP.
  ENDIF.
*  SELECT * FROM regup INTO TABLE gt_regup
*        WHERE zbukr = p_zbukr
*          AND laufd = p_laufd
*          AND lifnr IN s_lifnr
*          AND zlsch IN s_zlsch
*          AND hbkid IN s_hbkid
*          AND xvorl = space.  "dont pull proposal run
*  IF gt_regup IS INITIAL.
*    WRITE: / 'No payment data to process'.
*    EXIT.
*  ENDIF.
  SELECT * FROM reguh INTO TABLE lt_reguh
          WHERE zbukr = p_zbukr
            AND laufd = p_laufd
            AND lifnr IN s_lifnr
            AND rzawe IN s_zlsch
            AND hbkid IN s_hbkid
            AND xvorl = space.
  IF lt_reguh IS INITIAL.
    WRITE: / 'No payment data to process'.
    STOP.
  ENDIF.
  SELECT * FROM regup INTO TABLE gt_regup
    FOR ALL ENTRIES IN lt_reguh
    WHERE laufd = lt_reguh-laufd
      AND laufi = lt_reguh-laufi
      AND xvorl = space
      AND zbukr = lt_reguh-zbukr
      AND lifnr = lt_reguh-lifnr.
  IF gt_regup IS INITIAL.
    WRITE: / 'No payment Line items data to process'.
    STOP.
  ENDIF.
  SELECT * FROM lfa1 INTO TABLE gt_lfa1
        FOR ALL ENTRIES IN gt_regup
        WHERE lifnr = gt_regup-lifnr.
  IF gt_lfa1 IS INITIAL.
    WRITE: 'No Vendor information...'.
    EXIT.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA_PAYEE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM process_data_payee .

  DATA: lt_regup TYPE TABLE OF regup,
        lv_email TYPE adr6-smtp_addr,
        lv_prsnr TYPE knvk-prsnr.


  DATA: lt_regup_header TYPE TABLE OF regup,
        ls_regup_header LIKE LINE OF lt_regup_header,
        lt_knvk TYPE TABLE OF knvk,
        ls_knvk LIKE LINE OF lt_knvk,
        ls_adrc TYPE adrc.
 data:  lv_faxuser(25) TYPE c VALUE 'SAPEEFTFAX',
        lv_sender(25) TYPE c VALUE 'ACCOUNTS PAYABLE'.

 data: ls_FPFORMOUTPUT type FPFORMOUTPUT,
       LV_Medium type FPMEDIUM,
       lv_EMAIL_SUBJECT Type SO_OBJ_DES,
       LT_EMAIL_TEXT  type BCSY_TEXT,
       LS_EMAIL_TEXT  type LINE OF BCSY_TEXT,
       LV_LANGUAGE TYPE SYLANGU,
       LS_ADDRESS type cl_wzre_service_nast_print=>ty_mail_fax_addr,
       lv_event type BOOLE_D.

  lt_regup_header[] = gt_regup[].
  DELETE ADJACENT DUPLICATES FROM lt_regup_header COMPARING lifnr empfg vblnr waers.
  SORT lt_regup_header BY lifnr empfg vblnr waers.
  SORT gt_regup BY lifnr empfg vblnr waers.

  gs_outputparams-nodialog = 'X'. "No printer dialog
  gs_outputparams-GETPDF = 'X'. "launch print preview

CALL FUNCTION 'FP_JOB_OPEN'
  CHANGING
    ie_outputparams       = gs_outputparams
 EXCEPTIONS
   CANCEL                = 1
   USAGE_ERROR           = 2
   SYSTEM_ERROR          = 3
   INTERNAL_ERROR        = 4
   OTHERS                = 5 .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.


  LOOP AT lt_regup_header INTO ls_regup_header.
    CLEAR: lt_regup,
           lv_prsnr,
           lv_email,
           gs_lfa1,
           gs_email,
           gt_email,
           gs_alt_payee.

    gv_waers = ls_regup_header-waers.
    gv_vblnr = ls_regup_header-vblnr.
    gv_alt_payee = ls_regup_header-empfg+1(10).
    IF gv_alt_payee IS INITIAL.
      CONTINUE.
    ENDIF.
*    if gv_alt_payee is initial.
*      gv_alt_payee = ls_regup_header-lifnr.
*    endif.
    READ TABLE gt_regup WITH KEY lifnr = ls_regup_header-lifnr
                                 empfg = ls_regup_header-empfg
                                 vblnr = ls_regup_header-vblnr
                                 waers = ls_regup_header-waers "gs_lfa1-lifnr
                                 TRANSPORTING NO FIELDS.
    LOOP AT gt_regup INTO gs_regup
                          WHERE lifnr = ls_regup_header-lifnr
                            AND empfg = ls_regup_header-empfg
                            AND vblnr = ls_regup_header-vblnr
                            AND waers = ls_regup_header-waers. "gs_lfa1-lifnr.
      APPEND gs_regup TO lt_regup.
    ENDLOOP.
    CHECK lt_regup IS NOT INITIAL.
*Actual Vendor
    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = ls_regup_header-lifnr.
*Alternate Payee
    SELECT SINGLE * FROM lfa1 INTO gs_alt_payee
        WHERE lifnr = gv_alt_payee.
      CALL FUNCTION gv_FUNCNAME
        EXPORTING
         /1BCDWB/DOCPARAMS        =  gs_docparams
          gt_regup                 = lt_regup
          gi_payee                 = 'P'
          gs_vendor                = gs_lfa1
          gi_sender                = lv_sender
          gi_faxuser               = lv_faxuser
          gi_vblnr                 = gv_vblnr
          gs_ardc                  = gs_adrc
          gs_lfa1                  = gs_alt_payee
          gi_currency              = gv_waers
          gi_date                  = p_laufd
          gi_lifnr                 = gs_alt_payee-lifnr
       IMPORTING
         /1BCDWB/FORMOUTPUT       = ls_FPFORMOUTPUT
       EXCEPTIONS
         USAGE_ERROR              = 1
         SYSTEM_ERROR             = 2
         INTERNAL_ERROR           = 3
         OTHERS                   = 4  .
      IF sy-subrc <> 0.
      else.
          TRY.
*for meail:	IS_ADDRESS-recip_email_addr
*for fax: IS_ADDRESS-recip_fax_country
*	 IS_ADDRESS-recip_fax_number
*	 IS_ADDRESS-sender_fax_country
*	 IS_ADDRESS-sender_fax_number.
          clear: lt_email_text.
          lv_medium = 'MAIL'.    "for fax TELEFAX
          lv_email_subject = 'Payment Remittance Advice'.
          lv_language = sy-langu.
          ls_address-recip_email_addr = 'sahmad@uniongas.com'.
          CONCATENATE 'Payment Remittance Advice for ' gs_lfa1-name1 into ls_email_text.
          append ls_email_text to lt_email_text.
          CALL METHOD cl_wzre_service_nast_print=>send_data_pdf
            EXPORTING
              iv_medium        = lv_medium
              iv_email_subject = lv_email_subject
              it_email_text    = lt_email_text
              is_formoutput    = ls_fpformoutput
              iv_language      = lv_language
              is_address       = ls_address
            IMPORTING
              ev_sent          = lv_event              .
           commit WORK.
           CATCH cx_wzre_internal_error .
          ENDTRY.
      endif.
*---------------------------------------------------------

*---------------------------------------------------------

*    IF p_email = 'X'. "for email/fax to alt_payee
**       clear: ls_adrc.
**       select single * from adrc into ls_adrc
**        where addnumber = lfa1-adrnr.
**       if ls_adrc-fax_number is not initial.
**      IF gs_alt_payee-telfx IS NOT INITIAL.
**         perform fax_form tables lt_regup
**                           using gs_alt_payee.
**      ELSE. "following for email
*      CLEAR: lt_knvk.
*      SELECT * INTO TABLE lt_knvk FROM knvk
*                      WHERE lifnr = gv_alt_payee
*                        AND abtnr = c_abtnr  "Department
*                        AND pafkt = c_pafkt. "Function
*      LOOP AT lt_knvk INTO ls_knvk.
*        CLEAR: lv_email.
*        SELECT SINGLE smtp_addr INTO lv_email FROM adr6
*               WHERE addrnumber = gs_alt_payee-adrnr
*                 AND persnumber = ls_knvk-prsnr.
*        IF lv_email CA '@'.
*          gs_email-email = lv_email.
*          APPEND gs_email TO gt_email.
*        ENDIF.
*      ENDLOOP.
**         IF lv_email IS INITIAL OR
**            lv_email NA '@'.
*      IF gt_email IS NOT INITIAL. "send email
*        PERFORM email_form TABLES lt_regup
*                        USING 'P'.
*        CONTINUE.
*      ENDIF.
*      IF gt_email IS INITIAL AND  "if no email then send fax
*         gs_alt_payee-telfx IS NOT INITIAL.
*        PERFORM fax_form TABLES lt_regup
*                         USING gs_alt_payee
*                               'P'.
*        CONTINUE.
*      ENDIF.
*      IF gt_email IS INITIAL AND  "Maintain error
*         gs_alt_payee-telfx IS INITIAL.
*        gs_no_email-bukrs = p_zbukr.
*        gs_no_email-instance = sy-sysid.
*        gs_no_email-phone = gs_alt_payee-telf1.
*        gs_no_email-lifnr = gv_alt_payee.
*        gs_no_email-name1 = gs_alt_payee-name1.
*        gs_no_email-text  = 'Payee Wrong / blank email/No Fax'.
*        APPEND gs_no_email TO gt_no_email.
*        CONTINUE.
*      ENDIF.
**      ENDIF.
*    ELSE.  "for download
*      gs_email-email = p_downe.
*      APPEND gs_email TO gt_email.
*      PERFORM email_form TABLES lt_regup
*                         USING 'P'.
*    ENDIF.
**    ENDIF.
  ENDLOOP.
CALL FUNCTION 'FP_JOB_CLOSE'
* IMPORTING
*   E_RESULT             =
 EXCEPTIONS
   USAGE_ERROR          = 1
   SYSTEM_ERROR         = 2
   INTERNAL_ERROR       = 3
   OTHERS               = 4  .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

ENDFORM.                    " PROCESS_DATA_PAYEE
*&---------------------------------------------------------------------*
*&      Form  PRINT_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM email_form TABLES pt_regup STRUCTURE regup
                USING p_flag TYPE c.



  DATA: st_output_options     TYPE ssfcompop,
        st_control_parameters TYPE ssfctrlop,
        fm_name TYPE rs38l_fnam,
        l_name TYPE rs38l_fnam,
        lv_so_name TYPE so_name.

  CONSTANTS: c_mail  TYPE tddevice VALUE 'EMAIL',
             c_email TYPE nast-nacha VALUE '5'.
*********************************************
  LOOP AT gt_email INTO gs_email.
    CLEAR gv_mail.
    lv_so_name = gs_email-email. "gv_email.
    CALL FUNCTION 'CREATE_RECIPIENT_OBJ_PPF'
      EXPORTING
        ip_mailaddr       = lv_so_name
      IMPORTING
        ep_recipient_id   = gv_mail-emrec
      EXCEPTIONS
        invalid_recipient = 1.
    IF sy-subrc <> 0.

    ENDIF.
* Store the address number
    IF p_flag = 'P'.
      gv_mail-adrnr = gs_alt_payee-adrnr.
    ELSE.
      gv_mail-adrnr = gs_lfa1-adrnr.
    ENDIF.
    PERFORM mail_form TABLES pt_regup
                      USING p_flag.

  ENDLOOP.

ENDFORM.                    " EMAIL_FORM
*&---------------------------------------------------------------------*
*&      Form  GET_FM_NAME
*&---------------------------------------------------------------------*
*       text
*&---------------------------------------------------------------------*
FORM get_fm_name .

clear: gv_fpname,
       gv_FUNCNAME,
       gv_fPINTERFACETYPE,
       gv_fUNCNAME_inbound.

gv_fpname = 'Z_PAYMENT_REMITTANCE'.
TRY .
  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name                     = gv_fpname
   IMPORTING
     E_FUNCNAME                 = gv_FUNCNAME
     E_INTERFACE_TYPE           = gv_fPINTERFACETYPE
     EV_FUNCNAME_INBOUND        = gv_fUNCNAME_inbound .
 CATCH cx_fp_api_repository.
      "Exception API (Repository)

 CATCH cx_fp_api_usage.
      "Exception API (Use)

 CATCH cx_fp_api_internal.
      "Exception API (Internal).
 CATCH cx_root.

ENDTRY.




ENDFORM.                    " GET_FM_NAME
*&---------------------------------------------------------------------*
*&      Form  MAIL_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mail_form TABLES pt_regup1 STRUCTURE regup
               USING p_flag TYPE c.

  CONSTANTS: c_true  TYPE xfeld    VALUE 'X',
             c_mail  TYPE tddevice VALUE 'MAIL'.
  DATA: ls_snd TYPE swotobjid,          " Sender object
        ls_obj TYPE swotobjid.          " Email object
  DATA:   lv_opt     TYPE ssfcompop,
          lv_ctrl    TYPE ssfctrlop.
  DATA: lv_mm(2),
        lv_dd(2),
        lv_year(4),
        lv_cdate(10),
        lv_co(3) TYPE c VALUE 'C/o',
        lv_faxuser(15) TYPE c VALUE 'SAPEEFTFAX',
        lv_sender(25) TYPE c VALUE 'ACCOUNTS PAYABLE'.
  DATA: ls_lfa1 TYPE lfa1.


  CLEAR gs_no_email.
  IF p_flag = 'P'.
    ls_lfa1 = gs_alt_payee.
  ELSE.
    ls_lfa1 = gs_lfa1.
  ENDIF.
* Set the output to email
  lv_ctrl-device   = c_mail.
* Make sure the email gets sent (i.e. it is committed)
  lv_opt-bcs_commit = c_true.
*Subject for Email
  lv_dd = p_laufd+6(2).
  lv_mm = p_laufd+4(2).
  lv_year =  p_laufd(4).
  CONCATENATE lv_year '/' lv_mm '/' lv_dd INTO lv_cdate.
  CONCATENATE 'Payment Remittance Advice ' lv_cdate lv_co
  gs_adrc-name1 INTO lv_opt-tdtitle SEPARATED BY space.

* Get the sender object
  PERFORM get_sender CHANGING ls_snd.
* Get the mail object
  PERFORM get_mail_obj CHANGING ls_obj.
* Call the function module to produce the smartform email
  CALL FUNCTION gv_fm_name
    EXPORTING
*      ARCHIVE_INDEX              =
*      ARCHIVE_INDEX_TAB          =
*      ARCHIVE_PARAMETERS         =
       control_parameters         = lv_ctrl
       mail_appl_obj              = ls_obj
       mail_recipient             = gv_mail-emrec
       mail_sender                = ls_snd
       output_options             = lv_opt
*      USER_SETTINGS              = 'X'
       gs_lfa1                    = ls_lfa1                 "gs_lfa1
       gs_ardc                    = gs_adrc
       gi_lifnr                   = ls_lfa1-lifnr  "gs_lfa1-lifnr
       gi_date                    = p_laufd
       gi_currency                = gv_waers
       gi_vblnr                   = gv_vblnr
       gi_faxuser                 = lv_faxuser
       gi_sender                  = lv_sender
       gs_vendor                  = gs_lfa1
       gi_payee                   = p_flag
*  IMPORTING
*      DOCUMENT_OUTPUT_INFO       =
*      JOB_OUTPUT_INFO            =
*      JOB_OUTPUT_OPTIONS         =
   TABLES
       gt_regup                   = pt_regup1
*       invoice_item               = p_invoice_item
    EXCEPTIONS
       formatting_error           = 1
       internal_error             = 2
       send_error                 = 3
       user_canceled              = 4
       OTHERS                     = 5.
* Trouble...
  IF sy-subrc <> 0.
    IF sy-subrc = 1.
      gs_no_email-text  = 'Smartform Fomatting Error'.
    ENDIF.
    IF sy-subrc = 2.
      gs_no_email-text  = 'Smartform Internal Error'.
    ENDIF.
    IF sy-subrc = 3.
      gs_no_email-text  = 'Smartform Sending Error'.
    ENDIF.
    IF sy-subrc = 4.
      gs_no_email-text  = 'Smartform User Cancelled Error'.
    ENDIF.
    IF sy-subrc = 5.
      gs_no_email-text  = 'Smartform Others Error'.
    ENDIF.
    gs_no_email-bukrs = p_zbukr.
    gs_no_email-instance = sy-sysid.
    gs_no_email-phone = ls_lfa1-telf1.
    gs_no_email-lifnr = ls_lfa1-lifnr.
    gs_no_email-name1 = ls_lfa1-name1.
    APPEND gs_no_email TO gt_no_email.
  ELSE.
    gs_yes_email-instance = sy-sysid.
    gs_yes_email-lifnr = ls_lfa1-lifnr.
    gs_yes_email-name1 = ls_lfa1-name1.
    CONCATENATE 'Email sent at ' gs_email-email INTO
                  gs_yes_email-text SEPARATED BY space.
    APPEND gs_yes_email TO gt_yes_email.
  ENDIF.

ENDFORM.                    " MAIL_FORM
*&---------------------------------------------------------------------*
*&      Form  GET_SENDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_sender  CHANGING f_snd TYPE swotobjid.

* Create the sender object
  CALL FUNCTION 'CREATE_SENDER_OBJECT_PPF'
    EXPORTING
      ip_sender      = sy-uname
    IMPORTING
      ep_sender_id   = f_snd
    EXCEPTIONS
      invalid_sender = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.

  ENDIF.

ENDFORM.                    " GET_SENDER
*&---------------------------------------------------------------------*
*&      Form  GET_MAIL_OBJ
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_mail_obj  CHANGING f_obj TYPE swotobjid.

  INCLUDE <cntn01>.                   " Container related stuff

  DATA:
      ls_inbyr TYPE soud-inbyr,         " Object year
      ls_inbno TYPE soud-inbno,         " Object number
      l_bor    TYPE swotobjid-objkey,   " BOR Object key
      ls_fold  TYPE swc_object,         " Folder object
      BEGIN OF ls_key,                  " Object key structure
        foldertype   TYPE sofm-foltp,   " Folder type
        folderyear   TYPE sofm-folyr,   " Folder year
        foldernumber TYPE sofm-folno,   " Folder number
        type         TYPE sofm-doctp,   " Document type
        year         TYPE sofm-docyr,   " Document year
        number       TYPE sofm-docno,   " Document number
        forwarder    TYPE soub-usrnam,  " User
      END OF ls_key.

* Get the object year and object number
  SELECT SINGLE inbyr inbno INTO (ls_inbyr, ls_inbno)       "#EC *
        FROM soud
        WHERE sapnam  = sy-uname
          AND deleted = space.
* If not found, create a new user, and try to get the data again
  IF sy-subrc NE 0.
    CALL FUNCTION 'SO_USER_AUTOMATIC_INSERT'
      EXPORTING
        sapname        = sy-uname
      EXCEPTIONS
        no_insert      = 1
        sap_name_exist = 2
        x_error        = 3
        OTHERS         = 4.
    IF sy-subrc NE 0.
*      exit.
    ELSE.
      SELECT SINGLE inbyr inbno INTO (ls_inbyr, ls_inbno)   "#EC *
            FROM soud
            WHERE sapnam  = sy-uname
              AND deleted = space.
    ENDIF.
  ENDIF.
* Create the object key
  ls_key-type   = 'FOL'.
  ls_key-year   = ls_inbyr.
  ls_key-number = ls_inbno.
* Create the BOR key from the object key
  l_bor = ls_key.
* Create the object
  swc_create_object ls_fold 'SOFMFOL' l_bor.
  IF sy-subrc = 0.
    swc_object_to_persistent ls_fold f_obj.
* Could not make the object persistent, trouble
    IF sy-subrc NE 0.

    ENDIF.
* Trouble, object could not be created
  ELSE.

  ENDIF.


ENDFORM.                    " GET_MAIL_OBJ
*&---------------------------------------------------------------------*
*&      Form  DISABLE_ENABLE_FIELDS
*&---------------------------------------------------------------------*
*       enable or disable selection screen fields
*----------------------------------------------------------------------*
FORM disable_enable_fields .

  CONSTANTS:   lc_x   TYPE char1 VALUE 'X',
               lc_clr TYPE char3 VALUE 'DN1'.

  IF p_email = lc_x.
*    sy-ucomm = 'ONLI'.
    LOOP AT SCREEN.
      IF screen-group1 = lc_clr.
        screen-input = 0. " Disable the input
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    CLEAR p_downe.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = lc_clr.
        screen-input = 1. " enable the input
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " DISABLE_ENABLE_FIELDS
*&---------------------------------------------------------------------*
*&      Form  EMAIL_LOG
*&---------------------------------------------------------------------*
*       email log to the user
*----------------------------------------------------------------------*
FORM email_log .

  IF gt_no_email IS NOT INITIAL.
    PERFORM send_email.
  ENDIF.
  IF gt_yes_email IS NOT INITIAL.
    PERFORM send_yes_email.
  ENDIF.

ENDFORM.                    " EMAIL_LOG
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_email .

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

ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_build_mail_content  CHANGING p_lt_objtxt TYPE table.

  DATA: lwa_objtxt       TYPE solisti1,
        lwa_email_data   TYPE ty_email_data.

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
              text-008 '</td><td>'
              text-009 '</td><td>'
              text-013 '</td><td>'
              text-010 '</td></tr>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

*  CONCATENATE '<tr> <td>'
*              text-008
*              '</td> <td>'
*              text-031
*              '</td> </tr> </table>'
*              INTO lwa_objtxt
*              SEPARATED BY space.
*
*  APPEND lwa_objtxt TO p_lt_objtxt.
*  CLEAR: lwa_objtxt.
*
*  CONCATENATE '<br> </br> <b> <u>'
*              text-009
*              '</u> </b>'
*              INTO lwa_objtxt
*              SEPARATED BY space.
*
*  APPEND lwa_objtxt TO p_lt_objtxt.
*  CLEAR: lwa_objtxt.

  LOOP AT gt_no_email INTO gs_no_email.

* Prepare Table
*    CONCATENATE '<br> <table border="2" width="50%">'
*                '<tr> <td>'
*                INTO lwa_objtxt
*                SEPARATED BY space.

*    APPEND lwa_objtxt TO p_lt_objtxt.
*    CLEAR: lwa_objtxt.

    CONCATENATE  '<tr> <td>'
                gs_no_email-instance '</td><td>'
                gs_no_email-bukrs '</td><td>'
                gs_no_email-lifnr '</td><td>'
                gs_no_email-name1 '</td><td>'
                gs_no_email-phone '</td><td>'
                gs_no_email-text  '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.

    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.

*    AT LAST.
*
*      MOVE: '</table> <br>' TO lwa_objtxt.
*      APPEND: lwa_objtxt TO p_lt_objtxt.
*      CLEAR: lwa_objtxt.
*
*    ENDAT.

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
*&      Form  RELEASE_EMAILS
*&---------------------------------------------------------------------*
*       Call program to push mail from SAP mail outbox
*----------------------------------------------------------------------*
FORM release_emails .

  CONSTANTS: lc_int(3)    TYPE c VALUE 'INT'.


  SUBMIT rsconn01 WITH mode = lc_int
                  WITH output = space
                  AND RETURN.

ENDFORM.                    " RELEASE_EMAILS
*&---------------------------------------------------------------------*
*&      Form  SEND_YES_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_yes_email .


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
  PERFORM build_yes_mail_content CHANGING lt_objtxt.

* Object with main text of the mail.
  lwa_objtxt = lv_space.
  APPEND lwa_objtxt TO lt_objtxt.
  CLEAR lwa_objtxt.

  DESCRIBE TABLE lt_objtxt LINES lv_lines.

  CONCATENATE text-011
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
    WRITE: /'Confirmation email log is emailed.'.

  ENDIF.

ENDFORM.                    " SEND_YES_EMAIL
*&---------------------------------------------------------------------*
*&      Form  BUILD_YES_MAIL_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_yes_mail_content  CHANGING p_lt_objtxt TYPE table.

  DATA: lwa_objtxt       TYPE solisti1,
        lwa_email_data   TYPE ty_email_data.

*Prepare HTML mail header
  CONCATENATE '<html>'
              '<body>'
              '<h4 style="font-family:arial"><caption><b><u>'
              text-015
              '</u></b><caption></h4>'
              '<ul>'
              '</ul>'
         INTO lwa_objtxt
         SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td width = "15%">'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE text-014 '</td><td width = "15%">'
              text-008
              '</td> <td width = "30%">'
              text-009
              '</td> <td width = "40%">'
              text-010
              '</td> </tr>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

*  CONCATENATE '<tr> <td>'
*              text-008
*              '</td> <td>'
*              text-031
*              '</td> </tr> </table>'
*              INTO lwa_objtxt
*              SEPARATED BY space.
*
*  APPEND lwa_objtxt TO p_lt_objtxt.
*  CLEAR: lwa_objtxt.
*
*  CONCATENATE '<br> </br> <b> <u>'
*              text-009
*              '</u> </b>'
*              INTO lwa_objtxt
*              SEPARATED BY space.
*
*  APPEND lwa_objtxt TO p_lt_objtxt.
*  CLEAR: lwa_objtxt.

  LOOP AT gt_yes_email INTO gs_yes_email.

* Prepare Table
*    CONCATENATE '<br> <table border="2" width="50%">'
*                '<tr> <td>'
*                INTO lwa_objtxt
*                SEPARATED BY space.

*    APPEND lwa_objtxt TO p_lt_objtxt.
*    CLEAR: lwa_objtxt.

    CONCATENATE  '<tr> <td width = "15%">'
                gs_yes_email-instance
                '</td> <td width = "15%">'
                gs_yes_email-lifnr
                '</td> <td width = "30%">'
                gs_yes_email-name1
                '</td> <td width = "40%">'
                gs_yes_email-text
                '</td> </tr>'
                INTO lwa_objtxt
                SEPARATED BY space.

    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.

*    AT LAST.
*
*      MOVE: '</table> <br>' TO lwa_objtxt.
*      APPEND: lwa_objtxt TO p_lt_objtxt.
*      CLEAR: lwa_objtxt.
*
*    ENDAT.

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

ENDFORM.                    " BUILD_YES_MAIL_CONTENT
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA_VENDOR
*&---------------------------------------------------------------------*
*  Vendor - email for all remittances (vendor & Alt Payee transactions)
*----------------------------------------------------------------------*
FORM process_data_vendor .

  DATA: lt_regup TYPE TABLE OF regup,
          lv_email TYPE adr6-smtp_addr,
          lv_prsnr TYPE knvk-prsnr.


  DATA: lt_regup_header TYPE TABLE OF regup,
        ls_regup_header LIKE LINE OF lt_regup_header,
        lt_knvk TYPE TABLE OF knvk,
        ls_knvk LIKE LINE OF lt_knvk.
 data:  lv_faxuser(25) TYPE c VALUE 'SAPEEFTFAX',
        lv_sender(25) TYPE c VALUE 'ACCOUNTS PAYABLE'.

data: ls_FPFORMOUTPUT type FPFORMOUTPUT,
       LV_Medium type FPMEDIUM,
       lv_EMAIL_SUBJECT Type SO_OBJ_DES,
       LT_EMAIL_TEXT  type BCSY_TEXT,
       LS_EMAIL_TEXT  type LINE OF BCSY_TEXT,
       LV_LANGUAGE TYPE SYLANGU,
       LS_ADDRESS type cl_wzre_service_nast_print=>ty_mail_fax_addr,
       lv_event type BOOLE_D.

  lt_regup_header[] = gt_regup[].
  DELETE ADJACENT DUPLICATES FROM lt_regup_header COMPARING lifnr vblnr waers.
  SORT lt_regup_header BY lifnr  vblnr waers.
  SORT gt_regup BY lifnr waers.

*gs_outputparams-nodialog = space.
*gs_outputparams-preview = 'X'.
*gs_outputparams-dest = 'CEC2F4650_BW'.

CALL FUNCTION 'FP_JOB_OPEN'
  CHANGING
    ie_outputparams       = gs_outputparams
 EXCEPTIONS
   CANCEL                = 1
   USAGE_ERROR           = 2
   SYSTEM_ERROR          = 3
   INTERNAL_ERROR        = 4
   OTHERS                = 5 .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

  LOOP AT lt_regup_header INTO ls_regup_header.
    CLEAR: lt_regup,
           lv_prsnr,
           lv_email,
           gs_lfa1,
           gs_email,
           gt_email,
           gs_alt_payee.

    gv_waers = ls_regup_header-waers.
    gv_vblnr = ls_regup_header-vblnr.
    READ TABLE gt_regup WITH KEY lifnr = ls_regup_header-lifnr
                                 vblnr = ls_regup_header-vblnr
                                 waers = ls_regup_header-waers "gs_lfa1-lifnr
                                 TRANSPORTING NO FIELDS.
    LOOP AT gt_regup INTO gs_regup
                          WHERE lifnr = ls_regup_header-lifnr
                            AND vblnr = ls_regup_header-vblnr
                            AND waers = ls_regup_header-waers. "gs_lfa1-lifnr.
      APPEND gs_regup TO lt_regup.
    ENDLOOP.
    sort lt_regup by lifnr. " bldat vblnr.
    CHECK lt_regup IS NOT INITIAL.
    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = ls_regup_header-lifnr.
*    gs_docparams-langu = sy-langu.
*    gs_docparams-country = 'CA'.

      CALL FUNCTION gv_FUNCNAME
        EXPORTING
         /1BCDWB/DOCPARAMS        =  gs_docparams
          gt_regup                 = lt_regup
          gi_payee                 = 'V'
          gs_vendor                = gs_lfa1
          gi_sender                = lv_sender
          gi_faxuser               = lv_faxuser
          gi_vblnr                 = gv_vblnr
          gs_ardc                  = gs_adrc
          gs_lfa1                  = gs_lfa1
          gi_currency              = gv_waers
          gi_date                  = p_laufd
          gi_lifnr                 = gs_lfa1-lifnr
       IMPORTING
         /1BCDWB/FORMOUTPUT       = ls_fpformoutput
       EXCEPTIONS
         USAGE_ERROR              = 1
         SYSTEM_ERROR             = 2
         INTERNAL_ERROR           = 3
         OTHERS                   = 4  .
      IF sy-subrc <> 0.
   else.
          TRY.
*for meail:	IS_ADDRESS-recip_email_addr
*for fax: IS_ADDRESS-recip_fax_country
*	 IS_ADDRESS-recip_fax_number
*	 IS_ADDRESS-sender_fax_country
*	 IS_ADDRESS-sender_fax_number.
          clear: lt_email_text.
          lv_medium = 'MAIL'.
          lv_email_subject = 'PRA'.
          lv_language = sy-langu.
          ls_address-recip_email_addr = P_DOWNE.
          CONCATENATE 'TEST ' gs_lfa1-name1 into ls_email_text.
          append ls_email_text to lt_email_text.
          CALL METHOD cl_wzre_service_nast_print=>send_data_pdf
            EXPORTING
              iv_medium        = lv_medium
              iv_email_subject = lv_email_subject
              it_email_text    = lt_email_text
              is_formoutput    = ls_fpformoutput
              iv_language      = lv_language
              is_address       = ls_address
            IMPORTING
              ev_sent          = lv_event              .
          commit WORK.
           CATCH cx_wzre_internal_error .
             write: / 'error'.
          ENDTRY.
      endif.

*    IF p_email = 'X'. "for email to vendor
*      CLEAR: lt_knvk.
*      SELECT * INTO TABLE lt_knvk FROM knvk
*                      WHERE lifnr = gs_lfa1-lifnr
*                        AND abtnr = c_abtnr  "Department
*                        AND pafkt = c_pafkt. "Function
*      LOOP AT lt_knvk INTO ls_knvk.
*        CLEAR: lv_email.
*        SELECT SINGLE smtp_addr INTO lv_email FROM adr6
*               WHERE addrnumber = gs_lfa1-adrnr
*                 AND persnumber = ls_knvk-prsnr.
*        IF lv_email CA '@'.
*          gs_email-email = lv_email.
*          APPEND gs_email TO gt_email.
*        ENDIF.
*      ENDLOOP.
*      IF gt_email IS NOT INITIAL. "send email
*        PERFORM email_form TABLES lt_regup
*                           USING 'V'.
*        CONTINUE.
*      ENDIF.
*      IF gt_email IS INITIAL AND  "send fax
*         gs_lfa1-telfx IS NOT INITIAL.
*        PERFORM fax_form TABLES lt_regup
*                         USING gs_lfa1
*                               'V'.
*        CONTINUE.
*      ENDIF.
**          gv_email = lv_email.
*      IF gt_email IS INITIAL AND
*         gs_lfa1-telfx IS INITIAL.
*        gs_no_email-bukrs = p_zbukr.
*        gs_no_email-instance = sy-sysid.
*        gs_no_email-phone = gs_lfa1-telf1.
*        gs_no_email-lifnr = gs_lfa1-lifnr.
*        gs_no_email-name1 = gs_lfa1-name1.
*        gs_no_email-text  = 'Vendor Wrong / blank email/No Fax'.
*        APPEND gs_no_email TO gt_no_email.
*        CONTINUE.
*      ENDIF.
**      ENDIF.
*    ELSE.  "for download
*      gs_email-email = p_downe.
*      APPEND gs_email TO gt_email.
*      PERFORM email_form TABLES lt_regup
*                         USING 'V'.
*
*    ENDIF.
  ENDLOOP.
  CALL FUNCTION 'FP_JOB_CLOSE'
*   IMPORTING
*     E_RESULT             =
   EXCEPTIONS
     USAGE_ERROR          = 1
     SYSTEM_ERROR         = 2
     INTERNAL_ERROR       = 3
     OTHERS               = 4 .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " PROCESS_DATA_VENDOR
*&---------------------------------------------------------------------*
*&      Form  FAX_FORM
*&---------------------------------------------------------------------*
*       Fax Form to Vendor
*----------------------------------------------------------------------*
FORM fax_form  TABLES pt_regup STRUCTURE regup
                USING p_vendor TYPE lfa1
                      p_flag TYPE c.

  DATA: lv_output  TYPE ssfcompop,
        lv_control TYPE ssfctrlop.
  DATA: lv_faxuser(15) TYPE c VALUE 'SAPEEFTFAX',
        lv_sender(25) TYPE c VALUE 'ACCOUNTS PAYABLE'.
*******Parameter for Fax
*  lv_control-NO_DIALOG = 'X'.
*  lv_control-device = 'TELEFAX'.
*
*  lv_output-TDTELELAND = p_vendor-LAND1.
**-Telecommunications partner.
  lv_output-tdtelenum = p_vendor-telfx.
**-Name of the SAPoffice user
*  lv_output-TDFAXUSER = sy-UNAME.
**-Field of length 3 bytes
*  lv_output-FAXFORMAT = 'OTF'.
**-Scheduled for sending:
**'IMM': immediately
**'NIG': at night
*  lv_output-TDSCHEDULE = 'IMM'.
**-Desired send date
*  lv_output-TDSENDDATE = sy-datum.
**-Desired send time
*  lv_output-TDSENDTIME = sy-UZEIT.
**********End of Fax Parameters
**Parameters regarding printer ZEFX
  lv_control-no_dialog = 'X'.
  lv_control-device = 'PRINTER'.
*lv_control-getotf = 'X'.
  lv_output-tddest = 'ZFAX'.
*LV_OUTPUT-TDPRINTER = 'ZFACSYS'.
*LV_OUTPUT-TDIMMED = 'X'. Print Immediately
  lv_output-tdnewid = 'X'.

*******End of Printer Parameters
  CALL FUNCTION gv_fm_name
      EXPORTING
*      ARCHIVE_INDEX              =
*      ARCHIVE_INDEX_TAB          =
*      ARCHIVE_PARAMETERS         =
         control_parameters         = lv_control
*       mail_appl_obj              =
*       mail_recipient             =
*       mail_sender                =
         output_options             = lv_output
        user_settings              = ' '
         gs_lfa1                    = p_vendor
         gs_ardc                    = gs_adrc
         gi_lifnr                   = p_vendor-lifnr
         gi_date                    = p_laufd
         gi_currency                = gv_waers
         gi_vblnr                   = gv_vblnr
         gi_faxuser                 = lv_faxuser
         gi_sender                  = lv_sender
         gs_vendor                  = gs_lfa1
         gi_payee                   = p_flag
*  IMPORTING
*      DOCUMENT_OUTPUT_INFO       =
*      JOB_OUTPUT_INFO            =
*      JOB_OUTPUT_OPTIONS         =
     TABLES
         gt_regup                   = pt_regup
*       invoice_item               = p_invoice_item
      EXCEPTIONS
         formatting_error           = 1
         internal_error             = 2
         send_error                 = 3
         user_canceled              = 4
         OTHERS                     = 5.
* Trouble...
  IF sy-subrc <> 0.
    IF sy-subrc = 1.
      gs_no_email-text  = ', Smartform Fomatting Error'.
    ENDIF.
    IF sy-subrc = 2.
      gs_no_email-text  = ', Smartform Internal Error'.
    ENDIF.
    IF sy-subrc = 3.
      gs_no_email-text  = ', Smartform Sending Error'.
    ENDIF.
    IF sy-subrc = 4.
      gs_no_email-text  = ', Smartform User Cancelled Error'.
    ENDIF.
    IF sy-subrc = 5.
      gs_no_email-text  = ', Smartform Others Error'.
    ENDIF.
    gs_no_email-bukrs = p_zbukr.
    gs_no_email-instance = sy-sysid.
    gs_no_email-phone = p_vendor-telf1.
    gs_no_email-lifnr = p_vendor-lifnr.
    gs_no_email-name1 = p_vendor-name1.
    CONCATENATE 'Unable to fax @' p_vendor-telfx gs_no_email-text INTO
      gs_no_email-text SEPARATED BY space.
    APPEND gs_no_email TO gt_no_email.
  ELSE.
    gs_yes_email-instance = sy-sysid.
    gs_yes_email-lifnr = p_vendor-lifnr.
    gs_yes_email-name1 = p_vendor-name1.
    CONCATENATE 'Fax sent at ' p_vendor-telfx INTO
                  gs_yes_email-text SEPARATED BY space.
    APPEND gs_yes_email TO gt_yes_email.
  ENDIF.

ENDFORM.                    " FAX_FORM
