*&---------------------------------------------------------------------*
*& Report  ZFAPI047
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 2013/12/17 - Modified program to only email vendors if the alt payee*
*& SDP56933     was not sent an email.
*& GYMANA
*&
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 22-July-2019  SUBRAMS2  D30K930047  CHG0152767: Make program work for*
*                         D30K930050 D30K930059   '*' company code     *
************************************************************************
REPORT  zfapi047.

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
* Begin of changes CHG0152767 SUBRAMS2
TYPES: BEGIN OF ty_bnk_item,
       zbukr TYPE DZBUKR,
       lifnr TYPE lifnr,
       vblnr TYPE vblnr,
       LAUFI TYPE LAUFI,
       END OF ty_bnk_item.
* End of changes CHG0152767 SUBRAMS2

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
      gt_bnk_batch TYPE TABLE OF bnk_batch_header,
      gs_adrc  TYPE adrc,
      gs_lfa1 TYPE lfa1,
      gt_lfa1 TYPE TABLE OF lfa1,
      gs_alt_payee TYPE lfa1,
      gv_alt_payee TYPE lfa1-lifnr,
      gv_adrnr TYPE t001-adrnr,
      gv_waers TYPE waers,
      gv_vblnr TYPE vblnr,
      git_regup_temp TYPE STANDARD TABLE OF regup,
      gwa_regup_temp TYPE regup,
      gt_reguhm TYPE TABLE OF reguhm,
      gs_reguhm TYPE reguhm,
      gt_bnk_item  TYPE STANDARD TABLE OF ty_bnk_item, "SUBRAMS2 CHG0152767
      gt_bnk_item_temp TYPE STANDARD TABLE OF ty_bnk_item, "SUBRAMS2 CHG0152767
      gs_bnk_item  TYPE bnk_batch_item,                    "SUBRAMS2 CHG0152767
      gt_reguhm_temp TYPE TABLE OF reguhm,                 "SUBRAMS2 CHG0152767
      gs_reguhm_temp TYPE reguhm.                          "SUBRAMS2 CHG0152767

DATA: gv_email TYPE swotobjid,
      gv_fm_name TYPE rs38l_fnam,
      gv_mail TYPE ty_mail.
DATA: ge_mail   TYPE string,
      ge_name   TYPE string,
      ge_domain TYPE string,
      gv_company_code TYPE regup-zbukr.

DATA: lt_regup TYPE TABLE OF regup,
      lv_email TYPE adr6-smtp_addr,
      lv_prsnr TYPE knvk-prsnr.

DATA: lt_regup_header TYPE TABLE OF regup,
      ls_regup_header LIKE LINE OF lt_regup_header,
      lt_knvk TYPE TABLE OF knvk,
      ls_knvk LIKE LINE OF lt_knvk,
      ls_adrc TYPE adrc.

TYPES : BEGIN OF ty_t001,
         bukrs  TYPE t001-bukrs,
         adrnr  TYPE t001-adrnr,
        END OF ty_t001.

DATA : git_t001     TYPE STANDARD TABLE OF ty_t001,
       git_adrc     TYPE STANDARD TABLE OF adrc,
       gwa_t001     TYPE ty_t001,
       gwa_adrc     TYPE adrc,
       vendor_email_flag(1)  TYPE c.                        "SDP56933

CONSTANTS: c_abtnr TYPE knvk-abtnr VALUE '0009', "financial deparment
           c_pafkt TYPE knvk-pafkt VALUE '13'. "function

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*PARAMETERS: p_zbukr TYPE regup-zbukr OBLIGATORY.
SELECT-OPTIONS : s_zbukr FOR regup-zbukr OBLIGATORY.
PARAMETERS :     p_laufd TYPE regup-laufd OBLIGATORY.
SELECT-OPTIONS: s_lifnr FOR regup-lifnr,
                s_zlsch FOR regup-zlsch.
                "s_hbkid FOR regup-hbkid.
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
         gv_mail,
         gv_company_code,
         gt_reguhm.

  REFRESH : git_t001,
            git_adrc.

  CLEAR :  gwa_t001,
           gwa_adrc.

  SET COUNTRY 'US'.

  PERFORM get_data.
  PERFORM get_fm_name.
  PERFORM process_data_payee.
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

*  SELECT SINGLE adrnr INTO gv_adrnr FROM t001 WHERE bukrs = p_zbukr.
*  IF sy-subrc <> 0.
*    WRITE : / 'Enter correct company code...'.
*    STOP.
*  ENDIF.
*  SELECT SINGLE * FROM adrc INTO gs_adrc WHERE addrnumber = gv_adrnr.
*  IF sy-subrc <> 0.
*    WRITE : / 'Company Code address data is not maintained...'.
*    STOP.
*  ENDIF.
  CLEAR gt_reguhm_temp[]. "SUBRAMS2 CHG0152767

  SELECT bukrs adrnr
     FROM t001
    INTO TABLE git_t001
    WHERE bukrs IN s_zbukr.
  IF sy-subrc NE 0.
    WRITE : / 'Enter correct company code...'.
    STOP.
  ELSE.

    SELECT *
      FROM adrc
      INTO TABLE git_adrc
      FOR ALL ENTRIES IN git_t001
      WHERE addrnumber = git_t001-adrnr.
    IF sy-subrc <> 0.
      WRITE : / 'Company Code address data is not maintained...'.
      STOP.
    ENDIF.
  ENDIF.
  "-------------
  SELECT * FROM bnk_batch_header INTO TABLE gt_bnk_batch
       WHERE laufd_f = p_laufd
         AND zbukr  IN s_zbukr
*         AND hbkid  IN s_hbkid
         AND xvorl = space
         AND laufd_f <> space
         AND laufi_f <> space.

  "---------------
  IF gt_bnk_batch[] IS NOT INITIAL.
     select * from reguhm INTO TABLE gt_reguhm
       FOR ALL ENTRIES IN gt_bnk_batch
       WHERE zbukr   = gt_bnk_batch-zbukr
         AND laufd_m = gt_bnk_batch-LAUFD_F
         AND laufi_m = gt_bnk_batch-laufi_f
         AND rzawe   in s_zlsch.
* Begin of changes SUBRAMS2 CHG0152767
  ENDIF.
    CLEAR gt_bnk_batch[].
    SELECT * FROM bnk_batch_header INTO TABLE gt_bnk_batch
       WHERE laufd_f = p_laufd
         AND zbukr  = '*'
         AND xvorl = space
         AND laufd_f <> space
         AND laufi_f <> space.

   IF gt_bnk_batch IS NOT INITIAL.
*  Fetch details from Item table (Company code value exists)
     SELECT zbukr lifnr vblnr LAUFI FROM bnk_batch_item INTO TABLE gt_bnk_item
       FOR ALL ENTRIES IN gt_bnk_batch
       WHERE guid  = gt_bnk_batch-guid
         AND laufd = gt_bnk_batch-laufd
         AND laufi = gt_bnk_batch-laufi
         AND ZBUKR IN s_zbukr.
   ENDIF.
   IF gt_bnk_item IS NOT INITIAL.
     select * from reguhm INTO TABLE gt_reguhm_temp
       FOR ALL ENTRIES IN gt_bnk_item
       WHERE LAUFD   = p_laufd
         AND LAUFI   = gt_bnk_item-laufi
         AND zbukr   = gt_bnk_item-zbukr
         AND lifnr   = gt_bnk_item-lifnr
         AND vblnr   = gt_bnk_item-vblnr
         AND rzawe   in s_zlsch.
     IF gt_reguhm_temp IS NOT INITIAL.
       IF gt_reguhm IS NOT INITIAL.
         SORT gt_reguhm BY laufd laufi zbukr lifnr kunnr empfg vblnr.
       LOOP AT gt_reguhm_temp INTO gs_reguhm_temp.
         READ TABLE gt_reguhm TRANSPORTING NO FIELDS
              WITH KEY laufd = gs_reguhm_temp-laufd
                       laufi = gs_reguhm_temp-laufi
                       zbukr = gs_reguhm_temp-zbukr
                       lifnr = gs_reguhm_temp-lifnr
                       kunnr = gs_reguhm_temp-kunnr
                       empfg = gs_reguhm_temp-empfg
                       vblnr = gs_reguhm_temp-vblnr
               BINARY SEARCH.
         IF sy-subrc NE 0.
           APPEND gs_reguhm_temp TO gt_reguhm.
         ENDIF.
         CLEAR gs_reguhm_temp.
       ENDLOOP.
       ELSE.
        APPEND LINES OF gt_reguhm_temp TO gt_reguhm.
       ENDIF.
     ENDIF.
    ENDIF.
* End of changes SUBRAMS2 CHG0152767
    if gt_reguhm[] is not INITIAL.
    SELECT * FROM regup INTO TABLE gt_regup
      FOR ALL ENTRIES IN gt_reguhm
        WHERE zbukr = gt_reguhm-zbukr
          AND laufd = gt_reguhm-laufd
          AND laufi = gt_reguhm-laufi
          AND lifnr IN s_lifnr
*          AND zlsch IN s_zlsch
**          AND hbkid = gt_bnk_batch-hbkid
          AND xvorl = space.  "dont pull proposal run
    endif.
*    SELECT * FROM regup INTO TABLE gt_regup
*      FOR ALL ENTRIES IN gt_bnk_batch
*        WHERE zbukr = gt_bnk_batch-zbukr
*          AND laufd = gt_bnk_batch-laufd_f
*          AND laufi = gt_bnk_batch-laufi_f
*          AND lifnr IN s_lifnr
*          AND zlsch IN s_zlsch
**          AND hbkid = gt_bnk_batch-hbkid
*          AND xvorl = space
*  ENDIF.                     "SUBRAMS2 CHG0152767

  IF gt_regup IS INITIAL.
    WRITE: / 'No payment data to process'.
    EXIT.
  ENDIF.

  DELETE gt_regup WHERE ( ( vblnr = space ) OR
                          ( vblnr IS INITIAL ) ).
* Logic for removing duplication of entries in REGUP Table
*  IF gt_regup[] IS NOT INITIAL.
*    REFRESH : git_regup_temp.
*    CLEAR   : gwa_regup_temp.
*
*    LOOP AT gt_regup INTO gs_regup.
*      MOVE-CORRESPONDING gs_regup TO gwa_regup_temp.
*      CLEAR : gwa_regup_temp-laufi.
*
*      APPEND gwa_regup_temp TO git_regup_temp.
*      CLEAR : gwa_regup_temp.
*    ENDLOOP.
*
*    SORT git_regup_temp BY laufd xvorl zbukr lifnr kunnr empfg vblnr bukrs belnr gjahr buzei.
*    DELETE ADJACENT DUPLICATES FROM git_regup_temp.
*    gt_regup[] = git_regup_temp[].
*  ENDIF.

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

  DATA: lv_faxn TYPE adr3-fax_number.

  lt_regup_header[] = gt_regup[].
  SORT lt_regup_header BY lifnr empfg vblnr waers.
  DELETE ADJACENT DUPLICATES FROM lt_regup_header
                COMPARING lifnr empfg vblnr waers.
*  SORT lt_regup_header BY lifnr empfg vblnr waers.
  SORT gt_regup BY lifnr empfg vblnr waers.

*  PERFORM get_fm_name.

  LOOP AT lt_regup_header INTO ls_regup_header.
    CLEAR: lt_regup,
           lv_prsnr,
           lv_email,
           gs_lfa1,
           gs_email,
           gt_email,
           gt_fax,
           gs_fax,
           gs_alt_payee,
           vendor_email_flag.                               "SDP56933

    CLEAR : gv_company_code,
            gs_adrc.

    gv_company_code = ls_regup_header-zbukr.

    READ TABLE git_t001 INTO gwa_t001
                        WITH KEY bukrs = gv_company_code.
    IF sy-subrc = 0.
      READ TABLE git_adrc INTO gwa_adrc
                          WITH KEY addrnumber = gwa_t001-adrnr.
      IF sy-subrc = 0.
        gs_adrc = gwa_adrc.
      ENDIF.
    ENDIF.

    gv_waers = ls_regup_header-waers.
    gv_vblnr = ls_regup_header-vblnr.
    gv_alt_payee = ls_regup_header-empfg+1(10).
    IF gv_alt_payee IS INITIAL.
      vendor_email_flag = 'X'.                              "SDP56933
    ELSE.
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
      IF p_email = 'X'. "for email/fax to alt_payee
        CLEAR: lt_knvk.
        SELECT * INTO TABLE lt_knvk FROM knvk
                        WHERE lifnr = gv_alt_payee
                          AND abtnr = c_abtnr  "Department
                          AND pafkt = c_pafkt. "Function
*Get Emails
        LOOP AT lt_knvk INTO ls_knvk.
          CLEAR: lv_email.
          SELECT SINGLE smtp_addr INTO lv_email FROM adr6
                 WHERE addrnumber = gs_alt_payee-adrnr
                   AND persnumber = ls_knvk-prsnr.
          IF lv_email CA '@'.
            gs_email-email = lv_email.
            APPEND gs_email TO gt_email.
          ENDIF.
        ENDLOOP.
*Get Fax, if no email is available
*        IF gt_email[] IS INITIAL.
*          LOOP AT lt_knvk INTO ls_knvk.
*            CLEAR: lv_faxn.
*            SELECT SINGLE fax_number INTO lv_faxn FROM adr3
*                   WHERE addrnumber = gs_alt_payee-adrnr
*                     AND persnumber = ls_knvk-prsnr.
*            IF lv_faxn IS NOT INITIAL.
*              gs_fax-fax_num = lv_faxn.
*              APPEND gs_fax TO gt_fax.
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*         IF lv_email IS INITIAL OR
*            lv_email NA '@'.
        IF gt_email IS NOT INITIAL. "send email
          PERFORM email_form TABLES lt_regup
                          USING 'P'.
*        ELSEIF gt_email IS INITIAL AND  "if no email then send fax
*               gt_fax IS NOT INITIAL.
*          PERFORM fax_form TABLES lt_regup
*                           USING gs_alt_payee
*                                 'P'.
*        ELSEIF gt_email IS INITIAL AND  "Maintain error
*               gt_fax IS INITIAL.
        ELSE.
          gs_no_email-bukrs = gv_company_code. "p_zbukr.
          gs_no_email-instance = sy-sysid.
          gs_no_email-phone = gs_alt_payee-telf1.
          gs_no_email-lifnr = gv_alt_payee.
          gs_no_email-name1 = gs_alt_payee-name1.
          gs_no_email-text  = 'Payee Wrong / blank email'.
          APPEND gs_no_email TO gt_no_email.
*          vendor_email_flag = 'X'.                            "SDP56933
        ENDIF.
      ELSE.  "for download
        gs_email-email = p_downe.
        APPEND gs_email TO gt_email.
        PERFORM email_form TABLES lt_regup
                           USING 'P'.
      ENDIF.
    ENDIF.

    IF vendor_email_flag EQ 'X'.                            "SDP56933
      PERFORM process_data_vendor.                          "SDP56933
    ENDIF.                                                  "SDP56933

  ENDLOOP.

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

  DATA: lv_tdsfname TYPE tdsfname.

  lv_tdsfname = 'Z_REMITTANCE_ADVICE'.
*  IF p_zbukr = 'UGL'.
*    lv_tdsfname = 'Z_REMITTANCE_ADVICE'.
*  ELSE.
*    lv_tdsfname = 'Z_REMITTANCE_ADVICE_SPECTRA'.
*  ENDIF.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lv_tdsfname "'Z_REMITTANCE_ADVICE'
    IMPORTING
      fm_name            = gv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.

  ENDIF.

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
        lv_faxuser(15) TYPE c VALUE 'SAP FAX',
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
    gs_no_email-bukrs = gv_company_code. "p_zbukr.
    gs_no_email-instance = sy-sysid.
    gs_no_email-phone = ls_lfa1-telf1.
    gs_no_email-lifnr = ls_lfa1-lifnr.
    gs_no_email-name1 = ls_lfa1-name1.
    APPEND gs_no_email TO gt_no_email.
  ELSE.
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
              '<tr> <td width = "20%">'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE text-008
              '</td> <td width = "40%">'
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

    CONCATENATE  '<tr> <td width = "20%">'
                gs_yes_email-lifnr
                '</td> <td width = "40%">'
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
FORM process_data_vendor.
*                                                              "SDP56933
*  DATA: lt_regup TYPE TABLE OF regup,                         "SDP56933
*          lv_email TYPE adr6-smtp_addr,                       "SDP56933
*          lv_prsnr TYPE knvk-prsnr.                           "SDP56933
*                                                              "SDP56933
*                                                              "SDP56933
*  DATA: lt_regup_header TYPE TABLE OF regup,                  "SDP56933
*        ls_regup_header LIKE LINE OF lt_regup_header,         "SDP56933
*        lt_knvk TYPE TABLE OF knvk,                           "SDP56933
*        ls_knvk LIKE LINE OF lt_knvk.                         "SDP56933
*                                                              "SDP56933
*  lt_regup_header[] = gt_regup[].                             "SDP56933
*  SORT lt_regup_header BY lifnr  vblnr waers.                 "SDP56933
*  DELETE ADJACENT DUPLICATES FROM lt_regup_header             "SDP56933
*                      COMPARING lifnr vblnr waers.            "SDP56933
*  SORT lt_regup_header BY lifnr  vblnr waers.                 "SDP56933
*  SORT gt_regup BY lifnr waers.                               "SDP56933
                                                            "SDP56933
*  PERFORM get_fm_name.                                        "SDP56933
                                                            "SDP56933
*  LOOP AT lt_regup_header INTO ls_regup_header.               "SDP56933
  DATA: lv_faxn TYPE adr3-fax_number.

  CLEAR: lt_regup,
         lv_prsnr,
         lv_email,
         gs_lfa1,
         gs_email,
         gt_email,
         gt_fax,
         gs_fax,
         gs_alt_payee.

  CLEAR : gv_company_code,
          gs_adrc.

  gv_company_code = ls_regup_header-zbukr.

  READ TABLE git_t001 INTO gwa_t001
                      WITH KEY bukrs = gv_company_code.
  IF sy-subrc = 0.
    READ TABLE git_adrc INTO gwa_adrc
                        WITH KEY addrnumber = gwa_t001-adrnr.
    IF sy-subrc = 0.
      gs_adrc = gwa_adrc.
    ENDIF.
  ENDIF.

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
  CHECK lt_regup IS NOT INITIAL.
  READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = ls_regup_header-lifnr.
  IF p_email = 'X'. "for email to vendor
    CLEAR: lt_knvk.
    SELECT * INTO TABLE lt_knvk FROM knvk
                    WHERE lifnr = gs_lfa1-lifnr
                      AND abtnr = c_abtnr  "Department
                      AND pafkt = c_pafkt. "Function
    LOOP AT lt_knvk INTO ls_knvk.
      CLEAR: lv_email.
      SELECT SINGLE smtp_addr INTO lv_email FROM adr6
             WHERE addrnumber = gs_lfa1-adrnr
               AND persnumber = ls_knvk-prsnr.
      IF lv_email CA '@'.
        gs_email-email = lv_email.
        APPEND gs_email TO gt_email.
      ENDIF.
    ENDLOOP.
*Get Fax, if no email is available
*    IF gt_email[] IS INITIAL.
*      LOOP AT lt_knvk INTO ls_knvk.
*        CLEAR: lv_faxn.
*        SELECT SINGLE fax_number INTO lv_faxn FROM adr3
*               WHERE addrnumber = gs_lfa1-adrnr
*                 AND persnumber = ls_knvk-prsnr.
*        IF lv_faxn IS NOT INITIAL.
*          gs_fax-fax_num = lv_faxn.
*          APPEND gs_fax TO gt_fax.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
    IF gt_email IS NOT INITIAL. "send email
      PERFORM email_form TABLES lt_regup
                         USING 'V'.
*    ELSEIF gt_email IS INITIAL AND  "send fax
**       gs_lfa1-telfx IS NOT INITIAL.
*           gt_fax IS NOT INITIAL.
*      PERFORM fax_form TABLES lt_regup
*                       USING gs_lfa1
*                             'V'.
**          gv_email = lv_email.
*    ELSEIF gt_email IS INITIAL AND
**       gs_lfa1-telfx IS INITIAL.
*           gt_fax IS INITIAL.
    ELSE.
      gs_no_email-bukrs = gv_company_code. "p_zbukr.
      gs_no_email-instance = sy-sysid.
      gs_no_email-phone = gs_lfa1-telf1.
      gs_no_email-lifnr = gs_lfa1-lifnr.
      gs_no_email-name1 = gs_lfa1-name1.
      gs_no_email-text  = 'Vendor Wrong / blank email'.
      APPEND gs_no_email TO gt_no_email.
    ENDIF.
  ELSE.  "for download
    gs_email-email = p_downe.
    APPEND gs_email TO gt_email.
    PERFORM email_form TABLES lt_regup
                       USING 'V'.
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
  CONSTANTS: c_true  TYPE xfeld    VALUE 'X'.
  DATA: lv_output  TYPE ssfcompop,
        lv_control TYPE ssfctrlop.
  DATA: lv_faxuser(15) TYPE c VALUE 'SAPEEFTFAX',
        lv_sender(25) TYPE c VALUE 'ACCOUNTS PAYABLE',
        lv_mm(2),
        lv_dd(2),
        lv_year(4),
        lv_cdate(10),
        lv_co(3) TYPE c VALUE 'C/o'.

********Parameter for Fax
*  lv_control-no_dialog = 'X'.
*  lv_control-device = 'TELEFAX'.
*
*  lv_output-tdteleland = p_vendor-land1.
***-Telecommunications partner.
*  lv_output-tdtelenum = p_vendor-telfx.
***-Name of the SAPoffice user
*  lv_output-tdfaxuser = sy-uname.
***-Field of length 3 bytes
*  lv_output-faxformat = 'PDF'."'OTF'.
***-Scheduled for sending:
***'IMM': immediately
***'NIG': at night
*  lv_output-tdschedule = 'IMM'.
***-Desired send date
*  lv_output-tdsenddate = sy-datum.
***-Desired send time
*  lv_output-tdsendtime = sy-uzeit.
*
***********End of Fax Parameters
**Begin of fax parameter for facsys
***FACSYS is configured as printer, custom script and third party tool
***is used to fax it. Print control ZFAXS, ZFAXF are used in smartform
***to print fax number and other information which are retrieved by FACSYS's
***custom script and send the fax.
  LOOP AT gt_fax INTO gs_fax.
    lv_output-tdtelenum = gs_fax-fax_num. "p_vendor-telfx.
**Parameters regarding printer ZEFX
    lv_control-no_dialog = 'X'.
    lv_control-device = 'PRINTER'.
*lv_control-getotf = 'X'.
    lv_output-tddest = 'ZFAX'.
*LV_OUTPUT-TDPRINTER = 'ZFACSYS'.
    lv_output-tdimmed = 'X'. "Print Immediately
    lv_output-tdnewid = 'X'.
**End of FACSYS parameters
* Make sure the email gets sent (i.e. it is committed)
    lv_output-bcs_commit = c_true.
*Subject for Email
    lv_dd = p_laufd+6(2).
    lv_mm = p_laufd+4(2).
    lv_year =  p_laufd(4).
    CONCATENATE lv_year '/' lv_mm '/' lv_dd INTO lv_cdate.
    CONCATENATE 'Payment Remittance Advice ' lv_cdate lv_co
    gs_adrc-name1 INTO lv_output-tdtitle SEPARATED BY space.

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
      gs_no_email-bukrs = gv_company_code. "p_zbukr.
      gs_no_email-instance = sy-sysid.
      gs_no_email-phone = p_vendor-telf1.
      gs_no_email-lifnr = p_vendor-lifnr.
      gs_no_email-name1 = p_vendor-name1.
      CONCATENATE 'Unable to fax @' gs_fax-fax_num gs_no_email-text INTO
        gs_no_email-text SEPARATED BY space.
      APPEND gs_no_email TO gt_no_email.
    ELSE.
      gs_yes_email-lifnr = p_vendor-lifnr.
      gs_yes_email-name1 = p_vendor-name1.
      CONCATENATE 'Fax sent at ' gs_fax-fax_num INTO
                    gs_yes_email-text SEPARATED BY space.
      APPEND gs_yes_email TO gt_yes_email.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FAX_FORM
