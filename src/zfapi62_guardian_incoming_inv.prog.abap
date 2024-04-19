*&---------------------------------------------------------------------*
*& Report  ZFAPI60_GUARDIAN_INCOMING_INV
*&
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* 23/11/2018   AKMADASU   D30K929280   CHG0130577 - GEOAMPS email      *
*                                      subject and heading change      *
* 09/10/2019   SUBRAMS2   D30K930198   CHG0161543 payment method and   *
*                         D30K930236   payment method supplement based *
*                         D30K930238   on the incoming invoice date    *
* 26-Mar-2021 KOTTAPAN    D30K930927 CHG0208608 Add logic to allow     *
*                         one time vendor other than OTLAND also       *
* 14-Feb-2022 DADIM       D30K932022  CHG0236952 change posting date   *
*                         when document date greater than System date  *
* -----------  ----------  --------------------------------------------*

REPORT  zfapi62_guardian_incoming_inv MESSAGE-ID zs.

TYPES: BEGIN OF ty_data,
"       docno   TYPE i,
       col1(8),           "Batch Date
       col2 TYPE string,  "Batch ID
       col3 TYPE string,  " Batch Description
       col4 TYPE string,  " Currency
       col5 TYPE string,  " Due Date
       col6 TYPE string,  " Company code
       col7 TYPE string,  " Payment Metod
       col8 TYPE string,  " Payment Supplement
       col9 TYPE string, " Payment ID
       col10 TYPE string, " Payee Name
       col11 TYPE string, " Payee Name2
       col12 TYPE string, " Street Address
       col13 TYPE string, " City
       col14 TYPE string, " Postal Code
       col15 TYPE string, " Province Code
       col16 TYPE string, " Country Code
       col17 TYPE string, " Vendor Number
       col18 TYPE string, " Vendor Text
       col19 TYPE string, " Vendor Reference
       col20 TYPE string, " Account Reference
       col21 TYPE string, " Tax Code
       col22 TYPE string, " GL Amount
       col23 TYPE string, " Tax Amount
       col24 TYPE string, " Primary Cost Element
       col25 TYPE string, " Internal Order
       col26 TYPE string, " Tax PCE
       col27 TYPE string, " Contract# / Remarks
       col28 TYPE string, "WBS
       END OF ty_data.
TYPES: BEGIN OF ty_acc_data,
         docno  TYPE i,
         bldat  TYPE bkpf-bldat, "DOCUMENT DATE
         bukrs  TYPE bkpf-bukrs, "COMPANY CODE
         budat  TYPE bkpf-budat, "POSTING DATE
         waers  TYPE bkpf-waers, "CURRENCY
         xblnr  TYPE bkpf-xblnr, "Reference Document #
         bktxt  TYPE bkpf-bktxt, "HEADER TEXT
         blart  TYPE bkpf-blart, "DOCUMENT TYPE
         bschl_gl  TYPE bseg-bschl, "POSTING KEY GL
         zlsch  TYPE bseg-zlsch, " payment method
         uzawe  TYPE bseg-uzawe, "payment method supp
         "-----
         lifnr  TYPE bseg-lifnr,
         sgtxt_v  TYPE bseg-sgtxt, " LINE ITEM TEXT
         bschl_v  TYPE bseg-bschl, "POSTING KEY Vendor
         shkzg_v  TYPE bseg-shkzg,  "DR/CR indicator Vendor
         wrbtr_v  TYPE bapidoccur, "LOCAL CURRENCY AMOUNT Vendor
         name1 TYPE name1_gp,
         name2 TYPE name2_gp,
         name3 TYPE name3_gp,
         street TYPE stras_gp,
         city TYPE ort01_gp,
         postal_code TYPE pstlz,
         region TYPE regio,
         country_code TYPE land1_gp,
         "-----
         shkzg_gl  TYPE bseg-shkzg,  "DR/CR indicator Vendor
         wrbtr_gl  TYPE bapidoccur, "LOCAL CURRENCY AMOUNT
         wrbtr_tax  TYPE bapidoccur, "LOCAL CURRENCY AMOUNT TAX
*         valut  TYPE bseg-valut, "VALUE DATE
         saknr  TYPE bseg-saknr, "GL CODE
         saknr_tax  TYPE bseg-saknr, "GL CODE
*         kostl  TYPE bseg-kostl, "COST CENTER
         aufnr  TYPE bseg-aufnr, "INTERNAL ODER
*         nplnr  TYPE bseg-nplnr, "NETWORK
*         vornr  TYPE cobl-vornr, "ACTIVITY
*         projk24 TYPE prps-posid, "WBS 24 characters long
*         projk  TYPE bseg-projk, "WBS
*         zuonr  TYPE bseg-zuonr, "ASSIGNMENT
         sgtxt_gl  TYPE bseg-sgtxt, " LINE ITEM TEXT
         mwskz  TYPE bseg-mwskz, "Tax Code
*         menge  TYPE bseg-menge, "Qty
*         meins  TYPE bseg-meins, "Unit
         cont_rmks(200), "  type tdline,
         vendor_text TYPE tdline,
         wbs_element TYPE ps_posid,
       END OF ty_acc_data.
DATA:BEGIN OF gt_tab OCCURS 0,         "Text file format
           text1(1000),
     END OF gt_tab.
DATA: BEGIN OF gt_msg OCCURS 0,
           text1(100),
      END OF gt_msg.
TYPES: BEGIN OF ty_msg,
        msgrp TYPE char01,
        msgid TYPE symsgid,
        msgty TYPE symsgty,
        msgno TYPE symsgno,
        msg_text(250),
       END OF ty_msg.

DATA: gv_brgru TYPE t003-brgru,
*      gv_group TYPE apqi-groupid,
      gt_id_msg TYPE TABLE OF ty_msg,
      gs_id_msg LIKE LINE OF gt_id_msg.
DATA: gt_data TYPE TABLE OF ty_data,
      gs_data TYPE ty_data,
      gt_acc_data TYPE TABLE OF ty_acc_data,
      gs_acc_data TYPE ty_acc_data,
      gt_doc_data TYPE TABLE OF ty_acc_data,
      gs_doc_data TYPE ty_acc_data,
      gt_proc_data TYPE TABLE OF ty_acc_data,
      gt_nonproc_data TYPE TABLE OF ty_acc_data.

DATA: gv_cont_rmks TYPE tdline,
      gt_cont_rmks TYPE TABLE OF tdline,
      gs_docheader TYPE bapiache09,
      gt_gl_litems TYPE TABLE OF bapiacgl09,
      gs_gl_litems TYPE bapiacgl09,
      gt_cur_litems TYPE TABLE OF bapiaccr09,
      gs_cur_litems TYPE bapiaccr09,
      gs_customercpd TYPE bapiacpa09,
      gt_return TYPE TABLE OF bapiret2,
      gs_return TYPE bapiret2,
      gt_payable  TYPE TABLE OF bapiacap09,
      gs_payable TYPE bapiacap09,
      gv_sfile TYPE text1024, "rfpdo-rfbifile.
      gv_counter TYPE i.

DATA: "gv_nbr_file      TYPE numc5,             "File Sequence Number "
      gv_flag_err_proc TYPE flag,             "Flag-Errors-Process
      ge_mail   TYPE string,
      ge_name   TYPE string,
      ge_domain TYPE string.

CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP',
        gc_c             TYPE char1              "C / Character        "
                         VALUE 'C',
        gc_n             TYPE char1              "N / No-disply / Numerc
                         VALUE 'N',
        gc_x             TYPE char1              "X / Yes / True       "
                         VALUE 'X',
        gc_onetimevendor TYPE lfa1-lifnr VALUE 'OTLAND',
*       Begin of changes SUBRAMS2 CHG0161543
        gc_p       TYPE char1
                         VALUE 'P',
        gc_9       TYPE char1
                         VALUE '9',
        gc_la      TYPE char2
                        VALUE 'LA'.
*       End of changes SUBRAMS2 CHG0161543
*        gc_witht_03      TYPE lfbw-witht VALUE '03', "withhoding tax type
*        gc_witht_42      TYPE lfbw-witht VALUE '42', "withhoding tax type
*        gc_witht_fe      TYPE lfbw-witht VALUE 'FE', "withhoding tax type
*        gc_witht_fb      TYPE lfbw-witht VALUE 'FB'. "withhoding tax type

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X'
                                      USER-COMMAND cmd,
            p_sfile   LIKE        rfpdo-rfbifile MODIF ID srv, "dsp,
            p_arfile TYPE rfpdo-lboxfile MODIF ID srv, "dsp,
            p_lgfile TYPE rfpdo-lboxfile MODIF ID srv, "dsp.
            "p_ifile   LIKE        rfpdo-rfbifile MODIF ID srv,
            r_local   RADIOBUTTON GROUP rad1,
            p_file    TYPE        rfpdo-rfbifile DEFAULT 'H:\'
                                                  MODIF ID lcl.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
PARAMETERS:  p_blart TYPE bkpf-blart DEFAULT 'GD', " OBLIGATORY ,
             p_ident TYPE char04 DEFAULT 'GUAR' OBLIGATORY,
             p_zterm TYPE lfb1-zterm,
             p_email TYPE string DEFAULT
                    'SAHMAD@spectraenergy.com' OBLIGATORY,
             p_guard TYPE string OBLIGATORY,
             p_test TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.


AT SELECTION-SCREEN OUTPUT.
  PERFORM  toggle_functionality.

AT SELECTION-SCREEN ON p_email.
  ge_mail = p_email.
  FIND REGEX
  '^([a-z](?:\w|-|\.)*)@((\w|-|(\.?!\.)))+\.[[:alpha:]]{2,4}$'
  IN ge_mail
  IGNORING CASE
  SUBMATCHES ge_name ge_domain.

  IF sy-subrc <> 0.
    MESSAGE e000(zfi01) WITH
     'Enter correct email address for errors' '' '' ''.
    STOP.
  ENDIF.

AT SELECTION-SCREEN ON p_guard.
  ge_mail = p_guard.
  FIND REGEX
  '^([a-z](?:\w|-|\.)*)@((\w|-|(\.?!\.)))+\.[[:alpha:]]{2,4}$'
  IN ge_mail
  IGNORING CASE
  SUBMATCHES ge_name ge_domain.

  IF sy-subrc <> 0.
    MESSAGE e000(zfi01) WITH
     'Enter correct Guardian email address ' '' '' ''.
    STOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',TXT File,*.txt'
      static    = 'X'
    CHANGING
      file_name = p_file.

START-OF-SELECTION.

  CLEAR:  gv_flag_err_proc,
          gt_data,
          gt_acc_data,
          gt_doc_data,
          gt_proc_data,
          gt_nonproc_data,
          gt_msg,
          gt_tab,
          gt_id_msg.

  IF p_blart IS INITIAL.
    WRITE : / 'Input Document Type at selection screen.'.
    STOP.
  ENDIF.

  IF gv_flag_err_proc <> 'X'.
    "if user has no authorization, dont execute.
*    SELECT SINGLE brgru INTO gv_brgru FROM t003
*      WHERE blart = gc_blart.
*    AUTHORITY-CHECK OBJECT 'F_BKPF_BLA'
*             ID 'BRGRU' FIELD gv_brgru
*             ID 'ACTVT' FIELD '01'.
*    IF sy-subrc <> 0.
*      WRITE : / sy-uname, 'dont have access to run this interface'.
*      STOP.
*    ENDIF.
    "gv_targetpath.
    REFRESH: gt_tab,
             gt_msg.
    IF r_server = 'X'.
      IF p_sfile IS INITIAL.
        WRITE : / 'Please enter Application Server File Path/name.'.
        STOP.
      ENDIF.
      IF p_arfile IS INITIAL.
        WRITE : / 'Please enter Archive file Path.'.
        STOP.
      ENDIF.
      IF p_lgfile IS INITIAL.
        WRITE : / 'Please enter Log file Path.'.
        STOP.
      ENDIF.
    ENDIF.
    IF r_local = 'X' AND
       p_file IS INITIAL.
      WRITE : / 'Please enter Local PC file Path.'.
      STOP.
    ENDIF.
    IF r_server IS NOT INITIAL.
      PERFORM upload_file_server.
    ELSE.
      PERFORM upload_file_pc.
    ENDIF.
    PERFORM prepare_data_file.
    IF gt_doc_data[] IS INITIAL.
      gv_flag_err_proc = 'X'.
      gs_id_msg-msgrp = 'G'.
      gs_id_msg-msgid = 'ZFI01'.
      gs_id_msg-msgty = 'E'.
      gs_id_msg-msgno = '000'.
      MOVE text-035 TO gs_id_msg-msg_text.
      APPEND gs_id_msg TO gt_id_msg.
      CLEAR gs_id_msg.
    ENDIF.
    IF gv_flag_err_proc <> 'X'.
      "-----------
      PERFORM validate_post_data.
      "-----------
      IF p_test IS NOT INITIAL.
        SKIP 1.
        WRITE : / 'Interface is completed in TEST Mode..'.
      ELSE.
*        "Not in test mode, then archive the file
        IF r_server IS NOT INITIAL.
          PERFORM  archive_file.
        ENDIF.
      ENDIF.
    ELSE.
      gs_id_msg-msgrp = 'G'.
      gs_id_msg-msgid = 'ZFI01'.
      gs_id_msg-msgty = 'E'.
      gs_id_msg-msgno = '000'.
      MOVE text-036 TO gs_id_msg-msg_text.
      APPEND gs_id_msg TO gt_id_msg.
      CLEAR gs_id_msg.
    ENDIF. "gv_flag_err_proc
  ELSE.
    gs_id_msg-msgrp = 'G'.
    gs_id_msg-msgid = 'ZFI01'.
    gs_id_msg-msgty = 'E'.
    gs_id_msg-msgno = '000'.
    MOVE text-036 TO gs_id_msg-msg_text.
    APPEND gs_id_msg TO gt_id_msg.
    CLEAR gs_id_msg.
  ENDIF. "gv_flag_err_proc
  "Withholding tax update for Park document
*  IF gt_park_data[] IS NOT INITIAL.
*    PERFORM submit_rfwt0010.
*  ENDIF.
  "------------Output messages
  IF p_test IS INITIAL.
    PERFORM email_log.
  ENDIF.
  PERFORM display_messages.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE_SERVER
*&---------------------------------------------------------------------*
*       Upload file from Application Server
*----------------------------------------------------------------------*
FORM upload_file_server .

  DATA: ls_msg TYPE ty_msg.

  gv_sfile = p_sfile.
  OPEN DATASET gv_sfile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'E'.
    ls_msg-msgno = '000'.
    MOVE text-033 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    CONCATENATE 'Input File: ' gv_sfile INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    gv_flag_err_proc = 'X'.
    EXIT.
  ENDIF.
  DO.
    CLEAR: gs_data,
           gt_tab.
    READ DATASET gv_sfile INTO gt_tab-text1.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    PERFORM split_data USING gt_tab
                             gs_data.
    APPEND gs_data TO gt_data.
  ENDDO.
  CLOSE DATASET gv_sfile.
  IF sy-subrc <> 0.
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'E'.
    ls_msg-msgno = '000'.
    MOVE text-034 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    CONCATENATE 'Input File: ' gv_sfile INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    gv_flag_err_proc = 'X'.
    EXIT.
  ENDIF.
ENDFORM.                    " UPLOAD_FILE_SERVER
*&---------------------------------------------------------------------*
*&      Form  SPLIT_DATA
*&---------------------------------------------------------------------*
*       Split data to appropriate column
*----------------------------------------------------------------------*
FORM split_data  USING    p_gt_tab STRUCTURE gt_tab
                          p_output STRUCTURE gs_data.

  SPLIT p_gt_tab AT cl_abap_char_utilities=>horizontal_tab
  INTO p_output-col1
       p_output-col2
       p_output-col3
       p_output-col4
       p_output-col5
       p_output-col6
       p_output-col7
       p_output-col8
       p_output-col9
       p_output-col10
       p_output-col11
       p_output-col12
       p_output-col13
       p_output-col14
       p_output-col15
       p_output-col16
       p_output-col17
       p_output-col18
       p_output-col19
       p_output-col20
       p_output-col21
       p_output-col22
       p_output-col23
       p_output-col24
       p_output-col25
       p_output-col26
       p_output-col27
       p_output-col28.
ENDFORM.                    " SPLIT_DATA
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE_PC
*&---------------------------------------------------------------------*
*       Upload file from PC
*----------------------------------------------------------------------*
FORM upload_file_pc .

  DATA:   lt_auszug TYPE STANDARD TABLE OF string.
  DATA:   lv_auszug_file TYPE string,
          ls_msg TYPE ty_msg.

  REFRESH lt_auszug[].
  lv_auszug_file = p_file.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename            = lv_auszug_file
      has_field_separator = 'X'
      filetype            = 'ASC'
    CHANGING
      data_tab            = gt_data "lt_auszug
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
      CONCATENATE 'File Open error in file' p_file+2
            'in disk drive' p_file+0(2) INTO ls_msg-msg_text
            SEPARATED BY space.
      APPEND ls_msg TO gt_id_msg.
      gv_flag_err_proc = 'X'.
    WHEN 2.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      CONCATENATE 'Read error in file' p_file+2
       'in disk drive' p_file+0(2) INTO ls_msg-msg_text
       SEPARATED BY space.
      APPEND ls_msg TO gt_id_msg.
      gv_flag_err_proc = 'X'.
    WHEN 18.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      CONCATENATE 'Read error in file' p_file+2
       'in disk drive' p_file+0(2) INTO ls_msg-msg_text
       SEPARATED BY space.
      APPEND ls_msg TO gt_id_msg.
      gv_flag_err_proc = 'X'.
  ENDCASE.

ENDFORM.                    " UPLOAD_FILE_PC
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*      Prepare file data for validating / posting.
*Record type = 1 for Header, 2 for Vendor and 3 for GL line item
*There should be only ONE "One time vendor" in a document.
*As One time vendor data goes to BAPI header, one ata time.
*If there are multiple one time vendor then there should be multiple
*header and GL lines to match one time vendor lines.
*----------------------------------------------------------------------*
FORM prepare_data_file.

  DATA: lv_counter TYPE i,
        lv_docno   TYPE i,
        ls_tbsl    TYPE tbsl.

  LOOP AT gt_data INTO gs_data.
    lv_counter = lv_counter + 1.
*    "skip first line (column header)
*    IF lv_counter = 1.
*      CONTINUE.
*    ENDIF.
    CLEAR:  ls_tbsl, gs_acc_data.
    CONDENSE gs_data-col1.
    CONDENSE gs_data-col2.
    CONDENSE gs_data-col3.
    CONDENSE gs_data-col4.
    CONDENSE gs_data-col5.
    CONDENSE gs_data-col6.
    CONDENSE gs_data-col7.
    CONDENSE gs_data-col8.
    CONDENSE gs_data-col9.
    CONDENSE gs_data-col10.
    CONDENSE gs_data-col11.
    CONDENSE gs_data-col12.
    CONDENSE gs_data-col13.
    CONDENSE gs_data-col14.
    CONDENSE gs_data-col15.
    CONDENSE gs_data-col16.
    CONDENSE gs_data-col17.
    CONDENSE gs_data-col18.
    CONDENSE gs_data-col19.
    CONDENSE gs_data-col20.
    CONDENSE gs_data-col21.
    CONDENSE gs_data-col22.
    CONDENSE gs_data-col23.
    CONDENSE gs_data-col24.
    CONDENSE gs_data-col25.
    CONDENSE gs_data-col26.
    CONDENSE gs_data-col27.
    CONDENSE gs_data-col28.

    lv_docno = lv_docno + 1.
    gs_acc_data-blart  = p_blart.
    gs_acc_data-docno =  lv_docno.
    "CONCATENATE p_ident '-' gs_data-col19 gs_data-col20
    CONCATENATE p_ident '-' gs_data-col19
                            INTO gs_acc_data-xblnr.
    gs_acc_data-waers = gs_data-col4.
    CONCATENATE gs_data-col2 gs_data-col3 INTO gs_acc_data-bktxt
                                          SEPARATED BY '-'. "space.
*    gs_acc_data-bktxt = gs_data-col3.
    gs_acc_data-bldat = gs_data-col5.
    gs_acc_data-budat = sy-datum. "gs_data-col5.
    gs_acc_data-bukrs = gs_data-col6.
    gs_acc_data-zlsch = gs_data-col7. " payment method
    gs_acc_data-uzawe = gs_data-col8. " payment method supp
    gs_acc_data-name1 = gs_data-col10.
    gs_acc_data-name2 = gs_data-col11.
    "    gs_acc_data-name3 = gs_data-col27.
    gs_acc_data-street = gs_data-col12.
    gs_acc_data-city = gs_data-col13.
    gs_acc_data-postal_code = gs_data-col14.
    gs_acc_data-region = gs_data-col15.
    gs_acc_data-country_code = gs_data-col16.
    gs_acc_data-lifnr = gs_data-col17.
    gs_acc_data-vendor_text = gs_data-col18.
    CONCATENATE gs_data-col9 '-' gs_data-col10 gs_data-col11 INTO gs_acc_data-sgtxt_v.
    gs_acc_data-mwskz = gs_data-col21.
    IF gs_acc_data-lifnr IS NOT INITIAL.
      PERFORM conversion_routine CHANGING gs_acc_data-lifnr.
    ENDIF.
    gs_acc_data-wrbtr_gl = gs_data-col22.
    gs_acc_data-wrbtr_tax = gs_data-col23.
    gs_acc_data-saknr = gs_data-col24.
    gs_acc_data-aufnr = gs_data-col25.
    gs_acc_data-saknr_tax = gs_data-col26.
    gs_acc_data-wrbtr_v = gs_acc_data-wrbtr_tax + gs_acc_data-wrbtr_gl.
    gs_acc_data-cont_rmks = gs_data-col27.
    gs_acc_data-wbs_element = gs_data-col28.
    CONCATENATE gs_data-col9 gs_data-col10 INTO gs_acc_data-sgtxt_gl
                                         SEPARATED BY space.
    TRANSLATE gs_acc_data-saknr TO UPPER CASE.
    TRANSLATE gs_acc_data-saknr_tax TO UPPER CASE.
    IF gs_acc_data-saknr IS NOT INITIAL.
      PERFORM conversion_routine CHANGING gs_acc_data-saknr.
    ENDIF.
    IF gs_acc_data-saknr_tax IS NOT INITIAL.
      PERFORM conversion_routine CHANGING gs_acc_data-saknr_tax.
    ENDIF.
    IF gs_acc_data-aufnr IS NOT INITIAL.
      PERFORM conversion_routine CHANGING gs_acc_data-aufnr.
    ENDIF.
    IF gs_acc_data-wbs_element IS NOT INITIAL.
      PERFORM conversion_wbs CHANGING gs_acc_data-wbs_element.
    ENDIF.
    "------------------
    APPEND gs_acc_data TO gt_doc_data. "gt_acc_data.
  ENDLOOP.
*  break sahmad.
ENDFORM.                    " PREPARE_DATA
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_ROUTINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM conversion_routine  CHANGING iv_var TYPE clike.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = iv_var
    IMPORTING
      output = iv_var.

ENDFORM.                    " CONVERSION_ROUTINE
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_POST_DATA
*&---------------------------------------------------------------------*
*In test mode Validate only
*Post data if not running test mode.
*----------------------------------------------------------------------*
FORM validate_post_data .
  DATA:     lv_itemno_acc TYPE bapiaccr08-itemno_acc,
            lv_docno TYPE i,
            ls_lfb1 TYPE lfb1.



  CLEAR: gs_docheader,
         gt_gl_litems,
         gs_gl_litems,
         gt_cur_litems,
         gs_cur_litems,
         gt_return,
         gs_return,
         gt_id_msg,
         gs_id_msg,
         gt_acc_data,
         gt_nonproc_data,
         gt_proc_data,
         gt_payable,
         gs_payable,
         gs_customercpd.

  SORT gt_doc_data BY docno. "xblnr bktxt bukrs bldat budat waers blart lifnr.

* Start of Added by KOTTAPAN for CHG0208608on 26.03.21
  TYPES : BEGIN OF ty_lfa1,
            lifnr TYPE lifnr,
            land1 TYPE land1_gp,
            name1	TYPE name1_gp,
            name2	TYPE name2_gp,
            ort01	TYPE ort01_gp,
            pstlz	TYPE pstlz,
            regio TYPE regio,
            stras	TYPE stras_gp,
          END OF ty_lfa1.
  DATA: lt_lfa1 TYPE TABLE OF ty_lfa1,
        ls_lfa1 TYPE ty_lfa1.

  IF gt_doc_data IS NOT INITIAL.
    SELECT lifnr
           land1
           name1
           name2
           ort01
           pstlz
           regio
           stras FROM lfa1 INTO TABLE lt_lfa1
           FOR ALL ENTRIES IN gt_doc_data
           WHERE lifnr = gt_doc_data-lifnr.
  ENDIF.
* End of Add by KOTTAPAN for CHG0208608 on 26.03.21

  LOOP AT gt_doc_data INTO gs_doc_data.
    CLEAR: lv_itemno_acc,
           gs_docheader,
           gs_cur_litems,
           gt_cur_litems,
           gt_gl_litems,
           gs_gl_litems,
           gt_payable,
           gs_payable,
           gs_customercpd,
           gv_cont_rmks,
           gt_cont_rmks.
*Load header data
    gs_docheader-obj_type = 'BKPFF'.
    gs_docheader-username = sy-uname.
    gs_docheader-header_txt = gs_doc_data-bktxt.
    gs_docheader-comp_code = gs_doc_data-bukrs.
    gs_docheader-doc_date = gs_doc_data-bldat.
    gs_docheader-pstng_date = gs_doc_data-budat.
    gs_docheader-doc_type = gs_doc_data-blart.
    gs_docheader-ref_doc_no = gs_doc_data-xblnr.
* Status set to '2' to park the documentin the BAPI
    gs_docheader-doc_status = space.
    "Vendor
    "Add Creditor line / vendor line
    lv_itemno_acc = lv_itemno_acc + 1.
    gs_payable-itemno_acc    = lv_itemno_acc.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_doc_data-lifnr
      IMPORTING
        output = gs_doc_data-lifnr.
    CLEAR: ls_lfb1.
    SELECT SINGLE * FROM lfb1 INTO ls_lfb1
      WHERE lifnr = gs_doc_data-lifnr
        AND bukrs = gs_doc_data-bukrs.
    gs_payable-vendor_no     = gs_doc_data-lifnr.
    CONDENSE ls_lfb1-zwels NO-GAPS.
    IF p_zterm IS INITIAL.
      gs_payable-pmnttrms = ls_lfb1-zterm.
    ELSE.
      gs_payable-pmnttrms = p_zterm.
    ENDIF.
*    gs_payable-tax_code  = gs_doc_data-mwskz.
*    Begin of changes SUBRAMS2 CHG0161543
*    gs_payable-pymt_meth = gs_doc_data-zlsch. "ls_lfb1-zwels(1).
*    gs_payable-pmtmthsupl = gs_doc_data-uzawe.
    IF gs_doc_data-lifnr = gc_onetimevendor OR "  Added by KOTTAPAN for CHG0208608 26.03.21
       gs_doc_data-lifnr IS INITIAL.
      IF gs_doc_data-bldat LE sy-datum.
        gs_payable-pymt_meth  = gc_9.  "9
      ELSEIF gs_doc_data-bldat GT sy-datum.
        gs_payable-pymt_meth  = gc_p.  "P
        gs_docheader-pstng_date = gs_doc_data-bldat.
      ENDIF.
      gs_payable-pmtmthsupl = gc_la.  "LA
* Start of Added by KOTTAPAN for CHG0208608 on 26.03.21
    ELSE.
      gs_payable-pymt_meth  = ls_lfb1-zwels.
      gs_payable-pmtmthsupl = ls_lfb1-uzawe.  "LA
    ENDIF.
* End of Added by KOTTAPAN for CHG0208608on 26.03.21
*    End of changes SUBRAMS2 CHG0161543
*Start of change by DADIM for CHG0236952
    IF gs_doc_data-bldat GT sy-datum.
      gs_docheader-pstng_date = gs_doc_data-bldat.
    ENDIF.
*End of change by DADIM for CHG0236952

    gs_payable-item_text = gs_doc_data-sgtxt_v.
    APPEND gs_payable TO gt_payable.
    "------------One time vendor
    IF gs_doc_data-lifnr = gc_onetimevendor OR
       gs_doc_data-lifnr IS INITIAL.
      gs_customercpd-name = gs_doc_data-name1.
      gs_customercpd-name_2 = gs_doc_data-name2.
      "gs_customercpd-name_3 = gs_doc_data-name3.
      gs_customercpd-postl_code = gs_doc_data-postal_code.
      gs_customercpd-city = gs_doc_data-city.
      gs_customercpd-country = gs_doc_data-country_code.
      gs_customercpd-street = gs_doc_data-street.
      gs_customercpd-region = gs_doc_data-region.
* Start of Added by KOTTAPAN for CHG0208608on 26.03.21
    ELSE.
      READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY lifnr = gs_doc_data-lifnr.
      IF sy-subrc = 0.
        IF gs_doc_data-name1 IS NOT INITIAL.
          gs_customercpd-name = gs_doc_data-name1.
        ELSE.
          gs_customercpd-name = ls_lfa1-name1.
        ENDIF.
        IF gs_doc_data-name2 IS NOT INITIAL.
          gs_customercpd-name_2 = gs_doc_data-name2.
        ELSE.
          gs_customercpd-name_2 = ls_lfa1-name2.
        ENDIF.
        "gs_customercpd-name_3 = gs_doc_data-name3.
        IF gs_doc_data-postal_code IS NOT INITIAL.
          gs_customercpd-postl_code = gs_doc_data-postal_code.
        ELSE.
          gs_customercpd-postl_code = ls_lfa1-pstlz.
        ENDIF.
        IF gs_doc_data-city IS NOT INITIAL.
          gs_customercpd-city = gs_doc_data-city.
        ELSE.
          gs_customercpd-city = ls_lfa1-ort01.
        ENDIF.
        IF gs_doc_data-country_code IS NOT INITIAL.
          gs_customercpd-country = gs_doc_data-country_code.
        ELSE.
          gs_customercpd-country = ls_lfa1-land1.
        ENDIF.
        IF gs_doc_data-street IS NOT INITIAL.
          gs_customercpd-street = gs_doc_data-street.
        ELSE.
          gs_customercpd-street = ls_lfa1-stras.
        ENDIF.
        IF gs_customercpd-region IS NOT INITIAL.
          gs_customercpd-region = gs_doc_data-region.
        ELSE.
          gs_customercpd-region = ls_lfa1-regio.
        ENDIF.
      ENDIF.
* End of Add by KOTTAPAN for CHG0208608 on 26.03.21
    ENDIF.
    "Vendor Curreny
    CLEAR gs_cur_litems.
    gs_cur_litems-itemno_acc = lv_itemno_acc.
    gs_cur_litems-currency   = gs_doc_data-waers.
    gs_cur_litems-amt_doccur = ( gs_doc_data-wrbtr_v * -1 ).
    APPEND gs_cur_litems TO gt_cur_litems.
    "------------GL
    lv_itemno_acc = lv_itemno_acc + 1.
*     Load Account GL line segment
    gs_gl_litems-itemno_acc    = lv_itemno_acc.
    gs_gl_litems-gl_account    = gs_doc_data-saknr.
    gs_gl_litems-comp_code     = gs_doc_data-bukrs.
*   Begin of changes SUBRAMS2 CHG0161543
*    gs_gl_litems-pstng_date    = gs_doc_data-budat.
    IF gs_doc_data-bldat LE sy-datum.
      gs_gl_litems-pstng_date    = gs_doc_data-budat.
    ELSEIF gs_doc_data-bldat GT sy-datum.
      gs_gl_litems-pstng_date    = gs_doc_data-bldat.
    ENDIF.
*   End of changes SUBRAMS2 CHG0161543
    gs_gl_litems-ref_key_1     = gs_doc_data-xblnr. "+3(12).
    gs_gl_litems-item_text     = gs_doc_data-sgtxt_gl.
    gs_gl_litems-orderid       = gs_doc_data-aufnr.
    gs_gl_litems-tax_code  = gs_doc_data-mwskz.
    gs_gl_litems-wbs_element = gs_doc_data-wbs_element.
    APPEND gs_gl_litems TO gt_gl_litems.
*       Load GL currency line segment
    CLEAR gs_cur_litems.
    gs_cur_litems-itemno_acc    = lv_itemno_acc.
    gs_cur_litems-currency      = gs_doc_data-waers.
    "amount can be negative / positive as comes from input file
    gs_cur_litems-amt_doccur = gs_doc_data-wrbtr_gl.
    gs_cur_litems-amt_base = gs_doc_data-wrbtr_gl.
    APPEND gs_cur_litems TO gt_cur_litems.
    "------------Tax
    CLEAR: gs_cur_litems,
           gs_gl_litems.
    lv_itemno_acc = lv_itemno_acc + 1.
    gs_gl_litems-itemno_acc    = lv_itemno_acc.
    gs_gl_litems-gl_account    = gs_doc_data-saknr_tax.
    gs_gl_litems-comp_code     = gs_doc_data-bukrs.
*   Begin of changes SUBRAMS2 CHG0161543
*    gs_gl_litems-pstng_date    = gs_doc_data-budat.
    IF gs_doc_data-bldat LE sy-datum.
      gs_gl_litems-pstng_date    = gs_doc_data-budat.
    ELSEIF gs_doc_data-bldat GT sy-datum.
      gs_gl_litems-pstng_date    = gs_doc_data-bldat.
    ENDIF.
*   End of changes SUBRAMS2 CHG0161543
    gs_gl_litems-ref_key_1     = gs_doc_data-xblnr.
    gs_gl_litems-item_text     = gs_doc_data-sgtxt_gl.
*    gs_gl_litems-tax_code  = gs_doc_data-mwskz.
    "gs_gl_litems-orderid       = gs_doc_data-aufnr.
    APPEND gs_gl_litems TO gt_gl_litems.
*       Load Tax currency line segment
    CLEAR gs_cur_litems.
    gs_cur_litems-itemno_acc    = lv_itemno_acc.
    gs_cur_litems-currency      = gs_doc_data-waers.
    "amount can be negative / positive as comes from input file
    gs_cur_litems-amt_doccur = gs_doc_data-wrbtr_tax.
    gs_cur_litems-amt_base = gs_doc_data-wrbtr_tax.
    APPEND gs_cur_litems TO gt_cur_litems.
    "long text update
    gv_cont_rmks = gs_doc_data-vendor_text.
    IF gv_cont_rmks IS NOT INITIAL.
      APPEND gv_cont_rmks TO gt_cont_rmks.
    ENDIF.
    gv_cont_rmks = gs_doc_data-cont_rmks(40).
    IF gv_cont_rmks IS NOT INITIAL.
      APPEND gv_cont_rmks TO gt_cont_rmks.
    ENDIF.
    gv_cont_rmks = gs_doc_data-cont_rmks+40(40).
    IF gv_cont_rmks IS NOT INITIAL.
      APPEND gv_cont_rmks TO gt_cont_rmks.
    ENDIF.
    gv_cont_rmks = gs_doc_data-cont_rmks+80(40).
    IF gv_cont_rmks IS NOT INITIAL.
      APPEND gv_cont_rmks TO gt_cont_rmks.
    ENDIF.
    gv_cont_rmks = gs_doc_data-cont_rmks+120(40).
    IF gv_cont_rmks IS NOT INITIAL.
      APPEND gv_cont_rmks TO gt_cont_rmks.
    ENDIF.
******Re-use gt_acc_data internal table to keep data
******of current loop.In case of Error, we need to update custom table
    APPEND gs_doc_data TO gt_acc_data.

    "validate / post data
    PERFORM bapi_validate_post.
  ENDLOOP.

ENDFORM.                    " VALIDATE_DATA
*&---------------------------------------------------------------------*
*&      Form  BAPI_VALIDATE_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bapi_validate_post.
  DATA: lv_first TYPE xfeld,
        lv_format TYPE bapitga-textformat VALUE 'ASC',
        ls_skb1 TYPE skb1,
        lv_error TYPE xfeld,
        ls_return TYPE bapiret2.
  DATA: lv_belnr TYPE bkpf-belnr,
        lv_gjahr TYPE bkpf-gjahr,
        lv_id TYPE thead-tdid VALUE '0003',
       lv_language TYPE thead-tdspras,
       lv_name TYPE thead-tdname,
       lv_object TYPE thead-tdobject VALUE 'BELEG',
       ls_lines TYPE tline,
       lt_lines TYPE TABLE OF tline.

  CLEAR gt_return.
  IF p_test IS NOT INITIAL. "running in test mode
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader    = gs_docheader
        customercpd       = gs_customercpd
*       CONTRACTHEADER    =
      TABLES
        accountgl         = gt_gl_litems
*       accountreceivable =
        accountpayable    = gt_payable
                                                                                                                         " accounttax        = gt_accounttax
        currencyamount    = gt_cur_litems
*       accountwt         = gt_whtax
*       criteria          =
*       valuefield        =
*       EXTENSION1        =
        return            = gt_return.
*         PAYMENTCARD             =
*         CONTRACTITEM            =
*         EXTENSION2              =
*         REALESTATE              =
*         ACCOUNTWT               =

    CLEAR: gs_id_msg.

    LOOP AT gt_return INTO gs_return.
      CLEAR: gs_id_msg.
      IF gs_return-type = 'E' OR
         gs_return-type = 'A'.
        IF lv_first IS INITIAL.
          MOVE text-010 TO gs_id_msg-msg_text.
          APPEND gs_id_msg TO gt_id_msg.
          CONCATENATE gs_docheader-comp_code
                      gs_docheader-doc_type
                      gs_docheader-doc_date
                      gs_docheader-pstng_date
                      gs_docheader-fis_period
                      gs_docheader-ref_doc_no
                      gs_docheader-header_txt
                 INTO gs_id_msg-msg_text SEPARATED BY space.
          APPEND gs_id_msg TO gt_id_msg.
          lv_first = 'X'.
        ENDIF.
        CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          id                 = gs_return-id
          number             = gs_return-number
          language           = sy-langu
          textformat         = lv_format
*            LINKPATTERN        =
          message_v1         = gs_return-message_v1
          message_v2         = gs_return-message_v2
          message_v3         = gs_return-message_v3
          message_v4         = gs_return-message_v4
*             LANGUAGE_ISO       =
        IMPORTING
          message            = gs_return-message
*           RETURN             =
*         TABLES
*           TEXT               =
     .
        gs_id_msg-msgty = gs_return-type.
        gs_id_msg-msgid = gs_return-id.
        gs_id_msg-msgno = gs_return-number.
        CONCATENATE gs_return-number '-'
                    gs_return-message
               INTO gs_id_msg-msg_text
           SEPARATED BY space.
        APPEND gs_id_msg TO gt_id_msg.
        lv_error = 'X'.
      ENDIF.
    ENDLOOP.
    "------------
  ELSE. "Not in test mode--Post
    "------------
    CLEAR: gt_return,
           lv_first,
           lv_belnr,
           lv_gjahr.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
   EXPORTING
     documentheader    = gs_docheader
     customercpd       = gs_customercpd
*    IMPORTING
*      OBJ_KEY           = lv_obj_key
   TABLES
     accountgl         = gt_gl_litems
     "accounttax        = gt_accounttax
*      ACCOUNTRECEIVABLE =
     accountpayable    = gt_payable
     currencyamount    = gt_cur_litems
*      EXTENSION1        =
*     accountwt         = gt_whtax
     return            = gt_return.
    LOOP AT gt_return INTO gs_return.
      CLEAR: gs_id_msg.
      IF lv_first IS INITIAL.
*          MOVE text-011 TO gs_id_msg-msg_text.
*          APPEND gs_id_msg TO gt_id_msg.
        CONCATENATE gs_docheader-comp_code
                    gs_docheader-doc_type
                    gs_docheader-doc_date
                    gs_docheader-pstng_date
                    gs_docheader-fis_period
                    gs_docheader-ref_doc_no
                    gs_docheader-header_txt
                    INTO gs_id_msg-msg_text SEPARATED BY space.
        APPEND gs_id_msg TO gt_id_msg.
        lv_first = 'X'.
      ENDIF.
      IF gs_return-type = 'E' OR
         gs_return-type = 'A'.
        lv_error = 'X'.
      ELSE.

      ENDIF.
      IF gs_return-type = 'S' AND
         gs_return-id = 'RW' AND
         gs_return-number = '605' AND
         lv_belnr IS INITIAL.
        lv_belnr = gs_return-message_v2(10).
        lv_gjahr = gs_return-message_v2+14(4).
      ENDIF.
      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          id                 = gs_return-id
          number             = gs_return-number
          language           = sy-langu
          textformat         = lv_format
*        LINKPATTERN        =
          message_v1         = gs_return-message_v1
          message_v2         = gs_return-message_v2
          message_v3         = gs_return-message_v3
          message_v4         = gs_return-message_v4
*                LANGUAGE_ISO       =
        IMPORTING
          message            = gs_return-message
*                RETURN             =
*              TABLES
*                TEXT               =
         .
      gs_id_msg-msgty = gs_return-type.
      gs_id_msg-msgid = gs_return-id.
      gs_id_msg-msgno = gs_return-number.
      CONCATENATE gs_return-number '-'
                  gs_return-message
             INTO gs_id_msg-msg_text
         SEPARATED BY space.
      APPEND gs_id_msg TO gt_id_msg.
    ENDLOOP.
*In case of error, collect data to into error table
    IF lv_error IS NOT INITIAL.
      APPEND LINES OF gt_acc_data TO gt_nonproc_data.
    ELSE.
      CLEAR ls_return.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = ls_return.
      "------------------
      APPEND LINES OF gt_acc_data TO gt_proc_data.
      "----------------
      IF gt_cont_rmks[] IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_belnr
          IMPORTING
            output = lv_belnr.

        "update Long text
        "UGL 01030111492017
        CLEAR: lt_lines,
                gs_id_msg.
        CONCATENATE gs_docheader-comp_code lv_belnr lv_gjahr
                              INTO lv_name RESPECTING BLANKS.
        LOOP AT gt_cont_rmks INTO gv_cont_rmks.
          ls_lines-tdformat = '*'.
          ls_lines-tdline = gv_cont_rmks.
          APPEND ls_lines TO lt_lines.
        ENDLOOP.
        CALL FUNCTION 'CREATE_TEXT'
          EXPORTING
            fid       = lv_id
            flanguage = sy-langu
            fname     = lv_name
            fobject   = lv_object
          TABLES
            flines    = lt_lines
          EXCEPTIONS
            no_init   = 1
            no_save   = 2
            OTHERS    = 3.
        CASE sy-subrc.
          WHEN 1 OR 3.
            CONCATENATE 'Creation of text did not work'
                    lv_belnr
                    gs_docheader-comp_code
                    gs_docheader-doc_type
                    gs_docheader-doc_date
                    gs_docheader-pstng_date
                    gs_docheader-fis_period
                    gs_docheader-ref_doc_no
                    gs_docheader-header_txt
             INTO gs_id_msg-msg_text SEPARATED BY space.
            APPEND gs_id_msg TO gt_id_msg.
          WHEN 2.
            CONCATENATE 'Saving of the text did not work'
                    lv_belnr
                    gs_docheader-comp_code
                    gs_docheader-doc_type
                    gs_docheader-doc_date
                    gs_docheader-pstng_date
                    gs_docheader-fis_period
                    gs_docheader-ref_doc_no
                    gs_docheader-header_txt
             INTO gs_id_msg-msg_text SEPARATED BY space.
            APPEND gs_id_msg TO gt_id_msg.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR gt_acc_data.
ENDFORM.                    " BAPI_VALIDATE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
*Display Message at screen (spool)
*Generate Log if not running in test mode
*----------------------------------------------------------------------*
FORM display_messages .
  DATA: lv_logfile TYPE zfit_xparam-value1,
        lv_error TYPE xfeld.

  CONCATENATE p_lgfile 'Log File_' sy-datum '_' sy-uzeit '.LOG'
                                          INTO lv_logfile.
  SKIP 2.
  LOOP AT gt_id_msg INTO gs_id_msg.
    "print for spool
    WRITE: / "gs_id_msg-msgid,
             "gs_id_msg-msgty,
             "gs_id_msg-msgno,
             gs_id_msg-msg_text.
  ENDLOOP.
  "download to Log location
  IF p_test IS INITIAL AND
     lv_logfile IS NOT INITIAL AND
     r_server IS NOT INITIAL.
    OPEN DATASET lv_logfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      WRITE: / 'Unable to generate log file..'.
      WRITE: / lv_logfile.
      lv_error = 'X'.
    ELSE.
      LOOP AT gt_id_msg INTO gs_id_msg.
        TRANSFER gs_id_msg TO lv_logfile.
        IF sy-subrc <> 0.
          lv_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.
      CLOSE DATASET lv_logfile.
      IF sy-subrc <> 0.
        lv_error = 'X'.
      ENDIF.
      IF lv_error IS INITIAL.
        WRITE : / 'Log file is generated..'.
        WRITE : / lv_logfile.
      ENDIF.
    ENDIF.
  ENDIF.
  SKIP 2.
  WRITE: / '*----------------End of messages -------------------*'.
ENDFORM.                    " DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_FILE
*&---------------------------------------------------------------------*
*       Archive input file from Application Server
*----------------------------------------------------------------------*
FORM archive_file.
  DATA: lv_source TYPE text1024,
        lv_target TYPE text1024,
        lv_data TYPE string,
        ls_msg TYPE ty_msg.
  "-----------------------------
  ls_msg-msgrp = 'G'.
  ls_msg-msgid = 'ZFI01'.
  ls_msg-msgty = 'I'.
  ls_msg-msgno = '000'.
  MOVE text-028 TO ls_msg-msg_text.
  APPEND ls_msg TO gt_id_msg.
  CLEAR ls_msg.
  lv_source = p_sfile.
  CONCATENATE p_sfile '_' sy-datum '_' sy-uzeit '.ARC' INTO  lv_target.
  OPEN DATASET lv_source FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    ls_msg-msgrp = 'G'.
    ls_msg-msg_text = text-011.
    APPEND ls_msg TO gt_id_msg.
    CONCATENATE 'Source File:' lv_source INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    EXIT.
  ENDIF.
  OPEN DATASET lv_target FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    ls_msg-msgrp = 'G'.
    ls_msg-msg_text = text-011.
    APPEND ls_msg TO gt_id_msg.
    CONCATENATE 'Archive File:' lv_target INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    EXIT.
  ENDIF.
*   copy the file from source to destination
  DO.
    READ DATASET lv_source INTO lv_data.
    IF sy-subrc EQ 0.
      TRANSFER lv_data TO lv_target.
      CLEAR lv_data.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
*   Delete the source file after copy
  DELETE DATASET lv_source.
*   Close all open dataset
  CLOSE DATASET: lv_source,
                 lv_target.
  IF sy-subrc <> 0.
  ELSE.
    CONCATENATE 'Source File' lv_source INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    CONCATENATE 'Target File: ' lv_target INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    MOVE text-030 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
  ENDIF.

  SKIP 2.
ENDFORM.                    " ARCHIVE_FILE
*&---------------------------------------------------------------------*
*&      Form  F_TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toggle_functionality .

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

ENDFORM.                    " TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  EMAIL_LOG
*&---------------------------------------------------------------------*
*       Email Log to FI and Guadian email address
*----------------------------------------------------------------------*
FORM email_log .
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
  PERFORM build_mail_content CHANGING lt_objtxt.

* Object with main text of the mail.
  lwa_objtxt = lv_space.
  APPEND lwa_objtxt TO lt_objtxt.
  CLEAR lwa_objtxt.

  DESCRIBE TABLE lt_objtxt LINES lv_lines.
**--Start of changes by akmadasu for CHG0130577
*  CONCATENATE text-005
  CONCATENATE text-048
**--end of changes by akmadasu for CHG0130577
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
  lwa_reclist-receiver   = p_guard.
  APPEND lwa_reclist TO lt_reclist.
**--start of changes by akmadasu for CHG0130577
  lwa_reclist-receiver   = 'ONTUGLLandsINQ@uniongas.com'.
  APPEND lwa_reclist TO lt_reclist.
**--end of changes by akmadasu for CHG0130577

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
    WRITE: /'log is emailed.'.

  ENDIF.
ENDFORM.                    " EMAIL_LOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
FORM build_mail_content  CHANGING p_lt_objtxt TYPE table.

  DATA: lwa_objtxt       TYPE solisti1,
        "lwa_email_data   TYPE ty_email_data,
        lv_lines TYPE n LENGTH 10,
        lv_wrbtr(14),
        lt_data TYPE TABLE OF ty_acc_data.

*Prepare HTML mail header
  CONCATENATE '<html>'
              '<body>'
              '<h4 style="font-family:arial"><caption><b><u>'
**--start of changes by akmadasu for CHG0130577
*              text-007
              text-049
**--end of changes by akmadasu for CHG0130577
              '</u></b><caption></h4>'
              '<ul>'
              '</ul>'
         INTO lwa_objtxt
         SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "-------------------
  CONCATENATE '<table border="2" width="100%">' ' '
              INTO lwa_objtxt
              SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "source is input file data
  DESCRIBE TABLE gt_data LINES lv_lines.
*  lv_lines = lv_lines - 1.
  CONCATENATE  '<tr> <td>'
                text-045 '</td><td>'
                lv_lines '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "------------
  MOVE: '</table> <br>' TO lwa_objtxt.
  APPEND: lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "-------------------------------
  CONCATENATE  '<tr> <td>'
                text-043 '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "-------------
  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td>'
              INTO lwa_objtxt
              SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  CONCATENATE text-014 '</td><td>'
              text-013 '</td><td>'
              text-015 '</td><td>'
              text-016 '</td><td>'
              text-017 '</td><td>'
              text-018 '</td><td>'
              text-019 '</td><td>'
              text-020 '</td><td>'
              text-010 '</td></tr>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  LOOP AT gt_nonproc_data INTO gs_acc_data.
    WRITE gs_acc_data-wrbtr_v TO lv_wrbtr DECIMALS 2.
    CONCATENATE  '<tr> <td>'
                gs_acc_data-bukrs '</td><td>'
                gs_acc_data-bldat '</td><td>'
                gs_acc_data-budat '</td><td>'
                gs_acc_data-waers '</td><td>'
                gs_acc_data-xblnr '</td><td>'
                gs_acc_data-bktxt '</td><td>'
                lv_wrbtr '</td><td>'
                gs_acc_data-lifnr '</td><td>'
                gs_acc_data-saknr '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.

    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
  ENDLOOP.

  MOVE: '</table> <br>' TO lwa_objtxt.
  APPEND: lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "----------------Log
  IF p_lgfile IS NOT INITIAL.
    CONCATENATE  '<tr> <td>'
                  text-044 '</td><td>'
                  p_lgfile '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.
    APPEND lwa_objtxt TO p_lt_objtxt.
  ENDIF.
  CLEAR: lwa_objtxt.
  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td>'
              INTO lwa_objtxt
              SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  LOOP AT gt_id_msg INTO gs_id_msg.
*    WRITE: /  gs_id_msg-msg_text.
    CONCATENATE  '<tr> <td>'
                sy-sysid '</td><td>'
                gs_id_msg-msg_text '</td></tr>'
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
ENDFORM.                    " BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_WBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM conversion_wbs  CHANGING iv_var TYPE clike.

  CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
    EXPORTING
      input  = iv_var
    IMPORTING
      output = iv_var.

ENDFORM.                    " CONVERSION_WBS
