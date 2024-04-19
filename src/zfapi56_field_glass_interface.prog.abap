*&---------------------------------------------------------------------*
*& Report  ZFAPI56_FIELD_GLASS_INTERFACE
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 05-Sep-2018  KBANERJEE   D30K929073  WH Tax Issue with Field Glass   *
*                                      interface                       *
* 04-Dec-2018  JRHARTUNG   D30K929335  CHG0131446  Conserve the column *
*                                      headings prior to sorting       *
*&---------------------------------------------------------------------*

REPORT  zfapi56_field_glass_interface MESSAGE-ID zs.


TYPES: BEGIN OF ty_data,
       col1(3),       " Invoice Line#
       col2(10),  " Document Date
       col3(16),  " Reference
       col4(3),   " Invoice Type
       col5(3),   " Invoice Type
       col6(5),   " Line Type
       col7(10),  " Posting Date
       col8(13),  " Quantity
       col9(3),   " UOM
       col10(13), " Unit Price
       col11(5),  " Currency
       col12(11), " VMS Tax %
       col13(12),  " Tax Amount
       col14(13), " Total Bill Rate
       col15(13), " Amount
       col16(25), " Header Text
       col17(50), " First Name (line item text)
       col18(50), " Last Name (Line Item text)
       col19(3),  "SAP ECC System code
       col20(50), "Cost Object (WBS/IO/Network/CC)
       col21(4),  "Task Code / Activity
       col22(4),  "Personnel Area
       col23(10), "Vendor code
       col24(1),  "Indicator for WH Tax
       col25(75), "GL Account info
       docno   TYPE i,
       END OF ty_data.
TYPES: BEGIN OF ty_acc_data,
         docno  TYPE i,
         itmno  TYPE i,
         source(1), "F = File, T=Table
         bldat  TYPE bkpf-bldat, "DOCUMENT DATE
         bukrs  TYPE bkpf-bukrs, "COMPANY CODE
         budat  TYPE bkpf-budat, "POSTING DATE
         waers  TYPE bkpf-waers, "CURRENCY
         xblnr  TYPE bkpf-xblnr, "Reference Document #
         bktxt  TYPE bkpf-bktxt, "HEADER TEXT
         blart  TYPE bkpf-blart, "DOCUMENT TYPE
         bschl  TYPE bseg-bschl, "POSTING KEY
         wrbtr  TYPE bseg-wrbtr, "LOCAL CURRENCY AMOUNT
         valut  TYPE bseg-valut, "VALUE DATE
         saknr  TYPE bseg-saknr, "GL CODE
         kostl  TYPE bseg-kostl, "COST CENTER
         aufnr  TYPE bseg-aufnr, "INTERNAL ODER
         nplnr  TYPE bseg-nplnr, "NETWORK
         vornr  TYPE cobl-vornr, "ACTIVITY
         projk24 TYPE prps-posid, "WBS 24 characters long
         projk  TYPE bseg-projk, "WBS
         zuonr  TYPE bseg-zuonr, "ASSIGNMENT
         sgtxt  TYPE bseg-sgtxt, " LINE ITEM TEXT
         mwskz  TYPE bseg-mwskz, "Tax Code
         shkzg  TYPE bseg-shkzg,  "DR/CR indicator
  "
         lifnr  TYPE bseg-lifnr,
         menge  TYPE bseg-menge, "Qty
         meins  TYPE bseg-meins, "Unit
  "
         invtype1(3),
         invtype2(3),
         linetype(5),
         fname(50), " First Name (line item text)
         lname(50), " Last Name (Line Item text)
         quantity TYPE bseg-menge,
         uom      TYPE bseg-meins,
         persa TYPE persa,
         costobject(50),
         tax_amount TYPE bset-hwste,
         tax_code   TYPE ztax_code,
         amount TYPE bseg-wrbtr,
         ecc_id(3),
         whtax(1),  "indicator for WHTAX, if 'X' then park document
*         unit_price type bseg-wrbtr,
*         tot_bill_rate type bseg-wrbtr,

       END OF ty_acc_data.
DATA:BEGIN OF gt_tab OCCURS 0,         "Text file format
           text1(208),
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
*CONSTANTS: gc_blart TYPE bkpf-blart VALUE 'PP',
*           gc_saknr TYPE bseg-saknr VALUE '0000251720'."Offset GL
DATA: gv_brgru TYPE t003-brgru,
      gv_group TYPE apqi-groupid,
      gt_id_msg TYPE TABLE OF ty_msg,
      gs_id_msg LIKE LINE OF gt_id_msg.
DATA: gt_data TYPE TABLE OF ty_data,
      gs_data TYPE ty_data,
      gt_acc_data TYPE TABLE OF ty_acc_data,
      gs_acc_data TYPE ty_acc_data,
      gt_doc_data TYPE TABLE OF ty_acc_data,
      gs_doc_data TYPE ty_acc_data,
      gt_proc_data TYPE TABLE OF ty_acc_data,
      gt_nonproc_data TYPE TABLE OF ty_acc_data,
      gt_park_data TYPE TABLE OF ty_acc_data,
      gt_nonpark_data TYPE TABLE OF ty_acc_data.

DATA: gs_docheader TYPE bapiache09,
      gt_gl_litems TYPE TABLE OF bapiacgl09,
      gs_gl_litems TYPE bapiacgl09,
      gt_cur_litems TYPE TABLE OF bapiaccr09,
      gs_cur_litems TYPE bapiaccr09,
      gt_cus_litems TYPE TABLE OF bapiacar09,
      gs_cus_litems TYPE bapiacar09,
      gt_return TYPE TABLE OF bapiret2,
      gs_return TYPE bapiret2,
      gt_accounttax TYPE TABLE OF bapiactx09,
      gs_accounttax TYPE bapiactx09,
      gt_payable  TYPE TABLE OF bapiacap09,
      gs_payable TYPE bapiacap09,
      gt_whtax TYPE TABLE OF bapiacwt09,
      gs_whtax TYPE bapiacwt09,
      gv_sfile TYPE text1024, "rfpdo-rfbifile.
      gv_counter TYPE i.
"Park document
DATA: gt_bkpf	TYPE TABLE OF	bkpf,
      gt_bseg	TYPE TABLE OF	bseg,
      gt_bsec	TYPE TABLE OF	bsec,
      gt_bset	TYPE TABLE OF	bset,
      gt_bsez	TYPE TABLE OF	bsez,
      gs_bkpf	TYPE bkpf,
      gs_bseg	TYPE bseg,
      gs_bsec	TYPE bsec,
      gs_bset	TYPE bset,
      gs_bsez	TYPE bsez.
DATA: gv_obj_id        TYPE char14,            "Object ID            "
      gv_flag_pres     TYPE flag,              "Flag-Presentatn Server
      gv_fdelim        TYPE char1.             "File Delimiter       "
DATA: gv_logfile_path TYPE zfit_xparam-value1,
      gv_logfile_name TYPE zfit_xparam-value1.
DATA: gv_nbr_file      TYPE numc5,             "File Sequence Number "
      gv_flag_err_proc TYPE flag,             "Flag-Errors-Process
      ge_mail   TYPE string,
      ge_name   TYPE string,
      ge_domain TYPE string.

*
TYPES: BEGIN OF ty_wa_file_list,                 "File List            "
        srvr_pres        TYPE flag,              "Presentation Server  "
        srvr_appl        TYPE flag,              "Application Server   "
        filename_fp_in   TYPE text1024,          "Input Filepath       "
        filename_fn_in   TYPE localfile,         "Input Filename       "
        filename_fp_arc  TYPE text1024,          "Archive Filepath     "
        filename_fn_arc  TYPE localfile,         "Archive Filename     "
        filename_fp_err  TYPE text1024,          "Error Filepath       "
        filename_fn_err  TYPE localfile,         "Error Filename       "
        filename_fp_log  TYPE text1024,          "Log Filepath         "
        filename_fn_log  TYPE localfile,         "Log Filename         "
       END   OF ty_wa_file_list.

TYPES:  ty_it_file_list  TYPE STANDARD TABLE OF ty_wa_file_list.

TYPES:  ty_wa_xparam     TYPE zfit_xparam.       "Parameter Master     "
TYPES:  ty_it_xparam     TYPE STANDARD TABLE OF ty_wa_xparam.
DATA:   git_xparam       TYPE ty_it_xparam,      "Parameter Master     "
        git_file_list    TYPE ty_it_file_list.   "File List            "
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP',
        gc_param_in_int  TYPE zparamtype         "Inbound Interface    "
                         VALUE 'INBOUND_INTERFACE',
        gc_param_obj_id  TYPE zparamsubtype      "Object ID            "
                         VALUE 'I_FG_00001',
        gc_filepath_in   TYPE zparamkey          "Input Filepath       "
                         VALUE 'INPUT_FILEPATH',
        gc_filepath_arch TYPE zparamkey          "Archive Filepath     "
                         VALUE 'ARCHIVE_FILEPATH',
        gc_filepath_log  TYPE zparamkey          "Log Filepath         "
                         VALUE 'LOG_FILEPATH',
        gc_pattern_fn_in TYPE string             "Pattern-Filename In  "
                         VALUE '.*',
        gc_c             TYPE char1              "C / Character        "
                         VALUE 'C',
        gc_n             TYPE char1              "N / No-disply / Numerc
                         VALUE 'N',
        gc_x             TYPE char1              "X / Yes / True       "
                         VALUE 'X',
        gc_witht_03      TYPE lfbw-witht VALUE '03', "withhoding tax type
        gc_witht_42      TYPE lfbw-witht VALUE '42', "withhoding tax type
        gc_witht_fe      TYPE lfbw-witht VALUE 'FE', "withhoding tax type
        gc_witht_fb      TYPE lfbw-witht VALUE 'FB', "withhoding tax type
**--START OF CHANGES FOR CHG0120819 BY KBANERJEE
        gc_witht_nc      TYPE lfbw-witht VALUE 'NC'. "withhoding tax type NC
**--END OF CHANGES FOR CHG0120819 BY KBANERJEE

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X'
                                      USER-COMMAND cmd,
            p_sfile   LIKE        rfpdo-rfbifile MODIF ID dsp,
            p_ifile   LIKE        rfpdo-rfbifile MODIF ID srv,
            r_local   RADIOBUTTON GROUP rad1,
            p_file    TYPE        rfpdo-rfbifile DEFAULT 'H:\'
                                                  MODIF ID lcl,
            p_objid   TYPE        zparamkey        "Object ID
                                  VISIBLE LENGTH 12
                                  OBLIGATORY MODIF ID dsp.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS: p_arfile TYPE rfpdo-lboxfile OBLIGATORY MODIF ID dsp.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-003.
PARAMETERS: p_sourcf  TYPE xfeld DEFAULT 'X',  "Source File
            p_sourct  TYPE xfeld DEFAULT 'X'.  "Source Table
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-004.
PARAMETERS:  "p_saknr TYPE bseg-saknr DEFAULT '0000440000',
             p_blart TYPE bkpf-blart DEFAULT 'KR' ,
             p_park  TYPE bkpf-blart DEFAULT 'KR',
             p_mspvnd TYPE bseg-lifnr DEFAULT '0000034631',
             p_taxtol TYPE bseg-wrbtr,
             p_eccid  TYPE zfi_fglass_trans-ecc_id DEFAULT 'CE',
             p_test TYPE xfeld DEFAULT 'X',
             p_email TYPE string DEFAULT
                    'SAHMAD@spectraenergy.com' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
* Select the program parameter values
  PERFORM  f_select_xparam.

AT SELECTION-SCREEN OUTPUT.
  PERFORM  f_toggle_functionality.

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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',TXT File,*.txt'
      static    = 'X'
    CHANGING
      file_name = p_file.

AT SELECTION-SCREEN.
* Check if the ZFIT_XPARAM entries have been maintained
  PERFORM  f_validate_xparam_entries.

START-OF-SELECTION.

  CLEAR:  gv_flag_err_proc,
          git_file_list[],
          gt_data,
          gt_acc_data,
          gt_doc_data,
          gt_proc_data,
          gt_nonproc_data,
          gt_msg,
          gt_tab,
          gt_id_msg,
          gv_logfile_name,
          gv_logfile_path,
          gt_park_data,
          gt_nonpark_data.
*  IF p_saknr IS INITIAL.
*    WRITE : / 'Input GL Account at selection screen.'.
*    STOP.
*  ENDIF.
  IF p_blart IS INITIAL.
    WRITE : / 'Input Document Type at selection screen.'.
    STOP.
  ENDIF.
  IF p_mspvnd IS INITIAL.
    WRITE : / 'Input MSP Vendor at selection screen.'.
    STOP.
  ENDIF.
  IF p_sourcf IS INITIAL AND
     p_sourct IS INITIAL.
    WRITE : / 'Select Source of data for interface'.
    STOP.
  ENDIF.
** Build the list of files to be processed
  PERFORM  f_build_file_list.

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
    IF r_server = 'X' AND
       p_sfile IS INITIAL AND
       p_sourcf IS NOT INITIAL.
      WRITE : / 'Please enter Application Server File Path.'.
      STOP.
    ENDIF.
    IF r_local = 'X' AND
       p_file IS INITIAL AND
       p_sourcf IS NOT INITIAL.
      WRITE : / 'Please enter Local PC file Path.'.
      STOP.
    ENDIF.
    "--------Source is file
    IF p_sourcf IS NOT INITIAL.
      IF r_server IS NOT INITIAL.
        PERFORM upload_file_server.
      ELSE.
        PERFORM upload_file_pc.
      ENDIF.
      PERFORM prepare_data_file.
    ENDIF.
    "--------Source is table
    IF p_sourct IS NOT INITIAL.
      PERFORM prepare_data_table.
    ENDIF.
    "---------------------
    IF gt_acc_data[] IS INITIAL.
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
      PERFORM prepare_data.
      PERFORM validate_post_data.
      "
*      IF p_test IS INITIAL.
*        PERFORM validate_park_data.
*      ENDIF.
      IF p_test IS INITIAL.
        IF gt_nonproc_data[] IS NOT INITIAL.
          PERFORM updata_custom_table.
        ELSEIF gt_nonproc_data[] IS INITIAL AND
               p_sourct IS NOT INITIAL.
          CLEAR gv_counter.
          SELECT COUNT(*) INTO gv_counter FROM zfi_fglass_trans.
          IF gv_counter > 0.
            DELETE FROM zfi_fglass_trans.
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ENDIF.
      IF p_test IS NOT INITIAL.
        SKIP 1.
        WRITE : / 'Interface is completed in TEST Mode..'.
      ELSE.
*        "Not in test mode, then archive the file
        IF r_server IS NOT INITIAL.
          IF p_sourcf IS NOT INITIAL.
            PERFORM  archive_file.
          ENDIF.
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
  IF gt_park_data[] IS NOT INITIAL.
    PERFORM submit_rfwt0010.
  ENDIF.
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

  DATA: ls_msg TYPE ty_msg,
        lwa_file_list TYPE ty_wa_file_list.

  LOOP AT git_file_list INTO lwa_file_list.

    CONCATENATE lwa_file_list-filename_fp_in
                lwa_file_list-filename_fn_in
                INTO gv_sfile.
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

      IF gs_data-col24 <> 'X'.
        MOVE ' ' TO gs_data-col24.
      ENDIF.
      IF gs_data-col25 IS INITIAL.
        MOVE ' ' TO gs_data-col25.
      ENDIF.
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
  ENDLOOP.
ENDFORM.                    " UPLOAD_FILE_SERVER
*&---------------------------------------------------------------------*
*&      Form  SPLIT_DATA
*&---------------------------------------------------------------------*
*       Split data to appropriate column
*----------------------------------------------------------------------*
FORM split_data  USING    p_gt_tab STRUCTURE gt_tab
                          p_output STRUCTURE gs_data.

  SPLIT p_gt_tab AT cl_abap_char_utilities=>horizontal_tab
  INTO p_output-col1      " Invoice Line#
       p_output-col2      " Document Date
       p_output-col3      " Reference
       p_output-col4      " Invoice Type
       p_output-col5      " Invoice Type
       p_output-col6      " Line Type
       p_output-col7      " Posting Date
       p_output-col8      " Quantity
       p_output-col9      " UOM
       p_output-col10     " Unit Price
       p_output-col11     " Currency
       p_output-col12     " VMS Tax %
       p_output-col13     " Tax Amount
       p_output-col14     " Total Bill Rate
       p_output-col15     " Amount
       p_output-col16     " Header Text
       p_output-col17     " First Name (line item text)
       p_output-col18     " Last Name (Line Item text)
       p_output-col19     "SAP ECC System code
       p_output-col20     "Cost Object (WBS/IO/Network/CC)
       p_output-col21     "Task Code
       p_output-col22     "Personnel Area
       p_output-col23     "Vendor code
       p_output-col24     "WH Tax indicator
       p_output-col25.    "GL Account info

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
*  gt_tab[] = lt_auszug[].

ENDFORM.                    " UPLOAD_FILE_PC
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*      Prepare file data for validating / posting.
*----------------------------------------------------------------------*
FORM prepare_data_file.

  DATA: lv_counter TYPE i,
       lv_date     TYPE datum,
       lv_dat1     TYPE char6,
       lv_bukrs    TYPE bseg-bukrs,
       lv_bukrs_s  TYPE string,
       lv_objecttype(3),
       lv_costobject(30),
       lv_activity(4),
       lv_nplnr(12),
       lv_waers    TYPE bkpf-waers,
       lv_bktxt    TYPE bkpf-bktxt,
       lv_xblnr    TYPE bkpf-xblnr,
       lv_tax_percn TYPE kbetr_tax,
       lv_bldat    TYPE bkpf-bldat,
       lv_budat    TYPE bkpf-budat,
       lv_mm(2),
       lv_dd(2),
       lv_year(4),
       lv_length TYPE i,
       lv_msp TYPE i,
       lv_aufnr TYPE aufk-aufnr,
       lv_ecc(5),
       lv_saknr(15),
       lv_desc(55),
       ls_t005      TYPE t005.

*col2  Doc data
*col7  Post date
*col11 Currency
*Col16 header text
*col3  reference
*col23 Vendor

* Begin changes - CHG0131446 - conserve column headings prior to sort
  CLEAR                                     gs_data.
  READ     TABLE gt_data               INTO gs_data INDEX 1.
  DELETE         gt_data                            INDEX 1.
  SORT           gt_data BY col3 col16 col2 col7 col11 col23.
  INSERT         gs_data               INTO gt_data INDEX 1.
  CLEAR                                     gs_data.
* End changes   - CHG0131446 - conserve column headings prior to sort

  DELETE gt_data WHERE col2 = space.
  LOOP AT gt_data INTO gs_data.

    lv_counter = lv_counter + 1.
    "skip first line (column header)
    IF lv_counter = 1.
      CONTINUE.
    ENDIF.
    CONDENSE gs_data-col19 NO-GAPS.
    IF gs_data-col19 <> p_eccid.
      CONTINUE.
    ENDIF.
    CLEAR: gs_acc_data,
           lv_nplnr,
           lv_activity.
    CONDENSE gs_data-col2.
    CONDENSE gs_data-col5.
    CONDENSE gs_data-col7.
    CONDENSE gs_data-col3.
    CONDENSE gs_data-col11.
    CONDENSE gs_data-col6.
    CONDENSE gs_data-col20.
    CONDENSE gs_data-col16.
*    REPLACE ALL OCCURRENCES OF ',' IN gs_data-col12
*                            WITH space.
    CONDENSE gs_data-col12 NO-GAPS.
    REPLACE ALL OCCURRENCES OF ',' IN gs_data-col13
                            WITH space.
    CONDENSE gs_data-col13 NO-GAPS.
    REPLACE ALL OCCURRENCES OF ',' IN gs_data-col15
                            WITH space.
    CONDENSE gs_data-col15 NO-GAPS.
*    SPLIT gs_data-col2 AT '/' INTO lv_dd lv_mm lv_year.
**    lv_date = gs_data-col2.
*    "date format from DD/MM/YYYY
*    CONCATENATE lv_year lv_mm lv_dd INTO lv_bldat.
*    IF strlen( lv_mm ) < 2.
*      CONCATENATE '0' lv_mm INTO lv_mm.
*    ENDIF.
*    IF strlen( lv_dd ) < 2.
*      CONCATENATE '0' lv_dd INTO lv_dd.
*    ENDIF.
*    SPLIT gs_data-col7 AT '/' INTO lv_dd lv_mm lv_year.
*    IF strlen( lv_mm ) < 2.
*      CONCATENATE '0' lv_mm INTO lv_mm.
*    ENDIF.
*    IF strlen( lv_dd ) < 2.
*      CONCATENATE '0' lv_dd INTO lv_dd.
*    ENDIF.
*    CONCATENATE lv_year lv_mm lv_dd INTO lv_budat.
    "cost object
    CONDENSE gs_data-col20. " NO-GAPS.
    SPLIT gs_data-col20 AT '|' INTO lv_objecttype
                                    lv_bukrs_s
                                    lv_costobject.
    CONDENSE lv_objecttype NO-GAPS.
    CONDENSE lv_bukrs_s NO-GAPS.
    CONDENSE lv_costobject.
    lv_bukrs = lv_bukrs_s.

    CLEAR: lv_bktxt.
    "lv_bktxt = gs_data-col16.
    CASE gs_data-col5.
      WHEN 'TS'.
        lv_bktxt = 'Time Sheet'.
      WHEN 'ES'.
        lv_bktxt = 'Expense Sheet'.
      WHEN 'MI'.
        lv_bktxt = 'Miscellaneous Item'.
    ENDCASE.
    lv_waers = gs_data-col11.
    lv_xblnr = gs_data-col3.
*    lv_tax_percn = gs_data-col12.
    "Header
    gs_acc_data-bldat = gs_data-col2. "lv_bldat.
    gs_acc_data-budat = gs_data-col7. "lv_budat.
    gs_acc_data-bukrs = lv_bukrs.
    gs_acc_data-waers = lv_waers.
    gs_acc_data-bktxt = lv_bktxt.
    gs_acc_data-xblnr = lv_xblnr.

    "Line item
    CONDENSE gs_data-col4 NO-GAPS.
    CONDENSE gs_data-col5 NO-GAPS.
    CONDENSE gs_data-col8 NO-GAPS.
    CONDENSE gs_data-col9 NO-GAPS.
    CONDENSE gs_data-col10 NO-GAPS.
    CONDENSE gs_data-col14 NO-GAPS.
    CONDENSE gs_data-col16.
    CONDENSE gs_data-col17 NO-GAPS.
    CONDENSE gs_data-col18 NO-GAPS.
    CONDENSE gs_data-col21 NO-GAPS.
    CONDENSE gs_data-col22 NO-GAPS.
    CONDENSE gs_data-col23 NO-GAPS.
    gs_acc_data-source = 'F'.
    IF gs_data-col4 = 'KI'. "Kelly Services
      gs_acc_data-lifnr = p_mspvnd.
    ELSE.
      gs_acc_data-lifnr = gs_data-col23.
    ENDIF.
*    gs_acc_data-lifnr  = gs_data-col23.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_acc_data-lifnr
      IMPORTING
        output = gs_acc_data-lifnr.

*        gs_acc_data-valut  = lv_date.
    "-----------GL Account
*    gs_acc_data-saknr  = p_saknr. "gc_saknr.
*    TRANSLATE gs_acc_data-saknr TO UPPER CASE.
    CONDENSE gs_data-col25. " NO-GAPS.
    SPLIT gs_data-col25 AT '|' INTO lv_ecc
                                    lv_saknr
                                    lv_desc.
    CONDENSE lv_ecc NO-GAPS.
    CONDENSE lv_saknr NO-GAPS.
    CONDENSE lv_desc.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_saknr
      IMPORTING
        output = gs_acc_data-saknr.
    "gs_acc_data-saknr  = lv_saknr.
    TRANSLATE gs_acc_data-saknr TO UPPER CASE.
    "--------------------
    gs_acc_data-blart  = p_blart.
    "Cost object
    CASE lv_objecttype. "gs_data-col20(3).
      WHEN 'NET'.  "Network
        CONDENSE lv_costobject NO-GAPS.
        SPLIT lv_costobject AT '-' INTO
                                lv_nplnr
                                lv_activity.
        gs_acc_data-nplnr = lv_nplnr.
        "lv_costobject. "gs_data-col20+13(12).
        gs_acc_data-vornr  = lv_activity. "gs_data-col21.
      WHEN 'ORD'.  "Internal Order
        CONDENSE lv_costobject NO-GAPS.
        gs_acc_data-aufnr = lv_costobject. "gs_data-col20+13(12).
      WHEN 'WBS'.  "WBS
        CONDENSE lv_costobject NO-GAPS.
        gs_acc_data-projk24 = lv_costobject. "gs_data-col20+13(24).
      WHEN 'CCC'.  "Cost Center
        CONDENSE lv_costobject NO-GAPS.
        gs_acc_data-kostl = lv_costobject. "gs_data-col20+13(10).
      WHEN 'STT'.   "Statistical Order
        SPLIT lv_costobject AT space INTO lv_aufnr
                                        gs_acc_data-kostl.
        CONDENSE lv_aufnr NO-GAPS.
        CONDENSE gs_acc_data-kostl NO-GAPS.
        gs_acc_data-aufnr = lv_aufnr.
      WHEN OTHERS.
    ENDCASE.
    "gs_acc_data-vornr  = gs_data-col21.
    TRANSLATE gs_acc_data-nplnr TO UPPER CASE.
    TRANSLATE gs_acc_data-vornr TO UPPER CASE.
    TRANSLATE gs_acc_data-projk24 TO UPPER CASE.
    TRANSLATE gs_acc_data-vornr TO UPPER CASE.
    "------------------ line item text
    IF gs_data-col5 = 'ES'.  "Expense sheet
      CONCATENATE gs_data-col17(1) '.' gs_data-col18
                  gs_data-col16
             INTO gs_acc_data-sgtxt SEPARATED BY space.
    ELSE.
      CONCATENATE gs_data-col17(1) '.' gs_data-col18
                  gs_data-col2 gs_data-col6
             INTO gs_acc_data-sgtxt SEPARATED BY space.
    ENDIF.
    gs_acc_data-fname = gs_data-col17.
    gs_acc_data-lname = gs_data-col18.
*    gs_acc_data-sgtxt  = gs_data-col18.
    "------------------
    IF gs_acc_data-saknr IS NOT INITIAL.
      PERFORM conversion_routine CHANGING gs_acc_data-saknr.
    ENDIF.
    IF gs_acc_data-kostl IS NOT INITIAL.
      PERFORM conversion_routine CHANGING gs_acc_data-kostl.
    ENDIF.
    IF gs_acc_data-aufnr IS NOT INITIAL.
      PERFORM conversion_routine CHANGING gs_acc_data-aufnr.
    ENDIF.
    IF gs_acc_data-nplnr IS NOT INITIAL.
      PERFORM conversion_routine CHANGING gs_acc_data-nplnr.
    ENDIF.
    IF gs_acc_data-vornr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
        EXPORTING
          input  = gs_acc_data-vornr
        IMPORTING
          output = gs_acc_data-vornr.
    ENDIF.
    CLEAR gs_acc_data-projk.
    IF gs_acc_data-projk24 IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = gs_acc_data-projk24
        IMPORTING
          output    = gs_acc_data-projk
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
*******
    gs_acc_data-invtype1 = gs_data-col4.
    gs_acc_data-invtype2 = gs_data-col5.
    gs_acc_data-linetype = gs_data-col6.
    gs_acc_data-tax_code = gs_data-col12.
    gs_acc_data-tax_amount = gs_data-col13 .
    gs_acc_data-amount = gs_data-col15.
    gs_acc_data-quantity = gs_data-col8.
    gs_acc_data-uom = gs_data-col9.
    gs_acc_data-ecc_id = gs_data-col19.
    gs_acc_data-persa = gs_data-col22.
    gs_acc_data-whtax = gs_data-col24.
    "----------document Type
    IF gs_acc_data-whtax IS INITIAL.
      gs_acc_data-blart = p_blart.
    ELSE.
      gs_acc_data-blart = p_park.
    ENDIF.
    "Get Tax code
    CLEAR: gs_acc_data-mwskz,
           gs_acc_data-shkzg.
    SELECT SINGLE mwskz INTO gs_acc_data-mwskz FROM zfi_fglass_tax
                            WHERE tax_code = gs_acc_data-tax_code.
    "------------
    gs_acc_data-wrbtr  = gs_data-col15. "lv_wrbtr.
    "amount is including tax. so exclude it from document amount.
*    gs_acc_data-wrbtr  = gs_acc_data-wrbtr - gs_acc_data-tax_amount.
    IF gs_acc_data-wrbtr < 0. "invtype1 = 'CD'.
      gs_acc_data-shkzg = 'H'.   "CR
      gs_acc_data-bschl = '50'.  "CR
    ELSE.
      gs_acc_data-shkzg = 'S'.   "DR
      gs_acc_data-bschl = '40'.  "DR
    ENDIF.
    CLEAR:  gs_acc_data-menge,
            gs_acc_data-meins.
*    IF gs_acc_data-invtype1 = 'IN'.
    gs_acc_data-menge = gs_data-col8.
    gs_acc_data-meins = gs_data-col9.
    IF gs_acc_data-meins IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = gs_acc_data-meins
*         LANGUAGE       = SY-LANGU
        IMPORTING
          output         = gs_acc_data-meins
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
*   Implement suitable error handling here
      ENDIF.
    ENDIF.
*   ENDIF.
    "------------------
    APPEND gs_acc_data TO gt_acc_data.
  ENDLOOP.
  break sahmad.
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
* Validate and Post data. In case of error, fill invalid to IT to
* process by BDC
*----------------------------------------------------------------------*
FORM validate_post_data .
**--START OF CHANGES FOR CHG0120819 BY KBANERJEE
*Local table types
  TYPES:BEGIN OF lty_lfbw,
         lifnr     TYPE lifnr,
         bukrs     TYPE bukrs,
         witht     TYPE witht,
         wt_subjct TYPE wt_subjct,
         wt_withcd TYPE wt_withcd,
    END OF lty_lfbw.
**--END OF CHANGES FOR CHG0120819 BY KBANERJEE
  DATA: lv_not_first TYPE xfeld,
             lv_bukrs TYPE bkpf-bukrs,
             lv_blart TYPE bkpf-blart,
             lv_budat TYPE bkpf-budat,
             lv_bldat TYPE bkpf-bldat,
             lv_monat TYPE bkpf-monat,
             lv_waers TYPE bkpf-waers,
             lv_kursf TYPE bkpf-kursf,
             lv_xblnr TYPE bkpf-xblnr,
             lv_bktxt TYPE bkpf-bktxt,
             lv_wrbtr TYPE bapidoccur,
             lv_wrbtr1 TYPE bseg-wrbtr,
             lv_base_amt TYPE bapidoccur,
             lv_records TYPE i,
             lv_rows    TYPE i,
             lv_itemno_acc TYPE bapiaccr08-itemno_acc,
             lv_shkzg TYPE tbsl-shkzg,
             lv_kunnr TYPE kunde_pa,
             ls_ekko TYPE ekko,
             lv_docno TYPE i,
             lv_cr_amount TYPE bapidoccur,
             lv_tax_amount TYPE bapidoccur,
             lv_tax_amount_interface TYPE bseg-wrbtr,
             lv_tax TYPE bapidoccur,
             lv_tot_tax TYPE bset-fwste,
             lv_lifnr TYPE bseg-lifnr,
             lv_fname(20),
             lv_lname(20),
             lv_whtax(1),
             ls_lfb1 TYPE lfb1,
             lt_doc_data TYPE TABLE OF ty_acc_data,
             ls_lfbw TYPE lfbw.
  DATA: lt_tax_info TYPE TABLE OF rtax1u15,
        ls_tax_info TYPE rtax1u15.
  CONSTANTS: lc_vst TYPE ktosl VALUE 'VST'.
**--START OF CHANGES FOR CHG0120819 BY KBANERJEE
  DATA:lt_lfbw   TYPE STANDARD TABLE OF lty_lfbw INITIAL SIZE 0,
       lwa_lfbw  TYPE lty_lfbw.
**--END OF CHANGES FOR CHG0120819 BY KBANERJEE
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
         gt_accounttax,
         gs_accounttax,
         gt_payable,
         gs_payable,
         gt_whtax,
         gs_whtax.

  "docno in this internal table is virtual document to separate each
*document.
  SORT gt_doc_data BY whtax xblnr bktxt bukrs bldat budat waers blart lifnr.
  lt_doc_data[] = gt_doc_data[].
* Process all records - G.Ymana
*  DELETE lt_doc_data WHERE whtax IS NOT INITIAL.
  DESCRIBE TABLE lt_doc_data LINES lv_records.
  LOOP AT lt_doc_data INTO gs_doc_data.
    IF  "gs_doc_data-docno <> lv_docno.
        gs_doc_data-bukrs <> lv_bukrs OR
        "gs_doc_data-blart <> lv_blart OR
        gs_doc_data-bldat <> lv_bldat OR
        gs_doc_data-budat <> lv_budat OR
        gs_doc_data-waers <> lv_waers OR
        gs_doc_data-xblnr <> lv_xblnr OR
        gs_doc_data-bktxt <> lv_bktxt OR
        gs_doc_data-lifnr <> lv_lifnr OR
        gs_doc_data-whtax <> lv_whtax.
      IF lv_not_first IS NOT INITIAL.
        lv_tax = abs( lv_tax_amount ) - abs( lv_tax_amount_interface ).
        lv_tax = abs( lv_tax ).
        IF lv_tax > p_taxtol.
          PERFORM tax_tolerance_error USING lv_tax
                                            p_taxtol
                                            lv_bukrs
                                            lv_bldat
                                            lv_budat
                                            lv_waers
                                            lv_xblnr
                                            lv_bktxt
                                            lv_lifnr.
        ELSE.
          CLEAR: ls_lfb1.
          SELECT SINGLE * FROM lfb1 INTO ls_lfb1
            WHERE lifnr = lv_lifnr
              AND bukrs = lv_bukrs.
          "Add Creditor line / vendor line
          lv_itemno_acc = lv_itemno_acc + 1.
          gs_payable-itemno_acc    = lv_itemno_acc.
          gs_payable-vendor_no     = lv_lifnr.
          CONDENSE ls_lfb1-zwels NO-GAPS.
          gs_payable-pmnttrms  = ls_lfb1-zterm.
          gs_payable-pymt_meth = ls_lfb1-zwels(1).
          CONDENSE lv_fname NO-GAPS.
          CONDENSE lv_lname NO-GAPS.
          CONCATENATE lv_fname(1) '.' lv_lname
                     'Per. ending' lv_bldat
             INTO gs_payable-item_text SEPARATED BY space.
          APPEND gs_payable TO gt_payable.
          CLEAR gs_cur_litems.
          gs_cur_litems-itemno_acc = lv_itemno_acc.
          gs_cur_litems-currency   = lv_waers.
          gs_cur_litems-amt_doccur = lv_cr_amount * -1.
          APPEND gs_cur_litems TO gt_cur_litems.
          "WH Tax info for park documents only
          IF gs_docheader-doc_status <> 2.
**--START OF CHANGES FOR CHG0120819 BY KBANERJEE
*Added withholding tax type 'NC' and also included
*Subject to withholding tax indicator in below query
*In case there are multiple items for withholding tax
*all are to be updated in document line item
*            CLEAR ls_lfbw.
*            SELECT SINGLE * FROM lfbw INTO ls_lfbw
*              WHERE lifnr = lv_lifnr
*                AND bukrs = lv_bukrs
*                AND ( witht = gc_witht_03 OR
*                      witht = gc_witht_42 OR
*                      witht = gc_witht_fe OR
*                      witht = gc_witht_fb ).
            REFRESH lt_lfbw.
            SELECT lifnr
                   bukrs
                   witht
                   wt_subjct
                   wt_withcd
              FROM lfbw
              INTO TABLE lt_lfbw
              WHERE lifnr = lv_lifnr
                AND bukrs = lv_bukrs
                AND ( witht = gc_witht_03
                   OR witht = gc_witht_42
                   OR witht = gc_witht_fe
                   OR witht = gc_witht_fb
                   OR witht = gc_witht_nc )
                AND wt_subjct = gc_x.
**--END OF CHANGES FOR CHG0120819 BY KBANERJEE
            IF sy-subrc = 0.
**--START OF CHANGES FOR CHG0120819 BY KBANERJEE
*Include all relevant tax codes in WHT information
*              gs_whtax-itemno_acc = lv_itemno_acc.
*              gs_whtax-wt_type = ls_lfbw-witht.
*              gs_whtax-wt_code = ls_lfbw-wt_withcd.
*              APPEND gs_whtax TO gt_whtax.
              LOOP AT lt_lfbw INTO lwa_lfbw.
                gs_whtax-itemno_acc = lv_itemno_acc.
                gs_whtax-wt_type    = lwa_lfbw-witht.
                gs_whtax-wt_code    = lwa_lfbw-wt_withcd.
                APPEND gs_whtax TO gt_whtax.
                CLEAR:lwa_lfbw,gs_whtax.
              ENDLOOP.
**--END OF CHANGES FOR CHG0120819 BY KBANERJEE
            ENDIF.
          ENDIF.
          "-----------------------
          PERFORM bapi_validate_post.
        ENDIF.
      ENDIF.
      lv_not_first = 'X'.
*      lv_docno = gs_doc_data-docno.
      lv_bukrs = gs_doc_data-bukrs.
      lv_blart = gs_doc_data-blart.
      lv_bldat = gs_doc_data-bldat.
      lv_budat = gs_doc_data-budat.
      lv_waers = gs_doc_data-waers.
      lv_xblnr = gs_doc_data-xblnr.
      lv_bktxt = gs_doc_data-bktxt.
      lv_lifnr = gs_doc_data-lifnr.
      lv_lname = gs_doc_data-lname.
      lv_fname = gs_doc_data-fname.
      lv_whtax = gs_doc_data-whtax.
      CLEAR: lv_itemno_acc,
             lv_cr_amount,
             lv_tax_amount,
             lv_tax_amount_interface,
             gs_docheader,
             gs_cur_litems,
             gt_cur_litems,
             gt_gl_litems,
             gs_gl_litems,
             gt_accounttax,
             gs_accounttax,
             gt_payable,
             gs_payable,
             gt_whtax,
             gs_whtax.

    ENDIF.
    lv_tax_amount_interface = lv_tax_amount_interface +
                              gs_doc_data-tax_amount.
*Load header data
    gs_docheader-obj_type = 'BKPFF'.
    gs_docheader-username = sy-uname.
    IF gs_doc_data-invtype1 = 'KI'.
      gs_docheader-header_txt = 'KELLY INVOICE'.
    ELSE.
      gs_docheader-header_txt = gs_doc_data-bktxt.
    ENDIF.
    gs_docheader-comp_code = gs_doc_data-bukrs.
    gs_docheader-doc_date = gs_doc_data-bldat.
    gs_docheader-pstng_date = sy-datum.     "gs_doc_data-budat.
    gs_docheader-doc_type = gs_doc_data-blart.
    gs_docheader-ref_doc_no = gs_doc_data-xblnr.
* Status set to '2' to park the documentin the BAPI
    IF gs_doc_data-whtax IS NOT INITIAL.
      gs_docheader-doc_status = 2.
    ENDIF.


    lv_itemno_acc = lv_itemno_acc + 1.
*     Load Account GL line segment
    gs_gl_litems-itemno_acc    = lv_itemno_acc.
    gs_gl_litems-gl_account    = gs_doc_data-saknr.
    gs_gl_litems-comp_code     = gs_doc_data-bukrs.
    gs_gl_litems-pstng_date    = sy-datum.        "gs_doc_data-budat.
    gs_gl_litems-ref_key_1     = gs_doc_data-xblnr. "+3(12).
    gs_gl_litems-item_text     = gs_doc_data-sgtxt.
    gs_gl_litems-costcenter    = gs_doc_data-kostl.
    gs_gl_litems-orderid       = gs_doc_data-aufnr.
    gs_gl_litems-wbs_element   = gs_doc_data-projk24. "projk.
    gs_gl_litems-network       = gs_doc_data-nplnr.
    gs_gl_litems-activity      = gs_doc_data-vornr.
    gs_gl_litems-tax_code      = gs_doc_data-mwskz.
    gs_gl_litems-alloc_nmbr    = gs_doc_data-zuonr.
    gs_gl_litems-value_date    = gs_doc_data-valut.
    gs_gl_litems-quantity      = gs_doc_data-menge.
    gs_gl_litems-base_uom      = gs_doc_data-meins.
    APPEND gs_gl_litems TO gt_gl_litems.
*       Load GL currency line segment
    CLEAR: lt_tax_info,
           lv_base_amt,
           lv_tax,
           lv_tot_tax.
    IF gs_doc_data-mwskz IS NOT INITIAL.
      CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
        EXPORTING
          i_bukrs = gs_doc_data-bukrs
          i_mwskz = gs_doc_data-mwskz
          i_waers = gs_doc_data-waers
          i_wrbtr = gs_doc_data-wrbtr
        IMPORTING
          e_fwste = lv_tot_tax
        TABLES
          t_mwdat = lt_tax_info.
*VST will always be the HST line for the Canadian tax procedure
      DELETE lt_tax_info WHERE ktosl <> lc_vst.
      lv_tax_amount = lv_tax_amount + lv_tot_tax.
      "-----------------
      CLEAR lv_tot_tax.
      LOOP AT lt_tax_info INTO ls_tax_info.
        IF ls_tax_info-wmwst = 0 OR  "tax amount
          ls_tax_info-wmwst IS INITIAL OR
          ls_tax_info-hkont IS INITIAL.
          CONTINUE.
        ENDIF.
        lv_tot_tax = lv_tot_tax + ls_tax_info-wmwst.
      ENDLOOP.
    ELSE.  "no tax code assigned

    ENDIF.
    CLEAR gs_cur_litems.
    gs_cur_litems-itemno_acc    = lv_itemno_acc.
    gs_cur_litems-currency      = gs_doc_data-waers.
    "amount can be negative / positive as comes from input file
    gs_cur_litems-amt_doccur = gs_doc_data-wrbtr - lv_tot_tax.
    APPEND gs_cur_litems TO gt_cur_litems.
    "amount for creditor / vendor line
    lv_cr_amount = lv_cr_amount + gs_cur_litems-amt_doccur.
****Tax
    LOOP AT lt_tax_info INTO ls_tax_info.
      CLEAR gs_accounttax.
      IF ls_tax_info-wmwst = 0 OR  "tax amount
        ls_tax_info-wmwst IS INITIAL OR
        ls_tax_info-hkont IS INITIAL.
        CONTINUE.
      ENDIF.
      lv_itemno_acc = lv_itemno_acc + 1.
      gs_accounttax-itemno_acc = lv_itemno_acc.
      gs_accounttax-tax_code = gs_doc_data-mwskz.
      gs_accounttax-acct_key   = ls_tax_info-ktosl.
      gs_accounttax-cond_key   = ls_tax_info-kschl.
      gs_accounttax-taxjurcode = ls_tax_info-txjcd.
      gs_accounttax-taxjurcode_deep  = ls_tax_info-txjcd_deep.
      gs_accounttax-taxjurcode_level = ls_tax_info-txjlv.
      APPEND gs_accounttax TO gt_accounttax.

      CLEAR gs_cur_litems.
      gs_cur_litems-itemno_acc = gs_accounttax-itemno_acc.
      gs_cur_litems-currency   = gs_doc_data-waers.
      gs_cur_litems-amt_doccur = ls_tax_info-wmwst.
      gs_cur_litems-amt_base = ls_tax_info-kawrt.
      "gs_doc_data-wrbtr. "<-----------It's not the base, but gross
      "amount
      APPEND gs_cur_litems TO gt_cur_litems.
*      lv_tax_amount = lv_tax_amount + ls_tax_info-wmwst.
      "amount for creditor
      lv_cr_amount = lv_cr_amount + ls_tax_info-wmwst.
    ENDLOOP.
*******end of tax
******Re-use gt_acc_data internal table to keep data
******of current loop.In case of Error, we need to update custom table
    APPEND gs_doc_data TO gt_acc_data.
******
    lv_rows = lv_rows + 1.
    IF lv_records <= lv_rows.   "execute, because at last reocrds,
      lv_tax = abs( lv_tax_amount ) - abs( lv_tax_amount_interface ).
      lv_tax = abs( lv_tax ).
      IF lv_tax > p_taxtol. " OR lv_tax < p_taxtol.
        PERFORM tax_tolerance_error USING lv_tax
                                          p_taxtol
                                          lv_bukrs
                                          lv_bldat
                                          lv_budat
                                          lv_waers
                                          lv_xblnr
                                          lv_bktxt
                                          lv_lifnr.
      ELSE.
        CLEAR: ls_lfb1.
        SELECT SINGLE * FROM lfb1 INTO ls_lfb1
          WHERE lifnr = lv_lifnr
            AND bukrs = lv_bukrs.
        "Add Creditor line / vendor line
        lv_itemno_acc = lv_itemno_acc + 1.
        gs_payable-itemno_acc    = lv_itemno_acc.
        gs_payable-vendor_no     = lv_lifnr.
        CONDENSE ls_lfb1-zwels NO-GAPS.
        gs_payable-pmnttrms  = ls_lfb1-zterm.
        gs_payable-pymt_meth = ls_lfb1-zwels(1).
        CONDENSE lv_fname NO-GAPS.
        CONDENSE lv_lname NO-GAPS.
        CONCATENATE lv_fname(1) '.' lv_lname
                   'Per. ending' lv_bldat
           INTO gs_payable-item_text SEPARATED BY space.
        APPEND gs_payable TO gt_payable.
        CLEAR gs_cur_litems.
        gs_cur_litems-itemno_acc = lv_itemno_acc.
        gs_cur_litems-currency   = lv_waers.
        gs_cur_litems-amt_doccur = lv_cr_amount * -1.
        APPEND gs_cur_litems TO gt_cur_litems.
        "WH Tax info for park documents only
        IF gs_docheader-doc_status <> 2.
**--START OF CHANGES FOR CHG0120819 BY KBANERJEE
*Added withholding tax type 'NC' and also included
*Subject to withholding tax indicator in below query
*In case there are multiple items for withholding tax
*all are to be updated in document line item
*          CLEAR ls_lfbw.
*          SELECT SINGLE * FROM lfbw INTO ls_lfbw
*            WHERE lifnr = lv_lifnr
*              AND bukrs = lv_bukrs
**              and WT_SUBJCT = 'X'  "  Changed on Aug 11th by NARIGEAL " CHG012081,
*               AND ( witht = gc_witht_03 OR
*                     witht = gc_witht_42 OR
*                     witht = gc_witht_fe OR
*                      witht = gc_witht_fb ).
          REFRESH lt_lfbw.
          SELECT lifnr
                 bukrs
                 witht
                 wt_subjct
                 wt_withcd
            FROM lfbw
            INTO TABLE lt_lfbw
            WHERE lifnr = lv_lifnr
              AND bukrs = lv_bukrs
              AND ( witht = gc_witht_03
                 OR witht = gc_witht_42
                 OR witht = gc_witht_fe
                 OR witht = gc_witht_fb
                 OR witht = gc_witht_nc )
              AND wt_subjct = gc_x.
*--END OF CHANGES FOR CHG0120819 BY KBANERJEE
          IF sy-subrc = 0.
**--START OF CHANGES FOR CHG0120819 BY KBANERJEE
*Include all relevant tax codes in WHT information
*              gs_whtax-itemno_acc = lv_itemno_acc.
*              gs_whtax-wt_type = ls_lfbw-witht.
*              gs_whtax-wt_code = ls_lfbw-wt_withcd.
*              APPEND gs_whtax TO gt_whtax.
            LOOP AT lt_lfbw INTO lwa_lfbw.
              gs_whtax-itemno_acc = lv_itemno_acc.
              gs_whtax-wt_type    = lwa_lfbw-witht.
              gs_whtax-wt_code    = lwa_lfbw-wt_withcd.
              APPEND gs_whtax TO gt_whtax.
              CLEAR:lwa_lfbw,gs_whtax.
            ENDLOOP.
**--END OF CHANGES FOR CHG0120819 BY KBANERJEE
          ENDIF.
        ENDIF.
        "-----------------------
        PERFORM bapi_validate_post.   " it will exit the loop without
        EXIT.             "executing bapi_validate_post at top.
      ENDIF.
    ENDIF.
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

  CLEAR gt_return.
  IF gs_docheader-doc_status <> 2.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader    = gs_docheader
*       CUSTOMERCPD       =
*       CONTRACTHEADER    =
      TABLES
        accountgl         = gt_gl_litems
*       accountreceivable =
        accountpayable    = gt_payable
        accounttax        = gt_accounttax
        currencyamount    = gt_cur_litems
        accountwt         = gt_whtax
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
  ENDIF.

  IF lv_error IS INITIAL AND
     p_test IS INITIAL.    "not running in test mode
    CLEAR: gt_return,
           lv_first.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
   EXPORTING
     documentheader    = gs_docheader
*    IMPORTING
*      OBJ_KEY           = lv_obj_key
   TABLES
     accountgl         = gt_gl_litems
     accounttax        = gt_accounttax
*      ACCOUNTRECEIVABLE =
     accountpayable    = gt_payable
     currencyamount    = gt_cur_litems
*      EXTENSION1        =
     accountwt         = gt_whtax
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
  ENDIF.
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
    IF gs_docheader-doc_status = 2.
      APPEND LINES OF gt_acc_data TO gt_park_data.
    ELSE.
      APPEND LINES OF gt_acc_data TO gt_proc_data.
    ENDIF.
  ENDIF.
  CLEAR gt_acc_data.
ENDFORM.                    " BAPI_VALIDATE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_messages .
  DATA: lv_logfile TYPE zfit_xparam-value1,
        lv_error TYPE xfeld.

  CONCATENATE gv_logfile_path gv_logfile_name INTO lv_logfile.

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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_file.
  DATA: lv_source TYPE text1024,
        lv_target TYPE text1024,
        lv_data TYPE string,
        ls_msg TYPE ty_msg,
        lwa_file_list TYPE ty_wa_file_list.
  "-----------------------------
  ls_msg-msgrp = 'G'.
  ls_msg-msgid = 'ZFI01'.
  ls_msg-msgty = 'I'.
  ls_msg-msgno = '000'.
  MOVE text-028 TO ls_msg-msg_text.
  APPEND ls_msg TO gt_id_msg.
  CLEAR ls_msg.
  LOOP AT git_file_list INTO lwa_file_list
                        WHERE srvr_appl = gc_x.
    CONCATENATE lwa_file_list-filename_fp_in
                lwa_file_list-filename_fn_in
           INTO lv_source.
    CONCATENATE lwa_file_list-filename_fp_arc
                lwa_file_list-filename_fn_arc
                "'_' sy-datum '_' sy-UZEIT
           INTO lv_target.
    OPEN DATASET lv_source FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      ls_msg-msgrp = 'G'.
      ls_msg-msg_text = text-011.
      APPEND ls_msg TO gt_id_msg.
      CONCATENATE 'Source File:' lv_source INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      CONTINUE.
    ENDIF.
    OPEN DATASET lv_target FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      ls_msg-msgrp = 'G'.
      ls_msg-msg_text = text-011.
      APPEND ls_msg TO gt_id_msg.
      CONCATENATE 'Archive File:' lv_target INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      CONTINUE.
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

  ENDLOOP.
  SKIP 2.
ENDFORM.                    " ARCHIVE_FILE
*&---------------------------------------------------------------------*
*&      Form  f_select_xparam
*&---------------------------------------------------------------------*
*       Select the program parameter values
*----------------------------------------------------------------------*
FORM f_select_xparam.
  DATA: lwa_xparam TYPE ty_wa_xparam.

  CLEAR:    gv_obj_id,
            gv_logfile_path,
            gv_logfile_name.
  CLEAR    p_objid.

  CLEAR    git_xparam[].
  SELECT   *
    INTO   TABLE git_xparam
    FROM   zfit_xparam
   WHERE   paramtype = gc_param_in_int
     AND   subtype   = gc_param_obj_id.
  IF     ( sy-subrc EQ 0 ).
    SORT   git_xparam          ASCENDING BY mandt paramtype subtype
                                            key1 key2 key3 key4 key5.
    MOVE   gc_param_obj_id               TO gv_obj_id.
    MOVE   gc_param_obj_id               TO p_objid.
  ELSE.
    CLEAR  git_xparam[].
    CLEAR  gv_obj_id.
    CLEAR  p_objid.
  ENDIF.
  gv_obj_id = gc_param_obj_id.
  READ TABLE git_xparam INTO lwa_xparam
       WITH KEY subtype = gv_obj_id
                key1    = gc_filepath_arch.
  p_arfile = lwa_xparam-value1.
  CLEAR lwa_xparam.
  READ TABLE git_xparam INTO lwa_xparam
       WITH KEY subtype = gv_obj_id
                key1    = gc_filepath_in.
  p_sfile = lwa_xparam-value1.

ENDFORM.                    " f_select_xparam
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
*&      Form  F_VALIDATE_XPARAM_ENTRIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_validate_xparam_entries .

  DATA: lwa_xparam TYPE ty_wa_xparam.

* Check if the program parameter table is maintained
  IF git_xparam[] IS INITIAL.
    MESSAGE  e000(zfi01) WITH text-021.
  ENDIF.
  gv_obj_id = gc_param_obj_id.
  IF r_server IS NOT INITIAL.
* Check if the input filepath has been maintained
    CLEAR lwa_xparam.
    READ TABLE git_xparam INTO lwa_xparam
                          WITH KEY subtype = gv_obj_id
                                   key1    = gc_filepath_in.
    IF sy-subrc EQ 0.
      IF lwa_xparam-value1 IS INITIAL.
        MESSAGE  e000(zfi01) WITH text-022.

      ENDIF.
    ELSE.
      MESSAGE  e000(zfi01) WITH text-022.
    ENDIF.
* Check if the archive filepath has been maintained
    CLEAR lwa_xparam.
    READ TABLE git_xparam INTO lwa_xparam
                                   WITH KEY subtype = gv_obj_id
                                            key1    = gc_filepath_arch.
    IF sy-subrc EQ 0.
      IF lwa_xparam-value1 IS INITIAL.
        MESSAGE e000(zfi01) WITH text-023.
      ENDIF.
    ELSE.
      MESSAGE e000(zfi01) WITH text-023.
    ENDIF.

  ENDIF.
ENDFORM.                    " F_VALIDATE_XPARAM_ENTRIES
*&---------------------------------------------------------------------*
*&      Form  F_ERROR_IN_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_error_in_process   USING    iv_nbr_file     TYPE numc5
           iv_nbr_doc                  TYPE numc5
           iv_rc                       TYPE numc5
           iv_msgid                    TYPE symsgid
           iv_msgty                    TYPE symsgty
           iv_msgno                    TYPE any
           iv_msgv1                    TYPE any
           iv_msgv2                    TYPE any
           iv_msgv3                    TYPE any
           iv_msgv4                    TYPE any.

  DATA:    lv_type_fld                 TYPE char1,
           lv_msgno_c                  TYPE char3,
           lv_msgno                    TYPE symsgno,
           lv_text                     TYPE text240,
           ls_msg TYPE ty_msg.

  gv_flag_err_proc = 'X'.

  DESCRIBE FIELD iv_msgno TYPE lv_type_fld.

  IF lv_type_fld CS gc_c.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_msgno
      IMPORTING
        output = lv_msgno_c.
    IF lv_msgno_c CO '0123456789'.
      MOVE lv_msgno_c TO lv_msgno.
    ENDIF.
  ELSEIF  lv_type_fld CS gc_n.
    MOVE  iv_msgno  TO lv_msgno.
  ENDIF.
  IF iv_msgid IS INITIAL.
    CONCATENATE iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4
           INTO lv_text SEPARATED BY space.
  ELSE.
    MESSAGE ID iv_msgid TYPE iv_msgty NUMBER lv_msgno
          INTO lv_text WITH iv_msgv1 iv_msgv2 iv_msgv3
                            iv_msgv4.
  ENDIF.
  ls_msg-msgrp = 'G'.
  ls_msg-msgid = iv_msgid.
  ls_msg-msgty = iv_msgty.
  ls_msg-msgno = lv_msgno.
  CONCATENATE lv_text ', Source File:' iv_nbr_file iv_nbr_doc
                                         INTO ls_msg-msg_text.
  APPEND ls_msg TO gt_id_msg.
ENDFORM.                    " F_ERROR_IN_PROCESS
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FILE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_build_file_list .
  DATA:    lwa_xparam                  TYPE ty_wa_xparam,
           lwa_file_list               TYPE ty_wa_file_list,
           lwa_dir_list                TYPE epsfili,
           lit_dir_list                LIKE STANDARD TABLE
                                          OF lwa_dir_list.
  DATA:    lv_dir_name                 TYPE epsdirnam,
           lv_file_mask                TYPE epsfilnam,
           lv_rc                       TYPE numc5,
           lv_text                     TYPE string,
           lv_pattern                  TYPE string,
           lv_token                    TYPE string,
           lv_offset                   TYPE i,
           lv_length                   TYPE i,
           lv_subrc                    TYPE sysubrc,
           ls_msg TYPE ty_msg.

  gv_obj_id = gc_param_obj_id.
  IF r_local EQ gc_x.

    MOVE     gc_x  TO lwa_file_list-srvr_pres.
    MOVE     p_file TO lwa_file_list-filename_fp_in.
    APPEND   lwa_file_list TO git_file_list.

  ELSEIF r_server EQ gc_x .

    READ  TABLE git_xparam INTO lwa_xparam
                       WITH KEY subtype = gv_obj_id
                                key1    = gc_filepath_in.
    IF sy-subrc EQ 0.
      lv_dir_name = lwa_xparam-value1.
      IF  lwa_xparam-value2 IS NOT INITIAL.
        lv_pattern = lwa_xparam-value2.
      ELSE.
        lv_pattern = gc_pattern_fn_in.
      ENDIF.
* Get the list of files in the application directory
      IF p_ifile IS NOT INITIAL.
        lv_file_mask = p_ifile.
      ENDIF.

      CLEAR    lit_dir_list[].

      CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
        EXPORTING
          dir_name               = lv_dir_name
          file_mask              = lv_file_mask
        TABLES
          dir_list               = lit_dir_list
        EXCEPTIONS
          invalid_eps_subdir     = 1
          sapgparam_failed       = 2
          build_directory_failed = 3
          no_authorization       = 4
          read_directory_failed  = 5
          too_many_read_errors   = 6
          empty_directory_list   = 7
          OTHERS                 = 8.

      lv_rc = sy-subrc.

      IF lv_rc <> 0 AND
         lv_rc <> 7 .
        gv_flag_err_proc = 'X'.
        CLEAR    lit_dir_list[].
        PERFORM  f_error_in_process USING  0   0     lv_rc
                                           space     space     0
                                           text-024  lv_rc
                                           space     space.
        RETURN.
      ENDIF.

      DELETE lit_dir_list WHERE name IS INITIAL.

      IF lit_dir_list[] IS INITIAL.
*        gv_flag_err_proc = 'X'.
        ls_msg-msgrp = 'G'.
        ls_msg-msgid = 'ZFI01'.
        ls_msg-msgty = 'I'.
        ls_msg-msgno = '000'.
        MOVE text-025 TO ls_msg-msg_text.
        APPEND ls_msg TO gt_id_msg.
        MESSAGE  i000(zfi01) WITH text-025.

        RETURN.
      ENDIF.

*eject
* Evaluate the files in the inbound application directory
      CLEAR lwa_dir_list.
      LOOP AT lit_dir_list INTO lwa_dir_list.
        lv_text = lwa_dir_list-name.
* Search the string using a pattern; return offset, length, and token
        CALL FUNCTION 'ZFI_PARSE_STRING_USING_PATTERN'
          EXPORTING
            iv_text       = lv_text
            iv_pattern    = lv_pattern
          IMPORTING
            cv_offset     = lv_offset
            cv_length     = lv_length
            cv_token      = lv_token
            cv_subrc      = lv_subrc
          EXCEPTIONS
            no_text       = 1
            no_pattern    = 2
            search_error  = 3
            pattern_error = 4
            parse_error   = 5
            OTHERS        = 6.
        lv_rc = sy-subrc.
        IF lv_rc <> 0.
*          gv_flag_err_proc = 'X'.   "run
*          CLEAR    git_file_list[].
          PERFORM  f_error_in_process
            USING    0         0         lv_rc
                     space     space     0
                     text-026  lv_rc
                     lwa_dir_list-name   space.
          "RETURN.
          CONTINUE.
        ENDIF.
*eject
* If the pattern is found, then save the filename in the file list
        IF lv_subrc EQ 0 AND
           lv_token IS NOT INITIAL.
          CLEAR lwa_file_list.
          lwa_file_list-srvr_appl = gc_x.
          lwa_file_list-filename_fp_in = lv_dir_name.
          lwa_file_list-filename_fn_in = lwa_dir_list-name.
          PERFORM  f_generate_filenames
                                   CHANGING lwa_file_list.
          APPEND lwa_file_list TO git_file_list.
        ENDIF.
        CLEAR  lwa_dir_list.
      ENDLOOP.

    ENDIF.

  ENDIF.

  IF     ( git_file_list[] IS INITIAL ).
*    gv_flag_err_proc = 'X'.
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'I'.
    ls_msg-msgno = '000'.
    MOVE text-027 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    MESSAGE  i000(zfi01) WITH text-027.
    RETURN.
  ENDIF.

  SORT     git_file_list       ASCENDING BY filename_fp_in
                                            filename_fn_in.
ENDFORM.                    " F_BUILD_FILE_LIST
*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_FILENAMES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_generate_filenames  CHANGING cwa_file_list TYPE ty_wa_file_list.

  DATA:    lwa_xparam     TYPE ty_wa_xparam.
  DATA:    lv_filename    TYPE localfile,
           lv_filename_p  TYPE localfile.

  IF  gv_flag_err_proc IS NOT INITIAL .
    RETURN.
  ENDIF.
  IF r_server IS INITIAL OR
     cwa_file_list-filename_fn_in IS INITIAL.
    RETURN.
  ENDIF.
* Modify the filename
  lv_filename = cwa_file_list-filename_fn_in.
  CLEAR lv_filename_p.
  PERFORM  f_modify_filename       USING    lv_filename
                                            gc_x
                                            gc_x
                                   CHANGING lv_filename_p.
* Set the archive filepath and filename
  CLEAR lwa_xparam.
  READ TABLE git_xparam INTO lwa_xparam
                       WITH KEY subtype = gv_obj_id
                                key1    = gc_filepath_arch.
  IF ( sy-subrc EQ 0 ).
    MOVE     lwa_xparam-value1
      TO     cwa_file_list-filename_fp_arc.
    CONCATENATE lv_filename_p lwa_xparam-value2
                INTO cwa_file_list-filename_fn_arc.
  ENDIF.

* Set the log filepath and filename
  CLEAR lwa_xparam.
  READ     TABLE git_xparam            INTO lwa_xparam
                                   WITH KEY subtype = gv_obj_id
                                            key1    = gc_filepath_log.
  IF ( sy-subrc EQ 0 ).

    MOVE lwa_xparam-value1 TO gv_logfile_path.
    CONCATENATE lv_filename_p lwa_xparam-value2
                      INTO     gv_logfile_name.

  ENDIF.

ENDFORM.                    " F_GENERATE_FILENAMES
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_modify_filename  USING iv_filename TYPE localfile
                              iv_flag_retain TYPE flag
                              iv_flag_adddts TYPE flag
                     CHANGING cv_filename TYPE localfile.
  DATA:    lv_filename     TYPE localfile,
           lv_file_ext_old TYPE char30,
           lv_ptr1         TYPE i,
           lv_datum(8)     TYPE c,
           lv_uzeit(6)     TYPE c.

  DATA:    lwa_results_tab TYPE match_result,
           lit_results_tab TYPE match_result_tab.
  CLEAR    cv_filename.
  IF     ( iv_filename                   IS INITIAL ).
    RETURN.
  ENDIF.
  CLEAR    lv_filename.
  CLEAR    lv_file_ext_old.
  CLEAR    lv_ptr1.
  CLEAR lit_results_tab[].
  FIND ALL OCCURRENCES OF '.' IN iv_filename
                              RESULTS lit_results_tab.
  IF lit_results_tab[] IS INITIAL.
    CLEAR lv_filename.
    MOVE iv_filename TO lv_filename.
  ELSE.
    SORT lit_results_tab  DESCENDING BY offset.
    CLEAR lwa_results_tab.
    READ TABLE lit_results_tab INTO lwa_results_tab INDEX 1.
    lv_ptr1 = lwa_results_tab-offset.
    CLEAR lv_filename.
    MOVE iv_filename+0(lv_ptr1) TO lv_filename.
    ADD 1 TO lv_ptr1.
    CLEAR lv_file_ext_old.
    MOVE iv_filename+lv_ptr1(30) TO lv_file_ext_old.
  ENDIF.
*eject
  IF iv_flag_retain  IS INITIAL OR
     lv_file_ext_old IS INITIAL.
    IF iv_flag_adddts IS INITIAL.
      cv_filename = lv_filename.
    ELSE.
      lv_datum = sy-datum.
      lv_uzeit = sy-uzeit.
      CONCATENATE lv_filename '_' lv_datum '_'
                  lv_uzeit INTO cv_filename.
    ENDIF.
  ELSE.
    IF  iv_flag_adddts IS INITIAL.
      CONCATENATE lv_filename '_' lv_file_ext_old
                             INTO cv_filename.
    ELSE.
      lv_datum = sy-datum.
      lv_uzeit = sy-uzeit.
      CONCATENATE lv_filename '_' lv_file_ext_old '_'
                  lv_datum    '_' lv_uzeit
                                  INTO cv_filename.

    ENDIF.

  ENDIF.

ENDFORM.                    " F_MODIFY_FILENAME
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_data_table .

  DATA: lt_table_data TYPE TABLE OF zfi_fglass_trans,
        ls_table_data TYPE zfi_fglass_trans.

  SELECT * FROM zfi_fglass_trans INTO TABLE lt_table_data.

  LOOP AT lt_table_data INTO ls_table_data.
    CLEAR: gs_acc_data.
    CLEAR: gs_acc_data.
    "Header
    gs_acc_data-source = 'T'.
    gs_acc_data-bldat = ls_table_data-bldat.
    gs_acc_data-budat = ls_table_data-budat.
    gs_acc_data-bukrs = ls_table_data-bukrs.
    gs_acc_data-waers = ls_table_data-waers.
    gs_acc_data-bktxt = ls_table_data-bktxt.
    gs_acc_data-xblnr = ls_table_data-xblnr.
    "Line item
    gs_acc_data-lifnr  = ls_table_data-lifnr.
    gs_acc_data-wrbtr  = ls_table_data-wrbtr.
    " - ls_table_data-tax_amount.
*    gs_acc_data-saknr  = p_saknr.
    gs_acc_data-saknr  = ls_table_data-saknr.
    gs_acc_data-nplnr = ls_table_data-nplnr.
    gs_acc_data-aufnr = ls_table_data-aufnr.
    gs_acc_data-projk24 = ls_table_data-projk24.
    gs_acc_data-kostl = ls_table_data-kostl.
    gs_acc_data-vornr  = ls_table_data-vornr.
    gs_acc_data-sgtxt = ls_table_data-sgtxt.
    gs_acc_data-fname = ls_table_data-fname.
    gs_acc_data-lname = ls_table_data-lname.
    SELECT SINGLE mwskz INTO gs_acc_data-mwskz FROM zfi_fglass_tax
                            WHERE tax_code = ls_table_data-tax_code.
    IF ls_table_data-wrbtr < 0. "invtype1 = 'CD'.
      gs_acc_data-shkzg = 'H'.   "CR
      gs_acc_data-bschl = '50'.  "CR
    ELSE.
      gs_acc_data-shkzg = 'S'.   "DR
      gs_acc_data-bschl = '40'.  "DR
    ENDIF.
    CLEAR gs_acc_data-projk.
    IF gs_acc_data-projk24 IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = gs_acc_data-projk24
        IMPORTING
          output    = gs_acc_data-projk
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
    ENDIF.
    gs_acc_data-invtype1 = ls_table_data-invtype1.
    gs_acc_data-invtype2 = ls_table_data-invtype2.
    gs_acc_data-linetype = ls_table_data-linetype.
    gs_acc_data-tax_code = ls_table_data-tax_code.
    gs_acc_data-tax_amount = ls_table_data-tax_amount.
    gs_acc_data-amount = ls_table_data-wrbtr.
    gs_acc_data-quantity = ls_table_data-menge.
    gs_acc_data-uom = ls_table_data-meins.
    gs_acc_data-ecc_id = ls_table_data-ecc_id.
    gs_acc_data-persa = ls_table_data-persa.
    gs_acc_data-whtax = ls_table_data-whtax.
    "Document type
    IF gs_acc_data-whtax IS INITIAL.
      gs_acc_data-blart = p_blart.
    ELSE.
      gs_acc_data-blart = p_park.
    ENDIF.
    CLEAR:  gs_acc_data-menge,
            gs_acc_data-meins.
*    IF ls_table_data-invtype1 = 'IN'.
    gs_acc_data-menge = ls_table_data-menge.
    gs_acc_data-meins = ls_table_data-meins.
*    ENDIF.
    "------------------
    APPEND gs_acc_data TO gt_acc_data.
  ENDLOOP.
ENDFORM.                    " PREPARE_DATA_TABLE
*&---------------------------------------------------------------------*
*&      Form  TAX_TOLERANCE_ERROR
*&---------------------------------------------------------------------*
*       Tax Tolerance error
*----------------------------------------------------------------------*
FORM tax_tolerance_error  USING    p_lv_tax TYPE bapidoccur
                                   p_p_taxtol TYPE bseg-wrbtr
                                   iv_bukrs TYPE bkpf-bukrs
                                   iv_bldat TYPE bkpf-bldat
                                   iv_budat TYPE bkpf-budat
                                   iv_waers TYPE bkpf-waers
                                   iv_xblnr TYPE bkpf-xblnr
                                   iv_bktxt TYPE bkpf-bktxt
                                   iv_lifnr TYPE bseg-lifnr.
  DATA: lv_tax(14),
        lv_tax_tol(14).

  lv_tax = p_lv_tax.
  lv_tax_tol = p_p_taxtol.
  CLEAR gs_id_msg.
  " MOVE text-012
  CONCATENATE 'Difference between Calculated and interface tax:'
  lv_tax ' and allowed tax tolerance: ' lv_tax_tol
  INTO gs_id_msg-msg_text SEPARATED BY space.
  APPEND gs_id_msg TO gt_id_msg.
  CONCATENATE 'Error Header Record: '
              iv_bukrs
              iv_bldat
              iv_budat
              iv_waers
              iv_xblnr
              iv_bktxt
              iv_lifnr
              INTO gs_id_msg-msg_text SEPARATED BY space.
  APPEND gs_id_msg TO gt_id_msg.
  "-------------------------------------
  APPEND LINES OF gt_acc_data TO gt_nonproc_data.
  CLEAR gt_acc_data.
ENDFORM.                    " TAX_TOLERANCE_ERROR
*&---------------------------------------------------------------------*
*&      Form  UPDATA_CUSTOM_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM updata_custom_table .
  DATA: lt_table_data TYPE TABLE OF zfi_fglass_trans,
        ls_table_data TYPE zfi_fglass_trans,
        lv_buzei TYPE buzei.

* G.Ymana - Only delete from error log table when table flag is set.
  IF p_sourct IS NOT INITIAL.
    DELETE FROM zfi_fglass_trans.
    COMMIT WORK.
  ENDIF.

  LOOP AT gt_nonproc_data INTO gs_doc_data.
    lv_buzei = lv_buzei + 1.
    ls_table_data-bukrs = gs_doc_data-bukrs.
    ls_table_data-bldat = gs_doc_data-bldat.
    ls_table_data-budat = gs_doc_data-budat.
    ls_table_data-waers = gs_doc_data-waers.
    ls_table_data-xblnr = gs_doc_data-xblnr.
    ls_table_data-bktxt = gs_doc_data-bktxt.
    ls_table_data-buzei = lv_buzei.
    ls_table_data-cdate = sy-datum.
    ls_table_data-ctime = sy-uzeit.
    ls_table_data-wrbtr = gs_doc_data-wrbtr.
    ls_table_data-sgtxt = gs_doc_data-sgtxt.
    ls_table_data-lifnr = gs_doc_data-lifnr.
    ls_table_data-menge = gs_doc_data-menge.
    ls_table_data-meins = gs_doc_data-meins.
    ls_table_data-tax_amount = gs_doc_data-tax_amount.
    ls_table_data-tax_code = gs_doc_data-tax_code.
    ls_table_data-invtype1 = gs_doc_data-invtype1.
    ls_table_data-invtype2 = gs_doc_data-invtype2.
    ls_table_data-linetype = gs_doc_data-linetype.
    ls_table_data-persa = gs_doc_data-persa.
    ls_table_data-ecc_id = gs_doc_data-ecc_id.
    ls_table_data-vornr = gs_doc_data-vornr.
    ls_table_data-projk24 = gs_doc_data-projk24.
    ls_table_data-nplnr = gs_doc_data-nplnr.
    ls_table_data-aufnr = gs_doc_data-aufnr.
    ls_table_data-kostl = gs_doc_data-kostl.
    ls_table_data-fname = gs_doc_data-fname.
    ls_table_data-lname = gs_doc_data-lname.
    ls_table_data-usnam = sy-uname.
    ls_table_data-whtax = gs_doc_data-whtax.
    ls_table_data-saknr = gs_doc_data-saknr.
    APPEND ls_table_data TO lt_table_data.
  ENDLOOP.
  IF lt_table_data[] IS NOT INITIAL.
    MODIFY zfi_fglass_trans FROM TABLE lt_table_data.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " UPDATA_CUSTOM_TABLE
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_data .

  CLEAR: gt_doc_data[].
  APPEND LINES OF gt_acc_data TO gt_doc_data.

ENDFORM.                    " PREPARE_DATA
*&---------------------------------------------------------------------*
*&      Form  EMAIL_LOG
*&---------------------------------------------------------------------*
*       text
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
ENDFORM.                    " EMAIL_LOG
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
FORM f_build_mail_content  CHANGING p_lt_objtxt TYPE table.

  DATA: lwa_objtxt       TYPE solisti1,
        "lwa_email_data   TYPE ty_email_data,
        lv_lines TYPE n LENGTH 10,
        lv_wrbtr(14),
        lt_data TYPE TABLE OF ty_acc_data.

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
  "-------------------
  CONCATENATE '<table border="2" width="100%">' ' '
              INTO lwa_objtxt
              SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "source is input file data
  DESCRIBE TABLE gt_data LINES lv_lines.
  lv_lines = lv_lines - 1.
  CONCATENATE  '<tr> <td>'
                text-045 '</td><td>'
                lv_lines '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "source is input file data
  lt_data[] = gt_doc_data[].
  DELETE lt_data WHERE source <> 'F'.
  DESCRIBE TABLE lt_data LINES lv_lines.
  CONCATENATE  '<tr> <td>'
                text-037 '</td><td>'
                lv_lines '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "Records processed from input file
  CLEAR lv_lines.
  lt_data[] = gt_proc_data[].
  DELETE lt_data WHERE source <> 'F'.
  DESCRIBE TABLE lt_data LINES lv_lines.
  CONCATENATE  '<tr> <td>'
                text-039 '</td><td>'
                lv_lines '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "Records parked
  CLEAR lv_lines.
  lt_data[] = gt_park_data[].
*  DELETE lt_data WHERE source <> 'T'.
  DESCRIBE TABLE lt_data LINES lv_lines.
  CONCATENATE  '<tr> <td>'
                text-046 '</td><td>'
                lv_lines '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "Records non-processed from input file
  CLEAR lv_lines.
  lt_data[] = gt_nonproc_data[].
  DELETE lt_data WHERE source <> 'F'.
  DESCRIBE TABLE lt_data LINES lv_lines.
  CONCATENATE  '<tr> <td>'
                text-041 '</td><td>'
                lv_lines '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "source is input Custom Table
  CLEAR lv_lines.
  lt_data[] = gt_doc_data[].
  DELETE lt_data WHERE source <> 'T'.
  DESCRIBE TABLE lt_data LINES lv_lines.
  CONCATENATE  '<tr> <td>'
                text-038 '</td><td>'
                lv_lines '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "Records processed from input file
  CLEAR lv_lines.
  lt_data[] = gt_proc_data[].
  DELETE lt_data WHERE source <> 'T'.
  DESCRIBE TABLE lt_data LINES lv_lines.
  CONCATENATE  '<tr> <td>'
                text-040 '</td><td>'
                lv_lines '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  "Records non-processed from table
  CLEAR lv_lines.
  lt_data[] = gt_nonproc_data[].
  DELETE lt_data WHERE source <> 'T'.
  DESCRIBE TABLE lt_data LINES lv_lines.
  CONCATENATE  '<tr> <td>'
                text-042 '</td><td>'
                lv_lines '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "Records not parked
*  CLEAR lv_lines.
*  lt_data[] = gt_nonpark_data[].
*  DELETE lt_data WHERE source <> 'T'.
*  DESCRIBE TABLE lt_data LINES lv_lines.
*  CONCATENATE  '<tr> <td>'
*                text-047 '</td><td>'
*                lv_lines '</td></tr>'
*                INTO lwa_objtxt
*                SEPARATED BY space.
*  APPEND lwa_objtxt TO p_lt_objtxt.
*  CLEAR: lwa_objtxt.

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

  CONCATENATE text-010 '</td><td>'
              text-014 '</td><td>'
              text-013 '</td><td>'
              text-015 '</td><td>'
              text-016 '</td><td>'
              text-017 '</td><td>'
              text-018 '</td><td>'
              text-019 '</td><td>'
              text-020 '</td></tr>'
              INTO lwa_objtxt
              SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  LOOP AT gt_nonproc_data INTO gs_acc_data.
    WRITE gs_acc_data-wrbtr TO lv_wrbtr DECIMALS 2.
    CONCATENATE  '<tr> <td>'
                sy-sysid '</td><td>'
                gs_acc_data-bukrs '</td><td>'
                gs_acc_data-bldat '</td><td>'
                gs_acc_data-budat '</td><td>'
                gs_acc_data-waers '</td><td>'
                gs_acc_data-xblnr '</td><td>'
                gs_acc_data-bktxt '</td><td>'
                lv_wrbtr '</td><td>'
                gs_acc_data-lifnr '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.

    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
  ENDLOOP.

  MOVE: '</table> <br>' TO lwa_objtxt.
  APPEND: lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
  "----------------Log
  IF gv_logfile_path IS NOT INITIAL.
    CONCATENATE  '<tr> <td>'
                  text-044 '</td><td>'
                  gv_logfile_path gv_logfile_name '</td></tr>'
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
ENDFORM.                    " F_BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_RFWT0010
*&---------------------------------------------------------------------*
* Adjustment of Withholding Tax Information to Relevant Types
*----------------------------------------------------------------------*
FORM submit_rfwt0010 .
  DATA: lt_seltab  TYPE TABLE OF rsparams,
        ls_seltab  LIKE LINE OF lt_seltab,
        lt_abaplist TYPE TABLE OF abaplist,
        lt_list TYPE list_string_table,
        ls_list TYPE LINE OF list_string_table,
        ls_text TYPE tline,
        lt_text TYPE TABLE OF tline.
  DATA: p_vendor TYPE xfeld VALUE 'X',
        p_customer TYPE xfeld,
        s_lifnr TYPE RANGE OF lfb1-lifnr WITH HEADER LINE,
        s_kunnr TYPE RANGE OF knb1-kunnr WITH HEADER LINE,
        s_bukrs TYPE RANGE OF t001-bukrs WITH HEADER LINE,
        s_belnr TYPE RANGE OF bkpf-belnr WITH HEADER LINE,
        s_gjahr TYPE RANGE OF bkpf-gjahr WITH HEADER LINE,
        s_budat TYPE RANGE OF bkpf-budat WITH HEADER LINE,
        p_normal TYPE xfeld,
        p_parked TYPE xfeld VALUE 'X',
        p_recurr TYPE xfeld,
        s_dbelnr TYPE RANGE OF bkpf-belnr,
        p_ltest TYPE xfeld,
        p_today TYPE xfeld,
        p_other TYPE xfeld,
        p_date TYPE bkpf-budat,
        p_new_type TYPE xfeld VALUE 'X',
        p_code1 TYPE xfeld VALUE 'X',
        p_down_pay TYPE xfeld,
        p_code2 TYPE xfeld,
        p_old_type TYPE xfeld VALUE 'X'.
  "-------------------------------------------------------------
  s_budat-sign = 'I'.
  s_budat-option = 'EQ'.
  s_budat-low = sy-datum.
  APPEND s_budat.
  LOOP AT gt_park_data INTO gs_doc_data.
    IF gs_doc_data-lifnr IS NOT INITIAL.
      s_lifnr-sign = 'I'.
      s_lifnr-option = 'EQ'.
      s_lifnr-low = gs_doc_data-lifnr.
      APPEND s_lifnr.
    ENDIF.
    s_bukrs-sign = 'I'.
    s_bukrs-option = 'EQ'.
    s_bukrs-low = gs_doc_data-bukrs.
    APPEND s_bukrs.
    s_budat-sign = 'I'.
    s_budat-option = 'EQ'.
    s_budat-low = gs_doc_data-budat.
    APPEND s_budat.
  ENDLOOP.
  WAIT UP TO 30 SECONDS.
  SUBMIT rfwt0010 "VIA SELECTION-SCREEN
               WITH vendor EQ p_vendor
               WITH i_lifnr IN s_lifnr
               WITH customer EQ p_customer
               WITH i_kunnr IN s_kunnr
               WITH i_bukrs IN s_bukrs
               WITH i_belnr IN s_belnr
               WITH i_gjahr IN s_gjahr
               WITH i_budat IN s_budat
               WITH normal EQ p_normal
               WITH parked EQ p_parked
               WITH recurr EQ p_recurr
               WITH i_dbelnr IN s_dbelnr
               WITH test EQ p_ltest
               WITH da_today EQ p_today
               WITH da_other EQ p_other
               WITH date EQ p_date
               WITH new_type EQ p_new_type
               WITH code1 EQ p_code1
               WITH down_pay EQ p_down_pay
               WITH code2 EQ p_code2
               WITH old_type EQ p_old_type
               EXPORTING LIST TO MEMORY
               AND RETURN.
  WAIT UP TO 20 SECONDS.
***Capture message from BDC
  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = lt_abaplist
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.
  CALL FUNCTION 'LIST_TO_ASCI'
    IMPORTING
      list_string_ascii  = lt_list
    TABLES
      listobject         = lt_abaplist
    EXCEPTIONS
      empty_list         = 1
      list_index_invalid = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
  ENDIF.
  LOOP AT lt_list INTO ls_list.
    ls_text-tdline = ls_list.
    APPEND ls_text TO lt_text.
  ENDLOOP.
  CALL FUNCTION 'LIST_FREE_MEMORY'
    TABLES
      listobject = lt_abaplist.
ENDFORM.                    " SUBMIT_RFWT0010
