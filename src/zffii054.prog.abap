*&---------------------------------------------------------------------*
*& Report  ZFFII054
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 14-FEB-2019  KBANERJEE   D30K929579  CHG0137485-Extend the logic to  *
*                          D30K929602  replace IO/WBS by suspense cost *
*                          D30K929604  center from table               *
*                          D30K929606  ZFI_WD_SUSCCENTR if it is not   *
*                          D30K929608  valid for posting under below   *
*                          D30K929612  validations:                    *
*                          D30K929620  Status is NE REL                *
*                          D30K929626  Status is NE TECO               *
*                          D30K929628  Status EQ Delete Ind, Locked,   *
*                          D30K929630  Closed, Acct Assignmt Locked,   *
*                          D30K929632  Deletion Flag                   *
*                          D30K929634                                  *
*                          D30K929636                                  *
*                          D30K929640                                  *
*                          D30K929642                                  *
*                          D30K929644                                  *
*                          D30K929658                                  *
* 17-MAY-2023 APPUKUTA     D30K932432  Remove Lines with  GL 252125   *
************************************************************************
REPORT  zffii054 MESSAGE-ID zs.

TABLES: apqi,
        bseg.

TYPES: BEGIN OF ty_data,
       col1(4),  " CC / Posting key
       col2(15),  " Doc Date / GL Code
       col3(15),  " Post Date / Amount
       col4(15),  " Fiscal Period / Qty
       col5(5),   " Doc Type / UoM
       col6(25),  " Header Text / Value date
       col7(50),  " Reference field / Line item text
       col8(8),   " Currency / Personnel number
       col9(15),  " Reference trans/Internal order
       col10(10), " Cost Center
       col11(24), " WBS
       col12(2),  " Tax code
       col13(1),  " Payment block
       col14(18), " Assignment
       col15(10), " Customer
       END OF ty_data.
TYPES: BEGIN OF ty_acc_data,
         docno  TYPE i,
         itemno  TYPE i,
         bukrs  TYPE bkpf-bukrs, "COMPANY CODE
         bldat  TYPE bkpf-bldat, "DOCUMENT DATE
         budat  TYPE bkpf-budat, "POSTING DATE
         blart  TYPE bkpf-blart, " Document type
         waers  TYPE bkpf-waers, "CURRENCY
         gjahr  TYPE bkpf-gjahr,
         monat  TYPE bkpf-monat,  " Month
         bktxt  TYPE bkpf-bktxt, "HEADER TEXT
         xblnr  TYPE bkpf-xblnr, "Reference Document #
         "------------
         bschl  TYPE bseg-bschl, "POSTING KEY
         shkzg  TYPE bseg-shkzg,  "DR/CR indicator
         saknr  TYPE bseg-saknr, "GL CODE
         wrbtr  TYPE bseg-wrbtr, "LOCAL CURRENCY AMOUNT
         mwskz  TYPE bseg-mwskz, "Tax Code
         menge  TYPE bseg-menge, "Qty
         meins  TYPE bseg-meins, "Unit
         zuonr  TYPE bseg-zuonr, "ASSIGNMENT
         sgtxt  TYPE bseg-sgtxt, " LINE ITEM TEXT
         kostl  TYPE bseg-kostl, "COST CENTER
         "---------------------------
         valut  TYPE bseg-valut, "VALUE DATE
         aufnr  TYPE bseg-aufnr, "INTERNAL ODER
         nplnr  TYPE bseg-nplnr, "NETWORK
         vornr  TYPE cobl-vornr, "ACTIVITY
         projk24 TYPE prps-posid, "WBS 24 characters long
         projk  TYPE bseg-projk, "WBS
         pernr  TYPE bseg-pernr,
         zlspr  TYPE bseg-zlspr,
         kunnr  TYPE bseg-kunnr,
        "-----------
         sactt  TYPE bseg-saknr, "Suspense Acct
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
         sccentr TYPE kostl,     "Suspense Cost Center
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
         filename TYPE localfile, "File name
         error(1),
       END OF ty_acc_data.
DATA:BEGIN OF gt_tab OCCURS 0,         "Text file format
           text1(500),
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
TYPES: BEGIN OF ty_fileinfo,
       name TYPE text1024,
       tot_rec TYPE i,
       END OF ty_fileinfo.
DATA: gv_error TYPE xfeld,
      gv_group TYPE apqi-groupid,
      gv_group_post TYPE apqi-groupid,
      gt_id_msg TYPE TABLE OF ty_msg,
      gs_id_msg LIKE LINE OF gt_id_msg.
DATA: gt_data TYPE TABLE OF ty_data,
      gs_data TYPE ty_data,
      gt_acc_data TYPE TABLE OF ty_acc_data,
      gs_acc_data TYPE ty_acc_data,
      gt_acc_data_error TYPE TABLE OF ty_acc_data,
      gt_acc_data_error_sacct TYPE TABLE OF ty_acc_data,
      gt_bdc_data TYPE TABLE OF ty_acc_data,
      gt_error_data TYPE TABLE OF ty_acc_data,
      gt_fileinfo TYPE TABLE OF ty_fileinfo,
      gs_fileinfo TYPE ty_fileinfo,
      gt_t247 TYPE TABLE OF t247,
      gs_t247 TYPE t247,
      gt_zfi_wd_susccentr TYPE TABLE OF zfi_wd_susccentr,
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE.
      gt_acc_temp TYPE STANDARD TABLE OF ty_acc_data INITIAL SIZE 0.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
DATA: gv_sfile TYPE text1024,
      gv_bdc_flag TYPE xfeld,
      gv_docno TYPE i,
      gv_logfile_path TYPE zfit_xparam-value1,
      gv_logfile_name TYPE zfit_xparam-value1,
      ge_mail   TYPE string,
      ge_name   TYPE string,
      ge_domain TYPE string.

TYPES: BEGIN OF ty_wa_file_list,                 "File List            "
        srvr_pres        TYPE flag,              "Presentation Server  "
        srvr_appl        TYPE flag,              "Application Server   "
        filepath TYPE text1024,          "Input Filepath       "
        filename TYPE localfile,         "Input Filename       "
        exist(1),                       "File already processed
        error(1),                       "Error with file
       END   OF ty_wa_file_list.

TYPES:  ty_it_file_list  TYPE STANDARD TABLE OF ty_wa_file_list.
DATA:   gt_file_list    TYPE ty_it_file_list,    "File List            "
        gs_file_list TYPE ty_wa_file_list.
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP',
        gc_pattern_fn_in TYPE string             "Pattern-Filename In  "
                         VALUE '.*',
        gc_c             TYPE char1              "C / Character        "
                         VALUE 'C',
        gc_n             TYPE char1              "N / No-disply / Numerc
                         VALUE 'N',
        gc_x             TYPE char1              "X / Yes / True       "
                         VALUE 'X'.
DATA: BEGIN OF bdcdata OCCURS 0.               "batch input data
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
TYPES: BEGIN OF ty_bdc_rel_msg,
         id  TYPE apqi-groupid,
         msg TYPE tline,
       END OF ty_bdc_rel_msg.
DATA: gt_bdc_rel_msg TYPE TABLE OF ty_bdc_rel_msg WITH HEADER LINE, "tline OCCURS 0,
      gt_bdc_rel_msg_error TYPE TABLE OF ty_bdc_rel_msg WITH HEADER LINE, "TYPE tline OCCURS 0,
      gt_bdc_rel_msg_one TYPE tline OCCURS 0,
      gt_bdc_error_msg TYPE TABLE OF bapiret2.
DATA: s_gid TYPE RANGE OF apqi-groupid WITH HEADER LINE,
      s_date TYPE RANGE OF apqi-getdate WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:  p_sfile   LIKE        rfpdo-rfbifile MODIF ID dsp.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:  p_email1 TYPE string OBLIGATORY,
             p_email2 TYPE string OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-019.
PARAMETERS:  p_check1  AS CHECKBOX,
             p_sactt TYPE bseg-saknr.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid '/WORKDAY/GL/' INTO p_sfile.

START-OF-SELECTION.

  CLEAR:  s_gid,
          s_date,
          gt_fileinfo,
          gt_file_list[],
          gt_data,
          gt_acc_data,
          gt_msg,
          gt_tab,
          gt_id_msg,
          gv_logfile_name,
          gv_logfile_path,
          gv_group_post,
          gt_bdc_rel_msg,
          gt_bdc_rel_msg_error,
          gv_error.
  REFRESH: s_gid,
           s_date,
           gt_tab,
           gt_msg,
           gt_bdc_rel_msg,
           gt_bdc_rel_msg_error.

  IF p_sfile IS INITIAL.
    WRITE : / 'Please enter Application Server File Path.'.
    STOP.
  ENDIF.
  "--------------------
  ge_mail = p_email1.
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
  IF p_check1 IS NOT INITIAL AND
     p_sactt IS INITIAL.
    WRITE : / 'Input Suspense Account...'.
    STOP.
  ENDIF.
  ge_mail = p_email2.
  FIND REGEX
  '^([a-z](?:\w|-|\.)*)@((\w|-|(\.?!\.)))+\.[[:alpha:]]{2,4}$'
  IN ge_mail
  IGNORING CASE
  SUBMATCHES ge_name ge_domain.
  "------------------------------
** Build the list of files to be processed
  PERFORM  build_file_list.
  IF gt_file_list[] IS INITIAL.
    WRITE: / 'There no file to process..'.
    STOP.
  ENDIF.
*  PERFORM get_date_months.
  PERFORM get_general_data.
  CLEAR: gt_acc_data_error_sacct.
  PERFORM upload_process_file_server.
  "---------------------
  PERFORM archive_file.
  PERFORM update_processed_file.
  "------------Output messages
  PERFORM email_log.
  PERFORM display_messages.
*&---------------------------------------------------------------------*
*&      Form  upload_process_file_server
*&---------------------------------------------------------------------*
*       Upload file from Application Server and process it.
*----------------------------------------------------------------------*
FORM upload_process_file_server .

  DATA: lv_cnt TYPE i,
        ls_msg TYPE ty_msg,
        ls_payfile TYPE zfi_wd_payfile,
        ls_file_list TYPE ty_wa_file_list.
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
  DATA:ls_acc_temp TYPE ty_acc_data.
  FIELD-SYMBOLS:<lfs_acc_data> TYPE ty_acc_data.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE

  LOOP AT gt_file_list INTO ls_file_list.
    CLEAR ls_payfile.
    SELECT SINGLE * FROM zfi_wd_payfile INTO ls_payfile
      WHERE filename = ls_file_list-filename.
    IF sy-subrc = 0.
      ls_file_list-exist = 'X'.
      MODIFY gt_file_list FROM ls_file_list TRANSPORTING exist.
      CONTINUE.
    ENDIF.
    "---------------
    gs_file_list = ls_file_list.
    "---------------
    REFRESH: gt_tab.
    CLEAR: gt_tab.
    CLEAR: lv_cnt.
    CONCATENATE ls_file_list-filepath
                ls_file_list-filename
                INTO gv_sfile.
    OPEN DATASET gv_sfile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      MOVE text-006 TO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      CONCATENATE 'Input File: ' gv_sfile INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      ls_file_list-error = 'X'.
      MODIFY gt_file_list FROM ls_file_list TRANSPORTING error.
      CONTINUE.
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
* Following Lines Added          D30K932432
      IF gs_data-col2 eq '252125' AND
         gs_data-col7 eq space .
         CONTINUE .
      ENDIF .
      CASE syst-tabix .
        WHEN 2 OR 3 .
          gs_data-col14 = '1' .
      ENDCASE .
* End of new lines               D30K932432
      APPEND gs_data TO gt_data.
      lv_cnt = lv_cnt + 1.
    ENDDO.
    CLOSE DATASET gv_sfile.
    IF sy-subrc <> 0.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      MOVE text-007 TO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      CONCATENATE 'Input File: ' gv_sfile INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      "gv_flag_err_proc = 'X'.
*      EXIT.
    ENDIF.
    lv_cnt = lv_cnt - 1.
    gs_fileinfo-name = gv_sfile.
    gs_fileinfo-tot_rec = lv_cnt.
    APPEND gs_fileinfo TO gt_fileinfo.
    "-----------Prepare file data for posting
    CLEAR:  gt_acc_data,
            gv_docno.
    PERFORM prepare_data_file.
    IF gt_acc_data[] IS NOT INITIAL.
      PERFORM post_data_bdc.
    ENDIF.
    "------------Error data
    IF gt_acc_data_error[] IS NOT INITIAL.
      PERFORM post_data_bdc_error.
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
*Replace IOs and WBSes in output
      LOOP AT gt_acc_data_error ASSIGNING <lfs_acc_data>.
        READ TABLE gt_acc_temp INTO ls_acc_temp
                               WITH KEY sgtxt = <lfs_acc_data>-sgtxt
                                        bschl = <lfs_acc_data>-bschl
                                        wrbtr = <lfs_acc_data>-wrbtr
                                        pernr = <lfs_acc_data>-pernr.
        IF sy-subrc IS INITIAL.
          IF ls_acc_temp-aufnr IS NOT INITIAL.
            <lfs_acc_data>-aufnr = ls_acc_temp-aufnr.
          ELSEIF ls_acc_temp-projk24 IS NOT INITIAL.
            <lfs_acc_data>-projk24 = ls_acc_temp-projk24.
          ENDIF.
        ENDIF.
        CLEAR ls_acc_temp.
      ENDLOOP.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
      LOOP AT gt_acc_data_error INTO gs_acc_data.
        IF gs_acc_data-sactt IS INITIAL
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
        AND gs_acc_data-sccentr IS INITIAL.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
          CONTINUE.
        ENDIF.
        APPEND gs_acc_data TO gt_acc_data_error_sacct.
      ENDLOOP.
    ENDIF.
    "---------------
    IF gt_acc_data_error[] IS INITIAL AND
       gt_acc_data[] IS INITIAL.
      "gv_flag_err_proc = 'X'.
      gs_id_msg-msgrp = 'G'.
      gs_id_msg-msgid = 'ZFI01'.
      gs_id_msg-msgty = 'E'.
      gs_id_msg-msgno = '000'.
      CONCATENATE 'There is no data to process for file:' gv_sfile INTO gs_id_msg-msg_text
                                                  SEPARATED BY space.
      APPEND gs_id_msg TO gt_id_msg.
      CLEAR gs_id_msg.
    ENDIF.
    "-----------
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
       p_output-col15.

ENDFORM.                    " SPLIT_DATA
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*      Prepare file data for validating / posting.
*----------------------------------------------------------------------*
FORM prepare_data_file.

  DATA: lv_counter TYPE i,
        lv_itemno  TYPE i,
        lv_mm(2),
        lv_mmm TYPE t247-ktx,
        lv_dd(2),
        lv_year(4),
        lv_date(8),
        lv_debit TYPE bseg-wrbtr,
        lv_credit TYPE bseg-wrbtr,
        ls_csks TYPE csks,
        ls_aufk TYPE aufk,
        ls_prps TYPE prps,
        ls_skb1 TYPE skb1,
        lv_tabix TYPE sy-tabix,
        lt_acc_data  TYPE TABLE OF ty_acc_data,
        ls_acc_data  TYPE ty_acc_data,
        ls_susccentr TYPE zfi_wd_susccentr,
        ls_ska1 TYPE ska1.
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
  FIELD-SYMBOLS:<lfs_acc_data> TYPE ty_acc_data.
  DATA:lt_io_data  TYPE STANDARD TABLE OF ty_acc_data INITIAL SIZE 0,
       ls_io_data  TYPE ty_acc_data,
       lt_wbs_data TYPE STANDARD TABLE OF ty_acc_data INITIAL SIZE 0,
       ls_wbs_data TYPE ty_acc_data.
  TYPES:BEGIN OF lty_io_objnr,
         aufnr TYPE aufnr,
         objnr TYPE j_objnr,
       END OF lty_io_objnr,
       BEGIN OF lty_wbs_objnr,
         pspnr TYPE ps_posnr,
         objnr TYPE j_objnr,
       END OF lty_wbs_objnr,
       BEGIN OF lty_status.
          INCLUDE TYPE jest.
  TYPES:END OF lty_status.
  DATA:lt_io_objnr   TYPE STANDARD TABLE OF lty_io_objnr INITIAL SIZE 0,
       ls_io_objnr   TYPE lty_io_objnr,
       lt_wbs_objnr  TYPE STANDARD TABLE OF lty_wbs_objnr,
       ls_wbs_objnr  TYPE lty_wbs_objnr,
       lt_io_status  TYPE STANDARD TABLE OF lty_status   INITIAL SIZE 0,
       ls_io_status  TYPE lty_status,
       lt_wbs_status TYPE STANDARD TABLE OF lty_status   INITIAL SIZE 0,
       ls_wbs_status TYPE lty_status.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
  CLEAR: gs_acc_data,
         gv_error.
  "-----------------------------
  LOOP AT gt_data INTO gs_data.
    lv_counter = lv_counter + 1.
    CONDENSE gs_data-col1 NO-GAPS.
    CONDENSE gs_data-col2 NO-GAPS.
    CONDENSE gs_data-col3 NO-GAPS.
    CONDENSE gs_data-col4 NO-GAPS.
    CONDENSE gs_data-col5 NO-GAPS.
    CONDENSE gs_data-col6.
    CONDENSE gs_data-col7.
    CONDENSE gs_data-col8 NO-GAPS.
    CONDENSE gs_data-col9 NO-GAPS.
    CONDENSE gs_data-col10 NO-GAPS.
    CONDENSE gs_data-col11 NO-GAPS.
    CONDENSE gs_data-col12 NO-GAPS.
    CONDENSE gs_data-col13 NO-GAPS.
    CONDENSE gs_data-col14.
    CONDENSE gs_data-col15 NO-GAPS.

    REPLACE ALL OCCURRENCES OF ',' IN gs_data-col3
                            WITH space.
    CONDENSE gs_data-col3 NO-GAPS.

    "Header
    IF lv_counter = 1.
      REPLACE ALL OCCURRENCES OF '#' IN gs_data-col8 WITH space.
      CONDENSE gs_data-col8 NO-GAPS.
      REPLACE ALL OCCURRENCES OF '#' IN gs_data-col9 WITH space.
      CONDENSE gs_data-col9 NO-GAPS.
      "---------
      gs_acc_data-bukrs = gs_data-col1.
*      lv_mmm = gs_data-col2+3(3).
*      TRANSLATE lv_mmm TO UPPER CASE.
*      CLEAR gs_t247.
*      READ TABLE gt_t247 INTO gs_t247
*                            WITH KEY  ktx = lv_mmm.
*      CONCATENATE gs_data-col2+7(4) gs_t247-mnr gs_data-col2(2)
*                                    INTO gs_acc_data-bldat.
*      lv_mmm = gs_data-col3+3(3).
*      TRANSLATE lv_mmm TO UPPER CASE.
*      CLEAR gs_t247.
*      READ TABLE gt_t247 INTO gs_t247
*                            WITH KEY  ktx = lv_mmm.
*      CONCATENATE gs_data-col3+7(4) gs_t247-mnr gs_data-col3(2)
*                                    INTO gs_acc_data-budat.
      gs_acc_data-bldat = gs_data-col2.
      gs_acc_data-budat = gs_data-col3.
      gs_acc_data-blart = gs_data-col5.
      gs_acc_data-monat = gs_data-col4.
      gs_acc_data-bktxt = gs_data-col6.
      gs_acc_data-xblnr = 'PAYROLL'. "gs_data-col7.
      gs_acc_data-waers = gs_data-col8.
    ELSE.
      "Line item
      REPLACE ALL OCCURRENCES OF '#' IN gs_data-col15 WITH space.
      CONDENSE gs_data-col15 NO-GAPS.
      "-----------GL Account
      gs_acc_data-saknr = gs_data-col2.
      TRANSLATE gs_acc_data-saknr TO UPPER CASE.
      "--------------------
      "Cost object
      gs_acc_data-aufnr = gs_data-col9.
      gs_acc_data-kostl = gs_data-col10.
      gs_acc_data-projk24 = gs_data-col11.
      TRANSLATE gs_acc_data-projk24 TO UPPER CASE.
      TRANSLATE gs_acc_data-kostl TO UPPER CASE.
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
      "----------validate cost objects
      CLEAR: ls_csks,
             ls_aufk,
             ls_prps,
             ls_skb1,
             gv_error.
      IF gs_acc_data-kostl IS NOT INITIAL.
        SELECT SINGLE *  FROM csks INTO ls_csks
                                   WHERE kostl = gs_acc_data-kostl
                                     AND bukrs = gs_acc_data-bukrs.
        IF sy-subrc <> 0.
          gv_error = 'X'.
        ENDIF.
      ENDIF.
      IF gs_acc_data-aufnr IS NOT INITIAL.
        SELECT SINGLE * FROM aufk INTO ls_aufk
                   WHERE aufnr = gs_acc_data-aufnr.
        IF sy-subrc <> 0.
          gv_error = 'X'.
        ENDIF.
      ENDIF.
      IF gs_acc_data-projk IS NOT INITIAL.
        SELECT SINGLE * FROM prps INTO ls_prps
                 WHERE pspnr = gs_acc_data-projk.
        IF sy-subrc <> 0.
          gv_error = 'X'.
        ENDIF.
      ENDIF.
      gs_acc_data-sactt = space.
      IF gs_acc_data-saknr IS NOT INITIAL.
        SELECT SINGLE * FROM skb1 INTO ls_skb1
          WHERE bukrs = gs_acc_data-bukrs
            AND saknr = gs_acc_data-saknr.
        IF sy-subrc <> 0.
          gv_error = 'X'.
          IF p_check1 IS NOT INITIAL.
            gs_acc_data-sactt = p_sactt. "Suspense Acct
            PERFORM conversion_routine CHANGING gs_acc_data-sactt.
            SELECT SINGLE * FROM skb1 INTO ls_skb1
              WHERE bukrs = gs_acc_data-bukrs
                AND saknr = gs_acc_data-sactt.
            IF sy-subrc <> 0.
              gs_acc_data-sactt = space.
            ENDIF.
          ELSE.
            gs_acc_data-sactt = gs_data-col7+8(10).
          ENDIF.
        ENDIF.
      ELSE.
        gv_error = 'X'.
        IF p_check1 IS NOT INITIAL.
          gs_acc_data-sactt = p_sactt.  "Suspense Acct
          PERFORM conversion_routine CHANGING gs_acc_data-sactt.
          SELECT SINGLE * FROM skb1 INTO ls_skb1
              WHERE bukrs = gs_acc_data-bukrs
                AND saknr = gs_acc_data-sactt.
          IF sy-subrc <> 0.
            gs_acc_data-sactt = space.
          ENDIF.
        ELSE.
          gs_acc_data-sactt = gs_data-col7+8(10).
        ENDIF.
      ENDIF.
      "-----------
      gs_acc_data-error = gv_error.
      gs_acc_data-bschl = gs_data-col1.
      gs_acc_data-wrbtr = gs_data-col3.
      IF gs_acc_data-bschl = '50'. "gs_acc_data-gs_acc_data-wrbtr < 0.
        gs_acc_data-shkzg = 'H'.   "CR
      ELSE.
        gs_acc_data-shkzg = 'S'.   "DR
      ENDIF.
      gs_acc_data-menge = gs_data-col4.
      gs_acc_data-meins = gs_data-col5.
*      IF gs_data-col6 IS NOT INITIAL.
*        lv_mmm = gs_data-col6+3(3).
*        TRANSLATE lv_mmm TO UPPER CASE.
*        CLEAR gs_t247.
*        READ TABLE gt_t247 INTO gs_t247
*                              WITH KEY  ktx = lv_mmm.
*        CONCATENATE gs_data-col6+7(4) gs_t247-mnr gs_data-col6(2)
*                                      INTO gs_acc_data-valut.
*      ENDIF.
      gs_acc_data-valut = gs_data-col6.
      gs_acc_data-sgtxt = gs_data-col7.
      gs_acc_data-pernr = gs_data-col8.
      gs_acc_data-mwskz = gs_data-col12.
      gs_acc_data-zlspr = gs_data-col13.
      gs_acc_data-zuonr = gs_data-col14.
      gs_acc_data-kunnr = gs_data-col15.
      IF gs_acc_data-meins IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input          = gs_acc_data-meins
*           LANGUAGE       = SY-LANGU
          IMPORTING
            output         = gs_acc_data-meins
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
*   Implement suitable error handling here
        ENDIF.
      ENDIF.
      "--------------Suspense Cost Center
      IF gs_acc_data-sactt IS INITIAL.
        IF gs_acc_data-kostl IS INITIAL AND
           gs_acc_data-aufnr IS INITIAL AND
           gs_acc_data-projk24 IS INITIAL.
          CLEAR : ls_susccentr,
                  ls_ska1.
          SELECT SINGLE * FROM ska1 INTO ls_ska1
                          WHERE saknr = gs_acc_data-saknr.
          IF sy-subrc = 0.
            IF ls_ska1-xbilk IS INITIAL. "if GL Acct is not balance sheet
              READ TABLE gt_zfi_wd_susccentr INTO ls_susccentr WITH KEY
                                                   bukrs = gs_acc_data-bukrs.
              IF sy-subrc = 0.
                gs_acc_data-kostl = ls_susccentr-kostl.
                PERFORM conversion_routine CHANGING gs_acc_data-kostl.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      "-------------------------
      gs_acc_data-filename = gs_file_list-filename.
      "------------
      APPEND gs_acc_data TO gt_acc_data.
      "-----------
    ENDIF.
  ENDLOOP.
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
*Get object numbers for all IOs
  lt_io_data = gt_acc_data.
  DELETE lt_io_data WHERE aufnr IS INITIAL.
  IF lt_io_data IS NOT INITIAL.
    SORT lt_io_data BY  aufnr.
    SELECT aufnr objnr
      FROM aufk
      INTO TABLE lt_io_objnr
      FOR ALL ENTRIES IN lt_io_data
      WHERE aufnr = lt_io_data-aufnr
        AND bukrs = lt_io_data-bukrs.
    IF sy-subrc IS INITIAL.
      SORT lt_io_objnr BY aufnr objnr.
    ENDIF.
  ENDIF.
*Get object numbers for all WBSs
  lt_wbs_data = gt_acc_data.
  DELETE lt_wbs_data WHERE projk24 IS INITIAL.
  SORT lt_wbs_data BY projk24.
  IF lt_wbs_data IS NOT INITIAL.
    SELECT pspnr objnr
      FROM prps
      INTO TABLE lt_wbs_objnr
      FOR ALL ENTRIES IN lt_wbs_data
      WHERE pspnr  = lt_wbs_data-projk
        AND pbukr  = lt_wbs_data-bukrs.
    IF sy-subrc IS INITIAL.
      SORT lt_wbs_objnr BY pspnr objnr.
    ENDIF.
  ENDIF.
*Get the status for all IOs
  IF lt_io_objnr IS NOT INITIAL.
    SELECT *
       FROM jest
       INTO TABLE lt_io_status
       FOR ALL ENTRIES IN lt_io_objnr
       WHERE objnr = lt_io_objnr-objnr
         AND inact = space.
    IF sy-subrc IS INITIAL.
      SORT lt_io_status BY objnr stat.
    ENDIF.
  ENDIF.
*Get the status for all WBSs
  IF lt_wbs_objnr IS NOT INITIAL.
    SELECT *
       FROM jest
       INTO TABLE lt_wbs_status
       FOR ALL ENTRIES IN lt_wbs_objnr
       WHERE objnr = lt_wbs_objnr-objnr
         AND inact = space.
    IF sy-subrc IS INITIAL.
      SORT lt_wbs_status BY objnr stat.
    ENDIF.
  ENDIF.
  gt_acc_temp = gt_acc_data.
  LOOP AT gt_acc_data ASSIGNING <lfs_acc_data>.
    IF <lfs_acc_data>-aufnr IS NOT INITIAL.
      READ TABLE lt_io_data INTO ls_io_data
                            WITH KEY aufnr = <lfs_acc_data>-aufnr
                            BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_io_objnr INTO ls_io_objnr
                               WITH KEY aufnr = ls_io_data-aufnr.
        IF sy-subrc IS INITIAL.
          "check if status is released
          READ TABLE lt_io_status INTO ls_io_status
                                  WITH KEY objnr = ls_io_objnr-objnr
                                           stat  = 'I0002'.
          IF sy-subrc IS INITIAL.
            "check if status is deletion indicator set
            READ TABLE lt_io_status INTO ls_io_status
                                    WITH KEY objnr = ls_io_objnr-objnr
                                             stat  = 'I0013'.
            IF sy-subrc IS NOT INITIAL.
              "check if status is locked
              READ TABLE lt_io_status INTO ls_io_status
                                      WITH KEY objnr = ls_io_objnr-objnr
                                               stat  = 'I0043'.
              IF sy-subrc IS NOT INITIAL.
                "check if status is closed
                READ TABLE lt_io_status INTO ls_io_status
                                        WITH KEY objnr = ls_io_objnr-objnr
                                                 stat  = 'I0046'.
                IF sy-subrc IS NOT INITIAL.
                  "check if status is accnt assignment locked
                  READ TABLE lt_io_status INTO ls_io_status
                                          WITH KEY objnr = ls_io_objnr-objnr
                                                   stat  = 'I0064'.
                  IF sy-subrc IS NOT INITIAL.
                    "check if status is deletion flag set
                    READ TABLE lt_io_status INTO ls_io_status
                                            WITH KEY objnr = ls_io_objnr-objnr
                                                     stat  = 'I0076'.
                    IF sy-subrc IS INITIAL.
                      PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                    ENDIF."endif I0076
                  ELSE.
                    PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                  ENDIF."endif 'I0064
                ELSE.
                  PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                ENDIF."endif I0046
              ELSE.
                PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
              ENDIF."endif I0043
            ELSE.
              PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
            ENDIF."endif I0013
          ELSE.
            "check if status is TECO
            READ TABLE lt_io_status INTO ls_io_status
                                    WITH KEY objnr = ls_io_objnr-objnr
                                             stat  = 'I0045'.
            IF sy-subrc IS INITIAL.
              "check if status is deletion indicator set
              READ TABLE lt_io_status INTO ls_io_status
                                      WITH KEY objnr = ls_io_objnr-objnr
                                               stat  = 'I0013'.
              IF sy-subrc IS NOT INITIAL.
                "check if status is locked
                READ TABLE lt_io_status INTO ls_io_status
                                        WITH KEY objnr = ls_io_objnr-objnr
                                                 stat  = 'I0043'.
                IF sy-subrc IS NOT INITIAL.
                  "check if status is closed
                  READ TABLE lt_io_status INTO ls_io_status
                                          WITH KEY objnr = ls_io_objnr-objnr
                                                   stat  = 'I0046'.
                  IF sy-subrc IS NOT INITIAL.
                    "check if status is accnt assignment locked
                    READ TABLE lt_io_status INTO ls_io_status
                                            WITH KEY objnr = ls_io_objnr-objnr
                                                     stat  = 'I0064'.
                    IF sy-subrc IS NOT INITIAL.
                      "check if status is deletion flag set
                      READ TABLE lt_io_status INTO ls_io_status
                                              WITH KEY objnr = ls_io_objnr-objnr
                                                       stat  = 'I0076'.
                      IF sy-subrc IS INITIAL.
                        PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                      ENDIF."endif I0076
                    ELSE.
                      PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                    ENDIF."endif 'I0064
                  ELSE.
                    PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                  ENDIF."endif I0046
                ELSE.
                  PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                ENDIF."endif I0043
              ELSE.
                PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
              ENDIF."endif I0013
            ELSE.
              PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
            ENDIF."endif I0045
          ENDIF."endif I0002
        ELSE.
          PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
        ENDIF."endif lt_io_objnr
      ENDIF."endif lt_io_data
    ELSEIF <lfs_acc_data>-projk24 IS NOT INITIAL.
      READ TABLE lt_wbs_data INTO ls_wbs_data
                             WITH KEY projk24 = <lfs_acc_data>-projk24
                             BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_wbs_objnr INTO ls_wbs_objnr
                                WITH KEY pspnr = ls_wbs_data-projk.
        IF sy-subrc IS INITIAL.
          "check if status is released
          READ TABLE lt_wbs_status INTO ls_wbs_status
                                   WITH KEY objnr = ls_wbs_objnr-objnr
                                            stat  = 'I0002'.
          IF sy-subrc IS INITIAL.
            "check if status is deletion indicator set
            READ TABLE lt_wbs_status INTO ls_wbs_status
                                     WITH KEY objnr = ls_wbs_objnr-objnr
                                              stat  = 'I0013'.
            IF sy-subrc IS NOT INITIAL.
              "check if status is locked
              READ TABLE lt_wbs_status INTO ls_wbs_status
                                       WITH KEY objnr = ls_wbs_objnr-objnr
                                                stat  = 'I0043'.
              IF sy-subrc IS NOT INITIAL.
                "check if status is closed
                READ TABLE lt_wbs_status INTO ls_wbs_status
                                        WITH KEY objnr = ls_wbs_objnr-objnr
                                                 stat  = 'I0046'.
                IF sy-subrc IS NOT INITIAL.
                  "check if status is accnt assigment locked
                  READ TABLE lt_wbs_status INTO ls_wbs_status
                                          WITH KEY objnr = ls_wbs_objnr-objnr
                                                   stat  = 'I0064'.
                  IF sy-subrc IS NOT INITIAL.
                    "check if status is deletion flag set
                    READ TABLE lt_wbs_status INTO ls_wbs_status
                                             WITH KEY objnr = ls_wbs_objnr-objnr
                                                      stat  = 'I0076'.
                    IF sy-subrc IS INITIAL.
                      PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                    ENDIF."endif 'I0076
                  ELSE.
                    PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                  ENDIF."endif I0064
                ELSE.
                  PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                ENDIF."endif I0046
              ELSE.
                PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
              ENDIF."endif I0043
            ELSE.
              PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
            ENDIF."endif I0013
          ELSE.
            "check if status is TECO
            READ TABLE lt_wbs_status INTO ls_wbs_status
                                     WITH KEY objnr = ls_wbs_objnr-objnr
                                              stat  = 'I0045'.
            IF sy-subrc IS INITIAL.
              "check if status is deletion indicator set
              READ TABLE lt_wbs_status INTO ls_wbs_status
                                       WITH KEY objnr = ls_wbs_objnr-objnr
                                                stat  = 'I0013'.
              IF sy-subrc IS NOT INITIAL.
                "check if status is locked
                READ TABLE lt_wbs_status INTO ls_wbs_status
                                         WITH KEY objnr = ls_wbs_objnr-objnr
                                                  stat  = 'I0043'.
                IF sy-subrc IS NOT INITIAL.
                  "check if status is closed
                  READ TABLE lt_wbs_status INTO ls_wbs_status
                                          WITH KEY objnr = ls_wbs_objnr-objnr
                                                   stat  = 'I0046'.
                  IF sy-subrc IS NOT INITIAL.
                    "check if status is accnt assigment locked
                    READ TABLE lt_wbs_status INTO ls_wbs_status
                                            WITH KEY objnr = ls_wbs_objnr-objnr
                                                     stat  = 'I0064'.
                    IF sy-subrc IS NOT INITIAL.
                      "check if status is deletion flag set
                      READ TABLE lt_wbs_status INTO ls_wbs_status
                                               WITH KEY objnr = ls_wbs_objnr-objnr
                                                        stat  = 'I0076'.
                      IF sy-subrc IS INITIAL.
                        PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                      ENDIF."endif 'I0076
                    ELSE.
                      PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                    ENDIF."endif I0064
                  ELSE.
                    PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                  ENDIF."endif I0046
                ELSE.
                  PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
                ENDIF."endif I0043
              ELSE.
                PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
              ENDIF."endif I0013
            ELSE.
              PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
            ENDIF."endif I0045
          ENDIF."endif I0002
        ELSE.
          PERFORM f_replace_sccentr CHANGING <lfs_acc_data>.
        ENDIF."endif lt_io_objnr
      ENDIF."endif lt_wbs_data
    ENDIF."endif <lfs_acc_data>-aufnr is initial
    CLEAR:ls_io_data,ls_wbs_data,ls_io_objnr,
          ls_wbs_objnr,ls_susccentr,ls_io_status,
          ls_wbs_status.
  ENDLOOP.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
  SORT gt_acc_data BY pernr wrbtr bschl.
  lt_acc_data[] = gt_acc_data[].
  DELETE lt_acc_data WHERE error IS INITIAL.
  DELETE ADJACENT DUPLICATES FROM lt_acc_data COMPARING pernr.
  LOOP AT lt_acc_data INTO ls_acc_data.
    READ TABLE gt_acc_data WITH KEY pernr = ls_acc_data-pernr
                           TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT gt_acc_data INTO gs_acc_data FROM lv_tabix.
      IF gs_acc_data-pernr <> ls_acc_data-pernr.
        EXIT.
      ENDIF.
      gs_acc_data-error = 'X'.
      MODIFY gt_acc_data FROM gs_acc_data TRANSPORTING error.
    ENDLOOP.
  ENDLOOP.
  gt_acc_data_error[] = gt_acc_data[].
  DELETE gt_acc_data_error WHERE error IS INITIAL. "error records
  DELETE gt_acc_data WHERE error IS NOT INITIAL.   " no error
  "---------------Generate Balanced internal document-------------
  CLEAR: lv_debit, lv_credit, lv_counter,
         gv_docno, lv_itemno.
  gv_docno = gv_docno + 1.
  LOOP AT gt_acc_data INTO gs_acc_data.
    IF lv_itemno > 500 AND
      lv_debit = lv_credit.
      CLEAR: lv_debit,
             lv_credit,
             lv_itemno.
      gv_docno = gv_docno + 1.
    ENDIF.
    lv_counter = lv_counter + 1.
    gs_acc_data-docno = gv_docno.
    "Line item
    lv_itemno = lv_itemno + 1.
    gs_acc_data-itemno = lv_itemno.
    IF gs_acc_data-bschl = '40'.
      lv_debit = lv_debit + gs_acc_data-wrbtr.
    ELSE.
      lv_credit = lv_credit + gs_acc_data-wrbtr.
    ENDIF.
    MODIFY gt_acc_data FROM gs_acc_data TRANSPORTING docno itemno.
  ENDLOOP.
  "---Error data
  CLEAR: lv_debit, lv_credit, lv_counter,
         gv_docno, lv_itemno.
  gv_docno = gv_docno + 1.
  LOOP AT gt_acc_data_error INTO gs_acc_data.
    IF lv_itemno > 500 AND
       lv_debit = lv_credit.
      CLEAR: lv_debit,
             lv_credit,
             lv_itemno.
      gv_docno = gv_docno + 1.
    ENDIF.
    lv_counter = lv_counter + 1.
    gs_acc_data-docno = gv_docno.
    "Line item
    lv_itemno = lv_itemno + 1.
    gs_acc_data-itemno = lv_itemno.
    IF gs_acc_data-bschl = '40'.
      lv_debit = lv_debit + gs_acc_data-wrbtr.
    ELSE.
      lv_credit = lv_credit + gs_acc_data-wrbtr.
    ENDIF.
    MODIFY gt_acc_data_error FROM gs_acc_data TRANSPORTING docno itemno.
  ENDLOOP.
  "-------------------------------------------------------
  CLEAR gt_data.
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
*&      Form  DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_messages .
  DATA: ls_data TYPE ty_acc_data,
        lv_wrbtr(14),
        lv_menge(13),
        lv_text(350),
        lv_logfile TYPE zfit_xparam-value1,
        lv_error TYPE xfeld,
        ls_text TYPE tline,
        lv_msg TYPE tdline,
        ls_file_list TYPE LINE OF ty_it_file_list.

  CONCATENATE p_sfile 'LOG/LOG_' sy-datum sy-uzeit '.TXT'
                                          INTO lv_logfile.
  CONDENSE lv_logfile NO-GAPS.
  SKIP 2.
  LOOP AT gt_id_msg INTO gs_id_msg.
    "print for spool
    WRITE: /  gs_id_msg-msg_text.
  ENDLOOP.
  SKIP 2.
  WRITE : / '****************BDC Session(s)*********************'.
  LOOP AT s_gid.
    WRITE: s_gid-low.
  ENDLOOP.
  WRITE : / '****************Duplicate File(s) - Not Processed**'.
  LOOP AT gt_file_list INTO ls_file_list WHERE exist = 'X'.
    WRITE: /  ls_file_list-filename.
  ENDLOOP.
  WRITE : / '****************BDC Log *********************'.
  LOOP AT gt_bdc_rel_msg. " INTO ls_text.
    WRITE: / "gt_bdc_rel_msg-id,
             gt_bdc_rel_msg-msg.
  ENDLOOP.
  SKIP 2.
  WRITE: / 'Records with Suspension Acct or Wrong GL Acct..'.
  SKIP 1.
  LOOP AT gt_acc_data_error_sacct INTO ls_data.
    IF ls_data-sactt IS INITIAL
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
    AND ls_data-sccentr IS INITIAL.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
      CONTINUE.
    ENDIF.
    lv_wrbtr = ls_data-wrbtr.
    lv_menge = ls_data-menge.
    CONCATENATE   ls_data-bukrs  ls_data-bldat  ls_data-budat ls_data-blart
    ls_data-waers ls_data-bktxt ls_data-xblnr ls_data-bschl ls_data-shkzg
    ls_data-saknr ls_data-valut lv_wrbtr ls_data-mwskz lv_menge
    ls_data-meins ls_data-zuonr ls_data-sgtxt ls_data-kostl ls_data-aufnr
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
*    ls_data-projk ls_data-pernr ls_data-zlspr ls_data-sactt
     ls_data-projk24 ls_data-pernr ls_data-zlspr ls_data-sactt
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
    ls_data-sccentr
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
    ls_data-filename
    INTO lv_text SEPARATED BY space.
    WRITE: / lv_text.
  ENDLOOP.
  SKIP 1.
*  LOOP AT gt_bdc_rel_msg_error. " INTO ls_text.
*    WRITE: / gt_bdc_rel_msg_error-id,  "ls_text-tdline.
*             gt_bdc_rel_msg_error-msg.
*  ENDLOOP.
  "download to Log location
  OPEN DATASET lv_logfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE: / 'Unable to generate log file..'.
    WRITE: / lv_logfile.
    lv_error = 'X'.
  ELSE.
    TRANSFER text-015 TO lv_logfile.
    TRANSFER text-000 TO lv_logfile.
    LOOP AT gt_id_msg INTO gs_id_msg.
      TRANSFER gs_id_msg TO lv_logfile.
      IF sy-subrc <> 0.
        lv_error = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

****************Duplicate File(s)*********************
    TRANSFER text-000 TO lv_logfile.
    TRANSFER text-018 TO lv_logfile.
    TRANSFER text-000 TO lv_logfile.
    LOOP AT gt_file_list INTO ls_file_list WHERE exist = 'X'.
      TRANSFER ls_file_list-filename TO lv_logfile.
    ENDLOOP.
    TRANSFER text-000 TO lv_logfile.
    TRANSFER text-017 TO lv_logfile.
    TRANSFER text-000 TO lv_logfile.
****************BDC Session(s)*********************
    LOOP AT s_gid.
      TRANSFER s_gid-low TO lv_logfile.
    ENDLOOP.
    TRANSFER text-000 TO lv_logfile.
    TRANSFER text-014 TO lv_logfile.
    TRANSFER text-000 TO lv_logfile.
    LOOP AT gt_bdc_rel_msg. " INTO ls_text.
*      CONCATENATE gt_bdc_rel_msg-id '|'
*                  gt_bdc_rel_msg-msg INTO lv_msg.
      TRANSFER gt_bdc_rel_msg-msg TO lv_logfile.
    ENDLOOP.
*    SKIP 2.
*    LOOP AT gt_bdc_rel_msg_error. " INTO ls_text.
*      CONCATENATE gt_bdc_rel_msg_error-id '|'  "ls_text-tdline.
*                  gt_bdc_rel_msg_error-msg INTO lv_msg.
*      TRANSFER  lv_msg TO lv_logfile.
*    ENDLOOP.
    "----------Suspense Account
    TRANSFER text-000 TO lv_logfile.
    LOOP AT gt_acc_data_error_sacct INTO ls_data.
      IF ls_data-sactt IS INITIAL
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
    AND ls_data-sccentr IS INITIAL.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
        CONTINUE.
      ENDIF.
      lv_wrbtr = ls_data-wrbtr.
      lv_menge = ls_data-menge.
      CONCATENATE ls_data-bukrs  ls_data-bldat  ls_data-budat ls_data-blart
      ls_data-waers ls_data-bktxt ls_data-xblnr ls_data-bschl ls_data-shkzg
      ls_data-saknr ls_data-valut lv_wrbtr ls_data-mwskz lv_menge
      ls_data-meins ls_data-zuonr ls_data-sgtxt ls_data-kostl ls_data-aufnr
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
      "ls_data-projk ls_data-pernr ls_data-zlspr ls_data-sactt
      ls_data-projk24 ls_data-pernr ls_data-zlspr ls_data-sactt
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
      ls_data-sccentr
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
      ls_data-filename
      INTO lv_text SEPARATED BY space.
      TRANSFER lv_text TO lv_logfile.
    ENDLOOP.
    "----------
    TRANSFER text-000 TO lv_logfile.
    TRANSFER text-016 TO lv_logfile.
    TRANSFER text-000 TO lv_logfile.
    CLOSE DATASET lv_logfile.
    IF sy-subrc <> 0.
      lv_error = 'X'.
    ENDIF.
    IF lv_error IS INITIAL.
      WRITE : / 'Log file is generated..'.
      WRITE : / lv_logfile.
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
        ls_file_list TYPE ty_wa_file_list.
  "-----------------------------
  ls_msg-msgrp = 'G'.
  ls_msg-msgid = 'ZFI01'.
  ls_msg-msgty = 'I'.
  ls_msg-msgno = '000'.
  MOVE text-008 TO ls_msg-msg_text.
  APPEND ls_msg TO gt_id_msg.
  CLEAR ls_msg.
  LOOP AT gt_file_list INTO ls_file_list.
    "WHERE srvr_appl = gc_x.
    CONCATENATE ls_file_list-filepath
                ls_file_list-filename
           INTO lv_source.
    CONCATENATE ls_file_list-filepath 'ARCH/'
                ls_file_list-filename
                '_' sy-datum '_' sy-uzeit
           INTO lv_target.
    OPEN DATASET lv_source FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      ls_msg-msgrp = 'G'.
      ls_msg-msg_text = text-009.
      APPEND ls_msg TO gt_id_msg.
      CONCATENATE 'Source File:' lv_source INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      CONTINUE.
    ENDIF.
    OPEN DATASET lv_target FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      ls_msg-msgrp = 'G'.
      ls_msg-msg_text = text-010.
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
*      MOVE text-011 TO ls_msg-msg_text.
*      APPEND ls_msg TO gt_id_msg.
    ENDIF.

  ENDLOOP.
  SKIP 2.
ENDFORM.                    " ARCHIVE_FILE
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

  "gv_flag_err_proc = 'X'.

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
*&      Form  BUILD_FILE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_file_list .
  DATA:   " lwa_xparam                  TYPE ty_wa_xparam,
           ls_file_list               TYPE ty_wa_file_list,
           ls_dir_list                TYPE epsfili,
           lt_dir_list                LIKE STANDARD TABLE
                                          OF ls_dir_list.
  DATA:    lv_dir_name                 TYPE epsdirnam,
           lv_file_mask                TYPE epsfilnam,
           lv_rc                       TYPE numc5,
           lv_text                     TYPE string,
           lv_pattern                  TYPE string,
           lv_extension                TYPE string,
           lv_file                     TYPE string,
           lv_token                    TYPE string,
           lv_offset                   TYPE i,
           lv_length                   TYPE i,
           lv_subrc                    TYPE sysubrc,
           ls_msg TYPE ty_msg.

  lv_file_mask = 'PAYROLL_EAST*.*'.
  lv_dir_name = p_sfile.

  CLEAR    lt_dir_list[].

  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      dir_name               = lv_dir_name
      file_mask              = lv_file_mask
    TABLES
      dir_list               = lt_dir_list
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
*        gv_flag_err_proc = 'X'.
    CLEAR    lt_dir_list[].
    PERFORM  f_error_in_process USING  0   0     lv_rc
                                       space     space     0
                                       text-003  lv_rc
                                       space     space.
    RETURN.
  ENDIF.

  DELETE lt_dir_list WHERE name IS INITIAL.

  IF lt_dir_list[] IS INITIAL.
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'I'.
    ls_msg-msgno = '000'.
    MOVE text-004 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    MESSAGE  i000(zfi01) WITH text-004.

    RETURN.
  ENDIF.

*eject
* Evaluate the files in the inbound application directory
  CLEAR ls_dir_list.
  lv_pattern = 'TXT'.
  CONDENSE lv_pattern NO-GAPS.
  LOOP AT lt_dir_list INTO ls_dir_list.
    SPLIT ls_dir_list-name AT '.' INTO lv_file lv_extension.
    CONDENSE lv_extension NO-GAPS.
    IF lv_extension <> lv_pattern.
      CONTINUE.
    ENDIF.
    lv_text = ls_dir_list-name.
    CLEAR ls_file_list.
    ls_file_list-srvr_appl = gc_x.
    ls_file_list-filepath = lv_dir_name.
    ls_file_list-filename = ls_dir_list-name.
    APPEND ls_file_list TO gt_file_list.

  ENDLOOP.
  IF     ( gt_file_list[] IS INITIAL ).
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'I'.
    ls_msg-msgno = '000'.
    MOVE text-005 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    MESSAGE  i000(zfi01) WITH text-005.
    RETURN.
  ENDIF.

  SORT     gt_file_list       ASCENDING BY filepath
                                            filename.
ENDFORM.                    " F_BUILD_FILE_LIST
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
        ls_objpack  TYPE sopcklsti1,
        ls_objtxt   TYPE solisti1,
        ls_doc_chng TYPE sodocchgi1,
        ls_reclist  TYPE somlreci1,
        lv_space     TYPE char01  VALUE ' ',
        ls_msg TYPE ty_msg.


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
  ls_objtxt = lv_space.
  APPEND ls_objtxt TO lt_objtxt.
  CLEAR ls_objtxt.

  DESCRIBE TABLE lt_objtxt LINES lv_lines.

  CONCATENATE text-012
              lc_hyphen
              lc_log
              INTO lv_string
              SEPARATED BY space.

  ls_doc_chng-obj_descr  = lv_string.
  ls_doc_chng-sensitivty = lc_f.
  ls_doc_chng-doc_size   = lv_lines * 255.

* Pack to main body.
  ls_objpack-head_start = 1.
  ls_objpack-head_num   = 0.
  ls_objpack-body_start = 1.
  ls_objpack-body_num   = lv_lines.
  ls_objpack-doc_type   = lc_htm.
  APPEND ls_objpack TO lt_objpack.
  CLEAR ls_objpack.

  ls_reclist-copy = lc_x.

* Map Email ID(s)
  ls_reclist-receiver   = p_email1.
  ls_reclist-rec_type   = lc_u.
  ls_reclist-com_type   = lc_int.
  ls_reclist-notif_del  = lc_x.
  ls_reclist-notif_ndel = lc_x.
  ls_reclist-copy       = space.
  APPEND ls_reclist TO lt_reclist.
  ls_reclist-receiver   = p_email2.
  APPEND ls_reclist TO lt_reclist.


* Funcion module for sending email.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = ls_doc_chng
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
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'E'.
    ls_msg-msgno = '000'.
    CASE sy-subrc..
      WHEN 1.
        CONCATENATE text-013 ',Too_many_receivers'
                        INTO ls_msg-msg_text
                        SEPARATED BY space.
      WHEN 2.
        CONCATENATE text-013 ',Document_not_sent'
                       INTO ls_msg-msg_text
                       SEPARATED BY space.
      WHEN 3.
        CONCATENATE text-013 ',Document_type_not_exist'
             INTO ls_msg-msg_text
             SEPARATED BY space.
      WHEN 4.
        CONCATENATE text-013 ',Operation_no_authorization'
             INTO ls_msg-msg_text
             SEPARATED BY space.
      WHEN 5.
        CONCATENATE text-013 ',Parameter_error'
             INTO ls_msg-msg_text
             SEPARATED BY space.
      WHEN 6.
        CONCATENATE text-013 ',X_Error'
             INTO ls_msg-msg_text
             SEPARATED BY space.
      WHEN 7.
        CONCATENATE text-013 ',Enqueue_error'
             INTO ls_msg-msg_text
             SEPARATED BY space.
      WHEN 8.
        MOVE text-013 TO ls_msg-msg_text.
      WHEN OTHERS.
        MOVE text-013 TO ls_msg-msg_text.
    ENDCASE.
    APPEND ls_msg TO gt_id_msg.
  ELSE.
**Call program to push mail from SAP mail outbox
*    SUBMIT rsconn01 WITH mode = lc_int
*                    WITH output = space
*                    AND RETURN.
    WRITE: /'Error log is emailed.'.

  ENDIF.
ENDFORM.                    " EMAIL_LOG
*&---------------------------------------------------------------------*
*&      Form  POST_DATA_BDC
*&---------------------------------------------------------------------*
*       Post / Park data using BDC
*----------------------------------------------------------------------*
FORM post_data_bdc .
*program LZMM_SA_WFF03

  DATA:   trans_in_process(1),            "flag - trans in process
          open_session(1) VALUE 'X',      "flag - batch session open?
          lv_not_first TYPE xfeld,
          lv_post TYPE xfeld,
          lv_docno TYPE i,
          lv_records TYPE i,
          ls_lfb1 TYPE lfb1,
          lv_zzloc TYPE zzloc,
          lv_screen(4),
          lv_meins(3) ,
          ls_lfbw TYPE lfbw,
          lv_tax TYPE wmwst,
          lv_amount TYPE wrbtr,
          lv_qty TYPE menge_d.
  DATA:  lv_net_amount TYPE bseg-wrbtr,
         lv_wrbtr TYPE char13,
         lv_menge TYPE char13,
         lv_tax_amount TYPE char13,
         ls_skb1 TYPE skb1,
         lt_doc_data TYPE TABLE OF ty_acc_data.

  SORT gt_acc_data BY docno ASCENDING itemno DESCENDING.
  DESCRIBE TABLE gt_acc_data LINES lv_records.
  "------------------------
  CHECK gt_acc_data[] IS NOT INITIAL.
  WAIT UP TO 1 SECONDS.
  "-----------------------------
  REFRESH bdcdata.
  CONCATENATE 'ZFI-PY-' sy-uzeit+1(5) INTO gv_group_post. " = p_bdcid.
  gv_group = gv_group_post. "p_bdcid.
  PERFORM open_batch_session.
  "-------Collect BDC sesseion info for use in BDC log routine
  s_gid-sign = 'I'.
  s_gid-option = 'EQ'.
  s_gid-low = gv_group_post.
  APPEND s_gid.
  "-----------------------
  LOOP AT gt_acc_data INTO gs_acc_data.
    CLEAR: ls_skb1.
    SELECT SINGLE * FROM skb1 INTO ls_skb1
        WHERE bukrs = gs_acc_data-bukrs
          AND saknr = gs_acc_data-saknr.
*   Open batch session, if not open.
    IF gs_acc_data-docno <> lv_docno.
      IF lv_not_first IS NOT INITIAL.
        PERFORM close_transaction_post.
      ENDIF.
      lv_docno = gs_acc_data-docno.
      lv_not_first = 'X'.
      "Document Header
      PERFORM start_new_transaction_post.
    ENDIF.
    PERFORM start_next_lineitem_post.
    "------------
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = gs_acc_data-meins
        language       = sy-langu
      IMPORTING
*       LONG_TEXT      =
        output         = lv_meins
*       SHORT_TEXT     =
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    "GL Line item
    lv_net_amount = gs_acc_data-wrbtr.
    lv_qty = gs_acc_data-menge.
    IF lv_qty < 0.
      lv_qty = lv_qty * -1.
    ENDIF.
    WRITE lv_net_amount TO lv_wrbtr DECIMALS 2.
    WRITE lv_qty TO lv_menge DECIMALS 3.
    "------
    PERFORM bdc_screen USING 'SAPMF05A' '0300'.
    "------------
    PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-WRBTR'.
    PERFORM bdc_field  USING 'BDC_OKCODE' '=AB'.
    PERFORM bdc_field  USING 'BSEG-WRBTR' lv_wrbtr.
    IF gs_acc_data-mwskz IS NOT INITIAL.
      PERFORM bdc_field  USING 'BSEG-MWSKZ' gs_acc_data-mwskz.
      "PERFORM bdc_field  USING 'BKPF-XMWST' 'X'.
    ENDIF.
    IF lv_qty IS NOT INITIAL.
      PERFORM bdc_field  USING 'BSEG-MENGE' lv_menge.
      PERFORM bdc_field  USING 'BSEG-MEINS' lv_meins.
    ENDIF.
    PERFORM bdc_field  USING 'BSEG-SGTXT' gs_acc_data-sgtxt.
    PERFORM bdc_field  USING 'BSEG-ZUONR' gs_acc_data-zuonr.
    IF gs_acc_data-valut IS NOT INITIAL AND
       ls_skb1-fstag = 'BSHT'.  "Balance sheet acct
      PERFORM bdc_field  USING 'BSEG-VALUT' gs_acc_data-valut.
    ENDIF.
    "PERFORM bdc_field  USING 'DKACB-FMORE' 'X'.
    "---------------
    PERFORM bdc_screen USING 'SAPLKACB' '0002'.
    "---------------
    PERFORM bdc_field  USING 'BDC_OKCODE' '=ENTE'.
    IF gs_acc_data-pernr IS NOT INITIAL AND
       ls_skb1-fstag = 'PAYR'.
      "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-PERNR'.
      PERFORM bdc_field  USING 'COBL-PERNR' gs_acc_data-pernr.
    ENDIF.

    IF gs_acc_data-aufnr IS NOT INITIAL. "Internal Order
      "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-AUFNR'.
      PERFORM bdc_field  USING 'COBL-AUFNR' gs_acc_data-aufnr.
    ENDIF.
    IF gs_acc_data-kostl IS NOT INITIAL. "CC
      "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-KOSTL'.
      PERFORM bdc_field  USING 'COBL-KOSTL' gs_acc_data-kostl.
    ENDIF.
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
    IF gs_acc_data-sccentr IS NOT INITIAL. "CC
      "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-KOSTL'.
      PERFORM bdc_field  USING 'COBL-KOSTL' gs_acc_data-sccentr.
    ENDIF.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
    IF gs_acc_data-projk24 IS NOT INITIAL. "WBS
      "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-PS_POSID'.
      PERFORM bdc_field  USING 'COBL-PS_POSID' gs_acc_data-projk24.
    ENDIF.
    "------
    PERFORM bdc_screen USING 'SAPMF05A' '0700'.
    PERFORM bdc_field  USING 'BDC_CURSOR' 'RF05A-NEWKO'.
    "------------
  ENDLOOP.
*-----
  PERFORM close_transaction_post.
  PERFORM close_session.
  PERFORM release_bdc USING 'X'.  "text-049.

ENDFORM.                    " POST_DATA_BDC
*&---------------------------------------------------------------------*
*&      Form  OPEN_BATCH_SESSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM open_batch_session .

  gv_bdc_flag = 'X'.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = gv_group
*     HOLDDATE          = SY-DATUM
      keep              = 'X'
      user              = sy-uname
    EXCEPTIONS
      group_invalid     = 1
      group_is_locked   = 2
      holddate_invalid  = 3
      internal_error    = 4
      queue_error       = 5
      running           = 6
      system_lock_error = 7
      user_invalid      = 8.

  IF sy-subrc <> 0.
    MESSAGE e004 WITH gv_group.
  ENDIF.
ENDFORM.                    " OPEN_BATCH_SESSION
*&---------------------------------------------------------------------*
*&      Form  CLOSE_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM close_transaction_post .

  PERFORM bdc_screen USING 'SAPMF05A' '0700'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'RF05A-NEWBS'.
  PERFORM bdc_field USING 'BDC_OKCODE' '=BU'.

  PERFORM insert_session USING 'F-02'.
  "---------
  "gv_crosscompany = space.
ENDFORM.                    " CLOSE_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  BDC_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_screen  USING program dynpro.

  CLEAR bdcdata.
  bdcdata-program = program.
  bdcdata-dynpro = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.

ENDFORM.                    " BDC_SCREEN
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  INSERT_SESSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM insert_session USING iv_tcode TYPE tstc-tcode.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = iv_tcode
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.
  IF sy-subrc <> 0.
    MESSAGE e013 WITH sy-subrc.
  ENDIF.

  REFRESH bdcdata.
ENDFORM.                    " INSERT_SESSION
*&---------------------------------------------------------------------*
*&      Form  START_NEW_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_new_transaction_post .
  DATA: lv_date(10) TYPE c,
        lv_bktxt TYPE bkpf-bktxt.

  WRITE gs_acc_data-bldat TO lv_date(000010) MM/DD/YYYY.
* Header Data Screen (initial screen)
  PERFORM bdc_screen USING 'SAPMF05A' '100'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'RF05A-NEWKO'.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'BKPF-BLDAT' lv_date. "gs_acc_data-bldat.
  PERFORM bdc_field  USING 'BKPF-BLART' gs_acc_data-blart. "p_blart.
  PERFORM bdc_field  USING 'BKPF-BUKRS' gs_acc_data-bukrs.

  WRITE gs_acc_data-budat TO lv_date(000010) MM/DD/YYYY.
*  WRITE sy-datum TO lv_date(000010) MM/DD/YYYY.
  PERFORM bdc_field  USING 'BKPF-BUDAT' lv_date. "gs_acc_data-budat.
*  PERFORM bdc_field  USING 'BKPF-MONAT' gs_acc_data-monat.
  PERFORM bdc_field  USING 'BKPF-WAERS' gs_acc_data-waers.
  PERFORM bdc_field  USING 'BKPF-XBLNR' gs_acc_data-xblnr.
  PERFORM bdc_field  USING 'BKPF-BKTXT' gs_acc_data-bktxt.
  "
ENDFORM.                    " START_NEW_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  START_NEXT_LINEITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_next_lineitem .
*DATA: lv_wrbtr TYPE char13,
*        lv_date(10) TYPE c.
**
**SELECT SINGLE fstag INTO skb1-fstag
**    FROM skb1
**   WHERE bukrs = w_bukrs
**     AND saknr = xltab-saknr.
**  IF skb1-fstag = 'STCF'.
**    PERFORM bdc_field  USING 'RF05V-NEWBW' xltab-newbw. "Transaction Type
**  ENDIF.
*  WRITE gs_acc_data-wrbtr TO lv_wrbtr DECIMALS 2.
*  SHIFT lv_wrbtr LEFT DELETING LEADING '0'.
*  PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-SGTXT'.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '=ZK'.
*  PERFORM bdc_field  USING 'BSEG-WRBTR' lv_wrbtr. "gs_acc_data-wrbtr.
*  PERFORM bdc_field  USING 'BSEG-MWSKZ' gs_acc_data-mwskz.
*  PERFORM bdc_field  USING 'BKPF-XMWST' 'X'.
*
*  WRITE gs_acc_data-valut TO lv_date(000010) MM/DD/YYYY.
*  PERFORM bdc_field  USING 'BSEG-VALUT' lv_date. "gs_acc_data-valut.
*  PERFORM bdc_field  USING 'BSEG-ZUONR' gs_acc_data-zuonr.
*  PERFORM bdc_field  USING 'BSEG-SGTXT' gs_acc_data-sgtxt.
**  IF gs_acc_data-bschl = '40'.
**  ENDIF.
*  PERFORM bdc_field  USING 'DKACB-FMORE' 'X'.
*  "
*  PERFORM bdc_screen USING 'SAPLKACB' '0002'.
*  IF gs_acc_data-projk24 IS NOT INITIAL. "Project
*    PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-PS_POSID'.
*    PERFORM bdc_field  USING 'COBL-PS_POSID' gs_acc_data-projk24.
*  ENDIF.
*  IF gs_acc_data-aufnr IS NOT INITIAL. "Internal Order
*    PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-AUFNR'.
*    PERFORM bdc_field  USING 'COBL-AUFNR' gs_acc_data-aufnr.
*  ENDIF.
*  IF gs_acc_data-nplnr IS NOT INITIAL OR  "Network/Activity
*     gs_acc_data-vornr IS NOT INITIAL.
*    PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-VORNR'.
*    PERFORM bdc_field  USING 'COBL-NPLNR' gs_acc_data-nplnr.
*    PERFORM bdc_field  USING 'COBL-VORNR' gs_acc_data-vornr.
*  ENDIF.
*  IF gs_acc_data-kostl IS NOT INITIAL. "Cost Center
*    PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-KOSTL'.
*    PERFORM bdc_field  USING 'COBL-KOSTL' gs_acc_data-kostl.
*  ENDIF.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '=ENTE'.
*  "--------
*  PERFORM bdc_screen USING 'SAPMF05A' '0330'.
*  PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-XREF1'.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
*  PERFORM bdc_field  USING 'BSEG-XREF1' gs_acc_data-xblnr+3(12).
ENDFORM.                    " START_NEXT_LINEITEM
*&---------------------------------------------------------------------*
*&      Form  CLOSE_SESSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM close_session .
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open
      queue_error.
  IF sy-subrc = 0.
    "MESSAGE-ID zs
*    MESSAGE i003 WITH gv_group.
*    SKIP 1.
*    WRITE : / 'BDC Sesscion', gv_group , ' is created'.
*    SKIP 2.
  ENDIF.
ENDFORM.                    " CLOSE_SESSION
*&---------------------------------------------------------------------*
*&      Form  RELEASE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM release_bdc USING iv_msg TYPE char01. "tline-tdline.

  DATA: lv_group1 TYPE d0100-mapn,
        lt_seltab  TYPE TABLE OF rsparams,
        ls_seltab  LIKE LINE OF lt_seltab,
        lt_abaplist TYPE TABLE OF abaplist,
        lt_list TYPE list_string_table,
        ls_list TYPE LINE OF list_string_table,
        lt_apqi TYPE TABLE OF apqi,
        ls_apqi TYPE apqi,
        ls_text TYPE tline,
        lt_text TYPE TABLE OF tline.

  lv_group1 = gv_group.

  SUBMIT rsbdcsub WITH mappe EQ lv_group1
     WITH von EQ sy-datum
     WITH bis EQ sy-datum
     WITH fehler EQ 'X'
     WITH z_verarb EQ 'X'
*  WITH LOGALL EQ 'X'
     EXPORTING LIST TO MEMORY
     AND RETURN.
****Capture message from BDC
*  CALL FUNCTION 'LIST_FROM_MEMORY'
*    TABLES
*      listobject = lt_abaplist
*    EXCEPTIONS
*      not_found  = 1
*      OTHERS     = 2.
*  IF sy-subrc <> 0.
**   Implement suitable error handling here
*  ENDIF.
*  CALL FUNCTION 'LIST_TO_ASCI'
*    IMPORTING
*      list_string_ascii  = lt_list
*    TABLES
*      listobject         = lt_abaplist
*    EXCEPTIONS
*      empty_list         = 1
*      list_index_invalid = 2
*      OTHERS             = 3.
*  IF sy-subrc <> 0.
*  ENDIF.
*  LOOP AT lt_list INTO ls_list.
**    ls_text-tdline = ls_list.
**    APPEND ls_text TO lt_text.
*    IF iv_msg <> 'E'.
*      gt_bdc_rel_msg-id = gv_group.
*      gt_bdc_rel_msg-msg = ls_list.
*      APPEND gt_bdc_rel_msg.
*    ELSE.
*      gt_bdc_rel_msg_error-id = gv_group.
*      gt_bdc_rel_msg_error-msg = ls_list.
*      APPEND gt_bdc_rel_msg_error.
*    ENDIF.
*  ENDLOOP.
*  CALL FUNCTION 'LIST_FREE_MEMORY'
*    TABLES
*      listobject = lt_abaplist.
******Get BDC status from table
*  WAIT UP TO 20 SECONDS.
****
*  ls_text-tdline = iv_msg.
*  APPEND ls_text TO gt_bdc_rel_msg.
****
*  SELECT * FROM apqi INTO TABLE lt_apqi
*    WHERE groupid = lv_group1
*      AND creator = sy-uname
*      AND credate = sy-datum.
*  SORT lt_apqi BY credate cretime DESCENDING.
*  IF lt_apqi[] IS NOT INITIAL.
*    READ TABLE lt_apqi INTO ls_apqi INDEX 1.
*    IF ls_apqi-qstate = 'E'.
*      CONCATENATE '******SESSION' lv_group1 text-048 INTO ls_text-tdline
*                                                      SEPARATED BY space.
*      APPEND ls_text TO gt_bdc_rel_msg.
*    ENDIF.
*  ENDIF.
***************************
*  IF lt_text[] IS INITIAL.
*    ls_text-tdline = 'No BDC Release message for Posted Document'.
*    APPEND ls_text TO lt_text.
*  ENDIF.
*  IF iv_msg = 'E'.
*    APPEND LINES OF lt_text[] TO gt_bdc_rel_msg_error[].
*  ELSE.
*    APPEND LINES OF lt_text[] TO gt_bdc_rel_msg[].
*  ENDIF.

***End of Capture message
ENDFORM.                    " RELEASE_BDC
*&---------------------------------------------------------------------*
*&      Form  CLOSE_TRANSACTION_PARK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM close_transaction_park .

  PERFORM bdc_field  USING 'BDC_CURSOR' 'BKPF-XBLNR'.
  PERFORM bdc_field USING 'BDC_OKCODE' '=BP'.
  "-----
  PERFORM bdc_screen USING 'SAPLZFI_TV_WORKFLOW' '0300'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'POPUP-TEXTLINE1'.
  PERFORM bdc_field USING 'BDC_OKCODE' '=OPT1'.
  "------
  PERFORM insert_session USING 'FBV1'.
ENDFORM.                    " CLOSE_TRANSACTION_PARK
*&---------------------------------------------------------------------*
*&      Form  START_NEW_TRANSACTION_PARK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM start_new_transaction_park .

  DATA: lv_date(10) TYPE c,
        lv_bktxt TYPE bkpf-bktxt.

  WRITE gs_acc_data-bldat TO lv_date(000010) MM/DD/YYYY.
* Header Data Screen (initial screen)
  PERFORM bdc_screen USING 'SAPLF040' '100'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'RF05V-NEWKO'.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'BKPF-BLDAT' lv_date. "gs_acc_data-bldat.
  PERFORM bdc_field  USING 'BKPF-BLART' ' '. "p_blart.
  PERFORM bdc_field  USING 'BKPF-BUKRS' gs_acc_data-bukrs.

*  WRITE gs_acc_data-budat TO lv_date(000010) MM/DD/YYYY.
  WRITE sy-datum TO lv_date(000010) MM/DD/YYYY.
  PERFORM bdc_field  USING 'BKPF-BUDAT' lv_date. "gs_acc_data-budat.
*  PERFORM bdc_field  USING 'BKPF-MONAT' gs_acc_data-monat.
  PERFORM bdc_field  USING 'BKPF-WAERS' gs_acc_data-waers.
  PERFORM bdc_field  USING 'BKPF-XBLNR' gs_acc_data-xblnr.
  PERFORM bdc_field  USING 'BKPF-BKTXT' lv_bktxt.   "gs_acc_data-bktxt.
  PERFORM bdc_field  USING 'VBKPF-XBWAE' 'X'.
  PERFORM bdc_field  USING 'FS006-DOCID' '*'.
  "
*  PERFORM bdc_field  USING 'RF05V-NEWBS' gs_acc_data-bschl.
*  IF gs_acc_data-bschl = '31' or
*     gs_acc_data-bschl = '22'.
*    PERFORM bdc_field  USING 'RF05V-NEWKO' gs_acc_data-lifnr. "saknr.
*  ELSE.
*    PERFORM bdc_field  USING 'RF05V-NEWKO' gs_acc_data-saknr.
*  ENDIF.

ENDFORM.                    " START_NEW_TRANSACTION_PARK
*&---------------------------------------------------------------------*
*&      Form  BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_OBJTXT  text
*----------------------------------------------------------------------*
FORM build_mail_content CHANGING p_lt_objtxt TYPE table.

  DATA: ls_data TYPE ty_acc_data,
        ls_objtxt       TYPE solisti1,
        lv_lines TYPE n LENGTH 10,
        lv_wrbtr(14),
        lv_menge(13),
        lv_rec(6),
        lv_tabix TYPE sy-tabix,
        lv_text(250),
        ls_text TYPE tline,
        ls_file_list TYPE LINE OF ty_it_file_list.

*Prepare HTML mail header
  CONCATENATE '<html>'
              '<body>'
              '<h4 style="font-family:arial"><caption><b><u>'
              text-012
              '</u></b><caption></h4>'
              '<ul>'
              '</ul>'
         INTO ls_objtxt
         SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  "-------------------
  CONCATENATE '<table border="2" width="100%">' ' '
              INTO ls_objtxt
              SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  "BDC session info
  CONCATENATE  '<tr> <td><b>'
                'System</td><td>'
                'BDC ID</b></td></tr>'
                INTO ls_objtxt
                SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  LOOP AT s_gid.
    CONCATENATE  '<tr> <td>'
                sy-sysid '</td><td>'
                s_gid-low '</td></tr>'
                INTO ls_objtxt
                SEPARATED BY space.
    APPEND ls_objtxt TO p_lt_objtxt.
  ENDLOOP.
  MOVE: '</table> <br>' TO ls_objtxt.
  APPEND: ls_objtxt TO p_lt_objtxt.
  "Records processed from input file
  CONCATENATE '<table border="2" width="100%">' ' '
              INTO ls_objtxt
              SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  CONCATENATE  '<tr> <td><b>'
               'File(s) Processed</b></td></tr>'
               INTO ls_objtxt
               SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  CONCATENATE  '<tr> <td><b>'
               'File Name</td><td>'
               'Records</b></td></tr>'
               INTO ls_objtxt
               SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  LOOP AT gt_fileinfo INTO gs_fileinfo.
    lv_rec = gs_fileinfo-tot_rec.
    CONCATENATE  '<tr> <td>'
                gs_fileinfo-name'</td><td>'
                lv_rec'</td></tr>'
                INTO ls_objtxt
                SEPARATED BY space.
    APPEND ls_objtxt TO p_lt_objtxt.
  ENDLOOP.
  MOVE: '</table> <br>' TO ls_objtxt.
  APPEND: ls_objtxt TO p_lt_objtxt.
  "--------Duplicate file
  CONCATENATE '<table border="2" width="100%">' ' '
               INTO ls_objtxt
               SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  CONCATENATE  '<tr> <td><b>'
               'Duplicate File Name (Not processed)</b></td></tr>'
               INTO ls_objtxt
               SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  LOOP AT gt_file_list INTO ls_file_list WHERE exist = 'X'.
    CONCATENATE  '<tr> <td>'
              ls_file_list-filename'</td></tr>'
              INTO ls_objtxt
              SEPARATED BY space.
    APPEND ls_objtxt TO p_lt_objtxt.
  ENDLOOP.
  MOVE: '</table> <br>' TO ls_objtxt.
  APPEND: ls_objtxt TO p_lt_objtxt.
****BDC log
  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td><b>'
              'BDC Log' '</b></td></tr>'
              INTO ls_objtxt
              SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  "-----------
  PERFORM get_bdc_messages.
  "-----------
  LOOP AT gt_bdc_rel_msg_one INTO ls_text.
    CONCATENATE  '<tr> <td>'
                sy-sysid '</td><td>'
                ls_text-tdline '</td></tr>'
                INTO ls_objtxt
                SEPARATED BY space.
    APPEND ls_objtxt TO p_lt_objtxt.
  ENDLOOP.

  MOVE: '</table> <br>' TO ls_objtxt.
  APPEND: ls_objtxt TO p_lt_objtxt.
************log file name
*  CONCATENATE  '<tr> <td>'
*                  text-051 '</td><td>'
*                  gv_logfile_path gv_logfile_name '</td></tr>'
*                  INTO ls_objtxt
*                  SEPARATED BY space.
*  APPEND ls_objtxt TO p_lt_objtxt.
  "----------------Messages
  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td>'
              INTO ls_objtxt
              SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  CONCATENATE  '<tr> <td><b>'
               'Log - Messsages' '</b></td></tr>'
                INTO ls_objtxt
                SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  LOOP AT gt_id_msg INTO gs_id_msg.
    CONCATENATE  '<tr> <td>'
                sy-sysid '</td><td>'
                gs_id_msg-msg_text '</td></tr>'
                INTO ls_objtxt
                SEPARATED BY space.
    APPEND ls_objtxt TO p_lt_objtxt.
  ENDLOOP.
  MOVE: '</table> <br>' TO ls_objtxt.
  APPEND: ls_objtxt TO p_lt_objtxt.
  "Records with Suspense Acct
  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td>'
              INTO ls_objtxt
              SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  CONCATENATE  '<tr> <td><b>'
               'Records with Suspension Acct or Wrong GL Acct..' '</b></td></tr>'
                INTO ls_objtxt
                SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
  LOOP AT gt_acc_data_error_sacct INTO ls_data.
    IF ls_data-sactt IS INITIAL
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
    AND ls_data-sccentr IS INITIAL.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
      CONTINUE.
    ENDIF.
    lv_wrbtr = ls_data-wrbtr.
    lv_menge = ls_data-menge.
    CONCATENATE ls_data-bukrs  ls_data-bldat  ls_data-budat ls_data-blart
    ls_data-waers ls_data-bktxt ls_data-xblnr ls_data-bschl ls_data-shkzg
    ls_data-saknr ls_data-valut lv_wrbtr ls_data-mwskz lv_menge
    ls_data-meins ls_data-zuonr ls_data-sgtxt ls_data-kostl ls_data-aufnr
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
*    ls_data-projk ls_data-pernr ls_data-zlspr ls_data-sactt ls_data-filename
     ls_data-projk24 ls_data-pernr ls_data-zlspr ls_data-sactt ls_data-sccentr
     ls_data-filename
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
    INTO lv_text SEPARATED BY space.
    CONCATENATE  '<tr> <td>'
                sy-sysid '</td><td>'
                lv_text '</td></tr>'
                INTO ls_objtxt
                SEPARATED BY space.
    APPEND ls_objtxt TO p_lt_objtxt.
  ENDLOOP.
  MOVE: '</table> <br>' TO ls_objtxt.
  APPEND: ls_objtxt TO p_lt_objtxt.
*End of HTML text here
  CONCATENATE '</body>'
              '</html>'
         INTO ls_objtxt
         SEPARATED BY space.
  APPEND ls_objtxt TO p_lt_objtxt.
*End of HTML text here
  CONCATENATE '</body>'
              '</html>'
         INTO ls_objtxt
         SEPARATED BY space.

  APPEND ls_objtxt TO p_lt_objtxt.

ENDFORM.                    " BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*&      Form  START_NEXT_LINEITEM_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_next_lineitem_post .

  PERFORM bdc_field  USING 'RF05A-NEWBS' gs_acc_data-bschl.
  IF gs_acc_data-sactt IS NOT INITIAL.
    "p_check1 IS NOT INITIAL.
    PERFORM bdc_field  USING 'RF05A-NEWKO' gs_acc_data-sactt.
  ELSE.
    PERFORM bdc_field  USING 'RF05A-NEWKO' gs_acc_data-saknr.
  ENDIF.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.

ENDFORM.                    " START_NEXT_LINEITEM_POST
*&---------------------------------------------------------------------*
*&      Form  START_NEXT_LINEITEM_PARK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_next_lineitem_park .

  PERFORM bdc_field  USING 'BDC_CURSOR' 'RF05V-NEWKO'.
  PERFORM bdc_field  USING 'RF05V-NEWBS' gs_acc_data-bschl.
  IF gs_acc_data-bschl = '31' OR
     gs_acc_data-bschl = '22'.
*    PERFORM bdc_field  USING 'RF05V-NEWKO' gs_acc_data-lifnr. "saknr.
  ELSE.
    PERFORM bdc_field  USING 'RF05V-NEWKO' gs_acc_data-saknr.
  ENDIF.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.

ENDFORM.                    " START_NEXT_LINEITEM_PARK
*&---------------------------------------------------------------------*
*&      Form  GET_BDC_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_bdc_messages .
  DATA: lt_seltab  TYPE TABLE OF rsparams,
          ls_seltab  LIKE LINE OF lt_seltab,
          lt_abaplist TYPE TABLE OF abaplist,
          lt_list TYPE list_string_table,
          ls_list TYPE LINE OF list_string_table,
          ls_text TYPE tline,
          lt_text TYPE TABLE OF tline,
          ls_gid TYPE RANGE OF apqi-groupid WITH HEADER LINE.

  "-------------------------------------------------------------
  s_date-sign = 'I'.
  s_date-option = 'EQ'.
  s_date-low = sy-datum.
  APPEND s_date.
**  IF gv_group_post IS NOT INITIAL.
**    s_gid-sign = 'I'.
**    s_gid-option = 'EQ'.
**    s_gid-low = gv_group_post.
**    APPEND s_gid.
**  ENDIF.
*  ls_gid-sign = 'I'.
*  ls_gid-option = 'EQ'.
*  ls_gid-low = s_gid-low.
*  APPEND ls_gid.

  CLEAR: gt_bdc_rel_msg_one.
  REFRESH gt_bdc_rel_msg_one.
*  "----------------------------
  IF s_gid[] IS NOT INITIAL.
    WAIT UP TO 90 SECONDS.
    SUBMIT zfglr030_bdc_log "VIA SELECTION-SCREEN
                 WITH s_gid IN s_gid
                 WITH s_date IN s_date
                 EXPORTING LIST TO MEMORY
                 AND RETURN.
    WAIT UP TO 60 SECONDS.
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
    "-------------
    APPEND LINES OF lt_text[] TO gt_bdc_rel_msg_one[].
    LOOP AT lt_text INTO ls_text.
*    gt_bdc_rel_msg-id = s_gid-low.
      gt_bdc_rel_msg-msg = ls_text-tdline.
      APPEND gt_bdc_rel_msg.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_BDC_MESSAGES
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESSED_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM update_processed_file .
  DATA: lv_commit(1),
        ls_file TYPE LINE OF ty_it_file_list,
        ls_payfile TYPE zfi_wd_payfile.

  LOOP AT gt_file_list INTO ls_file.
    IF ls_file-exist IS NOT INITIAL.
      CONTINUE.
    ENDIF.
    ls_payfile-filename = ls_file-filename.
    ls_payfile-crt_date = sy-datum.
    ls_payfile-crt_time = sy-uzeit.
    INSERT zfi_wd_payfile FROM ls_payfile.
    lv_commit = 'X'.
  ENDLOOP.
  IF lv_commit IS NOT INITIAL.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    " UPDATE_PROCESSED_FILE
*&---------------------------------------------------------------------*
*&      Form  POST_DATA_BDC_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM post_data_bdc_error .
  DATA:   trans_in_process(1),            "flag - trans in process
            open_session(1) VALUE 'X',      "flag - batch session open?
            lv_not_first TYPE xfeld,
            lv_post TYPE xfeld,
            lv_docno TYPE i,
            lv_records TYPE i,
            ls_lfb1 TYPE lfb1,
            lv_zzloc TYPE zzloc,
            lv_screen(4),
            lv_meins(3) ,
            ls_lfbw TYPE lfbw,
            lv_tax TYPE wmwst,
            lv_amount TYPE wrbtr,
            lv_qty TYPE menge_d.
  DATA:  lv_net_amount TYPE bseg-wrbtr,
         lv_wrbtr TYPE char13,
         lv_menge TYPE char13,
         lv_tax_amount TYPE char13,
         ls_ska1 TYPE ska1,
         ls_skb1 TYPE skb1,
         lt_doc_data TYPE TABLE OF ty_acc_data.

  SORT gt_acc_data_error BY docno ASCENDING itemno DESCENDING.
  DESCRIBE TABLE gt_acc_data_error LINES lv_records.
  "------------------------
  CHECK gt_acc_data_error[] IS NOT INITIAL.
  WAIT UP TO 1 SECONDS.
  "-----------------------------
  REFRESH bdcdata.

  CONCATENATE 'ZFI-PY-ERR' sy-uzeit+4(2) INTO gv_group_post. " = p_bdcid.
*  gv_group_post = 'ZFI-PY-ERR'.
  gv_group = gv_group_post. "p_bdcid.
  PERFORM open_batch_session.
  "-------Collect BDC sesseion info for use in BDC log routine
  READ TABLE s_gid WITH KEY low = gv_group_post TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    s_gid-sign = 'I'.
    s_gid-option = 'EQ'.
    s_gid-low = gv_group_post.
    APPEND s_gid.
  ENDIF.
  "-----------------------
  LOOP AT gt_acc_data_error INTO gs_acc_data.
    CLEAR: ls_skb1,
           ls_ska1.
    IF gs_acc_data-sactt IS INITIAL.
      SELECT SINGLE * FROM skb1 INTO ls_skb1
          WHERE bukrs = gs_acc_data-bukrs
            AND saknr = gs_acc_data-saknr.
    ELSE.
      SELECT SINGLE * FROM skb1 INTO ls_skb1
        WHERE bukrs = gs_acc_data-bukrs
          AND saknr = gs_acc_data-sactt.
      SELECT SINGLE * FROM ska1 INTO ls_ska1
        WHERE saknr = gs_acc_data-sactt.
    ENDIF.
*   Open batch session, if not open.
    IF gs_acc_data-docno <> lv_docno.
      IF lv_not_first IS NOT INITIAL.
        PERFORM close_transaction_post.
      ENDIF.
      lv_docno = gs_acc_data-docno.
      lv_not_first = 'X'.
      "Document Header
      PERFORM start_new_transaction_post.
    ENDIF.
    PERFORM start_next_lineitem_post.
    "------------
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = gs_acc_data-meins
        language       = sy-langu
      IMPORTING
*       LONG_TEXT      =
        output         = lv_meins
*       SHORT_TEXT     =
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    "GL Line item
    lv_net_amount = gs_acc_data-wrbtr.
    lv_qty = gs_acc_data-menge.
    IF lv_qty < 0.
      lv_qty = lv_qty * -1.
    ENDIF.
    WRITE lv_net_amount TO lv_wrbtr DECIMALS 2.
    WRITE lv_qty TO lv_menge DECIMALS 3.
    "------
    PERFORM bdc_screen USING 'SAPMF05A' '0300'.
    "------------
    PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-WRBTR'.
    PERFORM bdc_field  USING 'BDC_OKCODE' '=AB'.
    PERFORM bdc_field  USING 'BSEG-WRBTR' lv_wrbtr.
    IF gs_acc_data-mwskz IS NOT INITIAL.
      PERFORM bdc_field  USING 'BSEG-MWSKZ' gs_acc_data-mwskz.
      "      PERFORM bdc_field  USING 'BKPF-XMWST' 'X'.
    ENDIF.
    IF lv_qty IS NOT INITIAL.
      PERFORM bdc_field  USING 'BSEG-MENGE' lv_menge.
      PERFORM bdc_field  USING 'BSEG-MEINS' lv_meins.
    ENDIF.
    PERFORM bdc_field  USING 'BSEG-SGTXT' gs_acc_data-sgtxt.
    PERFORM bdc_field  USING 'BSEG-ZUONR' gs_acc_data-zuonr.
    IF gs_acc_data-valut IS NOT INITIAL AND
       ls_skb1-fstag = 'BSHT'.  "Balance sheet acct
      PERFORM bdc_field  USING 'BSEG-VALUT' gs_acc_data-valut.
    ENDIF.

    "PERFORM bdc_field  USING 'DKACB-FMORE' 'X'.
    "---------------
    PERFORM bdc_screen USING 'SAPLKACB' '0002'.
    "---------------
    PERFORM bdc_field  USING 'BDC_OKCODE' '=ENTE'.
    IF ls_ska1-xbilk IS INITIAL.   "if Suspense Acct is not balance sheet
      IF gs_acc_data-pernr IS NOT INITIAL AND
         ls_skb1-fstag = 'PAYR'.
        "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-PERNR'.
        PERFORM bdc_field  USING 'COBL-PERNR' gs_acc_data-pernr.
      ENDIF.
      IF gs_acc_data-aufnr IS NOT INITIAL. "Internal Order
        "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-AUFNR'.
        PERFORM bdc_field  USING 'COBL-AUFNR' gs_acc_data-aufnr.
      ENDIF.
      IF gs_acc_data-kostl IS NOT INITIAL. "CC
        "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-KOSTL'.
        PERFORM bdc_field  USING 'COBL-KOSTL' gs_acc_data-kostl.
      ENDIF.
**--BEGIN OF CHANGES FOR CHG0137485 BY KBANERJEE
      IF gs_acc_data-sccentr IS NOT INITIAL. "CC
        "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-KOSTL'.
        PERFORM bdc_field  USING 'COBL-KOSTL' gs_acc_data-sccentr.
      ENDIF.
**--END OF CHANGES FOR CHG0137485 BY KBANERJEE
      IF gs_acc_data-projk24 IS NOT INITIAL. "WBS
        "PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-PS_POSID'.
        PERFORM bdc_field  USING 'COBL-PS_POSID' gs_acc_data-projk24.
      ENDIF.
    ENDIF.
    "------
*    PERFORM bdc_screen USING 'SAPMF05A' '0300'.
*    PERFORM bdc_field  USING 'BDC_OKCODE' '=AB'.
*    "------
*    PERFORM bdc_screen USING 'SAPLKACB' '0002'.
*    PERFORM bdc_field  USING 'BDC_OKCODE' '=ENTE'.
    "------
    PERFORM bdc_screen USING 'SAPMF05A' '0700'.
    PERFORM bdc_field  USING 'BDC_CURSOR' 'RF05A-NEWKO'.
    "------------
  ENDLOOP.
*-----
  PERFORM close_transaction_post.
  PERFORM close_session.
  PERFORM release_bdc USING 'E'.  "text-049.
ENDFORM.                    " POST_DATA_BDC_ERROR
*&---------------------------------------------------------------------*
*&      Form  GET_DATE_MONTHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date_months .

  SELECT * FROM t247 INTO TABLE gt_t247
    WHERE spras = sy-langu.

ENDFORM.                    " GET_DATE_MONTHS
*&---------------------------------------------------------------------*
*&      Form  GET_GENERAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_general_data .

  CLEAR : gt_zfi_wd_susccentr.
  SELECT * FROM zfi_wd_susccentr INTO TABLE gt_zfi_wd_susccentr.

ENDFORM.                    " GET_GENERAL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_REPLACE_SCCENTR
*&---------------------------------------------------------------------*
*      "replace with suspense cost center and clear the IO or WBS
*----------------------------------------------------------------------*
*      <--PC_ACC_DATA Accounting data
*----------------------------------------------------------------------*
FORM f_replace_sccentr  CHANGING pc_acc_data TYPE ty_acc_data.
  DATA: ls_susccentr TYPE zfi_wd_susccentr.
  IF pc_acc_data-aufnr IS NOT INITIAL.
    CLEAR pc_acc_data-aufnr.
  ELSEIF pc_acc_data-projk24 IS NOT INITIAL.
    CLEAR pc_acc_data-projk24.
  ENDIF.
  READ TABLE gt_zfi_wd_susccentr INTO ls_susccentr
                                 WITH KEY bukrs = pc_acc_data-bukrs.
  IF sy-subrc = 0.
    pc_acc_data-error   = 'X'.
    pc_acc_data-sccentr = ls_susccentr-kostl.
    PERFORM conversion_routine CHANGING pc_acc_data-sccentr.
  ENDIF.
ENDFORM.                    " F_REPLACE_SCCENTR
