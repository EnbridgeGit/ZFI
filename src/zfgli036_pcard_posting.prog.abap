*&---------------------------------------------------------------------*
*& Report  ZFGLI036_PCARD_POSTING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfgli036_pcard_posting MESSAGE-ID zs.

DATA: BEGIN OF bdcdata OCCURS 500.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
*include: ZCCOI006_COST_PLAN_LOAD_F01.

TYPES: BEGIN OF ty_data,
       col1(1) TYPE c,       " Record Type
       col2    TYPE string,  " Document Date / Posting key
       col3    TYPE string,  " Header Company code/Line item cc
       col4    TYPE string,  " Posting Date / LC Amount
       col5    TYPE string,  " Currency/Value Date
       col6    TYPE string,  " BOA Reference/GL Code
       col7    TYPE string,  " Doc Header / Cost Center
       col8    TYPE string,  " Internal Order
       col9    TYPE string,  " Network
       col10   TYPE string,  " Activity
       col11   TYPE string,  " WBS
       col12   TYPE string,  " BOA Trans. ID
       col13   TYPE string,  " Country of Trans.
       col14   TYPE string,  " Region/State of Tans.
       col15   TYPE string,  " Last 4 digit of CC
       col16   TYPE string,  " Card Holder Last name
       col17   TYPE string,  " Card Holder first name
       col18   TYPE string,  " Line Item text (Merchant name)
       docno   TYPE i,
       END OF ty_data.
TYPES: BEGIN OF ty_acc_data,
         docno  TYPE i,
         itmno  TYPE i,
         bldat  TYPE bkpf-bldat, "DOCUMENT DATE
         bukrs  TYPE bkpf-bukrs, "COMPANY CODE
         budat  TYPE bkpf-budat, "POSTING DATE
         waers  TYPE bkpf-waers, "CURRENCY
         xblnr  TYPE bkpf-xblnr, "Reference Document #
         bktxt  TYPE bkpf-bktxt, "HEADER TEXT
         blart  TYPE bkpf-blart, "DOCUMENT TYPE
         bschl  TYPE bseg-bschl, "POSTING KEY
         newbk  TYPE rf05a-newbk,"COMPANY CD NEXT LINE
         abukrs TYPE bseg-bukrs,
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
CONSTANTS: gc_blart TYPE bkpf-blart VALUE 'PP',
           gc_saknr TYPE bseg-saknr VALUE '0000251043'.  "Offset GL account
DATA: gv_brgru TYPE t003-brgru,
      gv_group TYPE apqi-groupid,
      gv_bdc_group TYPE i,
      gt_id_msg TYPE TABLE OF ty_msg,
      gs_id_msg LIKE LINE OF gt_id_msg.
DATA: gt_data TYPE TABLE OF ty_data,
      gs_data TYPE ty_data,
      gt_acc_data TYPE TABLE OF ty_acc_data,
      gs_acc_data TYPE ty_acc_data,
      gt_doc_data TYPE TABLE OF ty_acc_data,
      gs_doc_data TYPE ty_acc_data,
      gt_bdc_data TYPE TABLE OF ty_acc_data.
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
      gv_sfile TYPE text1024. "rfpdo-rfbifile.
DATA: gv_obj_id        TYPE char14,            "Object ID            "
      gv_flag_pres     TYPE flag,              "Flag-Presentatn Server
      gv_fdelim        TYPE char1.             "File Delimiter       "
DATA: gv_logfile_path TYPE zfit_xparam-value1,
      gv_logfile_name TYPE zfit_xparam-value1.
DATA: gv_nbr_file      TYPE numc5,             "File Sequence Number "
      gv_flag_err_proc TYPE flag.             "Flag-Errors-Process

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
                         VALUE 'I_P2C_GL_036',
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
                         VALUE 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X'  USER-COMMAND cmd,
            p_sfile   LIKE        rfpdo-rfbifile MODIF ID dsp,
            p_ifile   LIKE        rfpdo-rfbifile MODIF ID srv,
            r_local   RADIOBUTTON GROUP rad1,
            p_file    TYPE        rfpdo-rfbifile DEFAULT 'H:\' MODIF ID lcl,
            p_objid   TYPE        zparamkey        "Object ID            "
                                  VISIBLE LENGTH 12
                                  OBLIGATORY MODIF ID dsp.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS: p_arfile TYPE rfpdo-lboxfile OBLIGATORY MODIF ID dsp,
            p_arfnam TYPE rfpdo-lboxfile NO-DISPLAY DEFAULT 'PCARD'.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS:  p_test TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
* Select the program parameter values
  PERFORM  f_select_xparam.

AT SELECTION-SCREEN OUTPUT.
  PERFORM  f_toggle_functionality.

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

  CLEAR: gv_flag_err_proc,
         git_file_list[],
         gt_data,
        gt_acc_data,
        gt_doc_data,
        gt_bdc_data,
        gt_msg,
        gt_tab,
        gv_bdc_group,
        gt_id_msg,
        gv_logfile_name,
        gv_logfile_path.
* Build the list of files to be processed
  PERFORM  f_build_file_list.

  IF gv_flag_err_proc <> 'X'.
    "if user has no authorization, dont execute.
    SELECT SINGLE brgru INTO gv_brgru FROM t003
      WHERE blart = gc_blart.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BLA'
             ID 'BRGRU' FIELD gv_brgru
             ID 'ACTVT' FIELD '01'.
    IF sy-subrc <> 0.
      WRITE : / sy-uname, 'dont have access to run this interface'.
      STOP.
    ENDIF.
    "gv_targetpath.
    REFRESH: gt_tab,
             gt_msg.
    IF r_server = 'X' AND
       p_sfile IS INITIAL.
      WRITE : / 'Please enter Application Server File Path.'.
      STOP.
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
    IF gt_data IS INITIAL.
      gv_flag_err_proc = 'X'.
      gs_id_msg-msgrp = 'G'.
      gs_id_msg-msgid = 'ZFI01'.
      gs_id_msg-msgty = 'E'.
      gs_id_msg-msgno = '000'.
      MOVE text-034 TO gs_id_msg-msg_text.
      APPEND gs_id_msg TO gt_id_msg.
      CLEAR gs_id_msg.
*      SKIP 1.
*      WRITE : / 'No file data to process..'.
*      STOP.
    ENDIF.
    IF gv_flag_err_proc <> 'X'.
      CONCATENATE 'PCARD' sy-datum INTO gv_group. "BDC Session ID
      PERFORM prepare_data.
      PERFORM maintain_899_rows_new.
      PERFORM validate_post_data.
      SKIP 2.
      WRITE: / '*----------------Following are messages -------------------*'.
      IF p_test IS INITIAL AND
         gt_bdc_data[] IS NOT INITIAL.
        PERFORM create_bdc.
      ENDIF.
      IF p_test IS NOT INITIAL.
        SKIP 1.
        WRITE : / 'Interface is completed in TEST Mode..'.
      ELSE.
        "Not in test mode, then archive the file
        IF r_server IS NOT INITIAL.
*          PERFORM  f_archive_file.
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
  "------------Output messages
  PERFORM display_messages.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE_SERVER
*&---------------------------------------------------------------------*
*       Upload file from Application Server
*----------------------------------------------------------------------*
FORM upload_file_server .

  DATA: ls_msg TYPE ty_msg,
        lwa_file_list TYPE ty_wa_file_list.

*  CONDENSE p_sfile NO-GAPS.
*  CONDENSE p_ifile NO-GAPS.
*  CONCATENATE p_sfile p_ifile INTO gv_sfile.
  LOOP AT git_file_list INTO lwa_file_list.
*Filename_FP_in
*Filename_fn_in
    CONCATENATE lwa_file_list-filename_fp_in lwa_file_list-filename_fn_in
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
*    APPEND gt_msg.
*    WRITE : / 'Can not open text file for process, Please check text file, security or path'.
      EXIT.
    ENDIF.
    DO.
      CLEAR: gs_data,
             gt_tab.
      READ DATASET gv_sfile INTO gt_tab-text1. "fl_string.
      IF sy-subrc <> 0.
*      WRITE: / 'Unable to read File data..'.
        EXIT.
      ENDIF.
      PERFORM split_data USING gt_tab
                               gs_data.
      APPEND gs_data TO gt_data.
    ENDDO.
    CLOSE DATASET gv_sfile.
    IF sy-subrc <> 0.
*    gt_msg-text1 = 'Unable to close text file for process, Please check.'.
*    APPEND gt_msg.
*    WRITE: / 'Unable to close text file for process, Please check.'.
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
        INTO p_output-col1 "RECORD TYPE
             p_output-col2  " Document Date / Posting key
             p_output-col3  " Header Company code/Line item cc
             p_output-col4  " Posting Date / LC Amount
             p_output-col5  " Currency/Value Date
             p_output-col6  " BOA Reference/GL Code
             p_output-col7  " Doc Header / Cost Center
             p_output-col8  " Internal Order
             p_output-col9  " Network
             p_output-col10  " Activity
             p_output-col11  " WBS
             p_output-col12  " BOA Trans. ID
             p_output-col13  " Country of Trans.
             p_output-col14  " Region/State of Tans.
             p_output-col15  " Last 4 digit of CC
             p_output-col16  " Card Holder Last name
             p_output-col17  " Card Holder first name
             p_output-col18.  " Line Item text (Merchant name).

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
*      CONCATENATE 'Read error in file' p_file+2
*            'in disk drive' p_file+0(2) INTO gt_msg-text1
*            SEPARATED BY space.
*      APPEND gt_msg.
*      WRITE: / gt_msg-text1.
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
**      MESSAGE e503(fv) WITH p_file+2 p_file+0(2) INTO gt_msg-text1.
*      CONCATENATE 'Read error in file' p_file+2
*       'in disk drive' p_file+0(2) INTO gt_msg-text1
*       SEPARATED BY space.
*      APPEND gt_msg.
*      WRITE: / gt_msg-text1.
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
FORM prepare_data .

  CONSTANTS: lc_blart TYPE bkpf-blart VALUE 'PP',
             lc_us    TYPE t005-land1 VALUE 'US',
             lc_ca    TYPE t005-land1 VALUE 'CA'.
  DATA: lv_counter TYPE i,
        lv_debit  TYPE bseg-wrbtr,
        lv_credit TYPE bseg-wrbtr,
        lv_land1  TYPE land1,
        lv_rgion  TYPE zrgion,
        lv_date   TYPE datum,
        lv_dat1 TYPE char6,
        lv_docno TYPE i,
        ls_t005   TYPE t005.
*Input file has line items before the header record. to handle
*this issue following logic is developed and keep the remaining
*logic as it is.
  lv_docno = 1.
  LOOP AT gt_data INTO gs_data.
    CASE gs_data-col1.
      WHEN '1'.  "Header
        gs_data-docno = lv_docno.
        MODIFY gt_data FROM gs_data TRANSPORTING docno.
        lv_docno = lv_docno + 1.
      WHEN '2'.  "Line item
        gs_data-docno = lv_docno.
        MODIFY gt_data FROM gs_data TRANSPORTING docno.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
  ENDLOOP.
  SORT gt_data BY docno col1.
*end of line/header items.
  LOOP AT gt_data INTO gs_data.
    CASE gs_data-col1.
      WHEN '1'.  "Header
        CLEAR: gs_acc_data.
        CONDENSE gs_data-col2 NO-GAPS.
        CONDENSE gs_data-col3 NO-GAPS.
        CONDENSE gs_data-col4 NO-GAPS.
        CONDENSE gs_data-col5 NO-GAPS.
        CONDENSE gs_data-col6.
        CONDENSE gs_data-col8.
        "MMDDYYYY
        "YYYYMMDD
        gs_acc_data-xblnr = gs_data-col6.
        CONCATENATE gs_data-col2+4(4) gs_data-col2(2) gs_data-col2+2(2) INTO lv_date.
        gs_acc_data-bldat = lv_date. "gs_data-col2.
        gs_acc_data-budat = lv_date. "gs_data-col2. "gs_data-col4.
*        MOVE gs_acc_data-xblnr+3(6) TO lv_dat1.
*        CONCATENATE '20' lv_dat1+4(2) lv_dat1(2) lv_dat1+2(2) INTO lv_date.
*        gs_acc_data-budat = lv_date.
*        gs_acc_data-bldat = lv_date.
        gs_acc_data-bukrs = gs_data-col3.
        gs_acc_data-waers = gs_data-col5.
        gs_acc_data-bktxt = gs_data-col7.
        gs_acc_data-blart = gc_blart.
      WHEN '2'.  "Line item
        CONDENSE gs_data-col2 NO-GAPS.
        CONDENSE gs_data-col3 NO-GAPS.
        CONDENSE gs_data-col4 NO-GAPS.
        CONDENSE gs_data-col5 NO-GAPS.
        CONDENSE gs_data-col6 NO-GAPS.
        CONDENSE gs_data-col7 NO-GAPS.
        CONDENSE gs_data-col8 NO-GAPS.
        CONDENSE gs_data-col9 NO-GAPS.
        CONDENSE gs_data-col10 NO-GAPS.
        CONDENSE gs_data-col11 NO-GAPS.
        CONDENSE gs_data-col12 NO-GAPS.
        CONDENSE gs_data-col13 NO-GAPS.
        CONDENSE gs_data-col14 NO-GAPS.
        CONDENSE gs_data-col15 NO-GAPS.
        CONDENSE gs_data-col16 NO-GAPS.
        CONDENSE gs_data-col17 NO-GAPS.
*        CONDENSE gs_data-col18 NO-GAPS.
        gs_acc_data-bschl  = gs_data-col2.
*        gs_acc_data-newbk  = gs_data-col3.
        gs_acc_data-abukrs = gs_data-col3.
        gs_acc_data-wrbtr  = gs_data-col4.
        "Date format is MMDDYYYY. Convert it to YYYYMMDD
        CONCATENATE gs_data-col5+4(4) gs_data-col5(2)
                    gs_data-col5+2(2) INTO lv_date.
        gs_acc_data-valut  = lv_date. "gs_data-col5.
        gs_acc_data-saknr  = gs_data-col6.
        TRANSLATE gs_acc_data-saknr TO UPPER CASE.
        gs_acc_data-kostl  = gs_data-col7.
        gs_acc_data-aufnr  = gs_data-col8.
        gs_acc_data-nplnr  = gs_data-col9. "--------------------------
        TRANSLATE gs_acc_data-nplnr TO UPPER CASE.
        gs_acc_data-vornr  = gs_data-col10.
        TRANSLATE gs_acc_data-vornr TO UPPER CASE.
        gs_acc_data-projk24  = gs_data-col11.
        TRANSLATE gs_acc_data-projk24 TO UPPER CASE.
        CONCATENATE gs_data-col15 gs_data-col16 gs_data-col17
                    INTO gs_acc_data-zuonr. " SEPARATED BY space.
        gs_acc_data-sgtxt  = gs_data-col18.
        "------------------
        CLEAR ls_t005.
        lv_land1 = gs_data-col13. "old spec 13  " new spec 14
        lv_rgion = gs_data-col14. "old spec 14  " new spec 15
        IF "lv_land1 = lc_us OR
           lv_land1 = lc_ca.
        ELSEIF lv_land1 = lc_us.
          lv_rgion = lc_us. "custom table has region US for US transactions
        ELSE.
          SELECT SINGLE * FROM t005 INTO ls_t005 WHERE land1 = lv_land1.
          lv_land1 = ls_t005-intca.
          lv_rgion = ls_t005-intca.
        ENDIF.
*******
        "Get Tax code
        CLEAR: gs_acc_data-mwskz,
               gs_acc_data-shkzg.
        SELECT SINGLE mwskz INTO gs_acc_data-mwskz FROM zfi_pcard_tax
                            WHERE "land1 = lv_land1
                                 rgion = lv_rgion.
        "AND saknr = gs_acc_data-saknr.
        "Get debit / credit indicator
        SELECT SINGLE shkzg INTO gs_acc_data-shkzg FROM tbsl
                            WHERE bschl = gs_acc_data-bschl.
******
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
******
        APPEND gs_acc_data TO gt_acc_data.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

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
             lv_docno TYPE i.
  DATA: lt_tax_info TYPE TABLE OF rtax1u15,
        ls_tax_info TYPE rtax1u15.
  CONSTANTS: lc_vst TYPE ktosl VALUE 'VST'.

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
         gt_bdc_data,
         gt_accounttax,
         gs_accounttax.

  "docno in this internal table is virtual document to separate each document.
  SORT gt_doc_data BY docno itmno bldat bukrs budat waers xblnr bktxt blart.
  DESCRIBE TABLE gt_doc_data LINES lv_records.
  LOOP AT gt_doc_data INTO gs_doc_data.
    IF  gs_doc_data-docno <> lv_docno.
*        gs_data-bukrs <> lv_bukrs OR
*        gs_data-blart <> lv_blart OR
*        gs_data-bldat <> lv_bldat OR
*        gs_data-budat <> lv_budat OR
**        gs_data-monat <> lv_monat OR
*        gs_data-waers <> lv_waers OR
*        gs_data-xblnr <> lv_xblnr OR
*        gs_data-bktxt <> lv_bktxt.
      IF lv_not_first IS NOT INITIAL.
        PERFORM bapi_validate_post.
      ENDIF.
      lv_not_first = 'X'.
      lv_docno = gs_doc_data-docno.
      lv_bukrs = gs_doc_data-bukrs.
      lv_blart = gs_doc_data-blart.
      lv_bldat = gs_doc_data-bldat.
      lv_budat = gs_doc_data-budat.
*      lv_monat = gs_data-monat.
      lv_waers = gs_doc_data-waers.
      lv_xblnr = gs_doc_data-xblnr.
      lv_bktxt = gs_doc_data-bktxt.
      CLEAR: lv_itemno_acc,
             gs_docheader,
             gs_cur_litems,
             gt_cur_litems,
             gt_gl_litems,
             gs_gl_litems,
             gt_accounttax,
             gs_accounttax.

    ENDIF.
*    CLEAR lv_shkzg.
*    SELECT SINGLE shkzg INTO lv_shkzg
*    FROM tbsl
*   WHERE bschl = gs_data-bschl.
*Load header data
    gs_docheader-obj_type = 'BKPFF'.
    gs_docheader-username = sy-uname.
    gs_docheader-header_txt = gs_doc_data-bktxt.
    gs_docheader-comp_code = gs_doc_data-bukrs.
    gs_docheader-doc_date = gs_doc_data-bldat.
    gs_docheader-pstng_date = gs_doc_data-budat.
*       gs_Docheader-trans_date = -valut.
    gs_docheader-doc_type = gs_doc_data-blart.
    gs_docheader-ref_doc_no = gs_doc_data-xblnr.
*    IF gs_data-bschl = '40' OR gs_data-bschl = '50'.
    lv_itemno_acc = lv_itemno_acc + 1.
*     Load Account GL line segment
    gs_gl_litems-itemno_acc    = lv_itemno_acc.
    gs_gl_litems-gl_account    = gs_doc_data-saknr.
    gs_gl_litems-comp_code     = gs_doc_data-abukrs.
    gs_gl_litems-pstng_date    = gs_doc_data-budat.
    gs_gl_litems-ref_key_1     = gs_doc_data-xblnr+3(12).
    gs_gl_litems-item_text     = gs_doc_data-sgtxt.
    gs_gl_litems-costcenter    = gs_doc_data-kostl.
    gs_gl_litems-orderid       = gs_doc_data-aufnr.
    gs_gl_litems-wbs_element   = gs_doc_data-projk24. "projk.
*      gs_gl_litems-plant         = gs_doc_data-werks.
*      TRANSLATE gs_gl_litems-plant to UPPER CASE.
*      gs_gl_litems-material      = gs_data-matnr.
    gs_gl_litems-network       = gs_doc_data-nplnr.
    gs_gl_litems-activity      = gs_doc_data-vornr.
    gs_gl_litems-tax_code      = gs_doc_data-mwskz.
    gs_gl_litems-alloc_nmbr    = gs_doc_data-zuonr.
    gs_gl_litems-value_date    = gs_doc_data-valut.
    APPEND gs_gl_litems TO gt_gl_litems.
*       Load GL currency line segment
    CLEAR: lt_tax_info,
           lv_base_amt.
    IF gs_doc_data-mwskz IS NOT INITIAL.
      IF gs_doc_data-shkzg = 'S'.
        lv_wrbtr1 = gs_doc_data-wrbtr.
      ELSE. "gs_doc_data-shkzg = 'H'.
        lv_wrbtr1 = gs_doc_data-wrbtr * -1.
      ENDIF.
      CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
        EXPORTING
          i_bukrs = gs_doc_data-bukrs
          i_mwskz = gs_doc_data-mwskz
          i_waers = gs_doc_data-waers
          i_wrbtr = lv_wrbtr1 "gs_doc_data-wrbtr
        TABLES
          t_mwdat = lt_tax_info.
*VST will always be the HST line for the Canadian tax procedure
      DELETE lt_tax_info WHERE ktosl <> lc_vst.
      "-----------------
      LOOP AT lt_tax_info INTO ls_tax_info.
        IF ls_tax_info-wmwst = 0 OR  "tax amount
           ls_tax_info-wmwst IS INITIAL OR
           ls_tax_info-hkont IS INITIAL.
          CONTINUE.
        ENDIF.
        lv_base_amt = lv_base_amt + ls_tax_info-kawrt.  "base amount
      ENDLOOP.
      IF lt_tax_info[] IS INITIAL OR   "no tax calculated data
         lv_base_amt IS INITIAL.
        IF gs_doc_data-shkzg = 'S'.
          lv_base_amt = gs_doc_data-wrbtr.
        ELSE. " gs_doc_data-shkzg = 'H'.
          lv_base_amt = gs_doc_data-wrbtr * -1.
        ENDIF.
      ENDIF.
    ELSE.  "no tax code assigned
      IF gs_doc_data-shkzg = 'S'.
        lv_base_amt = gs_doc_data-wrbtr.
      ELSE. " gs_doc_data-shkzg = 'H'.
        lv_base_amt = gs_doc_data-wrbtr * -1.
      ENDIF.
    ENDIF.
    CLEAR gs_cur_litems.
    gs_cur_litems-itemno_acc    = lv_itemno_acc.
    gs_cur_litems-currency      = gs_doc_data-waers.
*      CLEAR lv_shkzg.
*      SELECT SINGLE shkzg INTO lv_shkzg
*      FROM tbsl
*     WHERE bschl = gs_data-bschl.
*    IF gs_doc_data-shkzg = 'S'.
*      gs_cur_litems-amt_doccur = gs_doc_data-wrbtr.
*    ELSEIF gs_doc_data-shkzg = 'H'.
*      gs_cur_litems-amt_doccur = gs_doc_data-wrbtr * -1.
*    ELSE.
*      gs_cur_litems-amt_doccur = 0.
*    ENDIF.
    gs_cur_litems-amt_doccur = lv_base_amt.
    APPEND gs_cur_litems TO gt_cur_litems.
****Tax
    IF gs_doc_data-mwskz IS NOT INITIAL.
*        IF gs_doc_data-shkzg = 'S'.
*          lv_wrbtr = gs_doc_data-wrbtr.
*        ELSE. "gs_doc_data-shkzg = 'H'.
*          lv_Wrbtr = gs_doc_data-wrbtr * -1.
*        ENDIF.
*       CLEAR: lt_tax_info.
*         CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
*         EXPORTING
*         i_bukrs   = gs_doc_data-bukrs
*         i_mwskz = gs_doc_data-mwskz
*         i_waers  = gs_doc_data-waers
*         i_wrbtr   = lv_Wrbtr "gs_doc_data-wrbtr
*      TABLES
*         T_MWDAT  = lt_tax_info.
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
        "gs_doc_data-wrbtr. "<-----------It's not the base, but gross amount
        APPEND gs_cur_litems TO gt_cur_litems.
      ENDLOOP.
    ENDIF.
*******end of tax
*    ENDIF.
*****************
    IF gs_doc_data-bschl = '40' OR gs_doc_data-bschl = '50'.
    ELSE.
*      gv_err_found = 'X'.
*      w_chk_err_found = 'X'.
      CONCATENATE text-037 gs_doc_data-bschl INTO gs_id_msg-msg_text
                                             SEPARATED BY space.
*      gs_id_msg-msg_text = text-037.
      APPEND gs_id_msg TO gt_id_msg.
    ENDIF.
******Re-use gt_acc_data internal table to keep data
******of current loop.In case of Error, we need to generate BDC
    APPEND gs_doc_data TO gt_acc_data.
******
*    IF p_test IS INITIAL.
    lv_rows = lv_rows + 1.
    IF lv_records <= lv_rows.   "execute, because at last reocrds,
      PERFORM bapi_validate_post.   " it will exit the loop without
      EXIT.                    "executing bapi_validate_post at top.
    ENDIF.
*    ENDIF.
  ENDLOOP.

ENDFORM.                    " VALIDATE_DATA
*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_899_ROWS
*&---------------------------------------------------------------------*
*       maitain 899 rows per document and also create offset entry
*----------------------------------------------------------------------*
FORM maintain_899_rows .
  TYPES: BEGIN OF ty_cc_total,
           bukrs TYPE bseg-bukrs,
           wrbtr TYPE hwbas_bses, "bseg-wrbtr,
         END OF ty_cc_total.

  DATA: lv_counter TYPE i,
        lv_docno TYPE i,
        lv_itmno TYPE i,
        lv_tabix TYPE sy-tabix,
        lt_data TYPE TABLE OF ty_acc_data,
        ls_data TYPE ty_acc_data,
        lt_cc_total TYPE TABLE OF ty_cc_total WITH HEADER LINE, " WITH KEY bukrs.
        ls_acc_data TYPE ty_acc_data.

  SORT gt_acc_data BY bldat bukrs budat waers xblnr bktxt blart.
  lt_data[] = gt_acc_data[].
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING bldat bukrs budat
                                                    waers xblnr bktxt
                                                    blart.
  LOOP AT lt_data INTO ls_data.
    CLEAR: lv_counter,
           lt_cc_total.
    REFRESH: lt_cc_total.
    lv_docno = lv_docno + 1.
    lv_itmno = 1.
    READ TABLE gt_acc_data WITH KEY bldat = ls_data-bldat
                                    bukrs = ls_data-bukrs
                                    budat = ls_data-budat
                                    waers = ls_data-waers
                                    xblnr = ls_data-xblnr
                                    bktxt = ls_data-bktxt
                                    blart = ls_data-blart
                                    TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT gt_acc_data INTO gs_acc_data FROM lv_tabix.
      IF  gs_acc_data-bldat <> ls_data-bldat OR
          gs_acc_data-bukrs <> ls_data-bukrs OR
          gs_acc_data-budat <> ls_data-budat OR
          gs_acc_data-waers <> ls_data-waers OR
          gs_acc_data-xblnr <> ls_data-xblnr OR
          gs_acc_data-bktxt <> ls_data-bktxt OR
          gs_acc_data-blart <> ls_data-blart.
        EXIT.
      ENDIF.
      "first line item number is reserve for offset entry
      "which will be posted to header CC.
      lv_itmno = lv_itmno + 1.
      "collect amount data in table for offset entry
*      lt_cc_total-bukrs = gs_acc_data-abukrs.
      IF gs_acc_data-shkzg = 'H'. "Credit
        lt_cc_total-wrbtr = gs_acc_data-wrbtr * -1.
      ELSE. "debit
        lt_cc_total-wrbtr = gs_acc_data-wrbtr.
      ENDIF.
      COLLECT lt_cc_total.
      "----------
      IF gs_acc_data-shkzg = 'H' OR
         gs_acc_data-wrbtr < 0.
        gs_acc_data-wrbtr = abs( gs_acc_data-wrbtr ) .
      ENDIF.
      gs_acc_data-docno = lv_docno.
      gs_acc_data-itmno = lv_itmno.
      "keep data in local area because this data need for offset entry
      "after exiting this loop
      ls_acc_data = gs_acc_data.
      "-------------
      APPEND gs_acc_data TO gt_doc_data.
      lv_counter = lv_counter + 1.
      "Requirement is 899 rows.
      IF lv_counter > 899.
        "gs_acc_data-valut = .
        gs_acc_data-zuonr = space.
        gs_acc_data-mwskz = space.
        gs_acc_data-sgtxt = space.
        gs_acc_data-kostl = space.
        gs_acc_data-aufnr = space.
        gs_acc_data-nplnr = space.
        gs_acc_data-vornr = space.
        gs_acc_data-projk24 = space.
        gs_acc_data-projk = space.
        "Offset Entry in document
        gs_acc_data-itmno = 1.
        LOOP AT lt_cc_total.
          IF lt_cc_total-wrbtr < 0.
            "Debit entry
            gs_acc_data-shkzg = 'S'.
            gs_acc_data-bschl = '40'.
            gs_acc_data-abukrs = gs_acc_data-bukrs. "header CC
            gs_acc_data-wrbtr = abs( lt_cc_total-wrbtr ).
            gs_acc_data-saknr = gc_saknr.
            APPEND gs_acc_data TO gt_doc_data.
          ELSEIF lt_cc_total-wrbtr > 0.
            gs_acc_data-shkzg = 'H'.
            gs_acc_data-bschl = '50'.
            gs_acc_data-abukrs = gs_acc_data-bukrs.
            gs_acc_data-wrbtr = lt_cc_total-wrbtr.
            gs_acc_data-saknr = gc_saknr.
            APPEND gs_acc_data TO gt_doc_data.
          ENDIF.
        ENDLOOP.
        CLEAR lt_cc_total.
        REFRESH lt_cc_total.
        "----
        lv_docno = lv_docno + 1.
        lv_counter = 0.
        lv_itmno = 1.
      ENDIF.
    ENDLOOP.
    "Offset entry in document.
*    IF lv_offset_amt <> 0.
    "gs_acc_data-valut = .
    ls_acc_data-zuonr = space.
    ls_acc_data-mwskz = space.
    ls_acc_data-sgtxt = space.
    ls_acc_data-kostl = space.
    ls_acc_data-aufnr = space.
    ls_acc_data-nplnr = space.
    ls_acc_data-vornr = space.
    ls_acc_data-projk24 = space.
    ls_acc_data-projk = space.
    "Offset Entry in document
    ls_acc_data-itmno = 1.
    LOOP AT lt_cc_total.
      IF lt_cc_total-wrbtr < 0.
        "Debit entry
        ls_acc_data-shkzg = 'S'.
        ls_acc_data-bschl = '40'.
        ls_acc_data-abukrs = ls_acc_data-bukrs.
        ls_acc_data-wrbtr = abs( lt_cc_total-wrbtr ).
        ls_acc_data-saknr = gc_saknr.
        APPEND ls_acc_data TO gt_doc_data.
      ELSEIF lt_cc_total-wrbtr > 0.
        "Credit entry
        ls_acc_data-shkzg = 'H'.
        ls_acc_data-bschl = '50'.
        ls_acc_data-abukrs = ls_acc_data-bukrs.
        ls_acc_data-wrbtr = lt_cc_total-wrbtr.
        ls_acc_data-saknr = gc_saknr.
        APPEND ls_acc_data TO gt_doc_data.
      ENDIF.
    ENDLOOP.
    CLEAR lt_cc_total.
    REFRESH lt_cc_total.
  ENDLOOP.

ENDFORM.                    " MAINTAIN_899_ROWS
*&---------------------------------------------------------------------*
*&      Form  BAPI_VALIDATE_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_validate_post.
  DATA: lv_first TYPE xfeld,
           lv_format TYPE bapitga-textformat VALUE 'ASC',
           ls_skb1 TYPE skb1,
        lv_error TYPE xfeld.

  CLEAR gt_return.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader    = gs_docheader
*     CUSTOMERCPD       =
*     CONTRACTHEADER    =
    TABLES
      accountgl         = gt_gl_litems
*     accountreceivable =
*     ACCOUNTPAYABLE    =
      accounttax        = gt_accounttax
      currencyamount    = gt_cur_litems
*     criteria          =
*     valuefield        =
*     EXTENSION1        =
      return            = gt_return.
*       PAYMENTCARD             =
*       CONTRACTITEM            =
*       EXTENSION2              =
*       REALESTATE              =
*       ACCOUNTWT               =

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
*        MOVE gs_docheader TO gs_msg-msg_text.
        APPEND gs_id_msg TO gt_id_msg.
        lv_first = 'X'.
      ENDIF.
      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id                 = gs_return-id
        number             = gs_return-number
        language           = sy-langu
        textformat         = lv_format
*   LINKPATTERN        =
        message_v1         = gs_return-message_v1
        message_v2         = gs_return-message_v2
        message_v3         = gs_return-message_v3
        message_v4         = gs_return-message_v4
*           LANGUAGE_ISO       =
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
*      gv_err_found = 'X'.
*      w_chk_err_found = 'X'.
*      ENDIF.
      lv_error = 'X'.
    ENDIF.
  ENDLOOP.
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
*      ACCOUNTPAYABLE    =
     currencyamount    = gt_cur_litems
*      EXTENSION1        =
     return            = gt_return.
    LOOP AT gt_return INTO gs_return.
      CLEAR: gs_id_msg.
      IF gs_return-type = 'E' OR
         gs_return-type = 'A'.
        IF lv_first IS INITIAL.
          MOVE text-011 TO gs_id_msg-msg_text.
          APPEND gs_id_msg TO gt_id_msg.
          CONCATENATE gs_docheader-comp_code
                      gs_docheader-doc_type
                      gs_docheader-doc_date
                      gs_docheader-pstng_date
                      gs_docheader-fis_period
                      gs_docheader-ref_doc_no
                      gs_docheader-header_txt
                      INTO gs_id_msg-msg_text SEPARATED BY space.
*             MOVE gs_docheader TO gs_msg-msg_text.
          APPEND gs_id_msg TO gt_id_msg.
          lv_first = 'X'.
        ENDIF.
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
*In case of error, collect data to create BDC session.
  IF lv_error IS NOT INITIAL.
    APPEND LINES OF gt_acc_data TO gt_bdc_data.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*     EXPORTING
*       WAIT          =
*     IMPORTING
*       RETURN        =
              .

  ENDIF.
  CLEAR gt_acc_data.
ENDFORM.                    " BAPI_VALIDATE
*&---------------------------------------------------------------------*
*&      Form  CREATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_bdc .

  DATA:   trans_in_process(1),            "flag - trans in process
          open_session(1) VALUE 'X',      "flag - batch session open?
          lv_not_first TYPE xfeld,
          lv_post TYPE xfeld,
          lv_docno TYPE i.

  REFRESH bdcdata.
  PERFORM open_batch_session.
  SORT gt_bdc_data BY docno itmno bldat bukrs budat waers xblnr bktxt blart.
  LOOP AT gt_bdc_data INTO gs_acc_data.
*   Open batch session, if not open.
    IF lv_docno <> gs_acc_data-docno.
      IF lv_not_first IS NOT INITIAL.
        PERFORM close_transaction.
      ENDIF.
      lv_docno = gs_acc_data-docno.
      lv_not_first = 'X'.
      lv_docno = gs_acc_data-docno.
      "Document Header
      PERFORM start_new_transaction.
      "
    ELSE.
      PERFORM bdc_screen USING 'SAPMF05A' '0330'.
      "------
*      PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-XREF1'.
*      PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
*      PERFORM bdc_field  USING 'BSEG-XREF1' gs_acc_data-xblnr+3(12).
      "------
      PERFORM bdc_field  USING 'BDC_CURSOR' 'RF05A-NEWBK'.
      PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
      PERFORM bdc_field  USING 'BSEG-XREF1' gs_acc_data-xblnr+3(12).
      PERFORM bdc_field  USING 'RF05A-NEWBK' gs_acc_data-abukrs.
      PERFORM bdc_field  USING 'RF05A-NEWBS' gs_acc_data-bschl.
      PERFORM bdc_field  USING 'RF05A-NEWKO' gs_acc_data-saknr.
      "
    ENDIF.
    PERFORM bdc_screen USING 'SAPMF05A' '0300'.
    PERFORM start_next_lineitem.                  "Posting Key
  ENDLOOP.
*-----
  PERFORM close_transaction.
  PERFORM close_session.

ENDFORM.                    " CREATE_BDC
*&---------------------------------------------------------------------*
*&      Form  OPEN_BATCH_SESSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM open_batch_session .

*data: lv_group TYPE APQI-GROUPID.
*
** gv_bdc_group = gv_bdc_group + 1.
*
** CONCATENATE 'PCARD-' gv_bdc_group INTO lv_group.
* CONCATENATE 'PCARD' sy-datum INTO gv_group.

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
FORM close_transaction .

  PERFORM bdc_screen USING 'SAPMF05A' '0330'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-ABPER'.
  PERFORM bdc_field USING 'BDC_OKCODE' '=BU'.

  PERFORM insert_session.

ENDFORM.                    " CLOSE_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  INSERT_SESSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_session .

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = 'F-02'
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

  REFRESH bdcdata.                 "Refresh BDCData

ENDFORM.                    " INSERT_SESSION
*&---------------------------------------------------------------------*
*&      Form  START_NEW_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_new_transaction .
  DATA: lv_date(10) TYPE c.
  WRITE gs_acc_data-bldat TO lv_date(000010) MM/DD/YYYY.
* Header Data Screen (initial screen)
  PERFORM bdc_screen USING 'SAPMF05A' '100'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'RF05A-NEWKO'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'BKPF-BLDAT' lv_date. "gs_acc_data-bldat.
  PERFORM bdc_field  USING 'BKPF-BLART' gc_blart.
  PERFORM bdc_field  USING 'BKPF-BUKRS' gs_acc_data-bukrs.

  WRITE gs_acc_data-budat TO lv_date(000010) MM/DD/YYYY.
  PERFORM bdc_field  USING 'BKPF-BUDAT' lv_date. "gs_acc_data-budat.
*  PERFORM bdc_field  USING 'BKPF-MONAT' gs_acc_data-monat.
  PERFORM bdc_field  USING 'BKPF-WAERS' gs_acc_data-waers.
  PERFORM bdc_field  USING 'BKPF-XBLNR' gs_acc_data-xblnr.
  PERFORM bdc_field  USING 'BKPF-BKTXT' gs_acc_data-bktxt.
  "
  PERFORM bdc_field  USING 'RF05A-NEWBS' gs_acc_data-bschl.
  PERFORM bdc_field  USING 'RF05A-NEWKO' gs_acc_data-saknr.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.



ENDFORM.                    " START_NEW_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  BDC_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2284   text
*      -->P_2285   text
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
*      -->P_2289   text
*      -->P_XLTAB_BLDAT  text
*----------------------------------------------------------------------*
FORM bdc_field  USING fnam fval.

  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    " BDC_FIELD
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
    SKIP 1.
    WRITE : / 'BDC Sesscion', gv_group , ' is created'.
    SKIP 2.
  ENDIF.
ENDFORM.                    " CLOSE_SESSION
*&---------------------------------------------------------------------*
*&      Form  START_NEXT_LINEITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_next_lineitem .

  DATA: lv_wrbtr TYPE char13,
        lv_date(10) TYPE c.
*
*SELECT SINGLE fstag INTO skb1-fstag
*    FROM skb1
*   WHERE bukrs = w_bukrs
*     AND saknr = xltab-saknr.
*  IF skb1-fstag = 'STCF'.
*    PERFORM bdc_field  USING 'RF05V-NEWBW' xltab-newbw. "Transaction Type
*  ENDIF.
  WRITE gs_acc_data-wrbtr TO lv_wrbtr DECIMALS 2.
  SHIFT lv_wrbtr LEFT DELETING LEADING '0'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-SGTXT'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=ZK'.
  PERFORM bdc_field  USING 'BSEG-WRBTR' lv_wrbtr. "gs_acc_data-wrbtr.
  PERFORM bdc_field  USING 'BSEG-MWSKZ' gs_acc_data-mwskz.
  PERFORM bdc_field  USING 'BKPF-XMWST' 'X'.

  WRITE gs_acc_data-valut TO lv_date(000010) MM/DD/YYYY.
  PERFORM bdc_field  USING 'BSEG-VALUT' lv_date. "gs_acc_data-valut.
  PERFORM bdc_field  USING 'BSEG-ZUONR' gs_acc_data-zuonr.
  PERFORM bdc_field  USING 'BSEG-SGTXT' gs_acc_data-sgtxt.
*  IF gs_acc_data-bschl = '40'.
*  ENDIF.
  PERFORM bdc_field  USING 'DKACB-FMORE' 'X'.
  "
  PERFORM bdc_screen USING 'SAPLKACB' '0002'.
  IF gs_acc_data-saknr <> gc_saknr. "'0000251720'.
*  IF gs_acc_data-bschl = '40'.
    IF gs_acc_data-projk24 IS NOT INITIAL. "Project
      PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-PS_POSID'.
      PERFORM bdc_field  USING 'COBL-PS_POSID' gs_acc_data-projk24.
    ENDIF.
    IF gs_acc_data-aufnr IS NOT INITIAL. "Internal Order
      PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-AUFNR'.
      PERFORM bdc_field  USING 'COBL-AUFNR' gs_acc_data-aufnr.
    ENDIF.
    IF gs_acc_data-nplnr IS NOT INITIAL OR  "Network/Activity
       gs_acc_data-vornr IS NOT INITIAL.
      PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-VORNR'.
      PERFORM bdc_field  USING 'COBL-NPLNR' gs_acc_data-nplnr.
      PERFORM bdc_field  USING 'COBL-VORNR' gs_acc_data-vornr.
    ENDIF.
    IF gs_acc_data-kostl IS NOT INITIAL. "Cost Center
      PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-KOSTL'.
      PERFORM bdc_field  USING 'COBL-KOSTL' gs_acc_data-kostl.
    ENDIF.
*  ENDIF.
  ELSE.
    PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-MATNR'.
  ENDIF.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=ENTE'.
  "--------
  PERFORM bdc_screen USING 'SAPMF05A' '0330'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-XREF1'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'BSEG-XREF1' gs_acc_data-xblnr+3(12).

ENDFORM.                    " START_NEXT_LINEITEM
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
  LOOP AT git_file_list INTO lwa_file_list WHERE srvr_appl = gc_x.
    CONCATENATE lwa_file_list-filename_fp_in lwa_file_list-filename_fn_in
                                                           INTO lv_source.
    CONCATENATE lwa_file_list-filename_fp_arc lwa_file_list-filename_fn_arc
                                                           INTO lv_target.
    OPEN DATASET lv_source FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      ls_msg-msgrp = 'G'.
      ls_msg-msg_text = 'Unable to Open Source file during Archive process '.
      APPEND ls_msg TO gt_id_msg.
      CONCATENATE 'Source File:' lv_source INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      CONTINUE.
    ENDIF.
    OPEN DATASET lv_target FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      ls_msg-msgrp = 'G'.
      ls_msg-msg_text = 'Unable to Open Archive file during Archive process '.
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
*  -->  p1        text
*  <--  p2        text
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
*  -->  p1        text
*  <--  p2        text
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
*&      Form  F_ARCHIVE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_archive_file .

  DATA:    lwa_return                  TYPE bapireturn,
           lv_source_dir               TYPE btcxpgpar,
           lv_target_dir               TYPE btcxpgpar,
           lv_source_fname             TYPE btcxpgpar,
           lv_target_fname             TYPE btcxpgpar,
           lwa_file_list               TYPE ty_wa_file_list,
           ls_msg                      TYPE ty_msg.
  "-----------------------------
  ls_msg-msgrp = 'G'.
  ls_msg-msgid = 'ZFI01'.
  ls_msg-msgty = 'I'.
  ls_msg-msgno = '000'.
  MOVE text-028 TO ls_msg-msg_text.
  APPEND ls_msg TO gt_id_msg.
  "-----------------------------
  LOOP AT git_file_list INTO lwa_file_list WHERE srvr_appl = gc_x.
*      MOVE     p_sfile  TO lv_source_dir.
*      MOVE     p_ifile  TO lv_source_fname.
*
*      CONCATENATE p_ifile '.ARC' INTO lv_target_fname.
*      MOVE   p_arfile TO lv_target_dir.
    lv_source_dir   = lwa_file_list-filename_fp_in.
    lv_source_fname = lwa_file_list-filename_fn_in.
    lv_target_dir   = lwa_file_list-filename_fp_arc.
    lv_target_fname = lwa_file_list-filename_fn_arc.
*     Archive the data via a move file

    CALL FUNCTION 'ZFI_FILE_HANDLE'
      EXPORTING
        i_source_dir        = lv_source_dir
        i_target_dir        = lv_target_dir
        i_source_fname      = lv_source_fname
        i_target_fname      = lv_target_fname
        i_command           = 'M' "Move"
        i_date_time_stamp   = space
        i_rename_arc_to_new = space
      IMPORTING
        e_return            = lwa_return.

    IF   ( lwa_return-type CA 'aAeE' ).

      PERFORM  f_error_in_process    USING    gv_nbr_file   0     8
                                              space     space     0
                                              text-031  text-132
                                              lv_source_dir
                                              lv_source_fname.
      "return.
      EXIT.
    ELSE.
      CONCATENATE 'Source Path: ' lv_source_dir INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      CONCATENATE 'Source File: ' lv_source_fname INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      CONCATENATE 'Target Path: ' lv_target_dir INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      CONCATENATE 'Target File: ' lv_target_fname INTO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
      MOVE text-030 TO ls_msg-msg_text.
      APPEND ls_msg TO gt_id_msg.
    ENDIF.
    WAIT UP TO 1 SECONDS.
  ENDLOOP.
  "-----------------------------
  MOVE text-029 TO ls_msg-msg_text.
  APPEND ls_msg TO gt_id_msg.
  "-----------------------------

ENDFORM.                    " F_ARCHIVE_FILE
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
        PERFORM  f_error_in_process
                                  USING    0         0         lv_rc
                                           space     space     0
                                           text-024  lv_rc
                                           space     space.

        RETURN.
      ENDIF.

      DELETE lit_dir_list WHERE name IS INITIAL.

      IF lit_dir_list[] IS INITIAL.
        gv_flag_err_proc = 'X'.
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
          gv_flag_err_proc = 'X'.
          CLEAR    git_file_list[].
          PERFORM  f_error_in_process
                                   USING    0         0         lv_rc
                                            space     space     0
                                            text-026  lv_rc
                                            lwa_dir_list-name   space.
          RETURN.
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
    gv_flag_err_proc = 'X'.
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
*&      Form  MAINTAIN_899_ROWS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM maintain_899_rows_new .
  TYPES: BEGIN OF ty_cc_total,
             bukrs TYPE bseg-bukrs,
             wrbtr TYPE hwbas_bses, "bseg-wrbtr,
           END OF ty_cc_total.

  DATA: lv_shkzg TYPE bseg-shkzg,
        lv_counter TYPE i,
        lv_docno TYPE i,
        lv_itmno TYPE i,
        lv_tabix TYPE sy-tabix,
        lt_data TYPE TABLE OF ty_acc_data,
        ls_data TYPE ty_acc_data,
        lt_cc_total TYPE TABLE OF ty_cc_total WITH HEADER LINE, " WITH KEY bukrs.
        ls_acc_data TYPE ty_acc_data.

  SORT gt_acc_data BY bldat bukrs budat waers xblnr bktxt blart.
  lt_data[] = gt_acc_data[].
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING bldat bukrs budat
                                                    waers xblnr bktxt
                                                    blart.
  LOOP AT lt_data INTO ls_data.
    CLEAR: lv_counter,
           lt_cc_total.
    REFRESH: lt_cc_total.
    lv_docno = lv_docno + 1.
    lv_itmno = 1.
    READ TABLE gt_acc_data WITH KEY bldat = ls_data-bldat
                                    bukrs = ls_data-bukrs
                                    budat = ls_data-budat
                                    waers = ls_data-waers
                                    xblnr = ls_data-xblnr
                                    bktxt = ls_data-bktxt
                                    blart = ls_data-blart
                                    TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT gt_acc_data INTO gs_acc_data FROM lv_tabix.
      IF  gs_acc_data-bldat <> ls_data-bldat OR
          gs_acc_data-bukrs <> ls_data-bukrs OR
          gs_acc_data-budat <> ls_data-budat OR
          gs_acc_data-waers <> ls_data-waers OR
          gs_acc_data-xblnr <> ls_data-xblnr OR
          gs_acc_data-bktxt <> ls_data-bktxt OR
          gs_acc_data-blart <> ls_data-blart.
        EXIT.
      ENDIF.
      "first line item number is reserve for offset entry
      "which will be posted to header CC.
      lv_itmno = lv_itmno + 1.
      "----------
      lv_shkzg = gs_acc_data-shkzg.
      IF gs_acc_data-shkzg = 'H' OR
         gs_acc_data-wrbtr < 0.
        gs_acc_data-wrbtr = abs( gs_acc_data-wrbtr ) .
      ENDIF.
      gs_acc_data-docno = lv_docno.
      gs_acc_data-itmno = lv_itmno.
      "-------------
      APPEND gs_acc_data TO gt_doc_data.
      "Offset Entry to Liability account
      IF lv_shkzg = 'H'.
        gs_acc_data-shkzg = 'S'.
        gs_acc_data-bschl = '40'.
      ELSE.
        gs_acc_data-shkzg = 'H'.
        gs_acc_data-bschl = '50'.
      ENDIF.
      "Offset Entry in document
      gs_acc_data-itmno = 1.
*      gs_acc_data-zuonr = space.
      gs_acc_data-mwskz = space.
*      gs_acc_data-sgtxt = space.
      gs_acc_data-kostl = space.
      gs_acc_data-aufnr = space.
      gs_acc_data-nplnr = space.
      gs_acc_data-vornr = space.
      gs_acc_data-projk24 = space.
      gs_acc_data-projk = space.
      gs_acc_data-abukrs = gs_acc_data-bukrs. "header CC
      gs_acc_data-wrbtr = abs( gs_acc_data-wrbtr ).
      gs_acc_data-saknr = gc_saknr.
      "----------------
      APPEND gs_acc_data TO gt_doc_data.
      lv_counter = lv_counter + 2.
      "Requirement is 899 rows.
      IF lv_counter > 899.
        lv_docno = lv_docno + 1.
        lv_counter = 0.
        lv_itmno = 1.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " MAINTAIN_899_ROWS_NEW
