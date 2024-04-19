REPORT  zffii041 MESSAGE-ID zs LINE-SIZE 132 LINE-COUNT 65
                 NO STANDARD PAGE HEADING.
************************************************************************
*  Client:    Spectra Energy.                                          *
*  Author:    Mohammad T. Khan.                                        *
*  Date:      August, 2008.                                            *
*  Track #:   TR562.                                                   *
*                                                                      *
*  Description:                                                        *
*     - The purpose of this program is to create a BDC session for     *
*       posting Journal Entries from the EXCEL sheet using Trans. FB01.*
*  NOTE: The BDC session name must be in row 3 & col 3 of spread sheet.*
************************************************************************
* ---------------------- CHANGE LOG -----------------------------------*
*  Date   TR #     By        Description                               *
*-------- -------- -------   ----------------------------------------- *
*02/13/15 SDP42717 S AHMAD   addition to BDC and New BAPI call for
*                            Validation.
*01/07/15 SDP79989 M Khan    Fill out BDC fields (Branch and Sector).  *
*
*11/28/13 SDP59431 M Khan    Don't add zeros in cost center when it    *
*                            starts with letter "U".
*09/26/13 SDP53936 GYMANA    Rewrote program check incoming spreadsheet*
*                            file for errors via posting check BAPI    *
*                            Then create BDC session using transaction *
*                            FBV1.                                     *
*                                                                      *
*25/01/13  SDP27706 M Khan Add field Sales District in Exel and BDC    *
*          SDP28402 M Khan Add status RVST 2 GL_LINE_ITEM_PROFITIBILITY*
*                                                                      *
*10/07/12  995  M Khan  Change C: drive to H: drive with               *
*                           directory, file selection using F4         *
*                                                                      *
*2012/12/06 928  gymana  Add Value date (BSEG-VALUT)                   *
*                                                                      *
*09/11/11  928  M Khan  Add new Treasury field status groups and change*
*                       excel max. row limit I_END_ROW = 999 to 99999  *
*                                                                      *
*03/04/09  580  M Khan  Screen changes for upgrade to ECC 6.0          *
*                                                                      *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 29-DEC-2020  KMB         D30K930791 CHG0201827 Assignment validation *
*&---------------------------------------------------------------------*

TABLES: skb1,                             "Cost Center Master Data
        tbsl.                             "Posting Key

* Batch input data
DATA: BEGIN OF bdcdata OCCURS 500.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

* Working Data
DATA:   tbldat            TYPE d,
        tbudat            TYPE d,
        w_bdc_session(12) TYPE c,
        scrn300_flg(1)    TYPE c,
        scrn301_flg(1)    TYPE c,
        first_doc(1)      TYPE c VALUE 'X',
        ln_cntr           TYPE i VALUE 99,
        w_chk_err_found   TYPE c,
        w_doc_err_found   TYPE c,
        w_curr-itemno_acc TYPE bapiaccr08-itemno_acc VALUE '1',
        w_line-itemno_acc TYPE bapiacgl08-itemno_acc VALUE '1',
        w_bukrs           LIKE bkpf-bukrs,
        w_budat           LIKE bkpf-budat,
        w_xblnr           LIKE bkpf-xblnr,
        w_waers           LIKE bkpf-waers.

TYPES: BEGIN OF ty_msg,
        msgid TYPE symsgid,
        msgty TYPE symsgty,
        msgno TYPE symsgno,
        msg_text(250),
       END OF ty_msg.

DATA: gt_msg TYPE TABLE OF ty_msg,
      gs_msg LIKE LINE OF gt_msg.


DATA: lv_rfcdest TYPE rfcdest,
      lt_return TYPE TABLE OF bapiret2,
      ls_return TYPE bapiret2,
      lt_bapi_curr TYPE TABLE OF bapiaccr08,
      ls_bapi_curr TYPE bapiaccr08,
      lt_bapi_line TYPE TABLE OF bapiacgl08,
      ls_bapi_line TYPE bapiacgl08,
      ls_bapi_header TYPE bapiache08.

*Formated Input record
DATA:
  BEGIN OF xltab OCCURS 1,
    bukrs    LIKE bkpf-bukrs,           "Company Code
    blart    LIKE bkpf-blart,           "Document Type
    bldat    LIKE bkpf-bldat,           "Document date
    budat    LIKE bkpf-budat,           "Posting  date
    monat    LIKE bkpf-monat,           "PERIOD
    waers    LIKE bkpf-waers,           "Currency Code
    kursf(9) TYPE c,           "Exchange Rate
    xblnr    LIKE bkpf-xblnr,           "Reference
    bktxt    LIKE bkpf-bktxt,           "Document Header Text
    bschl    LIKE bseg-bschl,           "Posting Key
    saknr    LIKE bseg-saknr,           "GL Account
    newbw    LIKE bseg-anbwa,           "Transaction Type
    mwskz    LIKE bseg-mwskz,           "Tax Code
    newbk    LIKE bbseg-newbk,          "Company code for next line item
*    WRBTR(15) TYPE N,                   "Amount               "TR580
    wrbtr(15) TYPE c,                   "$                     "TR580
    menge(15) TYPE c,                   "Quantity
    meins    LIKE bseg-meins,           "UOM
    sgtxt    LIKE bseg-sgtxt,           "Line Item Description
    kostl    LIKE bseg-kostl,           "Cost Center
    projn    LIKE bseg-projn,           "Project
    aufnr    LIKE bseg-aufnr,           "Order #
    matnr    LIKE bseg-matnr,           "Material #
    pernr    LIKE bseg-pernr,           "Personell #
    werks    LIKE bseg-werks,           "Plant Code
    field1   LIKE rkeak-field,          "Customer
    field2   LIKE rkeak-field,          "Sales Org
    field3   LIKE rkeak-field,          "Branch
    field4   LIKE rkeak-field,          "Sector
    field5   LIKE rkeak-field,          "Segment
    field6   LIKE rkeak-field,          "Rate Class
    field7   LIKE rkeak-field,          "Service Class
    field15  LIKE rkeak-field,          "S.A. Number
    valut    LIKE bseg-valut,           "Value Date              "TR928
    wwsld    LIKE ce01100-wwsld,        "Sales District       "SDP27706
    wwltl    LIKE ce01100-wwltl,
    wwrsn    LIKE ce11100-wwrsn,
    VTWEG    LIKE CE01100-VTWEG,
*Start of changes by AHMADT for CHG0201827
    zuonr    LIKE bseg-zuonr,
*End of changes by AHMADT for CHG0201827
  END OF xltab.

TYPES: BEGIN OF kcde_intern_struc.
        INCLUDE STRUCTURE  kcde_cells.
TYPES: END OF kcde_intern_struc.

DATA: exceltab TYPE kcde_intern_struc OCCURS 0 WITH HEADER LINE.
DATA: gs_docheader TYPE bapiache09, "bapiache08,
      gt_gl_litems TYPE TABLE OF bapiacgl09, "bapiacgl08,
      gs_gl_litems TYPE bapiacgl09, "bapiacgl08,
      gt_cur_litems TYPE TABLE OF bapiaccr09, "bapiaccr08,
      gs_cur_litems TYPE bapiaccr09, "bapiaccr08,
      gt_cus_litems TYPE TABLE OF bapiacar09,
      gs_cus_litems TYPE bapiacar09,
      gs_criteria TYPE  bapiackec9,
      gt_criteria	TYPE TABLE OF	bapiackec9,
      gs_valuefield TYPE  bapiackev9,
      gt_valuefield	TYPE TABLE OF	bapiackev9,
      gt_return TYPE TABLE OF bapiret2,
      gs_return TYPE bapiret2,
      gs_data LIKE xltab,
      gt_data LIKE TABLE OF xltab,
      gs_gl LIKE xltab,
      gt_gl LIKE TABLE OF xltab,
      gt_extension TYPE TABLE OF BAPIPAREX,
      gs_extension type BAPIPAREX.

*======================================================================*
*              SELECTION SCREEN                                        *
*======================================================================*
*
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME.
PARAMETERS: p_filein LIKE rlgrap-filename OBLIGATORY DEFAULT
                     'H:\saptemp\generic upload.xlsx'. "TR995
*                     'C:\saptemp\generic upload.xlsx'. "TR995

SELECTION-SCREEN SKIP.
PARAMETERS:      p_crtbdc(1)   TYPE c DEFAULT 'N'.

SELECTION-SCREEN END OF BLOCK box.


*======================================================================*
*************************************************************************
*Start Of TR995 changes
*AT SELECTION-SCREEN.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filein.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_filein = wit_filename_tab.
    ELSE.
      CLEAR p_filein.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_filein.
  PERFORM check_file_path.
*End of TR995 changes
*======================================================================*
*              Main Processing Block                                   *
*======================================================================*

START-OF-SELECTION.

  PERFORM generate_report_header.

  PERFORM upload_exce_to_internal_tab.
  IF xltab[] IS INITIAL.
    STOP.
  ELSE.
    PERFORM data_checks.
*    PERFORM pre_check_validation.
    PERFORM move_data_for_validation.
    PERFORM data_validation.
    IF p_crtbdc = 'Y' AND w_chk_err_found <> 'X'.
      PERFORM create_batch_input.
    ENDIF.

    IF gt_msg[] IS NOT INITIAL.
      WRITE: / '************Error Messages************'.
    ENDIF.
    LOOP AT gt_msg INTO gs_msg.

      IF gs_msg-msg_text(5) = 'BKPFF'.
        SKIP.
        WRITE / 'Document:'.
      ENDIF.

      IF ln_cntr >= 55.
        PERFORM generate_report_header.
      ENDIF.

      WRITE: / gs_msg-msg_text.
      ADD +1 TO ln_cntr.

      IF gs_msg-msg_text(5) = 'BKPFF'.
        SKIP.
      ENDIF.

    ENDLOOP.
    SKIP .
    WRITE: / 'Process is completed...........'.
  ENDIF.

*======================================================================*
*              Upload EXCEL Data                                       *
*======================================================================*
FORM upload_exce_to_internal_tab.
  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
    EXPORTING
      filename                = p_filein
      i_begin_col             = 1
      i_begin_row             = 1
* Start of changes by AHMADT for CHG0201827
*      i_end_col               = 37
       i_end_col              = 38
* End of changes by AHAMDT for CHG0201827
      i_end_row               = 99999
    TABLES
      intern                  = exceltab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_FOR_INTERACTION'
      EXPORTING
        headline = '!! ERROR !!'
        text1    = 'Unsuccessful EXCEL Upload '
        text2    = 'Please check the file path/name and try again'
        text3    = ' '
        text4    = 'Press OK Button to Continue'
        button_1 = 'OK'.
    STOP.
  ENDIF.

  LOOP AT exceltab.
    IF exceltab-row = 3 AND exceltab-col = 3.
      MOVE exceltab-value TO w_bdc_session.
    ENDIF.
    IF exceltab-row < 5.
      CONTINUE.
    ENDIF.

    CASE exceltab-col.
      WHEN 1.  MOVE exceltab-value TO xltab-bukrs.
      WHEN 2.  MOVE exceltab-value TO xltab-blart.
      WHEN 3.  MOVE exceltab-value TO xltab-bldat.
      WHEN 4.  MOVE exceltab-value TO xltab-budat.
      WHEN 5.  MOVE exceltab-value TO xltab-monat.
      WHEN 6.  MOVE exceltab-value TO xltab-waers.
      WHEN 7.  WRITE exceltab-value TO xltab-kursf LEFT-JUSTIFIED.
      WHEN 8.  MOVE exceltab-value TO xltab-xblnr.
      WHEN 9.  MOVE exceltab-value TO xltab-bktxt.
      WHEN 10.  MOVE exceltab-value TO xltab-bschl.
      WHEN 11.
        "    CONCATENATE '0000' exceltab-value INTO xltab-saknr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = exceltab-value
          IMPORTING
            output = xltab-saknr.

      WHEN 12. MOVE exceltab-value TO xltab-newbw.
      WHEN 13. MOVE exceltab-value TO xltab-mwskz.
      WHEN 14. MOVE exceltab-value TO xltab-newbk.
      WHEN 15. WRITE exceltab-value TO xltab-wrbtr LEFT-JUSTIFIED.
      WHEN 16. MOVE exceltab-value TO xltab-menge.
      WHEN 17. MOVE exceltab-value TO xltab-meins.
      WHEN 18. MOVE exceltab-value TO xltab-sgtxt.
      WHEN 19. MOVE exceltab-value TO xltab-kostl.
      WHEN 20. MOVE exceltab-value TO xltab-projn.
      WHEN 21. MOVE exceltab-value TO xltab-aufnr.
      WHEN 22. MOVE exceltab-value TO xltab-matnr.
      WHEN 23. MOVE exceltab-value TO xltab-pernr.
      WHEN 24. MOVE exceltab-value TO xltab-werks.
               TRANSLATE xltab-werks to UPPER CASE.
      WHEN 25. MOVE exceltab-value TO xltab-field1.
      WHEN 26. MOVE exceltab-value TO xltab-field2.
      WHEN 27. MOVE exceltab-value TO xltab-field3.
      WHEN 28. MOVE exceltab-value TO xltab-field4.
      WHEN 29. MOVE exceltab-value TO xltab-field5.
      WHEN 30. MOVE exceltab-value TO xltab-field6.
      WHEN 31. MOVE exceltab-value TO xltab-field7.
      WHEN 32. MOVE exceltab-value TO xltab-field15.
      WHEN 33. MOVE exceltab-value TO xltab-valut.           "TR928
      WHEN 34. MOVE exceltab-value TO xltab-wwsld.          "SDP27706
      WHEN 35. MOVe exceltab-value to xltab-wwltl.
      WHEN 36. MOVe exceltab-value to xltab-wwrsn.
      WHEN 37. MOVe exceltab-value to xltab-VTWEG.
*Start of changes by AHMADT for CHG0201827
      WHEN 38. MOVE exceltab-value TO xltab-zuonr.
*End of changes by AHMADT for CHG0201827
      WHEN OTHERS.
    ENDCASE.
    AT END OF row.
      APPEND xltab.
      CLEAR  xltab.
    ENDAT.
  ENDLOOP.
ENDFORM.                    "UPLOAD_EXCE_TO_INTERNAL_TAB
*======================================================================*
*DATA CHECK: Total of Debit & Credit amount in sheet should be equal   *
*======================================================================*
FORM data_checks.

  DATA: amt_debit LIKE bseg-wrbtr,
        amt_credit LIKE bseg-wrbtr.

  LOOP AT xltab.
    CLEAR tbsl-shkzg.
    SELECT SINGLE shkzg INTO tbsl-shkzg
      FROM tbsl
     WHERE bschl = xltab-bschl.

    IF tbsl-shkzg = 'H'.
      ADD xltab-wrbtr TO amt_credit.
    ELSE.
      ADD xltab-wrbtr TO amt_debit.
    ENDIF.
  ENDLOOP.
* Total Amount for column (WRBTR) of excel sheet must be zero.
  IF amt_debit <> amt_credit.
    WRITE: /1 text-101.
    STOP.
  ENDIF.
ENDFORM.                    "DATA_CHECKS

*======================================================================*
*       Form  PRE_CHECK_VALIDATION
*======================================================================*

FORM pre_check_validation.

  CLEAR: w_chk_err_found.

  LOOP AT xltab.

*   Document header record....
    IF first_doc = ' '.
      IF xltab-bukrs <> space.
        PERFORM call_posting_check_bapi.
        MOVE 1 TO w_curr-itemno_acc.
        MOVE 1 TO w_line-itemno_acc.
*         CLEAR TABLES.
        CLEAR: ls_bapi_header, ls_bapi_line, ls_bapi_curr.
        REFRESH: lt_bapi_line, lt_bapi_curr.
      ENDIF.
    ENDIF.

    IF xltab-bukrs <> space.
*   Save common header fields
      MOVE xltab-waers TO w_waers.
      MOVE xltab-bukrs TO w_bukrs.
      MOVE xltab-budat TO w_budat.
      MOVE xltab-xblnr TO w_xblnr.
      MOVE xltab-waers TO w_waers.
*   Load document header segment
      ls_bapi_header-obj_type = 'BKPFF'.
      ls_bapi_header-username = sy-uname.
      ls_bapi_header-header_txt = xltab-bktxt.
      ls_bapi_header-comp_code = xltab-bukrs.
      ls_bapi_header-doc_date = xltab-bldat.
      ls_bapi_header-pstng_date = xltab-budat.
      ls_bapi_header-trans_date = xltab-valut.
      ls_bapi_header-doc_type = xltab-blart.
      ls_bapi_header-ref_doc_no = xltab-xblnr.
      first_doc = ' '.
    ENDIF.
*   Load currency line segment
    ls_bapi_curr-itemno_acc    = w_curr-itemno_acc.
    ls_bapi_curr-currency      = w_waers.
*   CHECK POSTING KEY TO DETERMINE DB/CR AMOUNT
    IF xltab-bschl = '40' OR xltab-bschl = '02'.
      ls_bapi_curr-amt_doccur = xltab-wrbtr.
    ELSEIF xltab-bschl = '50' OR xltab-bschl = '11'.
      ls_bapi_curr-amt_doccur = xltab-wrbtr * -1.
    ENDIF.
    APPEND ls_bapi_curr TO lt_bapi_curr.
    CLEAR ls_bapi_curr.
    ADD 1 TO w_curr-itemno_acc.
* Load Account GL line segment
    ls_bapi_line-itemno_acc    = w_line-itemno_acc.
    ls_bapi_line-gl_account    = xltab-saknr.
    ls_bapi_line-comp_code     = w_bukrs.
    ls_bapi_line-pstng_date    = w_budat.
    ls_bapi_line-ref_key_1     = w_xblnr.
    ls_bapi_line-item_text     = xltab-sgtxt.
*    IF xltab-kostl <> ' '.                            "SDP59431
    IF xltab-kostl <> ' ' AND xltab-kostl+0(1) <> 'U'.      "SDP59431
      ls_bapi_line-costcenter    = xltab-kostl.
      SHIFT ls_bapi_line-costcenter RIGHT
            DELETING TRAILING space.
      TRANSLATE ls_bapi_line-costcenter USING ' 0'.
    ENDIF.
    IF xltab-aufnr <> ' '.
      ls_bapi_line-orderid       = xltab-aufnr.
      SHIFT ls_bapi_line-orderid RIGHT
            DELETING TRAILING space.
      TRANSLATE ls_bapi_line-orderid USING ' 0'.
    ENDIF.
    ls_bapi_line-wbs_element   = xltab-projn.
    APPEND ls_bapi_line TO lt_bapi_line.
    CLEAR ls_bapi_line.
    ADD 1 TO w_line-itemno_acc.

  ENDLOOP.

  PERFORM call_posting_check_bapi.

ENDFORM.                    " PRE_CHECK_VALIDATION
*======================================================================*
*       Form  CALL_POSTING_CHECK_BAPI
*======================================================================*

FORM call_posting_check_bapi.

  CALL FUNCTION 'BAPI_ACC_GL_POSTING_CHECK'
    EXPORTING
      documentheader = ls_bapi_header "gv_BAPI_header
    TABLES
      accountgl      = lt_bapi_line "gt_BAPI_line
      currencyamount = lt_bapi_curr "gt_BAPI_curr
      return         = lt_return
*     EXTENSION1     =
    .
  CLEAR: gs_msg, w_doc_err_found.
  MOVE ls_bapi_header TO gs_msg-msg_text.
  APPEND gs_msg TO gt_msg.

  LOOP AT lt_return INTO ls_return.
    CLEAR: gs_msg.
    IF ls_return-type = 'E' OR
       ls_return-type = 'A'.
      IF ls_return-number <> '808' AND
         ls_return-number <> '609' AND
         ls_return-number <> '235'.
        gs_msg-msgty = ls_return-type.
        gs_msg-msgid = ls_return-id.
        gs_msg-msgno = ls_return-number.
        CONCATENATE ls_return-number '-'
                    ls_return-message
               INTO gs_msg-msg_text
           SEPARATED BY space.

*        GS_MSG-MSG_TEXT = LS_RETURN-MESSAGE.
        APPEND gs_msg TO gt_msg.
        w_chk_err_found = 'X'.
        w_doc_err_found = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF w_doc_err_found <> 'X'.
    gs_msg-msg_text = 'No Errors Found.'.
    APPEND gs_msg TO gt_msg.
  ENDIF.


ENDFORM.                    "CALL_POSTING_CHECK_BAPI

*======================================================================*
*                FORM CREATE_BATCH_INPUT                               *
* - This is the main routine of the program which reads each record    *
*   from the input file and creates the batch input data.              *
*======================================================================*
FORM create_batch_input.

  DATA:   trans_in_process(1),            "flag - trans in process
          open_session(1) VALUE 'X'.      "flag - batch session open?

  REFRESH bdcdata.
  CLEAR xltab.

  LOOP AT xltab.

*   Open batch session, if not open.
    IF open_session = 'X'.
      PERFORM open_batch_session.
      MOVE ' ' TO open_session.
    ENDIF.

*   Document header record....
    IF xltab-bukrs <> space.
      MOVE xltab-bukrs TO w_bukrs.
      IF trans_in_process = 'X'.
        PERFORM close_transaction.
        CLEAR trans_in_process.
      ENDIF.
      PERFORM start_new_transaction.
      trans_in_process = 'X'.
      scrn300_flg = ' '.
    ENDIF.

    IF scrn300_flg = 'X'.
      PERFORM bdc_screen USING 'SAPLF040' '0300'.
    ENDIF.
    PERFORM start_next_lineitem.                  "Posting Key
    IF scrn301_flg = 'X'.
      PERFORM bdc_screen USING 'SAPLF040' '0301'.
    ENDIF.
    IF scrn300_flg = 'X'.
      PERFORM bdc_screen USING 'SAPLKACB'   '0002'.
      PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.
    ENDIF.
*
**Get Field Status Group Using G/L Account Number.
    SELECT SINGLE fstag INTO skb1-fstag
      FROM skb1
     WHERE bukrs = w_bukrs
       AND saknr = xltab-saknr.
    IF sy-subrc = 0.
      scrn301_flg = space.
      CASE skb1-fstag.
*               WHEN 'BREV' OR 'GASS' OR 'REVN' OR 'RVOT'.
        WHEN 'BREV' OR 'GASS' OR 'REVN' OR 'RVOT' OR 'RVST'.
          PERFORM gl_line_item_profitibility.
          MOVE 'X' TO scrn300_flg.
        WHEN 'EXMM' OR 'PROJ' OR 'STCF'.
          PERFORM gl_line_item_non_profitibility.
          MOVE 'X' TO scrn300_flg.
        WHEN 'PAYR'.
          PERFORM gl_line_item_non_profit_payr.
          MOVE 'X' TO scrn300_flg.
        WHEN 'BSBB' OR 'BSBA' OR 'BSHT'.
          PERFORM gl_line_item_bal_sheet_valdate.     "TR928
          MOVE 'X' TO scrn300_flg.
        WHEN OTHERS.
          PERFORM gl_line_item_balance_sheet.         "TR928
          MOVE 'X' TO scrn300_flg.
      ENDCASE.
    ELSE.
      IF scrn301_flg IS INITIAL.
        PERFORM bdc_screen USING 'SAPLF040' '0301'.
      ENDIF.
      PERFORM line_item_301.
      scrn301_flg = 'X'.
      scrn300_flg = space.
    ENDIF.

  ENDLOOP.
  IF scrn301_flg IS NOT INITIAL.
    PERFORM bdc_screen USING 'SAPLF040' '0301'.
  ELSE.
    PERFORM bdc_screen USING 'SAPLF040' '0300'.
  ENDIF.
  PERFORM close_transaction.
  PERFORM close_session.
ENDFORM.                    "CREATE_BATCH_INPUT

*======================================================================*
*                 FORM START_NEW_TRANSACTION                           *
* - This routine provides the BDC mapping for the initial screen in    *
* the transaction.                                                     *
*======================================================================*
FORM start_new_transaction.

* Header Data Screen (initial screen)
  PERFORM bdc_screen USING 'SAPLF040' '100'.
  PERFORM bdc_field  USING 'BKPF-BLDAT' xltab-bldat.
  PERFORM bdc_field  USING 'BKPF-BLART' xltab-blart.
  PERFORM bdc_field  USING 'BKPF-BUKRS' xltab-bukrs.
  PERFORM bdc_field  USING 'BKPF-BUDAT' xltab-budat.
  PERFORM bdc_field  USING 'BKPF-MONAT' xltab-monat.
  PERFORM bdc_field  USING 'BKPF-WAERS' xltab-waers.
  PERFORM bdc_field  USING 'BKPF-XBLNR' xltab-xblnr.
  PERFORM bdc_field  USING 'BKPF-BKTXT' xltab-bktxt.
  PERFORM bdc_field  USING 'BKPF-KURSF' xltab-kursf.

ENDFORM.                    "START_NEW_TRANSACTION

*======================================================================*
*                 FORM CLOSE_TRANSACTION                               *
* - This routine closes the current transaction.                       *
*======================================================================*
FORM close_transaction.

*  PERFORM bdc_screen USING 'SAPLF040' '300'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'PBBP'.
  PERFORM bdc_screen USING 'SAPLKACB'  '0002'.
  PERFORM bdc_field USING 'BDC_OKCODE' 'ENTE'.

*  PERFORM BDC_SCREEN USING 'SAPLF040' '0700'.
*  PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'BSEG-XBLNR'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '=PBBP'.
*  PERFORM BDC_FIELD  USING 'BKPF-XBLNR' XLTAB-XBLNR.
*  PERFORM BDC_FIELD  USING 'BKPF-SGTXT' XLTAB-SGTXT.

  PERFORM insert_session.

ENDFORM.                    "CLOSE_TRANSACTION

*======================================================================*
*                   FORM START_NEXT_LINEITEM                           *
* - This routine enters the posting key and account for the next line  *
* item.  This was put in a seperate routine for clarity as only these  *
* 2-3 fields appear on the previous screen.  The rest of the line item *
* information appears on a subsequent screen in the transaction.       *
*======================================================================*
FORM start_next_lineitem.

  SELECT SINGLE fstag INTO skb1-fstag
    FROM skb1
   WHERE bukrs = w_bukrs
     AND saknr = xltab-saknr.
  IF skb1-fstag = 'STCF'.
    PERFORM bdc_field  USING 'RF05V-NEWBW' xltab-newbw. "Transaction Type
  ENDIF.
  PERFORM bdc_field  USING 'RF05V-NEWBS' xltab-bschl.
  PERFORM bdc_field  USING 'RF05V-NEWKO' xltab-saknr.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.

ENDFORM.                    "START_NEXT_LINEITEM

*======================================================================*
*This is the BDC mapping for screen when entering a G/L line item for  *
*Balance Sheet Account.                                                *
*======================================================================*
FORM gl_line_item_balance_sheet.

  PERFORM bdc_screen USING 'SAPLF040' '0300'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-WRBTR'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'PBBP'.
  PERFORM bdc_field  USING 'BSEG-WRBTR' xltab-wrbtr.
  PERFORM bdc_field  USING 'BSEG-VALUT' xltab-valut.
*Start of changes by KMB for CHG0201827
  PERFORM bdc_field  USING 'BSEG-ZUONR' xltab-zuonr.
**End of changes by KMB for CHG0201827
  PERFORM bdc_field  USING 'BSEG-SGTXT' xltab-sgtxt.
  PERFORM bdc_field  USING 'DKACB-FMORE' ' '.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWBS' XLTAB-BSCHL.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWKO' XLTAB-SAKNR.
****
*  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
*  PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'COBL-ANLN1'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.

*OLD CODE
*  PERFORM BDC_SCREEN USING 'SAPLF040' '0300'.
*  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  XLTAB-WRBTR.
*  PERFORM BDC_FIELD  USING 'BSEG-MENGE'  XLTAB-MENGE.
*  PERFORM BDC_FIELD  USING 'BSEG-MEINS'  XLTAB-MEINS.
*  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  XLTAB-SGTXT.
*  PERFORM BDC_FIELD  USING 'DKACB-FMORE' ' '.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWBK' XLTAB-NEWBK.
*  PERFORM START_NEXT_LINEITEM.
*  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.   "or use ENTE

ENDFORM.                    "GL_LINE_ITEM_BALANCE_SHEET

*======================================================================*
*This is the BDC mapping for screen when entering a G/L line item for  *
*Balance Sheet Account. This form will include the value date when the *
*field status group equals BSBB                                        *
*======================================================================*
FORM gl_line_item_bal_sheet_valdate.                              "TR928

  PERFORM bdc_screen USING 'SAPLF040' '0300'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-SGTXT'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'PBBP'.
  PERFORM bdc_field  USING 'BSEG-WRBTR' xltab-wrbtr.
  PERFORM bdc_field  USING 'BSEG-VALUT' xltab-valut.
*Start of changes by KMB for CHG0201827
  PERFORM bdc_field  USING 'BSEG-ZUONR' xltab-zuonr.
*End of changes by KMB for CHG0201827
  PERFORM bdc_field  USING 'BSEG-SGTXT' xltab-sgtxt.
  PERFORM bdc_field  USING 'DKACB-FMORE' ' '.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWBS' XLTAB-BSCHL.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWKO' XLTAB-SAKNR.
****
*  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
*  PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'COBL-ANLN1'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.

* OLD CODE
*                                                                 "TR928
*  PERFORM BDC_SCREEN USING 'SAPLF040' '0300'.                    "TR928
*  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  XLTAB-WRBTR.            "TR928
*  PERFORM BDC_FIELD  USING 'BSEG-MENGE'  XLTAB-MENGE.            "TR928
*  PERFORM BDC_FIELD  USING 'BSEG-MEINS'  XLTAB-MEINS.            "TR928
*  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  XLTAB-SGTXT.            "TR928
*  PERFORM BDC_FIELD  USING 'DKACB-FMORE' ' '.                    "TR928
*  PERFORM BDC_FIELD  USING 'RF05V-NEWBK' XLTAB-NEWBK.            "TR928
*  PERFORM BDC_FIELD  USING 'BSEG-VALUT'  XLTAB-VALUT.            "TR928
*  PERFORM START_NEXT_LINEITEM.
*  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.   "or use ENTE

ENDFORM.                                                          "TR928

*======================================================================*
*This is the BDC mapping for screen when entering a G/L line item with *
*Profitibility Segment                                                 *
*======================================================================*
FORM gl_line_item_profitibility.

  PERFORM bdc_screen USING 'SAPLF040' '0300'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-SGTXT'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'BSEG-WRBTR' xltab-wrbtr.
  PERFORM bdc_field  USING 'BSEG-MWSKZ' xltab-mwskz.
*  PERFORM BDC_FIELD  USING 'BSEG-VALUT' XLTAB-VALUT.
  PERFORM BDC_FIELD  USING 'BSEG-MENGE'  XLTAB-MENGE.
  PERFORM BDC_FIELD  USING 'BSEG-MEINS'  XLTAB-MEINS.
  PERFORM bdc_field  USING 'BSEG-SGTXT' xltab-sgtxt.
  PERFORM bdc_field  USING 'RF05V-NEWBS' xltab-bschl.
  PERFORM bdc_field  USING 'RF05V-NEWKO' xltab-saknr.
******
*  PERFORM bdc_screen USING 'SAPLKACB' '0002'.
*  PERFORM bdc_field  USING 'BDC_CURSOR' 'DKACB-XERGO'.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '=COBL_XERGO'.
*  PERFORM bdc_field  USING 'DKACB-XERGO' 'X'.
*  PERFORM bdc_field  USING 'COBL-MATNR' xltab-matnr.
*  PERFORM bdc_field  USING 'COBL-WERKS' xltab-werks.
  "----
*  PERFORM bdc_screen USING 'SAPLKEAK' '0300'.
*  PERFORM bdc_field  USING 'BDC_CURSOR' 'RKEAK-FIELD(09)'.
*  PERFORM bdc_field  USING 'BDC_OKCODE' '=WEIT'.
*  PERFORM bdc_field  USING 'RKEAK-FIELD(09)' xltab-VTWEG.
****
  PERFORM bdc_screen USING 'SAPLKACB' '0002'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-WERKS'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=ENTE'.
  PERFORM bdc_field  USING 'DKACB-XERGO' 'X'.
  PERFORM bdc_field  USING 'COBL-MATNR' xltab-matnr.
  PERFORM bdc_field  USING 'COBL-WERKS' xltab-werks.
****
  PERFORM bdc_screen USING 'SAPLKEAK' '0300'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'RKEAK-FIELD(01)'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=P+'.             "next page
  PERFORM bdc_field  USING 'RKEAK-FIELD(01)' xltab-field1.
  PERFORM bdc_field  USING 'RKEAK-FIELD(08)' xltab-field2.
 "------------
  PERFORM bdc_field  USING 'RKEAK-FIELD(09)' xltab-VTWEG.
****
  PERFORM bdc_screen USING 'SAPLKEAK' '0300'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'RKEAK-FIELD(09)'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=P+'.            "next page
  PERFORM bdc_field  USING  'RKEAK-FIELD(02)' xltab-field4. "SDP79989
  PERFORM bdc_field  USING  'RKEAK-FIELD(08)' xltab-field6.
  PERFORM bdc_field  USING  'RKEAK-FIELD(09)' xltab-field7.
  PERFORM bdc_field  USING  'RKEAK-FIELD(12)' xltab-field3. "SDP79989
****
  PERFORM bdc_screen USING 'SAPLKEAK' '0300'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'RKEAK-FIELD(06)'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '=WEIT'.
  PERFORM bdc_field  USING 'RKEAK-FIELD(02)' xltab-wwltl.
  PERFORM bdc_field  USING 'RKEAK-FIELD(08)' xltab-wwrsn.
  PERFORM bdc_field  USING 'RKEAK-FIELD(03)' xltab-field15.
  PERFORM bdc_field  USING 'RKEAK-FIELD(06)' xltab-wwsld.
****
  PERFORM bdc_screen USING 'SAPLKACB' '0002'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-WERKS'.
  PERFORM bdc_field  USING 'BDC_OKCODE' 'ENTE'.
  PERFORM bdc_field  USING 'COBL-WERKS' xltab-werks.
  PERFORM bdc_field  USING 'COBL-MATNR' xltab-matnr.
****
*  PERFORM BDC_SCREEN USING 'SAPLF040' '0330'.
*  PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'RF05V-NEWKO'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWBS' XLTAB-BSCHL.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWKO' XLTAB-SAKNR.

* OLD CODE
*  PERFORM BDC_SCREEN USING 'SAPLF040' '0300'.
*  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'  XLTAB-WRBTR.
*  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'  XLTAB-MWSKZ.
*  PERFORM BDC_FIELD  USING 'BSEG-VALUT'  XLTAB-VALUT.
*  PERFORM BDC_FIELD  USING 'BSEG-MENGE'  XLTAB-MENGE.
*  PERFORM BDC_FIELD  USING 'BSEG-MEINS'  XLTAB-MEINS.
*  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'  XLTAB-SGTXT.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWBS' XLTAB-BSCHL.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWKO' XLTAB-SAKNR.

*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ZK'.          "More data

*  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
*  PERFORM BDC_FIELD  USING 'DKACB-XERGO' 'X'.
*  PERFORM BDC_FIELD  USING 'COBL-MATNR'  XLTAB-MATNR.
*  PERFORM BDC_FIELD  USING 'COBL-WERKS'  XLTAB-WERKS.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.        "or use ENTE

*Profitibility Segment
*  PERFORM BDC_SCREEN USING  'SAPLKEAK'      '0300'.
*  PERFORM BDC_FIELD  USING  'RKEAK-FIELD(01)' XLTAB-FIELD1.
*   PERFORM BDC_FIELD  USING  'RKEAK-FIELD(08)' XLTAB-FIELD2.
*  PERFORM BDC_FIELD  USING  'BDC_OKCODE'    '=P+'.  "TR580

*  PERFORM BDC_SCREEN USING  'SAPLKEAK'      '0300'. "TR580
*  PERFORM BDC_FIELD  USING  'RKEAK-FIELD(12)' XLTAB-FIELD3.
*  PERFORM BDC_FIELD  USING  'RKEAK-FIELD(02)' XLTAB-FIELD4.
*  PERFORM BDC_FIELD  USING  'RKEAK-FIELD(03)' XLTAB-FIELD5.
*  PERFORM BDC_FIELD  USING  'RKEAK-FIELD(08)' XLTAB-FIELD6.
*  PERFORM BDC_FIELD  USING  'RKEAK-FIELD(09)' XLTAB-FIELD7.
*  PERFORM BDC_FIELD  USING  'BDC_OKCODE'    '=P+'.  "TR580
*   PERFORM BDC_FIELD  USING  'BDC_OKCODE'    '/00'.  "SDP28402 CHG

*  PERFORM BDC_SCREEN USING  'SAPLKEAK'      '0300'. "TR580
*  PERFORM BDC_FIELD  USING  'RKEAK-FIELD(03)' XLTAB-FIELD15.
*  PERFORM BDC_FIELD  USING  'RKEAK-FIELD(06)' XLTAB-WWSLD.  "SDP27706
*  PERFORM BDC_FIELD  USING  'BDC_OKCODE'    '/00'.

*  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.            "CHG
*  PERFORM BDC_FIELD  USING  'BDC_OKCODE'    '/00'.       "CHG

*   PERFORM BDC_SCREEN USING 'SAPLF040' '0330'.
ENDFORM.                    "GL_LINE_ITEM_PROFITIBILITY

*======================================================================*
*This is the BDC mapping for screen when entering a G/L line item with *
*Non Profitibility Segment                                             *
*======================================================================*
FORM gl_line_item_non_profitibility.

  PERFORM bdc_screen USING 'SAPLF040' '0300'.
*  PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'RF05V-NEWKO'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'BSEG-WRBTR' xltab-wrbtr.
  PERFORM bdc_field  USING 'BSEG-MWSKZ' xltab-mwskz.
  PERFORM bdc_field  USING 'BSEG-MENGE' xltab-menge.
  PERFORM bdc_field  USING 'BSEG-MEINS' xltab-meins.
  PERFORM bdc_field  USING 'BSEG-SGTXT' xltab-sgtxt.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWBS' XLTAB-BSCHL.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWKO' XLTAB-SAKNR.
*  PERFORM BDC_FIELD  USING 'DKACB-FMORE' 'X'.
****
  PERFORM bdc_screen USING 'SAPLKACB' '0002'.
  IF xltab-aufnr <> space.
    PERFORM bdc_field  USING 'COBL-AUFNR' xltab-aufnr.
  ENDIF.
  IF xltab-projn <> space.
    PERFORM bdc_field  USING 'COBL-PS_PSP_PNR' xltab-projn.
  ENDIF.
  IF xltab-kostl <> space.
    PERFORM bdc_field  USING 'COBL-KOSTL' xltab-kostl.
  ENDIF.
*  PERFORM bdc_field  USING 'COBL-PERNR'    xltab-pernr.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.
****
*  PERFORM BDC_SCREEN USING 'SAPLF040' '0330'.
*  PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'BSEG-LZBKZ'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '=PBBP'.

* OLD CODE
*  PERFORM BDC_SCREEN USING 'SAPLF040'      '0300'.
*  PERFORM BDC_FIELD  USING 'BSEG-WRBTR'    XLTAB-WRBTR.
*  IF SKB1-FSTAG = 'EXMM'.
*    PERFORM BDC_FIELD  USING 'BSEG-MWSKZ'    XLTAB-MWSKZ.
*  ENDIF.
*  PERFORM BDC_FIELD  USING 'BSEG-MENGE'    XLTAB-MENGE.
*  PERFORM BDC_FIELD  USING 'BSEG-MEINS'    XLTAB-MEINS.
*  PERFORM BDC_FIELD  USING 'BSEG-SGTXT'    XLTAB-SGTXT.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWBK'   XLTAB-NEWBK.
*  PERFORM START_NEXT_LINEITEM.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' 'ZK'.          "More data
*  PERFORM BDC_SCREEN USING 'SAPLKACB' '0002'.
* PERFORM BDC_FIELD  USING 'COBL-KOSTL'    XLTAB-KOSTL.
* PERFORM BDC_FIELD  USING 'COBL-PS_POSID' XLTAB-PROJN.
* PERFORM BDC_FIELD  USING 'COBL-AUFNR'    XLTAB-AUFNR.
* PERFORM BDC_FIELD  USING 'COBL-PERNR'    XLTAB-PERNR.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/8'.        "or use ENTE
*  PERFORM BDC_SCREEN USING 'SAPLF040' '0330'.

ENDFORM.                    "GL_LINE_ITEM_NON_PROFITIBILITY

*======================================================================*
*This is the BDC mapping for screen when entering a G/L line item with *
*Non Profitibility Segment                                             *
*======================================================================*
FORM gl_line_item_non_profit_payr.

  PERFORM bdc_screen USING 'SAPLF040' '0300'.
*  PERFORM BDC_FIELD  USING 'BDC_CURSOR' 'RF05V-NEWKO'.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'BSEG-WRBTR' xltab-wrbtr.
*  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ' XLTAB-MWSKZ.
  PERFORM bdc_field  USING 'BSEG-MENGE' xltab-menge.
  PERFORM bdc_field  USING 'BSEG-MEINS' xltab-meins.
  PERFORM bdc_field  USING 'BSEG-SGTXT' xltab-sgtxt.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWBS' XLTAB-BSCHL.
*  PERFORM BDC_FIELD  USING 'RF05V-NEWKO' XLTAB-SAKNR.
*  PERFORM BDC_FIELD  USING 'DKACB-FMORE' 'X'.
****
  PERFORM bdc_screen USING 'SAPLKACB' '0002'.
  IF xltab-aufnr <> space.
    PERFORM bdc_field  USING 'COBL-AUFNR' xltab-aufnr.
  ENDIF.
  IF xltab-projn <> space.
    PERFORM bdc_field  USING 'COBL-PS_PSP_PNR' xltab-projn.
  ENDIF.
  IF xltab-kostl <> space.
    PERFORM bdc_field  USING 'COBL-KOSTL' xltab-kostl.
  ENDIF.
  PERFORM bdc_field  USING 'COBL-PERNR'    xltab-pernr.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.


ENDFORM.                    "GL_LINE_ITEM_NON_PROFIT_PAYR
*======================================================================*
*       Form  SCREEN_302                                               *
*======================================================================*

*FORM SCREEN_302.

*  PERFORM BDC_SCREEN USING 'SAPLF040' '302'.
*  PERFORM BDC_FIELD  USING 'BSEG-WRBTR' XLTAB-WRBTR.
* Turn on Calculate Tax checkbox
*  PERFORM BDC_FIELD  USING 'BKPF-XMWST' XLTAB-XMWST.
*  PERFORM BDC_FIELD  USING 'BSEG-MWSKZ' XLTAB-MWSKZ.
*  PERFORM BDC_FIELD  USING 'BSEG-ZTERM' XLTAB-ZTERM.
*  PERFORM BDC_FIELD  USING 'BSEG-ZFBDT' XLTAB-ZFBDT.
*  PERFORM BDC_FIELD  USING 'BSEG-UZAWE' XLTAB-UZAWE.
*  PERFORM BDC_FIELD  USING 'BSEG-SGTXT' XLTAB-SGTXT.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '=ZK'.

*  PERFORM BDC_SCREEN USING 'SAPLF040' '0332'.
*  PERFORM BDC_FIELD  USING 'BSEG-XREF3' XLTAB-XREF3.
*  PERFORM BDC_FIELD  USING 'BDC_OKCODE' '=SB'.

*ENDFORM.                    "screen_302
*======================================================================*
*     FORM OPEN_BATCH_SESSION                                          *
* - This routine just simply opens up a new batch input session.       *
*======================================================================*
FORM open_batch_session.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = w_bdc_session
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
    MESSAGE e004 WITH w_bdc_session.
  ENDIF.

ENDFORM.                    "OPEN_BATCH_SESSION

*======================================================================*
*     FORM INSERT_SESSION                                              *
* - This routine inserts the BDC data for one transaction into the     *
*   batch input session.                                               *
*======================================================================*
FORM insert_session.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = 'FBV1'
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

ENDFORM.                    "INSERT_SESSION

*======================================================================*
*     FORM CLOSE_SESSION                                               *
* - This routine simply closes the current batch input session.        *
*======================================================================*
FORM close_session.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open
      queue_error.
  IF sy-subrc = 0.
    MESSAGE i003 WITH w_bdc_session.
  ENDIF.
ENDFORM.                    "CLOSE_SESSION

*======================================================================*
*                FORM BDC_SCREEN                                       *
*----------------------------------------------------------------------*
*  Description:                                                        *
*  - This routine adds an entry to the table BDCDATA with screen       *
*    information from a particular transaction.  This is used as part  *
*    of the process for creating data for batch input.                 *
*                                                                      *
*  Parameters:                                                         *
*      -->  PROGRAM - Program name of the screen                       *
*           DNYPRO  - Screen number                                    *
*======================================================================*
FORM bdc_screen USING program dynpro.

  CLEAR bdcdata.
  bdcdata-program = program.
  bdcdata-dynpro = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.

ENDFORM.                    "BDC_SCREEN
*======================================================================*
*               FORM BDC_FIELD                                         *
*----------------------------------------------------------------------*
*  Description:                                                        *
*  - This routine adds an entry to the table BDCDATA with field        *
*    information from a particular transaction.  This is used as part  *
*    of the process for creating data for batch input.                 *
*                                                                      *
*  Parameters:                                                         *
*      -->  FNAM - name of the field on the screen                     *
*           FVAL - value to be entered for that field on the           *
*                  screen                                              *
*======================================================================*
FORM bdc_field USING fnam fval.

  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.

ENDFORM.                    "BDC_FIELD

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_filein
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH

*&---------------------------------------------------------------------*
*&      Form  generate_report_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM generate_report_header.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 'ZFFII041', 45 'Error Detail Report'.
  WRITE: 106 'Date:', sy-datum, '@', sy-uzeit.
  WRITE: /50 sy-datum.
  WRITE: 121 'Page:', sy-pagno.
  SKIP.

  MOVE '2' TO ln_cntr.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "generate_report_header
*&---------------------------------------------------------------------*
*&      Form  LINE_ITEM_301
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM line_item_301 .

*  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'BDC_CURSOR' 'BSEG-SGTXT'.
  PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field  USING 'BSEG-WRBTR' xltab-wrbtr.
  PERFORM bdc_field  USING 'BSEG-SGTXT' xltab-sgtxt.
*  PERFORM bdc_field  USING 'RF05V-NEWBS' xltab-bschl.
*  PERFORM bdc_field  USING 'RF05V-NEWKO' xltab-saknr.


ENDFORM.                    " LINE_ITEM_301
*&---------------------------------------------------------------------*
*&      Form  DATA_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_validation .
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
           lv_records TYPE i,
           lv_rows    TYPE i,
           lv_itemno_acc TYPE bapiaccr08-itemno_acc,
           lv_shkzg TYPE tbsl-shkzg,
           lv_kunnr TYPE kunde_pa,
           ls_ekko TYPE ekko.

  CLEAR: gs_docheader,
         gt_gl_litems,
         gs_gl_litems,
         gt_cur_litems,
         gs_cur_litems,
         gt_cus_litems,
         gs_cus_litems,
         gt_criteria,
         gt_valuefield,
         gs_criteria,
         gs_valuefield,
         gt_return,
         gs_return,
         gt_msg,
         gs_msg,
         w_chk_err_found,
         gt_extension,
         gs_extension.
*         gv_err_found,
*         gs_gl,
*         gt_gl.

  DESCRIBE TABLE gt_data LINES lv_records.

  LOOP AT gt_data INTO gs_data.
    IF  gs_data-bukrs <> lv_bukrs OR
        gs_data-blart <> lv_blart OR
        gs_data-bldat <> lv_bldat OR
        gs_data-budat <> lv_budat OR
        gs_data-monat <> lv_monat OR
        gs_data-waers <> lv_waers OR
        gs_data-xblnr <> lv_xblnr OR
        gs_data-bktxt <> lv_bktxt OR
        gs_data-kursf <> lv_kursf.
      IF lv_not_first IS NOT INITIAL.
        PERFORM bapi_validate.
      ENDIF.
      lv_not_first = 'X'.
      lv_bukrs = gs_data-bukrs.
      lv_blart = gs_data-blart.
      lv_bldat = gs_data-bldat.
      lv_budat = gs_data-budat.
      lv_monat = gs_data-monat.
      lv_waers = gs_data-waers.
      lv_xblnr = gs_data-xblnr.
      lv_bktxt = gs_data-bktxt.
      lv_kursf = gs_data-kursf.
      CLEAR: lv_itemno_acc,
             gs_docheader,
             gs_cur_litems,
             gt_cur_litems,
             gt_gl_litems,
             gs_gl_litems,
             gt_cus_litems,
             gs_cus_litems,
             gs_gl,
             gt_gl,
             gt_extension,
             gs_extension.
    ENDIF.
    CLEAR lv_shkzg.
    SELECT SINGLE shkzg INTO lv_shkzg
    FROM tbsl
   WHERE bschl = gs_data-bschl.
*Load header data
    gs_docheader-obj_type = 'BKPFF'.
    gs_docheader-username = sy-uname.
    gs_docheader-header_txt = gs_data-bktxt.
    gs_docheader-comp_code = gs_data-bukrs.
    gs_docheader-doc_date = gs_data-bldat.
    gs_docheader-pstng_date = gs_data-budat.
*       gs_Docheader-trans_date = -valut.
    gs_docheader-doc_type = gs_data-blart.
    gs_docheader-ref_doc_no = gs_data-xblnr.
*    IF gs_data-saknr IS NOT INITIAL.
    IF gs_data-bschl = '40' OR gs_data-bschl = '50'.
      lv_itemno_acc = lv_itemno_acc + 1.
"Exntesion
*      STRUCTURE  Types TE_STRUC
*VALUEPART1	Types	VALUEPART
*VALUEPART2	Types	VALUEPART
*VALUEPART3	Types	VALUEPART
*VALUEPART4	Types	VALUEPART
    if gs_data-pernr is not initial.
      gs_extension-structure = 'ZFFII041_PERNR'.
      gs_extension-valuepart1 = lv_itemno_acc.
      gs_extension-valuepart2 = gs_data-pernr.
      APPEND gs_extension to gt_extension.
    endif.
*     Load Account GL line segment
      gs_gl_litems-itemno_acc    = lv_itemno_acc.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_data-saknr
          IMPORTING
            output = gs_gl_litems-gl_account.
*      gs_gl_litems-gl_account    = gs_data-saknr.
      gs_gl_litems-comp_code     = gs_data-bukrs.
      gs_gl_litems-pstng_date    = gs_data-budat.
      gs_gl_litems-ref_key_1     = gs_data-xblnr.
      gs_gl_litems-item_text     = gs_data-sgtxt.
      gs_gl_litems-costcenter    = gs_data-kostl.
      gs_gl_litems-orderid       = gs_data-aufnr.
      gs_gl_litems-wbs_element   = gs_data-projn. "projk.
      gs_gl_litems-plant         = gs_data-werks.
      TRANSLATE gs_gl_litems-plant to UPPER CASE.
      gs_gl_litems-material      = gs_data-matnr.

*      gs_gl_litems-network       = gs_data-nplnr.
*      gs_gl_litems-activity      = gs_data-vornr.
      gs_gl_litems-tax_code      = gs_data-mwskz.
      APPEND gs_gl_litems TO gt_gl_litems.
*       Load GL currency line segment
      gs_cur_litems-itemno_acc    = lv_itemno_acc.
      gs_cur_litems-currency      = gs_data-waers.
      gs_cur_litems-EXCH_RATE     = gs_data-kursf.
      CLEAR lv_shkzg.
      SELECT SINGLE shkzg INTO lv_shkzg
      FROM tbsl
     WHERE bschl = gs_data-bschl.
      IF lv_shkzg = 'S'.
        gs_cur_litems-amt_doccur = gs_data-wrbtr.
      ELSEIF lv_shkzg = 'H'.
        gs_cur_litems-amt_doccur = gs_data-wrbtr * -1.
      ELSE.
        gs_cur_litems-amt_doccur = 0.
      ENDIF.
      APPEND gs_cur_litems TO gt_cur_litems.
      IF gs_data-field1 IS NOT INITIAL. "Customer
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'KNDNR'. "'FIELD(03)'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_data-field1
          IMPORTING
            output = lv_kunnr.
        gs_criteria-character  = lv_kunnr. "gs_Data-field1.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'KNDNR'. "'FIELD(03)'.
      ENDIF.
      IF gs_data-field2 IS NOT INITIAL. "Sales org
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'VKORG'. "'FIELD(03)'.
        gs_criteria-character  = gs_data-field2.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'VKORG'. "'FIELD(03)'.
      ENDIF.
      IF gs_data-field3 IS NOT INITIAL. "Branch
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'WWBRN'. "'FIELD(03)'.
        gs_criteria-character  = gs_data-field3.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'WWBRN'. "'FIELD(03)'.
      ENDIF.
      IF gs_data-field4 IS NOT INITIAL. "Sector
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'WWSCT'. "'FIELD(03)'.
        gs_criteria-character  = gs_data-field4.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'WWSCT'. "'FIELD(03)'.
      ENDIF.
      IF gs_data-field5 IS NOT INITIAL. "Segment
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'WWSEG'. "'FIELD(03)'.
        gs_criteria-character  = gs_data-field5.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'WWSEG'. "'FIELD(03)'.
      ENDIF.
      IF gs_data-field6 IS NOT INITIAL. "Rate Class
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'WWRAT'. "'FIELD(03)'.
        gs_criteria-character  = gs_data-field6.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'WWRAT'. "'FIELD(03)'.
      ENDIF.
      IF gs_data-field7 IS NOT INITIAL. "Service Class
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'WWSER'. "'FIELD(03)'.
        gs_criteria-character  = gs_data-field7.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'WWSER'. "'FIELD(03)'.
      ENDIF.
      IF gs_data-field15 IS NOT INITIAL. "SA Number
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'WWSNO'. "'FIELD(03)'.
        gs_criteria-character  = gs_data-field15.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'WWSNO'. "'FIELD(03)'.
      ENDIF.
*      IF gs_data-pernr IS NOT INITIAL. "Sales Person
*        gs_criteria-itemno_acc = lv_itemno_acc.
*        gs_criteria-fieldname  = 'WWSLS'. "'FIELD(03)'.
*        gs_criteria-character  = gs_data-pernr.
*        APPEND gs_criteria TO gt_criteria.
*        gs_valuefield-itemno_acc = lv_itemno_acc.
*        gs_valuefield-fieldname  = 'WWSLS'. "'FIELD(03)'.
*      ENDIF.
      IF gs_data-wwsld IS NOT INITIAL.  "Sales District
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'WWSLD'. "'FIELD(03)'.
        gs_criteria-character  = gs_data-wwsld.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'WWSLD'. "'FIELD(03)'.
      ENDIF.
       IF gs_data-VTWEG IS NOT INITIAL.  "Distribution Channel
        gs_criteria-itemno_acc = lv_itemno_acc.
        gs_criteria-fieldname  = 'VTWEG'.
        gs_criteria-character  = gs_data-VTWEG.
        APPEND gs_criteria TO gt_criteria.
        gs_valuefield-itemno_acc = lv_itemno_acc.
        gs_valuefield-fieldname  = 'VTWEG'.
      ENDIF.
*******
*      MOVE-CORRESPONDING gs_data TO gs_gl.
*      APPEND gs_gl TO gt_gl.
    ENDIF.
*    IF gs_data-saknr IS INITIAL AND
*       gs_data-field1 IS NOT INITIAL. "customer
    IF gs_data-bschl = '02' OR gs_data-bschl = '11'.
*     Load Account GL line segment
      lv_itemno_acc = lv_itemno_acc + 1.
      gs_cus_litems-itemno_acc    = lv_itemno_acc.
       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_data-saknr
          IMPORTING
            output = gs_cus_litems-customer.
*      gs_cus_litems-customer      = gs_data-saknr.
      gs_cus_litems-comp_code     = gs_data-bukrs.
      gs_cus_litems-ref_key_1     = gs_data-xblnr.
      gs_cus_litems-item_text     = gs_data-sgtxt.
      gs_cus_litems-tax_code      = gs_data-mwskz.
*      gs_cus_litems-alloc_nmbr    = gs_data-zuonr.
      APPEND gs_cus_litems TO gt_cus_litems.
*       Load GL currency line segment
      gs_cur_litems-itemno_acc    = lv_itemno_acc.
      gs_cur_litems-currency      = gs_data-waers.
      gs_cur_litems-EXCH_RATE     = gs_data-kursf.
      IF lv_shkzg = 'S'.
        gs_cur_litems-amt_doccur = gs_data-wrbtr.
      ELSEIF lv_shkzg = 'H'.
        gs_cur_litems-amt_doccur = gs_data-wrbtr * -1.
      ELSE.
        gs_cur_litems-amt_doccur = 0.
      ENDIF.
      APPEND gs_cur_litems TO gt_cur_litems.
    ENDIF.

*****************
    IF gs_data-bschl = '02' OR gs_data-bschl = '11' OR
       gs_data-bschl = '40' OR gs_data-bschl = '50'.
    ELSE.
*      gv_err_found = 'X'.
      w_chk_err_found = 'X'.
      gs_msg-msg_text = text-001.
      APPEND gs_msg TO gt_msg.
    ENDIF.
******
    lv_rows = lv_rows + 1.
    IF lv_records <= lv_rows.   "execute, because at last reocrds,
      PERFORM bapi_validate.   " it will exit the loop without
      EXIT.                    "executing bapi_validate at top.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DATA_VALIDATION
*&---------------------------------------------------------------------*
*&      Form  BAPI_VALIDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_validate .
  DATA: lv_first TYPE xfeld,
         lv_format TYPE bapitga-textformat VALUE 'ASC',
         ls_skb1 TYPE skb1.

  CLEAR gt_return.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader    = gs_docheader
*     CUSTOMERCPD       =
*     CONTRACTHEADER    =
    TABLES
      accountgl         = gt_gl_litems
      accountreceivable = gt_cus_litems
*     ACCOUNTPAYABLE    =
*     ACCOUNTTAX        =
      currencyamount    = gt_cur_litems
      criteria          = gt_criteria
      valuefield        = gt_valuefield
*      EXTENSION1        =
      return            = gt_return
*       PAYMENTCARD             =
*       CONTRACTITEM            =
       EXTENSION2              = gt_extension.
*       REALESTATE              =
*       ACCOUNTWT               =

  CLEAR: gs_msg.

  LOOP AT gt_return INTO gs_return.
    CLEAR: gs_msg.
    IF gs_return-type = 'E' OR
       gs_return-type = 'A'.
      IF lv_first IS INITIAL.
        CONCATENATE gs_docheader-comp_code
                    gs_docheader-doc_type
                    gs_docheader-doc_date
                    gs_docheader-pstng_date
                    gs_docheader-fis_period
                    gs_docheader-ref_doc_no
                    gs_docheader-header_txt
                    INTO gs_msg-msg_text SEPARATED BY space.
*        MOVE gs_docheader TO gs_msg-msg_text.
        APPEND gs_msg TO gt_msg.
        lv_first = 'X'.
      ENDIF.
*      IF ls_return-number <> '808' AND
*         ls_return-number <> '609' AND
*         ls_return-number <> '235'.
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
      gs_msg-msgty = gs_return-type.
      gs_msg-msgid = gs_return-id.
      gs_msg-msgno = gs_return-number.
      CONCATENATE gs_return-number '-'
                  gs_return-message
             INTO gs_msg-msg_text
         SEPARATED BY space.
      APPEND gs_msg TO gt_msg.
*      gv_err_found = 'X'.
      w_chk_err_found = 'X'.
*      ENDIF.
    ENDIF.
  ENDLOOP.
**Validate tax code
**  IF gv_err_found IS INITIAL.
*  LOOP AT gt_gl INTO gs_gl.
*    IF gs_gl-saknr IS INITIAL.
*      CONTINUE.
*    ENDIF.
*    CLEAR: ls_skb1.
*    CALL FUNCTION 'READ_SKB1'
*      EXPORTING
*        xbukrs         = gs_gl-bukrs
*        xsaknr         = gs_gl-hkont
*      IMPORTING
*        xskb1          = ls_skb1
*      EXCEPTIONS
*        key_incomplete = 1
*        not_authorized = 2
*        not_found      = 3
*        OTHERS         = 4.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*    IF ls_skb1-xmwno IS INITIAL.
*      IF ls_skb1-mwskz IS NOT INITIAL AND
*         gs_gl-mwskz IS INITIAL.
*        CONCATENATE 'Tax code is required for Company' gs_gl-bukrs
*                    'GL Account' gs_gl-hkont INTO gs_msg-msg_text
*                    SEPARATED BY space.
*        APPEND gs_msg TO gt_msg.
*        gv_err_found = 'X'.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
  CLEAR: gt_gl,
         gs_gl.
ENDFORM.                    " BAPI_VALIDATE
*&---------------------------------------------------------------------*
*&      Form  MOVE_DATA_FOR_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_data_for_validation .

  DATA: lv_bukrs TYPE bkpf-bukrs.
  CLEAR: gt_data,
         gs_data.
  LOOP AT xltab.
    CASE xltab-bukrs.
      WHEN space.
        gs_data-bschl = xltab-bschl.
        gs_data-saknr = xltab-saknr.
        TRANSLATE gs_data-saknr to UPPER CASE.
        gs_data-newbw = xltab-newbw.
        gs_data-mwskz = xltab-mwskz.
        gs_data-newbk = xltab-newbk.
        gs_data-wrbtr = xltab-wrbtr.
        gs_data-menge = xltab-menge.
        gs_data-meins = xltab-meins.
        gs_data-sgtxt = xltab-sgtxt.
        gs_data-kostl = xltab-kostl.
        IF xltab-kostl IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = xltab-kostl
            IMPORTING
              output = gs_data-kostl.
        ENDIF.
        gs_data-projn = xltab-projn.
        gs_data-aufnr = xltab-aufnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = xltab-aufnr
          IMPORTING
            output = gs_data-aufnr.
*           gs_data-matnr = xltab-matnr.
        IF xltab-matnr IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = xltab-matnr
            IMPORTING
              output       = gs_data-matnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
          IF sy-subrc <> 0.
*              Implement suitable error handling here
          ENDIF.
        ENDIF.
        gs_data-pernr = xltab-pernr.

        gs_data-werks = xltab-werks.
        TRANSLATE gs_data-werks to UPPER CASE.

        gs_data-field1 = xltab-field1.          "Customer
*           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*           EXPORTING
*              input  = xltab-field1
*          IMPORTING
*            output = gs_data-field1.
        gs_data-field2 = xltab-field2.          "Sales Org
        gs_data-field3 = xltab-field3.          "Branch
        gs_data-field4 = xltab-field4.          "Sector
        gs_data-field5 = xltab-field5.          "Segment
        gs_data-field6 = xltab-field6.          "Rate Class
        gs_data-field7 = xltab-field7.          "Service Class
        gs_data-VTWEG = xltab-VTWEG.          "Distribution Channel
        gs_data-field15 = xltab-field15.          "S.A. Number
        gs_data-valut = xltab-valut.           "Value Date              "TR928
        gs_data-wwsld = xltab-wwsld.        "Sales District       "SDP27706
        gs_data-wwltl = xltab-wwltl.
        gs_data-wwrsn = xltab-wwrsn.
*Start of changes by AHMADT for CHG0201827
        gs_data-zuonr = xltab-zuonr.
*End of changes by AHMADT for CHG0201827
        "append data
        APPEND gs_data TO gt_data.
      WHEN OTHERS.
        CLEAR gs_data.
        gs_data-bukrs = xltab-bukrs.
        gs_data-blart = xltab-blart.
        gs_data-bldat = xltab-bldat.
        gs_data-budat = xltab-budat.
        gs_data-monat = xltab-monat.
        gs_data-waers = xltab-waers.
        gs_data-xblnr = xltab-xblnr.
        gs_data-bktxt = xltab-bktxt.
        gs_data-kursf = xltab-kursf.
***********line item
        gs_data-bschl = xltab-bschl.
        gs_data-saknr = xltab-saknr.
        TRANSLATE gs_data-saknr to UPPER CASE.
        gs_data-newbw = xltab-newbw.
        gs_data-mwskz = xltab-mwskz.
        gs_data-newbk = xltab-newbk.
        gs_data-wrbtr = xltab-wrbtr.
        gs_data-menge = xltab-menge.
        gs_data-meins = xltab-meins.
        gs_data-sgtxt = xltab-sgtxt.
        gs_data-kostl = xltab-kostl.
        IF xltab-kostl IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = xltab-kostl
            IMPORTING
              output = gs_data-kostl.
        ENDIF.
        gs_data-projn = xltab-projn.
        gs_data-aufnr = xltab-aufnr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = xltab-aufnr
          IMPORTING
            output = gs_data-aufnr.
*           gs_data-matnr = xltab-matnr.
        IF xltab-matnr IS NOT INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input        = xltab-matnr
            IMPORTING
              output       = gs_data-matnr
            EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
          IF sy-subrc <> 0.
*              Implement suitable error handling here
          ENDIF.
        ENDIF.
        gs_data-pernr = xltab-pernr.
        gs_data-werks = xltab-werks.
        TRANSLATE gs_data-werks to UPPER CASE.
        gs_data-field1 = xltab-field1.          "Customer
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = xltab-field1
          IMPORTING
            output = gs_data-field1.
        gs_data-field2 = xltab-field2.          "Sales Org
        gs_data-field3 = xltab-field3.          "Branch
        gs_data-field4 = xltab-field4.          "Sector
        gs_data-field5 = xltab-field5.          "Segment
        gs_data-field6 = xltab-field6.          "Rate Class
        gs_data-field7 = xltab-field7.          "Service Class
        gs_data-VTWEG = xltab-VTWEG.          "Distribution Channel
        gs_data-field15 = xltab-field15.          "S.A. Number
        gs_data-valut = xltab-valut.           "Value Date              "TR928
        gs_data-wwsld = xltab-wwsld.        "Sales District       "SDP27706
        gs_data-wwltl = xltab-wwltl.
        gs_data-wwrsn = xltab-wwrsn.
*Start of changes by AHMADT for CHG0201827
        gs_data-zuonr = xltab-zuonr.
*End of changes by AHMADT for CHG0201827
        "append data
        APPEND gs_data TO gt_data.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " MOVE_DATA_FOR_VALIDATION
