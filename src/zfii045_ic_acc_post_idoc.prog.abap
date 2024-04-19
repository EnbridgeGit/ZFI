*&---------------------------------------------------------------------*
*& Report  ZFII045_IC_ACC_POST_IDOC
*&
*&---------------------------------------------------------------------*
*& CHANGES:                                                            *
*&Date     Ticket#  By      Desaciption                                *
*&14.11.2013 53808  M Khan  uPDATE EEM-IC Interface (prog ZFII045) for *
*&                          trading partners feed to US.               *
*&---------------------------------------------------------------------*
REPORT  zfii045_ic_acc_post_idoc LINE-SIZE 255.


TABLES: zfi_ic_acct_xref,
        cobk,
        aufk.
DATA: BEGIN OF ty_coep,
        zcomp(20),
        zkey(57),
        kokrs  TYPE coep-kokrs,
        belnr  TYPE coep-belnr,
        bukrs  TYPE coep-bukrs,
        kstar  TYPE coep-kstar,
        objnr  TYPE coep-objnr,
        sgtxt  TYPE coep-sgtxt,
        wtgbtr TYPE coep-wtgbtr,
        twaer  TYPE coep-twaer,
      END OF ty_coep.

TYPES: BEGIN OF ty_msg,
        msgid TYPE symsgid,
        msgty TYPE symsgty,
        msgno TYPE symsgno,
        msg_text(250),
       END OF ty_msg.

DATA: gt_zfi_ic_documents TYPE TABLE OF zfi_ic_documents.
DATA: gv_idoc_control TYPE edidc,
      gv_idoc_header  TYPE e1bpache08,
      gv_idoc_line    TYPE e1bpacgl08,
      gv_idoc_curr    TYPE e1bpaccr08,
      gt_idoc_line    TYPE TABLE OF e1bpacgl08,
      gt_idoc_curr    TYPE TABLE OF e1bpaccr08.
DATA: gs_int_edidd TYPE edidd,
      gt_int_edidd TYPE TABLE OF edidd.
DATA: gt_msg TYPE TABLE OF ty_msg,
      gs_msg LIKE LINE OF gt_msg,
      gv_error.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_appl TYPE zappl_area OBLIGATORY DEFAULT 'FITV'.
SELECT-OPTIONS: s_kokrs FOR cobk-kokrs NO-EXTENSION OBLIGATORY,
                s_refbt FOR cobk-blart OBLIGATORY DEFAULT 'TV',
                s_budat FOR cobk-budat NO-EXTENSION OBLIGATORY,
                s_aufnr FOR aufk-aufnr,
                s_auart FOR aufk-auart  OBLIGATORY DEFAULT 'IC01'.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_acchk AS CHECKBOX,
            p_idoc  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_objtyp TYPE e1bpache08-obj_type OBLIGATORY DEFAULT 'BKPFF',
            p_objkey TYPE e1bpache08-obj_key OBLIGATORY,
            p_objsys TYPE e1bpache08-obj_sys OBLIGATORY,
            p_gjahr  TYPE gjahr OBLIGATORY,
            p_bldat  TYPE bldat OBLIGATORY,
            p_budat  TYPE budat OBLIGATORY,
            p_trand  TYPE budat OBLIGATORY,
            p_blart  TYPE blart OBLIGATORY,
            p_refdc  TYPE belnr OBLIGATORY,
            p_monat  TYPE monat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.

  MOVE sy-datum(4) TO p_gjahr.

START-OF-SELECTION.

  PERFORM get_process_data.

*&---------------------------------------------------------------------*
*&      Form  get_process_data
*&---------------------------------------------------------------------*
*       Extract Data
*----------------------------------------------------------------------*
FORM get_process_data .

  DATA: lt_cobk TYPE TABLE OF cobk,
        ls_cobk TYPE cobk,
        lt_coep TYPE TABLE OF coep,
        ls_coep TYPE coep,
        lt_bkpf TYPE TABLE OF bkpf,
        ls_bkpf TYPE bkpf,
        lv_awkey TYPE bkpf-awkey,
        lv_tabix TYPE sy-tabix,
        ls_line TYPE coep,
        ltn_coep LIKE TABLE OF ty_coep,
        lsn_coep LIKE ty_coep,
        lsn_coep1 LIKE ty_coep,
        ls_aufk TYPE aufk,
        lt_aufk TYPE TABLE OF aufk,
        lv_kokrs TYPE coep-kokrs,
        lv_belnr TYPE coep-belnr,
        lv_zcomp LIKE lsn_coep-zcomp,
        lv_item_no TYPE i,
        lv_wtgbtr TYPE coep-wtgbtr,
        ls_zfi_ic_documents TYPE zfi_ic_documents.

  CLEAR: gt_msg,
         gt_zfi_ic_documents.

  SELECT * FROM cobk INTO TABLE lt_cobk
                     WHERE kokrs IN s_kokrs
*                       AND refbt IN s_refbt
                        AND blart IN s_refbt
                       AND budat IN s_budat.
  IF lt_cobk IS INITIAL.
    WRITE : / 'No data to process....'.
    STOP.
  ENDIF.
  SELECT * FROM coep INTO TABLE lt_coep
           FOR ALL ENTRIES IN lt_cobk
                     WHERE kokrs = lt_cobk-kokrs
                       AND belnr = lt_cobk-belnr.
  LOOP AT lt_cobk INTO ls_cobk.
    lv_awkey = ls_cobk-refbn.
    SELECT * FROM bkpf APPENDING TABLE lt_bkpf
                       WHERE gjahr = ls_cobk-gjahr
                         AND blart = ls_cobk-blart
                         AND awkey = lv_awkey.
  ENDLOOP.
  LOOP AT lt_coep INTO ls_coep.
*check if document is already posted. don't proceed if already posted.
    SELECT SINGLE * FROM zfi_ic_documents INTO ls_zfi_ic_documents
                    WHERE kokrs = ls_coep-kokrs
                      AND belnr = ls_coep-belnr.
    CHECK sy-subrc <> 0.
    SELECT SINGLE * FROM aufk INTO ls_aufk
                   WHERE objnr = ls_coep-objnr
                     AND auart IN s_auart
                     AND aufnr IN s_aufnr.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING ls_coep TO lsn_coep.
    lsn_coep-zcomp = ls_aufk-user0.
    CONCATENATE lsn_coep-kokrs lsn_coep-belnr
                lsn_coep-zcomp ls_aufk-objnr INTO lsn_coep-zkey.
    APPEND lsn_coep TO ltn_coep.
    APPEND ls_aufk TO lt_aufk.
  ENDLOOP.
  IF ltn_coep[] IS INITIAL.
    WRITE: / 'No Data to process...'.
    STOP.
  ENDIF.
* here move data according to the IDoc
  SORT lt_cobk BY kokrs belnr.
*sort lt_coep by kokrs belnr objnr.
  SORT ltn_coep BY kokrs belnr zcomp objnr.
  LOOP AT ltn_coep INTO lsn_coep.
    lsn_coep1 = lsn_coep.
    AT NEW zkey.
      CLEAR: ls_cobk,
             lv_item_no,
             lv_wtgbtr,
             gv_idoc_control,
             gv_idoc_header,
             gv_idoc_line,
             gv_idoc_curr,
             gt_idoc_line,
             gt_idoc_curr.
      READ TABLE lt_cobk INTO ls_cobk WITH KEY
                         kokrs = lsn_coep1-kokrs
                         belnr = lsn_coep1-belnr.
      PERFORM idoc_control_record USING ls_cobk
                                        lsn_coep1-zcomp.
      PERFORM header_segment USING ls_cobk.
    ENDAT.
    lv_item_no = lv_item_no + 1.
    lv_wtgbtr = lv_wtgbtr + lsn_coep-wtgbtr.
    PERFORM gl_lineitem_segment TABLES lt_bkpf
                                       lt_aufk
                                USING  ls_cobk
                                       lsn_coep
                                       lv_item_no.
*    PERFORM curr_amount_segment USING lsn_coep
*                                      lv_item_no.
    MOVE-CORRESPONDING lsn_coep TO ls_coep.
    AT END OF zkey.
      lv_item_no = lv_item_no + 1.
      PERFORM gl_lineitemtotal_segment TABLES lt_bkpf
                                              lt_aufk
                                       USING  ls_cobk
                                              ls_coep
                                              lv_wtgbtr
                                              lv_item_no.
      PERFORM idoc_segments.
      IF p_idoc IS NOT INITIAL OR
         p_acchk IS NOT INITIAL.
        PERFORM pre_check_validation.
      ENDIF.
      IF gv_error IS INITIAL AND
         p_idoc IS NOT INITIAL.
        PERFORM distribute_idoc.
        IF gs_msg-msg_text IS INITIAL.
          PERFORM map_custom_table USING ls_cobk.
        ENDIF.
      ENDIF.
    ENDAT.
  ENDLOOP.
  IF gt_zfi_ic_documents[] IS NOT INITIAL.
    PERFORM update_custom_table.
  ENDIF.
  IF gt_msg[] IS NOT INITIAL.
    WRITE: / '************Error Messages************'.
  ENDIF.
  LOOP AT gt_msg INTO gs_msg.
    WRITE: / gs_msg-msg_text.
  ENDLOOP.
  SKIP 2.
  WRITE: / 'Process is completed...........'.
ENDFORM.                    " get_process_data
*&---------------------------------------------------------------------*
*&      Form  IDOC_CONTROL_RECORD
*&---------------------------------------------------------------------*
*       Populate Control Segment for IDoc
*----------------------------------------------------------------------*
FORM idoc_control_record USING ls_cobk TYPE cobk
                               p_comp LIKE ty_coep-zcomp.

  DATA: lv_own_logical_system TYPE tbdls-logsys,
        lv_zparamtype TYPE zparamtype,
        lv_tb_rfcdest TYPE tb_rfcdest,
        lv_logdes TYPE edi_logdes.

  CLEAR gv_idoc_control.
  gv_idoc_control-direct = '1'.
  gv_idoc_control-mestyp = 'ACC_GL_POSTING'.     "Message Type
  gv_idoc_control-idoctp = 'ACC_GL_POSTING01'.   "Basic type
  gv_idoc_control-cimtyp = space.          "Extension
*  UNPACK '1' TO CONTROL_RECORD-DOCNUM.        "IDoc number #1

  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = lv_own_logical_system
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 0.

  gv_idoc_control-sndprt = 'LS'.                   "Partner type of sender
  gv_idoc_control-sndprn = lv_own_logical_system.  " own logical system
  lv_tb_rfcdest = lv_own_logical_system.

*  SELECT SINGLE port INTO gv_idoc_control-sndpor
*         FROM edipoa WHERE logdes = lv_tb_rfcdest.
  CLEAR lv_tb_rfcdest.
  CONDENSE p_comp.
  CONCATENATE 'ECC' p_comp INTO lv_zparamtype.
  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      imp_paramtype = lv_zparamtype
    IMPORTING
      exp_rfcdest   = lv_tb_rfcdest.
  "Receiver port-SAP System
  SELECT SINGLE port INTO gv_idoc_control-rcvpor
         FROM edipoa WHERE logdes = lv_tb_rfcdest.

  gv_idoc_control-rcvprt = 'LS'.               "Partner Type of Receiver
  gv_idoc_control-rcvprn =  lv_tb_rfcdest. "Partner #  / logical system of Receiver



ENDFORM.                    " IDOC_CONTROL_RECORD
*&---------------------------------------------------------------------*
*&      Form  HEADER_SEGMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM header_segment  USING    p_ls_cobk TYPE cobk.

  CLEAR gv_idoc_header.
  gv_idoc_header-obj_type = p_objtyp.
  gv_idoc_header-obj_key = p_objkey.
  gv_idoc_header-obj_sys = p_objsys.
  gv_idoc_header-username = sy-uname.
  gv_idoc_header-header_txt = p_ls_cobk-bltxt.
*  gv_idoc_Header-comp_code = .
  gv_idoc_header-fisc_year = p_gjahr.
  gv_idoc_header-doc_date = p_bldat.
  gv_idoc_header-pstng_date = p_budat.
  gv_idoc_header-trans_date = p_trand.
  gv_idoc_header-doc_type = p_blart.
  gv_idoc_header-ref_doc_no = p_refdc.
*  gv_idoc_header-REF_KEY1 = p_refdc.
ENDFORM.                    " HEADER_SEGMENT
*&---------------------------------------------------------------------*
*&      Form  GL_LINEITEM_SEGMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gl_lineitem_segment  TABLES   p_lt_bkpf STRUCTURE bkpf
                                   p_lt_aufk STRUCTURE aufk
                           USING   p_ls_cobk TYPE cobk
                                   p_ls_coep LIKE ty_coep
                                   p_item_no TYPE i.
  DATA: lv_hkontex TYPE zhkontexp,
        lv_awkey TYPE bkpf-awkey,
        ls_aufk TYPE aufk,
        ls_bkpf TYPE bkpf,
        lv_network1(20),
        lv_network2(20),
        lv_zbukr TYPE bukrs,
        lv_costcenter TYPE kostl,
        lv_orderid TYPE aufnr,
        lv_wbs_element TYPE ps_posid,
        lv_network TYPE nplnr,
        lv_routing_no TYPE co_aufpl,
        lv_rfcdest TYPE rfcdest.

  CLEAR: gv_idoc_curr,
         gv_idoc_line.

  lv_rfcdest = gv_idoc_control-rcvprn.

  READ TABLE p_lt_aufk INTO ls_aufk WITH KEY objnr = p_ls_coep-objnr.
  lv_zbukr = ls_aufk-user1.
  SELECT SINGLE zhkontexp INTO lv_hkontex FROM zfi_ic_acct_xref
                                 WHERE appl_area = 'FITV'
                                   AND zbukr = lv_zbukr
                                   AND sbukr = p_ls_cobk-refbk "SDP53808
                                   AND hkont = p_ls_coep-kstar.
  lv_awkey = p_ls_cobk-refbn.
  READ TABLE p_lt_bkpf INTO ls_bkpf WITH KEY bukrs = p_ls_cobk-refbk
                                             gjahr = p_ls_cobk-gjahr
                                             blart = p_ls_cobk-blart
                                             awkey = lv_awkey.
  gv_idoc_line-itemno_acc   = p_item_no.
  gv_idoc_line-gl_account  = lv_hkontex.
  gv_idoc_line-comp_code   = ls_aufk-user1.
  gv_idoc_line-pstng_date  = p_budat.
  gv_idoc_line-doc_type    = p_blart.
  gv_idoc_line-fisc_year   = p_gjahr.
  gv_idoc_line-fis_period  = p_monat.
  gv_idoc_line-item_text   = p_ls_coep-sgtxt.
  CONCATENATE ls_bkpf-belnr ls_bkpf-bukrs INTO gv_idoc_line-ref_key_1.
  IF ls_aufk-user2 = 'CC'.
    lv_costcenter = ls_aufk-user3.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_costcenter
      IMPORTING
        output = lv_costcenter.

    gv_idoc_line-costcenter = lv_costcenter. "ls_aufk-user3.
  ENDIF.
  IF ls_aufk-user2 = 'IO'.
    lv_orderid = ls_aufk-user3.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_orderid
      IMPORTING
        output = lv_orderid.
    gv_idoc_line-orderid = lv_orderid. "ls_aufk-user3.
  ENDIF.
  IF ls_aufk-user2 = 'PR'.
    lv_wbs_element = ls_aufk-user3.
*    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
*      EXPORTING
*        input  = lv_wbs_element
*      IMPORTING
*        output = lv_wbs_element.
    CALL FUNCTION 'ZCONVERSION_EXIT_ABPSN_INPUT'
    DESTINATION lv_rfcdest
      EXPORTING
        input         = lv_wbs_element
      IMPORTING
        OUTPUT        = lv_wbs_element.
    gv_idoc_line-wbs_element = lv_wbs_element. "ls_aufk-user3.
  ENDIF.
  IF ls_aufk-user2 = 'NA'.
    SPLIT ls_aufk-user3 AT '.' INTO lv_network1 lv_network2.
    lv_network = lv_network1.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_network
      IMPORTING
        output = lv_network.
    gv_idoc_line-network = lv_network. " lv_network1.
    CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
      EXPORTING
        input  = lv_network2
      IMPORTING
        output = lv_network2.

*     lv_routing_no = lv_network2.
    gv_idoc_line-routing_no = space. "lv_routing_no. "lv_network2.
    gv_idoc_line-activity = lv_network2.
  ENDIF.
  APPEND gv_idoc_line TO gt_idoc_line.
************currency line item
  gv_idoc_curr-itemno_acc = p_item_no.
  gv_idoc_curr-curr_type = space.
  gv_idoc_curr-currency = p_ls_coep-twaer. "'CAD'.
  gv_idoc_curr-currency_iso = p_ls_coep-twaer. "'CAD'.
  gv_idoc_curr-amt_doccur = p_ls_coep-wtgbtr.
  gv_idoc_curr-exch_rate = space.
  gv_idoc_curr-exch_rate_v = space.
  APPEND gv_idoc_curr TO gt_idoc_curr.

ENDFORM.                    " GL_LINEITEM_SEGMENT
*&---------------------------------------------------------------------*
*&      Form  CURR_AMOUNT_SEGMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_COEP  text
*----------------------------------------------------------------------*
FORM curr_amount_segment  USING p_ls_coep LIKE ty_coep
                                p_item_no TYPE i.
  CLEAR gv_idoc_curr.
  gv_idoc_curr-itemno_acc = p_item_no.
  gv_idoc_curr-curr_type = space.
  gv_idoc_curr-currency = p_ls_coep-twaer. "'CAD'.
  gv_idoc_curr-currency_iso = p_ls_coep-twaer. "'CAD'.
  gv_idoc_curr-amt_doccur = p_ls_coep-wtgbtr.
  gv_idoc_curr-exch_rate = space.
  gv_idoc_curr-exch_rate_v = space.
ENDFORM.                    " CURR_AMOUNT_SEGMENT
*&---------------------------------------------------------------------*
*&      Form  GL_LINEITEMTOTAL_SEGMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gl_lineitemtotal_segment   TABLES p_lt_bkpf STRUCTURE bkpf
                                       p_lt_aufk STRUCTURE aufk
                                 USING p_ls_cobk TYPE cobk
                                       p_ls_coep TYPE coep
                                       p_wtgbtr TYPE coep-wtgbtr
                                       p_item_no TYPE i.
  DATA: lv_hkontic TYPE zhkontic,
        lv_vbund   TYPE vbund,               "SDP53808
        lv_awkey TYPE bkpf-awkey,
        ls_aufk TYPE aufk,
        ls_bkpf TYPE bkpf,
        lv_network1(20),
        lv_network2(20),
        lv_zbukr TYPE bukrs.

  CLEAR: gv_idoc_curr,
         gv_idoc_line.

  READ TABLE p_lt_aufk INTO ls_aufk WITH KEY objnr = p_ls_coep-objnr.
  lv_zbukr = ls_aufk-user1.
  SELECT SINGLE zhkontic vbund INTO (lv_hkontic, lv_vbund)
                                  FROM zfi_ic_acct_xref
                                 WHERE appl_area = 'FITV'
                                  AND zbukr = lv_zbukr
                                  AND sbukr = p_ls_cobk-refbk "SDP53808
                                  AND hkont = p_ls_coep-kstar.
  lv_awkey = p_ls_cobk-refbn.
  READ TABLE p_lt_bkpf INTO ls_bkpf WITH KEY bukrs = p_ls_cobk-refbk
                                             gjahr = p_ls_cobk-gjahr
                                             blart = p_ls_cobk-blart
                                             awkey = lv_awkey.
  gv_idoc_line-itemno_acc   = p_item_no.
  gv_idoc_line-gl_account  = lv_hkontic.
  gv_idoc_line-comp_code   = ls_aufk-user1.
  gv_idoc_line-pstng_date  = p_budat.
  gv_idoc_line-doc_type    = p_blart.
  gv_idoc_line-fisc_year   = p_gjahr.
  gv_idoc_line-fis_period  = p_monat.
  gv_idoc_line-item_text   = 'IC FI-Intercompany Billing'.
  CONCATENATE ls_bkpf-belnr ls_bkpf-bukrs INTO gv_idoc_line-ref_key_1.
  gv_idoc_line-ref_key_2 = lv_vbund.                          "SDP53808
  gv_idoc_line-costcenter = space.
  gv_idoc_line-orderid = space.
  gv_idoc_line-wbs_element = space.
  gv_idoc_line-network = space.
  gv_idoc_line-routing_no = space.
  APPEND gv_idoc_line TO gt_idoc_line.
************currency line item
  p_wtgbtr = p_wtgbtr * -1. "make it credit amount
  gv_idoc_curr-itemno_acc = p_item_no.
  gv_idoc_curr-curr_type = space.
  gv_idoc_curr-currency = p_ls_coep-twaer. "'CAD'.
  gv_idoc_curr-currency_iso = p_ls_coep-twaer. "'CAD'.
  gv_idoc_curr-amt_doccur = p_wtgbtr.
  gv_idoc_curr-exch_rate = space.
  gv_idoc_curr-exch_rate_v = space.
  APPEND gv_idoc_curr TO gt_idoc_curr.

ENDFORM.                    " GL_LINEITEMTOTAL_SEGMENT
*&---------------------------------------------------------------------*
*&      Form  IDOC_SEGMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM idoc_segments .

  CLEAR: gs_int_edidd,
         gt_int_edidd.
*Header segment
  gs_int_edidd-segnam = 'E1BPACHE08'.
  gs_int_edidd-sdata = gv_idoc_header.
  APPEND gs_int_edidd TO gt_int_edidd.
*Line items
  LOOP AT gt_idoc_line INTO gv_idoc_line.
    gs_int_edidd-segnam = 'E1BPACGL08'.
    gs_int_edidd-sdata = gv_idoc_line.
    APPEND gs_int_edidd TO gt_int_edidd.
  ENDLOOP.
*Currency items
  LOOP AT gt_idoc_curr INTO gv_idoc_curr.
    gs_int_edidd-segnam = 'E1BPACCR08'.
    gs_int_edidd-sdata = gv_idoc_curr.
    APPEND gs_int_edidd TO gt_int_edidd.
  ENDLOOP.

ENDFORM.                    " IDOC_SEGMENTS
*&---------------------------------------------------------------------*
*&      Form  DISTRIBUTE_IDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM distribute_idoc .

  DATA: lt_idoc_control LIKE TABLE OF gv_idoc_control.

  CLEAR: gs_msg.
  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
    EXPORTING
      master_idoc_control            = gv_idoc_control
    TABLES
      communication_idoc_control     = lt_idoc_control
      master_idoc_data               = gt_int_edidd
    EXCEPTIONS
      error_in_idoc_control          = 1
      error_writing_idoc_status      = 2
      error_in_idoc_data             = 3
      sending_logical_system_unknown = 4
      OTHERS                         = 5.

  CASE sy-subrc.
    WHEN 0.
      COMMIT WORK.
      CALL FUNCTION 'DEQUEUE_ALL'.
    WHEN 1.
      ROLLBACK WORK.
      gs_msg-msg_text = 'Error_in_idoc_control'.
    WHEN 2.
      ROLLBACK WORK.
      gs_msg-msg_text = 'Error_writing_idoc_status'.
    WHEN 3.
      ROLLBACK WORK.
      gs_msg-msg_text = 'Error_in_idoc_data'.
    WHEN 4.
      ROLLBACK WORK.
      gs_msg-msg_text = 'Sending_logical_system_unknown'.
    WHEN OTHERS.
      ROLLBACK WORK.
      gs_msg-msg_text = 'Others'.
  ENDCASE.
  IF gs_msg-msg_text IS NOT INITIAL.
    APPEND gs_msg TO gt_msg.
  ENDIF.
ENDFORM.                    " DISTRIBUTE_IDOC
*&---------------------------------------------------------------------*
*&      Form  PRE_CHECK_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pre_check_validation .

  DATA: lv_rfcdest TYPE rfcdest,
        lt_return TYPE TABLE OF bapiret2,
        ls_return TYPE bapiret2,
        lt_idoc_curr TYPE TABLE OF bapiaccr08,
        ls_idoc_curr TYPE bapiaccr08,
        lt_idoc_line TYPE TABLE OF bapiacgl08,
        ls_idoc_line TYPE bapiacgl08,
        ls_idoc_header TYPE bapiache08.


  CLEAR: gv_error.

  LOOP AT gt_idoc_curr INTO gv_idoc_curr.
    ls_idoc_curr-itemno_acc    = gv_idoc_curr-itemno_acc.
    ls_idoc_curr-curr_type     = gv_idoc_curr-curr_type.
    ls_idoc_curr-currency      = gv_idoc_curr-currency.
    ls_idoc_curr-currency_iso  = gv_idoc_curr-currency_iso.
    ls_idoc_curr-amt_doccur    = gv_idoc_curr-amt_doccur.
    ls_idoc_curr-exch_rate     = space.
    ls_idoc_curr-exch_rate_v   = space.
    APPEND ls_idoc_curr TO lt_idoc_curr.
  ENDLOOP.
  LOOP AT gt_idoc_line INTO gv_idoc_line.
    MOVE-CORRESPONDING gv_idoc_line TO ls_idoc_line.
    APPEND ls_idoc_line TO lt_idoc_line.
  ENDLOOP.
  MOVE-CORRESPONDING gv_idoc_header TO ls_idoc_header.

  lv_rfcdest = gv_idoc_control-rcvprn.

  CALL FUNCTION 'BAPI_ACC_GL_POSTING_CHECK'
    DESTINATION lv_rfcdest
    EXPORTING
      documentheader = ls_idoc_header "gv_idoc_header
    TABLES
      accountgl      = lt_idoc_line "gt_idoc_line
      currencyamount = lt_idoc_curr "gt_idoc_curr
      return         = lt_return
*     EXTENSION1     =
    .
  LOOP AT lt_return INTO ls_return.
    CLEAR: gs_msg.
    IF ls_return-type = 'E' OR
       ls_return-type = 'A'.
      IF ls_return-number <> '032' AND                    "SDP53808
         ls_return-number <> '609'.                       "SDP53808
      gs_msg-msgty = ls_return-type.
      gs_msg-msgid = ls_return-id.
      gs_msg-msgno = ls_return-number.
      CONCATENATE 'System:' lv_rfcdest '--' ls_return-message
                                    INTO gs_msg-msg_text.
      APPEND gs_msg TO gt_msg.
      gv_error = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " PRE_CHECK_VALIDATION
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CUSTOM_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM update_custom_table.

  DELETE ADJACENT DUPLICATES FROM gt_zfi_ic_documents
                            COMPARING kokrs belnr.
  INSERT zfi_ic_documents FROM TABLE gt_zfi_ic_documents
          ACCEPTING DUPLICATE KEYS.
  COMMIT WORK.

ENDFORM.                    " UPDATE_CUSTOM_TABLE
*&---------------------------------------------------------------------*
*&      Form  MAP_CUSTOM_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM map_custom_table  USING    p_ls_cobk TYPE cobk.

  DATA: ls_zfi_ic_documents TYPE zfi_ic_documents.

  ls_zfi_ic_documents-kokrs     = p_ls_cobk-kokrs.
  ls_zfi_ic_documents-belnr     = p_ls_cobk-belnr.
  ls_zfi_ic_documents-post_date = sy-datum.
  ls_zfi_ic_documents-post_time = sy-uzeit.
  APPEND ls_zfi_ic_documents TO gt_zfi_ic_documents.

ENDFORM.                    " MAP_CUSTOM_TABLE
