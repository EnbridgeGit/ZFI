*&---------------------------------------------------------------------*
*& Report  ZFAPI060_INVOICEINFO_EXTRACT (UG version)
*&
*&---------------------------------------------------------------------*
*&  Client:     Spectra Energy.
*&  Date:       September 2016
*&  Author:     Glenn Ymana
*&
*& This program will read specific SAP tables associated with Vendor
*& Invoice payment status, including VBSEGK, BSAK, BSEG, LFA1, PAYR
*& T042ZT, and T042U
*& Selected records will be written to an output extract file
*& All 3 ECCs will have the same copy of this program.
*& All 3 files from the ECCs will be merged and sent to InvoiceInfo
*& to be made available to all Vendors via the InvoiceInfo app
*&---------------------------------------------------------------------*
*& NOTE - any changes made to this program must be made to the other
*&        copies in the other ECCs
*&      - Do not overwrite the other copies as each has been modified
*&        slightly different to suit the other companies.
*&---------------------------------------------------------------------*
REPORT  zfapi060_invoiceinfo_extract.

TABLES: bsak, bsik, vbsegk, bkpf, lfa1, zpaymethod.

TYPES: BEGIN OF ty_output,
        status     TYPE string,   "(10)
        company    TYPE string,   "(4)
        pblkdesc   TYPE string,   "(20)
        vendorno   TYPE string,   "(10)
        clrdocno   TYPE string,   "(10)
        cleardate  TYPE string,   "(8)
        paymethod  TYPE string,   "(30)
        docno      TYPE string,   "(10)
        refno      TYPE string,   "(20)
        docdate    TYPE string,   "(8)
        invamt     TYPE string,   "(13)
        invcurr    TYPE string,   "(5)
        discamt    TYPE string,   "(13)
        payterms   TYPE string,   "(50)
        compcd     TYPE string,   "(4)
        withtaxamt TYPE string,   "(13)
        payee      TYPE string,   "(10)
        checkno    TYPE string,   "(13)
        paydate    TYPE string,   "(8)
        paycurr    TYPE string,   "(5)
        amtpaid    TYPE string,   "(13)
        encashdt   TYPE string,   "(8)
        invpayamt  TYPE string,   "(13)
        netduedate TYPE string,   "(8)
       END OF ty_output.

TYPES: BEGIN OF ty_vbsegk_output,
        lifnr        LIKE vbsegk-lifnr,
        text2        LIKE t042zt-text2,
        belnr        LIKE vbsegk-belnr,
        xblnr        LIKE bkpf-xblnr,
        zfbdt        LIKE vbsegk-zfbdt,
        zbd1t        LIKE vbsegk-zbd1t,
        zbd2t        LIKE vbsegk-zbd2t,
        shkzg        LIKE vbsegk-shkzg,
        wrbtr        LIKE vbsegk-wrbtr,
        waers        LIKE bkpf-waers,
        zlsch        LIKE vbsegk-zlsch,
        wskto        LIKE vbsegk-wskto,
        text1        LIKE t052u-text1,
        bukrs        LIKE vbsegk-bukrs,
        qbshb        LIKE vbsegk-qbshb,
        empfb        LIKE vbsegk-empfb,
        zlspr        LIKE vbsegk-zlspr,
       END OF ty_vbsegk_output.

TYPES: BEGIN OF ty_bsik_output,
        lifnr        LIKE bsik-lifnr,
        ebeln        LIKE bsik-ebeln,
        augbl        LIKE bsik-augbl,
        augdt        LIKE bsik-augdt,
        text2        LIKE t042zt-text2,
        belnr        LIKE bsik-belnr,
        zfbdt        LIKE bsik-zfbdt,
        zbd1t        LIKE bsik-zbd1t,
        zbd2t        LIKE bsik-zbd2t,
        xblnr        LIKE bsik-xblnr,
        bldat        LIKE bsik-bldat,
        shkzg        LIKE bsik-shkzg,
        wrbtr        LIKE bsik-wrbtr,
        waers        LIKE bsik-waers,
        zlsch        LIKE bsik-zlsch,
        wskto        LIKE bsik-wskto,
        text1        LIKE t052u-text1,
        bukrs        LIKE bsik-bukrs,
        qbshb        LIKE bsik-qbshb,
        empfb        LIKE bsik-empfb,
        zlspr        LIKE bsik-zlspr,
       END OF ty_bsik_output.

TYPES: BEGIN OF ty_bsak_output,
        lifnr        LIKE bsak-lifnr,
        ebeln        LIKE bsak-ebeln,
        augbl        LIKE bsak-augbl,
        augdt        LIKE bsak-augdt,
        text2        LIKE t042zt-text2,
        belnr        LIKE bsak-belnr,
        xblnr        LIKE bsak-xblnr,
        zfbdt        LIKE bsak-zfbdt,
        zbd1t        LIKE bsak-zbd1t,
        zbd2t        LIKE bsak-zbd2t,
        bldat        LIKE bsak-bldat,
        shkzg        LIKE bsak-shkzg,
        wrbtr        LIKE bsak-wrbtr,
        waers        LIKE bsak-waers,
        wskto        LIKE bsak-wskto,
        text1        LIKE t052u-text1,
        bukrs        LIKE bsak-bukrs,
        xzahl        LIKE bsak-xzahl,
        qbshb        LIKE bsak-qbshb,
        empfb        LIKE bsak-empfb,
        zlspr        LIKE bsak-zlspr,
        zlsch        LIKE bsak-zlsch,
        zterm        LIKE bsak-zterm,
        zaldt        LIKE payr-zaldt,
        chect        LIKE payr-chect,
        bancd        LIKE payr-bancd,
        rwbtr        LIKE payr-rwbtr,
        pwaers       LIKE payr-waers,
       END OF ty_bsak_output.

DATA: gt_output    TYPE TABLE OF ty_output,
      gs_output    TYPE ty_output,
      lv_string    TYPE string,
      lv_crlf      TYPE char2.

DATA: gt_vbsegk_out     TYPE TABLE OF ty_vbsegk_output WITH HEADER LINE,
      gs_vbsegk_out     TYPE ty_vbsegk_output,
      gt_bsik_out       TYPE TABLE OF ty_bsik_output WITH HEADER LINE,
      gs_bsik_out       TYPE ty_bsik_output,
      gt_bsak_out       TYPE TABLE OF ty_bsak_output WITH HEADER LINE,
      gs_bsak_out       TYPE ty_bsak_output,
      gt_bsak_out2      TYPE TABLE OF ty_bsak_output WITH HEADER LINE,
      gs_bsak_out2      TYPE ty_bsak_output,
      wa_frmdate        LIKE sy-datum,
      wa_company        LIKE gs_output-company,
      wa_text1          LIKE t052u-text1,
      wa_text2          LIKE t042zt-text2,
      wa_zaldt          LIKE payr-zaldt,
      wa_chect          LIKE payr-chect,
      wa_bancd          LIKE payr-bancd,
      wa_waers          LIKE payr-waers,
      wa_rwbtr          LIKE payr-rwbtr,
      wa_prev_clrno     LIKE bsak-augbl,
      wa_prev_amt       LIKE bsak-wrbtr,
      wa_invpayamt      LIKE bsak-wrbtr,
      wa_invamt         LIKE bsak-wrbtr,
      wa_prev_curr      LIKE bsak-waers,
      wa_prev_text2     LIKE t042zt-text2,
      wa_netduedt       LIKE vbsegk-zfbdt,
      wa_netduedtcalc   LIKE vbsegk-zfbdt,
      wa_currdate       LIKE sy-datum.

CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs  FOR  vbsegk-bukrs OBLIGATORY.
PARAMETERS:     p_frmdat TYPE bsak-bldat OBLIGATORY,
                p_todat  TYPE bsak-bldat OBLIGATORY DEFAULT sy-datum.
SELECT-OPTIONS: s_blart  FOR  bkpf-blart,
                s_ktokk  FOR  lfa1-ktokk.

SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X'  USER-COMMAND cmd,
            p_sfile   LIKE        rfpdo-rfbifile MODIF ID srv,
            r_local   RADIOBUTTON GROUP rad1,
            p_lfile   TYPE        rfpdo-rfbifile DEFAULT 'H:\' MODIF ID lcl.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  DATA: wa_frmyr(4)  TYPE c,
        wa_frmmth(2) TYPE c.

  wa_frmyr        = sy-datum(4) - 3.
  wa_frmmth       = sy-datum+4(2).
  wa_frmdate(4)   = wa_frmyr.
  wa_frmdate+4(2) = wa_frmmth.
  wa_frmdate+6(2) = sy-datum+6(2).
  p_frmdat        = wa_frmdate.
  wa_currdate     = sy-datum.      "Change from system date for testing

* Set Company
  CASE sy-sysid.
    WHEN 'D30' OR 'D22' OR 'P01' OR 'S01' OR 'S02' OR 'Q02'.
      wa_company = 'UGL'.
    WHEN 'C11' OR 'Q11' OR 'P11' OR 'SBX' OR 'S11' OR 'Q12'.
      wa_company = 'SETW'.
    WHEN 'DEC' OR 'QEC' OR 'PEC'.
      wa_company = 'US'.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.


  PERFORM  f_toggle_functionality.

START-OF-SELECTION.

  CLEAR: gt_output,
         gs_output.

  PERFORM get_vbsegk_data.
  PERFORM get_bsik_data.
  PERFORM get_bsak_data.

  IF gt_output[] IS INITIAL.
    WRITE : / 'no data to output.'.
  ELSE.
    PERFORM output_data.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_VBSEGK_DATA
*&---------------------------------------------------------------------*

FORM get_vbsegk_data .

  SELECT a~lifnr a~belnr e~xblnr a~zfbdt a~zbd1t a~zbd2t a~wrbtr e~waers
         a~shkzg a~zlsch a~wskto c~text1 a~bukrs a~qbshb a~empfb a~zlspr
    INTO CORRESPONDING FIELDS OF TABLE gt_vbsegk_out
*    FROM vbsegk AS a INNER JOIN t042zt AS b ON a~zlsch EQ b~zlsch
    FROM vbsegk AS a INNER JOIN t052u AS c ON a~zterm EQ c~zterm
                     INNER JOIN lfa1 AS d ON a~lifnr EQ d~lifnr
                     INNER JOIN bkpf as e ON ( a~bukrs EQ e~bukrs
                                          AND a~belnr EQ e~belnr
                                          AND a~gjahr EQ e~gjahr )
    WHERE a~bukrs IN s_bukrs
      AND a~zfbdt BETWEEN p_frmdat AND p_todat
      AND d~ktokk NOT IN s_ktokk.

  IF gt_vbsegk_out[] IS INITIAL.
    WRITE : / 'No VBSEGK data to process...'.
    RETURN.
  ENDIF.

  LOOP AT gt_vbsegk_out INTO gs_vbsegk_out.
    CLEAR: gs_output, wa_invamt, wa_invpayamt, wa_netduedt,
           wa_netduedtcalc.
    gs_output-status = 'In-Process'.
    gs_output-company = wa_company.

    CASE gs_vbsegk_out-zlspr.
      WHEN 'A' OR 'B'.
        gs_output-pblkdesc = 'Blocked for Payment'.
      WHEN 'R'.
        gs_output-pblkdesc = 'Blocked by PO'.
      WHEN OTHERS.
        gs_output-pblkdesc = ' '.
    ENDCASE.

    SELECT * FROM zpaymethod
     WHERE zlsch EQ gs_vbsegk_out-zlsch.

      IF sy-subrc EQ 0.
         gs_vbsegk_out-text2 = zpaymethod-text.
      ENDIF.
    ENDSELECT.

    wa_invamt = gs_vbsegk_out-wrbtr +
                gs_vbsegk_out-qbshb.
    wa_invpayamt = gs_vbsegk_out-wrbtr -
                   gs_vbsegk_out-wskto.
    wa_netduedtcalc = gs_vbsegk_out-zfbdt +
                      gs_vbsegk_out-zbd1t.
    IF gs_vbsegk_out-zbd2t EQ 0.
       wa_netduedt = wa_netduedtcalc.
    ELSE.
       IF wa_currdate LE wa_netduedtcalc.
          wa_netduedt = wa_netduedtcalc.
       ELSE.
          wa_netduedt = gs_vbsegk_out-zfbdt +
                       gs_vbsegk_out-zbd2t.
       ENDIF.
    ENDIF.

    IF gs_vbsegk_out-shkzg EQ 'S'.
       wa_invpayamt = wa_invpayamt * -1.
       wa_invamt = wa_invamt * -1.
    ENDIF.

    gs_output-vendorno   = gs_vbsegk_out-lifnr.
    gs_output-paymethod  = gs_vbsegk_out-text2.
    gs_output-docno      = gs_vbsegk_out-belnr.
    gs_output-refno      = gs_vbsegk_out-xblnr.
    gs_output-docdate    = gs_vbsegk_out-zfbdt.
    gs_output-invamt     = wa_invamt.
    gs_output-invpayamt  = wa_invpayamt.
    gs_output-netduedate = wa_netduedt.
    gs_output-invcurr    = gs_vbsegk_out-waers.
    gs_output-discamt    = gs_vbsegk_out-wskto.
    gs_output-payterms   = gs_vbsegk_out-text1.
    gs_output-compcd     = gs_vbsegk_out-bukrs.
    gs_output-withtaxamt = gs_vbsegk_out-qbshb.
    gs_output-payee      = gs_vbsegk_out-empfb.

    APPEND gs_output TO gt_output.
  ENDLOOP.


ENDFORM.                    " GET_VBSEGK_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_BSIK_DATA
*&---------------------------------------------------------------------*
FORM get_bsik_data.

  SELECT a~lifnr a~ebeln a~augbl a~augdt a~belnr a~xblnr a~zfbdt a~zbd1t
         a~zbd2t a~zterm a~bldat a~shkzg a~wrbtr a~waers a~zlsch a~wskto
         c~text1 a~bukrs a~qbshb a~empfb a~zlspr
    INTO CORRESPONDING FIELDS OF TABLE gt_bsik_out
*    FROM bsik AS a INNER JOIN t042zt AS b ON a~zlsch EQ b~zlsch
    FROM bsik AS a INNER JOIN t052u AS c ON a~zterm EQ c~zterm
                   INNER JOIN lfa1 AS d ON a~lifnr EQ d~lifnr
    WHERE a~bukrs IN s_bukrs
      AND a~blart IN s_blart
      AND a~bldat BETWEEN p_frmdat AND p_todat
      AND d~ktokk NOT IN s_ktokk.

  IF gt_bsik_out[] IS INITIAL.
    WRITE : / 'No BSIK data to process...'.
    RETURN.
  ENDIF.

  LOOP AT gt_bsik_out INTO gs_bsik_out.
    CLEAR: gs_output, wa_invamt, wa_invpayamt, wa_netduedt,
           wa_netduedtcalc.
    gs_output-status = 'Approved'.
    gs_output-company = wa_company.

    CASE gs_bsik_out-zlspr.
      WHEN 'A' OR 'B'.
        gs_output-pblkdesc = 'Blocked for Payment'.
      WHEN 'R'.
        gs_output-pblkdesc = 'Blocked by PO'.
      WHEN OTHERS.
        gs_output-pblkdesc = ' '.
    ENDCASE.

    SELECT * FROM zpaymethod
     WHERE zlsch EQ gs_bsik_out-zlsch.

      IF sy-subrc EQ 0.
         gs_bsik_out-text2 = zpaymethod-text.
      ENDIF.
    ENDSELECT.

    wa_invamt = gs_bsik_out-wrbtr +
                gs_bsik_out-qbshb.
    wa_invpayamt = gs_bsik_out-wrbtr -
                   gs_bsik_out-wskto.
    wa_netduedtcalc = gs_bsik_out-zfbdt +
                      gs_bsik_out-zbd1t.
    IF gs_bsik_out-zbd2t EQ 0.
       wa_netduedt = wa_netduedtcalc.
    ELSE.
       IF wa_currdate LE wa_netduedtcalc.
          wa_netduedt = wa_netduedtcalc.
       ELSE.
          wa_netduedt = gs_bsik_out-zfbdt +
                       gs_bsik_out-zbd2t.
       ENDIF.
    ENDIF.

    IF gs_bsik_out-shkzg EQ 'S'.
       wa_invpayamt = wa_invpayamt * -1.
       wa_invamt = wa_invamt * -1.
    ENDIF.

    gs_output-vendorno   = gs_bsik_out-lifnr.
    gs_output-clrdocno   = gs_bsik_out-augbl.
    gs_output-cleardate = gs_bsik_out-augdt.
    gs_output-paymethod  = gs_bsik_out-text2.
    gs_output-docno      = gs_bsik_out-belnr.
    gs_output-refno      = gs_bsik_out-xblnr.
    gs_output-docdate    = gs_bsik_out-bldat.
    gs_output-invamt     = wa_invamt.
    gs_output-invpayamt  = wa_invpayamt.
    gs_output-netduedate = wa_netduedt.
    gs_output-invcurr    = gs_bsik_out-waers.
    gs_output-discamt    = gs_bsik_out-wskto.
    gs_output-payterms   = gs_bsik_out-text1.
    gs_output-compcd     = gs_bsik_out-bukrs.
    gs_output-withtaxamt = gs_bsik_out-qbshb.
    gs_output-payee      = gs_bsik_out-empfb.
    gs_output-paydate    = gs_bsik_out-augdt.

    APPEND gs_output TO gt_output.
  ENDLOOP.

ENDFORM.                    "get_bsik_data

*&---------------------------------------------------------------------*
*&      Form  GET_BSAK_DATA
*&---------------------------------------------------------------------*

FORM get_bsak_data.

  SELECT lifnr ebeln augbl augdt belnr xblnr zfbdt zbd1t zbd2t shkzg
         bldat wrbtr zterm zlsch waers wskto bukrs xzahl qbshb empfb
         zlspr
    INTO CORRESPONDING FIELDS OF TABLE gt_bsak_out
    FROM bsak
   WHERE bukrs IN s_bukrs
     AND blart IN s_blart
     AND ( bldat GE p_frmdat
     AND bldat LE p_todat ).

  IF gt_bsak_out[] IS INITIAL.
    WRITE : / 'No BSAK data to process...'.
    RETURN.
  ENDIF.

  LOOP AT gt_bsak_out INTO gs_bsak_out.

    CLEAR: wa_text1, wa_text2, wa_zaldt, wa_chect,
           wa_bancd, wa_waers, wa_rwbtr.

    SELECT * FROM lfa1
     WHERE lifnr EQ gs_bsak_out-lifnr
       AND ktokk NOT IN s_ktokk.
    ENDSELECT.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING gs_bsak_out TO gs_bsak_out2.

*    SELECT text2 FROM t042zt
*      INTO wa_text2
*     WHERE zlsch EQ gs_bsak_out-zlsch.
*    ENDSELECT.
*
*    IF sy-subrc EQ 0.
*      MOVE wa_text2 TO gs_bsak_out2-text2.
*    ENDIF.

    SELECT * FROM zpaymethod
     WHERE zlsch EQ gs_bsak_out-zlsch.

      IF sy-subrc EQ 0.
         gs_bsak_out2-text2 = zpaymethod-text.
      ENDIF.
    ENDSELECT.

    SELECT text1 FROM t052u
      INTO wa_text1
     WHERE zterm EQ gs_bsak_out-zterm.
    ENDSELECT.

    IF sy-subrc EQ 0.
      MOVE wa_text1 TO gs_bsak_out2-text1.
    ENDIF.

    SELECT zaldt chect bancd waers rwbtr
      FROM payr
      INTO (wa_zaldt, wa_chect, wa_bancd, wa_waers, wa_rwbtr)
     WHERE zbukr EQ gs_bsak_out-bukrs
       AND vblnr EQ gs_bsak_out-augbl.

      IF sy-subrc EQ 0.
        MOVE wa_zaldt TO gs_bsak_out2-zaldt.
        MOVE wa_chect TO gs_bsak_out2-chect.
        MOVE wa_bancd TO gs_bsak_out2-bancd.
        MOVE wa_waers TO gs_bsak_out2-pwaers.
        MOVE wa_rwbtr TO gs_bsak_out2-rwbtr.
      ENDIF.
    ENDSELECT.

    APPEND gs_bsak_out2 TO gt_bsak_out2.

  ENDLOOP.

  SORT gt_bsak_out2 BY augbl ASCENDING bukrs ASCENDING xzahl DESCENDING.

  LOOP AT gt_bsak_out2 INTO gs_bsak_out2.
    CLEAR: gs_output, wa_invamt, wa_invpayamt, wa_netduedt,
           wa_netduedtcalc.

* If there is a change in document number, save it.

    IF gs_bsak_out2-augbl <> wa_prev_clrno.
      MOVE gs_bsak_out2-augbl TO wa_prev_clrno.
      CLEAR: wa_prev_amt, wa_prev_curr, wa_prev_text2.
    ENDIF.

* If BSAK record is a payment record, save the amount and curr

    IF gs_bsak_out2-xzahl EQ 'X'.
*     If PAYR check payment is present, use PAYR amt and curr
*     Else use bsak amt and curr
      IF gs_bsak_out2-chect IS INITIAL.
        MOVE gs_bsak_out2-wrbtr TO wa_prev_amt.
        MOVE gs_bsak_out2-waers TO wa_prev_curr.
      ELSE.
        MOVE gs_bsak_out2-rwbtr TO wa_prev_amt.
        MOVE gs_bsak_out2-pwaers TO wa_prev_curr.
      ENDIF.
*      MOVE gs_bsak_out2-zterm TO wa_prev_paymethod.
      MOVE gs_bsak_out2-text2 TO wa_prev_text2.
*     Bypass payment records. Do not write to file.
      CONTINUE.
    ENDIF.

    gs_output-status = 'Paid'.
    gs_output-company = wa_company.

    CASE gs_bsak_out2-zlspr.
      WHEN 'A' OR 'B'.
        gs_output-pblkdesc = 'Blocked for Payment'.
      WHEN 'R'.
        gs_output-pblkdesc = 'Blocked by PO'.
      WHEN OTHERS.
        gs_output-pblkdesc = ' '.
    ENDCASE.

    wa_invamt = gs_bsak_out2-wrbtr +
                gs_bsak_out2-qbshb.
    wa_invpayamt = gs_bsak_out2-wrbtr -
                   gs_bsak_out2-wskto.
    wa_netduedtcalc = gs_bsak_out2-zfbdt +
                      gs_bsak_out2-zbd1t.
    IF gs_bsak_out2-zbd2t EQ 0.
       wa_netduedt = wa_netduedtcalc.
    ELSE.
       IF wa_currdate LE wa_netduedtcalc.
*       IF sy-datum LE wa_netduedtcalc.
          wa_netduedt = wa_netduedtcalc.
       ELSE.
          wa_netduedt = gs_bsak_out2-zfbdt +
                        gs_bsak_out2-zbd2t.
       ENDIF.
    ENDIF.

    IF gs_bsak_out2-shkzg EQ 'S'.
       wa_invpayamt = wa_invpayamt * -1.
       wa_invamt = wa_invamt * -1.
    ENDIF.

    gs_output-vendorno   = gs_bsak_out2-lifnr.
    gs_output-clrdocno   = gs_bsak_out2-augbl.
    gs_output-cleardate = gs_bsak_out2-augdt.
    gs_output-paymethod  = wa_prev_text2.
    gs_output-docno      = gs_bsak_out2-belnr.
    gs_output-refno      = gs_bsak_out2-xblnr.
    gs_output-docdate    = gs_bsak_out2-bldat.
    gs_output-invamt     = wa_invamt.
    gs_output-invpayamt  = wa_invpayamt.
    gs_output-netduedate = wa_netduedt.
    gs_output-invcurr    = gs_bsak_out2-waers.
    gs_output-discamt    = gs_bsak_out2-wskto.
    gs_output-payterms   = gs_bsak_out2-text1.
    gs_output-compcd     = gs_bsak_out2-bukrs.
    gs_output-withtaxamt = gs_bsak_out2-qbshb.
    gs_output-payee      = gs_bsak_out2-empfb.
    gs_output-checkno    = gs_bsak_out2-chect.

    IF gs_bsak_out2-chect IS NOT INITIAL.
      gs_output-paydate  = gs_bsak_out2-zaldt.
      gs_output-encashdt = gs_bsak_out2-bancd.
    ELSE.
      gs_output-paydate  = gs_bsak_out2-augdt.
      gs_output-encashdt = ' '.
    ENDIF.

    gs_output-paycurr  = wa_prev_curr.
    gs_output-amtpaid  = wa_prev_amt.

    IF wa_prev_amt <> 0.
       APPEND gs_output TO gt_output.
    ENDIF.

  ENDLOOP.
ENDFORM.                    "get_bsak_data

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_DATA
*&---------------------------------------------------------------------*

FORM output_data .
  IF r_server = 'X'.
    PERFORM download_to_server.
  ELSE.
    PERFORM download_to_pc.
  ENDIF.
ENDFORM.                    " OUTPUT_DATA
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
*&      Form  DOWNLOAD_TO_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_to_server .
*Writing CDN file
  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open file for output.'.
  ELSE.
    LOOP AT gt_output INTO gs_output.
      CONCATENATE gs_output-status
                  gs_output-company
                  gs_output-pblkdesc
                  gs_output-vendorno
                  gs_output-clrdocno
                  gs_output-cleardate
                  gs_output-paymethod
                  gs_output-docno
                  gs_output-refno
                  gs_output-docdate
                  gs_output-invamt
                  gs_output-invcurr
                  gs_output-discamt
                  gs_output-payterms
                  gs_output-compcd
                  gs_output-withtaxamt
                  gs_output-payee
                  gs_output-checkno
                  gs_output-paydate
                  gs_output-paycurr
                  gs_output-amtpaid
                  gs_output-encashdt
                  gs_output-invpayamt
                  gs_output-netduedate
             INTO lv_string
        SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

      TRANSFER lv_string TO p_sfile.
    ENDLOOP.

    CLOSE DATASET p_sfile.
    WRITE: / 'File is downloaded', p_sfile.
  ENDIF.
ENDFORM.                    " DOWNLOAD_TO_SERVER
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_TO_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_to_pc .
  DATA: lv_filename TYPE string.
*Download Canadian file
  MOVE p_lfile TO lv_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
      write_field_separator     = 'X'
      trunc_trailing_blanks_eol = space "'X'
    CHANGING
      data_tab                  = gt_output
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24.
  IF sy-subrc <> 0.
    WRITE: / 'Error with downloading file at PC ', sy-subrc.
  ELSE.
    WRITE: / 'Successfully created at ', p_lfile.
  ENDIF.
ENDFORM.                    " DOWNLOAD_TO_PC
