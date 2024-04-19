*&---------------------------------------------------------------------*
*& Report  ZFARI001_CONTRAX_PAYMENTS
*&
*&---------------------------------------------------------------------*
*&**********************************************************************
*  Author:      Mohammad T. Khan                                       *
*  Date:        July, 2011.                                            *
*  Issue Log:   TR804                                                  *
*  Description:                                                        *
*     - This program will receive the fle from  customer               *
*       payments and translate into an FI/AR BDC psoting.              *
*                                                                      *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*& 2013/04/04 - Add 2 new AR account fields to selection screen and    *
*& GYMANA       modify screen 301 to use one of the 2 new fields       *
*& SDP42984     depending on customer type.                            *
*&                                                                     *
*& 2013/03/11 - Moved the session names to the selection screen to     *
*& GYMANA       allow fexibility in changing the names.                *
*& SDP42984                                                            *
*&                                                                     *
*& 2013/01/30 - Corrected a bug where the posting key was incorrectly  *
*& GYMANA       set for amounts less than $1.00 (incorrect data type   *
*& SDP39341     set for payment amounts). Also made a change to include*
*&              the currency type on the batch session for Canadian    *
*&              amounts.                                               *
*& 12/11/2018 AKMADASU  CHG0115682 Input Contrax Interface Payment File*
*&                                changes                              *
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 02-FEB-2019  KBANERJEE   D30K929670 CHG0115682-CAD Sessions not and  *
*                          D30K929660 Update in source code logic      *
* 06-MAR-2019  KBANERJEE   D30K929683 CHG0115683-Update source code    *
*                                     logic to incorporate any         *
*                                     variants  names                  *
* 28-MAR-2019  KBANERJEE   D30K929746 CHG0141661-Changes to Contrax    *
*                                     file interface for dates         *
* 15-JUN-2022  DADIM       D30K932211 CHG0252113-Changes done to post  *
*                                    negative amount records w/o errors*                                                                     *
************************************************************************
REPORT  zfari001_contrax_payments.

TABLES zfc07. "Contrax Payment Acccount Lookup

*Input File
DATA: BEGIN OF editrec OCCURS 0,
        payment_type(8)    TYPE c,
        "filler1            TYPE c,
        payment_amt(16)    TYPE c,
        "filler2(2)         TYPE c,
**-- START OF CHANGES BY AKMADASU FOR CHG0115682
*        CURRENCY_CODE(7)   TYPE C,
*        FILLER3            TYPE C,
*        PAYMENT_DATE(8)    TYPE C,
*        FILLER4(5)         TYPE C,
        currency_code(8)   TYPE c,
        "filler3            TYPE c,
        payment_date(12)   TYPE c,
        "filler4(5)         TYPE c,
        payment_rdate(12)  TYPE c,
**-- END OF CHANGES BY AKMADASU FOR CHG0115682
        exchange_rate(16)  TYPE c,
        source_code(5)     TYPE c,
**-- START OF CHANGES BY AKMADASU FOR CHG0115682
        billing_id(8)      TYPE c,
*        CUSTOMER_NUM(8)    TYPE C,
        customer_num(10)   TYPE c,
**-- END OF CHANGES BY AKMADASU FOR CHG0115682
      END OF editrec.

TYPES: BEGIN OF line,
        bldat   LIKE bbkpf-bldat,
        budat   LIKE bbkpf-budat,
        blart   LIKE bbkpf-blart,
        bukrs   LIKE bbkpf-bukrs,
        waers   LIKE bbkpf-waers,
        monat   LIKE bbkpf-monat,
        xblnr   LIKE bbkpf-xblnr,
        bktxt   LIKE bbkpf-bktxt,
        newbs   LIKE zbseg-newbs,
        newko   LIKE zbseg-newko,
        wrbtr   LIKE zbseg-wrbtr,
        zuonr   LIKE bseg-zuonr, " ADDED BY AKMADASU  CHG0115682
        sgtxt   LIKE zbseg-sgtxt,
        hkont   LIKE zbseg-hkont,                           "SDP42984
       END OF line,
**-- START OF CHANGES BY AKMADASU FOR CHG0115682
       BEGIN OF gty_source_code,
         srccode TYPE z_srccode,
         END OF gty_source_code,
       BEGIN OF gty_srccode.
        INCLUDE TYPE zfis_src_code.
TYPES:END OF gty_srccode.
DATA:gt_source_code TYPE STANDARD TABLE OF gty_source_code INITIAL SIZE 0,
     gs_source_code TYPE                   gty_source_code,
     gt_r_distri    TYPE STANDARD TABLE OF gty_srccode INITIAL SIZE 0,
     gt_r_st        TYPE STANDARD TABLE OF gty_srccode INITIAL SIZE 0.
**-- END OF CHANGES BY AKMADASU FOR CHG0115682


DATA: cdn_ctrx TYPE STANDARD TABLE OF line WITH HEADER LINE.

DATA: usa_ctrx TYPE STANDARD TABLE OF line WITH HEADER LINE.

DATA: wrk_symbolic(4) TYPE c VALUE '$sys',
      w_line_item_count TYPE i VALUE 0,
      w_payment_amt(16) TYPE p DECIMALS 2.                  "SDP39341

* batch input data
DATA: BEGIN OF bdcdata OCCURS 500.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
* transaction code for batch input
CONSTANTS: g_tcode LIKE tstc-tcode VALUE 'FB01'.
* BDC Session Name
*DATA: BDC_SESSION_CAD(12) VALUE 'CAD_CTR_PYMT',
*      BDC_SESSION_USD(12) VALUE 'USD_CTR_PYMT'.

************************************************************************
*                    SELECTION SCREEN                                  *
************************************************************************


SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP 1.

PARAMETER:  infile LIKE filename-fileextern
              DEFAULT '/usr/sap/interfaces/$sys/CONTRAX/zbis255.chk'.
*              DEFAULT '/usr/sap/interfaces/$sys/IFFI055/zbis255.txt'.
SELECTION-SCREEN SKIP 1.
PARAMETER:
 p_xblnr  LIKE  bbkpf-xblnr DEFAULT 'CONTRAX',
 p_bktxt  LIKE  bbkpf-bktxt DEFAULT 'Contrax Payments',
 p_blart  LIKE  bbkpf-blart DEFAULT 'S6',
 p_bukrs  LIKE  bbkpf-bukrs DEFAULT 'UGL',
 p_canbdc(12) TYPE c DEFAULT 'CAD_CTR_PYMT',                "SDP42984
 p_usbdc(12)  TYPE c DEFAULT 'USD_CTR_PYMT',                "SDP42984
 p_caacct LIKE  bseg-hkont DEFAULT '140720',                "SDP42984
 p_usacct LIKE  bseg-hkont DEFAULT '140730'.                "SDP42984
**--START OF CHANGES FOR CHG0115682 BY KBANERJEE
SELECT-OPTIONS:s_distri FOR  zfc07-srccode,
               s_sandt  FOR  zfc07-srccode.
**--END OF CHANGES FOR CHG0115682 BY KBANERJEE
SELECTION-SCREEN END OF BLOCK box1.

************************************************************************
*                     AT SELECTION-SCREEN                              *
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  REPLACE wrk_symbolic WITH sy-sysid INTO infile.
  CONDENSE: infile NO-GAPS.

************************************************************************
*                   START OF SELECTION                                 *
************************************************************************
START-OF-SELECTION.
  REFRESH: cdn_ctrx, usa_ctrx.
  PERFORM open_datasets.
  PERFORM build_contrx_records.

************************************************************************
*                        OPEN_DATASETS                                 *
************************************************************************

FORM open_datasets.
  OPEN DATASET infile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE e006(zm) WITH infile.
  ENDIF.

ENDFORM.                    "OPEN_DATASETS

************************************************************************
*                 BUILD_CONTRX_RECORDS                                 *
************************************************************************
FORM build_contrx_records.

  REFRESH editrec.
**--START OF CHANGES FOR CHG0115682 BY KBANERJEE
  PERFORM f_get_variant.
**--END OF CHANGES FOR CHG0115682 BY KBANERJEE
  DO.
    READ DATASET infile INTO editrec.
    IF ( sy-index = 1 ).
      IF ( sy-subrc = 4 ).
        MESSAGE i028(zs).
        STOP.
      ENDIF.
    ENDIF.
*   When end of file, Exit.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    MOVE editrec-payment_amt TO w_payment_amt.              "SDP39341
**--START OF CHANGES FOR CHG0115682 BY KBANERJEE
    "IF editrec-currency_code = 'CDN'.
    IF editrec-currency_code = '$CAD'.
**--END OF CHANGES FOR CHG0115682 BY KBANERJEE
      PERFORM build_cdn_contrax_records.
    ELSE.
      PERFORM build_usa_contrax_records.
    ENDIF.
  ENDDO.

  PERFORM open_batch_session USING p_canbdc.                "SDP42984
  PERFORM build_bdc_session TABLES cdn_ctrx.
  PERFORM bdc_close_group USING p_canbdc.                   "SDP42984

  PERFORM open_batch_session USING p_usbdc.                 "SDP42984
  PERFORM build_bdc_session TABLES usa_ctrx.
  PERFORM bdc_close_group USING p_usbdc.                    "SDP42984

ENDFORM.                    "BUILD_CONTRX_RECORDS


************************************************************************
*                 BUILD_CDN_CONTRAX_RECORDS                            *
************************************************************************
FORM build_cdn_contrax_records.
  DATA : lv_wrbtr(16) TYPE p DECIMALS 2.          "Added by DADIM for CHG0252113
*Record: 1
  MOVE-CORRESPONDING editrec TO cdn_ctrx.
*   MOVE SY-DATUM TO: CDN_CTRX-BLDAT, CDN_CTRX-BUDAT.            "TR928
*START OF CHANGES FOR CHG0115683 BY KBANERJEEE
*  MOVE editrec-payment_date TO: cdn_ctrx-bldat, cdn_ctrx-budat. "TR928
*  MOVE editrec-payment_rdate TO cdn_ctrx-zuonr." ADDED BY AKMADASU FOR CHG0115682
  MOVE editrec-payment_rdate TO:cdn_ctrx-bldat, cdn_ctrx-budat. "TR928
  MOVE editrec-payment_date  TO cdn_ctrx-zuonr." ADDED BY AKMADASU FOR CHG0115
*END OF CHANGES FOR CHG0115683 BY KBANERJEE
*   MOVE SY-DATUM+4(2)   TO  CDN_CTRX-MONAT.    "Period          "TR928
*START OF CHANGES FOR CHG0115683 BY KBANERJEE
*  MOVE editrec-payment_date+4(2) TO  cdn_ctrx-monat.   "Period  "TR928
  MOVE editrec-payment_rdate+4(2) TO  cdn_ctrx-monat.   "Period  "TR928
*END OF CHANGES FOR CHG0115683 BY KBANERJEE
  MOVE: p_blart        TO  cdn_ctrx-blart,
        p_bukrs        TO  cdn_ctrx-bukrs,
        p_xblnr        TO  cdn_ctrx-xblnr,
        p_bktxt        TO  cdn_ctrx-bktxt,
        'CAD'          TO  cdn_ctrx-waers,                  "SDP39341
        editrec-payment_amt   TO  cdn_ctrx-wrbtr,
*         P_BKTXT        TO  CDN_CTRX-SGTXT,  " Commented BY AKMADASU FOR CHG0115682
        p_caacct       TO  cdn_ctrx-hkont.                  "SDP42984
**--START OF CHANGES BY AKMADASU FOR CHG0115682
  CONCATENATE p_bktxt '-' editrec-billing_id INTO cdn_ctrx-sgtxt.
**--END OF CHANGES BY AKMADASU FOR CHG0115682
*Start of changes by DADIM for CHG0252113
*  IF cdn_ctrx-wrbtr < 0.
  MOVE cdn_ctrx-wrbtr TO lv_wrbtr.
  IF lv_wrbtr < 0.
*End of changes by DADIM for CHG0252113
    cdn_ctrx-wrbtr = cdn_ctrx-wrbtr  * -1.
    MOVE '02'  TO    cdn_ctrx-newbs.
  ELSE.
    MOVE '11'  TO  cdn_ctrx-newbs.
  ENDIF.

*  IF EDITREC-SOURCE_CODE = 'IMS'  OR EDITREC-SOURCE_CODE = 'IMSC'  OR
*     EDITREC-SOURCE_CODE = 'IMSS' OR EDITREC-SOURCE_CODE = 'IMSSC' OR
*     EDITREC-SOURCE_CODE = 'IMSST'.
**--START OF CHANGES FOR CHG0115682 BY KBANERJEE
  IF s_distri IS NOT INITIAL.
    IF editrec-source_code IN gt_r_distri.
*      IF editrec-source_code = '332'  OR editrec-source_code = '795'  OR
*     editrec-source_code = '793'  OR editrec-source_code = '794'  OR
*     editrec-source_code = '796'  OR editrec-source_code = '791'.
      CONCATENATE 'CX' editrec-customer_num INTO cdn_ctrx-newko.
    ENDIF.
    IF s_sandt IS NOT INITIAL.
      IF editrec-source_code IN gt_r_st.
        CONCATENATE 'ST' editrec-customer_num INTO cdn_ctrx-newko.
      ENDIF.
    ENDIF.
  ENDIF.
**--END OF CHANGES FOR CHG0115682 BY KBANERJEE
  CONDENSE cdn_ctrx-newko NO-GAPS.
  APPEND cdn_ctrx.

*Record: 2
  CLEAR cdn_ctrx-newko.
  IF w_payment_amt > 0.                                     "SDP39341
    MOVE  '40'           TO  cdn_ctrx-newbs.
  ELSE.
    MOVE  '50'           TO  cdn_ctrx-newbs.
  ENDIF.

  SELECT SINGLE * FROM zfc07
   WHERE srccode = editrec-source_code
     AND pymttyp = editrec-payment_type.

  IF sy-subrc = 0.
    MOVE zfc07-oacct TO  cdn_ctrx-newko.
  ENDIF.
  APPEND cdn_ctrx.
  CLEAR  cdn_ctrx.

ENDFORM.                    "BUILD_CDN_CONTRAX_RECORDS

************************************************************************
*                 BUILD_USA_CONTRAX_RECORDS                            *
************************************************************************
FORM build_usa_contrax_records.
*Record: 1
  MOVE-CORRESPONDING editrec TO usa_ctrx.
*   MOVE SY-DATUM TO: USA_CTRX-BLDAT, USA_CTRX-BUDAT.            "TR928
*START OF CHANGES FOR CHG0115683 BY KBANERJEE
*  MOVE editrec-payment_date TO: usa_ctrx-bldat, usa_ctrx-budat. "TR928
*  MOVE editrec-payment_rdate TO usa_ctrx-zuonr." ADDED BY AKMADASU FOR CHG0115682
**   MOVE SY-DATUM+4(2)   TO  USA_CTRX-MONAT.     "Period         "TR928
  MOVE editrec-payment_rdate TO: usa_ctrx-bldat, usa_ctrx-budat. "TR928
  MOVE editrec-payment_date  TO usa_ctrx-zuonr." ADDED BY AKMADASU FOR CHG0115682
*   MOVE SY-DATUM+4(2)   TO  USA_CTRX-MONAT.     "Period         "TR928
*END OF CHANGES FOR CHG0115683 BY KBANERJEE
*START OF CHANGES FOR CHG0115683 BY KBANERJEE
*  MOVE editrec-payment_date+4(2)   TO  usa_ctrx-monat. "Period  "TR928
  MOVE editrec-payment_rdate+4(2)   TO  usa_ctrx-monat. "Period  "TR928
*END OF CHANGES FOR CHG0115683 BY KBANERJEE
  MOVE: p_blart        TO  usa_ctrx-blart,
        p_bukrs        TO  usa_ctrx-bukrs,
        p_xblnr        TO  usa_ctrx-xblnr,
        p_bktxt        TO  usa_ctrx-bktxt,
        'USD'          TO  usa_ctrx-waers,
        editrec-payment_amt  TO  usa_ctrx-wrbtr,
*         P_BKTXT        TO  USA_CTRX-SGTXT, " COMMENTED BY AKMADASU FOR CHG0115682
        p_usacct       TO  usa_ctrx-hkont.                  "SDP42984
**--START OF CHANGES BY AKMADASU FOR CHG0115682
  CONCATENATE p_bktxt '-' editrec-billing_id INTO usa_ctrx-sgtxt.
**--END OF CHANGES BY AKMADASU FOR CHG0115682
  IF usa_ctrx-wrbtr < 0.
    usa_ctrx-wrbtr = usa_ctrx-wrbtr * -1.
    MOVE   '02'         TO  usa_ctrx-newbs.
  ELSE.
    MOVE   '11'         TO  usa_ctrx-newbs.
  ENDIF.
**--START OF CHANGES BY AKMADASU
*IF EDITREC-SOURCE_CODE = 'IMS'  OR EDITREC-SOURCE_CODE = 'IMSC'  OR
*   EDITREC-SOURCE_CODE = 'IMSS' OR EDITREC-SOURCE_CODE = 'IMSSC' OR
*   EDITREC-SOURCE_CODE = 'IMSST'.
**--END OF CHANGES BY AKMADASU FOR CHG0115682
  IF s_distri IS NOT INITIAL.
**--START OF CHANGES FOR CHG0115682 BY KBANERJEE
    IF editrec-source_code IN gt_r_distri.
*  IF editrec-source_code = '332'  OR editrec-source_code = '795'  OR
*     editrec-source_code = '793'  OR editrec-source_code = '794'  OR
*     editrec-source_code = '796'  OR editrec-source_code = '791'.
**-- END OF CHANGES BY AKMADASU
      CONCATENATE 'CX' editrec-customer_num INTO usa_ctrx-newko.
    ENDIF.
  ENDIF.
  IF s_sandt IS NOT INITIAL.
    IF editrec-source_code IN gt_r_st.
      CONCATENATE 'ST' editrec-customer_num INTO usa_ctrx-newko.
    ENDIF.
  ENDIF.
**--END OF CHANGES FOR CHG0115682 BY KBANERJEE
  CONDENSE usa_ctrx-newko NO-GAPS.

  APPEND usa_ctrx.

*Record: 2
  IF w_payment_amt < 0.                                     "SDP39341
    MOVE  '50'           TO  usa_ctrx-newbs.
  ELSE.
    MOVE  '40'           TO  usa_ctrx-newbs.
  ENDIF..

  SELECT SINGLE * FROM zfc07
   WHERE srccode = editrec-source_code
     AND pymttyp = editrec-payment_type.
  CLEAR usa_ctrx-newko.
  IF sy-subrc = 0.
    MOVE zfc07-usd_oacct TO  usa_ctrx-newko.
  ELSE.
    MOVE editrec-payment_type TO usa_ctrx-newko.
  ENDIF.
  APPEND usa_ctrx.
  CLEAR  usa_ctrx.

ENDFORM.                    "BUILD_USA_CONTRAX_RECORDS

************************************************************************
*                        BUILD_BDC_SESSION                            *
************************************************************************
FORM build_bdc_session TABLES itab LIKE cdn_ctrx[].
  DATA: tbldat TYPE d,
        tbudat TYPE d,
        tzuonr TYPE d," ADDED BY AKMADASU
        zzuonr(10), " ADDED BY AKMADASU
        zbldat(10),                    "Date in user format
        zbudat(10).
**--START OF CHANGES FOR CHG0115682 BY KBANERJEE
  DATA lv_new_item TYPE flag.
**--END OF CHANGES FOR CHG0115682 BY KBANERJEE
  LOOP AT itab.
* convert dates to user-default format
    tbldat = itab-bldat.
    tbudat = itab-budat.
    tzuonr = itab-zuonr. " ADDED BY AKMADASU
    WRITE tzuonr TO zzuonr DD/MM/YYYY. " ADDED BY AKMADASU
    WRITE tbldat TO zbldat DD/MM/YYYY.
    WRITE tbudat TO zbudat DD/MM/YYYY.
    IF w_line_item_count = 0.
*Header Screen
      PERFORM bdc_screen USING 'SAPMF05A' '100'.
      PERFORM bdc_field  USING 'BKPF-BLDAT' zbldat.
      PERFORM bdc_field  USING 'BKPF-BLART' itab-blart.
      PERFORM bdc_field  USING 'BKPF-BUKRS' itab-bukrs.
      PERFORM bdc_field  USING 'BKPF-BUDAT' zbudat.
      PERFORM bdc_field  USING 'BKPF-MONAT' itab-monat.
      PERFORM bdc_field  USING 'BKPF-WAERS' itab-waers.
      PERFORM bdc_field  USING 'BKPF-XBLNR' itab-xblnr.
      PERFORM bdc_field  USING 'BKPF-BKTXT' itab-bktxt.
    ENDIF.
    w_line_item_count = w_line_item_count + 1.
    IF w_line_item_count = 999.
      CLEAR w_line_item_count.
    ENDIF.

*Detail Line Screen 301: Posting key = 11, 02.
    IF itab-newbs = '11' OR itab-newbs = '02'.
      PERFORM bdc_field  USING 'RF05A-NEWBS' itab-newbs.
      PERFORM bdc_field  USING 'RF05A-NEWKO' itab-newko.
      PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.  "Press Enter Key

      PERFORM bdc_screen USING 'SAPMF05A' '0301'.
      PERFORM bdc_field  USING 'BSEG-WRBTR' itab-wrbtr.
      PERFORM bdc_field  USING 'BSEG-ZUONR' zzuonr. " ADDED BY AKMADASU BY CHG0115682
      PERFORM bdc_field  USING 'BSEG-SGTXT' itab-sgtxt.
      SHIFT itab-newko LEFT DELETING LEADING space.
      IF itab-newko(2) = 'ST'.                              "SDP42984
        PERFORM bdc_field  USING 'BSEG-HKONT' itab-hkont.   "SDP42984
      ENDIF.                                                "SDP42984
    ENDIF.

*Detail Line Screen 300: Posting key = 40, 50.
    IF itab-newbs = '40' OR itab-newbs = '50'.
      PERFORM bdc_field  USING 'RF05A-NEWBS' itab-newbs.
      PERFORM bdc_field  USING 'RF05A-NEWKO' itab-newko.
      PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.  "Press Enter Key
      PERFORM bdc_screen USING 'SAPMF05A' '0300'.
      PERFORM bdc_field  USING 'DKACB-FMORE' ' '.
      PERFORM bdc_field  USING 'BSEG-WRBTR' itab-wrbtr.
      PERFORM bdc_field  USING 'BSEG-SGTXT' itab-sgtxt.
**--START OF CHANGES FOR CHG0115682 BY KBANERJEE
      lv_new_item = 'X'.
**--END OF CHANGES FOR CHG0115682 BY KBANERJEE
    ENDIF.
**--START OF CHANGES FOR CHG0115682 BY KBANERJEE
    "AT LAST.
    IF lv_new_item IS NOT INITIAL.
**--END OF CHANGES FOR  CHG0115682 BY KBANERJEE
      PERFORM bdc_field  USING 'BDC_OKCODE'  '/11'.

      PERFORM insert_session..
      REFRESH bdcdata.
      CLEAR:  bdcdata, w_line_item_count.
**--START OF CHANGES FOR  CHG0115682 BY KBANERJEE
    ENDIF.
    "ENDAT.
    CLEAR lv_new_item.
**--END OF CHANGES FOR  CHG0115682 BY KBANERJEE
  ENDLOOP.

ENDFORM.                    "BUILD_BDC_SESSION

************************************************************************
*                        OPEN_BATCH_SESSION                            *
************************************************************************
FORM open_batch_session USING session_name.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client              = sy-mandt
      group               = session_name
      keep                = 'X'
      user                = sy-uname
    EXCEPTIONS
      client_invalid      = 1
      destination_invalid = 2
      group_invalid       = 3
      group_is_locked     = 4
      holddate_invalid    = 5
      internal_error      = 6
      queue_error         = 7
      running             = 8
      system_lock_error   = 9
      user_invalid        = 10
      OTHERS              = 11.

  IF sy-subrc <> 0.
    MESSAGE e004(zs) WITH session_name.
  ENDIF.

ENDFORM.                    "OPEN_BATCH_SESSION

************************************************************************
*     FORM INSERT_SESSION                                              *
************************************************************************
FORM insert_session.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = g_tcode
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.
  IF sy-subrc <> 0.
    MESSAGE e013(zs) WITH sy-subrc.
  ENDIF.

ENDFORM.                    "INSERT_SESSION

************************************************************************
*                           BDC_CLOSE_GROUP                            *
************************************************************************
FORM bdc_close_group USING session_name.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1
      queue_error = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
    MESSAGE i003(zs) WITH session_name.
  ENDIF.

ENDFORM.                    " BDC_CLOSE
*---------------------------------------------------------------------*
*      Form  BDC_SCREEN
*---------------------------------------------------------------------*
FORM bdc_screen USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                               " BDC_SCREEN

*---------------------------------------------------------------------*
*      Form  BDC_FIELD
*---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                               " BDC_FIELD
**--START OF CHANGES FOR CHG0115682 BY KBANERJEE
*&---------------------------------------------------------------------*
*&      Form  F_GET_VARIANT
*&---------------------------------------------------------------------*
*     Get variant values
*----------------------------------------------------------------------*
FORM f_get_variant.
  DATA:lv_variant   TYPE raldb_vari,
       lv_report    TYPE raldb_repo,
       ls_var_data  TYPE varid.
  DATA:ls_r_srccode TYPE gty_srccode.
  DATA:lt_sel_values TYPE STANDARD TABLE OF rsparams
                     INITIAL SIZE 0,
       ls_sel_values TYPE rsparams.

  lv_report = sy-repid.
  IF sy-batch IS NOT INITIAL.
    MOVE sy-slset TO lv_variant.
  ELSE.
    lv_variant = sy-slset.
  ENDIF.

  CALL FUNCTION 'RS_VARIANT_VALUES_TECH_DATA'
    EXPORTING
      report               = lv_report
      variant              = lv_variant
    IMPORTING
      techn_data           = ls_var_data
    TABLES
      variant_values       = lt_sel_values
    EXCEPTIONS
      variant_non_existent = 1
      variant_obsolete     = 2
      OTHERS               = 3.
  IF sy-subrc = 0.
    SORT lt_sel_values BY selname.
**--START OF CHANGES FOR CHG0115683 BY KBANERJEE
*    CASE lv_variant.
*      WHEN  'PAYMENT'.
**--END OF CHANGES FOR CHG0115683 BY KBANERJEE
    LOOP AT lt_sel_values INTO ls_sel_values.
      IF  ls_sel_values-selname = 'S_DISTRI'.
        ls_r_srccode-low    = ls_sel_values-low.
        ls_r_srccode-sign   = ls_sel_values-sign.
        ls_r_srccode-option = ls_sel_values-option.
        APPEND ls_r_srccode TO gt_r_distri.
      ENDIF.
      IF ls_sel_values-selname = 'S_SANDT'.
        ls_r_srccode-low    = ls_sel_values-low.
        ls_r_srccode-sign   = ls_sel_values-sign.
        ls_r_srccode-option = ls_sel_values-option.
        APPEND ls_r_srccode TO gt_r_st.
      ENDIF.
      CLEAR:ls_sel_values,ls_r_srccode.
    ENDLOOP.
**--START OF CHANGES FOR CHG0115683 BY KBANERJEE
*      WHEN OTHERS.
**Do nothing
*    ENDCASE.
**--END OF CHANGES FOR CHG0115683 BY KBANERJEE
  ENDIF.
ENDFORM.                    " F_GET_VARIANT
**--END OF CHANGES FOR CHG0115682 BY KBANERJEE
