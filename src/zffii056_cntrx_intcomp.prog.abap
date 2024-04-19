REPORT  zffii056_cntrx_intcomp  MESSAGE-ID zs.
************************************************************************
* Program Name: ZFFII056_CNTRX_INTCOMP                                 *
* Date        : March 2021                                             *
* Author      : Rajendra Nagiri(nagirir).                              *
* Program Description:                                                 *
*  This program will Receive Storage and transportation information    *
*  from contrax system to SAP and Creates session in SM35 for Postings.*
*                                                                      *
************************************************************************
*CHANGES:                                                              *
*Issue      By      Date       Description                             *
*----------------------------------------------------------------------*
************************************************************************
TABLES: lfa1, zfit_cntrxicmap.
TYPES: BEGIN OF ty_ifile,
  type(10)              TYPE c,
  year(4)               TYPE c,
  month(2)              TYPE c,
  pty_id(8)             TYPE c,
  rate_class(12)        TYPE c,
  st_code(6)            TYPE c,
  sc_code(2)            TYPE c,
  seasonal_class(4)     TYPE c,
  rate_type(4)          TYPE c,
  charge_type(4)        TYPE c,
  sr_usage(4)           TYPE c,
  st_sub_type(6)        TYPE c,
  non_rate_item_type(8) TYPE c,
  cycled_ind(1)         TYPE c,
  tier_step_level(2)    TYPE c,
  amount(16)            TYPE p DECIMALS 2,
  amount_uom(8)         TYPE c,
  volume(15)            TYPE c,
  volume_uom(8)         TYPE c,
  item_class(8)         TYPE c,
  exchange_rate(20)     TYPE c,
  ssegid(10)            TYPE c,
  srsubtype(10)         TYPE c,
  srusersup(10)         TYPE c,
  contrref(15)          TYPE c,
  shrtlnterm(10)        TYPE c,
  sr_num(8)             TYPE c,
  unit_rate(8)          TYPE c,
  unit_rate_uom(8)      TYPE c,
  END OF ty_ifile,
BEGIN OF ty_data,
  awkey TYPE awkey,   " Reference key
  uname TYPE usnam,   " Username
  bktxt TYPE bktxt,   " Document Header Text
  bukrs TYPE bukrs,   " Commpany code
  bldat TYPE char10,  " Document Date
  budat TYPE char10,  " Posting Date
  gjahr TYPE gjahr,   " Fiscal Year #from input file
  buzei TYPE buzei,   " item
  monat TYPE monat,   " Posting Period
  blart TYPE blart,   " Document type
  xblnr TYPE xblnr,   " Reference Doc Number
  wrbtr TYPE char15,  " Amount in Document Currency
  waers TYPE waers,   " Currency key
  wrbtr1 TYPE char15, " Amount in Document Currency
  waers1 TYPE waers,  " Currency key
  kursr TYPE char10,  " Exchange Rate
  hkont TYPE hkont,   " GL/Customer Nume
  saknr TYPE saknr,   " GL/Customer name
  bschl TYPE bschl,   " Posing Key
  menge TYPE char16,  " Quantity
  meins TYPE char8,   " UOM
  mwskz TYPE mwskz,   " Tax
  mwskz1 TYPE mwskz,  " Tax1
  sgtxt TYPE sgtxt,   " Item Text
  lifnr TYPE lifnr,   " Party ID/ Customer
  zuonr TYPE dzuonr,  " Assignment Number
  kostl TYPE kostl,   " Cost Center
  susp  TYPE char1,   " suspense ACC flag
END OF ty_data.
TYPES: BEGIN OF ty_keko,
  bzobj TYPE ck_obj,    " REFERENCE OBJECT
  kalnr	TYPE ck_kalnr1, "	Cost Estimate Number - Product Costing
  kalka	TYPE ck_kalka,  "	Costing Type
  kadky TYPE ck_kadky,  " Costing Date (Key)
  tvers	TYPE ck_tvers,  "	Costing Version
  bwvar TYPE ck_bwvar,  " Valuation Variant in Costing
  kkzma TYPE ck_kkzma,  " Costs Entered Manually in Additive or Automatic Cost Est.
  matnr	TYPE matnr,	    " Material Number
  werks	TYPE werks_d,	  " Plant
  kadat TYPE ck_abdat,  " Costing Date From
  bidat	TYPE ck_bidat,  "	Costing Date To
  END OF ty_keko.

DATA: wa_data TYPE ty_data,
      it_data TYPE STANDARD TABLE OF ty_data.
DATA: msg(80)       TYPE          c,
      gv_input(400) TYPE          c,
      s_splits      TYPE          string,
      t_splits      LIKE TABLE OF s_splits,
      wa_ifile      TYPE          ty_ifile,
      it_ifile      TYPE STANDARD TABLE OF ty_ifile,
      it_sfile      TYPE STANDARD TABLE OF ty_ifile,
      gv_negative_flag TYPE char1.
DATA:  BEGIN OF bdcdata OCCURS 500.
        INCLUDE STRUCTURE bdcdata.
DATA:  END OF bdcdata.

DATA: it_zmaptb TYPE STANDARD TABLE OF zfit_cntrxicmap,
      wa_zmaptb TYPE zfit_cntrxicmap.

FIELD-SYMBOLS:  <curcol>      TYPE          any.

CONSTANTS: delimtr(1) TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
           c_tcode TYPE tstc-tcode VALUE 'FB01',
           c_usd   TYPE waers VALUE 'USD',
           c_cad   TYPE waers VALUE 'CAD',
           c_gj    TYPE meins VALUE 'GJ',
           c_gj1   TYPE meins VALUE 'GJ1',
           c_mmb   TYPE meins VALUE 'MMB',
           c_gjdif TYPE char6 VALUE '1.0551',
           c_natgas TYPE matnr VALUE 'NATGAS',
           c_gegd   TYPE werks_d VALUE 'GEGD',
           c_x     TYPE c     VALUE 'X',
           c_e     TYPE c     VALUE 'E'.

*************************************************************************
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_input LIKE filenameci-fileextern OBLIGATORY,
            p_group LIKE bgr00-group DEFAULT 'ZCNTRXGL-ACT',
            p_tcode LIKE tstc-tcode DEFAULT 'FB01'.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS:	p_bukrs TYPE bukrs DEFAULT 'EGD' OBLIGATORY MODIF ID mi1, " Company code
            p_blart TYPE t003-blart DEFAULT 'SS' OBLIGATORY MODIF ID mi1, " Document Type
            p_mwskz TYPE bseg-mwskz DEFAULT 'I0' OBLIGATORY MODIF ID mi1,
p_bktxt TYPE bktxt MODIF ID mi1. " Header Text
SELECT-OPTIONS: s_enritm FOR zfit_cntrxicmap-non_rate_item_type
                          NO INTERVALS DEFAULT 'MISC'.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME.
SELECT-OPTIONS: s_ptyid FOR zfit_cntrxicmap-pty_id NO INTERVALS,
  s_ratecl FOR zfit_cntrxicmap-rate_class NO INTERVALS,
  s_stcode FOR zfit_cntrxicmap-st_code    NO INTERVALS,
  s_ratetp FOR zfit_cntrxicmap-rate_type  NO INTERVALS,
  s_srusag FOR zfit_cntrxicmap-sr_usage   NO INTERVALS,
  s_nritem FOR zfit_cntrxicmap-non_rate_item_type NO INTERVALS,
  s_cyclin FOR zfit_cntrxicmap-cycled_ind NO INTERVALS,
  s_tstepl FOR zfit_cntrxicmap-tier_step_level NO INTERVALS,
  s_conref FOR zfit_cntrxicmap-contract_reference NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b4.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
PARAMETERS: p_actual  RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND uc1,
            p_est     RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b3.
*************************************************************************
*************************************************************************

INITIALIZATION.
  PERFORM getdata_maptable.
  PERFORM default_filepath.

*************************************************************************
*************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_enritm-low.
  PERFORM f4_exclude_rateclass.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ptyid-low.
  PERFORM f4_values USING 'PTY_ID' 'S_PTYID-LOW' CHANGING s_ptyid-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ratecl-low.
  PERFORM f4_values USING 'RATE_CLASS' 'S_RATECL-LOW' CHANGING s_ratecl-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_stcode-low.
  PERFORM f4_values USING 'ST_CODE' 'S_STCODE-LOW' CHANGING s_stcode-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_ratetp-low.
  PERFORM f4_values USING 'RATE_TYPE' 'S_RATETP-LOW' CHANGING s_ratetp-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_srusag-low.
  PERFORM f4_values USING 'SR_USAGE' 'S_SRUSAG-LOW' CHANGING s_srusag-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_nritem-low.
  PERFORM f4_values USING 'NON_RATE_ITEM_TYPE' 'S_NRITEM-LOW' CHANGING s_nritem-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_cyclin-low.
  PERFORM f4_values USING 'CYCLED_IND' 'S_CYCLIN-LOW' CHANGING s_cyclin-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tstepl-low.
  PERFORM f4_values USING 'TIER_STEP_LEVEL' 'S_TSTEPL-LOW' CHANGING s_tstepl-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_conref-low.
  PERFORM f4_values USING 'CONTRACT_REFERENCE' 'S_CONREF-LOW' CHANGING s_conref-low.

*************************************************************************
*************************************************************************

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_output.

*************************************************************************
*************************************************************************
START-OF-SELECTION.

  PERFORM data_read.      " Read data from input file
  PERFORM data_preprocess_new. " Filter file data with mapping table
  IF p_test IS INITIAL.
    PERFORM data_post_bdc." Create BDC Session
  ELSE.
    PERFORM data_testrun. " Display ALV outptu
  ENDIF.

*----------------------------------------------------------------------*
*  This routine reads the tab-delimited input file sent by the
*  client community and splits it into its various components.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DATA_READ
*&---------------------------------------------------------------------*
FORM data_read .
*Open Data Set for Reading
  OPEN DATASET p_input  FOR INPUT  IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH p_input msg.
    STOP.
  ENDIF.
  CLEAR gv_input.
*Read data set
  DO.
    READ DATASET p_input INTO gv_input.

    IF sy-subrc <> 0.  "Exit when file is completely read in
      EXIT.
    ENDIF.
    CLEAR t_splits.
    SPLIT gv_input AT delimtr INTO TABLE t_splits.

    LOOP AT t_splits INTO s_splits.
      ASSIGN COMPONENT sy-tabix
             OF STRUCTURE wa_ifile
             TO <curcol>.
      MOVE s_splits TO <curcol>.
    ENDLOOP.
    CONDENSE: wa_ifile-pty_id.
    APPEND wa_ifile TO it_ifile.
    CLEAR: wa_ifile.
  ENDDO.
*Close Data set after Read
  CLOSE DATASET p_input.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH text-004 p_input msg.
    STOP.
  ENDIF.
ENDFORM.                    " DATA_READ
*&---------------------------------------------------------------------*
*&      Form  DATA_POST_BDC
*&---------------------------------------------------------------------*
FORM data_post_bdc.
*For Every Entry/Record of a File, we are creating a posting document.
  LOOP AT it_data INTO wa_data.
    CLEAR: gv_negative_flag.
    IF wa_data-wrbtr < 0.
      gv_negative_flag = 'X'.
    ENDIF.
    AT FIRST.
      PERFORM open_bdc_session.
    ENDAT.

    REFRESH: bdcdata." Clear BDC Data for each line item

*DEBIT/CREDIT ENTRY
    PERFORM screen_100. " Header Data
    CASE gv_negative_flag.
      WHEN ' '.
        PERFORM bdc_field  USING 'RF05A-NEWBS' '40'.
        PERFORM bdc_field  USING 'RF05A-NEWKO' wa_data-hkont.
      WHEN 'X'.
        PERFORM bdc_field  USING 'RF05A-NEWBS' '50'.
        PERFORM bdc_field  USING 'RF05A-NEWKO' wa_data-hkont.
        wa_data-wrbtr = wa_data-wrbtr * -1.
    ENDCASE.
    PERFORM bdc_screen USING 'SAPMF05A'    '300'.
    PERFORM bdc_field  USING 'BSEG-WRBTR'  wa_data-wrbtr.
    PERFORM bdc_field  USING 'BSEG-SGTXT'  wa_data-sgtxt.
    PERFORM bdc_field  USING 'BSEG-ZUONR'  wa_data-zuonr.
*Do not updated ZERO Volumes
    IF wa_data-menge <> 0.
      PERFORM bdc_field USING 'BSEG-MENGE'  wa_data-menge.
      PERFORM bdc_field USING 'BSEG-MEINS'  wa_data-meins.
    ENDIF.
*OPPOSITE Entry for Above Debit/Credit
    CASE gv_negative_flag.
      WHEN ' '.
        PERFORM bdc_field  USING 'RF05A-NEWBS' '50'.
      WHEN 'X'.
        PERFORM bdc_field  USING 'RF05A-NEWBS' '40'.
    ENDCASE.
    PERFORM bdc_field  USING 'RF05A-NEWKO' wa_data-saknr.
    IF wa_data-susp IS INITIAL. " wa_data-hkont ne '00160303'.
      PERFORM bdc_field USING  'BSEG-MWSKZ'  p_mwskz.
    ENDIF.
    PERFORM bdc_field  USING 'BDC_OKCODE' '=ZK'.

    PERFORM bdc_screen USING 'SAPLKACB' '0002'.
    IF wa_data-susp IS INITIAL. " wa_data-hkont ne '00160303'.
      PERFORM bdc_field  USING 'BDC_CURSOR' 'COBL-KOSTL'.
      PERFORM bdc_field  USING 'COBL-KOSTL' wa_data-kostl.
    ENDIF.
    PERFORM bdc_field  USING 'BDC_OKCODE' '=ENTE'.
*Opposite Entry Debit/Credit
    PERFORM bdc_screen USING 'SAPMF05A' '0330'.
    PERFORM bdc_field  USING 'BDC_OKCODE' '/00'.

    PERFORM bdc_screen USING 'SAPMF05A'    '300'.
    PERFORM bdc_field  USING 'BSEG-WRBTR'  wa_data-wrbtr.
    PERFORM bdc_field  USING 'BSEG-SGTXT'  wa_data-sgtxt.
    PERFORM bdc_field  USING 'BSEG-ZUONR'  wa_data-zuonr.

    PERFORM bdc_screen USING 'SAPLKACB' '0002'.
    PERFORM bdc_field  USING 'BDC_OKCODE' '=ENTE'.
    PERFORM bdc_screen USING 'SAPMF05A'   '0300'.
    PERFORM bdc_field  USING 'BDC_OKCODE' '=BU'.
    PERFORM bdc_screen USING 'SAPLKACB' '0002'.
    PERFORM bdc_field  USING 'BDC_OKCODE' '/8'.

    PERFORM insert_session.
    AT LAST.
      PERFORM close_bdc_session.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " DATA_POST_BDC
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT
*&---------------------------------------------------------------------*
FORM screen_output .
  IF p_actual = 'X'.
    p_group = 'ZCNTRXGL-ACT'.
    p_bktxt = text-001.
  ELSEIF p_est = 'X'.
    p_group = 'ZCNTRXGL-EST'.
    p_bktxt = text-002.
  ENDIF.
ENDFORM.                    " SCREEN_OUTPUT

*----------------------------  OPEN BDC_SESSION ---------------------
FORM open_bdc_session.
  DATA: lv_hdate  LIKE sy-datum.

  COMPUTE lv_hdate = sy-datum - 1.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client            = sy-mandt
      group             = p_group
      holddate          = lv_hdate
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
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '004' WITH p_group lv_hdate.
  ENDIF.
ENDFORM.                    "OPEN_BDC_SESSION

*----------------------------  CLOSE_BDC_SESSION  ---------------------

FORM close_bdc_session.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open
      queue_error.
  IF sy-subrc = 0.
    MESSAGE ID 'ZS' TYPE 'I' NUMBER '003' WITH p_group.
  ENDIF.
ENDFORM.                    "CLOSE_BDC_SESSION

*-----------------------  BDC_SCREEN  ---------------------------------
*  This routine adds an entry to the table BDCDATA with screen info
*  from a particular transaction.  This is used as part of the process
* from creating data for batch input.

FORM bdc_screen USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin ='X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_SCREEN

*-----------------------  BDC_FIELD  ----------------------------------
*  This routine adds an entry to the table BDCDATA with field info from
*  a particular transaction.  This is used as part of the process for
*  creating data for batch input.

FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD

*------------------------  INSERT_SESSION  ----------------------------
*  This routine inserts the BDC data for one transaction into the batch
*  input session
*-----------------------------------------------------------------------
FORM insert_session.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode          = c_tcode
    TABLES
      dynprotab      = bdcdata
    EXCEPTIONS
      internal_error = 1
      not_open       = 2
      queue_error    = 3
      tcode_invalid  = 4.
  IF sy-subrc <> 0 ##MG_MISSING.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '013' WITH c_tcode.
  ENDIF.
ENDFORM.                    "INSERT_SESSION

*&---------------------------------------------------------------------*
*&      Form  SCREEN_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_100.

  PERFORM bdc_screen USING 'SAPMF05A' '100'.
  PERFORM bdc_field USING 'BKPF-BLDAT' wa_data-bldat.
  PERFORM bdc_field USING 'BKPF-BLART' wa_data-blart.
  PERFORM bdc_field USING 'BKPF-XBLNR' wa_data-xblnr.
  PERFORM bdc_field USING 'BKPF-BUKRS' wa_data-bukrs.
  PERFORM bdc_field USING 'BKPF-BUDAT' wa_data-budat.
  PERFORM bdc_field USING 'RF05A-NEWKO' wa_data-hkont.
  PERFORM bdc_field USING 'BKPF-WAERS' wa_data-waers.
  PERFORM bdc_field USING 'BKPF-BKTXT' wa_data-bktxt.

ENDFORM.                                                    "SCREEN_100
*&---------------------------------------------------------------------*
*&      Form  DATA_PREPROCESS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_preprocess_new.

  DATA: last_day_of_month LIKE sy-datum,
        lv_budat          LIKE sy-datum,
        lv_fday           LIKE sy-datum,
        lv_lday           LIKE sy-datum,
        lw_keko           TYPE ty_keko,
        lv_amount         TYPE kstel,
        lv_menge          TYPE menge_d,
        lv_wrbtr          TYPE wrbtr,
        w_post_fg         TYPE char1.

  CONSTANTS:c_suspense TYPE zconref  VALUE 'SUSPENSE'.

  REFRESH it_sfile.

*keep data from selection only
  LOOP AT it_ifile INTO wa_ifile WHERE pty_id           IN s_ptyid
                                 AND rate_class         IN s_ratecl
                                 AND st_code            IN s_stcode
                                 AND rate_type          IN s_ratetp
                                 AND sr_usage           IN s_srusag
                                 AND non_rate_item_type IN s_nritem
                                 AND cycled_ind         IN s_cyclin
                                 AND tier_step_level    IN s_tstepl
                                 AND contrref           IN s_conref.
    APPEND wa_ifile TO it_sfile.
  ENDLOOP.

* Logic for First Day & Last Day based on Period from input file
  CLEAR: wa_ifile, lv_fday, lv_lday.
  READ TABLE it_sfile INTO wa_ifile INDEX 1.
  IF sy-subrc = 0.
    CONCATENATE wa_ifile-year wa_ifile-month '01' INTO lv_fday.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_fday
      IMPORTING
        last_day_of_month = lv_lday
      EXCEPTIONS
        day_in_no_date.
    IF  sy-subrc <> 0.
      WRITE: /1 text-005, sy-subrc, lv_fday.
    ENDIF.
  ENDIF.
*Logic for KEKO & KEPH tables
  CLEAR: lw_keko.
  SELECT SINGLE bzobj kalnr kalka kadky tvers bwvar kkzma matnr werks kadat bidat
    FROM keko INTO lw_keko
    WHERE matnr = c_natgas
    AND   werks = c_gegd
    AND   kadat LE lv_lday
    AND   bidat GE lv_lday.
  IF sy-subrc = 0.
    CLEAR: lv_amount.
    SELECT SINGLE kst001
      FROM keph
      INTO lv_amount
      WHERE bzobj = lw_keko-bzobj
      AND kalnr = lw_keko-kalnr
      AND kalka  = lw_keko-kalka
      AND kadky  = lw_keko-kadky
      AND tvers = lw_keko-tvers
      AND bwvar  = lw_keko-bwvar
      AND kkzma  = lw_keko-kkzma.
  ENDIF.
***update i_data file
  CLEAR : wa_ifile,w_post_fg.
  LOOP AT it_sfile INTO wa_ifile.
    CLEAR: wa_data, wa_zmaptb,w_post_fg.
    READ TABLE it_zmaptb INTO wa_zmaptb
      WITH KEY pty_id             = wa_ifile-pty_id
               rate_class         = wa_ifile-rate_class
               st_code            = wa_ifile-st_code
               rate_type          = wa_ifile-rate_type
               sr_usage           = wa_ifile-sr_usage
               st_sub_type        = wa_ifile-st_sub_type
               non_rate_item_type = wa_ifile-non_rate_item_type
               cycled_ind         = wa_ifile-cycled_ind
               tier_step_level    = wa_ifile-tier_step_level
               contract_reference = wa_ifile-contrref.
    IF sy-subrc = 0.
      IF wa_ifile-amount = '0.00' AND wa_zmaptb-quantity_based IS INITIAL.
        CONTINUE.
      ENDIF.
      w_post_fg = 'X'.
    ELSE.
      IF wa_ifile-non_rate_item_type IN s_enritm.  "Ignore offset line
        CONTINUE.
      ELSE.
        CLEAR wa_zmaptb.
        READ TABLE it_zmaptb INTO wa_zmaptb
                             WITH KEY pty_id             = wa_ifile-pty_id
                                      contract_reference = c_suspense.
        IF sy-subrc = 0.
          IF wa_ifile-amount = '0.00' AND wa_zmaptb-quantity_based IS INITIAL.
            CONTINUE.
          ENDIF.
          w_post_fg = 'X'.
          wa_data-susp = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF w_post_fg = 'X'.
*HEADER DATA TO WA_DATA
      wa_data-bldat = sy-datum. " Current date as Document date
      wa_data-blart = p_blart.  " Document type
      wa_data-bukrs = p_bukrs.  " Company Code
      IF wa_ifile-contrref IS INITIAL.
        wa_data-xblnr = wa_zmaptb-contract_reference.
      ELSE.
        wa_data-xblnr = wa_ifile-contrref. " Reference
      ENDIF.
**POSTING DATE LOGIC
      IF p_actual = 'X'.
        wa_data-budat = sy-datum.     " Current Date as posting date
      ELSEIF p_est = 'X'.
        wa_data-budat = lv_lday.
      ENDIF.
      wa_data-lifnr = wa_ifile-pty_id." Vendor
      wa_data-mwskz = p_mwskz.        " TAX
      wa_data-monat = wa_ifile-month. " Period
      wa_data-bktxt = p_bktxt.        " Header text From Selection
      wa_data-uname = sy-uname.       " Username
      wa_data-gjahr = wa_ifile-year.  " Fiscal year

      CASE wa_ifile-amount_uom.
        WHEN '$CDN'.
          wa_data-waers = 'CAD'.
        WHEN '$US'.
          wa_data-waers = 'USD'.
      ENDCASE.
      IF wa_ifile-amount < 0.
        wa_data-wrbtr = wa_ifile-amount. " -Ve Amount
        wa_data-menge = wa_ifile-volume * -1 . " +Ve QUANTITY
        CONDENSE: wa_data-wrbtr, wa_data-menge.
      ELSE.
        wa_data-wrbtr = wa_ifile-amount. " +ve Amount
        CONDENSE: wa_data-wrbtr.
      ENDIF.
      CONCATENATE 'S&T' wa_zmaptb-pty_id INTO wa_data-zuonr SEPARATED BY space.
      wa_data-kostl = wa_zmaptb-new_cost_center.
      wa_data-hkont = wa_zmaptb-new_glaccount.
      wa_data-saknr = wa_zmaptb-offset_glaccount.

      CONCATENATE wa_ifile-pty_id   wa_ifile-rate_class wa_ifile-st_code
                  wa_ifile-rate_type wa_ifile-sr_usage wa_ifile-st_sub_type
                  wa_ifile-non_rate_item_type wa_ifile-cycled_ind wa_ifile-tier_step_level
                  wa_ifile-contrref INTO wa_data-sgtxt SEPARATED BY '.'.
      IF wa_data-menge IS INITIAL.
        wa_data-menge = wa_ifile-volume.
      ENDIF.
      wa_data-meins = wa_ifile-volume_uom.

      CONDENSE: wa_data-menge, wa_data-meins.
      IF wa_data-meins = c_mmb.
        CLEAR: lv_menge.
        lv_menge = wa_data-menge * c_gjdif.
        wa_data-meins = c_gj.
        wa_data-menge = lv_menge.
        CONDENSE wa_data-menge.
      ENDIF.
      IF wa_zmaptb-quantity_based IS NOT INITIAL.
        CLEAR: lv_wrbtr.
        lv_wrbtr = wa_ifile-volume * lv_amount.
        lv_wrbtr = lv_wrbtr / 1000.
        wa_data-wrbtr = lv_wrbtr.
        CONDENSE: wa_data-wrbtr.
      ENDIF.
      APPEND wa_data TO it_data.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " DATA_PREPROCESS_NEW
*&---------------------------------------------------------------------*
*&      Form  F4_EXCLUDE_RATECLASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_exclude_rateclass .
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        ls_return TYPE ddshretval,
        lt_exrtcl TYPE STANDARD TABLE OF zfit_cntrxicmap,
        lw_exrtcl TYPE zfit_cntrxicmap.

  SELECT * FROM zfit_cntrxicmap
    INTO TABLE lt_exrtcl.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NON_RATE_ITEM_TYPE'
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'S_ENRITM-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = lt_exrtcl
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    RETURN.
  ELSE.
    READ TABLE lt_return INTO ls_return INDEX 1.
    s_enritm-low = ls_return-fieldval.
  ENDIF.
ENDFORM.                    " F4_EXCLUDE_RATECLASS
*&---------------------------------------------------------------------*
*&      Form  F4_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0512   text
*      <--P_S_PTYID  text
*----------------------------------------------------------------------*
FORM f4_values USING value(p_retfield)
                     value(p_dynfld)
               CHANGING p_s_ptyid.
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        ls_return TYPE ddshretval.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = p_retfield
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = p_dynfld
      value_org       = 'S'
    TABLES
      value_tab       = it_zmaptb
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    RETURN.
  ELSE.
    READ TABLE lt_return INTO ls_return INDEX 1.
    p_s_ptyid = ls_return-fieldval.
  ENDIF.

ENDFORM.                    " F4_VALUES
*&---------------------------------------------------------------------*
*&      Form  GETDATA_MAPTABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM getdata_maptable .
  REFRESH it_zmaptb.
  SELECT * FROM zfit_cntrxicmap
    INTO TABLE it_zmaptb.
ENDFORM.                    " GETDATA_MAPTABLE
*&---------------------------------------------------------------------*
*&      Form  DATA_TESTRUN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_testrun .
*Declare local structures/variables

  DATA: lt_fcat   TYPE slis_t_fieldcat_alv,
        lw_fcat   TYPE slis_fieldcat_alv,
        lv_repid  TYPE sy-repid.

** Build fieldcatlog
  CLEAR: lw_fcat.
  REFRESH: lt_fcat.
  lw_fcat-fieldname   = 'BUKRS'.
  lw_fcat-seltext_m   = 'Company Code'(007).
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'LIFNR'.
  lw_fcat-seltext_m   = 'Vendor'(008).
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'BKTXT'.
  lw_fcat-seltext_m   = 'Header Text'(009).
  lw_fcat-outputlen   = 25.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'BLDAT'.
  lw_fcat-seltext_m   = 'Document Date'(010).
  lw_fcat-outputlen   = 14.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'BUDAT'.
  lw_fcat-seltext_m   = 'Posting Date'(011).
  lw_fcat-outputlen   = 14.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'GJAHR'.
  lw_fcat-seltext_m   = 'Fiscal Year'(012).
  lw_fcat-outputlen   = 7.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'MONAT'.
  lw_fcat-seltext_m   = 'Posting Peiod'(013).
  lw_fcat-outputlen   = 10.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'BLART'.
  lw_fcat-seltext_m   = 'Doc. Type'(014).
  lw_fcat-outputlen   = 9.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'XBLNR'.
  lw_fcat-seltext_m   = 'Reference'(015).
  lw_fcat-outputlen   = 15.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'WRBTR'.
  lw_fcat-seltext_m   = 'Amount'(016).
  lw_fcat-outputlen   = 15.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'WAERS'.
  lw_fcat-seltext_m   = 'Currency'(017).
*  lw_fcat-col_pos     = 7.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'HKONT'.
  lw_fcat-seltext_m   = 'GL Account'(018).
  lw_fcat-outputlen   = 10.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'SAKNR'.
  lw_fcat-seltext_m   = 'Offset GL'(019).
  lw_fcat-outputlen   = 10.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'KOSTL'.
  lw_fcat-seltext_m   = 'Cost Center'(020).
  lw_fcat-outputlen   = 10.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'MENGE'.
  lw_fcat-seltext_m   = 'Quantity'(021).
  lw_fcat-outputlen   = 12.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'MEINS'.
  lw_fcat-seltext_m   = 'UOM'(022).
  lw_fcat-outputlen   = 7.
  APPEND lw_fcat TO lt_fcat.


  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'MWSKZ'.
  lw_fcat-seltext_m   = 'Tax Code'(023).
  lw_fcat-outputlen   = 7.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'SGTXT'.
  lw_fcat-seltext_m   = 'Item Text'(024).
  lw_fcat-outputlen   = 40.
  APPEND lw_fcat TO lt_fcat.

  CLEAR  lw_fcat.
  lw_fcat-fieldname   = 'ZUONR'.
  lw_fcat-seltext_m   = 'Assignment'(025).
  lw_fcat-outputlen   = 40.
  APPEND lw_fcat TO lt_fcat.

  lv_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = lv_repid
      it_fieldcat        = lt_fcat[]
      i_save             = c_x
    TABLES
      t_outtab           = it_data
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2 ##FM_SUBRC_OK.

ENDFORM.                    " DATA_TESTRUN
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_FILEPATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_filepath.
  IF p_actual = 'X'.
    CONCATENATE:  '/usr/sap/interfaces/' sy-sysid+0(3) '/LSDSATREV/'
                  'satrevact.dat' INTO p_input.
  ELSEIF p_est = 'X'.
    CONCATENATE:  '/usr/sap/interfaces/' sy-sysid+0(3) '/LSDSATREV/'
              'satrev.dat' INTO p_input.
  ENDIF.
ENDFORM.                    " DEFAULT_FILEPATH
