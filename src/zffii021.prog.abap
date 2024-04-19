REPORT zffii021 MESSAGE-ID zs LINE-COUNT 65 LINE-SIZE 255.

************************************************************************
* Program Description
* This abap splits the input file into 2 files
* - the first file will contain the Canadian currency transactions
* - the second file will contain the US$ currency transaction
*----------------------------------------------------------------------
* Changes
* 2010/10/12 gymana   TR466 Added two new input fields: rate & rate uom
* 2007/10/31 mdemeest TR453 Added salesperson
* 2007/09/21 mdemeest TR486 For FUEL, if amount entered, don't
*                           recalculate.
* 2006/09/14 mdemeest TR307 Check for opposite signs on amount & volume
*                           after summarization
* 2006/04/22 mdemeest TR114 S&T expanded file
* 2005/08/03 mdemeest #____ Remove zero amounts into a separate file
*                           NO Action will be taken with this file
* 2005/04/06 mdemeest #____ New abap
* 2021/02/23 NAGIRIR COG changes to pass Aggregate Customer from      *
*                         selection screen
* 2022/04/25 NAGIRIR CHG0247976 Populate Party ID At Text Field for    *
*                    D30K932171 S&T Postings                           *
************************************************************************
TABLES: zstgl,            "G/L Lookup table
        zfc02,            "Contrax- Rate Class Lookup Table
        zstconv,          "Conversion Factors of UOM to GJ
        zwacog,
        kna1,             " Added for COG
        zstcurr.          "Conversion Currency of Contrax to SAP
*


* Used to parse file into various fields
DATA:  BEGIN OF wa        OCCURS 0,
       item_type(10)              TYPE c,
       applied_year(4)            TYPE c,
       applied_month(2)           TYPE c,
       pty_id(8)                  TYPE c,
       rate_class(12)             TYPE c,
       st_code(6)                 TYPE c,
       sc_code(2)                 TYPE c,
       seasonal_class(4)          TYPE c,
       rate_type(4)               TYPE c,
       charge_type(4)             TYPE c,
       sr_usage(4)                TYPE c,
       st_sub_type(6)             TYPE c,
       non_rate_item_type(8)      TYPE c,
       cycled_ind(1)              TYPE c,
       tier_step_level(2)         TYPE c,
       amount(17)                 TYPE c,
       amount_uom(8)              TYPE c,
       volume(15)                 TYPE c,
       volume_uom(8)              TYPE c,
       item_class(8)              TYPE c,
       exchange_rate(20)          TYPE c,
       ssegid(10)                 TYPE c,
       srsubtype(10)              TYPE c,
       srusersup(10)              TYPE c,
       contrref(15)               TYPE c,
       shrtlngterm(10)            TYPE c,
       waers LIKE zstcurr-waers,
* derived info from zstgl
       saknr LIKE zstgl-saknr,              "Acct number
       mwskz LIKE zstgl-mwskz,              "Tax Code
       matnr LIKE zstgl-matnr,              "material number

       oacct LIKE zstgl-oacct,              "Offset Acct Number
       omwskz LIKE zstgl-omwskz,            "Offset tax code

       wwrat LIKE zfc02-wwrat,
       bukrs LIKE zfc02-bukrs,
       factor LIKE zstconv-factor,
       amountn TYPE p DECIMALS 2,
       volumen TYPE p DECIMALS 5,
       conv_vol TYPE p DECIMALS 5,
       wwsls(18)                  TYPE c,           "TR453
       rate(8)                    TYPE c,           "TR466
       rate_uom(8)                TYPE c,           "TR466
       pty_id1(8)                 TYPE c." Added for D30K932171
DATA:  END OF wa.

*------------------------  Selection Screen  ---------------------------
* Input File
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER: infile1 LIKE filenameci-fileextern
                  DEFAULT '/usr/sap/interfaces/P01/DRCO0078/BKPF.SAP'.
SELECTION-SCREEN END OF BLOCK box.
SKIP 1.
* Output Files
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETER: outfile1 LIKE filenameci-fileextern
            DEFAULT '/usr/sap/interfaces/P01/DRCO0078/st_f_22.sap',
           outfile2 LIKE filenameci-fileextern
            DEFAULT '/usr/sap/interfaces/P01/DRCO0078/st_fb01.sap',
           outfile3 LIKE filenameci-fileextern
            DEFAULT '/usr/sap/interfaces/P01/DRCO0078/zbis231_zero.chk'.
SELECTION-SCREEN END OF BLOCK box2.

PARAMETER: p_waers LIKE zstcurr-waers.

SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-004.
SELECT-OPTIONS:  s_sruse FOR wa-sr_usage       NO INTERVALS,
                 s_stsub FOR wa-st_sub_type    NO INTERVALS.
SELECTION-SCREEN END OF BLOCK box4.

SELECTION-SCREEN BEGIN OF BLOCK box3  WITH FRAME TITLE text-003.
*START of CHange COG
PARAMETER: p_ptyid(8) TYPE c.
PARAMETER: p_kunnr   TYPE kunag OBLIGATORY DEFAULT ''.
SELECT-OPTIONS : s_kunnr FOR kna1-kunnr NO INTERVALS.
*END of Change COG
PARAMETER: p_input AS CHECKBOX.
PARAMETER: p_conv AS CHECKBOX.
PARAMETER: p_sumvol AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK box3.
*-------------------------  End of Input Screen  -----------------------

*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
DATA:  delimiter     TYPE x   VALUE '09',
       char(21)      TYPE c,
       recsize       TYPE i,
       wa_glacct(10) TYPE c,
       zero_const(17) TYPE c VALUE '                0'.

DATA:  wa_inrec1(400)  TYPE c.                      "Input Record


* WA to summarize & output files to create BDC
DATA:  BEGIN OF waout     OCCURS 0,
       item_type(10)              TYPE c,
       applied_year(4)            TYPE c,
       applied_month(2)           TYPE c,
       pty_id(8)                  TYPE c,   "Customer Number
       rate_class(12)             TYPE c,
       sr_usage(4)                TYPE c,
       amountz(15) TYPE c,
       conv_volz(15) TYPE c,
       volume_uom(8)              TYPE c,
       item_class(8)              TYPE c,
       exchange_rate(20)          TYPE c,
       saknr LIKE zstgl-saknr,              "Acct number
       mwskz LIKE zstgl-mwskz,              "Tax Code
       matnr LIKE zstgl-matnr,              "Material Number
       oacct LIKE zstgl-oacct,              "Offset Acct
       omwskz LIKE zstgl-omwskz,            "Offset Tax code
       wwrat LIKE zfc02-wwrat,
       bukrs LIKE zfc02-bukrs,
       srusersup(10)              TYPE c,
       waers LIKE zstcurr-waers,             "Amt uom from SAP
       conv_vol TYPE p  DECIMALS 5,   "Converted Vol in GJ
       amountn LIKE wa-amountn,
       wwsls(18)                  TYPE c,   "Salesperson TR453
       pty_id1(8)                  TYPE c." Added for D30K932171
*       amount(17) type c,
*       volume(17) type c.
DATA:  END OF waout.


*-----------------------------------------------------------------------
PERFORM open_files.                      "Open files
PERFORM input_file_into_wa.              "reads file into wa
IF p_input = 'X'.                        "displays input file
  PERFORM print_input_file.
ENDIF.

PERFORM get_zstconv_zstcurr.        "Volume to GJ & Currency Conversion

PERFORM get_valid_zstgl.
PERFORM get_zfc02_info.


IF p_conv = 'X'.
  PERFORM print_converted_file.
ENDIF.

PERFORM summarize_waout.
PERFORM check_for_opposite_signs.
PERFORM check_for_zero_amounts.
IF p_sumvol = 'X'.
  PERFORM print_summary_volume..
ENDIF.

WRITE: /100 text-001.            "End of Report

PERFORM split_file.

LOOP AT wa.
*  write: / wa-amount, wa-volume, wa-volume_uom, wa-factor,
*                                     wa-conv_vol,      wa-amount_uom,
*                                                       wa-waers.
ENDLOOP.

CLOSE DATASET:  infile1,  outfile1,  outfile2, outfile3.


************************************************************************
* Routine to open the physical files for input & output in text mode.
*------------------------  OPEN_FILES ----------------------------------
FORM open_files.
  OPEN DATASET infile1  FOR INPUT  IN TEXT MODE.
  OPEN DATASET outfile1 FOR OUTPUT IN TEXT MODE.
  OPEN DATASET outfile2 FOR OUTPUT IN TEXT MODE.
  OPEN DATASET outfile3 FOR OUTPUT IN TEXT MODE.
ENDFORM.                    "OPEN_FILES

*----------------------INPUT_FILE_INTO_WA ------------------------------
* read file into wa
*-----------------------------------------------------------------------
FORM input_file_into_wa.
*START of changes COG
  TYPES : BEGIN OF ty_pty,
            pty_id TYPE char10,
          END OF ty_pty.
  DATA: lwa_pty TYPE ty_pty,
        lt_pty TYPE STANDARD TABLE OF ty_pty.

  LOOP AT s_kunnr.
    CLEAR lwa_pty.
    lwa_pty-pty_id = s_kunnr-low.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lwa_pty-pty_id
      IMPORTING
        output = lwa_pty-pty_id.
    APPEND lwa_pty TO lt_pty.
  ENDLOOP.
*END of changes COG
  DO.
    CLEAR wa_inrec1.
    CLEAR wa.
    READ DATASET infile1 INTO wa_inrec1.

    IF sy-subrc <> 0.           "Exit when file is completely read in
      EXIT.
    ENDIF.

    SPLIT wa_inrec1 AT delimiter INTO
           wa-item_type          wa-applied_year      wa-applied_month
           wa-pty_id             wa-rate_class        wa-st_code
           wa-sc_code            wa-seasonal_class    wa-rate_type
           wa-charge_type        wa-sr_usage          wa-st_sub_type
           wa-non_rate_item_type wa-cycled_ind        wa-tier_step_level
           wa-amount             wa-amount_uom        wa-volume
           wa-volume_uom         wa-item_class        wa-exchange_rate
           wa-ssegid             wa-srsubtype         wa-srusersup
           wa-contrref           wa-shrtlngterm       wa-wwsls
           wa-rate               wa-rate_uom.                     "TR466
    SHIFT wa-wwsls LEFT DELETING LEADING space.                   "TR453
    wa-pty_id1 = wa-pty_id. " Added For Change D30K932171
    IF wa-item_type = 'item_type'.     "This ignores heading line
    ELSE.
      MOVE wa-amount TO wa-amountn.
      MOVE wa-volume TO wa-volumen.

      IF p_ptyid = wa-pty_id OR p_ptyid = '        '.
*START of Change COG
        SHIFT wa-pty_id LEFT DELETING LEADING ' '.
        CLEAR: lwa_pty.
        READ TABLE lt_pty INTO lwa_pty WITH KEY pty_id = wa-pty_id.
        IF sy-subrc NE 0.
          wa-pty_id = p_kunnr.
        ENDIF.
*      append wa.
*END of Change COG
        APPEND wa.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.                    "input_file_into_wa

*------------------------ GET_VALID_ZSTGL ------------------------------
* Validate input record to ZSTGL table and if found, add accounting
* information to the record.  If not found, the write the record to a
* report so that the Client & Business Analyst can determine what needs
* to be added to the table.
*-----------------------------------------------------------------------
FORM get_valid_zstgl.
  WRITE: /1 text-100.
  WRITE: /.
  WRITE: /1 text-008, 10 text-017,  20 text-010,
         30 text-012, 40 text-013,
         50 text-014, 60 text-015,  70 text-016,
         80 text-044, 90 text-011, 100 text-020,
        120 text-022, 140 text-023.

  LOOP AT wa.
    SELECT SINGLE * FROM zstgl
      WHERE c_nrttyp = wa-non_rate_item_type
        AND c_svctyp = wa-st_code
        AND c_seascl = wa-seasonal_class
        AND c_rtetyp = wa-rate_type
        AND c_chgtyp = wa-charge_type
        AND c_sruse = wa-sr_usage
        AND c_stsub = wa-st_sub_type
        AND c_term = wa-shrtlngterm
        AND c_sccode = wa-sc_code.
*----------------------------------------------------------------------
* If the entry is not found, write to the report so that the client
* can add entry to the table
* otherwise, write required info to output file
*-----------------------------------------------------------------------
    IF sy-subrc <> '0'.
      WRITE: /  wa-pty_id             UNDER text-008,
                wa-non_rate_item_type UNDER text-017,
                wa-st_code            UNDER text-010,
                wa-seasonal_class     UNDER text-012,
                wa-rate_type          UNDER text-013,
                wa-charge_type        UNDER text-014,
                wa-sr_usage           UNDER text-015,
                wa-st_sub_type        UNDER text-016,
                wa-shrtlngterm        UNDER text-044,
                wa-sc_code            UNDER text-011,
                wa-amount             UNDER text-020,
                wa-volume             UNDER text-022,
                wa-volume_uom         UNDER text-023.
      SELECT SINGLE * FROM zstgl
            WHERE c_svctyp = 'ERROR'.
      IF sy-subrc = '0'.
        MOVE zstgl-saknr TO wa-saknr.
        IF wa-waers = 'CAD'.
          MOVE zstgl-oacct TO wa-oacct.
        ELSE.
          MOVE zstgl-usd_saknr TO wa-oacct.
        ENDIF.
      ENDIF.
    ELSE.
      MOVE zstgl-saknr     TO wa-saknr.
      MOVE zstgl-mwskz     TO wa-mwskz.
      MOVE zstgl-matnr     TO wa-matnr.
      IF wa-waers = 'CAD'.    "decision for canadian or us offset acct
        MOVE zstgl-oacct     TO wa-oacct.
        MOVE zstgl-omwskz    TO wa-omwskz.
      ELSE.
        MOVE zstgl-usd_saknr TO wa-oacct.
        MOVE zstgl-usmwskz   TO wa-omwskz.
      ENDIF.
*      if ( wa-sr_usage = 'FUEL' or wa-st_sub_type = 'FUEL' ).
      IF ( wa-sr_usage IN s_sruse OR wa-st_sub_type IN s_stsub ).
        IF ( wa-amount = zero_const OR wa-amount = space ) .
          SELECT SINGLE * FROM zwacog
             WHERE volume_uom = wa-volume_uom.

          IF sy-subrc = '0'.
            COMPUTE wa-amountn = wa-volume * zwacog-wacog.
            COMPUTE wa-amount = wa-volume * zwacog-wacog.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY wa.

  ENDLOOP.
ENDFORM.                    "get_valid_zstgl

*---------------------  GET_ZFC02 INFO --------------------------------
*  This routine loops thru the work area (WA) and selects the correct
*  record from ZFC02 to determine the SAP Rate Class & company code
*  The information is added to each record in the work area.
*-----------------------------------------------------------------------
FORM  get_zfc02_info.

  LOOP AT wa.
    CLEAR: wa-wwrat, wa-bukrs.
    SELECT SINGLE * FROM zfc02
       WHERE c_ratecl  = wa-rate_class.
*        and c_svctyp  = wa-st_code
*        and c_seascl  = wa-seasonal_class.

    IF sy-subrc = '0'.
      MOVE zfc02-wwrat TO wa-wwrat.
      MOVE zfc02-bukrs TO wa-bukrs.
      MODIFY wa.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "GET_ZFC02_INFO


*----------------------   GET_ZSTCONV_ZSTCURR  -------------------------
*  This routine looks at each individual line & translates the
*  S&T file-supplied currency to a valid SAP currency.  It also
*  converts the S&T file-supplied volume & unit of measure to
*  a standard unit of measure (GJ) for SAP.  These new values are added
*  to the WA record and the WA record is modified.  The S&T file should
*  always have the UOM for amount & volume fields filled.
*-----------------------------------------------------------------------
FORM get_zstconv_zstcurr.
  WRITE: /1 text-008, 10 text-017,  20 text-010,
         30 text-012, 40 text-013,
         50 text-014, 60 text-015,  70 text-016,
         80 text-044, 90 text-011, 100 text-020,
        120 text-022, 140 text-023.

  CLEAR: wa-waers, wa-factor.
  LOOP AT wa.
    CLEAR: wa-factor, wa-conv_vol, wa-waers.

    SELECT SINGLE * FROM zstconv
       WHERE volume_uom = wa-volume_uom.
    IF sy-subrc = '0'.
      MOVE zstconv-factor TO wa-factor.
      COMPUTE wa-conv_vol = wa-factor * wa-volumen.
    ELSE.
      IF wa-non_rate_item_type = 'GST'.
      ELSE.
        WRITE: /  wa-pty_id             UNDER text-008,
                  wa-non_rate_item_type UNDER text-017,
                  wa-st_code            UNDER text-010,
                  wa-seasonal_class     UNDER text-012,
                  wa-rate_type          UNDER text-013,
                  wa-charge_type        UNDER text-014,
                  wa-sr_usage           UNDER text-015,
                  wa-st_sub_type        UNDER text-016,
                  wa-shrtlngterm        UNDER text-044,
                  wa-sc_code            UNDER text-011,
                  wa-amount             UNDER text-020,
                  wa-volume             UNDER text-022,
                  wa-volume_uom         UNDER text-023,
                  text-002.
      ENDIF.
    ENDIF.

*  If there's no currency or the currency is in error, assume CAD
    SELECT SINGLE * FROM zstcurr
        WHERE c_waers = wa-amount_uom.
    IF sy-subrc = '0'.
      MOVE zstcurr-waers TO wa-waers.
    ELSE.
      MOVE 'CAD'         TO wa-waers.
    ENDIF.

    MODIFY wa.
  ENDLOOP.

  WRITE: /.
ENDFORM.                    "get_zstconv_zstcurr
*----------------------  SUMMARIZE_WAOUT  ------------------------------
FORM summarize_waout.
  LOOP AT wa.
    MOVE-CORRESPONDING wa TO waout.
    COLLECT waout.
  ENDLOOP.
ENDFORM.                    "summarize_waout
*----------------------- SPLIT_FILE ------------------------------------
* This routine move the WA into the workout area (WAOUT) splitting it
* into
* 1. Customer Records (OUTFILE1) for transaction F_22 and
* 2. GL Records (OUTFILE2) for transaction FB01
* 3. Zero Records (OUTFILE3) which are not processed
* and summarizes the files.
*-----------------------------------------------------------------------
FORM split_file.

*   ALL RECORDS WITH AMOUNTS NOT EQUAL TO ZERO will be in outfile1 or
*   outfile2.
*-----------------------------------------------------------------------
* Split records based on currency code.
* - Canadian currency will be in outfile1
* - US currency will be in outfile2
*-----------------------------------------------------------------------
  LOOP AT waout.
    MOVE waout-amountn TO waout-amountz.
    MOVE waout-conv_vol TO waout-conv_volz.
*       select single * from zfstar
*          where saknr = waout-saknr
*            and itemcl = waout-item_class.

    IF waout-waers = 'CAD'.
*           move '000000000000900949' to waout-matnr.
      TRANSFER waout TO outfile1.
*           EXIT.
    ELSE.
      TRANSFER waout TO outfile2.
    ENDIF.
  ENDLOOP.


ENDFORM.                    "SPLIT_FILE
*--------------------- CHECK_FOR_OPPOSITE_SIGNS ------------------------
* Amounts & volumes must have the same sign in the record.  If
* different, must be split into 2 separate records with same details
*-----------------------------------------------------------------------
FORM check_for_opposite_signs.
  DATA: waholdvol LIKE waout-conv_vol.
  LOOP AT waout.
    IF  ( waout-amountn > 0 AND waout-conv_vol < 0 ) OR
        ( waout-amountn < 0 AND waout-conv_vol > 0 ).
      MOVE waout-conv_vol TO waholdvol.
      MOVE 0 TO waout-conv_vol.
      MODIFY waout.
      MOVE waholdvol TO waout-conv_vol.
      MOVE 0 TO waout-amountn.
      APPEND waout.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "check_for_opposite_signs

*&---------------------------------------------------------------------*
*&      Form  check_for_zero_amounts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_for_zero_amounts.
  LOOP AT waout.
*if ( waout-amountn ca '123456789' ).
    IF waout-amountn <> 0.
    ELSE.
      PERFORM write_zero_amount.
    ENDIF.

  ENDLOOP.
ENDFORM.                    "check_for_zero_amounts

*&---------------------------------------------------------------------*
*&      Form  write_zero_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_zero_amount.
  ADD '0.01' TO waout-amountn.
  MODIFY waout.
  COMPUTE waout-amountn = waout-amountn * -1.
  MOVE 0 TO waout-conv_vol.
  APPEND waout.
ENDFORM.                    "write_zero_amount
*-------------------------  PRINT_INPUT_FILE ---------------------------
* This lists all the input fields on the file being received from S&T
*-----------------------------------------------------------------------
FORM print_input_file.
  WRITE: / text-035.
  WRITE: / text-005, text-006, text-007, text-008, text-009,
           text-010, text-011, text-012, text-013, text-014,
           text-015, text-016, text-017, text-018, text-019,
           text-020, text-021, text-022, text-023, text-024,
           text-025,
           text-040, text-041, text-042, text-043, text-044.
  LOOP AT wa.
    WRITE: / wa-item_type UNDER text-005, wa-applied_year UNDER text-006,
            wa-applied_month UNDER text-007, wa-pty_id UNDER text-008,
            wa-rate_class UNDER text-009, wa-st_code UNDER text-010,
            wa-sc_code UNDER text-011, wa-seasonal_class UNDER text-012,
            wa-rate_type UNDER text-013, wa-charge_type UNDER text-014,
            wa-sr_usage UNDER text-015, wa-st_sub_type UNDER text-016,
            wa-non_rate_item_type UNDER text-017,
            wa-cycled_ind UNDER text-018,
            wa-tier_step_level UNDER text-019, wa-amount UNDER text-020,
            wa-amount_uom UNDER text-021, wa-volume UNDER text-022,
            wa-volume_uom UNDER text-023, wa-item_class UNDER text-024,
            wa-exchange_rate UNDER text-025,
            wa-ssegid UNDER text-040, wa-srsubtype UNDER text-041,
            wa-srusersup UNDER text-042, wa-contrref UNDER text-043,
            wa-shrtlngterm UNDER text-044.
  ENDLOOP.
ENDFORM.                    "print_input_file



*&---------------------------------------------------------------------*
*&      Form  print_converted_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_converted_file.
  WRITE: / text-036.
*write: / text-005, text-006, text-007,
  WRITE: / text-008, text-009,
           text-010, text-011, text-012, text-013, text-014,
           text-015, text-016, text-017, text-026, text-027,
           text-028, text-029, text-022, text-023, text-031,
           text-032, text-020,
           text-021, text-030, text-024.
  LOOP AT wa.
*write: / wa-item_type under text-005, wa-applied_year under text-006,
*        wa-applied_month under text-007,
    WRITE: / wa-pty_id UNDER text-008,
            wa-rate_class UNDER text-009, wa-st_code UNDER text-010,
            wa-sc_code UNDER text-011, wa-seasonal_class UNDER text-012,
            wa-rate_type UNDER text-013, wa-charge_type UNDER text-014,
            wa-sr_usage UNDER text-015, wa-st_sub_type UNDER text-016,
            wa-non_rate_item_type UNDER text-017,
            wa-matnr+12(6) UNDER text-026, wa-saknr+4(6) UNDER text-027,
            wa-wwrat UNDER text-028, wa-bukrs UNDER text-029,
            wa-volumen UNDER text-022, wa-volume_uom UNDER text-023,
            wa-factor UNDER text-031, wa-conv_vol UNDER text-032,
            wa-amount UNDER text-020,
            wa-amount_uom UNDER text-021,
            wa-waers UNDER text-030,
            wa-item_class UNDER text-024,
            wa-exchange_rate UNDER text-025.
  ENDLOOP.
ENDFORM.                    "print_converted_file

*&---------------------------------------------------------------------*
*&      Form  print_summary_volume
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM print_summary_volume.
  WRITE: / text-037.
*write: / text-005, text-006, text-007,
  WRITE: / text-008, text-009,
           text-015, text-026, text-027,
           text-028, text-029, text-022,
           text-032, text-020,
           text-030, text-024.
  LOOP AT waout.
*write: / wa-item_type under text-005, wa-applied_year under text-006,
*        wa-applied_month under text-007,
    WRITE: / waout-pty_id UNDER text-008,
            waout-rate_class UNDER text-009,
            waout-sr_usage UNDER text-015,
      waout-matnr+12(6) UNDER text-026, waout-saknr+4(6) UNDER text-027,
            waout-wwrat UNDER text-028, waout-bukrs UNDER text-029,
            waout-conv_vol UNDER text-032,
            waout-amountn UNDER text-020,
            waout-waers UNDER text-030,
            wa-item_class UNDER text-024,
            wa-exchange_rate UNDER text-025.
  ENDLOOP.
ENDFORM.                    "print_summary_volume
