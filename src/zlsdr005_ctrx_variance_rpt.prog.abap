REPORT zlsdr003 NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132.

************************************************************************
*  Author:      Glenn Ymana
*  Date:        May 26, 2016
*  Project:     Contrax Rate Splitting
*  Issue Log:   ACR244
*
* Program Description
* This ABAP will read the Rate Split input file and map each record as
* a line item. Effective date will be taken from the mapped record and
* prices for all applicable Condition types held within SAP will be
* read from tables A032& KONP under sales org Z002. The report will take
* the volumes from the rate split file and the prices from the tables to
* calculate the amount of each component records. The variances between
* the amount posted from the file and the sum of the calculated
* component amounts will be reported.
*
* NOTE: ZLSDR003_CONDITION_RPT was used as a template for this pgm.
*----------------------------------------------------------------------
* Changes:
************************************************************************

************************************************************************
*    TABLES                                                            *
************************************************************************
TABLES: zglcond_ctx,          "GL / Condition Type Mapping Table
        A032,             "Price Group/Material Table
        konp.             "conditions Item Table

************************************************************************
*    SELECT OPTIONS                                                    *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-001.
PARAMETER: infile LIKE filenameci-fileextern DEFAULT
                '/usr/sap/interfaces/P01/BANNER/zbis100rrorder.dat'.
SELECTION-SCREEN END OF BLOCK box.

************************************************************************
*    VARIABLES                                                         *
************************************************************************

* Used to parse file into various fields

DATA:  BEGIN OF CREC OCCURS 0,
        APP_YR(4)             TYPE C,     "Year transaction applied
        APP_MTH(2)            TYPE C,     "Month transaction applied
        CUST_ID(8)            TYPE C,     "Customer number
        RATE_CL(12)           TYPE C,     "Rate class
        SERV_TYPE(6)          TYPE C,     "Service type
        SERV_CL(2)            TYPE C,     "Service class
        SEAS_CL(4)            TYPE C,     "Seasonal class
        RATE_TYPE(4)          TYPE C,     "Rate type
        CHARGE_TYPE(4)        TYPE C,     "Charge type
        SR_USAGE(4)           TYPE C,     "SR USAGE
        ST_SUBTYPE(6)         TYPE C,     "ST SUBTYPE
        NON_RATE_ITEM_TYP(8)  TYPE C,     "NON-RATE ITEM TYPE
        TIER_STEP_LVL(2)      TYPE C,     "TIER STEP LEVEL
        SECTOR_SIZE(1)        TYPE C,     "SECTOR SIZE
        SECTOR(6)             TYPE C,     "SECTOR
        AMOUNT(16)            TYPE C,     "TRANSACTION AMOUNT
        VOLUME(18)            TYPE C,     "TRANSACTION VOLUME
        CUST_TYPE(1)          TYPE C,     "CUSTOMER TYPE
        GECA_CODE(6)          TYPE C,     "GEOGRAPHIC AREA
        VOL_UOM(8)            TYPE C,     "VOLUME UNIT OF MEASURE
        SA_NUM(8)             TYPE C,     "CONTRACT NUMBER
        INV_NUM(8)            TYPE C,     "INVOICE # Ticket 65368
        REBILL_YR(4)          TYPE C,     "REBILL YEAR         ACR244
        REBILL_MTH(2)         TYPE C,     "REBILL MONTH        ACR244
        SO_ID(4)              TYPE C.     "SERVICE OFFERING ID ACR244

DATA: END OF CREC.

DATA: BEGIN OF intab OCCURS 0,
        NON_RATE_ITEM_TYP(8)  TYPE C,
        SERV_TYPE(6)          TYPE C,
        CUST_TYPE(1)          TYPE C,
        RATE_TYPE(4)          TYPE C,
        CHARGE_TYPE(4)        TYPE C,
        SR_USAGE(4)           TYPE C,
        ST_SUBTYPE(6)         TYPE C,
        AMOUNT(16)            TYPE P DECIMALS 2,
        VOLUME(16)            TYPE P DECIMALS 3,
        EFF_DATE              TYPE D,
      END OF intab.

DATA: itab_glcondctx LIKE zglcond_ctx OCCURS 0 WITH HEADER LINE.

DATA:  BEGIN OF itab_condtype OCCURS 0,
         knumh    LIKE A032-knumh,
         vtweg    LIKE A032-vtweg,
         kschl    LIKE A032-kschl,
         kbetr    LIKE konp-kbetr,
         kpein    LIKE konp-kpein,
         kmein    LIKE konp-kmein,
         datab    LIKE A032-datab,
         datbi    LIKE A032-datbi,
         vtext    LIKE zglcond-vtext,
         cons(16) TYPE P DECIMALS 3,
         amt(16)  TYPE P DECIMALS 2,
       END OF itab_condtype.

DATA:  BEGIN OF wa_A032 OCCURS 0,
         knumh    LIKE A032-knumh,
         vtweg    LIKE A032-vtweg,
         kschl    LIKE A032-kschl,
         datab    LIKE A032-datab,
         datbi    LIKE A032-datbi,
       END OF wa_A032.

DATA:  BEGIN OF err_file OCCURS 0,
        NON_RATE_ITEM_TYP(8) TYPE C,
        SERV_TYPE(6)         TYPE C,
        CUST_TYPE(1)         TYPE C,
        RATE_TYPE(4)         TYPE C,
        CHARGE_TYPE(4)       TYPE C,
        SR_USAGE(4)          TYPE C,
        ST_SUBTYPE(6)        TYPE C,
        AMOUNT(16)           TYPE P DECIMALS 2,
        eff_date             TYPE d,
        err_msg(80)          TYPE c,
       END OF err_file.

DATA:  BEGIN OF split_table OCCURS 0,
         kschl        LIKE A032-kschl,
         vtweg        LIKE A032-vtweg,
         vtext        LIKE zglcond-vtext,
         kbetr        LIKE konp-kbetr,
         datab        LIKE A032-datab,
         cons         TYPE p DECIMALS 3,
         amt          TYPE p DECIMALS 2,
         tot_type_amt TYPE p DECIMALS 2,
       END OF split_table.

DATA:  splttab LIKE split_table OCCURS 0 WITH HEADER LINE.

DATA:  inrec(400),
       ln_cntr                    TYPE i VALUE 0,
       w_rate                     LIKE zrrate-rate,
       w_effdate                  TYPE d,
       w_cons(13)                 TYPE p DECIMALS 3,
       w_trans_amt(13)            TYPE p DECIMALS 2,
       sum_trans_amt(13)          TYPE p DECIMALS 2,
       w_grand_tot_trans_amt(15)  TYPE p DECIMALS 2,
       w_file_tot_trans_amt(15)   TYPE p DECIMALS 2,
       w_variance(13)             TYPE p DECIMALS 2,
       va_amt                     TYPE p DECIMALS 2,
       split_sum_amt(13)          TYPE p DECIMALS 2,
       sum_amt_by_split_cd(13)    TYPE p DECIMALS 2,
       prev_kschl                 LIKE splttab-kschl,
       w_ctype_not_found_flag(1)  TYPE c,
       w_errfile_created_flag(1)  TYPE c,
       w_desc(40)                 TYPE c,
       msg_text(50).

************************************************************************
*    START OF SELECTION (MAINLINE)                                     *
************************************************************************
START-OF-SELECTION.

  PERFORM open_files.                      "Open files
  PERFORM split_file.
  PERFORM generate_split_sum_rpt.
  PERFORM generate_err_rpt.

END-OF-SELECTION.
************************************************************************
* Routine to open the physical files for input & output in text mode.
*************************  OPEN_FILES **********************************
FORM open_files.
  OPEN DATASET infile FOR INPUT IN TEXT MODE
               MESSAGE msg_text.

  IF sy-subrc NE 0.
    WRITE: 'File cannot be opened. Reason: ', msg_text.
  ENDIF.

  DO.
    READ DATASET infile INTO crec.
    IF sy-subrc <> '0'.
      EXIT.
    ENDIF.
    APPEND crec.
  ENDDO.

ENDFORM.                    "OPEN_FILES


************************************************************************
*  This routine reads the input file one record at a time and
*  maps certain field types to the correct Contrax RR component codes.
************************************************************************
FORM split_file.
  ln_cntr = '0'.
  LOOP AT crec.
    MOVE crec-amount TO w_trans_amt.
    IF w_trans_amt = 0.

      MOVE-CORRESPONDING crec TO intab.
      MOVE crec-volume TO w_cons.
*      IF crec-cons_sign = '-'.
*        w_cons = w_cons * -1.
*      ENDIF.
      MOVE crec-amount TO w_trans_amt.
*      IF crec-trans_amt_sign = '-'.
*        w_trans_amt = w_trans_amt * -1.
*      ENDIF.
      MOVE w_cons TO intab-volume.
      MOVE w_trans_amt TO intab-amount.

      CONCATENATE crec-app_yr crec-app_mth '01'
             INTO intab-eff_date.

      APPEND intab.

    ELSE.
      IF ln_cntr = 0.
        PERFORM write_reconc_hdg.
      ENDIF.

      PERFORM map_cond_type.

      IF ln_cntr >= 55.
        NEW-PAGE.
        PERFORM write_reconc_hdg.
      ENDIF.
    ENDIF.
  ENDLOOP.

  FORMAT INTENSIFIED ON.
  SKIP 2.
  WRITE /46 'Input File Total:'.
  WRITE (14) w_file_tot_trans_amt UNDER text-011.
  WRITE /46 'Component Grand Total:'.
  WRITE (14) w_grand_tot_trans_amt UNDER text-011.
  ln_cntr = ln_cntr + 4.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "SPLIT_FILE

************************************************************************
* Using the GL Class code, find the corresponding condition types
************************************************************************
FORM  map_cond_type.

  DATA: ls_zlsdbn002 TYPE zlsdbn002.

  MOVE 'N' TO w_ctype_not_found_flag.
  MOVE crec-volume TO w_cons.
*  IF crec-cons_sign = '-'.
*    w_cons = w_cons * -1.
*  ENDIF.
  MOVE crec-amount TO w_trans_amt.
*  IF crec-trans_amt_sign = '-'.
*    w_trans_amt = w_trans_amt * -1.
*  ENDIF.

* Select all Condition Type component records that match the input
* criteria
  REFRESH itab_glcondctx.
  CLEAR itab_glcondctx.

  SELECT * INTO TABLE itab_glcondctx
    FROM zglcond_ctx
   WHERE c_nrttp  = crec-non_rate_item_typ AND
         c_svctyp = crec-serv_type AND
         c_custtyp = '*' AND
         c_rtetyp = crec-rate_type AND
         c_chgtyp = crec-charge_type AND
         c_sruse = crec-sr_usage AND
         c_stsub = crec-st_subtype AND
         c_sccode = '**'.

  IF sy-subrc <> 0.
    MOVE-CORRESPONDING crec TO err_file.
    MOVE w_trans_amt TO err_file-amount.
    CONCATENATE crec-app_yr crec-app_mth '01'
           INTO err_file-eff_date.
    MOVE 'No matching condition type codes found in table ZGLCOND_CTX.'
         TO err_file-err_msg.
    APPEND err_file.
    MOVE 'Y' TO w_ctype_not_found_flag.
    MOVE 'Y' TO w_errfile_created_flag.
    "------------
    LOOP AT itab_glcondctx.
         itab_glcondctx-vtweg = 'Z0'.
         modify itab_glcondctx.
    ENDLOOP.
  ELSE.
    "record found
    CLEAR ls_zlsdbn002.
    SELECT SINGLE * from zlsdbn002 INTO ls_zlsdbn002
      WHERE towncode = crec-geca_code(2)
        AND municode = crec-geca_code+2(4).
    LOOP AT itab_glcondctx.
         itab_glcondctx-vtweg = ls_zlsdbn002-vtweg.
         modify itab_glcondctx.
    ENDLOOP.
   "----------------
  ENDIF.

  IF w_ctype_not_found_flag <> 'Y'.
    PERFORM process-split-records.
    IF w_ctype_not_found_flag <> 'Y'.
      PERFORM print-split-detail.
    ENDIF.
  ENDIF.

ENDFORM.                    "MAP_RATERIDER

************************************************************************
* Determine the transaction amt for each split.
************************************************************************
FORM process-split-records.

  REFRESH itab_condtype.
  CLEAR   itab_condtype.

  sum_trans_amt = 0.

  LOOP AT itab_glcondctx.

    CLEAR wa_A032.

    CONCATENATE crec-app_yr crec-app_mth '01' INTO w_effdate.

    SELECT SINGLE knumh vtweg kschl datab datbi
      INTO wa_A032
      FROM A032
     WHERE kschl = itab_glcondctx-kschl
       AND vtweg = itab_glcondctx-vtweg
       AND datab <= w_effdate
       AND datbi >= w_effdate.

    IF sy-subrc = 0.
      SELECT * FROM konp
       WHERE knumh = wa_A032-knumh
         AND kschl = itab_glcondctx-kschl
         AND loevm_ko <> 'X'.

        IF sy-subrc = 0.
          MOVE wa_A032-knumh to itab_condtype-knumh.
          MOVE wa_A032-vtweg to itab_condtype-vtweg.
          MOVE wa_A032-kschl to itab_condtype-kschl.
          MOVE wa_A032-datab to itab_condtype-datab.
          MOVE wa_A032-datbi to itab_condtype-datbi.
          MOVE konp-kbetr to itab_condtype-kbetr.
          MOVE konp-kpein to itab_condtype-kpein.
          MOVE konp-kmein to itab_condtype-kmein.
          itab_condtype-vtext = itab_glcondctx-vtext.
          itab_condtype-cons = w_cons.
          itab_condtype-amt = ( w_cons / itab_condtype-kpein ) *
                              itab_condtype-kbetr.
          APPEND itab_condtype.
          PERFORM load_condtypes.
          CLEAR itab_condtype.
        ELSE.
          MOVE-CORRESPONDING crec TO err_file.
          MOVE w_trans_amt TO err_file-amount.
          CONCATENATE crec-app_yr crec-app_mth '01'
                 INTO err_file-eff_date.
          MOVE 'No cond. type detail records found in table A032/KONP.'
           TO err_file-err_msg.
          APPEND err_file.
          MOVE 'Y' TO w_errfile_created_flag.
          MOVE 'Y' TO w_ctype_not_found_flag.
        ENDIF.                                              "SDP41704
      ENDSELECT.                                            "SDP41704
    ELSE.                                                   "SDP41704
      MOVE-CORRESPONDING crec TO err_file.
      MOVE w_trans_amt TO err_file-amount.
      CONCATENATE crec-app_yr crec-app_mth '01'
             INTO err_file-eff_date.
      MOVE 'No cond. type detail records found in table A032/KONP.'
           TO err_file-err_msg.
      APPEND err_file.
      MOVE 'Y' TO w_errfile_created_flag.
      MOVE 'Y' TO w_ctype_not_found_flag.
    ENDIF.                                                  "SDP41704
  ENDLOOP.

ENDFORM.                    "PROCESS-SPLIT-RECORDS

************************************************************************
*  This routine creates a summary table by Condition Type code and
*  effective date.  Amounts are totaled for each cond. type / effective
*  date and those amount totals are summed up by condition type code.
************************************************************************
FORM load_condtypes.

  READ TABLE splttab WITH KEY kschl = itab_condtype-kschl
                              vtweg = itab_condtype-vtweg
                              datab = itab_condtype-datab.

* If there is a match in table splttab, then add itab_condtype
* amounts to the splttab record (sum up)
* If no match, then a new record is added to splttab.

  IF sy-subrc = 0.
* sum up dollar amounts.
    MOVE splttab-amt TO va_amt.
    ADD itab_condtype-amt TO va_amt.
    MOVE va_amt TO splttab-amt.

    MODIFY splttab INDEX sy-tabix TRANSPORTING amt.
    CLEAR splttab.
  ELSE.
*  no match in splttab. Add itab_condtype to splttab.
    IF itab_condtype-amt = '0'.
    ELSE.
      MOVE-CORRESPONDING itab_condtype TO splttab.
      APPEND splttab. CLEAR splttab.
    ENDIF.
  ENDIF.
ENDFORM.                    "load_condtypes

************************************************************************
* Print each input record and it's corresponding condition splits on
* the detail report.
************************************************************************
FORM print-split-detail.

* Print original Input record.

  CONCATENATE crec-non_rate_item_typ '/'
              crec-serv_type '/'
              crec-cust_type '/'
              crec-rate_type '/'
              crec-charge_type '/'
              crec-sr_usage '/'
              crec-st_subtype
         INTO w_desc.

  FORMAT INTENSIFIED ON.
  WRITE: /     w_desc UNDER text-004,
          (13) w_cons UNDER text-006 DECIMALS 3,
          (10) w_effdate UNDER text-010 USING
                                 EDIT MASK '____/__/__',
          (13) w_trans_amt UNDER text-011 DECIMALS 2.
  SKIP.
  ln_cntr = ln_cntr + 2.
  ADD w_trans_amt TO w_file_tot_trans_amt.
  FORMAT INTENSIFIED OFF.

* Print component splits

  LOOP AT itab_condtype.
    WRITE: /     itab_condtype-kschl  UNDER text-004,
                 itab_condtype-vtweg  UNDER text-024,
                 itab_condtype-kschl  UNDER text-005,
            (13) itab_condtype-cons   UNDER text-006,
                 itab_condtype-kbetr  UNDER text-007,
            (10) itab_condtype-datab  UNDER text-010 USING
                                      EDIT MASK '____/__/__',
            (13) itab_condtype-amt    UNDER text-011.
    sum_trans_amt = sum_trans_amt + itab_condtype-amt.
    ln_cntr = ln_cntr + 1.
  ENDLOOP.

  SKIP.
  w_variance = w_trans_amt - sum_trans_amt.
  WRITE: /46   'Component Total Amt',
          (13) sum_trans_amt UNDER text-011,
          111  'Variance: ',
          (10)  w_variance.

  ADD sum_trans_amt TO w_grand_tot_trans_amt.
  SKIP.
  ln_cntr = ln_cntr + 3.

ENDFORM.                    "PRINT-AND-WRITE-SPLIT-DETAIL

************************************************************************
* Generate Split Report header
************************************************************************
FORM write_reconc_hdg.

  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 36 text-002.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.

*  WRITE: /44 'File Process Date:'.
*  WRITE (10) crec-process_date USING EDIT MASK '____/__/__'.
  WRITE: /121 text-pge, sy-pagno.
  SKIP.
  WRITE: /3 text-003, 87 text-009.
*  WRITE: /3 text-004, 13 text-005, 35 text-006, 53 text-007,
*            79 text-010, 95 text-011.
  WRITE: /3 text-004, 15 text-024, 22 text-005, 43 text-006, 61 text-007,
            87 text-010, 103 text-011.
  SKIP.
  MOVE '6' TO ln_cntr.
  FORMAT INTENSIFIED OFF.

ENDFORM.                    "WRITE_RECONC_HDG

************************************************************************
* Generate Summary by Split Code Report
************************************************************************
FORM generate_split_sum_rpt.

  PERFORM write_sum_hdg.
* Loop through SPLTTAB and generate a summary line for each split code &
* rate with non-zero amounts.

  SORT splttab.
  LOOP AT splttab.
    IF ln_cntr >= 59.
      PERFORM write_sum_hdg.
    ENDIF.
* Print summed amount by split code for the previous code.
    IF NOT splttab-kschl = prev_kschl AND
       NOT prev_kschl IS INITIAL AND
       NOT sum_amt_by_split_cd IS INITIAL.
      WRITE (15) sum_amt_by_split_cd UNDER text-021.
      SKIP.
      MOVE 0 TO sum_amt_by_split_cd.
      MOVE splttab-kschl TO prev_kschl.
    ENDIF.

    IF splttab-amt <> 0.
      WRITE /(4)  splttab-kschl UNDER text-018.
      WRITE  (20) splttab-vtext UNDER text-005.
      WRITE  (15) splttab-kbetr UNDER text-007.
      WRITE  (10) splttab-datab UNDER text-009.
      WRITE  (15)  splttab-amt  UNDER text-019.
      ln_cntr = ln_cntr + 1.
      ADD splttab-amt TO split_sum_amt.
      ADD splttab-amt TO sum_amt_by_split_cd.
      MOVE splttab-kschl TO prev_kschl.
    ENDIF.
  ENDLOOP.

  IF NOT sum_amt_by_split_cd IS INITIAL.
    WRITE (15) sum_amt_by_split_cd UNDER text-021.
  ENDIF.
  SKIP 2.

  WRITE /60 'SPLIT SUMMARY GRAND TOTAL'.
  WRITE (15) split_sum_amt UNDER text-021.
  ln_cntr = ln_cntr + 2.

  PERFORM generate_zeroamt_report.

ENDFORM.                    "GENERATE_SPLIT_SUM_RPT

************************************************************************
* Generate Summary by Split Code Report Header
************************************************************************

FORM write_sum_hdg.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 41 text-016.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /50 sy-datum.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.
  WRITE: /3 text-017, 57 text-009, 100 text-020.
  WRITE: /3 text-018, 15 text-005, 30 text-007, 57 text-010,
         77 text-019, 100 text-021 .
  SKIP.
  MOVE '6' TO ln_cntr.
  FORMAT INTENSIFIED OFF.
ENDFORM.                    "WRITE_SUM_HDG

************************************************************************
* Generate Banner Zero Amt Record Listing
************************************************************************
FORM generate_zeroamt_report.

  PERFORM write_zeroamt_hdg.

  LOOP AT intab.
    IF ln_cntr >= 59.
      PERFORM write_zeroamt_hdg.
    ENDIF.

    CLEAR w_desc.
    CONCATENATE intab-non_rate_item_typ '/'
                intab-serv_type '/'
                intab-cust_type '/'
                intab-rate_type '/'
                intab-charge_type '/'
                intab-sr_usage '/'
                intab-st_subtype
           INTO w_desc.

    WRITE: /    w_desc            UNDER text-005,
           (13) intab-amount      UNDER text-011,
           (14) intab-volume      UNDER text-023,
           (10) intab-eff_date    UNDER text-010 USING
                                  EDIT MASK '____/__/__'.
    ln_cntr = ln_cntr + 1.
  ENDLOOP.

ENDFORM.                    "generate_zeroamt_report

************************************************************************
* Generate Banner Zero Amt Record Listing Header
************************************************************************

FORM write_zeroamt_hdg.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 41 text-022.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /50 sy-datum.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.
  WRITE: /78 text-009.
  WRITE: /3 text-005, 40 text-011, 57 text-023, 78 text-010.
  SKIP.
  MOVE '6' TO ln_cntr.
  FORMAT INTENSIFIED OFF.
ENDFORM.                    "write_zeroamt_hdg

************************************************************************
* Generate Error Report
************************************************************************
FORM generate_err_rpt.

  PERFORM write_err_hdg.
  IF w_errfile_created_flag = 'Y'.
    LOOP AT err_file.
      IF ln_cntr >= 59.
        PERFORM write_err_hdg.
      ENDIF.

      CLEAR w_desc.
      CONCATENATE err_file-non_rate_item_typ '/'
                  err_file-serv_type '/'
                  err_file-cust_type '/'
                  err_file-rate_type '/'
                  err_file-charge_type '/'
                  err_file-sr_usage '/'
                  err_file-st_subtype
             INTO w_desc.

      WRITE: /    w_desc               UNDER text-005,
             (13) err_file-amount      UNDER text-011,
             (10) err_file-eff_date    UNDER text-010 USING
                                       EDIT MASK '____/__/__',
                   err_file-err_msg    UNDER text-015.
      ln_cntr = ln_cntr + 1.
    ENDLOOP.
  ELSE.
    WRITE 43 '*** No errors to report ***'.
  ENDIF.
ENDFORM.                    "GENERATE_ERR_RPT

************************************************************************
* Generate Error Report Header
************************************************************************

FORM write_err_hdg.
  NEW-PAGE.
  CLEAR ln_cntr.
  FORMAT INTENSIFIED ON.
  WRITE: /1 text-001, 41 text-012.
  WRITE: 106 text-dte, sy-datum, text-amp, sy-uzeit.
  WRITE: /50 sy-datum.
  WRITE: 121 text-pge, sy-pagno.
  SKIP.
  WRITE: /57 text-009.
  WRITE: /3 text-005, 40 text-011, 57 text-010, 72 text-015.
  SKIP.
  MOVE '6' TO ln_cntr.
  FORMAT INTENSIFIED OFF.
ENDFORM.                    "WRITE_ERR_HDG
