*&---------------------------------------------------------------------*
*&  Include           ZFGLI007_ROUTINES
*&---------------------------------------------------------------------*
*&
*& This include contains all the subroutines called in the main program
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_FILE
*&---------------------------------------------------------------------*
* Reads input file from application server, populates the input itab
*----------------------------------------------------------------------*
FORM READ_FILE .
  data: lv_msg(100),
        lv_inrec(250).

  OPEN DATASET p_infile FOR INPUT IN TEXT MODE ENCODING DEFAULT WITH SMART LINEFEED.

  IF sy-subrc <> 0.
    MESSAGE E006 WITH p_infile.
  ENDIF.

DO.
  READ DATASET p_infile INTO lv_inrec.

    IF sy-subrc = 0.
      split lv_inrec at '|' INTO
                gs_input-post_date
                gs_input-doc_date
                gs_input-doc_type
                gs_input-tax_code
                gs_input-profit_cntr
                gs_input-acc_class
                gs_input-gl_ac
                gs_input-recon_key
                gs_input-post_key
                gs_input-amount.

      IF sy-subrc <> 0.
        MESSAGE  'Error reading file... check the file format.' TYPE 'E'.
      ENDIF.

      REPLACE ALL OCCURRENCES OF '"' in gs_input-doc_type WITH ''.
      CONDENSE gs_input-doc_type.

      REPLACE ALL OCCURRENCES OF '"' in gs_input-tax_code WITH ''.
      CONDENSE gs_input-tax_code.

      REPLACE ALL OCCURRENCES OF '"' in gs_input-profit_cntr WITH ''.
      CONDENSE gs_input-profit_cntr.

      REPLACE ALL OCCURRENCES OF '"' in gs_input-acc_class WITH ''.
      CONDENSE gs_input-acc_class.

      REPLACE ALL OCCURRENCES OF '"' in gs_input-gl_ac WITH ''.
      CONDENSE gs_input-gl_ac.

      REPLACE ALL OCCURRENCES OF '"' in gs_input-recon_key WITH ''.
      CONDENSE gs_input-recon_key.

      REPLACE ALL OCCURRENCES OF '"' in gs_input-post_key WITH ''.
      CONDENSE gs_input-post_key.

      APPEND gs_input to gt_input.
      clear lv_inrec.

    ELSE.
      EXIT.

    ENDIF.

ENDDO.

CLOSE DATASET p_infile.
SORT gt_input ASCENDING BY post_date doc_date.

ENDFORM.                    " READ_FILE
*&---------------------------------------------------------------------*
*&      Form  DATA_TRANSLATION
*&---------------------------------------------------------------------*
* Maps all data and creates offset entries
*----------------------------------------------------------------------*
FORM DATA_TRANSLATION .

  DATA: lv_len_gl TYPE i,
        lv_len_pc TYPE i,

        lv_prctr  TYPE prctr,
        lv_cis_gl TYPE hkont,

        map_error  TYPE c,
        copa_error TYPE c.

  CONSTANTS lc_compare TYPE DEC5_2  VALUE '0.00'.

  PERFORM init_structures.

  LOOP AT gt_input INTO gs_input.

    CLEAR: gs_bbseg_sort, lv_len_gl, lv_len_pc, lv_prctr, lv_cis_gl, gv_saknr, copa_error, map_error, gs_cskb.
    MOVE-CORRESPONDING gs_bbseg TO gs_bbseg_sort.             "initializing the sort structure

    gs_bbseg_sort-budat = gs_input-post_date.   "posting key
    gs_bbseg_sort-bldat = gs_input-doc_date.    "document date
    gs_bbseg_sort-stype = 2.
    gs_bbseg_sort-tbnam = 'ZBSEG'.
    gs_bbseg_sort-wrbtr = abs( gs_input-amount ). "amount
    CONDENSE: gs_bbseg_sort-wrbtr.

    gs_bbseg_sort-zuonr = gs_input-recon_key.   "assignment number = reconciliation key

    lv_cis_gl = gs_input-gl_ac.
    SHIFT lv_cis_gl LEFT DELETING LEADING '0'.
    lv_len_gl = strlen( lv_cis_gl ).

    lv_prctr  = gs_input-profit_cntr.
    SHIFT lv_prctr LEFT DELETING LEADING '0'.
    lv_len_pc = strlen( lv_prctr ).

    CONCATENATE lv_cis_gl(lv_len_gl) '-' lv_prctr(lv_len_pc) '-' gs_input-doc_type(2) ' - ' p_bktxt   "item text = CIS GL + profit center + CIS Doc type + default text
    INTO gs_bbseg_sort-sgtxt RESPECTING BLANKS.

    CLEAR gs_map_table1.

    SELECT SINGLE * FROM ZLFICIS01
      INTO gs_map_table1
      WHERE     ZCIS_AC     = gs_input-gl_ac
         AND    BUKRS       = p_bukrs
         AND  ( ZAC_CLASS   = gs_input-acc_class   OR ZAC_CLASS   = '*' )
         AND  ( PRCTR       = gs_input-profit_cntr OR PRCTR = '*' )
         AND  ( ZCIS_TAX_CD = gs_input-tax_code    OR ZCIS_TAX_CD = '*' ).


    IF sy-subrc = 0. "if mapping data was found, fill in the respective fields

      gs_bbseg_sort-hkont = gs_map_table1-saknr.       "GL A/c number

      IF gs_map_table1-zsap_tax_cd IS NOT INITIAL.
        gs_bbseg_sort-mwskz = gs_map_table1-zsap_tax_cd. "Tax code
      ENDIF.

      IF gs_map_table1-kostl IS NOT INITIAL.
        gs_bbseg_sort-kostl = gs_map_table1-kostl.     "Cost center
      ENDIF.

      IF gs_map_table1-aufnr IS NOT INITIAL.
        gs_bbseg_sort-aufnr = gs_map_table1-aufnr.     "Order number
      ENDIF.

    ENDIF.

    IF ( gs_map_table1-saknr IS INITIAL OR  "its possible that a record was found, but with incomplete mapping data
         gs_map_table1-oacct IS INITIAL ).  "if fetched record is incomplete it will be marked as error

      map_error = 'X'.
    ENDIF.


    IF gs_input-amount > lc_compare .            "posting key

      IF gs_bbseg_sort-hkont = p_kunnr.
        gs_bbseg_sort-newbs = '02'.
      ELSE.
        gs_bbseg_sort-newbs = '40'.
      ENDIF.

    ELSE.

      IF gs_bbseg_sort-hkont = p_kunnr.
        gs_bbseg_sort-newbs = '11'.
      ELSE.
        gs_bbseg_sort-newbs = '50'.
      ENDIF.

    ENDIF.

*** filling the COPA fields
    READ TABLE gt_ska1 INTO gv_saknr WITH KEY saknr = gs_map_table1-saknr BINARY SEARCH.

    READ TABLE gt_cskb INTO gs_cskb WITH KEY kstar = gs_map_table1-saknr.

    IF ( gs_map_table1-matnr IS NOT INITIAL AND gv_saknr IS NOT INITIAL
       AND map_error IS INITIAL AND gs_cskb-katyp = '11' ). "Only if COPA check is passed will COPA related fields be populated

      gs_bbseg_sort-vkorg = p_vkorg.    "sales org. default from sel. screen

      CASE gs_input-acc_class.          "service class
        WHEN 'RES'.
          gs_bbseg_sort-wwser = 'RS'.
        WHEN 'IND'.
          gs_bbseg_sort-wwser = 'IN'.
        WHEN 'COM'.
          gs_bbseg_sort-wwser = 'CM'.
        WHEN 'NGAS'.
          gs_bbseg_sort-wwser = 'NG'.
        WHEN OTHERS.
          gs_bbseg_sort-wwser = '???'.
      ENDCASE.

      clear gs_map_table2.
      READ TABLE gt_map_table2 INTO gs_map_table2
                               WITH KEY prctr = gs_input-profit_cntr.

      IF sy-subrc = 0.
        gs_bbseg_sort-wwbrn = gs_map_table2-wwbrn.      "branch
        gs_bbseg_sort-werks = gs_map_table2-werks.      "plant
      ENDIF.

      gs_bbseg_sort-matnr = gs_map_table1-matnr.        "material

      gs_bbseg_sort-kndnr = '1'.                        "Customer
      gs_bbseg_sort-wwsct = 'Z'.                        "sector
      gs_bbseg_sort-wwseg = 'ZZ'.                       "segment
      gs_bbseg_sort-wwdvs = 'Z0'.                       "Division

    ELSEIF ( gs_map_table1-matnr IS INITIAL AND gv_saknr IS NOT INITIAL AND gs_cskb-katyp = '11' )  "If material is not maintained but GL belongs to P&L group and cost element type = 11
                                                                                             "OR
        OR ( gs_map_table1-matnr IS NOT INITIAL AND gv_saknr IS INITIAL ).   "if material is wrongly maintained even though cost element type is not revenue


        copa_error = 'X'.

    ENDIF.
*** end of COPA fields

    IF ( map_error  = 'X'  OR    "If the mapping was unsuccessful or a COPA error flag was raised
         copa_error = 'X' ).     "this record will be marked as an error record and pushed to error file
                                 "otherwise it will be moved to the output file
       CONCATENATE gs_bbseg_sort-sgtxt ' - Error'
              INTO gs_bbseg_sort-sgtxt.

      gs_bbseg_sort-hkont = p_hkont.
      MOVE '/' to gs_bbseg_sort-mwskz.
      APPEND gs_bbseg_sort to gt_bbseg_error.

*** creating offset entry for error
      gs_bbseg_sort-hkont = p_kunnr.

      IF gs_input-amount > lc_compare.
        gs_bbseg_sort-newbs = '11'.
      ELSE.
        gs_bbseg_sort-newbs = '02'.
      ENDIF.

      APPEND gs_bbseg_sort to gt_bbseg_error.
*** end of creating offset entry for error

**** new code added for writing error logs to screen
      clear gs_errlog.
      gs_errlog-gl_acc = gs_input-gl_ac.
      gs_errlog-amount = gs_input-amount.
      SHIFT gs_errlog-gl_acc LEFT DELETING LEADING '0'.
      COLLECT gs_errlog INTO gt_errlog.
**** end of new code for writing error logs

      gv_errorfile = 'X'.
      CONTINUE.

    ELSE.
      APPEND gs_bbseg_sort to gt_bbseg_sort.

    ENDIF.


** creating offset entry
    gs_bbseg_sort-hkont = gs_map_table1-oacct.  "offset GL A/c

    IF gs_map_table1-zsap_otax_cd IS NOT INITIAL.  "offset tax code
      gs_bbseg_sort-mwskz = gs_map_table1-zsap_otax_cd.
    ELSE.
      MOVE '/' to gs_bbseg_sort-mwskz.
    ENDIF.

    MOVE '/' TO: gs_bbseg_sort-kostl, gs_bbseg_sort-aufnr.

    IF gs_input-amount > lc_compare .            "offset posting key

      IF gs_bbseg_sort-hkont = p_kunnr.
        gs_bbseg_sort-newbs = '11'.
      ELSE.
        gs_bbseg_sort-newbs = '50'.
      ENDIF.

    ELSE.

      IF gs_bbseg_sort-hkont = p_kunnr.
        gs_bbseg_sort-newbs = '02'.
      ELSE.
        gs_bbseg_sort-newbs = '40'.
      ENDIF.

    ENDIF.

*** check if Contra GL/Account belongs to P&L GL group
    CLEAR gv_saknr.
    READ TABLE gt_ska1 INTO gv_saknr WITH KEY saknr = gs_map_table1-oacct.

    IF gv_saknr IS INITIAL.           "If contra account does not belong to P&L GL group then set COPA related fields to initial values.

      MOVE '/' TO: gs_bbseg_sort-vkorg,       gs_bbseg_sort-kndnr,
                   gs_bbseg_sort-matnr,       gs_bbseg_sort-werks,
                   gs_bbseg_sort-wwser,       gs_bbseg_sort-wwbrn,
                   gs_bbseg_sort-wwsct,       gs_bbseg_sort-wwseg.


    ENDIF.

    APPEND gs_bbseg_sort to gt_bbseg_sort.

  ENDLOOP.

ENDFORM.                    " DATA_TRANSLATION
*&---------------------------------------------------------------------*
*&      Form  INIT_STRUCTURES
*&---------------------------------------------------------------------*
* initializes all the fields to '/' and reads other tables
*----------------------------------------------------------------------*
FORM INIT_STRUCTURES .
DATA: struct_name(5) TYPE c.

struct_name = 'BGR00'.
PERFORM init_nodata USING struct_name.

struct_name = 'BBKPF'.
PERFORM init_nodata USING struct_name.

struct_name = 'BBSEG'.
PERFORM init_nodata USING struct_name.


SELECT * FROM ZLSDCIS02 INTO TABLE gt_map_table2.

SELECT    saknr
  FROM    ska1
  INTO TABLE gt_ska1
  WHERE ktopl = 'COAT' AND ktoks = 'PL'.

SORT gt_ska1 ASCENDING.

SELECT kstar katyp kostl aufnr
  INTO TABLE gt_cskb
  FROM cskb FOR ALL ENTRIES IN gt_ska1
  WHERE kstar = gt_ska1-saknr AND kokrs = '10' AND datbi = '99991231'.

ENDFORM.                    " INIT_STRUCTURES
*&---------------------------------------------------------------------*
*&      Form  INIT_NODATA
*&---------------------------------------------------------------------*
*  initializes all the fields to '/'
*----------------------------------------------------------------------*
*      -->P_STRUCT_NAME  Name of structure to be initialized
*----------------------------------------------------------------------*
FORM INIT_NODATA  USING  P_STRUCT_NAME.
  DATA: field_text(25),
        tab_name(10).

  IF p_struct_name = 'BBSEG'.
    tab_name = 'ZPA_BSEGGS'.
  ELSE.
    tab_name = p_struct_name.
  ENDIF.

  SELECT * FROM dd03l WHERE tabname = tab_name.

    IF dd03l-fieldname = '.INCLUDE'.
      CONTINUE.
    ENDIF.

    CLEAR field_text.

    CONCATENATE 'GS_' p_struct_name '-' dd03l-fieldname INTO field_text.
    ASSIGN (field_text) to <fs1>.
    <fs1> = nodata.

  ENDSELECT.
ENDFORM.                    " INIT_NODATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_OUTPUT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_OUTPUT_FILE .
  DATA: lv_msg(100).
  CLEAR: gs_bbseg_sort.

  OPEN DATASET p_oufile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

  IF sy-subrc <> 0.
    MESSAGE E006 WITH p_oufile.
  ENDIF.

  PERFORM build_bgr00 USING 'S'.        "populate the BDC session data for successfully mapped data
  TRANSFER outrec to p_oufile.          "and move to output file
  CLEAR outrec.

  LOOP AT gt_bbseg_sort INTO gs_bbseg_sort.

      AT NEW budat.
        line_count = 0.
      ENDAT.

      IF line_count = 0.
        PERFORM build_bbkpf ."USING 'S'.  "Create a new doc header for successfully mapped data
        TRANSFER outrec to p_oufile.    "and move to output file
        CLEAR outrec.
      ENDIF.

      MOVE-CORRESPONDING gs_bbseg_sort TO gs_bbseg.
      MOVE gs_bbseg to outrec.        "move the line items to output file
      TRANSFER outrec to p_oufile.
      CLEAR outrec.
      ADD 1 TO line_count.

      IF line_count = 900.            "max 900 line items per document
        line_count = 0.
      ENDIF.

  ENDLOOP.

  CLOSE DATASET p_oufile.

  IF sy-subrc = 0.
      MESSAGE S100 WITH 'Output file successfully generated'.
  ELSE.
      CONCATENATE 'Unable to close output file ' p_oufile INTO lv_msg.
      MESSAGE lv_msg TYPE 'E' .
  ENDIF.

  IF gv_errorfile = 'X'.
    PERFORM create_error_file.
  ENDIF.

ENDFORM.                    " CREATE_OUTPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  BUILD_BGR00
*&---------------------------------------------------------------------*
*   build BDC session record
*----------------------------------------------------------------------*
FORM BUILD_BGR00 USING P_REC_TYPE.

  CLEAR outrec.

  gs_bgr00-stype = 0.

  IF p_rec_type = 'S'.        "successfully mapped
    gs_bgr00-group = p_group .

  ELSEIF p_rec_type = 'E'.    "error records
    CONCATENATE p_group '-ERR' "p_blart
    INTO gs_bgr00-group .

  ENDIF.

  gs_bgr00-mandt = sy-mandt.
  gs_bgr00-usnam = 'BATCH'.

  MOVE gs_bgr00 to outrec.

  CLEAR gs_bgr00.
  PERFORM INIT_NODATA USING 'BGR00'.

ENDFORM.                    " BUILD_BGR00
*&---------------------------------------------------------------------*
*&      Form  BUILD_BBKPF
*&---------------------------------------------------------------------*
*   build document header record
*----------------------------------------------------------------------*
FORM BUILD_BBKPF ."USING P_REC_TYPE.

  CLEAR outrec.

  gs_bbkpf-stype = 1.
  gs_bbkpf-tcode = 'FB01'.
  gs_bbkpf-bldat = gs_bbseg_sort-bldat.
  gs_bbkpf-blart = p_blart.
  gs_bbkpf-bukrs = p_bukrs.
  gs_bbkpf-budat = gs_bbseg_sort-budat.
  gs_bbkpf-waers = p_waers.
  gs_bbkpf-xblnr = p_xblnr.

*  IF p_rec_type = 'S'.        "successfully mapped.
    gs_bbkpf-bktxt = p_bktxt.

*  ELSEIF p_rec_type = 'E'.    "error records
*    gs_bbkpf-bktxt = p_bktxt.
*
*  ENDIF.

  MOVE gs_bbkpf to outrec.

  CLEAR gs_bbkpf.
  PERFORM INIT_NODATA USING 'BBKPF'.

ENDFORM.                    " BUILD_BBKPF
*&---------------------------------------------------------------------*
*&      Form  CREATE_ERRORFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_ERROR_FILE .
  DATA: lv_msg(100).

OPEN DATASET p_erfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      MESSAGE E006 WITH p_erfile.
    ENDIF.

    CLEAR: gs_bbseg_sort, gs_bbseg, line_count.

    PERFORM build_bgr00 USING 'E'.        "populate the BDC session data for error file
    TRANSFER outrec to p_erfile.          "and move to the file
    CLEAR outrec.

    LOOP AT gt_bbseg_error INTO gs_bbseg_sort.

      AT NEW budat.
        line_count = 0.
      ENDAT.

      IF line_count = 0.
        PERFORM build_bbkpf ."USING 'E'.          "Create a new doc header for error file
        TRANSFER outrec to p_erfile.            "and move to the file
        CLEAR outrec.
      ENDIF.

      MOVE-CORRESPONDING gs_bbseg_sort TO gs_bbseg.

      MOVE gs_bbseg to outrec.
      TRANSFER outrec TO p_erfile.
      CLEAR outrec.
      ADD 1 TO line_count.

      IF line_count = 900.            "max 900 line items per document
        line_count = 0.
      ENDIF.

    ENDLOOP.

    CLOSE DATASET p_erfile.
    IF sy-subrc = 0.
*** revised code for writing error logs to screen
      CLEAR gs_errlog.
      WRITE / 'Error file generated with the following GLs'.
      WRITE:/ 'GL Account', 25 'Amount'.
      LOOP AT gt_errlog INTO gs_errlog.
        WRITE:/ gs_errlog-gl_acc, 20 gs_errlog-amount.
      ENDLOOP.
*** end of revised code
    ELSE.
      CONCATENATE 'Unable to close error file ' p_oufile INTO lv_msg.
      MESSAGE lv_msg TYPE 'E' .
    ENDIF.
ENDFORM.                    " CREATE_ERRORFILE
