FUNCTION zbapi_vendor_list_lookup.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_NAME1) TYPE  NAME1_GP
*"     VALUE(IV_NAME2) TYPE  NAME2_GP OPTIONAL
*"     VALUE(IV_POSTAL_CODE) TYPE  PSTLZ OPTIONAL
*"     VALUE(IV_COMPANYCODE) TYPE  BAPIVENDOR_01-COMP_CODE DEFAULT
*"       'UGL'
*"  EXPORTING
*"     VALUE(ET_GENERALDETAIL) TYPE  ZFI_VENDOR_GENDATA_TAB
*"  EXCEPTIONS
*"      NO_RECORD_FOUND
*"----------------------------------------------------------------------

  DATA: lv_where TYPE string,
        lv_option(2),
        lv_counter TYPE i,
        lt_where LIKE TABLE OF lv_where,
        lt_lfa1 TYPE TABLE OF lfa1,
        lt_lfb1 TYPE TABLE OF lfb1,
        ls_lfa1 TYPE lfa1,
        ls_lfb1 TYPE lfb1,
        ls_gd TYPE zfi_vendor_gendata,
        lv_ktokk TYPE lfa1-ktokk VALUE 'VNDR'.

  DATA: lr_name1  TYPE RANGE OF lfa1-name1 WITH HEADER LINE,
        lr_name2  TYPE RANGE OF lfa1-name2 WITH HEADER LINE,
        lr_pstlz  TYPE RANGE OF lfa1-pstlz WITH HEADER LINE.

  CLEAR et_generaldetail.
  IF iv_companycode IS INITIAL.
    iv_companycode = 'UGL'.
  ENDIF.
  "---------------
  FIND '*' IN iv_name1.
  IF sy-subrc = 0.
    lv_option = 'CP'.
  ELSE.
    FIND '+' IN iv_name1.
    IF sy-subrc = 0.
      lv_option = 'CP'.
    ELSE.
      lv_option = 'EQ'.
    ENDIF.
  ENDIF.
  lr_name1-sign   = 'I'.
  lr_name1-option = lv_option.
  lr_name1-low    = iv_name1.
  APPEND lr_name1.
  lr_name1-sign   = 'I'.
  lr_name1-option = lv_option.
  lr_name1-low    = to_upper( iv_name1 ).
  APPEND lr_name1.
  lr_name1-sign   = 'I'.
  lr_name1-option = lv_option.
  lr_name1-low    = to_lower( iv_name1 ).
  APPEND lr_name1.
  "------------------
  IF iv_name2 IS NOT INITIAL.
    FIND '*' IN iv_name2.
    IF sy-subrc = 0.
      lv_option = 'CP'.
    ELSE.
      FIND '+' IN iv_name2.
      IF sy-subrc = 0.
        lv_option = 'CP'.
      ELSE.
        lv_option = 'EQ'.
      ENDIF.
    ENDIF.
    lr_name2-sign   = 'I'.
    lr_name2-option = lv_option.
    lr_name2-low    = iv_name2.
    APPEND lr_name2.
    lr_name2-sign   = 'I'.
    lr_name2-option = lv_option.
    lr_name2-low    = to_upper( iv_name2 ).
    APPEND lr_name2.
    lr_name2-sign   = 'I'.
    lr_name2-option = lv_option.
    lr_name2-low    = to_lower( iv_name2 ).
    APPEND lr_name2.
  ENDIF.
  "------------------
  IF iv_postal_code IS NOT INITIAL.
    iv_postal_code = to_upper( iv_postal_code ).
    FIND '*' IN iv_postal_code.
    IF sy-subrc = 0.
      lv_option = 'CP'.
    ELSE.
      FIND '+' IN iv_postal_code.
      IF sy-subrc = 0.
        lv_option = 'CP'.
      ELSE.
        lv_option = 'EQ'.
      ENDIF.
    ENDIF.
    lr_pstlz-sign   = 'I'.
    lr_pstlz-option = lv_option.
    lr_pstlz-low    = iv_postal_code.
    APPEND lr_pstlz.
  ENDIF.
  "---------------------Dynamic Where Clause
  lv_where = ' name1 IN lr_name1 '.
  APPEND lv_where TO lt_where.
  lv_where = 'AND ktokk = lv_ktokk '.
  APPEND lv_where TO lt_where.
*  lv_where = ' AND LOEVM = space '.
*  APPEND lv_where TO lt_where.
  IF iv_name2 IS NOT INITIAL.
    lv_where = 'AND name2 IN lr_name2 '.
    APPEND lv_where TO lt_where.
  ENDIF.
  IF iv_postal_code IS NOT INITIAL.
    lv_where = 'AND pstlz IN lr_pstlz '.
    APPEND lv_where TO lt_where.
  ENDIF.
  SELECT  * FROM lfa1 INTO TABLE lt_lfa1
    WHERE  (lt_where).
  IF sy-subrc <> 0.
    RAISE no_record_found.
  ENDIF.

  IF iv_companycode IS NOT INITIAL.
    SELECT * FROM lfb1 INTO TABLE lt_lfb1
      FOR ALL ENTRIES IN lt_lfa1
      WHERE lifnr = lt_lfa1-lifnr
        AND bukrs = iv_companycode.
    "AND sperr = space.
    IF sy-subrc <> 0.
      RAISE no_record_found.
    ENDIF.
  ENDIF.
  LOOP AT lt_lfa1 INTO ls_lfa1.
    CLEAR ls_lfb1.
    READ TABLE lt_lfb1 INTO ls_lfb1 WITH KEY lifnr = ls_lfa1-lifnr.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    lv_counter = lv_counter + 1.
    IF lv_counter > 50.
      EXIT.
    ENDIF.
    ls_gd-lifnr = ls_lfa1-lifnr.
    ls_gd-name1 = ls_lfa1-name1.
    ls_gd-name2 = ls_lfa1-name2.
    ls_gd-stras = ls_lfa1-stras.
    ls_gd-mcod3 = ls_lfa1-mcod3.
    ls_gd-regio = ls_lfa1-regio.
    ls_gd-pstlz = ls_lfa1-pstlz.
    IF ls_lfb1-sperr IS NOT INITIAL.
      ls_gd-sperr = ls_lfb1-sperr.
    ELSE.
      ls_gd-sperr = ls_lfa1-sperr.
    ENDIF.
    IF ls_lfa1-loevm IS NOT INITIAL.
      ls_gd-sperr = ls_lfa1-loevm.
    ENDIF.
    ls_gd-bukrs = ls_lfb1-bukrs.
    APPEND ls_gd TO et_generaldetail.
  ENDLOOP.
  IF et_generaldetail[] IS INITIAL.
    RAISE no_record_found.
  ENDIF.

ENDFUNCTION.
