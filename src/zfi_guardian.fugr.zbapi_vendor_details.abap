FUNCTION zbapi_vendor_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_VENDORNO) TYPE  BAPIVENDOR_01-VENDOR_NO
*"     VALUE(IV_COMPANYCODE) TYPE  BAPIVENDOR_01-COMP_CODE DEFAULT
*"       'UGL'
*"  EXPORTING
*"     VALUE(ES_GENERALDETAIL) TYPE  ZFI_VENDOR_GENDATA
*"  EXCEPTIONS
*"      NO_RECORD_FOUND
*"----------------------------------------------------------------------

  DATA: ls_lfa1 TYPE lfa1,
        ls_lfb1 TYPE lfb1,
        lv_vndr TYPE lfa1-ktokk VALUE 'VNDR'.

  IF iv_companycode IS INITIAL.
    iv_companycode = 'UGL'.
  ENDIF.
  SELECT SINGLE * FROM lfa1 INTO ls_lfa1
    WHERE lifnr = iv_vendorno
      AND ktokk = lv_vndr.
  "AND sperr = space
  "AND loevm = space.
  IF sy-subrc <> 0.
    RAISE no_record_found.
  ENDIF.
  IF iv_companycode IS NOT INITIAL.
    SELECT SINGLE * FROM lfb1 INTO ls_lfb1 WHERE lifnr = iv_vendorno
                                             AND bukrs = iv_companycode.
    "AND sperr = space.
    IF sy-subrc <> 0.
      RAISE no_record_found.
    ENDIF.
  ENDIF.
  es_generaldetail-lifnr = ls_lfa1-lifnr.
  es_generaldetail-name1 = ls_lfa1-name1.
  es_generaldetail-name2 = ls_lfa1-name2.
  es_generaldetail-stras = ls_lfa1-stras.
  es_generaldetail-mcod3 = ls_lfa1-mcod3.
  es_generaldetail-regio = ls_lfa1-regio.
  es_generaldetail-pstlz = ls_lfa1-pstlz.
  "es_generaldetail-sperr = ls_lfb1-sperr.
  IF ls_lfb1-sperr IS NOT INITIAL.
    es_generaldetail-sperr = ls_lfb1-sperr.
  ELSE.
    es_generaldetail-sperr = ls_lfa1-sperr.
  ENDIF.
  IF ls_lfa1-loevm IS NOT INITIAL.
    es_generaldetail-sperr = ls_lfa1-loevm.
  ENDIF.
  IF ls_lfb1-bukrs IS NOT INITIAL.
    es_generaldetail-bukrs = ls_lfb1-bukrs.
  ELSE.
    es_generaldetail-bukrs = iv_companycode.
  ENDIF.

ENDFUNCTION.
