FUNCTION zbapi_io_actual_cost.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_GROUPNAME) TYPE  BAPICO_GROUP-GROUPNAME
*"     VALUE(IV_FISCALYEAR) TYPE  GJAHR
*"  EXPORTING
*"     VALUE(ET_IO_DATA) TYPE  ZFI_IO_DATA_TAB
*"  EXCEPTIONS
*"      NO_IO_ASSIGNED_TO_GROUP
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_data,
            group_name  TYPE  bapiset_groupname,
            current_date  TYPE  datum,
            aufnr TYPE  aufnr,
            budget_year TYPE  gjahr,
            primary_cost_element  TYPE  kstar,
            pd1_actuals TYPE  wkgxxx,
            pd2_actuals TYPE  wkgxxx,
            pd3_actuals TYPE  wkgxxx,
            pd4_actuals TYPE  wkgxxx,
            pd5_actuals TYPE  wkgxxx,
            pd6_actuals TYPE  wkgxxx,
            pd7_actuals TYPE  wkgxxx,
            pd8_actuals TYPE  wkgxxx,
            pd9_actuals TYPE  wkgxxx,
            pd10_actuals  TYPE  wkgxxx,
            pd11_actuals  TYPE  wkgxxx,
            pd12_actuals  TYPE  wkgxxx,
         END OF ty_data.
  DATA: lv_error TYPE xfeld,
        lv_tabix TYPE sy-tabix,
        lv_aufnr1 TYPE aufk-aufnr,
        lv_aufnr2 TYPE aufk-aufnr,
        lv_actuals1 TYPE  wkgxxx,
        lv_actuals2 TYPE  wkgxxx,
        lv_actuals3 TYPE  wkgxxx,
        lv_actuals4 TYPE  wkgxxx,
        lv_actuals5 TYPE  wkgxxx,
        lv_actuals6 TYPE  wkgxxx,
        lv_actuals7 TYPE  wkgxxx,
        lv_actuals8 TYPE  wkgxxx,
        lv_actuals9 TYPE  wkgxxx,
        lv_actuals10 TYPE  wkgxxx,
        lv_actuals11 TYPE  wkgxxx,
        lv_actuals12 TYPE  wkgxxx,
        lt_return TYPE TABLE OF bapiret2,
        ls_return TYPE bapiret2,
        lt_hnodes TYPE TABLE OF bapiset_hier,
        ls_hnodes TYPE bapiset_hier,
        lt_hvalues TYPE TABLE OF bapi1117_values,
        ls_hvalues TYPE bapi1117_values,
        lt_setleaf TYPE TABLE OF setleaf,
        lt_setleaf1 TYPE TABLE OF setleaf,
        ls_setleaf TYPE setleaf,
        lt_cosp TYPE TABLE OF cosp,
        ls_cosp TYPE cosp,
        lt_aufk TYPE TABLE OF aufk,
        ls_aufk TYPE aufk,
        ls_io_data TYPE ty_data, "zfi_io_data,
        lt_io_data TYPE TABLE OF ty_data, "zfi_io_data,
        ls_io_data1 TYPE zfi_io_data,
        lt_io_data1 TYPE TABLE OF zfi_io_data.
  DATA: lr_aufnr  TYPE RANGE OF aufk-aufnr WITH HEADER LINE,
        lr_setname TYPE RANGE OF setleaf-setname WITH HEADER LINE.
  CONSTANTS: lc_0103 TYPE setclass VALUE '0103',
             lc_versn TYPE versn VALUE '000',
             lc_wrttp_4 TYPE co_wrttp VALUE '04'.

  CLEAR: et_io_data.

  CALL FUNCTION 'BAPI_INTERNALORDRGRP_GETDETAIL'
    EXPORTING
      groupname       = iv_groupname
    IMPORTING
      return          = ls_return
    TABLES
      hierarchynodes  = lt_hnodes
      hierarchyvalues = lt_hvalues.

  IF ls_return-type = 'E' OR
     ls_return-type = 'A'.
    CONCATENATE 'No Internal Orders Assigned to Group '
                     iv_groupname INTO ls_return-message.
    EXIT.
    "raise error.
  ENDIF.
  "----------
  CLEAR lr_setname.
  REFRESH lr_setname.
  lr_setname-option = 'EQ'.
  lr_setname-sign = 'I'.
  lr_setname-high = space.
  LOOP AT lt_hnodes INTO ls_hnodes.
    lr_setname-low = ls_hnodes-groupname.
    APPEND lr_setname.
  ENDLOOP.
  "----------
  LOOP AT lt_hvalues INTO ls_hvalues.
    lv_aufnr1 = ls_hvalues-valfrom.
    IF lv_aufnr1 IS INITIAL.
      CONTINUE.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_aufnr1
        IMPORTING
          output = lr_aufnr-low.

    ENDIF.
    lv_aufnr2 = ls_hvalues-valto.
    IF lv_aufnr2 IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_aufnr2
        IMPORTING
          output = lr_aufnr-high.
      lr_aufnr-option = 'BT'.
    ELSE.
      lr_aufnr-option = 'EQ'.
      lr_aufnr-high = space.
    ENDIF.
    lr_aufnr-sign   = 'I'.
    APPEND lr_aufnr.
    CLEAR ls_setleaf.
    IF lr_aufnr-high IS INITIAL.
      SELECT SINGLE * FROM setleaf INTO ls_setleaf
        WHERE setclass = lc_0103
          AND valfrom = lr_aufnr-low
          AND setname IN lr_setname.
      IF sy-subrc = 0.
        APPEND ls_setleaf TO lt_setleaf.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM setleaf INTO ls_setleaf
        WHERE setclass = lc_0103
          AND valfrom >= lr_aufnr-low
          AND valto <= lr_aufnr-high
          AND setname IN lr_setname.
      IF sy-subrc = 0.
        APPEND ls_setleaf TO lt_setleaf.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lr_aufnr[] IS INITIAL.
    ls_return-type = 'E'.
    CONCATENATE 'No Internal Orders Assigned to Group '
                   iv_groupname INTO ls_return-message.
    EXIT.
  ENDIF.
  SELECT * FROM aufk INTO TABLE lt_aufk
    WHERE aufnr IN lr_aufnr.
  IF lt_aufk[] IS INITIAL.
    ls_return-type = 'E'.
    CONCATENATE 'No Internal Orders Assigned to Group '
                   iv_groupname INTO ls_return-message.
    EXIT.
  ENDIF.
  SELECT * FROM cosp INTO TABLE lt_cosp
    FOR ALL ENTRIES IN lt_aufk
    WHERE objnr = lt_aufk-objnr
      AND gjahr = iv_fiscalyear
      AND versn = lc_versn
      AND wrttp = lc_wrttp_4.
  IF lt_cosp[] IS INITIAL.
    ls_return-type = 'E'.
    CONCATENATE 'No Actual Cost data for orders of this Group '
                           iv_groupname INTO ls_return-message.
    EXIT.
  ENDIF.
  LOOP AT lt_cosp INTO ls_cosp.
    CLEAR: ls_setleaf,
           ls_aufk.
    READ TABLE lt_aufk INTO ls_aufk WITH KEY objnr = ls_cosp-objnr.
    lt_setleaf1[] = lt_setleaf[].
    DELETE lt_setleaf1 WHERE valfrom < ls_aufk-aufnr
                          OR valto > ls_aufk-aufnr.
    IF lt_setleaf1[] IS NOT INITIAL.
      READ TABLE lt_setleaf1 INTO ls_setleaf WITH KEY valfrom = ls_aufk-aufnr.
      IF sy-subrc <> 0.
        READ TABLE lt_setleaf1 INTO ls_setleaf WITH KEY valto = ls_aufk-aufnr.
        IF sy-subrc <> 0.
          READ TABLE lt_setleaf1 INTO ls_setleaf INDEX 1.
        ENDIF.
      ENDIF.
    ENDIF.
    "-------------------
    ls_io_data-group_name = ls_setleaf-setname.
    ls_io_data-current_date = sy-datum.
    ls_io_data-aufnr = ls_aufk-aufnr.
    ls_io_data-budget_year = ls_cosp-gjahr.
    ls_io_data-primary_cost_element = ls_cosp-kstar.
    ls_io_data-pd1_actuals = ls_cosp-wkg001.
    ls_io_data-pd2_actuals = ls_cosp-wkg002.
    ls_io_data-pd3_actuals = ls_cosp-wkg003.
    ls_io_data-pd4_actuals = ls_cosp-wkg004.
    ls_io_data-pd5_actuals = ls_cosp-wkg005.
    ls_io_data-pd6_actuals = ls_cosp-wkg006.
    ls_io_data-pd7_actuals = ls_cosp-wkg007.
    ls_io_data-pd8_actuals = ls_cosp-wkg008.
    ls_io_data-pd9_actuals = ls_cosp-wkg009.
    ls_io_data-pd10_actuals = ls_cosp-wkg010.
    ls_io_data-pd11_actuals = ls_cosp-wkg011.
    ls_io_data-pd12_actuals = ls_cosp-wkg012.
    "------------------
    APPEND ls_io_data TO lt_io_data. "et_io_data.
  ENDLOOP.
  SORT lt_io_data BY group_name aufnr primary_cost_element.
  LOOP AT lt_io_data INTO ls_io_data.
    ls_io_data1-group_name = ls_io_data-group_name.
    ls_io_data1-current_date = ls_io_data-current_date.
    ls_io_data1-aufnr = ls_io_data-aufnr.
    ls_io_data1-budget_year = ls_io_data-budget_year.
    ls_io_data1-primary_cost_element = ls_io_data-primary_cost_element.
    APPEND ls_io_data1 TO lt_io_data1.
  ENDLOOP.
  SORT lt_io_data1 BY group_name aufnr primary_cost_element.
  DELETE ADJACENT DUPLICATES FROM lt_io_data1
            COMPARING group_name aufnr primary_cost_element.
  LOOP AT lt_io_data1 INTO ls_io_data1.
    CLEAR: lv_tabix,
          lv_actuals1,
          lv_actuals2,
          lv_actuals3,
          lv_actuals4,
          lv_actuals5,
          lv_actuals6,
          lv_actuals7,
          lv_actuals8,
          lv_actuals9,
          lv_actuals10,
          lv_actuals11,
          lv_actuals12.
    READ TABLE lt_io_data WITH KEY group_name = ls_io_data1-group_name
               aufnr = ls_io_data1-aufnr
               primary_cost_element = ls_io_data1-primary_cost_element
               TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_io_data INTO ls_io_data FROM lv_tabix.
      IF ls_io_data-group_name <> ls_io_data1-group_name OR
         ls_io_data-aufnr <> ls_io_data1-aufnr OR
         ls_io_data-primary_cost_element <> ls_io_data1-primary_cost_element.
        EXIT.
      ENDIF.
      lv_actuals1 = lv_actuals1 + ls_io_data-pd1_actuals.
      lv_actuals2 = lv_actuals2 + ls_io_data-pd2_actuals.
      lv_actuals3 = lv_actuals3 + ls_io_data-pd3_actuals.
      lv_actuals4 = lv_actuals4 + ls_io_data-pd4_actuals.
      lv_actuals5 = lv_actuals5 + ls_io_data-pd5_actuals.
      lv_actuals6 = lv_actuals6 + ls_io_data-pd6_actuals.
      lv_actuals7 = lv_actuals7 + ls_io_data-pd7_actuals.
      lv_actuals8 = lv_actuals8 + ls_io_data-pd8_actuals.
      lv_actuals9 = lv_actuals9 + ls_io_data-pd9_actuals.
      lv_actuals10 = lv_actuals10 + ls_io_data-pd10_actuals.
      lv_actuals11 = lv_actuals11 + ls_io_data-pd11_actuals.
      lv_actuals12 = lv_actuals12 + ls_io_data-pd12_actuals.
    ENDLOOP.
    WRITE lv_actuals1 TO ls_io_data1-pd1_actuals DECIMALS 2.
    WRITE lv_actuals2 TO ls_io_data1-pd2_actuals DECIMALS 2.
    WRITE lv_actuals3 TO ls_io_data1-pd3_actuals DECIMALS 2.
    WRITE lv_actuals4 TO ls_io_data1-pd4_actuals DECIMALS 2.
    WRITE lv_actuals5 TO ls_io_data1-pd5_actuals DECIMALS 2.
    WRITE lv_actuals6 TO ls_io_data1-pd6_actuals DECIMALS 2.
    WRITE lv_actuals7 TO ls_io_data1-pd7_actuals DECIMALS 2.
    WRITE lv_actuals8 TO ls_io_data1-pd8_actuals DECIMALS 2.
    WRITE lv_actuals9 TO ls_io_data1-pd9_actuals DECIMALS 2.
    WRITE lv_actuals10 TO ls_io_data1-pd10_actuals DECIMALS 2.
    WRITE lv_actuals11 TO ls_io_data1-pd11_actuals DECIMALS 2.
    WRITE lv_actuals12 TO ls_io_data1-pd12_actuals DECIMALS 2.
    IF lv_actuals1 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd1_actuals.
    ENDIF.
    IF lv_actuals2 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd2_actuals.
    ENDIF.
    IF lv_actuals3 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd3_actuals.
    ENDIF.
    IF lv_actuals4 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd4_actuals.
    ENDIF.
    IF lv_actuals5 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd5_actuals.
    ENDIF.
    IF lv_actuals6 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd6_actuals.
    ENDIF.
    IF lv_actuals7 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd7_actuals.
    ENDIF.
    IF lv_actuals8 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd8_actuals.
    ENDIF.
    IF lv_actuals9 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd9_actuals.
    ENDIF.
    IF lv_actuals10 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd10_actuals.
    ENDIF.
    IF lv_actuals11 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd11_actuals.
    ENDIF.
    IF lv_actuals12 < 0.
      PERFORM set_sign_to_front CHANGING ls_io_data1-pd12_actuals.
    ENDIF.
    APPEND ls_io_data1 TO et_io_data.
  ENDLOOP.
  IF et_io_data[] IS INITIAL.
    CONCATENATE 'No Internal Orders Assigned to Group '
                 iv_groupname INTO ls_return-message.
  ENDIF.



ENDFUNCTION.
