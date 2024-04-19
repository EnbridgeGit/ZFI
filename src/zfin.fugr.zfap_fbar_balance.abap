FUNCTION ZFAP_FBAR_BALANCE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_ACT) TYPE  CHAR1
*"  TABLES
*"      BANK_ACC_INPUT STRUCTURE  ZFAP_BANK_ACC_NO
*"      STATEMENT_DATE_INPUT STRUCTURE  ZFAP_STATEMENT_DAT
*"      COM_CODE STRUCTURE  ZFAP_COMP_CODE
*"      FEBKO_BAL_OUTPUT STRUCTURE  ZFAP_FEBKO_BALANCE
*"      BANK_ACC_OUTPUT STRUCTURE  ZFAP_BANK_ACC
*"----------------------------------------------------------------------
  DATA: T_T012T TYPE TABLE OF T012T WITH HEADER LINE,
        T_T001 TYPE TABLE OF T001 WITH HEADER LINE,
        gv_count                TYPE  i,
        lv_output type ZFAP_FEBKO_BALANCE,
        lv_bank_output type ZFAP_BANK_ACC.

  IF p_act = 'X'.
    IF bank_acc_input[]  IS NOT INITIAL AND
         statement_date_input[] IS NOT INITIAL.

      REFRESH : febko_bal_output.
      CLEAR   : febko_bal_output.

      SELECT ktonr azdat bukrs waers ssvoz ssbtr esvoz esbtr hbkid hktid
        FROM febko
        INTO TABLE febko_bal_output
        WHERE ANWND = '0001' AND
              ktonr IN  bank_acc_input[]     AND
              azdat IN  statement_date_input[] AND
              bukrs IN COM_CODE[].

      IF NOT febko_bal_output[] is INITIAL.
        SELECT * FROM T012T INTO TABLE T_T012T
          FOR ALL ENTRIES IN febko_bal_output
          WHERE SPRAS = 'EN' AND
                HBKID = febko_bal_output-HBKID AND
                HKTID = febko_bal_output-HKTID.
        IF NOT T_T012T[] IS INITIAL.
          SELECT * FROM T001 INTO TABLE T_T001
            FOR ALL ENTRIES IN T_T012T
            WHERE BUKRS = T_T012T-BUKRS.
        ENDIF.
      ENDIF.

      LOOP AT febko_bal_output INTO lv_output.
        gv_count = sy-tabix.
        READ TABLE T_T012T WITH KEY HBKID = lv_output-hbkid hktid = lv_output-hktid.
        if sy-subrc = 0.
          lv_output-text1 = t_t012t-text1.
          READ TABLE T_T001 WITH KEY BUKRS = T_T012T-BUKRS.
          IF SY-SUBRC = 0.
            lv_output-BUTXT = t_t001-BUTXT.
          ENDIF.
        endif.
        MODIFY febko_bal_output FROM lv_output INDEX GV_COUNT TRANSPORTING TEXT1 BUTXT.
      ENDLOOP.

    ENDIF.
  ELSE.
    SELECT bukrs hbkid hktid bankn waers hkont FROM T012K
      INTO CORRESPONDING FIELDS OF TABLE BANK_ACC_OUTPUT
      WHERE bukrs in COM_CODE[] AND
            bankn in bank_acc_input[].
    IF SY-SUBRC = 0.
      SELECT * FROM T012T INTO TABLE T_T012T
        FOR ALL ENTRIES IN BANK_ACC_OUTPUT
        WHERE SPRAS = 'EN' AND
              HBKID = BANK_ACC_OUTPUT-HBKID AND
              HKTID = BANK_ACC_OUTPUT-HKTID.
      IF SY-SUBRC = 0.
        LOOP AT BANK_ACC_OUTPUT INTO lv_bank_output.
          gv_count = sy-tabix.
          READ TABLE T_T012T WITH KEY BUKRS = lv_bank_output-bukrs HBKID = lv_bank_output-hbkid hktid = lv_bank_output-hktid.
          if sy-subrc = 0.
            lv_bank_output-text1 = t_t012t-text1.
          endif.
          lv_bank_output-sys = 'UG'.
          MODIFY BANK_ACC_OUTPUT FROM lv_bank_output INDEX gv_count TRANSPORTING text1 sys.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFUNCTION.
