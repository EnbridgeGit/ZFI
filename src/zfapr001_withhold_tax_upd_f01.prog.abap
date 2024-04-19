*&---------------------------------------------------------------------*
*&  Include           ZFAPR001_WITHHOLD_TAX_UPD_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  BSK_INITIAL_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BSK_INITIAL_VALUE .

  CLEAR : GV_CNT_BLK.

* Defaulting values for Radio Button R10.
  PERFORM CLEAR_SET_DEFAULT_R10.

* Defaulting values for Radio Button R20.
  PERFORM CLEAR_SET_DEFAULT_R20.


  LOOP AT SCREEN.

    IF SCREEN-GROUP3 = 'BLK'.
      GV_CNT_BLK = GV_CNT_BLK + 1.
    ENDIF.

    IF ( SCREEN-GROUP1 = GC_MODIF_ID_R10 ) OR
       ( SCREEN-GROUP1 = GC_MODIF_ID_R20 ) OR
       ( SCREEN-GROUP3 = 'COM' )           OR
       ( SCREEN-GROUP3 = 'BLK'  AND
         GV_CNT_BLK    GT 2 ).

      SCREEN-INPUT  = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  GV_TITLE = 'Custom program to change cleared invoices'(071).
  SET TITLEBAR 'BSK' WITH GV_TITLE.


ENDFORM.                    " BSK_INITIAL_VALUE
*&---------------------------------------------------------------------*
*&      Form  R10_INITIAL_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM R10_INITIAL_VALUE .

  CLEAR : GV_CNT_BLK.

* Defaulting values for Radio Button BSK.
  PERFORM CLEAR_SET_DEFAULT_BSK.

* Defaulting values for Radio Button R20.
  PERFORM CLEAR_SET_DEFAULT_R20.

  LOOP AT SCREEN.

    IF SCREEN-GROUP3 = 'BLK'.
      GV_CNT_BLK = GV_CNT_BLK + 1.
    ENDIF.

    IF ( SCREEN-GROUP1 = GC_MODIF_ID_BSK ) OR
       ( SCREEN-GROUP1 = GC_MODIF_ID_R20 ) OR
       ( SCREEN-GROUP3 = 'COM' )           OR
       ( SCREEN-GROUP3 = 'BLK'  AND
         GV_CNT_BLK    = 2 )               OR
       ( SCREEN-GROUP3 = 'BLK'  AND
         GV_CNT_BLK    GT 4 )  .

      SCREEN-INPUT  = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  GV_TITLE = 'RFWT0010 to change parked invoices & open invoices'(072).
  SET TITLEBAR 'R10' WITH GV_TITLE.


ENDFORM.                    " R10_INITIAL_VALUE
*&---------------------------------------------------------------------*
*&      Form  R20_INITIAL_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM R20_INITIAL_VALUE .

  CLEAR : GV_CNT_BLK.

* Defaulting values for Radio Button BSK.
  PERFORM CLEAR_SET_DEFAULT_BSK.

* Defaulting values for Radio Button R10.
  PERFORM CLEAR_SET_DEFAULT_R10.


  LOOP AT SCREEN.

    IF SCREEN-GROUP3 = 'BLK'.
      GV_CNT_BLK = GV_CNT_BLK + 1.
    ENDIF.

    IF   ( SCREEN-NAME   = 'PRB_R10'        ) OR
         ( SCREEN-NAME   = 'PRB_R20'        ) OR
         ( SCREEN-NAME   = 'PRB_BSK'        ) OR
         ( SCREEN-GROUP3 = 'BLK'   AND
           GV_CNT_BLK    = 1     )           OR
         ( SCREEN-GROUP3 = 'BLK'   AND
           GV_CNT_BLK    = 5     )           OR
         ( SCREEN-GROUP3 = 'BLK'   AND
           GV_CNT_BLK    = 6     )           OR
         ( SCREEN-GROUP3 = 'BLK'   AND
           GV_CNT_BLK    = 7     ) .

      CONTINUE.
    ENDIF.
    IF ( SCREEN-GROUP1 NE GC_MODIF_ID_R20 ).

      SCREEN-INPUT  = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

  GV_TITLE = 'RFWT0020 to change open and cleared invoices'(073).
  SET TITLEBAR 'R20' WITH GV_TITLE.


ENDFORM.                    " R20_INITIAL_VALUE
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_R20
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SUBMIT_R20 .

  SUBMIT RFWT0020 WITH I_LIFNR   IN I_LIF_20[]
                  WITH I_STCD1   IN I_STCD1[]
                  WITH I_STCD2   IN I_STCD2[]
                  WITH I_KUNNR   IN I_KUN_20[]
                  WITH I_KSTCD1  IN I_KSTCD1[]
                  WITH I_KSTCD2  IN I_KSTCD2[]
                  WITH I_BUKRS   IN I_BUK_20[]
                  WITH I_BELNR   IN I_BEL_20[]
                  WITH I_TIME    IN I_TIME[]
                  WITH XCREATE   =  XCREATE
                  WITH XMODIFY   =  XMODIFY
                  WITH ENTRY     =  ENTRY
                  WITH R_INV     =  R_INV
                  WITH R_PAY     =  R_PAY
                  WITH TEST      =  TEST_20
*                    VIA SELECTION-SCREEN
                  AND RETURN.


ENDFORM.                    " SUBMIT_R20
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_R10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SUBMIT_R10 .

  SUBMIT RFWT0010 WITH VENDOR     = VENDOR
                  WITH I_LIFNR   IN I_LIFNR[]
                  WITH CUSTOMER   = CUSTOMER
                  WITH I_KUNNR   IN I_KUNNR[]
                  WITH I_BUKRS   IN I_BUKRS[]
                  WITH I_BELNR   IN I_BELNR[]
                  WITH I_GJAHR   IN I_GJAHR[]
                  WITH I_BUDAT   IN I_BUDAT[]
                  WITH NORMAL     = NORMAL
                  WITH PARKED     = PARKED
                  WITH RECURR     = RECURR
                  WITH I_DBELNR  IN I_DBELNR[]
                  WITH TEST       = TEST
                  WITH DA_TODAY   = DA_TODAY
                  WITH DA_OTHER   = DA_OTHER
                  WITH DATE       = DATE
                  WITH NEW_TYPE   = NEW_TYPE
                  WITH CODE1      = CODE1
                  WITH DOWN_PAY   = DOWN_PAY
                  WITH CODE2      = CODE2
                  WITH OLD_TYPE   = OLD_TYPE
*                    VIA SELECTION-SCREEN
                  AND RETURN.

ENDFORM.                    " SUBMIT_R10
*&---------------------------------------------------------------------*
*&      Form  SELECTION_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SELECTION_CHECK .


  IF PRB_R10 = 'X'.

    IF I_BUKRS[] IS INITIAL.
      MESSAGE 'Please enter Company Code'(074) TYPE 'S'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ELSEIF PRB_BSK = 'X'.

    IF S_BUKRS[] IS INITIAL.
      MESSAGE 'Please enter Company Code'(074) TYPE 'S'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF P_GJAHR IS INITIAL.
      MESSAGE 'Please enter Fiscal Year'(075) TYPE 'S'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.


ENDFORM.                    " SELECTION_CHECK
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_AND_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SUBMIT_AND_DISPLAY .

  IF PRB_R20 = 'X'.

    PERFORM SUBMIT_R20.

  ELSEIF PRB_R10 = 'X'.

    PERFORM SUBMIT_R10.

  ELSEIF PRB_BSK = 'X'.

    PERFORM FETCH_DATA_FOR_WITHHOLD_CHK.

    PERFORM FORMAT_WITHHOLD_DATA_FOR_BDC.

    PERFORM CALL_TO_BDC.


  ENDIF.

ENDFORM.                    " SUBMIT_AND_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CLEAR_SET_DEFAULT_BSK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CLEAR_SET_DEFAULT_BSK .

  REFRESH : S_BUKRS,
            S_LIFNR,
            S_BELNR,
            S_BUDAT.

  CLEAR :   S_BUKRS,
            S_LIFNR,
            S_BELNR,
            S_BUDAT,
            P_GJAHR.

  P_TEST = 'X'.

ENDFORM.                    " CLEAR_SET_DEFAULT_BSK
*&---------------------------------------------------------------------*
*&      Form  CLEAR_SET_DEFAULT_R10
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CLEAR_SET_DEFAULT_R10 .

  REFRESH : I_LIFNR,
            I_KUNNR,
            I_BUKRS,
            I_BELNR,
            I_GJAHR,
            I_BUDAT,
            I_DBELNR.

  CLEAR :   I_LIFNR,
            I_KUNNR,
            I_BUKRS,
            I_BELNR,
            I_GJAHR,
            I_BUDAT,
            I_DBELNR,
            VENDOR,
            CUSTOMER,
            RECURR,
            DA_OTHER,
            DATE,
            DOWN_PAY,
            CODE2.

  NORMAL   = 'X'.
  PARKED   = 'X'.
  TEST     = 'X'.
  DA_TODAY = 'X'.
  NEW_TYPE = 'X'.
  CODE1    = 'X'.
  OLD_TYPE = 'X'.

ENDFORM.                    " CLEAR_SET_DEFAULT_R10
*&---------------------------------------------------------------------*
*&      Form  CLEAR_SET_DEFAULT_R20
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CLEAR_SET_DEFAULT_R20 .

  REFRESH : I_LIF_20,
            I_STCD1,
            I_STCD2,
            I_KUN_20,
            I_KSTCD1,
            I_KSTCD2,
            I_BUK_20,
            I_BEL_20,
            I_TIME.

  CLEAR :   I_LIF_20,
            I_STCD1,
            I_STCD2,
            I_KUN_20,
            I_KSTCD1,
            I_KSTCD2,
            I_BUK_20,
            I_BEL_20,
            I_TIME,
            XMODIFY,
            R_PAY.

  XCREATE = 'X'.
  ENTRY   = 'X'.
  R_INV   = 'X'.
  TEST_20 = 'X'.

ENDFORM.                    " CLEAR_SET_DEFAULT_R20
*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA_FOR_WITHHOLD_CHK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FETCH_DATA_FOR_WITHHOLD_CHK .

  REFRESH : GIT_BSAK,
            GIT_WITH_ITEM,
            GIT_WITH_ITEM_BDC_FIELD,
            GIT_LFBW,
            GIT_LFB1,
            GIT_WITH_TAX_FINAL,
            GIT_ALV_DISPLAY,
            GIT_BDCDATA ,
            GIT_BDC_MSG_ERROR,
            GIT_FIELDCAT_DISPLAY,
            GIT_EXTAB,
            GIT_SORT.


  CLEAR   : GWA_BSAK,
            GWA_WITH_ITEM,
            GWA_WITH_ITEM_BDC_FIELD,
            GWA_LFBW,
            GWA_LFB1,
            GWA_WITH_TAX_FINAL,
            GWA_ALV_DISPLAY,
            GWA_BDCOPTS,
            GWA_FIELDCAT_DISPLAY,
            GWA_LAYOUT,
            GWA_ALV_PRINT,
            GWA_EXTAB,
            GWA_SORT,
            GSTR_ERROR_TEXT,
            GV_REP ,
            GV_FIELD_COUNT,
            GV_EVEN_ODD_CHK.


  SELECT BUKRS LIFNR UMSKS UMSKZ AUGDT AUGBL ZUONR GJAHR BELNR BUZEI BUDAT
    FROM BSAK
    INTO TABLE GIT_BSAK
    WHERE BUKRS IN S_BUKRS   AND
          LIFNR IN S_LIFNR   AND
          GJAHR  = P_GJAHR   AND
          BELNR IN S_BELNR   AND
          BUDAT IN S_BUDAT.
  IF SY-SUBRC = 0.
    SELECT BUKRS BELNR GJAHR BUZEI WITHT WT_WITHCD WT_QSSHH WT_QSSHB
      FROM WITH_ITEM
      INTO TABLE GIT_WITH_ITEM
      FOR ALL ENTRIES IN GIT_BSAK
        WHERE BUKRS = GIT_BSAK-BUKRS AND
              BELNR = GIT_BSAK-BELNR AND
              GJAHR = GIT_BSAK-GJAHR AND
              BUZEI = GIT_BSAK-BUZEI.
    IF SY-SUBRC = 0.

      CLEAR : GV_BUKRS_AFTER,
              GV_BELNR_AFTER,
              GV_GJAHR_AFTER,
              GV_BUKRS_BEFORE,
              GV_BELNR_BEFORE,
              GV_GJAHR_BEFORE.

      LOOP AT GIT_WITH_ITEM INTO GWA_WITH_ITEM.

        GWA_WITH_ITEM_BDC_FIELD-BUKRS               = GWA_WITH_ITEM-BUKRS.
        GWA_WITH_ITEM_BDC_FIELD-BELNR               = GWA_WITH_ITEM-BELNR.
        GWA_WITH_ITEM_BDC_FIELD-GJAHR               = GWA_WITH_ITEM-GJAHR.
        GWA_WITH_ITEM_BDC_FIELD-BUZEI               = GWA_WITH_ITEM-BUZEI.
        GWA_WITH_ITEM_BDC_FIELD-WITHT               = GWA_WITH_ITEM-WITHT.
        GWA_WITH_ITEM_BDC_FIELD-WT_WITHCD           = GWA_WITH_ITEM-WT_WITHCD.
        GWA_WITH_ITEM_BDC_FIELD-WT_QSSHH            = GWA_WITH_ITEM-WT_QSSHH.
        GWA_WITH_ITEM_BDC_FIELD-WT_QSSHB            = GWA_WITH_ITEM-WT_QSSHB.

        GV_BUKRS_BEFORE = GWA_WITH_ITEM-BUKRS.
        GV_BELNR_BEFORE = GWA_WITH_ITEM-BELNR.
        GV_GJAHR_BEFORE = GWA_WITH_ITEM-GJAHR.


        IF ( GV_BUKRS_AFTER NE GV_BUKRS_BEFORE ) OR
           ( GV_BELNR_AFTER NE GV_BELNR_BEFORE ) OR
           ( GV_GJAHR_AFTER NE GV_GJAHR_BEFORE ) .

          CLEAR : GC_TAX_ADD_CNT_CHR.

          GC_TAX_ADD_CNT_CHR = GC_TAX_ADD_CNT_CHR + 1.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = GC_TAX_ADD_CNT_CHR
            IMPORTING
              OUTPUT = GC_TAX_ADD_CNT_CHR.              " Logic to add leading zero.

          CLEAR : GSTR_WT_QSSHB.

          CONCATENATE GC_WT_QSSHB
                      '('
                      GC_TAX_ADD_CNT_CHR
                      ')'
                 INTO GSTR_WT_QSSHB.

          GWA_WITH_ITEM_BDC_FIELD-WT_QSSHB_BDC_FIELD  = GSTR_WT_QSSHB.

          APPEND  GWA_WITH_ITEM_BDC_FIELD TO GIT_WITH_ITEM_BDC_FIELD.
          CLEAR : GWA_WITH_ITEM_BDC_FIELD.

        ELSE.

          GC_TAX_ADD_CNT_CHR = GC_TAX_ADD_CNT_CHR + 1.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = GC_TAX_ADD_CNT_CHR
            IMPORTING
              OUTPUT = GC_TAX_ADD_CNT_CHR.              " Logic to add leading zero.

          CLEAR : GSTR_WT_QSSHB.

          CONCATENATE GC_WT_QSSHB
                      '('
                      GC_TAX_ADD_CNT_CHR
                      ')'
                      INTO GSTR_WT_QSSHB.

          GWA_WITH_ITEM_BDC_FIELD-WT_QSSHB_BDC_FIELD  = GSTR_WT_QSSHB.

          APPEND  GWA_WITH_ITEM_BDC_FIELD TO GIT_WITH_ITEM_BDC_FIELD.
          CLEAR : GWA_WITH_ITEM_BDC_FIELD.

        ENDIF.

        GV_BUKRS_AFTER = GV_BUKRS_BEFORE.
        GV_BELNR_AFTER = GV_BELNR_BEFORE.
        GV_GJAHR_AFTER = GV_GJAHR_BEFORE.


      ENDLOOP.
    ENDIF.
    SELECT LIFNR BUKRS
      FROM LFB1
      INTO TABLE GIT_LFB1
      FOR ALL ENTRIES IN GIT_BSAK
      WHERE LIFNR = GIT_BSAK-LIFNR AND
            BUKRS = GIT_BSAK-BUKRS.

    SELECT LIFNR BUKRS WITHT WT_SUBJCT
      FROM LFBW
      INTO TABLE GIT_LFBW
      FOR ALL ENTRIES IN GIT_BSAK
      WHERE LIFNR = GIT_BSAK-LIFNR AND
            BUKRS = GIT_BSAK-BUKRS.

  ENDIF.
ENDFORM.                    " FETCH_DATA_FOR_WITHHOLD_CHK
*&---------------------------------------------------------------------*
*&      Form  FORMAT_WITHHOLD_DATA_FOR_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FORMAT_WITHHOLD_DATA_FOR_BDC .

  LOOP AT GIT_BSAK INTO GWA_BSAK.
**    READ TABLE GIT_WITH_ITEM INTO GWA_WITH_ITEM
*                             WITH KEY  BUKRS = GWA_BSAK-BUKRS
*                                       BELNR = GWA_BSAK-BELNR
*                                       GJAHR = GWA_BSAK-GJAHR
*                                       BUZEI = GWA_BSAK-BUZEI.
*    IF SY-SUBRC = 0.
    LOOP AT GIT_WITH_ITEM_BDC_FIELD INTO  GWA_WITH_ITEM_BDC_FIELD
                                    WHERE BUKRS = GWA_BSAK-BUKRS AND
                                          BELNR = GWA_BSAK-BELNR AND
                                          GJAHR = GWA_BSAK-GJAHR AND
                                          BUZEI = GWA_BSAK-BUZEI.

      IF ( GWA_WITH_ITEM_BDC_FIELD-WT_QSSHH IS NOT INITIAL ) OR
         ( GWA_WITH_ITEM_BDC_FIELD-WT_QSSHB IS NOT INITIAL ).

* Checking if vendor and company code combination is valid.
        READ TABLE GIT_LFB1 INTO GWA_LFB1
                            WITH KEY LIFNR = GWA_BSAK-LIFNR
                                     BUKRS = GWA_BSAK-BUKRS.
        IF SY-SUBRC = 0.
* Checking if vendor has withholding tax or not
* If no then make WT_QSSHH and WT_QSSHB to '0.00'.
          READ TABLE GIT_LFBW INTO GWA_LFBW
                              WITH KEY LIFNR = GWA_BSAK-LIFNR
                                       BUKRS = GWA_BSAK-BUKRS
                                       WITHT = GWA_WITH_ITEM_BDC_FIELD-WITHT.
          IF SY-SUBRC = 0.
            IF GWA_LFBW-WT_SUBJCT NE 'X'.

              GWA_WITH_TAX_FINAL-LIFNR              = GWA_BSAK-LIFNR.
              GWA_WITH_TAX_FINAL-BELNR              = GWA_BSAK-BELNR.
              GWA_WITH_TAX_FINAL-BUKRS              = GWA_BSAK-BUKRS.
              GWA_WITH_TAX_FINAL-GJAHR              = GWA_BSAK-GJAHR.
              GWA_WITH_TAX_FINAL-BUZEI              = GWA_BSAK-BUZEI.
              GWA_WITH_TAX_FINAL-WT_WITHCD          = GWA_WITH_ITEM_BDC_FIELD-WT_WITHCD.
              GWA_WITH_TAX_FINAL-WT_QSSHH           = GWA_WITH_ITEM_BDC_FIELD-WT_QSSHH.
              GWA_WITH_TAX_FINAL-WT_QSSHB           = GWA_WITH_ITEM_BDC_FIELD-WT_QSSHB.
              GWA_WITH_TAX_FINAL-WT_QSSHB_BDC_FIELD = GWA_WITH_ITEM_BDC_FIELD-WT_QSSHB_BDC_FIELD.

              APPEND  GWA_WITH_TAX_FINAL TO GIT_WITH_TAX_FINAL.
              CLEAR : GWA_WITH_TAX_FINAL.

            ENDIF.
          ELSE.

            GWA_WITH_TAX_FINAL-LIFNR               = GWA_BSAK-LIFNR.
            GWA_WITH_TAX_FINAL-BELNR               = GWA_BSAK-BELNR.
            GWA_WITH_TAX_FINAL-BUKRS               = GWA_BSAK-BUKRS.
            GWA_WITH_TAX_FINAL-GJAHR               = GWA_BSAK-GJAHR.
            GWA_WITH_TAX_FINAL-BUZEI               = GWA_BSAK-BUZEI.
            GWA_WITH_TAX_FINAL-WT_WITHCD           = GWA_WITH_ITEM_BDC_FIELD-WT_WITHCD.
            GWA_WITH_TAX_FINAL-WT_QSSHH            = GWA_WITH_ITEM_BDC_FIELD-WT_QSSHH.
            GWA_WITH_TAX_FINAL-WT_QSSHB            = GWA_WITH_ITEM_BDC_FIELD-WT_QSSHB.
            GWA_WITH_TAX_FINAL-WT_QSSHB_BDC_FIELD  = GWA_WITH_ITEM_BDC_FIELD-WT_QSSHB_BDC_FIELD.

            APPEND  GWA_WITH_TAX_FINAL TO GIT_WITH_TAX_FINAL.
            CLEAR : GWA_WITH_TAX_FINAL.

          ENDIF.
        ENDIF.

      ENDIF.
*    ENDIF.
    ENDLOOP.
    CLEAR : GWA_BSAK,
            GWA_WITH_ITEM_BDC_FIELD,
            GWA_LFBW,
            GWA_LFB1.
  ENDLOOP.

ENDFORM.                    " FORMAT_WITHHOLD_DATA_FOR_BDC
*&---------------------------------------------------------------------*
*&      Form  CALL_TO_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CALL_TO_BDC .

  DATA: LV_LINE(3) TYPE C.

  REFRESH : GIT_BDCDATA,
            GIT_BDC_MSG_ERROR.
  CLEAR :   GWA_BDCDATA,
            GWA_BDC_MSG_ERROR.

* Call BDC when it is not test mode
  IF P_TEST NE GC_X.

    REFRESH : GIT_WITH_TAX_FINAL_TEMP.
    CLEAR   : GWA_WITH_TAX_FINAL_TEMP.

    LOOP AT GIT_WITH_TAX_FINAL INTO GWA_WITH_TAX_FINAL.

      GWA_WITH_TAX_FINAL_TEMP-LIFNR = GWA_WITH_TAX_FINAL-LIFNR.
      GWA_WITH_TAX_FINAL_TEMP-BELNR = GWA_WITH_TAX_FINAL-BELNR.
      GWA_WITH_TAX_FINAL_TEMP-BUKRS = GWA_WITH_TAX_FINAL-BUKRS.
      GWA_WITH_TAX_FINAL_TEMP-GJAHR = GWA_WITH_TAX_FINAL-GJAHR.
      GWA_WITH_TAX_FINAL_TEMP-BUZEI = GWA_WITH_TAX_FINAL-BUZEI.

      APPEND  GWA_WITH_TAX_FINAL_TEMP TO GIT_WITH_TAX_FINAL_TEMP.
      CLEAR : GWA_WITH_TAX_FINAL_TEMP.

    ENDLOOP.
*    GIT_WITH_TAX_FINAL_TEMP[] = GIT_WITH_TAX_FINAL[].

    SORT GIT_WITH_TAX_FINAL_TEMP BY LIFNR BELNR BUKRS GJAHR BUZEI.
    DELETE ADJACENT DUPLICATES FROM GIT_WITH_TAX_FINAL_TEMP.

    LOOP AT GIT_WITH_TAX_FINAL_TEMP INTO GWA_WITH_TAX_FINAL_TEMP.

      CLEAR : LV_LINE,
              GWA_ALV_DISPLAY.

      LV_LINE = GWA_WITH_TAX_FINAL_TEMP-BUZEI.

      PERFORM :  FILL_BDC_DATA USING 'SAPMF05L' '0102' 'X'  ' '  ' ',
                 FILL_BDC_DATA USING  ''  ''  ''   'BDC_CURSOR'  'RF05L-BELNR',
                 FILL_BDC_DATA USING  ''  ''  ''   'RF05L-BELNR'  GWA_WITH_TAX_FINAL_TEMP-BELNR,
                 FILL_BDC_DATA USING  ''  ''  ''   'RF05L-BUKRS'  GWA_WITH_TAX_FINAL_TEMP-BUKRS,
                 FILL_BDC_DATA USING  ''  ''  ''   'RF05L-GJAHR'  GWA_WITH_TAX_FINAL_TEMP-GJAHR,
                 FILL_BDC_DATA USING  ''  ''  ''   'RF05L-BUZEI'  LV_LINE,
                 FILL_BDC_DATA USING  ''  ''  ''   'BDC_OKCODE'   '/00'.

      PERFORM :  FILL_BDC_DATA USING 'SAPMF05L' '0302' 'X'  ' '  ' ',
                 FILL_BDC_DATA USING  ''  ''  ''   'BDC_CURSOR'  'BSEG-ZUONR',
                 FILL_BDC_DATA USING  ''  ''  ''   'BDC_OKCODE'  '=QS'.

      PERFORM :  FILL_BDC_DATA USING 'SAPLFWTD' '0700' 'X'  ' '  ' ',
                 FILL_BDC_DATA USING  ''  ''  ''   'BDC_CURSOR'  'WITH_DIALG-WT_SELDATE',
                 FILL_BDC_DATA USING  ''  ''  ''   'BDC_OKCODE'  '=GO'.

*      LOOP AT GIT_WITH_TAX_FINAL INTO  GWA_WITH_TAX_FINAL
*                                 WHERE BELNR = GWA_WITH_TAX_FINAL_TEMP-BELNR AND
*                                       BUKRS = GWA_WITH_TAX_FINAL_TEMP-BUKRS AND
*                                       GJAHR = GWA_WITH_TAX_FINAL_TEMP-GJAHR AND
*                                       BUZEI = GWA_WITH_TAX_FINAL_TEMP-BUZEI .

      PERFORM :  FILL_BDC_DATA USING 'SAPLFWTD' '0110' 'X'  ' '  ' '.

      LOOP AT GIT_WITH_TAX_FINAL INTO  GWA_WITH_TAX_FINAL
                              WHERE BELNR = GWA_WITH_TAX_FINAL_TEMP-BELNR AND
                                    BUKRS = GWA_WITH_TAX_FINAL_TEMP-BUKRS AND
                                    GJAHR = GWA_WITH_TAX_FINAL_TEMP-GJAHR AND
                                    BUZEI = GWA_WITH_TAX_FINAL_TEMP-BUZEI .

        PERFORM :  FILL_BDC_DATA USING  ''  ''  ''   'BDC_CURSOR'  GWA_WITH_TAX_FINAL-WT_QSSHB_BDC_FIELD,
                   FILL_BDC_DATA USING  ''  ''  ''   GWA_WITH_TAX_FINAL-WT_QSSHB_BDC_FIELD ''.

      ENDLOOP.

      PERFORM :  FILL_BDC_DATA USING  ''  ''  ''   'BDC_OKCODE'  '=GO'.

      PERFORM :  FILL_BDC_DATA USING 'SAPMF05L' '0302' 'X'  ' '  ' ',
                 FILL_BDC_DATA USING  ''  ''  ''   'BDC_CURSOR'  'BSEG-ZUONR',
                 FILL_BDC_DATA USING  ''  ''  ''   'BDC_OKCODE'  '=AE'.


      GWA_BDCOPTS-DISMODE = GC_BDC_MODE.
      GWA_BDCOPTS-UPDMODE = GC_BDC_OPTION.
      GWA_BDCOPTS-NOBINPT = GC_X.

      CALL TRANSACTION                GC_BDC_TCODE
                        USING         GIT_BDCDATA
                        OPTIONS FROM  GWA_BDCOPTS
                        MESSAGES INTO GIT_BDC_MSG_ERROR.

      IF SY-SUBRC = 0.

        GWA_ALV_DISPLAY-MSG       = 'Withholding Tax Removed Successfully'.
        GWA_ALV_DISPLAY-LIFNR     = GWA_WITH_TAX_FINAL_TEMP-LIFNR.
        GWA_ALV_DISPLAY-BELNR     = GWA_WITH_TAX_FINAL_TEMP-BELNR.
        GWA_ALV_DISPLAY-BUKRS     = GWA_WITH_TAX_FINAL_TEMP-BUKRS.
        GWA_ALV_DISPLAY-GJAHR     = GWA_WITH_TAX_FINAL_TEMP-GJAHR.
        GWA_ALV_DISPLAY-BUZEI     = GWA_WITH_TAX_FINAL_TEMP-BUZEI.
*        GWA_ALV_DISPLAY-WT_WITHCD = GWA_WITH_TAX_FINAL_temp-WT_WITHCD.
*        GWA_ALV_DISPLAY-WT_QSSHH  = GWA_WITH_TAX_FINAL_temp-WT_QSSHH.
*        GWA_ALV_DISPLAY-WT_QSSHB  = GWA_WITH_TAX_FINAL_temp-WT_QSSHB.

        APPEND  GWA_ALV_DISPLAY TO GIT_ALV_DISPLAY.
        CLEAR : GWA_ALV_DISPLAY.

      ELSE.
        LOOP AT GIT_BDC_MSG_ERROR INTO GWA_BDC_MSG_ERROR WHERE MSGTYP = GC_E .

          CLEAR: GSTR_ERROR_TEXT.
*     Format Error message from BDC message
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              MSGID               = GWA_BDC_MSG_ERROR-MSGID
              MSGNR               = GWA_BDC_MSG_ERROR-MSGNR
              MSGV1               = GWA_BDC_MSG_ERROR-MSGV1
              MSGV2               = GWA_BDC_MSG_ERROR-MSGV2
              MSGV3               = GWA_BDC_MSG_ERROR-MSGV3
              MSGV4               = GWA_BDC_MSG_ERROR-MSGV4
            IMPORTING
              MESSAGE_TEXT_OUTPUT = GSTR_ERROR_TEXT.
          CONCATENATE  GWA_ALV_DISPLAY-MSG GSTR_ERROR_TEXT INTO GWA_ALV_DISPLAY-MSG SEPARATED BY SPACE.
        ENDLOOP.

        CONDENSE GWA_ALV_DISPLAY-MSG.
        IF GWA_ALV_DISPLAY-MSG IS NOT INITIAL.

          GWA_ALV_DISPLAY-LIFNR     = GWA_WITH_TAX_FINAL_TEMP-LIFNR.
          GWA_ALV_DISPLAY-BELNR     = GWA_WITH_TAX_FINAL_TEMP-BELNR.
          GWA_ALV_DISPLAY-BUKRS     = GWA_WITH_TAX_FINAL_TEMP-BUKRS.
          GWA_ALV_DISPLAY-GJAHR     = GWA_WITH_TAX_FINAL_TEMP-GJAHR.
          GWA_ALV_DISPLAY-BUZEI     = GWA_WITH_TAX_FINAL_TEMP-BUZEI.
*          GWA_ALV_DISPLAY-WT_WITHCD = GWA_WITH_TAX_FINAL-WT_WITHCD.
*          GWA_ALV_DISPLAY-WT_QSSHH  = GWA_WITH_TAX_FINAL-WT_QSSHH.
*          GWA_ALV_DISPLAY-WT_QSSHB  = GWA_WITH_TAX_FINAL-WT_QSSHB.

          APPEND  GWA_ALV_DISPLAY TO GIT_ALV_DISPLAY.
          CLEAR : GWA_ALV_DISPLAY.
        ELSE.

          GWA_ALV_DISPLAY-MSG       = 'Withholding Tax Update Failed'.
          GWA_ALV_DISPLAY-LIFNR     = GWA_WITH_TAX_FINAL_TEMP-LIFNR.
          GWA_ALV_DISPLAY-BELNR     = GWA_WITH_TAX_FINAL_TEMP-BELNR.
          GWA_ALV_DISPLAY-BUKRS     = GWA_WITH_TAX_FINAL_TEMP-BUKRS.
          GWA_ALV_DISPLAY-GJAHR     = GWA_WITH_TAX_FINAL_TEMP-GJAHR.
          GWA_ALV_DISPLAY-BUZEI     = GWA_WITH_TAX_FINAL_TEMP-BUZEI.
*          GWA_ALV_DISPLAY-WT_WITHCD = GWA_WITH_TAX_FINAL-WT_WITHCD.
*          GWA_ALV_DISPLAY-WT_QSSHH  = GWA_WITH_TAX_FINAL-WT_QSSHH.
*          GWA_ALV_DISPLAY-WT_QSSHB  = GWA_WITH_TAX_FINAL-WT_QSSHB.

          APPEND  GWA_ALV_DISPLAY TO GIT_ALV_DISPLAY.
          CLEAR : GWA_ALV_DISPLAY.

        ENDIF.
      ENDIF.

      CLEAR :   GWA_ALV_DISPLAY.
      REFRESH : GIT_BDCDATA,
                GIT_BDC_MSG_ERROR.

      CLEAR :   GWA_BDCDATA,
                GWA_BDC_MSG_ERROR.


    ENDLOOP.
  ENDIF.

* Display alv output
  PERFORM DISPLAY_REPORT.


ENDFORM.                    " CALL_TO_BDC

*&---------------------------------------------------------------------*
*&      Form  fill_bdc_data
*&---------------------------------------------------------------------*
*      format data for BDC
*----------------------------------------------------------------------*
FORM FILL_BDC_DATA  USING    VALUE(CV_PROGRAM)
                             VALUE(CV_DYNPRO)
                             VALUE(CV_DYNBEGIN)
                             VALUE(CV_FNAM)
                             VALUE(CV_FVAL).
  CLEAR GWA_BDCDATA .
  IF CV_DYNBEGIN = 'X' .
    GWA_BDCDATA-PROGRAM   = CV_PROGRAM .
    GWA_BDCDATA-DYNPRO    = CV_DYNPRO .
    GWA_BDCDATA-DYNBEGIN  = CV_DYNBEGIN .
    APPEND GWA_BDCDATA TO GIT_BDCDATA.
  ELSE.
    GWA_BDCDATA-FNAM = CV_FNAM.
    GWA_BDCDATA-FVAL = CV_FVAL.
    APPEND GWA_BDCDATA TO GIT_BDCDATA.
  ENDIF.

ENDFORM.                    " FILL_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*    Display alv report
*----------------------------------------------------------------------*
FORM DISPLAY_REPORT .

*ALV Layout for display
  PERFORM ALV_FIELDCAT_DISPLAY.

*ALV display for output
  PERFORM ALV_FUNCTION_CALL.

ENDFORM.                    " DISPLAY_REPORT
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCAT_DISPLAY
*&---------------------------------------------------------------------*
FORM ALV_FIELDCAT_DISPLAY .

  REFRESH : GIT_FIELDCAT_DISPLAY.
  CLEAR   : GV_FIELD_COUNT.

  IF P_TEST NE GC_X.

    PERFORM POPULATE_FIELD_CATALOG USING:
            'LIFNR'(076)               GC_NO_1     'Vendor'(077)                                        'GIT_ALV_DISPLAY'(094)    '',
            'BELNR'(078)               GC_NO_1     'Document No'(079)                                   'GIT_ALV_DISPLAY'(094)    '',
            'BUKRS'(080)               GC_NO_1     'Company Code'(081)                                  'GIT_ALV_DISPLAY'(094)    '',
            'GJAHR'(082)               GC_NO_1     'Fiscal Year'(083)                                   'GIT_ALV_DISPLAY'(094)    '',
            'BUZEI'(084)               GC_NO_1     'Line Item No'(085)                                  'GIT_ALV_DISPLAY'(094)    '',
*            'WT_WITHCD'(086)           GC_NO_1     'Withholding Tax Code'(087)                          'GIT_ALV_DISPLAY'(094)    '',
*            'WT_QSSHH'(088)            GC_NO_1     'WH Tax Base Amount'(089)                            'GIT_ALV_DISPLAY'(094)    '',
*            'WT_QSSHB'(090)            GC_NO_1     'WH Tax Base Amount Doc Currency'(091)               'GIT_ALV_DISPLAY'(094)    '',
            'MSG'(092)                 GC_NO_1     'Status'(093)                                        'GIT_ALV_DISPLAY'(094)    ''.

  ELSEIF P_TEST = GC_X.

    PERFORM POPULATE_FIELD_CATALOG USING:
            'LIFNR'(076)               GC_NO_1     'Vendor'(077)                                        'GIT_WITH_TAX_FINAL'(095)    '',
            'BELNR'(078)               GC_NO_1     'Document No'(079)                                   'GIT_WITH_TAX_FINAL'(095)    '',
            'BUKRS'(080)               GC_NO_1     'Company Code'(081)                                  'GIT_WITH_TAX_FINAL'(095)    '',
            'GJAHR'(082)               GC_NO_1     'Fiscal Year'(083)                                   'GIT_WITH_TAX_FINAL'(095)    '',
            'BUZEI'(084)               GC_NO_1     'Line Item No'(085)                                  'GIT_WITH_TAX_FINAL'(095)    ''.
*            'WT_WITHCD'(086)           GC_NO_1     'Withholding Tax Code'(087)                          'GIT_WITH_TAX_FINAL'(095)    '',
*            'WT_QSSHH'(088)            GC_NO_1     'WH Tax Base Amount'(089)                            'GIT_WITH_TAX_FINAL'(095)    '',
*            'WT_QSSHB'(090)            GC_NO_1     'WH Tax Base Amount Doc Currency'(091)               'GIT_WITH_TAX_FINAL'(095)    ''.

  ENDIF.
ENDFORM.                    " ALV_FIELDCAT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  populate_field_catalog
*&---------------------------------------------------------------------*
* i_fieldname => Name of the field to be displayed
* i_colpos    => Position of the field where the field should be displayed
*                in the output
* i_seltextl  => Text for displayed field
*----------------------------------------------------------------------*
FORM POPULATE_FIELD_CATALOG  USING          I_FIELDNAME
                                            I_ROWPOS
                                            I_SELTEXTL
                                            I_TABNAME
                                            I_FIXED_COL.


  GWA_FIELDCAT_DISPLAY-FIELDNAME   = I_FIELDNAME.
  GWA_FIELDCAT_DISPLAY-ROW_POS     = I_ROWPOS.

  GV_FIELD_COUNT = GV_FIELD_COUNT + 1.
  GWA_FIELDCAT_DISPLAY-COL_POS     = GV_FIELD_COUNT.

  GV_EVEN_ODD_CHK = GV_FIELD_COUNT MOD 2.

  IF GV_EVEN_ODD_CHK = 1.
    GWA_FIELDCAT_DISPLAY-EMPHASIZE   = 'C200'.
  ELSE.
    GWA_FIELDCAT_DISPLAY-EMPHASIZE   = 'C100'.
  ENDIF.

  GWA_FIELDCAT_DISPLAY-SELTEXT_L   = I_SELTEXTL.
  GWA_FIELDCAT_DISPLAY-TABNAME     = I_TABNAME.
  GWA_FIELDCAT_DISPLAY-FIX_COLUMN  = I_FIXED_COL.

  APPEND  GWA_FIELDCAT_DISPLAY TO GIT_FIELDCAT_DISPLAY.
  CLEAR GWA_FIELDCAT_DISPLAY .
ENDFORM.                    " populate_field_catalog
*&---------------------------------------------------------------------*
*&      Form  ALV_FUNCTION_CALL
*&---------------------------------------------------------------------*
FORM ALV_FUNCTION_CALL .

*alv layout
  GWA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GWA_LAYOUT-ZEBRA = 'X'.

  GWA_ALV_PRINT-NO_COVERPAGE = 'X'.
  GWA_ALV_PRINT-NO_PRINT_SELINFOS = 'X'.
  GWA_ALV_PRINT-NO_PRINT_LISTINFOS = 'X'.
  GWA_ALV_PRINT-NO_NEW_PAGE = 'X'.

*Sorting Option
  GWA_SORT-SPOS      =  1.
  GWA_SORT-FIELDNAME =  'LIFNR'(076).
  GWA_SORT-UP        = 'X'.
  GWA_SORT-SUBTOT    = 'X'.
  APPEND GWA_SORT TO GIT_SORT.
  CLEAR GWA_SORT.

*Assigning report Name
  GV_REP = SY-REPID .

  CLEAR GWA_EXTAB.
  GWA_EXTAB-FCODE = '&VEXCEL'.
  APPEND  GWA_EXTAB TO GIT_EXTAB.

  IF P_TEST NE GC_X.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = GV_REP
        IS_LAYOUT          = GWA_LAYOUT
        IT_FIELDCAT        = GIT_FIELDCAT_DISPLAY
        IT_EXCLUDING       = GIT_EXTAB
        IT_SORT            = GIT_SORT
        I_SAVE             = GC_A
*       is_variant         = gc_variant
        IS_PRINT           = GWA_ALV_PRINT
      TABLES
        T_OUTTAB           = GIT_ALV_DISPLAY
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSEIF P_TEST = GC_X.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = GV_REP
        IS_LAYOUT          = GWA_LAYOUT
        IT_FIELDCAT        = GIT_FIELDCAT_DISPLAY
        IT_EXCLUDING       = GIT_EXTAB
        IT_SORT            = GIT_SORT
        I_SAVE             = GC_A
*       is_variant         = gc_variant
        IS_PRINT           = GWA_ALV_PRINT
      TABLES
        T_OUTTAB           = GIT_WITH_TAX_FINAL
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.


ENDFORM.                    " ALV_FUNCTION_CALL
