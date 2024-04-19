FUNCTION Z_FATCA_CHECK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS
*"     VALUE(BELNR) TYPE  BELNR_D
*"     VALUE(GJAHR) TYPE  GJAHR
*"     VALUE(LIFNR) TYPE  LIFNR
*"  EXPORTING
*"     VALUE(NO_PASS) TYPE  FLAG
*"----------------------------------------------------------------------

  DATA: LV_QSSHB     TYPE WT_BS1,
        LV_EXDT      TYPE WT_EXDT,
        LV_ANS       TYPE XFELD,
        R_WITHT      TYPE RANGE OF LFBW-WITHT,
        R_WITHT_LINE LIKE LINE  OF R_WITHT.

  REFRESH R_WITHT.
  R_WITHT_LINE-SIGN = 'I'.
  R_WITHT_LINE-OPTION = 'EQ'.
  R_WITHT_LINE-LOW = '03'.
  APPEND R_WITHT_LINE TO R_WITHT.
  R_WITHT_LINE-LOW = '42'.
  APPEND R_WITHT_LINE TO R_WITHT.
  R_WITHT_LINE-LOW = '04'.
  APPEND R_WITHT_LINE TO R_WITHT.
  R_WITHT_LINE-LOW = '43'.
  APPEND R_WITHT_LINE TO R_WITHT.
  R_WITHT_LINE-LOW = '05'.
  APPEND R_WITHT_LINE TO R_WITHT.
  R_WITHT_LINE-LOW = '44'.
  APPEND R_WITHT_LINE TO R_WITHT.

  DATA: LV_BELNR  TYPE BELNR_D,
        LV_BUKRS TYPE BUKRS,
        LV_REFKEY TYPE AWKEY.

  CONCATENATE BELNR GJAHR INTO LV_REFKEY.

  SELECT SINGLE BELNR BUKRS INTO (LV_BELNR, LV_BUKRS) FROM BKPF WHERE AWKEY = LV_REFKEY.
  CHECK SY-SUBRC EQ 0.

  SELECT SINGLE WT_QSSHB FROM WITH_ITEM INTO LV_QSSHB
    WHERE BUKRS = LV_BUKRS AND
          BELNR = LV_BELNR AND
          GJAHR = GJAHR AND
          WITHT IN R_WITHT.
  IF SY-SUBRC NE 0 OR ( SY-SUBRC = 0 AND LV_QSSHB = 0 ).
    SELECT SINGLE WT_EXDT FROM LFBW INTO LV_EXDT
     WHERE LIFNR = LIFNR AND
           BUKRS = LV_BUKRS AND
           WITHT IN R_WITHT AND
           WT_SUBJCT = 'X'.
    IF SY-SUBRC = 0.
      CALL FUNCTION 'Z_POPUP_WITH_2_OPTIONS'
        EXPORTING
*         DEFAULTOPTION = '1'
          TEXTLINE1     = 'Was all or part of the services on this'
          TEXTLINE2     = 'invoice performed in the continental U.S.?'
          TEXTLINE3     = '(Note: If AP has already processed this invoice'
          TEXTLINE4     = 'for US Withholding Tax, answer No)'
*         TEXTLINE5     = ' '
          TEXT_OPTION1  = 'Yes'
          TEXT_OPTION2  = 'No'
          TITLE         = 'FATCA Requirement'
*         START_COLUMN  = 25
*         START_ROW     = 6
        IMPORTING
          ANSWER        = LV_ANS.
      IF LV_ANS = '1'.
        MESSAGE I000(ZFI_WORKFLOW) WITH 'Rejecting Back to AP for US Withholding Tax Processing'.
        NO_PASS = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.



ENDFUNCTION.
