FUNCTION Z_CHECK_TCURF.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(KURST) TYPE  KURST_CURR
*"     VALUE(FCURR) TYPE  FCURR_CURR
*"     VALUE(TCURR) TYPE  TCURR_CURR
*"  EXPORTING
*"     VALUE(EXISTS) TYPE  CHAR1
*"----------------------------------------------------------------------
  DATA: LV_TCURF LIKE TCURF.

  CLEAR EXISTS.
  SELECT SINGLE * FROM TCURF INTO LV_TCURF WHERE
    KURST = KURST AND
    FCURR = FCURR  AND
    TCURR = TCURR.
  IF SY-SUBRC = 0.
    EXISTS = 'X'.
  ENDIF.

ENDFUNCTION.
