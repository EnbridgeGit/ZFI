FUNCTION zsrm_isnetcheck.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ISNVDR) TYPE  CHAR10
*"     VALUE(ILAND1) TYPE  LAND1_GP
*"     VALUE(IREGIO) TYPE  REGIO
*"  EXPORTING
*"     VALUE(INSQUAL) TYPE  CHAR1
*"----------------------------------------------------------------------
*begin of addition by AKEDIA on 11/22
  DATA: lv_zzisnvdr TYPE z_isnvdr.

  SELECT SINGLE zzisnvdr
    FROM lfa1
    INTO lv_zzisnvdr
   WHERE lifnr = isnvdr.

  IF sy-subrc = 0 AND lv_zzisnvdr IS NOT INITIAL.
*end of addition by AKEDIA on 11/22
    SELECT SINGLE isnqual
      FROM zisnqual
      INTO insqual
*      WHERE isnvdr = isnvdr  "commented by akedia on 11/22
      WHERE isnvdr = lv_zzisnvdr   "added by akedia on 11/22
        AND land1  = iland1
        AND regio  = iregio.
  ENDIF.                      "added by akedia on 11/22
ENDFUNCTION.
