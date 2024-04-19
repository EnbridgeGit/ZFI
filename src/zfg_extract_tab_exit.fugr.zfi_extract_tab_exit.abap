FUNCTION zfi_extract_tab_exit.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF gty_tabname,
          value1   TYPE tabname,
          value2   TYPE zparamvalue,
        END OF gty_tabname.
  DATA:gc_extract TYPE zparamtype    VALUE 'AUDIT_EXTRACT',
       gc_tabname TYPE zparamsubtype VALUE 'TABNAME'.
  DATA:gt_tabname TYPE STANDARD TABLE OF gty_tabname
                  INITIAL SIZE 0.
  DATA:gwa_tabname    TYPE gty_tabname.
  IF callcontrol-step = 'DISP'.
    SELECT  value1 value2
      FROM  zfit_xparam
 INTO TABLE gt_tabname
      WHERE paramtype = gc_extract
        AND subtype   = gc_tabname.
    IF sy-subrc IS INITIAL.
      SORT gt_tabname BY value1.
    ENDIF.
    IF gt_tabname IS NOT INITIAL.
      LOOP AT gt_tabname INTO gwa_tabname.
        CONCATENATE gwa_tabname-value1
                    gwa_tabname-value2
               INTO record_tab-string
               SEPARATED BY '|'.
*        record_tab-string(30)    = gwa_tabname-value1.
*        record_tab-string+31(60) = gwa_tabname-value2.
        APPEND record_tab.
        CLEAR:gwa_tabname.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFUNCTION.
