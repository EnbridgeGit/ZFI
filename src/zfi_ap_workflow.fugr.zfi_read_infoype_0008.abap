FUNCTION ZFI_READ_INFOYPE_0008.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_PERNR) TYPE  PERNR_D
*"     VALUE(IMP_INFTY) TYPE  PRELP-INFTY
*"  EXPORTING
*"     VALUE(EXP_INFOTYPE_0008) TYPE  HRFRPBS4_INFTY0008_TAB
*"  EXCEPTIONS
*"      INFTY_NOT_FOUND
*"----------------------------------------------------------------------
 CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
      tclas           = 'A'
      pernr           = imp_pernr
      infty           = imp_infty
      begda           = sy-datum
      endda           = sy-datum
      bypass_buffer   = ' '
      legacy_mode     = ' '
    TABLES
      infty_tab       = exp_infotype_0008
    EXCEPTIONS
      infty_not_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.




ENDFUNCTION.
