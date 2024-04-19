FUNCTION zfi_get_manager .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_USERNAME) TYPE  WFSYST-INITIATOR OPTIONAL
*"     VALUE(IMP_PERNR) TYPE  PERNR_D OPTIONAL
*"     VALUE(IMP_RFCDEST) TYPE  TB_RFCDEST OPTIONAL
*"  EXPORTING
*"     VALUE(EXP_MANAGER) TYPE  WFSYST-INITIATOR
*"     VALUE(EXP_MANAGER_NUM) TYPE  PERNR_D
*"  EXCEPTIONS
*"      NOBODY_FOUND
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*

  DATA: lv_username TYPE username,
        lv_manager  TYPE username,
        lv_pernr    TYPE p_pernr,
        lv_log_dest TYPE tb_rfcdest.

  lv_username = imp_username+2(12).

  "Get RFC Destination Details
  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      imp_paramtype = 'HR'
    IMPORTING
      exp_rfcdest   = lv_log_dest.

  CALL FUNCTION 'ZFI_GET_MANAGER_DETAILS' DESTINATION lv_log_dest
    EXPORTING
      imp_username     = lv_username
      imp_pernr        = imp_pernr
    IMPORTING
      exp_nwid         = lv_manager
      exp_mgr_no       = lv_pernr
    EXCEPTIONS
      nobody_found     = 1
      no_manager_found = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
    RAISE nobody_found.
  ELSE.
    CONCATENATE 'US' lv_manager INTO exp_manager.
    exp_manager_num = lv_pernr.
  ENDIF.

ENDFUNCTION.
