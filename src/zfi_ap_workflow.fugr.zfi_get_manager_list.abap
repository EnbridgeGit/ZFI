FUNCTION zfi_get_manager_list.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_USERNAME) TYPE  WFSYST-INITIATOR
*"  TABLES
*"      GT_APPLIST STRUCTURE  ZFI_USER_LIST
*"----------------------------------------------------------------------
  DATA: ls_userlist TYPE zfi_user_list,
          lt_userlist TYPE STANDARD TABLE OF zfi_user_list.

  DATA: lv_username   TYPE wfsyst-initiator,
        lv_manager    TYPE wfsyst-initiator,
        lv_pernr      TYPE p_pernr.

  CONCATENATE 'US' imp_username INTO lv_username.

  CALL FUNCTION 'ZFI_GET_MANAGER'
    EXPORTING
      imp_username    = lv_username
    IMPORTING
      exp_manager     = lv_manager
      exp_manager_num = lv_pernr
    EXCEPTIONS
      nobody_found    = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    "No manager at all, could raise an exception.
    "Raise no_manager_found
    EXIT.
  ENDIF.

  ls_userlist-userid = lv_manager+2(12).
  ls_userlist-pernr  = lv_pernr.
  APPEND ls_userlist TO lt_userlist.

  WHILE lv_pernr <> ' '.
    CLEAR ls_userlist.
    lv_username = lv_manager.
    CALL FUNCTION 'ZFI_GET_MANAGER'
      EXPORTING
        imp_username    = lv_username
      IMPORTING
        exp_manager     = lv_manager
        exp_manager_num = lv_pernr
      EXCEPTIONS
        nobody_found    = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      "No manager at all, could raise an exception.
      "Raise no_manager_found
      EXIT.
    ELSE.
      ls_userlist-userid = lv_manager+2(12).
      ls_userlist-pernr  = lv_pernr.
      APPEND ls_userlist TO lt_userlist.
    ENDIF.
  ENDWHILE.

  gt_applist[] = lt_userlist[].




ENDFUNCTION.
