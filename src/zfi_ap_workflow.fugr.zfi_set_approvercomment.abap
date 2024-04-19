FUNCTION zfi_set_approvercomment.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_WORKITEM_ID) TYPE  SWW_WIID
*"     REFERENCE(IMP_OBJECT_KEY) TYPE  SWO_TYPEID
*"     REFERENCE(IMP_USERNAME) TYPE  USERNAME
*"     REFERENCE(IMP_COMMENTS) TYPE  CHAR255
*"----------------------------------------------------------------------

  DATA: lwa_zfit_apprl_cmnt TYPE zfit_apprl_cmnt,
         lv_timestamp TYPE timestamp.

  GET TIME STAMP FIELD lv_timestamp.

  lwa_zfit_apprl_cmnt-workitem_id = imp_workitem_id.
  lwa_zfit_apprl_cmnt-object_key  = imp_object_key.
  lwa_zfit_apprl_cmnt-username    = imp_username.
  lwa_zfit_apprl_cmnt-comments    = imp_comments.


  lwa_zfit_apprl_cmnt-timestamp = lv_timestamp.

  MODIFY zfit_apprl_cmnt FROM lwa_zfit_apprl_cmnt.
  IF sy-subrc = 0.

  ENDIF.



ENDFUNCTION.
