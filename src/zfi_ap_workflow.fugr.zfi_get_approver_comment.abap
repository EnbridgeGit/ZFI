FUNCTION zfi_get_approver_comment.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_OBJECTKEY) TYPE  SWO_TYPEID
*"     REFERENCE(IMP_WORKITEM_ID) TYPE  SWW_WIID OPTIONAL
*"  TABLES
*"      USER_COMMENT STRUCTURE  ZFIT_APPRL_CMNT
*"----------------------------------------------------------------------

  DATA: lv_workitem_id  TYPE sww_wiid,
        lv_objectkey    TYPE swo_typeid,
        lv_wfuser       LIKE sy-uname.


  lv_workitem_id = imp_workitem_id.
  lv_objectkey = imp_objectkey.

  SELECT SINGLE value1
    FROM zvar
    INTO lv_wfuser
    WHERE programm = 'ALL'
      AND varname = 'WORKFLOWID'
    .

    SELECT *
      FROM zfit_apprl_cmnt
      INTO TABLE user_comment
      WHERE object_key = lv_objectkey
        AND USERNAME <> lv_wfuser
    .
      "AND workitem_id = lv_workitem_id.

  IF sy-subrc IS INITIAL.
    SORT user_comment DESCENDING BY timestamp.
  ENDIF.




ENDFUNCTION.
