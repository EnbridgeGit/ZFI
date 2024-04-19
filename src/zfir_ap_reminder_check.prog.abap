*&---------------------------------------------------------------------*
*& Report  ZFIR_AP_REMINDER_CHECK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfir_ap_reminder_check.


DATA: rangetab_for_id TYPE swfartwiid,
      rangetab_for_type TYPE swfartwitp,
      rangetab_for_creation_date TYPE swfartcrdat,
      rangetab_for_creation_time TYPE swfartcrtim,
      rangetab_for_task TYPE swfartrhtsk,
      rangetab_for_state TYPE swfartwista,
      rangetab_for_priority TYPE swfartprio,
      rangetab_for_dhsta TYPE swfartdhsta,
      lt_wrkitm TYPE swfatwrkitm,
      ls_wrkitm TYPE swfawrkitm,
      ls_wi2obj TYPE sww_wi2obj,
      int TYPE REF TO if_swf_rep_workitem_selection,
      ltp_string TYPE string,
      ltp_username TYPE sy-uname,
      lta_state TYPE swfartwista,
      lst_state TYPE swfarwista,
      lta_task TYPE swfartrhtsk,
      lst_task TYPE swfarrhtsk,
      ltp_return TYPE sysubrc,
      ltp_start_date TYPE syst-datum,
      ltp_bukrs TYPE bukrs,
      ltp_belnr TYPE belnr_d,
      ltp_gjahr TYPE gjahr,
      ltp_rc_owner TYPE wfsyst-initiator,
      lta_date TYPE swfartcrdat,
      lst_date TYPE swfarcrdat,
      lt_recipients TYPE swrtagent,
      ls_recipients TYPE swragent,
      lt_org_result TYPE swrtagent,
      lt_uname TYPE wfsyst-initiator,
      ltp_attr LIKE swr_wihdr.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.
PARAMETERS lv_task TYPE char10 DEFAULT 'TS02000013' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK a1.

START-OF-SELECTION.
  CALL FUNCTION 'ZFI_GET_START_DATE'
    EXPORTING
      imp_wf_type              = 'AP'
    IMPORTING
      exp_start_date           = ltp_start_date
    EXCEPTIONS
      invalid_factory_calendar = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  lst_state-sign = 'I'.
  lst_state-option = 'EQ'.
  lst_state-low = 'READY'.
  lst_state-high = ''.
  INSERT lst_state INTO TABLE lta_state.
  CLEAR lst_state.
  lst_state-sign = 'I'.
  lst_state-option = 'EQ'.
  lst_state-low = 'STARTED'.
  lst_state-high = ''.
  INSERT lst_state INTO TABLE lta_state.

  lst_task-sign = 'I'.
  lst_task-option = 'EQ'.
  lst_task-low = lv_task.
  lst_task-high = ''.
  INSERT lst_task INTO TABLE lta_task.
  lst_date-sign = 'I'.
  lst_date-option = 'BT'.
  lst_date-low = ltp_start_date.
  lst_date-high = sy-datum.
  INSERT lst_date INTO TABLE lta_date.

  IF int IS INITIAL.
    int = cl_swf_rep_manager=>get_instance( ).
  ENDIF.


*- convert parameters
*rangetab_for_id[] = id[].
*rangetab_for_type[] = type[].
  rangetab_for_creation_date[] = lta_date.
*rangetab_for_creation_time[] = ct[].
  rangetab_for_task[] = lta_task.
  rangetab_for_state[] = lta_state.
*rangetab_for_priority[] = prio[].
*rangetab_for_dhsta[] = dhsta[].

  CALL METHOD int->clear( ).
*CALL METHOD int->set_filter_strategy( filter ).

  CALL METHOD int->set_range_tab( rangetab_for_id ).
  CALL METHOD int->set_range_tab( rangetab_for_type ).
  CALL METHOD int->set_range_tab( rangetab_for_creation_date ).
  CALL METHOD int->set_range_tab( rangetab_for_creation_time ).
  CALL METHOD int->set_range_tab( rangetab_for_task ).
  CALL METHOD int->set_range_tab( rangetab_for_state ).
  CALL METHOD int->set_range_tab( rangetab_for_priority ).
  CALL METHOD int->set_range_tab( rangetab_for_dhsta ).

  CALL METHOD int->read( ).

  CALL METHOD int->get_entries
    IMPORTING
      ex_wientries = lt_wrkitm.

  LOOP AT lt_wrkitm INTO ls_wrkitm.


*  CALL FUNCTION 'SAP_WAPI_PUT_BACK_WORKITEM'
*  EXPORTING
*    workitem_id          = ls_wrkitm-wi_id
**         USER                 = SY-UNAME
**         LANGUAGE             = SY-LANGU
*         DO_COMMIT            = 'X'
*       IMPORTING
**         NEW_STATUS           =
**         RETURN_CODE          =
**       TABLES
**         MESSAGE_LINES        =
**         MESSAGE_STRUCT       =
*          .

    CALL FUNCTION 'SAP_WAPI_WORKITEM_RECIPIENTS'
      EXPORTING
        workitem_id           = ls_wrkitm-wi_id
        language              = sy-langu
        user                  = sy-uname
*      check_possible_agents = ' '
      IMPORTING
        return_code           = ltp_return
      TABLES
        recipients            = lt_recipients
*     MESSAGE_LINES         =
*     MESSAGE_STRUCT        =
        original_rule_result  = lt_org_result.

    READ TABLE lt_recipients INTO ls_recipients INDEX 1.
    IF sy-subrc IS INITIAL.

    ENDIF.
    SELECT SINGLE * FROM sww_wi2obj INTO ls_wi2obj
      WHERE wi_id = ls_wrkitm-wi_id
        AND wi_rh_task = lv_task
        AND catid = 'BO'
        AND typeid = 'FIPP'.

    ltp_bukrs = ls_wi2obj-instid(4).
    ltp_belnr = ls_wi2obj-instid+4(10).
    ltp_gjahr = ls_wi2obj-instid+14(4).

    CALL FUNCTION 'ZFI_AP_GET_ROUTE_CODE_OWNER'
      EXPORTING
        imp_bukrs    = ltp_bukrs
        imp_belnr    = ltp_belnr
        imp_gjahr    = ltp_gjahr
      IMPORTING
        exp_rc_owner = ltp_rc_owner
      EXCEPTIONS
        nobody_found = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.

    IF ls_recipients-objid = ltp_rc_owner+2.
      CONCATENATE ls_wi2obj-instid ' :Forwarding not required' INTO ltp_string.
    ELSE.
      ltp_username = ltp_rc_owner+2.
      CALL FUNCTION 'ZFI_FORWARD_WORKITEM_2_USER'
        EXPORTING
          imp_workitem_id = ls_wrkitm-wi_id
          imp_username    = ltp_username
        EXCEPTIONS
          error           = 1.
      IF sy-subrc IS INITIAL.
        CONCATENATE ls_wi2obj-instid ' :Success' INTO ltp_string.
        lt_uname = ls_recipients-objid.
        CONCATENATE 'US' lt_uname INTO lt_uname.

        CALL FUNCTION 'ZFI_AP_NOTIFICATION'
          EXPORTING
            imp_bukrs       = ltp_bukrs
            imp_belnr       = ltp_belnr
            imp_gjahr       = ltp_gjahr
            imp_username    = lt_uname"ls_recipients-objidChange
            imp_user_action = 'REMI'
            imp_comments    = 'Forwarded Due maximum time hold is over'.
      ELSE.
        CONCATENATE ls_wi2obj-instid ' :Unable to Forward' INTO ltp_string.
      ENDIF.
    ENDIF.

    WRITE / ltp_string.
    CLEAR ls_wi2obj.
  ENDLOOP.
