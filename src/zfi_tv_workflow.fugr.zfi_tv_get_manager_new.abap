FUNCTION zfi_tv_get_manager_new.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      ACTOR_TAB STRUCTURE  SWHACTOR
*"      AC_CONTAINER STRUCTURE  SWCONT
*"  EXCEPTIONS
*"      NOBODY_FOUND
*"----------------------------------------------------------------------
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 04-APR-2019  KBANERJEE   D30K929753  CHG0138931 - For inactive       *
*                                      employees determined as approver*
*                                      function module should only     *
*                                      return the set of employees     *
*                                      maintained as approver in GS01  *
*                                      It should not return all        *
*                                      employees assigned to the role  *
*                                      as approvers.                   *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 17-SEP-2019  KMB         DHRK916330  CHG0138931 FUT issues           *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 06-NOV-2019  KMB         D30K930248  CHG0165377 For inactive         *
*                                      employees determined as approver*
*                                      function module should only     *
*                                      return the set of employees     *
*                                      maintained as approver in GS01  *
*                                      It should not return all        *
*                                      employees assigned to the role  *
*                                      as approvers.                   *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 11-NOV-2019  KMB         D30K930279  CHG0165377 For inactive         *
*                                      employees determined as approver*
*                                      function module should only     *
*                                      return the set of employees     *
*                                      maintained as approver in GS01  *
*                                      It should not return all        *
*                                      employees assigned to the role  *
*                                      as approvers.                   *
************************************************************************

  INCLUDE <cntn01>.

*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
  TYPES:BEGIN OF lty_approver,
         aprvr TYPE swhactor-objid,
    END OF lty_approver.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
  DATA:  lt_actors TYPE STANDARD TABLE OF swhactor,
         wa_actor TYPE swhactor,
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
*         lt_approver LIKE swhactor-objid.
         lt_approver TYPE STANDARD TABLE OF lty_approver
                     INITIAL SIZE 0,
         ls_approver TYPE lty_approver.
*END OF CHANGES BY KBANERJEE FOR CHG0138931

  DATA: tne TYPE char1,
        userid TYPE swp_initia,
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
*        manager TYPE swp_initia,
         lt_manager TYPE STANDARD TABLE OF zfi_initiator,
         ls_manager TYPE zfi_initiator,
*END OF CHANGES BY KBANERJEE FOR CHG0138931
        loop TYPE swp_prio,
        w_role TYPE c,
        findmanager TYPE char1,
        lta_users TYPE STANDARD TABLE OF agr_users,
        lst_users TYPE agr_users.
*BOC by KMB on 06.11.2019 inactive employees for CHG0165377
  DATA:lv_setid       TYPE setid.
  DATA:lt_approvers   TYPE STANDARD TABLE OF rgsb4
                      INITIAL SIZE 0,
       ls_approvers   TYPE rgsb4.
  CONSTANTS : lc_set TYPE c LENGTH 18 VALUE 'ZINACTIVE_APPOVERS'.
*EOC by KMB on 06.11.2019 inactive employees for CHG0165377

  DATA: gc_tne_admin TYPE agr_name VALUE 'Z:BC_WORKFLOW'.

  swc_get_element ac_container 'USER' userid.
  swc_get_element ac_container 'LOOP' loop.
  swc_get_element ac_container 'FINDMANAGER' findmanager.

  IF findmanager = 'X'.
    IF loop LE '3'.
      tne = 'N'.
      IF userid = 'USGLEBEL'.   " Ticket 18794
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
*        lt_approver = 'ACCAPPS1'.
        ls_approver-aprvr = 'ACCAPPS1'.
        APPEND ls_approver TO lt_approver.
        CLEAR ls_approver.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
      ELSE.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
*        CALL FUNCTION 'ZFI_TV_GET_MANAGER'
*          EXPORTING
*            imp_tne_admin = tne
*            imp_userid    = userid
*          IMPORTING
*            exp_manager   = manager.
*        IF NOT manager IS INITIAL.
*          lt_approver = manager+2(12).
        CALL FUNCTION 'ZFI_TV_GET_MANAGER'
          EXPORTING
            imp_tne_admin = tne
            imp_userid    = userid
          IMPORTING
            exp_manager   = lt_manager.
        IF NOT lt_manager IS INITIAL.
          LOOP AT lt_manager INTO ls_manager.
            ls_approver-aprvr = ls_manager-mgr+2(12).
            APPEND ls_approver TO lt_approver.
            CLEAR:ls_approver,ls_manager.
          ENDLOOP.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
*BOC by KMB on 18.9.2019 FOR CHG0138931 11.11.2019 Uncommenting w_role = 'X'
*BOC by KMB on 18.9.2019 FOR CHG0138931
        ELSE.
          w_role = 'X'.
*EOC by KMB on 18.9.2019 FOR CHG0138931
*EOC by KMB on 18.9.2019 FOR CHG0138931 11.11.2019 Uncommenting w_role = 'X'
        ENDIF.
      ENDIF.
      IF NOT lt_approver IS INITIAL.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
        LOOP AT lt_approver INTO ls_approver.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
          wa_actor-otype = 'US'.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
*        wa_actor-objid = lt_approver.
          wa_actor-objid = ls_approver-aprvr.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
          APPEND wa_actor TO lt_actors.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
          CLEAR:ls_approver,wa_actor.
        ENDLOOP.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
        actor_tab[] = lt_actors[].
      ENDIF.
*BOC by KMB on 18.9.2019 FOR CHG0138931 11.11.2019 Uncommenting w_role = 'X'
*BOC by KMB on 18.9.2019 FOR CHG0138931
    ELSE.
      w_role = 'X'.
*      CALL FUNCTION 'ZFI_TV_GET_MANAGER'
*        EXPORTING
*          imp_tne_admin = tne
*          imp_userid    = userid
*        IMPORTING
*          exp_manager   = lt_manager.
*      IF NOT lt_manager IS INITIAL.
*        LOOP AT lt_manager INTO ls_manager.
*          ls_approver-aprvr = ls_manager-mgr+2(12).
*          APPEND ls_approver TO lt_approver.
*          CLEAR:ls_approver,ls_manager.
*        ENDLOOP.
*      ENDIF.
*EOC by KMB on 18.9.2019 FOR CHG0138931
*EOC by KMB on 18.9.2019 FOR CHG0138931 11.11.2019 Uncommenting w_role = 'X'
    ENDIF.
  ELSE.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
*    lt_approver = userid+2(12).
    ls_approver-aprvr = userid+2(12).
*END OF CHANGES BY KBANERJEE FOR CHG0138931
    wa_actor-otype = 'US'.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
*    wa_actor-objid = lt_approver.
    wa_actor-objid = ls_approver-aprvr.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
    APPEND wa_actor TO lt_actors.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
    CLEAR:ls_approver,wa_actor.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
    actor_tab[] = lt_actors[].
  ENDIF.

*BOC by KMB on 18.9.2019 FOR CHG0138931
*  IF w_role = 'X'.
*    CALL FUNCTION 'RSRA_USERS_OF_AGR_GET'
*      EXPORTING
*        i_agr_name            = gc_tne_admin
*      TABLES
*        activity_groups_users = lta_users
*      EXCEPTIONS
*        no_user_available     = 1
*        OTHERS                = 2.
*
*    IF sy-subrc EQ 0.
*      LOOP AT lta_users INTO lst_users.
*        wa_actor-otype = 'US'.
*        wa_actor-objid = lst_users-uname.
*        APPEND wa_actor TO lt_actors.
*      ENDLOOP.
*      actor_tab[] = lt_actors[].
*    ENDIF.
*  ENDIF.
*EOC by KMB on 18.9.2019 FOR CHG0138931

*BOC by KMB on 06.11.2019 inactive employees for CHG0165377
  IF w_role = 'X'.
*Get the approvers maintained in GS01
    CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
      EXPORTING
        shortname                = lc_set
      IMPORTING
        new_setid                = lv_setid
      EXCEPTIONS
        no_set_found             = 1
        no_set_picked_from_popup = 2
        wrong_class              = 3
        wrong_subclass           = 4
        table_field_not_found    = 5
        fields_dont_match        = 6
        set_is_empty             = 7
        formula_in_set           = 8
        set_is_dynamic           = 9
        OTHERS                   = 10.
    IF sy-subrc = 0.
*Get values from 'AP_TEAM' set id
      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          setnr         = lv_setid
        TABLES
          set_values    = lt_approvers
        EXCEPTIONS
          set_not_found = 1
          OTHERS        = 2.
      IF sy-subrc = 0.
* Implement suitable error handling here
        SORT lt_approvers BY from.
      ENDIF.
    ENDIF.
    IF lt_approvers IS NOT INITIAL.
      LOOP AT lt_approvers INTO ls_approvers.
        IF ls_approvers-from IS NOT INITIAL.
          MOVE ls_approvers-from TO wa_actor-objid.
          MOVE 'US' TO wa_actor-otype.
        ENDIF.
        IF ls_approvers-to IS NOT INITIAL.
          MOVE ls_approvers-to TO wa_actor-objid.
          MOVE 'US' TO wa_actor-otype.
        ENDIF.
        APPEND wa_actor TO lt_actors.
        CLEAR:ls_approvers,wa_actor.
      ENDLOOP.
      actor_tab[] = lt_actors[].
    ENDIF.
  ENDIF.
*EOC by KMB on 06.11.2019 inactive employees for CHG0165377

  READ TABLE actor_tab INDEX 1 INTO wa_actor.
  IF sy-subrc NE 0.
    RAISE nobody_found.
  ENDIF.




ENDFUNCTION.
