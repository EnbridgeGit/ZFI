FUNCTION zfi_tv_get_manager.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_TNE_ADMIN) TYPE  CHAR1
*"     VALUE(IMP_PERNR) TYPE  PERNR-PERNR OPTIONAL
*"     VALUE(IMP_USERID) TYPE  WFSYST-INITIATOR OPTIONAL
*"  EXPORTING
*"     REFERENCE(EXP_MANAGER) TYPE  ZFITT_INITIATOR
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
*Revision # MZH01                                 Name: Zakir Hossain  *
*SDP Ticket # ACR-3515                            Date: 3/21/2017      *
*Description: Call new function module to get manager from HR systems  *
*&---------------------------------------------------------------------*
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
*17-MAY-2019  KBANERJEE  D30K929852    FUT issues                      *
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

  DATA: ltp_tneadmin TYPE xubname,
        ltp_username TYPE xubname.
  DATA: ltp_log_dest TYPE tb_rfcdest,
        lta_users TYPE STANDARD TABLE OF agr_users,
        lst_users TYPE agr_users,
        lst_user1 TYPE username,
        ltp_pernr TYPE pernr_d.

  DATA: gc_tne_admin TYPE agr_name VALUE 'Z:BC_WORKFLOW'.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
  DATA:lv_setid    TYPE setid.
  DATA:lt_approvers TYPE STANDARD TABLE OF rgsb4     INITIAL SIZE 0,
       ls_approvers TYPE rgsb4.
  DATA:ls_exp_manager TYPE zfi_initiator.
  DATA:lv_mloop TYPE i.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
  CONSTANTS : lc_set TYPE c LENGTH 18 VALUE 'ZINACTIVE_APPOVERS'. "Added by KMB on 06.11.2019 inactive employees for CHG0165377
  IF imp_tne_admin = 'N'.
* Get RFC Destination Details
    CALL FUNCTION 'ZFI_GET_RFC_DEST'
      EXPORTING
        imp_paramtype = 'HR'
      IMPORTING
        exp_rfcdest   = ltp_log_dest.

*    Start of change by ddwivedi on /02.

    lst_user1 = imp_userid+2.

    CALL FUNCTION 'ZFI_CONVERT_USER_TO_PERNR' DESTINATION ltp_log_dest
      EXPORTING
        imp_username = lst_user1 "imp_userid commented by ddwivedi on 3/7.
      IMPORTING
        exp_pernr    = ltp_pernr.

*Begin of comment MZH01
*   End of change by ddwivedi on 3/02.
*   Get Manager of the User
*    CALL FUNCTION 'ZFI_GET_MANAGER_DETAILS' DESTINATION ltp_log_dest
*      EXPORTING
**       imp_username    = ltp_username
*        imp_pernr       = ltp_pernr "imp_pernr commented by ddwivedi on 3/02
*      IMPORTING
*        exp_nwid        = ltp_username
*      EXCEPTIONS
*        NOBODY_FOUND
*        NO_MANAGER_FOUND.
*End of comment MZH01

* Begin of MZH01
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
**   Get Manager of the User
*    CALL FUNCTION 'ZFI_GET_MGR_DETAILS_NEW' DESTINATION ltp_log_dest
*      EXPORTING
*        imp_pernr        = ltp_pernr
*      IMPORTING
*        exp_nwid         = ltp_username
*      EXCEPTIONS
*        nobody_found     = 1
*        no_manager_found = 2.
*   Get Manager of the User and also get the no of times
*   manager determination logic is executed
    CALL FUNCTION 'ZFI_GET_MGR_DETAILS_CPY' DESTINATION ltp_log_dest
      EXPORTING
        imp_pernr        = ltp_pernr
      IMPORTING
        exp_nwid         = ltp_username
        exp_mloop        = lv_mloop
      EXCEPTIONS
        nobody_found     = 1
        no_manager_found = 2.
    IF sy-subrc IS INITIAL.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
* End of MZH01

      IF ltp_username IS INITIAL
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
       OR lv_mloop GT 1."Manager loop executed more than once
*Do not include the approvers with T & E Admin role
** Get T and E Admin User
*      CALL FUNCTION 'RSRA_USERS_OF_AGR_GET'
*        EXPORTING
*          i_agr_name            = gc_tne_admin
*        TABLES
*          activity_groups_users = lta_users
*        EXCEPTIONS
*          no_user_available     = 1
*          OTHERS                = 2.
*
*      IF sy-subrc EQ 0.
*        READ TABLE lta_users INTO lst_users INDEX 1.
*        CONCATENATE 'US' lst_users-uname INTO exp_manager.
*      ENDIF.
*Get the approvers maintained in GS01
        CALL FUNCTION 'G_SET_GET_ID_FROM_NAME'
          EXPORTING
            shortname                = 'ZINACTIVE_APPOVERS'
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
              CONCATENATE 'US' ls_approvers-from INTO ls_exp_manager-mgr.
            ENDIF.
            IF ls_approvers-to IS NOT INITIAL.
              CONCATENATE 'US' ls_approvers-to INTO ls_exp_manager-mgr.
            ENDIF.
            APPEND ls_exp_manager TO exp_manager. "Added by KMB on 17.9.2019 FOR CHG0138931
            SORT exp_manager BY mgr.
            DELETE ADJACENT DUPLICATES FROM exp_manager COMPARING mgr.
*BOC by KMB on 17.9.2019 FOR CHG0138931
*            IF exp_manager IS NOT INITIAL.
*              APPEND ls_exp_manager TO exp_manager.
*            ENDIF.
*EOC by KMB on 17.9.2019 FOR CHG0138931
            CLEAR:ls_approvers,ls_exp_manager.
          ENDLOOP.
        ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
      ELSE.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
*Expense manager is a return table now not variable
*      CONCATENATE 'US' ltp_username INTO exp_manager.
        CONCATENATE 'US' ltp_username INTO ls_exp_manager.
        APPEND ls_exp_manager TO exp_manager.
        CLEAR ls_exp_manager.
*END OF CHANGES BY KBANERJEE FOR CHG0138931
      ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
    ENDIF."sy-subrc check after get manager FM
*END OF CHANGES BY KBANERJEE FOR CHG0138931
  ELSEIF imp_tne_admin = 'Y'.
*BOC by KMB on 06.11.2019 inactive employees for CHG0165377
* For inactive employees approvers are determined from SET instead of admin user
** Get T and E Admin User
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
*      READ TABLE lta_users INTO lst_users INDEX 1.
**BEGIN OF CHANGES BY KBANERJEE FOR CHG0138931
**Expense manager is a return table now not variable
**      CONCATENATE 'US' lst_users-uname INTO exp_manager.
*      CONCATENATE 'US' lst_users-uname INTO ls_exp_manager.
*      APPEND ls_exp_manager TO exp_manager.
*      CLEAR ls_exp_manager.
**END OF CHANGES BY KBANERJEE FOR CHG0138931
*    ENDIF.
*get the approvers maintained in gs01
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
          CONCATENATE 'US' ls_approvers-from INTO ls_exp_manager-mgr.
        ENDIF.
        IF ls_approvers-to IS NOT INITIAL.
          CONCATENATE 'US' ls_approvers-to INTO ls_exp_manager-mgr.
        ENDIF.
*BOC by KMB on 11.11.2019 inactive employees for CHG0165377
*        SORT exp_manager BY mgr.
*        DELETE ADJACENT DUPLICATES FROM exp_manager COMPARING mgr.
*EOC by KMB on 11.11.2019 inactive employees for CHG0165377
        APPEND ls_exp_manager TO exp_manager.
        CLEAR:ls_approvers,ls_exp_manager.
      ENDLOOP.
*BOC by KMB on 11.11.2019 inactive employees for CHG0165377
      SORT exp_manager BY mgr.
      DELETE ADJACENT DUPLICATES FROM exp_manager COMPARING mgr.
*EOC by KMB on 11.11.2019 inactive employees for CHG0165377
    ENDIF.
*EOC by KMB on 06.11.2019 inactive employees for CHG0165377
  ENDIF.

ENDFUNCTION.
