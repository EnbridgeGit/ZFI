*&---------------------------------------------------------------------*
*&  Include           LZFI_BP_WORKFLOWF02
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  SET_MAIL_BODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mail_body .

  DATA: lst_objtxt     TYPE solisti1,
        lst_but000 TYPE but000,
        w_usr TYPE BU_CHUSR.

  CONCATENATE 'Dear User,' lst_objtxt-line INTO lst_objtxt-line SEPARATED BY space.
  INSERT lst_objtxt INTO TABLE gta_objtxt.
  CLEAR lst_objtxt-line.
  INSERT lst_objtxt INTO TABLE gta_objtxt.

  SELECT SINGLE * FROM but000 INTO lst_but000 WHERE partner = gtp_businesspartner.
* Insert by SPARGA tkt 20743
  clear w_usr.
  IF not lst_but000-chusr is INITIAL.
    w_usr = lst_but000-chusr.
  ELSE.
    w_usr = lst_but000-crusr.
  ENDIF.
* End of Insert SPARGA


  CASE gtp_decision_type.
    WHEN 'NOTI'.
*     Set Suubject line.
      CONCATENATE gtp_businesspartner 'Business Partner Approval Required' INTO gst_doc_chng-obj_descr SEPARATED BY space.
*     Set mail body
      CONCATENATE 'Business Partner' gtp_businesspartner 'changed by' w_usr 'is waiting for your approval. Please do the needful.'
        INTO lst_objtxt-line SEPARATED BY space.

    WHEN 'REMI'.
*     Set Suubject line.
      CONCATENATE gtp_businesspartner 'Business Partner Reminder for Approval' INTO gst_doc_chng-obj_descr SEPARATED BY space.
*     Set mail body
      CONCATENATE 'REMINDER:Business Partner' gtp_businesspartner 'changed by' w_usr 'is waiting for your approval. Please do the needful.'
        INTO lst_objtxt-line SEPARATED BY space.

    WHEN 'CORE'.
*     Set Suubject line.
      CONCATENATE gtp_businesspartner 'Business Partner Rejected' INTO gst_doc_chng-obj_descr SEPARATED BY space.
*     Set mail body
      CONCATENATE 'Business Partner' gtp_businesspartner 'changed by' w_usr 'has been rejected by' sy-uname "SPARGA tkt 20743
         INTO lst_objtxt-line SEPARATED BY space.

    WHEN 'COAP'.
*     Set Suubject line.
      CONCATENATE gtp_businesspartner 'Business Partner Approved' INTO gst_doc_chng-obj_descr SEPARATED BY space.
*     Set mail body
      CONCATENATE 'Business Partner' gtp_businesspartner 'changed by' w_usr 'has been approved by' sy-uname "SPARGA tkt 20743
        INTO lst_objtxt-line SEPARATED BY space.

    WHEN 'DELE'.
*     Set Suubject line.
      CONCATENATE gtp_businesspartner 'Business Parther Deleted' INTO gst_doc_chng-obj_descr SEPARATED BY space.
*     Set mail body
      CONCATENATE 'Business Partner' gtp_businesspartner 'changed by' w_usr 'has been deleted.'
        INTO lst_objtxt-line SEPARATED BY space.

    WHEN 'FORW'.
*     Set Suubject line.
      CONCATENATE gtp_businesspartner 'Business Partner forwarded' INTO gst_doc_chng-obj_descr SEPARATED BY space.
*     Set mail body
      CONCATENATE 'Business Partner' gtp_businesspartner 'changed by' w_usr 'has been forwarded.'
        INTO lst_objtxt-line SEPARATED BY space.

  ENDCASE.

  INSERT lst_objtxt INTO TABLE gta_objtxt.
  CLEAR lst_objtxt-line.
  INSERT lst_objtxt INTO TABLE gta_objtxt.

  IF gtp_comment IS INITIAL.
    CONCATENATE gtp_comment 'NO USER COMMENT' INTO lst_objtxt-line SEPARATED BY space.
  ELSE.
    CONCATENATE 'User Comments - ' gtp_comment INTO
      lst_objtxt-line SEPARATED BY space.
  ENDIF.
  INSERT lst_objtxt INTO TABLE gta_objtxt.
  CLEAR lst_objtxt-line.
  INSERT lst_objtxt INTO TABLE gta_objtxt.

  lst_objtxt =  'This is System generated mail.Do not reply to this mail'.

ENDFORM.                    " SET_MAIL_BODY

*&---------------------------------------------------------------------*
*&      Form  SET_MAILING_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mailing_parameters .
  DATA: ltp_tab_lines  TYPE sy-tabix,
        lst_objpack    TYPE sopcklsti1,
        lst_objbin     TYPE solisti1.

* Creation of the entry for the document
  DESCRIBE TABLE gta_objtxt LINES ltp_tab_lines.
  CLEAR: lst_objpack-transf_bin.
  lst_objpack-head_start = 1.
  lst_objpack-head_num   = 0.
  lst_objpack-body_start = 1.
  lst_objpack-body_num   = ltp_tab_lines.
  lst_objpack-doc_type   = 'RAW'.
  INSERT lst_objpack INTO TABLE gta_objpack.
ENDFORM.                    " SET_MAILING_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  GET_EMAIL_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LST_BUT000_CRUSR  text
*      -->P_0026   text
*----------------------------------------------------------------------*
FORM get_email_id  USING im_username TYPE username
                         im_copy     TYPE flag.

  DATA: ltp_email_id TYPE  ad_smtpadr,
        lst_reclist  TYPE somlreci1.
  DATA: ltp_log_dest TYPE tb_rfcdest.
  DATA: lst_details  TYPE ZHRGRADE_INFO.

* Get RFC Destination Details
  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      IMP_PARAMTYPE = 'HR'
    IMPORTING
      exp_rfcdest = ltp_log_dest.

* Get EMail ID of Approver from DHR
  CALL FUNCTION 'ZFI_GET_EMAIL_ID' DESTINATION ltp_log_dest
    EXPORTING
      IMP_USER_ID           = im_username
    IMPORTING
      EXP_EMAIL_ADDRESS     = ltp_email_id
    EXCEPTIONS
      SYSTEM_FAILURE        = 1
      COMMUNICATION_FAILURE = 2.

  IF sy-subrc = 0 and ltp_email_id is not INITIAL.
    lst_reclist-receiver = ltp_email_id.
    lst_reclist-copy = im_copy.
    lst_reclist-rec_type = 'U'.
    INSERT  lst_reclist INTO TABLE gta_reclist.

*  else.
** Get EMail ID of Approver from HR data mart
*    CALL FUNCTION 'ZFI_GET_HRDATA' DESTINATION ltp_log_dest
*      EXPORTING
*        IMP_USERIS  = im_username
*      IMPORTING
*        EXP_DETAILS = lst_details.
*
*    IF sy-subrc = 0 and lst_details is not INITIAL.
*      ltp_email_id = lst_details-MANAGER_NET_ID.
*      lst_reclist-receiver = lst_details-MANAGER_NET_ID.
*      lst_reclist-copy = im_copy.
*      lst_reclist-rec_type = 'U'.
*      INSERT  lst_reclist INTO TABLE gta_reclist.
*    endif.
  ENDIF.

  IF ltp_email_id IS INITIAL.

    DATA: lst_usr21 TYPE usr21.

    SELECT SINGLE * FROM usr21 INTO lst_usr21
      WHERE bname = im_username.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE smtp_addr FROM adr6 INTO ltp_email_id WHERE addrnumber = lst_usr21-addrnumber
        AND persnumber = lst_usr21-persnumber.
      IF sy-subrc = 0.
        lst_reclist-receiver = ltp_email_id.
        lst_reclist-copy = im_copy.
        lst_reclist-rec_type = 'U'.
        INSERT  lst_reclist INTO TABLE gta_reclist.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.                    " GET_EMAIL_ID
*&---------------------------------------------------------------------*
*&      Form  GET_RECIPENT_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM get_recipent_list .

  DATA: lst_but000   TYPE but000,
        ltp_objkey   TYPE swo_typeid,
        ltp_wi       TYPE sww_wiid,
        lta_wi_user  TYPE STANDARD TABLE OF swwuserwi,
        lwa_wi_user  TYPE swwuserwi,
        lta_agr_user TYPE STANDARD TABLE OF agr_users,
        lwa_agr_user TYPE agr_users,
        lv_role      TYPE AGR_NAME,
        lv_low       TYPE tvarv-low.

  REFRESH: gta_reclist.

  ltp_objkey = gtp_businesspartner.

  select single low from TVARV
  into lv_low
  where NAME = 'ZFI_TREASURY_ADMIN'.

  if sy-subrc = 0.
    lv_role = lv_low.
  endif.

* Get Creator of Business Partner
  SELECT SINGLE * FROM but000 INTO lst_but000 WHERE partner = gtp_businesspartner.
  IF sy-subrc IS INITIAL.

  ENDIF.

  IF gtp_decision_type = 'NOTI'  OR gtp_decision_type = 'COAP' or gtp_decision_type = 'CORE'. "SPARGA tkt 20743
    CALL FUNCTION 'RSRA_USERS_OF_AGR_GET'
      EXPORTING
        i_agr_name            = lv_role
*       TIME_DEPENDENT        = 'X'
      TABLES
        activity_groups_users = lta_agr_user
      EXCEPTIONS
        no_user_available     = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
    IF NOT lta_agr_user IS INITIAL.
      LOOP AT lta_agr_user into lwa_agr_user.
        PERFORM get_email_id USING lwa_agr_user-uname space.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF gtp_decision_type = 'COAP'.
* Insert by SPARGA tkt 20743
    IF not lst_but000-chusr is INITIAL.
      PERFORM get_email_id USING lst_but000-chusr space.
    ELSE.
      PERFORM get_email_id USING lst_but000-crusr space.
    ENDIF.
* End of Insert SPARGA
    PERFORM get_email_id USING sy-uname space.
  ENDIF.

  IF gtp_decision_type = 'CORE'.
* Insert by SPARGA tkt 20743
    IF not lst_but000-chusr is INITIAL.
      PERFORM get_email_id USING lst_but000-chusr space.
    ELSE.
      PERFORM get_email_id USING lst_but000-crusr space.
    ENDIF.
* End of Insert SPARGA
    PERFORM get_email_id USING sy-uname space.
  ENDIF.

  IF gtp_decision_type = 'REMI'.
*  Get agent for workitem
    SELECT SINGLE wi FROM zfit_wi_key INTO ltp_wi WHERE objkey = ltp_objkey.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'RH_SWWUSERWI_READ_FOR_WI'
        EXPORTING
          wi_id          = ltp_wi
        TABLES
          act_wi_of_user = lta_wi_user
        EXCEPTIONS
          nothing_found  = 1
          OTHERS         = 2.
      IF sy-subrc = 0.

      ENDIF.
      IF NOT lta_wi_user IS INITIAL.
        LOOP AT lta_wi_user INTO lwa_wi_user.
          PERFORM get_email_id USING lwa_wi_user-user_id space.
        ENDLOOP.
      ENDIF.

    ELSE.

      CALL FUNCTION 'RSRA_USERS_OF_AGR_GET'
        EXPORTING
          i_agr_name            = lv_role
*           TIME_DEPENDENT        = 'X'
        TABLES
          activity_groups_users = lta_agr_user
        EXCEPTIONS
          no_user_available     = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      IF NOT lta_agr_user IS INITIAL.
        LOOP AT lta_agr_user INTO lwa_agr_user.
          PERFORM get_email_id USING lwa_agr_user-uname space.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gtp_decision_type = 'DELE'.
    PERFORM get_email_id USING lst_but000-crusr space.
  ENDIF.

  IF gtp_decision_type = 'FORW'.
    PERFORM get_email_id USING gtp_manager space.
  ENDIF.

ENDFORM.                    " GET_RECIPENT_LIST
