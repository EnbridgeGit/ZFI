FUNCTION ZWF_FIND_EMAIL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ID) TYPE  XUBNAME OPTIONAL
*"     VALUE(PERNR) TYPE  PERNR_D OPTIONAL
*"  EXPORTING
*"     VALUE(EMAIL) TYPE  AD_SMTPADR
*"     VALUE(USERID) TYPE  SWP_AGENT
*"----------------------------------------------------------------------

  DATA: ltp_email_id TYPE  ad_smtpadr,
        ltp_username TYPE username,
        lst_reclist  TYPE somlreci1.
  DATA: ltp_log_dest TYPE tb_rfcdest.

* Get RFC Destination Details
  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      IMP_PARAMTYPE = 'HR'
    IMPORTING
      exp_rfcdest   = ltp_log_dest.

  IF ID IS INITIAL AND NOT PERNR IS INITIAL.
    CALL FUNCTION 'ZFI_CONVERT_PERNR_TO_USER' DESTINATION ltp_log_dest
      EXPORTING
        imp_pernr    = pernr
      IMPORTING
        exp_username = ltp_username.
    ID = ltp_username.
    concatenate 'US' ltp_username into USERID.
    CONDENSE USERID NO-GAPS.
  ENDIF.

  CALL FUNCTION 'ZFI_GET_EMAIL_ID' DESTINATION ltp_log_dest
    EXPORTING
      imp_user_id             = ID
    IMPORTING
      exp_email_address       = ltp_email_id
    EXCEPTIONS
      email_id_not_maintained = 1
      OTHERS                  = 2.
  IF sy-subrc = 0.
    EMAIL = ltp_email_id.
  ENDIF.

  IF ltp_email_id IS INITIAL.
    DATA: lst_usr21 TYPE usr21.
    SELECT SINGLE * FROM usr21 INTO lst_usr21
      WHERE bname = ID.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE smtp_addr FROM adr6 INTO ltp_email_id
        WHERE addrnumber = lst_usr21-addrnumber
        AND persnumber = lst_usr21-persnumber.
      IF sy-subrc = 0.
        EMAIL = ltp_email_id.
      ENDIF.
    ENDIF.
  ENDIF.



ENDFUNCTION.
