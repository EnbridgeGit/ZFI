*&---------------------------------------------------------------------*
*&  Include           LZFI_DOA_VIEWF01
*&---------------------------------------------------------------------*
FORM new_entry_validation.
  DATA: ltp_timestamp TYPE timestamp,
        ltp_username TYPE syuname,
        ltp_pernr TYPE pernr_d,
        lst_user_addr  TYPE user_addr .
  GET TIME STAMP FIELD ltp_timestamp.
  ltp_username = sy-uname.

  zfimv_doa-changedon = ltp_timestamp.
  zfimv_doa-changedby = ltp_username.

  DATA: ltp_log_dest TYPE tb_rfcdest.

  "Get RFC Destination Details
  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      imp_sysid     = sy-sysid
      imp_client    = sy-mandt
    IMPORTING
      exp_rfcdest   = ltp_log_dest.

  IF zfimv_doa-lookup_key_type = 'E'.
    ltp_pernr = zfimv_doa-lookup_key.
    CALL FUNCTION 'ZFI_GET_USER_DETAILS' DESTINATION ltp_log_dest
      EXPORTING
        imp_pernr             = ltp_pernr
      IMPORTING
        exp_vorna             = zfimv_doa-vorna
        exp_nachn             = zfimv_doa-nachn
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2.
  ELSEIF zfimv_doa-lookup_key_type = 'G'.
    "Do Nothing as First Name and Last Name is not required for Grade
  ENDIF.

  ENDFORM.                    "new_entry_validation
