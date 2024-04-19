FUNCTION ZFI_CREATE_SHORTCUT_TCODE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_RECIPIENT_USER_ID) TYPE  SYUNAME
*"     REFERENCE(IMP_TRANSACTION) TYPE  TCODE
*"     REFERENCE(IMP_SHORTCUT_PARAM) TYPE  ZFITT_SHORTCUT_PARAMETER
*"  EXPORTING
*"     REFERENCE(EXP_CONTENT) TYPE  STRING
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_CREATE_SHORTCUT_TCODE                     *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  19-Sep-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Shortcut                                      *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*
*** Declaration for shortcut content
  DATA :  ltp_parameter TYPE text255,
           ltp_pernr(12) TYPE c,
           lst_shortcut_param TYPE zfis_shortcut_parameter.
  DATA :  ltp_tcode TYPE tcode.

* Check if transaction code is available
  CLEAR ltp_tcode.
  SELECT SINGLE tcode FROM tstc
                INTO ltp_tcode
                WHERE tcode EQ imp_transaction.

  IF ltp_tcode IS INITIAL.
    MESSAGE 'Enter a valid transaction'
      TYPE zif_fi_constants=>gc_type_error
      DISPLAY LIKE zif_fi_constants=>gc_type_error.
*    EXIT.
  ENDIF.

** Populate the parameters to be passed to the shortcut
  IF imp_shortcut_param IS NOT INITIAL.
    CLEAR ltp_parameter.
    LOOP AT imp_shortcut_param INTO lst_shortcut_param.
      CONCATENATE ltp_parameter
                  lst_shortcut_param-fieldname '='
                  lst_shortcut_param-fieldvalue ';'
             INTO ltp_parameter.
    ENDLOOP.
  ENDIF.

*** create the shortcut content for the required transaction
  CALL FUNCTION 'SWN_CREATE_SHORTCUT'
    EXPORTING
      i_transaction           = imp_transaction
      i_parameter             = ltp_parameter
      i_sysid                 = sy-sysid
      i_client                = sy-mandt
      i_user                  = imp_recipient_user_id
      i_language              = sy-langu
      i_windowsize            = 'Normal window'
    IMPORTING
      shortcut_string         = exp_content
    EXCEPTIONS
      inconsistent_parameters = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.




ENDFUNCTION.
