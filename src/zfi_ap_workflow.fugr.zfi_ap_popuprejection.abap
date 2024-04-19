FUNCTION zfi_ap_popuprejection.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EXP_REASON) TYPE  ZREJECTION_REASON-REJECTION_REASON
*"----------------------------------------------------------------------
  DATA: lt_sval       LIKE TABLE OF sval WITH HEADER LINE,
        lv_return(1)  TYPE c.

  CLEAR lt_sval.
  lt_sval-tabname     = 'ZREJECTION_REASON'.
  lt_sval-fieldname   = 'REJECTION_REASON'.
  lt_sval-value       = ''.
  lt_sval-field_attr  = ' '.
  lt_sval-field_obl   = 'X'.
  lt_sval-fieldtext   = 'Reason:'.
  "lt_sval-comp_code   =
  "lt_sval-comp_tab    =
  "lt_sval-comp_field  =
  lt_sval-novaluehlp  = 'X'.
  APPEND lt_sval.

  DO.
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = 'X'
        popup_title     = 'Please enter a Rejection Reason.'
        start_column    = '5'
        start_row       = '5'
      IMPORTING
        returncode      = lv_return
      TABLES
        fields          = lt_sval
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    IF lv_return = ''.
      "Exit the loop
      Exit.
    ENDIF.

  ENDDO.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_sval.
    IF lt_sval-fieldname = 'REJECTION_REASON'.
      exp_reason = lt_sval-value.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
