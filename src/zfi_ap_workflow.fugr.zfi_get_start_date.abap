FUNCTION zfi_get_start_date.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_WF_TYPE) TYPE  ZFI_WF_TYPE
*"  EXPORTING
*"     REFERENCE(EXP_START_DATE) TYPE  SYST-DATUM
*"     REFERENCE(EXP_START_TIME) TYPE  SYST-UZEIT
*"----------------------------------------------------------------------

  DATA: ltp_today TYPE sy-datum,
         ltp_fcal TYPE wfcid,
         ltp_days TYPE zfi_rem_days,
         lv_value TYPE z_varvaluel.

* Get Factory Calendar
  SELECT SINGLE value1
    FROM zvar
    INTO lv_value
    WHERE programm = 'ZFI_GET_START_DATE'
      AND varname = 'FACTORY_CALENDAR'.

  IF sy-subrc <> 0.
    RAISE invalid_factory_calendar.
  ELSE.
    ltp_fcal = lv_value.
  ENDIF.

  SELECT SINGLE value1
    FROM zvar
    INTO lv_value
    WHERE programm = 'ZFI_GET_START_DATE'
      AND varname = 'AP_REMINDER_NUM_DAYS'
  .

  IF sy-subrc IS NOT INITIAL.
    ltp_days = 1.
  ELSE.
    ltp_days = lv_value.
  ENDIF.

  CALL FUNCTION 'START_TIME_DETERMINE'
    EXPORTING
      duration                   = ltp_days
*     UNIT                       =
      factory_calendar           = ltp_fcal
    IMPORTING
      start_date                 = exp_start_date
      start_time                 = exp_start_time
    EXCEPTIONS
      factory_calendar_not_found = 1
      date_out_of_calendar_range = 2
      date_not_valid             = 3
      unit_conversion_error      = 4
      si_unit_missing            = 5
      parameters_not_valid       = 6
      OTHERS                     = 7.
  IF sy-subrc <> 0.
    RAISE invalid_factory_calendar.
  ENDIF.





ENDFUNCTION.
