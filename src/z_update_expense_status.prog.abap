*&---------------------------------------------------------------------*
*& Report  Z_UPDATE_EXPENSE_STATUS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_UPDATE_EXPENSE_STATUS.
TABLES: PTRV_HEAD.

DATA:
      PERNR TYPE BAPIEMPL-PERNR,
      TRIPID TYPE BAPITRVXXX-TRIPNO,
      PERIOD TYPE BAPITRVXXX-PERIOD,
      W_RET LIKE BAPIRETURN.

Data: Begin of T_TRIP occurs 0,
        PERNR LIKE PTRV_HEAD-PERNR,
        REINR LIKE PTRV_HEAD-REINR,
      END OF T_Trip.

SELECT-OPTIONS: S_TRIP for PTRV_HEAD-REINR no INTERVALS.
Parameters: ANTRG TYPE PTRV_PERIO-ANTRG OBLIGATORY,
            ABREC TYPE PTRV_PERIO-ABREC OBLIGATORY.

start-of-selection.

  select pernr reinr from PTRV_HEAD into table T_TRIP
    where REINR in S_TRIP.

end-of-selection.

  Loop at T_TRIP.
    Clear: PERNR, TRIPID.
    PERNR = T_TRIP-PERNR.
    TRIPID = T_TRIP-REINR.
    PERIOD = '000'.

    CALL FUNCTION 'BAPI_TRIP_CHANGE_STATUS'
      EXPORTING
        EMPLOYEENUMBER = PERNR
        TRIPNUMBER     = TRIPID
        PERIODNUMBER   = PERIOD
        APPROVED_NEW   = ANTRG
        ACCOUNT_NEW    = ABREC
      IMPORTING
        RETURN         = W_RET.

    IF W_RET-TYPE = 'E'.
      Write:/ 'Trip', TRIPID, 'for Employee', PERNR, 'could not be updated'.
      Write:/ W_RET-MESSAGE.
    ELSE.
      Write:/ 'Trip', TRIPID, 'for Employee', PERNR, 'updated successfully'.
    ENDIF.
  ENDLOOP.
  If SY-SUBRC NE 0.
    Write:/ 'No Trips could be selected for given selection'.
  ENDIF.
