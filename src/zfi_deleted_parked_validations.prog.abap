*&---------------------------------------------------------------------*
*&  Include           ZFI_DELETED_PARKED_VALIDATIONS
*&---------------------------------------------------------------------*
at selection-screen on S_BUKRS.
  if S_BUKRS-LOW is not initial.

    select SINGLE BUKRS
             from t001
             into GV_BUKRS
            where BUKRS in S_BUKRS. "#EC WARNOK
    if SY-SUBRC <> 0.
      message TEXT-001 type 'I'.
      stop.
    endif.
  endif.

at selection-screen on S_BELNR.
  if S_BELNR-LOW is not initial.
    select BUKRS BELNR
      from BKPF
      into table GT_VALIDATION
     where  BUKRS in S_BUKRS and
            BELNR in S_BELNR ##TOO_MANY_ITAB_FIELDS.
    if SY-SUBRC <> 0.
      message TEXT-002 type 'I'.
      stop.
    endif.
  endif.


at selection-screen on S_BLART.
  if S_BLART-LOW is not initial.
    select BLART
      from T003
      into table GT_VALIDATION
     where  bLART in S_BLART ##TOO_MANY_ITAB_FIELDS.
    if SY-SUBRC <> 0.
      message TEXT-003 type 'I'.
      stop.
    endif.
  endif.


at selection-screen on S_GJAHR.
  if S_GJAHR-LOW is not initial.
    select BUKRS BELNR GJAHR
      from BKPF
      into table GT_VALIDATION
     where  BUKRS in S_BUKRS and
            BELNR in S_BELNR and
            GJAHR in S_GJAHR ##TOO_MANY_ITAB_FIELDS.
    if SY-SUBRC <> 0.
      message TEXT-004 type 'I'.
      stop.
    endif.
  endif.
