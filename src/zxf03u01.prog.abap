*&---------------------------------------------------------------------*
*&  Include           ZXF03U01
*&---------------------------------------------------------------------*
CONSTANTS: c_e    TYPE c       VALUE 'E'.

DATA: lv_error  TYPE char100,
      lv_altchk TYPE char1.

IF bukrsaccount-bukrs is not INITIAL.
  CLEAR: lv_error, lv_altchk.

  SELECT SINGLE altchk
    FROM zfit_altaccchk
    INTO lv_altchk
    WHERE bukrs = bukrsaccount-bukrs
      and tcode = sy-tcode.
  IF sy-subrc is INITIAL and
    lv_altchk IS NOT INITIAL AND
    bukrsaccount-altkt IS INITIAL.
    CONCATENATE 'Account'(001)
                 bukrsaccount-saknr
    'in Co. Code EGD requires assignment to an alternative account (Oracle EBS)'(002)
      INTO lv_error SEPARATED BY space.
    return-message = lv_error.
    MESSAGE lv_error TYPE c_e.
  ENDIF.
ENDIF.
