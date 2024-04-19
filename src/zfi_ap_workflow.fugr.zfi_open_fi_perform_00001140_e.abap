FUNCTION ZFI_OPEN_FI_PERFORM_00001140_E.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_PARKED) TYPE  XFELD OPTIONAL
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_EXCTAB STRUCTURE  EXCLTAB_LINE
*"--------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_OPEN_FI_PERFORM_00001140_E                *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  14-Oct-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :                                                *
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
  DATA: lt_exctab TYPE TABLE OF excltab_line
                    WITH HEADER LINE.
  DATA: lt_bseg TYPE TABLE OF bseg,
        lt_bkpf TYPE TABLE OF bkpf.                           "Note 565919


  DATA: lwa_bseg              TYPE bseg,
        lwa_bseg_temp         TYPE bseg,
        lwa_bkpf              TYPE bkpf,
        lwa_zfit_valid_blart  TYPE zfit_valid_blart.
  DATA: lv_usnam              TYPE usnam.

  CALL FUNCTION 'BF_FUNCTIONS_FIND'
    EXPORTING
      i_event       = '00001140'
    TABLES
      t_fmrfc       = fmtab
    EXCEPTIONS
      nothing_found = 4
      OTHERS        = 8.
  CHECK sy-subrc = 0.

*------------------ Save interface data --------------------------------
  lt_bkpf[] = t_bkpf[].
  lt_bseg[] = t_bseg[].







****Validation for User Parking the Invoice Should be able to post
  IF sy-ucomm = 'BU'.
    READ TABLE lt_bkpf INTO lwa_bkpf INDEX 1.
********changes by HM for doc type check**************3/20

Select single * from zfit_valid_blart into lwa_zfit_valid_blart where blart = lwa_bkpf-blart.
    IF sy-subrc = 0.
      SELECT SINGLE usnam INTO lv_usnam
        FROM bkpf
        WHERE bukrs = lwa_bkpf-bukrs
        AND   belnr = lwa_bkpf-belnr
        AND   gjahr = lwa_bkpf-gjahr.
      IF lv_usnam = sy-uname.
        MESSAGE e031(zfi_workflow) WITH sy-uname lwa_bkpf-belnr.
      ENDIF.
    ENDIF.
  ENDIF.


* For Enabling Post Button
  DELETE TABLE lt_exctab WITH TABLE KEY okcod = 'BU'.
* Collect okcodes provided by the customer into the programme table
  LOOP AT lt_exctab.
    t_exctab = lt_exctab.
    COLLECT t_exctab.
  ENDLOOP.

ENDFUNCTION.
