FUNCTION ZFI_AP_GET_ROUTE_CODE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_BUKRS) TYPE  BKPF-BUKRS
*"     REFERENCE(IMP_BELNR) TYPE  BKPF-BELNR
*"     REFERENCE(IMP_GJAHR) TYPE  BKPF-GJAHR
*"  EXPORTING
*"     REFERENCE(EXP_ROUTE_CODE) TYPE  XREF3
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_AP_GET_ROUTE_CODE                         *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  19-Sep-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Get Route Code Owner                          *
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

  SELECT SINGLE xref3 FROM vbsegk INTO exp_route_code
    WHERE ausbk = imp_bukrs
      AND belnr = imp_belnr
      AND gjahr = imp_gjahr.
  IF sy-subrc IS INITIAL.
  ENDIF.




ENDFUNCTION.
