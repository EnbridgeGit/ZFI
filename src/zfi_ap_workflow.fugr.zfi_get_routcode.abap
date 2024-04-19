FUNCTION zfi_get_routcode.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_ROUTE_CODE) TYPE  XREF3
*"  EXPORTING
*"     REFERENCE(EXP_RC_OWNER) TYPE  WFSYST-INITIATOR
*"  EXCEPTIONS
*"      NOBODY_FOUND
*"----------------------------------------------------------------------
*Modification History:
*-----------------------------------------------------------------------
* Date : 27-01-2023
* Developer : ChayaArunGupta.G
* Business Contact : Siva
* Business Technical Developer : Zakir
* Transport Number: S01K900739
* Description : DOA Migration from US to UG.
*               Removed Default Destionation to PEC
*               Adjusted the ode accordingly to UG.
*"----------------------------------------------------------------------
*The Below code is commented from Line 16 - 29 for DOA Migration from US to UG.

*  CALL FUNCTION 'ZFI_GET_ROUTCODE' DESTINATION imp_destination
*    EXPORTING
*      imp_route_code = imp_route_code
*    IMPORTING
*      exp_rc_owner   = exp_rc_owner
*    EXCEPTIONS
*      nobody_found   = 1
*      OTHERS         = 2.
*  IF sy-subrc <> 0.
*    RAISE nobody_found.
*  ENDIF.

  DATA: ltp_route_code TYPE xref3,
         ltp_rc_owner   TYPE username.

  IF imp_route_code IS NOT INITIAL .
*   Get the owner of route code
    SELECT SINGLE owner INTO ltp_rc_owner FROM zfit_rc_owner WHERE route_code = imp_route_code.
    IF sy-subrc IS INITIAL.
*     Return owner as agent of this rule
      CONCATENATE zif_fi_constants=>gc_objtype_us ltp_rc_owner INTO exp_rc_owner.
    ELSE.
      RAISE nobody_found.
    ENDIF.
  ELSE.
    RAISE nobody_found.
  ENDIF.



ENDFUNCTION.
