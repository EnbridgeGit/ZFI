FUNCTION zfi_bp_check_fm_workflow.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(OBJTYPE) LIKE  SWETYPECOU-OBJTYPE
*"     REFERENCE(OBJKEY) LIKE  SWEINSTCOU-OBJKEY
*"     REFERENCE(EVENT) LIKE  SWEINSTCOU-EVENT
*"     REFERENCE(RECTYPE) LIKE  SWETYPECOU-RECTYPE
*"  TABLES
*"      EVENT_CONTAINER STRUCTURE  SWCONT
*"  EXCEPTIONS
*"      ACTIVE_WF_FOUND
*"      INVALID_BP_TYPE
*"      INVALID_INPUT
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_BP_CHECK_FM_WORKFLOW                      *
*& Author             :  Harshada Patil                                *
*& Creation Date      :  1-Nov-2011                                    *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Check workflow                                *
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


  DATA: ltp_partner TYPE bu_partner,
        ltp_bptype TYPE bu_bpkind.

  ltp_partner = objkey.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ltp_partner
    IMPORTING
      output = ltp_partner.

  SELECT SINGLE bpkind INTO ltp_bptype FROM but000 WHERE partner = ltp_partner.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE bptype FROM zfit_valid_bp_ty INTO ltp_bptype WHERE bptype = ltp_bptype.
    IF sy-subrc IS NOT INITIAL.
      RAISE invalid_bp_type.
    ENDIF.
  ELSE.
    RAISE invalid_input.
  ENDIF.

ENDFUNCTION.
