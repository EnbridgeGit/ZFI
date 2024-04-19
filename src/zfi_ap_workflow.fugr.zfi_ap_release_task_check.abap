FUNCTION ZFI_AP_RELEASE_TASK_CHECK.
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
*"      RELEASE_NOT_REQUIRED
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Program Name       : ZFI_AP_RELEASE_TASK_CHECK                      *
*& Author             : Shankar Balasubramaniam                        *
*& Creation Date      : 14-Oct-11                                      *
*& Object ID          :                                                *
*& Application Area   : FICO                                           *
*& Description        : Check for AP Active Workflow instance          *
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
  DATA: lta_blart TYPE STANDARD TABLE OF zfit_valid_blart,
        lst_blart TYPE zfit_valid_blart,
        ltp_bukrs TYPE bukrs,
        ltp_belnr TYPE belnr_d,
        ltp_gjahr TYPE gjahr,
        ltp_blart TYPE blart.

  ltp_bukrs = objkey+0(4).
  ltp_belnr = objkey+4(13).
  ltp_gjahr = objkey+14(17).


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ltp_belnr
    IMPORTING
      output = ltp_belnr.
* Check validity of Document Type.
  SELECT SINGLE blart INTO ltp_blart
    FROM bkpf WHERE bukrs = ltp_bukrs
                AND belnr = ltp_belnr
                AND gjahr = ltp_gjahr.
  IF sy-subrc IS INITIAL.
    SELECT * INTO TABLE lta_blart
      FROM zfit_valid_blart
    WHERE blart = ltp_blart.
    IF sy-subrc IS INITIAL.
      READ TABLE lta_blart INTO lst_blart WITH KEY blart = ltp_blart.
      IF sy-subrc IS INITIAL.
        RAISE release_not_required.
      ENDIF.
    ENDIF.
  ENDIF.




ENDFUNCTION.
