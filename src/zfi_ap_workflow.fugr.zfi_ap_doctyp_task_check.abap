FUNCTION zfi_ap_doctyp_task_check.
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
*"      ACTIVE_WF_FOUND
*"----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*& Program Name       : ZFI_AP_DOCTYP_TASK_CHECK                       *
*& Author             : Shankar Balasubramaniam                        *
*& Creation Date      : 14-Oct-11                                      *
*& Object ID          :                                                *
*& Application Area   : FICO                                           *
*& Description        : Check for Document Type for Service Conformer  *
*                       Workflow                                       *
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
  DATA: lta_ekko TYPE STANDARD TABLE OF ekko WITH HEADER LINE,
        lst_ekko TYPE ekko,
        lta_rseg TYPE STANDARD TABLE OF rseg WITH HEADER LINE ,
        lst_rseg TYPE rseg,
        ltp_bukrs TYPE bukrs,
        ltp_belnr TYPE belnr_d,
        ltp_gjahr TYPE gjahr,
        ltp_bsart TYPE bsart.
  CONSTANTS : c_bsart TYPE bsart VALUE 'ZF'.

  DATA: ltp_objtype like SWOTOBJID-OBJTYPE,
      ltp_return   TYPE sysubrc,
      lta_worklist TYPE swrtwihdr,
      ltp_objkey   TYPE swotobjid-objkey,
      ltp_lines    TYPE i.

*  ltp_bukrs = objkey+0(4).
  ltp_belnr = objkey(10).
  ltp_gjahr = objkey+10(4).


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ltp_belnr
    IMPORTING
      output = ltp_belnr.
* Check validity of Document Type.

  SELECT *
    FROM rseg
    INTO  TABLE lta_rseg
    WHERE  belnr = ltp_belnr
                AND gjahr = ltp_gjahr.


  IF sy-subrc EQ 0.
    SELECT * FROM ekko
       INTO TABLE lta_ekko
      FOR ALL ENTRIES IN lta_rseg
                 WHERE ebeln = lta_rseg-ebeln.
    IF sy-subrc IS INITIAL.
      READ TABLE lta_ekko INTO lst_ekko WITH KEY bsart = c_bsart.
      IF sy-subrc IS NOT INITIAL.
        RAISE release_not_required.
      ENDIF.
    ENDIF.
  ENDIF.

  WAIT UP TO 3 seconds.

  ltp_objtype = 'BUS2081'.
  ltp_objkey = objkey.

  CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
    EXPORTING
      objtype     = ltp_objtype
      objkey      = ltp_objkey
    IMPORTING
      return_code = ltp_return
    TABLES
      worklist    = lta_worklist.

  DELETE lta_worklist WHERE wi_rh_task NE 'WS02000003'.

  DESCRIBE TABLE lta_worklist LINES ltp_lines.
  IF ltp_lines <> 0.
*   Raise exception for actve workflow for the same instances
    RAISE active_wf_found.
  ENDIF.


ENDFUNCTION.
