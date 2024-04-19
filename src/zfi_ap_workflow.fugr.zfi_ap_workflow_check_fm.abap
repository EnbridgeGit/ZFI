FUNCTION ZFI_AP_WORKFLOW_CHECK_FM.
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
*"      INVALID_DOCUMENT_TYPE
*"      INVALID_KEY
*"      NON_AP_DOCUMENT
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_AP_WORKFLOW_CHECK_FM                      *
*& Author             :  Shankar Balasubramaniam                       *
*& Creation Date      :  19-Sep-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Check Function Module                         *
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

DATA: lta_worklist TYPE swrtwihdr,
        lst_worklist TYPE swr_wihdr,
        ltp_objkey   TYPE swotobjid-objkey,
        lta_filter   TYPE swrttask,
        lst_filter   TYPE swr_task,
        lst_t003     TYPE t003,
        ltp_return   TYPE sysubrc,
        ltp_lines    TYPE i.


  DATA: ltp_bukrs TYPE bukrs,
        ltp_belnr TYPE belnr_d,
        ltp_gjahr TYPE gjahr,
        ltp_blart TYPE blart.

  ltp_bukrs = objkey+0(4).
  ltp_belnr = objkey+4(13).
  ltp_gjahr = objkey+14(17).

  CLEAR: ltp_objkey.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = ltp_belnr
    IMPORTING
      output = ltp_belnr.

  lst_filter-wi_rh_task = zif_fi_constants=>gc_task_98300005.
  INSERT lst_filter INTO TABLE lta_filter.
  lst_filter-wi_rh_task = zif_fi_constants=>gc_wf_98300001.
  INSERT lst_filter INTO TABLE lta_filter.

  CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
    EXPORTING
      objtype        = zif_fi_constants=>gc_objtype_fipp
      objkey         = objkey
    IMPORTING
      return_code    = ltp_return
    TABLES
      task_filter    = lta_filter
      worklist       = lta_worklist
*     MESSAGE_LINES  =
*     MESSAGE_STRUCT =
    .

  DESCRIBE TABLE lta_worklist LINES ltp_lines.
  IF ltp_lines <> 0.
*   Raise exception for actve workflow for the same instances
    RAISE active_wf_found.
  ENDIF.


* Check validity of Document Type.
  SELECT SINGLE blart INTO ltp_blart
    FROM bkpf WHERE bukrs = ltp_bukrs
                AND belnr = ltp_belnr
                AND gjahr = ltp_gjahr.
  IF sy-subrc IS NOT INITIAL.
    RAISE invalid_key.
  ELSE.
    SELECT SINGLE blart INTO ltp_blart
      FROM zfit_valid_blart
    WHERE blart = ltp_blart
      AND workflow = 'AP'.
    IF sy-subrc IS NOT INITIAL.
      RAISE invalid_document_type.
    ENDIF.
  ENDIF.
*




ENDFUNCTION.
