FUNCTION ZFI_GL_WORKFLOW_CHECK.
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
*"----------------------------------------------------------------------

  DATA: lta_worklist TYPE swrtwihdr,
        lst_worklist TYPE swr_wihdr,
        ltp_objkey   TYPE swotobjid-objkey,
        lst_t003     TYPE t003,
        ltp_return   TYPE sysubrc,
        ltp_lines    TYPE i.

  DATA: ltp_bukrs TYPE bukrs,
        ltp_belnr TYPE belnr_d,
        ltp_gjahr TYPE gjahr,
        ltp_blart TYPE blart,
        w_blart TYPE blart.


  CLEAR: ltp_objkey.
  ltp_objkey = objkey.

  ltp_bukrs = objkey+0(4).
  ltp_belnr = objkey+4(13).
  ltp_gjahr = objkey+14(17).

  CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
    EXPORTING
      objtype     = 'FIPP'
      objkey      = ltp_objkey
    IMPORTING
      return_code = ltp_return
    TABLES
      worklist    = lta_worklist.

  DELETE lta_worklist WHERE wi_rh_task NE 'WS02000013'.
  DESCRIBE TABLE lta_worklist LINES ltp_lines.
  IF ltp_lines <> 0.
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
    SELECT SINGLE BLART from ZGL_WF_DOC into w_blart
      where blart = ltp_blart AND
            active = 'X'.
    if sy-subrc NE 0.
      RAISE invalid_document_type.
    Endif.
*    CASE ltp_blart.
*      WHEN 'SA'.
*      WHEN 'SV'.
*      WHEN 'IR'.
*      WHEN 'PY'.
*      WHEN OTHERS.
*        RAISE invalid_document_type.
*    ENDCASE.
  ENDIF.



ENDFUNCTION.
