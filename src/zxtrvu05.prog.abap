*&---------------------------------------------------------------------*
*&  Include           ZXTRVU05
*&---------------------------------------------------------------------*

CONSTANTS:  lc_doctype TYPE saeobjart VALUE 'ZTERECPDF',
            lc_objtype TYPE saeanwdid VALUE 'BUS2089',
            lc_x       TYPE char1     VALUE 'X',
            lc_clstype TYPE char2     VALUE 'BO'.

DATA:     lv_objid   TYPE toav0-object_id,
          lv_barcode TYPE toav0-arc_doc_id,
          lv_objidn  TYPE bds_objid,
          lv_logsys  TYPE char10.


CLEAR:lv_objid,
      lv_barcode,
      lv_objidn,
      lv_logsys.


CONCATENATE trip_header-pernr
            trip_header-reinr
            INTO lv_objid.



CONCATENATE trip_header-pernr
            trip_header-reinr
            INTO lv_barcode.

*To Update BDS_BAR_IN table

  CALL FUNCTION 'ALINK_BARCODE_GLOBAL'
    EXPORTING
      object_type      = lc_objtype
      object_id        = lv_objid
      i_document_type  = lc_doctype
      barcode          = lv_barcode
*     NO_DOC_CHANGE    =
      no_popup         = lc_x
*     UPDATETASK       =
*     NO_CHECK         =
    EXCEPTIONS
      no_authority     = 1
      no_customizing   = 2
      error_connection = 3
      error_parameter  = 4
      user_exit        = 5
      general_error    = 6
      OTHERS           = 7.

  IF sy-subrc <> 0.

  ENDIF.
