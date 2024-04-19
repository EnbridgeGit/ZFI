REPORT rkocor24 .
TABLES: onr00, cobrb.

SELECT-OPTIONS: s_objnr FOR onr00-objnr.
PARAMETERS: p_test TYPE c DEFAULT 'X'.


DATA: it_objnr LIKE onr00 OCCURS 0 WITH HEADER LINE.
CONSTANTS: con_status_abrv LIKE tj02-istat
                           VALUE 'I0028',
           con_status_abre LIKE tj02-istat
                           VALUE 'I0027'.
DATA: abrv_subrc LIKE sy-subrc,
      abre_subrc LIKE sy-subrc.

START-OF-SELECTION.
  SELECT * FROM onr00
    WHERE objnr IN s_objnr.
    it_objnr = onr00.
    APPEND it_objnr.
  ENDSELECT.
* Objects found ?
  IF NOT sy-subrc IS INITIAL.
    WRITE text-006.  "no objects found
    STOP.
  ENDIF.

  LOOP AT it_objnr.
    CALL FUNCTION 'STATUS_CHECK'
      EXPORTING
        objnr             = it_objnr-objnr
        status            = con_status_abrv
      EXCEPTIONS
        object_not_found  = 1
        status_not_active = 2
        OTHERS            = 3.
*   active
    IF sy-subrc IS INITIAL.
      abrv_subrc = sy-subrc.
*   not active
    ELSEIF sy-subrc = 2.
      abrv_subrc = sy-subrc.
      CALL FUNCTION 'STATUS_CHECK'
        EXPORTING
*         BYPASS_BUFFER     = ' '
*         CLIENT            = SY-MANDT
          objnr             = it_objnr-objnr
          status            = con_status_abre
        EXCEPTIONS
          object_not_found  = 1
          status_not_active = 2
          OTHERS            = 3.
      IF sy-subrc IS INITIAL OR sy-subrc = 2.
        abre_subrc = sy-subrc.
      ELSEIF sy-subrc = 1 OR sy-subrc = 3.
        "WRITE: / text-001, it_objnr-objnr.
        CONTINUE.
      ENDIF.
    ELSEIF sy-subrc = 1 OR sy-subrc = 3.
      "WRITE: / text-001, it_objnr-objnr.
      CONTINUE.
    ENDIF.
    SELECT * FROM cobrb
        WHERE objnr = it_objnr-objnr
        AND  ( avorg = space OR avorg = 'KOAO' OR avorg = 'KOAW' ) .
      EXIT.
    ENDSELECT.
*   rules existing
    IF sy-subrc IS INITIAL.
*     but status not active
      IF NOT abrv_subrc IS INITIAL AND NOT abre_subrc IS INITIAL.
*        WRITE: / text-002, it_objnr-objnr.
        PERFORM update USING it_objnr-objnr ' '.
      ENDIF.
**   no rules existing
    ELSEIF NOT sy-subrc IS INITIAL.
*    but status active
      IF abrv_subrc IS INITIAL OR abre_subrc IS INITIAL.
*        WRITE: / text-003, it_objnr-objnr.
        PERFORM update USING it_objnr-objnr 'X'.
      ENDIF.
    ENDIF.

  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJNR_OBJNR  text
*      -->P_0148   text
*----------------------------------------------------------------------*
FORM update USING    p_objnr LIKE onr00-objnr
                     p_inactive LIKE jstat-inact.
  DATA: it_status LIKE jstat OCCURS 5 WITH HEADER LINE,
        ld_objnr LIKE jsto-objnr.

  CHECK p_test IS INITIAL.

  it_status-stat = con_status_abrv.
  it_status-inact = p_inactive.
  APPEND it_status.
  ld_objnr = p_objnr.

  CALL FUNCTION 'STATUS_CHANGE_INTERN'
    EXPORTING
      objnr               = ld_objnr
    TABLES
      status              = it_status
    EXCEPTIONS
      object_not_found    = 1
      status_inconsistent = 2
      status_not_allowed  = 3
      OTHERS              = 4.
  IF sy-subrc IS INITIAL.
    COMMIT WORK.
  ELSE.
    WRITE: text-004, sy-subrc.
  ENDIF.

ENDFORM.                               " UPDATE
