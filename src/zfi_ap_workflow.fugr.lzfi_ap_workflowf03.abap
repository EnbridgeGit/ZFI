*----------------------------------------------------------------------*
***INCLUDE LZFI_AP_WORKFLOWF03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_TABLE_ENTRIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_PERNR  text
*      -->P_LV_DOC_TYPE  text
*      -->P_IMP_TCODE  text
*      -->P_IMP_SYSTEM  text
*      <--P_LS_DOA  text
*----------------------------------------------------------------------*
FORM get_table_entries  USING    iv_pernr      TYPE pernr_d
                                 iv_doc_type   TYPE zfi_doc_type
                                 iv_tcode      TYPE sytcode
                                 iv_system     TYPE zfi_system
                        CHANGING cs_doa        TYPE ZFIT_DOA_NEW.

  DATA: lv_doc_type   TYPE zfi_doc_type,
        lv_lookup_key TYPE zfi_lookup_key,
        lv_grade      TYPE zfi_lookup_key,
        lt_doa        TYPE TABLE OF ZFIT_DOA_NEW,
        lw_doa        TYPE ZFIT_DOA_NEW,
        lv_line       TYPE i.

  IF iv_tcode = 'FTR_EDIT'.
    lv_doc_type = iv_doc_type+0(3).
  ELSE.
    lv_doc_type = iv_doc_type+0(2).
  ENDIF.

* Insert SPARGA SDP 71124
  PERFORM get_grade_of_employee USING iv_pernr iv_system CHANGING lv_grade.
  IF NOT lv_grade IS INITIAL.    "76724
    REFRESH lt_doa.
    "Look for DOA Table by Grade
    lv_lookup_key = lv_grade.
    SELECT * FROM ZFIT_DOA_NEW INTO TABLE lt_doa
      WHERE lookup_key = lv_lookup_key
        AND lookup_key_type = 'G'.

    IF sy-subrc = 0.
      READ TABLE lt_doa INTO lw_doa WITH KEY tcode = iv_tcode doc_type = space.
      IF sy-subrc = 0.
        cs_doa = lw_doa.
      ENDIF.

      READ TABLE lt_doa INTO lw_doa WITH KEY tcode = iv_tcode doc_type = iv_doc_type.
      IF sy-subrc = 0.
        IF cs_doa IS INITIAL OR ( lw_doa-amount GT cs_doa-amount ).
          cs_doa = lw_doa.
        ENDIF.
      ELSE.
        READ TABLE lt_doa INTO lw_doa WITH KEY tcode = iv_tcode doc_type = lv_doc_type.
        IF sy-subrc = 0.
          IF cs_doa IS INITIAL OR ( lw_doa-amount GT cs_doa-amount ).
            cs_doa = lw_doa.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  REFRESH lt_doa.
  lv_lookup_key = iv_pernr.

  SELECT *
    FROM ZFIT_DOA_NEW
    INTO CORRESPONDING FIELDS OF TABLE lt_doa
    WHERE lookup_key      = lv_lookup_key
      AND lookup_key_type = 'E'.

  IF sy-subrc = 0.
    READ TABLE lt_doa INTO lw_doa WITH KEY tcode = iv_tcode doc_type = iv_doc_type.
    IF sy-subrc = 0.
      IF cs_doa IS INITIAL OR ( lw_doa-amount GT cs_doa-amount ).
        cs_doa = lw_doa.
      ENDIF.
    ELSE.
      READ TABLE lt_doa INTO lw_doa WITH KEY tcode = iv_tcode doc_type = lv_doc_type.
      IF sy-subrc = 0.
        IF cs_doa IS INITIAL OR ( lw_doa-amount GT cs_doa-amount ).
          cs_doa = lw_doa.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE lt_doa INTO lw_doa WITH KEY tcode = iv_tcode doc_type = space.
    IF sy-subrc = 0.
      IF cs_doa IS INITIAL OR ( lw_doa-amount GT cs_doa-amount ).
        cs_doa = lw_doa.
      ENDIF.
    ENDIF.

    READ TABLE lt_doa INTO lw_doa WITH KEY tcode = space doc_type = space.
    IF sy-subrc = 0.
      IF cs_doa IS INITIAL OR ( lw_doa-amount GT cs_doa-amount ).
        cs_doa = lw_doa.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_TABLE_ENTRIES

*&---------------------------------------------------------------------*
*&      Form  GET_GRADE_OF_EMPLOYEE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IV_PERNR  text
*      -->P_IV_SYSTEM  text
*      <--P_LV_GRADE  text
*----------------------------------------------------------------------*
FORM get_grade_of_employee  USING iv_pernr  TYPE pernr_d
                                  iv_system TYPE zfi_system
                         CHANGING cv_grade  TYPE zfi_lookup_key.


  DATA: lv_hr_rfc     TYPE tb_rfcdest,
        lt_infty_0008 TYPE hrfrpbs4_infty0008_tab,
        ls_infty_0008 TYPE hrfrpbs4_infty0008.

  PERFORM get_rfc CHANGING lv_hr_rfc.

  "Read Infotype 0008 from HR System
  CALL FUNCTION 'ZFI_READ_INFOYPE_0008' DESTINATION lv_hr_rfc
    EXPORTING
      imp_pernr         = iv_pernr
      imp_infty         = '0008'
    IMPORTING
      exp_infotype_0008 = lt_infty_0008
    EXCEPTIONS
      infty_not_found   = 1.

  IF sy-subrc IS INITIAL.
    SORT lt_infty_0008 BY endda DESCENDING.
    READ TABLE lt_infty_0008 INTO ls_infty_0008 INDEX 1.
    IF sy-subrc IS INITIAL.
      cv_grade = ls_infty_0008-trfgr.
    ELSE.
      "      MESSAGE e061(zfi_workflow) RAISING no_grade.
    ENDIF.
  ELSE.
    "    MESSAGE e061(zfi_workflow) RAISING no_grade.
  ENDIF.

ENDFORM.                    " GET_GRADE_OF_EMPLOYEE

*&---------------------------------------------------------------------*
*&      Form  GET_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_HR_RFC  text
*----------------------------------------------------------------------*
FORM get_rfc  CHANGING cv_log_dest TYPE tb_rfcdest.
  "Get RFC Destination Details for HR
  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
* Begin of changes by <Chaya>
* DOA Changes
*      imp_sysid   = sy-sysid
*      imp_client  = sy-mandt
    imp_paramtype = 'HR'
* End of changes by <Chaya>
    IMPORTING
      exp_rfcdest = cv_log_dest.


ENDFORM.                    " GET_RFC
