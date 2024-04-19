FUNCTION zfi_get_rfc_dest.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IMP_PARAMTYPE) TYPE  ZPARAMTYPE DEFAULT 'ECCUG'
*"  EXPORTING
*"     REFERENCE(EXP_RFCDEST) TYPE  TB_RFCDEST
*"----------------------------------------------------------------------

  DATA: ls_zvarsys TYPE zvarsys,
        lv_mydest  TYPE rfcdisplay-rfcdest.

  CONSTANTS c_programm TYPE zvarsys-programm VALUE 'LOGSYS'.

  "Fetch the RFC Destination
  CASE imp_paramtype.
* Begin of Changes <Chaya>
* DOA Changes US to UG .
    WHEN 'ECCUS'.
      SELECT SINGLE * FROM zvarsys INTO ls_zvarsys
              WHERE programm = c_programm
                AND varname  = 'ECCUS_RFC'.
    WHEN 'ECCUG'.
      SELECT SINGLE * FROM zvarsys INTO ls_zvarsys
        WHERE programm = c_programm
          AND varname  = 'ECCUG_RFC'.
* End of Changes <Chaya>
    WHEN 'ECCSW'.
      SELECT SINGLE * FROM zvarsys INTO ls_zvarsys
        WHERE programm = c_programm
          AND varname  = 'ECCSW_RFC'.
    WHEN 'SRM'.
      SELECT SINGLE * FROM zvarsys INTO ls_zvarsys
        WHERE programm = c_programm
          AND varname  = 'SRM_RFC'.
    WHEN 'HR'." OR  'WORKFLOW'.
      SELECT SINGLE * FROM zvarsys INTO ls_zvarsys
        WHERE programm = c_programm
          AND varname  = 'HCM_RFC'.
  ENDCASE.

  IF sy-subrc IS INITIAL.

    lv_mydest = ls_zvarsys-value1.

    CALL FUNCTION 'RFC_CHECK_DESTINATION'
      EXPORTING
        mydest                        = lv_mydest
        mytype                        = 'A'
      EXCEPTIONS
        empty_destination             = 1
        invalid_logical_destination   = 2
        destination_with_special_char = 3
        internal_destination_id       = 4
        empty_rfctype                 = 5
        OTHERS                        = 6.

    CASE sy-subrc.
      WHEN 0.
        exp_rfcdest = ls_zvarsys-value1.
      WHEN 1.
        MESSAGE e050(zfi_workflow).
      WHEN 2.
        MESSAGE e051(zfi_workflow).
      WHEN 3.
        MESSAGE e052(zfi_workflow).
      WHEN 4.
        MESSAGE e053(zfi_workflow).
      WHEN 5.
        MESSAGE e054(zfi_workflow).
      WHEN 6.
        MESSAGE e055(zfi_workflow).
      WHEN 7.
        MESSAGE e056(zfi_workflow).
    ENDCASE.
  ENDIF.
ENDFUNCTION.
