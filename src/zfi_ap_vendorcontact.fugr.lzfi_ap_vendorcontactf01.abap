*----------------------------------------------------------------------*
***INCLUDE LZFI_AP_VENDORCONTACTF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EDIT_CONTACT
*&---------------------------------------------------------------------*
FORM edit_contact .
  DATA: lv_line   TYPE i,
        ls_addr3  TYPE addr3_dia,
        lt_addr3  LIKE TABLE OF ls_addr3.
  "ls_addr3d TYPE addr3_data,
  "lt_addr3d LIKE TABLE OF ls_addr3d.




  "Get the current line data
  GET CURSOR LINE lv_line.
  IF lv_line = 0.
    EXIT.
  ENDIF.

  READ TABLE gt_contact INTO ls_contact INDEX lv_line.


  ls_addr3-persnumber = ls_contact-pers.
  ls_addr3-addrnumber = ls_contact-addr.
  ls_addr3-pers_group = 'BP'.
  "Check for display mode.
  IF gv_aktyp = 'A'.
    ls_addr3-maint_mode = 'DISPLAY'.
  ELSE.
    ls_addr3-maint_mode = 'CHANGE'.
  ENDIF.
  APPEND ls_addr3 TO lt_addr3.


  CALL FUNCTION 'ADDR_PERS_COMP_DIALOG'
*     EXPORTING
*       CHECK_ADDRESS                     = 'X'
*       IV_TIME_DEPENDENT_COMM_DATA       = ' '
*     IMPORTING
*       OK_CODE                           =
    TABLES
      number_handle_tab                 = lt_addr3
    EXCEPTIONS
      address_not_exist                 = 1
      group_not_valid                   = 2
      parameter_error                   = 3
      internal_error                    = 4
      OTHERS                            = 5
  .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'ADDR_MEMORY_SAVE'
* EXPORTING
*   EXECUTE_IN_UPDATE_TASK       = ' '
* EXCEPTIONS
*   ADDRESS_NUMBER_MISSING       = 1
*   PERSON_NUMBER_MISSING        = 2
*   INTERNAL_ERROR               = 3
*   DATABASE_ERROR               = 4
*   REFERENCE_MISSING            = 5
*   OTHERS                       = 6
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " EDIT_CONTACT
