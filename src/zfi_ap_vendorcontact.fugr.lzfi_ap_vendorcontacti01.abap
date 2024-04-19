*----------------------------------------------------------------------*
***INCLUDE LZFI_AP_VENDORCONTACTI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PROCESS_FUNCTION_CODE  INPUT
*&---------------------------------------------------------------------*
MODULE process_function_code INPUT.
  CASE sy-ucomm.
    WHEN 'EDITCON'. "'REFS' (DOUBLE CLICK)
      PERFORM edit_contact.
  ENDCASE.
ENDMODULE.                 " PROCESS_FUNCTION_CODE  INPUT
