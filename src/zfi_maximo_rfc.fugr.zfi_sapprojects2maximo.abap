FUNCTION zfi_sapprojects2maximo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TRIGGER) TYPE  INDICATOR OPTIONAL
*"  EXPORTING
*"     VALUE(DATA) TYPE  ZTT_FI_PERSONRESP
*"  EXCEPTIONS
*"      NO_DATA_FOUND
*"----------------------------------------------------------------------
*
  SELECT * FROM zfi_personresp
           INTO TABLE data.
  IF sy-subrc <> 0.
    RAISE no_data_found.
  ENDIF.



ENDFUNCTION.
