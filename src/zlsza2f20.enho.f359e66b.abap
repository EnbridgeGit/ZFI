"Name: \PR:SAPLSZA2\FO:CHECK_ADDR1_DATA\SE:END\EI
ENHANCEMENT 0 ZLSZA2F20.
*BTBOUNDY EHP5 FIX
*LIMIT Vendor Name to 35 Characters
DATA: lv_isavendor TYPE char1,
      lv_lifnr     TYPE lfa1-lifnr.

CLEAR lv_isavendor.

IF address_handle(4) = 'LFA1'.
  lv_isavendor = 'X'.
ELSEIF address_number IS NOT INITIAL.
  "Lookup to see if this is a vendor
  SELECT SINGLE lifnr
    FROM lfa1
    INTO lv_lifnr
    WHERE adrnr = address_number.

  IF sy-subrc = 0.
    "This is assigned to a vendor
    lv_isavendor = 'X'.
  ELSE.
    lv_isavendor = ''.
  ENDIF.

ELSE.
  lv_isavendor = ''.
ENDIF.

IF lv_isavendor = 'X'.
  lv_length = STRLEN( addr1_data-name1 ).
  IF lv_length > 35.
    MESSAGE ID 'AM' TYPE 'E' NUMBER '228' WITH 5 'NAME1' lv_length 35.
  ENDIF.

  lv_length = STRLEN( addr1_data-name2 ).
  IF lv_length > 35.
    MESSAGE ID 'AM' TYPE 'E' NUMBER '228' WITH 5 'NAME2' lv_length 35.
  ENDIF.
ENDIF.

ENDENHANCEMENT.
