FUNCTION ZFI_DOC_TYPE_VALIDATION.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ZDOCTYPE) TYPE  BLART OPTIONAL
*"  EXPORTING
*"     VALUE(ZEXDOCTYPE) TYPE  BLART
*"     VALUE(ZEXLTEXT) TYPE  LTEXT_003T
*"----------------------------------------------------------------------
* check the document type in t003 table

  DATA : lwa_t003t TYPE t003t.

  SELECT SINGLE * FROM t003t INTO lwa_t003t
                                 WHERE spras EQ 'EN' AND
                                       blart EQ zdoctype.

  IF sy-subrc EQ 0 .

    zexdoctype = lwa_t003t-blart.
    zexltext  = lwa_t003t-ltext.

  ENDIF.

ENDFUNCTION.
