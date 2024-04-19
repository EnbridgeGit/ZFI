*&---------------------------------------------------------------------*
*& Report  Z_NOTE_2209660
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_NOTE_2209660.
DATA: lt_tfiblmpayblock TYPE TABLE OF tfiblmpayblock,
      ls_tfiblmpayblock LIKE LINE OF lt_tfiblmpayblock.

SELECT * FROM tfiblmpayblock INTO ls_tfiblmpayblock.
  APPEND ls_tfiblmpayblock TO lt_tfiblmpayblock.
ENDSELECT.

UPDATE tfiblmpayblock FROM TABLE lt_tfiblmpayblock.

IF sy-subrc = 0.

  WRITE 'Table TFIBLMPAYBLOCK has been updated'.

ELSE.

  WRITE 'Error occured: Table TFIBLMPAYBLOCK has not been updated'.

ENDIF.
