FUNCTION ZFI_PARSE_STRING_USING_PATTERN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_TEXT) TYPE  STRING
*"     REFERENCE(IV_PATTERN) TYPE  STRING
*"  EXPORTING
*"     REFERENCE(CV_OFFSET) TYPE  I
*"     REFERENCE(CV_LENGTH) TYPE  I
*"     REFERENCE(CV_TOKEN) TYPE  STRING
*"     REFERENCE(CV_SUBRC) TYPE  SYSUBRC
*"  EXCEPTIONS
*"      NO_TEXT
*"      NO_PATTERN
*"      SEARCH_ERROR
*"      PATTERN_ERROR
*"      PARSE_ERROR
*"----------------------------------------------------------------------

DATA:    lv_text    TYPE STRING,
           lv_success TYPE char1,
           lv_offset  TYPE I,
           lv_length  TYPE I,
           lv_token   TYPE STRING.

  DATA:    lr_matcher TYPE REF TO cl_abap_matcher.

  CLEAR    cv_offset.
  CLEAR    cv_length.
  CLEAR    cv_token.

  CHECK  ( cv_subrc   IS INITIAL ).

*eject
  IF     ( iv_text    IS INITIAL ).

    cv_subrc = 1. "raise no_text
    RAISE NO_TEXT.

  ENDIF.

  IF     ( iv_pattern IS INITIAL ).

    cv_subrc = 2. "raise no_pattern
    RAISE NO_PATTERN.

  ENDIF.

  CLEAR                  lv_text.
  MOVE     iv_text    TO lv_text.
  TRANSLATE              lv_text TO UPPER CASE.

* Search the string using the pattern
  TRY.

      CALL METHOD cl_abap_matcher=>contains
        EXPORTING
          pattern = iv_pattern
          text    = lv_text
        RECEIVING
          success = lv_success.

    CATCH cx_sy_matcher.

      cv_subrc = 3. "raise search_error
      RAISE SEARCH_ERROR.

    CATCH cx_sy_regex.

      cv_subrc = 4. "raise pattern_error
      RAISE PATTERN_ERROR.

  ENDTRY.

  IF     ( lv_success IS INITIAL ).
    RETURN.
  ENDIF.

*eject
* Get the offset and length

  CALL METHOD cl_abap_matcher=>get_object
    RECEIVING
      matcher = lr_matcher.

  TRY.

      lv_offset = lr_matcher->get_offset( ).

      lv_length = lr_matcher->get_length( ).

    CATCH CX_SY_MATCHER.

      cv_subrc = 5. "raise parse_error
      RAISE PARSE_ERROR.

  ENDTRY.

  IF     ( lv_length IS INITIAL ).
    RETURN.
  ENDIF.

* Set the token
  lv_token = iv_text+lv_offset(lv_length).

  IF     ( lv_token IS INITIAL ).
    RETURN.
  ENDIF.

  cv_offset = lv_offset.
  cv_length = lv_length.
  cv_token  = lv_token.



ENDFUNCTION.
