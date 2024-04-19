*&---------------------------------------------------------------------*
*& Report  ZFTRP002_MEMORECORDUPDATE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFTRR002_MEMORECORDUPDATE.


*Top Include for Data Definition
INCLUDE ZFTRR002_MEMORECORDUPDATE_TOP.
*INCLUDE ZFTRP002_memorecordupdate_top.

*Include for Form Definition.
INCLUDE ZFTRR002_MEMORECORDUPDATE_FORM.
*INCLUDE ZFTRP002_memorecordupdate_form.

START-OF-SELECTION.

*clear previous records before fetching.
  PERFORM clear.
* Fetching data from FDES table.
  PERFORM fetch_data.

* If test Mode then do not update the table FDES.
  IF  cb_test NE gc_x.
    IF git_fdes[] IS NOT INITIAL.

      PERFORM fetch_check_table_records.

*Update records using BDC for FF63
      PERFORM update_ff63_change.

    ENDIF.
  ENDIF.

  IF sy-batch NE gc_x.
    PERFORM display_final_format.
  ENDIF.
