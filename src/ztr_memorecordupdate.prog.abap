*&---------------------------------------------------------------------*
*& Report  ZTR_MEMORECORDUPDATE
*&
*&---------------------------------------------------------------------*
*&Date          Changed By    Description
*&Aug 17, 2012  SAHMAD        New Program
*&---------------------------------------------------------------------*

REPORT  ZTR_MEMORECORDUPDATE.
*Top Include for Data Definition
INCLUDE ztr_memorecordupdate_top.

*Include for Form Definition.
INCLUDE ztr_memorecordupdate_form.

*****************************************************************************************
*** AT SELECTION SCREEN ON BLOCK                                                      ***
*****************************************************************************************

*AT SELECTION-SCREEN ON BLOCK blk1.

*  CLEAR : gv_ebene_validation.
*  IF s_ebene[] IS NOT INITIAL.
*    SELECT SINGLE ebene
*       FROM t036
*       INTO gv_ebene_validation
*      WHERE ebene IN s_ebene[].
*    IF sy-subrc NE 0.
*      MESSAGE e000(zfi01) WITH text-002.
*    ENDIF.
*  ENDIF.

****************************************************************************************
*** AT SELECTION SCREEN ON BLOCK                                                     ***
****************************************************************************************

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
