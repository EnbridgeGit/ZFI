*&---------------------------------------------------------------------*
*&  Include           ZFAPE101_VENDOR_CONTACT_LGC
*&---------------------------------------------------------------------*

*=======================================================================
* START-OF-SELECTION
*=======================================================================
START-OF-SELECTION.
* Create Instance
  CREATE OBJECT gref_util.

* Fetch data
  PERFORM f_get_data.
* Update data to output table
  PERFORM f_fill_data.

*=======================================================================
* END-OF-SELECTION
*=======================================================================
END-OF-SELECTION.
  IF p_pres = abap_true." AND p_test NE abap_true.
    PERFORM f_write_to_presentation.
  ENDIF.
