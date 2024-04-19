*&---------------------------------------------------------------------*
*&  Include           ZHR_DOA_DEMO_OUT_MAIN
*&---------------------------------------------------------------------*

INITIALIZATION.

START-OF-SELECTION.

*Get config data and default data.
  PERFORM get_default_data.
*Get pernrs.
  PERFORM get_pernr.
*Fetch the data from correpsonding infotypes
  perform fill_detail_record.
  perform final_file.
END-OF-SELECTION.
*perform programstatus.


  IF p_pres = abap_true AND p_test NE abap_true.
    PERFORM write_to_presentation.
  ELSEIF  p_appl = abap_true AND p_test NE abap_true.
    PERFORM write_to_appl_server.

  ENDIF.

perform programstatus.
