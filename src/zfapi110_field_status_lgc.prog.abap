*&---------------------------------------------------------------------*
*&  Include           ZFAPI110_FIELD_STATUS_LGC
*&---------------------------------------------------------------------*
INITIALIZATION.

  PERFORM  f_get_file_path CHANGING p_path2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.

  PERFORM  f_get_f4_help_file_path.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.

  PERFORM  f_get_f4_help_file_path1.

START-OF-SELECTION.

  PERFORM  f_get_data.

*=======================================================================
* END-OF-SELECTION
*=======================================================================
END-OF-SELECTION.

  IF     ( p_dis EQ abap_true ).

    PERFORM  f_write_to_presentation.

  ELSEIF ( p_app EQ abap_true ).

    PERFORM  f_write_to_appl_server.

  ENDIF.
