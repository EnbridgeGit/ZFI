*&---------------------------------------------------------------------*
*&  Include           ZFI_UPDATE_IT0105_MAIN
*&---------------------------------------------------------------------*
INITIALIZATION.

AT SELECTION-SCREEN.
  lv_mode = 'N'.
* Opening window for path selection
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = p_file.
* start of selection event.
START-OF-SELECTION.

  perform upload_data.
  perform process_data.
  PERFORM create_alv.
