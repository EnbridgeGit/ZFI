*&---------------------------------------------------------------------*
*& Report  ZFI_PROCESS_MINING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfi_process_mining.

INCLUDE zfi_process_mining_top.
INCLUDE zfi_process_mining_ss.
INCLUDE zfi_process_mining_form.

INITIALIZATION.
PERFORM f_initialize_ss.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_ss_output.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pres.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_pres.

START-OF-SELECTION.

  PERFORM f_get_data.

  IF p_app_r = 'X'.
    PERFORM f_upload_data_app.
  ELSEIF p_pre_r = 'X'.
    PERFORM f_upload_data_pre.
  ENDIF.
