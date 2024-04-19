*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_MTD_MAIN
*&---------------------------------------------------------------------*
INITIALIZATION.

  PERFORM DEFAULT_VALUES.

START-OF-SELECTION.

  PERFORM PROCESS_MTD.
  PERFORM WRITE_FILE.

END-OF-SELECTION.
