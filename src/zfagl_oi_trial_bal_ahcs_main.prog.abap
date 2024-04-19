*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_AHCS_MAIN
*&---------------------------------------------------------------------*

INITIALIZATION.

  PERFORM DEFAULT_VALUES.

START-OF-SELECTION.

  PERFORM READ_PROCESS_YTD.

  PERFORM WRITE_FILE.

END-OF-SELECTION.
