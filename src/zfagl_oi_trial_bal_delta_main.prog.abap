*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_DELTA_MAIN
*&---------------------------------------------------------------------*
INITIALIZATION.

  PERFORM default_values.

START-OF-SELECTION.

  IF cb_bclr EQ 'X'.
    DELETE FROM zfi_trial_bal.
  ENDIF.

  IF cb_bclr EQ 'X'.
    DELETE FROM zfi_trial_bal.
  ELSEIF cb_clrd EQ 'X'.
    WRITE p_prd USING EDIT MASK '==ALPHA'. "Zero padding
    CONCATENATE p_prd'.' p_ryr INTO gv_dperiod.
    DELETE FROM zfi_trial_bal WHERE reporting_period EQ gv_dperiod.
  ENDIF.

  PERFORM delta_processing.
  PERFORM write_file.
  IF gv_file_ok = 'X'. "File write success then update history table
    DELETE zfi_trial_bal FROM TABLE gt_hist_bal_del.
    COMMIT WORK.
    gv_lines = lines( gt_upd_recs ).
    LOOP AT gt_upd_recs ASSIGNING <upd_rec>.
      MODIFY zfi_trial_bal FROM <upd_rec>.
      IF sy-subrc NE 0.
        WRITE:/ 'Record Update Failed - ', <upd_rec>.
      ELSE.
        gv_updates = gv_updates + 1.
      ENDIF.
    ENDLOOP.
    SHIFT gv_updates LEFT DELETING LEADING '0'.
    SHIFT gv_lines   LEFT DELETING LEADING '0'.
    WRITE:/ gv_updates, ' of ', gv_lines, 'records updated in table ZFI_TRIAL_BAL'.
  ENDIF.

END-OF-SELECTION.
