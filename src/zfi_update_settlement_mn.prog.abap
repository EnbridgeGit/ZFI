*&---------------------------------------------------------------------*
*&  Include           ZFI_UPDATE_SETTLEMENT_MN
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  DATA:lv_hkont TYPE ska1-saknr,
       lv_anln1 TYPE anlh-anln1.
  IF p_hkont IS NOT INITIAL.
    SELECT SINGLE saknr INTO lv_hkont FROM ska1 WHERE saknr = p_hkont.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Invalid G/L Account number' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSEIF p_anln1 IS NOT INITIAL.
    SELECT SINGLE anln1 INTO lv_anln1 FROM anlh WHERE anln1 = p_anln1.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Invalid FXA number' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
  IF p_hkont IS NOT INITIAL AND r_open EQ 'X'.
    MESSAGE 'Select either New Project or Open Project' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF p_anln1 IS NOT INITIAL AND ( r_new EQ 'X' OR r_exist EQ 'X' OR r_age EQ 'X' ).
    MESSAGE 'Select only Dummy FXA for open project' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF p_hkont IS NOT INITIAL AND p_anln1 IS NOT INITIAL.
    IF r_new EQ 'X' OR r_exist EQ 'X' OR r_age EQ 'X'.
      MESSAGE 'Enter G/L number only' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ELSE.
      MESSAGE 'Enter FXA number only' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSEIF r_ddate IS NOT INITIAL OR r_fxa IS NOT INITIAL.
    IF p_anln1 IS NOT INITIAL AND p_hkont IS NOT INITIAL.
      IF p_anln1 IS INITIAL.
        IF p_hkont IS INITIAL.
          MESSAGE 'Enter FXA Number or G/L Number' TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ELSEIF p_hkont IS NOT INITIAL.
        MESSAGE 'Enter Either FXA Number or G/L Number not both.' TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDIF.
  IF r_ddate IS NOT INITIAL.
    IF p_per_f IS INITIAL OR p_year_f IS INITIAL.
      MESSAGE 'Enter From Period and From Year' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
  IF r_new EQ 'X'.
    PERFORM get_data_new.
    PERFORM update_srule_new.
  ELSEIF r_sr = 'X'.
    PERFORM get_data_sr.
    PERFORM update_srule_sr.
  ELSEIF r_exist = 'X'.
    PERFORM get_data_exist.
    PERFORM update_srule_exist.
  ELSEIF r_open = 'X'.
    PERFORM get_data_open.
    PERFORM update_srule_open.
  ELSEIF r_age ='X'.
    PERFORM get_data_open.
    PERFORM update_srule_age.
  ELSEIF r_del = 'X'.
    PERFORM get_data_del.
    PERFORM update_srule_del.
  ELSEIF r_wip ='X'.
    PERFORM get_data_wip.
    PERFORM update_srule_wip.
  ELSEIF r_ddate = 'X'.
    PERFORM get_data_ddate.
    PERFORM update_srule_ddate.
  ELSEIF r_fxa = 'X'.
    PERFORM get_data_fxa.
    PERFORM update_srule_fxa.
  ENDIF.
  PERFORM build_fieldcatalog.
  IF gt_output[] IS NOT INITIAL.
    PERFORM display_alv.
  ELSE.
    WRITE : / 'No data to output......'.
  ENDIF.
