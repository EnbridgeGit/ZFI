FUNCTION zfi_doa_search_help_grd .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*& Function Module    :  ZFI_DOA_SEARCH_HELP_GRD                       *
*& Author             :  Bheemesh Vaikuntam                            *
*& Creation Date      :  01-NOV-2011                                   *
*& Object ID          :  NA                                            *
*& Application Area   :  NA                                            *
*& Description        :  Search help exit FM to get all Grade infolist *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log(Latest Version on Top)         *
*----------------------------------------------------------------------*
* Version No    :                                                      *
* Date          :                                                      *
* Modified By   :                                                      *
* Correction No :                                                      *
* Description   :                                                      *
*----------------------------------------------------------------------*


* EXIT immediately, if you do not want to handle this step
  IF callcontrol-step <> 'SELONE' AND
     callcontrol-step <> 'SELECT' AND
     " AND SO ON
     callcontrol-step <> 'DISP'.
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP SELONE  (Select one of the elementary searchhelps)
*"----------------------------------------------------------------------
* This step is only called for collective searchhelps. It may be used
* to reduce the amount of elementary searchhelps given in SHLP_TAB.
* The compound searchhelp is given in SHLP.
* If you do not change CALLCONTROL-STEP, the next step is the
* dialog, to select one of the elementary searchhelps.
* If you want to skip this dialog, you have to return the selected
* elementary searchhelp in SHLP and to change CALLCONTROL-STEP to
* either to 'PRESEL' or to 'SELECT'.
  IF callcontrol-step = 'SELONE'.






*   PERFORM SELONE .........
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF callcontrol-step = 'PRESEL'.
*   PERFORM PRESEL ..........
    EXIT.
  ENDIF.
*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpfull in this
* step.
  IF callcontrol-step = 'SELECT'.
*   PERFORM STEP_SELECT TABLES RECORD_TAB SHLP_TAB
*                       CHANGING SHLP CALLCONTROL RC.
*   IF RC = 0.
*     CALLCONTROL-STEP = 'DISP'.
*   ELSE.
*     CALLCONTROL-STEP = 'EXIT'.
*   ENDIF.

    EXIT. "Don't process STEP DISP additionally in this call.
  ENDIF.
*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
* Standard function modules F4UT_PARAMETER_VALUE_GET and
* F4UT_PARAMETER_RESULTS_PUT may be very helpfull in this step.
  IF callcontrol-step = 'DISP'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB SHLP_TAB
*                           CHANGING SHLP CALLCONTROL.
    TYPES : BEGIN OF lookupkey,
              lookupkey TYPE char12,
              trfar     TYPE char2,
              trfgb     TYPE char2,
              trfst     TYPE char2,
              begda     TYPE datum,
              endda     TYPE datum,
            END OF lookupkey.

    DATA: it_pernr  TYPE STANDARD TABLE OF lookupkey,
          it_grade  TYPE STANDARD TABLE OF lookupkey,
          iface     LIKE ddshfprop OCCURS 0 WITH HEADER LINE.

    DATA: wa_shlp  TYPE shlp_descr_t.

    DATA: lv_lookupkey_type TYPE char1,
          ltp_log_dest      TYPE tb_rfcdest.



    READ TABLE shlp_tab INTO wa_shlp INDEX 2.
    IF sy-subrc = 0.
      iface[] = wa_shlp-fieldprop[].
      READ TABLE iface WITH KEY fieldname = 'LOOKUP_KEY_TYPE'.
      IF sy-subrc = 0.
        lv_lookupkey_type = iface-defaultval+1(2).
      ENDIF.
      CLEAR: wa_shlp.
    ELSE.
      LOOP AT shlp_tab INTO wa_shlp
                       WHERE shlpname EQ 'ZFIDOA_LOOKUPKEY_GRADE'.
        iface[] = wa_shlp-fieldprop[].
        READ TABLE iface WITH KEY fieldname = 'LOOKUP_KEY_TYPE'.
        IF sy-subrc = 0.
          lv_lookupkey_type = iface-defaultval+1(2).
        ENDIF.
        CLEAR: wa_shlp.
      ENDLOOP.
    ENDIF.


    IF lv_lookupkey_type = 'G'.
      CALL FUNCTION 'ZFI_GET_RFC_DEST'
        EXPORTING
          imp_paramtype = 'HR'
**       IMP_SUBTYPE   = 'LOGSYS'
*         imp_sysid     = sy-sysid
*         imp_client    = sy-mandt
        IMPORTING
          exp_rfcdest   = ltp_log_dest.

      CALL FUNCTION 'ZFI_EMP_GRADE_GETLIST' DESTINATION ltp_log_dest
        EXPORTING
          imp_lookupkeytype = lv_lookupkey_type
        TABLES
          pernrlist         = it_pernr
          gradelist         = it_grade.

      record_tab[] = it_grade[].



    ENDIF.
    EXIT.

  ENDIF.

ENDFUNCTION.
