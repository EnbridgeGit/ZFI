*&---------------------------------------------------------------------*
*& Report  ZLPSR004_WBS_OVERVIEW
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zlpsr004_wbs_overview MESSAGE-ID zp NO STANDARD PAGE HEADING.
INCLUDE:zlpsr004_wbs_overview_top,
        zlpsr004_wbs_overview_scr,
        zlpsr004_wbs_overview_f01.

INITIALIZATION.
  PERFORM f_refresh.

START-OF-SELECTION.
  PERFORM:f_validate_inputs,
          f_fetch.
  IF gt_report_details IS INITIAL.
    MESSAGE s005(zp) WITH text-e04 DISPLAY LIKE gc_error.
    LEAVE LIST-PROCESSING.
  ELSE.
    PERFORM f_report_display.
  ENDIF.
