*&---------------------------------------------------------------------*
*& Report  ZFAPE001_VNDR_INVC_PAYM_MTHD
*&---------------------------------------------------------------------*
REPORT  zfape001_vndr_invc_paym_mthd  MESSAGE-ID zfi01
                                      LINE-COUNT 65
                                      LINE-SIZE 121
                                      NO STANDARD PAGE HEADING.

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPE001_VNDR_INVC_PAYM_MTHD                      *
*  Author:           John Hartung                                      *
*  Date:             June 23,2016                                      *
*  Ticket#:          ACR-1118                                          *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      AP Vendor Invoice Payment Method Update Program   *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 06/23/16 JRHARTUNG D30K926970 - ACR-1118 - Initial program           *
*----------------------------------------------------------------------*
************************************************************************

*eject
************************************************************************
*                              Top Include                             *
************************************************************************
INCLUDE zfape001_vndr_invc_paym_mthd_t.

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

* Get the program parameter values
  PERFORM  f_select_xparam.

* Set the selection screen defaults
  PERFORM  f_set_sel_scrn_defaults.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

* Selection screen output
  PERFORM  f_sel_scrn_output.

AT SELECTION-SCREEN.

  IF     ( s_bschl[] IS INITIAL ).
    MESSAGE  e000 WITH text-101.
  ENDIF.

  IF     ( s_zlsch[] IS INITIAL ).
    MESSAGE  e000 WITH text-102.
  ENDIF.

************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Get the data
  PERFORM  f_get_data                TABLES gt_bsik.

*eject
************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

  IF     ( gt_bsik[] IS INITIAL ).
    WRITE:   /001 text-201.
    MESSAGE  s000 WITH text-201.
  ENDIF.

* Update the vendor item
  PERFORM  f_update_vendor_item      TABLES gt_bsik
                                            gt_report.

* Output the report
  PERFORM  f_output_report           TABLES gt_report.

  READ     TABLE gt_report         WITH KEY fl_error = abap_true
                     TRANSPORTING NO FIELDS.
  IF     ( sy-subrc NE 0 ).
    MESSAGE    s000 WITH text-291.
  ELSE.
    IF     ( sy-batch                    IS NOT INITIAL ).
      MESSAGE  e000 WITH text-292.
    ELSE.
      MESSAGE  s000 WITH text-292.
    ENDIF.
  ENDIF.

************************************************************************
*                             Top-Of-Page                              *
************************************************************************
TOP-OF-PAGE.

* Print the top of page report header
  PERFORM  f_print_report_header.

  INCLUDE zfape001_vndr_invc_paym_mthd_f.
