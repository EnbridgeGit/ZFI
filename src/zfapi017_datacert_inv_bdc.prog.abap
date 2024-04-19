*&---------------------------------------------------------------------*
*& Report  ZFAPI130_CIMPL_INV_BDC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfapi017_datacert_inv_bdc MESSAGE-ID zfi01
                               LINE-COUNT 65
                               LINE-SIZE 120
                               NO STANDARD PAGE HEADING.

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI130_CIMPL_INV_BDC                            *
*  Author:           Shamsiya Shaffe                                   *
*  Date:             August 29, 2019                                   *
*  Track #:                                                            *
*  Application Area: FICO                                              *
*                                                                      *
*  Description:      Cimpl Interface Invoice Inbound BDC               *
*                                                                      *
*                    Read a file in the RFBIBL00 format, translate     *
*                    the records into BDC data, and post the Invoice   *
*                    transactions.                                     *
*                                                                      *
*                    Copy from US Report - ZFAPI017_DATACERT_INV_BDC   *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    TR # By      Description                                    *
* -------- ---- ------- ---------------------------------------------- *
*08/29/2019 D30K930117 SHAFFES CHG0153812 - Initial program development*
*10/11/2019 D30K930204 SHAFFES CHG0153812 - Initial program development*
*----------------------------------------------------------------------*
************************************************************************

************************************************************************
*                               Includes                               *
************************************************************************
INCLUDE zfapi017_cimpl_inv_bdc_top.
INCLUDE zfapi017_cimpl_inv_bdc_f01.

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

* Get the program parameter values

  PERFORM  f_select_xparam.

* Set the email ID

  PERFORM  f_set_email_id.

*eject
************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

* Selection screen fields to be disabled/hidden

  PERFORM  f_toggle_functionality.

* Set the selection screen options

  PERFORM  f_set_sel_scrn_options.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath.

* F4 help - presentation server - file open dialog

  PERFORM  f_f4_help_pres_file     CHANGING p_fpath.

AT SELECTION-SCREEN.

* Check if the ZFIT_XPARAM entries have been maintained

  PERFORM  f_validate_xparam_entries.

************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Initial the data elements

  PERFORM  f_initial_data_elements.

* Build the list of files to be processed

  PERFORM  f_build_file_list    TABLES git_file_list.

*eject
************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

* Process each file
  CLEAR                                gwa_file_list.
  LOOP AT  git_file_list          INTO gwa_file_list.
    gv_nbr_file = sy-tabix.

    CLEAR    git_rfbibl00[].
    CLEAR    git_docs_posted[].
    CLEAR    git_errs_data[].
    CLEAR    gv_flag_err_data.
    CLEAR    gv_count_err_data.

* Input the RFBIBL00 formatted data

    PERFORM  f_get_input_data TABLES   git_rfbibl00
                                       git_file_stats
                              USING    gwa_file_list.

* Process the RFBIBL00 formatted records

    PERFORM  f_process_rfbibl00
                              TABLES   git_rfbibl00.

* Archive the input file

    PERFORM  f_archive_file   TABLES   git_file_stats.

* Write the report

    PERFORM  f_write_report   TABLES   git_file_stats.

    IF     ( gv_flag_err_proc   IS NOT INITIAL ).
      EXIT.
    ENDIF.

    CLEAR  gwa_file_list.
  ENDLOOP.

  IF     ( gv_nbr_file              IS INITIAL ).

    PERFORM  f_write_report     TABLES git_file_stats.

  ENDIF.

*eject
* Send email containing run details

  PERFORM  f_send_email.

  IF   ( ( gv_flag_err_proc     IS NOT INITIAL ) AND
         ( sy-batch             IS NOT INITIAL )     ).
    MESSAGE  e000(zfi01)   WITH text-199.
  ENDIF.

************************************************************************
*                             Top-Of-Page                              *
************************************************************************
TOP-OF-PAGE.

  PERFORM  f_print_report_header.
