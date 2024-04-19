*&---------------------------------------------------------------------*
*& Report  ZFAPI008_POWERADVOCATE
*&---------------------------------------------------------------------*
REPORT  zfapi008_poweradvocate  MESSAGE-ID zfi01
                                LINE-COUNT 58
                                LINE-SIZE 170
                                NO STANDARD PAGE HEADING.

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI008_POWERADVOCATE                            *
*  Author:           John Hartung                                      *
*  Date:             May 19, 2014                                      *
*  Track #:          TR928 Release 1 - Streamline                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      AP PowerAdvocate - Outbound                       *
*                                                                      *
*                    Extract cleared vendor documents                  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By           Description                                    *
* -------- ------------ ---------------------------------------------- *
* 05/19/14 JRHARTUNG    D30K924122 - Initial program development       *
*                       D30K924256, D30K924362, D30K924476             *
* 07/26/17 JRHARTUNG    ACR-4710    D30K928284, D30K928296, D30K928308 *
*                       PowerAdvocate Break/Fix Enhancements           *
*                       1. Add Payment Method "N" - E-Payables BOA     *
*                       2. Replace invalid characters with a blank     *
*                       3. Scan Material Description for invalid chars *
*                       4. Copy the PO Number to all items in an MM    *
*                          (MIRO) Invoice (commented out D30K928520 )  *
*----------------------------------------------------------------------*
************************************************************************

INCLUDE zfapi008_poweradvocate_top.

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.
**-- stsrt of changes by AKMADASU
*
** Get the program parameter values
*  PERFORM  f_select_xparam.
*
** Initialization
*  PERFORM  f_initialization.
**-- END OF CHANGES BY AKMADASU
*eject
************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

* At selection-screen output
  PERFORM  f_at_sel_screen_output.

* Set the listbox values
  PERFORM  f_at_sel_out_set_lstbx_valus.
**-- stsrt of changes by AKMADASU
* Get the program parameter values
  PERFORM  f_select_xparam.

* Initialization
  PERFORM  f_initialization.
**-- stsrt of changes by AKMADASU
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath1.

* F4 help - presentation server - file open dialog - file 1
  PERFORM  f_f4_help_pres_file     CHANGING p_fpath1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath2.

* F4 help - presentation server - file open dialog - file 2
  PERFORM  f_f4_help_pres_file     CHANGING p_fpath2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath3.

* F4 help - presentation server - file open dialog - file 3
  PERFORM  f_f4_help_pres_file     CHANGING p_fpath3.

AT SELECTION-SCREEN.

* Check if the ZFIT_XPARAM entries have been maintained
  PERFORM  f_validate_xparam_entries.

************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Initial the data elements
  PERFORM  f_initial_data_elements.

* Validate the selection screen select-options
  CLEAR    gv_flag_err_vldt.

  PERFORM  f_validate_select_options
                                   CHANGING gv_flag_err_vldt.

  IF     ( gv_flag_err_vldt              IS NOT INITIAL ).
    LEAVE  LIST-PROCESSING.
  ENDIF.

*eject
************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

* Open the files
  PERFORM  f_open_files.

* Format the output column headings
  PERFORM  f_format_headings.

* Process the extract - main routine
  PERFORM  f_process_extract_main.

* Output the data
  PERFORM  f_output_data.

* Close the files
  PERFORM  f_close_files.

* Write the spool report
  PERFORM  f_write_report.

************************************************************************
*                             Top-Of-Page                              *
************************************************************************
TOP-OF-PAGE.

  PERFORM  f_print_report_header.

  INCLUDE zfapi008_poweradvocate_f01.
