*&---------------------------------------------------------------------*
*& Report  ZFAPI029_DOA_EXTRACT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFAPI029_DOA_EXTRACT.

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI028_DOA_EXTRACT                              *
*  Author:           MNagarathina                                      *
*  Date:             August 09, 2012                                   *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      AP DOA and User Roles Extract                     *
*                                                                      *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date      By           Transport   Description                      *
* ---------- ------------ ----------  -------------------------------- *
* 08/09/2012 MNagarathina DECK901428  Initial program development      *
*                                     Ticket#19069
* 09/19/2012 MNagarathina DECK907208  Rename obj id from 028 to 029    *
*                                     Ticket#19069
*----------------------------------------------------------------------*
************************************************************************

************************************************************************
*                               Includes                               *
************************************************************************
INCLUDE zfapi029_doa_extract_top.
INCLUDE zfapi029_doa_extract_f01.

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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath. "output file
* F4 help - presentation server - file open dialog - output file
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

* Select payment and related data
  PERFORM f_select_data.

* Generate the output
  PERFORM f_generate_output.

* Send email containing run details
  PERFORM  f_send_email.

*eject
************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.
* Display the results
  PERFORM  f_display_results.

  IF   ( ( gv_flag_error        IS NOT INITIAL ) AND
         ( sy-batch             IS NOT INITIAL )     ).
    MESSAGE  e000(zfi01)   WITH text-199.
  ENDIF.
