*&---------------------------------------------------------------------*
*& Report  ZFAPI112_OPEN_INVOICES
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI112_OPEN_INVOICES                        *
* Author             :   Paul Karunakar                                *
* Date               :   Feb 1, 2018                                   *
* Technical Contact  :   John Hartung                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Extract interface of Open and Parked          *
*                        Invoices to IAP                               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  KPAL        D30K928583  CHG0100815  Initial Development *
*                          D30K928768, D30K928884                      *
*----------------------------------------------------------------------*

INITIALIZATION.

  PERFORM  f_get_file_path         CHANGING p_path2.

* Get the filename
  PERFORM  f_get_file_name         CHANGING p_file2.

AT SELECTION-SCREEN    ON VALUE-REQUEST FOR p_path2.

  PERFORM  f_get_f4_help_file_path.

AT SELECTION-SCREEN    ON VALUE-REQUEST FOR p_path1.

  PERFORM  f_get_f4_help_file_path1.

START-OF-SELECTION.

* Main process
  PERFORM  f_process_main.

END-OF-SELECTION.
