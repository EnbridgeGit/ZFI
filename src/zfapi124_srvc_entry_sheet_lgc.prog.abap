*&---------------------------------------------------------------------*
*&  Include           ZFAPI124_SRVC_ENTRY_SHEET_LGC
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       : ZFAPI124_SRVC_ENTRY_SHEET                       *
* Author             :                                                 *
* Date               : Mar 26, 2018                                    *
* Technical Contact  : Paul Karunakar                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            : The Service Entry Sheet (SES) master data       *
*                      from each of the three SAP instances will be    *
*                      extracted in a delimited file and sent to IAP.  *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 26-Mar-2018  KPAL        D30K928726  CHG0106361  Initial Development *
*                          D30K928730, D30K928878, D30K928919          *
*                          D30K928927                                  *
*----------------------------------------------------------------------*

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

  PERFORM  f_get_filepath_filename CHANGING p_path2
                                            p_file2.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN    ON VALUE-REQUEST FOR p_path2.

  PERFORM  f_get_f4_help_file_path.

AT SELECTION-SCREEN    ON VALUE-REQUEST FOR p_path1.

  PERFORM  f_get_f4_help_file_path1.

************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

  PERFORM  f_initial_data_elements.

************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

  PERFORM  f_process_main.
