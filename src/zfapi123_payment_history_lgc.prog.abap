*&---------------------------------------------------------------------*
*&  Include           ZFAPI123_PAYMENT_HISTORY_LGC
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI123_PAYMENT_HISTORY                       *
* Include Program    :  ZFAPI123_PAYMENT_HISTORY_LGC                   *
* Author             :  Paul Karunakar                                 *
* Creation Date      :  02-May-2018                                    *
* Application Area   :  FICO                                           *
* Description        :  Payment History data from each of the three    *
*                       SAP instances will be extracted in a delimited *
*                       file and sent to IAP.                          *
*                                                                      *
*&---------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  --------------------------------*
* 02-May-2018  KPAL        D30K928709  CHG0108944  Initial Development *
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
