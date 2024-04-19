*&---------------------------------------------------------------------*
*&  Include           ZFAPI122_OPEN_INVC_ITEMS_LGC
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI122_OPEN_INVC_ITEMS                       *
* Include Program    :  ZFAPI122_OPEN_INVC_ITEMS_LGC                   *
* Author             :  Paul Karunakar                                 *
* Creation Date      :  16-Apr-2018                                    *
* Application Area   :  FICO                                           *
* Description        :  Open Invoice data from each of the three SAP   *
*                       instances will be extracted in a delimited     *
*                       file and sent to IAP.                          *
*                                                                      *
*&---------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 16-Apr-2018  KPAL        D30K928646  CHG0108943  Initial Development *
*                          D30K928913, D30K929061                      *
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
