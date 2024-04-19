*&---------------------------------------------------------------------*
*&  Include           ZFAPI116_USER_EXTRACT_LGC
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
*& Program Name       :  ZFAPI116_USER_EXTRACT                         *
*& Include            :  ZFAPI116_USER_EXTRACT_LGC                     *
*& Author             :  Kalinga Keshari Rout / Paul Karunakar         *
*& Creation Date      :  08-Mar-2018                                   *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  User data from each of the three SAP          *
*&                       instances will be extracted in a delimited    *
*&                       file and sent to IAP.                         *
*&                                                                     *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 08-Mar-2018  KROUT       D30K928695, CHG0105965  Initial development *
*                          D30K928697, D30K928840, D30K929003,         *
*                          D30K929075, D30K929081, D30K929083          *
************************************************************************

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

  PERFORM  f_get_file_path         CHANGING p_path2.

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

  PERFORM  f_get_data.

************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

  IF rb_pres =  gc_x.
    PERFORM  f_display_data .
  ENDIF.
