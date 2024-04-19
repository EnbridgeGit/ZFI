*&---------------------------------------------------------------------*
*& Report  ZFAPI114_PAYMENT_VOIDS_LGC                                  *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZFAPI114_PAYMENT_VOIDS                         *
* Include            :  ZFAPI114_PAYMENT_VOIDS_LGC                     *
* Author             :  Vijay Rajaputra                                *
* Date               :  05-Mar-2018                                    *
* Technical Contact  :  Vijay Rajaputra                                *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  ATCAT Payment Voids Extract Interface          *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 05-Mar-2018  VRAJAPUTRA    D30K928605 CHG0100819 Initial Development *
*                            D30K928780                                *
*&---------------------------------------------------------------------*

INITIALIZATION.
*->TO set Default Folder
  PERFORM  f_get_folder_path CHANGING p_path2.

*->TO set Default Folder
  PERFORM  f_get_file_name CHANGING p_file2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.
  PERFORM  f_get_f4_help_file_path.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.
  PERFORM  f_get_f4_help_file_path1.

START-OF-SELECTION.

  PERFORM  f_get_data.  " To Get the data

END-OF-SELECTION.

  PERFORM  f_format_data. " To buld the Final Data

  PERFORM  f_dis_data.  " To display or Place the file in Application layer.
