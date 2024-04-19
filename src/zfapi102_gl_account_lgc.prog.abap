*&---------------------------------------------------------------------*
*&  Include           ZFAPI102_GL_ACCOUNT_LGC
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI102_GL_ACCOUNT                           *
* Author             :                                                 *
* Date               :   16-Feb-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  G/L Accounts Extraction  for IAP               *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 16-Feb-2018  VRAJAPUTRA  D30K928566 CHG0100806 # Initial development *
*                          D30K928825, D30K928874, D30K928888          *
*&---------------------------------------------------------------------*

INITIALIZATION.

  PERFORM f_set_date.

*->To Get Default Folder
  PERFORM f_get_folder_path CHANGING p_path2.

*->TO set Default Folder
  PERFORM f_get_file_name CHANGING p_file2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.
  PERFORM f_get_f4_help_file_path.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.
  PERFORM f_get_f4_help_file_path1.

START-OF-SELECTION.

* Initial the data elements
  PERFORM  f_initial_data_elements.

  PERFORM f_get_data.  " To Get the data

END-Of-SELECTION.

  PERFORM f_format_data. " To buld the Final Data

  PERFORM f_dis_data.  " To display or Place the file in Application layer.
