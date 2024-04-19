*&---------------------------------------------------------------------*
*&  Include           ZFAPI101_COMPANY_LGC
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI101_COMPANY                              *
* Include Name       :   ZFAPI101_COMPANY_LGC                          *
* Author             :                                                 *
* Date               :   11-Jan-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Company Codes Extraction  for IAP              *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 15-FEB-2018  VRAJAPUTRA  D30K928562 CHG0100805 - Initial development *
*                          D30K928598                                  *
*&---------------------------------------------------------------------*

INITIALIZATION.
*->TO set Default Folder
  PERFORM f_get_folder_path CHANGING p_path2.

*->TO set Default File Name
  PERFORM f_get_file_name CHANGING p_file2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.
  PERFORM f_get_f4_help_file_path.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.
  PERFORM f_get_f4_help_file_path1.


START-OF-SELECTION.

  PERFORM f_get_bukrs_data CHANGING gt_bukrs.  " To Get BUKRS Data

  PERFORM f_final_data USING gt_bukrs         " To get format the final data
                    CHANGING gt_data.

END-OF-SELECTION.

  IF p_dis =  gc_x."'X'.
    PERFORM f_display_data USING gt_data.  " To display or Place the file in Application layer.
  ELSE.
    PERFORM f_transport_data USING gt_data. " To place the file in Application layer.
  ENDIF.
