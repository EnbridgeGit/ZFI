*&---------------------------------------------------------------------*
*&  Include           ZFAPI125_GOODS_RECEIPT_LGC
*&---------------------------------------------------------------------*
************************************************************************
*                            Enbridge Energy                           *
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI125_GOODS_RECEIPT                        *
*& Author             :  Kalinga Keshari Rout                          *
*& Creation Date      :  March 26, 2018                                *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Program extracts goods receipt history data   *
*                        for both cases full load and delta load and   *
*&                       file created application server.              "
*&---------------------------------------------------------------------*
*                      Modification Log                                *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 26-Mar-2018  KROUT       D30K928872  CHG0106485  Initial Development *
*                          D30K928904, D30K928931, D30K928955,         *
*                          D30K928981, D30K929077                      *
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
