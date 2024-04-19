*&---------------------------------------------------------------------*
*&  Include           ZFAPI129_INVOICE_WF_ITEMS_LGC
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI129_INVOICE_WF_ITEMS                      *
* Include Program    :  ZFAPI129_INVOICE_WF_ITEMS_LGC                  *
* Author             :  Vijay Rajaputra                                *
* Creation Date      :  05-Nov-2018                                    *
* Application Area   :  FICO                                           *
* Technical Contact  :  Vijay Rajaputra                                *
*                                                                      *
* Purpose            :  Event Logic Include Program                    *
*                                                                      *
*----------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 05-Nov-2018  VRAJAPUTRA  D30K929291  CHG0130803  Initial development *
*                          D30K929374                                  *
* 22-Jan-2019  VRAJAPUTRA  D30K929505  CHG0135339  Duplicate checks    *
*                          D30K929531                                  *
* 31-Jan-2019  VRAJAPUTRA  D30K929554  CHG0136094  Duplicate checks    *
*&---------------------------------------------------------------------*

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

  PERFORM  f_default_selection_screen.

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
