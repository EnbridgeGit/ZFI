*&---------------------------------------------------------------------*
*&  Include           ZFAPI118_POST_IAP_DOCUMENT_LGC
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI118_POST_IAP_DOCUMENT                        *
*  Include:          ZFAPI118_POST_IAP_DOCUMENT_LGC                    *
*  Author:           Vijay Rajaputra ( Copy of US System )             *
*  Date:             Aug, 01 2018                                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      Post IAP Documents for PO and Non-PO Invoices     *
*                    and Credit Memos                                  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 08/01/18 VRAJAPUTRA D30K928896 - CHG0109670 - Initial program        *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

* Initialization
  PERFORM  f_initialization.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

* Selection screen output
  PERFORM  f_sel_scrn_output.

AT SELECTION-SCREEN ON VALUE-REQUEST    FOR p_fnam1p.

* F4 help - presentation server - file open dialog
  PERFORM  f_f4_help_pres_file     CHANGING p_fnam1p.

AT SELECTION-SCREEN ON VALUE-REQUEST    FOR p_fnam2p.

* F4 help - presentation server - file open dialog
  PERFORM  f_f4_help_pres_file     CHANGING p_fnam2p.

AT SELECTION-SCREEN ON VALUE-REQUEST    FOR p_fnam3p.

* F4 help - presentation server - file open dialog
  PERFORM  f_f4_help_pres_file     CHANGING p_fnam3p.

AT SELECTION-SCREEN.

*eject
************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Validate the selection screen
  CLEAR    gv_fl_errr_prcs.
  CLEAR    gv_fl_errr_mstr.

  PERFORM  f_validate_selection_screen.

  IF       ( gv_fl_errr_prcs             IS NOT INITIAL ).
    IF     ( sy-batch                    IS     INITIAL ).
      LEAVE  LIST-PROCESSING.
    ENDIF.
  ENDIF.

  IF       ( gv_fl_errr_prcs             IS     INITIAL ).
    PERFORM  f_build_file_list.
  ENDIF.

************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

* Main process
  IF       ( gv_fl_errr_prcs             IS     INITIAL ).
    PERFORM  f_process_main.
  ENDIF.

* Send a notification that there was a process error
  IF     ( ( gv_fl_errr_mstr             IS NOT INITIAL ) AND
           ( rb_fsapl                    IS NOT INITIAL ) AND
           ( cb_test                     IS     INITIAL )     ).
    PERFORM  f_send_email.
  ENDIF.

  IF       ( gv_fl_errr_mstr             IS INITIAL ).
    macro_msg                       'B' 'S' text-291.
  ELSE.
    macro_msg                       'B' 'A' text-292.
  ENDIF.
