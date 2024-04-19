*&---------------------------------------------------------------------*
*&  Include          ZFAPI121_VENDOR_INBOUND_LGC
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFAPI121_VENDOR_INBOUND                       *
* Program Include    :   ZFAPI121_VENDOR_INBOUND_LGC                   *
* Author             :                                                 *
* Date               :   Apr 15, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Vendor Inbound Interface                      *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 15-Apr-2018  CPALYAM     D30K929071-Initial development              *
*----------------------------------------------------------------------*

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


*=======================================================================
* START-OF-SELECTION
*=======================================================================
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
