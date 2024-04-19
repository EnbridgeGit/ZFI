*&---------------------------------------------------------------------*
*& Report  ZFGLE035_CREATE_CHECK_DOC
*&---------------------------------------------------------------------*
REPORT  zfgle035_create_check_doc   MESSAGE-ID zfi01
                                    LINE-COUNT 65
                                    LINE-SIZE 120
                                    NO STANDARD PAGE HEADING.

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFGLE035_CREATE_CHECK_DOC                         *
*  Author:           John Hartung                                      *
*  Date:             December 11, 2013                                 *
*  Application Area: FICO GL                                           *
*                                                                      *
*  Description:      GL Create Check Document from G/L Posting         *
*                                                                      *
*                    FI G/L documents are queried and found items      *
*                    are used to create Check Documents.  These        *
*                    Check documents are required in order for         *
*                    FEBAN processing to automatically clear the       *
*                    postings once the actual check is cashed.         *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By           Transport  Description                         *
* -------- ------------ ---------- ----------------------------------- *
* 10/23/19 JRHARTUNG    D30K930228 CHG0160254 - Initial Program        *
*                                  Development                         *
* 10/31/19 JRHARTUNG    D30K930244 CHG0160254 - Do not adjust check #  *
*----------------------------------------------------------------------*
************************************************************************
INCLUDE: zfgle035_create_check_doc_top.

*eject
****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
*===============================================================
* class c_event_receiver: local class to handle print events...
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_top_of_page
        FOR EVENT print_top_of_page OF cl_gui_alv_grid,
      handle_end_of_page
        FOR EVENT print_end_of_page OF cl_gui_alv_grid,
      handle_top_of_list
        FOR EVENT print_top_of_list OF cl_gui_alv_grid,
      handle_end_of_list
        FOR EVENT print_end_of_list OF cl_gui_alv_grid.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*
* c_event_receiver (Definition)
*===============================================================

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
*===============================================================
* class c_event_receiver (Implementation)
*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_top_of_page.
  ENDMETHOD.                           "handle_top_of_page

  METHOD handle_end_of_page.
  ENDMETHOD.                           "handle_end_of_page

  METHOD handle_top_of_list.
  ENDMETHOD.                           "handle_top_of_list

  METHOD handle_end_of_list.
  ENDMETHOD.                           "handle_end_of_list

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*
* c_event_receiver (Implementation)
*===================================================================

*eject
************************************************************************
*                             Top-Of-Page                              *
************************************************************************
TOP-OF-PAGE.

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

* Set the report id
  CLEAR                                gv_repid.
  MOVE     sy-repid                 TO gv_repid.

* Get the default LVC display variant

  PERFORM  f_lvc_variant_default_get.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dspvar.

* Value request "drop-down" for the LVC display variant

  PERFORM  f_lvc_variant_f4.

AT SELECTION-SCREEN.

* Validate that the LVC display variant exists

  PERFORM  f_lvc_variant_exists.

*eject
************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Validate the select-options
  CLEAR    gv_flag_exit.

  PERFORM  f_validate_sel_opts.

  IF     ( gv_flag_exit IS NOT INITIAL ).

    LEAVE    LIST-PROCESSING.

  ENDIF.

* Initial the data elements

  PERFORM  f_initial_data_elements.

* Select the data

  PERFORM  f_select_data.

* Process the data

  PERFORM  f_process_data.

************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

  CALL     SCREEN 9001.

*eject
*&---------------------------------------------------------------------*
*&      Module  PBO_9001  OUTPUT
*&---------------------------------------------------------------------*
*       Screen 9001 Process Before Output Module.  Performs code
*       to create the GUI ALV grid and display the summary report.
*----------------------------------------------------------------------*
MODULE PBO_9001 OUTPUT.

* Set GUI status and title bar
  SET PF-STATUS 'MAIN9001'.
  SET TITLEBAR  'MAIN9001'.

* Create the ALV grid

  PERFORM  f_create_alv_grid_rpt1.

ENDMODULE.                 " PBO_9001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_9001  INPUT
*&---------------------------------------------------------------------*
*       Screen 9001 Process After Input Module.  Performs code
*       to exit the screen and free space used by the summary report.
*----------------------------------------------------------------------*
MODULE PAI_9001 INPUT.

* Process screen OK code
  CLEAR                      gv_ucomm.
  MOVE     gv_ok_code     TO gv_ucomm.
  CLEAR    gv_ok_code.
  CASE     gv_ucomm.
    WHEN     'BACK' OR 'EXIT' OR 'CANC'.

      PERFORM  f_exit_alv_screen_rpt1.

  ENDCASE.

ENDMODULE.                 " PAI_9001  INPUT

INCLUDE: zfgle035_create_check_doc_f01.
