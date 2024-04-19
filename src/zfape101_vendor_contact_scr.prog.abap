*&---------------------------------------------------------------------*
*&  Include           ZFAPE101_VENDOR_CONTACT_SCR
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK s5 WITH FRAME TITLE text-t05.
PARAMETERS : p_erpid TYPE CHAR05.
SELECT-OPTIONS: s_lifnr FOR gv_lifnr,
                s_ktokk FOR gv_ktokk.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN: BEGIN OF LINE,
POSITION 1, COMMENT 1(20) text-c07 FOR FIELD p_tab.
PARAMETERS:p_tab RADIOBUTTON GROUP r6 DEFAULT 'X' USER-COMMAND upd2.
SELECTION-SCREEN:POSITION 40,COMMENT 40(20) text-c08 FOR FIELD p_pipe.
PARAMETERS:p_pipe RADIOBUTTON GROUP r6.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK s5.

SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-t02.

SELECTION-SCREEN: BEGIN OF LINE,
POSITION 1,COMMENT 1(20) text-c03 FOR FIELD p_pres MODIF ID p1.
PARAMETERS:p_pres RADIOBUTTON GROUP r1 DEFAULT 'X' USER-COMMAND upd
MODIF ID p1.
PARAMETERS:p_appl RADIOBUTTON GROUP r1  MODIF ID p3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK s2.

SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME TITLE text-t03.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11) text-p01 FOR FIELD p_file1 MODIF ID m1.
PARAMETERS:p_path1 TYPE string MODIF ID m1.
PARAMETERS:p_file1 TYPE string MODIF ID m1.
SELECTION-SCREEN : END OF LINE.

SELECTION-SCREEN :BEGIN OF LINE .
SELECTION-SCREEN COMMENT 1(11) text-p02 FOR FIELD p_file3 MODIF ID m1.
PARAMETERS : p_path3 TYPE string MODIF ID m1.
PARAMETERS : p_file3 TYPE string MODIF ID m1.
SELECTION-SCREEN : END OF LINE.

SELECTION-SCREEN END OF BLOCK s3.

***********************************************************************
* Process before output - Selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF screen-group1 = 'M2'.
      IF p_pres = space .
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 = 'P3'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

  PERFORM f_default_filename.

AT SELECTION-SCREEN.

  IF sscrfields-ucomm = 'R3'.
    IF p_pres = abap_true.
      CLEAR p_file1.
      CLEAR p_path1.
    ENDIF.
  ENDIF.

  IF sscrfields-ucomm = gc_ucomm_onli.
    IF ( p_file1 IS INITIAL   AND
        p_pres EQ abap_true ) .
      MESSAGE i081(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    IF ( p_path1 IS INITIAL AND
         p_pres EQ abap_true ) .
      MESSAGE i082(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    gv_fname1 = p_file1.
    gv_fname2 = p_file3.
    gv_fold1 = p_path1.
    gv_fold2 = p_path3.

    CLEAR: p_file1,
           p_file3.

  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path1.
  cl_gui_frontend_services=>directory_browse(
    CHANGING
      selected_folder      = p_path1
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
         ).
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path3.
  cl_gui_frontend_services=>directory_browse(
    CHANGING
      selected_folder      = p_path3
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
         ).
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
