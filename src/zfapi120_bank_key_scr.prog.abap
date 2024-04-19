*&---------------------------------------------------------------------*
*&  Include           ZFAPI120_BANK_KEY_SCR
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPI120_BANK_KEY                             *
* Author             :                                                 *
* Date               :   Mar 22, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Bank Key Extract Interface                    *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  CPALYAM     D30K928630  CHG0106152-Initial development  *
*                          D30K928713, D30K928842, D30K928902          *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK s5 WITH FRAME TITLE text-t05.
PARAMETERS : p_erpid TYPE char05.
SELECT-OPTIONS: s_banks FOR bnka-banks,
                s_bankl FOR bnka-bankl.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE,
POSITION 1, COMMENT 1(20) text-c05 FOR FIELD p_full.
PARAMETERS:p_full RADIOBUTTON GROUP r5 DEFAULT 'X' USER-COMMAND upd2.
SELECTION-SCREEN:POSITION 40,COMMENT 40(20) text-c06 FOR FIELD p_delta.
PARAMETERS:p_delta RADIOBUTTON GROUP r5.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS s_updat FOR gv_updat NO-EXTENSION MODIF ID m3.
SELECTION-SCREEN END OF BLOCK s5.

SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-t02.

SELECTION-SCREEN: BEGIN OF LINE,
POSITION 1,COMMENT 1(20) text-c03 FOR FIELD p_pres MODIF ID p1.
PARAMETERS:p_pres RADIOBUTTON GROUP r1 USER-COMMAND upd
MODIF ID p1.
SELECTION-SCREEN:POSITION 40,COMMENT 40(20) text-c04 FOR FIELD p_appl
MODIF ID p1.
PARAMETERS:p_appl RADIOBUTTON GROUP r1 DEFAULT 'X' MODIF ID p1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK s2.

SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME TITLE text-t03.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11) text-p01 FOR FIELD p_file1 MODIF ID m1.
PARAMETERS:p_path1 TYPE string MODIF ID m1.
PARAMETERS:p_file1 TYPE string MODIF ID m1.
SELECTION-SCREEN : END OF LINE.

SELECTION-SCREEN END OF BLOCK s3.

SELECTION-SCREEN BEGIN OF BLOCK s4 WITH FRAME TITLE text-t04.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11) text-p01 FOR FIELD p_file2 MODIF ID m2.
PARAMETERS:p_path2 TYPE text128 MODIF ID m2.
PARAMETERS:p_file2 TYPE string  MODIF ID m2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:p_maxrec TYPE NUMC10 "Max Number Records per File
                     MODIF ID M2.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN END OF BLOCK s4.

***********************************************************************
* Process before output - Selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'M3'.
      IF p_full = space.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 = 'M2'.
      IF p_pres = space .
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 = 'M1'.
      IF p_appl = space .
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

  PERFORM f_default_filename.


AT SELECTION-SCREEN.

* TRANSLATE p_path2 TO UPPER CASE.
  IF sscrfields-ucomm = 'R3'.
    IF p_pres = abap_true.
      CLEAR p_file1.
      CLEAR p_path1.
    ELSE.
      CLEAR p_file2.
      CLEAR p_path2.
    ENDIF.
  ENDIF.

  IF sscrfields-ucomm = gc_ucomm_onli.

    IF ( p_file1 IS INITIAL   AND
        p_pres EQ abap_true ) OR
       ( p_file2 IS INITIAL AND
         p_appl EQ abap_true ).
      MESSAGE i081(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    IF ( p_path1 IS INITIAL AND
         p_pres EQ abap_true ) OR
        ( p_path2 IS INITIAL AND
        p_appl EQ abap_true ).
      MESSAGE i082(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    gv_fname1 = p_file1.
    gv_fold1 = p_path1.

    CLEAR: p_file1.
    IF p_erpid IS INITIAL.
      SET CURSOR FIELD 'P_ERPID'.
      MESSAGE e214(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path2.

  IF  p_pres EQ abap_true.
    PERFORM f_get_folder_path CHANGING p_path2.
  ELSEIF p_appl EQ abap_true.
    p_path2 = zcl_iap_interface_util=>f4_serverfile( ).
  ENDIF.
