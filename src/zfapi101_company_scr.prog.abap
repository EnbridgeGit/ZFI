*&---------------------------------------------------------------------*
*&  Include           ZFAPI101_COMPANY_SCR
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI101_COMPANY                              *
* Include Name       :   ZFAPI101_COMPANY_SCR                          *
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
* Selection Screen Declaration
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
PARAMETERS : p_erpid TYPE CHAR05." OBLIGATORY .
selection-SCREEN end of BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR t001-bukrs,
                s_ktopl FOR t001-ktopl.
selection-SCREEN end of BLOCK blk2.


SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-002.
PARAMETERS : p_app   RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmnd,
             p_dis   RADIOBUTTON GROUP rad2.
SELECTION-SCREEN end of BLOCK blk3.

*------Presentation Server--
SELECTION-SCREEN BEGIN OF BLOCK blk4 WITH FRAME TITLE text-010.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-p01 FOR FIELD p_path1 MODIF ID m1.
PARAMETERS:p_path1 TYPE string MODIF ID m1 .
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK blk4.


*---Application Layer---*
SELECTION-SCREEN BEGIN OF BLOCK blk5 WITH FRAME TITLE text-011.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-p01 FOR FIELD p_file2 MODIF ID m2.
PARAMETERS:p_path2 TYPE string MODIF ID m2 LOWER CASE.
PARAMETERS:p_file2 TYPE string MODIF ID m2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk5.


**********************************************************************
* Process before output - Selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF screen-group1 = 'M2'.
      IF p_dis = space .
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 = 'M1'.
      IF p_app = space .
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.

  IF sscrfields-ucomm = 'R3'.
    IF p_dis = abap_true.
      CLEAR p_path1.
    ELSE.
      CLEAR p_file2.
      CLEAR p_path2.
    ENDIF.
  ENDIF.

  IF sscrfields-ucomm = gc_ucomm_onli.

    IF p_app EQ gc_x. " If App SEver Selected
*->To check File NAme is entered or not
      IF  p_file2 IS INITIAL .
        MESSAGE e081(zfi01).
        CLEAR sscrfields-ucomm.
      ENDIF.
*--> TP check Folder NAme Entered or not
      IF  p_path2 IS INITIAL.
        MESSAGE e082(zfi01).
        CLEAR sscrfields-ucomm.
      ENDIF.
    else.  " If Presentation Selected
      IF p_path1 IS INITIAL.
        MESSAGE e082(zfi01).
        CLEAR sscrfields-ucomm.
      ENDIF.
    ENDIF.

    IF p_erpid is INITIAL.
      SET CURSOR FIELD 'P_ERPID'.
      MESSAGE e214(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.
  ENDIF.
