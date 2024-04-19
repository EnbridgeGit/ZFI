*&---------------------------------------------------------------------*
*& Report  ZFAPI112_OPEN_INVOICES
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI112_OPEN_INVOICES                        *
* Author             :   Paul Karunakar                                *
* Date               :   Feb 1, 2018                                   *
* Technical Contact  :   John Hartung                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Extract interface of Open and Parked          *
*                        Invoices to IAP                               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  KPAL        D30K928583  CHG0100815  Initial Development *
*                          D30K928768, D30K928884                      *
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS:       p_erpid  TYPE char05.
SELECT-OPTIONS:   s_bukrs  FOR  bsik-bukrs.
SELECT-OPTIONS:   s_lifnr  FOR  bsik-lifnr.
SELECT-OPTIONS:   s_gjahr  FOR  bsik-gjahr.
SELECT-OPTIONS:   s_belnr  FOR  bsik-belnr.
SELECT-OPTIONS:   s_bstat  FOR  bkpf-bstat.
PARAMETERS:       p_kurst  TYPE tcurv-kurst.
SELECTION-SCREEN: END   OF BLOCK blk1.

SELECTION-SCREEN: BEGIN OF BLOCK blk3 WITH FRAME TITLE text-002.
PARAMETERS:       p_appl   RADIOBUTTON GROUP rad2
                           DEFAULT 'X'
                           USER-COMMAND cmnd.
PARAMETERS:       p_pres   RADIOBUTTON GROUP rad2.
SELECTION-SCREEN: END   OF BLOCK blk3.

*------Presentation Server--
SELECTION-SCREEN: BEGIN OF BLOCK blk4 WITH FRAME TITLE text-003.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  01(11) text-p01
                           FOR FIELD p_path1
                           MODIF ID m1.
PARAMETERS:       p_path1  TYPE string
                           MODIF ID m1.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: END   OF BLOCK blk4.

*eject
*---Application Layer---*
SELECTION-SCREEN: BEGIN OF BLOCK blk5 WITH FRAME TITLE text-004.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  01(11) text-p01
                           FOR FIELD p_file2
                           MODIF ID m2.
PARAMETERS:       p_path2  TYPE string
                           MODIF ID m2
                           LOWER CASE.
PARAMETERS:       p_file2  TYPE string
                           MODIF ID m2.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: END   OF BLOCK blk5.

************************************************************************
* AT SELECTION-SCREEN Process Before OUTPUT
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

    IF screen-group1 = 'M1'.
      IF p_appl = space .
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*eject
AT SELECTION-SCREEN.

* TRANSLATE p_path2 TO UPPER CASE.
  IF sscrfields-ucomm = 'R3'.
    IF p_pres = abap_true.
      CLEAR p_path1.
    ELSE.
      CLEAR p_file2.
      CLEAR p_path2.
    ENDIF.
  ENDIF.

  IF sscrfields-ucomm = gc_ucomm_onli.

    IF p_appl EQ gc_x. " If Application Server Selected
*->To check File Name is entered or not
      IF p_file2 IS INITIAL.
        MESSAGE e081(zfi01).
        CLEAR sscrfields-ucomm.
      ENDIF.
*--> TP check Folder NAme Entered or not
      IF p_path2 IS INITIAL.
        MESSAGE e082(zfi01).
        CLEAR sscrfields-ucomm.
      ENDIF.
    ELSE.  " If Presentation Server Selected
      IF p_path1 IS INITIAL.
        MESSAGE e082(zfi01).
        CLEAR sscrfields-ucomm.
      ENDIF.
    ENDIF.

    IF p_erpid IS INITIAL.
      SET CURSOR FIELD 'P_ERPID'.
      MESSAGE e214(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    IF p_erpid IS INITIAL.
      SET CURSOR FIELD 'P_KURST'.
      MESSAGE e214(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

  ENDIF.
