*&---------------------------------------------------------------------*
*&  Include           ZFAPI122_OPEN_INVC_ITEMS_SCR
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI122_OPEN_INVC_ITEMS                       *
* Include Program    :  ZFAPI122_OPEN_INVC_ITEMS_SCR                   *
* Author             :  Paul Karunakar                                 *
* Creation Date      :  16-Apr-2018                                    *
* Application Area   :  FICO                                           *
* Description        :  Open Invoice data from each of the three SAP   *
*                       instances will be extracted in a delimited     *
*                       file and sent to IAP.                          *
*                                                                      *
*&---------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 16-Apr-2018  KPAL        D30K928646  CHG0108943  Initial Development *
*                          D30K928913, D30K929061                      *
*----------------------------------------------------------------------*

************************************************************************
*                           Selection Screen                           *
************************************************************************
* Select options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_erpid  TYPE char05.
SELECT-OPTIONS:   s_bukrs  FOR  bsik-bukrs.
SELECT-OPTIONS:   s_lifnr  FOR  bsik-lifnr.
SELECT-OPTIONS:   s_ktokk  FOR  lfa1-ktokk.
SELECT-OPTIONS:   s_gjahr  FOR  bsik-gjahr.
SELECT-OPTIONS:   s_belnr  FOR  bsik-belnr.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       cb_invco AS CHECKBOX
                           DEFAULT 'X'.
PARAMETERS:       cb_invcp AS CHECKBOX
                           DEFAULT 'X'.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       rb_fload RADIOBUTTON GROUP grp1
                           DEFAULT 'X'
                           USER-COMMAND cmd.
PARAMETERS:       rb_dload RADIOBUTTON GROUP grp1.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_date   FOR sy-datum
                           NO-EXTENSION
                           MODIF ID m3.
SELECTION-SCREEN: END   OF BLOCK blk1.

*eject
* Run options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       rb_appl  RADIOBUTTON GROUP rad2
                           DEFAULT 'X'
                           USER-COMMAND cmnd.
PARAMETERS:       rb_pres  RADIOBUTTON GROUP rad2.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  1(11) text-p01
                           FOR FIELD p_file1
                           MODIF ID M1.
PARAMETERS:       p_path1  TYPE string
                           MODIF ID M1.
PARAMETERS:       p_file1  TYPE string
                           MODIF ID M1
                           NO-DISPLAY.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  1(11) text-p01
                           FOR FIELD p_file2
                           MODIF ID M2.
PARAMETERS:       p_path2  TYPE string
                           LOWER CASE
                           MODIF ID M2.
PARAMETERS:       p_file2  TYPE string
                           MODIF ID M2.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_maxrec TYPE NUMC10 "Max Number Records per File
                           MODIF ID M2.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK blk2.

*eject
************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF screen-group1 = 'M3'.
      IF rb_fload = space.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 = 'M2'.
      IF rb_pres = space.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF screen-group1 = 'M1'.
      IF rb_appl = space.
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
    IF rb_pres = abap_true.
      CLEAR p_file1.
      CLEAR p_path1.
    ELSE.
      CLEAR p_file2.
      CLEAR p_path2.
    ENDIF.
  ENDIF.

  IF sscrfields-ucomm = gc_ucomm_onli.

    IF ( ( ( p_path1 IS INITIAL ) AND ( rb_pres EQ abap_true ) ) OR
         ( ( p_path2 IS INITIAL ) AND ( rb_appl EQ abap_true ) )    ).
      MESSAGE i082(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    IF p_erpid IS INITIAL.
      MESSAGE text-008 TYPE 'E'.
    ENDIF.

    IF ( ( rb_dload IS NOT INITIAL ) AND ( s_date IS INITIAL ) ).
      MESSAGE text-010 TYPE 'E'.
    ENDIF.

    IF rb_appl IS NOT INITIAL.
      IF p_path2 IS INITIAL.
        MESSAGE text-012 TYPE 'E'.
      ENDIF.
      IF p_file2 IS INITIAL.
        MESSAGE text-013 TYPE 'E'.
      ENDIF.
    ENDIF.

    CLEAR p_file1.

  ENDIF.
