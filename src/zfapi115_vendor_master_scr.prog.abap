*&---------------------------------------------------------------------*
*& Report  ZFAPI115_VENDOR_MASTER_SCR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI115_VENDOR_MASTER                        *
* Include            :   ZFAPI115_VENDOR_MASTER_SCR                    *
* Author             :   Vijay Rajaputra                               *
* Date               :   03-Mar-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Vendor Master Extraction for IAP               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 03-Mar-2018  VRAJAPUTRA  D30K928789 CHG0105961 - Initial development *
*                          D30K928848                                  *
*&---------------------------------------------------------------------*

* Selection Screen Declaration

* Select options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE text-010.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_erpid  TYPE char05.
SELECT-OPTIONS:   s_lifnr  FOR lfa1-lifnr.
SELECT-OPTIONS:   s_ktokk  FOR lfa1-ktokk.
SELECT-OPTIONS:   s_bukrs  FOR lfb1-bukrs.
SELECT-OPTIONS:   s_ekorg  FOR lfm1-ekorg.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK blk1.

*eject
* Extract options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK blk2 WITH FRAME TITLE text-020.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 01.
PARAMETERS:       p_xfull  RADIOBUTTON GROUP rad3
                           DEFAULT 'X'
                           USER-COMMAND cmnd.
SELECTION-SCREEN: COMMENT  03(12) text-021.
SELECTION-SCREEN: POSITION 22.
PARAMETERS:       p_xdelta RADIOBUTTON GROUP rad3.
SELECTION-SCREEN: COMMENT  24(13) text-022.
SELECTION-SCREEN: POSITION 51.
SELECT-OPTIONS:   s_date   FOR sy-datum
                           MODIF ID S1.
SELECTION-SCREEN: COMMENT  39(12) text-025
                           MODIF ID S1.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: SKIP 1.
PARAMETER:        cb_file1 AS CHECKBOX
                              DEFAULT 'X'.
PARAMETER:        cb_file2 AS CHECKBOX
                              DEFAULT 'X'.
PARAMETER:        cb_file3 AS CHECKBOX
                              DEFAULT 'X'.
PARAMETER:        cb_file4 AS CHECKBOX
                              DEFAULT 'X'.
PARAMETER:        cb_file5 AS CHECKBOX
                              DEFAULT 'X'.
PARAMETER:        cb_file6 AS CHECKBOX
                              DEFAULT 'X'.
PARAMETER:        cb_file7 AS CHECKBOX
                              DEFAULT 'X'.
SELECTION-SCREEN SKIP 1.
PARAMETERS:       cb_inblk AS CHECKBOX
                           DEFAULT 'X'
                           MODIF ID S2.
PARAMETERS:       cb_indel AS CHECKBOX
                           DEFAULT 'X'
                           MODIF ID S2.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK blk2.

*eject
* File options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK blk3 WITH FRAME TITLE text-030.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: POSITION 1.
PARAMETERS:       p_appl   RADIOBUTTON GROUP rad2
                           DEFAULT 'X'
                           USER-COMMAND cmnd.
SELECTION-SCREEN: COMMENT  03(18) text-031.
SELECTION-SCREEN: POSITION 22.
PARAMETERS:       p_pres   RADIOBUTTON GROUP rad2.
SELECTION-SCREEN: COMMENT  24(19) text-032.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  01(08) text-035
                           FOR FIELD p_path1
                           MODIF ID M1.
PARAMETERS:       p_path1  TYPE string
                           MODIF ID M1
                           LOWER CASE.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT  01(16) text-036
                           FOR FIELD p_file2
                           MODIF ID M2.
PARAMETERS:       p_path2  TYPE string
                           MODIF ID M2
                           LOWER CASE.
PARAMETERS:       p_file2  TYPE string
                           MODIF ID M2
                           LOWER CASE.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_maxrec TYPE NUMC10 "Max Number Records per File
                           MODIF ID M2.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK blk3.

*eject
************************************************************************
* Process before output - Selection screen
************************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF ( screen-group1 EQ 'S1' ).
      IF ( p_xfull IS INITIAL ).
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF ( screen-group1 EQ 'S2' ).
      IF ( p_xfull IS INITIAL ).
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF ( screen-group1 EQ 'M2' ).
      IF ( p_appl IS NOT INITIAL ).
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    IF ( screen-group1 EQ 'M1' ).
      IF ( p_pres IS NOT INITIAL ).
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

*eject
AT SELECTION-SCREEN.

  IF ( sscrfields-ucomm EQ 'R3' ).
    IF ( p_pres IS NOT INITIAL ).
      CLEAR p_path1.
    ELSE.
      CLEAR p_file2.
      CLEAR p_path2.
    ENDIF.
  ENDIF.

  IF ( sscrfields-ucomm EQ gc_ucomm_onli ).

    IF ( p_appl IS NOT INITIAL ). " Application Server Selected

      IF ( p_file2 IS INITIAL ).
        MESSAGE e081(zfi01).
        CLEAR sscrfields-ucomm.
      ENDIF.

      IF ( p_path2 IS INITIAL ).
        MESSAGE e082(zfi01).
        CLEAR sscrfields-ucomm.
      ENDIF.

    ELSE.  " Presentation Server Selected

      IF ( p_path1 IS INITIAL ).
        MESSAGE e082(zfi01).
        CLEAR sscrfields-ucomm.
      ENDIF.

    ENDIF.

    IF ( p_erpid IS INITIAL ).
      SET CURSOR FIELD 'P_ERPID'.
      MESSAGE e214(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    IF ( p_xdelta IS NOT INITIAL ).
      IF ( s_date-low IS INITIAL ).
        SET CURSOR FIELD 'S_DATE-LOW'.
        MESSAGE text-091 TYPE 'E'.
        CLEAR sscrfields-ucomm.
      ENDIF.
    ENDIF.

    IF ( ( cb_file1 IS INITIAL ) AND ( cb_file2 IS INITIAL ) AND
         ( cb_file3 IS INITIAL ) AND ( cb_file4 IS INITIAL ) AND
         ( cb_file5 IS INITIAL ) AND ( cb_file6 IS INITIAL ) AND
         ( cb_file7 IS INITIAL )     ).
      MESSAGE text-092 TYPE 'E'.
    ENDIF.

  ENDIF.
