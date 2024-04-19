*&---------------------------------------------------------------------*
*&  Include           ZFAPI110_FIELD_STATUS_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECTION-SCREEN: SKIP 1.
PARAMETERS     : p_erpid TYPE char05.
SELECT-OPTIONS : s_bukrs FOR  t004f-bukrs.
SELECT-OPTIONS : s_fstag FOR  t004f-fstag.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN: BEGIN OF BLOCK blk3 WITH FRAME TITLE text-002.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_app    RADIOBUTTON GROUP rad2
                           DEFAULT 'X'
                           USER-COMMAND cmnd,
                  p_dis    RADIOBUTTON GROUP rad2.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK blk3.

* Presentation Server
SELECTION-SCREEN: BEGIN OF BLOCK blk4 WITH FRAME TITLE text-003.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(11) text-p01
                           FOR FIELD p_file1
                           MODIF ID m1.
PARAMETERS:       p_path1  TYPE string
                           MODIF ID m1.
PARAMETERS:       p_file1  TYPE string
                           MODIF ID m1
                           NO-DISPLAY.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK blk4.

* Application Server
SELECTION-SCREEN: BEGIN OF BLOCK blk5 WITH FRAME TITLE text-004.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(11) text-p01
                           FOR FIELD p_file2
                           MODIF ID m2.
PARAMETERS:       p_path2  TYPE text128
                           MODIF ID m2.
PARAMETERS:       p_file2  TYPE string
                           MODIF ID m2.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: END   OF BLOCK blk5.

AT SELECTION-SCREEN OUTPUT .

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

  ENDLOOP .

AT SELECTION-SCREEN.

* TRANSLATE p_path2 TO UPPER CASE.
  IF sscrfields-ucomm = 'R3'.
    IF p_dis = abap_true.
      CLEAR p_file1.
      CLEAR p_path1.
    ELSE.
      CLEAR p_file2.
      CLEAR p_path2.
    ENDIF.
  ENDIF.

  IF sscrfields-ucomm = gc_ucomm_onli.

    IF ( p_path1 IS INITIAL AND
         p_dis EQ abap_true ) OR
        ( p_path2 IS INITIAL AND
        p_app EQ abap_true ).
      MESSAGE i082(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    IF p_erpid IS INITIAL .
      MESSAGE text-008 TYPE 'E'  .
    ENDIF .

    IF p_app IS NOT INITIAL .
      IF p_path2 IS INITIAL .
        MESSAGE text-012 TYPE 'E'  .
      ENDIF .
      IF p_file2 IS INITIAL .
        MESSAGE text-013 TYPE 'E' .
      ENDIF .
    ENDIF .

    CLEAR p_file1.

  ENDIF.
