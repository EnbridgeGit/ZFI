*&---------------------------------------------------------------------*
*&  Include           ZFAPI104_INTERNAL_ORDER_SCR
*&---------------------------------------------------------------------*
************************************************************************
*                            Enbridge Energy                           *
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI104_INTERNAL_ORDER                       *
*& Author             :  Kalinga Keshari Rout                          *
*& Creation Date      :  January 09, 2018                              *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Program extracts master data for both cases   *
*&                       full load and delta load and file created in  *
*&                       application server  .Frequency of data upload *
*&                        weekly                                       *
*&---------------------------------------------------------------------*
*                         Modification Log                             *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* -------------------------------------------------------------------- *
* 19-Jan-2018  KROUT       D30K928564  CHG0100808  Initial Development *
*                          D30K928834, D30K928890                      *
*----------------------------------------------------------------------*

TABLES : sscrfields .


SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.

PARAMETERS : p_erpid TYPE char05.
SELECTION-SCREEN END OF BLOCK blk1.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS :
                  s_aufnr FOR gs_aufk-aufnr,
                  s_autyp FOR gv_autyp.
PARAMETERS: rb_fload RADIOBUTTON GROUP grp1
                    USER-COMMAND cmd
                    DEFAULT 'X'.
PARAMETERS: rb_dload RADIOBUTTON GROUP grp1.
SELECT-OPTIONS : s_date FOR  sy-datum NO-EXTENSION MODIF ID m3 .

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-004.
PARAMETERS : p_app   RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmnd,
             p_dis   RADIOBUTTON GROUP rad2.
SELECTION-SCREEN END OF BLOCK blk3.

*------Presentation Server--
SELECTION-SCREEN BEGIN OF BLOCK blk4 WITH FRAME TITLE text-t10.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11) text-p01 FOR FIELD p_file1 MODIF ID m1.
PARAMETERS:p_path1 TYPE string MODIF ID m1 .
PARAMETERS:p_file1 TYPE string MODIF ID m1 NO-DISPLAY.
SELECTION-SCREEN : END OF LINE.

SELECTION-SCREEN END OF BLOCK blk4.


*---Application Layer---*
SELECTION-SCREEN BEGIN OF BLOCK blk5 WITH FRAME TITLE text-011.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(11) text-p01 FOR FIELD p_file2 MODIF ID m2.
PARAMETERS:p_path2 TYPE string MODIF ID m2 LOWER CASE.
PARAMETERS:p_file2 TYPE string MODIF ID m2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_maxrec TYPE NUMC10 "Max Number Records per File
                           MODIF ID M2.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN END OF BLOCK blk5.

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

*  TRANSLATE p_path2 TO UPPER CASE.
  IF sscrfields-ucomm = 'R3'.
    IF p_dis = abap_true.
      CLEAR p_file1.
      CLEAR p_path1.
    ELSE.
      CLEAR p_file2.
      CLEAR p_path2.
    ENDIF.
  ENDIF.
*  IF p_test NE abap_true.
  IF sscrfields-ucomm = gc_ucomm_onli.


    IF ( p_path1 IS INITIAL AND
         p_dis EQ abap_true ) OR
        ( p_path2 IS INITIAL AND
        p_app EQ abap_true ).
      MESSAGE i082(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.

    if p_erpid is initial .
      MESSAGE text-008 type 'E'  .

    endif .

    if rb_dload is not initial and s_date is initial . "and gv_chk is not initial .
      message text-010 type 'E' .
    endif .

    if p_app is not initial .
      if p_path2 is initial .
        message text-012 type 'E'  .
      endif .
      if p_file2 is initial .
        message text-013 type 'E' .
      endif .
    endif .
    CLEAR p_file1.

  ENDIF.
