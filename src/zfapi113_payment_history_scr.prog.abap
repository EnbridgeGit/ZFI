*&---------------------------------------------------------------------*
*&  Include           ZFAPI113_PAYMENT_HISTORY_SCR
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :  ZFAPI113_PAYMENT_HISTORY                       *
* Include            "  ZFAPI113_PAYMENT_HISTORY_SCR                   *
* Author             :  Kalinga Keshari Rout                           *
* Creation Date      :  February 01, 2018                              *
* Object ID          :                                                 *
* Application Area   :  FICO                                           *
* Description        :  The Payment History interface lists all paid   *
*                       invoices with their corresponding payment      *
*                       information.  It includes all payments that    *
*                       have been processed from Checks, EFT Payments, *
*                       Wire Transfers or any other form of payment.   *
*                       The Payment History file should include one    *
*                       record per invoice AND payment.  Multiple      *
*                       invoices paid on the same payment should have  *
*                       one record for each invoice.  Multiple         *
*                       payments for the same invoice should have one  *
*                       record for each payment.                       *
*----------------------------------------------------------------------*
*                       Modification Log                               *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 09-Apr-2018  KROUT       D30K928652  CHG0100818#Initial development  *
*                          D30K928716, D30K928741, D30K928766,         *
*                          D30K928774, D30K928886                      *
*----------------------------------------------------------------------*

*eject
TABLES : sscrfields, bsak.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-000.
PARAMETERS : p_erpid TYPE char05   OBLIGATORY.
SELECT-OPTIONS : s_bukrs  FOR  bsak-bukrs,
                 s_lifnr  FOR  bsak-lifnr,
                 s_augdt  FOR  bsak-augdt,
                 s_augbl  FOR  bsak-augbl,
                 s_gjahr  FOR  bsak-gjahr,
                 s_belnr  FOR  bsak-belnr,
                 s_cpudt  FOR  bsak-cpudt.
PARAMETERS       p_kurst  TYPE kurst_curr.
SELECTION-SCREEN END   OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-004.
PARAMETERS : p_app   RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmnd ,
             p_dis   RADIOBUTTON GROUP rad2.
*            p_fpath TYPE rlgrap-filename NO-DISPLAY.
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
SELECTION-SCREEN END OF BLOCK blk5.

*eject
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF screen-group1 = 'M3'.
*      IF rb_fload = space.
*        screen-active = 1.
*      ELSE.
*        screen-active = 0.
*      ENDIF.
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

*eject
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

*  IF p_test NE abap_true.
  IF sscrfields-ucomm = gc_ucomm_onli.

    IF ( p_path1 IS INITIAL AND
         p_dis EQ abap_true ) OR
        ( p_path2 IS INITIAL AND
        p_app EQ abap_true ).
      MESSAGE i082(zfi01).
      CLEAR sscrfields-ucomm.
    ENDIF.
    IF p_erpid IS INITIAL.
      MESSAGE text-008 type 'E'  .
    ENDIF.

    IF p_app IS NOT INITIAL .
      IF p_path2 IS INITIAL .
        MESSAGE text-012 TYPE 'E'  .
      ENDIF .
      IF p_file2 IS INITIAL .
        MESSAGE text-013 TYPE 'E' .
      ENDIF .
    ENDIF .

*      CLEAR p_file4.
    CLEAR p_file1.

  ENDIF.
