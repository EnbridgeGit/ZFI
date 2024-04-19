*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_DELTA_SCR
*&---------------------------------------------------------------------*


PARAMETERS: p_ryear TYPE gjahr,
            p_period(2) TYPE n.

SELECT-OPTIONS: s_rldnr  FOR faglflext-rldnr,
                s_racct  FOR faglflext-racct,
                s_rbukrs FOR faglflext-rbukrs.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK exe WITH FRAME TITLE text-c03.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: cb_bclr AS CHECKBOX USER-COMMAND crc.
SELECTION-SCREEN COMMENT (45) text-cb2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: cb_clrd AS CHECKBOX USER-COMMAND crd.   "Delete Tracking Table for Desired Period
SELECTION-SCREEN COMMENT (45) text-cb3.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_ryr    TYPE gjahr MODIF ID crd.
PARAMETERS: p_prd(2) TYPE n MODIF ID crd.
SELECTION-SCREEN END OF BLOCK exe.

SELECTION-SCREEN BEGIN OF BLOCK fph WITH FRAME TITLE text-c01.
SELECT-OPTIONS: s_usco FOR t001-bukrs.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rd_pc  RADIOBUTTON GROUP rd4 USER-COMMAND rd4 MODIF ID rd4. " Generate PC file
SELECTION-SCREEN COMMENT (15) text-cb8   MODIF ID rd4.
PARAMETERS: rd_app RADIOBUTTON GROUP rd4 MODIF ID rd5 DEFAULT 'X'.      " Generate app server file
SELECTION-SCREEN COMMENT 21(25) text-cb7 MODIF ID rd5.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_file  TYPE localfile MODIF ID rd6.  " PC File Name
PARAMETERS: p_appf  TYPE localfile MODIF ID rd7.                " Application server File Name
PARAMETERS: p_errorf  TYPE localfile MODIF ID rd7.                " Application server Error file
PARAMETERS: p_logf  TYPE localfile MODIF ID rd7.                " Application server Log file
PARAMETERS: p_man   AS CHECKBOX MODIF ID rd7 USER-COMMAND man.  " Application server file path manual override
PARAMETERS: run_date TYPE datum DEFAULT sy-datum NO-DISPLAY.
PARAMETERS: run_time TYPE sy-uzeit DEFAULT sy-uzeit NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK fph.

AT SELECTION-SCREEN.

  PERFORM scrn_validation.

AT SELECTION-SCREEN OUTPUT.

  PERFORM modify_screen.

  IF rd_app EQ 'X' AND p_man EQ ''.
    PERFORM get_filename.
  ENDIF.
