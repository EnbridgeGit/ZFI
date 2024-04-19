*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_AHCS_SCR
*&---------------------------------------------------------------------*

PARAMETERS: p_ryear TYPE gjahr.

SELECT-OPTIONS: s_rpmax  FOR faglflext-rpmax,
                s_rldnr  FOR faglflext-rldnr,
                s_racct  FOR faglflext-racct,
                s_rbukrs FOR faglflext-rbukrs.

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
PARAMETERS: p_man   AS CHECKBOX MODIF ID rd7 USER-COMMAND man.  " Application server file path manual override
SELECTION-SCREEN END OF BLOCK fph.

AT SELECTION-SCREEN OUTPUT.

  PERFORM modify_screen.

  IF rd_app EQ 'X' AND p_man EQ ''.
    PERFORM get_filename.
  ENDIF.
