*&---------------------------------------------------------------------*
*&  Include           ZFAGL_OI_TRIAL_BAL_MTD_SCR
*&---------------------------------------------------------------------*

PARAMETERS: P_RYEAR TYPE GJAHR,
            P_PERIOD(2) TYPE N.

SELECT-OPTIONS: S_RLDNR  FOR FAGLFLEXT-RLDNR,
                S_RACCT  FOR FAGLFLEXT-RACCT,
                S_RBUKRS FOR FAGLFLEXT-RBUKRS.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK FPH WITH FRAME TITLE TEXT-C01.
SELECT-OPTIONS: S_USCO FOR T001-BUKRS.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: RD_PC  RADIOBUTTON GROUP RD4 USER-COMMAND RD4 MODIF ID RD4. " Generate PC file
SELECTION-SCREEN COMMENT (15) TEXT-CB8   MODIF ID RD4.
PARAMETERS: RD_APP RADIOBUTTON GROUP RD4 MODIF ID RD5 DEFAULT 'X'.      " Generate app server file
SELECTION-SCREEN COMMENT 21(25) TEXT-CB7 MODIF ID RD5.
SELECTION-SCREEN END OF LINE.
PARAMETERS: P_FILE  TYPE LOCALFILE MODIF ID RD6.  " PC File Name
PARAMETERS: P_APPF  TYPE LOCALFILE MODIF ID RD7.                " Application server File Name
PARAMETERS: P_MAN   AS CHECKBOX MODIF ID RD7 USER-COMMAND MAN.  " Application server file path manual override
SELECTION-SCREEN END OF BLOCK FPH.

AT SELECTION-SCREEN OUTPUT.

  PERFORM MODIFY_SCREEN.

  IF RD_APP EQ 'X' AND P_MAN EQ ''.
    PERFORM GET_FILENAME.
  ENDIF.
