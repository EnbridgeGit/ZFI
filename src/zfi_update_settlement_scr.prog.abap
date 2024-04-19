*&---------------------------------------------------------------------*
*&  Include           ZFI_UPDATE_SETTLEMENT_SCR
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
select-OPTIONS:s_PSPNR for PRPS-PSPNR." OBLIGATORY.
PARAMETERS:p_HKONT TYPE SAKNR,
           p_anln1 type cobrb-anln1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-000.
PARAMETERS:P_PERIOD TYPE COBRB-GABPE OBLIGATORY,
           P_YEAR   type COBRB-GBISJ OBLIGATORY.
PARAMETERS:P_PER_F TYPE COBRB-GABPE MODIF ID AS,
           P_YEAR_F   type COBRB-GBISJ MODIF ID AS.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-000.
PARAMETERS:
           R_NEW    RADIOBUTTON GROUP SGRP USER-COMMAND CMD DEFAULT 'X',
           R_SR     RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: R_EXIST  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN:COMMENT 5(35) text-110 FOR FIELD R_EXIST.
SELECTION-SCREEN:POSITION 60,COMMENT 40(20) text-111 FOR FIELD P_PERBZ.
PARAMETERS: P_PERBZ TYPE PERBZ.
SELECTION-SCREEN: END   OF LINE.
PARAMETERS:R_OPEN   RADIOBUTTON GROUP SGRP,
          R_AGE    RADIOBUTTON GROUP SGRP,
          R_DEL    RADIOBUTTON GROUP SGRP,
          R_WIP    RADIOBUTTON GROUP SGRP,
          R_DDATE  RADIOBUTTON GROUP SGRP,
          R_FXA    RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN END OF BLOCK b2.