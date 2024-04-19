*&---------------------------------------------------------------------*
*&  Include           ZXF05U01
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"             VALUE(I_LFB1) LIKE  LFB1 STRUCTURE  LFB1
*"             VALUE(I_LFM1) LIKE  LFM1 STRUCTURE  LFM1
*"             VALUE(I_ADDRHANDLE) LIKE  ADDR1_SEL-ADDRHANDLE
*"                             OPTIONAL
*"       TABLES
*"              T_LFBK STRUCTURE  LFBK OPTIONAL
*"              T_LFB5 STRUCTURE  LFB5 OPTIONAL
*"              T_LFZA STRUCTURE  LFZA OPTIONAL
*"              T_LFBW STRUCTURE  LFBW OPTIONAL
*"              T_LFAS STRUCTURE  LFAS OPTIONAL
*"              T_LFAT STRUCTURE  LFAT OPTIONAL
*"              T_LFLR STRUCTURE  LFLR OPTIONAL
*"              T_LFM2 STRUCTURE  LFM2 OPTIONAL
*"              T_WYT1 STRUCTURE  WYT1 OPTIONAL
*"              T_WYT1T STRUCTURE  WYT1T OPTIONAL
*"              T_WYT3 STRUCTURE  WYT3 OPTIONAL
*"----------------------------------------------------------------------


IF i_lfa1-ktokk <> i_lfa1-begru.
  MESSAGE ID 'Z1' TYPE 'I' NUMBER '50' DISPLAY LIKE 'E' WITH i_lfa1-ktokk i_lfa1-ktokk.
  PERFORM navigate_to_affected_screen(sapmf02k)
    USING '120' '' '' '' ''.
ENDIF.
