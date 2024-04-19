*----------------------------------------------------------------------*
***INCLUDE LZFI_TV_WORKFLOWO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  INIT_300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_300 OUTPUT.
  SET PF-STATUS 'POPUP3'.
  SET TITLEBAR '300' WITH POPUP-TITLE.
  CASE POPUP-DEFAULTOPTION.
    WHEN '1'.
      SET CURSOR FIELD 'popup-option1'.
    WHEN '2'.
      SET CURSOR FIELD 'popup-option2'.
  ENDCASE.
ENDMODULE.                 " INIT_300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OK_CODE_300 INPUT.
  CLEAR SAVE_OK_CODE.
  SAVE_OK_CODE = OK_CODE.
  CASE SAVE_OK_CODE.
    WHEN 'CANC'.
      ANTWORT = 'A'.
    WHEN 'OPT1'.
      ANTWORT = '1'.
    WHEN 'OPT2'.
      ANTWORT = '2'.
  ENDCASE.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDMODULE.                 " OK_CODE_300  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCREEN OUTPUT.
  clear gv_must.
  IF GV_twoline NE 'X' OR
    ( GV_twoline = 'X' AND gv_hide = 'X' ).
*  IF gv_sourcetext NE 'SELECT USER' OR
*    ( gv_sourcetext = 'SELECT USER' AND gv_hide = 'X' ).
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'GV_SOURCETEXT1' OR SCREEN-NAME = 'GV_TARGETTEXT1'.
        SCREEN-INVISIBLE = '1'.
        SCREEN-output = '0'.
        SCREEN-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    gv_must = 'X'.
  ENDIF.
ENDMODULE.                 " SET_SCREEN  OUTPUT
