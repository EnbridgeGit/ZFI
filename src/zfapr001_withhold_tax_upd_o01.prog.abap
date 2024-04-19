*&---------------------------------------------------------------------*
*&  Include           ZFAPR001_WITHHOLD_TAX_UPD_O01
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  CLEAR : OK_CODE.

  SET PF-STATUS 'SCREEN_100'.
  SET TITLEBAR  '0100'.
  GV_REPID = SY-REPID.

  IF GV_CONTAINER IS INITIAL.
*   create control container
    CREATE OBJECT GV_CONTAINER
      EXPORTING
        CONTAINER_NAME = 'TEXTEDITOR'
      EXCEPTIONS
        OTHERS         = 1.
    IF SY-SUBRC NE 0.
*    add your handling
    ENDIF.


* create calls constructor, which initializes, creats and links
* TextEdit Control
    CREATE OBJECT GV_EDITOR
      EXPORTING
        PARENT                     = GV_CONTAINER
        WORDWRAP_MODE              = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
        WORDWRAP_POSITION          = GC_LINE_LENGTH
        WORDWRAP_TO_LINEBREAK_MODE = CL_GUI_TEXTEDIT=>TRUE.

* set the new readonly_mode
    CALL METHOD GV_EDITOR->SET_READONLY_MODE
      EXPORTING
        READONLY_MODE = GV_EDITOR->TRUE.

* TO HIDE THE TOOLBAR
    CALL METHOD GV_EDITOR->SET_TOOLBAR_MODE
      EXPORTING
        TOOLBAR_MODE           = 0
      EXCEPTIONS
        ERROR_CNTL_CALL_METHOD = 1
        INVALID_PARAMETER      = 2
        OTHERS                 = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

  REFRESH : GIT_TESTTABLE.

  GWA_TEXTSTRUCT-LINE = 'Vendor master changed from not liable to liable :-'(024).
  APPEND GWA_TEXTSTRUCT TO GIT_TESTTABLE.

  GWA_TEXTSTRUCT-LINE = ''(027).
  APPEND GWA_TEXTSTRUCT TO GIT_TESTTABLE.

  GWA_TEXTSTRUCT-LINE = ' 1. Run RFWT0010 to change parked invoices'(025).
  APPEND GWA_TEXTSTRUCT TO GIT_TESTTABLE.

  GWA_TEXTSTRUCT-LINE = ' 2. Run RFWT0020 to change open & cleared invoices'(026).
  APPEND GWA_TEXTSTRUCT TO GIT_TESTTABLE.

  GWA_TEXTSTRUCT-LINE = ''(027).
  APPEND GWA_TEXTSTRUCT TO GIT_TESTTABLE.

  GWA_TEXTSTRUCT-LINE = 'Vendor master changed from liable to not liable :-'(028).
  APPEND GWA_TEXTSTRUCT TO GIT_TESTTABLE.

  GWA_TEXTSTRUCT-LINE = ''(027).
  APPEND GWA_TEXTSTRUCT TO GIT_TESTTABLE.

  GWA_TEXTSTRUCT-LINE = ' 1. Run RFWT0010 to change parked invoices & open invoices'(029).
  APPEND GWA_TEXTSTRUCT TO GIT_TESTTABLE.

  GWA_TEXTSTRUCT-LINE = ' 2. Run custom program to change cleared invoices'(030).
  APPEND GWA_TEXTSTRUCT TO GIT_TESTTABLE.

* insert table into control
  CALL METHOD GV_EDITOR->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE = GIT_TESTTABLE.

ENDMODULE.                 " STATUS_0100  OUTPUT
