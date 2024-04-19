*&---------------------------------------------------------------------*
*& Module Pool       ZFAPI57_ZFI_FGLASS_TRANS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

PROGRAM  ZFAPI57_ZFI_FGLASS_TRANS.

***&SPWIZARD: DATA DECLARATION FOR TABLECONTROL 'ZFI_FGLASS_TRAN'
*&SPWIZARD: DEFINITION OF DDIC-TABLE
TABLES:   ZFI_FGLASS_TRANS.

*&SPWIZARD: TYPE FOR THE DATA OF TABLECONTROL 'ZFI_FGLASS_TRAN'
TYPES: BEGIN OF T_ZFI_FGLASS_TRAN,
         BUKRS LIKE ZFI_FGLASS_TRANS-BUKRS,
         BLDAT LIKE ZFI_FGLASS_TRANS-BLDAT,
         BUDAT LIKE ZFI_FGLASS_TRANS-BUDAT,
         WAERS LIKE ZFI_FGLASS_TRANS-WAERS,
         XBLNR LIKE ZFI_FGLASS_TRANS-XBLNR,
         BKTXT LIKE ZFI_FGLASS_TRANS-BKTXT,
         CDATE LIKE ZFI_FGLASS_TRANS-CDATE,
         CTIME LIKE ZFI_FGLASS_TRANS-CTIME,
         BUZEI LIKE ZFI_FGLASS_TRANS-BUZEI,
         WRBTR TYPE ZFI_FGLASS_TRANS-WRBTR,
         SGTXT LIKE ZFI_FGLASS_TRANS-SGTXT,
         LIFNR LIKE ZFI_FGLASS_TRANS-LIFNR,
         MENGE LIKE ZFI_FGLASS_TRANS-MENGE,
         MEINS LIKE ZFI_FGLASS_TRANS-MEINS,
         TAX_AMOUNT LIKE ZFI_FGLASS_TRANS-TAX_AMOUNT,
         TAX_CODE LIKE ZFI_FGLASS_TRANS-TAX_CODE,
         INVTYPE1 LIKE ZFI_FGLASS_TRANS-INVTYPE1,
         INVTYPE2 LIKE ZFI_FGLASS_TRANS-INVTYPE2,
         LINETYPE LIKE ZFI_FGLASS_TRANS-LINETYPE,
         PERSA LIKE ZFI_FGLASS_TRANS-PERSA,
         ECC_ID LIKE ZFI_FGLASS_TRANS-ECC_ID,
         VORNR LIKE ZFI_FGLASS_TRANS-VORNR,
         PROJK24 LIKE ZFI_FGLASS_TRANS-PROJK24,
         NPLNR LIKE ZFI_FGLASS_TRANS-NPLNR,
         AUFNR LIKE ZFI_FGLASS_TRANS-AUFNR,
         KOSTL LIKE ZFI_FGLASS_TRANS-KOSTL,
         SAKNR LIKE ZFI_FGLASS_TRANS-SAKNR,
         FNAME LIKE ZFI_FGLASS_TRANS-FNAME,
         LNAME LIKE ZFI_FGLASS_TRANS-LNAME,
         USNAM LIKE ZFI_FGLASS_TRANS-USNAM,
         WHTAX LIKE ZFI_FGLASS_TRANS-WHTAX,
       END OF T_ZFI_FGLASS_TRAN.

*&SPWIZARD: INTERNAL TABLE FOR TABLECONTROL 'ZFI_FGLASS_TRAN'
DATA:     G_ZFI_FGLASS_TRAN_ITAB   TYPE T_ZFI_FGLASS_TRAN OCCURS 0,
          G_ZFI_FGLASS_TRAN_WA     TYPE T_ZFI_FGLASS_TRAN. "work area
DATA:     G_ZFI_FGLASS_TRAN_COPIED.           "copy flag

*&SPWIZARD: DECLARATION OF TABLECONTROL 'ZFI_FGLASS_TRAN' ITSELF
CONTROLS: ZFI_FGLASS_TRAN TYPE TABLEVIEW USING SCREEN 0100.

*&SPWIZARD: LINES OF TABLECONTROL 'ZFI_FGLASS_TRAN'
DATA:     G_ZFI_FGLASS_TRAN_LINES  LIKE SY-LOOPC.

DATA:     OK_CODE LIKE SY-UCOMM.

*&SPWIZARD: OUTPUT MODULE FOR TC 'ZFI_FGLASS_TRAN'. DO NOT CHANGE THIS L
*&SPWIZARD: COPY DDIC-TABLE TO ITAB
MODULE ZFI_FGLASS_TRAN_INIT OUTPUT.
  IF G_ZFI_FGLASS_TRAN_COPIED IS INITIAL.
*&SPWIZARD: COPY DDIC-TABLE 'ZFI_FGLASS_TRANS'
*&SPWIZARD: INTO INTERNAL TABLE 'g_ZFI_FGLASS_TRAN_itab'
    SELECT * FROM ZFI_FGLASS_TRANS
       INTO CORRESPONDING FIELDS
       OF TABLE G_ZFI_FGLASS_TRAN_ITAB.
    sort G_ZFI_FGLASS_TRAN_ITAB by xblnr bukrs.
    G_ZFI_FGLASS_TRAN_COPIED = 'X'.
    REFRESH CONTROL 'ZFI_FGLASS_TRAN' FROM SCREEN '0100'.
  ENDIF.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'ZFI_FGLASS_TRAN'. DO NOT CHANGE THIS L
*&SPWIZARD: MOVE ITAB TO DYNPRO
MODULE ZFI_FGLASS_TRAN_MOVE OUTPUT.
*  IF G_ZFI_FGLASS_TRAN_WA-TAX_AMOUNT < 0.
*     G_ZFI_FGLASS_TRAN_WA-TAX_AMOUNT = G_ZFI_FGLASS_TRAN_WA-TAX_AMOUNT
*                                       * -1.
*  ENDIF.
*  IF G_ZFI_FGLASS_TRAN_WA-MENGE < 0.
*     G_ZFI_FGLASS_TRAN_WA-MENGE = G_ZFI_FGLASS_TRAN_WA-MENGE
*                                       * -1.
*  ENDIF.
*  IF G_ZFI_FGLASS_TRAN_WA-WRBTR < 0.
*     G_ZFI_FGLASS_TRAN_WA-WRBTR = G_ZFI_FGLASS_TRAN_WA-WRBTR
*                                       * -1.
*  ENDIF.
  MOVE-CORRESPONDING G_ZFI_FGLASS_TRAN_WA TO ZFI_FGLASS_TRANS.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TC 'ZFI_FGLASS_TRAN'. DO NOT CHANGE THIS L
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE ZFI_FGLASS_TRAN_GET_LINES OUTPUT.
  G_ZFI_FGLASS_TRAN_LINES = SY-LOOPC.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'ZFI_FGLASS_TRAN'. DO NOT CHANGE THIS LI
*&SPWIZARD: MODIFY TABLE
MODULE ZFI_FGLASS_TRAN_MODIFY INPUT.

  MOVE-CORRESPONDING zfi_fglass_trans TO g_zfi_fglass_tran_wa.
  g_zfi_fglass_tran_wa-usnam = sy-uname.
  MODIFY g_zfi_fglass_tran_itab
    FROM g_zfi_fglass_tran_wa
    INDEX zfi_fglass_tran-current_line
**-- start of changes by akmadasu CHG0127237
*    TRANSPORTING
     TRANSPORTING lifnr
**-- end of changes by akmadasu CHG0127237
    vornr projk24 nplnr aufnr kostl saknr usnam.
ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TC 'ZFI_FGLASS_TRAN'. DO NOT CHANGE THIS LI
*&SPWIZARD: PROCESS USER COMMAND
MODULE ZFI_FGLASS_TRAN_USER_COMMAND INPUT.
  OK_CODE = SY-UCOMM.
  PERFORM USER_OK_TC USING    'ZFI_FGLASS_TRAN'
                              'G_ZFI_FGLASS_TRAN_ITAB'
                              'FLAG'
                     CHANGING OK_CODE.
  SY-UCOMM = OK_CODE.
ENDMODULE.

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                          P_TABLE_NAME
                          P_MARK_NAME
                 CHANGING P_OK      LIKE SY-UCOMM.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA: L_OK              TYPE SY-UCOMM,
         L_OFFSET          TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
   SEARCH P_OK FOR P_TC_NAME.
   IF SY-SUBRC <> 0.
     EXIT.
   ENDIF.
   L_OFFSET = STRLEN( P_TC_NAME ) + 1.
   L_OK = P_OK+L_OFFSET.
*&SPWIZARD: execute general and TC specific operations                 *
   CASE L_OK.
     WHEN 'INSR'.                      "insert row
       PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                         P_TABLE_NAME.
       CLEAR P_OK.

     WHEN 'DELE'.                      "delete row
       PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME.
       CLEAR P_OK.

     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                             L_OK.
       CLEAR P_OK.

     WHEN 'MARK'.                      "mark all filled lines
       PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME   .
       CLEAR P_OK.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                           P_TABLE_NAME
                                           P_MARK_NAME .
       CLEAR P_OK.


   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_insert_row
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_LINES_NAME       LIKE FELD-NAME.
   DATA L_SELLINE          LIKE SY-STEPL.
   DATA L_LASTLINE         TYPE I.
   DATA L_LINE             TYPE I.
   DATA L_TABLE_NAME       LIKE FELD-NAME.
   FIELD-SYMBOLS <TC>                 TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>              TYPE STANDARD TABLE.
   FIELD-SYMBOLS <LINES>              TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_LINES_NAME.
   ASSIGN (L_LINES_NAME) TO <LINES>.

*&SPWIZARD: get current line                                           *
   GET CURSOR LINE L_SELLINE.
   IF SY-SUBRC <> 0.                   " append line to table
     L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line                                               *
     IF L_SELLINE > <LINES>.
       <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
     ELSE.
       <TC>-TOP_LINE = 1.
     ENDIF.
   ELSE.                               " insert line into table
     L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
     L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
   ENDIF.
*&SPWIZARD: set new cursor line                                        *
   L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.

*&SPWIZARD: insert initial line                                        *
   INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
   <TC>-LINES = <TC>-LINES + 1.
*&SPWIZARD: set cursor                                                 *
   SET CURSOR LINE L_LINE.

 ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM fcode_delete_row
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME
                        P_MARK_NAME   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TABLE_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <WA>.
   FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
   DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

   LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     IF <MARK_FIELD> = 'X'.
       DELETE <TABLE> INDEX SYST-TABIX.
       IF SY-SUBRC = 0.
         <TC>-LINES = <TC>-LINES - 1.
       ENDIF.
     ENDIF.
   ENDLOOP.

 ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                       P_OK.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
   DATA L_TC_NEW_TOP_LINE     TYPE I.
   DATA L_TC_NAME             LIKE FELD-NAME.
   DATA L_TC_LINES_NAME       LIKE FELD-NAME.
   DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <LINES>      TYPE I.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.
*&SPWIZARD: get looplines of TableControl                              *
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
   ASSIGN (L_TC_LINES_NAME) TO <LINES>.


*&SPWIZARD: is no line filled?                                         *
   IF <TC>-LINES = 0.
*&SPWIZARD: yes, ...                                                   *
     L_TC_NEW_TOP_LINE = 1.
   ELSE.
*&SPWIZARD: no, ...                                                    *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
          EXPORTING
               ENTRY_ACT             = <TC>-TOP_LINE
               ENTRY_FROM            = 1
               ENTRY_TO              = <TC>-LINES
               LAST_PAGE_FULL        = 'X'
               LOOPS                 = <LINES>
               OK_CODE               = P_OK
               OVERLAPPING           = 'X'
          IMPORTING
               ENTRY_NEW             = L_TC_NEW_TOP_LINE
          EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
               OTHERS                = 0.
   ENDIF.

*&SPWIZARD: get actual tc and column                                   *
   GET CURSOR FIELD L_TC_FIELD_NAME
              AREA  L_TC_NAME.

   IF SYST-SUBRC = 0.
     IF L_TC_NAME = P_TC_NAME.
*&SPWIZARD: et actual column                                           *
       SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
     ENDIF.
   ENDIF.

*&SPWIZARD: set the new top line                                       *
   <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

*&SPWIZARD: get the table, which belongs to the tc                     *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <TABLE> ASSIGNING <WA>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_COST_OBJECTS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VALIDATE_COST_OBJECTS INPUT.
 DATA: lv_projk TYPE bseg-projk,
       lv_lifnr type lifnr," added by akmadasu CHG0127237
        lv_error TYPE xfeld,
        ls_lfa1 type lfa1," added by akmadasu CHG0127237
        ls_prps TYPE prps,
        ls_csks TYPE csks,
        ls_aufk TYPE aufk,
        ls_afko TYPE afko,
        ls_afvc TYPE afvc,
        ls_ska1 TYPE ska1.

  IF sy-ucomm = 'SAVE'. " or
*   sy-ucomm = space.
    IF zfi_fglass_trans-nplnr IS INITIAL AND
       zfi_fglass_trans-vornr IS NOT INITIAL.
      MESSAGE e000(zfi01) WITH 'Activity requires Network id also '
      '' ''.
    ENDIF.
    IF zfi_fglass_trans-projk24 IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          input     = zfi_fglass_trans-projk24
        IMPORTING
          output    = lv_projk
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
*  Implement suitable error handling here
      ENDIF.
      SELECT SINGLE * FROM prps INTO ls_prps WHERE pspnr = lv_projk.
      IF sy-subrc <> 0.
        MESSAGE e000(zfi01) WITH 'Wrong WBS ' zfi_fglass_trans-projk24
        '' ''.
      ENDIF.
    ENDIF.
**-- start of changes by akmadasu CHG0127237
        IF zfi_fglass_trans-lifnr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input     = zfi_fglass_trans-lifnr
        IMPORTING
          output    = lv_lifnr
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
*  Implement suitable error handling here
      ENDIF.
      SELECT SINGLE * FROM lfa1 INTO ls_lfa1 WHERE lifnr = lv_lifnr.
      IF sy-subrc <> 0.
        MESSAGE e000(zfi01) WITH 'Invalid Vendor' zfi_fglass_trans-lifnr
        '' ''.
      ENDIF.
    ENDIF.
**-- end of changes by akmadasu CHG0127237
    IF zfi_fglass_trans-kostl IS NOT INITIAL.
      PERFORM conversion_routine CHANGING zfi_fglass_trans-kostl.
      SELECT SINGLE * FROM csks INTO ls_csks
                     WHERE kostl = zfi_fglass_trans-kostl.
      IF sy-subrc <> 0.
        MESSAGE e000(zfi01) WITH 'Wrong WBS ' zfi_fglass_trans-kostl
        '' ''.
      ENDIF.
    ENDIF.
    IF zfi_fglass_trans-aufnr IS NOT INITIAL.
      PERFORM conversion_routine CHANGING zfi_fglass_trans-aufnr.
      SELECT SINGLE * FROM aufk INTO ls_aufk
                          WHERE aufnr = zfi_fglass_trans-aufnr.
      IF sy-subrc <> 0.
        MESSAGE e000(zfi01) WITH 'Wrong Order ' zfi_fglass_trans-kostl
        '' ''.
      ENDIF.
    ENDIF.
    IF zfi_fglass_trans-nplnr IS NOT INITIAL.
      PERFORM conversion_routine CHANGING zfi_fglass_trans-nplnr.
      SELECT SINGLE * FROM aufk INTO ls_aufk
        WHERE aufnr = zfi_fglass_trans-nplnr
          AND autyp = '20'. "networ.
      IF sy-subrc <> 0.
        MESSAGE e000(zfi01) WITH 'Wrong Network '
        zfi_fglass_trans-nplnr '' ''.
      ENDIF.
    ENDIF.
    IF zfi_fglass_trans-vornr IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_NUMCV_INPUT'
        EXPORTING
          input  = zfi_fglass_trans-vornr
        IMPORTING
          output = zfi_fglass_trans-vornr.
      SELECT SINGLE * FROM afko INTO ls_afko
        WHERE aufnr = zfi_fglass_trans-nplnr.
      IF ls_afko-aufpl IS NOT INITIAL.
        SELECT SINGLE * FROM afvc INTO ls_afvc
          WHERE aufpl = ls_afko-aufpl
            AND vornr = zfi_fglass_trans-vornr.
        IF sy-subrc <> 0.
          MESSAGE e000(zfi01) WITH 'Wrong Operation '
          zfi_fglass_trans-vornr '' ''.
        ENDIF.
      ELSE.
        MESSAGE e000(zfi01) WITH 'Wrong Operation '
        zfi_fglass_trans-vornr '' ''.
      ENDIF.
    ENDIF.
    IF zfi_fglass_trans-saknr is INITIAL.
       MESSAGE e000(zfi01) WITH 'Blank GL code is not allowed '
        zfi_fglass_trans-SAKNR '' ''.
    else.
       CLEAR ls_ska1.
       SELECT SINGLE * from ska1 INTO ls_ska1 WHERE saknr = zfi_fglass_trans-saknr.
       IF sy-subrc <> 0.
          MESSAGE e000(zfi01) WITH 'Wrong GL code.'
          zfi_fglass_trans-SAKNR '' ''.
       ENDIF.
    ENDIF.

  ENDIF.
ENDMODULE.                 " VALIDATE_COST_OBJECTS  INPUT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_ROUTINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM conversion_routine  CHANGING iv_var TYPE clike.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = iv_var
    IMPORTING
      output = iv_var.

ENDFORM.                    " CONVERSION_ROUTINE
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
 DATA: lv_okcode TYPE sy-ucomm.

  lv_okcode = ok_code.
  CASE lv_okcode.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'EXIT' OR
         'CANCEL' OR
         'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SAVE_DATA .
 DATA: ls_table TYPE zfi_fglass_trans,
        lt_table TYPE TABLE OF zfi_fglass_trans.

  DELETE FROM zfi_fglass_trans.
  COMMIT WORK AND WAIT.
  LOOP AT g_zfi_fglass_tran_itab INTO g_zfi_fglass_tran_wa.
    MOVE-CORRESPONDING g_zfi_fglass_tran_wa
    TO ls_table.
    MODIFY zfi_fglass_trans FROM ls_table.
    IF sy-subrc <> 0.

    ENDIF.
  ENDLOOP.
* DELETE FROM zfi_fglass_trans.
* COMMIT WORK.
* MODIFY zfi_fglass_trans FROM TABLE lt_table.
  COMMIT WORK AND WAIT.
  MESSAGE i000(zfi01) WITH 'TABLE: ZFI_FGLASS_TRANS has been updated.'
                           '' '' ''.
*   & & & & &
  "set SCREEN 0.
  " LEAVE SCREEN.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
SET PF-STATUS 'ZFAPI057'.
SET TITLEBAR 'T01'.

ENDMODULE.                 " STATUS_0100  OUTPUT
