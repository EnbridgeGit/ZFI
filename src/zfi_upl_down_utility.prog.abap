*&---------------------------------------------------------------------*
*& Report  ZFI_UPLOAD_XREF_DATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfi_upl_down_utility.
*>Type pools
TYPE-POOLS: abap.

*>Field Symbols
FIELD-SYMBOLS <f_fs> TYPE table.
FIELD-SYMBOLS <f_fs2> TYPE ANY.
FIELD-SYMBOLS <f_field> TYPE ANY.

*>Constants
CONSTANTS c_temp(10) VALUE 'C:\TEMP\'.

*>Types
TYPES tabname LIKE dcobjdef-name .

TYPES : BEGIN OF ty_output,
 data(5000).
TYPES END OF ty_output.

TYPES : BEGIN OF ty_req,
 fieldname TYPE fieldname,
 position  TYPE tabfdpos.
TYPES END OF ty_req.

TYPES : BEGIN OF ty_temp,
col TYPE tabfdpos.
TYPES : END OF ty_temp.
types: BEGIN OF ty_ZFI_ALSMEX_TABLINE,
       ROW    Type  KCD_EX_ROW_N,
       COL    Type  KCD_EX_COL_N,
       VALUE  Type  CHAR256,
      END OF ty_ZFI_ALSMEX_TABLINE.

*>Workarea
DATA w_output   TYPE ty_output.
DATA w_excep    TYPE ty_output.
* Begin of changes for EMEA Wave 0
*DATA w_input    TYPE alsmex_tabline.
DATA w_input    TYPE ty_zfi_alsmex_tabline.
* Begin of changes for EMEA Wave 0

DATA d_ref      TYPE REF TO data.
DATA st_dd02v_wa TYPE dd02v.

*>Internal Tables
DATA: BEGIN OF t_dbtab OCCURS 0.
        INCLUDE STRUCTURE dntab.
DATA: END OF t_dbtab.
* Begin of changes for EMEA Wave 0
*DATA t_input        TYPE STANDARD TABLE OF alsmex_tabline.
DATA t_input        TYPE STANDARD TABLE OF ty_zfi_alsmex_tabline.
* End of changes for EMEA Wave 0
DATA t_excep        TYPE STANDARD TABLE OF ty_output.
DATA t_output       TYPE STANDARD TABLE OF ty_output WITH HEADER LINE.
DATA: i_dd03p_tab   TYPE STANDARD TABLE OF dd03p WITH HEADER LINE.
DATA t_alv_cat      TYPE TABLE OF lvc_s_fcat.
DATA w_alv_cat      LIKE LINE OF t_alv_cat.

*>Variables
DATA v_times TYPE sy-index.
DATA v_maxrec TYPE i.
DATA v_nooffields TYPE i.
DATA v_header(5000).
DATA v_item(5000).
*DATA v_isnumber.
DATA v_error.
DATA v_errormsg(100).
DATA v_numbervalue(255).
DATA v_filename TYPE string.
DATA v_outfile(200).
DATA v_outstring(200).
DATA v_sheetno TYPE i.
DATA v_lrow TYPE i.
DATA v_lcol TYPE i.
DATA v_frow TYPE i.
DATA v_fcol TYPE i.
DATA v_desk(200).
DATA v_process.
DATA v_processsource.
DATA v_required.
DATA v_exceptions.
DATA v_string TYPE string.
DATA v_length TYPE i.
DATA v_extn TYPE string.
DATA v_file TYPE string.
DATA v_result TYPE abap_bool.
DATA v_answer TYPE c.
DATA v_folder TYPE string.
DATA v_file_exists.
DATA v_tablen TYPE ddobjname.
DATA v_clidep.
DATA ftype.

*Constants
CONSTANTS c_xls TYPE char3 VALUE 'XLS'.
CONSTANTS c_numspace(11) TYPE c VALUE '0123456789 '.

*>Macros
*>To check whether the datatype has Currency, Qty Values
DEFINE check_datatype.
  v_isnumber = space.
  read table t_dbtab index &1.
  if t_dbtab-inttype = 'P' or
     t_dbtab-inttype = 'F' or
     t_dbtab-inttype = 'I' or
     t_dbtab-inttype = 'X' or
     t_dbtab-inttype = 'N'.
    v_isnumber = 'X'.
  endif.
END-OF-DEFINITION.

*>Determind the File Name for Download File
DEFINE get_filename.
  if not p_ppath is initial.
    concatenate p_ppath '\' sy-datum '-' sy-uzeit '-' &1 '.CSV'
    into v_filename.
  elseif not p_bckpth is initial.
    concatenate p_bckpth '\' sy-datum '-' sy-uzeit '-' &1 '.CSV'
    into v_filename.
  else.
    concatenate c_temp sy-datum '-' sy-uzeit '-' &1 '.CSV'
    into v_filename.
  endif.
END-OF-DEFINITION.

*>Selection Screen
*>Table Name
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-010.
PARAMETERS: p_tablen TYPE dd02l-tabname.
SELECTION-SCREEN END OF BLOCK blk1.

*>Upload,Download Radio Box
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-011.
PARAMETER : p_down RADIOBUTTON GROUP sel DEFAULT 'X' USER-COMMAND dn.
PARAMETER : p_upld RADIOBUTTON GROUP sel.
SELECTION-SCREEN COMMENT 36(27) text-024.
PARAMETER : p_delt RADIOBUTTON GROUP sel.
SELECTION-SCREEN COMMENT 36(27) text-038.
SELECTION-SCREEN END OF BLOCK blk2.

*>Download Parameters
SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-012.
PARAMETER : p_recs(3) TYPE c MODIF ID dn.
SELECTION-SCREEN COMMENT 37(20) text-007 MODIF ID dn.
PARAMETER : p_ppath(200) TYPE c MODIF ID dn.
PARAMETER : p_dnpad AS CHECKBOX MODIF ID dn.
SELECTION-SCREEN END OF BLOCK blk3.

*>Upload,Download Radio Box
SELECTION-SCREEN BEGIN OF BLOCK blk5 WITH FRAME TITLE text-013.
PARAMETER : p_modify RADIOBUTTON GROUP del MODIF ID up DEFAULT 'X',
            p_delall RADIOBUTTON GROUP del MODIF ID up.
SELECTION-SCREEN SKIP 1.

*>Presentation Server Parameters
PARAMETER : p_prsn RADIOBUTTON GROUP sav MODIF ID up DEFAULT 'X'
USER-COMMAND up.
PARAMETERS p_header TYPE i MODIF ID up1 DEFAULT 1.
PARAMETERS:  p_frow      TYPE i MODIF ID up1 DEFAULT 1 NO-DISPLAY.
PARAMETERS:  p_fcol      TYPE i MODIF ID up1 DEFAULT 1 NO-DISPLAY.
PARAMETERS:  p_lrow      TYPE i MODIF ID up1 DEFAULT 65536 NO-DISPLAY.
PARAMETERS:  p_lcol      TYPE i MODIF ID up1 DEFAULT 256 NO-DISPLAY.
PARAMETERS:  p_sheet(2)  TYPE c MODIF ID up1 DEFAULT 1 NO-DISPLAY.
PARAMETERS:  p_desk      LIKE rlgrap-filename MODIF ID up1.

*>Application Server Parameters
PARAMETER : p_appl RADIOBUTTON GROUP sav MODIF ID up.
PARAMETERS:  p_apath(60) TYPE c MODIF ID up2.
SELECTION-SCREEN END OF BLOCK blk5.

SELECTION-SCREEN BEGIN OF BLOCK blk6 WITH FRAME TITLE text-025.

*Deletion Parameters
PARAMETER : p_dlall RADIOBUTTON GROUP dlt MODIF ID dl DEFAULT 'X'
USER-COMMAND dt,
            p_delpa RADIOBUTTON GROUP dlt MODIF ID dl,
            p_delpth LIKE rlgrap-filename MODIF ID dl1,
            p_chkbox type char1 NO-DISPLAY, "AS CHECKBOX  MODIF ID dl USER-COMMAND ck ,
            p_bckpth LIKE rlgrap-filename MODIF ID dl2,
            p_dlpad AS CHECKBOX MODIF ID dl2.
SELECTION-SCREEN END OF BLOCK blk6.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-059  .

SELECTION-SCREEN BEGIN OF LINE  .
SELECTION-SCREEN COMMENT 9(79) text-c01.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE  .
SELECTION-SCREEN COMMENT 13(79) text-c02.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE  .
SELECTION-SCREEN COMMENT 13(79) text-c03.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.


*>At Selection Screen
AT SELECTION-SCREEN.
*check  for Standard table

  If 'Z'NE p_tablen+0(1) and 'Y'NE p_tablen+0(1) and  p_tablen is not initial.
    Message 'Only Custom table is allowed' type 'E' .
  ENDIF.

*>Get Client Information
  PERFORM get_client.

*>Show Message when the Table entries selected
*>for deletion/insertion

  CLEAR v_answer.
  IF sy-ucomm = 'ONLI' OR sy-ucomm = 'CRET'.
    IF ( p_upld = 'X' AND p_delall = 'X' ) OR
       ( p_delt = 'X' AND p_dlall = 'X' ).

      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
        EXPORTING
          textline1     = text-031 "Are you sure you want to Delete?
          titel         = text-032 "'Confirm'
          start_column  = 25
          start_row     = 6
          defaultoption = 'N'
        IMPORTING
          answer        = v_answer.

      IF v_answer = 'N'.
        CALL SELECTION-SCREEN 1000.
      ENDIF.
    ENDIF.
  ENDIF.

*Checking if the Directory exists for download
  IF NOT p_ppath IS INITIAL.

    CLEAR : v_folder, v_file_exists.
    v_folder = p_ppath.

    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory = v_folder
      RECEIVING
        result    = v_file_exists
      EXCEPTIONS
        OTHERS    = 5.

    IF sy-subrc <> 0.
*     Do nothing
    ENDIF.

    IF v_file_exists IS INITIAL.
      v_error = 'X'.
      MOVE text-028 TO v_errormsg.
    ENDIF.

  ENDIF.

*Checking if the Directory exists for download for backup
  CHECK v_error NE 'X'.
  IF NOT p_bckpth IS INITIAL.
    CLEAR : v_folder, v_file_exists.
    v_folder = p_bckpth.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory = v_folder
      RECEIVING
        result    = v_file_exists
      EXCEPTIONS
        OTHERS    = 5.

    IF sy-subrc <> 0.
*     Do nothing
    ENDIF.

    IF v_file_exists IS INITIAL.
      v_error = 'X'.
      MOVE text-029 TO v_errormsg.
    ENDIF.

  ENDIF.

*>Providing Help Menu to Select the Desktop File
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_desk.
  PERFORM get_file.

*>Providing Help Menu to Select the Desktop File
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ppath.
  PERFORM get_file_ppath.

*>Providing Help Menu to Select the Desktop File
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_delpth.
  PERFORM get_file_delpth.

*>Providing Help Menu to store Back up file
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_bckpth.
  PERFORM get_file_bckpth.

*>Selection Screen Display Attributes Changes
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    PERFORM when_download.
    PERFORM when_upload.
    PERFORM when_deletion.
    MODIFY SCREEN.
  ENDLOOP.

  LOOP AT SCREEN.
    PERFORM when_desktop.
    PERFORM when_application.
    MODIFY SCREEN.
  ENDLOOP.

  LOOP AT SCREEN.
    PERFORM when_partial.
    PERFORM when_backup.
    MODIFY SCREEN.
  ENDLOOP.

INITIALIZATION.

  IF p_desk IS INITIAL.
    MOVE 'C:\Book1.xls' TO p_desk.
  ENDIF.

*>Move X if Download Radio box is initial
  IF p_down EQ space.
    MOVE 'X' TO p_down.
  ENDIF.

*>Start Of Selection
START-OF-SELECTION.

*>Check Input is initial.
  PERFORM input_table.

  CHECK v_error NE 'X'.

  IF p_down EQ 'X'.
    v_process = 'D'.
  ELSEIF p_upld EQ 'X'.
    v_process = 'U'.
    IF p_prsn = 'X'.
      v_processsource = 'D'.
      MOVE p_sheet TO v_sheetno.
      MOVE p_frow  TO v_frow.
      MOVE p_fcol  TO v_fcol.
      MOVE p_lrow  TO v_lrow.
      MOVE p_lcol  TO v_lcol.
      MOVE p_desk  TO v_desk.
      PERFORM input_file_attributes.
    ELSE.
      v_processsource = 'A'.
    ENDIF.
  ELSEIF p_delt EQ 'X'.
    v_process = 'E'.
    MOVE p_sheet TO v_sheetno.
    MOVE p_frow  TO v_frow.
    MOVE p_fcol  TO v_fcol.
    MOVE p_lrow  TO v_lrow.
    MOVE p_lcol  TO v_lcol.
    PERFORM input_file_attributes_del.
  ENDIF.

  CHECK v_error NE 'X'.
  CASE v_process.
*>If Downalod Radio box is enabled, Start download Process
    WHEN 'D'.
*>Move Const if Physical Path is initial for Download File
      IF p_ppath EQ space.
        get_filename p_tablen.
        MOVE v_filename TO p_ppath.
        CLEAR v_filename.
        PERFORM download_data.
      ELSE.
        get_filename p_tablen.
        MOVE v_filename TO p_ppath.
        CLEAR v_filename.
        PERFORM download_data.
      ENDIF.
    WHEN 'U'.
*>If Upload Radio box is enabled, Start Upload Process
      PERFORM upload_data.
    WHEN 'E'.
      PERFORM delete_data.
  ENDCASE.

END-OF-SELECTION.

  IF v_error EQ space.

    CASE v_process.
      WHEN 'D'.
*>Display Result
        PERFORM print_result.
      WHEN 'U'.
*>Print result
        PERFORM print_upload_status.
      WHEN 'E'.
        IF NOT p_dlall IS INITIAL.
          PERFORM print_delete_all_status.
        ELSE.
          PERFORM print_delete_par_status.
        ENDIF.
    ENDCASE.

  ELSE.
    FORMAT COLOR COL_NEGATIVE.
    WRITE:/ v_errormsg.
  ENDIF.



  INCLUDE zfi_upload_xref_data_get_exf01.

INCLUDE ZFI_UPLOAD_XREF_DATA_GET_CLF01.
