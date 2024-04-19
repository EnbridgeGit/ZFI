*&---------------------------------------------------------------------*
*& Report  ZBPCI021
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zbpci021.
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: hotspot_click  FOR EVENT link_click OF cl_salv_events_table
                         IMPORTING row column.
ENDCLASS.               "LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_handler
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD hotspot_click.
    PERFORM handle_click USING row column.
  ENDMETHOD.                    "hotspot_click1
ENDCLASS.               "lcl_event_handler

TABLES: anla,
        anlc.
TYPES: BEGIN OF ty_file_output,
        asset_class TYPE char11,
        comma1 TYPE c,
        asset_cate TYPE char08,
        comma2 TYPE c,
        asset_datasrc TYPE char10,
        comma3 TYPE c,
        asset_entity  TYPE char10,
        comma4 TYPE c,
        asset_plant   TYPE char04,
        comma5 TYPE c,
        asset_time    TYPE char08,
        comma6 TYPE c,
        asset_ttype   TYPE char14,
        comma7 TYPE c,
        asset_amount  TYPE char15,
       END OF ty_file_output.
TYPES: BEGIN OF ty_alv_output,
        bukrs TYPE anla-bukrs,
        anln1 TYPE anla-anln1,
        anln2 TYPE anla-anln2,
        anlkl TYPE anla-anlkl,
        ktogr TYPE anla-ktogr,
        gjahr TYPE anlc-gjahr,
        knafa(15) TYPE c, "anlc-knafa,
        kostl TYPE anlz-kostl,
        werks TYPE anlz-werks,
       END OF ty_alv_output.
DATA: gt_file_output TYPE TABLE OF ty_file_output,
      gs_file_output TYPE ty_file_output,
      gt_alv_output  TYPE TABLE OF ty_alv_output,
      gs_alv_output  TYPE ty_alv_output.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR  anla-bukrs DEFAULT 'UGL' OBLIGATORY.
PARAMETERS    : p_gjahr TYPE anlc-gjahr DEFAULT '2014' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b3 with FRAME TITLE text-003.
SELECT-OPTIONS: s_anlkl FOR  anla-anlkl .
SELECTION-SCREEN end of BLOCK b3.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_pc RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmd,
            p_file TYPE rfpdo-lboxfile DEFAULT 'H:\SAPTEMP\AALoad.csv', " OBLIGATORY.
            r_server RADIOBUTTON GROUP rad2,
            p_sfile TYPE rfpdo-rfbifile, " OBLIGATORY..
            c_tfile as CHECKBOX,
            p_tcfile TYPE rfpdo-rfbifile.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/' INTO p_sfile.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/' INTO p_tcfile.

  "'/BPC/SAP-' p_gjahr '-AALoad.csv' INTO p_sfile.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_values.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  PERFORM get_file.
  PERFORM get_pc_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sfile.
  PERFORM get_server_file.

START-OF-SELECTION.

IF c_tfile is NOT INITIAL AND
   r_server is INITIAL.
   Write: / 'Server file is not selected, Touch file cannot be created.'.
   STOP.
ENDIF.
IF c_tfile IS NOT INITIAL AND
  p_tcfile is INITIAL.
  write : / 'Input Touch File path and name.'.
  stop.
ENDIF.
*CONCATENATE p_sfile 'SAP-' p_gjahr '-DepLoad.csv' INTO p_sfile.

  IF s_anlkl[] IS INITIAL OR
     p_file IS INITIAL.
    WRITE : / 'Asset Class is required in selection screen..'.
    STOP.
  ENDIF.

  CLEAR:  gt_file_output,
          gs_file_output,
          gt_alv_output,
          gs_alv_output.

  PERFORM get_data.
  PERFORM download_data.
  PERFORM display_alv.
*&---------------------------------------------------------------------*
*&      Form  SET_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_values .

*  break sahmad.
*
*  CONSTANTS: lc_x   TYPE char1 VALUE 'X'.
*
*  IF r_pc = lc_x.
*    p_file = 'H:\SAPTEMP\'.
*  ELSE.
*    CLEAR p_file.
*  ENDIF.

ENDFORM.                    " SET_VALUES
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lv_append TYPE c,
        lv_tabix TYPE sy-tabix,
        lv_amount type anlc-knafa,
        lt_anla TYPE TABLE OF anla,
        ls_anla TYPE anla,
        lt_anlz TYPE TABLE OF anlz,
        ls_anlz TYPE anlz,
        lt_anlc TYPE TABLE OF anlc,
        ls_anlc TYPE anlc.

  SELECT * FROM anla INTO TABLE lt_anla
    WHERE bukrs IN s_bukrs
      AND anlkl IN s_anlkl.
  IF lt_anla IS INITIAL.
    WRITE : / 'No Data to process for selection criteria.'.
    STOP.
  ENDIF.
  IF lt_anla IS NOT INITIAL.
    SELECT * FROM anlc INTO TABLE lt_anlc
      FOR ALL ENTRIES IN lt_anla
      WHERE bukrs = lt_anla-bukrs
        AND anln1 = lt_anla-anln1
        AND gjahr = p_gjahr.
    IF lt_anlc IS INITIAL.
      WRITE : / 'No Data to process for selection criteria.'.
      STOP.
    ENDIF.
    SELECT * FROM anlz INTO TABLE lt_anlz
      FOR ALL ENTRIES IN lt_anla
      WHERE bukrs = lt_anla-bukrs
        AND anln1 = lt_anla-anln1
        AND anln2 = lt_anla-anln2.
  ENDIF.
*Prepare table for ALV output
  SORT lt_anla BY bukrs anln1 anln2.
  SORT lt_anlz BY bukrs anln1 anln2 bdatu.
  SORT lt_anlc BY bukrs anln1 anln2.
  LOOP AT lt_anla INTO ls_anla.
    CLEAR: gs_alv_output,
           ls_anlz,
           lv_append.
    gs_alv_output-bukrs = ls_anla-bukrs.
    gs_alv_output-anln1 = ls_anla-anln1.
    gs_alv_output-anln2 = ls_anla-anln2.
    gs_alv_output-anlkl = ls_anla-anlkl.
    gs_alv_output-ktogr = ls_anla-ktogr.
    READ TABLE lt_anlz INTO ls_anlz WITH KEY bukrs = ls_anla-bukrs
                                             anln1 = ls_anla-anln1
                                             anln2 = ls_anla-anln2.
    gs_alv_output-kostl = ls_anlz-kostl.
    gs_alv_output-werks = ls_anlz-werks.
    READ TABLE lt_anlc WITH KEY bukrs = ls_anla-bukrs
                                anln1 = ls_anla-anln1
                                anln2 = ls_anla-anln2
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    CHECK sy-subrc = 0.
    LOOP AT lt_anlc INTO ls_anlc FROM lv_tabix.
      IF ls_anlc-bukrs <> ls_anla-bukrs OR
         ls_anlc-anln1 <> ls_anla-anln1 OR
         ls_anlc-anln2 <> ls_anla-anln2.
        EXIT.
      ENDIF.
      CHECK ls_anlc-knafa <> 0.
      gs_alv_output-gjahr = ls_anlc-gjahr.
      lv_amount = ls_anlc-knafa.
      if lv_amount < 0.
          gs_alv_output-knafa = lv_amount.
          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
            CHANGING
              value         = gs_alv_output-knafa.
      else.
        gs_alv_output-knafa = lv_amount.
      endif.
      CONDENSE gs_alv_output-knafa NO-GAPS.
      SHIFT gs_alv_output-knafa right.
      lv_append = 'X'.
      APPEND gs_alv_output TO gt_alv_output.
    ENDLOOP.
*    IF lv_append IS INITIAL.
*      APPEND gs_alv_output TO gt_alv_output.
*    ENDIF.
  ENDLOOP.
*Clear Memory
  CLEAR: lt_anlc,
         lt_anlz,
         lt_anla.
*Prepare table for file output
  LOOP AT gt_alv_output INTO gs_alv_output.
    gs_file_output-comma1 = ','.
    gs_file_output-comma2 = ','.
    gs_file_output-comma3 = ','.
    gs_file_output-comma4 = ','.
    gs_file_output-comma5 = ','.
    gs_file_output-comma6 = ','.
    gs_file_output-comma7 = ','.
    gs_file_output-asset_cate    = 'Actual'.
    gs_file_output-asset_ttype   = 'Dep_Load'.
    gs_file_output-asset_datasrc = 'SAP_AA'.
     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = gs_alv_output-anlkl
     IMPORTING
        OUTPUT        = gs_alv_output-anlkl.
    CONCATENATE 'AC_' gs_alv_output-anlkl INTO gs_file_output-asset_class.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = gs_alv_output-kostl
     IMPORTING
        OUTPUT        = gs_alv_output-kostl.
    gs_file_output-asset_entity  =  gs_alv_output-kostl.
    gs_file_output-asset_plant   =  gs_alv_output-werks.
    CONCATENATE  gs_alv_output-gjahr '1200' INTO gs_file_output-asset_time.
    gs_file_output-asset_amount  =  gs_alv_output-knafa.
    APPEND gs_file_output TO gt_file_output.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_data .
  IF r_pc = 'X'.
    PERFORM pc_download.
  ELSE.
    PERFORM server_download.
  ENDIF.
ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  PC_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pc_download .
  DATA: lv_filename TYPE string.
*Download Canadian file
  MOVE p_file TO lv_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
      trunc_trailing_blanks_eol = space "'X'
    CHANGING
      data_tab                  = gt_file_output
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
*       TITEL =
        msgid = 'PG'
        msgty = 'I'
        msgno = '016'
        msgv1 = 'Error with downloading CSV file at PC '
*       MSGV2 =
*       MSGV3 =
*       MSGV4 =
      .

*    WRITE: / 'Error with downloading CSV file at PC ', sy-subrc.
  ENDIF.
ENDFORM.                    " PC_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  SERVER_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM server_download .

  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
*       TITEL =
        msgid = 'PG'
        msgty = 'I'
        msgno = '016'
        msgv1 = 'Unable to open CSV file for output on Server.'
*       MSGV2 =
*       MSGV3 =
*       MSGV4 =
      .
*    WRITE:/ 'Unable to open text file for output.'.
  ELSE.
    LOOP AT gt_file_output INTO gs_file_output.
      TRANSFER gs_file_output TO p_sfile.
    ENDLOOP.
    CLOSE DATASET p_sfile.
  ENDIF.
*********************Blank Touch File******
  OPEN DATASET p_tcfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
      EXPORTING
*       TITEL =
        msgid = 'PG'
        msgty = 'I'
        msgno = '016'
        msgv1 = 'Unable to open Touch file for output on Server.'
*       MSGV2 =
*       MSGV3 =
*       MSGV4 =
      .
*    WRITE:/ 'Unable to open text file for output.'.
  ELSE.
    CLOSE DATASET p_tcfile.
  ENDIF.
ENDFORM.                    " SERVER_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv .
  DATA:   ls_key         TYPE salv_s_layout_key,
          lo_table       TYPE REF TO cl_salv_table,
          lo_layout      TYPE REF TO cl_salv_layout,
          lo_functions   TYPE REF TO cl_salv_functions,
          lo_display     TYPE REF TO cl_salv_display_settings,
          lo_columns     TYPE REF TO cl_salv_columns_table,
          lo_column      TYPE REF TO cl_salv_column_table,  "#EC NEEDED
          lo_content     TYPE REF TO cl_salv_form_element,
          lo_grid        TYPE REF TO cl_salv_form_layout_grid,
          lo_events_salv TYPE REF TO cl_salv_events_table,
          lo_event       TYPE REF TO lcl_event_handler.

  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*          r_container    = lr_con1
*          container_name = 'ALV_CON1'
        IMPORTING
          r_salv_table   = lo_table
        CHANGING
          t_table        = gt_alv_output.
    CATCH cx_salv_msg .                                 "#EC NO_HANDLER
  ENDTRY.
*Function settings
  lo_functions = lo_table->get_functions( ).
  lo_functions->set_all( abap_true ).
*Display Setting
  lo_display = lo_table->get_display_settings( ).

  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
*Event
  lo_events_salv = lo_table->get_event( ).
  CREATE OBJECT lo_event.
  SET HANDLER: lo_event->hotspot_click
               FOR lo_events_salv.
*Set layout
  lo_layout = lo_table->get_layout( ).
  ls_key-report = sy-repid.
  lo_layout->set_key( ls_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*  CALL METHOD lo_layout->set_initial_layout
*    EXPORTING
*      value = p_vari.
*Get columns
  CALL METHOD lo_table->get_columns
    RECEIVING
      value = lo_columns.
*****Change ALV Fields  - title etc.
  PERFORM alv_fields USING lo_columns lo_column.
******Display ALV
  CALL METHOD lo_table->display.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  HANDLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_click   USING iv_row TYPE salv_de_row
                          iv_column TYPE salv_de_column.

  DATA: ls_alv_output TYPE ty_alv_output.

  READ TABLE gt_alv_output INTO ls_alv_output INDEX iv_row.
  CHECK sy-subrc = 0.
*  CASE iv_column.
*      WHEN 'ANLN1'.
  SET PARAMETER ID 'AN1' FIELD ls_alv_output-anln1.
  SET PARAMETER ID 'AN2' FIELD ls_alv_output-anln2.
  SET PARAMETER ID 'BUK' FIELD ls_alv_output-bukrs.
  CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
*  ENDCASE.


ENDFORM.                    " HANDLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_fields  USING    io_columns TYPE REF TO cl_salv_columns_table
                          io_column  TYPE REF TO cl_salv_column_table.

data: lv_short_text TYPE SCRTEXT_S,
      lv_med_text type SCRTEXT_M,
      lv_long_text TYPE SCRTEXT_L.

*****hot spot
  TRY.
      io_column ?= io_columns->get_column( 'ANLN1' ).
      CALL METHOD io_column->set_cell_type
        EXPORTING
      value = if_salv_c_cell_type=>hotspot.
    CATCH cx_salv_not_found .
  ENDTRY.

  TRY .
      io_column ?= io_columns->get_column( 'KNAFA' ).
      CALL METHOD io_column->set_alignment
      EXPORTING
        value  = IF_SALV_C_ALIGNMENT=>RIGHT.
      lv_short_text = 'Amount'.
      lv_med_text = 'Amount'.
      lv_long_text = 'Amount'.
      CALL METHOD io_column->set_short_text
      EXPORTING
        value  = lv_short_text.
      CALL METHOD io_column->set_medium_text
      EXPORTING
        value  = lv_med_text.
      CALL METHOD io_column->set_long_text
      EXPORTING
        value  = lv_long_text.
  CATCH cx_salv_not_found .

  ENDTRY.

ENDFORM.                    "alv_fields
*&---------------------------------------------------------------------*
*&      Form  GET_PC_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_PC_FILE .

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',DAT File,*.csv'
      static    = 'X'
    CHANGING
      file_name = p_file.

ENDFORM.                    " GET_PC_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_SERVER_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_SERVER_FILE .
 CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    EXPORTING
*     DIRECTORY        = ' '
      filemask         = '*.*'
    IMPORTING
      serverfile       = p_sfile
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.                    " GET_SERVER_FILE
