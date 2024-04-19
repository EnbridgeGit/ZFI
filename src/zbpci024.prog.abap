*&---------------------------------------------------------------------*
*& Report  ZBPCI024
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zbpci024.
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
        anek,
        anep.
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
        bukrs TYPE anek-bukrs,
        anlkl TYPE anla-anlkl,
        anln1 TYPE anek-anln1,
        anln2 TYPE anek-anln2,
        gjahr TYPE anek-gjahr,
        monat TYPE anek-monat,
        lnran TYPE anek-lnran,
        budat TYPE anek-budat,
        bldat TYPE anek-bldat,
        bwasl TYPE anep-bwasl,
        anbtr(15) TYPE c,
        kostl TYPE anlz-kostl,
        werks TYPE anlz-werks,
        nafaz(15) TYPE c,
       END OF ty_alv_output.
DATA: gt_file_output TYPE TABLE OF ty_file_output,
      gs_file_output TYPE ty_file_output,
      gt_alv_output  TYPE TABLE OF ty_alv_output,
      gs_alv_output  TYPE ty_alv_output.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR  anla-bukrs DEFAULT 'UGL' OBLIGATORY,
                s_anlkl FOR  anla-anlkl.
PARAMETERS    : p_gjahr TYPE anek-gjahr DEFAULT '2014' OBLIGATORY,
                p_monat TYPE anek-monat DEFAULT '01' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
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

*CONCATENATE p_sfile 'SAP-' p_monat p_gjahr '-AA_Dep_periodic.csv' INTO p_sfile.

  IF s_anlkl[] IS INITIAL.
    WRITE : / 'Asset Class is required in selection screen..'.
    STOP.
  ENDIF.
  IF p_file IS INITIAL AND
     r_pc IS NOT INITIAL.
    WRITE : / 'PC File is required is required in selection screen..'.
    STOP.
  ENDIF.
  IF p_sfile IS INITIAL AND
     r_server IS NOT INITIAL.
    WRITE : / 'Server File is required is required in selection screen..'.
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

**  break sahmad.
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
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_file .

  IF r_pc = 'X'.
    CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
      EXPORTING
        mask      = ',DAT File,*.csv'
        static    = 'X'
      CHANGING
        file_name = p_file.
  ELSE.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
*       DIRECTORY        = ' '
        filemask         = '*.*'
      IMPORTING
        serverfile       = p_file
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  CONSTANTS c_period12 TYPE peraf VALUE '12'.
  DATA: lv_append TYPE c,
        lv_tabix  TYPE sy-tabix,
        lv_tabix2 TYPE sy-tabix,
        lv_peraf  TYPE peraf,
        lv_amount TYPE anep-anbtr,
        lt_anla TYPE TABLE OF anla,
        ls_anla TYPE anla,
        lt_anlz TYPE TABLE OF anlz,
        ls_anlz TYPE anlz,
        lt_anek TYPE TABLE OF anek,
        ls_anek TYPE anek,
        lt_anep TYPE TABLE OF anep,
        ls_anep TYPE anep,
        lt_anea TYPE TABLE OF anea,
        ls_anea TYPE anea,
        lt_anlp TYPE TABLE OF anlp,
        ls_anlp TYPE anlp,
        lt_alv_output  TYPE TABLE OF ty_alv_output.

  SELECT * FROM anla INTO TABLE lt_anla
    WHERE bukrs IN s_bukrs
      AND anlkl IN s_anlkl.
  IF lt_anla IS INITIAL.
    WRITE : / 'No Data to process for selection criteria.'.
    STOP.
  ENDIF.
  lv_peraf = p_monat.
  IF lt_anla IS NOT INITIAL.
    SELECT * FROM anek INTO TABLE lt_anek
      FOR ALL ENTRIES IN lt_anla
      WHERE bukrs = lt_anla-bukrs
        AND anln1 = lt_anla-anln1
        AND anln2 = lt_anla-anln2
        AND gjahr = p_gjahr
        AND monat = p_monat.
*    IF lt_anek IS INITIAL.
*      WRITE : / 'No Data to process for selection criteria.'.
*      STOP.
*    ENDIF.
    SELECT * FROM anlp INTO TABLE lt_anlp
      FOR ALL ENTRIES IN lt_anla
      WHERE  bukrs = lt_anla-bukrs
        AND  gjahr = p_gjahr
        AND  peraf = lv_peraf "c_period12
*AFBNR
        AND  anln1 = lt_anla-anln1
        AND  anln2 = lt_anla-anln2.
*        AND  AFABER
*ZUJHR
*ZUCOD
    SELECT * FROM anlz INTO TABLE lt_anlz
     FOR ALL ENTRIES IN lt_anla
     WHERE bukrs = lt_anla-bukrs
       AND anln1 = lt_anla-anln1
       AND anln2 = lt_anla-anln2.
    if lt_anek[] is not INITIAL.
    SELECT * FROM anep INTO TABLE lt_anep
      FOR ALL ENTRIES IN lt_anek
      WHERE bukrs = lt_anek-bukrs
        AND anln1 = lt_anek-anln1
        AND anln2 = lt_anek-anln2
        AND gjahr = lt_anek-gjahr
        AND lnran = lt_anek-lnran.
    endif.
    IF lt_anep[] IS NOT INITIAL.
      SELECT * FROM anea INTO TABLE lt_anea
        FOR ALL ENTRIES IN lt_anep
        WHERE bukrs  = lt_anep-bukrs
          AND anln1  = lt_anep-anln1
          AND anln2  = lt_anep-anln2
          AND gjahr  = lt_anep-gjahr
          AND lnran  = lt_anep-lnran
          AND afabe  = lt_anep-afabe
          AND zujhr  = lt_anep-zujhr
          AND zucod  = lt_anep-zucod.
    ENDIF.

  ENDIF.
*Prepare table for ALV output
  SORT lt_anla BY bukrs anln1 anln2.
  SORT lt_anek BY bukrs anln1 anln2 gjahr monat.
  SORT lt_anep BY bukrs anln1 anln2 gjahr lnran.
  SORT lt_anlz BY bukrs anln1 anln2.
  SORT lt_anea BY bukrs anln1	anln2 gjahr	lnran
                  afabe	zujhr	zucod.
  SORT lt_anlp BY bukrs anln1 anln2 gjahr peraf.
  CLEAR ls_anlp.
  LOOP AT lt_anla INTO ls_anla.
    CLEAR: gs_alv_output,
           ls_anlz,
           lv_append,
           lv_tabix,
           lv_tabix2.

    gs_alv_output-bukrs = ls_anla-bukrs.
    gs_alv_output-anln1 = ls_anla-anln1.
    gs_alv_output-anln2 = ls_anla-anln2.
    gs_alv_output-anlkl = ls_anla-anlkl.
    READ TABLE lt_anlz INTO ls_anlz WITH KEY bukrs = ls_anla-bukrs
                                             anln1 = ls_anla-anln1
                                             anln2 = ls_anla-anln2.
    gs_alv_output-kostl = ls_anlz-kostl.
    gs_alv_output-werks = ls_anlz-werks.
    READ TABLE lt_anek WITH KEY bukrs = ls_anla-bukrs
                                anln1 = ls_anla-anln1
                                anln2 = ls_anla-anln2
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
*    CHECK sy-subrc = 0.
    LOOP AT lt_anek INTO ls_anek FROM lv_tabix.
      IF ls_anek-bukrs <> ls_anla-bukrs OR
         ls_anek-anln1 <> ls_anla-anln1 OR
         ls_anek-anln2 <> ls_anla-anln2.
        EXIT.
      ENDIF.
      CLEAR: gs_alv_output-nafaz,
             gs_alv_output-anbtr.
      gs_alv_output-gjahr = ls_anek-gjahr.
      gs_alv_output-monat = ls_anek-monat.
      gs_alv_output-lnran = ls_anek-lnran.
      gs_alv_output-budat = ls_anek-budat.
      gs_alv_output-bldat = ls_anek-bldat.
      READ TABLE lt_anep WITH KEY bukrs = ls_anek-bukrs
                                  anln1 = ls_anek-anln1
                                  anln2 = ls_anek-anln2
                                  gjahr = ls_anek-gjahr
                                  lnran = ls_anek-lnran
                                  TRANSPORTING NO FIELDS.
      lv_tabix2 = sy-tabix.
*      CHECK sy-subrc = 0.
      LOOP AT lt_anep INTO ls_anep FROM lv_tabix2.
        IF ls_anek-bukrs <> ls_anep-bukrs OR
           ls_anek-anln1 <> ls_anep-anln1 OR
           ls_anek-anln2 <> ls_anep-anln2 OR
           ls_anek-gjahr <> ls_anep-gjahr OR
           ls_anek-lnran <> ls_anep-lnran.
          EXIT.
        ENDIF.
*For Group Assets (anla-xanglr = 'X') only transaction type 701, 702, and 705
*should be included for periodic depreciations
*For Non-Group Assets (anla-xanglr = ' ') all transaction types should be included
*(Remove exclusion of 701, 702, and 705 in the logic).
*        IF ls_anla-xanlgr <> 'X' AND
*           ( ls_anep-bwasl = '701' or
*             ls_anep-bwasl = '702' or
*             ls_anep-bwasl = '705' ).
*              CONTINUE.
*        ELSEIF ls_anla-xanlgr = 'X' AND
          IF ls_anla-xanlgr = 'X' AND
             ( ls_anep-bwasl <> '701' AND
               ls_anep-bwasl <> '702' AND
               ls_anep-bwasl <> '705' ).
              CONTINUE.
          ENDIF.
        CLEAR ls_anea.
        READ TABLE lt_anea INTO ls_anea WITH KEY
                          bukrs	= ls_anep-bukrs
                          anln1	= ls_anep-anln1
                          anln2	= ls_anep-anln2
                          gjahr	= ls_anep-gjahr
                          lnran	= ls_anep-lnran
                          afabe	= ls_anep-afabe
                          zujhr	= ls_anep-zujhr
                          zucod	= ls_anep-zucod.
        IF ls_anea-nafav + ls_anea-nafal = 0.
          CONTINUE.
        ENDIF.
        gs_alv_output-bwasl = ls_anep-bwasl.
        lv_amount = ls_anea-nafav + ls_anea-nafal.
        IF lv_amount < 0.
           gs_alv_output-anbtr = lv_amount.
           CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
             CHANGING
               value         = gs_alv_output-anbtr.
*           gs_alv_output-anbtr = lv_amount.
        else.
          gs_alv_output-anbtr = lv_amount.
        ENDIF.
        CONDENSE gs_alv_output-anbtr NO-GAPS.
        SHIFT gs_alv_output-anbtr RIGHT.
        APPEND gs_alv_output TO gt_alv_output.
        lv_append = 'X'.
      ENDLOOP.
    ENDLOOP.
    CLEAR lv_tabix.
    READ TABLE lt_anlp WITH KEY bukrs = ls_anla-bukrs
                                anln1 = ls_anla-anln1
                                anln2 = ls_anla-anln2
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_anlp INTO ls_anlp FROM lv_tabix.
      IF ls_anlp-bukrs <> ls_anla-bukrs OR
         ls_anlp-anln1 <> ls_anla-anln1 OR
         ls_anlp-anln2 <> ls_anla-anln2.
        EXIT.
      ENDIF.
      CHECK ls_anlp-nafaz <> 0.
      gs_alv_output-gjahr = ls_anlp-gjahr.
      gs_alv_output-monat = ls_anlp-peraf.
*         gs_alv_output-lnran = ls_anek-lnran.
*      gs_alv_output-nafaz = ls_anlp-nafaz.
      lv_amount = ls_anlp-nafaz.
      IF lv_amount < 0.
         gs_alv_output-anbtr = lv_amount.
           CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
             CHANGING
               value         = gs_alv_output-anbtr.
*         gs_alv_output-anbtr = lv_amount.
         gs_alv_output-nafaz = gs_alv_output-anbtr.
      else.
        gs_alv_output-anbtr = ls_anlp-nafaz.
        gs_alv_output-nafaz = ls_anlp-nafaz.
      ENDIF.
      SHIFT gs_alv_output-anbtr RIGHT.
      gs_alv_output-bwasl = '999'.
      APPEND gs_alv_output TO gt_alv_output.
    ENDLOOP.
  ENDLOOP.
*Clear Memory
  CLEAR: lt_anek,
         lt_anep,
         lt_anlz,
         lt_anla,
         lt_anea,
         lt_anlp.
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
    gs_file_output-asset_ttype   = gs_alv_output-bwasl.
    gs_file_output-asset_datasrc = 'SAP_AA'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_alv_output-anlkl
      IMPORTING
        output = gs_alv_output-anlkl.
    CONCATENATE 'AC_' gs_alv_output-anlkl INTO gs_file_output-asset_class.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input         = gs_alv_output-kostl
     IMPORTING
        OUTPUT        = gs_alv_output-kostl.
    gs_file_output-asset_entity  =  gs_alv_output-kostl.
    gs_file_output-asset_plant   =  gs_alv_output-werks.
    CONCATENATE  gs_alv_output-gjahr gs_alv_output-monat '00' INTO gs_file_output-asset_time.
    gs_file_output-asset_amount  =  gs_alv_output-anbtr.
*    IF gs_alv_output-anbtr <> 0 AND
*       gs_alv_output-nafaz IS INITIAL.
*      APPEND gs_file_output TO gt_file_output.
*    ENDIF.
    IF gs_alv_output-nafaz <> 0.
      gs_file_output-asset_ttype   = 'DEPREC'.
      gs_file_output-asset_amount  =  gs_alv_output-nafaz.
*      APPEND gs_file_output TO gt_file_output.
    ENDIF.
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
**************Blank Touch File
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
  TRY.

      io_column ?= io_columns->get_column( 'NAFAZ' ).
      CALL METHOD io_column->set_technical
        EXPORTING
          value = if_salv_c_bool_sap=>true.

      io_column ?= io_columns->get_column( 'ANBTR' ).
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
FORM get_pc_file .

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
FORM get_server_file .

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
