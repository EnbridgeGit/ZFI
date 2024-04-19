*&---------------------------------------------------------------------*
*&  Include           ZDYNAMIC_F01                                     *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  check_inputvalue
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TABLEN  text
*----------------------------------------------------------------------*
FORM check_inputvalue  USING pv_tablename TYPE c.

  DATA w_dd02l TYPE dd02l.

  IF pv_tablename IS INITIAL.

    v_error = 'X'.
    MOVE text-002 TO v_errormsg.

  ELSE.

    SELECT SINGLE * FROM dd02l
      INTO w_dd02l
     WHERE tabname = pv_tablename
       AND as4local = 'A'
       AND as4vers = '0000'   .

    IF sy-subrc <> 0.

      v_error = 'X'.
      CONCATENATE 'Table - ' p_tablen
      text-001 INTO v_errormsg SEPARATED BY space.

    ELSE.

      IF w_dd02l-tabclass NE 'TRANSP'.

        v_error = 'X'.
        CONCATENATE 'Input -' p_tablen
                    'is not an Transparent Table'
                    INTO v_errormsg SEPARATED BY space.

      ELSE.

        IF v_process = 'U'.
          CHECK w_dd02l-mainflag = 'N'.
          v_error = 'X'.
          CONCATENATE text-033 p_tablen
                      INTO v_errormsg SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " check_inputvalue
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_data .

  CHECK v_error EQ space.
*>Determine the Input Table is valid
  PERFORM check_inputvalue USING p_tablen.

  CHECK v_error EQ space.
*>Retrieve the Input Table Structure
  PERFORM get_table_structure.

  CHECK v_error EQ space.
*>Create the Dynamic Table Structure
  PERFORM create_dynamic_table.

  CHECK v_error EQ space.
*>Retrive Records
  PERFORM get_records.

  CHECK v_error EQ space.
*>Populate Output Data
  PERFORM populate_data.

  CHECK v_error EQ space.
*>Create Output File (Download)
  PERFORM crate_file.

ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_data .

  CHECK v_error EQ space.
*>Determine the Input Table is valid
  PERFORM check_inputvalue USING p_tablen.

  CHECK v_error EQ space.
*>Check Parameter table is maintained
*  PERFORM check_param_table.
*  CHECK v_error EQ space.
*>Retrieve the Input Table Structure
  PERFORM get_table_structure.

  CHECK v_error EQ space.
*>Create the Dynamic Table Structure
  PERFORM create_dynamic_table.

  CHECK v_error EQ space.
  PERFORM read_input_file.

ENDFORM.                    " UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  get_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file .

  CALL FUNCTION 'WS_FILENAME_GET'                           "#EC *
    EXPORTING
      def_filename     = p_desk
      def_path         = '/'
      mask             = ',*.*,*.*.'
      mode             = 'O'
      title            = text-034 "'Get filename'
    IMPORTING
      filename         = p_desk
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

ENDFORM.                    " get_file
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_PPATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file_ppath .

  CLEAR v_folder.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title    = 'Select Only File Path'
    CHANGING
      selected_folder = v_folder.

  IF sy-subrc <> 0.
  ENDIF.

  p_ppath = v_folder.

ENDFORM.                    " GET_FILE_PPATH
*&---------------------------------------------------------------------*
*&      Form  get_table_structure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_table_structure .

  REFRESH t_dbtab.
  REFRESH t_alv_cat.
  CLEAR   t_alv_cat.
  CLEAR   w_alv_cat.
  CLEAR   v_nooffields.

  CALL FUNCTION 'NAMETAB_GET'                               "#EC *
    EXPORTING
      langu               = sy-langu
      tabname             = p_tablen
    TABLES
      nametab             = t_dbtab
    EXCEPTIONS
      internal_error      = 1
      table_has_no_fields = 2
      table_not_activ     = 3
      no_texts_found      = 4
      OTHERS              = 5.

  DESCRIBE TABLE t_dbtab LINES v_nooffields.

  LOOP AT t_dbtab.
    w_alv_cat-col_pos   = t_dbtab-position.
    w_alv_cat-fieldname = t_dbtab-fieldname.
    w_alv_cat-tabname   = p_tablen.
    w_alv_cat-ref_table = p_tablen.
    w_alv_cat-ref_field = t_dbtab-fieldname.
    w_alv_cat-scrtext_l = t_dbtab-fieldtext.
    w_alv_cat-scrtext_m = t_dbtab-fieldtext.
    w_alv_cat-scrtext_s = t_dbtab-fieldtext.
    APPEND w_alv_cat TO t_alv_cat.
    CLEAR w_alv_cat.
  ENDLOOP.

ENDFORM.                    " get_table_structure
*&---------------------------------------------------------------------*
*&      Form  create_dynamic_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_dynamic_table .

* internal table build
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = t_alv_cat
    IMPORTING
      ep_table        = d_ref.

  ASSIGN d_ref->* TO <f_fs>.
ENDFORM.                    " create_dynamic_table
*&---------------------------------------------------------------------*
*&      Form  input_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_table .

  IF p_tablen IS INITIAL.
    v_error = 'X'.
    v_errormsg = text-036. "Table name is expected ...!
  ENDIF.

  CHECK v_error NE 'X'.
  IF ( p_tablen+0(1) NE 'Z' AND p_tablen+0(1) NE '/' ).
    v_error = 'X'.
    v_errormsg = text-037. "Please specify Custom Table Name ...!
  ENDIF.

ENDFORM.                    " input_table
*&---------------------------------------------------------------------*
*&      Form  get_records
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_records .

  CLEAR v_maxrec.

  MOVE p_recs TO v_maxrec.
*>Select Records to Database
  CASE v_maxrec.
    WHEN '0'.
      SELECT *
        FROM (p_tablen)
        INTO CORRESPONDING FIELDS OF
       TABLE <f_fs>.
    WHEN OTHERS.
      SELECT * UP TO v_maxrec ROWS
        FROM (p_tablen)
        INTO CORRESPONDING FIELDS OF
       TABLE <f_fs>.
  ENDCASE.

ENDFORM.                    " get_records
*&---------------------------------------------------------------------*
*&      Form  populate_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM populate_data.

  DATA v_text(100).
  DATA: table TYPE REF TO data.
  CREATE DATA table TYPE (p_tablen).
  DATA v_flength TYPE i.
  DATA w_offset TYPE i.
  DATA w_charval TYPE char255.

  CLEAR v_times.
*>Check if there is no error
  CHECK v_error EQ space.
*>Sort t_dbtab
  SORT t_dbtab BY position.

  LOOP AT <f_fs> ASSIGNING <f_fs2>.
    AT FIRST.
      DO v_nooffields TIMES.
        IF v_times GT v_nooffields.
          EXIT.
        ELSE.
          READ TABLE t_dbtab INDEX sy-index.
          v_times = sy-index.
          IF sy-subrc = 0.
            IF v_times EQ 1.
              MOVE t_dbtab-fieldname TO v_header.
            ELSE.
              CONCATENATE v_header ',' t_dbtab-fieldname INTO v_header.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.

      MOVE v_header TO w_output-data.
      APPEND w_output TO t_output.
      CLEAR w_output.
      CLEAR v_header.
    ENDAT.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <f_fs2> TO <f_field>.
      IF sy-subrc <> 0.
        MOVE v_item TO w_output-data.
        APPEND w_output TO t_output.
        CLEAR w_output.
        CLEAR v_item.
        EXIT.
      ELSE.
        IF sy-index EQ 1.
          WRITE <f_field> TO v_item.
        ELSE.
          DESCRIBE FIELD <f_field> TYPE ftype.
          DESCRIBE FIELD <f_field> OUTPUT-LENGTH v_flength.
          CLEAR: w_offset,
                 v_numbervalue.
          w_offset = 255 - v_flength.
          CASE ftype.
            WHEN 'P' OR 'I'.
              UNPACK <f_field> TO w_charval.
              CONDENSE w_charval.
              v_numbervalue = w_charval+w_offset.
              CONCATENATE v_item ',' v_numbervalue INTO v_item.
            WHEN 'C' OR 'N'.
              IF <f_field> CO c_numspace.
                SHIFT <f_field> RIGHT DELETING TRAILING space.
                IF NOT <f_field> IS INITIAL.
                  TRANSLATE <f_field> USING ' 0'.
                ENDIF.
              ENDIF.
              CONCATENATE v_item ',' <f_field> INTO v_item.
            WHEN OTHERS.
              WRITE <f_field> TO v_numbervalue.
              CONCATENATE v_item ',' v_numbervalue INTO v_item.
          ENDCASE.

        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.



ENDFORM.                    " populate_data
*&---------------------------------------------------------------------*
*&      Form  crate_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM crate_file .

  IF NOT p_chkbox IS INITIAL.
    MOVE p_bckpth TO v_filename.
  ELSE.
    MOVE p_ppath TO v_filename.
  ENDIF.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename = v_filename
      filetype = 'ASC'
    TABLES
      data_tab = t_output.

ENDFORM.                    " crate_file
*&---------------------------------------------------------------------*
*&      Form  print_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_result .

  CLEAR v_outstring.

  MOVE p_ppath TO v_outfile.

  CONCATENATE 'File' v_outfile 'is created'
              INTO v_outstring
              SEPARATED BY space.

  FORMAT COLOR COL_POSITIVE.
  WRITE:/ v_outstring.

  CLEAR v_filename.
  CLEAR v_outfile.
  CLEAR t_output.
  CLEAR v_filename.

ENDFORM.                    " print_result
*&---------------------------------------------------------------------*
*&      Form  when_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM when_download .

*>If Download Radio Box is Enabld, Hide Upload Parameters
  IF p_down EQ 'X'.

    p_dlall = 'X'.
    CLEAR : p_chkbox, p_delpa.

    IF screen-group1 = 'UP'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
    IF screen-group1 = 'UP1'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
    IF screen-group1 = 'UP2'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
    IF screen-group1 = 'DL'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
    IF screen-group1 = 'DL1'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
    IF screen-group1 = 'DL2'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
  ENDIF.

ENDFORM.                    " when_download
*&---------------------------------------------------------------------*
*&      Form  when_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM when_upload .

*>If Upload Radio Box is Enabld, Hide Download Parameters
  IF p_upld EQ 'X'.

    p_dlall = 'X'.
    CLEAR : p_chkbox, p_delpa.

    IF screen-group1 = 'DN'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
  ENDIF.

*>If Upload Radio Box is Enabld, Hide Deletion Parameters
  IF p_upld EQ 'X'.
    IF screen-group1 = 'DL'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
  ENDIF.

  IF p_upld EQ 'X'.
    IF screen-group1 = 'DL1'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
  ENDIF.

  IF p_upld EQ 'X'.
    IF screen-group1 = 'DL2'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
  ENDIF.

ENDFORM.                    " when_upload
*&---------------------------------------------------------------------*
*&      Form  when_desktop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM when_desktop .

  IF p_appl EQ 'X'.
    IF screen-group1 = 'UP1'.
      screen-active = 0.
      screen-input = 0.
      screen-output = 0.
    ENDIF.
  ENDIF.

ENDFORM.                    " when_desktop
*&---------------------------------------------------------------------*
*&      Form  when_application
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM when_application .

  IF p_prsn EQ 'X'.
    IF screen-group1 = 'UP2'.
      screen-active = 0.
      screen-input = 0.
      screen-output = 0.
    ENDIF.
  ENDIF.

ENDFORM.                    " when_application
*&---------------------------------------------------------------------*
*&      Form  input_file_attributes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_file_attributes .

*>Check input file is not initial
  IF v_desk IS INITIAL.
    v_error = 'X'.
    MOVE text-015 TO v_errormsg.
  ELSE.
    FIND '.' IN v_desk.
    IF sy-subrc <> 0.
      v_error = 'X'.
      MOVE text-021 TO v_errormsg.
    ENDIF.
  ENDIF.

*Validating File Extension entered for Upload
  CHECK v_error NE 'X'.
  v_string = p_desk.

  v_length = STRLEN( v_string ).
  v_length = v_length - 3.
  v_extn = v_string+v_length.
  TRANSLATE v_extn TO UPPER CASE.

  IF v_extn NE 'XLS'.
    v_error = 'X'.
    MOVE text-024 TO v_errormsg.
  ENDIF.

*Checking File Existence on Presentation Server for Upload
  CHECK v_error NE 'X'.
  v_file = p_desk.

  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file            = v_file
    RECEIVING
      result          = v_result
    EXCEPTIONS
      cntl_error      = 1
      error_no_gui    = 2
      wrong_parameter = 3
      OTHERS          = 4.

  IF v_result NE 'X'.
    v_error = 'X'.
    MOVE text-026 TO v_errormsg.
  ENDIF.

*>Check sheet no is not initial
  CHECK v_error NE 'X'.
  IF v_sheetno IS INITIAL.
    v_error = 'X'.
    MOVE text-016 TO v_errormsg.
  ENDIF.

*>Check start row is not initial
  CHECK v_error NE 'X'.
  IF p_frow IS INITIAL.
    v_error = 'X'.
    MOVE text-017 TO v_errormsg.
  ENDIF.

*>Check start column is not initial
  CHECK v_error NE 'X'.
  IF p_fcol IS INITIAL.
    v_error = 'X'.
    MOVE text-018 TO v_errormsg.
  ENDIF.

*>Check end row is not initial
  CHECK v_error NE 'X'.
  IF p_lrow IS INITIAL.
    v_error = 'X'.
    MOVE text-019 TO v_errormsg.
  ENDIF.

*>Check end column is not initial
  CHECK v_error NE 'X'.
  IF p_fcol IS INITIAL.
    v_error = 'X'.
    MOVE text-020 TO v_errormsg.
  ENDIF.

ENDFORM.                    " input_file_attributes
*&---------------------------------------------------------------------*
*&      Form  READ_INPUT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_input_file .

  CLEAR t_output.
  REFRESH t_output.

  IF NOT p_delpa IS INITIAL.
    v_filename = p_delpth.
  ELSE.
    v_filename = v_desk.
  ENDIF.

  CLEAR t_output.
  REFRESH t_output.
*>Load the csv file content
  PERFORM load_csv_file.
*>Load the split the csv file content
  PERFORM process_csv_input.

ENDFORM.                    " READ_INPUT_FILE
*&---------------------------------------------------------------------*
*&      Form  LOAD_CSV_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_csv_file .

  DATA v_file TYPE rlgrap-filename.


  MOVE v_filename TO v_file.
  CLEAR t_input.
  REFRESH t_input.

  IF NOT p_delpa IS INITIAL.
    CALL FUNCTION 'ZFI_EXCEL_TO_RICEF'
      EXPORTING
        filename                = v_file
        i_begin_col             = v_fcol
        i_begin_row             = v_frow
        i_end_col               = v_lcol
        i_end_row               = v_lrow
        sheetname               = v_sheetno
      TABLES
        intern                  = t_input
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2.

  ELSE.
    CALL FUNCTION 'ZFI_EXCEL_TO_RICEF'
      EXPORTING
        filename                = v_file
        i_begin_col             = v_fcol
        i_begin_row             = v_frow
        i_end_col               = v_lcol
        i_end_row               = v_lrow
        sheetname               = v_sheetno
      TABLES
        intern                  = t_input
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2.
  ENDIF.

  IF t_input[] IS INITIAL.
    v_error = 'X'.
    MOVE text-023 TO v_errormsg.
  ENDIF.


ENDFORM.                    " LOAD_CSV_FILE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CSV_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_csv_input .

  FIELD-SYMBOLS <dyn_table> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <dyn_wa>.
  FIELD-SYMBOLS <dyn_field> TYPE ANY.

  DATA v_index TYPE i.
  DATA v_mandt_found.

  ASSIGN d_ref->* TO <dyn_table>.
  CREATE DATA d_ref LIKE LINE OF <dyn_table>.
  ASSIGN d_ref->* TO <dyn_wa>.

  SORT t_dbtab BY position.
  READ TABLE t_dbtab WITH KEY fieldname = 'MANDT'.
  IF sy-subrc = 0.
    v_mandt_found = 'X'.
  ENDIF.

  IF NOT p_delall IS INITIAL.
    DELETE FROM (p_tablen).
  ENDIF.

  SORT t_input BY row col.
  LOOP AT t_input INTO w_input WHERE row > p_header.

    CLEAR t_dbtab.
    READ TABLE t_dbtab WITH KEY fieldname = w_input-value
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.
      CONTINUE.
    ELSE.
      IF w_input-row = 1 OR
         w_input-col > v_nooffields.
        v_error = 'X'.
        MOVE text-030 TO v_errormsg.
        STOP.
      ELSEIF w_input-row > 1.

        CLEAR v_index.
        MOVE w_input-col TO v_index.

        ASSIGN COMPONENT v_index OF STRUCTURE <dyn_wa> TO <dyn_field>.

        IF sy-subrc = 0.
          IF v_index EQ 1 AND v_mandt_found EQ 'X'.
            MOVE w_input-value TO <dyn_field>.
          ELSE.
            MOVE w_input-value TO <dyn_field>.
          ENDIF.
        ENDIF.

        AT END OF row.

          IF v_exceptions EQ 'X'.
            APPEND w_excep TO t_excep.
            CLEAR w_excep.
          ELSE.
            APPEND <dyn_wa> TO <f_fs>.
            CLEAR <dyn_wa>.
          ENDIF.

          v_required = space.
          v_exceptions = space.

        ENDAT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT <f_fs>.
  DELETE ADJACENT DUPLICATES FROM
  <f_fs> COMPARING ALL FIELDS.

  IF NOT p_delpa IS INITIAL.
    DELETE (p_tablen) FROM TABLE <f_fs>.
  ELSE.
    MODIFY (p_tablen) FROM TABLE <f_fs>.
  ENDIF.

  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.


ENDFORM.                    " PROCESS_CSV_INPUT
*&---------------------------------------------------------------------*
*&      Form  print_upload_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_upload_status .

  CLEAR v_outstring.

  MOVE p_desk TO v_outfile.

  CONCATENATE 'Table' p_tablen 'is populated'
              'with input file' v_outfile
              INTO v_outstring
              SEPARATED BY space.

  FORMAT COLOR COL_POSITIVE.
  WRITE:/ v_outstring.

  CLEAR v_filename.
  CLEAR v_outfile.

  SKIP 2.

  FORMAT COLOR COL_NEGATIVE.
  LOOP AT t_excep INTO w_excep.
    WRITE w_excep.
  ENDLOOP.

ENDFORM.                    " print_upload_status

*&---------------------------------------------------------------------*
*&      Form  check_required
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RV_INDEX   text
*----------------------------------------------------------------------*
FORM check_required USING rv_index.

  SORT t_dbtab BY position.
  READ TABLE t_dbtab WITH KEY position = rv_index.
  IF sy-subrc = 0.
    IF t_dbtab-keyflag EQ 'X'.
      v_required = 'X'.
    ELSE.
      v_required = space.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_required
*&---------------------------------------------------------------------*
*&      Form  check_param_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM check_param_table .

*  DATA v_fvalue TYPE zvalue.
*
*  SELECT fvalue UP TO 1 ROWS
*        INTO v_fvalue
*        FROM zparams
*       WHERE pname  = 'ZGLBC_DYNAMIC_TOOL'
*         AND fvalue = p_tablen.
*
*  ENDSELECT.
*
*  IF NOT v_fvalue EQ p_tablen.
*    v_error = 'X'.
*    v_errormsg = text-022.
*  ENDIF.

*ENDFORM.                    " check_param_table
*&---------------------------------------------------------------------*
*&      Form  when_deletion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM when_deletion .

*>If Deletion Radio Box is Enabld, Hide Upload and Download Parameters
  IF p_delt EQ 'X'.
    IF screen-group1 = 'DN'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
    IF screen-group1 = 'UP'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
    IF screen-group1 = 'UP1'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
    IF screen-group1 = 'UP2'.
      screen-active  = 0.
      screen-input   = 0.
      screen-output  = 0.
    ENDIF.
    IF p_delpa NE 'X'.
      IF screen-group1 = 'DL1'.
        screen-active  = 0.
        screen-input   = 0.
        screen-output  = 0.
      ENDIF.
    ENDIF.
    IF p_chkbox NE 'X'.
      IF screen-group1 = 'DL2'.
        screen-active  = 0.
        screen-input   = 0.
        screen-output  = 0.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " when_deletion
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data .

  IF NOT p_chkbox IS INITIAL.
    IF p_bckpth EQ space.
      get_filename p_tablen.
      MOVE v_filename TO p_bckpth.
      CLEAR v_filename.
      PERFORM download_data.
    ELSE.
      get_filename p_tablen.
      MOVE v_filename TO p_bckpth.
      CLEAR v_filename.
      PERFORM download_data.
    ENDIF.
  ENDIF.

  IF NOT p_delpa IS INITIAL.
    PERFORM upload_data.
  ENDIF.

  IF p_dlall = 'X' AND
     v_answer = 'J'.
    DELETE FROM (p_tablen).
  ENDIF.

ENDFORM.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  when_partial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM when_partial .
  IF p_delpa EQ 'X'.
    IF screen-group1 = 'DL1'.
      screen-active  = 1.
      screen-input = 1.
      screen-output = 1.
    ENDIF.
  ENDIF.

ENDFORM.                    " when_partial
*&---------------------------------------------------------------------*
*&      Form  when_backup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM when_backup .

  IF p_chkbox EQ 'X'.
    IF screen-group1 = 'DL2'.
      screen-active  = 1.
      screen-input   = 1.
      screen-output = 1.
    ENDIF.
  ENDIF.

ENDFORM.                    " when_backup
*&---------------------------------------------------------------------*
*&      Form  get_file_bckpth
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file_bckpth .

  CLEAR v_folder.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = 'Select Only File Path'
    CHANGING
      selected_folder      = v_folder
*  EXCEPTIONS
*    CNTL_ERROR           = 1
*    ERROR_NO_GUI         = 2
*    NOT_SUPPORTED_BY_GUI = 3
*    others               = 4
          .
  IF sy-subrc <> 0.
*   Do nothing
  ENDIF.

  p_bckpth = v_folder.


ENDFORM.                    " get_file_bckpth
*&---------------------------------------------------------------------*
*&      Form  get_file_delpth
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file_delpth .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = p_delpth
      def_path         = '/'
      mask             = ',*.*,*.*.'
      mode             = 'O'
      title            = 'Get filename'
    IMPORTING
      filename         = p_delpth
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

ENDFORM.                    " get_file_delpth
*&---------------------------------------------------------------------*
*&      Form  input_file_attributes_del
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_file_attributes_del .

  IF NOT p_delpa IS INITIAL.
*>Check input file is not initial
    IF p_delpth IS INITIAL.
      v_error = 'X'.
      MOVE text-015 TO v_errormsg.
    ELSE.
      FIND '.' IN p_delpth.
      IF sy-subrc <> 0.
        v_error = 'X'.
        MOVE text-021 TO v_errormsg.
      ENDIF.
    ENDIF.

*Validating File Extension entered for Deletion
    CHECK v_error NE 'X'.

    v_string = p_delpth.

    v_length = STRLEN( v_string ).
    v_length = v_length - 3.
    v_extn = v_string+v_length.
    TRANSLATE v_extn TO UPPER CASE.

    IF v_extn NE 'XLS'.
      v_error = 'X'.
      MOVE text-024 TO v_errormsg.
    ENDIF.

*Checking File Existence on Presentation Server for Deletion
    CHECK v_error NE 'X'.

    v_file = p_delpth.

    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file            = v_file
      RECEIVING
        result          = v_result
      EXCEPTIONS
        cntl_error      = 1
        error_no_gui    = 2
        wrong_parameter = 3
        OTHERS          = 4.

    IF v_result NE 'X'.
      v_error = 'X'.
      MOVE text-026 TO v_errormsg.
    ENDIF.
  ENDIF.

ENDFORM.                    " input_file_attributes_del
*&---------------------------------------------------------------------*
*&      Form  print_delete_all_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_delete_all_status .

  CLEAR v_outstring.

  IF NOT p_chkbox IS INITIAL.
    CONCATENATE 'All Data from' p_tablen 'is deleted,' 'Backup taken.'
                INTO v_outstring
                SEPARATED BY space.
  ELSE.
    CONCATENATE 'All Data from' p_tablen 'is deleted'
                INTO v_outstring
                SEPARATED BY space.
  ENDIF.

  FORMAT COLOR COL_POSITIVE.
  WRITE:/ v_outstring.

  CLEAR v_filename.
  CLEAR t_output.
  CLEAR v_filename.

ENDFORM.                    " print_delete_all_status
*&---------------------------------------------------------------------*
*&      Form  print_delete_par_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_delete_par_status .

  CLEAR v_outstring.

  MOVE p_delpth TO v_outfile.

  IF NOT p_chkbox IS INITIAL.
    CONCATENATE 'Table' p_tablen 'is deleted'
                'from input file' v_outfile '.' 'Backup taken.'
                INTO v_outstring
                SEPARATED BY space.
  ELSE.
    CONCATENATE 'Table' p_tablen 'is deleted'
                'from input file' v_outfile
                INTO v_outstring
                SEPARATED BY space.
  ENDIF.

  FORMAT COLOR COL_POSITIVE.
  WRITE:/ v_outstring.

  CLEAR v_filename.
  CLEAR v_outfile.

  SKIP 2.

  FORMAT COLOR COL_NEGATIVE.
  LOOP AT t_excep INTO w_excep.
    WRITE w_excep.
  ENDLOOP.

ENDFORM.                    " print_delete_par_status
*&---------------------------------------------------------------------*
*&      Form  get_client
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_client .

  v_tablen = p_tablen.

  CALL FUNCTION 'DDIF_TABL_GET'
    EXPORTING
      name      = v_tablen
      state     = 'A'
    IMPORTING
      dd02v_wa  = st_dd02v_wa
    TABLES
      dd03p_tab = i_dd03p_tab.

  IF sy-subrc <> 0.
*   Do nothing
  ENDIF.

  v_clidep = st_dd02v_wa-clidep.


ENDFORM.                    " get_client
