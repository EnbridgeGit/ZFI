*&---------------------------------------------------------------------*
*& Report  ZFI_DOA_READ_LOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfi_doa_read_load..

FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE.

DATA:  table_ref TYPE REF TO data,
       lv_file   TYPE string.

PARAMETERS: p_tab TYPE dd02l-tabname DEFAULT 'ZFIT_DOA'. " Input Table Name
PARAMETERS: p_fname TYPE string.                         " File Path to write/read file into PC folder

PARAMETERS: p_upl TYPE c RADIOBUTTON GROUP r1,    " Read data from file - GUI_UPLOAD
            p_downl TYPE c RADIOBUTTON GROUP r1.  " Write data into a file - GUI_DOWNLOAD

* Browse to select a file to read/write
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM browse_file USING p_fname.


START-OF-SELECTION.

* Create dynamic table table from the table name passed in parameter
  PERFORM create_dynamic_itab.

* Write file, read file , load data into custom table
  PERFORM write_read_load.

*&---------------------------------------------------------------------*
*&      Form  BROWSE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FNAME  text
*----------------------------------------------------------------------*
FORM browse_file  USING p_fname TYPE any.

  DATA: lt_file TYPE  filetable,
        ls_file LIKE LINE OF lt_file,
        rc TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select a File'
      default_extension       = '.txt'
      file_filter             = '.txt'
      initial_directory       = 'H:\Test Data'
    CHANGING
      file_table              = lt_file
      rc                      = rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc EQ 0.
    READ TABLE lt_file INTO ls_file INDEX 1.
    IF sy-subrc EQ 0.
      p_fname = ls_file-filename.
    ENDIF.
  ENDIF.

ENDFORM.                    " BROWSE_FILE
*&---------------------------------------------------------------------*
*&      Form  WRITE_READ_LOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_read_load .

  DATA: lv_ln TYPE i.

  IF p_downl EQ 'X'.
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename            = p_fname
        filetype            = 'ASC'
        has_field_separator = 'X'
      TABLES
        data_tab            = <fs_table>.
    IF sy-subrc EQ 0.
      lv_ln = lines( <fs_table> ).
      MODIFY (p_tab) FROM TABLE <fs_table>.
      COMMIT WORK.
      IF sy-subrc EQ 0.
        WRITE:/ lv_ln, ' Records loaded into the table'.
      ENDIF.
    ENDIF.
  ELSE.

    SELECT * INTO TABLE <fs_table> FROM (p_tab).
    IF sy-subrc EQ 0.
*Download the internal table data into a file in PC
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename              = p_fname
          filetype              = 'ASC'
          write_field_separator = 'X'
        TABLES
          data_tab              = <fs_table>.
      IF sy-subrc EQ 0.
        lv_ln = lines( <fs_table> ).
        WRITE:/ lv_ln, ' Records written'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " WRITE_READ_LOAD
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_dynamic_itab .

  CREATE DATA table_ref TYPE STANDARD TABLE OF (p_tab).
  ASSIGN table_ref->* TO <fs_table>.
*  CREATE DATA wa_ref LIKE LINE OF <fs_table>.
*  ASSIGN wa_ref->* TO <fs_wa>.

ENDFORM.                    " CREATE_DYNAMIC_ITAB
