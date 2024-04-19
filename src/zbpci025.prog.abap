*&---------------------------------------------------------------------*
*& Report  ZPPMI022
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zbpci025.

TABLES: bpja,
        tbp3a,
        prps.

TYPES: BEGIN OF typ_output,
         gjahr  TYPE char8,
         comma1 TYPE c,
         posid  TYPE char24,
         comma2 TYPE c,
         wtjhr  TYPE char15,
       END OF typ_output.
DATA: BEGIN OF gs_data,
         gjahr  TYPE gjahr,
         posid  TYPE prps-posid,
         wtjhr  TYPE bp_wjt,
       END OF gs_data.
DATA:BEGIN OF gt_tab OCCURS 0,  "Text file format
           text1(208),
     END OF gt_tab.
CONSTANTS: gc_app     TYPE dxfields-location VALUE 'A'.
DATA: gt_output TYPE TABLE OF typ_output,
      gs_output TYPE typ_output,
      gt_data   LIKE HASHED TABLE OF gs_data WITH UNIQUE KEY posid gjahr,
      gt_bpja   TYPE TABLE OF bpja,
      gs_bpja   TYPE bpja,
      gv_path   TYPE dxfields-longpath.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_pbukr FOR prps-pbukr DEFAULT 'UGL' OBLIGATORY,
                s_gjahr FOR bpja-gjahr OBLIGATORY,
                s_objnr FOR bpja-objnr NO-DISPLAY.
PARAMETERS: p_wrttp TYPE bpja-wrttp DEFAULT '41' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: r_pc RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmd,
            p_file TYPE rfpdo-lboxfile DEFAULT 'H:\SAPTEMP\', " OBLIGATORY.
            r_server RADIOBUTTON GROUP rad2,
            p_sfile TYPE rfpdo-rfbifile, " OBLIGATORY..
            c_tfile as CHECKBOX,
            p_tcfile TYPE rfpdo-rfbifile.
SELECTION-SCREEN END OF BLOCK b2.
***************
AT SELECTION-SCREEN OUTPUT.
  PERFORM pbo_data.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  PERFORM get_file.
  PERFORM get_pc_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sfile.
  PERFORM get_server_file.
******************
INITIALIZATION.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/capex/' INTO p_sfile.
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/BPC/capex/capex.tch' INTO p_tcfile.
******************
START-OF-SELECTION.

CONCATENATE p_sfile 'SAP-' s_gjahr-low '-Req.csv' INTO p_sfile.

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

  PERFORM selection_validation.
  CLEAR: gt_output,
         gt_data,
         gt_bpja.

  PERFORM get_data.

  IF r_server = 'X'. "application server
    IF gt_output[] IS NOT INITIAL. " internal table for data
*Downloading of text file to Application server.
      PERFORM as_downloading_file.
    ENDIF.
  ELSE.  "Presentation Server
    IF gt_output IS NOT INITIAL.
      PERFORM pc_download_file.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  AS_DOWNLOADING_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM as_downloading_file .
  OPEN DATASET p_sfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open text file for output., Check path/security'.
  ELSE.
    LOOP AT gt_output INTO gs_output.
      TRANSFER gs_output TO p_sfile.
    ENDLOOP.
    CLOSE DATASET p_sfile.
    IF sy-subrc = 0.
        WRITE: / 'File successfully created at ', p_sfile.
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
  ENDIF.
ENDFORM.                    " AS_DOWNLOADING_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_prps TYPE TABLE OF prps,
        ls_prps TYPE prps,
        lt_bpja TYPE TABLE OF bpja.

  CLEAR s_objnr.
  REFRESH s_objnr.

  s_objnr-sign = 'I'.
  s_objnr-option = 'CP'.
  s_objnr-low = 'PR*'.      "only projects
  APPEND s_objnr.

  SELECT * FROM bpja INTO TABLE gt_bpja
     WHERE objnr IN s_objnr
      AND gjahr IN s_gjahr
      AND wrttp = p_wrttp.
*    WHERE lednr IN s_lednr
*      AND objnr IN s_objnr
*      AND gjahr IN s_gjahr
*      AND versn IN s_versn
*      AND WRTTP = p_WRTTP.

  IF gt_bpja[] IS INITIAL.
    WRITE : / 'No data to output..'.
    STOP.
  ENDIF.

  lt_bpja[] = gt_bpja[].
  SORT lt_bpja BY objnr.
  DELETE ADJACENT DUPLICATES FROM lt_bpja COMPARING objnr.
  IF lt_bpja IS NOT INITIAL.
    SELECT * FROM prps INTO TABLE lt_prps
      FOR ALL ENTRIES IN lt_bpja
      WHERE objnr = lt_bpja-objnr.
  ENDIF.
  LOOP AT gt_bpja INTO gs_bpja.
    CLEAR ls_prps.
    READ TABLE lt_prps INTO ls_prps
         WITH KEY objnr = gs_bpja-objnr.
*get only level 1 in project hierarchy.
    IF ls_prps-stufe <> 1.
      CONTINUE.
    ENDIF.
    IF s_pbukr IS NOT INITIAL.
      IF ls_prps-pbukr NOT IN s_pbukr.
        CONTINUE.
      ENDIF.
    ENDIF.
    gs_data-gjahr  = gs_bpja-gjahr.
    gs_data-posid  = ls_prps-posid.
    gs_data-wtjhr  = gs_bpja-wtjhr.
    COLLECT gs_data INTO gt_data.
  ENDLOOP.
  SORT gt_data BY posid gjahr.
  LOOP AT gt_data INTO gs_data.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        input  = gs_data-posid
      IMPORTING
        output = gs_data-posid.
    CONCATENATE gs_data-gjahr '1200' INTO gs_output-gjahr.
    gs_output-comma1 = ','.
    gs_output-posid  = gs_data-posid.
    REPLACE ALL OCCURRENCES OF '-' IN gs_output-posid WITH '.'.
    gs_output-comma2 = ','.
    gs_output-wtjhr  = gs_data-wtjhr.
    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
         CHANGING
         value         = gs_output-wtjhr.
    CONDENSE gs_output-wtjhr NO-GAPS.
    SHIFT gs_output-wtjhr RIGHT.
    APPEND gs_output TO gt_output.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PC_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pc_download_file .

  DATA: lv_filename TYPE string.

*Download Canadian file
  MOVE p_file TO lv_filename.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
      trunc_trailing_blanks_eol = space "'X'
    CHANGING
      data_tab                  = gt_output
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
    WRITE: / 'Error with downloading file at PC ', sy-subrc.
  ELSE.
    WRITE: / 'File successfully created at ', p_file.
  ENDIF.

ENDFORM.                    " PC_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  PBO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pbo_data .
*IF r_pc = 'X'.
*   p_file = 'h:\'.
*else.
*   clear p_file.
*ENDIF.

ENDFORM.                    " PBO_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECTION_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selection_validation .

*if s_gjahr is INITIAL.
*   write: / 'Enter Year on selection screen..'.
*   stop.
*endif.
*if s_versn is INITIAL.
*   write: / 'Enter Version on selection screen..'.
*   stop.
*endif.
  IF ( r_pc = 'X' AND p_file IS INITIAL ) OR
     ( r_server = 'X' AND p_sfile IS INITIAL ).
    WRITE: / 'Enter File Name & Path on selection screen..'.
    STOP.
  ENDIF.
ENDFORM.                    " SELECTION_VALIDATION
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
*&---------------------------------------------------------------------*
*&      Form  GET_LEDNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_lednr_low.

  DATA: lt_tbp0l TYPE TABLE OF tbp0l,
        lt_return TYPE STANDARD TABLE OF ddshretval WITH HEADER LINE.

  SELECT * FROM tbp0l INTO TABLE lt_tbp0l.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LEDNR'
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'S_LEDNR-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = lt_tbp0l
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_LEDNR
*&---------------------------------------------------------------------*
*&      Form  GET_LEDNR_HIGH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_lednr_high .

  DATA: lt_tbp0l TYPE TABLE OF tbp0l,
        lt_return TYPE STANDARD TABLE OF ddshretval WITH HEADER LINE.

  SELECT * FROM tbp0l INTO TABLE lt_tbp0l.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LEDNR'
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'S_LEDNR-HIGH'
      value_org       = 'S'
    TABLES
      value_tab       = lt_tbp0l
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_LEDNR_HIGH
*&---------------------------------------------------------------------*
*&      Form  GET_VERSN_LOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_versn_low .
  TYPES: BEGIN OF ty_versn,
         versn TYPE char3,
         END OF ty_versn.
  DATA: lt_versn TYPE TABLE OF ty_versn,
        lt_return TYPE STANDARD TABLE OF ddshretval WITH HEADER LINE.

  SELECT DISTINCT versn  FROM bpja INTO TABLE lt_versn.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VERSN'
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'S_VERSN-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = lt_versn
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " GET_VERSN_LOW
*&---------------------------------------------------------------------*
*&      Form  GET_VERSN_HIGH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_versn_high .

  TYPES: BEGIN OF ty_versn,
          versn TYPE char3,
          END OF ty_versn.
  DATA: lt_versn TYPE TABLE OF ty_versn,
        lt_return TYPE STANDARD TABLE OF ddshretval WITH HEADER LINE.

  SELECT DISTINCT versn  FROM bpja INTO TABLE lt_versn.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VERSN'
      dynpprog        = sy-repid
      dynpnr          = '1000'
      dynprofield     = 'S_VERSN-HIGH'
      value_org       = 'S'
    TABLES
      value_tab       = lt_versn
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_VERSN_HIGH
