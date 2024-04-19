class ZCL_IAP_INTERFACE_UTIL definition
  public
  create public .

public section.
*"* public components of class ZCL_IAP_INTERFACE_UTIL
*"* do not include other source files here!!!

  class-methods GET_DAT_FILENAME
    importing
      !IM_LFILE type C
      !IM_PARAM1 type ANY optional
      !IM_PARAM2 type ANY optional
    returning
      value(RE_FILENAME) type STRING .
  class-methods DOWNLOAD_FILE_PC
    importing
      !IM_FILENAME type ANY
      !IM_DATA_TAB type TABLE
      !IM_DELIM type FLAG default SPACE
    raising
      CX_SY_FILE_IO .
  class-methods F4_SERVERFILE
    returning
      value(RE_FILE) type IBIPPARMS-PATH .
  class-methods ADD_PIPES
    importing
      !IM_REC type ANY
      !IM_DELIM type CHAR02 default '|~'
    exporting
      !EX_OUTREC type ANY .
  class-methods WRITE_FILE_SERVER
    importing
      !IM_FILENAME type ANY
      !IM_DATA_TAB type TABLE
    raising
      CX_SY_FILE_IO .
  type-pools SLIS .
  class-methods BUILD_FIELDCAT_FROM_STRUC
    importing
      !IM_STRUCTURE type DD02L-TABNAME
    returning
      value(RE_FIELDCAT) type SLIS_T_FIELDCAT_ALV .
  class-methods DISPLAY_TBL_IN_ALV
    importing
      !IM_REPID type SY-REPID default SY-CPROG
      !IM_STRUC_NAME type DD02L-TABNAME
      !IM_PF_STATUS_SET type SLIS_FORMNAME optional
      !IM_USER_COMMAND type SLIS_FORMNAME optional
      !IM_LAYOUT type SLIS_LAYOUT_ALV optional
      !IM_TITLE type C optional
      !IM_VARIANT type DISVARIANT optional
    changing
      !IM_DATA_TAB type TABLE .
protected section.
*"* protected components of class ZCL_IAP_INTERFACE_UTIL
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IAP_INTERFACE_UTIL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IAP_INTERFACE_UTIL IMPLEMENTATION.


method ADD_PIPES.

  DATA:
    lv_fields TYPE i,
    lv_type(1).

  FIELD-SYMBOLS:
    <fld>  TYPE  ANY.

  DESCRIBE FIELD im_rec TYPE lv_type COMPONENTS lv_fields.
  DO lv_fields TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE im_rec TO <fld>.
    IF sy-index = 1.
      ex_outrec = <fld>.
    ELSE.
      CONCATENATE ex_outrec  <fld>
             INTO ex_outrec SEPARATED BY im_delim.
    ENDIF.
  ENDDO.
endmethod.


method BUILD_FIELDCAT_FROM_STRUC.
  DATA:
    ls_fieldcat            TYPE slis_fieldcat_alv.


* Generate ALV field catalog
  REFRESH re_fieldcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = im_structure
    CHANGING
      ct_fieldcat            = re_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc NE 0.
    MESSAGE e016(rp) WITH 'Error generating ALV field catalog'.
  ENDIF.

* Get text from structure
  LOOP AT re_fieldcat INTO ls_fieldcat.
    SELECT SINGLE ddtext
     FROM  dd03t
     INTO  ls_fieldcat-seltext_l
     WHERE tabname = im_structure
      AND  as4local = 'A'
      AND  ddlanguage = sy-langu
      AND  fieldname = ls_fieldcat-fieldname.
    MODIFY re_fieldcat FROM ls_fieldcat.
  ENDLOOP.

endmethod.


method DISPLAY_TBL_IN_ALV.

* Use this method to display an internal table in the ALV grid.
* The table must defined with a structure, and pass that structure
* into this method.

  TYPE-POOLS: slis.            " ABAP List Viewer Data Definition

  DATA: lv_callback_program    TYPE sy-repid,
        ls_layout              TYPE slis_layout_alv,
        lt_fieldcat            TYPE slis_t_fieldcat_alv,
        lv_grid_title          TYPE lvc_title,
        ls_print               TYPE slis_print_alv.

  lv_callback_program = im_repid.
  lv_grid_title = im_title.

* ALV layout settings
  ls_layout = im_layout.
  ls_layout-colwidth_optimize = 'X'.

* define print settings
  ls_print-no_print_listinfos = 'X'."dont list sort/subtotal/filter

* Generate ALV field catalog
  lt_fieldcat = build_fieldcat_from_struc( im_struc_name ).

* Display report with ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = lv_callback_program
      is_layout                = ls_layout
      it_fieldcat              = lt_fieldcat
      i_callback_pf_status_set = im_pf_status_set
      i_callback_user_command  = im_user_command
      i_grid_title             = lv_grid_title
      i_save                   = 'A'
      is_print                 = ls_print
      is_variant               = im_variant
    TABLES
      t_outtab                 = im_data_tab
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc NE 0.
    MESSAGE e016(rp) WITH 'Error displaying report in ALV'.
  ENDIF.

endmethod.


METHOD download_file_pc.
  DATA:
    lv_filename TYPE string.

  lv_filename = im_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
      write_field_separator   = im_delim
    TABLES
      data_tab                = im_data_tab
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc NE 0.
    RAISE EXCEPTION TYPE cx_sy_file_io.
  ENDIF.

ENDMETHOD.


method F4_SERVERFILE.
  data : lv_dirname TYPE dirname.

*" Fetch the directory name to be used
SELECT SINGLE dirname
  from user_dir
  into lv_dirname
  where aliass eq 'Z_IAP_FOLDER'.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    EXPORTING

*\      directory        = ' '
      directory        = lv_dirname "'D:\ '
*\      filemask         = ' '
      filemask         = '*.*'
    IMPORTING
      serverfile       = re_file
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
endmethod.


method GET_DAT_FILENAME.
  DATA:
    lv_logical_filename TYPE filename-fileintern.

  lv_logical_filename = im_lfile.


* Lookup logical file path
  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename            = lv_logical_filename
    parameter_1                   = im_param1
    parameter_2                   = im_param2
*   PARAMETER_3                   = ' '
    IMPORTING
      file_name                   = re_filename
    EXCEPTIONS
      file_not_found                = 1
      OTHERS                        = 2.
endmethod.


method WRITE_FILE_SERVER.
DATA: lv_file TYPE string.

FIELD-SYMBOLS: <f_t_table> TYPE ANY.

  lv_file = im_filename.
*  IF im_binary_mode IS NOT INITIAL.
*    OPEN DATASET lv_file FOR OUTPUT IN BINARY MODE.
*  ELSE.
    OPEN DATASET lv_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
*  ENDIF.
  IF sy-subrc EQ 0.
    LOOP AT im_data_tab ASSIGNING <f_t_table> .
      TRANSFER <f_t_table> TO lv_file.
    ENDLOOP.
    CLOSE DATASET lv_file.
  ENDIF.
  IF sy-subrc = 1.
    RAISE EXCEPTION TYPE cx_sy_file_io.
  ENDIF.
endmethod.
ENDCLASS.
