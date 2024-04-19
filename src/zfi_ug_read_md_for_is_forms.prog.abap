*&---------------------------------------------------------------------*
*&  Include           ZFI_US_READ_MD_FOR_IS_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fcat USING p_tabname TYPE any
                      pt_fcat   TYPE STANDARD TABLE.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_tabname
    CHANGING
      ct_fieldcat            = pt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.                    " BUILD_FCAT

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_report USING p_tab   TYPE any
                              pt_data TYPE STANDARD TABLE.

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.

  PERFORM build_fcat USING p_tab lt_fieldcat[].
  PERFORM build_layout.
  PERFORM execute_alv USING lt_fieldcat[] pt_data[].

ENDFORM.                    " DISPLAY_ALV_REPORT

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .

  gs_layout-no_input          = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-totals_text       = 'Totals'(201).
  gs_layout-zebra             = 'X'.

ENDFORM.                    " BUILD_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_FIELDCAT[]  text
*      -->P_PT_DATA[]  text
*----------------------------------------------------------------------*
FORM execute_alv USING pt_fieldcat TYPE STANDARD TABLE
                       pt_data     TYPE STANDARD TABLE.

  DATA: wa_sort  TYPE slis_sortinfo_alv,
        gv_repid TYPE sy-repid.

  gv_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = gs_layout
      it_fieldcat        = pt_fieldcat
      i_save             = 'X'
    TABLES
      t_outtab           = pt_data
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    WRITE: /'Alv report could not be generated'.
  ENDIF.

ENDFORM.                    " EXECUTE_ALV
*&---------------------------------------------------------------------*
*&      Form  SPLIT_ALV_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM split_alv_container .

  DATA: lt_fcat TYPE lvc_t_fcat,
        ls_layo TYPE lvc_s_layo.

  DATA: splitter   TYPE REF TO cl_gui_splitter_container,
        parent1    TYPE REF TO cl_gui_container,
        ref_grid1  TYPE REF TO cl_gui_alv_grid,
        lv_tabname TYPE tabname.

  lv_tabname = p_tabs.
  PERFORM set_container_grid   USING '0' '0' splitter parent1 ref_grid1.

  IF "lv_tabname NE 'AUSP' AND
     lv_tabname NE 'AUSP+MARA'.
    IF <gt_data> IS ASSIGNED.
      PERFORM fcatalog_alv_display USING lv_tabname <gt_data>[] ref_grid1.
    ENDIF.
  ELSE.
    IF lv_tabname EQ 'AUSP'.
      PERFORM fcatalog_alv_display USING lv_tabname gt_mat_charac[] ref_grid1.
    ELSEIF lv_tabname EQ 'AUSP+MARA'.
      PERFORM fcatalog_alv_display USING lv_tabname gt_mara_ausp[] ref_grid1.
    ENDIF.
  ENDIF.

ENDFORM.                    " SPLIT_ALV_CONTAINER
*&---------------------------------------------------------------------*
*&      Module  STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9010 OUTPUT.

  SET PF-STATUS 'ZFI_IS'.
  SET TITLEBAR  'ZFI_IST'.

  PERFORM split_alv_container.

ENDMODULE.                 " STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9010 INPUT.

  CALL METHOD cl_gui_cfw=>dispatch.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CALL METHOD cl_gui_cfw=>flush.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      IF gr_container IS BOUND.
        CALL METHOD gr_container->free.
      ENDIF.
      CALL METHOD cl_gui_cfw=>flush.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9010  INPUT


*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG_MERGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1034   text
*      <--P_LT_FCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcatalog_merge  USING    p_tabname TYPE any
                         CHANGING pt_fcat   TYPE STANDARD TABLE.

  DATA: ls_fcat  TYPE LINE OF lvc_t_fcat,
        lv_lines TYPE i.

  IF "p_tabname NE 'AUSP' AND
     p_tabname NE 'AUSP+MARA'.
    PERFORM get_fld_catalog USING p_tabname CHANGING pt_fcat[].
  ELSE.
    IF p_tabname EQ 'AUSP'.
      ls_fcat-fieldname = 'MATNR'.
      ls_fcat-col_pos   = 1.
      ls_fcat-scrtext_l = 'Material'.
      ls_fcat-scrtext_m = 'Material'.
      ls_fcat-scrtext_s = 'Material'.
      APPEND ls_fcat TO pt_fcat.

      ls_fcat-fieldname = 'ATBEZ'.
      ls_fcat-col_pos   = 2.
      ls_fcat-scrtext_l = 'Attribute 1'.
      ls_fcat-scrtext_m = 'Attribute 1'.
      ls_fcat-scrtext_s = 'Attribute 1'.
      APPEND ls_fcat TO pt_fcat.

      ls_fcat-fieldname = 'ATWRT'.
      ls_fcat-col_pos   = 3.
      ls_fcat-scrtext_l = 'Attribute 2'.
      ls_fcat-scrtext_m = 'Attribute 2'.
      ls_fcat-scrtext_s = 'Attribute 2'.
      APPEND ls_fcat TO pt_fcat.

    ELSEIF p_tabname EQ 'AUSP+MARA'.
      PERFORM get_fld_catalog USING 'MARA' CHANGING pt_fcat[].
      lv_lines = lines( pt_fcat ).
      ls_fcat-fieldname = 'ATBEZ'.
      ls_fcat-scrtext_l = 'Attribute 1'.
      ls_fcat-scrtext_m = 'Attribute 1'.
      ls_fcat-scrtext_s = 'Attribute 1'.
      lv_lines          = lv_lines + 1.
      ls_fcat-col_pos   = lv_lines.
      APPEND ls_fcat TO pt_fcat.

      ls_fcat-fieldname = 'ATWRT'.
      ls_fcat-scrtext_l = 'Attribute 2'.
      ls_fcat-scrtext_m = 'Attribute 2'.
      ls_fcat-scrtext_s = 'Attribute 2'.
      lv_lines          = lv_lines + 1.
      ls_fcat-col_pos   = lv_lines.
      APPEND ls_fcat TO pt_fcat.
    ENDIF.
  ENDIF.

ENDFORM.                    " FIELDCATALOG_MERGE
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LAYO  text
*      -->P_GT_LFA1[]  text
*      -->P_LT_FCAT[]  text
*----------------------------------------------------------------------*
FORM set_alv_display  USING    p_grid  TYPE REF TO cl_gui_alv_grid
                               ps_layo TYPE any
                               pt_data TYPE STANDARD TABLE
                               pt_fcat TYPE STANDARD TABLE.

  CALL METHOD p_grid->set_table_for_first_display
    EXPORTING
      is_layout       = ps_layo
    CHANGING
      it_outtab       = pt_data[]
      it_fieldcatalog = pt_fcat.

ENDFORM.                    " SET_ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  GET_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0999   text
*      -->P_1000   text
*      -->P_GRAPHIC_PARENT1  text
*----------------------------------------------------------------------*
FORM get_container  USING  p_row       TYPE any
                           p_col       TYPE any
                           p_splitter  TYPE REF TO cl_gui_splitter_container
                           p_container TYPE REF TO cl_gui_container
                           p_grid      TYPE REF TO cl_gui_alv_grid.

*  CALL METHOD P_SPLITTER->GET_CONTAINER
*    EXPORTING
*      ROW       = P_ROW
*      COLUMN    = P_COL
*    RECEIVING
*      CONTAINER = P_CONTAINER.
*
*  CREATE OBJECT P_GRID
*    EXPORTING
*      I_PARENT = GRAPHIC_PARENT1.

ENDFORM.                    " GET_CONTAINER

*&---------------------------------------------------------------------*
*&      Form  SET_CONTAINER_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPLITTER  text
*      -->P_1912   text
*      -->P_1913   text
*      -->P_1914   text
*      -->P_GT_LFA1[]  text
*----------------------------------------------------------------------*
FORM set_container_grid  USING  p_row       TYPE any
                           p_col       TYPE any
                           p_splitter  TYPE REF TO cl_gui_splitter_container
                           p_container TYPE REF TO cl_gui_container
                           p_grid      TYPE REF TO cl_gui_alv_grid.

  IF p_row NE 0.
    CALL METHOD p_splitter->get_container
      EXPORTING
        row       = p_row
        column    = p_col
      RECEIVING
        container = p_container.

    CREATE OBJECT p_grid
      EXPORTING
        i_parent = p_container.
  ELSE.
    CREATE OBJECT p_grid
      EXPORTING
        i_parent = cl_gui_container=>default_screen.
  ENDIF.

ENDFORM.                    " SET_CONTAINER_GRID
*&---------------------------------------------------------------------*
*&      Form  FCATALOG_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1919   text
*      -->P_GT_LFA1[]  text
*      -->P_1921   text
*----------------------------------------------------------------------*
FORM fcatalog_alv_display  USING p_tabname  TYPE any
                                 pt_data    TYPE STANDARD TABLE
                                 p_grid     TYPE REF TO cl_gui_alv_grid.

  DATA: lt_fcat  TYPE lvc_t_fcat,
        ls_layo  TYPE lvc_s_layo,
        lv_lines(15) TYPE n.

  FIELD-SYMBOLS: <list> LIKE LINE OF gt_list.

  PERFORM fieldcatalog_merge USING p_tabname CHANGING lt_fcat[].

  READ TABLE gt_list ASSIGNING <list> WITH KEY key = p_tabname.
  IF sy-subrc EQ 0.
    CONCATENATE <list>-text '- ' p_tabname INTO ls_layo-grid_title.
  ELSE.
    ls_layo-grid_title = p_tabname.
  ENDIF.
  ls_layo-zebra      = 'X'.
  lv_lines           = lines( pt_data ).
  SHIFT lv_lines LEFT DELETING LEADING '0'.
  CONCATENATE ls_layo-grid_title space '(' lv_lines ')' INTO ls_layo-grid_title.

  IF "p_tabname NE 'AUSP' AND
     p_tabname NE 'AUSP+MARA' .
    PERFORM set_alv_display USING p_grid ls_layo pt_data[] lt_fcat[].
  ELSE.
    IF p_tabname EQ 'AUSP'.
      PERFORM set_alv_display USING p_grid ls_layo gt_mat_charac[] lt_fcat[].
    ELSEIF p_tabname EQ 'AUSP+MARA'.
      PERFORM set_alv_display USING p_grid ls_layo gt_mara_ausp[] lt_fcat[].
    ENDIF.
  ENDIF.

ENDFORM.                    " FCATALOG_ALV_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen .

*  LOOP AT SCREEN.
*    CASE SCREEN-GROUP1.
*      WHEN 'VEN'.
*        IF CB_LFAB = 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'PRJ'.
*        IF CB_PROJ EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'WBS'.
*        IF CB_PRPS EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'WO'.
*        IF CB_AUFK EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'LOC'.
*        IF CB_T499S EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'BNK'.
*        IF CB_BNKA EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'KNA'.
*        IF CB_KNA1 EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'INV'.
*        IF CB_T001L EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'MAT'.
*        IF CB_MARA EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'LHM'.
*        IF CB_T001W EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'AH'.
*        IF CB_ANAH EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'EQ'. " Equipment master
*        IF CB_EQUI EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'RD2'.
*        IF RD_FILE EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'RD4'.  "PC file
*        IF RD_FILE EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'RD5'.  "Application server file
*        IF RD_FILE EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'RD6'.  "PC file
*        IF RD_PC EQ 'X' AND RD_FILE EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'RD7'.  " App Server file
*        IF RD_APP EQ 'X' AND RD_FILE EQ 'X'.
*          SCREEN-INPUT = 1.       "Visible
*          SCREEN-INVISIBLE = 0.
*          MODIFY SCREEN.
*        ELSE.
*          SCREEN-INPUT = 0.       "Hidden
*          SCREEN-INVISIBLE = 1.
*          MODIFY SCREEN.
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN

*&---------------------------------------------------------------------*
*&      Form  CREATE_SPLITTER_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CUSTOM_CONTAINER  text
*      -->P_SPLITTER  text
*----------------------------------------------------------------------*
FORM create_splitter_container USING "P_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER
                                     p_splitter  TYPE REF TO cl_gui_splitter_container.

**   create container in which to place splitter
**   (place it in the custom control named CONTAINER

  IF gr_container IS INITIAL.
    CREATE OBJECT gr_container
      EXPORTING
        container_name = 'CONTAINER'.

* Create splitter container in which to place graphics
    CREATE OBJECT p_splitter
      EXPORTING
        parent  = gr_container
        rows    = 2
        columns = 1
        align   = 15. " (splitter fills the whole custom container)
  ENDIF.

ENDFORM.                    " CREATE_SPLITTER_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  SPLIT_RECORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM split_records .

*  IF CB_LFAB  EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_LFA1[] 'LFA1' GV_COUNT.
*    REFRESH: GT_LFA1.
*    PERFORM SPLIT_WRITE USING GT_LFB1[] 'LFB1' GV_COUNT2.
*    REFRESH: GT_LFB1.
*  ELSEIF CB_PROJ  EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_PROJ[] 'PROJ' GV_COUNT.
*  ELSEIF CB_PRPS EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_PRPS[] 'PRPS' GV_COUNT.
*  ELSEIF CB_AUFK  EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_AUFK[] 'AUFK' GV_COUNT.
*  ELSEIF CB_T499S EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_T499S[] 'T499S' GV_COUNT.
*    PERFORM SPLIT_WRITE USING GT_IFLOT[] 'IFLOT' GV_COUNT2.
*  ELSEIF CB_ANAH  EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_ANLH[] 'ANLH' GV_COUNT.
*    PERFORM SPLIT_WRITE USING GT_ANLA[] 'ANLA' GV_COUNT2.
*  ELSEIF CB_BNKA  EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_BNKA[] 'BNKA' GV_COUNT.
*    PERFORM SPLIT_WRITE USING GT_T012[] 'T012' GV_COUNT2.
*  ELSEIF CB_KNA1  EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_KNA1[] 'KNA1' GV_COUNT.
*    PERFORM SPLIT_WRITE USING GT_KNB1[] 'KNB1' GV_COUNT2.
*  ELSEIF CB_T001L EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_T001L[] 'T001L' GV_COUNT.
*  ELSEIF CB_MARA EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_MARA[] 'MARA' GV_COUNT.
*    PERFORM SPLIT_WRITE USING GT_MARA[] 'MARC' GV_COUNT2.
*  ELSEIF CB_T001W  EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_T001W[] 'T001W' GV_COUNT.
*  ELSEIF CB_EQUI  EQ 'X'.
*    PERFORM SPLIT_WRITE USING GT_EQUI[] 'EQUI' GV_COUNT.
*  ENDIF.

ENDFORM.                    " SPLIT_RECORDS
*&---------------------------------------------------------------------*
*&      Form  SPLIT_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_MARA[]  text
*----------------------------------------------------------------------*
FORM split_write USING pt_data TYPE STANDARD TABLE
                       p_tab   TYPE any
                       p_count TYPE any.

*  DATA: lv_count      TYPE i,
*        lv_file       TYPE string,
*        lv_file_no(4) TYPE n,
*        lv_tabix      TYPE sytabix,
*        lt_indx       TYPE STANDARD TABLE OF zfi_index_is,
*        rd_split_tab  TYPE REF TO data,
*        rd_split_line TYPE REF TO data,
*        rd_data_line  TYPE REF TO data,
*        lv_findx      TYPE text50,
*        lv_lines      TYPE i,
*        lv_from       TYPE zfrom_indx,
*        lv_to         TYPE zto_indx,
*        file_name     TYPE localfile.
*
*  DATA: lv         TYPE i,
*        lv_message TYPE string,
*        lr_exc     TYPE REF TO cx_root.
*
*  DATA: lt_header    TYPE truxs_t_text_data,
*      ls_header    TYPE LINE OF truxs_t_text_data,
*      lt_dfies     TYPE dfies_table,
*      lv_error(75) TYPE c.
*
*  FIELD-SYMBOLS: <split_tab>  TYPE STANDARD TABLE,
*                 <split_line> TYPE any,
*                 <data>       TYPE any,
*                 <index>      LIKE LINE OF lt_indx.
*
*  CHECK pt_data[] IS NOT INITIAL OR p_count IS NOT INITIAL.
*
*  CREATE DATA rd_split_tab TYPE STANDARD TABLE OF (p_tab).
*  ASSIGN rd_split_tab->* TO <split_tab>.
*
*  CREATE DATA rd_split_line TYPE (p_tab).
*  ASSIGN rd_split_line->* TO <split_line>.
*
*  CREATE DATA rd_data_line TYPE (p_tab).
*  ASSIGN rd_data_line->* TO <data>.
*
*  IF rd_pc EQ 'X'.
*    file_name = p_file.
*  ELSE.
*    file_name = p_appf.
*  ENDIF.
*
*  IF p_count IS INITIAL.
*    lv_lines = lines( pt_data ).
*  ENDIF.
*
*  IF lv_lines LE p_recs AND rd_rwix EQ '' AND rd_indx EQ '' AND p_rp EQ ''.
*    PERFORM generate_filename USING file_name lv_file_no lv_findx p_tab CHANGING lv_file.
*    PERFORM write_file USING p_tab pt_data[] lv_file.
*  ELSE.
*    IF rd_indx EQ 'X'.     " Only fill in custom table with index
*      PERFORM fill_index_table USING p_tab p_count.
*    ELSEIF rd_rwix EQ 'X'. " Split files based on counter in custom index table
*      SELECT * FROM zfi_index_is INTO TABLE lt_indx
*                                 WHERE dbtabname EQ p_tab
*                                 AND   zrun      EQ 'X'
*                                 AND   zcomp     NE 'X'.
*      IF sy-subrc EQ 0.
*        LOOP AT lt_indx ASSIGNING <index>.
*          LOOP AT pt_data ASSIGNING <data>
*                          FROM <index>-zfrom_indx TO <index>-zto_indx.
*            lv_tabix = sy-tabix.
*            APPEND INITIAL LINE TO <split_tab> ASSIGNING <split_line>.
*            <split_line> = <data>.
*            IF lv_tabix EQ <index>-zto_indx.
*              CLEAR: lv_file.
*              lv_from = <index>-zfrom_indx.
*              lv_to   = <index>-zto_indx.
*              SHIFT lv_from LEFT DELETING LEADING '0'.
*              SHIFT lv_to   LEFT DELETING LEADING '0'.
*              CONCATENATE 'Counter('lv_from '-' lv_to ')' INTO lv_findx.
*              IF rd_xlsx EQ 'X'. ".XLSX file extension
*                PERFORM generate_filename USING file_name lv_file_no lv_findx p_tab CHANGING lv_file.
*                PERFORM write_file USING p_tab pt_data[] lv_file.
*                <index>-zrun  = abap_true.
*                <index>-zcomp = abap_true.
*                MODIFY zfi_index_is FROM <index>.
*              ELSE. ".XLS file extension
*                PERFORM generate_filename USING file_name lv_file_no lv_findx p_tab CHANGING lv_file.
*                PERFORM write_file USING p_tab pt_data[] lv_file.
*                <index>-zrun  = abap_true.
*                <index>-zcomp = abap_true.
*                MODIFY zfi_index_is FROM <index>.
*              ENDIF.
*              FREE: <split_tab>[].
*            ENDIF.
*          ENDLOOP.
*        ENDLOOP.
*      ENDIF.
*    ELSE. " Do not use Index table
*      IF rd_pc EQ 'X'. " PC file
*        LOOP AT pt_data ASSIGNING <data>.
*          lv_tabix     = sy-tabix.
*          lv_count     = lv_count + 1.
*          APPEND INITIAL LINE TO <split_tab> ASSIGNING <split_line>.
*          <split_line> = <data>.
*          IF ( lv_count EQ p_recs ) OR ( lv_tabix EQ lv_lines ).
*            CLEAR: lv_file.
*            lv_file_no = lv_file_no + 1.
*            SHIFT lv_file_no LEFT DELETING LEADING '0'.
*            IF rd_xlsx EQ 'X'. ".XLSX file extension
*              PERFORM generate_filename USING file_name lv_file_no lv_findx p_tab CHANGING lv_file.
*              PERFORM write_file USING p_tab <split_tab>[] lv_file.
*            ELSE.
*              PERFORM generate_filename USING file_name lv_file_no lv_findx p_tab CHANGING lv_file.
*              PERFORM write_file USING p_tab <split_tab>[] lv_file.
*            ENDIF.
*            FREE: <split_tab>[], lv_count.
*          ENDIF.
*        ENDLOOP.
*      ELSE.  "App server file
*        LOOP AT pt_data ASSIGNING <data>.
*          lv_tabix = sy-tabix.
*          lv_count = lv_count + 1.
*          IF lv_tabix EQ 1.
*            IF gv_file_no IS INITIAL.
*              gv_file_no = 1.
*            ELSE.
*              gv_file_no = gv_file_no + 1.
*            ENDIF.
*            PERFORM generate_filename USING file_name gv_file_no lv_findx p_tab CHANGING lv_file.
*            PERFORM build_header USING p_tab ls_header lt_header[] CHANGING lt_dfies[].
*            OPEN DATASET lv_file FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
*            IF sy-subrc EQ 0.
*              TRANSFER ls_header TO lv_file.
*            ELSE.
*              WRITE: / p_tab ,' - ', lv_error , ' - ',  lv_file.
*              EXIT.
*            ENDIF.
*          ELSEIF lv_count EQ p_recs.
*            gv_file_no = gv_file_no + 1.
*            WRITE: / p_tab ,' - ', 'File successful', ' - ',  lv_file.
*            CLOSE DATASET lv_file.
*            CLEAR: lv_file.
*            PERFORM generate_filename USING file_name gv_file_no lv_findx p_tab CHANGING lv_file.
*            PERFORM build_header USING p_tab ls_header lt_header[] CHANGING lt_dfies[].
*            OPEN DATASET lv_file FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
*            IF sy-subrc EQ 0.
*              TRANSFER ls_header TO lv_file.
*            ELSE.
*              WRITE: / p_tab ,' - ', lv_error , ' - ',  lv_file.
*              EXIT.
*            ENDIF.
*            CLEAR lv_count.
*          ENDIF.
*
*          PERFORM write_unix_file USING <data> lv_file lt_dfies[] p_tab.
*          IF lv_tabix EQ lv_lines. "Write last file status/close file
*            WRITE: / p_tab ,' - ', 'File successful' , ' - ',  lv_file.
*            CLOSE DATASET lv_file.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
*  ENDIF.


*  IF lv_lines LE p_recs.
*    CONCATENATE p_file p_tab '_' sy-datum '_' sy-uzeit '.xlsx' INTO lv_file.
**    PERFORM gui_download USING pt_data[] lv_file.
*    PERFORM f_transfer_data_pres USING pt_data[] lv_file.
*  ELSE.
*    LOOP AT pt_data ASSIGNING <data>.
*      lv_tabix     = sy-tabix.
*      lv_count     = lv_count + 1.
*      APPEND INITIAL LINE TO <split_tab> ASSIGNING <split_line>.
*      <split_line> = <data>.
*      IF ( lv_count EQ p_recs ) OR ( lv_tabix EQ lv_lines ).
*        lv_file_no = lv_file_no + 1.
*        CLEAR: lv_file.
*        CONCATENATE p_file 'SAP_SW_'p_tab '_' sy-datum '_' sy-uzeit '_'
*                            lv_file_no  '.xlsx' INTO lv_file.
*        PERFORM f_transfer_data_pres USING <split_tab>[] lv_file.
*        CLEAR: <split_tab>[], lv_count.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

ENDFORM.                    " SPLIT_WRITE
*&---------------------------------------------------------------------*
*&      Form  F_TRANSFER_DATA_PRES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_MARA[]  text
*----------------------------------------------------------------------*
FORM f_transfer_data_pres  USING pt_data TYPE STANDARD TABLE
                                 p_file  TYPE any.

  DATA: gt_bintab  TYPE solix_tab,
        lv_size    TYPE i.

  PERFORM create_xls_from_itab USING    pt_data[]
                               CHANGING gt_bintab[] lv_size.

  cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_size
        filename                  = p_file
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = gt_bintab
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
        OTHERS                    = 24
           ).

ENDFORM.                    " F_TRANSFER_DATA_PRES

*&---------------------------------------------------------------------*
*&      Form  CREATE_XLS_FROM_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_xls_from_itab USING    pt_data   TYPE STANDARD TABLE
                          CHANGING pt_bintab TYPE solix_tab
                                   p_size    TYPE i.

  DATA: mt_fcat        TYPE lvc_t_fcat,
        mt_data        TYPE REF TO data,
        m_flavour      TYPE string,
        m_version      TYPE string,
        mo_result_data TYPE REF TO cl_salv_ex_result_data_table,
        mo_columns     TYPE REF TO cl_salv_columns_table,
        mo_aggreg      TYPE REF TO cl_salv_aggregations,
        mo_salv_table  TYPE REF TO cl_salv_table,
        m_file_type    TYPE salv_bs_constant,
        it_sort        TYPE lvc_t_sort,    " -> alv control: table of sort criteria
        it_filt        TYPE lvc_t_filt,    " -> alv control: table of filter conditions
        e_xstring      TYPE xstring,       " -> xstring with our excel file
        is_layout      TYPE lvc_s_layo.

  FIELD-SYMBOLS <tab> TYPE ANY TABLE.

  GET REFERENCE OF pt_data INTO mt_data.

*create fieldcatalog
  ASSIGN mt_data->* TO <tab>.
  TRY .
      cl_salv_table=>factory(
      EXPORTING
        list_display = abap_false
      IMPORTING
        r_salv_table = mo_salv_table
      CHANGING
        t_table      = <tab> ).
    CATCH cx_salv_msg.

  ENDTRY.

*get colums & aggregation info to create fieldcat
  mo_columns  = mo_salv_table->get_columns( ).
  mo_aggreg   = mo_salv_table->get_aggregations( ).
  mt_fcat     =  cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                r_columns      = mo_columns
                                r_aggregations = mo_aggreg ).

  IF cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_25 OR
     cl_salv_bs_a_xml_base=>get_version( ) EQ if_salv_bs_xml=>version_26.

    mo_result_data = cl_salv_ex_util=>factory_result_data_table(
        r_data                      = mt_data
        s_layout                    = is_layout
        t_fieldcatalog              = mt_fcat
        t_sort                      = it_sort
        t_filter                    = it_filt
    ).

    CASE cl_salv_bs_a_xml_base=>get_version( ).
      WHEN if_salv_bs_xml=>version_25.
        m_version = if_salv_bs_xml=>version_25.
      WHEN if_salv_bs_xml=>version_26.
        m_version = if_salv_bs_xml=>version_26.
    ENDCASE.

    m_file_type = if_salv_bs_xml=>c_type_xlsx.
    m_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export.

* transformation of data to excel
    CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
      EXPORTING
        xml_type      = m_file_type
        xml_version   = m_version
        r_result_data = mo_result_data
        xml_flavour   = m_flavour
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = e_xstring.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = e_xstring
      IMPORTING
        output_length = p_size
      TABLES
        binary_tab    = pt_bintab.
  ENDIF.

ENDFORM.                    "create_xls_from_itab
*&---------------------------------------------------------------------*
*&      Form  GUI_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<SPLIT_TAB>[]  text
*      -->P_GV_FILE  text
*----------------------------------------------------------------------*
FORM gui_download  USING    pt_data  TYPE STANDARD TABLE
                            p_file   TYPE any.

  cl_gui_frontend_services=>gui_download(
      EXPORTING
*        bin_filesize              = lv_size
        filename                  = p_file
        filetype                  = 'ASC'
        write_field_separator     = 'X'
      CHANGING
        data_tab                  = pt_data
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
        OTHERS                    = 24
           ).

ENDFORM.                    " GUI_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  BUILD_INDEX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_LINES  text
*----------------------------------------------------------------------*
FORM build_index  USING p_tab   TYPE any
                        p_lines TYPE any
                        pt_data TYPE STANDARD TABLE.

*  TYPES: BEGIN OF ty_key,
*           fieldname TYPE fieldname,
*         END OF ty_key.
*
*  DATA: lt_dfies TYPE STANDARD TABLE OF dfies,
*        lt_keys  TYPE STANDARD TABLE OF ty_key,
*        lv_tabix TYPE sytabix,
*        lv_count TYPE i,
*        last_count TYPE i,
*        ls_index TYPE zfi_index_is,
*        lv_where TYPE string.
*
*  FIELD-SYMBOLS: <dfies> LIKE LINE OF lt_dfies,
*                 <key>   LIKE LINE OF lt_keys.
*
*  DATA: rd_split_tab  TYPE REF TO data,
*        rd_split_line TYPE REF TO data,
*        rd_data_line  TYPE REF TO data.
*
*  FIELD-SYMBOLS: <split_tab>  TYPE STANDARD TABLE,
*                 <split_line> TYPE any,
*                 <data>       TYPE any,
*                 <val>        TYPE any.
*
*  CALL FUNCTION 'DDIF_FIELDINFO_GET'
*    EXPORTING
*      tabname        = p_tab
*    TABLES
*      dfies_tab      = lt_dfies
*    EXCEPTIONS
*      not_found      = 1
*      internal_error = 2
*      OTHERS         = 3.
*
*  IF sy-subrc EQ 0.
*    LOOP AT lt_dfies ASSIGNING <dfies>
*                     WHERE keyflag EQ 'X' AND fieldname NE 'MANDT'.
*      APPEND <dfies>-fieldname TO lt_keys.
*    ENDLOOP.
*  ENDIF.
*
*  CREATE DATA rd_split_tab TYPE STANDARD TABLE OF (p_tab).
*  ASSIGN rd_split_tab->* TO <split_tab>.
*
*  CREATE DATA rd_split_line TYPE (p_tab).
*  ASSIGN rd_split_line->* TO <split_line>.
*
*  CREATE DATA rd_data_line TYPE (p_tab).
*  ASSIGN rd_data_line->* TO <data>.
*
*  DELETE FROM zfi_index_is WHERE dbtabname = p_tab.
*
*  LOOP AT pt_data ASSIGNING <data>.
*    lv_tabix = sy-tabix.
*    IF lv_count IS INITIAL.
*      CLEAR: ls_index.
*      ls_index-zfrom_indx = last_count + 1.
*    ENDIF.
*
*    lv_count = lv_count + 1.
*
*    IF ( lv_count EQ p_recs ) OR ( lv_tabix EQ p_lines ).
*      ls_index-dbtabname = p_tab.
*      last_count        = last_count + lv_count.
*      ls_index-zto_indx = last_count.
*      MODIFY zfi_index_is FROM ls_index.
*      CLEAR lv_count.
*    ENDIF.
*  ENDLOOP.
*  IF sy-subrc EQ 0.
*    WRITE: / 'Index build complete'.
*  ENDIF.

ENDFORM.                    " BUILD_INDEX
*&---------------------------------------------------------------------*
*&      Form  WRITE_PC_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<SPLIT_TAB>[]  text
*      -->P_LV_FILE  text
*----------------------------------------------------------------------*
FORM write_pc_file USING pt_tab    TYPE STANDARD TABLE
                         pt_header TYPE STANDARD TABLE
                         p_file    TYPE any
                         p_tabname TYPE any.

  DATA: lv_seperator TYPE char01.

  IF rd_xlsx EQ 'X'.
    lv_seperator = 'X'.
  ENDIF.

* Write header
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = p_file
      filetype                = 'ASC'
    TABLES
      data_tab                = pt_header
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

* Write file data
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = p_file
      filetype                = 'ASC'
      append                  = 'X'
      write_field_separator   = lv_seperator
    TABLES
      data_tab                = pt_tab
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

  IF sy-subrc EQ 0.
    WRITE: / p_tabname ,' - ', 'File successful', ' - ',  p_file.
  ENDIF.

ENDFORM.                    " WRITE_PC_FILE

*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TAB  text
*----------------------------------------------------------------------*
FORM build_header USING p_tab          TYPE any
                        ps_header      TYPE any
                        ps_tech_header TYPE any
                        "pt_header      TYPE truxs_t_text_data
                        pt_header      TYPE string_table
                  CHANGING pt_dfies    TYPE dfies_table.

  DATA: lv_tabix     TYPE sytabix,
        lv_lines     TYPE i,
        lv_delimiter TYPE char4,
        ls_header    LIKE LINE OF pt_header,
        lt_dfies     TYPE dfies_table,
        ls_dfies     LIKE LINE OF lt_dfies.

  DATA: temp_header       LIKE LINE OF pt_header,
        temp_tech_header  LIKE LINE OF pt_header.

  FIELD-SYMBOLS: <dfies> LIKE LINE OF pt_dfies.

  REFRESH: pt_header.

  IF rd_csv NE 'X' AND rd_delim NE 'X'.
    lv_delimiter = cl_abap_char_utilities=>horizontal_tab.
  ELSE.
    IF rd_csv EQ 'X'.
      lv_delimiter = ','.
    ELSE.
      lv_delimiter = p_delim.
    ENDIF.
  ENDIF.

  PERFORM read_ddif USING p_tab CHANGING lt_dfies[].

  IF p_tab EQ 'STXL'.
    APPEND INITIAL LINE TO lt_dfies ASSIGNING <dfies>.
    <dfies>-tabname   = 'STXL'.
    <dfies>-fieldname = 'TEXT'.
    <dfies>-scrtext_s = 'Texts'.
    <dfies>-scrtext_m = 'Texts'.
    <dfies>-scrtext_l = 'Texts'.

* Do not need CLUSTD data for ODI as per Farhan
    DELETE lt_dfies WHERE fieldname EQ 'CLUSTD'.
  ENDIF.

  CLEAR: ls_header,lv_tabix.
  lv_lines = lines( lt_dfies ).

  IF p_tab NE 'AUSP+MARA' AND
     p_tab NE 'TOA01'     AND
     p_tab NE 'DRAD'      AND
     p_tab NE 'SOOD'.

    LOOP AT lt_dfies ASSIGNING <dfies>.
      lv_tabix = sy-tabix.
      PERFORM concatenation USING <dfies> lv_delimiter
                            CHANGING ps_header ps_tech_header.
      IF lv_lines EQ lv_tabix.
        APPEND ps_header TO pt_header.
        APPEND ps_tech_header TO pt_header.
        APPEND LINES OF lt_dfies TO pt_dfies.
      ENDIF.
    ENDLOOP.

  ELSEIF p_tab EQ 'AUSP+MARA'. "AUSP and MARA table

    PERFORM read_ddif USING 'MARA' CHANGING pt_dfies[].
    APPEND INITIAL LINE TO pt_dfies ASSIGNING <dfies>.
    <dfies>-fieldname = 'ATBEZ'.
    <dfies>-scrtext_s = 'Desc.'.
    <dfies>-scrtext_m = 'Description'.
    <dfies>-scrtext_l = 'Char. description'.
    <dfies>-inttype   = 'C'.

    APPEND INITIAL LINE TO pt_dfies ASSIGNING <dfies>.
    <dfies>-fieldname = 'ATWRT'.
    <dfies>-scrtext_s = 'Char.Value'.
    <dfies>-scrtext_m = 'Char. Value'.
    <dfies>-scrtext_l = 'Characteristic Value'.
    <dfies>-inttype   = 'C'.

    lv_lines = lines( pt_dfies ).
    LOOP AT pt_dfies ASSIGNING <dfies>.
      lv_tabix = sy-tabix.
      PERFORM concatenation USING <dfies> lv_delimiter
                            CHANGING ps_header ps_tech_header.
      IF lv_lines EQ lv_tabix.
        APPEND ps_header TO pt_header.
        APPEND ps_tech_header TO pt_header.
      ENDIF.
    ENDLOOP.


  ELSEIF p_tab EQ 'TOA01'.

    REFRESH: pt_header, pt_dfies.
    PERFORM read_ddif USING p_tab CHANGING lt_dfies[].
    LOOP AT lt_dfies ASSIGNING <dfies>.
      lv_tabix = sy-tabix.
      CHECK <dfies>-fieldname NE 'MANDT'.
      ls_dfies = <dfies>.
      APPEND ls_dfies TO pt_dfies.
      PERFORM concatenation USING <dfies> lv_delimiter
                             CHANGING ps_header ps_tech_header.
    ENDLOOP.

    PERFORM read_ddif USING 'TOAAT' CHANGING lt_dfies[].
    lv_lines = lines( lt_dfies ).

    LOOP AT lt_dfies ASSIGNING <dfies>.
      lv_tabix = sy-tabix.
      CHECK <dfies>-fieldname NE 'MANDT'.
      ls_dfies = <dfies>.
      APPEND ls_dfies TO pt_dfies.
      PERFORM concatenation USING <dfies> lv_delimiter
                            CHANGING ps_header ps_tech_header.
      IF lv_lines EQ lv_tabix.
        APPEND ps_header TO pt_header.
        APPEND ps_tech_header TO pt_header.
      ENDIF.
    ENDLOOP.

  ELSEIF p_tab EQ 'DRAD'.

    REFRESH: pt_header, pt_dfies.
    PERFORM read_ddif USING p_tab CHANGING lt_dfies[].
    LOOP AT lt_dfies ASSIGNING <dfies>.
      ls_dfies = <dfies>.
      APPEND ls_dfies TO pt_dfies.
      PERFORM concatenation USING <dfies> lv_delimiter
                            CHANGING ps_header ps_tech_header.
    ENDLOOP.

    PERFORM read_ddif USING 'DMS_DOC2LOIO' CHANGING lt_dfies[].
    LOOP AT lt_dfies ASSIGNING <dfies>.
      ls_dfies = <dfies>.
      APPEND ls_dfies TO pt_dfies.
      PERFORM concatenation USING <dfies> lv_delimiter
                            CHANGING ps_header ps_tech_header.
    ENDLOOP.

    PERFORM read_ddif USING 'DMS_PH_CD1' CHANGING lt_dfies[].
    LOOP AT lt_dfies ASSIGNING <dfies>.
      ls_dfies = <dfies>.
      APPEND ls_dfies TO pt_dfies.
      PERFORM concatenation USING <dfies> lv_delimiter
                                 CHANGING ps_header ps_tech_header.
    ENDLOOP.

    PERFORM read_ddif USING 'DRAT' CHANGING lt_dfies[].
    lv_lines = lines( lt_dfies ).
    LOOP AT lt_dfies ASSIGNING <dfies>.
      lv_tabix = sy-tabix.
      ls_dfies = <dfies>.
      APPEND ls_dfies TO pt_dfies.
      PERFORM concatenation USING <dfies> lv_delimiter
                            CHANGING ps_header ps_tech_header.
      IF lv_lines EQ lv_tabix.
        APPEND ps_header TO pt_header.
        APPEND ps_tech_header TO pt_header.
      ENDIF.
    ENDLOOP.

  ELSEIF p_tab EQ 'SOOD'.

    REFRESH: pt_header, pt_dfies,lt_dfies.
    PERFORM read_ddif USING 'SOODK' CHANGING lt_dfies[].

    INSERT INITIAL LINE INTO lt_dfies ASSIGNING <dfies> INDEX 1.
    <dfies>-fieldname = 'RELTYPE'.
    <dfies>-scrtext_s = 'Rel. Type'.
    <dfies>-scrtext_m = 'Rel. type'.
    <dfies>-scrtext_l = 'Relationship type'.

    INSERT INITIAL LINE INTO lt_dfies ASSIGNING <dfies> INDEX 2.
    <dfies>-fieldname = 'INSTID_A'.
    <dfies>-scrtext_s = 'InstanceID'.
    <dfies>-scrtext_m = 'Instance ID'.
    <dfies>-scrtext_l = 'Instance ID'.

    INSERT INITIAL LINE INTO lt_dfies ASSIGNING <dfies> INDEX 3.
    <dfies>-fieldname = 'TYPEID_A'.
    <dfies>-scrtext_s = 'Obj. Type'.
    <dfies>-scrtext_m = 'Object Type'.
    <dfies>-scrtext_l = 'Object Type'.

    INSERT INITIAL LINE INTO lt_dfies ASSIGNING <dfies> INDEX 4.
    <dfies>-fieldname = 'TYPEID_B'.
    <dfies>-scrtext_s = 'Obj. Type'.
    <dfies>-scrtext_m = 'Object Type'.
    <dfies>-scrtext_l = 'Object Type'.

    APPEND INITIAL LINE TO lt_dfies ASSIGNING <dfies>.
    <dfies>-fieldname = 'OBJDES'.
    <dfies>-scrtext_m = 'Document Title'.
    <dfies>-scrtext_l = 'Document Title'.

    APPEND INITIAL LINE TO lt_dfies ASSIGNING <dfies>.
    <dfies>-fieldname = 'TEXTS'.
    <dfies>-scrtext_s = 'Texts'.
    <dfies>-scrtext_m = 'Texts'.
    <dfies>-scrtext_l = 'Texts'.

    lv_lines = lines( lt_dfies ).
    LOOP AT lt_dfies ASSIGNING <dfies>.
      lv_tabix = sy-tabix.
      ls_dfies = <dfies>.
      APPEND ls_dfies TO pt_dfies.
      PERFORM concatenation USING <dfies> lv_delimiter
                            CHANGING ps_header ps_tech_header.
      IF lv_lines EQ lv_tabix.
        APPEND ps_header TO pt_header.
        APPEND ps_tech_header TO pt_header.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " BUILD_HEADER

*&---------------------------------------------------------------------*
*&      Form  FILL_INDEX_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TAB  text
*      -->P_LV_LINES  text
*----------------------------------------------------------------------*
FORM fill_index_table  USING p_tab   TYPE any
                             p_lines TYPE any.

*  DATA: ls_index TYPE zfi_index_is.
*
*  DATA: div_no     TYPE i,
*        remainder  TYPE i,
*        last_count TYPE i,
*        lv_count   TYPE i,
*        last_indx  TYPE i.
*
*  IF p_lines <= p_recs.
*    ls_index-dbtabname  = p_tab.
*    ls_index-zfrom_indx = 1.
*    ls_index-zto_indx   = p_lines.
*    MODIFY zfi_index_is FROM ls_index.
*  ELSE.
*    div_no = p_lines / p_recs. " truncating integer division
*    DO div_no TIMES.
*      lv_count = lv_count + 1.
*      IF lv_count EQ 1. " First record of index table
*        ls_index-dbtabname  = p_tab.
*        ls_index-zfrom_indx = lv_count.
*        ls_index-zto_indx   = p_recs.
*        MODIFY zfi_index_is FROM ls_index.
*        last_indx = ls_index-zto_indx.
*      ELSE.
*        ls_index-dbtabname  = p_tab.
*        ls_index-zfrom_indx = last_indx + 1.
*        ls_index-zto_indx   = last_indx + p_recs.
*        MODIFY zfi_index_is FROM ls_index.
*        last_indx           = ls_index-zto_indx.
*      ENDIF.
*    ENDDO.
*    remainder = p_lines MOD p_recs. " get remainder
*    IF remainder IS NOT INITIAL.
*      ls_index-dbtabname  = p_tab.
*      ls_index-zfrom_indx = last_indx + 1.
*      ls_index-zto_indx   = last_indx + remainder.
*      MODIFY zfi_index_is FROM ls_index.
*    ENDIF.
*  ENDIF.

*  WRITE: / 'Index build complete for - ',  p_tab ,  ' - ' , p_lines, ' records'.

ENDFORM.                    " FILL_INDEX_TABLE
*&---------------------------------------------------------------------*
*&      Form  READ_TABLE_SELECTED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_table_selected .

  DATA : db_cursor   TYPE cursor,
         lv_pkg_size TYPE i.

*  IF CB_MARA EQ 'X'.
*    PERFORM GET_PACKAGE_SIZE USING 'MARA' CHANGING LV_PKG_SIZE.
*    OPEN CURSOR WITH HOLD DB_CURSOR FOR
*    SELECT * FROM MARA
*    BYPASSING BUFFER.
*    DO.
**** To Fetch data in chunks of 2gb
*      FETCH NEXT CURSOR DB_CURSOR
*      INTO TABLE GT_MARA
*      PACKAGE SIZE LV_PKG_SIZE.
*      IF SY-SUBRC NE 0.
*        CLOSE CURSOR DB_CURSOR.
*        EXIT.
*      ELSE.
*        PERFORM SPLIT_WRITE USING GT_MARA[] 'MARA' ''.
*        FREE: GT_MARA[].
*      ENDIF.
*    ENDDO.
*
*    CLEAR: GV_FILE_NO.
** Read MARC
*    PERFORM GET_PACKAGE_SIZE USING 'MARC' CHANGING LV_PKG_SIZE.
*    OPEN CURSOR WITH HOLD DB_CURSOR FOR
*    SELECT * FROM MARC
*    BYPASSING BUFFER.
*    DO.
**** To Fetch data in chunks of 2gb
*      FETCH NEXT CURSOR DB_CURSOR
*      INTO TABLE GT_MARC
*      PACKAGE SIZE LV_PKG_SIZE.
*      IF SY-SUBRC NE 0.
*        CLOSE CURSOR DB_CURSOR.
*        EXIT.
*      ELSE.
*        PERFORM SPLIT_WRITE USING GT_MARC[] 'MARC' ''.
*        FREE: GT_MARC[].
*      ENDIF.
*    ENDDO.
*    CLEAR: GV_FILE_NO.
*
*  ELSEIF CB_AUFK EQ 'X'.
*    PERFORM GET_PACKAGE_SIZE USING 'AUFK' CHANGING LV_PKG_SIZE.
*    OPEN CURSOR WITH HOLD DB_CURSOR FOR
*    SELECT * FROM AUFK
*    BYPASSING BUFFER.
*    DO.
**** To Fetch data in chunks of 2gb
*      FETCH NEXT CURSOR DB_CURSOR
*      INTO TABLE GT_AUFK
*      PACKAGE SIZE LV_PKG_SIZE.
*      IF SY-SUBRC NE 0.
*        CLOSE CURSOR DB_CURSOR.
*        EXIT.
*      ELSE.
*        PERFORM SPLIT_WRITE USING GT_AUFK[] 'AUFK' ''.
*        FREE: GT_AUFK[].
*      ENDIF.
*    ENDDO.
*    CLEAR: GV_FILE_NO.
*  ENDIF.

ENDFORM.                    " READ_TABLE_SELECTED

*&---------------------------------------------------------------------*
*&      Form  WRITE_APP_SERVER_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<SPLIT_TAB>[]  text
*      -->P_LT_HEADER[]  text
*      -->P_LV_FILE  text
*----------------------------------------------------------------------*
FORM write_app_server_file USING pt_tab    TYPE STANDARD TABLE
                                 pt_header TYPE truxs_t_text_data
                                 p_file    TYPE any
                                 pt_dfies  TYPE dfies_table
                                 p_tabname TYPE any.


  PERFORM delimit_write_file USING pt_tab[] pt_header[] pt_dfies[] p_tabname.

ENDFORM.                    " WRITE_APP_SERVER_FILE
*&---------------------------------------------------------------------*
*&      Form  DELIMIT_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<SPLIT_TAB>[]  text
*----------------------------------------------------------------------*
FORM delimit_file USING    pt_data     TYPE STANDARD TABLE
                           pt_header   TYPE truxs_t_text_data
                           pt_dfies    TYPE dfies_table
                  CHANGING pt_temp_tab TYPE truxs_t_text_data.

  DATA: lv_temp_data LIKE LINE OF pt_temp_tab,
        lv_delimiter TYPE char4,
        lv_val       TYPE string.

  FIELD-SYMBOLS: <header> LIKE LINE OF pt_header,
                 <data>   TYPE any,
                 <dfies>  LIKE LINE OF pt_dfies,
                 <fval>   TYPE any.

  IF rd_csv NE 'X' AND rd_delim NE 'X'.
    lv_delimiter = htab.
  ELSE.
    IF rd_csv EQ 'X'.
      lv_delimiter = ','.
    ELSE.
      lv_delimiter = p_delim.
    ENDIF.
  ENDIF.

  LOOP AT pt_data ASSIGNING <data>.
    CLEAR: lv_temp_data.
    LOOP AT pt_dfies ASSIGNING <dfies>.
      IF sy-tabix NE 1. " Not the first field
        UNASSIGN <fval>.
        ASSIGN COMPONENT <dfies>-fieldname OF STRUCTURE <data> TO <fval>.
        lv_val = <fval>.
* Look for commas in field value then wrap around with double quote
        IF rd_csv EQ 'X' AND <dfies>-inttype NE 'N' AND <dfies>-inttype NE 'D' AND
           <dfies>-inttype NE 'T' AND <dfies>-inttype NE 'P' AND
           <dfies>-inttype NE 'F'.
          SEARCH lv_val  FOR ','.
          IF sy-subrc EQ 0.
            CONCATENATE '"' lv_val '"' INTO lv_val.
          ENDIF.
        ENDIF.
        PERFORM format_output USING <dfies>-fieldname <dfies>-inttype <dfies>-domname <data> p_tabs lv_delimiter
                              CHANGING lv_val.
        IF <dfies>-inttype EQ 'C' OR <dfies>-inttype EQ 'N' OR
           <dfies>-inttype EQ 'D' OR <dfies>-inttype EQ 'T'.
          CONCATENATE lv_temp_data <fval> INTO lv_temp_data SEPARATED BY lv_delimiter.
        ELSE.
          PERFORM format_output USING <dfies>-fieldname <dfies>-inttype <dfies>-domname <data> p_tabs lv_delimiter
                                CHANGING lv_val.
          CONCATENATE lv_temp_data lv_val INTO lv_temp_data SEPARATED BY lv_delimiter.
        ENDIF.
      ELSE.
        UNASSIGN <fval>.
        ASSIGN COMPONENT <dfies>-fieldname OF STRUCTURE <data> TO <fval>.
        lv_val = <fval>.
        PERFORM format_output USING <dfies>-fieldname <dfies>-inttype <dfies>-domname <data> p_tabs lv_delimiter
                              CHANGING lv_val.
        lv_temp_data = lv_val.
      ENDIF.
    ENDLOOP.
    APPEND lv_temp_data TO pt_temp_tab.
  ENDLOOP.

* Add header row to the table
  READ TABLE pt_header ASSIGNING <header> INDEX 1.
  IF sy-subrc EQ 0.
    INSERT <header> INTO pt_temp_tab INDEX 1.
  ENDIF.

ENDFORM.                    " DELIMIT_FILE
*&---------------------------------------------------------------------*
*&      Form  WRITE_APP_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DATA[]  text
*----------------------------------------------------------------------*
FORM write_app_file USING pt_data   TYPE truxs_t_text_data
                          p_file    TYPE any
                          p_tabname TYPE any.

  DATA: lv_error(75) TYPE c.

  FIELD-SYMBOLS: <data> TYPE any.

  OPEN DATASET p_file FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.
    LOOP AT pt_data ASSIGNING <data>.
      TRANSFER <data> TO p_file.
    ENDLOOP.
  ELSE.
    WRITE: / p_tabname ,' - ', lv_error , ' - ',  p_file.
  ENDIF.

  CLOSE DATASET p_file.

ENDFORM.                    " WRITE_APP_FILE
*&---------------------------------------------------------------------*
*&      Form  GENERATE_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILE_NAME  text
*      <--P_LV_FILE  text
*----------------------------------------------------------------------*
FORM generate_filename  USING    p_fname   TYPE any
                                 p_file_no TYPE any
                                 p_indx_no TYPE any
                                 p_tabname TYPE any
                        CHANGING p_file    TYPE any.

  DATA: lv_sys       TYPE text10,
        lv_extension TYPE text10,
        lv_indx_ext  TYPE text50,
        lv_tb TYPE string.

  DATA: lv_filename TYPE localfile,
     lv_param_1  TYPE text15,
     lv_param_3  TYPE text15.

  p_file = p_fname.

  IF rd_xlsx EQ 'X'.
    lv_extension = '.xlsx'.
  ELSEIF rd_xls EQ 'X'.
    lv_extension = '.xls'.
  ELSEIF rd_csv EQ 'X'.
    lv_extension = '.csv'.
  ELSE.
    lv_extension = '.txt'.
  ENDIF.

  IF rd_pc NE 'X'.
    IF p_indx_no IS NOT INITIAL.
      IF p_indx_no IS NOT INITIAL.
        CONCATENATE '_' p_indx_no  lv_extension INTO lv_indx_ext.
      ELSE.
        lv_indx_ext = lv_extension.
      ENDIF.
      REPLACE '.EXT' IN p_file WITH lv_indx_ext.
      IF p_tabname NE 'STXL'.
        REPLACE 'TABL' IN p_file WITH p_tabname.
      ELSE.
        CONCATENATE p_tabname '_' s_tdobje-low INTO lv_tb.
        REPLACE 'TABL' IN p_file WITH lv_tb.
      ENDIF.
    ELSE.
      IF p_file_no IS NOT INITIAL.
        CONCATENATE '_' p_file_no  lv_extension INTO lv_indx_ext.
      ELSE.
        lv_indx_ext = lv_extension.
      ENDIF.
      REPLACE '.EXT' IN p_file WITH lv_indx_ext.
      IF p_tabname NE 'STXL'.
        REPLACE 'TABL' IN p_file WITH p_tabname.
      ELSE.
        CONCATENATE p_tabname '_' s_tdobje-low INTO lv_tb.
        REPLACE 'TABL' IN p_file WITH lv_tb.
      ENDIF.
    ENDIF.
  ELSE.
    IF sy-sysid EQ 'RDC'.
      lv_param_1 = 'DEV'.
      CONCATENATE 'US_' sy-sysid INTO lv_param_3.
    ELSEIF sy-sysid EQ 'RQC'.
      lv_param_1 = 'QA'.
      CONCATENATE 'US_PR_QA_' sy-sysid INTO lv_param_3.
    ELSEIF sy-sysid EQ 'PEC'.
      lv_param_1 = 'PRD'.
      CONCATENATE 'US_PR_PRD_' sy-sysid INTO lv_param_3.
    ELSEIF sy-sysid EQ 'S02'.
      lv_param_1 = 'DEV'.
      CONCATENATE 'UG_' sy-sysid INTO lv_param_3.
    ELSEIF sy-sysid EQ 'Q02'.
      lv_param_1 = 'QA'.
      CONCATENATE 'UG_PR_QA_' sy-sysid INTO lv_param_3.
    ELSEIF sy-sysid EQ 'P01'.
      lv_param_1 = 'PRD'.
      CONCATENATE 'UG_PR_PRD_' sy-sysid INTO lv_param_3.
    ELSEIF sy-sysid EQ 'S11'.
      lv_param_1 = 'QA'.
      CONCATENATE 'SW_PR_QA_' sy-sysid INTO lv_param_3.
    ENDIF.
    IF p_tabname NE 'STXL'.
      CONCATENATE  p_file lv_param_3 '_' p_tabname '_' p_file_no '_' sy-datum lv_extension INTO p_file.
    ELSE.
      CONCATENATE  p_file lv_param_3 '_STXL_' s_tdobje-low '_' p_file_no '_' sy-datum lv_extension INTO p_file.
    ENDIF.
  ENDIF.

ENDFORM.                    " GENERATE_FILENAME

*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_filename .

  DATA: lv_filename TYPE localfile,
        lv_param_1  TYPE text15,
        lv_param_3  TYPE text15.

  IF sy-sysid EQ 'RDC'.
    lv_param_1 = 'DEV'.
    CONCATENATE 'US_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'RQC'.
    lv_param_1 = 'QA'.
    CONCATENATE 'US_PR_QA_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'PEC'.
    lv_param_1 = 'PRD'.
    CONCATENATE 'US_PR_PRD_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'S02'.
    lv_param_1 = 'DEV'.
    CONCATENATE 'UG_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'Q02'.
    lv_param_1 = 'QA'.
    CONCATENATE 'UG_PR_QA_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'P01'.
    lv_param_1 = 'PRD'.
    CONCATENATE 'UG_PR_PRD_' sy-sysid INTO lv_param_3.
  ELSEIF sy-sysid EQ 'S11'.
    lv_param_1 = 'QA'.
    CONCATENATE 'SW_PR_QA_' sy-sysid INTO lv_param_3.
  ENDIF.

* Lookup logical file path
  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = 'ZFI_READ_MD_FOR_IS'
      parameter_1      = lv_param_1
      parameter_2      = lv_param_1
      parameter_3      = lv_param_3
    IMPORTING
      file_name        = p_appf
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

ENDFORM.                    " GET_FILENAME

*&---------------------------------------------------------------------*
*&      Form  BROWSE_DIRECTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM browse_directory  USING p_path TYPE any.


  DATA: lv_path TYPE string,
        lv_folder TYPE string.


  lv_path = p_path.
  lv_folder = 'H:\'.

  cl_gui_frontend_services=>directory_browse(
  EXPORTING
    initial_folder = lv_folder
   CHANGING
     selected_folder      = lv_path
   EXCEPTIONS
     cntl_error           = 1
     error_no_gui         = 2
     not_supported_by_gui = 3
     OTHERS               = 4
        ).

  IF sy-subrc <> 0.

  ENDIF.

ENDFORM.                    " BROWSE_DIRECTORY
*&---------------------------------------------------------------------*
*&      Form  WRITE_PCFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_DATA[]  text
*      -->P_LT_HEADER[]  text
*      -->P_LV_FILE  text
*----------------------------------------------------------------------*
FORM write_pcfile  USING         p_tab      TYPE any
                                 pt_data    TYPE STANDARD TABLE
                                 p_file     TYPE any.

  DATA: "lt_header       TYPE truxs_t_text_data,
        lt_header       TYPE string_table,
        lt_fdata        TYPE string_table,
        ls_header       LIKE LINE OF lt_header,
        ls_tech_header  LIKE LINE OF lt_header,
        lv_lines        TYPE i,
        lt_dfies        TYPE dfies_table.

  IF rd_pc EQ 'X'. " PC file
    IF rd_xlsx EQ 'X'.
      PERFORM f_transfer_data_pres USING pt_data[] p_file.
    ELSE. ".csv, .xls, delimited file
      PERFORM build_header USING p_tab ls_header ls_tech_header lt_header[] CHANGING lt_dfies[].
      PERFORM delimit_format_pc_file USING pt_data[] lt_dfies[] CHANGING lt_fdata[] .
      PERFORM write_pc_file USING lt_fdata[] lt_header[] p_file p_tab.
    ENDIF.
*  ELSE. " Application server file
*    PERFORM build_header USING p_tab ls_header ls_tech_header lt_header[] CHANGING lt_dfies[].
*    PERFORM write_app_server_file USING pt_data[] lt_header[] p_file lt_dfies[] p_tab.
  ENDIF.

ENDFORM.                    " WRITE_PCFILE
*&---------------------------------------------------------------------*
*&      Form  READ_TABLE_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_table_count .

*  CLEAR: GV_COUNT, GV_COUNT2.
*
*  IF CB_LFAB EQ 'X'.  " Vendor Master
*    SELECT COUNT(*) INTO GV_COUNT FROM LFA1.
*    SELECT COUNT(*) INTO GV_COUNT2 FROM LFB1.
*  ENDIF.
*
*  IF CB_PROJ EQ 'X'.  " Project
*    SELECT COUNT(*) INTO GV_COUNT FROM PROJ.
*  ENDIF.
*
*  IF  CB_PRPS EQ 'X'. " WBS Element
*    SELECT COUNT(*) INTO GV_COUNT FROM PRPS.
*  ENDIF.
*
*  IF CB_AUFK EQ 'X'. " Work Order
*    SELECT COUNT(*) INTO GV_COUNT FROM AUFK.
*  ENDIF.
*
*  IF CB_T499S EQ 'X'. " Location Master
*    SELECT COUNT(*) INTO GV_COUNT FROM T499S.
*    SELECT COUNT(*) INTO GV_COUNT2 FROM IFLOT.
*  ENDIF.
*
*  IF CB_ANAH EQ 'X'.  " Asset Master
*    SELECT COUNT(*) INTO GV_COUNT FROM ANLA.
*    SELECT COUNT(*) INTO GV_COUNT2 FROM ANLH.
*  ENDIF.
*
*  IF CB_BNKA EQ 'X'.  " Bank Master
*    SELECT COUNT(*) INTO GV_COUNT FROM T012.
*    SELECT COUNT(*) INTO GV_COUNT2 FROM BNKA.
*  ENDIF.
*
*  IF CB_KNA1 EQ 'X'.   " Customer master
*    SELECT COUNT(*) INTO GV_COUNT FROM KNA1.
*    SELECT COUNT(*) INTO GV_COUNT2 FROM KNB1.
*  ENDIF.
*
*  IF CB_T001L EQ 'X'.  " Inventory Location
*    SELECT COUNT(*) INTO GV_COUNT FROM T001L.
*  ENDIF.
*
*  IF CB_MARA EQ 'X'.
*    SELECT COUNT(*) INTO GV_COUNT FROM MARA.
*    SELECT COUNT(*) INTO GV_COUNT2 FROM MARC.
*  ENDIF.
*
*  IF CB_T001W EQ 'X'. " Location Hierarchy Master
*    SELECT COUNT(*) INTO GV_COUNT FROM T001W.
*  ENDIF.
*
*  IF CB_EQUI EQ 'X'. "Equipment master
*    SELECT COUNT(*) INTO GV_COUNT FROM EQUI.
*  ENDIF.

ENDFORM.                    " READ_TABLE_COUNT

*&---------------------------------------------------------------------*
*&      Form  GET_PACKAGE_SIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4489   text
*      <--P_G_PACKAGE_SIZE  text
*----------------------------------------------------------------------*
FORM get_package_size  USING    p_tabname  TYPE any
                       CHANGING p_pkg_size TYPE any.

  DATA : lt_dfies   TYPE TABLE OF dfies,
         ls_dfies   TYPE          dfies,
         struc_size TYPE i VALUE 0.

  CLEAR: p_pkg_size.

  CALL FUNCTION 'DDIF_NAMETAB_GET'
    EXPORTING
      tabname   = p_tabname
    TABLES
      dfies_tab = lt_dfies
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
** Logic for calculating Package size
* To calculate the memory taken by one record
  LOOP AT lt_dfies INTO ls_dfies.
    struc_size = struc_size + ls_dfies-leng.
  ENDLOOP.
***** To calculaten maximum no of records that can be accomodated in p_pkg
*  p_pkg_size = 2147483648 / struc_size.
  p_pkg_size = p_pkg / struc_size.

ENDFORM.                    " GET_PACKAGE_SIZE
*&---------------------------------------------------------------------*
*&      Form  DELIMIT_WRITE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_TAB[]  text
*      -->P_PT_HEADER[]  text
*      -->P_PT_DFIES[]  text
*----------------------------------------------------------------------*
FORM delimit_write_file USING pt_data     TYPE STANDARD TABLE
                              pt_header   TYPE truxs_t_text_data
                              pt_dfies    TYPE dfies_table
                              p_tabname   TYPE any.

  DATA: lv_temp_data LIKE LINE OF pt_header,
        lv_val       TYPE string.

  FIELD-SYMBOLS: <header> LIKE LINE OF pt_header,
                 <data>   TYPE any,
                 <dfies>  LIKE LINE OF pt_dfies,
                 <fval>   TYPE any.

  DATA: lv_error(75) TYPE c.

  OPEN DATASET p_file FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.
* Add header row to the table
    READ TABLE pt_header ASSIGNING <header> INDEX 1.
    IF sy-subrc EQ 0.
      TRANSFER <header> TO p_file.
    ENDIF.
    LOOP AT pt_data ASSIGNING <data>.
      CLEAR: lv_temp_data.
      LOOP AT pt_dfies ASSIGNING <dfies>.
        IF sy-tabix NE 1. " Not the first field
          ASSIGN COMPONENT <dfies>-fieldname OF STRUCTURE <data> TO <fval>.
          IF <dfies>-inttype EQ 'C' OR <dfies>-inttype EQ 'N' OR
             <dfies>-inttype EQ 'D' OR <dfies>-inttype EQ 'T'.
            CONCATENATE lv_temp_data <fval> INTO lv_temp_data SEPARATED BY htab.
          ELSE.
            lv_val = <fval>.
            CONCATENATE lv_temp_data lv_val INTO lv_temp_data SEPARATED BY htab.
          ENDIF.
        ELSE.
          ASSIGN COMPONENT <dfies>-fieldname OF STRUCTURE <data> TO <fval>.
          lv_temp_data = <fval>.
        ENDIF.
      ENDLOOP.
      TRANSFER lv_temp_data  TO p_file.
    ENDLOOP.
  ELSE.
    WRITE: / p_tabname ,' - ', lv_error , ' - ',  p_file.
  ENDIF.

  CLOSE DATASET p_file.

ENDFORM.                    " DELIMIT_WRITE_FILE
*&---------------------------------------------------------------------*
*&      Form  WRITE_UNIX_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<DATA>  text
*      -->P_LT_HEADER[]  text
*      -->P_P_FILE  text
*      -->P_LT_DFIES[]  text
*      -->P_P_TAB  text
*----------------------------------------------------------------------*
FORM write_unix_file  USING      ps_data   TYPE any
                                 p_file    TYPE any
                                 pt_dfies  TYPE dfies_table
                                 p_tabname TYPE any.

  DATA: lv_temp_data TYPE LINE OF string_table,
        lv_delimiter TYPE char4,
        lv_val       TYPE string.

  FIELD-SYMBOLS: <dfies>  LIKE LINE OF pt_dfies,
                 <fval>   TYPE any.

  IF rd_xlsx EQ 'X' OR rd_xls EQ 'X' OR rd_tab EQ 'X'.
    lv_delimiter =  htab.
  ELSEIF rd_csv EQ 'X'.
    lv_delimiter = ','.
  ELSE.
    lv_delimiter = p_delim.
  ENDIF.

  CLEAR: lv_temp_data.
  LOOP AT pt_dfies ASSIGNING <dfies>.
    CLEAR: lv_val.
    IF sy-tabix NE 1. " Not the first field
      UNASSIGN: <fval>.
      CLEAR: lv_val.
      ASSIGN COMPONENT <dfies>-fieldname OF STRUCTURE ps_data TO <fval>.
      IF <fval> IS ASSIGNED.
        IF ( <dfies>-inttype EQ 'P' OR <dfies>-inttype EQ 'F' ) AND <fval> LT 0.
          <fval> = <fval> * -1.
          lv_val = <fval>.
          SHIFT lv_val LEFT DELETING LEADING space.
          CONCATENATE '-' lv_val INTO lv_val.
        ELSE.
          lv_val = <fval>.
* Look for commas in field value then wrap around with double quote
          IF ( rd_csv EQ 'X' AND
             ( <dfies>-inttype NE 'N' AND <dfies>-inttype NE 'D' AND
               <dfies>-inttype NE 'T' AND <dfies>-inttype NE 'P' AND
               <dfies>-inttype NE 'F' ) ).
            SEARCH lv_val  FOR ','.
            IF sy-subrc EQ 0.
              CONCATENATE '"' lv_val '"' INTO lv_val.
            ENDIF.
          ENDIF.
        ENDIF.

        PERFORM format_output USING <dfies>-fieldname <dfies>-inttype <dfies>-domname
                                     ps_data p_tabname lv_delimiter
                              CHANGING lv_val.
        IF p_tabs EQ 'SOOD' AND <dfies>-fieldname EQ 'TEXTS' AND lv_val EQ ''.
          CLEAR: lv_temp_data.
          CONTINUE.
        ELSE.
          CONCATENATE lv_temp_data lv_delimiter lv_val INTO lv_temp_data.
        ENDIF.
      ENDIF.
    ELSE.  "First entry of DFIES table
      ASSIGN COMPONENT <dfies>-fieldname OF STRUCTURE ps_data TO <fval>.
      IF <fval> IS ASSIGNED.
        lv_val = <fval>.
      ENDIF.
      PERFORM format_output USING <dfies>-fieldname <dfies>-inttype <dfies>-domname
                                   ps_data p_tabname  lv_delimiter
                            CHANGING lv_val.
      IF p_tabs EQ 'SOOD' AND <dfies>-fieldname EQ 'TEXTS' AND lv_val EQ ''.
        CLEAR: lv_temp_data.
        CONTINUE.
      ENDIF.
      lv_temp_data = lv_val.
    ENDIF.
  ENDLOOP.

  IF p_tabs EQ 'SOOD' AND lv_temp_data EQ ''.

  ELSEIF p_tabs EQ 'SRGBTBREL'.
    TRANSFER ps_data TO p_file.
  ELSE.
    IF p_tabs EQ 'EKPO'.
      PERFORM rmv_special_characters USING lv_temp_data.
    ENDIF.
    TRANSFER lv_temp_data  TO p_file.
  ENDIF.



ENDFORM.                    " WRITE_UNIX_FILE
*&---------------------------------------------------------------------*
*&      Form  SELECT_WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_write_data .

  DATA: lv_pkg_size TYPE i,
        lv_tabname  TYPE tabname,
        db_cursor   TYPE cursor,
        l_refitab   TYPE REF TO data.

  UNASSIGN: <gt_data>.
  REFRESH: gt_cond, gt_ausp, gt_mara_ausp.
  CLEAR: gv_file_no.
  lv_tabname = p_tabs.

  PERFORM read_pkg_and_build_where USING lv_tabname lv_pkg_size.

  IF p_nr EQ 'X'.
    IF p_tabs NE 'AUSP+MARA' AND p_tabs NE 'TOA01' AND p_tabs NE 'DRAD'.
      CREATE DATA l_refitab TYPE TABLE OF (p_tabs).
      ASSIGN l_refitab->* TO <gt_data>.
      PERFORM read_process_reg USING p_tabs lv_pkg_size.
    ELSE.
      PERFORM read_process_excep USING p_tabs lv_pkg_size.
    ENDIF.
  ELSE.
    IF p_tabs NE 'AUSP+MARA' AND
       p_tabs NE 'ADR6'      AND
       p_tabs NE 'IHPA'      AND
       p_tabs NE 'STXL'      AND
       p_tabs NE 'TOA01'     AND
       p_tabs NE 'DRAD'      AND
       p_tabs NE 'SOOD'      AND
       p_tabs NE 'SRGBTBREL' AND
       p_tabs NE 'EKBE'.
      CREATE DATA l_refitab TYPE TABLE OF (p_tabs).
      ASSIGN l_refitab->* TO <gt_data>.
      PERFORM read_process_regular USING p_tabs lv_pkg_size.
    ELSE.
      PERFORM read_process_exception USING p_tabs lv_pkg_size.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECT_WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_PKG_SIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3985   text
*----------------------------------------------------------------------*
FORM read_pkg_size  USING  p_tabname  TYPE any
                           p_pkg_size TYPE any.

  DATA : lt_dfies       TYPE TABLE OF dfies,
         ls_dfies       TYPE          dfies,
         struc_size     TYPE i VALUE 0,
         ls_key         LIKE LINE OF gt_keys,
         ls_cond        LIKE LINE OF gt_cond,
         field_s         TYPE fieldname,
         lv_lines       TYPE i.

  IF gt_cond[] IS INITIAL.
    lv_lines = lines( gt_keys ).
    LOOP AT gt_keys INTO ls_key.
      IF lv_lines EQ sy-tabix.
        CONCATENATE 'S_' ls_key-fieldname INTO field_s.
        CONCATENATE ls_key-fieldname 'IN' field_s '.'
                    INTO ls_cond-cond SEPARATED BY space.
        APPEND ls_cond TO gt_cond.
      ELSE.
        CONCATENATE 'S_' ls_key-fieldname INTO field_s.
        CONCATENATE ls_key-fieldname 'IN' field_s 'AND'
                    INTO ls_cond-cond SEPARATED BY space.
        APPEND ls_cond TO gt_cond.
      ENDIF.
    ENDLOOP.
  ENDIF.

***** To calculaten maximum no of records that can be accomodated in 2gb
  p_pkg_size = p_pkg / gv_struc_size.

ENDFORM.                    " READ_PKG_SIZE
*&---------------------------------------------------------------------*
*&      Form  CREATE_TABLES_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_tables_list .

  DATA: name   TYPE vrm_id,
        value   LIKE LINE OF gt_list.

  REFRESH: gt_list.

  name = 'P_TABS'.
  value-key = 'LFA1'.
  value-text = 'Vendor Master'.
  APPEND value TO gt_list.

  value-key = 'EKBE'.
  value-text = 'History per Purchasing Document'.
  APPEND value TO gt_list.

  value-key = 'LFB1'.
  value-text = 'Vendor Master'.
  APPEND value TO gt_list.

  value-key = 'KNA1'.
  value-text = 'Customer Master'.
  APPEND value TO gt_list.

  value-key = 'KNB1'.
  value-text = 'Customer Master'.
  APPEND value TO gt_list.

  value-key = 'PRPS'.
  value-text = 'WBS Element'.
  APPEND value TO gt_list.

  value-key = 'PROJ'.
  value-text = 'Projects'.
  APPEND value TO gt_list.

  value-key = 'AUFK'.
  value-text = 'Work Order'.
  APPEND value TO gt_list.

  value-key = 'T499S'.
  value-text = 'Location'.
  APPEND value TO gt_list.

  value-key = 'IFLOT'.
  value-text = 'Location'.
  APPEND value TO gt_list.

  value-key = 'EQUI'.
  value-text = 'Equipment Master'.
  APPEND value TO gt_list.

  value-key = 'EQST'.
  value-text = 'Equipment to BOM Link'.
  APPEND value TO gt_list.

  value-key = 'EQUZ'.
  value-text = 'Equipment Time Segment'.
  APPEND value TO gt_list.

  value-key = 'EQKT'.
  value-text = 'Equipment Short Texts'.
  APPEND value TO gt_list.

  value-key = 'ILOA'.
  value-text = 'PM Object Location and Account Assignment'.
  APPEND value TO gt_list.

  value-key = 'JEST'.
  value-text = 'Individual Object Status'.
  APPEND value TO gt_list.

  value-key = 'MARA'.
  value-text = 'Material Master'.
  APPEND value TO gt_list.

  value-key  = 'MARC'.
  value-text = 'Material Master'.
  APPEND value TO gt_list.

  value-key  = 'MARD'.
  value-text = 'Storage Location Data for Material'.
  APPEND value TO gt_list.

  value-key  = 'PROP'.
  value-text = 'Forecast parameters'.
  APPEND value TO gt_list.

  value-key  = 'EINA'.
  value-text = 'Purchasing Info Record'.
  APPEND value TO gt_list.

  value-key  = 'AUSP'.
  value-text = 'Material Characteristics'.
  APPEND value TO gt_list.

  value-key  = 'AUSP+MARA'.
  value-text = 'MARA with Characteristics'.
  APPEND value TO gt_list.

  value-key = 'T001W'.
  value-text = 'Location Hierarchy Master'.
  APPEND value TO gt_list.

  value-key = 'BNKA'.
  value-text = 'Bank Master'.
  APPEND value TO gt_list.

  value-key = 'T012'.
  value-text = 'Bank Master'.
  APPEND value TO gt_list.

  value-key = 'ANLA'.
  value-text = 'Asset Master'.
  APPEND value TO gt_list.

  value-key = 'ANLH'.
  value-text = 'Asset Master'.
  APPEND value TO gt_list.

  value-key = 'T001L'.
  value-text = 'Inventory Location'.
  APPEND value TO gt_list.
***********************
  value-key  = 'MBEW'.
  value-text = 'Material Valuation'.
  APPEND value TO gt_list.

  value-key  = 'ADR6'.
  value-text = 'E-Mail Addresses (Business Address Services)'.
  APPEND value TO gt_list.

  value-key  = 'EKKO'.
  value-text = 'Purchasing Document Header'.
  APPEND value TO gt_list.

  value-key  = 'LFM1'.
  value-text = 'Vendor master record purchasing organization data'.
  APPEND value TO gt_list.

  value-key  = 'NAST'.
  value-text = 'Message Status'.
  APPEND value TO gt_list.

  value-key  = 'T024'.
  value-text = 'Purchasing Groups'.
  APPEND value TO gt_list.

  value-key  = 'EKPO'.
  value-text = 'Purchasing Document Line Item'.
  APPEND value TO gt_list.

  value-key  = 'EKET'.
  value-text = 'Scheduling Agreement Schedule Lines'.
  APPEND value TO gt_list.

  value-key = 'BKPF'.
  value-text = 'Accounting Document Header'.
  APPEND value TO gt_list.

  value-key = 'BSEG'.
  value-text = 'Accounting Document Segment'.
  APPEND value TO gt_list.

  value-key = 'T001'.
  value-text = 'Company Codes'.
  APPEND value TO gt_list.

  value-key = 'TJ02T'.
  value-text = 'System status texts'.
  APPEND value TO gt_list.

  value-key = 'BPJA'.
  value-text = 'Totals Record for Annual Total - Controlling Obj.'.
  APPEND value TO gt_list.

  value-key = 'PRTE'.
  value-text = 'Scheduling Data for Project Item'.
  APPEND value TO gt_list.

  value-key = 'VBSEGK'.
  value-text = 'Doc. Segment for Vendor Doc. Parking'.
  APPEND value TO gt_list.

  value-key = 'BSIK'.
  value-text = 'Accounting: Secondary Index for Vendors'.
  APPEND value TO gt_list.

  value-key = 'VBSEGD'.
  value-text = 'Doc. Segment for Customer Doc. Parking'.
  APPEND value TO gt_list.

  value-key = 'BSID'.
  value-text = 'Accounting: Secondary Index for Customers'.
  APPEND value TO gt_list.

  value-key = 'COEJ'.
  value-text = 'CO Object: Line Items (by Fiscal Year)'.
  APPEND value TO gt_list.

  value-key = 'CSKS'.
  value-text = 'Cost Center Master Data'.
  APPEND value TO gt_list.

  value-key = 'EKKN'.
  value-text = 'Account Assignment in Purchasing Document'.
  APPEND value TO gt_list.

  value-key = 'IHPA'.
  value-text = 'Plant Maintenance: Partners'.
  APPEND value TO gt_list.

  value-key = 'KSSK'.
  value-text = 'Allocation Table: Object to Class'.
  APPEND value TO gt_list.

  value-key = 'T006'.
  value-text = 'Units of Measurement'.
  APPEND value TO gt_list.

  value-key = 'T006A'.
  value-text = 'Assign Internal to Language-Dependent Unit'.
  APPEND value TO gt_list.

  value-key = 'T023T'.
  value-text = 'Material Group Descriptions'.
  APPEND value TO gt_list.

  value-key = 'T399D'.
  value-text = 'Control Parameters for MRP'.
  APPEND value TO gt_list.

  value-key = 'TCJ04'.
  value-text = 'Person in Responsible for Project'.
  APPEND value TO gt_list.

  value-key = 'TJ03'.
  value-text = 'Object types'.
  APPEND value TO gt_list.

  value-key = 'TKT09'.
  value-text = 'Texts for versions in master tables'.
  APPEND value TO gt_list.

  value-key = 'AFIH'.
  value-text = 'Maintenance order header'.
  APPEND value TO gt_list.

  value-key = 'INOB'.
  value-text = 'Link between Internal Number and Object'.
  APPEND value TO gt_list.

  value-key = 'MSEG'.
  value-text = 'Document Segment: Material'.
  APPEND value TO gt_list.

  value-key = 'STXL'.
  value-text = 'STXD SAPscript text file lines'.
  APPEND value TO gt_list.

  value-key = 'MAKT'.
  value-text = 'Material Descriptions'.
  APPEND value TO gt_list.

  value-key = 'QMFE'.
  value-text = 'Quality notification - items'.
  APPEND value TO gt_list.

  value-key = 'STAS'.
  value-text = 'BOMs - Item Selection'.
  APPEND value TO gt_list.

  value-key = 'TOA01'.
  value-text = 'Link table 1'.
  APPEND value TO gt_list.

  value-key = 'DRAD'.
  value-text = 'Document-Object Link'.
  APPEND value TO gt_list.

  value-key = 'SOOD'.
  value-text = 'SAPoffice: Object definition'.
  APPEND value TO gt_list.

  value-key = 'SRGBTBREL'.
  value-text = 'Relationships in GOS Environment'.
  APPEND value TO gt_list.

  value-key = 'ADR2'.
  value-text = 'Telephone Numbers (Business Address Services)'.
  APPEND value TO gt_list.

  value-key = 'ADR3'.
  value-text = 'Fax Numbers (Business Address Services)'.
  APPEND value TO gt_list.

  value-key = 'ADRC'.
  value-text = 'Addresses (Business Address Services)'.
  APPEND value TO gt_list.

  value-key = 'KNVK'.
  value-text = 'Customer Master Contact Partner'.
  APPEND value TO gt_list.

  value-key = 'LFBW'.
  value-text = 'Vendor master record (withholding tax types) X'.
  APPEND value TO gt_list.

  value-key = 'LFM1'.
  value-text = 'Vendor master record purchasing organization data'.
  APPEND value TO gt_list.

  value-key = 'WYT3'.
  value-text = 'Partner Functions'.
  APPEND value TO gt_list.

  SORT gt_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = name
      values = gt_list.

ENDFORM.                    " CREATE_TABLES_LIST
*&---------------------------------------------------------------------*
*&      Form  SELECT_AND_WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3994   text
*----------------------------------------------------------------------*
FORM select_and_write_data USING p_tabname  TYPE any
                                 p_pkg_size TYPE any.



ENDFORM.                    " SELECT_AND_WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<XTAB>  text
*----------------------------------------------------------------------*
FORM write_files  USING  pt_data TYPE STANDARD TABLE
                         p_tab   TYPE any.

  DATA: lv_tabix      TYPE sytabix,
          lv_count      TYPE i.
  "LV_FILE_NO(4) TYPE N.

  DATA: "lt_header     TYPE truxs_t_text_data,
        lt_header      TYPE string_table,
        ls_header      LIKE LINE OF lt_header,
        ls_tech_header LIKE LINE OF lt_header,
        rd_split_tab  TYPE REF TO data,
        rd_split_line TYPE REF TO data,
        rd_data_line  TYPE REF TO data,
        lt_dfies      TYPE dfies_table,
        lv_file       TYPE string,
        lv_error(75)  TYPE c,
        file_name     TYPE localfile,
        lv_type       TYPE char30,
        lt_ekkn       TYPE STANDARD TABLE OF ty_ekkn,
        lv_internal   TYPE string,
        lv_external   TYPE string.

  FIELD-SYMBOLS: <split_tab>       TYPE STANDARD TABLE,
                 <split_line>      TYPE any,
                 <data>            TYPE any,
                 <internal_format> TYPE any,
                 <external_format> TYPE any.

  CLEAR: gv_lines.
  gv_lines = lines( pt_data ).

  IF p_tab NE 'AUSP+MARA' AND p_tab NE 'TOA01' AND
     p_tab NE 'DRAD'      AND p_tab NE 'SOOD'  AND p_tab NE 'STXL'.
    lv_type = p_tab.
  ELSE.
    IF p_tab EQ 'AUSP+MARA'.
      lv_type = 'TY_MARAAUSP'.
    ELSEIF p_tab EQ 'AUSP'.
      lv_type = 'TY_MAT_CHARAC'.
    ELSEIF p_tab EQ 'TOA01'.
      lv_type = 'TY_TOA01'.
    ELSEIF p_tab EQ 'DRAD'.
      lv_type = 'TY_DRAD'.
    ELSEIF p_tab EQ 'SOOD'.
      lv_type = 'TY_SOODK'.
    ELSEIF p_tab EQ 'STXL'.
      lv_type = 'TY_STXL'.
    ENDIF.
  ENDIF.

  CREATE DATA rd_split_tab TYPE STANDARD TABLE OF (lv_type).
  ASSIGN rd_split_tab->* TO <split_tab>.

  CREATE DATA rd_split_line TYPE (lv_type).
  ASSIGN rd_split_line->* TO <split_line>.

  CREATE DATA rd_data_line TYPE (lv_type).
  ASSIGN rd_data_line->* TO <data>.

  IF rd_pc EQ 'X'.
    file_name = p_file.
  ELSE.
    file_name = p_appf.
  ENDIF.

  IF rd_pc EQ 'X'. " PC file
    LOOP AT pt_data ASSIGNING <data>.
      lv_tabix     = sy-tabix.
      lv_count     = lv_count + 1.
      APPEND INITIAL LINE TO <split_tab> ASSIGNING <split_line>.
      <split_line> = <data>.
      IF ( lv_count EQ p_recs ) OR ( lv_tabix EQ gv_lines ).
        CLEAR: lv_file.
        gv_file_no = gv_file_no + 1.
        SHIFT gv_file_no LEFT DELETING LEADING '0'.
        PERFORM generate_filename USING file_name gv_file_no '' p_tab CHANGING lv_file.
        PERFORM write_pcfile USING p_tab <split_tab>[] lv_file.
        FREE: <split_tab>[], lv_count.
      ENDIF.
    ENDLOOP.
  ELSE.  "App server file
    LOOP AT pt_data ASSIGNING <data>.
      lv_tabix = sy-tabix.
      lv_count = lv_count + 1.
* Build header reacord only once
      IF lv_tabix EQ 1.
        PERFORM build_header USING p_tab ls_header ls_tech_header lt_header[] CHANGING lt_dfies[].
      ENDIF.

      IF lv_count EQ 1.
* Build file number sequence
        IF gv_file_no IS INITIAL.
          gv_file_no = 1.
        ELSE.
          gv_file_no = gv_file_no + 1.
        ENDIF.
        CLEAR: lv_file.
        PERFORM generate_filename USING file_name gv_file_no '' p_tab CHANGING lv_file.
        OPEN DATASET lv_file FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc EQ 0.
          TRANSFER ls_header TO lv_file.
          TRANSFER ls_tech_header TO lv_file.
        ELSE.
          WRITE: / p_tab ,' - ', lv_error , ' - ',  lv_file.
          EXIT.
        ENDIF.
      ENDIF.

      PERFORM write_unix_file USING <data> lv_file lt_dfies[] p_tab.
      IF lv_count EQ p_recs OR lv_tabix EQ gv_lines.
* Close the file if it's the last record or or number of records match
        WRITE: / p_tab ,' - ', 'File successful' , ' - ',  lv_file.
        CLOSE DATASET lv_file.
        CLEAR: lv_count.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " WRITE_FILES
*&---------------------------------------------------------------------*
*&      Form  GET_TABLE_KEYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_table_keys .

  DATA : lt_dfies       TYPE TABLE OF dfies,
         ls_key         LIKE LINE OF gt_keys.

  DATA: lv_tabname TYPE ddobjname.

  FIELD-SYMBOLS: <dfies> LIKE LINE OF gt_dfies.

  REFRESH: gt_keys, r_keys.
  CLEAR: gv_struc_size.

  IF p_tabs NE 'AUSP+MARA'.
    lv_tabname = p_tabs.
  ELSE.
    lv_tabname = 'AUSP'.
  ENDIF.

  CALL FUNCTION 'DDIF_NAMETAB_GET'
    EXPORTING
      tabname   = lv_tabname
    TABLES
      dfies_tab = gt_dfies
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

  IF sy-subrc EQ 0.
    LOOP AT gt_dfies ASSIGNING <dfies>.
      gv_struc_size = gv_struc_size + <dfies>-leng.
      IF <dfies>-keyflag   EQ 'X'     AND
         <dfies>-fieldname NE 'MANDT' AND
         <dfies>-fieldname NE 'CLIENT'.

        ls_key-fieldname = <dfies>-fieldname.
        APPEND ls_key TO gt_keys.

        r_keys-low    = <dfies>-fieldname.
        r_keys-option = 'EQ'.
        r_keys-sign   = 'I'.
        APPEND r_keys.
      ENDIF.
    ENDLOOP.

    IF p_tabs EQ 'BKPF'.
      PERFORM build_exception_keys.
    ELSEIF p_tabs EQ 'IHPA'.
      PERFORM build_exception_keys.
    ELSEIF p_tabs EQ 'ADR6'.
      REFRESH: gt_keys, r_keys.
      PERFORM build_exception_keys.
    ELSEIF p_tabs EQ 'STXL'.
      PERFORM build_exception_keys.
    ELSEIF p_tabs EQ 'TOA01'.
      PERFORM build_exception_keys.
    ELSEIF p_tabs EQ 'DRAD'.
      PERFORM build_exception_keys.
      DELETE gt_keys WHERE fieldname EQ 'OBJKY'.
      DELETE r_keys WHERE low EQ 'OBJKY'.
    ELSEIF p_tabs EQ 'DRAT'.
      PERFORM build_exception_keys.
      DELETE gt_keys WHERE fieldname EQ 'DOKAR' OR fieldname EQ 'DOKNR' OR fieldname EQ 'LANGU'.
      DELETE r_keys WHERE low EQ 'DOKAR' OR low EQ 'DOKNR' OR low EQ 'LANGU'.
    ELSEIF p_tabs EQ 'PRPS'.
      PERFORM build_exception_keys.
      DELETE gt_keys WHERE fieldname EQ 'PSPNR'.
      DELETE r_keys WHERE low EQ 'PSPNR'.
    ELSEIF p_tabs EQ 'SOOD'.
      PERFORM build_exception_keys.
    ELSEIF p_tabs EQ 'SRGBTBREL'.
      PERFORM build_exception_keys.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_TABLE_KEYS
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCRN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_scrn .

  DATA: lv_fld1 TYPE fieldname,
        lv_fld2 TYPE fieldname,
        lv_name(132) TYPE c.

  FIELD-SYMBOLS: <key> LIKE LINE OF gt_keys.

  LOOP AT SCREEN.
    CLEAR: lv_fld1, lv_fld2.
    lv_name = screen-name.
    IF screen-name(2) EQ 'S_' OR screen-name(4) EQ '%_S_'.
      IF screen-name(2) EQ 'S_'.
        SHIFT lv_name LEFT BY 2 PLACES.
        SPLIT lv_name AT '-' INTO lv_fld1 lv_fld2.
      ELSE.
        SHIFT lv_name LEFT BY 4 PLACES.
        SPLIT lv_name AT '_' INTO lv_fld1 lv_fld2.
      ENDIF.
      IF lv_fld1 IN r_keys.
        screen-input     = 1.       "Visible
        screen-invisible = 0.
        MODIFY SCREEN.
      ELSE.
        screen-input     = 0.       "Hidden
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF rd_alv EQ 'X'. "ALV report
* Hide all file related parameters when ALV selected
      IF screen-group1 EQ 'RD2' OR
         screen-group1 EQ 'RD4' OR
         screen-group1 EQ 'RD5' OR
         screen-group1 EQ 'RD6' OR
         screen-group1 EQ 'RD7'.
        screen-input     = 0.       "Hidden
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSE. "File write mode
      IF rd_pc EQ 'X'.
        IF screen-group1 EQ 'RD4' OR
           screen-group1 EQ 'RD6'.
          screen-input     = 1.       "Visible
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 EQ 'RD7'. "Server File Path
          screen-input     = 0.       "Hidden
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        CLEAR p_man.
      ELSE. "Server File
        IF screen-group1 EQ 'RD5' OR
           screen-group1 EQ 'RD7'.
          screen-input     = 1.       "Visible
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 EQ 'RD6'. "PC File Path
          screen-input     = 0.       "Hidden
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.


*    IF p_tabs EQ 'STXL' AND p_nr EQ 'X'.
*      IF screen-name EQ 'P_NREC' OR screen-name EQ 'P_NR'.
*        screen-input     = 0.       "Hidden
*        screen-invisible = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.

    IF screen-name EQ 'P_APPF' AND p_man EQ ''.
      screen-input = '0'.
      MODIFY SCREEN.
    ELSEIF p_man EQ 'X' AND screen-name EQ 'P_APPF'.
      screen-input = '1'.
      MODIFY SCREEN.
    ENDIF.

    IF p_pk EQ 'X'  AND screen-group1 EQ 'ALV'.
      screen-input     = 0.       "Hidden
      screen-invisible = 1.
      MODIFY SCREEN.
      CLEAR rd_alv.
      rd_file = 'X'.
    ELSEIF p_pk EQ '' AND screen-group1 EQ 'ALV'.
      screen-input     = 1.       "Visible
      screen-invisible = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name EQ 'P_REC'.
      p_rec = gv_struc_size.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_SCRN
*&---------------------------------------------------------------------*
*&      Form  HIDE_SHOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SCREEN  text
*----------------------------------------------------------------------*
FORM hide_show  USING  p_group  TYPE any
                       p_screen TYPE any.

  DATA: lv_fld1 TYPE fieldname,
         lv_fld2 TYPE fieldname.

  FIELD-SYMBOLS: <key> LIKE LINE OF gt_keys.
  LOOP AT SCREEN.
    IF screen-group1 EQ 'VEN'.
      IF screen-name(2) EQ 'S_'.
        SPLIT screen-name AT '-' INTO lv_fld1 lv_fld2.
        SHIFT lv_fld1 LEFT DELETING LEADING 'S_'.
        LOOP AT gt_keys ASSIGNING <key>.
          IF lv_fld1 EQ <key>-fieldname.
            screen-input     = 1.       "Visible
            screen-invisible = 0.
            MODIFY SCREEN.
          ELSE.
            screen-input     = 0.       "Hidden
            screen-invisible = 1.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " HIDE_SHOW
*&---------------------------------------------------------------------*
*&      Form  READ_PKG_AND_BUILD_WHERE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TABNAME  text
*      -->P_LV_PKG_SIZE  text
*----------------------------------------------------------------------*
FORM read_pkg_and_build_where  USING    p_tabname  TYPE any
                           p_pkg_size TYPE any.

  DATA : ls_key   LIKE LINE OF gt_keys,
         ls_cond  LIKE LINE OF gt_cond,
         field_s  TYPE fieldname,
         lv_lines TYPE i.

  CHECK p_nowhr EQ space.

  IF gt_cond[] IS INITIAL.
    IF p_tabs NE 'ADR6' AND
       p_tabs NE 'IHPA' AND
       p_tabs NE 'STXL' AND
       p_tabs NE 'PRPS' AND
       p_tabs NE 'DRAT'.
      lv_lines = lines( gt_keys ).
      LOOP AT gt_keys INTO ls_key.
        IF lv_lines EQ sy-tabix.
          CONCATENATE 'S_' ls_key-fieldname INTO field_s.
          CONCATENATE ls_key-fieldname 'IN' field_s '.'
                      INTO ls_cond-cond SEPARATED BY space.
          APPEND ls_cond TO gt_cond.
        ELSE.
          CONCATENATE 'S_' ls_key-fieldname INTO field_s.
          CONCATENATE ls_key-fieldname 'IN' field_s 'AND'
                      INTO ls_cond-cond SEPARATED BY space.
          APPEND ls_cond TO gt_cond.
        ENDIF.
      ENDLOOP.
    ELSE.
      IF p_tabs EQ 'PRPS'.
        CONCATENATE 'PSPNR' 'IN' 'S_PSPNR2' 'AND'
                      INTO ls_cond-cond SEPARATED BY space.
        APPEND ls_cond TO gt_cond.

        CONCATENATE 'PBUKR' 'IN' 'S_PBUKR' '.'
                      INTO ls_cond-cond SEPARATED BY space.
        APPEND ls_cond TO gt_cond.

      ELSEIF p_tabs EQ 'DRAT'.

        CONCATENATE 'DOKAR' 'IN' 'S_DOKAR2' 'AND'
                     INTO ls_cond-cond SEPARATED BY space.
        APPEND ls_cond TO gt_cond.

        CONCATENATE 'DOKNR' 'IN' 'S_DOKNR2' 'AND'
                      INTO ls_cond-cond SEPARATED BY space.
        APPEND ls_cond TO gt_cond.

        CONCATENATE 'DOKVR' 'IN' 'S_DOKVR' 'AND'
                      INTO ls_cond-cond SEPARATED BY space.
        APPEND ls_cond TO gt_cond.

        CONCATENATE 'DOKTL' 'IN' 'S_DOKTL' 'AND'
                      INTO ls_cond-cond SEPARATED BY space.
        APPEND ls_cond TO gt_cond.

        CONCATENATE 'LANGU' 'IN' 'S_LANGU2' '.'
                      INTO ls_cond-cond SEPARATED BY space.
        APPEND ls_cond TO gt_cond.
      ENDIF.
    ENDIF.
  ENDIF.

***** To calculaten maximum no of records that can be accomodated in 2gb
  CHECK gv_struc_size IS NOT INITIAL.
  p_pkg_size = p_pkg / gv_struc_size.

ENDFORM.                    " READ_PKG_AND_BUILD_WHERE

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS_REGULAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TABNAME  text
*----------------------------------------------------------------------*
FORM read_process_regular USING p_tabname TYPE any
                                p_pkg     TYPE any.

  DATA: db_cursor TYPE cursor.
  IF p_rbr NE 'X'.
    OPEN CURSOR WITH HOLD db_cursor FOR
    SELECT * FROM (p_tabname) BYPASSING BUFFER WHERE (gt_cond).
    DO.
*** To Fetch data in chunks of 2gb
      FETCH NEXT CURSOR db_cursor INTO TABLE <gt_data> PACKAGE SIZE p_pkg.
      IF sy-subrc NE 0.
        CLOSE CURSOR db_cursor.
        EXIT.
      ELSE.
        IF rd_alv EQ 'X'.
          CALL SCREEN 9010.
        ELSE.
          PERFORM write_files USING <gt_data> p_tabname.
        ENDIF.
      ENDIF.
    ENDDO.
  ELSE.
******Read chunk of data using Select UP TO rows*******

    DATA: lv_dbcnt  TYPE sydbcnt.

    IF p_tabname EQ 'MARA'.
      DATA: gt_mara   TYPE STANDARD TABLE OF mara,
           ls_mara   LIKE LINE OF gt_mara,
           hold_mara LIKE LINE OF gt_mara.
      DO.
        SELECT * FROM (p_tabname) UP TO p_rn ROWS INTO TABLE gt_mara
        WHERE matnr IN s_matnr
        AND   matnr >= ls_mara-matnr.
        IF gt_mara[] IS NOT INITIAL.
          lv_dbcnt = sy-dbcnt.
          READ TABLE gt_mara INTO ls_mara INDEX 1.
          IF hold_mara = ls_mara.
* GT_MARA will always have it's first record from 2nd loop pass onwards
* same as last record of the previous loop pass so delete the first
* record since we already wrote the previous file with that record
            DELETE gt_mara INDEX 1.
          ENDIF.
* Read last record so it could be used in where condition
          READ TABLE gt_mara INTO ls_mara INDEX lv_dbcnt.
          PERFORM write_files USING gt_mara p_tabname.
          CLEAR: gt_mara[].
          hold_mara = ls_mara.
        ELSE.
          EXIT.
        ENDIF.
        IF p_rn NE lv_dbcnt.
          EXIT.
        ENDIF.
      ENDDO.
    ELSEIF p_tabname EQ 'MARC'.
      PERFORM test_open_cursor.
*      PERFORM READ_MARC.
*      PERFORM READ_MARC_BY_KEYS.
    ELSEIF p_tabname EQ 'T001'.
      PERFORM read_t001_by_keys.
    ENDIF.
  ENDIF.
  CLEAR: gv_file_no.

ENDFORM.                    " READ_PROCESS_REGULAR
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS_EXCEPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TABNAME  text
*      -->P_P_PKG_SIZE  text
*----------------------------------------------------------------------*
FORM read_process_exception  USING p_tabname TYPE any
                                   p_pkgsz     TYPE any.

  DATA: db_cursor TYPE cursor.

  TYPES: BEGIN OF ty_srgbtbrel2,
            reltype	  TYPE oblreltype,
            instid_a  TYPE sibfboriid,
            typeid_a  TYPE sibftypeid,
            catid_a	  TYPE sibfcatid,
            instid_b  TYPE sibfboriid,
            typeid_b  TYPE sibftypeid,
            catid_b	  TYPE sibfcatid,
           END OF ty_srgbtbrel2.


  DATA: lt_srgbtbrel2 TYPE STANDARD TABLE OF srgbtbrel.

  DATA: lta_solix     TYPE solix_tab,
        lt_string     TYPE string_table,
        ls_string     LIKE LINE OF lt_string,
        gt_solix      TYPE solix_tab.

  DATA: ltp_docid      TYPE sofolenti1-doc_id      ,
      lwa_doc_data   TYPE sofolenti1,
      lta_objhdr      TYPE STANDARD TABLE OF solisti1,
      lta_objcont     TYPE STANDARD TABLE OF solisti1.

  FIELD-SYMBOLS: <srgbtbrel2> LIKE LINE OF lt_srgbtbrel2.

  FIELD-SYMBOLS: <ausp>   LIKE LINE OF gt_ausp,
                 <charac> LIKE LINE OF gt_mat_charac.

  TYPES: BEGIN OF ty_srgbtbrel,
           reltype   TYPE oblreltype,
           typeid_a	 TYPE sibftypeid,
           typeid_b	 TYPE sibftypeid,
           instid_a  TYPE sibfboriid,
           instid_b  TYPE sibfboriid,
         END OF ty_srgbtbrel,

         BEGIN OF ty_sood,
           objtp  TYPE  so_obj_tp,
           objyr  TYPE so_obj_yr,
           objno  TYPE so_obj_no,
           objdes TYPE so_obj_des,
         END OF ty_sood.


  DATA: lt_srgbtbrel TYPE STANDARD TABLE OF ty_srgbtbrel,
        lt_sood      TYPE STANDARD TABLE OF ty_sood,
        lt_soodk     TYPE STANDARD TABLE OF soodk,
        ls_sodk      LIKE LINE OF gt_soodk,
        lt_soli      TYPE STANDARD TABLE OF soli.

  FIELD-SYMBOLS: <srgbtbrel> LIKE LINE OF lt_srgbtbrel,
                 <soli>      LIKE LINE OF lt_soli,
                 <sood>      LIKE LINE OF lt_sood,
                 <sodk>      LIKE LINE OF gt_soodk.

  IF p_tabname EQ 'AUSP'.
    REFRESH: gt_ausp,gt_mat_charac.
    OPEN CURSOR WITH HOLD db_cursor FOR
        SELECT a~matnr b~objek b~atinn b~atzhl b~mafid
                     b~klart b~adzhl b~atwrt c~atbez
                             FROM mara AS a INNER JOIN ausp AS b
                             ON    a~matnr = b~objek
                             INNER JOIN cabnt AS c ON b~atinn = c~atinn AND b~adzhl = c~adzhl
                             WHERE b~objek IN s_objek
                             AND   b~atinn IN s_atinn
                             AND   b~atzhl IN s_atzhl
                             AND   b~mafid IN s_mafid
                             AND   b~klart IN s_klart
                             AND   b~adzhl IN s_adzhl
                             AND   c~spras EQ sy-langu.
    DO.
      FETCH NEXT CURSOR db_cursor
                INTO TABLE gt_ausp
                PACKAGE SIZE p_pkgsz.
      IF sy-subrc NE 0.
        CLOSE CURSOR db_cursor.
        EXIT.
      ELSE. "Fetch success
        LOOP AT gt_ausp ASSIGNING <ausp>.
          APPEND INITIAL LINE TO gt_mat_charac ASSIGNING <charac>.
          <charac>-matnr = <ausp>-matnr.
          <charac>-atwrt = <ausp>-atwrt.
          <charac>-atbez = <ausp>-atbez.
        ENDLOOP.
        IF rd_alv EQ 'X'.
          CALL SCREEN 9010.
        ELSE.
          PERFORM write_files USING gt_mat_charac[] p_tabname.
        ENDIF.
      ENDIF.
    ENDDO.
  ELSEIF p_tabname EQ 'EKBE'.

    REFRESH: gt_ekbe.
    OPEN CURSOR WITH HOLD db_cursor FOR
    SELECT * FROM (p_tabname) BYPASSING BUFFER.
    DO.
*** To Fetch data in chunks of 2gb
      FETCH NEXT CURSOR db_cursor INTO TABLE gt_ekbe PACKAGE SIZE p_pkgsz.
      IF sy-subrc NE 0.
        CLOSE CURSOR db_cursor.
        EXIT.
      ELSE.
        IF rd_alv EQ 'X'.
          CALL SCREEN 9010.
        ELSE.
          PERFORM write_files USING gt_ekbe p_tabname.
        ENDIF.
      ENDIF.
    ENDDO.
    CLEAR: gv_file_no.

  ELSEIF p_tabname EQ 'AUSP+MARA'. "MARA & AUSP
    REFRESH: gt_mara_ausp.
    OPEN CURSOR WITH HOLD db_cursor FOR
           SELECT * FROM mara AS a INNER JOIN ausp AS b
                                ON    a~matnr = b~objek
                                INNER JOIN cabnt AS c ON b~atinn = c~atinn AND b~adzhl = c~adzhl
                                WHERE b~objek IN s_objek
                                AND   b~atinn IN s_atinn
                                AND   b~atzhl IN s_atzhl
                                AND   b~mafid IN s_mafid
                                AND   b~klart IN s_klart
                                AND   b~adzhl IN s_adzhl
                                AND   c~spras EQ sy-langu.
    DO.
      FETCH NEXT CURSOR db_cursor
                INTO CORRESPONDING FIELDS OF TABLE gt_mara_ausp
                PACKAGE SIZE p_pkgsz.
      IF sy-subrc NE 0.
        CLOSE CURSOR db_cursor.
        EXIT.
      ELSE. "Fetch success
        IF rd_alv EQ 'X'.
          CALL SCREEN 9010.
        ELSE.
          PERFORM write_files USING gt_mara_ausp[] p_tabname.
        ENDIF.
      ENDIF.
    ENDDO.
  ELSEIF p_tabname EQ 'ADR6'.
    REFRESH: gt_adr6.
    OPEN CURSOR WITH HOLD db_cursor FOR
    SELECT * FROM (p_tabname) BYPASSING BUFFER
    WHERE addrnumber IN s_adr61 AND
          persnumber IN s_adr62 AND
          date_from  IN s_adr63 AND
          consnumber IN s_adr64.
    DO.
*** To Fetch data in chunks of 2gb
      FETCH NEXT CURSOR db_cursor INTO TABLE gt_adr6 PACKAGE SIZE p_pkgsz.
      IF sy-subrc NE 0.
        CLOSE CURSOR db_cursor.
        EXIT.
      ELSE.
        IF rd_alv EQ 'X'.
          CALL SCREEN 9010.
        ELSE.
          PERFORM write_files USING gt_adr6 p_tabname.
        ENDIF.
      ENDIF.
    ENDDO.
    CLEAR: gv_file_no.
  ELSEIF p_tabname EQ 'IHPA'.
    REFRESH: gt_ihpa.
    OPEN CURSOR WITH HOLD db_cursor FOR
    SELECT * FROM (p_tabname) BYPASSING BUFFER
    WHERE objnr   IN s_objnr AND
          parvw   IN s_parvw AND
          counter IN s_ihpa3.
    DO.
*** To Fetch data in chunks of 2gb
      FETCH NEXT CURSOR db_cursor INTO TABLE gt_ihpa PACKAGE SIZE p_pkgsz.
      IF sy-subrc NE 0.
        CLOSE CURSOR db_cursor.
        EXIT.
      ELSE.
        IF rd_alv EQ 'X'.
          CALL SCREEN 9010.
        ELSE.
          PERFORM write_files USING gt_ihpa p_tabname.
        ENDIF.
      ENDIF.
    ENDDO.
  ELSEIF p_tabname EQ 'STXL'.
    REFRESH: gt_stxl.
    OPEN CURSOR WITH HOLD db_cursor FOR
    SELECT mandt relid tdobject tdname tdid tdspras srtf2 clustr
           FROM (p_tabname) BYPASSING BUFFER
    WHERE relid    IN s_relid  AND
          tdobject IN s_tdobje AND
          tdname   IN s_tdname AND
          tdid     IN s_tdid   AND
          tdspras  IN s_tdspra AND
          srtf2    IN s_srtf2.
    DO.
*** To Fetch data in chunks of 2gb
      FETCH NEXT CURSOR db_cursor INTO TABLE gt_stxl PACKAGE SIZE p_pkg.
      IF sy-subrc NE 0.
        CLOSE CURSOR db_cursor.
        EXIT.
      ELSE.
        IF rd_alv EQ 'X'.
          CALL SCREEN 9010.
        ELSE.
          PERFORM write_files USING gt_stxl p_tabname.
        ENDIF.
      ENDIF.
    ENDDO.

******Read chunk of data using Select UP TO rows*******
*    DATA: LS_STXL   LIKE LINE OF GT_STXL,
*          HOLD_STXL LIKE LINE OF GT_STXL,
*          LV_DBCNT  TYPE SYDBCNT.
*
*    DO.
*      SELECT MANDT RELID TDOBJECT TDNAME TDID TDSPRAS SRTF2 CLUSTR
*           FROM (P_TABNAME) UP TO P_PKG ROWS INTO TABLE GT_STXL
*    WHERE RELID    IN S_RELID  AND
*          TDOBJECT IN S_TDOBJE AND
*          TDNAME   IN S_TDNAME AND
*          TDID     IN S_TDID   AND
*          TDSPRAS  IN S_TDSPRA AND
*          SRTF2    IN S_SRTF2  AND
*        ( RELID    >= LS_STXL-RELID AND
*          TDOBJECT >= LS_STXL-TDOBJECT AND
*          TDNAME   >= LS_STXL-TDNAME  AND
*          TDID     >= LS_STXL-TDID AND
*          TDSPRAS  >= LS_STXL-TDSPRAS AND
*          SRTF2    >= LS_STXL-SRTF2 ).
*      IF GT_STXL[] IS NOT INITIAL.
*        LV_DBCNT = SY-DBCNT.
*        READ TABLE GT_STXL INTO LS_STXL INDEX 1.
*        IF HOLD_STXL = LS_STXL.
** GT_STXL will always have it's first record from 2nd loop pass onwards
** same as last record of the previous loop pass so delete the first
** record since we already wrote the previous file with that record
*          DELETE GT_STXL INDEX 1.
*        ENDIF.
** Read last record so it could be used in where condition
*        READ TABLE GT_STXL INTO LS_STXL INDEX LV_DBCNT.
*        PERFORM WRITE_FILES USING GT_STXL P_TABNAME.
*        CLEAR: GT_STXL[].
*        HOLD_STXL = LS_STXL.
*      ELSE.
*        EXIT.
*      ENDIF.
*      IF P_PKG NE LV_DBCNT.
*        EXIT.
*      ENDIF.
*    ENDDO.
***********************************************
  ELSEIF p_tabname EQ 'TOA01'.
    REFRESH: gt_toa01.
    OPEN CURSOR WITH HOLD db_cursor FOR
              SELECT a~sap_object a~object_id a~archiv_id
                     a~arc_doc_id a~ar_object a~ar_date
                     a~del_date a~reserve b~arc_doc_id b~filename
                     b~creator b~descr b~creatime
                     FROM toa01 AS a INNER JOIN toaat AS b
                     ON    a~arc_doc_id = b~arc_doc_id
                     WHERE a~sap_object IN s_sapobj
                     AND   a~object_id  IN s_aobjid
                     AND   a~archiv_id  IN s_archid
                     AND   a~arc_doc_id IN s_arcdid.
    DO.
      FETCH NEXT CURSOR db_cursor
                INTO CORRESPONDING FIELDS OF TABLE gt_toa01
                PACKAGE SIZE p_pkgsz.
      IF sy-subrc NE 0.
        CLOSE CURSOR db_cursor.
        EXIT.
      ELSE. "Fetch success
        IF rd_alv EQ 'X'.
          CALL SCREEN 9010.
        ELSE.
          PERFORM write_files USING gt_toa01[] p_tabname.
        ENDIF.
      ENDIF.
    ENDDO.
  ELSEIF p_tabname EQ 'DRAD'.

    DATA:t_cd TYPE STANDARD TABLE OF dms_ph_cd1,
         lv_tabix TYPE sytabix,
         lv_loid TYPE sdok_loid,
         lv_objid TYPE sdok_loid.

    REFRESH:gt_drad.
    OPEN CURSOR WITH HOLD db_cursor FOR
        SELECT a~mandt a~dokar a~doknr a~dokvr a~doktl
                         a~dokob a~obzae a~objky a~vrkstat a~vrkstat1
                         a~vobj a~vkey a~vdir a~viewflag a~delflag a~longtext_id
                         a~cad_pos a~cm_fixed b~mandt b~dokar  b~doknr
                         b~dokvr b~doktl  b~lo_type b~lo_index b~lo_objid b~lo_is_ref
                         c~mandt c~phio_id c~ph_class c~loio_id
                         c~lo_class c~state c~descript c~langu c~crea_user
                         c~crea_time c~chng_user c~chng_time c~stor_cat
                         c~doc_format c~doc_prot c~prop01 c~prop02 c~prop03
                         c~prop04 c~prop05 c~prop06 c~prop07 c~prop08 c~prop09 c~prop10
                         d~mandt d~dokar d~doknr d~dokvr d~doktl d~langu d~dktxt d~ltxin d~dktxt_uc
                         FROM drad AS a INNER JOIN dms_doc2loio AS b ON  a~dokar = b~dokar
                         AND a~doknr = b~doknr
                         AND a~dokvr = b~dokvr
                         AND a~doktl = b~doktl

                         INNER JOIN drat AS d ON a~dokar = d~dokar
                         AND a~doknr = d~doknr
                         AND a~dokvr = d~dokvr
                         AND a~doktl = d~doktl

                         INNER JOIN dms_ph_cd1 AS c ON b~lo_objid = c~loio_id
                         WHERE a~dokar IN s_dokar
                         AND   a~doknr IN s_doknr
                         AND   a~objky IN s_obky.
    DO.
      FETCH NEXT CURSOR db_cursor
                INTO CORRESPONDING FIELDS OF TABLE gt_drad
                PACKAGE SIZE p_pkgsz.
      IF sy-subrc NE 0.
        CLOSE CURSOR db_cursor.
        EXIT.
      ELSE. "Fetch success
        IF rd_alv EQ 'X'.
          CALL SCREEN 9010.
        ELSE.
          PERFORM write_files USING gt_drad[] p_tabname.
        ENDIF.
      ENDIF.
    ENDDO.
  ELSEIF p_tabname EQ 'SOOD'.
    REFRESH: gt_soodk.
    SELECT reltype typeid_a typeid_b instid_a instid_b
                           INTO TABLE lt_srgbtbrel
                           FROM srgbtbrel
                           WHERE reltype IN s_objyr
                           AND typeid_a IN s_objno
                           AND instid_a IN s_objtp
                           AND typeid_b IN s_tidb.

    LOOP AT lt_srgbtbrel ASSIGNING <srgbtbrel>.
      APPEND INITIAL LINE TO gt_soodk ASSIGNING <sodk>.
      <sodk>-objtp     = <srgbtbrel>-instid_b+17(3).
      <sodk>-objyr     = <srgbtbrel>-instid_b+20(2).
      <sodk>-objno     = <srgbtbrel>-instid_b+22(12).
      <sodk>-reltype   = <srgbtbrel>-reltype.
      <sodk>-typeid_a  = <srgbtbrel>-typeid_a.
      <sodk>-typeid_b	 = <srgbtbrel>-typeid_b.
      <sodk>-instid_a  = <srgbtbrel>-instid_a.
    ENDLOOP.

    IF gt_soodk[] IS NOT INITIAL.
      SORT gt_soodk BY objtp objyr objno.
      SELECT objtp objyr objno objdes
             FROM sood INTO TABLE lt_sood
             FOR ALL ENTRIES IN gt_soodk
             WHERE objtp = gt_soodk-objtp
             AND   objyr = gt_soodk-objyr
             AND   objno = gt_soodk-objno
             AND   objdes NE ''.
      SORT lt_sood.
      LOOP AT gt_soodk ASSIGNING <sodk>.
        READ TABLE lt_sood ASSIGNING <sood>
                           WITH KEY objtp = <sodk>-objtp
                                    objyr = <sodk>-objyr
                                    objno = <sodk>-objno BINARY SEARCH.
        IF sy-subrc EQ 0.
          <sodk>-objdes = <sood>-objdes.
        ENDIF.
      ENDLOOP.
      SORT gt_soodk.
    ENDIF.

    IF rd_alv EQ 'X'.
      CALL SCREEN 9010.
    ELSE.
      PERFORM write_files USING gt_soodk[] p_tabname.
    ENDIF.
  ELSEIF p_tabname EQ 'SRGBTBREL'.
    SELECT * INTO TABLE lt_srgbtbrel2
             FROM srgbtbrel
             WHERE reltype IN s_objyr
             AND typeid_a  IN s_objno
             AND instid_a  IN s_objtp
             AND catid_a   IN s_tidb.
    IF sy-subrc EQ 0.
      LOOP AT lt_srgbtbrel2 ASSIGNING <srgbtbrel2>.
        CLEAR: gv_instid_a, gv_brelguid.
        REFRESH: lta_solix.
        ltp_docid   = <srgbtbrel2>-instid_b.
        gv_instid_a = <srgbtbrel2>-instid_a.
        gv_brelguid = <srgbtbrel2>-brelguid.
        CALL FUNCTION 'SO_DOCUMENT_READ_API1'
          EXPORTING
            document_id                = ltp_docid
          IMPORTING
            document_data              = lwa_doc_data
          TABLES
            object_header              = lta_objhdr[]
            object_content             = lta_objcont[]
            contents_hex               = lta_solix[]
          EXCEPTIONS
            document_id_not_exist      = 1
            operation_no_authorization = 2
            x_error                    = 3
            OTHERS                     = 4.

        PERFORM write_srgbtbrel_files USING lta_solix[] p_tabname
                            <srgbtbrel2>-instid_b <srgbtbrel2>-typeid_b .
      ENDLOOP.
      PERFORM write_files USING lt_srgbtbrel2[] p_tabname.
    ENDIF.
  ENDIF.

ENDFORM.                    " READ_PROCESS_EXCEPTION

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS_REG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TABNAME  text
*      -->P_P_PKG_SIZE  text
*----------------------------------------------------------------------*
FORM read_process_reg  USING p_tabname TYPE any
                             p_pkg     TYPE any.

  SELECT * FROM (p_tabname)
              UP TO p_nrec ROWS
              INTO TABLE <gt_data> WHERE (gt_cond) .
  IF sy-subrc EQ 0.
    IF rd_alv EQ 'X'.
      CALL SCREEN 9010.
    ELSE.
      PERFORM write_files USING <gt_data> p_tabname.
*      PERFORM write_files_backup USING <gt_data> p_tabname.
    ENDIF.
  ENDIF.

  DATA: ls_stxl LIKE LINE OF gt_stxl,
          lv_count TYPE i.

  DO.
    lv_count = lv_count + 1.
    IF lv_count GT 1.
      SELECT mandt relid tdobject tdname tdid tdspras srtf2 clustr
           FROM (p_tabname) UP TO p_pkg ROWS INTO TABLE gt_stxl
    WHERE relid    IN s_relid  AND
          tdobject IN s_tdobje AND
          tdname   IN s_tdname AND
          tdid     IN s_tdid   AND
          tdspras  IN s_tdspra AND
          srtf2    IN s_srtf2  AND
        ( relid    >= ls_stxl-relid AND
          tdobject >= ls_stxl-tdobject AND
          tdname   >= ls_stxl-tdname  AND
          tdid     >= ls_stxl-tdid AND
          tdspras  >= ls_stxl-tdspras AND
          srtf2    >= ls_stxl-srtf2 ).
    ELSE.
      SELECT mandt relid tdobject tdname tdid tdspras srtf2 clustr
                  FROM (p_tabname) UP TO p_pkg ROWS INTO TABLE gt_stxl
           WHERE relid    IN s_relid  AND
                 tdobject IN s_tdobje AND
                 tdname   IN s_tdname AND
                 tdid     IN s_tdid   AND
                 tdspras  IN s_tdspra AND
                 srtf2    IN s_srtf2.
    ENDIF.

    IF gt_stxl[] IS NOT INITIAL.
      READ TABLE gt_stxl INTO ls_stxl INDEX sy-dbcnt.
      PERFORM write_files USING gt_stxl p_tabname.
      CLEAR: gt_stxl[].
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " READ_PROCESS_REG

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS_EXCEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TABNAME  text
*      -->P_P_PKG_SIZE  text
*----------------------------------------------------------------------*
FORM read_process_excep  USING p_tabname TYPE any
                               p_pkg     TYPE any.

  FIELD-SYMBOLS: <ausp>       LIKE LINE OF gt_ausp,
                 <mat_charac> LIKE LINE OF gt_mat_charac,
                 <toa01>      LIKE LINE OF gt_toa01,
                 <drad>       LIKE LINE OF gt_drad.

  IF p_tabname EQ 'AUSP'.
    SELECT a~matnr b~objek b~atinn b~atzhl b~mafid
             b~klart b~adzhl b~atwrt c~atbez
                     INTO TABLE gt_ausp
                     UP TO p_nrec ROWS
                     FROM mara AS a INNER JOIN ausp AS b
                     ON    a~matnr = b~objek
                     INNER JOIN cabnt AS c ON b~atinn = c~atinn AND b~adzhl = c~adzhl
                     WHERE b~objek IN s_objek
                     AND   b~atinn IN s_atinn
                     AND   b~atzhl IN s_atzhl
                     AND   b~mafid IN s_mafid
                     AND   b~klart IN s_klart
                     AND   b~adzhl IN s_adzhl
                     AND   c~spras EQ sy-langu.
    SORT gt_ausp.
    LOOP AT gt_ausp ASSIGNING <ausp>.
      APPEND INITIAL LINE TO gt_mat_charac ASSIGNING <mat_charac>.
      <mat_charac>-matnr = <ausp>-matnr.
      <mat_charac>-atwrt = <ausp>-atwrt.
      <mat_charac>-atbez = <ausp>-atbez.
    ENDLOOP.
    IF rd_alv EQ 'X'.
      CALL SCREEN 9010.
    ELSE.
      PERFORM write_files USING gt_mat_charac[] p_tabname.
    ENDIF.
  ELSEIF p_tabname EQ 'AUSP+MARA'. " MARA and AUSP
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_mara_ausp
                       UP TO p_nrec ROWS
                       FROM mara AS a INNER JOIN ausp AS b
                       ON    a~matnr = b~objek
                       INNER JOIN cabnt AS c ON b~atinn = c~atinn AND b~adzhl = c~adzhl
                       WHERE b~objek IN s_objek
                       AND   b~atinn IN s_atinn
                       AND   b~atzhl IN s_atzhl
                       AND   b~mafid IN s_mafid
                       AND   b~klart IN s_klart
                       AND   b~adzhl IN s_adzhl
                       AND   c~spras EQ sy-langu.
    SORT gt_mara_ausp.
    IF rd_alv EQ 'X'.
      CALL SCREEN 9010.
    ELSE.
      PERFORM write_files USING gt_mara_ausp[] p_tabname.
    ENDIF.
  ELSEIF p_tabname EQ 'ADR6'.
    SELECT * FROM (p_tabname)
            UP TO p_nrec ROWS
            INTO TABLE <gt_data>
            WHERE addrnumber IN s_adr61 AND
                  persnumber IN s_adr62 AND
                  date_from  IN s_adr63 AND
                  consnumber IN s_adr64.
    IF sy-subrc EQ 0.
      IF rd_alv EQ 'X'.
        CALL SCREEN 9010.
      ELSE.
        PERFORM write_files USING <gt_data> p_tabname.
      ENDIF.
    ENDIF.
  ELSEIF p_tabname EQ 'STXL'.
    SELECT * FROM (p_tabname)
            UP TO p_nrec ROWS
            INTO TABLE <gt_data>
            WHERE relid    IN s_relid  AND
                  tdobject IN s_tdobje AND
                  tdname   IN s_tdname AND
                  tdid     IN s_tdid   AND
                  tdspras  IN s_tdspra AND
                  srtf2    IN s_srtf2.
    IF sy-subrc EQ 0.
      IF rd_alv EQ 'X'.
        CALL SCREEN 9010.
      ELSE.
        PERFORM write_files USING <gt_data> p_tabname.
      ENDIF.
    ENDIF.

  ELSEIF p_tabname EQ 'TOA01'.
    SELECT a~sap_object a~object_id a~archiv_id
          a~arc_doc_id a~ar_object a~ar_date
          a~del_date a~reserve b~arc_doc_id b~filename
          b~creator b~descr b~creatime
          INTO TABLE gt_toa01
          UP TO p_nrec ROWS
          FROM toa01 AS a LEFT OUTER JOIN toaat AS b
          ON    a~arc_doc_id = b~arc_doc_id
          WHERE a~sap_object IN s_sapobj
          AND   a~object_id  IN s_aobjid
          AND   a~archiv_id  IN s_archid
          AND   a~arc_doc_id IN s_arcdid.
    SORT gt_toa01.
    LOOP AT gt_toa01 ASSIGNING <toa01> WHERE filename EQ ''.
      <toa01>-filename = <toa01>-object_id.
    ENDLOOP.
    IF rd_alv EQ 'X'.
      CALL SCREEN 9010.
    ELSE.
      PERFORM write_files USING gt_toa01[] p_tabname.
    ENDIF.
  ELSEIF p_tabname EQ 'DRAD'.

    DATA:t_cd TYPE STANDARD TABLE OF dms_ph_cd1,
         lv_tabix TYPE sytabix,
         lv_loid TYPE sdok_loid,
         lv_objid TYPE sdok_loid.

*    FIELD-SYMBOLS: <cd> LIKE LINE OF lt_cd.

    SELECT a~mandt a~dokar a~doknr a~dokvr a~doktl
                     a~dokob a~obzae a~objky a~vrkstat a~vrkstat1
                     a~vobj a~vkey a~vdir a~viewflag a~delflag a~longtext_id
                     a~cad_pos a~cm_fixed b~mandt b~dokar  b~doknr
                     b~dokvr b~doktl  b~lo_type b~lo_index b~lo_objid b~lo_is_ref
                     c~mandt c~phio_id c~ph_class c~loio_id
                     c~lo_class c~state c~descript c~langu c~crea_user
                     c~crea_time c~chng_user c~chng_time c~stor_cat
                     c~doc_format c~doc_prot c~prop01 c~prop02 c~prop03
                     c~prop04 c~prop05 c~prop06 c~prop07 c~prop08 c~prop09 c~prop10
                     d~mandt d~dokar d~doknr d~dokvr d~doktl d~langu d~dktxt d~ltxin d~dktxt_uc
                     INTO TABLE gt_drad
                     UP TO p_nrec ROWS
                     FROM drad AS a INNER JOIN dms_doc2loio AS b ON  a~dokar = b~dokar
                     AND a~doknr = b~doknr
                     AND a~dokvr = b~dokvr
                     AND a~doktl = b~doktl

                     INNER JOIN drat AS d ON a~dokar = d~dokar
                     AND a~doknr = d~doknr
                     AND a~dokvr = d~dokvr
                     AND a~doktl = d~doktl

                     INNER JOIN dms_ph_cd1 AS c ON b~lo_objid = c~loio_id
                     WHERE a~dokar IN s_dokar
                     AND   a~doknr IN s_doknr
                     AND   a~objky IN s_obky.

*  ELSEIF p_tabname EQ 'DRAT'.
*
*    DATA: lt_cd TYPE STANDARD TABLE OF dms_ph_cd1,
*          lv_tabix TYPE sytabix,
*          lv_loid TYPE sdok_loid,
*          lv_objid TYPE sdok_loid.
*
*    FIELD-SYMBOLS: <cd> LIKE LINE OF lt_cd.
*
*
*    SELECT a~mandt a~dokar a~doknr a~dokvr a~doktl
*                     a~dokob a~obzae a~objky a~vrkstat a~vrkstat1
*                     a~vobj a~vkey a~vdir a~viewflag a~delflag a~longtext_id
*                     a~cad_pos a~cm_fixed b~mandt b~dokar  b~doknr
*                     b~dokvr b~doktl  b~lo_type b~lo_index b~lo_objid b~lo_is_ref
*                     c~mandt c~phio_id c~ph_class c~loio_id
*                     c~lo_class c~state c~descript c~langu c~crea_user
*                     c~crea_time c~chng_user c~chng_time c~stor_cat
*                     c~doc_format c~doc_prot c~prop01 c~prop02 c~prop03
*                     c~prop04 c~prop05 c~prop06 c~prop07 c~prop08 c~prop09
*                     c~prop10 d~mandt d~dokar d~doknr d~dokvr d~doktl d~langu
*                     d~dktxt d~ltxin d~dktxt_uc
*                     INTO TABLE gt_drad
*                     UP TO p_nrec ROWS
*
*                     FROM drad AS a INNER JOIN dms_doc2loio AS b ON  a~dokar = b~dokar
*                     AND a~doknr = b~doknr
*                     AND a~dokvr = b~dokvr
*                     AND a~doktl = b~doktl
*
*                     INNER JOIN drat AS d ON a~dokar = d~dokar
*
*
*                     INNER JOIN dms_ph_cd1 AS c ON b~lo_objid = c~loio_id
*                     WHERE a~dokar IN s_dokar
*                     AND   a~doknr IN s_doknr
*                     AND   a~objky IN s_obky.
    IF rd_alv EQ 'X'.
      CALL SCREEN 9010.
    ELSE.
      PERFORM write_files USING gt_drad[] p_tabname.
    ENDIF.
  ENDIF.

ENDFORM.                    " READ_PROCESS_EXCEP

*&---------------------------------------------------------------------*
*&      Form  GET_FLD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TABNAME  text
*      <--P_PT_FCAT[]  text
*----------------------------------------------------------------------*
FORM get_fld_catalog  USING    p_tabname TYPE any
                      CHANGING pt_fcat   TYPE STANDARD TABLE.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_tabname
    CHANGING
      ct_fieldcat            = pt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.                    " GET_FLD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_EXCEPTION_KEYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_exception_keys .

  DATA: ls_key LIKE LINE OF gt_keys.

  IF p_tabs EQ 'ADR6'.

    r_keys-low    = 'ADR61'.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.
    ls_key-fieldname = r_keys-low.
    APPEND ls_key TO gt_keys.

    r_keys-low    = 'ADR62'.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.
    ls_key-fieldname = r_keys-low.
    APPEND ls_key TO gt_keys.

    r_keys-low    = 'ADR63'.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.
    ls_key-fieldname = r_keys-low.
    APPEND ls_key TO gt_keys.

    r_keys-low    = 'ADR64'.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.
    ls_key-fieldname = r_keys-low.
    APPEND ls_key TO gt_keys.
  ELSEIF p_tabs EQ 'BKPF'.

    ls_key-fieldname = 'BLART'.
    APPEND ls_key TO gt_keys.
    r_keys-low = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'BLDAT'.
    APPEND ls_key TO gt_keys.
    r_keys-low = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'BUDAT'.
    APPEND ls_key TO gt_keys.
    r_keys-low = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'MONAT'.
    APPEND ls_key TO gt_keys.
    r_keys-low = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'XBLNR'.
    APPEND ls_key TO gt_keys.
    r_keys-low = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'DBBLG'.
    APPEND ls_key TO gt_keys.
    r_keys-low = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'STBLG'.
    APPEND ls_key TO gt_keys.
    r_keys-low = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'STJAH'.
    APPEND ls_key TO gt_keys.
    r_keys-low = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'BSTAT'.
    APPEND ls_key TO gt_keys.
    r_keys-low = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign = 'I'.
    APPEND r_keys.
  ELSEIF p_tabs EQ 'IHPA'.

    ls_key-fieldname = 'IHPA3'.
    APPEND ls_key TO gt_keys.

    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

  ELSEIF p_tabs EQ 'STXL'.
    ls_key-fieldname = 'TDOBJE'.
    APPEND ls_key TO gt_keys.

    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'TDSPRA'.
    APPEND ls_key TO gt_keys.

    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

  ELSEIF p_tabs EQ 'TOA01'.

    ls_key-fieldname = 'SAPOBJ'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'AOBJID'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'ARCHID'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'ARCDID'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

  ELSEIF p_tabs EQ 'DRAD'.
    ls_key-fieldname = 'OBKY'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

  ELSEIF p_tabs EQ 'DRAT'.

    ls_key-fieldname = 'DOKAR2'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'DOKNR2'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'LANGU2'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

  ELSEIF p_tabs EQ 'PRPS'.
    ls_key-fieldname = 'PSPNR2'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'PBUKR'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.
  ELSEIF p_tabs EQ 'SOOD'.
* field TIDB is not part of the primary keys of the table
* so add it here to show on selection screen for SOOD.
* also change selection screen texts
    ls_key-fieldname = 'TIDB'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.
  ELSEIF p_tabs EQ 'SRGBTBREL'.
* field TIDB is not part of the primary keys of the table
* so add it here to show on selection screen for SOOD.
* also change selection screen texts
    ls_key-fieldname = 'TIDB'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'OBJYR'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'OBJTP'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.

    ls_key-fieldname = 'OBJNO'.
    APPEND ls_key TO gt_keys.
    r_keys-low    = ls_key-fieldname.
    r_keys-option = 'EQ'.
    r_keys-sign   = 'I'.
    APPEND r_keys.
  ENDIF.

ENDFORM.                    " BUILD_EXCEPTION_KEYS
*&---------------------------------------------------------------------*
*&      Form  BUILD_EXCEPTION_WHERE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_exception_where .

  DATA: ls_cond  LIKE LINE OF gt_cond,
        field_s  TYPE fieldname.

  IF p_tabs EQ 'BKPF'.
    CONCATENATE 'S_' 'BLART' INTO field_s.
    CONCATENATE 'BLART' 'IN' field_s 'AND' INTO ls_cond-cond SEPARATED BY space.
    APPEND ls_cond TO gt_cond.

    CONCATENATE 'S_' 'BLDAT' INTO field_s.
    CONCATENATE 'BLDAT' 'IN' field_s 'AND' INTO ls_cond-cond SEPARATED BY space.
    APPEND ls_cond TO gt_cond.

    CONCATENATE 'S_' 'BUDAT' INTO field_s.
    CONCATENATE 'BUDAT' 'IN' field_s 'AND' INTO ls_cond-cond SEPARATED BY space.
    APPEND ls_cond TO gt_cond.

    CONCATENATE 'S_' 'MONAT' INTO field_s.
    CONCATENATE 'MONAT' 'IN' field_s 'AND' INTO ls_cond-cond SEPARATED BY space.
    APPEND ls_cond TO gt_cond.

    CONCATENATE 'S_' 'XBLNR' INTO field_s.
    CONCATENATE 'XBLNR' 'IN' field_s 'AND' INTO ls_cond-cond SEPARATED BY space.
    APPEND ls_cond TO gt_cond.

    CONCATENATE 'S_' 'DBBLG' INTO field_s.
    CONCATENATE 'DBBLG' 'IN' field_s 'AND' INTO ls_cond-cond SEPARATED BY space.
    APPEND ls_cond TO gt_cond.

    CONCATENATE 'S_' 'STBLG' INTO field_s.
    CONCATENATE 'STBLG' 'IN' field_s 'AND' INTO ls_cond-cond SEPARATED BY space.
    APPEND ls_cond TO gt_cond.

    CONCATENATE 'S_' 'STJAH' INTO field_s.
    CONCATENATE 'STJAH' 'IN' field_s 'AND' INTO ls_cond-cond SEPARATED BY space.
    APPEND ls_cond TO gt_cond.

    CONCATENATE 'S_' 'BSTAT' INTO field_s.
    CONCATENATE 'BSTAT' 'IN' field_s 'AND' INTO ls_cond-cond SEPARATED BY space.
    APPEND ls_cond TO gt_cond.
  ENDIF.

ENDFORM.                    " BUILD_EXCEPTION_WHERE
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_CONVERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<DATA>  text
*----------------------------------------------------------------------*
FORM output_conversion  USING p_fname TYPE any
                              p_input TYPE any
                              p_tab   TYPE any
                              p_val   TYPE any.

  IF p_tab EQ 'PROJ'.
    IF p_fname EQ 'PSPNR'.
      CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
        EXPORTING
          input  = p_input
        IMPORTING
          output = p_val.
    ELSEIF p_fname EQ 'PSPID'.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = p_input
        IMPORTING
          output = p_val.
    ENDIF.
  ELSEIF p_tab EQ 'PRPS'.
    IF p_fname EQ 'POSID'.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = p_input
        IMPORTING
          output = p_val.
    ELSEIF p_fname EQ 'PSPNR'.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = p_input
        IMPORTING
          output = p_val.
    ENDIF.
  ELSEIF p_tab EQ 'PRTE'.
    IF p_fname EQ 'POSNR'.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = p_input
        IMPORTING
          output = p_val.
    ENDIF.
  ELSEIF p_tab EQ 'EKKN'.

  ENDIF.

ENDFORM.                    " OUTPUT_CONVERSION
*&---------------------------------------------------------------------*
*&      Form  FILL_SELECTION_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_selection_texts .

  DATA: lt_dfies TYPE STANDARD TABLE OF dfies,
       lv_sfld  TYPE string.

  FIELD-SYMBOLS: <dfies> LIKE LINE OF lt_dfies,
                 <fs>    TYPE any.

*%_s_lifnr_%_app_%-text  "example

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = p_tabs
    TABLES
      dfies_tab      = lt_dfies
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  IF p_tabs EQ 'SOOD'.
* Add fields on selection screen whic is not part of primary keys
    APPEND INITIAL LINE TO lt_dfies ASSIGNING <dfies>.
    <dfies>-keyflag   = 'X'.
    <dfies>-fieldname = 'TIDB'.
    <dfies>-scrtext_m = 'Obj Type'.
  ELSEIF p_tabs EQ 'SRGBTBREL'.

  ENDIF.

  LOOP AT lt_dfies ASSIGNING <dfies>
                   WHERE keyflag   EQ 'X'
                   AND   fieldname NE 'MANDT'
                   AND   fieldname NE 'CLIENT'.
    UNASSIGN: <fs>.
    CLEAR: lv_sfld.

    CONCATENATE '%_s_' <dfies>-fieldname '_%_app_%-text' INTO lv_sfld.
    IF <dfies>-scrtext_m NE ''.
      ASSIGN (lv_sfld) TO <fs>.
      IF <fs> IS ASSIGNED.
        <fs> = <dfies>-scrtext_m.
      ENDIF.
    ELSEIF <dfies>-scrtext_s NE ''.
      ASSIGN (lv_sfld) TO <fs>.
      IF <fs> IS ASSIGNED.
        <fs> = <dfies>-scrtext_s.
      ENDIF.
    ELSEIF <dfies>-scrtext_l NE ''.
      ASSIGN (lv_sfld) TO <fs>.
      IF <fs> IS ASSIGNED.
        <fs> = <dfies>-scrtext_l.
      ENDIF.
    ELSEIF <dfies>-fieldtext NE ''.
      ASSIGN (lv_sfld) TO <fs>.
      IF <fs> IS ASSIGNED.
        <fs> = <dfies>-fieldtext.
      ENDIF.
    ELSE.
      ASSIGN (lv_sfld) TO <fs>.
      IF <fs> IS ASSIGNED.
        <fs> = <dfies>-fieldname.
      ENDIF.
    ENDIF.

    IF p_tabs EQ 'BKPF'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ELSEIF p_tabs EQ 'IHPA'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ELSEIF p_tabs EQ 'ADR6'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ELSEIF p_tabs EQ 'STXL'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ELSEIF p_tabs EQ 'TOA01'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ELSEIF p_tabs EQ 'DRAD'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ELSEIF p_tabs EQ 'DRAT'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ELSEIF p_tabs EQ 'PRPS'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ELSEIF p_tabs EQ 'SOOD'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ELSEIF p_tabs EQ 'SRGBTBREL'.
      PERFORM build_seltxt_exceptions USING <dfies>.
    ENDIF.
  ENDLOOP.

*  Add fields on the selection screen other than key fields
  IF p_tabs EQ 'PRPS'.
    LOOP AT lt_dfies ASSIGNING <dfies> WHERE fieldname EQ 'PBUKR'.
      UNASSIGN: <fs>.
      CLEAR: lv_sfld.
      CONCATENATE '%_s_' <dfies>-fieldname '_%_app_%-text' INTO lv_sfld.
      IF <dfies>-scrtext_m NE ''.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-scrtext_m.
        ENDIF.
      ELSEIF <dfies>-scrtext_s NE ''.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-scrtext_s.
        ENDIF.
      ELSEIF <dfies>-scrtext_l NE ''.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-scrtext_l.
        ENDIF.
      ELSEIF <dfies>-fieldtext NE ''.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-fieldtext.
        ENDIF.
      ELSE.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-fieldname.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSEIF p_tabs EQ 'SRGBTBREL'.

    LOOP AT lt_dfies ASSIGNING <dfies>
                     WHERE fieldname EQ 'RELTYPE'
                     OR    fieldname EQ 'INSTID_A'
                     OR    fieldname EQ 'TYPEID_A'
                     OR    fieldname EQ 'CATID_A'.
      UNASSIGN: <fs>.
      CLEAR: lv_sfld.
      IF <dfies>-fieldname EQ 'RELTYPE'.
        CONCATENATE '%_s_' 'OBJYR' '_%_app_%-text' INTO lv_sfld.
      ELSEIF <dfies>-fieldname EQ 'INSTID_A'.
        CONCATENATE '%_s_' 'OBJTP' '_%_app_%-text' INTO lv_sfld.
      ELSEIF <dfies>-fieldname EQ 'TYPEID_A'.
        CONCATENATE '%_s_' 'OBJNO' '_%_app_%-text' INTO lv_sfld.
      ELSEIF <dfies>-fieldname EQ 'CATID_A'.
        CONCATENATE '%_s_' 'TIDB' '_%_app_%-text' INTO lv_sfld.
      ENDIF.

      IF <dfies>-scrtext_m NE ''.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-scrtext_m.
        ENDIF.
      ELSEIF <dfies>-scrtext_s NE ''.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-scrtext_s.
        ENDIF.
      ELSEIF <dfies>-scrtext_l NE ''.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-scrtext_l.
        ENDIF.
      ELSEIF <dfies>-fieldtext NE ''.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-fieldtext.
        ENDIF.
      ELSE.
        ASSIGN (lv_sfld) TO <fs>.
        IF <fs> IS ASSIGNED.
          <fs> = <dfies>-fieldname.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " FILL_SELECTION_TEXTS

*&---------------------------------------------------------------------*
*&      Form  BUILD_SELTXT_EXCEPTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_seltxt_exceptions USING ps_dfies TYPE dfies.

  IF p_tabs EQ 'IHPA'.
    IF ps_dfies-fieldname EQ 'COUNTER'.
      PERFORM assign_selection_texts USING 'IHPA3' ps_dfies.
    ENDIF.
  ELSEIF p_tabs EQ 'STXL'.
    IF ps_dfies-fieldname EQ 'TDOBJECT'.
* Parameter is too long error - more than 8 characters for select-options
      PERFORM assign_selection_texts USING 'TDOBJE' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'TDSPRAS'.
      PERFORM assign_selection_texts USING 'TDSPRA' ps_dfies.
    ENDIF.
  ELSEIF p_tabs EQ 'TOA01'.
    IF ps_dfies-fieldname EQ 'SAP_OBJECT'.
      PERFORM assign_selection_texts USING 'SAPOBJ' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'OBJECT_ID'.
      PERFORM assign_selection_texts USING 'AOBJID' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'ARCHIV_ID'.
      PERFORM assign_selection_texts USING 'ARCHID' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'ARC_DOC_ID'.
      PERFORM assign_selection_texts USING 'ARCDID' ps_dfies.
    ENDIF.
  ELSEIF p_tabs EQ 'DRAD'.
    IF ps_dfies-fieldname EQ 'OBJKY'.
      PERFORM assign_selection_texts USING 'OBKY' ps_dfies.
    ENDIF.
  ELSEIF p_tabs EQ 'DRAT'.
    IF ps_dfies-fieldname EQ 'DOKAR'.
      PERFORM assign_selection_texts USING 'DOKAR2' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'DOKNR'.
      PERFORM assign_selection_texts USING 'DOKNR2' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'LANGU'.
      PERFORM assign_selection_texts USING 'LANGU2' ps_dfies.
    ENDIF.
  ELSEIF p_tabs EQ 'PRPS'.
    IF ps_dfies-fieldname EQ 'PSPNR'.
      PERFORM assign_selection_texts USING 'PSPNR2' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'PBUKR'.
      PERFORM assign_selection_texts USING 'PBUKR' ps_dfies.
    ENDIF.
  ELSEIF p_tabs EQ 'SOOD'.
    IF ps_dfies-fieldname EQ 'OBJTP'.
      ps_dfies-scrtext_m = 'Instance ID'.
      ps_dfies-scrtext_s = 'InstanceID'.
      ps_dfies-scrtext_l = 'Instance ID'.
      ps_dfies-fieldtext = 'Instance ID'.
      PERFORM assign_selection_texts USING 'OBJTP' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'OBJYR'.
      ps_dfies-scrtext_m = 'Relation Type'.
      ps_dfies-scrtext_s = 'Rel. Typ'.
      ps_dfies-scrtext_l = 'Relationship type'.
      ps_dfies-fieldtext = 'Relationship type'.
      PERFORM assign_selection_texts USING 'OBJYR' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'OBJNO'.
      ps_dfies-scrtext_m = 'Obj Type'.
      ps_dfies-scrtext_s = 'Obj Typ.'.
      ps_dfies-scrtext_l = 'Object Type'.
      ps_dfies-fieldtext = 'Object Type'.
      PERFORM assign_selection_texts USING 'OBJNO' ps_dfies.
    ENDIF.
  ELSEIF p_tabs EQ 'SRGBTBREL'.
    IF ps_dfies-fieldname EQ 'OBJYR'.
      ps_dfies-scrtext_m = 'Relationship type'.
      ps_dfies-scrtext_s = 'Relationship type'.
      ps_dfies-scrtext_l = 'Relationship type'.
      ps_dfies-fieldtext = 'Relationship type'.
      PERFORM assign_selection_texts USING 'OBJYR' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'OBJTP'.
      ps_dfies-scrtext_m = 'Instance Ident'.
      ps_dfies-scrtext_s = 'Instance Ident'.
      ps_dfies-scrtext_l = 'Instance Ident'.
      ps_dfies-fieldtext = 'Instance Ident'.
      PERFORM assign_selection_texts USING 'OBJTP' ps_dfies.
    ELSEIF ps_dfies-fieldname EQ 'OBJNO'.
      ps_dfies-scrtext_m = 'Obj Type'.
      ps_dfies-scrtext_s = 'Obj Typ.'.
      ps_dfies-scrtext_l = 'Object Type'.
      ps_dfies-fieldtext = 'Object Type'.
      PERFORM assign_selection_texts USING 'OBJNO' ps_dfies.

    ELSEIF ps_dfies-fieldname EQ 'TIDB'.
      ps_dfies-scrtext_m = 'Type of Objects'.
      ps_dfies-scrtext_s = 'Type of Objects'.
      ps_dfies-scrtext_l = 'Type of Objects'.
      ps_dfies-fieldtext = 'Type of Objects'.
      PERFORM assign_selection_texts USING 'TIDB' ps_dfies.
    ENDIF.
  ENDIF.

ENDFORM.                    " BUILD_SELTXT_EXCEPTIONS
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_SELECTION_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PS_DFIES  text
*----------------------------------------------------------------------*
FORM assign_selection_texts USING p_fname  TYPE any
                                  ps_dfies TYPE dfies.

  DATA: lv_sfld  TYPE string.

  FIELD-SYMBOLS: <fs> TYPE any.

  CONCATENATE '%_s_' p_fname '_%_app_%-text' INTO lv_sfld.
  ASSIGN (lv_sfld) TO <fs>.
  CHECK <fs> IS ASSIGNED.
  IF ps_dfies-scrtext_m NE ''.
    <fs> = ps_dfies-scrtext_m.
  ELSEIF ps_dfies-scrtext_s NE ''.
    <fs> = ps_dfies-scrtext_s.
  ELSEIF ps_dfies-scrtext_l NE ''.
    <fs> = ps_dfies-scrtext_l.
  ELSEIF ps_dfies-fieldtext NE ''.
    <fs> = ps_dfies-fieldtext.
  ELSE.
    <fs> = ps_dfies-fieldname.
  ENDIF.

ENDFORM.                    " ASSIGN_SELECTION_TEXTS
*&---------------------------------------------------------------------*
*&      Form  FORMAT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<SPLIT_TAB>[]  text
*      -->P_<SPLIT_LINE>  text
*      <--P_LT_EKKN[]  text
*----------------------------------------------------------------------*
*FORM format_data  USING    pt_split TYPE STANDARD TABLE
*                           ps_split TYPE any
*                  CHANGING pt_ekkn  TYPE ty_t_ekkn.
*
*  DATA: ls_ekkn     TYPE ty_ekkn,
*        lv_internal TYPE text30,
*        lv_external TYPE text30.
*
*  FIELD-SYMBOLS: <internal_format> TYPE any.
*
*  LOOP AT pt_split INTO ps_split.
*    MOVE-CORRESPONDING ps_split TO ls_ekkn.
*    ASSIGN COMPONENT 'PS_PSP_PNR' OF STRUCTURE ps_split TO <internal_format>.
*    lv_internal = <internal_format>.
*    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*      EXPORTING
*        input  = lv_internal
*      IMPORTING
*        output = lv_external.
*
*    ls_ekkn-ps_psp_pnr_txt = lv_external.
*    APPEND ls_ekkn TO pt_ekkn.
*  ENDLOOP.
*
*ENDFORM.                    " FORMAT_DATA
*&---------------------------------------------------------------------*
*&      Form  CONVERT_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<DFIES>  text
*      -->P_PS_DATA  text
*      -->P_P_TABNAME  text
*      <--P_LV_VAL  text
*----------------------------------------------------------------------*
FORM convert_output  USING    p_fname   TYPE any
                              ps_data   TYPE any
                              p_tab     TYPE any
                              p_delim   TYPE any
                     CHANGING p_val     TYPE any.

  TYPES: BEGIN OF ty_stxl_raw,
          clustr TYPE stxl-clustr,
          clustd TYPE stxl-clustd,
         END OF ty_stxl_raw.

  DATA: lv_internal   TYPE string,
        t_stxl_raw    TYPE STANDARD TABLE OF ty_stxl_raw,
        ls_staxl_raw  LIKE LINE OF t_stxl_raw,
        t_tline TYPE STANDARD TABLE OF tline.

  FIELD-SYMBOLS: <internal_format> TYPE any,
                 <tline> TYPE tline,
                 <val> TYPE any.

  IF p_tab EQ 'PROJ'.
    IF p_fname EQ 'PSPNR'.
      ASSIGN COMPONENT 'PSPNR' OF STRUCTURE ps_data TO <internal_format>.
      lv_internal = <internal_format>.
      CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
        EXPORTING
          input  = lv_internal
        IMPORTING
          output = p_val.
    ELSEIF p_fname EQ 'PSPID'.
      ASSIGN COMPONENT 'PSPID' OF STRUCTURE ps_data TO <internal_format>.
      lv_internal = <internal_format>.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = lv_internal
        IMPORTING
          output = p_val.
    ENDIF.
  ELSEIF p_tab EQ 'PRPS'.
    IF p_fname EQ 'POSID'.
      ASSIGN COMPONENT 'POSID' OF STRUCTURE ps_data TO <internal_format>.
      lv_internal = <internal_format>.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = lv_internal
        IMPORTING
          output = p_val.
    ELSEIF p_fname EQ 'PSPNR'.
      ASSIGN COMPONENT 'PSPNR' OF STRUCTURE ps_data TO <internal_format>.
      lv_internal = <internal_format>.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = lv_internal
        IMPORTING
          output = p_val.
    ENDIF.
  ELSEIF p_tab EQ 'PRTE'.
    IF p_fname EQ 'POSNR'.
      ASSIGN COMPONENT 'POSNR' OF STRUCTURE ps_data TO <internal_format>.
      lv_internal = <internal_format>.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = lv_internal
        IMPORTING
          output = p_val.
    ELSEIF p_fname EQ 'PSPHI'.
      ASSIGN COMPONENT 'PSPHI' OF STRUCTURE ps_data TO <internal_format>.
      lv_internal = <internal_format>.
      CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
        EXPORTING
          input  = lv_internal
        IMPORTING
          output = p_val.
    ENDIF.
  ELSEIF p_tab EQ 'EKKN'.
    IF p_fname EQ 'PS_PSP_PNR_TXT'.
      ASSIGN COMPONENT 'PS_PSP_PNR' OF STRUCTURE ps_data TO <internal_format>.
      lv_internal = <internal_format>.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = lv_internal
        IMPORTING
          output = p_val.
    ENDIF.
  ELSEIF p_tab EQ 'STXL'.
    IF p_fname EQ 'CLUSTD'.
      ASSIGN COMPONENT 'CLUSTR' OF STRUCTURE ps_data TO <val>.
      ls_staxl_raw-clustr = <val>.
      ASSIGN COMPONENT 'CLUSTD' OF STRUCTURE ps_data TO <val>.
      ls_staxl_raw-clustd = <val>.
      APPEND ls_staxl_raw TO t_stxl_raw.
      IMPORT tline = t_tline FROM INTERNAL TABLE t_stxl_raw.
      CONCATENATE p_val p_delim INTO p_val.
      LOOP AT t_tline ASSIGNING <tline>.
        REPLACE ALL OCCURRENCES OF ',' IN <tline>-tdline WITH '","'.
        CONCATENATE p_val <tline>-tdline INTO p_val SEPARATED BY space.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " CONVERT_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DELIMIT_PC_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_DATA[]  text
*----------------------------------------------------------------------*
FORM delimit_format_pc_file  USING     pt_data  TYPE STANDARD TABLE
                                pt_dfies TYPE dfies_table
*                      CHANGING  pt_delimited_data TYPE truxs_t_text_data.
                      CHANGING  pt_delimited_data TYPE string_table.

  DATA: lv_temp_data LIKE LINE OF pt_delimited_data,
        lv_index     TYPE sytabix,
        lv_delimiter TYPE char4,
        lv_val       TYPE string.

  FIELD-SYMBOLS: <data>   TYPE any,
                 <dfies>  LIKE LINE OF pt_dfies,
                 <fval>   TYPE any.

  IF rd_xlsx EQ 'X' OR rd_xls EQ 'X' OR rd_tab EQ 'X'.
    lv_delimiter =  htab.
  ELSEIF rd_csv EQ 'X'.
    lv_delimiter = ','.
  ELSE.
    lv_delimiter = p_delim.
  ENDIF.

  LOOP AT pt_data ASSIGNING <data>.
    lv_index = sy-tabix.
    CLEAR: lv_temp_data.
    LOOP AT pt_dfies ASSIGNING <dfies>.
      IF sy-tabix NE 1. " Not the first field
        UNASSIGN <fval>.
        CLEAR: lv_val.
        ASSIGN COMPONENT <dfies>-fieldname OF STRUCTURE <data> TO <fval>.
        IF <fval> IS ASSIGNED.
          IF ( <dfies>-inttype EQ 'P' OR <dfies>-inttype EQ 'F' ) AND <fval> LT 0.
            <fval> = <fval> * -1.
            lv_val = <fval>.
            SHIFT lv_val LEFT DELETING LEADING space.
            CONCATENATE '-' lv_val INTO lv_val.
          ELSE.
            lv_val = <fval>.
* Look for commas in field value then wrap around with double quote
            IF ( rd_csv EQ 'X' AND
               ( <dfies>-inttype NE 'N' AND <dfies>-inttype NE 'D' AND
                 <dfies>-inttype NE 'T' AND <dfies>-inttype NE 'P' AND
                 <dfies>-inttype NE 'F' ) ).
              SEARCH lv_val  FOR ','.
              IF sy-subrc EQ 0.
                CONCATENATE '"' lv_val '"' INTO lv_val.
              ENDIF.
            ENDIF.
          ENDIF.
          PERFORM format_output USING <dfies>-fieldname <dfies>-inttype <dfies>-domname
                                     <data> p_tabs lv_delimiter
                              CHANGING lv_val.
          SHIFT lv_val LEFT DELETING LEADING space.
          CONDENSE lv_val.
          CONCATENATE lv_temp_data lv_delimiter lv_val INTO lv_temp_data.
        ENDIF.
      ELSE. "First column in DFIES table
        UNASSIGN <fval>.
        ASSIGN COMPONENT <dfies>-fieldname OF STRUCTURE <data> TO <fval>.
        IF <fval> IS ASSIGNED.
          lv_val = <fval>.
        ENDIF.
        PERFORM format_output USING <dfies>-fieldname <dfies>-inttype <dfies>-domname
                                    <data> p_tabs lv_delimiter
                              CHANGING lv_val.
        lv_temp_data = lv_val.
      ENDIF.
    ENDLOOP.
    DELETE pt_data INDEX lv_index.
    APPEND lv_temp_data TO pt_delimited_data.
  ENDLOOP.

ENDFORM.                    " DELIMIT_PC_FILE
*&---------------------------------------------------------------------*
*&      Form  FORMAT_PC_FILE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_DATA[]  text
*      -->P_LT_DFIES[]  text
*----------------------------------------------------------------------*
FORM format_pc_file_data  USING pt_data  TYPE STANDARD TABLE
                                pt_dfies TYPE dfies_table.

  FIELD-SYMBOLS: <data>   TYPE any,
                 <dfies>  LIKE LINE OF pt_dfies,
                 <fval>   TYPE any.

  LOOP AT pt_data ASSIGNING <data>.
    LOOP AT pt_dfies ASSIGNING <dfies>.
      ASSIGN COMPONENT <dfies>-fieldname OF STRUCTURE <data> TO <fval>.
      IF <dfies>-inttype EQ 'D' AND <fval> IS INITIAL.
        <fval> = ''. "Blank our initial value for data type 'D'
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " FORMAT_PC_FILE_DATA
*&---------------------------------------------------------------------*
*&      Form  FORMAT_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<DFIES>_FIELDNAME  text
*      -->P_<DATA>  text
*      -->P_P_TABS  text
*      -->P_LV_DELIMITER  text
*      <--P_LV_VAL  text
*----------------------------------------------------------------------*
FORM format_output  USING     p_fname   TYPE any
                              p_ftype   TYPE any
                              p_domain  TYPE any
                              ps_data   TYPE any
                              p_tab     TYPE any
                              p_delim   TYPE any
                     CHANGING p_val     TYPE any.

  TYPES: BEGIN OF ty_stxl_raw,
          clustr TYPE stxl-clustr,
          clustd TYPE stxl-clustd,
         END OF ty_stxl_raw.

  DATA: lv_internal   TYPE string,
        lv_tline      TYPE string,
        has_comma     TYPE c,
        lv_tdid       TYPE tdid,
        lv_tdobject   TYPE thead-tdobject,
        lv_tdname     TYPE thead-tdname,
        t_stxl_raw    TYPE STANDARD TABLE OF ty_stxl_raw,
        ls_staxl_raw  LIKE LINE OF t_stxl_raw,
        t_tline       TYPE STANDARD TABLE OF tline,
        ls_soodk      TYPE soodk,
        lt_soli       TYPE STANDARD TABLE OF soli,
        lv_tabix      TYPE sytabix.

  FIELD-SYMBOLS: <internal_format> TYPE any,
                 <tline>           TYPE tline,
                 <val>             TYPE any,
                 <soli>            LIKE LINE OF lt_soli.

  CHECK p_ftype NE 'T' AND p_ftype NE 'P' AND p_ftype NE 'F'.

  IF p_ftype EQ 'D'.
    ASSIGN COMPONENT p_fname OF STRUCTURE ps_data TO <val>.
    IF <val> IS INITIAL.
      p_val = ''. " Blank out intitial value for if data type 'D'
    ENDIF.
  ELSE.
*    IF p_tab EQ 'STXL' AND p_fname EQ 'CLUSTR'.
    IF p_tab EQ 'STXL' AND p_fname EQ 'CLUSTR'.
      CLEAR: has_comma.
      ASSIGN COMPONENT 'TDID' OF STRUCTURE ps_data TO <val>.
      lv_tdid = <val>.
      ASSIGN COMPONENT 'TDOBJECT' OF STRUCTURE ps_data TO <val>.
      lv_tdobject = <val>.
      ASSIGN COMPONENT 'TDNAME' OF STRUCTURE ps_data TO <val>.
      lv_tdname = <val>.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = lv_tdid
          language                = 'E'
          name                    = lv_tdname
          object                  = lv_tdobject
        TABLES
          lines                   = t_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF sy-subrc EQ 0.
        CLEAR: has_comma, lv_tline.
        LOOP AT t_tline ASSIGNING <tline>.
          lv_tabix = sy-tabix.
          PERFORM rmv_cr_lnfeed CHANGING <tline>-tdline has_comma.
          IF p_tab EQ 'STXL' AND
             ( lv_tdobject EQ 'PLPO'  OR lv_tdobject EQ	'EQUI' OR
               lv_tdobject EQ	'IFLOT' OR lv_tdobject EQ	'MPLA' OR
               lv_tdobject EQ	'MPOS'  OR lv_tdid EQ	'PLPO' ).
            IF lv_tabix EQ 1.
              CONCATENATE lv_tline <tline>-tdline INTO lv_tline.
            ELSE.
              CONCATENATE lv_tline <tline>-tdline nline INTO lv_tline.
            ENDIF.
          ELSE.
            IF lv_tabix EQ 1.
              CONCATENATE lv_tline <tline>-tdline INTO lv_tline.
            ELSE.
              CONCATENATE lv_tline <tline>-tdline INTO lv_tline SEPARATED BY space.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF rd_csv EQ 'X' AND has_comma EQ 'Y'.
          CONCATENATE '"' lv_tline '"' INTO lv_tline.
        ENDIF.
        CONCATENATE p_val p_delim lv_tline INTO p_val.
      ELSE.
        CONCATENATE p_val 'READ_TEXT FM Error' INTO p_val SEPARATED BY space.
      ENDIF.

    ELSEIF p_fname EQ 'PSPHI' OR p_fname EQ 'PSPID' OR
           p_fname EQ 'POSID' OR p_fname EQ 'PSPNR' OR
           p_fname EQ 'POSNR' OR p_fname EQ 'PS_PSP_PNR'.
      PERFORM output_conv USING p_tab p_fname p_domain ps_data CHANGING p_val.
    ELSEIF p_tab EQ 'SOOD' AND p_fname EQ 'TEXTS'.
      ASSIGN COMPONENT 'OBJTP' OF STRUCTURE ps_data TO <val>.
      ls_soodk-objtp = <val>.
      ASSIGN COMPONENT 'OBJYR' OF STRUCTURE ps_data TO <val>.
      ls_soodk-objyr = <val>.
      ASSIGN COMPONENT 'OBJNO' OF STRUCTURE ps_data TO <val>.
      ls_soodk-objno = <val>.

      REFRESH: lt_soli.
      CALL FUNCTION 'SO_OBJECT_GET_CONTENT'
        EXPORTING
          object_id        = ls_soodk
        TABLES
          objcont          = lt_soli
        EXCEPTIONS
          archive_error    = 1
          object_not_exist = 2
          OTHERS           = 3.
      IF lt_soli[] IS NOT INITIAL.
        CLEAR: has_comma, lv_tline.
        LOOP AT lt_soli ASSIGNING <soli>.
          PERFORM rmv_cr_lnfeed CHANGING <soli>-line has_comma.
          CONCATENATE lv_tline <soli>-line INTO lv_tline SEPARATED BY space.
        ENDLOOP.
        SHIFT lv_tline LEFT DELETING LEADING space.
        IF rd_csv EQ 'X' AND has_comma EQ 'Y'.
          CONCATENATE '"' lv_tline '"' INTO lv_tline.
        ENDIF.
        p_val = lv_tline.
      ELSE.
        CLEAR: p_val.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " FORMAT_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CONCATENATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<DFIES>  text
*----------------------------------------------------------------------*
FORM concatenation  USING  ps_dfies    TYPE dfies lv_delimiter TYPE any
                    CHANGING ls_header TYPE any
                             ls_tech_header TYPE any.

  IF ps_dfies-scrtext_s NE ''.
    CONCATENATE ls_header ps_dfies-scrtext_s lv_delimiter INTO ls_header.
  ELSEIF ps_dfies-scrtext_m NE ''.
    CONCATENATE ls_header ps_dfies-scrtext_m lv_delimiter INTO ls_header.
  ELSEIF ps_dfies-scrtext_l NE ''.
    CONCATENATE ls_header ps_dfies-scrtext_l lv_delimiter INTO ls_header.
  ELSEIF ps_dfies-fieldtext NE ''.
    CONCATENATE ls_header ps_dfies-fieldtext lv_delimiter INTO ls_header.
  ELSE.
    CONCATENATE ls_header ps_dfies-fieldname lv_delimiter INTO ls_header.
  ENDIF.

  CONCATENATE ls_tech_header ps_dfies-fieldname lv_delimiter INTO ls_tech_header.

ENDFORM.                    " CONCATENATION
*&---------------------------------------------------------------------*
*&      Form  READ_DDIF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1780   text
*      <--P_LT_DFIES[]  text
*----------------------------------------------------------------------*
FORM read_ddif  USING    p_tab    TYPE any
                CHANGING pt_dfies TYPE STANDARD TABLE.

  DATA: lv_msg TYPE natxt.

  REFRESH: pt_dfies.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = p_tab
    TABLES
      dfies_tab      = pt_dfies
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  IF sy-subrc NE 0.
    CONCATENATE 'Data definition read error for table - 'p_tab
    INTO lv_msg SEPARATED BY space.
    MESSAGE a001(00) WITH lv_msg.
  ENDIF.

ENDFORM.                    " READ_DDIF
*&---------------------------------------------------------------------*
*&      Form  RMV_CR_LNFEED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<SOLI>_LINE  text
*----------------------------------------------------------------------*
FORM rmv_cr_lnfeed  CHANGING p_data  TYPE any
                             p_comma TYPE any.


  IF rd_csv EQ 'X'.
    IF p_comma EQ ''.
      SEARCH p_data FOR ','.
      IF sy-subrc EQ 0.
        p_comma = 'Y'.
      ENDIF.
    ENDIF.

    IF p_rcom NE ''.
      REPLACE ALL OCCURRENCES OF ',' IN p_data WITH p_rcom.
    ENDIF.
    REPLACE ALL OCCURRENCES OF '"' IN p_data WITH '""'.
  ENDIF.
  REPLACE ALL OCCURRENCES OF nline IN p_data WITH space.
  REPLACE ALL OCCURRENCES OF hcr_lf   IN p_data WITH space.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+0(1) IN p_data WITH space. "Remove Carriage return only
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+0(2) IN p_data WITH space. "Remove Line feed only
  CONDENSE p_data.

ENDFORM.                    " RMV_CR_LNFEED
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_CONV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FNAME  text
*      -->P_PS_DATA  text
*      <--P_P_VAL  text
*----------------------------------------------------------------------*
FORM output_conv  USING    p_tab    TYPE any
                           p_fname  TYPE any
                           p_domain TYPE any
                           ps_data  TYPE any
                  CHANGING p_val    TYPE any.

  DATA: lv_internal   TYPE string,
        lv_convexit   TYPE convexit.

  FIELD-SYMBOLS: <internal_format> TYPE any.

  SELECT SINGLE convexit INTO lv_convexit FROM dd01l
                                   WHERE domname  EQ p_domain
                                   AND   as4local EQ 'A'.

  IF lv_convexit CS 'ABPSP'.
    ASSIGN COMPONENT p_fname OF STRUCTURE ps_data TO <internal_format>.
    CHECK <internal_format> IS ASSIGNED AND <internal_format> IS NOT INITIAL.
    lv_internal = <internal_format>.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = lv_internal
      IMPORTING
        output = p_val.
  ELSEIF lv_convexit CS 'ABPSN'.
    ASSIGN COMPONENT p_fname OF STRUCTURE ps_data TO <internal_format>.
    CHECK <internal_format> IS ASSIGNED AND <internal_format> IS NOT INITIAL.
    lv_internal = <internal_format>.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        input  = lv_internal
      IMPORTING
        output = p_val.
  ELSEIF lv_convexit CS 'KONPD'.
    ASSIGN COMPONENT p_fname OF STRUCTURE ps_data TO <internal_format>.
    CHECK <internal_format> IS ASSIGNED AND <internal_format> IS NOT INITIAL.
    lv_internal = <internal_format>.
    CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
      EXPORTING
        input  = lv_internal
      IMPORTING
        output = p_val.
  ENDIF.

ENDFORM.                    " OUTPUT_CONV
*&---------------------------------------------------------------------*
*&      Form  READ_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_object .

  DATA: lt_objects TYPE sibflporbt,
        ls_object  LIKE LINE OF lt_objects,
        lt_links_a TYPE obl_t_link,
        lt_links_b TYPE obl_t_link,
        lw_links LIKE LINE OF lt_links_a,
        doc_id     TYPE sofolenti1-doc_id,
        lt_objectheader  TYPE STANDARD TABLE OF solisti1,
        lt_objectcontent TYPE STANDARD TABLE OF solisti1,
        lt_content_hex   TYPE STANDARD TABLE OF solix.

                                                            "BUS1001006
*  ls_object-typeid = 'BKPF'.
  ls_object-typeid = 'MESSAGE'.
  ls_object-catid  = 'BO'.
  APPEND ls_object TO lt_objects.

  TRY.
* Read the links for business object.

      CALL METHOD cl_binary_relation=>read_links_of_objects
        EXPORTING
          it_objects          = lt_objects
*         IP_LOGSYS           =
*         IT_ROLE_OPTIONS     =
*         IT_RELATION_OPTIONS =
*         IP_NO_BUFFER        = SPACE
        IMPORTING
          et_links_a          = lt_links_a
          et_links_b          = lt_links_b.

    CATCH cx_obl_parameter_error .
    CATCH cx_obl_internal_error .
    CATCH cx_obl_model_error .

  ENDTRY.

  LOOP AT lt_links_a INTO lw_links.

    MOVE lw_links-instid_b TO doc_id.

    CALL FUNCTION 'SO_DOCUMENT_READ_API1'

    EXPORTING

    document_id = doc_id

* FILTER = 'X '

* IMPORTING

* DOCUMENT_DATA =

    TABLES

    object_header = lt_objectheader

    object_content = lt_objectcontent

* OBJECT_PARA =

* OBJECT_PARB =

* ATTACHMENT_LIST =

* RECEIVER_LIST =

    contents_hex = lt_content_hex

    EXCEPTIONS

    document_id_not_exist = 1

    operation_no_authorization = 2

    x_error = 3

    OTHERS = 4.

  ENDLOOP.

ENDFORM.                    " READ_OBJECT
*&---------------------------------------------------------------------*
*&      Form  WRITE_SRGBTBREL_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STRING[]  text
*      -->P_P_TABNAME  text
*----------------------------------------------------------------------*
FORM write_srgbtbrel_files  USING    pt_data    TYPE solix_tab
                                     p_tab      TYPE any
                                     p_instid_b TYPE any
                                     p_typeid_b TYPE any.

  DATA: lv_file      TYPE string,
        lt_string    TYPE string_table,
        ls_string    LIKE LINE OF lt_string,
        lv_error(75) TYPE c,
        lv_fname     TYPE string,
        ln           TYPE i,
        lv_file_ext  TYPE so_fileext,
        lv_objtp     TYPE so_obj_tp,
        lv_objno     TYPE so_obj_no,
        lv_instid_b  TYPE string,
        lv_obj_yr    TYPE so_obj_yr,
        lv_offset    TYPE i,
        len          TYPE i,
        diff         TYPE i.

  FIELD-SYMBOLS: <data> LIKE LINE OF pt_data.

  FIND FIRST OCCURRENCE OF 'EXT' IN p_instid_b MATCH OFFSET lv_offset.
  len  = strlen( p_instid_b ).
  diff = len - lv_offset.

  lv_instid_b = p_instid_b+lv_offset(diff).
  lv_objtp    = lv_instid_b(3).
  lv_obj_yr   = lv_instid_b+3(2).
  diff        = diff - 5.
  lv_objno    = lv_instid_b+5(diff).

  SELECT SINGLE file_ext INTO lv_file_ext
                         FROM sood
                         WHERE objtp = lv_objtp
                         AND objyr   = lv_obj_yr
                         AND objno   = lv_objno
                         AND objnam  = p_typeid_b.
  IF sy-subrc NE 0.
    lv_file_ext = '.unknown'.
  ENDIF.

  IF p_tab EQ 'SRGBTBREL'.
    CONCATENATE gv_instid_a '_' gv_brelguid INTO lv_fname.
    IF rd_app EQ 'X'.
      SEARCH p_appf FOR '/'.
      IF sy-subrc EQ 0.
        SPLIT p_appf AT '/' INTO TABLE lt_string.
      ELSE.
        SPLIT p_appf AT '\'  INTO TABLE lt_string.
      ENDIF.

      ln = lines( lt_string ).
      READ TABLE lt_string INTO ls_string INDEX ln.
      IF sy-subrc EQ 0.
        lv_file =  p_appf.
        REPLACE ALL OCCURENCES OF ls_string IN lv_file WITH  lv_fname.
        CONCATENATE lv_file lv_file_ext INTO lv_file.
      ENDIF.
    ELSE.
      CONCATENATE p_file '\' lv_fname '.' lv_file_ext INTO lv_file.
      REPLACE ALL OCCURRENCES OF '\\' IN lv_file WITH '\'.
    ENDIF.
  ENDIF.

  IF rd_pc EQ 'X'.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
*       bin_filesize          = fsize_sm
        filename              = lv_file
        filetype              = 'BIN'
*       WRITE_FIELD_SEPARATOR = 'X'
*       WRITE_LF              = ''
*       CODEPAGE              = '1404'
      TABLES
        data_tab              = pt_data[].

    IF sy-subrc EQ 0.
      WRITE: / p_tab ,' - ', 'File successful', ' - ',  lv_file.
    ENDIF.
  ELSE.
    CLEAR: ls_string.
    CALL FUNCTION 'WSI_RAW_TO_STRING'
      IMPORTING
        output             = ls_string
      TABLES
        input              = pt_data
      EXCEPTIONS
        convertion_failure = 1
        OTHERS             = 2.

    OPEN DATASET lv_file FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
    TRANSFER ls_string TO lv_file.
    CLOSE DATASET lv_file.
    IF sy-subrc EQ 0.
      WRITE: / p_tab ,' - ', 'File successful', ' - ',  lv_file.
    ENDIF.
  ENDIF.

ENDFORM.                    " WRITE_SRGBTBREL_FILES
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILES_BACKUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<GT_DATA>  text
*      -->P_P_TABNAME  text
*----------------------------------------------------------------------*
FORM write_files_backup  USING    pt_data TYPE STANDARD TABLE
                                  p_tab   TYPE any..

  DATA: lv_tabix      TYPE sytabix,
        lv_count      TYPE i,
        lv_file_no(4) TYPE n.

  DATA: "lt_header     TYPE truxs_t_text_data,
        lt_header      TYPE string_table,
        ls_header      LIKE LINE OF lt_header,
        ls_tech_header LIKE LINE OF lt_header,
        rd_split_tab  TYPE REF TO data,
        rd_split_line TYPE REF TO data,
        rd_data_line  TYPE REF TO data,
        lt_dfies      TYPE dfies_table,
        lv_file       TYPE string,
        lv_error(75)  TYPE c,
        file_name     TYPE localfile,
        lv_type       TYPE char30,
        lt_ekkn       TYPE STANDARD TABLE OF ty_ekkn,
        lv_internal   TYPE string,
        lv_external   TYPE string.

  FIELD-SYMBOLS: <split_tab>       TYPE STANDARD TABLE,
                 <split_line>      TYPE any,
                 <data>            TYPE any,
                 <internal_format> TYPE any,
                 <external_format> TYPE any.

  CLEAR: gv_lines.
  gv_lines = lines( pt_data ).

  IF p_tab NE 'AUSP+MARA' AND p_tab NE 'TOA01' AND
     p_tab NE 'DRAD'      AND p_tab NE 'SOOD'.
    lv_type = p_tab.
  ELSE.
    IF p_tab EQ 'AUSP+MARA'.
      lv_type = 'TY_MARAAUSP'.
    ELSEIF p_tab EQ 'AUSP'.
      lv_type = 'TY_MAT_CHARAC'.
    ELSEIF p_tab EQ 'TOA01'.
      lv_type = 'TY_TOA01'.
    ELSEIF p_tab EQ 'DRAD'.
      lv_type = 'TY_DRAD'.
    ELSEIF p_tab EQ 'SOOD'.
      lv_type = 'TY_SOODK'.
    ENDIF.
  ENDIF.

  CREATE DATA rd_split_tab TYPE STANDARD TABLE OF (lv_type).
  ASSIGN rd_split_tab->* TO <split_tab>.

  CREATE DATA rd_split_line TYPE (lv_type).
  ASSIGN rd_split_line->* TO <split_line>.

  CREATE DATA rd_data_line TYPE (lv_type).
  ASSIGN rd_data_line->* TO <data>.

  IF rd_pc EQ 'X'.
    file_name = p_file.
  ELSE.
    file_name = p_appf.
  ENDIF.

  IF rd_pc EQ 'X'. " PC file
    LOOP AT pt_data ASSIGNING <data>.
      lv_tabix     = sy-tabix.
      lv_count     = lv_count + 1.
      APPEND INITIAL LINE TO <split_tab> ASSIGNING <split_line>.
      <split_line> = <data>.
      IF ( lv_count EQ p_recs ) OR ( lv_tabix EQ gv_lines ).
        CLEAR: lv_file.
        lv_file_no = lv_file_no + 1.
        SHIFT lv_file_no LEFT DELETING LEADING '0'.
        PERFORM generate_filename USING file_name lv_file_no '' p_tab CHANGING lv_file.
        PERFORM write_pcfile USING p_tab <split_tab>[] lv_file.
        FREE: <split_tab>[], lv_count.
      ENDIF.
    ENDLOOP.
  ELSE.  "App server file
    LOOP AT pt_data ASSIGNING <data>.
      lv_tabix = sy-tabix.
      lv_count = lv_count + 1.
      IF lv_tabix EQ 1.
        IF gv_file_no IS INITIAL.
          gv_file_no = 1.
        ELSE.
          gv_file_no = gv_file_no + 1.
        ENDIF.
        PERFORM generate_filename USING file_name gv_file_no '' p_tab CHANGING lv_file.
        PERFORM build_header USING p_tab ls_header ls_tech_header lt_header[] CHANGING lt_dfies[].
        OPEN DATASET lv_file FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc EQ 0.
          TRANSFER ls_header TO lv_file.
          TRANSFER ls_tech_header TO lv_file.
        ELSE.
          WRITE: / p_tab ,' - ', lv_error , ' - ',  lv_file.
          EXIT.
        ENDIF.
      ELSEIF lv_count EQ p_recs.
        gv_file_no = gv_file_no + 1.
        WRITE: / p_tab ,' - ', 'File successful', ' - ',  lv_file.
        CLOSE DATASET lv_file.
        CLEAR: lv_file.
        PERFORM generate_filename USING file_name gv_file_no '' p_tab CHANGING lv_file.
        PERFORM build_header USING p_tab ls_header ls_tech_header lt_header[] CHANGING lt_dfies[].
        OPEN DATASET lv_file FOR OUTPUT MESSAGE lv_error IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc EQ 0.
          TRANSFER ls_header TO lv_file.
          TRANSFER ls_tech_header TO lv_file.
        ELSE.
          WRITE: / p_tab ,' - ', lv_error , ' - ',  lv_file.
          EXIT.
        ENDIF.
        CLEAR lv_count.
      ENDIF.
      PERFORM write_unix_file USING <data> lv_file lt_dfies[] p_tab.
      IF lv_tabix EQ gv_lines. "Write last file status/close file
        WRITE: / p_tab ,' - ', 'File successful' , ' - ',  lv_file.
        CLOSE DATASET lv_file.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " WRITE_FILES_BACKUP
*&---------------------------------------------------------------------*
*&      Form  RMV_SPECIAL_CHARACTERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_TEMP_DATA  text
*----------------------------------------------------------------------*
FORM rmv_special_characters  USING p_data TYPE any .

  REPLACE ALL OCCURRENCES OF nline IN p_data WITH space.
  REPLACE ALL OCCURRENCES OF hcr_lf   IN p_data WITH space.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+0(1) IN p_data WITH space. "Remove Carriage return only
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf+0(2) IN p_data WITH space. "Remove Line feed only
  CONDENSE p_data.

ENDFORM.                    " RMV_SPECIAL_CHARACTERS
*&---------------------------------------------------------------------*
*&      Form  READ_UPTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_STXL  text
*      -->P_P_TABNAME  text
*----------------------------------------------------------------------*
FORM read_upto  USING    pt_data   TYPE STANDARD TABLE
                         p_tabname TYPE any.

  DATA: ls_stxl LIKE LINE OF gt_stxl,
        lv_count TYPE i.

  DO.
    lv_count = lv_count + 1.
    IF lv_count GT 1.
      SELECT mandt relid tdobject tdname tdid tdspras srtf2 clustr
           FROM (p_tabname) UP TO p_pkg ROWS INTO TABLE pt_data
    WHERE relid    IN s_relid  AND
          tdobject IN s_tdobje AND
          tdname   IN s_tdname AND
          tdid     IN s_tdid   AND
          tdspras  IN s_tdspra AND
          srtf2    IN s_srtf2  AND
        ( relid    >= ls_stxl-relid AND
          tdobject >= ls_stxl-tdobject AND
          tdname   >= ls_stxl-tdname  AND
          tdid     >= ls_stxl-tdid AND
          tdspras  >= ls_stxl-tdspras AND
          srtf2    >= ls_stxl-srtf2 ).
    ELSE.
      SELECT mandt relid tdobject tdname tdid tdspras srtf2 clustr
                  FROM (p_tabname) UP TO p_pkg ROWS INTO TABLE pt_data
           WHERE relid    IN s_relid  AND
                 tdobject IN s_tdobje AND
                 tdname   IN s_tdname AND
                 tdid     IN s_tdid   AND
                 tdspras  IN s_tdspra AND
                 srtf2    IN s_srtf2.
    ENDIF.

    IF pt_data[] IS NOT INITIAL.
      READ TABLE pt_data INTO ls_stxl INDEX sy-dbcnt.
      PERFORM write_files USING gt_stxl p_tabname.
      CLEAR: gt_stxl[].
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " READ_UPTO
*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select_write .

  CHECK sy-uname EQ 'MZHOSSAIN'.

  DATA: lv_pkg_size TYPE i,
       lv_tabname  TYPE tabname,
       db_cursor   TYPE cursor,
       l_refitab   TYPE REF TO data.

  UNASSIGN: <gt_data>.
  REFRESH: gt_cond, gt_ausp, gt_mara_ausp.
  lv_tabname = p_tabs.


  IF p_nr EQ 'X'.
    IF p_tabs NE 'AUSP+MARA' AND p_tabs NE 'TOA01' AND p_tabs NE 'DRAD'.
      CREATE DATA l_refitab TYPE TABLE OF (p_tabs).
      ASSIGN l_refitab->* TO <gt_data>.
      PERFORM read_process_reg USING p_tabs lv_pkg_size.
    ELSE.
      PERFORM read_process_excep USING p_tabs lv_pkg_size.
    ENDIF.
  ENDIF.

ENDFORM.                    " DATA_SELECT_WRITE
*&---------------------------------------------------------------------*
*&      Form  READ_MARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_marc .

  DATA: gt_data   TYPE STANDARD TABLE OF marc,
        ls_data   LIKE LINE OF gt_marc,
        hold_data LIKE LINE OF gt_marc,
        lv_dbcnt  TYPE sydbcnt,
        lv_count  TYPE i,
        lv_lines  TYPE i.

  DO.
    lv_count = lv_count + 1.
    IF lv_count GT 1.
      SELECT * FROM (p_tabs) UP TO p_rn ROWS INTO TABLE gt_data
      WHERE matnr IN s_matnr
      AND werks IN s_werks
      AND matnr >= ls_data-matnr
      AND werks >= ls_data-werks
      ORDER BY matnr werks.
    ELSE.
      SELECT * FROM (p_tabs) UP TO p_rn ROWS INTO TABLE gt_data
      WHERE matnr IN s_matnr
      AND   werks IN s_werks
      ORDER BY matnr werks.
    ENDIF.
    IF gt_data[] IS NOT INITIAL.
      lv_dbcnt = sy-dbcnt.
      READ TABLE gt_data INTO ls_data INDEX 1.
      IF hold_data = ls_data.
* GT_DATA will always have it's first record from 2nd loop pass onwards
* same as last record of the previous loop pass so delete the first
* record since we already wrote the previous file with that record
        DELETE gt_data INDEX 1.
      ENDIF.
* Read last record so it could be used in where condition
      lv_lines = lines( gt_data ).
      READ TABLE gt_data INTO ls_data INDEX lv_lines.
      PERFORM write_files USING gt_data p_tabs.
      CLEAR: gt_data[].
      hold_data = ls_data.
    ELSE.
      EXIT.
    ENDIF.
    IF p_rn NE lv_dbcnt OR lv_dbcnt IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.


ENDFORM.                    " READ_MARC
*&---------------------------------------------------------------------*
*&      Form  TEST_OPEN_CURSOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM test_open_cursor .

  DATA: db_cursor TYPE cursor.

  DATA: gt_data   TYPE STANDARD TABLE OF marc,
        ls_data   LIKE LINE OF gt_marc.

  FIELD-SYMBOLS: <data> LIKE LINE OF gt_data.

  OPEN CURSOR WITH HOLD db_cursor FOR

SELECT * FROM marc WHERE matnr IN s_matnr
       AND werks IN s_werks..
  DO.
    FETCH NEXT CURSOR db_cursor INTO TABLE gt_data PACKAGE SIZE p_rn.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    LOOP AT gt_data INTO ls_data.
      PERFORM write_files USING gt_data p_tabs.
      CLEAR: gt_data[].
      EXIT.
    ENDLOOP.
* Do any updated here
*    # LOOP AT IT_TABLE INTO WA_TABLE.
*      # MODIFY#
*      # ENDLOOP.
*      #CALL FUNCTION #DB_COMMIT#.
*      #COMMIT WORK.

  ENDDO.

  CLOSE CURSOR db_cursor.

ENDFORM.                    " TEST_OPEN_CURSOR
*&---------------------------------------------------------------------*
*&      Form  READ_MARC_BY_KEYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_marc_by_keys .

  DATA: lines TYPE i,
       gt_data   TYPE STANDARD TABLE OF marc.

  TYPES:
  BEGIN OF key_package_type,
    matnr TYPE matnr,
    werks TYPE werks,
  END OF key_package_type,

  BEGIN OF from_to,
    from TYPE key_package_type,
    to   TYPE key_package_type,
  END OF from_to,

t_key_packages_type TYPE STANDARD TABLE OF key_package_type.

  DATA: lt_key_packages TYPE t_key_packages_type,
        lt_from_to TYPE STANDARD TABLE OF from_to,
        ls_from_to LIKE LINE OF lt_from_to.

  FIELD-SYMBOLS: <key_pkg> LIKE LINE OF lt_key_packages,
                 <from_to> LIKE LINE OF lt_from_to.

* select only the primary keys, in packages
  SELECT matnr werks FROM marc
      INTO TABLE lt_key_packages PACKAGE SIZE p_rn
      WHERE matnr IN s_matnr
      AND   werks IN s_werks
      ORDER BY matnr werks.

    APPEND INITIAL LINE TO lt_from_to ASSIGNING <from_to>.
    READ TABLE lt_key_packages ASSIGNING <key_pkg> INDEX 1.
    <from_to>-from-matnr = <key_pkg>-matnr.
    <from_to>-from-werks = <key_pkg>-werks.

    UNASSIGN <key_pkg>.
    lines = lines( lt_key_packages ).
    READ TABLE lt_key_packages ASSIGNING <key_pkg> INDEX lines.
    <from_to>-to-matnr = <key_pkg>-matnr.
    <from_to>-to-werks = <key_pkg>-werks.

*    APPEND ls_from_to TO lt_from_to.
  ENDSELECT.

* select the actual data by the primary key packages
  LOOP AT lt_from_to ASSIGNING <from_to>.
    SELECT * FROM marc INTO TABLE gt_data
      WHERE matnr IN s_matnr
      AND   werks IN s_werks
      AND  ( ( matnr >= <from_to>-from-matnr AND matnr <= <from_to>-to-matnr )
      AND   ( werks >= <from_to>-from-werks AND werks <= <from_to>-to-werks ) )
      ORDER BY matnr werks.

    PERFORM write_files USING gt_data p_tabs.
    CLEAR: gt_data[].
  ENDLOOP.

ENDFORM.                    " READ_MARC_BY_KEYS
*&---------------------------------------------------------------------*
*&      Form  READ_T001_BY_KEYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_t001_by_keys .

*  DATA: LINES TYPE I,
*        GT_DATA   TYPE STANDARD TABLE OF T001.
*
*  TYPES:
*  BEGIN OF KEY_PACKAGE_TYPE,
*    KEY1 TYPE BUKRS,
*  END OF KEY_PACKAGE_TYPE,
*
*  BEGIN OF FROM_TO,
*    FROM TYPE KEY_PACKAGE_TYPE,
*    TO   TYPE KEY_PACKAGE_TYPE,
*  END OF FROM_TO,
*
*T_KEY_PACKAGES_TYPE TYPE STANDARD TABLE OF KEY_PACKAGE_TYPE.
*
*  DATA: LT_KEY_PACKAGES TYPE T_KEY_PACKAGES_TYPE,
*        LT_FROM_TO TYPE STANDARD TABLE OF FROM_TO,
*        LS_FROM_TO LIKE LINE OF LT_FROM_TO.
*
*  FIELD-SYMBOLS: <KEY_PKG> LIKE LINE OF LT_KEY_PACKAGES,
*                 <FROM_TO> LIKE LINE OF LT_FROM_TO.
*
** select only the primary keys, in packages
*  SELECT BUKRS  FROM T001
*      INTO TABLE LT_KEY_PACKAGES PACKAGE SIZE P_RN
*      WHERE BUKRS IN S_BUKRS
*      ORDER BY BUKRS.
*
*    APPEND INITIAL LINE TO LT_FROM_TO ASSIGNING <FROM_TO>.
*    READ TABLE LT_KEY_PACKAGES ASSIGNING <KEY_PKG> INDEX 1.
*    <FROM_TO>-FROM-KEY1 = <KEY_PKG>-KEY1.
*
*    UNASSIGN <KEY_PKG>.
*    LINES = LINES( LT_KEY_PACKAGES ).
*    READ TABLE LT_KEY_PACKAGES ASSIGNING <KEY_PKG> INDEX LINES.
*    <FROM_TO>-TO-KEY1 = <KEY_PKG>-KEY1.
*
**    APPEND ls_from_to TO lt_from_to.
*  ENDSELECT.
*
** select the actual data by the primary key packages
*  LOOP AT LT_FROM_TO ASSIGNING <FROM_TO>.
*    SELECT * FROM T001 INTO TABLE GT_DATA
*      WHERE BUKRS >= <FROM_TO>-FROM-KEY1
*      AND   BUKRS <= <FROM_TO>-TO-KEY1
*      ORDER BY BUKRS.
*
*    PERFORM WRITE_FILES USING GT_DATA P_TABS.
*    CLEAR: GT_DATA[].
*  ENDLOOP.

  DATA: db_cursor TYPE cursor.

  DATA: gt_data   TYPE STANDARD TABLE OF t001,
        ls_data   LIKE LINE OF gt_data.

  FIELD-SYMBOLS: <data> LIKE LINE OF gt_data.

  OPEN CURSOR WITH HOLD db_cursor FOR

SELECT * FROM t001 WHERE bukrs IN s_bukrs.
  DO.
    FETCH NEXT CURSOR db_cursor INTO TABLE gt_data PACKAGE SIZE p_rn.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    PERFORM write_files USING gt_data p_tabs.
    CLEAR: gt_data[].
  ENDDO.

  CLOSE CURSOR db_cursor.

ENDFORM.                    " READ_T001_BY_KEYS
