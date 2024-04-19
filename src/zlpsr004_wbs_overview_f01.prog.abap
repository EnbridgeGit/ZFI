*&---------------------------------------------------------------------*
*&  Include           ZLPSR004_WBS_OVERVIEW_F01
*&---------------------------------------------------------------------*
*Get the global data of clas from class constructor method
CLASS lcl_alv_event DEFINITION.
  PUBLIC SECTION .
    METHODS:
      on_user_command     FOR EVENT user_command OF cl_gui_alv_grid
                          IMPORTING e_ucomm,
      check_sel_rows      IMPORTING iv_ucomm   TYPE sy-ucomm
                          EXPORTING et_rows    TYPE lvc_t_row
                                    ev_failure TYPE char1,
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
                          IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_alv_event DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_alv_event IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_event IMPLEMENTATION.
  METHOD check_sel_rows.
    DATA: lt_rows TYPE lvc_t_row,
          lv_rows TYPE i.
*--- Get selected Rows
    o_alvgd->get_selected_rows(
      IMPORTING
        et_index_rows = lt_rows ).    " Indexes of Selected Rows

    IF lt_rows IS INITIAL.
      MESSAGE s005(zp) WITH text-e04 DISPLAY LIKE if_xo_const_message=>error.
      ev_failure = abap_true.
      RETURN.
    ELSE.
      et_rows = lt_rows.
    ENDIF.
  ENDMETHOD.                    "check_sel_rows
  METHOD handle_double_click.
    DATA: lv_index LIKE sy-index.
    IF NOT es_row_no-sub_row_id IS INITIAL.
      MESSAGE i025(psisso).
      EXIT.
    ENDIF.
    CLEAR lv_index.
    MOVE e_row-index TO lv_index.
    CALL TRANSACTION 'CJ20N' AND SKIP FIRST SCREEN.
  ENDMETHOD.                    "handle_double_click
  METHOD on_user_command.
    CASE e_ucomm.
      WHEN 'A'.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD. "handle_user_command
ENDCLASS.                    "lcl_alv_event IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_FCAT
*&---------------------------------------------------------------------*
*  Modifying field descriptions in Fieldcatalog
*----------------------------------------------------------------------*
*      -->P_REPTEXT   Field text for display
*      -->P_STEXT     Short text for field names
*      -->P_MTEXT     Middle text for field names
*      -->P_LTEXT     Long text for field names
*      -->P_FNAME     Field Names
*      <-->P_FIELDCAT Fieldctalog that is changed
*----------------------------------------------------------------------*
FORM f_modify_fcat  USING  p_reptext  TYPE reptext
                           p_stext    TYPE scrtext_s
                           p_mtext    TYPE scrtext_m
                           p_ltext    TYPE scrtext_l
                           p_fname    TYPE lvc_fname
                  CHANGING p_fieldcat TYPE lvc_s_fcat.

  p_fieldcat-reptext   = p_reptext.
  p_fieldcat-scrtext_s = p_stext.
  p_fieldcat-scrtext_m = p_mtext.
  p_fieldcat-scrtext_l = p_ltext.
*  IF p_fname = 'INVALID_RECORD'.
*    p_fieldcat-no_out = gc_set.
*  ENDIF.
ENDFORM.                    "f_modify_fcat
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FCAT
*&---------------------------------------------------------------------*
*   Build fieldcatalog for ALV display
*----------------------------------------------------------------------*
*      -->P_LWA_DFIES     Field characteristics
*      <--P_LWA_FIELDCAT  Fieldcatalog
*----------------------------------------------------------------------*
FORM f_build_fcat  USING    p_lwa_dfies    TYPE dfies
                   CHANGING p_lwa_fieldcat TYPE lvc_s_fcat.
  CASE p_lwa_dfies-fieldname.
    WHEN 'PSPID'.
      PERFORM f_modify_fcat USING text-h01 text-h02 text-h03 text-h04 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'VERNA'.
      PERFORM f_modify_fcat USING text-h05 text-h06 text-h07 text-h08 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'WBS'.
      PERFORM f_modify_fcat USING text-h09 text-h10 text-h11 text-h12 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'KALSM'.
      PERFORM f_modify_fcat USING text-h13 text-h14 text-h14 text-h15 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'POST1'.
      PERFORM f_modify_fcat USING text-h16 text-h16 text-h16 text-h17 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
**--start of changes by akmadasu CHG0138615
    WHEN 'ASLOC'.
      PERFORM f_modify_fcat USING text-h44 text-h44 text-h44 text-h45 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
**--end of changes by akmadasu CHG0138615
    WHEN 'STUFE'.
      PERFORM f_modify_fcat USING text-h18 text-h19 text-h19  text-h19 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'LOEVM'.
      PERFORM f_modify_fcat USING text-h20 text-h21 text-h22 text-h22 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'DESC'.
      PERFORM f_modify_fcat USING text-h23 text-h23 text-h23 text-h23 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'USR01'.
      PERFORM f_modify_fcat USING text-h24 text-h24 text-h24 text-h24 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'USR08'.
      PERFORM f_modify_fcat USING text-h25 text-h25 text-h25 text-h25 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'USR09'.
      PERFORM f_modify_fcat USING text-h26 text-h26 text-h26 text-h26 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'PLFAZ'.
      PERFORM f_modify_fcat USING text-h28 text-h29 text-h30 text-h31 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'PLSEZ'.
      PERFORM f_modify_fcat USING text-h32 text-h33 text-h34 text-h35 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN 'SPROG'.
      PERFORM f_modify_fcat USING text-h36 text-h37 text-h38 text-h39 p_lwa_dfies-fieldname
                          CHANGING p_lwa_fieldcat.
    WHEN 'EPROG'.
      PERFORM f_modify_fcat USING text-h40 text-h41 text-h42 text-h43 p_lwa_dfies-fieldname
                         CHANGING p_lwa_fieldcat.
    WHEN OTHERS.
*Do nothing
  ENDCASE.
ENDFORM.                    "f_build_fcat
*&---------------------------------------------------------------------*
*&      Form  build_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_field_cat.
  DATA:
    lo_tabdescr  TYPE REF TO cl_abap_structdescr,
    lo_columns   TYPE REF TO cl_salv_columns_table,
    lo_column    TYPE REF TO cl_salv_column,
    lo_data      TYPE REF TO data,
    lt_dfies     TYPE ddfields,
    lwa_dfies    TYPE dfies,
    lwa_fieldcat TYPE lvc_s_fcat.
  DATA lv_hidecol TYPE lvc_fname.


  CREATE DATA lo_data LIKE LINE OF gt_report_details.
  lo_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lo_data ).

  lt_dfies = cl_salv_data_descr=>read_structdescr( lo_tabdescr ).

  CLEAR gt_fcat.

  LOOP AT lt_dfies INTO lwa_dfies.
*Modify Field Catalog Headings for Display
    MOVE-CORRESPONDING lwa_dfies TO lwa_fieldcat.
    lwa_fieldcat-col_pos = sy-tabix.
    PERFORM f_build_fcat USING  lwa_dfies CHANGING lwa_fieldcat.
    APPEND lwa_fieldcat TO gt_fcat.
  ENDLOOP.

ENDFORM.                    "build_field_cat
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*   PBO Module for PF Status of ALV
*----------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  SET   PF-STATUS 'ZWBS'.
  SET TITLEBAR 'ZTITLE1'.
ENDMODULE.                    "status_1001 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*   PAI Module User Command for ALV
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
*Do nothing
  ENDCASE.
ENDMODULE.                    "user_command_1001 INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  IF gt_report_details IS NOT INITIAL.
    SORT gt_report_details BY wbs.
    DATA: ls_stable     TYPE lvc_s_stbl,
          ls_disvariant TYPE disvariant,
          lt_sort       TYPE lvc_t_sort,
          ls_sort       TYPE lvc_s_sort.
*create object for custom container
    CREATE OBJECT o_ccont
      EXPORTING
        container_name = gc_container.
*create object of alv grid
    CREATE OBJECT o_alvgd
      EXPORTING
        i_parent = o_ccont.
* create ALV event handler
    CREATE OBJECT o_alv_toolbar.
* Register event handler
    gwa_layout-grid_title = text-t03.
    gwa_layout-zebra      = abap_true.
    gwa_layout-cwidth_opt = abap_true.
    gwa_layout-sel_mode   = 'A'.
*    gwa_layout-excp_fname = 'STATUS'.
*    gwa_layout-stylefname = 'HANDL_BUTTON'.

*  Build filed catalog
    PERFORM build_field_cat.
    "SET HANDLER o_alv_toolbar->on_toolbar FOR o_alvgd.
    SET HANDLER o_alv_toolbar->on_user_command FOR o_alvgd.

    CLEAR ls_sort.
    ls_sort-fieldname = 'WBS'.
    ls_sort-up        = abap_true.
    ls_sort-spos      = 2.
    APPEND ls_sort TO lt_sort.
    IF gt_report_details IS NOT INITIAL.
      CALL METHOD o_alvgd->set_table_for_first_display
        EXPORTING
          i_save                        = 'A'
          is_layout                     = gwa_layout    " Layout
        CHANGING
          it_outtab                     = gt_report_details  " Output Table
          it_fieldcatalog               = gt_fcat      " Field Catalog
          it_sort                       = lt_sort
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.
      MOVE abap_true TO: ls_stable-row, ls_stable-col.
      o_alvgd->refresh_table_display(
        EXPORTING is_stable = ls_stable
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF."endif sy-subrc <> 0.

    ENDIF."endif it_alvout IS NOT INITIAL.
  ENDIF.
ENDMODULE.                    "display_alv OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_refresh .
  REFRESH:gt_proj_details,gt_wbs_nos ,gt_wbs_details, gt_report_details.
ENDFORM.                    " F_REFRESH
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE_INPUTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_validate_inputs.
  DATA: lv_proj  TYPE ps_pspid,
        lv_wbs  TYPE ps_posid,
        lv_status TYPE tj02-istat.

*Validate Project numbers
  SELECT pspid FROM proj INTO lv_proj UP TO 1 ROWS
    WHERE pspid IN s_proj.
  ENDSELECT.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE s005(zp) WITH text-e01 DISPLAY LIKE gc_error.
    LEAVE LIST-PROCESSING.
  ENDIF.
*Validate WBS elements
  SELECT pspnr FROM prps INTO lv_wbs UP TO 1 ROWS
    WHERE pspnr IN s_wbs.
  ENDSELECT.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE s005(zp) WITH text-e02 DISPLAY LIKE gc_error.
    LEAVE  LIST-PROCESSING.
  ENDIF.
*Validate Status
  SELECT istat FROM tj02 INTO lv_status UP TO 1 ROWS
    WHERE istat IN s_status.
  ENDSELECT.
  IF sy-subrc IS NOT INITIAL.
    CLEAR lv_status.
    SELECT estat FROM tj30 INTO lv_status UP TO 1 ROWS
    WHERE estat IN s_status.
    ENDSELECT.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s005(zp) WITH text-e03 DISPLAY LIKE gc_error.
      LEAVE  LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_VALIDATE_INPUTS
*&---------------------------------------------------------------------*
*&      Form  F_FETCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fetch.
*local internal table
  DATA:lt_proj_details TYPE STANDARD TABLE OF gty_proj_details INITIAL SIZE 0,
       lt_wbs_nos      TYPE STANDARD TABLE OF gty_wbs_nos      INITIAL SIZE 0,
       lt_wbs_details  TYPE STANDARD TABLE OF gty_wbs_details  INITIAL SIZE 0,
       lt_objnr        TYPE STANDARD TABLE OF jsto_pre         INITIAL SIZE 0,
       lt_status       TYPE STANDARD TABLE OF jest             INITIAL SIZE 0,
       lt_sys_status   TYPE STANDARD TABLE OF jest             INITIAL SIZE 0,
       lt_usr_status   TYPE STANDARD TABLE OF jest             INITIAL SIZE 0,
       lt_sysstat_text TYPE STANDARD TABLE OF gty_status_text  INITIAL SIZE 0,
       lt_usrstat_text TYPE STANDARD TABLE OF gty_status_text  INITIAL SIZE 0,
       lt_status_text  TYPE STANDARD TABLE OF gty_status_text  INITIAL SIZE 0,
       lt_sysstat      TYPE STANDARD TABLE OF gty_status       INITIAL SIZE 0,
       lt_usrstat      TYPE STANDARD TABLE OF gty_status       INITIAL SIZE 0.
  DATA:ls_objnr        TYPE jsto_pre,
       ls_usrstat_text TYPE gty_status_text,
       ls_sysstat_text TYPE gty_status_text,
       ls_usrstat      TYPE gty_status,
       ls_sysstat      TYPE gty_status.
  FIELD-SYMBOLS:<lfs_proj_details> TYPE gty_proj_details,
                <lfs_wbs_nos>      TYPE gty_wbs_nos.
**--START OF CHANGES BY AKMADASU
        DATA: LV_NAME TYPE THEAD-TDNAME,
              LT_LINES TYPE STANDARD TABLE OF TLINE,
              LWA_LINES TYPE TLINE.
**--end OF CHANGES BY AKMADASU
  DATA:lwa_objnr        TYPE jsto_pre,
       lwa_status       TYPE jest,
       lwa_status_text  TYPE gty_status_text.
  DATA:lv_status        TYPE bsvx-sttxt.
  DATA:lv_stsma         TYPE jsto-stsma,
       lv_anw_stat      TYPE xfeld,
       lv_sysstat_text  TYPE bsvx-sttxt,
       lv_usrstat_text  TYPE bsvx-sttxt.
  CONSTANTS:lc_dlfl     TYPE char4 VALUE 'DLFL'.
*Database fetch
  IF s_proj IS NOT INITIAL.
    IF s_wbs IS NOT INITIAL.
*Select data based on WBS and project Info entered
*Select data based on WBS Info entered
      SELECT pspnr
             "posid
             post1
             objnr
             psphi
             verna
             stufe
             kalsm
             loevm
             usr01
             usr08
             usr09
        FROM prps
        INTO TABLE gt_wbs_details
        WHERE pspnr IN s_wbs
        AND loevm EQ space.
      IF sy-subrc IS INITIAL.
        SORT gt_wbs_details BY pspnr.
      ENDIF.
*Fetch project details
*Select data based on project Info entered
      IF lt_wbs_details IS NOT INITIAL.
        SELECT  pspnr
                pspid
                plfaz
                plsez
                sprog
                eprog
          FROM proj
          INTO TABLE gt_proj_details
          WHERE pspnr IN s_proj.
        IF sy-subrc IS INITIAL.
          SORT gt_proj_details BY pspid.
        ENDIF.
      ENDIF.
    ELSE.
*Select data based on project Info entered
      SELECT  pspnr
              pspid
              plfaz
              plsez
              sprog
              eprog
        FROM proj
        INTO TABLE gt_proj_details
        WHERE pspid IN s_proj.
      IF sy-subrc IS INITIAL.
        SORT gt_proj_details BY pspid.
        lt_proj_details = gt_proj_details.
        DELETE ADJACENT DUPLICATES FROM lt_proj_details COMPARING pspid.
      ENDIF.
*Fetch WBS details
      IF lt_proj_details IS NOT INITIAL.
        SELECT posnr
               psphi
          FROM prhi
          INTO TABLE gt_wbs_nos
          FOR ALL ENTRIES IN lt_proj_details
          WHERE psphi = lt_proj_details-pspnr.
        IF sy-subrc IS INITIAL.
          SORT gt_wbs_nos BY wbs.
        ENDIF.
      ENDIF.
*Select data based on WBS Info entered
      SELECT pspnr
             "posid
             post1
             objnr
             psphi
             verna
             stufe
             kalsm
             loevm
             usr01
             usr08
             usr09
        FROM prps
        INTO TABLE gt_wbs_details
        FOR ALL ENTRIES IN gt_wbs_nos
        WHERE pspnr = gt_wbs_nos-wbs
        AND loevm EQ space.
      IF sy-subrc IS INITIAL.
        SORT gt_wbs_details BY pspnr.
      ENDIF.
    ENDIF.
  ELSE.
    IF s_wbs IS NOT INITIAL.
*Select data based on WBS Info entered
      SELECT pspnr
             "posid
             post1
             objnr
             psphi
             verna
             stufe
             kalsm
             loevm
             usr01
             usr08
             usr09
        FROM prps
        INTO TABLE gt_wbs_details
        WHERE pspnr IN s_wbs
        AND loevm EQ space.
      IF sy-subrc IS INITIAL.
        SORT gt_wbs_details BY pspnr.
        lt_wbs_details = gt_wbs_details.
        SORT lt_wbs_details BY psphi.
        DELETE ADJACENT DUPLICATES FROM lt_wbs_details COMPARING psphi.
      ENDIF.
*Fetch project details
*Select data based on project Info entered
      IF lt_wbs_details IS NOT INITIAL.
        SELECT  pspnr
                pspid
                plfaz
                plsez
                sprog
                eprog
          FROM proj
          INTO TABLE gt_proj_details
          FOR ALL ENTRIES IN lt_wbs_details
          WHERE pspnr EQ lt_wbs_details-psphi.
        IF sy-subrc IS INITIAL.
          SORT gt_proj_details BY pspid.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*If WBS level is entered filter the WBS fetched from DB
*based on the levels entered in selection
  IF s_level IS NOT INITIAL.
    DELETE gt_wbs_details WHERE stufe NOT IN s_level.
  ENDIF.

*Get the system and user status for WBS elements in projects
  IF gt_wbs_details IS NOT INITIAL.
    LOOP AT gt_wbs_details INTO gwa_wbs_details.
      lwa_objnr-objnr = gwa_wbs_details-objnr.
      APPEND lwa_objnr TO lt_objnr.
    ENDLOOP.
  ENDIF.
  IF lt_objnr IS NOT INITIAL.
    CALL FUNCTION 'STATUS_READ_MULTI'
      EXPORTING
        client               = sy-mandt
        only_active          = gc_set
*       ALL_IN_BUFFER        = ' '
*       GET_CHANGE_DOCUMENTS = ' '
*       NO_BUFFER_FILL       = ' '
      TABLES
        objnr_tab            = lt_objnr
        status               = lt_status.
*         JSTO_TAB                   =
*         JCDO_TAB                   =
*         JCDS_TAB                   = .
    LOOP AT lt_objnr INTO ls_objnr.
      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          client            = sy-mandt
          flg_user_stat     = gc_set
          objnr             = ls_objnr-objnr
          only_active       = gc_set
          spras             = sy-langu
        IMPORTING
          anw_stat_existing = lv_anw_stat
          e_stsma           = lv_stsma
          line              = lv_sysstat_text
          user_line         = lv_usrstat_text
        EXCEPTIONS
          object_not_found  = 1
          OTHERS            = 2.
      IF sy-subrc IS INITIAL.
*Filter by status selected in selection screen
        IF lv_sysstat_text IS NOT INITIAL.
*Get all system statuses
          ls_sysstat_text-objnr     = ls_objnr-objnr.
          ls_sysstat_text-desc      = lv_sysstat_text.
          ls_sysstat_text-spras     = sy-langu.
          APPEND ls_sysstat_text TO lt_sysstat_text.
        ENDIF.
*Get all user statuses
*Filter by status selected in selection screen
        IF lv_usrstat_text IS NOT INITIAL.
          ls_usrstat_text-objnr     = ls_objnr-objnr.
          ls_usrstat_text-desc      = lv_usrstat_text.
          ls_usrstat_text-spras     = sy-langu.
          APPEND ls_usrstat_text TO lt_usrstat_text.
        ENDIF.
      ENDIF.
      CLEAR :lv_usrstat_text,lv_sysstat_text,ls_sysstat_text,ls_usrstat_text,ls_objnr.
    ENDLOOP.
  ENDIF.

  APPEND LINES OF:lt_sysstat_text TO lt_status_text,
                  lt_usrstat_text TO lt_status_text.
  DELETE lt_status_text WHERE desc CS lc_dlfl."Deleted WBS - no display
*Get the text for system status from TJ02T
  IF lt_status IS NOT INITIAL.
    SORT lt_status BY stat.
    IF s_status IS NOT INITIAL.
      DELETE lt_status WHERE stat NOT IN s_status.
    ENDIF.
    IF lt_status IS NOT INITIAL.
      lt_sys_status = lt_status.
      DELETE lt_sys_status WHERE stat(1) NE gc_sys.
      DELETE ADJACENT DUPLICATES FROM lt_sys_status COMPARING stat.
      IF lt_sys_status IS NOT INITIAL.
        SELECT istat
               spras
               txt04
          FROM tj02t
          INTO TABLE lt_sysstat
          FOR ALL ENTRIES IN lt_sys_status
          WHERE istat = lt_sys_status-stat
            AND spras = sy-langu.
        IF sy-subrc IS INITIAL.
          SORT lt_sysstat BY stat.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*Get the text for user status using TJ30T
  IF lt_status IS NOT INITIAL.
    SORT lt_status BY stat.
    IF s_status IS NOT INITIAL.
      DELETE lt_status WHERE stat NOT IN s_status.
      DELETE ADJACENT DUPLICATES FROM lt_status COMPARING objnr.
    ENDIF.
    IF lt_status IS NOT INITIAL.
      lt_usr_status = lt_status.
      DELETE lt_usr_status WHERE stat(1) NE gc_usr.
      DELETE ADJACENT DUPLICATES FROM lt_sys_status COMPARING stat.
      IF lt_usr_status IS NOT INITIAL.
        SELECT estat
               spras
               txt04
          FROM tj30t
          INTO TABLE lt_usrstat
          FOR ALL ENTRIES IN lt_usr_status
          WHERE estat = lt_usr_status-stat
            AND spras = sy-langu.
        IF sy-subrc IS INITIAL.
          SORT lt_usrstat BY stat.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*Fill final table.
  SORT :gt_wbs_details  BY objnr,
        gt_proj_details BY pspnr,
        lt_status       BY objnr,
        lt_status_text  BY objnr.
  IF lt_status_text IS NOT INITIAL.
*    LOOP AT gt_wbs_details INTO gwa_wbs_details.
*      READ TABLE gt_proj_details INTO gwa_proj_details
*                                 WITH KEY pspnr = gwa_wbs_details-psphi
*                                 BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        gwa_report_details-pspid = gwa_proj_details-pspid.
*        gwa_report_details-plfaz = gwa_proj_details-plfaz.
*        gwa_report_details-plsez = gwa_proj_details-plsez.
*        gwa_report_details-sprog = gwa_proj_details-sprog.
*        gwa_report_details-eprog = gwa_proj_details-eprog.
*      ENDIF.
*      gwa_report_details-verna = gwa_wbs_details-verna.
*      gwa_report_details-wbs   = gwa_wbs_details-pspnr.
*      gwa_report_details-kalsm = gwa_wbs_details-kalsm.
*      gwa_report_details-post1 = gwa_wbs_details-post1.
*      gwa_report_details-stufe = gwa_wbs_details-stufe.
*      gwa_report_details-loevm = gwa_wbs_details-loevm.
*      gwa_report_details-usr01 = gwa_wbs_details-usr01.
*      gwa_report_details-usr08 = gwa_wbs_details-usr08.
*      gwa_report_details-usr09 = gwa_wbs_details-usr09.
*      LOOP AT lt_status INTO lwa_status
*                        WHERE objnr = gwa_wbs_details-objnr.
*        READ TABLE lt_status_text INTO lwa_status_text
*                                  WITH KEY stat = lwa_status-stat
*                                  BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          lv_status = lwa_status_text-desc.
*          CONCATENATE gwa_report_details-desc lv_status
*                 INTO gwa_report_details-desc
*                 SEPARATED BY space.
*        ENDIF.
*        CLEAR:lv_status,lwa_status,lwa_status_text.
*      ENDLOOP.
*      APPEND gwa_report_details TO gt_report_details.
*      CLEAR:gwa_wbs_details,gwa_report_details.
*    ENDLOOP.

    LOOP AT lt_status_text INTO lwa_status_text.
      READ TABLE lt_status      INTO lwa_status
                                WITH KEY objnr = lwa_status_text-objnr
                                BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lv_status = lwa_status_text-desc.
        IF s_status IS NOT INITIAL.
          READ TABLE lt_sysstat      INTO ls_sysstat
                                     WITH KEY stat = lwa_status-stat
                                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            IF lv_status CS ls_sysstat-desc.
              gwa_report_details-desc = lv_status.
            ENDIF.
          ENDIF.
          READ TABLE lt_usrstat      INTO ls_usrstat
                                     WITH KEY stat = lwa_status-stat
                                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            IF lv_status CS ls_usrstat-desc.
              gwa_report_details-desc = lv_status.
            ENDIF.
          ENDIF.
        ELSE.
          gwa_report_details-desc = lv_status.
        ENDIF.
        IF s_status IS NOT INITIAL AND gwa_report_details-desc IS INITIAL.
          CONTINUE.
        ENDIF.
*      CONCATENATE gwa_report_details-desc lv_status
*             INTO gwa_report_details-desc
*             SEPARATED BY space.
        READ TABLE gt_wbs_details INTO gwa_wbs_details
                                  WITH KEY objnr = lwa_status-objnr
                                  BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_report_details-verna = gwa_wbs_details-verna.
          gwa_report_details-wbs   = gwa_wbs_details-pspnr.
          gwa_report_details-kalsm = gwa_wbs_details-kalsm.
          gwa_report_details-post1 = gwa_wbs_details-post1.
          gwa_report_details-stufe = gwa_wbs_details-stufe.
          gwa_report_details-loevm = gwa_wbs_details-loevm.
          gwa_report_details-usr01 = gwa_wbs_details-usr01.
          gwa_report_details-usr08 = gwa_wbs_details-usr08.
          gwa_report_details-usr09 = gwa_wbs_details-usr09.
          READ TABLE gt_proj_details INTO gwa_proj_details
                                     WITH KEY pspnr = gwa_wbs_details-psphi
                                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            gwa_report_details-pspid = gwa_proj_details-pspid.
            gwa_report_details-plfaz = gwa_proj_details-plfaz.
            gwa_report_details-plsez = gwa_proj_details-plsez.
            gwa_report_details-sprog = gwa_proj_details-sprog.
            gwa_report_details-eprog = gwa_proj_details-eprog.
          ENDIF.
        ENDIF.
**--START OF CHANGES BY AKMADASU
        CONCATENATE 'E' lwa_status_text-OBJNR+2(8) INTO LV_NAME.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            ID                            = 'LTXT'
            LANGUAGE                      = 'E'
            NAME                          = LV_NAME
            OBJECT                        = 'PMS'
          TABLES
            LINES                         = LT_LINES
         EXCEPTIONS
           ID                            = 1
           LANGUAGE                      = 2
           NAME                          = 3
           NOT_FOUND                     = 4
           OBJECT                        = 5
           REFERENCE_CHECK               = 6
           WRONG_ACCESS_TO_ARCHIVE       = 7
           OTHERS                        = 8
                  .
        IF SY-SUBRC EQ 0.
* Implement suitable error handling here
          READ TABLE LT_LINES INTO LWA_LINES INDEX 2.
          IF SY-SUBRC IS INITIAL.
            gwa_report_details-asloc = LWA_LINES-TDLINE+0(20).
            clear:lt_lines[],lwa_lines.
          ENDIF.
        ENDIF.
        clear:lv_name.
***-- end of changes by akmadasu
        APPEND gwa_report_details TO gt_report_details.
      ENDIF.
      CLEAR:lv_status,lwa_status,lwa_status_text,
            gwa_wbs_details,gwa_report_details,
            gwa_proj_details.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_FETCH
*&---------------------------------------------------------------------*
*&      Form  F_REPORT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_report_display .
*Display ALV
  CALL SCREEN 1001.

*Intercative ALV functionality
ENDFORM.                    " F_REPORT_DISPLAY
