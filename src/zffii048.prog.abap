*&---------------------------------------------------------------------*
*& Report  ZFFII048
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zffii048.

TABLES: prps,
        cdhdr.

TYPES: BEGIN OF ty_objectid,
         objid TYPE cdhdr-objectid,
       END OF ty_objectid.
TYPES: BEGIN OF ty_output,
        project_id(35),
        proj_name(40),
        long_desc(50),
        start_date(10),
        end_date(10),
        status(1),
        proj_hier(30),
        bukrs(4),
       END OF ty_output.

CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.
DATA: gv_filename TYPE string,
      gt_objectid TYPE TABLE OF ty_objectid,
      gs_objectid TYPE ty_objectid,
      gt_output TYPE TABLE OF ty_output,
      gt_output1 TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X'
                                      USER-COMMAND cmd,
            p_sfile   LIKE        rfpdo-rfbifile MODIF ID srv, "dsp,
            r_local   RADIOBUTTON GROUP rad1,
            p_file    TYPE        rfpdo-rfbifile DEFAULT 'H:\'
                                                  MODIF ID lcl.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: r_delta  RADIOBUTTON GROUP rad2 DEFAULT 'X',
            r_all    RADIOBUTTON GROUP rad2.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_bukrs FOR prps-pbukr DEFAULT 'UGL' OBLIGATORY,
                s_posid FOR prps-posid OBLIGATORY,
                s_date FOR  prps-erdat NO-EXTENSION OBLIGATORY.
                "s_object FOR cdhdr-objectid NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  s_posid-sign = 'I'.
  s_posid-option = 'CP'. "'EQ'
  s_posid-low = '*12*'.
  APPEND s_posid.
  s_posid-sign = 'I'.
  s_posid-option = 'CP'. "'EQ'
  s_posid-low = '*13*'.
  APPEND s_posid.
  s_posid-sign = 'I'.
  s_posid-option = 'CP'. "'EQ'
  s_posid-low = '*14*'.
  APPEND s_posid.
  s_posid-sign = 'I'.
  s_posid-option = 'CP'. "'EQ'
  s_posid-low = '*15*'.
  APPEND s_posid.
  s_posid-sign = 'I'.
  s_posid-option = 'CP'. "'EQ'
  s_posid-low = '*16*'.
  APPEND s_posid.
  s_posid-sign = 'I'.
  s_posid-option = 'CP'. "'EQ'
  s_posid-low = '*17*'.
  APPEND s_posid.
  s_posid-sign = 'I'.
  s_posid-option = 'CP'. "'EQ'
  s_posid-low = '*18*'.
  APPEND s_posid.
  s_posid-sign = 'I'.
  s_posid-option = 'EQ'.
  s_posid-low = '43-08-308-4611'.
  APPEND s_posid.

  s_date-sign = 'I'.
  s_date-option = 'BT'.
  s_date-low = sy-datum - 7.
  s_date-high = sy-datum.
  APPEND s_date.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid '/WORKDAY/CO/' INTO p_sfile.

AT SELECTION-SCREEN OUTPUT.
  PERFORM  toggle_functionality.

START-OF-SELECTION.

  IF s_date-low IS INITIAL OR
     s_date-high IS INITIAL.
    WRITE: / 'Enter correct date on selection screen..'.
    STOP.
  ENDIF.
  IF s_date-low > s_date-high.
    WRITE: / 'From Date is higher than TO Date..'.
    STOP.
  ENDIF.
  IF r_server IS NOT INITIAL AND
       p_sfile IS INITIAL.
    WRITE : / 'Enter Application Server File Path Only'.
    STOP.
  ENDIF.
  IF r_local IS NOT INITIAL AND
     p_file IS INITIAL.
    WRITE : / 'Enter PC File Path Only'.
    STOP.
  ENDIF.
  IF r_delta IS NOT INITIAL.
    IF r_local IS NOT INITIAL.
      IF sy-sysid <> 'P01'.
        CONCATENATE p_file 'TEST_SAP_EAST_WBS_' sy-datum sy-uzeit '.TXT'
                                  INTO   gv_filename.
      ELSE.
        CONCATENATE p_file 'SAP_EAST_WBS_' sy-datum sy-uzeit '.TXT'
                                   INTO   gv_filename.
      ENDIF.
    ELSE.
      IF sy-sysid <> 'P01'.
        CONCATENATE p_sfile 'TEST_SAP_EAST_WBS_' sy-datum sy-uzeit '.TXT'
                                       INTO   gv_filename.
      ELSE.
        CONCATENATE p_sfile 'SAP_EAST_WBS_' sy-datum sy-uzeit '.TXT'
                                       INTO   gv_filename.
      ENDIF.
    ENDIF.
  ELSE.
    IF r_local IS NOT INITIAL.
      CONCATENATE p_file 'SE_ACTIVE_WBS.TXT' INTO gv_filename.
    ELSE.
      CONCATENATE p_sfile 'SE_ACTIVE_WBS.TXT' INTO gv_filename.
    ENDIF.
  ENDIF.
  CLEAR: gt_output,
         gt_output1,
         gs_output,
         gt_objectid.
  "Get data
  IF r_delta IS NOT INITIAL.
    PERFORM get_data_new.
  ELSE.
    PERFORM get_data_all.
  ENDIF.
  PERFORM download_data.
  IF gt_output[] IS INITIAL.
    SKIP 2.
    WRITE: / ' No WBS data to download, Empty file has been created.'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM toggle_functionality .
  LOOP AT SCREEN.
* Set the screen fields to display only
    IF  screen-group1 EQ gc_modif_id_dsp.
      screen-input = 0.
    ENDIF.
    IF r_local = 'X'.
      IF screen-group1 = 'LCL'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'LCL'.
        screen-input = 0.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.
ENDFORM.                    " TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_data .

  IF r_server IS NOT INITIAL.
    PERFORM download_server.
  ELSE.
    PERFORM download_pc.
  ENDIF.

ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SERVER
*&---------------------------------------------------------------------*
*       Download data to app server
*----------------------------------------------------------------------*
FORM download_server .
  DATA: lv_tabfield(1) TYPE x VALUE '09',
          lv_string TYPE string,
          lv_crlf TYPE c.

  lv_crlf = cl_abap_char_utilities=>cr_lf.

  OPEN DATASET gv_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open file for output.'.
  ELSE.

    LOOP AT gt_output INTO gs_output.
      "SHIFT gs_output-project_id LEFT DELETING LEADING '0'.
      CONCATENATE gs_output-project_id
                  gs_output-proj_name
                  gs_output-long_desc
                  gs_output-start_date
                  gs_output-end_date
                  gs_output-status
                  gs_output-proj_hier
                  gs_output-bukrs
      INTO lv_string
       SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
      TRANSFER lv_string TO gv_filename.
    ENDLOOP.
    CLOSE DATASET gv_filename.
    WRITE: / 'File is downloaded', gv_filename.
  ENDIF.
ENDFORM.                    " DOWNLOAD_SERVER
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_PC
*&---------------------------------------------------------------------*
*       Download file to PC.
*----------------------------------------------------------------------*
FORM download_pc .
  DATA: lv_filename TYPE string.

  MOVE gv_filename TO lv_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
      write_field_separator     = 'X'
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
    WRITE: / 'Successfully created at ', gv_filename.
  ENDIF.
ENDFORM.                    " DOWNLOAD_PC
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data_new .
  CONSTANTS: lc_i0001 TYPE jcds-stat VALUE 'I0001',
                lc_i0002 TYPE jcds-stat VALUE 'I0002',
                lc_i0045 TYPE jcds-stat VALUE 'I0045',
                lc_i0046 TYPE jcds-stat VALUE 'I0046',
                lc_i0064 TYPE jcds-stat VALUE 'I0064',
                lc_i0043 TYPE jcds-stat VALUE 'I0043',
                lc_i0076 TYPE jcds-stat VALUE 'I0076',
                lc_i0013 TYPE jcds-stat VALUE 'I0013'.

  DATA: lv_proj TYPE cdhdr-objectclas VALUE 'PROJ',
        lv_prps TYPE cdpos-tabname VALUE 'PRPS',
        lv_fname TYPE cdpos-fname VALUE 'POST1',
        lv_fname2 TYPE cdpos-fname VALUE 'BELKZ',
        lv_tabix TYPE sy-tabix,
        lv_find TYPE xfeld,
        lv_posid(24),
        lt_prps TYPE TABLE OF prps,
        ls_prps TYPE prps,
        ls_cdhdr TYPE cdhdr,
         lt_cdhdr TYPE TABLE OF cdhdr,
        lt_cdpos TYPE TABLE OF cdpos,
        ls_cdpos TYPE cdpos,
        lt_jcds TYPE TABLE OF jcds,
        ls_jcds TYPE jcds,
        lt_jest TYPE TABLE OF jest,
        ls_jest TYPE jest,
        lv_pspnr TYPE prps-pspnr.


  SELECT * FROM prps INTO TABLE lt_prps
    WHERE posid IN s_posid
      AND pbukr IN s_bukrs
      "AND BELKZ <> space
      AND ( ( erdat >= s_date-low AND erdat <= s_date-high ) OR
           ( aedat >= s_date-low AND aedat <= s_date-high ) ).
  IF lt_prps[] IS INITIAL.
    WRITE: / 'There is no WBS data for selection criteria for output.'.
    "STOP.
    EXIT.
  ENDIF.
  "build IT for object list
*  CLEAR s_object.
*  REFRESH s_object.
*  LOOP AT lt_prps INTO ls_prps.
*    s_object-sign = 'I'.
*    s_object-option = 'EQ'.
*    s_object-low = ls_prps-psphi.
*    APPEND s_object.
*  ENDLOOP.
  LOOP AT lt_prps INTO ls_prps.
    gs_objectid-objid = ls_prps-psphi.
    APPEND gs_objectid TO gt_objectid.
  ENDLOOP.
  IF gt_objectid[] IS NOT INITIAL.
    SELECT * FROM cdhdr INTO TABLE lt_cdhdr
      FOR ALL ENTRIES IN gt_objectid
      WHERE objectclas = lv_proj
        AND objectid = gt_objectid-objid
        AND ( udate >= s_date-low AND udate <= s_date-high ).
  ENDIF.
  IF lt_cdhdr[] IS NOT INITIAL.
    SELECT * FROM cdpos INTO TABLE lt_cdpos
      FOR ALL ENTRIES IN lt_cdhdr
      WHERE objectclas = lt_cdhdr-objectclas
        AND objectid = lt_cdhdr-objectid
        AND changenr = lt_cdhdr-changenr
        AND tabname  = lv_prps
        AND ( fname    = lv_fname OR
              fname    = lv_fname2 ).
  ENDIF.
  SELECT * FROM jcds INTO TABLE lt_jcds
  FOR ALL ENTRIES IN lt_prps
  WHERE objnr = lt_prps-objnr
    AND ( stat = lc_i0002 OR      "stat = lc_i0001 OR
          stat = lc_i0045 OR stat = lc_i0046 OR
          stat = lc_i0064 OR stat = lc_i0076 OR
          stat = lc_i0043 OR stat = lc_i0013 )
    AND inact = space
    "AND chind = 'U'
    AND ( udate >= s_date-low AND udate <= s_date-high ).
  SELECT * FROM jest INTO TABLE lt_jest
    FOR ALL ENTRIES IN lt_prps
    WHERE objnr = lt_prps-objnr
    AND ( stat = lc_i0046 OR stat = lc_i0001 OR
          stat = lc_i0064 OR stat = lc_i0002 OR
          stat = lc_i0045 OR stat = lc_i0076 OR
          stat = lc_i0043 OR stat = lc_i0013 )
    AND inact = space.
*  and chgnr = '001'.
  SORT lt_jcds BY objnr udate utime.
  SORT lt_jest BY objnr.
  LOOP AT lt_prps INTO ls_prps.
    CLEAR: gs_output,
           ls_cdhdr,
           ls_cdpos,
           ls_jest,
           lv_find,
           lv_tabix,
           lv_posid.
    "Changed data
    IF ls_prps-aedat IS NOT INITIAL.
      IF ls_prps-aedat <> ls_prps-erdat.      "created & change dates are different.
        CONTINUE.
      ENDIF.
    ENDIF.
    "if I0001 status (CRTD) then do not output
    READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_prps-objnr
                                             stat  = lc_i0001.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
*    lv_posid = ls_prps-posid.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        input  = ls_prps-posid
      IMPORTING
        output = lv_posid.
    "SHIFT lv_posid LEFT DELETING LEADING '0'.
    CONCATENATE 'WBS.E.' lv_posid INTO gs_output-project_id
                                       SEPARATED BY space.
*    REPLACE ALL OCCURRENCES OF '"' IN ls_prps-post1 WITH space.
    IF ls_prps-post1 IS INITIAL.
      gs_output-proj_name = 'No Description'.
    ELSE.
      gs_output-proj_name = ls_prps-post1.
    ENDIF.
    gs_output-long_desc = space.
    gs_output-start_date = '1900/01/01'.
    gs_output-end_date = '9999/12/31'.
    gs_output-proj_hier = 'Spectra East Projects'.
    gs_output-bukrs = ls_prps-pbukr.
    "--------get status
    "if inactive record is there then no need to check active status.
    CLEAR: lv_tabix, lv_find.
    READ TABLE lt_jest WITH KEY objnr = ls_prps-objnr
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_jest INTO ls_jest FROM lv_tabix.
      IF ls_jest-objnr <> ls_prps-objnr.
        EXIT.
      ENDIF.
      CHECK ls_jest-stat = lc_i0046 OR   "ls_jest-stat = lc_I0001
            ls_jest-stat = lc_i0064 OR ls_jest-stat = lc_i0076 OR
            ls_jest-stat = lc_i0043 OR ls_jest-stat = lc_i0013.
      gs_output-status = 'I'.
      lv_find = 'X'.
      EXIT.
    ENDLOOP.
    IF lv_find IS INITIAL.
      CLEAR: lv_tabix, lv_find.
      READ TABLE lt_jest WITH KEY objnr = ls_prps-objnr
                                  TRANSPORTING NO FIELDS.
      lv_tabix = sy-tabix.
      LOOP AT lt_jest INTO ls_jest FROM lv_tabix.
        IF ls_jest-objnr <> ls_prps-objnr.
          EXIT.
        ENDIF.
        "status RElL / TECO
        CHECK ls_jest-stat = lc_i0002 OR ls_jest-stat = lc_i0045.
        gs_output-status = 'A'.
        lv_find = 'X'.
        EXIT.
      ENDLOOP.
    ENDIF.
    "---------------
    CHECK lv_find IS NOT INITIAL.
    IF gs_output-status = 'A'.
      IF ls_prps-belkz IS INITIAL.
        gs_output-status = 'I'.
      ENDIF.
    ENDIF.
    "---------------
    APPEND gs_output TO gt_output.
  ENDLOOP.
  "-----Capture the changes
  "-----capture changes to POST1, BELKZ
  LOOP AT lt_cdpos INTO ls_cdpos.
    CLEAR: gs_output,
           ls_cdhdr,
           ls_prps,
           ls_jest,
           lv_find,
           lv_tabix,
           lv_posid.
    lv_pspnr = ls_cdpos-tabkey+3(8).
    READ TABLE lt_prps INTO ls_prps WITH KEY pspnr = lv_pspnr.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    IF ls_prps-aedat IS INITIAL.
      CONTINUE.
    ENDIF.
    IF ls_prps-aedat IS NOT INITIAL.
      IF ls_prps-aedat = ls_prps-erdat.      "created & change same dates
        CONTINUE.
      ENDIF.
    ENDIF.
    "if I0001 status (CRTD) then do not output
    READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_prps-objnr
                                             stat  = lc_i0001.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
*    lv_posid = ls_prps-posid.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        input  = ls_prps-posid
      IMPORTING
        output = lv_posid.
    "SHIFT lv_posid LEFT DELETING LEADING '0'.
    CONCATENATE 'WBS.E.' lv_posid INTO gs_output-project_id
                                       SEPARATED BY space.
    "check record already exist in output IT.
    READ TABLE gt_output WITH KEY project_id = gs_output-project_id
                          TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
*    REPLACE ALL OCCURRENCES OF '"' IN ls_prps-post1 WITH space.
    IF ls_prps-post1 IS INITIAL.
      gs_output-proj_name = 'No Description'.
    ELSE.
      gs_output-proj_name = ls_prps-post1.
    ENDIF.
    gs_output-long_desc = space.
    gs_output-start_date = '1900/01/01'.
    gs_output-end_date = '9999/12/31'.
    gs_output-proj_hier = 'Spectra East Projects'.
    gs_output-bukrs = ls_prps-pbukr.
    "--------get status
    "if inactive record is there then no need to check active status.
    CLEAR: lv_tabix, lv_find.
    READ TABLE lt_jest WITH KEY objnr = ls_prps-objnr
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_jest INTO ls_jest FROM lv_tabix.
      IF ls_jest-objnr <> ls_prps-objnr.
        EXIT.
      ENDIF.
      CHECK ls_jest-stat = lc_i0046 OR   "ls_jest-stat = lc_I0001
            ls_jest-stat = lc_i0064 OR ls_jest-stat = lc_i0076 OR
            ls_jest-stat = lc_i0043 OR ls_jest-stat = lc_i0013.
      gs_output-status = 'I'.
      lv_find = 'X'.
      EXIT.
    ENDLOOP.
    IF lv_find IS INITIAL.
      CLEAR: lv_tabix, lv_find.
      READ TABLE lt_jest WITH KEY objnr = ls_prps-objnr
                                  TRANSPORTING NO FIELDS.
      lv_tabix = sy-tabix.
      LOOP AT lt_jest INTO ls_jest FROM lv_tabix.
        IF ls_jest-objnr <> ls_prps-objnr.
          EXIT.
        ENDIF.
        "status RElL / TECO
        CHECK ls_jest-stat = lc_i0002 OR ls_jest-stat = lc_i0045.
        gs_output-status = 'A'.
        lv_find = 'X'.
        EXIT.
      ENDLOOP.
    ENDIF.
    "---------------
    CHECK lv_find IS NOT INITIAL.
    "---------------
    IF gs_output-status = 'A'.
      IF ls_prps-belkz IS INITIAL.
        gs_output-status = 'I'.
      ENDIF.
    ENDIF.
    "---------------
    APPEND gs_output TO gt_output.
  ENDLOOP.
  "-----Capture Status changes
  LOOP AT lt_prps INTO ls_prps.
    CLEAR: gs_output,
           ls_cdhdr,
           ls_cdpos,
           ls_jest,
           lv_find,
           lv_tabix,
           lv_posid.
    IF ls_prps-aedat IS INITIAL.
      CONTINUE.
    ENDIF.
    IF ls_prps-aedat IS NOT INITIAL.
      IF ls_prps-aedat = ls_prps-erdat.      "created & change same dates
        CONTINUE.
      ENDIF.
    ENDIF.
    "if I0001 status (CRTD) then do not output
    READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_prps-objnr
                                             stat  = lc_i0001.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
*   lv_posid = ls_prps-posid.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        input  = ls_prps-posid
      IMPORTING
        output = lv_posid.
    "SHIFT lv_posid LEFT DELETING LEADING '0'.
    CONCATENATE 'WBS.E.' lv_posid INTO gs_output-project_id
                                       SEPARATED BY space.
    READ TABLE gt_output WITH KEY project_id = gs_output-project_id
                          TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
*    REPLACE ALL OCCURRENCES OF '"' IN ls_prps-post1 WITH space.
    IF ls_prps-post1 IS INITIAL.
      gs_output-proj_name = 'No Description'.
    ELSE.
      gs_output-proj_name = ls_prps-post1.
    ENDIF.
    gs_output-long_desc = space.
    gs_output-start_date = '1900/01/01'.
    gs_output-end_date = '9999/12/31'.
    gs_output-proj_hier = 'Spectra East Projects'.
    gs_output-bukrs = ls_prps-pbukr.
    "Changed status data
    CLEAR: lv_tabix, lv_find.
    READ TABLE lt_jcds WITH KEY objnr = ls_prps-objnr
                               TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_jcds INTO ls_jcds FROM lv_tabix.
      IF ls_jcds-objnr <> ls_prps-objnr.
        EXIT.
      ENDIF.
      IF ls_jcds-stat = lc_i0046 OR       "ls_jcds-stat = lc_i0001 OR
         ls_jcds-stat = lc_i0064 OR ls_jcds-stat = lc_i0076 OR
         ls_jcds-stat = lc_i0043 OR ls_jcds-stat = lc_i0013.
        "gs_output-status = 'I'.
        lv_find = 'X'.
      ENDIF.
      IF ls_jcds-stat = lc_i0002 OR ls_jcds-stat = lc_i0045.
        "gs_output-status = 'A'.
        lv_find = 'X'.
      ENDIF.
    ENDLOOP.
    "if change record does not have status change then do not output.
    CHECK lv_find <> space.
    "--------get status
    "if inactive record is there then no need to check active status.
    CLEAR: lv_tabix, lv_find.
    READ TABLE lt_jest WITH KEY objnr = ls_prps-objnr
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_jest INTO ls_jest FROM lv_tabix.
      IF ls_jest-objnr <> ls_prps-objnr.
        EXIT.
      ENDIF.
      CHECK ls_jest-stat = lc_i0046 OR   "ls_jest-stat = lc_I0001
            ls_jest-stat = lc_i0064 OR ls_jest-stat = lc_i0076 OR
            ls_jest-stat = lc_i0043 OR ls_jest-stat = lc_i0013.
      gs_output-status = 'I'.
      lv_find = 'X'.
      EXIT.
    ENDLOOP.
    IF lv_find IS INITIAL.
      CLEAR: lv_tabix, lv_find.
      READ TABLE lt_jest WITH KEY objnr = ls_prps-objnr
                                  TRANSPORTING NO FIELDS.
      lv_tabix = sy-tabix.
      LOOP AT lt_jest INTO ls_jest FROM lv_tabix.
        IF ls_jest-objnr <> ls_prps-objnr.
          EXIT.
        ENDIF.
        "status RElL / TECO
        CHECK ls_jest-stat = lc_i0002 OR ls_jest-stat = lc_i0045.
        gs_output-status = 'A'.
        lv_find = 'X'.
        EXIT.
      ENDLOOP.
    ENDIF.
    "---------------
    CHECK lv_find IS NOT INITIAL.
    "---------------
    IF gs_output-status = 'A'.
      IF ls_prps-belkz IS INITIAL.
        gs_output-status = 'I'.
      ENDIF.
    ENDIF.
    "---------------
    APPEND gs_output TO gt_output.
  ENDLOOP.
ENDFORM.                    " GET_DATA_NEW
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_ALL
*&---------------------------------------------------------------------*
*       Get all active WBS
*----------------------------------------------------------------------*
FORM get_data_all .
  CONSTANTS: lc_i0001 TYPE jcds-stat VALUE 'I0001',
                  lc_i0002 TYPE jcds-stat VALUE 'I0002',
                  lc_i0045 TYPE jcds-stat VALUE 'I0045',
                  lc_i0046 TYPE jcds-stat VALUE 'I0046',
                  lc_i0064 TYPE jcds-stat VALUE 'I0064',
                  lc_i0043 TYPE jcds-stat VALUE 'I0043',
                  lc_i0076 TYPE jcds-stat VALUE 'I0076',
                  lc_i0013 TYPE jcds-stat VALUE 'I0013'.

  DATA: lv_tabix TYPE sy-tabix,
        lv_find TYPE xfeld,
        lv_posid(24),
        lt_prps TYPE TABLE OF prps,
        ls_prps TYPE prps,
        lt_jest TYPE TABLE OF jest,
        ls_jest TYPE jest,
        lv_pspnr TYPE prps-pspnr.


  SELECT * FROM prps INTO TABLE lt_prps
    WHERE posid IN s_posid
      AND pbukr IN s_bukrs
      AND belkz <> space.

  IF lt_prps[] IS INITIAL.
    WRITE: / 'There is no WBS data for selection criteria for output.'.
    "STOP.
    EXIT.
  ENDIF.
  SELECT * FROM jest INTO TABLE lt_jest
     FOR ALL ENTRIES IN lt_prps
     WHERE objnr = lt_prps-objnr
     AND ( stat = lc_i0046 OR stat = lc_i0001 OR
           stat = lc_i0064 OR stat = lc_i0002 OR
           stat = lc_i0045 OR stat = lc_i0076 OR
           stat = lc_i0043 OR stat = lc_i0013 )
     AND inact = space.
*  and chgnr = '001'.
  SORT lt_jest BY objnr.
  LOOP AT lt_prps INTO ls_prps.
    CLEAR: gs_output,
           ls_jest,
           lv_find,
           lv_tabix,
           lv_posid.
    "if I0001 status (CRTD) then do not output
    READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_prps-objnr
                                             stat  = lc_i0001.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
*    lv_posid = ls_prps-posid.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
      EXPORTING
        input  = ls_prps-posid
      IMPORTING
        output = lv_posid.
    "SHIFT lv_posid LEFT DELETING LEADING '0'.
    CONCATENATE 'WBS.E.' lv_posid INTO gs_output-project_id
                                       SEPARATED BY space.
*    REPLACE ALL OCCURRENCES OF '"' IN ls_prps-post1 WITH space.
    IF ls_prps-post1 IS INITIAL.
      gs_output-proj_name = 'No Description'.
    ELSE.
      gs_output-proj_name = ls_prps-post1.
    ENDIF.
    gs_output-long_desc = space.
    gs_output-start_date = '1900/01/01'.
    gs_output-end_date = '9999/12/31'.
    gs_output-proj_hier = 'Spectra East Projects'.
    gs_output-bukrs = ls_prps-pbukr.
    "--------get status
    "if inactive record is there then no need to check active status.
    CLEAR: lv_tabix, lv_find, gs_output-status.
    READ TABLE lt_jest WITH KEY objnr = ls_prps-objnr
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_jest INTO ls_jest FROM lv_tabix.
      IF ls_jest-objnr <> ls_prps-objnr.
        EXIT.
      ENDIF.
      CHECK ls_jest-stat = lc_i0046 OR   "ls_jest-stat = lc_I0001
            ls_jest-stat = lc_i0064 OR ls_jest-stat = lc_i0076 OR
            ls_jest-stat = lc_i0043 OR ls_jest-stat = lc_i0013.
      lv_find = 'X'.
    ENDLOOP.
    CHECK lv_find IS INITIAL.
    CLEAR: lv_tabix, lv_find.
    READ TABLE lt_jest WITH KEY objnr = ls_prps-objnr
                                TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_jest INTO ls_jest FROM lv_tabix.
      IF ls_jest-objnr <> ls_prps-objnr.
        EXIT.
      ENDIF.
      "status RElL / TECO
      CHECK ls_jest-stat = lc_i0002 OR ls_jest-stat = lc_i0045.
      gs_output-status = 'A'.
      lv_find = 'X'.
      EXIT.
    ENDLOOP.
    "---------------
    CHECK lv_find IS NOT INITIAL.
    APPEND gs_output TO gt_output.
  ENDLOOP.

ENDFORM.                    " GET_DATA_ALL
