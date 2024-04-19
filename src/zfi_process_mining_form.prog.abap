*&---------------------------------------------------------------------*
*&  Include           ZFI_PROCESS_MINING_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .
  CLEAR: gt_cdhdr[], gt_cdpos[], gt_final[].

  SELECT objectclas
         objectid
         changenr
         udate
    FROM cdhdr
INTO TABLE gt_cdhdr
   WHERE objectclas IN s_objcls
     AND objectid   IN s_objid
     AND changenr   IN s_chgnr
     AND udate      IN s_udate.

  IF sy-subrc = 0.
    SORT gt_cdhdr BY objectclas
                     objectid
                     changenr.
    DELETE ADJACENT DUPLICATES FROM gt_cdhdr
                          COMPARING objectclas
                                    objectid
                                    changenr.
  ENDIF.

  IF gt_cdhdr IS NOT INITIAL.
    SELECT objectclas
           objectid
           changenr
           tabname
           tabkey
           fname
           chngind
           value_new
           value_old
      FROM cdpos
INTO TABLE gt_cdpos
FOR ALL ENTRIES IN gt_cdhdr
     WHERE objectclas = gt_cdhdr-objectclas
       AND objectid   = gt_cdhdr-objectid
       AND changenr   = gt_cdhdr-changenr.
    IF sy-subrc = 0.
      SORT gt_cdpos BY objectclas
                       objectid
                       changenr.
    ENDIF.
  ENDIF.

  CLEAR: gs_cdhdr, gs_cdpos, gs_final.
  LOOP AT gt_cdpos INTO gs_cdpos.

    gs_final-objectclas = gs_cdpos-objectclas.
    gs_final-objectid   = gs_cdpos-objectid.
    gs_final-changenr   = gs_cdpos-changenr.
    gs_final-tabname    = gs_cdpos-tabname.
    gs_final-tabkey     = gs_cdpos-tabkey.
    gs_final-fname      = gs_cdpos-fname.
    gs_final-chngind    = gs_cdpos-chngind.
    gs_final-value_new  = gs_cdpos-value_new.
    gs_final-value_old  = gs_cdpos-value_old.


    READ TABLE gt_cdhdr INTO gs_cdhdr WITH KEY objectclas = gs_cdpos-objectclas
                                               objectid   = gs_cdpos-objectid
                                               changenr   = gs_cdpos-changenr
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      gs_final-udate = gs_cdhdr-udate.
    ENDIF.
    APPEND gs_final TO gt_final.
    CLEAR: gs_cdhdr, gs_cdpos, gs_final.
  ENDLOOP.
ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_DATA_APP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_upload_data_app .
  DATA: lv_string  TYPE string,
        lv_string1 TYPE string.

  CONSTANTS: lc_tab TYPE char1 VALUE cl_abap_char_utilities=>horizontal_tab,
             lc_under TYPE char1 VALUE '_',
             lc_txt   TYPE char4 VALUE '.TXT',
             lc_date  TYPE char8 VALUE 'DDMMYYYY',
             lc_time  TYPE char6 VALUE 'HHMMSS'.

  IF p_app CS lc_date.
    REPLACE lc_date IN p_app WITH sy-datum.
  ENDIF.

  IF p_app CS lc_time.
    REPLACE lc_time IN p_app WITH sy-uzeit.
  ENDIF.


  gv_path = p_app.

  CONDENSE gv_file_count NO-GAPS.
  CONCATENATE lc_under
              gv_file_count
              lc_txt
         INTO gv_file_ext.
  TRANSLATE gv_file_ext TO LOWER CASE.
  TRANSLATE gv_file_ext_last TO LOWER CASE.
  REPLACE ALL OCCURRENCES OF gv_file_ext_last IN gv_path WITH gv_file_ext.
  gv_file_ext_last = gv_file_ext.
  ADD 1 TO    gv_file_count.

  OPEN DATASET gv_path FOR OUTPUT
                      IN TEXT MODE
                      ENCODING DEFAULT.
  IF sy-subrc <> 0.
    MESSAGE text-001 TYPE 'E'.
  ENDIF.

  CONCATENATE text-002
              text-003
              text-004
              text-005
              text-006
              text-007
              text-008
              text-009
              text-010
              text-011
         INTO lv_string
 SEPARATED BY lc_tab.
  TRANSFER lv_string TO gv_path.
  CLEAR: lv_string.

  LOOP AT gt_final INTO gs_final.
    CONCATENATE gs_final-udate
                gs_final-objectclas
                gs_final-objectid
                gs_final-changenr
                gs_final-tabname
                gs_final-tabkey
                gs_final-fname
                gs_final-chngind
                gs_final-value_new
                gs_final-value_old
           INTO lv_string
   SEPARATED BY lc_tab.

    IF gv_num_records < p_rec.
      TRANSFER lv_string TO gv_path.
      ADD 1 TO gv_num_records.
      CLEAR lv_string.
    ELSE.
      CLOSE DATASET gv_path.
      WRITE:/ gv_path.
      WRITE:/ gv_num_records.
      gv_num_records = 0.
      CONDENSE gv_file_count NO-GAPS.
      CONCATENATE lc_under
                  gv_file_count
                  lc_txt
             INTO gv_file_ext.
      TRANSLATE gv_file_ext TO LOWER CASE.
      TRANSLATE gv_file_ext_last TO LOWER CASE.
      REPLACE ALL OCCURRENCES OF gv_file_ext_last IN gv_path WITH gv_file_ext.
      gv_file_ext_last = gv_file_ext.
      ADD 1 TO    gv_file_count.

      OPEN DATASET gv_path FOR OUTPUT
                     IN TEXT MODE
                     ENCODING DEFAULT.

      IF sy-subrc <> 0.
        MESSAGE text-001 TYPE 'E'.
      ENDIF.

      CONCATENATE text-002
                  text-003
                  text-004
                  text-005
                  text-006
                  text-007
                  text-008
                  text-009
                  text-010
                  text-011
             INTO lv_string1
     SEPARATED BY lc_tab.
      TRANSFER lv_string1 TO gv_path.
      CLEAR: lv_string1.

      TRANSFER lv_string TO gv_path.
      ADD 1 TO gv_num_records.
      CLEAR lv_string.
    ENDIF.
  ENDLOOP.

  IF gv_num_records < p_rec.
    CLOSE DATASET gv_path.
    WRITE:/ gv_path.
    WRITE:/ gv_num_records.
    gv_num_records = 0.
  ENDIF.
ENDFORM.                    " F_UPLOAD_DATA_APP
*&---------------------------------------------------------------------*
*&      Form  F_INITIALIZE_SS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_initialize_ss .
  IF p_app CS gc_sysid.
    REPLACE gc_sysid IN p_app WITH sy-sysid.
  ENDIF.
ENDFORM.                    " F_INITIALIZE_SS
*&---------------------------------------------------------------------*
*&      Form  F_SS_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ss_output .

  IF p_pre_r = 'X'.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'PRE'.
          screen-input = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        WHEN 'APP'.
          screen-input = 0.
          screen-invisible = 1.
          MODIFY SCREEN.

        WHEN 'REC'.
          screen-input = 0.
          screen-invisible = 1.
          MODIFY SCREEN.

      ENDCASE.
    ENDLOOP.
  ELSE.
    IF p_app_r = 'X'.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'PRE'.
            screen-input = 0.
            screen-invisible = 1.
            MODIFY SCREEN.
          WHEN 'APP'.
            screen-input = 1.
            screen-invisible = 0.
            MODIFY SCREEN.

          WHEN 'REC'.
            screen-input = 1.
            screen-invisible = 0.
            MODIFY SCREEN.

        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_SS_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_DATA_PRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_UPLOAD_DATA_PRE .
  DATA: lv_path TYPE string.

  IF p_pres IS NOT INITIAL.
    CLEAR: lv_path, gs_final.
    lv_path = p_pres.

    gs_final-udate      = text-002.
    gs_final-changenr   = text-003.
    gs_final-chngind    = text-004.
    gs_final-fname      = text-005.
    gs_final-objectclas = text-008.
    gs_final-objectid   = text-009.
    gs_final-tabkey     = text-007.
    gs_final-tabname    = text-006.
    gs_final-value_new  = text-010.
    gs_final-value_old  = text-011.
    INSERT  gs_final INTO gt_final INDEX 1.
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = lv_path
        filetype                = 'ASC'
        write_field_separator   = 'X'
      CHANGING
        data_tab                = gt_final
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
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    MESSAGE text-012 TYPE 'S' DISPLAY LIKE 'E'.

  ENDIF.

ENDFORM.                    " F_UPLOAD_DATA_PRE
