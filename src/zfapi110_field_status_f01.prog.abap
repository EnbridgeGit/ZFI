*&---------------------------------------------------------------------*
*&  Include           ZFAPI110_FIELD_STATUS_F01
*&---------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_FILE_PATH
*&---------------------------------------------------------------------*
FORM f_get_file_path  CHANGING cv_file1 TYPE any.

  CONSTANTS: lc_path1  TYPE filepath-pathintern VALUE 'ZFAPI110_FIELD_STATUS'.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = lc_path1
    IMPORTING
      file_name        = cv_file1
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Default File Name

  CONCATENATE p_erpid '_FIELD_STATUS_' sy-datum '_' sy-uzeit '.CSV' INTO p_file2.

ENDFORM.                    " F_GET_FILE_PATH
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH
*&---------------------------------------------------------------------*
FORM f_get_f4_help_file_path.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = p_path2
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_GET_F4_HELP_FILE_PATH
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH1
*&---------------------------------------------------------------------*
FORM f_get_f4_help_file_path1.

  DATA: lv_repid TYPE sy-repid,
        lv_dynnr TYPE sy-dynnr,
        lv_file TYPE rlgrap-filename.

  lv_repid = sy-repid.
  lv_dynnr = sy-dynnr.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = lv_repid
      dynpro_number = lv_dynnr
    CHANGING
      file_name     = lv_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE lv_file TO p_path1.

ENDFORM.                    " F_GET_F4_HELP_FILE_PATH1
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_data.

  CLEAR    gt_final[].

  CLEAR    gt_t004f[].

  SELECT   *
    FROM   t004f
    INTO   TABLE gt_t004f
   WHERE   bukrs IN s_bukrs
     AND   fstag IN s_fstag.
  IF     ( sy-subrc EQ 0 ).

    CLEAR                               gs_t004f.
    LOOP AT  gt_t004f              INTO gs_t004f.

      CLEAR                             gs_final.
      MOVE     p_erpid               TO gs_final-erpid.
      MOVE     gs_t004f-fstag        TO gs_final-fstag.
      MOVE     gs_t004f-bukrs        TO gs_final-bukrs.

* Set the Cost Center Indicator
      IF     ( gs_t004f-faus1+09(01) EQ '-' ) .
        MOVE   'S'                   TO gs_final-c_center.
      ELSEIF ( gs_t004f-faus1+09(01) EQ '+' ) .
        MOVE   'R'                   TO gs_final-c_center.
      ELSE.
        MOVE   'O'                   TO gs_final-c_center.
      ENDIF.

* Set the Internal Order Indicator
      IF     ( gs_t004f-faus1+10(01) EQ '-' ) .
        MOVE   'S'                   TO gs_final-i_order.
      ELSEIF ( gs_t004f-faus1+10(01) EQ '+' ) .
        MOVE   'R'                   TO gs_final-i_order.
      ELSE.
        MOVE   'O'                   TO gs_final-i_order.
      ENDIF.

* Set the WBS Element Indicator
      IF     ( gs_t004f-faus1+11(01) EQ '-' ).
        MOVE   'S'                   TO gs_final-wbs_element.
      ELSEIF ( gs_t004f-faus1+11(01) EQ '+' ) .
        MOVE   'R'                   TO gs_final-wbs_element.
      ELSE.
        MOVE   'O'                   TO gs_final-wbs_element.
      ENDIF.

* Set the Network Indicator
      IF     ( gs_t004f-faus1+18(01) EQ '-' ) .
        MOVE   'S'                   TO gs_final-network.
      ELSEIF ( gs_t004f-faus1+18(01) EQ '+' ) .
        MOVE   'R'                   TO gs_final-network.
      ELSE.
        MOVE   'O'                   TO gs_final-network.
      ENDIF.

* Set the Activity Indicator
      IF     ( gs_t004f-faus2+07(01) EQ '-' ) .
        MOVE   'S'                   TO gs_final-activity.
      ELSEIF ( gs_t004f-faus2+07(01) EQ '+' ) .
        MOVE   'R'                   TO gs_final-activity.
      ELSE.
        MOVE   'O'                   TO gs_final-activity.
      ENDIF.

      APPEND   gs_final              TO gt_final.

      CLEAR    gs_t004f.
    ENDLOOP.

  ENDIF.

ENDFORM.                    "f_get_data
*eject
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_TO_PRESENTATION
*&---------------------------------------------------------------------*
FORM f_write_to_presentation.

  DATA:    lv_file   TYPE STRING,
           lv_type   TYPE char1,
           lv_index  TYPE syindex.

  CONSTANTS: lc_sep TYPE c LENGTH 2 VALUE '|~'.

  FIELD-SYMBOLS: <fs_fld> TYPE ANY.

  CLEAR    gt_data_tab[].

  DESCRIBE FIELD gs_final TYPE lv_type COMPONENTS lv_index.


   CLEAR         gs_data_tab.
   CONCATENATE   text-f01
                 text-f02
                 text-f03
                 text-f04
                 text-f05
                 text-f06
                 text-f07
                 text-f08
            INTO gs_data_tab
    SEPARATED BY lc_sep.
   APPEND gs_data_tab to gt_data_tab.

  CLEAR                                     gs_final.
  LOOP AT  gt_final                    INTO gs_final.

    CLEAR                                   gs_data_tab.

    DO lv_index TIMES.

      ASSIGN COMPONENT sy-index OF STRUCTURE gs_final TO <fs_fld>.
      IF     ( sy-index EQ 1 ).
        MOVE   <fs_fld>                  TO gs_data_tab.
      ELSE.
        CONCATENATE                         gs_data_tab lc_sep
                                            <fs_fld>
                                       INTO gs_data_tab.
      ENDIF.
    ENDDO.

    APPEND   gs_data_tab                 TO gt_data_tab.

    CLEAR  gs_final.
  ENDLOOP.

  MOVE   p_path1   TO lv_file.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                        = lv_file
*   IMPORTING
    TABLES
      data_tab                        = gt_data_tab
    EXCEPTIONS
      file_write_error                = 1
      no_batch                        = 2
      gui_refuse_filetransfer         = 3
      invalid_type                    = 4
      no_authority                    = 5
      unknown_error                   = 6
      header_not_allowed              = 7
      separator_not_allowed           = 8
      filesize_not_allowed            = 9
      header_too_long                 = 10
      dp_error_create                 = 11
      dp_error_send                   = 12
      dp_error_write                  = 13
      unknown_dp_error                = 14
      access_denied                   = 15
      dp_out_of_memory                = 16
      disk_full                       = 17
      dp_timeout                      = 18
      file_not_found                  = 19
      dataprovider_exception          = 20
      control_flush_error             = 21
      OTHERS                          = 22.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
**--> Log File
    CLEAR                         gv_cn_recs.
    DESCRIBE TABLE gt_final LINES gv_cn_recs.
    PERFORM f_write_log.
  ENDIF.

ENDFORM.                    " F_WRITE_TO_PRESENTATION
*eject
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_TO_APPL_SERVER
*&---------------------------------------------------------------------*
FORM f_write_to_appl_server.

  DATA : lv_string  TYPE c LENGTH  1000,
         lv_message TYPE string,
         lv_path    TYPE string.

  CONSTANTS : lc_sep TYPE c LENGTH 2 VALUE '|~',
              lc_und TYPE c VALUE '_'.

  CLEAR                            lv_path.
  CONCATENATE p_erpid p_file2 INTO lv_path.
  CONCATENATE p_path2 lv_path INTO lv_path.

  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_message.
  IF sy-subrc IS INITIAL.

*   CLEAR         lv_string.
*   CONCATENATE   text-c01     "  ERP ID
*                 text-c02     "  Field Status Group
*                 text-c03     "  Cost Center Ind.
*                 text-c04     "  Internal Order Ind.
*                 text-c05     "  WBS Element Ind.
*                 text-c06     "  Network Ind.
*                 text-c07     "  Activity Ind.
*            INTO lv_string
*    SEPARATED BY lc_sep.
*   TRANSFER      lv_string TO lv_path.
*   ADD           1         TO gv_cn_recs.

   CLEAR         lv_string.
   CONCATENATE   text-f01
                 text-f02
                 text-f03
                 text-f04
                 text-f05
                 text-f06
                 text-f07
                 text-f08
            INTO lv_string
    SEPARATED BY lc_sep.
   TRANSFER      lv_string TO lv_path.

    CLEAR                 gs_final.
    LOOP AT gt_final INTO gs_final.

      CLEAR       lv_string.
      CONCATENATE gs_final-erpid
                  gs_final-fstag
                  gs_final-bukrs
                  gs_final-c_center
                  gs_final-i_order
                  gs_final-wbs_element
                  gs_final-network
                  gs_final-activity
             INTO lv_string
     SEPARATED BY lc_sep.
      TRANSFER    lv_string TO lv_path.
      ADD         1         TO gv_cn_recs.

      CLEAR  gs_final.
    ENDLOOP.

    CLOSE DATASET lv_path.

    PERFORM f_write_log.

  ELSE.

    WRITE: text-005, lv_message.

  ENDIF.

ENDFORM.                    " F_WRITE_TO_APPL_SERVER
*eject
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_LOG
*&---------------------------------------------------------------------*
FORM f_write_log.

  DATA : lv_string TYPE c LENGTH  1000,
         lv_records TYPE string.

  CLEAR : lv_string.

  lv_records = gv_cn_recs.
  CONCATENATE text-009 lv_records INTO lv_string.

  SKIP  2.
  WRITE lv_string.
  CLEAR lv_string.

ENDFORM.                    " F_WRITE_LOG
