*&---------------------------------------------------------------------*
*&  Include           ZFAPI116_USER_EXTRACT_F01
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
*& Program Name       :  ZFAPI116_USER_EXTRACT                         *
*& Include            :  ZFAPI116_USER_EXTRACT_F01                     *
*& Author             :  Kalinga Keshari Rout / Paul Karunakar         *
*& Creation Date      :  08-Mar-2018                                   *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  User data from each of the three SAP          *
*&                       instances will be extracted in a delimited    *
*&                       file and sent to IAP.                         *
*&                                                                     *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 08-Mar-2018  KROUT       D30K928695, CHG0105965  Initial development *
*                          D30K928697, D30K928840, D30K929003,         *
*                          D30K929075, D30K929081, D30K929083          *
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_file_path  CHANGING cv_file1 TYPE ANY .

  CONSTANTS: lc_path1  TYPE filepath-pathintern VALUE 'ZFAPI116_USER_EXTRACT'.

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

  CONCATENATE p_erpid '_USER_EXTRACT_'  sy-datum '_' sy-uzeit '.CSV' INTO p_file2.

ENDFORM.                    " F_GET_FILE_PATH
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_f4_help_file_path .

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
*       text
*----------------------------------------------------------------------*
FORM f_get_f4_help_file_path1 .

  DATA:    lv_repid TYPE sy-repid,
           lv_dynnr TYPE sy-dynnr,
           lv_file  TYPE rlgrap-filename.

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
*&      Form  F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_display_data .

  DATA:    lv_file    TYPE string.

  DATA: lt_tab_data       TYPE STANDARD TABLE OF string,
        ls_tab_data       TYPE string,
        ls_data           TYPE gty_final.

  CONSTANTS : lc_sep TYPE c LENGTH 2 VALUE '|~'. " for column separator

  CONCATENATE          text-f01            text-f02
                       text-f03            text-f04
                       text-f05            text-f06
                       text-f07            text-f08
                       text-f09            text-f10
                       text-f11            text-f12
                       text-f13            text-f14
                       text-f15            text-f16
                INTO ls_tab_data
                  SEPARATED BY lc_sep.
  APPEND ls_tab_data TO lt_tab_data.

  CLEAR ls_data.
  LOOP AT  gt_final INTO  ls_data.
    CLEAR ls_tab_data.
    zcl_iap_interface_util=>add_pipes( EXPORTING im_rec = ls_data
    IMPORTING ex_outrec = ls_tab_data ).

    APPEND ls_tab_data TO lt_tab_data.
  ENDLOOP.

  MOVE     p_path1 TO lv_file.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                        = lv_file
*   IMPORTING
    TABLES
*      data_tab                        = gt_final
      data_tab                        = lt_tab_data
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

  ENDIF.

ENDFORM.                    " F_DISPLAY_DATA
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_get_data .

  DATA:    lt_valid_doc_types          TYPE STANDARD TABLE
                                         OF gty_valid_doc_types,
           ls_valid_doc_types          TYPE gty_valid_doc_types.

  DATA:    lv_erpid                    TYPE zparamkey,
           lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex .

  CONSTANTS:
           lc_paramtype TYPE zparamtype      VALUE 'OUTBOUND_INTERFACE',
           lc_subtype   TYPE zparamsubtype   VALUE 'I_P2C_AP_116',
           lc_full_extr TYPE zparamkey       VALUE 'FULL_EXTRACT' ,
           lc_resp      TYPE zparamkey       VALUE 'RESPONSIBILITY',
           lc_doct      TYPE zparamkey       VALUE 'DOCUMENT_TYPE',
           lc_ap        TYPE z_workflow_type VALUE 'AP',
           lc_blocksize TYPE syindex         VALUE 30.

*eject
* Part-I.

  CLEAR    gv_count_recs.

  CLEAR               lv_erpid.
  MOVE     p_erpid TO lv_erpid.

  SELECT   *
    INTO   TABLE gt_xparam
    FROM   zfit_xparam
   WHERE   paramtype = lc_paramtype
     AND   subtype   = lc_subtype
     AND   key1      = lv_erpid .
  IF sy-subrc = 0 .

    READ TABLE gt_xparam  INTO gs_xparam WITH KEY key2 = lc_full_extr .
    IF sy-subrc = 0 AND gs_xparam-value1 = gc_x .

      gv_full_extract = gc_x .

***** Refresh custom table ZAPT_IAP_USER (delete all entries from table) and Commit Work

      IF     ( cb_test IS INITIAL ).
        DELETE FROM zapt_iap_user WHERE uname > SPACE.
        IF ( sy-subrc EQ 0 ).
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.

    ENDIF .

  ENDIF.

*eject
* RFC Destinations
  CLEAR    gv_rfcdest_hr .

  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      IMP_PARAMTYPE = gc_rfcname_hr
    IMPORTING
      EXP_RFCDEST   = gv_rfcdest_hr.

  IF     ( sy-subrc NE 0 ).
    CLEAR  gv_rfcdest_hr.
  ENDIF.

  CLEAR    gv_rfcdest_us .

  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      IMP_PARAMTYPE = gc_rfcname_us
    IMPORTING
      EXP_RFCDEST   = gv_rfcdest_us.

  IF     ( sy-subrc NE 0 ).
    CLEAR  gv_rfcdest_us.
  ENDIF.

*eject
* Build the list of responsibilities
  CLEAR                   gs_xparam.
  LOOP AT  gt_xparam INTO gs_xparam.
    IF       ( gs_xparam-key2      EQ lc_resp ).
      CLEAR                           gs_xparam_resp.
      MOVE     gs_xparam-paramtype TO gs_xparam_resp-paramtype.
      MOVE     gs_xparam-subtype   TO gs_xparam_resp-subtype.
      MOVE     gs_xparam-key1      TO gs_xparam_resp-key1.
      MOVE     gs_xparam-key2      TO gs_xparam_resp-key2.
      MOVE     gs_xparam-key3      TO gs_xparam_resp-key3.
      MOVE     gs_xparam-key4      TO gs_xparam_resp-key4.
      MOVE     gs_xparam-key5      TO gs_xparam_resp-key5.
      MOVE     gs_xparam-value1    TO gs_xparam_resp-value1.
      MOVE     gs_xparam-value2    TO gs_xparam_resp-value2.
      APPEND   gs_xparam_resp      TO gt_xparam_resp.
    ENDIF.
    CLEAR gs_xparam.
  ENDLOOP.

  SORT     gt_xparam_resp ASCENDING BY value1.
  DELETE   ADJACENT DUPLICATES    FROM gt_xparam_resp
                             COMPARING value1.

  IF     ( gt_xparam_resp[] IS NOT INITIAL ).

* Select the user names that are assigned the given responsibilities
    SELECT   uname
      INTO   TABLE gt_user_names
      FROM   agr_users FOR ALL ENTRIES IN gt_xparam_resp
     WHERE   agr_name  = gt_xparam_resp-value1
       AND   uname    IN s_uname.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_user_names[].
    ENDIF.

  ENDIF.

* Select and append the user names that have already been extracted to IAP
  SELECT   uname
    APPENDING TABLE gt_user_names
    FROM   zapt_iap_user
   WHERE   uname IN s_uname.

  SORT     gt_user_names ASCENDING BY uname.
  DELETE   ADJACENT DUPLICATES   FROM gt_user_names
                            COMPARING uname.

*eject
* Select the valid document types
  CLEAR    lt_valid_doc_types[].

  SELECT   blart
    INTO   TABLE lt_valid_doc_types
    FROM   zfit_valid_blart
   WHERE   workflow EQ lc_ap.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lt_valid_doc_types[].
  ENDIF.

  LOOP AT gt_xparam INTO gs_xparam.
    IF       ( gs_xparam-key2      EQ lc_doct ).
      CLEAR                           ls_valid_doc_types.
      MOVE     gs_xparam-value1    TO ls_valid_doc_types-blart.
      APPEND   ls_valid_doc_types  TO lt_valid_doc_types.
    ENDIF.
    CLEAR gs_xparam.
  ENDLOOP.

  IF     ( lt_valid_doc_types[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    gt_valid_doc_types[].

  SELECT   blart  brgru
    INTO   TABLE gt_valid_doc_types
    FROM   t003 FOR ALL ENTRIES IN lt_valid_doc_types
   WHERE   blart = lt_valid_doc_types-blart.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_valid_doc_types[].
  ENDIF.

  SORT     gt_valid_doc_types ASCENDING BY blart.
  DELETE   ADJACENT DUPLICATES        FROM gt_valid_doc_types
                                 COMPARING blart.

  IF   ( ( rb_appl   IS NOT INITIAL ) AND
         ( cb_test IS     INITIAL )     ).
    PERFORM  f_open_dataset.
  ENDIF.

*eject
* Part-II.

* Process the user names in batches
  DO.

* Calculate the low and high indices for the batch of user names
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch of user names
    CLEAR           gt_user_names_p[].
    APPEND LINES OF gt_user_names
               FROM lv_index_lo
                 TO lv_index_hi
                 TO gt_user_names_p.

    IF     ( gt_user_names_p[] IS INITIAL ).
      EXIT.
    ENDIF.

    CLEAR    gt_user_data[].
    CLEAR    gt_usr02[].
    CLEAR    gt_usr21[].
    CLEAR    gt_adrp[].
    CLEAR    gt_adrc[].
    CLEAR    gt_adr6[].
    CLEAR    gt_agr_users[].
    CLEAR    gt_doa_data[].
    CLEAR    gt_doa_data_p[].
    CLEAR    gt_iap_user_current[].
    CLEAR    gt_iap_user_history[].
    CLEAR    gt_iap_user_changes[].

    PERFORM  f_part_iii.
    PERFORM  f_part_iv.
    PERFORM  f_part_v.
    PERFORM  f_part_vi.
    PERFORM  f_part_vii.

  ENDDO.

  IF   ( ( rb_appl IS NOT INITIAL ) AND
         ( cb_test IS     INITIAL )     ).
    CLOSE    DATASET gv_filepath.
  ENDIF.

*eject
  IF   ( ( rb_appl IS NOT INITIAL ) AND
         ( cb_test IS NOT INITIAL )     ).
    gv_records = gv_count_recs.
    CONCATENATE  text-016 gv_records INTO gv_records.
    SKIP 2.
    WRITE: / gv_records.
    SKIP 1.
    WRITE: / text-015.
  ELSE.
    gv_records = gv_count_recs.
    CONCATENATE  text-009 gv_records INTO gv_records.
    SKIP 2.
    WRITE: / gv_records.
  ENDIF.

ENDFORM.                    " F_GET_DATA
*eject
*&---------------------------------------------------------------------*
*&      Form  F_PART_III
*&---------------------------------------------------------------------*
*       Retrieve user responsibility (role) data
*----------------------------------------------------------------------*
FORM f_part_iii .

  DATA:    lv_tabix TYPE sytabix.

* Part-III.

  PERFORM  f_get_personnel_data. "HR System

  PERFORM  f_get_user_details.

  CLEAR                      gs_user_data.
  LOOP AT  gt_user_data INTO gs_user_data.
    lv_tabix = sy-tabix.

    CLEAR                    gs_usr02.
    READ TABLE gt_usr02 INTO gs_usr02 WITH KEY uname = gs_user_data-uname.
    IF ( sy-subrc EQ 0 ).

      IF   ( ( gs_usr02-gltgv               LE sy-datum ) AND
             ( gs_usr02-gltgb               GE sy-datum )     ).
        IF   ( gs_user_data-status_user     EQ gc_i ).
          SKIP     1.
          WRITE: / text-e11, gs_user_data-pernr, '/', gs_usr02-uname.
          SKIP     1.
        ENDIF.
      ELSE.
        IF       ( gs_user_data-status_user EQ gc_a ).
          CLEAR                                gs_user_data-status_user.
          MOVE     gc_i                     TO gs_user_data-status_user.
          MODIFY   gt_user_data           FROM gs_user_data
                                         INDEX lv_tabix
                                  TRANSPORTING status_user.
        ENDIF.
      ENDIF.

    ELSE.

      IF         ( gs_user_data-status_user EQ gc_a ).
        CLEAR                                  gs_user_data-status_user.
        MOVE       gc_i                     TO gs_user_data-status_user.
        MODIFY     gt_user_data           FROM gs_user_data
                                         INDEX lv_tabix
                                  TRANSPORTING status_user.
      ENDIF.

    ENDIF.

    CLEAR  gs_user_data.
  ENDLOOP.

ENDFORM.                    " F_PART_III
*eject
*&---------------------------------------------------------------------*
*&      Form  F_PART_IV
*&---------------------------------------------------------------------*
*       Retrieve user responsibility (role) data
*----------------------------------------------------------------------*
FORM f_part_iv .

  DATA:      lv_tabix   TYPE sytabix.

  CONSTANTS: lc_resp    TYPE zparamkey       VALUE 'RESPONSIBILITY'.

* Part-IV

  SELECT   agr_name  uname  from_dat  to_dat
    INTO   TABLE gt_agr_users
    FROM   agr_users FOR ALL ENTRIES IN gt_user_names_p
   WHERE   uname     = gt_user_names_p-uname
     AND   uname    IN s_uname.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_agr_users BY agr_name ASCENDING.
  ELSE.
    CLEAR  gt_agr_users[].
  ENDIF.

  CLEAR                                     gs_agr_users.
  LOOP AT  gt_agr_users                INTO gs_agr_users.
    lv_tabix = sy-tabix.

    IF         ( gs_xparam_resp-value1   EQ gs_agr_users-agr_name ).
    ELSE.
      CLEAR                                 gs_xparam_resp.
      READ TABLE gt_xparam_resp        INTO gs_xparam_resp
                                   WITH KEY value1 = gs_agr_users-agr_name.
      IF       ( sy-subrc NE 0 ).
        CLEAR                               gs_xparam_resp.
        MOVE     gs_agr_users-agr_name   TO gs_xparam_resp-value1.
      ENDIF.
    ENDIF.

*eject
    IF       ( ( gs_xparam_resp-value1   EQ gs_agr_users-agr_name ) AND
               ( gs_xparam_resp-key2     EQ lc_resp               )     ).

      CLEAR                                 gs_agr_users-agr_name.
      MOVE       gs_xparam_resp-value2   TO gs_agr_users-agr_name.

      CLEAR                                 gs_agr_users-status_resp.

      IF     ( ( gs_agr_users-from_dat   LE sy-datum ) AND
               ( gs_agr_users-to_dat     GE sy-datum )     ).
        MOVE     gc_a                    TO gs_agr_users-status_resp.
      ELSE.
        MOVE     gc_i                    TO gs_agr_users-status_resp.
      ENDIF.

      MODIFY     gt_agr_users          FROM gs_agr_users
                                      INDEX lv_tabix.

    ELSE.

      DELETE     gt_agr_users         INDEX lv_tabix.

    ENDIF.

  ENDLOOP.

  SORT     gt_agr_users        ASCENDING BY uname agr_name.
  DELETE   ADJACENT DUPLICATES         FROM gt_agr_users
                                  COMPARING uname agr_name.

ENDFORM.                    " F_PART_IV
*eject
*&---------------------------------------------------------------------*
*&      Form  F_PART_V
*&---------------------------------------------------------------------*
*       Generate IAP user and responsibility data as of the current date
*----------------------------------------------------------------------*
FORM f_part_v .

  DATA: lv_smess        TYPE text255,
        lv_cmess        TYPE text255.

  DATA: lt_return       TYPE STANDARD TABLE OF bapiret2,
        ls_return       TYPE bapiret2.

* Part-V

* Generate IAP user and responsibility data as of the current date and time

  CALL FUNCTION 'ZAP_IAP_GET_DOA_DATA' DESTINATION gv_rfcdest_us
    TABLES
      USER_DATA             = gt_user_data
      DOA_DATA              = gt_doa_data
      RETURN                = lt_return
    EXCEPTIONS
      SYSTEM_FAILURE        = 1  MESSAGE lv_smess
      COMMUNICATION_FAILURE = 2  MESSAGE lv_cmess.

  CASE sy-subrc.
    WHEN 1.
      WRITE: /                   text-e23, lv_smess.
      MESSAGE  e000(zfi01) WITH  text-e23  lv_smess.
    WHEN 2.
      WRITE: /                   text-e24, lv_cmess.
      MESSAGE  e000(zfi01) WITH  text-e24  lv_cmess.
  ENDCASE.

  CLEAR                                     gs_user_data.
  CLEAR                                     gs_agr_users.
  LOOP AT  gt_agr_users                INTO gs_agr_users.

    IF         ( gs_user_data-uname      EQ gs_agr_users-uname ).
    ELSE.
      CLEAR                                 gs_user_data.
      READ TABLE gt_user_data          INTO gs_user_data
                                   WITH KEY uname = gs_agr_users-uname.
      IF       ( sy-subrc NE 0 ).
        CLEAR    gs_user_data.
        CLEAR    gs_agr_users.
        CONTINUE.
      ENDIF.
    ENDIF.

* Create the current general and coder user records

    IF       ( ( gs_agr_users-agr_name   EQ gc_general ) OR
               ( gs_agr_users-agr_name   EQ gc_coder   )    ).

      CLEAR                                 gs_iap_user_current.
      MOVE       gs_user_data-uname      TO gs_iap_user_current-uname.
      MOVE       gs_agr_users-agr_name   TO gs_iap_user_current-agr_name.
      MOVE       SPACE                   TO gs_iap_user_current-blart.
      MOVE       gs_user_data-uname_nwid TO gs_iap_user_current-uname_nwid.
      MOVE       gs_user_data-status_user
                                         TO gs_iap_user_current-status_user.
      MOVE       gs_agr_users-status_resp
                                         TO gs_iap_user_current-status_resp.
      MOVE       gs_user_data-pernr      TO gs_iap_user_current-pernr.
      MOVE       gs_user_data-trfgr      TO gs_iap_user_current-trfgr.
      MOVE       0                       TO gs_iap_user_current-amount.
      MOVE       gs_user_data-nachn      TO gs_iap_user_current-nachn.
      MOVE       gs_user_data-vorna      TO gs_iap_user_current-vorna.
      MOVE       gs_user_data-land1      TO gs_iap_user_current-land1.
      MOVE       gs_user_data-smtp_addr  TO gs_iap_user_current-smtp_addr.
      MOVE       gs_user_data-uname_mgr  TO gs_iap_user_current-uname_mgr.
      MOVE       gs_user_data-pernr_mgr  TO gs_iap_user_current-pernr_mgr.

      APPEND     gs_iap_user_current     TO gt_iap_user_current.

      CLEAR      gs_agr_users.
      CONTINUE.

    ENDIF.

* Create the current invoice approver user records
    CLEAR    gt_doa_data_p[].

    CLEAR                                   gs_doa_data.
    LOOP AT  gt_doa_data               INTO gs_doa_data.
      IF ( ( gs_doa_data-lookup_key_type EQ gc_e               ) AND
           ( gs_doa_data-lookup_key      EQ gs_user_data-pernr )     ) OR
         ( ( gs_doa_data-lookup_key_type EQ gc_g               ) AND
           ( gs_doa_data-lookup_key      EQ gs_user_data-trfgr )     ).
        APPEND gs_doa_data               TO gt_doa_data_p.
      ENDIF.
      CLEAR  gs_doa_data.
    ENDLOOP.

    CLEAR                                   gs_valid_doc_types.
    LOOP AT  gt_valid_doc_types        INTO gs_valid_doc_types.

      AUTHORITY-CHECK   OBJECT  'F_BKPF_BLA' FOR USER gs_user_data-uname
             ID  'BRGRU' FIELD  gs_valid_doc_types-brgru
             ID  'ACTVT' FIELD '01'.

      IF     ( sy-subrc NE 0 ).
        CLEAR    gs_valid_doc_types.
        CONTINUE.
      ENDIF.

      CLEAR    gv_flag_found.
      CLEAR    gv_doa_limit.

      PERFORM  f_calculate_doa       TABLES gt_doa_data_p
                                      USING gs_user_data
                                            gc_tcode_fv60
                                            gs_valid_doc_types-blart
                                   CHANGING gv_doa_limit
                                            gv_flag_found.

      IF     ( gv_flag_found             IS INITIAL ).
        CLEAR  gs_valid_doc_types.
        CONTINUE.
      ENDIF.

      CLEAR                                 gs_iap_user_current.
      MOVE       gs_user_data-uname      TO gs_iap_user_current-uname.
      MOVE       gs_agr_users-agr_name   TO gs_iap_user_current-agr_name.
      MOVE       gs_valid_doc_types-blart
                                         TO gs_iap_user_current-blart.
      MOVE       gs_user_data-uname_nwid TO gs_iap_user_current-uname_nwid.
      MOVE       gs_user_data-status_user
                                         TO gs_iap_user_current-status_user.
      MOVE       gs_agr_users-status_resp
                                         TO gs_iap_user_current-status_resp.
      MOVE       gs_user_data-pernr      TO gs_iap_user_current-pernr.
      MOVE       gs_user_data-trfgr      TO gs_iap_user_current-trfgr.
      MOVE       gv_doa_limit            TO gs_iap_user_current-amount.
      MOVE       gs_user_data-nachn      TO gs_iap_user_current-nachn.
      MOVE       gs_user_data-vorna      TO gs_iap_user_current-vorna.
      MOVE       gs_user_data-land1      TO gs_iap_user_current-land1.
      MOVE       gs_user_data-smtp_addr  TO gs_iap_user_current-smtp_addr.
      MOVE       gs_user_data-uname_mgr  TO gs_iap_user_current-uname_mgr.
      MOVE       gs_user_data-pernr_mgr  TO gs_iap_user_current-pernr_mgr.

      APPEND     gs_iap_user_current     TO gt_iap_user_current.

      CLEAR  gs_valid_doc_types.
    ENDLOOP.

    CLEAR  gs_agr_users.
  ENDLOOP.

  SORT     gt_iap_user_current ASCENDING BY uname agr_name blart.

ENDFORM.                    " F_PART_V
*eject
*&---------------------------------------------------------------------*
*&      Form  F_PART_VI
*&---------------------------------------------------------------------*
*       Retrieve IAP user and responsibility history from ZAPT_IAP_USER
*----------------------------------------------------------------------*
FORM f_part_vi .

* Part-VI

  IF     ( gt_user_names_p[] IS NOT INITIAL ).

    SELECT   *
      INTO   TABLE gt_iap_user_history
      FROM   zapt_iap_user FOR ALL ENTRIES IN gt_user_names_p
     WHERE   uname = gt_user_names_p-uname.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_iap_user_history ASCENDING BY uname agr_name blart.
    ELSE.
      CLEAR  gt_iap_user_history[].
    ENDIF.

  ENDIF.

ENDFORM.                    " F_PART_VI
*eject
*&---------------------------------------------------------------------*
*&      Form  F_PART_VII
*&---------------------------------------------------------------------*
*       Extract changed data to IAP and update the table ZAPT_IAP_USER
*----------------------------------------------------------------------*
FORM f_part_vii .

  DATA:    lt_iap_user_changes         TYPE STANDARD TABLE
                                         OF gty_iap_user.

  DATA:    lv_subrc TYPE sysubrc,
           lv_tabix TYPE sytabix,
           lv_count TYPE syindex.

* Part-VII

  CLEAR                                     gs_iap_user_current.
  LOOP AT  gt_iap_user_current         INTO gs_iap_user_current.

    CLEAR                                   gs_iap_user_history.
    READ TABLE gt_iap_user_history     INTO gs_iap_user_history
                        WITH KEY uname    = gs_iap_user_current-uname
                                 agr_name = gs_iap_user_current-agr_name
                                 blart    = gs_iap_user_current-blart
                   BINARY SEARCH.
    lv_subrc = sy-subrc.
    lv_tabix = sy-tabix.

    IF     ( lv_subrc EQ 0 ). "updates

      IF gs_iap_user_current-uname       EQ gs_iap_user_history-uname       AND
         gs_iap_user_current-agr_name    EQ gs_iap_user_history-agr_name    AND
         gs_iap_user_current-blart       EQ gs_iap_user_history-blart       AND
         gs_iap_user_current-uname_nwid  EQ gs_iap_user_history-uname_nwid  AND
         gs_iap_user_current-status_user EQ gs_iap_user_history-status_user AND
         gs_iap_user_current-status_resp EQ gs_iap_user_history-status_resp AND
         gs_iap_user_current-pernr       EQ gs_iap_user_history-pernr       AND
         gs_iap_user_current-trfgr       EQ gs_iap_user_history-trfgr       AND
         gs_iap_user_current-amount      EQ gs_iap_user_history-amount      AND
         gs_iap_user_current-nachn       EQ gs_iap_user_history-nachn       AND
         gs_iap_user_current-vorna       EQ gs_iap_user_history-vorna       AND
         gs_iap_user_current-land1       EQ gs_iap_user_history-land1       AND
         gs_iap_user_current-smtp_addr   EQ gs_iap_user_history-smtp_addr   AND
         gs_iap_user_current-uname_mgr   EQ gs_iap_user_history-uname_mgr   AND
         gs_iap_user_current-pernr_mgr   EQ gs_iap_user_history-pernr_mgr.
      ELSE.

        CLEAR                               gs_iap_user_current-mandt.
        MOVE     sy-mandt                TO gs_iap_user_current-mandt.
        CLEAR                               gs_iap_user_current-aedat.
        MOVE     sy-datum                TO gs_iap_user_current-aedat.
        CLEAR                               gs_iap_user_current-aezeit.
        MOVE     sy-uzeit                TO gs_iap_user_current-aezeit.
        CLEAR                               gs_iap_user_current-aenam.
        MOVE     sy-uname                TO gs_iap_user_current-aenam.
        APPEND   gs_iap_user_current     TO gt_iap_user_changes.

      ENDIF.

      DELETE   gt_iap_user_history    INDEX lv_tabix.

    ELSE. "( lv_subrc NE 0 ). "inserts

      IF   ( ( gs_iap_user_current-status_user EQ gc_i ) OR
             ( gs_iap_user_current-status_resp EQ gc_i )    ).
        CLEAR  gs_iap_user_current.
        CONTINUE.
      ENDIF.

      CLEAR                                 gs_iap_user_current-mandt.
      MOVE     sy-mandt                  TO gs_iap_user_current-mandt.
      CLEAR                                 gs_iap_user_current-aedat.
      MOVE     sy-datum                  TO gs_iap_user_current-aedat.
      CLEAR                                 gs_iap_user_current-aezeit.
      MOVE     sy-uzeit                  TO gs_iap_user_current-aezeit.
      CLEAR                                 gs_iap_user_current-aenam.
      MOVE     sy-uname                  TO gs_iap_user_current-aenam.
      APPEND   gs_iap_user_current       TO gt_iap_user_changes.

    ENDIF.

  ENDLOOP.

  CLEAR                                     gs_iap_user_history.
  LOOP AT  gt_iap_user_history         INTO gs_iap_user_history.

    CLEAR                                   gs_user_data.
    READ     TABLE gt_user_data        INTO gs_user_data
                           WITH KEY uname = gs_iap_user_history-uname.
    IF     ( sy-subrc NE 0 ).
      CLEAR                                 gs_user_data.
      MOVE     gc_i                      TO gs_user_data-status_user.
    ENDIF.

    IF   ( ( gs_user_data-status_user        EQ gc_i ) AND
           ( gs_iap_user_history-status_user EQ gc_i ) AND
           ( gs_iap_user_history-status_resp EQ gc_i )     ).
      CLEAR  gs_iap_user_history.
      CONTINUE.
    ENDIF.

    CLEAR                                   gs_iap_user_history-mandt.
    MOVE     sy-mandt                    TO gs_iap_user_history-mandt.
    CLEAR                                   gs_iap_user_history-status_user.
    MOVE     gs_user_data-status_user    TO gs_iap_user_history-status_user.
    CLEAR                                   gs_iap_user_history-status_resp.
    MOVE     gc_i                        TO gs_iap_user_history-status_resp.
    CLEAR                                   gs_iap_user_history-aedat.
    MOVE     sy-datum                    TO gs_iap_user_history-aedat.
    CLEAR                                   gs_iap_user_history-aezeit.
    MOVE     sy-uzeit                    TO gs_iap_user_history-aezeit.
    CLEAR                                   gs_iap_user_history-aenam.
    MOVE     sy-uname                    TO gs_iap_user_history-aenam.
    APPEND   gs_iap_user_history         TO gt_iap_user_changes.

    CLEAR    gs_iap_user_history.
  ENDLOOP.

  IF       ( gv_full_extract             EQ gc_x ).
    DELETE   gt_iap_user_changes      WHERE status_user = gc_i
                                         OR status_resp = gc_i.
  ENDIF.

  CLEAR    lt_iap_user_changes[].
  CLEAR    lv_count.

  CLEAR                                     gs_iap_user_changes.
  LOOP AT    gt_iap_user_changes       INTO gs_iap_user_changes.

    APPEND   gs_iap_user_changes         TO lt_iap_user_changes.
    ADD      1                           TO lv_count.

    IF     ( lv_count GE 200 ).

      IF     ( cb_test IS INITIAL ).

        MODIFY   zapt_iap_user CLIENT SPECIFIED FROM TABLE lt_iap_user_changes.
        IF     ( sy-subrc EQ 0 ).
          COMMIT   WORK AND WAIT.
        ELSE.
          ROLLBACK WORK.
          MESSAGE  e000(zfi01) WITH text-e51.
        ENDIF.

      ENDIF.

      PERFORM  f_final_data        TABLES lt_iap_user_changes.

      CLEAR    lt_iap_user_changes[].
      CLEAR    lv_count.

    ENDIF.

    CLEAR  gs_iap_user_changes.
  ENDLOOP.

  IF     ( lv_count GE 1 ).

    IF     ( cb_test IS INITIAL ).

      MODIFY   zapt_iap_user CLIENT SPECIFIED FROM TABLE lt_iap_user_changes.
      IF     ( sy-subrc EQ 0 ).
        COMMIT   WORK AND WAIT.
      ELSE.
        ROLLBACK WORK.
        MESSAGE  e000(zfi01) WITH text-e51.
      ENDIF.

    ENDIF.

    PERFORM  f_final_data          TABLES lt_iap_user_changes.

  ENDIF.

ENDFORM.                    " F_PART_VII
*eject
*&---------------------------------------------------------------------*
*&      Form  F_CALCULATE_DOA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_calculate_doa
  TABLES   it_doa_data                 TYPE gtt_doa_data
  USING    is_user_data                TYPE gty_user_data
           iv_tcode                    TYPE char4
           iv_blart                    TYPE blart
  CHANGING cv_doa_limit                TYPE zfi_doa_amount
           cv_flag_found               TYPE xflag.

  DATA:    ls_doa_data_f               TYPE gty_doa_data,
           lv_doc_type_1               TYPE zfi_doc_type,
           lv_doc_type_2               TYPE zfi_doc_type,
           lv_lookup_key               TYPE zfi_lookup_key.

  CLEAR    cv_doa_limit.
  CLEAR    cv_flag_found.

  IF     ( it_doa_data[]                 IS INITIAL ).
    RETURN.
  ENDIF.

  IF   ( ( is_user_data-uname            IS INITIAL ) OR
         ( is_user_data-status_user      EQ gc_i    )    ).
    RETURN.
  ENDIF.

  CLEAR    ls_doa_data_f.
  CLEAR    lv_doc_type_1.
  CLEAR    lv_doc_type_2.

* UG System
  IF           ( p_erpid                 EQ 'SAPUG'     ).
    CONCATENATE  iv_blart  '_UG'       INTO lv_doc_type_1.
    MOVE         iv_blart                TO lv_doc_type_2.
* SW System
  ELSEIF       ( p_erpid                 EQ 'SAPSW'     ).
    CONCATENATE  iv_blart  '_SW'       INTO lv_doc_type_1.
    MOVE         iv_blart                TO lv_doc_type_2.
* US System
  ELSE.       "( p_erpid                 EQ 'SAPUS'     ).
    MOVE         iv_blart                TO lv_doc_type_1.
    MOVE         iv_blart                TO lv_doc_type_2.
  ENDIF.

*eject
* Job Grade
  IF     ( is_user_data-trfgr            IS NOT INITIAL ).

    CLEAR                                   lv_lookup_key.
    MOVE   is_user_data-trfgr            TO lv_lookup_key.

    CLEAR                                   gs_doa_data.
    READ TABLE it_doa_data             INTO gs_doa_data
                                   WITH KEY lookup_key      = lv_lookup_key
                                            lookup_key_type = gc_g
                                            tcode           = iv_tcode
                                            doc_type        = SPACE.
    IF       ( sy-subrc EQ 0 ).
      CLEAR                                 ls_doa_data_f.
      MOVE     gs_doa_data               TO ls_doa_data_f.
    ENDIF.

    CLEAR                                   gs_doa_data.
    READ TABLE it_doa_data             INTO gs_doa_data
                                   WITH KEY lookup_key      = lv_lookup_key
                                            lookup_key_type = gc_g
                                            tcode           = iv_tcode
                                            doc_type        = lv_doc_type_1.
    IF       ( sy-subrc EQ 0 ).

      IF     ( gs_doa_data-amount        GT ls_doa_data_f-amount ).
        CLEAR                               ls_doa_data_f.
        MOVE   gs_doa_data               TO ls_doa_data_f.
      ENDIF.

    ELSE.

      CLEAR                                 gs_doa_data.
      READ TABLE it_doa_data           INTO gs_doa_data
                                   WITH KEY lookup_key      = lv_lookup_key
                                            lookup_key_type = gc_g
                                            tcode           = iv_tcode
                                            doc_type        = lv_doc_type_2.
      IF       ( sy-subrc EQ 0 ).

        IF     ( gs_doa_data-amount      GT ls_doa_data_f-amount ).
          CLEAR                             ls_doa_data_f.
          MOVE   gs_doa_data             TO ls_doa_data_f.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

*eject
* Personnel Number
  IF     ( is_user_data-pernr            IS NOT INITIAL ).

    CLEAR                                   lv_lookup_key.
    MOVE   is_user_data-pernr            TO lv_lookup_key.

    CLEAR                                   gs_doa_data.
    READ TABLE it_doa_data             INTO gs_doa_data
                                   WITH KEY lookup_key      = lv_lookup_key
                                            lookup_key_type = gc_e
                                            tcode           = iv_tcode
                                            doc_type        = lv_doc_type_1.
    IF       ( sy-subrc EQ 0 ).

      IF     ( gs_doa_data-amount        GT ls_doa_data_f-amount ).
        CLEAR                               ls_doa_data_f.
        MOVE   gs_doa_data               TO ls_doa_data_f.
      ENDIF.

    ELSE.

      CLEAR                                 gs_doa_data.
      READ TABLE it_doa_data           INTO gs_doa_data
                                   WITH KEY lookup_key      = lv_lookup_key
                                            lookup_key_type = gc_e
                                            tcode           = iv_tcode
                                            doc_type        = lv_doc_type_2.
      IF       ( sy-subrc EQ 0 ).

        IF     ( gs_doa_data-amount      GT ls_doa_data_f-amount ).
          CLEAR                             ls_doa_data_f.
          MOVE   gs_doa_data             TO ls_doa_data_f.
        ENDIF.

      ENDIF.

    ENDIF.

    CLEAR                                   gs_doa_data.
    READ TABLE it_doa_data             INTO gs_doa_data
                                   WITH KEY lookup_key      = lv_lookup_key
                                            lookup_key_type = gc_e
                                            tcode           = iv_tcode
                                            doc_type        = SPACE.
    IF       ( sy-subrc EQ 0 ).
      IF     ( gs_doa_data-amount        GT ls_doa_data_f-amount ).
        CLEAR                               ls_doa_data_f.
        MOVE   gs_doa_data               TO ls_doa_data_f.
      ENDIF.
    ENDIF.

    CLEAR                                   gs_doa_data.
    READ TABLE it_doa_data             INTO gs_doa_data
                                   WITH KEY lookup_key      = lv_lookup_key
                                            lookup_key_type = gc_e
                                            tcode           = SPACE
                                            doc_type        = SPACE.
    IF       ( sy-subrc EQ 0 ).
      IF     ( gs_doa_data-amount        GT ls_doa_data_f-amount ).
        CLEAR                               ls_doa_data_f.
        MOVE   gs_doa_data               TO ls_doa_data_f.
      ENDIF.
    ENDIF.

  ENDIF.

  CLEAR                                     cv_doa_limit.
  MOVE     ls_doa_data_f-amount          TO cv_doa_limit.

  IF     ( cv_doa_limit                  GT 0 ).
    MOVE   gc_x                          TO cv_flag_found.
  ENDIF.

ENDFORM.                    " F_CALCULATE_DOA
*eject
*&---------------------------------------------------------------------*
*&      Form  F_FINAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_final_data
  TABLES   it_iap_user                 TYPE gtt_iap_user.

  DATA:    ls_iap_user                 TYPE gty_iap_user,
           lv_char21                   TYPE char21,
           lv_string                   TYPE string.

  CONSTANTS: lc_sep(2)                 TYPE c VALUE '|~'.

  CLEAR                                     ls_iap_user.
  LOOP AT  it_iap_user                 INTO ls_iap_user.

    CLEAR                                   lv_char21.
    WRITE    ls_iap_user-amount          TO lv_char21
                                   DECIMALS 2.
    TRANSLATE                               lv_char21 USING ', - '.
    CONDENSE                                lv_char21 NO-GAPS.

    CLEAR                                   gs_final.
    MOVE     ls_iap_user-vorna           TO gs_final-fname.
    MOVE     ls_iap_user-nachn           TO gs_final-lname.
    MOVE     ls_iap_user-uname_nwid      TO gs_final-uname.
    MOVE     p_erpid                     TO gs_final-erpid.
    MOVE     ls_iap_user-pernr           TO gs_final-pernr.
    MOVE     ls_iap_user-agr_name        TO gs_final-resp.
    MOVE     ls_iap_user-status_resp     TO gs_final-status_resp.
    MOVE     ls_iap_user-blart           TO gs_final-blart.
    MOVE     lv_char21                   TO gs_final-sp_limit.
    MOVE     SPACE                       TO gs_final-sp_type.
    MOVE     ls_iap_user-smtp_addr       TO gs_final-addr.
    MOVE     ls_iap_user-uname_mgr       TO gs_final-uname_mgr.
    MOVE     ls_iap_user-pernr_mgr       TO gs_final-pernr_mgr.
    MOVE     'Enbridge'                  TO gs_final-company.
    MOVE     ls_iap_user-land1           TO gs_final-land1.
    MOVE     ls_iap_user-status_user     TO gs_final-status_user.

*eject
    IF     ( rb_appl                     IS NOT INITIAL ).

      CLEAR                                 lv_string.
*      CONCATENATE      gs_final-fname       gs_final-lname
*                       gs_final-uname       gs_final-erpid
*                       gs_final-pernr       gs_final-resp
*                       gs_final-status_resp gs_final-blart
*                       gs_final-sp_limit    gs_final-sp_type
*                       gs_final-addr        gs_final-uname_mgr
*                       gs_final-pernr_mgr   gs_final-company
*                       gs_final-land1       gs_final-status_user
*                                       INTO lv_string
*                               SEPARATED BY lc_sep.

      CONCATENATE      gs_final-erpid       gs_final-fname
                       gs_final-lname       gs_final-uname
                       gs_final-pernr       gs_final-resp
                       gs_final-sp_limit    gs_final-sp_type
                       gs_final-blart       gs_final-land1
                       gs_final-addr        gs_final-uname_mgr
                       gs_final-pernr_mgr   gs_final-company
                       gs_final-status_user gs_final-status_resp
                                       INTO lv_string
                               SEPARATED BY lc_sep.

      IF       ( cb_test                 IS INITIAL ).

        TRANSFER lv_string               TO gv_filepath.
        ADD      1                       TO gv_count_recs.

      ENDIF.

    ELSE. "( p_dis                       IS NOT INITIAL ).

      APPEND   gs_final                  TO gt_final.
      ADD      1                         TO gv_count_recs.

    ENDIF.

    CLEAR                                   gs_output.
    MOVE     gs_final-erpid              TO gs_output-erpid.
    MOVE     gs_final-uname              TO gs_output-uname.
    MOVE     gs_final-status_user        TO gs_output-status_user.
    MOVE     gs_final-resp               TO gs_output-resp.
    MOVE     gs_final-status_resp        TO gs_output-status_resp.
    MOVE     gs_final-blart              TO gs_output-blart.
    MOVE     gs_final-sp_limit           TO gs_output-sp_limit.
    MOVE     gs_final-land1              TO gs_output-land1.
    MOVE     gs_final-uname_mgr          TO gs_output-uname_mgr.
    CLEAR                                   lv_string.
    MOVE     gs_output                   TO lv_string.

    WRITE: / lv_string.

    CLEAR  ls_iap_user.
  ENDLOOP.

ENDFORM.                    " F_FINAL_DATA
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_PERSONNEL_DATA
*&---------------------------------------------------------------------*
*       Get the personnel data
*----------------------------------------------------------------------*
FORM f_get_personnel_data .

  DATA: lv_smess        TYPE text255,
        lv_cmess        TYPE text255.

  DATA: lt_return       TYPE STANDARD TABLE OF bapiret2,
        ls_return       TYPE bapiret2.

  CALL FUNCTION 'ZAP_IAP_GET_USER_DATA' DESTINATION gv_rfcdest_hr
    TABLES
      USER_NAMES            = gt_user_names_p
      USER_DATA             = gt_user_data
      RETURN                = lt_return
    EXCEPTIONS
      SYSTEM_FAILURE        = 1  MESSAGE lv_smess
      COMMUNICATION_FAILURE = 2  MESSAGE lv_cmess.

  CASE sy-subrc.
    WHEN 1.
      WRITE: /                   text-e21, lv_smess.
      MESSAGE  e000(zfi01) WITH  text-e21  lv_smess.
    WHEN 2.
      WRITE: /                   text-e22, lv_cmess.
      MESSAGE  e000(zfi01) WITH  text-e22  lv_cmess.
  ENDCASE.

ENDFORM.                    " F_GET_PERSONNEL_DATA
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_USER_DETAILS
*&---------------------------------------------------------------------*
*       Get the user details
*----------------------------------------------------------------------*
FORM f_get_user_details.

  DATA:    ls_usr02 TYPE gty_usr02.

  DATA:    lv_tabix TYPE sytabix.

  SELECT   bname  gltgv  gltgb
    INTO   TABLE gt_usr02
    FROM   usr02 FOR ALL ENTRIES IN gt_user_names_p
   WHERE   bname = gt_user_names_p-uname.
  IF     ( sy-subrc EQ 0 ).

    CLEAR                                   ls_usr02.
    LOOP AT  gt_usr02                  INTO ls_usr02.
      lv_tabix = sy-tabix.
      IF   ( ( ls_usr02-gltgv            IS INITIAL    ) OR
             ( ls_usr02-gltgv            EQ '00000000' )    ).
        CLEAR                               ls_usr02-gltgv.
        MOVE   '19000101'                TO ls_usr02-gltgv.
      ENDIF.
      IF   ( ( ls_usr02-gltgb            IS INITIAL    ) OR
             ( ls_usr02-gltgb            EQ '00000000' )    ).
        CLEAR                               ls_usr02-gltgb.
        MOVE   '99991231'                TO ls_usr02-gltgb.
      ENDIF.
      MODIFY   gt_usr02                FROM ls_usr02 INDEX lv_tabix.
      CLEAR    ls_usr02.
    ENDLOOP.

    SORT     gt_usr02 ASCENDING BY uname.

    SELECT   bname  persnumber  addrnumber
      INTO   TABLE gt_usr21
      FROM   usr21 FOR ALL ENTRIES IN gt_usr02
     WHERE   bname = gt_usr02-uname.
    IF     ( sy-subrc EQ 0 ).

      SORT     gt_usr21 ASCENDING BY bname persnumber addrnumber.

      SELECT   persnumber  name_first  name_last
        INTO   TABLE gt_adrp
        FROM   adrp FOR ALL ENTRIES IN gt_usr21
       WHERE   persnumber = gt_usr21-persnumber.
      IF     ( sy-subrc EQ 0 ).
        SORT   gt_adrp  ASCENDING BY persnumber.
      ELSE.
        CLEAR  gt_adrp[].
      ENDIF.

      SELECT   addrnumber  country
        INTO   TABLE gt_adrc
        FROM   adrc FOR ALL ENTRIES IN gt_usr21
       WHERE   addrnumber = gt_usr21-addrnumber.
      IF     ( sy-subrc NE 0 ).
        SORT   gt_adrc  ASCENDING BY addrnumber.
      ELSE.
        CLEAR  gt_adrc[].
      ENDIF.

      SELECT   addrnumber  persnumber  smtp_addr
        INTO   TABLE gt_adr6
        FROM   adr6 FOR ALL ENTRIES IN gt_usr21
       WHERE   addrnumber = gt_usr21-addrnumber
         AND   persnumber = gt_usr21-persnumber.
      IF     ( sy-subrc NE 0 ).
        SORT   gt_adr6  ASCENDING BY addrnumber persnumber.
      ELSE.
        CLEAR  gt_adr6[].
      ENDIF.

    ELSE.
      CLEAR  gt_usr21[].
    ENDIF.
  ELSE.
    CLEAR  gt_usr02[].
  ENDIF.

ENDFORM.                    " F_GET_USER_DETAILS
*eject
*&---------------------------------------------------------------------*
*&      Form  F_OPEN_DATASET
*&---------------------------------------------------------------------*
*       Open the dataset
*----------------------------------------------------------------------*
FORM f_open_dataset .

  DATA:    lv_msg_text TYPE text50,
           lv_string   TYPE STRING.

  CONSTANTS: lc_sep(2)                 TYPE c VALUE '|~'.

  IF   ( ( rb_appl IS     INITIAL ) OR
         ( cb_test IS NOT INITIAL )    ).
    RETURN.
  ENDIF.

  CONCATENATE               p_path2 p_erpid p_file2
                                       INTO gv_filepath.

  OPEN DATASET gv_filepath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
                           MESSAGE lv_msg_text.
  IF     ( sy-subrc NE 0 ).
    WRITE: /                      text-014, lv_msg_text.
    MESSAGE  e000(zfi01)     WITH text-014  lv_msg_text.
    EXIT.
  ENDIF.

  CLEAR                                     lv_string.
  CONCATENATE           text-f01            text-f02
                        text-f03            text-f04
                        text-f05            text-f06
                        text-f07            text-f08
                        text-f09            text-f10
                        text-f11            text-f12
                        text-f13            text-f14
                        text-f15            text-f16
                                       INTO lv_string
                               SEPARATED BY lc_sep.
*
  TRANSFER lv_string                     TO gv_filepath.

ENDFORM.                    " F_OPEN_DATASET
