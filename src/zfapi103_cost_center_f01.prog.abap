*******************************************************************************
*                            Spectra Energy                                   *
*&---------------------------------------------------------------------       *
*& Program Name       :  ZFAPI103_COST_CENTER                                 *
*& Author             :  Kalinga Keshari Rout                                 *
*& Creation Date      :  January 09, 2018                                     *
*& Object ID          :                                                       *
*& Application Area   :  FICO                                                 *
*& Description        :  Program extracts master data for both cases          *
*&                       full load and delta load and file created in         *
*&                       application server  .Frequency od daat upload        *
*&                        weekly                                              *
*&--------------------------------------------------------------------------- *
*                      Modification Log                                       *
* Changed On   Changed By  CTS        Description                             *
*-----------------------------------------------------------------------------*
* 19-Jan-2018  KROUT       D30K928561 CHG0100807 # Initial development        *
*                          D30K928677                                         *
*-----------------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
*       Get the data
*----------------------------------------------------------------------*
FORM f_get_data.

  CLEAR    gt_csks_key[].
  CLEAR    gt_csks[].
  CLEAR    gt_cskt[].
  CLEAR    gt_editpos[].
  CLEAR    gt_final[].

  IF     ( rb_fload IS NOT INITIAL ).

    SELECT   kokrs  kostl  datbi  datab  bkzkp  bukrs
      INTO   TABLE gt_csks
      FROM   csks
     WHERE   kokrs IN s_kokrs
       AND   kostl IN s_kostl.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_csks[].
      WRITE: / text-005.
      RETURN.
    ENDIF.

  ELSE.

    CALL FUNCTION 'CHANGEDOCUMENT_READ'
      EXPORTING
        date_of_change                   = s_date-low
        objectclass                      = 'KOSTL'
        time_of_change                   = '000000'
        date_until                       = s_date-high
        time_until                       = '235959'
*     IMPORTING
*       ET_CDRED_STR                     =
      TABLES
        editpos                          = gt_editpos
      EXCEPTIONS
        no_position_found                = 1
        wrong_access_to_archive          = 2
        time_zone_conversion_error       = 3
        OTHERS                           = 4.

    IF   ( ( sy-subrc NE 0 ) OR ( gt_editpos[] IS INITIAL ) ).
      WRITE: / text-006.
      RETURN.
    ENDIF.

    CLEAR                                   gs_editpos.
    LOOP AT  gt_editpos                INTO gs_editpos.
      CLEAR                                 gs_csks_key.
      MOVE   gs_editpos-objectid+00(04)  TO gs_csks_key-kokrs.
      MOVE   gs_editpos-objectid+04(10)  TO gs_csks_key-kostl.
      APPEND gs_csks_key                 TO gt_csks_key.
      CLEAR  gs_editpos.
    ENDLOOP.

    SORT     gt_csks_key ASCENDING BY kokrs kostl.
    DELETE   ADJACENT DUPLICATES FROM gt_csks_key
                            COMPARING kokrs kostl.

    IF     ( gt_csks_key[] IS NOT INITIAL ).

      SELECT   kokrs  kostl  datbi  datab  bkzkp  bukrs
        INTO   TABLE gt_csks
        FROM   csks FOR ALL ENTRIES IN gt_csks_key
       WHERE   kokrs = gt_csks_key-kokrs
         AND   kostl = gt_csks_key-kostl.
      IF     ( sy-subrc NE 0 ).
        CLEAR  gt_csks[].
        WRITE: / text-005.
        RETURN.
      ENDIF.

    ENDIF.

  ENDIF .

  SORT     gt_csks                  BY kokrs ASCENDING
                                       kostl ASCENDING
                                       datbi DESCENDING.
  DELETE   ADJACENT DUPLICATES    FROM gt_csks
                             COMPARING kokrs kostl.

  SELECT   spras  kokrs  kostl  datbi  ktext
    INTO   TABLE gt_cskt
    FROM   cskt FOR ALL ENTRIES IN gt_csks
   WHERE   spras = 'E'
     AND   kokrs = gt_csks-kokrs
     AND   kostl = gt_csks-kostl.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_cskt[].
  ENDIF.

  SORT     gt_cskt                  BY spras ASCENDING
                                       kokrs ASCENDING
                                       kostl ASCENDING
                                       datbi DESCENDING.
  DELETE   ADJACENT DUPLICATES    FROM gt_cskt
                             COMPARING spras kokrs kostl.

ENDFORM.                    " f_get_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_data
*&---------------------------------------------------------------------*
*       Format the data
*----------------------------------------------------------------------*
FORM f_format_data.

  IF     ( gt_csks[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     gs_csks.
  LOOP AT  gt_csks                     INTO gs_csks.

    CLEAR                                   gs_cskt.
    READ TABLE gt_cskt                 INTO gs_cskt
                                   WITH KEY spras = 'E'
                                            kokrs = gs_csks-kokrs
                                            kostl = gs_csks-kostl
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_cskt.
    ENDIF.

    CLEAR                                   gs_final.
    MOVE     p_erpid                     TO gs_final-erpid.
    MOVE     gs_csks-kokrs               TO gs_final-kokrs.
    MOVE     gs_csks-kostl               TO gs_final-kostl.
    MOVE     gs_csks-bukrs               TO gs_final-bukrs.
    MOVE     gs_cskt-ktext               TO gs_final-ktext.

    PERFORM  f_check_text          CHANGING gs_final-ktext.

    IF   ( ( gs_csks-datbi               GE sy-datum ) AND
           ( gs_csks-datab               LE sy-datum ) AND
           ( gs_csks-bkzkp               IS INITIAL  )     ).
      MOVE   '1'                         TO gs_final-bkzkp.
    ELSE.
      MOVE   '0'                         TO gs_final-bkzkp.
    ENDIF.

    APPEND   gs_final                    TO gt_final.

    CLEAR  gs_csks.
  ENDLOOP.

ENDFORM.                    " f_format_data
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_PATH2  text
*      <--P_P_PATH4  text
*----------------------------------------------------------------------*
FORM f_get_file_path  CHANGING cv_file1 TYPE any  .
*                               cv_file2 TYPE any .

  CONSTANTS:  lc_path1  TYPE filepath-pathintern VALUE 'ZFAPI103_COSTCENTER'.

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

  CONCATENATE p_erpid '_COST_CENTER_'  sy-datum '_' sy-uzeit '.CSV' INTO p_file2.

ENDFORM.                    " F_GET_FILE_PATH

*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_display_data .



  DATA: lv_file TYPE string ,
        lv_lines TYPE i ,
        lv_records TYPE string,
        lv_succ TYPE string .

  DATA: lt_tab_data       TYPE STANDARD TABLE OF string,
        ls_tab_data       TYPE string,
        ls_data           TYPE ty_final.

  CONSTANTS : lc_sep TYPE c LENGTH 2 VALUE '|~'. " for column separator

  CONCATENATE text-f01
              text-f02
              text-f03
              text-f04
              text-f05
              text-f06
                INTO ls_tab_data SEPARATED BY lc_sep.
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
*   BIN_FILESIZE                    =
      filename                        = lv_file
*   FILETYPE                        = 'ASC'
*   APPEND                          = ' '
*   WRITE_FIELD_SEPARATOR           = ' '
*   HEADER                          = '00'
*   TRUNC_TRAILING_BLANKS           = ' '
*   WRITE_LF                        = 'X'
*   COL_SELECT                      = ' '
*   COL_SELECT_MASK                 = ' '
*   DAT_MODE                        = ' '
*   CONFIRM_OVERWRITE               = ' '
*   NO_AUTH_CHECK                   = ' '
*   CODEPAGE                        = ' '
*   IGNORE_CERR                     = ABAP_TRUE
*   REPLACEMENT                     = '#'
*   WRITE_BOM                       = ' '
*   TRUNC_TRAILING_BLANKS_EOL       = 'X'
*   WK1_N_FORMAT                    = ' '
*   WK1_N_SIZE                      = ' '
*   WK1_T_FORMAT                    = ' '
*   WK1_T_SIZE                      = ' '
*   WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*   SHOW_TRANSFER_STATUS            = ABAP_TRUE
*   VIRUS_SCAN_PROFILE              = '/SCET/GUI_DOWNLOAD'
* IMPORTING
*   FILELENGTH                      =
    TABLES
      data_tab                        = lt_tab_data
*   FIELDNAMES                      =
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
     OTHERS                          = 22
            .
  IF sy-subrc <> 0.

  ENDIF.
*  ***********    write output in the spool
  CLEAR lv_lines .
  DESCRIBE TABLE  gt_final LINES lv_lines.
  lv_records = lv_lines.
  CONCATENATE text-009 lv_records INTO lv_succ.
  WRITE lv_succ .
  p_file1 = lv_file.

ENDFORM.                    " F_DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  F_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_transport_data .



  DATA : lv_path   TYPE string ,
        lv_msg_text(50) ,
        lv_string TYPE string   ,
        lv_lines TYPE i ,
        lv_records TYPE string,
        lv_succ TYPE string .

  CONSTANTS :  lc_sep(2) TYPE c VALUE '|~'.

  CONCATENATE p_path2 p_erpid p_file2 INTO lv_path.

  IF lv_path IS NOT INITIAL  .
    OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
                          MESSAGE lv_msg_text.
    IF sy-subrc NE 0.
      WRITE: text-007 , lv_msg_text.
      EXIT.
    ENDIF.
* Transferring Data
    IF gt_final[] IS NOT INITIAL .
      CONCATENATE text-f01
                  text-f02
                  text-f03
                  text-f04
                  text-f05
                  text-f06
                    INTO lv_string SEPARATED BY lc_sep.

      TRANSFER lv_string TO lv_path.
      CLEAR lv_string .

      LOOP AT gt_final INTO gs_final.
        CONCATENATE  gs_final-erpid
                     gs_final-kokrs
                     gs_final-kostl
                     gs_final-bukrs
                     gs_final-ktext
                     gs_final-bkzkp INTO lv_string SEPARATED BY lc_sep .

        TRANSFER lv_string TO lv_path.
        CLEAR :gs_final , lv_string .
      ENDLOOP.
    ELSE .
      TRANSFER lv_string TO lv_path.
    ENDIF .
* Closing the File
    CLOSE DATASET lv_path.

***********    write output in the spool
    CLEAR lv_lines .
    DESCRIBE TABLE  gt_final LINES lv_lines.
    lv_records = lv_lines.
    CONCATENATE text-009 lv_records INTO lv_succ.
    WRITE lv_succ .
  ENDIF .


ENDFORM.                    " F_TRANSPORT_DATA

*&---------------------------------------------------------------------*
*&      Form  f_check_text
*&---------------------------------------------------------------------*
*       Check the text for invalid characters
*----------------------------------------------------------------------*
FORM f_check_text
  CHANGING cv_ktext TYPE ktext.

  DATA : lv_string TYPE c LENGTH 50,
         lv_ind TYPE sy-index,
         lv_char TYPE c,
         lv_len TYPE i.

  CLEAR: lv_string,lv_len,lv_ind,lv_char.

  MOVE cv_ktext TO lv_string.
  CONDENSE lv_string.

  CONSTANTS : lc_sep1 TYPE c LENGTH 2  VALUE '|~',
              lc_sep2 TYPE c LENGTH 2  VALUE '~|',
              lc_rep1 TYPE c LENGTH 2  VALUE '|-',
              lc_rep2 TYPE c LENGTH 2  VALUE '-|'.

  REPLACE ALL OCCURRENCES OF lc_sep1 IN lv_string WITH lc_rep1.
  REPLACE ALL OCCURRENCES OF lc_sep2 IN lv_string WITH lc_rep2.

*--> Any Other Spl Chars less than Space replace by Space
  lv_len  = strlen( lv_string ).

  DO lv_len TIMES.
    lv_ind = sy-index - 1.
    CLEAR lv_char.
    MOVE     lv_string+lv_ind(1) TO lv_char.
    IF     ( lv_char LT ' ' ).
      MOVE   space  TO lv_string+lv_ind(1).
    ENDIF.
  ENDDO.

  CLEAR  cv_ktext.
  CONDENSE lv_string.
  MOVE   lv_string TO cv_ktext.

ENDFORM.                    " f_check_text

*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_f4_help_file_path1 .


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
