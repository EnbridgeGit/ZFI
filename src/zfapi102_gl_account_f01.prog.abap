*&---------------------------------------------------------------------*
*&  Include           ZFAPI102_GL_ACCOUNT_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI102_GL_ACCOUNT                           *
* Author             :                                                 *
* Date               :   16-Feb-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  G/L Accounts Extraction  for IAP               *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 16-Feb-2018  VRAJAPUTRA  D30K928566 CHG0100806 # Initial development *
*                          D30K928825, D30K928874, D30K928888          *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_get_data .

  REFRESH : gt_ska1,gt_skb1, gt_skat,gt_cskb.

  DATA : lt_delta TYPE STANDARD TABLE OF cdred,
         ls_ska1  TYPE gty_ska1,
         ls_delta TYPE cdred.


  IF p_com = gc_x.   " Complete Data
    SELECT ktopl
           saknr
           xspeb
      FROM ska1
      INTO TABLE gt_ska1
     WHERE ktopl IN s_ktopl
       AND saknr IN s_saknr.
    IF sy-subrc IS INITIAL.
      SORT gt_ska1.
    ENDIF.
  ELSE.   " For Delta Extraction with Data Range
    CALL FUNCTION 'CHANGEDOCUMENT_READ'
      EXPORTING
        date_of_change             = s_date-low
        objectclass                = 'SACH'
        date_until                 = s_date-high
      TABLES
        editpos                    = lt_delta
      EXCEPTIONS
        no_position_found          = 1
        wrong_access_to_archive    = 2
        time_zone_conversion_error = 3
        OTHERS                     = 4.
    IF sy-subrc IS INITIAL.

    ELSE.
      MESSAGE ID sy-msgid
                TYPE sy-msgty
              NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR : ls_delta,ls_ska1.

    LOOP AT lt_delta INTO ls_delta WHERE chngind = 'I' . " New Records
      ls_ska1-ktopl = ls_delta-objectid+0(4) .
      ls_ska1-saknr = ls_delta-objectid+4(10) .
      APPEND ls_ska1 TO gt_ska1 .
      CLEAR : ls_delta , ls_ska1.
    ENDLOOP.
  ENDIF.


  IF gt_ska1 IS NOT INITIAL.

    SELECT bukrs
           saknr
           fstag
           xspeb
           xintb
      INTO TABLE gt_skb1
      FROM skb1
      FOR ALL ENTRIES IN gt_ska1
      WHERE saknr = gt_ska1-saknr.
    IF sy-subrc IS  INITIAL.
      SORT gt_skb1.
    ENDIF.

    SELECT ktopl
           saknr
           txt20 FROM skat
      INTO TABLE gt_skat
      FOR ALL ENTRIES IN gt_ska1
      WHERE ktopl = gt_ska1-ktopl
       AND saknr = gt_ska1-saknr
       AND spras = sy-langu.
    IF sy-subrc IS INITIAL.
      SORT gt_skat.
    ENDIF.

    SELECT kstar
      FROM cskb
      INTO TABLE gt_cskb
       FOR ALL ENTRIES IN gt_ska1
     WHERE kstar = gt_ska1-saknr
      AND datbi >= sy-datum
      AND datab <= sy-datum.
    IF sy-subrc IS INITIAL.
      SORT gt_cskb BY kstar ASCENDING.
    ENDIF.

  ENDIF.

* Sort the Internal Tables
  SORT : gt_ska1 ASCENDING BY saknr,
         gt_skb1 ASCENDING BY saknr,
         gt_skat ASCENDING BY saknr.


ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DIS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_dis_data .

  IF p_dis =  gc_x.
    PERFORM f_display_data.  " To display or Place the file in Application layer.
  ELSE.
    PERFORM f_transport_data. " To place the file in Application layer.
  ENDIF.
*--> Log File
  PERFORM f_write_log.
ENDFORM.                    " F_DIS_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_format_data .

  REFRESH  gt_data.

  DATA: ls_data TYPE gty_data,
        ls_ska1 TYPE gty_ska1,
        ls_skb1 TYPE gty_skb1,
        ls_cskb TYPE gty_cskb,
        ls_skat TYPE gty_skat.

  CLEAR :ls_data,ls_ska1,ls_skb1,ls_skat,ls_cskb.

  LOOP AT gt_skb1 INTO ls_skb1.

    MOVE : p_erpid       TO ls_data-col1,  " ERP ID
           ls_skb1-bukrs TO ls_data-col2,  " Company Code
           ls_skb1-fstag TO ls_data-col7.  " Field Status Group

    READ TABLE gt_ska1 INTO ls_ska1 WITH KEY saknr = ls_skb1-saknr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      MOVE : ls_ska1-saknr TO ls_data-col3.
    ELSE.
      CLEAR ls_data-col3.
    ENDIF.

*--> Logicall Ststus Flag
    IF ls_ska1-xspeb = gc_x OR ls_skb1-xspeb = gc_x OR ls_skb1-xintb = gc_x.
      ls_data-col6 = '0'. " Set to zero
    ELSE.
      ls_data-col6 = '1'. " Set to one
    ENDIF.

    READ TABLE gt_skat INTO ls_skat WITH KEY saknr = ls_skb1-saknr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      PERFORM f_text_check CHANGING ls_skat-txt20. " To check for any Spl chars.
      MOVE : ls_skat-txt20 TO ls_data-col4.
    ELSE.
      CLEAR ls_skat.
    ENDIF.
*-->Check for G/L Account (Accounting) is also a Cost Element (Controlling).
    READ TABLE gt_cskb INTO ls_cskb WITH KEY kstar = ls_skat-saknr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      ls_data-col5 = gc_x.
    ELSE.
      ls_data-col5 = ''.
    ENDIF.

    APPEND ls_data TO gt_data.
    CLEAR :ls_data,ls_ska1,ls_skb1,ls_skat,ls_cskb.
  ENDLOOP.

ENDFORM.                    " F_FORMAT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_display_data .

  DATA lv_file TYPE string.

  DATA: lt_tab_data       TYPE STANDARD TABLE OF string,
        ls_tab_data       TYPE string,
        ls_data           TYPE gty_data.

  CONSTANTS : lc_sep TYPE c LENGTH 2 VALUE '|~'. " for column separator

  CONCATENATE text-f01
              text-f02
              text-f03
              text-f04
              text-f05
              text-f06
              text-f07
                INTO ls_tab_data
    SEPARATED BY lc_sep.
  APPEND ls_tab_data TO lt_tab_data.
  CLEAR ls_data.
  LOOP AT  gt_data INTO  ls_data.
    CLEAR ls_tab_data.
    zcl_iap_interface_util=>add_pipes( EXPORTING im_rec = ls_data
    IMPORTING ex_outrec = ls_tab_data ).

    APPEND ls_tab_data TO lt_tab_data.
  ENDLOOP.

  CLEAR lv_file.
  MOVE p_path1 TO lv_file.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_file
    TABLES
*     data_tab                = gt_data
      data_tab                = lt_tab_data
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
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  F_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_transport_data .

  DATA : lv_string TYPE c LENGTH  1000,
         ls_data   TYPE gty_data,
         lv_path   TYPE string.

  DATA lv_cn_recs  TYPE i.


  CLEAR : lv_string,ls_data,lv_path.

  CONSTANTS : lc_sep TYPE c LENGTH 2 VALUE '|~', " for column separator
              lc_und TYPE c VALUE '_'.

  CONCATENATE p_erpid p_file2 INTO lv_path SEPARATED BY lc_und.
  CONCATENATE p_path2 lv_path INTO lv_path.

*  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*    CONCATENATE text-f01
*                text-f02
*                text-f03
*                text-f04
*                text-f05
*                text-f07
*                text-f06
*           INTO lv_string
*   SEPARATED BY lc_sep.
*    TRANSFER lv_string to lv_path.
*    CLEAR : lv_string.

  PERFORM  f_open_dataset.

  LOOP AT gt_data INTO ls_data.

    IF ( gv_cn_recs_file GE gv_cn_recs_max ).
* Close the file
      IF   ( ( p_maxrec                    IS NOT INITIAL ) AND
             ( gv_filename                 IS NOT INITIAL )     ).

        CLOSE    DATASET gv_filename.

        CLEAR    gv_filename.
        CLEAR    gv_cn_recs_file.

      ENDIF.

* Open the file
      IF       ( gv_filename                 IS INITIAL ).

        PERFORM  f_open_dataset.

      ENDIF.

    ENDIF.

    ADD 1 TO gv_cn_recs_file.
    ADD 1 TO gv_cn_recs_total.

    CONCATENATE ls_data-col1
                ls_data-col2
                ls_data-col3
                ls_data-col4
                ls_data-col5
                ls_data-col6
                ls_data-col7
           INTO lv_string
   SEPARATED BY lc_sep.
*    TRANSFER lv_string TO lv_path.
    TRANSFER lv_string TO gv_filename.
    CLEAR : lv_string,ls_data.

  ENDLOOP.

  CLOSE DATASET gv_filename.

ENDFORM.                    " F_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_FOLDER_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_get_folder_path  CHANGING cv_file1 TYPE any.

  CONSTANTS lc_path1  TYPE filepath-pathintern VALUE 'ZFAPI102_GL ACCOUNTS'.

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

ENDFORM.                    " F_GET_FOLDER_PATH
*&---------------------------------------------------------------------*
*&      Form  F_GET_FILE_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_get_file_name  CHANGING cv_file2 TYPE any.

* Default Data File name
*  CONCATENATE text-007 sy-datum '_' sy-uzeit '.CSV' INTO CV_FILE2.
  MOVE     gc_filename    TO cv_file2.

ENDFORM.                    " F_GET_FILE_NAME
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

*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH1
*&---------------------------------------------------------------------*
*       text
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
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_write_log .

  DATA : lv_string TYPE c LENGTH  100,
         lv_records TYPE string,
         lv_lines TYPE i.
  CLEAR : lv_string, lv_lines.

  DESCRIBE TABLE  gt_data LINES lv_lines.
  lv_records = lv_lines.
  CONCATENATE text-009 lv_records INTO lv_string.
  WRITE lv_string.
  CLEAR lv_string.

ENDFORM.                    " F_WRITE_LOG
*&---------------------------------------------------------------------*
*&      Form  F_TEXT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_text_check  CHANGING cv_txt20 TYPE txt20.

  DATA : lv_string TYPE c LENGTH 50,
         lv_ind TYPE sy-index,
         lv_char TYPE c,
         lv_len TYPE i.

  CLEAR: lv_string,lv_len,lv_ind,lv_char.

  MOVE cv_txt20 TO lv_string.
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

  CLEAR  cv_txt20.
  CONDENSE lv_string.
  MOVE   lv_string TO cv_txt20.


ENDFORM.                    " F_TEXT_CHECK
*&---------------------------------------------------------------------*
*&      Form  F_SET_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_set_date .
*--> Setting Date as Sy=datum -1 to Sy-Datum.

  DATA lv_date TYPE sy-datum.
  RANGES ls_date FOR sy-datum.
  CLEAR: lv_date,ls_date.

  lv_date  = sy-datum - 1.
  ls_date-low = lv_date.
  ls_date-high = sy-datum.
  ls_date-sign = 'I'.
  ls_date-option = 'EQ'.
  APPEND ls_date TO s_date.


ENDFORM.                    " F_SET_DATE
*&---------------------------------------------------------------------*
*&      Form  F_INITIAL_DATA_ELEMENTS
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements .

  DATA:    lv_filename                 TYPE text128,
           lv_date_time_stamp          TYPE char10.


  CLEAR    gv_cn_recs_total.
  CLEAR    gv_cn_recs_max.
  CLEAR    gv_cn_recs_file.
  CLEAR    gv_cn_files.
  CLEAR    gv_filename.
  CLEAR    gv_filename_p.

*eject
* Set the maximum number of characters in a file
  gv_cn_recs_max = p_maxrec.

* Set the filename
  IF   ( ( p_app                         IS NOT INITIAL ) AND
         ( p_file2                       IS NOT INITIAL )     ).

    CLEAR                                   lv_filename.
    MOVE       p_file2                   TO lv_filename.

    IF     ( ( lv_filename               CS 'SSSSS'     ) AND
             ( p_erpid                   IS NOT INITIAL )     ).
      REPLACE  'SSSSS'                   IN lv_filename
                                       WITH p_erpid.
    ENDIF.

    IF       ( lv_filename               CS 'YYMMDDHHMM' ).
      CLEAR                                 lv_date_time_stamp.
      MOVE     sy-datum+2(6)             TO lv_date_time_stamp+0(6).
      MOVE     sy-uzeit+0(4)             TO lv_date_time_stamp+6(4).
      REPLACE  'YYMMDDHHMM'              IN lv_filename
                                       WITH lv_date_time_stamp.
    ENDIF.

    MOVE       lv_filename               TO gv_filename_p.

  ENDIF.
ENDFORM.                    " F_INITIAL_DATA_ELEMENTS
*&---------------------------------------------------------------------*
*&      Form  F_OPEN_DATASET
*&---------------------------------------------------------------------*
*       Open dataset
*----------------------------------------------------------------------*
FORM f_open_dataset .

  DATA:    lv_filename                 TYPE text256,
           lv_numc3                    TYPE numc3,
           lv_numc4                    TYPE numc4,
           lv_msg                      TYPE text100.

  ADD        1                           TO gv_cn_files.

  CLEAR                                     lv_filename.
  MOVE       gv_filename_p               TO lv_filename.

  IF       ( lv_filename                 CS 'NNNN' ).
    CLEAR                                   lv_numc4.
    MOVE     gv_cn_files                 TO lv_numc4.
    REPLACE  'NNNN'                      IN lv_filename
                                       WITH lv_numc4.
  ELSEIF   ( lv_filename                 CS 'NNN'  ).
    CLEAR                                   lv_numc3.
    MOVE     gv_cn_files                 TO lv_numc3.
    REPLACE  'NNN'                       IN lv_filename
                                       WITH lv_numc3.
  ENDIF.

  CLEAR                                     gv_filename.
  CONCATENATE   p_path2   lv_filename  INTO gv_filename.

  OPEN     DATASET gv_filename FOR OUTPUT IN TEXT MODE
                               ENCODING DEFAULT MESSAGE lv_msg.
  IF     ( sy-subrc NE 0 ).
    WRITE:   /001                 text-093, lv_msg.
    MESSAGE  e000(zfi01)     WITH text-093  lv_msg.
  ENDIF.

  PERFORM  f_create_header_rec.

ENDFORM.                    " F_OPEN_DATASET
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_HEADER_REC
*&---------------------------------------------------------------------*
*       Create the header record
*----------------------------------------------------------------------*
FORM f_create_header_rec .
  DATA:    ls_output                   TYPE string.

  CLEAR                                     ls_output.

  CONCATENATE text-f01
                text-f02
                text-f03
                text-f04
                text-f05
                text-f06
                text-f07
           INTO ls_output
   SEPARATED BY gc_delim.

  IF       ( p_app                      IS NOT INITIAL ).
    TRANSFER ls_output                   TO gv_filename.
  ELSEIF   ( p_dis                      IS NOT INITIAL ).
*    APPEND   ls_output                   TO gt_output.
  ENDIF.

  ADD      1                             TO gv_cn_recs_file.
  ADD      1                             TO gv_cn_recs_total.

ENDFORM.                    " F_CREATE_HEADER_REC
