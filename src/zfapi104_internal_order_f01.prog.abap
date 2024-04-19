*&---------------------------------------------------------------------*
*&  Include           ZFAPI104_INTERNAL_ORDER_F01
*&---------------------------------------------------------------------*
************************************************************************
*                            Enbridge Energy                           *
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI104_INTERNAL_ORDER                       *
*& Author             :  Kalinga Keshari Rout                          *
*& Creation Date      :  January 09, 2018                              *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Program extracts master data for both cases   *
*&                       full load and delta load and file created in  *
*&                       application server  .Frequency of data upload *
*&                        weekly                                       *
*&---------------------------------------------------------------------*
*                         Modification Log                             *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* -------------------------------------------------------------------- *
* 19-Jan-2018  KROUT       D30K928564  CHG0100808  Initial Development *
*                          D30K928834, D30K928890                      *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .

*  CONSTANTS: lc_autyp_int_ord TYPE auftyp VALUE '01'.

  IF rb_fload IS NOT INITIAL .

    SELECT   aufnr ktext bukrs phas3 loekz objnr
      INTO   TABLE gt_aufk
      FROM   aufk
     WHERE   aufnr IN s_aufnr
*       AND   autyp  = lc_autyp_int_ord
       AND   autyp IN s_autyp
       AND   astkz  = SPACE.
    IF     ( sy-subrc NE 0 ).
      WRITE text-002 .
      EXIT .
    ENDIF .

  ELSE .

    SELECT   aufnr ktext bukrs phas3 loekz objnr
      INTO   TABLE gt_aufk
      FROM   aufk
     WHERE   aufnr IN s_aufnr
*       AND   autyp  = lc_autyp_int_ord
       AND   autyp IN s_autyp
       AND   astkz  = SPACE
       AND ( erdat IN s_date OR aedat IN s_date ).
    IF     ( sy-subrc NE 0 ).
      WRITE  text-002 .
      EXIT .
    ENDIF .

  ENDIF .

ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_format_data .

  DATA: lv_line     TYPE j_stext.

  LOOP AT gt_aufk INTO gs_aufk .

    MOVE-CORRESPONDING gs_aufk TO gs_final .

    gs_final-erpid = p_erpid .

*    CONCATENATE p_erpid gs_final-bukrs INTO gs_final-erp_comp
*                                       SEPARATED BY '_'.

    PERFORM check_text CHANGING gs_final-ktext .

    IF ( ( gs_aufk-phas3 IS NOT INITIAL ) OR
         ( gs_aufk-loekz IS NOT INITIAL )    ).
      gs_final-status = '0' .
    ELSE .
      gs_final-status = '1' .

      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
*         CLIENT            = SY-MANDT
*         FLG_USER_STAT     = ' '
          objnr             = gs_aufk-objnr
*         ONLY_ACTIVE       = 'X'
          spras             = sy-langu
*         BYPASS_BUFFER     = ' '
        IMPORTING
*         ANW_STAT_EXISTING =
*         E_STSMA           =
          line              = lv_line
*         USER_LINE         =
*         STONR             =
        EXCEPTIONS
          object_not_found  = 1
          OTHERS            = 2.
      IF sy-subrc EQ 0.
        IF lv_line NS 'REL'.
          gs_final-status = '0' .
        ENDIF.
        IF lv_line CS 'AALK' OR
           lv_line CS 'LKD'  OR
           lv_line CS 'CLSD' OR
           lv_line CS 'DLFL' OR
           lv_line CS 'DLT'.
          gs_final-status = '0' .
        ENDIF.
      ENDIF.

    ENDIF.

    APPEND gs_final TO gt_final .
    CLEAR : gs_final , gs_aufk  .
  ENDLOOP .

ENDFORM.                    " F_FORMAT_DATA
*&---------------------------------------------------------------------*

**&---------------------------------------------------------------------*
*&      Form  F_GET_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_PATH2  text
*      <--P_P_PATH4  text
*----------------------------------------------------------------------*
FORM f_get_file_path  CHANGING cv_file1 TYPE any .
*                               cv_file2 TYPE any.


  CONSTANTS:  lc_path1  TYPE filepath-pathintern VALUE 'ZFAPI104_INTERNALORDER'.
*

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

*  CONCATENATE p_erpid '_INTERNAL_ORDER_'  sy-datum '_' sy-uzeit '.CSV' INTO p_file2.
  MOVE     gc_filename    TO p_file2.



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
        lv_succ TYPE string..

  DATA: lt_tab_data       TYPE STANDARD TABLE OF string,
        ls_tab_data       TYPE string,
        ls_data           TYPE ty_final.

  CONSTANTS : lc_sep TYPE c LENGTH 2 VALUE '|~'. " for column separator

  CONCATENATE text-f01
*              text-f02
              text-f03
              text-f04
              text-f05
              text-f06
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
*     BIN_FILESIZE            =
      filename                = lv_file
    TABLES
*     data_tab                = gt_final
      data_tab                = lt_tab_data
*     FIELDNAMES              =
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

  ENDIF.

  p_file1 = lv_file.
***********    write output in the spool
  CLEAR lv_lines .
  DESCRIBE TABLE  gt_final LINES lv_lines.
  lv_records = lv_lines.
  CONCATENATE text-009 lv_records INTO lv_succ.
  WRITE lv_succ .

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
        lv_string TYPE string ,
        lv_lines TYPE i ,
        lv_records TYPE string,
        lv_succ TYPE string. .
  CONSTANTS :  lc_sep(2) TYPE c VALUE '|~'.

  CONCATENATE p_path2 p_erpid p_file2 INTO lv_path.

  IF lv_path IS NOT INITIAL  .
*    OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
*                          MESSAGE lv_msg_text.
*    IF sy-subrc NE 0.
*      WRITE: text-014 , lv_msg_text.
*      EXIT.
*    ENDIF.
* Transferring Data

    PERFORM  f_open_dataset.
* Transferring Data
    IF gt_final[] IS NOT INITIAL .
      LOOP AT gt_final INTO gs_final.

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


        CONCATENATE gs_final-erpid
*                    gs_final-erp_comp
                    gs_final-bukrs
                    gs_final-aufnr
                    gs_final-ktext
                    gs_final-status INTO lv_string SEPARATED BY lc_sep .

*        TRANSFER lv_string TO lv_path.
        TRANSFER lv_string TO gv_filename.
        CLEAR :gs_final , lv_string .
      ENDLOOP.
    ELSE .
*      TRANSFER lv_string TO lv_path.
      TRANSFER lv_string TO gv_filename.
    ENDIF .
* Closing the File
*    CLOSE DATASET lv_path.
    CLOSE DATASET gv_filename.

***********    write output in the spool
    CLEAR lv_lines .
    DESCRIBE TABLE  gt_final LINES lv_lines.
    lv_records = lv_lines.
    CONCATENATE text-009 lv_records INTO lv_succ.
    WRITE lv_succ .
  ENDIF .

ENDFORM.                    " F_TRANSPORT_DATA

*----------------------------------------------------------------------*
*&      Form  CHECK_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_FINAL_KTEXT  text
*----------------------------------------------------------------------*
FORM check_text  CHANGING cv_ktext TYPE aufk-ktext.

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
ENDFORM.                    " CHECK_TEXT
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
  CONCATENATE   p_path2   lv_filename  INTO gv_filename
                                       SEPARATED BY '/'.

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
*              text-f02
              text-f03
              text-f04
              text-f05
              text-f06
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
