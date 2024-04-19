*&---------------------------------------------------------------------*
*&  Include           ZFAPI124_SRVC_ENTRY_SHEET_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       : ZFAPI124_SRVC_ENTRY_SHEET                       *
* Author             :                                                 *
* Date               : Mar 26, 2018                                    *
* Technical Contact  : Paul Karunakar                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            : The Service Entry Sheet (SES) master data       *
*                      from each of the three SAP instances will be    *
*                      extracted in a delimited file and sent to IAP.  *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 26-Mar-2018  KPAL        D30K928726  CHG0106361  Initial Development *
*                          D30K928730, D30K928878, D30K928919          *
*                          D30K928927                                  *
*----------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_filepath_filename
*&---------------------------------------------------------------------*
*       Get the application server filepath and filename
*----------------------------------------------------------------------*
FORM f_get_filepath_filename
  CHANGING cv_filepath  TYPE string
           cv_filename  TYPE string.

  CLEAR    cv_filepath.
  CLEAR    cv_filename.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = gc_lgcfilpth
    IMPORTING
      file_name        = cv_filepath
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  IF     ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE     gc_filename    TO cv_filename.

ENDFORM.                    " f_get_filepath_filename
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_f4_help_file_path
*&---------------------------------------------------------------------*
FORM f_get_f4_help_file_path.

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    IMPORTING
      serverfile       = p_path2
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.
  IF     ( sy-subrc NE 0 ).
    MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_get_f4_help_file_path
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_f4_help_file_path1
*&---------------------------------------------------------------------*
FORM f_get_f4_help_file_path1.

  DATA: lv_repid                       TYPE sy-repid,
        lv_dynnr                       TYPE sy-dynnr,
        lv_file                        TYPE rlgrap-filename.

  CLEAR                                     lv_repid.
  MOVE     sy-repid                      TO lv_repid.
  CLEAR                                     lv_dynnr.
  MOVE     sy-dynnr                      TO lv_dynnr.

  CLEAR    lv_file.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = lv_repid
      dynpro_number = lv_dynnr
    CHANGING
      file_name     = lv_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF     ( sy-subrc EQ 0 ).

    MOVE   lv_file   TO p_path1.

  ELSE.

    MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.                    " f_get_f4_help_file_path1
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

  DATA:    lv_filename                 TYPE text128,
           lv_date_time_stamp          TYPE char10.

  CLEAR    gt_essr[].
  CLEAR    gt_output[].

  CLEAR    gv_cn_recs_total.
  CLEAR    gv_cn_recs_file.
  CLEAR    gv_cn_files.
  CLEAR    gv_filename.
  CLEAR    gv_filename_p.

* Set the maximum number of records in a file
  gv_cn_recs_max = p_maxrec.

* Set the filename
  IF   ( ( rb_appl                       IS NOT INITIAL ) AND
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

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_main
*&---------------------------------------------------------------------*
*       Main process
*----------------------------------------------------------------------*
FORM f_process_main.

  DATA:    lv_cn_recs   TYPE char12.

* Open the application server dataset
  IF       ( rb_appl      IS NOT INITIAL ).
    PERFORM  f_open_dataset.
  ELSEIF   ( rb_pres      IS NOT INITIAL ).
    PERFORM  f_create_header_rec.
  ENDIF.

* Get the data
  PERFORM  f_get_data.

* Close the application server dataset
  IF       ( rb_appl      IS NOT INITIAL ).
    IF     ( gv_filename  IS NOT INITIAL ).
      CLOSE  DATASET gv_filename.
    ENDIF.
  ENDIF.

* Download the output data
  IF       ( rb_pres      IS NOT INITIAL ).
    PERFORM  f_download_output.
  ENDIF.

* Write the record counts to the spool
  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_total              TO lv_cn_recs.
  WRITE:  /001 text-061,                035 lv_cn_recs.

ENDFORM.                    " f_process_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
*       Get the data
*----------------------------------------------------------------------*
FORM f_get_data.

  DATA:    ls_entrysheet_header        TYPE bapiessr,
           lt_ses_accass               TYPE gtt_ses_accass,
           lt_ekbe                     TYPE gtt_ekbe,
           ls_ekbe                     TYPE gty_ekbe,
           ls_final                    TYPE gty_final,
           ls_output                   TYPE gty_output.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_inv_ind                  TYPE xflag,
           lv_amount                   TYPE char14,
           lv_percnt                   TYPE char7,
           lv_aavalu                   TYPE char30,
           lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_essr>             TYPE gty_essr,
                 <fs_ses_accass>       TYPE gty_ses_accass,
                 <fs_final>            TYPE gty_final,
                 <fs_attr>             TYPE ANY.

  ASSIGN   ls_final                      TO <fs_final>.

  DESCRIBE FIELD <fs_final> TYPE lv_dtype COMPONENTS lv_cn_comp.

  CLEAR    gt_essr[].

  IF     ( rb_fload IS NOT INITIAL ).

    SELECT   lblni  erdat  aedat  lwert  waers  packno
             txz01  ebeln  ebelp  loekz  kzabn
      INTO   TABLE gt_essr
      FROM   essr
     WHERE   lblni IN s_lblni.

  ELSEIF ( rb_dload IS NOT INITIAL ).

    SELECT   lblni  erdat  aedat  lwert  waers  packno
             txz01  ebeln  ebelp  loekz  kzabn
      INTO   TABLE gt_essr
      FROM   essr
     WHERE   lblni IN s_lblni
       AND ( erdat IN s_date OR
             aedat IN s_date ).

  ENDIF.

*eject
  IF     ( p_date                        IS NOT INITIAL ).

    LOOP AT  gt_essr              ASSIGNING <fs_essr>.
      lv_tabix = sy-tabix.
      IF     ( ( <fs_essr>-erdat         LT p_date ) AND
               ( <fs_essr>-aedat         LT p_date )     ).
        DELETE   gt_essr              INDEX lv_tabix.
      ENDIF.
    ENDLOOP.

  ENDIF.

  LOOP AT  gt_essr                ASSIGNING <fs_essr>.

    CLEAR    ls_entrysheet_header.
    CLEAR    lt_ses_accass[].

    CALL FUNCTION 'BAPI_ENTRYSHEET_GETDETAIL'
      EXPORTING
        entrysheet                    = <fs_essr>-lblni
        long_texts                    = ' '
      IMPORTING
        entrysheet_header             = ls_entrysheet_header
      TABLES
        entrysheet_account_assignment = lt_ses_accass.

    LOOP AT  lt_ses_accass        ASSIGNING <fs_ses_accass>.

      CLEAR    lv_inv_ind.

      IF     ( ls_entrysheet_header-po_number IS NOT INITIAL ).

        CLEAR    lt_ekbe[].
        SELECT   ebeln  ebelp  vgabe  lfbnr
          INTO   TABLE lt_ekbe
          FROM   ekbe
         WHERE   ebeln = ls_entrysheet_header-po_number
           AND   ebelp = ls_entrysheet_header-po_item
           AND   zekkn = <fs_ses_accass>-serial_no
           AND   vgabe = '2'
           AND   lfbnr = <fs_essr>-lblni.
        IF     ( sy-subrc EQ 0 ).
          lv_inv_ind = abap_true.
        ENDIF.

      ENDIF.

*eject
      CLEAR                                 lv_amount.
      WRITE  <fs_essr>-lwert             TO lv_amount
                                   CURRENCY <fs_essr>-waers.
      TRANSLATE                             lv_amount USING ', - '.
      CONDENSE                              lv_amount NO-GAPS.
      IF   ( <fs_essr>-lwert             LT 0 ).
        CONCATENATE   '-'   lv_amount  INTO lv_amount.
      ENDIF.

      CLEAR                                 lv_percnt.
      WRITE  <fs_ses_accass>-percentage  TO lv_percnt
                           NO-SIGN DECIMALS 1.
      TRANSLATE                             lv_percnt USING ', - '.
      CONDENSE                              lv_percnt NO-GAPS.
      IF   ( <fs_ses_accass>-percentage  LT 0 ).
        CONCATENATE   '-'   lv_percnt  INTO lv_percnt.
      ENDIF.

      CLEAR                                 lv_aavalu.
      WRITE  <fs_ses_accass>-accass_val  TO lv_aavalu
                                   DECIMALS 4.
      TRANSLATE                             lv_aavalu USING ', - '.
      CONDENSE                              lv_aavalu NO-GAPS.
      IF   ( <fs_ses_accass>-accass_val  LT 0 ).
        CONCATENATE   '-'   lv_aavalu  INTO lv_aavalu.
      ENDIF.

      PERFORM  f_rmv_splcs           CHANGING <fs_essr>-txz01.

*eject
      CLEAR                                 ls_final.
      MOVE   p_erpid                     TO ls_final-erpid.
      MOVE   <fs_essr>-lblni             TO ls_final-lblni.
      MOVE   <fs_essr>-txz01             TO ls_final-txz01.
      MOVE   <fs_essr>-ebeln             TO ls_final-ebeln.
      MOVE   <fs_essr>-ebelp             TO ls_final-ebelp.
      MOVE   <fs_essr>-loekz             TO ls_final-loekz.
      MOVE   <fs_essr>-kzabn             TO ls_final-kzabn.
      MOVE   lv_amount                   TO ls_final-lwert.
      MOVE   <fs_essr>-waers             TO ls_final-waers.
      MOVE   <fs_essr>-packno            TO ls_final-packno.
      MOVE   <fs_ses_accass>-serial_no   TO ls_final-serial_no.
      MOVE   <fs_ses_accass>-delete_ind  TO ls_final-delete_ind.
      MOVE   lv_percnt                   TO ls_final-percentage.
      MOVE   lv_aavalu                   TO ls_final-accass_val.
      MOVE   <fs_ses_accass>-gl_account  TO ls_final-gl_account.
      MOVE   <fs_ses_accass>-costcenter  TO ls_final-costcenter.
      MOVE   <fs_ses_accass>-order       TO ls_final-order.
      MOVE   <fs_ses_accass>-co_area     TO ls_final-co_area.
      MOVE   <fs_ses_accass>-prof_seg    TO ls_final-prof_seg.
      MOVE   <fs_ses_accass>-tax_code    TO ls_final-tax_code.
      MOVE   lv_inv_ind                  TO ls_final-inv_ind.

*     IF ( ( <fs_essr>-loekz             IS INITIAL ) AND
*          ( <fs_ses_accass>-delete_ind  IS INITIAL )     ).
*       MOVE '1'                         TO gs_final-status.
*     ELSE.
*       MOVE '0'                         TO gs_final-status.
*     ENDIF.

*eject
      CLEAR    ls_output.

      DO lv_cn_comp TIMES.

        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_final> TO <fs_attr>.
        IF     ( sy-index EQ 1 ).
          ls_output = <fs_attr>.
        ELSE.
          CONCATENATE ls_output <fs_attr>
                 INTO ls_output SEPARATED BY gc_delim.
        ENDIF.

      ENDDO.

      IF     ( rb_pres                   IS NOT INITIAL ).

        APPEND   ls_output               TO gt_output.

        ADD      1                       TO gv_cn_recs_total.

      ELSEIF ( rb_appl                   IS NOT INITIAL ).

        IF       ( gv_cn_recs_file       GE gv_cn_recs_max ).

* Close the file and open a new file
          IF   ( ( p_maxrec              IS NOT INITIAL ) AND
                 ( gv_filename           IS NOT INITIAL )     ).

            CLOSE    DATASET gv_filename.

            CLEAR    gv_filename.
            CLEAR    gv_cn_recs_file.

            PERFORM  f_open_dataset.

          ENDIF.

        ENDIF.

        IF       ( gv_filename           IS NOT INITIAL ).

          TRANSFER   ls_output           TO gv_filename.

          ADD      1                     TO gv_cn_recs_file.
          ADD      1                     TO gv_cn_recs_total.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " f_get_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_rmv_splcs
*&---------------------------------------------------------------------*
*       Remove special characters
*----------------------------------------------------------------------*
FORM f_rmv_splcs
  CHANGING cv_text      TYPE any.

  DATA:    lv_text(200) TYPE c,
           lv_len       TYPE syindex,
           lv_ptr       TYPE syindex,
           lv_char      TYPE char1.

  CONSTANTS:
           lc_sep1(2)   TYPE c VALUE '|~',
           lc_sep2(2)   TYPE c VALUE '~|',
           lc_sep3(2)   TYPE c VALUE '|-',
           lc_sep4(2)   TYPE c VALUE '-|'.

  CLEAR                                     lv_text.
  MOVE     cv_text                       TO lv_text.

  REPLACE  ALL OCCURRENCES OF lc_sep1    IN lv_text WITH lc_sep3.
  REPLACE  ALL OCCURRENCES OF lc_sep2    IN lv_text WITH lc_sep4.

  lv_len = strlen( lv_text ).

  DO lv_len TIMES.
    lv_ptr = sy-index - 1.
    CLEAR                         lv_char.
    MOVE     lv_text+lv_ptr(1) TO lv_char.
    IF     ( lv_char LT ' ' ).
      MOVE   SPACE             TO lv_text+lv_ptr(1).
    ENDIF.
  ENDDO.

  CLEAR               cv_text.
  MOVE     lv_text TO cv_text.

ENDFORM.                    " f_rmv_splcs
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_dataset
*&---------------------------------------------------------------------*
*       Open the application server dataset
*----------------------------------------------------------------------*
FORM f_open_dataset.

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

ENDFORM.                    " f_open_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_header_rec
*&---------------------------------------------------------------------*
*       Create the header record
*----------------------------------------------------------------------*
FORM f_create_header_rec.

  DATA:    ls_output                   TYPE gty_output.

  CLEAR                                     ls_output.
  CONCATENATE text-f01  text-f02  text-f03  text-f04
              text-f05  text-f06  text-f07  text-f08
              text-f09  text-f10  text-f11  text-f12
              text-f13  text-f14  text-f15  text-f16
              text-f17  text-f18  text-f19  text-f20
              text-f21                 INTO ls_output
                               SEPARATED BY gc_delim.

  IF       ( rb_appl                     IS NOT INITIAL ).
    TRANSFER ls_output                   TO gv_filename.
  ELSEIF   ( rb_pres                     IS NOT INITIAL ).
    APPEND   ls_output                   TO gt_output.
  ENDIF.

  ADD      1                             TO gv_cn_recs_file.
  ADD      1                             TO gv_cn_recs_total.

ENDFORM.                    " f_create_header_rec
*eject
*&---------------------------------------------------------------------*
*&      Form  f_download_output
*&---------------------------------------------------------------------*
*       Download the output
*----------------------------------------------------------------------*
FORM f_download_output.

  DATA:    lv_filename  TYPE string.

  IF     ( p_path1        IS INITIAL ).
    RETURN.
  ENDIF.

  IF     ( gt_output[]    IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                      lv_filename.
  MOVE     p_path1        TO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = lv_filename
    TABLES
      DATA_TAB                = gt_output
    EXCEPTIONS
      FILE_WRITE_ERROR        = 1
      NO_BATCH                = 2
      GUI_REFUSE_FILETRANSFER = 3
      INVALID_TYPE            = 4
      NO_AUTHORITY            = 5
      UNKNOWN_ERROR           = 6
      HEADER_NOT_ALLOWED      = 7
      SEPARATOR_NOT_ALLOWED   = 8
      FILESIZE_NOT_ALLOWED    = 9
      HEADER_TOO_LONG         = 10
      DP_ERROR_CREATE         = 11
      DP_ERROR_SEND           = 12
      DP_ERROR_WRITE          = 13
      UNKNOWN_DP_ERROR        = 14
      ACCESS_DENIED           = 15
      DP_OUT_OF_MEMORY        = 16
      DISK_FULL               = 17
      DP_TIMEOUT              = 18
      FILE_NOT_FOUND          = 19
      DATAPROVIDER_EXCEPTION  = 20
      CONTROL_FLUSH_ERROR     = 21
      OTHERS                  = 22.

  IF     ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_download_output
