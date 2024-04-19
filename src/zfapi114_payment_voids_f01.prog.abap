*&---------------------------------------------------------------------*
*& Report  ZFAPI114_PAYMENT_VOIDS_F01                                  *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZFAPI114_PAYMENT_VOIDS                         *
* Include            :  ZFAPI114_PAYMENT_VOIDS_F01                     *
* Author             :  Vijay Rajaputra                                *
* Date               :  05-Mar-2018                                    *
* Technical Contact  :  Vijay Rajaputra                                *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  ATCAT Payment Voids Extract Interface          *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 05-Mar-2018  VRAJAPUTRA    D30K928605 CHG0100819 Initial Development *
*                            D30K928780                                *
*&-------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_FOLDER_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_FOLDER_PATH CHANGING cv_file1 TYPE ANY.

  CONSTANTS:  lc_path1  TYPE filepath-pathintern VALUE 'ZFAPI114_PAYMENT_VOID'.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      LOGICAL_FILENAME = lc_path1
    IMPORTING
      FILE_NAME        = cv_file1
    EXCEPTIONS
      FILE_NOT_FOUND   = 1
      OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " F_GET_FOLDER_PATH

*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_F4_HELP_FILE_PATH .

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

FORM F_GET_F4_HELP_FILE_PATH1 .

  DATA: lv_repid TYPE sy-repid,
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

*&---------------------------------------------------------------------*
*&      Form  F_GET_FILE_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_FILE_NAME  CHANGING CV_FILE2 TYPE any.

* Default Data File name
  CONCATENATE text-007 sy-datum '_' sy-uzeit '.CSV' INTO CV_FILE2.

ENDFORM.                    " F_GET_FILE_NAME

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_GET_DATA .

  DATA: ls_cdhdr  TYPE gty_cdhdr,
        ls_object TYPE gty_object.

  DATA: lv_lifnr  TYPE lifnr.

  CONSTANTS: lc_BELEGR TYPE c LENGTH 6 VALUE 'BELEGR',
             lc_tcode  TYPE c LENGTH 4 VALUE 'FBRA'.

  REFRESH gt_data.
  CLEAR : ls_cdhdr, ls_object.

  REFRESH gt_object.

  SELECT objectid FROM cdhdr
    INTO TABLE gt_object
   WHERE OBJECTCLAS  = lc_belegr
     AND UDATE      IN s_date
     AND TCODE       = lc_tcode.
  IF sy-subrc is INITIAL.
    SORT gt_object.
  ENDIF.

  CLEAR                  ls_object.
  LOOP AT gt_object INTO ls_object.

    CLEAR  ls_cdhdr.
    ls_cdhdr-bukrs = ls_object-objectid+03(04). " BUKRS
    ls_cdhdr-augbl = ls_object-objectid+07(10). " AUGBL
    ls_cdhdr-gjahr = ls_object-objectid+17(04). " GJAHR

    CLEAR    lv_lifnr.
    SELECT   SINGLE lifnr
      INTO   lv_lifnr
      FROM   bseg
     WHERE   bukrs = ls_cdhdr-bukrs
       AND   belnr = ls_cdhdr-augbl
       AND   gjahr = ls_cdhdr-gjahr
       AND   koart = 'K'.
    IF     ( sy-subrc EQ 0 ).
      ls_cdhdr-lifnr = lv_lifnr.
    ENDIF.

    CONCATENATE ls_cdhdr-gjahr '0101' INTO ls_cdhdr-ldate.
    CONCATENATE ls_cdhdr-gjahr '1231' INTO ls_cdhdr-hdate.

    APPEND ls_cdhdr TO gt_cdhdr.

    CLEAR  ls_object.
  ENDLOOP.

  SORT  gt_cdhdr by BUKRS AUGBL GJAHR .
  DELETE ADJACENT DUPLICATES FROM gt_cdhdr COMPARING BUKRS AUGBL GJAHR. " To remove Multiple Entries.

*--> Need to fetch corresponding Only Payment info from Reguh table for corresponding records
  SELECT ZBUKR
         VBLNR
         ZALDT
    FROM REGUH
    INTO TABLE gt_reguh
     FOR ALL ENTRIES IN gt_cdhdr
   WHERE zbukr = gt_cdhdr-bukrs
     AND lifnr = gt_cdhdr-lifnr
     AND vblnr = gt_cdhdr-augbl.
*    AND ZALDT BETWEEN gt_cdhdr-ldate and gt_cdhdr-hdate.

ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_FORMAT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_FORMAT_DATA .

  DATA : ls_reguh TYPE gty_reguh,
         ls_cdhdr TYPE gty_cdhdr,
         ls_data  TYPE gty_data.

  CONSTANTS : lc_sep TYPE c VALUE '_'.

  CLEAR: ls_reguh, ls_data.

  SORT gt_reguh BY zbukr vblnr ASCENDING.

  LOOP AT gt_cdhdr INTO ls_cdhdr.

    READ TABLE gt_reguh INTO ls_reguh WITH KEY zbukr = ls_cdhdr-bukrs
                                               vblnr = ls_cdhdr-augbl BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      IF ls_reguh-zaldt GE ls_cdhdr-ldate AND " First Day of the Year
         ls_reguh-zaldt LE ls_cdhdr-hdate.    " Last Day of the Year
*--> ERP ID
*       ls_data-erpid = p_erpid.
*--> Col 1
        CONCATENATE ls_cdhdr-augbl " vblnr
                    ls_cdhdr-bukrs " zbukr
                    ls_cdhdr-gjahr " zaldt+0(4)
               INTO ls_data-col1
       SEPARATED BY lc_sep.
*--> Col 2
        ls_data-col2 = ls_cdhdr-augbl. " vblnr

        APPEND: ls_data to gt_data.
        CLEAR ls_data.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_FORMAT_DATA

*&---------------------------------------------------------------------*
*&      Form  F_DIS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_DIS_DATA .

  IF p_dis =  'X'.
    PERFORM f_display_data.  " To display or Place the file in Application layer.
  else.
    perform f_transport_data. " To place the file in Application layer.
  endif.

ENDFORM.                    " F_DIS_DATA

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_DISPLAY_DATA .

  DATA lv_file type string.
  CLEAR lv_file.

  MOVE P_Path1 to lv_file.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = lv_file
    TABLES
      DATA_TAB                = gt_data
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
  IF SY-SUBRC <> 0.
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
**--> Log File
    CLEAR                        gv_cn_recs.
    DESCRIBE TABLE gt_data LINES gv_cn_recs.
    PERFORM f_write_log.
  ENDIF.

ENDFORM.                    " F_DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  F_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_TRANSPORT_DATA .

  DATA : lv_string  TYPE c LENGTH  1000,
         ls_data    TYPE gty_data,
         lv_message TYPE string,
         lv_path    TYPE string.

  CLEAR : gv_cn_recs.

  CLEAR : lv_string, ls_data, lv_path.

  CONSTANTS : lc_sep TYPE c LENGTH 2 VALUE '|~', " for column separator
              lc_und TYPE c VALUE '_'.

  CONCATENATE p_erpid  p_file2 INTO lv_path SEPARATED BY lc_und.
  CONCATENATE p_path2 lv_path INTO lv_path.

  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_message.
  IF sy-subrc IS INITIAL.

    CONCATENATE       text-c01         text-c02
                                  INTO lv_string
                          SEPARATED BY lc_sep.
    TRANSFER    lv_string           TO lv_path.
    ADD         1                   TO gv_cn_recs.
    CLEAR : lv_string.

    LOOP AT  gt_data              INTO ls_data.

      CONCATENATE     ls_data-col1     ls_data-col2
                                  INTO lv_string
                          SEPARATED BY lc_sep.
      TRANSFER  lv_string           TO lv_path.
      ADD       1                   TO gv_cn_recs.
      CLEAR : lv_string, ls_data.

    ENDLOOP.

    CLOSE DATASET lv_path.

*--> Log File
    PERFORM f_write_log.
  ELSE.
    WRITE: Text-005, lv_message.
    EXIT.
  ENDIF.

ENDFORM.                    " F_TRANSPORT_DATA

*&---------------------------------------------------------------------*
*&      Form  F_WRITE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_WRITE_LOG .
  DATA : lv_string TYPE c LENGTH  100,
         lv_records TYPE string,
         lv_lines TYPE i.
  CLEAR : lv_string, lv_lines.

  lv_records = gv_cn_recs.
  CONCATENATE text-009 lv_records INTO lv_string.
  WRITE lv_string.
  CLEAR lv_string.

ENDFORM.                    " F_WRITE_LOG
