*&---------------------------------------------------------------------*
*&  Include           ZFAPI101_COMPANY_F01
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI101_COMPANY                              *
* Include Name       :   ZFAPI101_COMPANY_F01                          *
* Author             :                                                 *
* Date               :   11-Jan-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Company Codes Extraction  for IAP              *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 15-FEB-2018  VRAJAPUTRA  D30K928562 CHG0100805 - Initial development *
*                          D30K928598                                  *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_GET_BUKRS_DATA
*&---------------------------------------------------------------------*
*  FEtching Company code Deatils from BUKRS Table
*----------------------------------------------------------------------*

FORM F_GET_BUKRS_DATA  CHANGING CT_BUKRS TYPE GTT_BUKRS.

  CLEAR ct_bukrs.

  SELECT bukrs butxt land1 waers opvar
    FROM t001
    INTO TABLE ct_bukrs
   WHERE bukrs IN s_bukrs
     AND spras = sy-langu
     AND ktopl IN s_ktopl.

  IF sy-subrc is INITIAL.
    SORT ct_bukrs BY bukrs.
  ENDIF.

ENDFORM.                    " F_GET_BUKRS_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FINAL_DATA
*&---------------------------------------------------------------------*
*  Build the final data
*----------------------------------------------------------------------*

FORM F_FINAL_DATA  USING    IT_BUKRS  TYPE GTT_BUKRS
                   CHANGING CT_data   TYPE GTT_DATA.

* Local Variable declaration
  DATA : ls_bukrs TYPE gty_bukrs,
         ls_data  TYPE gty_data.

  CLEAR :ls_bukrs,ls_data.

  LOOP AT it_bukrs INTO ls_bukrs.
    PERFORM f_text_check CHANGING ls_bukrs-butxt. " To check for any Spl chars.
    MOVE: p_erpid        TO ls_data-col1,
          ls_bukrs-bukrs TO ls_data-col2,
          ls_bukrs-butxt TO ls_data-col3,
          ls_bukrs-land1 TO ls_data-col5,
          ls_bukrs-waers TO ls_data-col6.

    IF ls_bukrs-opvar = '9999'.
      ls_data-col4 = '0'.
    ELSE.
      ls_data-col4 = '1'.
    ENDIF.

    APPEND ls_data to ct_data.
    CLEAR : ls_bukrs,ls_data.
  ENDLOOP.

ENDFORM.                    " F_FINAL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*   To Ddiaply the Data
*----------------------------------------------------------------------*

FORM F_DISPLAY_DATA  USING    IT_DATA type gtt_data.

  DATA lv_file type string.
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
                INTO ls_tab_data
    SEPARATED BY lc_sep.
  APPEND ls_tab_data TO lt_tab_data.

  CLEAR ls_data.
  LOOP AT  it_data INTO  ls_data.
    CLEAR ls_tab_data.
    zcl_iap_interface_util=>add_pipes( EXPORTING im_rec = ls_data
    IMPORTING ex_outrec = ls_tab_data ).

    APPEND ls_tab_data TO lt_tab_data.
  ENDLOOP.

  CLEAR lv_file.

  MOVE p_path1 TO lv_file.

  CLEAR lv_file.

  MOVE P_Path1 to lv_file.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = lv_file
    TABLES
*     data_tab                = it_data
      data_tab                = lt_tab_data
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
*--> Log File
    PERFORM f_write_log.
  ENDIF.

ENDFORM.                    " F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  F_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM F_TRANSPORT_DATA  USING    IT_DATA TYPE gtt_data.

  DATA : lv_string  TYPE c LENGTH  1000,
         ls_data    TYPE gty_data,
         lv_message TYPE string,
         lv_path    TYPE string.

  CLEAR : lv_string, ls_data, lv_path, lv_message.

  CONSTANTS : lc_sep type c LENGTH 2 VALUE '|~', " for column separator
              lc_und TYPE c VALUE '_'.

  CONCATENATE p_erpid p_file2 INTO lv_path SEPARATED BY lc_und.
  CONCATENATE p_path2 lv_path INTO lv_path.

  " Example File name " \\SAPFileShare.gtna.gt.ds\FI\DEV\Out\I_P2C_AP_101\SAPUS_COMPANY_ERP_20180115_024251.TXT

  OPEN DATASET lv_path FOR OUTPUT IN TEXT MODE ENCODING DEFAULT MESSAGE lv_message.
  IF sy-subrc is INITIAL.
    CONCATENATE text-f01
                text-f02
                text-f03
                text-f04
                text-f05
                text-f06
             INTO lv_string
        SEPARATED BY lc_sep.
    TRANSFER lv_string TO lv_path.
    CLEAR : lv_string.
    LOOP AT it_data INTO ls_data.
      CONCATENATE ls_data-col1
                  ls_data-col2
                  ls_data-col3
                  ls_data-col4
                  ls_data-col5
                  ls_data-col6
             INTO lv_string
        SEPARATED BY lc_sep.
      TRANSFER lv_string TO lv_path.
      CLEAR : lv_string,ls_data.
    ENDLOOP.
    CLOSE DATASET lv_path.
*--> Log File
    PERFORM f_write_log.
  ELSE.
    WRITE: Text-008, lv_message.
    EXIT.
  ENDIF.

ENDFORM.                    " F_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM F_WRITE_LOG .
  DATA : lv_string  TYPE c LENGTH  100,
         lv_records TYPE string,
         lv_lines   TYPE i.
  CLEAR : lv_string, lv_lines.

  DESCRIBE TABLE  gt_data lines lv_lines.
  lv_records = lv_lines.
  CONCATENATE text-009 lv_records INTO lv_string.
  WRITE lv_string.
  CLEAR lv_string.

ENDFORM.                    " F_WRITE_LOG
*&---------------------------------------------------------------------*
*&      Form  F_FOLDER_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM F_GET_FOLDER_PATH  CHANGING CV_FILE1 type any.

  CONSTANTS  lc_path1  TYPE filepath-pathintern VALUE 'ZFAPI101_COMPANY'.

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
        lv_file type RLGRAP-FILENAME.

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
  CONCATENATE 'COMPANY_' sy-datum '_' sy-uzeit '.CSV' INTO CV_FILE2.

ENDFORM.                    " F_GET_FILE_NAME
*&---------------------------------------------------------------------*
*&      Form  F_TEXT_CHECK
*&---------------------------------------------------------------------*
*  To Check for any Spl Char available in Text and replace
*----------------------------------------------------------------------*

FORM F_TEXT_CHECK  CHANGING CV_BUTXT TYPE BUTXT.

  DATA : lv_string TYPE C LENGTH 50,
         lv_ind TYPE sy-index,
         lv_char TYPE c,
         lv_len TYPE i.

  CLEAR: lv_string,lv_len,lv_ind,lv_char.

  MOVE cv_butxt TO lv_string.
  CONDENSE lv_string.

  CONSTANTS : lc_sep1 TYPE c length 2  VALUE '|~',
              lc_sep2 TYPE c length 2  VALUE '~|',
              lc_rep1 TYPE c length 2  VALUE '|-',
              lc_rep2 TYPE c length 2  VALUE '-|'.

  REPLACE ALL OCCURRENCES OF lc_sep1 IN lv_string WITH lc_rep1.
  REPLACE ALL OCCURRENCES OF lc_sep2 IN lv_string WITH lc_rep2.

*--> Any Other Spl Chars less than Space replace by Space
  lv_len  = STRLEN( lv_string ).

  DO lv_len TIMES.
    lv_ind = sy-index - 1.
    CLEAR lv_char.
    MOVE     lv_string+lv_ind(1) TO lv_char.
    IF     ( lv_char LT ' ' ).
      MOVE   SPACE  TO lv_string+lv_ind(1).
    ENDIF.
  ENDDO.

  CLEAR  cv_butxt.
  CONDENSE lv_string.
  MOVE   lv_string TO cv_butxt.

ENDFORM.                    " F_TEXT_CHECK
