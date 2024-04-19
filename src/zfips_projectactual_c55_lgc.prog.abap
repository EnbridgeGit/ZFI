*&---------------------------------------------------------------------*
*&  Include           ZFIPS_PROJECTACTUAL_C55_LGC
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZFIPS_PROJECTACTUAL_C55                        *
* Include            :  ZFIPS_PROJECTACTUAL_C55_LGC                    *
* Author             :  Rajeshwar Reddy                                *
* Date               :  12th-Nov-2019                                  *
* Technical Contact  :  Rajeshwar Reddy                                *
* Business Contact   :                                                 *
* Purpose            :  WNew Interface from SAP to C55                 *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 12th-Nov-2019  JOOKONTR    D30K930280 CHG0160132 Initial Development *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 9th-Jul-2020  KMB     D30K930635      CHG0185357 ENHC0029276: YTD    *
*                                       Project actual costs extract to*
*                                       IDF                            *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 22nd-SEP-2020  KMB         D30K930689 CHG0193497 IDF performance     *
*&---------------------------------------------------------------------*


INITIALIZATION.

  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/C55/Archive/' INTO csvfile.
  "BOC by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
  CONCATENATE '/usr/sap/interfaces/' sy-sysid+0(3) '/C55/Archive/' INTO csvfile1.
  "EOC by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.


  "BOC by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file1.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file1 = wit_filename_tab.
    ELSE.
      CLEAR p_file1.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file1.
  IF p_local1 = 'X' AND p_idf = 'X'.
    PERFORM check_file_path1.
  ENDIF.
  "EOC by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF

AT SELECTION-SCREEN ON p_file.
*BOC by KMB on 22.9.2020 CHG0193497 IDF performance
*  IF p_local = 'X'.
  IF p_local = 'X' AND p_c55 = 'X'.
*EOC by KMB on 22.9.2020 CHG0193497 IDF performance
    PERFORM check_file_path.
  ENDIF.

START-OF-SELECTION.

  IF p_c55 = 'X'. ""Added by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
    CONCATENATE csvfile 'capex.tch' INTO tuchfile.
    CONCATENATE csvfile 'C55' '_' p_perio '_' sy-datum '_' sy-uzeit '.csv' INTO csvfile.
    IF p_local = 'X'.
      lv_local = 1.
    ELSE.
      lv_local = 0.
    ENDIF.

    PERFORM get_db_data.
    PERFORM sumarize_data.
    PERFORM print_report.
    PERFORM save_file.

*BOC by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
  ENDIF.
  IF p_idf = 'X'.
    CONCATENATE csvfile1 'capex.tch' INTO tuchfile1.
*BOC by KMB on 22.9.2020 CHG0193497 IDF performance
*    CONCATENATE csvfile1 'C55' '_' p_perio '_' sy-datum '_' sy-uzeit '.csv' INTO csvfile1.
    CONCATENATE csvfile1 'IDF' '_' p_perio '_' sy-datum '_' sy-uzeit '.csv' INTO csvfile1.
*EOC by KMB on 22.9.2020 CHG0193497 IDF performance
    IF p_local1 = 'X'.
      lv_local1 = 1.
    ELSE.
      lv_local1 = 0.
    ENDIF.

    PERFORM get_db_data1.
    PERFORM sumarize_data1.
    PERFORM PRINT_REPORT1.
    PERFORM save_file1.
  ENDIF.
*EOC by kmb on 9.7.2020 CHG0185357 ENHC0029276: YTD Project actual costs extract to IDF
