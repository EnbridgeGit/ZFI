*&---------------------------------------------------------------------*
*&  Include           ZHR_DOA_DEMO_OUT_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DEFAULT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_default_data .



  CASE gv_desk.
    WHEN 'D30CLNT020'.
      gv_sysid = 'DHRCLNT200'.
    WHEN 'D22CLNT050'.
      gv_sysid = 'QHRCLNT100'.
    WHEN 'RQCCLNT100'.
      gv_sysid = 'RQRCLNT100'.
    WHEN OTHERS.
      gv_sysid = 'PHRCLNT100'.
  ENDCASE.


  CALL FUNCTION 'RFC_GET_TABLE_ENTRIES'
    DESTINATION
    gv_sysid
    EXPORTING
      table_name = 'PA0008'
    TABLES
      entries    = gt_pa0008.
  IF sy-subrc = 0.
    SORT  gt_pa0008.
  ENDIF.

  CALL FUNCTION 'RFC_GET_TABLE_ENTRIES'
    DESTINATION
    gv_sysid
    EXPORTING
      table_name = 'PA0105'
    TABLES
      entries    = gt_pa0105.
  IF sy-subrc = 0.
    SORT  gt_pa0105.
  ENDIF.

  CALL FUNCTION 'RFC_GET_TABLE_ENTRIES'
    DESTINATION
    gv_sysid
    EXPORTING
      table_name = 'PA0001'
    TABLES
      entries    = gt_pa0001.
  IF sy-subrc = 0.
    SORT  gt_pa0001.
  ENDIF.
  CALL FUNCTION 'RFC_GET_TABLE_ENTRIES'
    DESTINATION
    gv_sysid
    EXPORTING
      table_name = 'PA0000'
    TABLES
      entries    = gt_pa0000.
  IF sy-subrc = 0.
    SORT  gt_pa0000.
  ENDIF.
ENDFORM.                    " GET_DEFAULT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PERNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pernr.
  SELECT * FROM zfit_doa_new INTO TABLE gt_zfit_doa_new WHERE tcode IN s_tcode AND doc_type IN s_type.
  SORT gt_zfit_doa_new BY lookup_key_type.
  SORT  gt_pa0008 BY trfgr.
  LOOP AT gt_zfit_doa_new INTO gs_zfit_doa_new WHERE lookup_key_type EQ 'G'.
    READ TABLE gt_pa0008 INTO gs_pa0008 WITH KEY trfgr = gs_zfit_doa_new-lookup_key BINARY SEARCH.
    IF sy-subrc = 0.
      gv_tabix = sy-tabix.
      LOOP AT gt_pa0008 FROM gv_tabix INTO gs_pa0008."WHERE trfgr EQ gs_ZFIT_DOA_NEW-lookup_key AND endda GE sy-datum.
        IF gs_pa0008-trfgr EQ gs_zfit_doa_new-lookup_key AND gs_pa0008-endda GE sy-datum.
          gs_pernr-pernr = gs_pa0008-pernr.
          APPEND gs_pernr TO gt_pernr.
        ENDIF.
      ENDLOOP.
      CLEAR gv_tabix.
    ENDIF.
  ENDLOOP.
  LOOP AT gt_zfit_doa_new INTO gs_zfit_doa_new WHERE lookup_key_type EQ 'E'.
    gs_pernr-pernr = gs_zfit_doa_new-lookup_key.
    APPEND gs_pernr TO gt_pernr.
  ENDLOOP.
  SORT gt_pernr.
  DELETE ADJACENT DUPLICATES FROM gt_pernr.
ENDFORM.                    " GET_PERNR
*&---------------------------------------------------------------------*
*&      Form  FILL_DETAIL_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_detail_record .
  IF gt_output_final IS INITIAL.
    PERFORM header.
    CLEAR gs_output_final.
  ENDIF.
  LOOP AT gt_pernr INTO gs_pernr.

    PERFORM staus_check.
    IF gv_active EQ 'X' AND gv_flag EQ 'X'.
      gs_output-pernr = gs_pernr-pernr.

      PERFORM get_0008.
      PERFORM get_0105.

*Append the records to the int table
      IF NOT gs_output IS INITIAL .
        APPEND gs_output TO gt_output.
        CLEAR : gs_output,
                 gv_active,
                 gv_flag.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " FILL_DETAIL_RECORD
*&---------------------------------------------------------------------*
*&      Form  WRITE_TO_PRESENTATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_to_presentation .
  CONSTANTS lc_error_type TYPE msgty VALUE 'E'.
*  * Write data file to Application server
  REPLACE ALL OCCURRENCES OF 'YYYYMMDD' IN p_file1 WITH sy-datum.
  REPLACE ALL OCCURRENCES OF 'HHMMSS' IN p_file1 WITH sy-uzeit.
  IF gt_output_final[] IS NOT INITIAL .
    CLEAR gv_filename1.
    CONCATENATE p_path1
    p_file1
    INTO gv_filename1
     SEPARATED BY gc_fslash.
    IF p_test NE gc_x.
      CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
        EXPORTING
          i_field_seperator    = ','  " Comma seperator
        TABLES
          i_tab_sap_data       = gt_output_final
        CHANGING
          i_tab_converted_data = gv_text
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = gv_filename1
        CHANGING
          data_tab                = gv_text
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
    ENDIF.
  ENDIF.

ENDFORM.                    " WRITE_TO_PRESENTATION
*&---------------------------------------------------------------------*
*&      Form  WRITE_TO_APPL_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_to_appl_server .
  CLEAR gs_output_final.
* Begin of Changes by <Chaya>
  DATA: lv_text1 TYPE string,
      lv_text2 TYPE string,
      lv_text3 TYPE string,
      lv_text4 TYPE string,
      lv_text5 TYPE string,
      lv_text6 TYPE string.
* End of Changes by <Chaya>

*  DATA : lv_msg TYPE string,
*           itab type truxs_t_text_data. " Error message
*  DATA: gt_text  TYPE truxs_t_text_data.

  CONSTANTS lc_error_type TYPE msgty VALUE 'E'.
  REPLACE ALL OCCURRENCES OF 'YYYYMMDD' IN p_file2 WITH sy-datum.
  REPLACE ALL OCCURRENCES OF 'HHMMSS' IN p_file2 WITH sy-uzeit.
*  * Write data file to Application server
  IF gt_output_final[] IS NOT INITIAL .
    CLEAR gv_filename1.

* Begin of Changes by <Chaya>
    IF p_path2 IS NOT INITIAL.

      TRANSLATE p_path2 TO LOWER CASE.

      SPLIT p_path2 AT '/' INTO lv_text1 lv_text2 lv_text3 lv_text4 lv_text5 lv_text6.

      IF lv_text5 EQ 'd30'.
        REPLACE 'd30' WITH 'D30' INTO p_path2.
      ELSEIF lv_text5 EQ 'd22'.
        REPLACE 'd22' WITH 'D22' INTO p_path2.
      ELSEIF lv_text5 EQ 'p01'.
        REPLACE 'p01' WITH 'P01' INTO p_path2.
      ENDIF.

      REPLACE 'pmo_ps' WITH 'PMO_PS' INTO p_path2.
      REPLACE 'out' WITH 'Out' INTO p_path2.
      REPLACE 'zdoa' WITH 'ZDOA' INTO p_path2.

    ENDIF.
* End of Changes by <Chaya>

    CONCATENATE p_path2
    p_file2
    INTO gv_filename1
     SEPARATED BY gc_fslash.

    OPEN DATASET gv_filename1 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      MESSAGE 'Unable to create file' TYPE 'I'.
      EXIT.
    ENDIF.
    IF p_test NE gc_x.

      LOOP AT gt_output_final INTO gs_output_final.
        CONDENSE gs_output_final-asl.
        CONCATENATE gs_output_final-username
                        gs_output_final-asl INTO gv_data SEPARATED BY ','.
        TRANSFER gv_data TO gv_filename1.
        CLEAR: gs_output_final.

      ENDLOOP.

      CLOSE DATASET gv_filename1.

      PERFORM create_file_in_main.
    ENDIF.
  ENDIF.

ENDFORM.                    " WRITE_TO_APPL_SERVER
*&---------------------------------------------------------------------*
*&      Form  HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM header .
*  gwa_output-pernr = 'Employee ID'.
  gs_output_final-username = 'Username'.
  gs_output_final-asl = 'ASL'.
  APPEND gs_output_final TO gt_output_final.

ENDFORM.                    " HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_0008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_0008 .
  LOOP AT gt_pa0008 INTO gs_pa0008 WHERE pernr EQ gs_pernr-pernr AND endda GE sy-datum.
    gs_output-trfgr = gs_pa0008-trfgr.
  ENDLOOP.
ENDFORM.                    " GET_0008
*&---------------------------------------------------------------------*
*&      Form  GET_0105
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_0105 .
  LOOP AT gt_pa0105 INTO gs_pa0105 WHERE pernr EQ gs_pernr-pernr AND subty EQ 'NWID' AND endda GE sy-datum.
    gs_output-usrid_long = gs_pa0105-usrid.
  ENDLOOP.
ENDFORM.                    " GET_0105
*&---------------------------------------------------------------------*
*&      Form  STAUS_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM staus_check .
  LOOP AT gt_pa0000 INTO gs_pa0000 WHERE pernr EQ gs_pernr-pernr AND  endda GE sy-datum.". and STAT2 eq '3'.
*    if sy-subrc eq 0.
    IF gs_pa0000-stat2 = '3'.
      gv_active = 'X'.
    ENDIF.
  ENDLOOP.
  IF gv_active EQ 'X'.
    LOOP AT gt_pa0001 INTO gs_pa0001 WHERE pernr EQ gs_pernr-pernr AND persg IN s_eegrp." and endda ge sy-datum.
      IF gs_pa0001-endda GE sy-datum.
        gv_flag = 'X'.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " STAUS_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_EMPLOYEE_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_employee_group .

ENDFORM.                    " CHECK_EMPLOYEE_GROUP
*&---------------------------------------------------------------------*
*&      Form  CREATE_FILE_IN_ID_FOLDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
" CREATE_FILE_IN_ID_FOLDER
*&---------------------------------------------------------------------*
*&      Form  GET_FOLDER_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_PATH2  text
*----------------------------------------------------------------------*
FORM get_folder_path  CHANGING p_path.
  DATA lv_folder TYPE string.

  lv_folder = p_path.
  cl_gui_frontend_services=>directory_browse(
  CHANGING
    selected_folder      = lv_folder
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    OTHERS               = 4
    ).
  p_path = lv_folder.
ENDFORM.                    " GET_FOLDER_PATH


*&---------------------------------------------------------------------*
*&      Form  DEFAULT_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_filename .

  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
   IMPORTING
     own_logical_system                   = gv_desk
* EXCEPTIONS
*   OWN_LOGICAL_SYSTEM_NOT_DEFINED       = 1
*   OTHERS                               = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CASE gv_desk.
    WHEN 'DECCLNT200'.
      gv_path_arch = '\\SAPFILESHARE.gtna.gt.ds\FI\DEV\OUT\I_P2C_AP_127\Arch'.
      gv_path_main = '\\SAPFILESHARE.gtna.gt.ds\FI\DEV\OUT\I_P2C_AP_127\'.
      gv_path_log = '\\SAPFILESHARE.gtna.gt.ds\FI\DEV\OUT\I_P2C_AP_127\log'.

    WHEN 'QECCLNT100'.
      gv_path_arch = '\\SAPFILESHARE.gtna.gt.ds\FI\QAS\OUT\I_P2C_AP_127\Arch'.
      gv_path_main = '\\SAPFILESHARE.gtna.gt.ds\FI\QAS\OUT\I_P2C_AP_127\'.
      gv_path_log = '\\SAPFILESHARE.gtna.gt.ds\FI\QAS\OUT\I_P2C_AP_127\log'.
    WHEN OTHERS.
      gv_path_arch = '\\SAPFILESHARE.gtna.gt.ds\FI\PRD\OUT\I_P2C_AP_127\Arch'.
      gv_path_main = '\\SAPFILESHARE.gtna.gt.ds\FI\PRD\OUT\I_P2C_AP_127\'.
      gv_path_log = '\\SAPFILESHARE.gtna.gt.ds\FI\PRD\OUT\I_P2C_AP_127\log'.
  ENDCASE.

  IF p_file2 IS INITIAL OR p_path2 IS INITIAL.
    IF p_appl EQ abap_true.
*      CLEAR gwa_path.
      p_path2 = gv_path_arch.
    ENDIF.
    IF p_file2 IS INITIAL.
      CONCATENATE text-p10
                    'YYYYMMDD' '_'
                    'HHMMSS' '_'
                  'Spectra' '.csv'
                  INTO p_file2.
    ENDIF.
  ENDIF.
  IF p_file4 IS INITIAL OR p_path4 IS INITIAL.
    IF p_appl EQ abap_true.

      p_path4 = gv_path_log.

    ENDIF.
    IF p_file4 IS INITIAL.
      CONCATENATE text-p10
                    'YYYYMMDD' '_'
                    'HHMMSS' '_'
*                  sy-datum '_'
*                  sy-uzeit '_'
                  'Spectra' '.csv'
                  INTO p_file4.
    ENDIF.
  ENDIF.
  CLEAR:p_file1,p_file3.
  IF p_test IS INITIAL.
    IF p_file1 IS INITIAL.
      CONCATENATE text-p10
                    'YYYYMMDD' '_'
                    'HHMMSS' '_'
*                  sy-datum '_'
*                  sy-uzeit '_'
                  'Spectra' '.csv'
                  INTO p_file1.
    ENDIF.
    IF p_file3 IS INITIAL.
      CONCATENATE text-p10
*                  text-y52
                    'YYYYMMDD' '_'
                    'HHMMSS' '_'
*                  sy-datum '_'
*                  sy-uzeit '_'
                  'Spectra' '.csv'
                  INTO p_file3.
    ENDIF.
  ENDIF.
  IF p_path1 IS INITIAL OR p_path3 IS INITIAL.

    p_path3 = p_path1 = 'V:'.

  ENDIF.
ENDFORM.                    " DEFAULT_FILENAME
*&---------------------------------------------------------------------*
*&      Form  CREATE_FILE_IN_MAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_file_in_main .
  CLEAR : gv_filename1.
  CONCATENATE  gv_path_main p_file2 INTO gv_filename1 SEPARATED BY gc_fslash.

  OPEN DATASET gv_filename1 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc NE 0.
    MESSAGE 'Unable to create file' TYPE 'I'.
    EXIT.
  ENDIF.
  IF p_test NE gc_x.
    LOOP AT gt_output_final INTO gs_output_final.
      CONDENSE gs_output_final-asl.
      CONCATENATE gs_output_final-username
                      gs_output_final-asl INTO gv_data SEPARATED BY ','.
      TRANSFER gv_data TO gv_filename1.
      CLEAR: gs_output_final.

    ENDLOOP.

    CLOSE DATASET gv_filename1.
  ENDIF.

ENDFORM.                    " CREATE_FILE_IN_MAIN
*&---------------------------------------------------------------------*
*&      Form  FILL_PROGRAM_STATS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PROGRAMSTATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM programstatus .

  DATA :it_msg TYPE  srm_t_solisti1.


  DATA:
    w_msg   TYPE char300,
    lv_tot     TYPE i.
  DESCRIBE TABLE gt_output_final LINES lv_tot.
  w_msg+0(11) = text-z01.
  WRITE w_msg+0(11).

  w_msg+11(30)  = sy-sysid.
  WRITE w_msg+11(30).
  w_msg+65(10) = text-z02.
  WRITE w_msg+65(10).
  w_msg+76(15)  = sy-uname.
  WRITE w_msg+76(15).
  w_msg+105(9) = text-z03.
  WRITE w_msg+105(9).
  w_msg+114(15) = 1 ."sy-pagno.
  WRITE w_msg+114(15).
  APPEND w_msg TO it_msg.
  CLEAR w_msg.
  w_msg+0(11) = text-z04.
  WRITE / w_msg+0(11).
  w_msg+12(30) = sy-cprog.
  WRITE w_msg+12(30).
  w_msg+65(9) = text-z05.
  WRITE w_msg+65(9).
  WRITE sy-datum DD/MM/YYYY TO w_msg+75(15).
  WRITE w_msg+75(15).
*  w_msg+75(15) = sy-datum.
  w_msg+105(9) = text-z06.
  WRITE w_msg+105(9).
  WRITE sy-uzeit USING EDIT MASK '__:__:__' TO w_msg+115(15).
  WRITE w_msg+115(15).
*  w_msg+114(15) = sy-uzeit.
  CONDENSE w_msg+164(15).
  APPEND w_msg TO it_msg.
  CLEAR w_msg.
  APPEND w_msg TO it_msg.

  w_msg+0(65) = text-z16.
  WRITE / w_msg+0(65).
  w_msg+65(14) = text-z13.
  WRITE w_msg+65(14).
  w_msg+79(65) = text-z16.
  WRITE w_msg+79(65).
  APPEND w_msg TO it_msg.
  CLEAR w_msg.
  w_msg+0(20) = text-z07.
  WRITE / w_msg+0(20).
  w_msg+20(10) = lv_tot.
  WRITE w_msg+20(10).
*  w_msg+65(20) = text-z21.
*  WRITE w_msg+65(20).
*  w_msg+85(10) = im_errors.
  w_msg+0(20) = text-z08.
  WRITE / w_msg+0(20).
  CLEAR gv_filename1.
  CONCATENATE p_path2
  p_file2
  INTO gv_filename1
   SEPARATED BY gc_fslash.
  WRITE / gv_filename1.
  APPEND w_msg TO it_msg.
  CLEAR :  w_msg,gv_filename1 .
  IF p_pres = abap_true AND p_test NE abap_true.
    CONCATENATE p_path3
      p_file3
      INTO gv_filename1
       SEPARATED BY gc_fslash.
  ELSEIF  p_appl = abap_true AND p_test NE abap_true.

    CONCATENATE p_path4
        p_file4
        INTO gv_filename1
         SEPARATED BY gc_fslash.
  ENDIF.

ENDFORM.                    " PROGRAMSTATUS
*&---------------------------------------------------------------------*
*&      Form  FINAL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM final_file .
  SORT  gt_zfit_doa_new BY lookup_key.
  SORT  gt_output BY pernr.
  LOOP AT gt_zfit_doa_new INTO gs_zfit_doa_new WHERE lookup_key_type EQ 'E'.
    READ TABLE gt_output INTO gs_output WITH KEY pernr = gs_zfit_doa_new-lookup_key BINARY SEARCH.
    IF sy-subrc = 0.

      LOOP AT gt_output FROM sy-tabix INTO gs_output WHERE pernr EQ gs_zfit_doa_new-lookup_key.
        IF gs_output-usrid_long IS NOT INITIAL.
          gs_output_final-username = gs_output-usrid_long.
          gs_output_final-asl = gs_zfit_doa_new-amount.
          APPEND gs_output_final TO gt_output_final.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  gt_output_final_c[] = gt_output_final[].
  LOOP AT gt_output INTO gs_output.
    CLEAR : gv_flag_c.
    READ TABLE gt_output_final_c INTO gs_output_final_c WITH KEY username = gs_output-usrid_long.

    IF sy-subrc = 0 .
      gv_amt = gs_output_final_c-asl.
      gv_flag_c = 'X'.
    ENDIF.

    LOOP AT gt_zfit_doa_new INTO gs_zfit_doa_new WHERE lookup_key_type EQ 'G' AND lookup_key EQ gs_output-trfgr.

      IF gv_flag_c = 'X'.

        IF gs_zfit_doa_new-amount GE gv_amt.
          DELETE gt_output_final WHERE username EQ gs_output-usrid_long.

          gs_output_final-username = gs_output-usrid_long.

          gs_output_final-asl = gs_zfit_doa_new-amount.
          APPEND gs_output_final TO gt_output_final.
          CLEAR : gv_flag_c.

        ENDIF.
      ELSE.
        IF gs_output-usrid_long IS NOT INITIAL.
          gs_output_final-username = gs_output-usrid_long.

          gs_output_final-asl = gs_zfit_doa_new-amount.
          APPEND gs_output_final TO gt_output_final.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " FINAL_FILE


*
