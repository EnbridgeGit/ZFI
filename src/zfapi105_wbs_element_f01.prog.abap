*&---------------------------------------------------------------------*
*&  Include           ZFAPI105_WBS_ELEMENT_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPI105_WBS_ELEMENT                          *
* Author             :                                                 *
* Date               :   Jan 10, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   For WBS data through interface for IAP        *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 10-Jan-2018  CPALYAM     D30K928578  CHG0100809-Initial Development  *                                                                     *
*                          D30K928836, D30K928894                      *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_DEFAULT_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_default_filename .
  DATA: lv_repid      TYPE sy-repid.

*  CLEAR: p_file2 ,p_path2. " To change the path at runtime
  IF p_file2 IS INITIAL OR p_path2 IS INITIAL.

    lv_repid = sy-repid.

    gv_path =
    zcl_iap_interface_util=>get_dat_filename( im_lfile = lv_repid ).
    REFRESH gt_pathsplit.
    SPLIT gv_path AT gc_fslash INTO TABLE gt_pathsplit.
    CLEAR gv_path.
    gv_path = lines( gt_pathsplit ).
    IF p_file2 IS INITIAL.
      IF NOT gt_pathsplit IS INITIAL.
        READ TABLE gt_pathsplit INTO gv_pathsplit INDEX gv_path.
        IF sy-subrc EQ 0.
*          p_file2 = gv_pathsplit.
          MOVE     gc_filename    TO p_file2.
        ENDIF.
      ENDIF.
    ENDIF.

    IF p_appl EQ abap_true.
      IF NOT gt_pathsplit IS INITIAL.
        DELETE gt_pathsplit INDEX gv_path.
      ENDIF.

      CLEAR gv_path.
      CONCATENATE LINES OF gt_pathsplit
      INTO gv_path SEPARATED BY
           gc_fslash.
      CONDENSE gv_path.
      p_path2 = gv_path.
    ENDIF.
  ENDIF.

*  CLEAR:p_file1.
  IF p_file1 IS INITIAL.

    CONCATENATE text-p00
                sy-datum '_'
                sy-uzeit '.CSV'
                INTO p_file1.

  ENDIF.

  IF NOT p_erpid IS INITIAL AND
         gv_flag IS INITIAL.
    gv_flag = gc_x.
*    CONCATENATE p_erpid p_file2 INTO p_file2 SEPARATED BY '_'.
    CONCATENATE p_erpid p_file1 INTO p_file1 SEPARATED BY '_'.
  ENDIF.

  IF p_path1 IS INITIAL.
    p_path1 = 'V:'.
  ENDIF.

ENDFORM.                    " DEFAULT_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_PATH2  text
*----------------------------------------------------------------------*
FORM f_get_folder_path  CHANGING p_path TYPE any.
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
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .

  CLEAR gt_prps[].
  IF p_full EQ gc_x.
    SELECT posid
           post1
           objnr
           pbukr
           verna
           loevm
            FROM prps
            INTO TABLE gt_prps
            WHERE posid IN s_posid[].
  ELSE.
    SELECT posid
         post1
         objnr
         pbukr
         verna
         loevm
          FROM prps
          INTO TABLE gt_prps
          WHERE posid IN s_posid[]   AND
                ( erdat IN s_aedat[] OR
                  aedat IN s_aedat[] ).
  ENDIF.
  IF sy-subrc NE 0.
    MESSAGE 'No Data Found'(001) TYPE 'I'.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_data .

  DATA: lv_line     TYPE j_stext,
        lv_cnt(10)  TYPE c,
        lv_output   TYPE string.

  IF NOT gt_prps[] IS INITIAL.
    LOOP AT gt_prps INTO gs_prps.
      CLEAR gs_output.
      MOVE-CORRESPONDING gs_prps TO gs_output.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = gs_output-posid
        IMPORTING
          output = gs_output-posid.

      gs_output-erpid = p_erpid.

      CONCATENATE gs_output-erpid gs_output-pbukr INTO gs_output-erp_comp
                                               SEPARATED BY '_'.

* Remove Special Characters
      PERFORM f_rmv_splcs CHANGING gs_output-post1.

* Defaulting the Flag status
      gs_output-status = 1.
      IF NOT gs_prps-loevm IS INITIAL.
        gs_output-status = 0.
        APPEND gs_output TO gt_output.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
*         CLIENT            = SY-MANDT
*         FLG_USER_STAT     = ' '
          objnr             = gs_prps-objnr
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
          gs_output-status = 0.
        ENDIF.
        IF lv_line CS 'AALK' OR
           lv_line CS 'LKD'  OR
           lv_line CS 'CLSD' OR
           lv_line CS 'DLFL' OR
           lv_line CS 'DLT'.
          gs_output-status = 0.
        ENDIF.
      ENDIF.

      APPEND gs_output TO gt_output.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_TO_PRESENTATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_write_to_presentation .
  DATA lv_msg TYPE string.

  CONSTANTS lc_error_type TYPE msgty VALUE 'E'.

  IF gt_output[] IS NOT INITIAL.

    CLEAR gs_output.
    LOOP AT  gt_output INTO  gs_output.
      CLEAR gs_tab_data.
      gref_util->add_pipes( EXPORTING im_rec = gs_output
      IMPORTING ex_outrec = gs_tab_data ).

      APPEND gs_tab_data TO gt_tab_data.
    ENDLOOP.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
*       PERCENTAGE = 0
        text       = text-d05.
* Data File

    CLEAR gv_filename1.
    CONCATENATE gv_fold1
    gv_fname1
    INTO gv_filename1
    SEPARATED BY gc_fslash.

    TRY.
        CALL METHOD zcl_iap_interface_util=>download_file_pc
          EXPORTING
            im_filename = gv_filename1
            im_data_tab = gt_tab_data[].
*            im_delim    = 'X'.
      CATCH cx_sy_file_io.
        CLEAR lv_msg.
        CONCATENATE text-e04 gv_filename1 INTO lv_msg.
        MESSAGE lv_msg TYPE lc_error_type.
    ENDTRY.
*    ENDIF.
  ENDIF.

  CLEAR gv_cn_recs_total.
  DESCRIBE TABLE gt_output LINES gv_cn_recs_total.
  WRITE: text-002, ':', gv_cn_recs_total.

ENDFORM.                    " WRITE_TO_PRESENTATION
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_TO_APPL_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_write_to_appl_server .
  DATA lv_msg TYPE string. " Error message

* Write data file to Application server

  CLEAR gs_output.
  LOOP AT  gt_output INTO  gs_output.
    CLEAR gs_tab_data.
    gref_util->add_pipes( EXPORTING im_rec = gs_output
    IMPORTING ex_outrec = gs_tab_data ).
    APPEND gs_tab_data TO gt_tab_data.
  ENDLOOP.

  IF NOT p_erpid IS INITIAL AND
         gv_flag IS INITIAL.
    gv_flag = gc_x.
    CONCATENATE p_erpid p_file2 INTO p_file2 SEPARATED BY '_'.
    CONCATENATE p_erpid p_file1 INTO p_file1 SEPARATED BY '_'.
  ENDIF.

  CLEAR                    gv_filename1.
  CONCATENATE              p_path2
                           gc_fslash
                           p_file2
                      INTO gv_filename1.

*  TRY.
*      gref_util->write_file_server( im_filename = gv_filename1
*     im_data_tab =  gt_tab_data[] ).
*    CATCH cx_sy_file_io.
*      CLEAR lv_msg.
*      CONCATENATE text-e04 gv_filename1 INTO lv_msg.
*      MESSAGE lv_msg TYPE 'E'.
*  ENDTRY.


  PERFORM  f_open_dataset.

  LOOP AT gt_tab_data INTO gs_tab_data.
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

    TRANSFER gs_tab_data TO gv_filename.

  ENDLOOP.
  CLOSE    DATASET gv_filename.

  WRITE: text-002, ':', gv_cn_recs_total.

ENDFORM.                    " F_WRITE_TO_APPL_SERVER
*&---------------------------------------------------------------------*
*&      Form  F_RMV_SPLCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_rmv_splcs  CHANGING cv_text TYPE any.

  CONSTANTS : lc_sep1(2) TYPE c VALUE '|~',
              lc_sep2(2) TYPE c VALUE '~|',
              lc_sep3(2) TYPE c VALUE '|-',
              lc_sep4(2) TYPE c VALUE '-|'.

  DATA : lv_text(200) TYPE c,
         lv_ind       TYPE sy-index,
         lv_char      TYPE c,
         lv_len       TYPE i.

  CLEAR: lv_text,lv_len,lv_ind,lv_char.

  MOVE cv_text TO lv_text.
  CONDENSE lv_text.

  REPLACE ALL OCCURRENCES OF lc_sep1 IN lv_text WITH lc_sep3.
  REPLACE ALL OCCURRENCES OF lc_sep2 IN lv_text WITH lc_sep4.

*--> Any Other Spl Chars less than Space replace by Space
  lv_len  = strlen( lv_text ).

  DO lv_len TIMES.
    lv_ind = sy-index - 1.
    CLEAR lv_char.
    MOVE     lv_text+lv_ind(1) TO lv_char.
    IF     ( lv_char LT ' ' ).
      MOVE   space  TO lv_text+lv_ind(1).
    ENDIF.
  ENDDO.

  CLEAR  cv_text.
  CONDENSE lv_text.
  MOVE   lv_text TO cv_text.

ENDFORM.                    " F_RMV_SPLCS
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
  IF   ( ( p_appl                        IS NOT INITIAL ) AND
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
                                       SEPARATED BY gc_fslash.

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
           INTO ls_output
   SEPARATED BY gc_delim.

  IF       ( p_appl                      IS NOT INITIAL ).
    TRANSFER ls_output                   TO gv_filename.
  ELSEIF   ( p_pres                      IS NOT INITIAL ).
    APPEND   ls_output                   TO gt_tab_data.
  ENDIF.

  ADD      1                             TO gv_cn_recs_file.
  ADD      1                             TO gv_cn_recs_total.

ENDFORM.                    " F_CREATE_HEADER_REC
