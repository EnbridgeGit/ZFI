*&---------------------------------------------------------------------*
*&  Include           ZFAPI120_BANK_KEY_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPI120_BANK_KEY                             *
* Author             :                                                 *
* Date               :   Mar 22, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Bank Key Extract Interface                    *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  CPALYAM     D30K928630  CHG0106152-Initial development  *
*                          D30K928713, D30K928842, D30K928902          *
*----------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  F_DEFAULT_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_default_filename .

  DATA: lv_repid TYPE sy-repid.

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
*        p_file2 = gv_pathsplit.
        MOVE     gc_filename    TO p_file2.
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

  IF p_file1 IS INITIAL.

    CONCATENATE text-p00
                sy-datum '_'
                sy-uzeit '.CSV'
                INTO p_file1.

  ENDIF.

*eject
  IF NOT p_erpid IS INITIAL AND
         gv_flag IS INITIAL.
    gv_flag = gc_x.
    CONCATENATE p_erpid p_file2 INTO p_file2 SEPARATED BY '_'.
    CONCATENATE p_erpid p_file1 INTO p_file1 SEPARATED BY '_'.
  ENDIF.

  IF p_path1 IS INITIAL.
    p_path1 = 'V:'.
  ENDIF.

ENDFORM.                    " F_DEFAULT_FILENAME
*eject
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

ENDFORM.                    " F_GET_FOLDER_PATH
*eject
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

*eject
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

  ENDIF.
*
*  gv_rec_count = lines( gt_tab_data ).
*
*  IF     ( gv_rec_count IS NOT INITIAL ).
*
*    lv_msg = gv_rec_count.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_msg
*      IMPORTING
*        output = lv_msg.
*
*    CONCATENATE 'Total number of records extracted:'(002)
*                lv_msg  INTO lv_msg  SEPARATED BY space.
*
*    WRITE lv_msg.
*
*  ELSE.
*
*    MESSAGE 'No Data Found'(001) TYPE 'I'.
*
*  ENDIF.

  CLEAR gv_cn_recs_total.
  DESCRIBE TABLE gt_output LINES gv_cn_recs_total.
  WRITE: text-002, ':', gv_cn_recs_total.

ENDFORM.                    " WRITE_TO_PRESENTATION
*eject
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

  CONSTANTS lc_error_type TYPE msgty VALUE 'E'.

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

  CLEAR gv_filename1.
  CONCATENATE p_path2
  p_file2
  INTO gv_filename1
  SEPARATED BY gc_fslash.

*  TRY.
*      gref_util->write_file_server( im_filename = gv_filename1
*      im_data_tab =  gt_tab_data[] ).
*    CATCH cx_sy_file_io.
*      CLEAR lv_msg.
*      CONCATENATE text-e04 gv_filename1 INTO lv_msg.
*      MESSAGE lv_msg TYPE lc_error_type.
*  ENDTRY.
*
*  gv_rec_count = lines( gt_tab_data ).
*
*  IF     ( gv_rec_count IS NOT INITIAL ).
*
*    lv_msg = gv_rec_count.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_msg
*      IMPORTING
*        output = lv_msg.
*
*    CONCATENATE 'Total number of records extracted:'(002)
*                lv_msg  INTO lv_msg  SEPARATED BY space.
*
*    WRITE lv_msg.
*
*  ELSE.
*
*    MESSAGE 'No Data Found'(001) TYPE 'I'.
*
*  ENDIF.

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
*eject
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       Get the vendor data
*----------------------------------------------------------------------*
FORM f_get_data.
  TYPES: BEGIN OF lty_cdhdr,
         objectclas TYPE  cdobjectcl,
         objectid	  TYPE  cdobjectv,
         changenr	  TYPE  cdchangenr,
         END OF   lty_cdhdr.

  DATA: lt_cdhdr    TYPE STANDARD TABLE OF lty_cdhdr,
        ls_cdhdr    TYPE lty_cdhdr,
        lt_bnka     TYPE STANDARD TABLE OF gty_bnka.

  CLEAR: lt_cdhdr[],
         gt_bnka[],
         gv_rec_count.

  IF p_full EQ gc_x.
    SELECT    banks
              bankl
              loevm
      INTO    TABLE gt_bnka
      FROM    bnka
      WHERE   banks IN s_banks
        AND   bankl IN s_bankl.
    IF sy-subrc EQ 0.
      SORT gt_bnka.
    ENDIF.
  ELSE.
    SELECT    objectclas
              objectid
              changenr
      INTO    TABLE lt_cdhdr
      FROM    cdhdr
     WHERE    objectclas EQ 'BANK'
       AND    udate      IN s_updat.
    IF sy-subrc EQ 0.
      SORT lt_cdhdr BY objectclas objectid.
      DELETE ADJACENT DUPLICATES FROM lt_cdhdr COMPARING objectclas objectid.

* Build Temporary Bank key table
      CLEAR lt_bnka[].
      LOOP AT lt_cdhdr INTO ls_cdhdr.
        CLEAR gs_bnka.
        MOVE: ls_cdhdr-objectid+03(03)  TO gs_bnka-banks,
              ls_cdhdr-objectid+06(15) TO gs_bnka-bankl.
        APPEND gs_bnka TO lt_bnka.
      ENDLOOP.

      IF NOT lt_bnka[] IS INITIAL.
        SELECT    banks
                  bankl
                  loevm
          INTO    TABLE gt_bnka
          FROM    bnka
          FOR ALL ENTRIES IN lt_bnka
          WHERE   banks IN s_banks
            AND   banks EQ lt_bnka-banks
            AND   bankl IN s_bankl
            AND   bankl EQ lt_bnka-bankl.
        IF sy-subrc EQ 0.
          SORT gt_bnka.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gt_bnka[] IS  INITIAL.
    MESSAGE 'No Data Found'(001) TYPE 'I'.
  ENDIF.

ENDFORM.                    " F_GET_DATA
*eject
*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATA
*&---------------------------------------------------------------------*
*       Fill the output record
*----------------------------------------------------------------------*
FORM f_fill_data.

  CLEAR    gt_output[].

  IF NOT gt_bnka[] IS INITIAL.
    LOOP AT gt_bnka INTO gs_bnka.
      CLEAR gs_output.
      MOVE: gs_bnka-banks     TO gs_output-banks,
            gs_bnka-bankl     TO gs_output-bankl,
            p_erpid           TO gs_output-erpid.

      IF gs_bnka-loevm IS INITIAL.
        MOVE '1'              TO gs_output-status.
      ELSE.
        MOVE '0'              TO gs_output-status.
      ENDIF.
      APPEND gs_output TO gt_output.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " F_FILL_DATA
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
