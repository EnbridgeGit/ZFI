*&---------------------------------------------------------------------*
*&  Include           ZFAPI125_GOODS_RECEIPT_F01
*&---------------------------------------------------------------------*
************************************************************************
*                            Enbridge Energy                           *
*&---------------------------------------------------------------------*
*& Program Name       :  ZFAPI125_GOODS_RECEIPT                        *
*& Author             :  Kalinga Keshari Rout                          *
*& Creation Date      :  March 26, 2018                                *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Program extracts goods receipt history data   *
*                        for both cases full load and delta load and   *
*&                       file created application server.              "
*&---------------------------------------------------------------------*
*                      Modification Log                                *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 26-Mar-2018  KROUT       D30K928872  CHG0106485  Initial Development *
*                          D30K928904, D30K928931, D30K928955,         *
*                          D30K928981, D30K929077                      *
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

  CLEAR    gt_t006a[].
  CLEAR    gt_ekko[].
  CLEAR    gt_po_hist[].
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

*eject
* Units of measure
  SELECT   msehi  mseh3
    INTO   TABLE gt_t006a
    FROM   t006a
   WHERE   spras = 'E'.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_t006a ASCENDING BY msehi.
  ELSE.
    CLEAR  gt_t006a[].
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
  IF       ( rb_fload     IS NOT INITIAL ).
    PERFORM  f_process_full_load.
  ELSEIF   ( rb_dload     IS NOT INITIAL ).
    PERFORM  f_process_delta_load.
  ENDIF.

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
*&      Form  f_process_full_load
*&---------------------------------------------------------------------*
*       Process the full load
*----------------------------------------------------------------------*
FORM f_process_full_load.

  DATA:    lt_ekko                     TYPE gtt_ekko,
           lt_ekpo                     TYPE gtt_ekpo,
           ls_ekpo                     TYPE gty_ekpo,
           lt_ekbe                     TYPE gtt_ekbe,
           lt_po_hist_101              TYPE gtt_po_hist,
           lt_po_hist_ref              TYPE gtt_po_hist,
           ls_po_hist                  TYPE gty_po_hist.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS: lc_blocksize              TYPE syindex VALUE 1000.

  FIELD-SYMBOLS: <fs_ekpo>             TYPE gty_ekpo,
                 <fs_ekbe>             TYPE gty_ekbe.

* Select the purchasing documents
  SELECT   ebeln  bsart  loekz  aedat
    INTO   TABLE gt_ekko
    FROM   ekko
   WHERE   ebeln IN s_ebeln
     AND   bsart IN s_bsart
     AND   aedat IN s_aedat
     AND   loekz  = space.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gt_ekko[].
    RETURN.
  ENDIF.

  SORT     gt_ekko             ASCENDING BY ebeln.
  DELETE   ADJACENT DUPLICATES         FROM gt_ekko
                                  COMPARING ebeln.

*eject
* Process the documents in batches
  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch
    CLEAR             lt_ekko[].
    APPEND   LINES OF gt_ekko
                 FROM lv_index_lo
                   TO lv_index_hi
                   TO lt_ekko.

    IF     ( lt_ekko[] IS INITIAL ).
      EXIT.
    ENDIF.

    CLEAR    lt_ekpo[].
    CLEAR    lt_ekbe[].
    CLEAR    lt_po_hist_101[].
    CLEAR    lt_po_hist_ref[].

* Select the purchasing document items
    SELECT   ebeln  ebelp  loekz  meins  webre  xersy
      INTO   TABLE lt_ekpo
      FROM   ekpo FOR ALL ENTRIES IN lt_ekko
     WHERE   ebeln = lt_ekko-ebeln
       AND   loekz = space
       AND   webre = abap_true.
*      AND   xersy = space. "do not remove the comment
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_ekpo[].
      CONTINUE.
    ENDIF.

    SORT     lt_ekpo           ASCENDING BY ebeln ebelp.
    DELETE   ADJACENT DUPLICATES       FROM lt_ekpo
                                  COMPARING ebeln ebelp.

*eject
* Select the purchasing document history
    SELECT   ebeln  ebelp  gjahr  belnr  buzei  vgabe
             zekkn  bewtp  bwart  menge  wrbtr  waers
             shkzg  xblnr  lfgja  lfbnr  lfpos  cpudt
      INTO   TABLE lt_ekbe
      FROM   ekbe FOR ALL ENTRIES IN lt_ekpo
     WHERE   ebeln = lt_ekpo-ebeln
       AND   ebelp = lt_ekpo-ebelp.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_ekpo[].
      CONTINUE.
    ENDIF.

    SORT     lt_ekbe           ASCENDING BY ebeln ebelp gjahr
                                            belnr buzei vgabe zekkn.
    DELETE   ADJACENT DUPLICATES       FROM lt_ekbe
                                  COMPARING ebeln ebelp gjahr
                                            belnr buzei vgabe zekkn.

*eject
* Process the history for each purchasing item
    LOOP AT  lt_ekbe              ASSIGNING <fs_ekbe>.

      IF     ( ( ls_ekpo-ebeln           EQ <fs_ekbe>-ebeln ) AND
               ( ls_ekpo-ebelp           EQ <fs_ekbe>-ebelp )     ).
      ELSE.
        CLEAR                               ls_ekpo.
        READ     TABLE lt_ekpo         INTO ls_ekpo
                                   WITH KEY ebeln = <fs_ekbe>-ebeln
                                            ebelp = <fs_ekbe>-ebelp
                              BINARY SEARCH.
        IF     ( sy-subrc NE 0 ).
          CLEAR        ls_ekpo.
        ENDIF.
      ENDIF.

      CLEAR                                 ls_po_hist.
      MOVE-CORRESPONDING <fs_ekbe>       TO ls_po_hist.
      CLEAR                                 ls_po_hist-meins.
      MOVE         ls_ekpo-meins         TO ls_po_hist-meins.

      IF       ( ( <fs_ekbe>-vgabe       EQ '1'             ) AND
                 ( <fs_ekbe>-bwart       EQ '101'           )     ).
        IF       ( <fs_ekbe>-lfbnr       IS INITIAL         ).
          APPEND   ls_po_hist            TO lt_po_hist_101.
        ELSEIF ( ( <fs_ekbe>-gjahr       EQ <fs_ekbe>-lfgja ) AND
                 ( <fs_ekbe>-belnr       EQ <fs_ekbe>-lfbnr ) AND
                 ( <fs_ekbe>-buzei       EQ <fs_ekbe>-lfpos )     ).
          APPEND   ls_po_hist            TO lt_po_hist_101.
        ELSE.
          APPEND   ls_po_hist            TO lt_po_hist_ref.
        ENDIF.
      ELSEIF   ( ( <fs_ekbe>-vgabe       EQ '1'             ) AND
                 ( <fs_ekbe>-bwart       EQ '102'           ) AND
                 ( <fs_ekbe>-lfbnr       IS NOT INITIAL     )     ).
        APPEND     ls_po_hist            TO lt_po_hist_ref.
      ELSEIF   ( ( <fs_ekbe>-vgabe       EQ '1'             ) AND
                 ( <fs_ekbe>-bwart       EQ '122'           ) AND
                 ( <fs_ekbe>-lfbnr       IS NOT INITIAL     )     ).
        APPEND     ls_po_hist            TO lt_po_hist_ref.
      ELSEIF   ( ( <fs_ekbe>-vgabe       EQ '2'             ) AND
                 ( <fs_ekbe>-lfbnr       IS NOT INITIAL     )     ).
        APPEND     ls_po_hist            TO lt_po_hist_ref.
      ENDIF.

*eject
      AT END OF ebelp.

        PERFORM  f_process_history   TABLES lt_po_hist_101
                                            lt_po_hist_ref.

        CLEAR    lt_po_hist_101[].
        CLEAR    lt_po_hist_ref[].

      ENDAT.

    ENDLOOP.

  ENDDO.

ENDFORM.                    " f_process_full_load
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_delta_load
*&---------------------------------------------------------------------*
*       Process the delta load
*----------------------------------------------------------------------*
FORM f_process_delta_load.

  DATA:    lt_ekko                     TYPE gtt_ekko,
           ls_ekko                     TYPE gty_ekko,
           lt_ekpo                     TYPE gtt_ekpo,
           ls_ekpo                     TYPE gty_ekpo,
           lt_ekbe                     TYPE gtt_ekbe,
           lt_ekbe_p                   TYPE gtt_ekbe,
           lt_po_hist_key              TYPE gtt_po_hist_key,
           ls_po_hist_key              TYPE gty_po_hist_key,
           lt_po_hist                  TYPE gtt_po_hist,
           lt_po_hist_101              TYPE gtt_po_hist,
           lt_po_hist_ref              TYPE gtt_po_hist,
           ls_po_hist                  TYPE gty_po_hist.

  FIELD-SYMBOLS: <fs_ekbe>             TYPE gty_ekbe.

  CLEAR    lt_ekko[].
  CLEAR    lt_ekpo[].
  CLEAR    lt_ekbe[].
  CLEAR    lt_ekbe_p[].
  CLEAR    lt_po_hist_key[].
  CLEAR    lt_po_hist[].
  CLEAR    lt_po_hist_101[].
  CLEAR    lt_po_hist_ref[].

* Select the purchasing document history
  SELECT   ebeln  ebelp  gjahr  belnr  buzei  vgabe
           zekkn  bewtp  bwart  menge  wrbtr  waers
           shkzg  xblnr  lfgja  lfbnr  lfpos  cpudt
    INTO   TABLE lt_ekbe
    FROM   ekbe
   WHERE   ebeln IN s_ebeln
     AND   cpudt IN s_date.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lt_ekpo[].
    RETURN.
  ENDIF.

  SORT     lt_ekbe             ASCENDING BY ebeln ebelp gjahr
                                            belnr buzei vgabe zekkn.
  DELETE   ADJACENT DUPLICATES         FROM lt_ekbe
                                  COMPARING ebeln ebelp gjahr
                                            belnr buzei vgabe zekkn.

*eject
* Select the purchasing document items
  SELECT   ebeln  ebelp  loekz  meins  webre  xersy
    INTO   TABLE lt_ekpo
    FROM   ekpo FOR ALL ENTRIES IN lt_ekbe
   WHERE   ebeln = lt_ekbe-ebeln
     AND   ebelp = lt_ekbe-ebelp
     AND   webre = abap_true.
*    AND   xersy = space. "do not remove the comment
  IF     ( sy-subrc NE 0 ).
    CLEAR  lt_ekpo[].
    RETURN.
  ENDIF.

  SORT     lt_ekpo             ASCENDING BY ebeln ebelp.
  DELETE   ADJACENT DUPLICATES         FROM lt_ekpo
                                  COMPARING ebeln ebelp.

* Select the purchasing documents
  SELECT   ebeln  bsart  loekz  aedat
    INTO   TABLE lt_ekko
    FROM   ekko FOR ALL ENTRIES IN lt_ekpo
   WHERE   ebeln  = lt_ekpo-ebeln
     AND   bsart IN s_bsart
     AND   aedat IN s_aedat.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lt_ekko[].
    RETURN.
  ENDIF.

  SORT     lt_ekko             ASCENDING BY ebeln.
  DELETE   ADJACENT DUPLICATES         FROM lt_ekko
                                  COMPARING ebeln.

*eject
* Build a purchasing history key table
  LOOP AT  lt_ekbe                ASSIGNING <fs_ekbe>.

    IF       ( ls_ekko-ebeln             EQ <fs_ekbe>-ebeln ).
    ELSE.
      CLEAR                                 ls_ekko.
      READ     TABLE lt_ekko           INTO ls_ekko
                                   WITH KEY ebeln = <fs_ekbe>-ebeln
                              BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CLEAR        ls_ekko.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF     ( ( ls_ekpo-ebeln             EQ <fs_ekbe>-ebeln ) AND
             ( ls_ekpo-ebelp             EQ <fs_ekbe>-ebelp )     ).
    ELSE.
      CLEAR                                 ls_ekpo.
      READ     TABLE lt_ekpo           INTO ls_ekpo
                                   WITH KEY ebeln = <fs_ekbe>-ebeln
                                            ebelp = <fs_ekbe>-ebelp
                              BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CLEAR        ls_ekpo.
        CONTINUE.
      ENDIF.
    ENDIF.

*eject
    CLEAR                                   ls_po_hist_key.
    MOVE         <fs_ekbe>-ebeln         TO ls_po_hist_key-ebeln.
    MOVE         <fs_ekbe>-ebelp         TO ls_po_hist_key-ebelp.
    IF       ( ( <fs_ekbe>-vgabe         EQ '1'             ) AND
               ( <fs_ekbe>-bwart         EQ '101'           )     ).
      IF       ( <fs_ekbe>-lfbnr         IS INITIAL         ).
        MOVE     <fs_ekbe>-gjahr         TO ls_po_hist_key-gjahr.
        MOVE     <fs_ekbe>-belnr         TO ls_po_hist_key-belnr.
        MOVE     <fs_ekbe>-buzei         TO ls_po_hist_key-buzei.
        APPEND   ls_po_hist_key          TO lt_po_hist_key.
      ELSEIF ( ( <fs_ekbe>-gjahr         EQ <fs_ekbe>-lfgja ) AND
               ( <fs_ekbe>-belnr         EQ <fs_ekbe>-lfbnr ) AND
               ( <fs_ekbe>-buzei         EQ <fs_ekbe>-lfpos )     ).
        MOVE     <fs_ekbe>-gjahr         TO ls_po_hist_key-gjahr.
        MOVE     <fs_ekbe>-belnr         TO ls_po_hist_key-belnr.
        MOVE     <fs_ekbe>-buzei         TO ls_po_hist_key-buzei.
        APPEND   ls_po_hist_key          TO lt_po_hist_key.
      ELSE.
        MOVE     <fs_ekbe>-lfgja         TO ls_po_hist_key-gjahr.
        MOVE     <fs_ekbe>-lfbnr         TO ls_po_hist_key-belnr.
        MOVE     <fs_ekbe>-lfpos         TO ls_po_hist_key-buzei.
        APPEND   ls_po_hist_key          TO lt_po_hist_key.
      ENDIF.
    ELSEIF   ( ( <fs_ekbe>-vgabe         EQ '1'             ) AND
               ( <fs_ekbe>-bwart         EQ '102'           ) AND
               ( <fs_ekbe>-lfbnr         IS NOT INITIAL     )     ).
      MOVE       <fs_ekbe>-lfgja         TO ls_po_hist_key-gjahr.
      MOVE       <fs_ekbe>-lfbnr         TO ls_po_hist_key-belnr.
      MOVE       <fs_ekbe>-lfpos         TO ls_po_hist_key-buzei.
      APPEND     ls_po_hist_key          TO lt_po_hist_key.
    ELSEIF   ( ( <fs_ekbe>-vgabe         EQ '1'             ) AND
               ( <fs_ekbe>-bwart         EQ '122'           ) AND
               ( <fs_ekbe>-lfbnr         IS NOT INITIAL     )     ).
      MOVE       <fs_ekbe>-lfgja         TO ls_po_hist_key-gjahr.
      MOVE       <fs_ekbe>-lfbnr         TO ls_po_hist_key-belnr.
      MOVE       <fs_ekbe>-lfpos         TO ls_po_hist_key-buzei.
      APPEND     ls_po_hist_key          TO lt_po_hist_key.
    ELSEIF   ( ( <fs_ekbe>-vgabe         EQ '2'             ) AND
               ( <fs_ekbe>-lfbnr         IS NOT INITIAL     )     ).
      MOVE       <fs_ekbe>-lfgja         TO ls_po_hist_key-gjahr.
      MOVE       <fs_ekbe>-lfbnr         TO ls_po_hist_key-belnr.
      MOVE       <fs_ekbe>-lfpos         TO ls_po_hist_key-buzei.
      APPEND     ls_po_hist_key          TO lt_po_hist_key.
    ENDIF.

  ENDLOOP.

  SORT     lt_po_hist_key ASCENDING BY ebeln ebelp gjahr belnr buzei.
  DELETE   ADJACENT DUPLICATES    FROM lt_po_hist_key
                             COMPARING ebeln ebelp gjahr belnr buzei.

*eject
* Select all originating history
  CLEAR    lt_ekbe[].

  SELECT   ebeln  ebelp  gjahr  belnr  buzei  vgabe
           zekkn  bewtp  bwart  menge  wrbtr  waers
           shkzg  xblnr  lfgja  lfbnr  lfpos  cpudt
    INTO   TABLE lt_ekbe
    FROM   ekbe FOR ALL ENTRIES IN lt_po_hist_key
   WHERE   ebeln = lt_po_hist_key-ebeln
     AND   ebelp = lt_po_hist_key-ebelp
     AND   gjahr = lt_po_hist_key-gjahr
     AND   belnr = lt_po_hist_key-belnr
     AND   buzei = lt_po_hist_key-buzei.
  IF     ( sy-subrc EQ 0 ).
    APPEND   LINES OF lt_ekbe            TO lt_ekbe_p.
  ENDIF.

* Select all referenced history
  CLEAR    lt_ekbe[].

  SELECT   ebeln  ebelp  gjahr  belnr  buzei  vgabe
           zekkn  bewtp  bwart  menge  wrbtr  waers
           shkzg  xblnr  lfgja  lfbnr  lfpos  cpudt
    INTO   TABLE lt_ekbe
    FROM   ekbe FOR ALL ENTRIES IN lt_po_hist_key
   WHERE   ebeln = lt_po_hist_key-ebeln
     AND   ebelp = lt_po_hist_key-ebelp
     AND   lfgja = lt_po_hist_key-gjahr
     AND   lfbnr = lt_po_hist_key-belnr
     AND   lfpos = lt_po_hist_key-buzei.
  IF     ( sy-subrc EQ 0 ).
    APPEND   LINES OF lt_ekbe            TO lt_ekbe_p.
  ENDIF.

  SORT     lt_ekbe_p           ASCENDING BY ebeln ebelp gjahr
                                            belnr buzei vgabe zekkn.
  DELETE   ADJACENT DUPLICATES         FROM lt_ekbe_p
                                  COMPARING ebeln ebelp gjahr
                                            belnr buzei vgabe zekkn.

  CLEAR    lt_ekbe[].
  CLEAR    lt_po_hist_key[].
  CLEAR    lt_po_hist_101[].
  CLEAR    lt_po_hist_ref[].

*eject
* Process the history for each purchasing item
  LOOP AT  lt_ekbe_p              ASSIGNING <fs_ekbe>.

    IF     ( ( ls_ekpo-ebeln             EQ <fs_ekbe>-ebeln ) AND
             ( ls_ekpo-ebelp             EQ <fs_ekbe>-ebelp )     ).
    ELSE.
      CLEAR                                 ls_ekpo.
      READ     TABLE lt_ekpo           INTO ls_ekpo
                                   WITH KEY ebeln = <fs_ekbe>-ebeln
                                            ebelp = <fs_ekbe>-ebelp
                              BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CLEAR        ls_ekpo.
      ENDIF.
    ENDIF.

    CLEAR                                   ls_po_hist.
    MOVE-CORRESPONDING <fs_ekbe>         TO ls_po_hist.
    CLEAR                                   ls_po_hist-meins.
    MOVE       ls_ekpo-meins             TO ls_po_hist-meins.

    IF       ( ( <fs_ekbe>-vgabe         EQ '1'             ) AND
               ( <fs_ekbe>-bwart         EQ '101'           )     ).
      IF       ( <fs_ekbe>-lfbnr         IS INITIAL         ).
        APPEND   ls_po_hist              TO lt_po_hist_101.
      ELSEIF ( ( <fs_ekbe>-gjahr         EQ <fs_ekbe>-lfgja ) AND
               ( <fs_ekbe>-belnr         EQ <fs_ekbe>-lfbnr ) AND
               ( <fs_ekbe>-buzei         EQ <fs_ekbe>-lfpos )     ).
        APPEND   ls_po_hist              TO lt_po_hist_101.
      ELSE.
        APPEND   ls_po_hist              TO lt_po_hist_ref.
      ENDIF.
    ELSEIF   ( ( <fs_ekbe>-vgabe         EQ '1'             ) AND
               ( <fs_ekbe>-bwart         EQ '102'           ) AND
               ( <fs_ekbe>-lfbnr         IS NOT INITIAL     )     ).
      APPEND     ls_po_hist              TO lt_po_hist_ref.
    ELSEIF   ( ( <fs_ekbe>-vgabe         EQ '1'             ) AND
               ( <fs_ekbe>-bwart         EQ '122'           ) AND
               ( <fs_ekbe>-lfbnr         IS NOT INITIAL     )     ).
      APPEND     ls_po_hist              TO lt_po_hist_ref.
    ELSEIF   ( ( <fs_ekbe>-vgabe         EQ '2'             ) AND
               ( <fs_ekbe>-lfbnr         IS NOT INITIAL     )     ).
      APPEND     ls_po_hist              TO lt_po_hist_ref.
    ENDIF.

*eject
    AT END OF ebelp.

      PERFORM  f_process_history     TABLES lt_po_hist_101
                                            lt_po_hist_ref.

      CLEAR    lt_po_hist_101[].
      CLEAR    lt_po_hist_ref[].

    ENDAT.

  ENDLOOP.

ENDFORM.                    " f_process_delta_load
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_history
*&---------------------------------------------------------------------*
*       Process the purchasing history
*----------------------------------------------------------------------*
FORM f_process_history
  TABLES   ct_po_hist_101              TYPE gtt_po_hist
           ct_po_hist_ref              TYPE gtt_po_hist.

  DATA:    ls_doc_sum                  TYPE gty_doc_sum,
           ls_doc_inv                  TYPE gty_doc_sum,
           ls_doc_102                  TYPE gty_doc_sum,
           ls_t006a                    TYPE gty_t006a,
           ls_final                    TYPE gty_final,
           ls_output                   TYPE gty_output.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_menge                    TYPE menge_d,
           lv_menge_c                  TYPE char17,
           lv_wrbtr                    TYPE wrbtr,
           lv_wrbtr_c                  TYPE char16,
           lv_meins                    TYPE meins,
           lv_status                   TYPE char1,
           lv_fl_deleted               TYPE xflag,
           lv_fl_invoiced              TYPE xflag,
           lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_po_hist_101>      TYPE gty_po_hist,
                 <fs_po_hist_ref>      TYPE gty_po_hist,
                 <fs_final>            TYPE gty_final,
                 <fs_attr>             TYPE ANY.

  ASSIGN   ls_final                      TO <fs_final>.

  DESCRIBE FIELD <fs_final> TYPE lv_dtype COMPONENTS lv_cn_comp.

  SORT     ct_po_hist_101 ASCENDING BY ebeln ebelp gjahr belnr buzei.
  SORT     ct_po_hist_ref ASCENDING BY ebeln ebelp lfgja lfbnr lfpos.

*eject
* Create a summarized record
  LOOP AT  ct_po_hist_101         ASSIGNING <fs_po_hist_101>.

    AT NEW buzei.

      CLEAR    ls_doc_sum.
      CLEAR    ls_doc_inv.
      CLEAR    ls_doc_102.
      CLEAR    lv_menge.
      CLEAR    lv_wrbtr.
      CLEAR    lv_status.
      CLEAR    lv_fl_deleted.
      CLEAR    lv_fl_invoiced.

      MOVE     <fs_po_hist_101>-ebeln    TO ls_doc_sum-ebeln.
      MOVE     <fs_po_hist_101>-ebelp    TO ls_doc_sum-ebelp.
      MOVE     <fs_po_hist_101>-gjahr    TO ls_doc_sum-gjahr.
      MOVE     <fs_po_hist_101>-belnr    TO ls_doc_sum-belnr.
      MOVE     <fs_po_hist_101>-buzei    TO ls_doc_sum-buzei.
      MOVE     <fs_po_hist_101>-meins    TO ls_doc_sum-meins.
      MOVE     <fs_po_hist_101>-waers    TO ls_doc_sum-waers.
      MOVE     <fs_po_hist_101>-xblnr    TO ls_doc_sum-xblnr.

    ENDAT.

    ADD        <fs_po_hist_101>-menge    TO ls_doc_sum-menge.
    ADD        <fs_po_hist_101>-wrbtr    TO ls_doc_sum-wrbtr.

*eject
* Roll-up the referenced transactions
    AT END OF buzei.

      READ TABLE ct_po_hist_ref      ASSIGNING <fs_po_hist_ref>
                              WITH KEY ebeln = <fs_po_hist_101>-ebeln
                                       ebelp = <fs_po_hist_101>-ebelp
                                       lfgja = <fs_po_hist_101>-gjahr
                                       lfbnr = <fs_po_hist_101>-belnr
                                       lfpos = <fs_po_hist_101>-buzei
                         BINARY SEARCH.
      lv_subrc = sy-subrc.
      lv_tabix = sy-tabix.
      IF     ( lv_subrc EQ 0 ).

        LOOP AT  ct_po_hist_ref   ASSIGNING <fs_po_hist_ref>
                                       FROM lv_tabix.

          IF   ( <fs_po_hist_ref>-lfgja NE <fs_po_hist_101>-gjahr ) OR
               ( <fs_po_hist_ref>-lfbnr NE <fs_po_hist_101>-belnr ) OR
               ( <fs_po_hist_ref>-lfpos NE <fs_po_hist_101>-buzei ).
            EXIT.
          ENDIF.

          IF     ( ( <fs_po_hist_ref>-vgabe        EQ '1'   ) AND
                   ( <fs_po_hist_ref>-bwart        EQ '101' )     ).
            ADD      <fs_po_hist_ref>-menge        TO ls_doc_sum-menge.
            ADD      <fs_po_hist_ref>-wrbtr        TO ls_doc_sum-wrbtr.
          ELSEIF ( ( <fs_po_hist_ref>-vgabe        EQ '1'   ) AND
                   ( <fs_po_hist_ref>-bwart        EQ '102' )     ).
            CLEAR                                     ls_doc_102.
            MOVE-CORRESPONDING <fs_po_hist_ref>    TO ls_doc_102.
            SUBTRACT <fs_po_hist_ref>-menge      FROM ls_doc_sum-menge.
            SUBTRACT <fs_po_hist_ref>-wrbtr      FROM ls_doc_sum-wrbtr.
          ELSEIF ( ( <fs_po_hist_ref>-vgabe        EQ '1'   ) AND
                   ( <fs_po_hist_ref>-bwart        EQ '122' )     ).
            SUBTRACT <fs_po_hist_ref>-menge      FROM ls_doc_sum-menge.
            SUBTRACT <fs_po_hist_ref>-wrbtr      FROM ls_doc_sum-wrbtr.
          ELSEIF ( ( <fs_po_hist_ref>-vgabe        EQ '2'   ) AND
                   ( <fs_po_hist_ref>-shkzg        EQ 'S'   )     ).
            CLEAR                                     ls_doc_inv-ebeln.
            MOVE     <fs_po_hist_ref>-ebeln        TO ls_doc_inv-ebeln.
            ADD      <fs_po_hist_ref>-menge        TO ls_doc_inv-menge.
            ADD      <fs_po_hist_ref>-wrbtr        TO ls_doc_inv-wrbtr.
          ELSEIF ( ( <fs_po_hist_ref>-vgabe        EQ '2'   ) AND
                   ( <fs_po_hist_ref>-shkzg        EQ 'H'   )     ).
            CLEAR                                     ls_doc_inv-ebeln.
            MOVE     <fs_po_hist_ref>-ebeln        TO ls_doc_inv-ebeln.
            SUBTRACT <fs_po_hist_ref>-menge      FROM ls_doc_inv-menge.
            SUBTRACT <fs_po_hist_ref>-wrbtr      FROM ls_doc_inv-wrbtr.
          ENDIF.

        ENDLOOP.

      ENDIF.

*eject
* Complete the summary structure
      IF       ( ls_doc_sum-menge        LT 0 ).
        CLEAR                               ls_doc_sum-menge.
        MOVE     0                       TO ls_doc_sum-menge.
      ENDIF.
      IF       ( ls_doc_sum-wrbtr        LT 0 ).
        CLEAR                               ls_doc_sum-wrbtr.
        MOVE     0                       TO ls_doc_sum-wrbtr.
      ENDIF.

      IF     ( ( ls_doc_sum-menge        LE 0 ) AND
               ( ls_doc_sum-wrbtr        LE 0 )     ).
        MOVE     abap_true               TO lv_fl_deleted.
      ENDIF.

      MOVE       ls_doc_sum-menge        TO ls_doc_sum-menge_open.
      MOVE       ls_doc_sum-wrbtr        TO ls_doc_sum-wrbtr_open.

      IF       ( ls_doc_inv-ebeln        IS NOT INITIAL ).
        SUBTRACT ls_doc_inv-menge           FROM ls_doc_sum-menge_open.
        SUBTRACT ls_doc_inv-wrbtr           FROM ls_doc_sum-wrbtr_open.
        MOVE     abap_true               TO lv_fl_invoiced.
      ENDIF.

      IF     ( ( ls_doc_sum-menge        EQ 0 ) AND
               ( ls_doc_sum-wrbtr        EQ 0 ) AND
               ( ls_doc_sum-menge_open   EQ 0 ) AND
               ( ls_doc_sum-wrbtr_open   EQ 0 )     ).
        MOVE     '0'                     TO lv_status.
      ELSE.
        MOVE     '1'                     TO lv_status.
      ENDIF.

*eject
* Format the output
      CLEAR                                 lv_menge_c.
      WRITE    ls_doc_sum-menge          TO lv_menge_c
                                       UNIT ls_doc_sum-meins.
      TRANSLATE                             lv_menge_c USING ', - '.
      CONDENSE                              lv_menge_c NO-GAPS.
      IF     ( ls_doc_sum-menge          LT 0 ).
        CONCATENATE   '-'  lv_menge_c  INTO lv_menge_c.
      ENDIF.
      CLEAR                                 lv_wrbtr_c.
      WRITE    ls_doc_sum-wrbtr          TO lv_wrbtr_c
                                   CURRENCY ls_doc_sum-waers.
      TRANSLATE                             lv_wrbtr_c USING ', - '.
      CONDENSE                              lv_wrbtr_c NO-GAPS.
      IF     ( ls_doc_sum-wrbtr          LT 0 ).
        CONCATENATE   '-'  lv_wrbtr_c  INTO lv_wrbtr_c.
      ENDIF.

* Unit of measure
      CLEAR    lv_meins.

      READ     TABLE gt_t006a     INTO ls_t006a
                              WITH KEY msehi = <fs_po_hist_101>-meins
                         BINARY SEARCH.
      IF     ( sy-subrc EQ 0 ).
        MOVE   ls_t006a-mseh3            TO lv_meins.
      ENDIF.

      CLEAR                                 ls_final.
      MOVE     p_erpid                   TO ls_final-erpid.
      MOVE     <fs_po_hist_101>-belnr    TO ls_final-belnr.
      MOVE     <fs_po_hist_101>-gjahr    TO ls_final-gjahr.
      MOVE     <fs_po_hist_101>-buzei    TO ls_final-buzei.
      MOVE     <fs_po_hist_101>-xblnr    TO ls_final-xblnr.
      MOVE     <fs_po_hist_101>-ebeln    TO ls_final-ebeln.
      MOVE     <fs_po_hist_101>-ebelp    TO ls_final-ebelp.
      MOVE     lv_menge_c                TO ls_final-menge.
      MOVE     lv_meins                  TO ls_final-meins.
      MOVE     lv_wrbtr_c                TO ls_final-wrbtr.
      MOVE     <fs_po_hist_101>-waers    TO ls_final-waers.
      IF     ( lv_fl_deleted             IS NOT INITIAL ).
        MOVE   'Yes'                     TO ls_final-ind_del.
      ENDIF.
      IF     ( lv_fl_invoiced            IS NOT INITIAL ).
        MOVE   'Yes'                     TO ls_final-ind_inv.
      ENDIF.

*eject
      CLEAR                                 lv_menge_c.
      WRITE    ls_doc_sum-menge_open     TO lv_menge_c
                                       UNIT ls_doc_sum-meins.
      TRANSLATE                             lv_menge_c USING ', - '.
      CONDENSE                              lv_menge_c NO-GAPS.
      IF     ( ls_doc_sum-menge_open     LT 0 ).
        CONCATENATE   '-'  lv_menge_c  INTO lv_menge_c.
      ENDIF.
      CLEAR                                 lv_wrbtr_c.
      WRITE    ls_doc_sum-wrbtr_open     TO lv_wrbtr_c
                                   CURRENCY ls_doc_sum-waers.
      TRANSLATE                             lv_wrbtr_c USING ', - '.
      CONDENSE                              lv_wrbtr_c NO-GAPS.
      IF     ( ls_doc_sum-wrbtr_open     LT 0 ).
        CONCATENATE   '-'  lv_wrbtr_c  INTO lv_wrbtr_c.
      ENDIF.

      MOVE     lv_menge_c                TO ls_final-menge_open.
      MOVE     lv_wrbtr_c                TO ls_final-wrbtr_open.
      MOVE     lv_status                 TO ls_final-status.

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

* Close the file and open a new file
        IF       ( gv_cn_recs_file       GE gv_cn_recs_max ).
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

    ENDAT.

  ENDLOOP.

ENDFORM.                    " f_process_history
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
                                       INTO ls_output
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
