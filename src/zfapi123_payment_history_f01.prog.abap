*&---------------------------------------------------------------------*
*&  Include           ZFAPI123_PAYMENT_HISTORY_F01
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI123_PAYMENT_HISTORY                       *
* Include Program    :  ZFAPI123_PAYMENT_HISTORY_F01                   *
* Author             :  Paul Karunakar                                 *
* Creation Date      :  02-May-2018                                    *
* Application Area   :  FICO                                           *
* Description        :  Payment History data from each of the three    *
*                       SAP instances will be extracted in a delimited *
*                       file and sent to IAP.                          *
*                                                                      *
*&---------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  --------------------------------*
* 02-May-2018  KPAL        D30K928709  CHG0108944  Initial Development *
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

  CLEAR    gt_bsak_key[].
  CLEAR    gt_bsak[].
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

* Process the open documents (main)
  PERFORM  f_process_payments.

* Process the parked documents (main)
  PERFORM  f_process_payment_voids.

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
*&      Form  f_process_payments
*&---------------------------------------------------------------------*
*       Process the open documents (main)
*----------------------------------------------------------------------*
FORM f_process_payments.

  DATA:    lt_bsak_key                 TYPE gtt_bsak_key,
           ls_bsak_key                 TYPE gty_bsak_key.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_index                    TYPE syindex.

  CONSTANTS: lc_blocksize              TYPE syindex VALUE 1000.

  FIELD-SYMBOLS: <fs_bsak_key>         TYPE gty_bsak_key.

  CLEAR    gt_bsak_key[].

  SELECT   bukrs  lifnr  augdt  augbl
    INTO   TABLE gt_bsak_key
    FROM   bsak
   WHERE   bukrs IN s_bukrs
     AND   lifnr IN s_lifnr
     AND   augdt IN s_augdt
     AND   augbl IN s_augbl
     AND   gjahr IN s_gjahr
     AND   belnr IN s_belnr
     AND   cpudt IN s_cpudt.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_bsak_key         ASCENDING BY bukrs augdt augbl lifnr.
    DELETE         ADJACENT DUPLICATES FROM gt_bsak_key
                                  COMPARING bukrs augdt augbl lifnr.
  ELSE.
    CLEAR  gt_bsak_key[].
  ENDIF.

*eject
  CLEAR      lt_bsak_key[].
  CLEAR      ls_bsak_key.
  CLEAR      lv_index.

  LOOP AT    gt_bsak_key          ASSIGNING <fs_bsak_key>.

    IF   ( ( <fs_bsak_key>-bukrs         NE ls_bsak_key-bukrs ) OR
           ( <fs_bsak_key>-augdt         NE ls_bsak_key-augdt ) OR
           ( <fs_bsak_key>-augbl         NE ls_bsak_key-augbl )    ).

      CLEAR                                 ls_bsak_key.
      MOVE   <fs_bsak_key>               TO ls_bsak_key.

      IF       ( lv_index                GE lc_blocksize ).
        CLEAR    lv_index.

        PERFORM  f_select_bsak_details   TABLES lt_bsak_key.

        CLEAR    lt_bsak_key[].

      ENDIF.

      ADD    1                           TO lv_index.

    ENDIF.

    APPEND   <fs_bsak_key>               TO lt_bsak_key.

  ENDLOOP.

  IF       ( lt_bsak_key[]               IS NOT INITIAL ).

    PERFORM  f_select_bsak_details   TABLES lt_bsak_key.

  ENDIF.

ENDFORM.                    " f_process_payments
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_bsak_details
*&---------------------------------------------------------------------*
FORM f_select_bsak_details
  TABLES   it_bsak_key                 TYPE gtt_bsak_key.

  DATA:    lt_bsak                     TYPE gtt_bsak,
           ls_bsak                     TYPE gty_bsak.

  FIELD-SYMBOLS: <fs_bsak>             TYPE gty_bsak.

  IF     ( it_bsak_key[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     it_bsak_key         ASCENDING BY lifnr bukrs augdt augbl.

  CLEAR    gt_bsak[].

  SELECT   bukrs  lifnr  umsks  umskz  augdt
           augbl  zuonr  gjahr  belnr  buzei
           waers  xblnr  blart  bschl  shkzg
           dmbtr  wrbtr  zlsch  empfb
    INTO   TABLE gt_bsak
    FROM   bsak FOR ALL ENTRIES IN it_bsak_key
   WHERE   lifnr = it_bsak_key-lifnr
     AND   bukrs = it_bsak_key-bukrs
     AND   augdt = it_bsak_key-augdt
     AND   augbl = it_bsak_key-augbl.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SORT     gt_bsak             ASCENDING BY bukrs umsks umskz
                                            augdt augbl gjahr belnr.

*eject
  CLEAR    lt_bsak[].
  CLEAR    ls_bsak.

  LOOP AT  gt_bsak                ASSIGNING <fs_bsak>.

    IF   ( ( <fs_bsak>-bukrs             NE ls_bsak-bukrs ) OR
           ( <fs_bsak>-umsks             NE ls_bsak-umsks ) OR
           ( <fs_bsak>-umskz             NE ls_bsak-umskz ) OR
           ( <fs_bsak>-augdt             NE ls_bsak-augdt ) OR
           ( <fs_bsak>-augbl             NE ls_bsak-augbl )    ).

      CLEAR                                 ls_bsak.
      MOVE       <fs_bsak>               TO ls_bsak.

      IF       ( lt_bsak[]               IS NOT INITIAL ).

        PERFORM  f_process_cleared_doc   TABLES lt_bsak.

      ENDIF.

      CLEAR      lt_bsak[].

    ENDIF.

    APPEND   <fs_bsak>                   TO lt_bsak.

  ENDLOOP.

  IF       ( lt_bsak[]                   IS NOT INITIAL ).

    PERFORM  f_process_cleared_doc       TABLES lt_bsak.

  ENDIF.

ENDFORM.                    " f_select_bsak_details
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_cleared_doc
*&---------------------------------------------------------------------*
FORM f_process_cleared_doc
  TABLES   it_bsak                     TYPE gtt_bsak.

  DATA:    lt_bsak                     TYPE gtt_bsak,
           lt_bsak_inv                 TYPE gtt_bsak,
           ls_bsak                     TYPE gty_bsak,
           ls_bsak_clr                 TYPE gty_bsak,
           ls_final                    TYPE gty_final,
           ls_output                   TYPE gty_output.

  DATA:    lv_lifnr                    TYPE lifnr,
           lv_fl_delete                TYPE xflag,
           lv_wrbtr_s                  TYPE wrbtr,
           lv_wrbtr_h                  TYPE wrbtr,
           lv_ph_ulid                  TYPE char40,
           lv_ch_ulid                  TYPE char20,
           lv_chect                    TYPE chect,
           lv_bancd                    TYPE bancd,
           lv_p_total                  TYPE char16,
           lv_augdt_c                  TYPE char10,
           lv_bancd_c                  TYPE char10,
           lv_i_ulid                   TYPE char20,
           lv_i_total                  TYPE char16,
           lv_v_ulid                   TYPE char20,
           lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_bsak>             TYPE gty_bsak,
                 <fs_final>            TYPE gty_final,
                 <fs_attr>             TYPE ANY.

  IF     ( it_bsak[] IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_bsak[].
  CLEAR    ls_bsak.

  CLEAR    lv_lifnr.
  CLEAR    lv_fl_delete.

*eject
* Summarize by accounting documents
  LOOP AT  it_bsak                ASSIGNING <fs_bsak>.

    AT FIRST.
      MOVE       <fs_bsak>-lifnr         TO lv_lifnr.
    ENDAT.

    IF       ( ( ls_bsak-bukrs           NE <fs_bsak>-bukrs ) OR
               ( ls_bsak-belnr           NE <fs_bsak>-belnr ) OR
               ( ls_bsak-gjahr           NE <fs_bsak>-gjahr )    ).

      CLEAR                                 ls_bsak-shkzg.
      CLEAR                                 ls_bsak-wrbtr.
      IF       ( lv_wrbtr_s              EQ lv_wrbtr_h ).
        MOVE     'S'                     TO ls_bsak-shkzg.
        MOVE      0                      TO ls_bsak-wrbtr.
      ELSEIF   ( lv_wrbtr_s              GT lv_wrbtr_h ).
        MOVE     'S'                     TO ls_bsak-shkzg.
        ADD      lv_wrbtr_s              TO ls_bsak-wrbtr.
        SUBTRACT lv_wrbtr_h            FROM ls_bsak-wrbtr.
      ELSEIF   ( lv_wrbtr_h              GT lv_wrbtr_s ).
        MOVE     'H'                     TO ls_bsak-shkzg.
        ADD      lv_wrbtr_h              TO ls_bsak-wrbtr.
        SUBTRACT lv_wrbtr_s            FROM ls_bsak-wrbtr.
      ENDIF.

      IF       ( ls_bsak-bukrs           IS NOT INITIAL ).
        APPEND   ls_bsak                 TO lt_bsak.
      ENDIF.

      CLEAR                                 ls_bsak.
      MOVE       <fs_bsak>               TO ls_bsak.

      CLEAR      lv_wrbtr_s.
      CLEAR      lv_wrbtr_h.

    ENDIF.

*eject
    IF       ( ( <fs_bsak>-umsks         IS NOT INITIAL ) OR
               ( <fs_bsak>-umskz         IS NOT INITIAL )    ).
      lv_fl_delete = abap_true.
    ENDIF.

    IF       ( ( <fs_bsak>-augbl         NE <fs_bsak>-belnr ) AND
               ( <fs_bsak>-xblnr         IS INITIAL         )     ).
      lv_fl_delete = abap_true.
    ENDIF.

    IF         ( lv_lifnr                NE <fs_bsak>-lifnr ).
      lv_fl_delete = abap_true.
    ENDIF.

    IF         ( <fs_bsak>-shkzg         EQ 'S' ).
      ADD        <fs_bsak>-wrbtr         TO lv_wrbtr_s.
    ELSE.     "( <fs_bsak>-shkzg         EQ 'H' ).
      ADD        <fs_bsak>-wrbtr         TO lv_wrbtr_h.
    ENDIF.

    AT LAST.

      CLEAR                                 ls_bsak-shkzg.
      CLEAR                                 ls_bsak-wrbtr.
      IF       ( lv_wrbtr_s              EQ lv_wrbtr_h ).
        MOVE     'S'                     TO ls_bsak-shkzg.
        MOVE      0                      TO ls_bsak-wrbtr.
      ELSEIF   ( lv_wrbtr_s              GT lv_wrbtr_h ).
        MOVE     'S'                     TO ls_bsak-shkzg.
        ADD      lv_wrbtr_s              TO ls_bsak-wrbtr.
        SUBTRACT lv_wrbtr_h            FROM ls_bsak-wrbtr.
      ELSEIF   ( lv_wrbtr_h              GT lv_wrbtr_s ).
        MOVE     'H'                     TO ls_bsak-shkzg.
        ADD      lv_wrbtr_h              TO ls_bsak-wrbtr.
        SUBTRACT lv_wrbtr_s            FROM ls_bsak-wrbtr.
      ENDIF.

      IF       ( ls_bsak-bukrs           IS NOT INITIAL ).
        APPEND   ls_bsak                 TO lt_bsak.
      ENDIF.

    ENDAT.

  ENDLOOP.

  IF     ( lv_fl_delete IS NOT INITIAL ).
    RETURN.
  ENDIF.

*eject
* Create final data
  CLEAR    lt_bsak_inv[].
  CLEAR    ls_bsak_clr.

  LOOP AT  lt_bsak                ASSIGNING <fs_bsak>.
    IF       ( <fs_bsak>-augbl           EQ <fs_bsak>-belnr ).
      CLEAR                                 ls_bsak_clr.
      MOVE     <fs_bsak>                 TO ls_bsak_clr.
    ELSE.
      APPEND   <fs_bsak>                 TO lt_bsak_inv.
    ENDIF.
  ENDLOOP.

  IF   ( ( ls_bsak_clr-bukrs IS INITIAL ) OR
         ( lt_bsak_inv[]     IS INITIAL )    ).
    RETURN.
  ENDIF.

  ASSIGN   ls_final                      TO <fs_final>.

  DESCRIBE FIELD <fs_final> TYPE lv_dtype COMPONENTS lv_cn_comp.

* Check ULID (payment ULID)
  CLEAR                                     lv_ch_ulid.
  CONCATENATE             ls_bsak_clr-augbl ls_bsak_clr-bukrs
                                            ls_bsak_clr-gjahr
                                       INTO lv_ch_ulid
                               SEPARATED BY '_'.

* Check and encashment date
  CLEAR    lv_chect.
  CLEAR    lv_bancd.
  CLEAR    lv_bancd_c.
  SELECT   SINGLE chect bancd
    INTO  (lv_chect, lv_bancd)
    FROM   payr
   WHERE   zbukr = ls_bsak_clr-bukrs
     AND   vblnr = ls_bsak_clr-augbl
     AND   gjahr = ls_bsak_clr-gjahr.
  IF     ( sy-subrc EQ 0 ).
    IF   ( lv_bancd                      IS NOT INITIAL ).
      CONCATENATE                           lv_bancd+0(4) '-'
                                            lv_bancd+4(2) '-'
                                            lv_bancd+6(2)
                                       INTO lv_bancd_c.
    ENDIF.
  ELSE.
    CLEAR  lv_chect.
    CLEAR  lv_bancd.
  ENDIF.

*eject
* Payment total
  CLEAR                                     lv_p_total.
  WRITE    ls_bsak_clr-wrbtr             TO lv_p_total
                                   CURRENCY ls_bsak_clr-waers.
  TRANSLATE                                 lv_p_total USING ', - '.
  CONDENSE                                  lv_p_total NO-GAPS.
  IF     ( ls_bsak_clr-shkzg             EQ 'H' ).
    CONCATENATE '-' lv_p_total         INTO lv_p_total.
  ENDIF.

* Payment date
  CLEAR                                     lv_augdt_c.
  CONCATENATE                  ls_bsak_clr-augdt+0(4) '-'
                               ls_bsak_clr-augdt+4(2) '-'
                               ls_bsak_clr-augdt+6(2)
                                       INTO lv_augdt_c.

  CLEAR                                     ls_final.
  MOVE     p_erpid                       TO ls_final-erpid.
  MOVE     lv_ch_ulid                    TO ls_final-ch_ulid.
  MOVE     ls_bsak_clr-bukrs             TO ls_final-bukrs.
  MOVE     ls_bsak_clr-augbl             TO ls_final-augbl.
  MOVE     lv_chect                      TO ls_final-chect.
  MOVE     space                         TO ls_final-p_voided.
  MOVE     lv_p_total                    TO ls_final-p_total.
  MOVE     ls_bsak_clr-waers             TO ls_final-waers.
  MOVE     lv_augdt_c                    TO ls_final-augdt.
  MOVE     lv_bancd_c                    TO ls_final-bancd.
  MOVE     ls_bsak_clr-zlsch             TO ls_final-zlsch.

  LOOP AT  lt_bsak_inv            ASSIGNING <fs_bsak>.

* Payment History ULID
    CLEAR                                   lv_ph_ulid.
    CONCATENATE             <fs_bsak>-belnr <fs_bsak>-bukrs
                            <fs_bsak>-gjahr <fs_bsak>-augbl
                                       INTO lv_ph_ulid
                               SEPARATED BY '_'.

* Invoice ULID
    CLEAR                                   lv_i_ulid.
    CONCATENATE             <fs_bsak>-belnr <fs_bsak>-bukrs
                                            <fs_bsak>-gjahr
                                       INTO lv_i_ulid
                               SEPARATED BY '_'.

*eject
* Invoice total
    CLEAR                                   lv_i_total.
    WRITE    <fs_bsak>-wrbtr             TO lv_i_total
                                   CURRENCY <fs_bsak>-waers.
    TRANSLATE                               lv_i_total USING ', - '.
    CONDENSE                                lv_i_total NO-GAPS.
    IF     ( <fs_bsak>-shkzg             EQ 'H' ).
      CONCATENATE '-' lv_i_total       INTO lv_i_total.
    ENDIF.

* Vendor ULID
    CLEAR                                   lv_v_ulid.
    CONCATENATE             <fs_bsak>-lifnr <fs_bsak>-bukrs
                                       INTO lv_v_ulid
                               SEPARATED BY '_'.

    CLEAR                                   ls_final-ph_ulid.
    MOVE     lv_ph_ulid                  TO ls_final-ph_ulid.
    CLEAR                                   ls_final-i_ulid.
    MOVE     lv_i_ulid                   TO ls_final-i_ulid.
    CLEAR                                   ls_final-inv_id.
    MOVE     lv_i_ulid                   TO ls_final-inv_id.
    CLEAR                                   ls_final-xblnr.
    MOVE     <fs_bsak>-xblnr             TO ls_final-xblnr.
    CLEAR                                   ls_final-i_total.
    MOVE     lv_i_total                  TO ls_final-i_total.
    CLEAR                                   ls_final-i_paid.
    MOVE     lv_i_total                  TO ls_final-i_paid.
    CLEAR                                   ls_final-belnr.
    MOVE     <fs_bsak>-belnr             TO ls_final-belnr.
    CLEAR                                   ls_final-v_ulid.
    MOVE     lv_v_ulid                   TO ls_final-v_ulid.
    CLEAR                                   ls_final-lifnr.
    MOVE     <fs_bsak>-lifnr             TO ls_final-lifnr.
    CLEAR                                   ls_final-empfb.
    MOVE     <fs_bsak>-empfb             TO ls_final-empfb.
    CLEAR                                   ls_final-gjahr.
    MOVE     <fs_bsak>-gjahr             TO ls_final-gjahr.
    CLEAR                                   ls_final-lc_date.
    MOVE     space                       TO ls_final-lc_date.

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

    IF     ( rb_pres                     IS NOT INITIAL ).

      APPEND   ls_output                 TO gt_output.

      ADD      1                         TO gv_cn_recs_total.

    ELSEIF ( rb_appl                     IS NOT INITIAL ).

* Close the file and open a new file
      IF       ( gv_cn_recs_file         GE gv_cn_recs_max ).
        IF   ( ( p_maxrec                IS NOT INITIAL ) AND
               ( gv_filename             IS NOT INITIAL )     ).

          CLOSE    DATASET gv_filename.

          CLEAR    gv_filename.
          CLEAR    gv_cn_recs_file.

          PERFORM  f_open_dataset.

        ENDIF.
      ENDIF.

      IF       ( gv_filename             IS NOT INITIAL ).

        TRANSFER   ls_output             TO gv_filename.
        ADD      1                       TO gv_cn_recs_file.
        ADD      1                       TO gv_cn_recs_total.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_process_cleared_doc
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_payment_voids
*&---------------------------------------------------------------------*
*       Process the parked documents (main)
*----------------------------------------------------------------------*
FORM f_process_payment_voids.

  DATA:    lt_objectid                 TYPE gtt_objectid,
           lt_cdhdr                    TYPE gtt_cdhdr,
           lt_cdhdr_p                  TYPE gtt_cdhdr,
           ls_cdhdr                    TYPE gty_cdhdr.

  DATA:    lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex,
           lv_lifnr                    TYPE lifnr.

  CONSTANTS:  lc_objectclas            TYPE cdobjectcl VALUE 'BELEGR',
              lc_tcode                 TYPE cdtcode    VALUE 'FBRA',
              lc_blocksize             TYPE syindex    VALUE 500.

  FIELD-SYMBOLS: <fs_objectid>         TYPE gty_objectid.

  CLEAR    lt_objectid[].
  CLEAR    lt_cdhdr[].

  SELECT   objectid udate utime
    INTO   TABLE lt_objectid
    FROM   cdhdr
   WHERE   objectclas  = lc_objectclas
     AND   udate      IN s_cpudt
     AND   tcode       = lc_tcode.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SORT     lt_objectid         ASCENDING BY objectid.
  DELETE   ADJACENT DUPLICATES         FROM lt_objectid
                                  COMPARING objectid.

*eject
  LOOP AT  lt_objectid            ASSIGNING <fs_objectid>.

    CLEAR                                   ls_cdhdr.
    MOVE   <fs_objectid>-objectid+03(04) TO ls_cdhdr-bukrs.
    MOVE   <fs_objectid>-objectid+07(10) TO ls_cdhdr-augbl.
    MOVE   <fs_objectid>-objectid+17(04) TO ls_cdhdr-gjahr.
    MOVE   <fs_objectid>-udate           TO ls_cdhdr-udate.
    MOVE   <fs_objectid>-utime           TO ls_cdhdr-utime.

    CLEAR    lv_lifnr.
    SELECT   SINGLE lifnr
      INTO   lv_lifnr
      FROM   bseg
     WHERE   bukrs = ls_cdhdr-bukrs
       AND   belnr = ls_cdhdr-augbl
       AND   gjahr = ls_cdhdr-gjahr
       AND   koart = 'K'.
    IF     ( sy-subrc EQ 0 ).
      MOVE   lv_lifnr                    TO ls_cdhdr-lifnr.
    ENDIF.

    CONCATENATE  ls_cdhdr-gjahr '0101' INTO ls_cdhdr-ldate.
    CONCATENATE  ls_cdhdr-gjahr '1231' INTO ls_cdhdr-hdate.

    APPEND   ls_cdhdr                    TO lt_cdhdr.

  ENDLOOP.

  DELETE   lt_cdhdr                WHERE lifnr IS INITIAL.
  SORT     lt_cdhdr                   BY bukrs ASCENDING
                                         augbl ASCENDING
                                         gjahr ASCENDING
                                         udate DESCENDING
                                         utime DESCENDING.
  DELETE   ADJACENT DUPLICATES         FROM lt_cdhdr
                                  COMPARING bukrs augbl gjahr.

*eject
* Process the change documents in batches
  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch of change documents
    CLEAR           lt_cdhdr_p[].
    APPEND LINES OF lt_cdhdr
               FROM lv_index_lo
                 TO lv_index_hi
                 TO lt_cdhdr_p.

    IF     ( lt_cdhdr_p[] IS INITIAL ).
      EXIT.
    ENDIF.

    PERFORM  f_process_voided_docs   TABLES lt_cdhdr_p.

  ENDDO.

ENDFORM.                    " f_process_payment_voids
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_voided_docs
*&---------------------------------------------------------------------*
*       Process the parked documents (main)
*----------------------------------------------------------------------*
FORM f_process_voided_docs
  TABLES   it_cdhdr                    TYPE gtt_cdhdr.

  DATA:    ls_cdhdr                    TYPE gty_cdhdr,
           lt_reguh                    TYPE gtt_reguh,
           ls_reguh                    TYPE gty_reguh,
           lt_regup                    TYPE gtt_regup,
           lt_payr                     TYPE gtt_payr,
           ls_payr                     TYPE gty_payr,
           ls_final                    TYPE gty_final,
           ls_output                   TYPE gty_output.

  DATA:    lv_ph_ulid                  TYPE char40,
           lv_ch_ulid                  TYPE char20,
           lv_chect                    TYPE chect,
           lv_bancd                    TYPE bancd,
           lv_p_total                  TYPE char16,
           lv_augdt_c                  TYPE char10,
           lv_bancd_c                  TYPE char10,
           lv_i_ulid                   TYPE char20,
           lv_i_total                  TYPE char16,
           lv_v_ulid                   TYPE char20,
           lv_empfb                    TYPE empfb,
           lv_lc_date                  TYPE char14,
           lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_regup>            TYPE gty_regup,
                 <fs_final>            TYPE gty_final,
                 <fs_attr>             TYPE ANY.

*eject
  ASSIGN   ls_final                      TO <fs_final>.

  DESCRIBE FIELD <fs_final> TYPE lv_dtype COMPONENTS lv_cn_comp.

  CLEAR    lt_reguh[].
  CLEAR    lt_regup[].
  CLEAR    lt_payr[].

  SELECT   laufd  laufi  xvorl  zbukr  lifnr  kunnr
           empfg  vblnr  waers  zaldt  rzawe  valut  rwbtr
    INTO   TABLE lt_reguh
    FROM   reguh FOR ALL ENTRIES IN it_cdhdr
   WHERE   zbukr = it_cdhdr-bukrs
     AND   lifnr = it_cdhdr-lifnr
     AND   vblnr = it_cdhdr-augbl.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SELECT   laufd  laufi  xvorl  zbukr  lifnr  kunnr
           empfg  vblnr  bukrs  belnr  gjahr  buzei
           xblnr  shkzg  wrbtr
    INTO   TABLE lt_regup
    FROM   regup FOR ALL ENTRIES IN lt_reguh
   WHERE   laufd = lt_reguh-laufd
     AND   laufi = lt_reguh-laufi
     AND   xvorl = lt_reguh-xvorl
     AND   zbukr = lt_reguh-zbukr
     AND   lifnr = lt_reguh-lifnr
     AND   kunnr = lt_reguh-kunnr
     AND   empfg = lt_reguh-empfg
     AND   vblnr = lt_reguh-vblnr.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lt_regup[].
  ENDIF.

  SELECT   zbukr  hbkid  hktid  rzawe  chect
           lifnr  vblnr  gjahr  bancd
    INTO   TABLE lt_payr
    FROM   payr FOR ALL ENTRIES IN it_cdhdr
   WHERE   zbukr = it_cdhdr-bukrs
     AND   lifnr = it_cdhdr-lifnr
     AND   vblnr = it_cdhdr-augbl.
  IF     ( sy-subrc NE 0 ).
    CLEAR  lt_payr[].
  ENDIF.

*eject
  DELETE   lt_reguh                   WHERE lifnr IS INITIAL.
  SORT     lt_reguh            ASCENDING BY zbukr lifnr vblnr.
  DELETE   ADJACENT DUPLICATES         FROM lt_reguh
                                  COMPARING zbukr lifnr vblnr.

  DELETE   lt_regup                   WHERE lifnr IS INITIAL.
  SORT     lt_regup            ASCENDING BY zbukr lifnr vblnr
                                            bukrs belnr gjahr buzei.
  DELETE   ADJACENT DUPLICATES         FROM lt_regup
                                  COMPARING zbukr lifnr vblnr
                                            bukrs belnr gjahr buzei.

  SORT     lt_payr             ASCENDING BY zbukr lifnr vblnr.
  DELETE   ADJACENT DUPLICATES         FROM lt_payr
                                  COMPARING zbukr lifnr vblnr.

  CLEAR    ls_reguh.
  CLEAR    ls_payr.

  LOOP AT  lt_regup               ASSIGNING <fs_regup>.

    IF     ( ( ls_reguh-zbukr            NE <fs_regup>-zbukr ) OR
             ( ls_reguh-lifnr            NE <fs_regup>-lifnr ) OR
             ( ls_reguh-vblnr            NE <fs_regup>-vblnr )    ).

      CLEAR                                 ls_reguh.
      READ     TABLE lt_reguh          INTO ls_reguh
                                   WITH KEY zbukr = <fs_regup>-zbukr
                                            lifnr = <fs_regup>-lifnr
                                            vblnr = <fs_regup>-vblnr
                              BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CLEAR  ls_reguh.
      ENDIF.

      CLEAR                                 ls_payr.
      READ     TABLE lt_payr           INTO ls_payr
                                   WITH KEY zbukr = <fs_regup>-zbukr
                                            lifnr = <fs_regup>-lifnr
                                            vblnr = <fs_regup>-vblnr
                              BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CLEAR  ls_payr.
      ENDIF.

      CLEAR                                 ls_cdhdr.
      READ     TABLE it_cdhdr          INTO ls_cdhdr
                                   WITH KEY bukrs = <fs_regup>-zbukr
                                            augbl = <fs_regup>-vblnr
                              BINARY SEARCH.
      IF     ( sy-subrc NE 0 ).
        CLEAR  ls_cdhdr.
      ENDIF.

*eject
* Check ULID (payment ULID)
      CLEAR                                 lv_ch_ulid.
      CONCATENATE         ls_reguh-vblnr    ls_reguh-zbukr
                                            <fs_regup>-gjahr
                                       INTO lv_ch_ulid
                               SEPARATED BY '_'.

* Encashment date
      CLEAR    lv_bancd_c.
      IF     ( ls_payr-bancd             IS NOT INITIAL ).
        CONCATENATE                         ls_payr-bancd+0(4) '-'
                                            ls_payr-bancd+4(2) '-'
                                            ls_payr-bancd+6(2)
                                       INTO lv_bancd_c.
      ENDIF.

* Payment total
      CLEAR                                 lv_p_total.
      WRITE    ls_reguh-rwbtr            TO lv_p_total
                                   CURRENCY ls_reguh-waers.
      TRANSLATE                             lv_p_total USING ', - '.
      CONDENSE                              lv_p_total NO-GAPS.
      IF     ( ls_reguh-rwbtr            LT 0 ).
        CONCATENATE '-' lv_p_total     INTO lv_p_total.
      ENDIF.

* Payment date
      CLEAR                                 lv_augdt_c.
      CONCATENATE                  ls_reguh-zaldt+0(4) '-'
                                   ls_reguh-zaldt+4(2) '-'
                                   ls_reguh-zaldt+6(2)
                                       INTO lv_augdt_c.

      CLEAR                                 ls_final.
      MOVE     p_erpid                   TO ls_final-erpid.
      MOVE     lv_ch_ulid                TO ls_final-ch_ulid.
      MOVE     ls_reguh-zbukr            TO ls_final-bukrs.
      MOVE     ls_reguh-vblnr            TO ls_final-augbl.
      MOVE     ls_payr-chect             TO ls_final-chect.
      MOVE     'Y'                       TO ls_final-p_voided.
      MOVE     lv_p_total                TO ls_final-p_total.
      MOVE     ls_reguh-waers            TO ls_final-waers.
      MOVE     lv_augdt_c                TO ls_final-augdt.
      MOVE     lv_bancd_c                TO ls_final-bancd.
      MOVE     ls_reguh-rzawe            TO ls_final-zlsch.

    ENDIF.

*eject
* Payment History ULID
    CLEAR                                   lv_ph_ulid.
    CONCATENATE            <fs_regup>-belnr <fs_regup>-bukrs
                           <fs_regup>-gjahr <fs_regup>-vblnr
                                       INTO lv_ph_ulid
                               SEPARATED BY '_'.

* Invoice ULID
    CLEAR                                   lv_i_ulid.
    CONCATENATE            <fs_regup>-belnr <fs_regup>-bukrs
                                            <fs_regup>-gjahr
                                       INTO lv_i_ulid
                               SEPARATED BY '_'.

* Invoice total
    CLEAR                                   lv_i_total.
    WRITE    <fs_regup>-wrbtr            TO lv_i_total
                                   CURRENCY ls_reguh-waers.
    TRANSLATE                               lv_i_total USING ', - '.
    CONDENSE                                lv_i_total NO-GAPS.
    IF     ( <fs_regup>-shkzg            EQ 'H' ).
      CONCATENATE '-' lv_i_total       INTO lv_i_total.
    ENDIF.

* Vendor ULID
    CLEAR                                   lv_v_ulid.
    CONCATENATE            <fs_regup>-lifnr <fs_regup>-bukrs
                                       INTO lv_v_ulid
                               SEPARATED BY '_'.

* Payee
    CLEAR                                   lv_empfb.
    IF     ( <fs_regup>-empfg+00(01)     EQ '>' ).
      MOVE   <fs_regup>-empfg+01(10)     TO lv_empfb.
    ENDIF.

* Last update date and time
    CLEAR                                   lv_lc_date.
    CONCATENATE                             ls_cdhdr-udate
                                            ls_cdhdr-utime
                                       INTO lv_lc_date.

*eject
    CLEAR                                   ls_final-ph_ulid.
    MOVE     lv_ph_ulid                  TO ls_final-ph_ulid.
    CLEAR                                   ls_final-i_ulid.
    MOVE     lv_i_ulid                   TO ls_final-i_ulid.
    CLEAR                                   ls_final-inv_id.
    MOVE     lv_i_ulid                   TO ls_final-inv_id.
    CLEAR                                   ls_final-xblnr.
    MOVE     <fs_regup>-xblnr            TO ls_final-xblnr.
    CLEAR                                   ls_final-i_total.
    MOVE     lv_i_total                  TO ls_final-i_total.
    CLEAR                                   ls_final-i_paid.
    MOVE     lv_i_total                  TO ls_final-i_paid.
    CLEAR                                   ls_final-belnr.
    MOVE     <fs_regup>-belnr            TO ls_final-belnr.
    CLEAR                                   ls_final-v_ulid.
    MOVE     lv_v_ulid                   TO ls_final-v_ulid.
    CLEAR                                   ls_final-lifnr.
    MOVE     <fs_regup>-lifnr            TO ls_final-lifnr.
    CLEAR                                   ls_final-empfb.
    MOVE     lv_empfb                    TO ls_final-empfb.
    CLEAR                                   ls_final-gjahr.
    MOVE     <fs_regup>-gjahr            TO ls_final-gjahr.
    CLEAR                                   ls_final-lc_date.
    MOVE     lv_lc_date                  TO ls_final-lc_date.

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

    IF     ( rb_pres                     IS NOT INITIAL ).

      APPEND   ls_output                 TO gt_output.

      ADD      1                         TO gv_cn_recs_total.

    ELSEIF ( rb_appl                     IS NOT INITIAL ).

* Close the file and open a new file
      IF       ( gv_cn_recs_file         GE gv_cn_recs_max ).
        IF   ( ( p_maxrec                IS NOT INITIAL ) AND
               ( gv_filename             IS NOT INITIAL )     ).

          CLOSE    DATASET gv_filename.

          CLEAR    gv_filename.
          CLEAR    gv_cn_recs_file.

          PERFORM  f_open_dataset.

        ENDIF.
      ENDIF.

      IF       ( gv_filename             IS NOT INITIAL ).

        TRANSFER   ls_output             TO gv_filename.
        ADD      1                       TO gv_cn_recs_file.
        ADD      1                       TO gv_cn_recs_total.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_process_voided_docs
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
  CONCATENATE text-c01  text-c02  text-c03  text-c04
              text-c05  text-c06  text-c07  text-c08
              text-c09  text-c10  text-c11  text-c12
              text-c13  text-c14  text-c15  text-c16
              text-c17  text-c18  text-c19  text-c20
              text-c21  text-c22  text-c23
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
