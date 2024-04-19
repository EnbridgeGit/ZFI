*&---------------------------------------------------------------------*
*&  Include           ZFAPI122_OPEN_INVC_ITEMS_F01
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI122_OPEN_INVC_ITEMS                       *
* Include Program    :  ZFAPI122_OPEN_INVC_ITEMS_F01                   *
* Author             :  Paul Karunakar                                 *
* Creation Date      :  16-Apr-2018                                    *
* Application Area   :  FICO                                           *
* Description        :  Open Invoice data from each of the three SAP   *
*                       instances will be extracted in a delimited     *
*                       file and sent to IAP.                          *
*                                                                      *
*&---------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 16-Apr-2018  KPAL        D30K928646  CHG0108943  Initial Development *
*                          D30K928913, D30K929061                      *
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

  CLEAR    gt_bsik_key[].
  CLEAR    gt_bvorg[].
  CLEAR    gt_bkpf[].
  CLEAR    gt_bseg[].
  CLEAR    gt_vbsegk_key[].
  CLEAR    gt_vbkpf[].
  CLEAR    gt_vbsegk[].
  CLEAR    gt_wtht[].
  CLEAR    gt_t001[].
  CLEAR    gt_t059u[].
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
* Select the company codes
  SELECT   bukrs  land1
    INTO   TABLE gt_t001
    FROM   t001.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_t001 ASCENDING BY bukrs.
  ELSE.
    CLEAR  gt_t001.
  ENDIF.

* Select the withholding tax names
  SELECT   land1  witht  text40
    INTO   TABLE gt_t059u
    FROM   t059u
   WHERE   spras = sy-langu.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_t059u ASCENDING BY land1 witht text40.
  ELSE.
    CLEAR  gt_t059u[].
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
  IF     ( cb_invco IS NOT INITIAL ).
    PERFORM  f_process_open_main.
  ENDIF.

* Process the parked documents (main)
  IF     ( cb_invcp IS NOT INITIAL ).
    PERFORM  f_process_parked_main.
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
*&      Form  f_process_open_main
*&---------------------------------------------------------------------*
*       Process the open documents (main)
*----------------------------------------------------------------------*
FORM f_process_open_main.

  DATA:    lt_bsik_key                 TYPE gtt_bsik_key,
           ls_bsik_key                 TYPE gty_bsik_key,
           ls_bkpf                     TYPE gty_bkpf,
           lt_bseg                     TYPE gtt_bseg,
           lt_wtht                     TYPE gtt_wtht,
           lt_cdhdr                    TYPE STANDARD TABLE OF cdhdr,
           ls_cdhdr                    TYPE cdhdr.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS: lc_blocksize              TYPE syindex VALUE 250.

  FIELD-SYMBOLS: <fs_bseg>             TYPE gty_bseg,
                 <fs_wtht>             TYPE gty_wtht.

  CLEAR    gt_bsik_key[].
  CLEAR    lt_bsik_key[].

  IF     ( rb_fload                      IS NOT INITIAL ).

    SELECT   bukrs  belnr  gjahr
      INTO   TABLE gt_bsik_key
      FROM   bsik
     WHERE   bukrs IN s_bukrs
       AND   lifnr IN s_lifnr
       AND   gjahr IN s_gjahr
       AND   belnr IN s_belnr
       AND   umsks  = space
       AND   umskz  = space.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_bsik_key[].
    ENDIF.

*eject
  ELSEIF ( rb_dload                      IS NOT INITIAL ).

    CLEAR    lt_cdhdr[].

    CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
      EXPORTING
        date_of_change             = s_date-low
        objectclass                = 'BELEG'
        date_until                 = s_date-high
      TABLES
        i_cdhdr                    = lt_cdhdr
      EXCEPTIONS
        no_position_found          = 1
        wrong_access_to_archive    = 2
        time_zone_conversion_error = 3
        OTHERS                     = 4.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    CLEAR                                   ls_cdhdr.
    LOOP AT  lt_cdhdr                  INTO ls_cdhdr.
      CLEAR                                 ls_bsik_key.
      MOVE     ls_cdhdr-objectid+03(04)  TO ls_bsik_key-bukrs.
      MOVE     ls_cdhdr-objectid+07(10)  TO ls_bsik_key-belnr.
      MOVE     ls_cdhdr-objectid+17(04)  TO ls_bsik_key-gjahr.
      IF   ( ( ls_bsik_key-bukrs         IN s_bukrs ) AND
             ( ls_bsik_key-belnr         IN s_belnr ) AND
             ( ls_bsik_key-gjahr         IN s_gjahr )     ).
        APPEND ls_bsik_key               TO lt_bsik_key.
      ENDIF.
      CLEAR  ls_cdhdr.
    ENDLOOP.

    IF     ( lt_bsik_key[] IS NOT INITIAL ).

      SORT   lt_bsik_key ASCENDING BY bukrs gjahr belnr.

      SELECT   bukrs  belnr  gjahr
        INTO   TABLE gt_bsik_key
        FROM   bsik FOR ALL ENTRIES IN lt_bsik_key
       WHERE   bukrs  = lt_bsik_key-bukrs
         AND   gjahr  = lt_bsik_key-gjahr
         AND   belnr  = lt_bsik_key-belnr
         AND   lifnr IN s_lifnr
         AND   umsks  = space
         AND   umskz  = space.
      IF     ( sy-subrc NE 0 ).
        CLEAR  gt_bsik_key[].
      ENDIF.

    ENDIF.

  ENDIF.

*eject
  IF     ( gt_bsik_key[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     gt_bsik_key         ASCENDING BY bukrs belnr gjahr.
  DELETE   ADJACENT DUPLICATES         FROM gt_bsik_key
                                  COMPARING bukrs belnr gjahr.

* Process the documents in batches
  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch
    CLEAR             lt_bsik_key[].
    APPEND   LINES OF gt_bsik_key
                 FROM lv_index_lo
                   TO lv_index_hi
                   TO lt_bsik_key.

    IF     ( lt_bsik_key[] IS INITIAL ).
      EXIT.
    ENDIF.

    CLEAR    gt_bkpf[].
    CLEAR    gt_bseg[].
    CLEAR    gt_wtht[].

    SELECT   bukrs  belnr  gjahr  blart  bldat  budat
             monat  cpudt  usnam  tcode  bvorg  xblnr
             bktxt  waers  kursf  bstat  awtyp  awkey
             xmwst  xreversal
*             zz_iap_ritm_doc
      INTO   TABLE gt_bkpf
      FROM   bkpf FOR ALL ENTRIES IN lt_bsik_key
     WHERE   bukrs = lt_bsik_key-bukrs
       AND   belnr = lt_bsik_key-belnr
       AND   gjahr = lt_bsik_key-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_bkpf[].
    ENDIF.

*eject
    SELECT   bukrs  belnr  gjahr  buzei  bschl  koart
             umskz  shkzg  mwskz  wrbtr  wmwst  qsshb
             sgtxt  saknr  lifnr  zfbdt  zterm  zbd1t
             zbd2t  zbd3t  skfbt  wskto  zlsch  zlspr
             hbkid  bvtyp  qbshb  uzawe  empfb
      INTO   TABLE gt_bseg
      FROM   bseg FOR ALL ENTRIES IN lt_bsik_key
     WHERE   bukrs = lt_bsik_key-bukrs
       AND   belnr = lt_bsik_key-belnr
       AND   gjahr = lt_bsik_key-gjahr.
    IF     ( sy-subrc EQ 0 ).
      DELETE gt_bseg WHERE koart <> 'K'.
    ELSE.
      CLEAR  gt_bseg[].
    ENDIF.

    SELECT   bukrs      belnr      gjahr      buzei
             witht      wt_withcd  wt_qsshb   wt_qbshb
      INTO   TABLE gt_wtht
      FROM   with_item FOR ALL ENTRIES IN lt_bsik_key
     WHERE   bukrs = lt_bsik_key-bukrs
       AND   belnr = lt_bsik_key-belnr
       AND   gjahr = lt_bsik_key-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_wtht[].
    ENDIF.

    SORT     gt_bkpf      ASCENDING BY bukrs belnr gjahr.
    SORT     gt_bseg      ASCENDING BY bukrs belnr gjahr buzei.
    SORT     gt_wtht      ASCENDING BY bukrs belnr gjahr buzei witht.

    CLEAR    ls_bkpf.
    CLEAR    lt_bseg[].
    CLEAR    lt_wtht[].

*eject
    LOOP AT    gt_bseg            ASSIGNING <fs_bseg>.

      APPEND   <fs_bseg>                 TO lt_bseg.

      AT END OF gjahr.

        CLEAR                               ls_bkpf.
        READ     TABLE gt_bkpf         INTO ls_bkpf
                                   WITH KEY bukrs = <fs_bseg>-bukrs
                                            belnr = <fs_bseg>-belnr
                                            gjahr = <fs_bseg>-gjahr
                              BINARY SEARCH.
        IF     ( sy-subrc EQ 0 ).

          CLEAR      lt_wtht[].

          READ TABLE gt_wtht       WITH KEY bukrs = <fs_bseg>-bukrs
                                            belnr = <fs_bseg>-belnr
                                            gjahr = <fs_bseg>-gjahr
                              BINARY SEARCH
                     TRANSPORTING NO FIELDS.
          lv_subrc = sy-subrc.
          lv_tabix = sy-tabix.
          IF     ( lv_subrc EQ 0 ).
            LOOP AT  gt_wtht      ASSIGNING <fs_wtht>
                                       FROM lv_tabix.
              IF     ( ( <fs_wtht>-bukrs EQ <fs_bseg>-bukrs ) AND
                       ( <fs_wtht>-belnr EQ <fs_bseg>-belnr ) AND
                       ( <fs_wtht>-gjahr EQ <fs_bseg>-gjahr )     ).
                APPEND   <fs_wtht>       TO lt_wtht.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.

          PERFORM  f_process_open_document
                                     TABLES lt_bseg
                                            lt_wtht
                                     USING  ls_bkpf.

        ENDIF.

        CLEAR  ls_bkpf.
        CLEAR  lt_bseg[].
        CLEAR  lt_wtht[].

      ENDAT.

    ENDLOOP.

  ENDDO.

ENDFORM.                    " f_process_open_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_open_document
*&---------------------------------------------------------------------*
*       Process an open document
*----------------------------------------------------------------------*
FORM f_process_open_document
  TABLES   it_bseg                     TYPE gtt_bseg
           it_wtht                     TYPE gtt_wtht
  USING    is_bkpf                     TYPE gty_bkpf.

  DATA:    lt_bseg                     TYPE gtt_bseg,
           ls_bseg_v                   TYPE gty_bseg,
           lt_wtht                     TYPE gtt_wtht,
           ls_bkpf                     TYPE gty_bkpf,
           ls_bvorg                    TYPE gty_bvorg,
           ls_header                   TYPE gty_header,
           ls_vendor                   TYPE gty_vendor,
           ls_wht_1                    TYPE gty_wht,
           ls_wht_2                    TYPE gty_wht.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_lifnr                    TYPE lifnr,
           lv_fl_delete                TYPE xflag.

  FIELD-SYMBOLS: <fs_bseg>             TYPE gty_bseg.

  CLEAR    lt_bseg[].
  CLEAR    ls_bseg_v.
  CLEAR    lt_wtht[].
  CLEAR    ls_bkpf.
  CLEAR    lv_fl_delete.

*eject
* Get the cross company line items
  IF     ( is_bkpf-bvorg                 IS INITIAL ).

    lt_bseg[] = it_bseg[].
    lt_wtht[] = it_wtht[].
    ls_bkpf   = is_bkpf.

  ELSE.

    READ     TABLE gt_bvorg        WITH KEY bvorg = is_bkpf-bvorg
                              BINARY SEARCH
                     TRANSPORTING NO FIELDS.
    lv_subrc = sy-subrc.
    lv_tabix = sy-tabix.
    IF     ( lv_subrc EQ 0 ).
      RETURN.
    ELSE.
      CLEAR                                 ls_bvorg.
      MOVE     is_bkpf-bvorg             TO ls_bvorg-bvorg.
      INSERT   ls_bvorg                INTO gt_bvorg INDEX lv_tabix.
    ENDIF.

    PERFORM  f_get_cc_docs         TABLES   lt_bseg
                                            lt_wtht
                                   USING    is_bkpf-bvorg
                                   CHANGING ls_bkpf.

  ENDIF.

  SORT     lt_bseg ASCENDING BY bukrs belnr gjahr.

*eject
* Set the vendor line item
  CLEAR                                     ls_bseg_v.
  READ     TABLE lt_bseg               INTO ls_bseg_v
                                   WITH KEY bukrs = ls_bkpf-bukrs
                                            belnr = ls_bkpf-belnr
                                            gjahr = ls_bkpf-gjahr
                                            koart = 'K'.
  IF     ( sy-subrc NE 0 ).
    CLEAR  ls_bseg_v.
  ENDIF.

  LOOP AT  lt_bseg                ASSIGNING <fs_bseg>
                                      WHERE koart = 'K'.

    IF           ( <fs_bseg>+00(21)      EQ ls_bseg_v+00(21) ).
    ELSEIF       ( ls_bseg_v-bukrs       IS INITIAL ).
      MOVE         <fs_bseg>             TO ls_bseg_v.
    ELSEIF       ( <fs_bseg>-lifnr       NE ls_bseg_v-lifnr ).
      lv_fl_delete = abap_true. "more than one vendor exists
    ELSE.
      IF         ( <fs_bseg>-shkzg       EQ ls_bseg_v-shkzg ).
        ADD        <fs_bseg>-wrbtr       TO ls_bseg_v-wrbtr.
        ADD        <fs_bseg>-wmwst       TO ls_bseg_v-wmwst.
        ADD        <fs_bseg>-qsshb       TO ls_bseg_v-qsshb.
        ADD        <fs_bseg>-skfbt       TO ls_bseg_v-skfbt.
        ADD        <fs_bseg>-wskto       TO ls_bseg_v-wskto.
        ADD        <fs_bseg>-qbshb       TO ls_bseg_v-qbshb.
      ELSE.
        SUBTRACT   <fs_bseg>-wrbtr     FROM ls_bseg_v-wrbtr.
        SUBTRACT   <fs_bseg>-wmwst     FROM ls_bseg_v-wmwst.
        SUBTRACT   <fs_bseg>-qsshb     FROM ls_bseg_v-qsshb.
        SUBTRACT   <fs_bseg>-skfbt     FROM ls_bseg_v-skfbt.
        SUBTRACT   <fs_bseg>-wskto     FROM ls_bseg_v-wskto.
        SUBTRACT   <fs_bseg>-qbshb     FROM ls_bseg_v-qbshb.
        IF       ( ls_bseg_v-wrbtr       LT 0 ).
          MULTIPLY ls_bseg_v-wrbtr       BY -1.
          MULTIPLY ls_bseg_v-wmwst       BY -1.
          MULTIPLY ls_bseg_v-qsshb       BY -1.
          MULTIPLY ls_bseg_v-skfbt       BY -1.
          MULTIPLY ls_bseg_v-wskto       BY -1.
          MULTIPLY ls_bseg_v-qbshb       BY -1.
          IF     ( ls_bseg_v-shkzg       EQ 'S' ).
            CLEAR                           ls_bseg_v-shkzg.
            MOVE   'H'                   TO ls_bseg_v-shkzg.
          ELSEIF ( ls_bseg_v-shkzg       EQ 'H' ).
            CLEAR                           ls_bseg_v-shkzg.
            MOVE   'S'                   TO ls_bseg_v-shkzg.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

*eject
  IF   ( ( ls_bseg_v-lifnr               IS INITIAL     ) OR
         ( lv_fl_delete                  IS NOT INITIAL )    ).
    RETURN.
  ENDIF.

  IF     ( s_ktokk[]                     IS NOT INITIAL ).
    SELECT   SINGLE lifnr
      INTO   lv_lifnr
      FROM   lfa1
     WHERE   lifnr  = ls_bseg_v-lifnr
       AND   ktokk IN s_ktokk.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.
  ENDIF.

* Withholding tax
  CLEAR    ls_wht_1.
  CLEAR    ls_wht_2.

  IF     ( lt_wtht[]                     IS NOT INITIAL ).

    PERFORM  f_get_withholding_tax   TABLES lt_wtht
                                      USING ls_bseg_v-bukrs
                                   CHANGING ls_wht_1
                                            ls_wht_2.

  ENDIF.

* Transform the document data for output
  CLEAR    ls_header.
  CLEAR    ls_vendor.

  MOVE-CORRESPONDING is_bkpf             TO ls_header.
  MOVE               is_bkpf-bukrs       TO ls_header-ausbk.
  MOVE-CORRESPONDING ls_bseg_v           TO ls_vendor.
  MOVE               is_bkpf-bukrs       TO ls_vendor-ausbk.

* Output the data
  PERFORM  f_output_data             USING  ls_header
                                            ls_vendor
                                            ls_wht_1
                                            ls_wht_2.

ENDFORM.                    " f_process_open_document
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_cc_docs
*&---------------------------------------------------------------------*
*       Get the cross company line items
*----------------------------------------------------------------------*
FORM f_get_cc_docs
  TABLES   ct_bseg                     TYPE gtt_bseg
           ct_wtht                     TYPE gtt_wtht
  USING    iv_bvorg                    TYPE bvorg
  CHANGING cs_bkpf                     TYPE gty_bkpf.

  DATA:    lt_bseg                     TYPE gtt_bseg,
           lt_wtht                     TYPE gtt_wtht,
           lt_bkpf                     TYPE gtt_bkpf,
           ls_bkpf                     TYPE gty_bkpf,
           lt_bvor                     TYPE STANDARD TABLE OF bvor.

  DATA:    lv_bukrs                    TYPE bukrs.

  CLEAR    ct_bseg[].
  CLEAR    ct_wtht[].
  CLEAR    cs_bkpf.

  IF     ( iv_bvorg IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_bseg[].
  CLEAR    lt_wtht[].
  CLEAR    lt_bkpf[].
  CLEAR    lt_bvor[].

  SELECT   *
    INTO   TABLE lt_bvor
    FROM   bvor
   WHERE   bvorg = iv_bvorg.
  IF     ( sy-subrc EQ 0 ).

    SELECT   bukrs  belnr  gjahr  blart  bldat  budat
             monat  cpudt  usnam  tcode  bvorg  xblnr
             bktxt  waers  kursf  bstat  awtyp  awkey
             xmwst  xreversal
*             zz_iap_ritm_doc
      INTO   TABLE lt_bkpf
      FROM   bkpf FOR ALL ENTRIES IN lt_bvor
     WHERE   bukrs = lt_bvor-bukrs
       AND   belnr = lt_bvor-belnr
       AND   gjahr = lt_bvor-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_bkpf[].
    ENDIF.

*eject
    SELECT   bukrs  belnr  gjahr  buzei  bschl  koart
             umskz  shkzg  mwskz  wrbtr  wmwst  qsshb
             sgtxt  saknr  lifnr  zfbdt  zterm  zbd1t
             zbd2t  zbd3t  skfbt  wskto  zlsch  zlspr
             hbkid  bvtyp  qbshb  uzawe  empfb
      INTO   TABLE lt_bseg
      FROM   bseg FOR ALL ENTRIES IN lt_bvor
     WHERE   bukrs = lt_bvor-bukrs
       AND   belnr = lt_bvor-belnr
       AND   gjahr = lt_bvor-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_bseg[].
    ENDIF.

    SELECT   bukrs      belnr      gjahr      buzei
             witht      wt_withcd  wt_qsshb   wt_qbshb
      INTO   TABLE lt_wtht
      FROM   with_item FOR ALL ENTRIES IN lt_bvor
     WHERE   bukrs = lt_bvor-bukrs
       AND   belnr = lt_bvor-belnr
       AND   gjahr = lt_bvor-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_wtht[].
    ENDIF.

    CLEAR                                   lv_bukrs.
    MOVE     iv_bvorg+10(04)             TO lv_bukrs.

    CLEAR                                   ls_bkpf.
    READ     TABLE lt_bkpf             INTO ls_bkpf
                                   WITH KEY bukrs = lv_bukrs.
    IF     ( sy-subrc NE 0 ).
      CLEAR  ls_bkpf.
    ENDIF.

    DELETE   lt_bseg                  WHERE koart <> 'K'.

    cs_bkpf   = ls_bkpf.
    ct_bseg[] = lt_bseg[].
    ct_wtht[] = lt_wtht[].

  ENDIF.

ENDFORM.                    " f_get_cc_docs
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_parked_main
*&---------------------------------------------------------------------*
*       Process the parked documents (main)
*----------------------------------------------------------------------*
FORM f_process_parked_main.

  DATA:    lt_vbsegk_key               TYPE gtt_vbsegk_key,
           ls_vbsegk_key               TYPE gty_vbsegk_key,
           lt_vbkpf                    TYPE gtt_vbkpf,
           lt_vbsegk                   TYPE gtt_vbsegk,
           ls_vbsegk                   TYPE gty_vbsegk,
           lt_wtht                     TYPE gtt_wtht,
           ls_wtht                     TYPE gtt_wtht,
           lt_cdhdr                    TYPE STANDARD TABLE OF cdhdr,
           ls_cdhdr                    TYPE cdhdr.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS: lc_blocksize              TYPE syindex VALUE 250.

  FIELD-SYMBOLS: <fs_vbkpf>            TYPE gty_vbkpf,
                 <fs_vbsegk>           TYPE gty_vbsegk,
                 <fs_wtht>             TYPE gty_wtht.

  CLEAR    gt_vbsegk_key[].
  CLEAR    lt_vbsegk_key[].

  IF     ( rb_fload                      IS NOT INITIAL ).

    SELECT   ausbk  belnr  gjahr
      INTO   TABLE gt_vbsegk_key
      FROM   vbsegk
     WHERE   ausbk IN s_bukrs
       AND   lifnr IN s_lifnr
       AND   gjahr IN s_gjahr
       AND   belnr IN s_belnr
       AND   umsks  = space
       AND   umskz  = space.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_vbsegk_key[].
    ENDIF.

*eject
  ELSEIF ( rb_dload                      IS NOT INITIAL ).

    CLEAR    lt_cdhdr[].

    CALL FUNCTION 'CHANGEDOCUMENT_READ_HEADERS'
      EXPORTING
        date_of_change             = s_date-low
        objectclass                = 'BELEGV'
        date_until                 = s_date-high
      TABLES
        i_cdhdr                    = lt_cdhdr
      EXCEPTIONS
        no_position_found          = 1
        wrong_access_to_archive    = 2
        time_zone_conversion_error = 3
        OTHERS                     = 4.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.

    CLEAR                                   ls_cdhdr.
    LOOP AT  lt_cdhdr                  INTO ls_cdhdr.
      CLEAR                                 ls_vbsegk_key.
      MOVE     ls_cdhdr-objectid+03(04)  TO ls_vbsegk_key-ausbk.
      MOVE     ls_cdhdr-objectid+11(10)  TO ls_vbsegk_key-belnr.
      MOVE     ls_cdhdr-objectid+21(04)  TO ls_vbsegk_key-gjahr.
      IF   ( ( ls_vbsegk_key-ausbk       IN s_bukrs ) AND
             ( ls_vbsegk_key-belnr       IN s_belnr ) AND
             ( ls_vbsegk_key-gjahr       IN s_gjahr )     ).
        APPEND ls_vbsegk_key             TO lt_vbsegk_key.
      ENDIF.
      CLEAR  ls_cdhdr.
    ENDLOOP.

    IF     ( lt_vbsegk_key[] IS NOT INITIAL ).

      SORT   lt_vbsegk_key ASCENDING BY ausbk belnr gjahr.

      SELECT   ausbk  belnr  gjahr
        INTO   TABLE gt_vbsegk_key
        FROM   vbsegk FOR ALL ENTRIES IN lt_vbsegk_key
       WHERE   ausbk  = lt_vbsegk_key-ausbk
         AND   belnr  = lt_vbsegk_key-belnr
         AND   gjahr  = lt_vbsegk_key-gjahr
         AND   lifnr IN s_lifnr
         AND   umsks  = space
         AND   umskz  = space.
      IF     ( sy-subrc NE 0 ).
        CLEAR  gt_vbsegk_key[].
      ENDIF.

    ENDIF.

  ENDIF.

*eject
  IF     ( gt_vbsegk_key[] IS INITIAL ).
    RETURN.
  ENDIF.

  SORT     gt_vbsegk_key       ASCENDING BY ausbk belnr gjahr.
  DELETE   ADJACENT DUPLICATES         FROM gt_vbsegk_key
                                  COMPARING ausbk belnr gjahr.

* Process the documents in batches
  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch
    CLEAR             lt_vbsegk_key[].
    APPEND   LINES OF gt_vbsegk_key
                 FROM lv_index_lo
                   TO lv_index_hi
                   TO lt_vbsegk_key.

    IF     ( lt_vbsegk_key[] IS INITIAL ).
      EXIT.
    ENDIF.

    CLEAR    gt_vbkpf[].
    CLEAR    gt_vbsegk[].
    CLEAR    gt_wtht[].

    SELECT   ausbk  belnr  gjahr  bukrs  bstat  blart
             bldat  budat  monat  cpudt  usnam  tcode
             bvorg  xblnr  bktxt  waers  kursf  xmwst
             awtyp  awkey
      INTO   TABLE gt_vbkpf
      FROM   vbkpf FOR ALL ENTRIES IN lt_vbsegk_key
     WHERE   ausbk = lt_vbsegk_key-ausbk
       AND   belnr = lt_vbsegk_key-belnr
       AND   gjahr = lt_vbsegk_key-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_vbkpf[].
    ENDIF.

*eject
    SELECT   ausbk  belnr  gjahr  bzkey  bukrs  buzei
             bschl  umskz  shkzg  mwskz  wrbtr  wmwst
             qsshb  qbshb  sgtxt  hkont  lifnr  zfbdt
             zterm  zbd1t  zbd2t  zbd3t  skfbt  wskto
             zlsch  zlspr  uzawe  hbkid  bvtyp  empfb
      INTO   TABLE gt_vbsegk
      FROM   vbsegk FOR ALL ENTRIES IN lt_vbsegk_key
     WHERE   ausbk = lt_vbsegk_key-ausbk
       AND   belnr = lt_vbsegk_key-belnr
       AND   gjahr = lt_vbsegk_key-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_vbsegk.
    ENDIF.

    SELECT   bukrs      belnr      gjahr      buzei
             witht      wt_withcd  wt_qsshb   wt_qbshb
      INTO   TABLE gt_wtht
      FROM   with_item FOR ALL ENTRIES IN lt_vbsegk_key
     WHERE   bukrs = lt_vbsegk_key-ausbk
       AND   belnr = lt_vbsegk_key-belnr
       AND   gjahr = lt_vbsegk_key-gjahr.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gt_wtht[].
    ENDIF.

    SORT     gt_vbkpf     ASCENDING BY ausbk belnr gjahr bukrs.
    SORT     gt_vbsegk    ASCENDING BY ausbk belnr gjahr bzkey.
    SORT     gt_wtht      ASCENDING BY bukrs belnr gjahr buzei witht.

    CLEAR    lt_vbkpf[].
    CLEAR    lt_vbsegk[].
    CLEAR    lt_wtht[].

*eject
    LOOP AT    gt_vbkpf           ASSIGNING <fs_vbkpf>.

      APPEND   <fs_vbkpf>                TO lt_vbkpf.

      AT END OF gjahr.

        CLEAR    lt_vbsegk[].
        CLEAR    lt_wtht[].

        READ     TABLE gt_vbsegk   WITH KEY ausbk = <fs_vbkpf>-ausbk
                                            belnr = <fs_vbkpf>-belnr
                                            gjahr = <fs_vbkpf>-gjahr
                              BINARY SEARCH
                     TRANSPORTING NO FIELDS.
        lv_subrc = sy-subrc.
        lv_tabix = sy-tabix.
        IF     ( lv_subrc EQ 0 ).
          LOOP AT    gt_vbsegk    ASSIGNING <fs_vbsegk>
                                       FROM lv_tabix.
            IF   ( ( <fs_vbsegk>-ausbk   EQ <fs_vbkpf>-ausbk ) AND
                   ( <fs_vbsegk>-belnr   EQ <fs_vbkpf>-belnr ) AND
                   ( <fs_vbsegk>-gjahr   EQ <fs_vbkpf>-gjahr )     ).
              APPEND <fs_vbsegk>         TO lt_vbsegk.
            ELSE.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.

*eject
        READ     TABLE gt_wtht     WITH KEY bukrs = <fs_vbkpf>-ausbk
                                            belnr = <fs_vbkpf>-belnr
                                            gjahr = <fs_vbkpf>-gjahr
                              BINARY SEARCH
                     TRANSPORTING NO FIELDS.
        lv_subrc = sy-subrc.
        lv_tabix = sy-tabix.
        IF     ( lv_subrc EQ 0 ).
          LOOP AT    gt_wtht      ASSIGNING <fs_wtht>
                                       FROM lv_tabix.
            IF   ( ( <fs_wtht>-bukrs     EQ <fs_vbkpf>-ausbk ) AND
                   ( <fs_wtht>-belnr     EQ <fs_vbkpf>-belnr ) AND
                   ( <fs_wtht>-gjahr     EQ <fs_vbkpf>-gjahr )     ).
              APPEND <fs_wtht>           TO lt_wtht.
            ELSE.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.

        PERFORM  f_process_parked_document
                                     TABLES lt_vbkpf
                                            lt_vbsegk
                                            lt_wtht
                                     USING  <fs_vbkpf>-ausbk.

        CLEAR    lt_vbkpf[].
        CLEAR    lt_vbsegk[].
        CLEAR    lt_wtht[].

      ENDAT.

    ENDLOOP.

  ENDDO.

ENDFORM.                    " f_process_parked_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_parked_document
*&---------------------------------------------------------------------*
*       Process a parked document
*----------------------------------------------------------------------*
FORM f_process_parked_document
  TABLES   it_vbkpf                    TYPE gtt_vbkpf
           it_vbsegk                   TYPE gtt_vbsegk
           it_wtht                     TYPE gtt_wtht
  USING    iv_ausbk                    TYPE ausbk.

  DATA:    ls_vbkpf                    TYPE gty_vbkpf,
           ls_vbsegk_v                 TYPE gty_vbsegk,
           ls_header                   TYPE gty_header,
           ls_vendor                   TYPE gty_vendor,
           ls_wht_1                    TYPE gty_wht,
           ls_wht_2                    TYPE gty_wht.

  DATA:    lv_lifnr                    TYPE lifnr,
           lv_fl_delete                TYPE xflag.

  FIELD-SYMBOLS: <fs_vbsegk>           TYPE gty_vbsegk.

* Read the document header
  CLEAR                                     ls_vbkpf.
  READ     TABLE it_vbkpf              INTO ls_vbkpf
                                   WITH KEY ausbk = iv_ausbk
                                            bukrs = iv_ausbk.
  IF     ( sy-subrc NE 0 ).
    CLEAR  ls_vbkpf.
  ENDIF.

*eject
* Set the vendor line item
  CLEAR                                     ls_vbsegk_v.
  READ     TABLE it_vbsegk             INTO ls_vbsegk_v
                                   WITH KEY ausbk = ls_vbkpf-ausbk
                                            belnr = ls_vbkpf-belnr
                                            gjahr = ls_vbkpf-gjahr
                                            bukrs = ls_vbkpf-ausbk.
  IF     ( sy-subrc NE 0 ).
    CLEAR  ls_vbsegk_v.
  ENDIF.

  LOOP AT  it_vbsegk              ASSIGNING <fs_vbsegk>.

    IF           ( <fs_vbsegk>+00(25)    EQ ls_vbsegk_v+00(25) ).
    ELSEIF       ( ls_vbsegk_v-ausbk     IS INITIAL ).
      MOVE         <fs_vbsegk>           TO ls_vbsegk_v.
    ELSEIF       ( <fs_vbsegk>-lifnr     NE ls_vbsegk_v-lifnr ).
      lv_fl_delete = abap_true. "more than one vendor exists
    ELSE.
      IF         ( <fs_vbsegk>-shkzg     EQ ls_vbsegk_v-shkzg ).
        ADD        <fs_vbsegk>-wrbtr     TO ls_vbsegk_v-wrbtr.
        ADD        <fs_vbsegk>-wmwst     TO ls_vbsegk_v-wmwst.
        ADD        <fs_vbsegk>-qsshb     TO ls_vbsegk_v-qsshb.
        ADD        <fs_vbsegk>-qbshb     TO ls_vbsegk_v-qbshb.
        ADD        <fs_vbsegk>-skfbt     TO ls_vbsegk_v-skfbt.
        ADD        <fs_vbsegk>-wskto     TO ls_vbsegk_v-wskto.
      ELSE.
        SUBTRACT   <fs_vbsegk>-wrbtr   FROM ls_vbsegk_v-wrbtr.
        SUBTRACT   <fs_vbsegk>-wmwst   FROM ls_vbsegk_v-wmwst.
        SUBTRACT   <fs_vbsegk>-qsshb   FROM ls_vbsegk_v-qsshb.
        SUBTRACT   <fs_vbsegk>-qbshb   FROM ls_vbsegk_v-qbshb.
        SUBTRACT   <fs_vbsegk>-skfbt   FROM ls_vbsegk_v-skfbt.
        SUBTRACT   <fs_vbsegk>-wskto   FROM ls_vbsegk_v-wskto.
        IF       ( ls_vbsegk_v-wrbtr     LT 0 ).
          MULTIPLY ls_vbsegk_v-wrbtr     BY -1.
          MULTIPLY ls_vbsegk_v-wmwst     BY -1.
          MULTIPLY ls_vbsegk_v-qsshb     BY -1.
          MULTIPLY ls_vbsegk_v-qbshb     BY -1.
          MULTIPLY ls_vbsegk_v-skfbt     BY -1.
          MULTIPLY ls_vbsegk_v-wskto     BY -1.
          IF     ( ls_vbsegk_v-shkzg     EQ 'S' ).
            CLEAR                           ls_vbsegk_v-shkzg.
            MOVE   'H'                   TO ls_vbsegk_v-shkzg.
          ELSEIF ( ls_vbsegk_v-shkzg     EQ 'H' ).
            CLEAR                           ls_vbsegk_v-shkzg.
            MOVE   'S'                   TO ls_vbsegk_v-shkzg.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

*eject
  IF   ( ( ls_vbsegk_v-lifnr             IS INITIAL     ) OR
         ( lv_fl_delete                  IS NOT INITIAL )    ).
    RETURN.
  ENDIF.

  IF     ( s_ktokk[]                     IS NOT INITIAL ).
    SELECT   SINGLE lifnr
      INTO   lv_lifnr
      FROM   lfa1
     WHERE   lifnr  = ls_vbsegk_v-lifnr
       AND   ktokk IN s_ktokk.
    IF     ( sy-subrc NE 0 ).
      RETURN.
    ENDIF.
  ENDIF.

* Withholding tax
  CLEAR    ls_wht_1.
  CLEAR    ls_wht_2.

  IF     ( it_wtht[]                     IS NOT INITIAL ).

    PERFORM  f_get_withholding_tax   TABLES it_wtht
                                      USING ls_vbsegk_v-ausbk
                                   CHANGING ls_wht_1
                                            ls_wht_2.

  ENDIF.

*eject
* Transform the document data for output
  CLEAR    ls_header.
  CLEAR    ls_vendor.

  MOVE-CORRESPONDING ls_vbkpf            TO ls_header.
  MOVE-CORRESPONDING ls_vbsegk_v         TO ls_vendor.

* Output the data
  PERFORM  f_output_data             USING  ls_header
                                            ls_vendor
                                            ls_wht_1
                                            ls_wht_2.

ENDFORM.                    " f_process_parked_document
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_withholding_tax
*&---------------------------------------------------------------------*
*       Get the withholding tax
*----------------------------------------------------------------------*
FORM f_get_withholding_tax
  TABLES   it_wtht                     TYPE gtt_wtht
  USING    iv_bukrs                    TYPE bukrs
  CHANGING cs_wht_1                    TYPE gty_wht
           cs_wht_2                    TYPE gty_wht.

  DATA:    lt_wht                      TYPE gtt_wht,
           ls_wht                      TYPE gty_wht,
           ls_t001                     TYPE gty_t001,
           ls_t059u                    TYPE gty_t059u.

  FIELD-SYMBOLS: <fs_wtht>             TYPE gty_wtht.

  CLEAR    cs_wht_1.
  CLEAR    cs_wht_2.

  IF     ( it_wtht[] IS INITIAL ).
    RETURN.
  ENDIF.

  LOOP AT  it_wtht                ASSIGNING <fs_wtht>.
    CLEAR                                   ls_wht.
    MOVE   <fs_wtht>-witht               TO ls_wht-witht.
    MOVE   <fs_wtht>-wt_withcd           TO ls_wht-wt_withcd.
    MOVE   <fs_wtht>-wt_qsshb            TO ls_wht-wt_qsshb.
    MOVE   <fs_wtht>-wt_qbshb            TO ls_wht-wt_qbshb.
    MOVE   <fs_wtht>-wt_qbshb            TO ls_wht-wt_qbshb_abs.
    IF   ( <fs_wtht>-wt_qbshb            LT 0 ).
      MULTIPLY ls_wht-wt_qbshb_abs       BY -1.
    ENDIF.
    COLLECT    ls_wht                  INTO lt_wht.
  ENDLOOP.

  SORT     lt_wht ASCENDING BY witht.

*eject
  CLEAR                                     ls_t001.
  READ     TABLE gt_t001               INTO ls_t001
                                   WITH KEY bukrs = iv_bukrs
                              BINARY SEARCH.
  IF     ( sy-subrc NE 0 ).
    CLEAR  ls_t001.
  ENDIF.

  CLEAR                                     ls_wht.
  READ     TABLE lt_wht                INTO ls_wht INDEX 1.
  IF     ( sy-subrc EQ 0 ).

    MOVE   ls_wht                        TO cs_wht_1.

    CLEAR                                   ls_t059u.
    READ     TABLE gt_t059u            INTO ls_t059u
                                   WITH KEY land1 = ls_t001-land1
                                            witht = ls_wht-witht.
    IF     ( sy-subrc EQ 0 ).

      MOVE   ls_t059u-text40             TO cs_wht_1-text40.

    ENDIF.

  ENDIF.

  CLEAR                                     ls_wht.
  READ     TABLE lt_wht                INTO ls_wht INDEX 2.
  IF     ( sy-subrc EQ 0 ).

    MOVE   ls_wht                        TO cs_wht_2.

    CLEAR                                   ls_t059u.
    READ     TABLE gt_t059u            INTO ls_t059u
                                   WITH KEY land1 = ls_t001-land1
                                            witht = ls_wht-witht.
    IF     ( sy-subrc EQ 0 ).

      MOVE   ls_t059u-text40             TO cs_wht_2-text40.

    ENDIF.

  ENDIF.

ENDFORM.                    " f_get_withholding_tax
*eject
*&---------------------------------------------------------------------*
*&      Form  f_output_data
*&---------------------------------------------------------------------*
*       Output the data
*----------------------------------------------------------------------*
FORM f_output_data
  USING    is_header                   TYPE gty_header
           is_vendor                   TYPE gty_vendor
           is_wht_1                    TYPE gty_wht
           is_wht_2                    TYPE gty_wht.

  DATA:    lt_output                   TYPE gtt_output,
           ls_output                   TYPE gty_output,
           ls_rec                      TYPE gty_rec.

  DATA:    lv_qsshb_1                  TYPE char21,
           lv_qsshb_2                  TYPE char21,
           lv_qbshb_1                  TYPE char21,
           lv_qbshb_2                  TYPE char21,
           lv_wrbtr                    TYPE char16,
           lv_due_date                 TYPE datum,
           lv_bktxt                    TYPE bktxt,
           lv_reversed                 TYPE char1,
           lv_wmwst                    TYPE char16,
           lv_wskto                    TYPE char16,
           lv_skfbt                    TYPE char16,
           lv_adv_text                 TYPE string,
           lv_sgtxt_v                  TYPE sgtxt.

  DATA:    lv_fl_found                 TYPE xflag,
           lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i,
           lv_cn_recs                  TYPE i,
           lv_cn_recs_buff             TYPE i.

  FIELD-SYMBOLS: <fs_rec>              TYPE gty_rec,
                 <fs_output>           TYPE gty_output,
                 <fs_attr>             TYPE ANY.

*eject
  CLEAR    lt_output[].
  CLEAR    ls_output.
  CLEAR    ls_rec.
  CLEAR    lv_fl_found.
  CLEAR    lv_dtype.
  CLEAR    lv_cn_comp.

  ASSIGN   ls_rec                        TO <fs_rec>.

  DESCRIBE FIELD <fs_rec> TYPE lv_dtype COMPONENTS lv_cn_comp.

* Withholding tax
  CLEAR    lv_qsshb_1.
  CLEAR    lv_qbshb_1.
  CLEAR    lv_qsshb_2.
  CLEAR    lv_qbshb_2.

  IF       ( is_wht_1-witht              IS NOT INITIAL ).

    WRITE    is_wht_1-wt_qsshb           TO lv_qsshb_1
                                   CURRENCY is_header-waers.
    TRANSLATE                               lv_qsshb_1 USING ', - '.
    CONDENSE                                lv_qsshb_1 NO-GAPS.
    IF     ( is_wht_1-wt_qsshb           LT 0 ).
      CONCATENATE '-' lv_qsshb_1       INTO lv_qsshb_1.
    ENDIF.
    WRITE    is_wht_1-wt_qbshb           TO lv_qbshb_1
                                   CURRENCY is_header-waers.
    TRANSLATE                               lv_qbshb_1 USING ', - '.
    CONDENSE                                lv_qbshb_1 NO-GAPS.
    IF     ( is_wht_1-wt_qbshb           LT 0 ).
      CONCATENATE '-' lv_qbshb_1       INTO lv_qbshb_1.
    ENDIF.

  ENDIF.

  IF       ( is_wht_2-witht              IS NOT INITIAL ).

    WRITE    is_wht_2-wt_qsshb           TO lv_qsshb_2
                                   CURRENCY is_header-waers.
    TRANSLATE                               lv_qsshb_2 USING ', - '.
    CONDENSE                                lv_qsshb_2 NO-GAPS.
    IF     ( is_wht_2-wt_qsshb           LT 0 ).
      CONCATENATE '-' lv_qsshb_2       INTO lv_qsshb_2.
    ENDIF.
    WRITE    is_wht_2-wt_qbshb           TO lv_qbshb_2
                                   CURRENCY is_header-waers.
    TRANSLATE                               lv_qbshb_2 USING ', - '.
    CONDENSE                                lv_qbshb_2 NO-GAPS.
    IF     ( is_wht_2-wt_qbshb           LT 0 ).
      CONCATENATE '-' lv_qbshb_2       INTO lv_qbshb_2.
    ENDIF.

  ENDIF.

*eject
* Invoice gross amount
  CLEAR                                     lv_wrbtr.
  WRITE    is_vendor-wrbtr               TO lv_wrbtr
                                   CURRENCY is_header-waers.
  TRANSLATE                                 lv_wrbtr USING ', - '.
  CONDENSE                                  lv_wrbtr NO-GAPS.
  IF   ( ( is_vendor-shkzg               EQ 'S' ) OR
         ( is_vendor-wrbtr               EQ  0  )    ).
  ELSE.
    CONCATENATE '-' lv_wrbtr           INTO lv_wrbtr.
  ENDIF.

* Due date
  CLEAR    lv_due_date.

  CALL FUNCTION 'NET_DUE_DATE_GET'
    EXPORTING
      I_ZFBDT = is_vendor-zfbdt
      I_ZBD1T = is_vendor-zbd1t
      I_ZBD2T = is_vendor-zbd2t
      I_ZBD3T = is_vendor-zbd3t
      I_SHKZG = is_vendor-shkzg
      I_REBZG = space
      I_KOART = 'K'
    IMPORTING
      E_FAEDT = lv_due_date.

* Document header text
  CLEAR                                     lv_bktxt.
  MOVE     is_header-bktxt               TO lv_bktxt.

  PERFORM  f_remove_special_chars  CHANGING lv_bktxt.

* Reversed
  CLEAR    lv_reversed.

  IF     ( is_header-xreversal           EQ '1' ).
    MOVE   'Y'                           TO lv_reversed.
  ELSE.
    MOVE   'N'                           TO lv_reversed.
  ENDIF.

* Tax amount
  CLEAR                                     lv_wmwst.
  WRITE    is_vendor-wmwst               TO lv_wmwst
                                   CURRENCY is_header-waers.
  TRANSLATE                                 lv_wmwst USING ', - '.
  CONDENSE                                  lv_wmwst NO-GAPS.
  IF   ( ( is_vendor-shkzg               EQ 'S' ) OR
         ( is_vendor-wmwst               EQ  0  )    ).
  ELSE.
    CONCATENATE '-' lv_wmwst           INTO lv_wmwst.
  ENDIF.

*eject
* Cash discount
  CLEAR                                     lv_wskto.
  WRITE    is_vendor-wskto               TO lv_wskto
                                   CURRENCY is_header-waers.
  TRANSLATE                                 lv_wskto USING ', - '.
  CONDENSE                                  lv_wskto NO-GAPS.
  IF   ( ( is_vendor-shkzg               EQ 'S' ) OR
         ( is_vendor-wskto               EQ  0  )    ).
  ELSE.
    CONCATENATE '-' lv_wskto           INTO lv_wskto.
  ENDIF.

* Discount base
  CLEAR                                     lv_skfbt.
  WRITE    is_vendor-skfbt               TO lv_skfbt
                                   CURRENCY is_header-waers.
  TRANSLATE                                 lv_skfbt USING ', - '.
  CONDENSE                                  lv_skfbt NO-GAPS.
  IF   ( ( is_vendor-shkzg               EQ 'S' ) OR
         ( is_vendor-skfbt               EQ  0  )    ).
  ELSE.
    CONCATENATE '-' lv_skfbt           INTO lv_skfbt.
  ENDIF.

* Payment advice text
  CLEAR    lv_adv_text.

  PERFORM  f_get_text                 USING is_header-ausbk
                                            is_header-belnr
                                            is_header-gjahr
                                            'BELEG'
                                            '0003'
                                   CHANGING lv_adv_text.

  PERFORM  f_remove_special_chars  CHANGING lv_adv_text.

* Vendor item description
  CLEAR                                     lv_sgtxt_v.
  MOVE     is_vendor-sgtxt               TO lv_sgtxt_v.

  PERFORM  f_remove_special_chars  CHANGING lv_sgtxt_v.

*eject
* Move header and vendor data elements
  MOVE     p_erpid                       TO ls_rec-erpid.
  CONCATENATE                               is_header-ausbk '_'
                                            is_header-belnr '_'
                                            is_header-gjahr
                                       INTO ls_rec-inv_id.
  MOVE     ls_rec-inv_id                 TO ls_rec-cc_inv_id.
*  MOVE     is_header-ritm                TO ls_rec-ritm.
  MOVE     is_header-ausbk               TO ls_rec-bukrs.
  MOVE     is_vendor-lifnr               TO ls_rec-lifnr.
  MOVE     is_vendor-umskz               TO ls_rec-umskz.
  MOVE     is_header-blart               TO ls_rec-blart.
  MOVE     is_header-tcode               TO ls_rec-tcode.
  MOVE     is_header-xblnr               TO ls_rec-xblnr.
  PERFORM  f_format_date              USING is_header-bldat
                                   CHANGING ls_rec-bldat.
  PERFORM  f_format_date              USING is_header-budat
                                   CHANGING ls_rec-budat.
  MOVE     is_header-monat               TO ls_rec-monat.
  MOVE     space                         TO ls_rec-pay_date.
  MOVE     space                         TO ls_rec-last_update.
  MOVE     is_header-belnr               TO ls_rec-belnr.
  MOVE     lv_wrbtr                      TO ls_rec-wrbtr.
  MOVE     is_header-waers               TO ls_rec-waers.
  PERFORM  f_format_date              USING lv_due_date
                                   CHANGING ls_rec-due_date.
  PERFORM  f_format_date              USING is_vendor-zfbdt
                                   CHANGING ls_rec-zfbdt.
  MOVE     lv_bktxt                      TO ls_rec-bktxt.
  MOVE     is_header-bstat               TO ls_rec-bstat.
  MOVE     lv_reversed                   TO ls_rec-reversed.
  MOVE     is_header-xmwst               TO ls_rec-xmwst.
  MOVE     lv_wmwst                      TO ls_rec-wmwst.
  MOVE     is_vendor-mwskz               TO ls_rec-mwskz.
  MOVE     is_wht_1-text40               TO ls_rec-wht_name_1.
  MOVE     is_wht_1-witht                TO ls_rec-wht_type_1.
  MOVE     is_wht_1-wt_withcd            TO ls_rec-wht_code_1.
  MOVE     lv_qsshb_1                    TO ls_rec-wht_base_1.
  MOVE     lv_qbshb_1                    TO ls_rec-wht_tax_1.
  MOVE     is_wht_2-text40               TO ls_rec-wht_name_2.
  MOVE     is_wht_2-witht                TO ls_rec-wht_type_2.
  MOVE     is_wht_2-wt_withcd            TO ls_rec-wht_code_2.
  MOVE     lv_qsshb_2                    TO ls_rec-wht_base_2.
  MOVE     lv_qbshb_2                    TO ls_rec-wht_tax_2.
  MOVE     is_vendor-empfb               TO ls_rec-empfb.
  MOVE     is_vendor-bvtyp               TO ls_rec-bvtyp.
  MOVE     is_vendor-hbkid               TO ls_rec-hbkid.
  MOVE     is_vendor-zterm               TO ls_rec-zterm.
  MOVE     lv_wskto                      TO ls_rec-wskto.
  MOVE     lv_skfbt                      TO ls_rec-skfbt.
  MOVE     is_vendor-zlsch               TO ls_rec-zlsch.
  MOVE     is_vendor-uzawe               TO ls_rec-uzawe.
  MOVE     is_vendor-zlspr               TO ls_rec-zlspr.
  MOVE     space                         TO ls_rec-unplan_dc.
  MOVE     lv_adv_text                   TO ls_rec-adv_text.
  MOVE     lv_sgtxt_v                    TO ls_rec-sgtxt_v.

*eject
  IF     ( lv_fl_found IS INITIAL ).

    CLEAR    ls_output.

    DO lv_cn_comp TIMES.

      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_rec> TO <fs_attr>.
      IF     ( sy-index EQ 1 ).
        ls_output = <fs_attr>.
      ELSE.
        CONCATENATE ls_output <fs_attr>
               INTO ls_output SEPARATED BY gc_delim.
      ENDIF.

    ENDDO.

    APPEND   ls_output                   TO lt_output.

  ENDIF.

*eject
  IF       ( lt_output[]                 IS INITIAL ).
    RETURN.
  ENDIF.

  lv_cn_recs_buff = LINES( lt_output ).

  IF       ( rb_pres                     IS NOT INITIAL ).
    APPEND   LINES OF lt_output          TO gt_output.
    ADD      lv_cn_recs_buff             TO gv_cn_recs_total.
    RETURN.
  ENDIF.

* Check if the maximum number of records per file is exceeded
  lv_cn_recs      = lv_cn_recs_buff + gv_cn_recs_file.

  IF       ( lv_cn_recs                  GT gv_cn_recs_max ).

* Close the file
    IF   ( ( p_maxrec                    IS NOT INITIAL ) AND
           ( gv_filename                 IS NOT INITIAL )     ).

      CLOSE    DATASET gv_filename.

      CLEAR    gv_filename.
      CLEAR    gv_cn_recs_file.

    ENDIF.

  ENDIF.

* Open the file
  IF       ( gv_filename                 IS INITIAL ).

    PERFORM  f_open_dataset.

  ENDIF.

  IF       ( gv_filename                 IS NOT INITIAL ).

    LOOP AT       lt_output       ASSIGNING <fs_output>.
      TRANSFER   <fs_output>             TO gv_filename.
    ENDLOOP.

    ADD      lv_cn_recs_buff             TO gv_cn_recs_file.
    ADD      lv_cn_recs_buff             TO gv_cn_recs_total.

  ENDIF.

ENDFORM.                    " f_output_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_date
*&---------------------------------------------------------------------*
*       Format a date for output
*----------------------------------------------------------------------*
FORM f_format_date
  USING    iv_datum                    TYPE datum
  CHANGING cv_date                     TYPE char10.

  DATA:    lv_date                     TYPE char10.

  CLEAR    cv_date.
  CLEAR    lv_date.

  IF     ( iv_datum IS INITIAL ).
    RETURN.
  ENDIF.

  CONCATENATE  iv_datum+0(4)  '-'
               iv_datum+4(2)  '-'
               iv_datum+6(2)           INTO lv_date.

  MOVE         lv_date                   TO cv_date.

ENDFORM.                    " f_format_date
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_text
*&---------------------------------------------------------------------*
*       Get the long text
*----------------------------------------------------------------------*
FORM f_get_text
  USING    iv_bukrs                    TYPE bukrs
           iv_belnr                    TYPE belnr_d
           iv_gjahr                    TYPE gjahr
           iv_object                   TYPE tdobject
           iv_id                       TYPE tdid
  CHANGING cv_text                     TYPE string.

  DATA:    lt_lines                    TYPE STANDARD TABLE OF tline,
           ls_lines                    TYPE tline.

  DATA:    lv_name                     TYPE tdobname,
           lv_text                     TYPE string.

  CLEAR    cv_text.
  CLEAR    lv_text.

  CLEAR                                     lv_name.
  MOVE     iv_bukrs                      TO lv_name+00(04).
  MOVE     iv_belnr                      TO lv_name+04(10).
  MOVE     iv_gjahr                      TO lv_name+14(04).

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT                  = sy-mandt
      ID                      = iv_id
      LANGUAGE                = 'E'
      NAME                    = lv_name
      OBJECT                  = iv_object
    TABLES
      LINES                   = lt_lines
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

*eject
  CLEAR                                     ls_lines.
  LOOP AT  lt_lines                    INTO ls_lines.
    IF   ( sy-tabix                      EQ 1 ).
      MOVE ls_lines-tdline               TO lv_text.
    ELSE.
      CONCATENATE          lv_text          ls_lines-tdline
                                       INTO lv_text
                               SEPARATED BY space.
    ENDIF.
    CLEAR  ls_lines.
  ENDLOOP.

  cv_text = lv_text.

ENDFORM.                    " f_get_text
*eject
*&---------------------------------------------------------------------*
*&      Form  f_remove_special_chars
*&---------------------------------------------------------------------*
*       Remove the special characters from text
*----------------------------------------------------------------------*
FORM f_remove_special_chars
  CHANGING cv_text                     TYPE ANY.

  DATA:    lv_text                     TYPE text1000,
           lv_len                      TYPE syindex,
           lv_ptr                      TYPE syindex,
           lv_char                     TYPE char1.

  CONSTANTS:
           lc_sep1                     TYPE char2 VALUE '|~',
           lc_sep2                     TYPE char2 VALUE '~|',
           lc_sep3                     TYPE char2 VALUE '|-',
           lc_sep4                     TYPE char2 VALUE '-|'.

  IF     ( cv_text IS INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_text.
  MOVE     cv_text                       TO lv_text.

  REPLACE  ALL OCCURRENCES    OF lc_sep1 IN lv_text WITH lc_sep3.
  REPLACE  ALL OCCURRENCES    OF lc_sep2 IN lv_text WITH lc_sep4.

  lv_len = STRLEN( lv_text ).

  DO       lv_len TIMES.
    lv_ptr = sy-index - 1.

    CLEAR                                   lv_char.
    MOVE     lv_text+lv_ptr(1)           TO lv_char.

    IF     ( lv_char                     LT space ).
      MOVE   space                       TO lv_text+lv_ptr(1).
    ENDIF.

  ENDDO.

  CLEAR                                     cv_text.
  MOVE     lv_text                       TO cv_text.

ENDFORM.                    " f_remove_special_chars
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
  CONCATENATE text-c01  text-c02  text-c03  text-c04 "lfa1 - 01
              text-c05  text-c06  text-c07  text-c08
              text-c09  text-c10  text-c11  text-c12
              text-c13  text-c14  text-c15  text-c16
              text-c17  text-c18  text-c19  text-c20
              text-c21  text-c22  text-c23  text-c24
              text-c25  text-c26  text-c27  text-c28
              text-c29  text-c30  text-c31  text-c32
              text-c33  text-c34  text-c35  text-c36
              text-c37  text-c38  text-c39  text-c40
              text-c41  text-c42  text-c43  text-c44
              text-c45  text-c46  text-c47  text-c73
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
