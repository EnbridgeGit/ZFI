*&---------------------------------------------------------------------*
*& Report  ZFAPI112_OPEN_INVOICES
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI112_OPEN_INVOICES                        *
* Author             :   Paul Karunakar                                *
* Date               :   Feb 1, 2018                                   *
* Technical Contact  :   John Hartung                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Extract interface of Open and Parked          *
*                        Invoices to IAP                               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  KPAL        D30K928583  CHG0100815  Initial Development *
*                          D30K928768, D30K928884                      *
*----------------------------------------------------------------------*

*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_file_path
*&---------------------------------------------------------------------*
FORM f_get_file_path
  CHANGING cv_file1                    TYPE any.

  CONSTANTS: lc_path1                  TYPE filepath-pathintern
                                       VALUE 'ZFAPI112_OPEN_INVOICES'.

  CLEAR    cv_file1.

  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = lc_path1
    IMPORTING
      file_name        = cv_file1
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.
  IF       ( sy-subrc NE 0 ).
    MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_get_file_path
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_file_name
*&---------------------------------------------------------------------*
FORM f_get_file_name
  CHANGING cv_file2                    TYPE any.

  CLEAR    cv_file2.

* Set the data file name
  CONCATENATE 'OPEN_INVOICES_' sy-datum '_' sy-uzeit '.CSV'
                                       INTO cv_file2.

ENDFORM.                    " f_get_file_name
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
  IF         ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_get_f4_help_file_path
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_f4_help_file_path1
*&---------------------------------------------------------------------*
FORM f_get_f4_help_file_path1.

  DATA:    lv_repid                    TYPE syrepid,
           lv_dynnr                    TYPE sydynnr,
           lv_file                     TYPE localfile.

  CLEAR                                     lv_repid.
  MOVE     sy-repid                      TO lv_repid.
  CLEAR                                     lv_dynnr.
  MOVE     sy-dynnr                      TO lv_dynnr.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = lv_repid
      dynpro_number = lv_dynnr
    CHANGING
      file_name     = lv_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF         ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE     lv_file TO p_path1.

ENDFORM.                    " f_get_f4_help_file_path1
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_main
*&---------------------------------------------------------------------*
*       Get the data
*----------------------------------------------------------------------*
FORM f_process_main.

  CLEAR    gt_bsik[].
  CLEAR    gt_bkpf[].
  CLEAR    gt_lfa1[].
  CLEAR    gt_vbsegk[].
  CLEAR    gt_vbkpf[].
  CLEAR    gt_vlfa1[].
  CLEAR    gt_data[].
  CLEAR    gt_data_dl[].

  CLEAR    gv_filename.
  CLEAR    gv_rec_count.

* Open the output dataset
  IF     ( p_appl IS NOT INITIAL ).

    PERFORM  f_open_dataset.

  ENDIF.

* Create the header record
  PERFORM  f_create_header.

* Get the open vendor items
  PERFORM  f_get_data_open_items.

* Complete the open vendor items
  PERFORM  f_complete_open_items.

* Get the parked vendor items
  PERFORM  f_get_data_parked_items.

* Complete the parked vendor items
  PERFORM  f_complete_parked_items.

* Close the output dataset
  IF     ( p_appl IS NOT INITIAL ).

    PERFORM  f_close_dataset.

* Download the data
  ELSE.

    PERFORM  f_download_data.

  ENDIF.

ENDFORM.                    " f_process_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_data_open_items
*&---------------------------------------------------------------------*
*       Get the open vendor items
*----------------------------------------------------------------------*
FORM f_get_data_open_items.

  DATA:    lt_bsik                     TYPE gtt_bsik,
           ls_bsik                     TYPE gty_bsik.

  DATA:    lv_fl_delete                TYPE xflag.

  FIELD-SYMBOLS: <fs_bsik>             TYPE gty_bsik.

  CLEAR    lt_bsik[].
  CLEAR    ls_bsik.

  SELECT   bukrs  belnr  gjahr  buzei  lifnr
           umsks  umskz  augdt  augbl  zuonr
           budat  bldat  waers  xblnr  blart
           bschl  shkzg  gsber  dmbtr  wrbtr
           wmwst  sgtxt  zterm  bstat  dmbe2  vertn
    INTO   TABLE lt_bsik
    FROM   bsik
   WHERE   bukrs IN s_bukrs
     AND   lifnr IN s_lifnr
     AND   gjahr IN s_gjahr
     AND   belnr IN s_belnr.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  DELETE   lt_bsik           WHERE umsks IS NOT INITIAL
                                OR umskz IS NOT INITIAL.

  SORT     lt_bsik ASCENDING BY bukrs belnr gjahr.

*eject
  LOOP AT  lt_bsik                ASSIGNING <fs_bsik>.

    AT NEW gjahr.
      CLEAR    ls_bsik.
      CLEAR    lv_fl_delete.
    ENDAT.

    IF           ( ls_bsik-bukrs         IS INITIAL ).
      MOVE         <fs_bsik>             TO ls_bsik.
    ELSEIF       ( <fs_bsik>-lifnr       NE ls_bsik-lifnr ).
      lv_fl_delete = abap_true.
    ELSE.
      IF         ( <fs_bsik>-shkzg       EQ ls_bsik-shkzg ).
        ADD        <fs_bsik>-dmbtr       TO ls_bsik-dmbtr.
        ADD        <fs_bsik>-wrbtr       TO ls_bsik-wrbtr.
        ADD        <fs_bsik>-wmwst       TO ls_bsik-wmwst.
        ADD        <fs_bsik>-dmbe2       TO ls_bsik-dmbe2.
      ELSE.
        SUBTRACT   <fs_bsik>-dmbtr     FROM ls_bsik-dmbtr.
        SUBTRACT   <fs_bsik>-wrbtr     FROM ls_bsik-wrbtr.
        SUBTRACT   <fs_bsik>-wmwst     FROM ls_bsik-wmwst.
        SUBTRACT   <fs_bsik>-dmbe2     FROM ls_bsik-dmbe2.
        IF       ( ls_bsik-dmbtr         LT 0 ).
          MULTIPLY ls_bsik-dmbtr         BY -1.
          MULTIPLY ls_bsik-wrbtr         BY -1.
          MULTIPLY ls_bsik-wmwst         BY -1.
          MULTIPLY ls_bsik-dmbe2         BY -1.
          IF     ( ls_bsik-shkzg         EQ 'S' ).
            CLEAR                           ls_bsik-shkzg.
            MOVE   'H'                   TO ls_bsik-shkzg.
          ELSEIF ( ls_bsik-shkzg         EQ 'H' ).
            CLEAR                           ls_bsik-shkzg.
            MOVE   'S'                   TO ls_bsik-shkzg.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    AT END OF gjahr.
      IF       ( lv_fl_delete            IS INITIAL ).
        APPEND   ls_bsik                 TO gt_bsik.
      ENDIF.
    ENDAT.

  ENDLOOP.

*eject
  IF     ( gt_bsik[] IS NOT INITIAL ).

    SORT     gt_bsik ASCENDING BY bukrs belnr gjahr.

    SELECT   lifnr  name1
      INTO   TABLE gt_lfa1
      FROM   lfa1 FOR ALL ENTRIES IN gt_bsik
     WHERE   lifnr = gt_bsik-lifnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_lfa1 ASCENDING BY lifnr.
    ELSE.
      CLEAR  gt_lfa1[].
    ENDIF.

    SELECT   bukrs  belnr  gjahr  budat  usnam
             tcode  waers  kursf  bstat  awtyp
             hwaer  hwae2
      INTO   TABLE gt_bkpf
      FROM   bkpf FOR ALL ENTRIES IN gt_bsik
     WHERE   bukrs = gt_bsik-bukrs
       AND   belnr = gt_bsik-belnr
       AND   gjahr = gt_bsik-gjahr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_bkpf ASCENDING BY bukrs belnr gjahr.
    ELSE.
      CLEAR  gt_bkpf[].
    ENDIF.

  ENDIF.

ENDFORM.                    " f_get_data_open_items
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_data_parked_items
*&---------------------------------------------------------------------*
*       Get the parked vendor items
*----------------------------------------------------------------------*
FORM f_get_data_parked_items.

  DATA:    lt_vbsegk                   TYPE gtt_vbsegk,
           ls_vbsegk                   TYPE gty_vbsegk.

  DATA:    lv_fl_delete                TYPE xflag.

  FIELD-SYMBOLS: <fs_vbsegk>           TYPE gty_vbsegk.

  CLEAR    lt_vbsegk[].
  CLEAR    ls_vbsegk.

  SELECT   ausbk  belnr  gjahr  bzkey  bukrs
           bschl  umskz  umsks  shkzg  gsber
           dmbtr  dmbe2  wrbtr  wmwst  sgtxt
           lifnr  zterm  vertn
    INTO   TABLE lt_vbsegk
    FROM   vbsegk
   WHERE   belnr IN s_belnr
     AND   gjahr IN s_gjahr
     AND   bukrs IN s_bukrs
     AND   lifnr IN s_lifnr.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  DELETE   lt_vbsegk         WHERE umsks IS NOT INITIAL
                                OR umskz IS NOT INITIAL.

  SORT     lt_vbsegk ASCENDING BY ausbk belnr gjahr bzkey bukrs.

*eject
  LOOP AT  lt_vbsegk              ASSIGNING <fs_vbsegk>.

    AT NEW gjahr.
      CLEAR    ls_vbsegk.
      CLEAR    lv_fl_delete.
    ENDAT.

    IF           ( ls_vbsegk-bukrs       IS INITIAL ).
      MOVE         <fs_vbsegk>           TO ls_vbsegk.
    ELSEIF       ( <fs_vbsegk>-lifnr     NE ls_vbsegk-lifnr ).
      lv_fl_delete = abap_true.
    ELSE.
      IF         ( <fs_vbsegk>-shkzg     EQ ls_vbsegk-shkzg ).
        ADD        <fs_vbsegk>-dmbtr     TO ls_vbsegk-dmbtr.
        ADD        <fs_vbsegk>-dmbe2     TO ls_vbsegk-dmbe2.
        ADD        <fs_vbsegk>-wrbtr     TO ls_vbsegk-wrbtr.
        ADD        <fs_vbsegk>-wmwst     TO ls_vbsegk-wmwst.
      ELSE.
        SUBTRACT   <fs_vbsegk>-dmbtr   FROM ls_vbsegk-dmbtr.
        SUBTRACT   <fs_vbsegk>-dmbe2   FROM ls_vbsegk-dmbe2.
        SUBTRACT   <fs_vbsegk>-wrbtr   FROM ls_vbsegk-wrbtr.
        SUBTRACT   <fs_vbsegk>-wmwst   FROM ls_vbsegk-wmwst.
        IF       ( ls_vbsegk-dmbtr       LT 0 ).
          MULTIPLY ls_vbsegk-dmbtr       BY -1.
          MULTIPLY ls_vbsegk-dmbe2       BY -1.
          MULTIPLY ls_vbsegk-wrbtr       BY -1.
          MULTIPLY ls_vbsegk-wmwst       BY -1.
          IF     ( ls_vbsegk-shkzg       EQ 'S' ).
            CLEAR                           ls_vbsegk-shkzg.
            MOVE   'H'                   TO ls_vbsegk-shkzg.
          ELSEIF ( ls_vbsegk-shkzg       EQ 'H' ).
            CLEAR                           ls_vbsegk-shkzg.
            MOVE   'S'                   TO ls_vbsegk-shkzg.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    AT END OF gjahr.
      IF       ( lv_fl_delete            IS INITIAL ).
        APPEND   ls_vbsegk               TO gt_vbsegk.
      ENDIF.
    ENDAT.

  ENDLOOP.

*eject
  IF     ( gt_vbsegk[] IS NOT INITIAL ).

    SORT     gt_vbsegk ASCENDING BY ausbk belnr gjahr bzkey bukrs.

    SELECT   lifnr  name1
      INTO   TABLE gt_vlfa1
      FROM   lfa1 FOR ALL ENTRIES IN gt_vbsegk
     WHERE   lifnr = gt_vbsegk-lifnr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_vlfa1 ASCENDING BY lifnr.
    ELSE.
      CLEAR  gt_vlfa1[].
    ENDIF.

    SELECT   ausbk  bukrs  belnr  gjahr  bstat
             blart  bldat  budat  usnam  tcode
             xblnr  waers  hwaer  hwae2  kursf  awtyp
      INTO   TABLE gt_vbkpf
      FROM   vbkpf FOR ALL ENTRIES IN gt_vbsegk
     WHERE   ausbk = gt_vbsegk-ausbk
       AND   bukrs = gt_vbsegk-bukrs
       AND   belnr = gt_vbsegk-belnr
       AND   gjahr = gt_vbsegk-gjahr.
    IF     ( sy-subrc EQ 0 ).
      SORT   gt_vbkpf ASCENDING BY ausbk bukrs belnr gjahr.
    ELSE.
      CLEAR  gt_vbkpf[].
    ENDIF.

  ENDIF.

ENDFORM.                    " f_get_data_parked_items
*eject
*&---------------------------------------------------------------------*
*&      Form  f_complete_open_items
*&---------------------------------------------------------------------*
*       Complete the open vendor items
*----------------------------------------------------------------------*
FORM f_complete_open_items.

  DATA:    lv_char16                   TYPE char16,
           lv_char12                   TYPE char12,
           lv_text                     TYPE text200,
           lv_kursf                    TYPE p LENGTH 9 DECIMALS 5.

  DATA:    lv_string                   TYPE string,
           lv_dtype                    TYPE c,
           lv_cnt_flds                 TYPE i,
           lv_lines                    TYPE i.

  CONSTANTS: lc_sep                    TYPE char2 VALUE '|~'.

  FIELD-SYMBOLS: <fs_attr>             TYPE any.

  IF     ( gt_bsik[] IS INITIAL ).
    RETURN.
  ENDIF.

  DESCRIBE FIELD gs_data TYPE lv_dtype COMPONENTS lv_cnt_flds.

*eject
* Open items
  CLEAR                                     gs_bsik.
  LOOP AT  gt_bsik                     INTO gs_bsik.

    CLEAR    gs_bkpf.
    CLEAR    gs_lfa1.
    CLEAR    gs_data.
    CLEAR    gv_ulid.
    CLEAR    gv_vulid.

    CLEAR                                   gs_bkpf.
    READ     TABLE gt_bkpf             INTO gs_bkpf
                                   WITH KEY bukrs = gs_bsik-bukrs
                                            belnr = gs_bsik-belnr
                                            gjahr = gs_bsik-gjahr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_bkpf.
    ENDIF.

    IF     ( gs_bkpf-bstat IN s_bstat ).
    ELSE.
      CLEAR  gs_bsik.
      CONTINUE.
    ENDIF.

    IF     ( gs_bsik-xblnr IS INITIAL ).
      CLEAR  gs_bsik.
      CONTINUE.
    ENDIF.

    CLEAR                                   gs_lfa1.
    READ     TABLE gt_lfa1             INTO gs_lfa1
                                   WITH KEY lifnr = gs_bsik-lifnr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_lfa1.
    ENDIF.

    CONCATENATE gs_bsik-belnr gs_bsik-bukrs gs_bsik-gjahr
                                       INTO gv_ulid
                               SEPARATED BY '_'.

    MOVE     gv_ulid                     TO gs_data-ap_ulid.
    MOVE     gv_ulid                     TO gs_data-inv_ulid.
    MOVE     gs_bsik-xblnr               TO gs_data-inv_num.

*   WRITE    gs_bsik-bldat               TO gs_data-inv_dat MM/DD/YYYY.
    PERFORM  f_format_date            USING gs_bsik-bldat
                                   CHANGING gs_data-inv_dat.

*eject
    CLEAR                                   lv_char16.
    WRITE    gs_bsik-wrbtr               TO lv_char16
                                   CURRENCY gs_bsik-waers.
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( gs_bsik-shkzg               EQ 'S' ) OR
           ( gs_bsik-wrbtr               EQ  0  )    ).
      MOVE   lv_char16                   TO gs_data-inv_total.
      MOVE   lv_char16                   TO gs_data-inv_amt.
    ELSE.
      CONCATENATE '-' lv_char16        INTO gs_data-inv_total.
      CONCATENATE '-' lv_char16        INTO gs_data-inv_amt.
    ENDIF.

    MOVE     gs_bkpf-tcode               TO gs_data-inv_src.
    MOVE     gs_bsik-blart               TO gs_data-inv_type.
    MOVE     gs_bsik-belnr               TO gs_data-vchr_num.

    CONCATENATE gs_bsik-lifnr gs_bsik-bukrs
                                       INTO gv_vulid
                               SEPARATED BY '_'.

    MOVE     gv_vulid                    TO gs_data-ven_ulid.
    MOVE     gs_bsik-lifnr               TO gs_data-ven_num.

*eject
* Exchange Rate Trx To Rpt
    IF     ( gs_bsik-waers EQ gc_cad ).
      lv_kursf = 1.
    ELSEIF ( gs_bkpf-hwaer EQ gc_cad ).
      IF   ( gs_bsik-dmbtr EQ 0      ) OR
           ( gs_bsik-wrbtr EQ 0      ).
        lv_kursf = 0.
      ELSE.
        lv_kursf = gs_bsik-dmbtr / gs_bsik-wrbtr.
      ENDIF.
    ELSEIF ( gs_bkpf-hwae2 EQ gc_cad ).
      IF   ( gs_bsik-dmbe2 EQ 0      ) OR
           ( gs_bsik-wrbtr EQ 0      ).
        lv_kursf = 0.
      ELSE.
        lv_kursf = gs_bsik-dmbe2 / gs_bsik-wrbtr.
      ENDIF.
    ELSE.
      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
          client           = sy-mandt
          date             = gs_bsik-budat
          foreign_amount   = gs_bsik-wrbtr
          foreign_currency = gs_bsik-waers
          local_currency   = gc_cad
          type_of_rate     = p_kurst
          read_tcurr       = gc_x
        IMPORTING
          exchange_rate    = lv_kursf
        EXCEPTIONS
          no_rate_found    = 1
          overflow         = 2
          no_factors_found = 3
          no_spread_found  = 4
          derived_2_times  = 5
          OTHERS           = 6.
      IF ( sy-subrc NE 0 ).
        lv_kursf = 0.
        MESSAGE  ID sy-msgid             TYPE sy-msgty
             NUMBER sy-msgno             INTO lv_text
               WITH sy-msgv1                  sy-msgv2
                    sy-msgv3                  sy-msgv4.
        WRITE: / lv_text.
      ENDIF.
    ENDIF.

    CLEAR                                   lv_char12.
    WRITE    lv_kursf                    TO lv_char12
                                   DECIMALS 5.
    CONDENSE                                lv_char12 NO-GAPS.

    MOVE     lv_char12                   TO gs_data-xrt_tc2rc.
    MOVE     gs_bsik-waers               TO gs_data-trans_curr.
    MOVE     gs_bsik-bukrs               TO gs_data-comp_code.
    MOVE     gs_bsik-zterm               TO gs_data-paym_terms.

*eject
* PO Indicator
    IF ( gs_bkpf-awtyp EQ 'RMRP' ).
      gs_data-po_ind    = 'YES'.
    ELSE.
      gs_data-po_ind    = 'NO'.
    ENDIF.

*   WRITE    gs_bsik-budat          TO gs_data-inv_rcvd  MM/DD/YYYY.
    PERFORM  f_format_date       USING gs_bsik-budat
                              CHANGING gs_data-inv_rcvd.
*   WRITE    gs_bsik-budat          TO gs_data-inv_entry MM/DD/YYYY.
    PERFORM  f_format_date       USING gs_bsik-budat
                              CHANGING gs_data-inv_entry.

    CLEAR                                   lv_char16.
    WRITE    gs_bsik-wrbtr               TO lv_char16
                                   CURRENCY gs_bsik-waers.
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( gs_bsik-shkzg               EQ 'S' ) OR
           ( gs_bsik-wrbtr               EQ  0  )    ).
      MOVE   lv_char16                   TO gs_data-paym_total.
    ELSE.
      CONCATENATE '-' lv_char16        INTO gs_data-paym_total.
    ENDIF.

    MOVE     '0'                         TO gs_data-paym_date.
    MOVE     gs_lfa1-name1               TO gs_data-ven_name.
    MOVE     gs_bsik-gsber               TO gs_data-bus_unit.
    MOVE     p_erpid                     TO gs_data-erp_id.
    MOVE     gs_bkpf-usnam               TO gs_data-entrd_by.
    MOVE     gs_bsik-sgtxt               TO gs_data-comment.

    CLEAR                                   lv_char16.
    WRITE    gs_bsik-wmwst               TO lv_char16
                                   CURRENCY gs_bsik-waers.
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( gs_bsik-shkzg               EQ 'S' ) OR
           ( gs_bsik-wmwst               EQ  0  )    ).
      MOVE   lv_char16                   TO gs_data-tax_amt.
    ELSE.
      CONCATENATE '-' lv_char16        INTO gs_data-tax_amt.
    ENDIF.

*eject
* Exchange Rate Trx To Acctg
    IF     ( gs_bkpf-waers EQ gs_bkpf-hwaer ).
      lv_kursf = 1.
    ELSE.
      IF   ( gs_bkpf-kursf GE 0 ).
        lv_kursf = gs_bkpf-kursf.
      ELSE.
        lv_kursf = 1 / ( gs_bkpf-kursf * -1 ).
      ENDIF.
    ENDIF.

    CLEAR                                   lv_char12.
    WRITE    lv_kursf                    TO lv_char12
                                   DECIMALS 5.
    CONDENSE                                lv_char12 NO-GAPS.

    MOVE     lv_char12                   TO gs_data-xrt_tc2ac.
    MOVE     gs_bkpf-hwaer               TO gs_data-acctg_curr.
    MOVE     ' '                         TO gs_data-urn.
    MOVE     ' '                         TO gs_data-link_url1.
    MOVE     ' '                         TO gs_data-link_typ1.
    MOVE     gs_bsik-gjahr               TO gs_data-fisc_year.
    MOVE     gs_bsik-bschl               TO gs_data-pstg_key.
    MOVE     gs_bsik-bstat               TO gs_data-paym_stat.
    MOVE     gs_bsik-vertn               TO gs_data-contract.

    CLEAR    lv_string.

    DO lv_cnt_flds TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE gs_data TO <fs_attr>.
      IF       ( sy-index EQ 1 ).
        MOVE     <fs_attr>                TO lv_string.
      ELSE.
        CONCATENATE lv_string <fs_attr> INTO lv_string
                                SEPARATED BY lc_sep.
      ENDIF.
    ENDDO.

    IF     ( p_appl IS NOT INITIAL ).

      TRANSFER lv_string                 TO gv_filename.
      ADD      1                         TO gv_rec_count.

    ELSE.

      APPEND   lv_string                 TO gt_data_dl.

    ENDIF.

    CLEAR    gs_bsik.
  ENDLOOP.

ENDFORM.                    " f_complete_open_items
*eject
*&---------------------------------------------------------------------*
*&      Form  f_complete_parked_items
*&---------------------------------------------------------------------*
*       Complete the parked vendor items
*----------------------------------------------------------------------*
FORM f_complete_parked_items.

  DATA:    lv_char16                   TYPE char16,
           lv_char12                   TYPE char12,
           lv_text                     TYPE text200,
           lv_kursf                    TYPE p LENGTH 9 DECIMALS 5.

  DATA:    lv_string                   TYPE string,
           lv_dtype                    TYPE c,
           lv_cnt_flds                 TYPE i,
           lv_lines                    TYPE i.

  CONSTANTS: lc_sep                    TYPE char2 VALUE '|~'.

  FIELD-SYMBOLS: <fs_attr>             TYPE any.

  IF     ( gt_vbsegk[] IS INITIAL ).
    RETURN.
  ENDIF.

  DESCRIBE FIELD gs_data TYPE lv_dtype COMPONENTS lv_cnt_flds.

*eject
* Parked items
  CLEAR                                     gs_vbsegk.
  LOOP AT  gt_vbsegk                   INTO gs_vbsegk.

    CLEAR    gs_vbkpf.
    CLEAR    gs_vlfa1.
    CLEAR    gs_data.
    CLEAR    gv_ulid.
    CLEAR    gv_vulid.

    CLEAR                                   gs_vbkpf.
    READ     TABLE gt_vbkpf            INTO gs_vbkpf
                                   WITH KEY ausbk = gs_vbsegk-ausbk
                                            bukrs = gs_vbsegk-bukrs
                                            belnr = gs_vbsegk-belnr
                                            gjahr = gs_vbsegk-gjahr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_vbkpf.
    ENDIF.

    IF     ( gs_vbkpf-bstat IN s_bstat ).
    ELSE.
      CLEAR  gs_vbsegk.
      CONTINUE.
    ENDIF.

    IF     ( gs_vbkpf-xblnr IS INITIAL ).
      CLEAR  gs_vbsegk.
      CONTINUE.
    ENDIF.

    CLEAR                                   gs_vlfa1.
    READ     TABLE gt_vlfa1            INTO gs_vlfa1
                                   WITH KEY lifnr = gs_vbsegk-lifnr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_vlfa1.
    ENDIF.

    CONCATENATE gs_vbsegk-belnr gs_vbsegk-ausbk gs_vbsegk-gjahr
                                       INTO gv_ulid
                               SEPARATED BY '_'.

    MOVE     gv_ulid                     TO gs_data-ap_ulid.
    MOVE     gv_ulid                     TO gs_data-inv_ulid.
    MOVE     gs_vbkpf-xblnr              TO gs_data-inv_num.

*   WRITE    gs_vbkpf-bldat              TO gs_data-inv_dat MM/DD/YYYY.
    PERFORM  f_format_date            USING gs_vbkpf-bldat
                                   CHANGING gs_data-inv_dat.

*eject
    CLEAR                                   lv_char16.
    WRITE    gs_vbsegk-wrbtr             TO lv_char16
                                   CURRENCY gs_vbkpf-waers.
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( gs_vbsegk-shkzg             EQ 'S' ) OR
           ( gs_vbsegk-wrbtr             EQ  0  )    ).
      MOVE   lv_char16                   TO gs_data-inv_total.
      MOVE   lv_char16                   TO gs_data-inv_amt.
    ELSE.
      CONCATENATE '-' lv_char16        INTO gs_data-inv_total.
      CONCATENATE '-' lv_char16        INTO gs_data-inv_amt.
    ENDIF.

    MOVE     gs_vbkpf-tcode              TO gs_data-inv_src.
    MOVE     gs_vbkpf-blart              TO gs_data-inv_type.
    MOVE     gs_vbsegk-belnr             TO gs_data-vchr_num.

    CONCATENATE gs_vbsegk-lifnr gs_vbsegk-bukrs
                                       INTO gv_vulid
                               SEPARATED BY '_'.

    MOVE     gv_vulid                    TO gs_data-ven_ulid.
    MOVE     gs_vbsegk-lifnr             TO gs_data-ven_num.

*eject
* ExchangeRateTrxToRpt
    IF     ( gs_vbkpf-waers  EQ gc_cad ).
      lv_kursf = 1.
    ELSEIF ( gs_vbkpf-hwaer  EQ gc_cad ).
      IF   ( gs_vbsegk-dmbtr EQ 0      ) OR
           ( gs_vbsegk-wrbtr EQ 0      ).
        lv_kursf = 0.
      ELSE.
        lv_kursf = gs_vbsegk-dmbtr / gs_vbsegk-wrbtr.
      ENDIF.
    ELSEIF ( gs_vbkpf-hwae2  EQ gc_cad ).
      IF   ( gs_vbsegk-dmbe2 EQ 0      ) OR
           ( gs_vbsegk-wrbtr EQ 0      ).
        lv_kursf = 0.
      ELSE.
        lv_kursf = gs_vbsegk-dmbe2 / gs_vbsegk-wrbtr.
      ENDIF.
    ELSE.
      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
          client           = sy-mandt
          date             = gs_vbkpf-budat
          foreign_amount   = gs_vbsegk-wrbtr
          foreign_currency = gs_vbkpf-waers
          local_currency   = gc_cad
          type_of_rate     = p_kurst
          read_tcurr       = gc_x
        IMPORTING
          exchange_rate    = lv_kursf
        EXCEPTIONS
          no_rate_found    = 1
          overflow         = 2
          no_factors_found = 3
          no_spread_found  = 4
          derived_2_times  = 5
          OTHERS           = 6.
      IF ( sy-subrc NE 0 ).
        lv_kursf = 0.
        MESSAGE  ID sy-msgid             TYPE sy-msgty
             NUMBER sy-msgno             INTO lv_text
               WITH sy-msgv1                  sy-msgv2
                    sy-msgv3                  sy-msgv4.
        WRITE: / lv_text.
      ENDIF.
    ENDIF.

    CLEAR                                   lv_char12.
    WRITE    lv_kursf                    TO lv_char12
                                   DECIMALS 5.
    CONDENSE                                lv_char12 NO-GAPS.

    MOVE     lv_char12                   TO gs_data-xrt_tc2rc.
    MOVE     gs_vbkpf-waers              TO gs_data-trans_curr.
    MOVE     gs_vbsegk-ausbk             TO gs_data-comp_code.
    MOVE     gs_vbsegk-zterm             TO gs_data-paym_terms.

*eject
* PO Indicator
    IF ( gs_vbkpf-awtyp EQ 'RMRP' ).
      gs_data-po_ind     = 'YES'.
    ELSE.
      gs_data-po_ind     = 'NO'.
    ENDIF.

*   WRITE    gs_vbkpf-budat         TO gs_data-inv_rcvd  MM/DD/YYYY.
    PERFORM  f_format_date       USING gs_vbkpf-budat
                              CHANGING gs_data-inv_rcvd.
*   WRITE    gs_vbkpf-budat         TO gs_data-inv_entry MM/DD/YYYY.
    PERFORM  f_format_date       USING gs_vbkpf-budat
                              CHANGING gs_data-inv_entry.

    CLEAR                                   lv_char16.
    WRITE    gs_vbsegk-wrbtr             TO lv_char16
                                   CURRENCY gs_vbkpf-waers.
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( gs_vbsegk-shkzg             EQ 'S' ) OR
           ( gs_vbsegk-wrbtr             EQ  0  )    ).
      MOVE   lv_char16                   TO gs_data-paym_total.
    ELSE.
      CONCATENATE '-' lv_char16        INTO gs_data-paym_total.
    ENDIF.

    MOVE     '0'                         TO gs_data-paym_date.
    MOVE     gs_vlfa1-name1              TO gs_data-ven_name.
    MOVE     gs_vbsegk-gsber             TO gs_data-bus_unit.
    MOVE     p_erpid                     TO gs_data-erp_id.
    MOVE     gs_vbkpf-usnam              TO gs_data-entrd_by.
    MOVE     gs_vbsegk-sgtxt             TO gs_data-comment.

    CLEAR                                   lv_char16.
    WRITE    gs_vbsegk-wmwst             TO lv_char16
                                   CURRENCY gs_vbkpf-waers.
    TRANSLATE                               lv_char16 USING ', - '.
    CONDENSE                                lv_char16 NO-GAPS.
    IF   ( ( gs_vbsegk-shkzg             EQ 'S' ) OR
           ( gs_vbsegk-wmwst             EQ  0  )    ).
      MOVE   lv_char16                   TO gs_data-tax_amt.
    ELSE.
      CONCATENATE '-' lv_char16        INTO gs_data-tax_amt.
    ENDIF.

*eject
* Exchange Rate Trx To Acctg
    IF     ( gs_vbkpf-waers EQ gs_vbkpf-hwaer ).
      lv_kursf = 1.
    ELSE.
      IF   ( gs_vbkpf-kursf GE 0 ).
        lv_kursf = gs_vbkpf-kursf.
      ELSE.
        lv_kursf = 1 / ( gs_vbkpf-kursf * -1 ).
      ENDIF.
    ENDIF.

    CLEAR                                   lv_char12.
    WRITE    lv_kursf                    TO lv_char12
                                   DECIMALS 5.
    CONDENSE                                lv_char12 NO-GAPS.

    MOVE     lv_char12                   TO gs_data-xrt_tc2ac.
    MOVE     gs_vbkpf-hwaer              TO gs_data-acctg_curr.
    MOVE     ' '                         TO gs_data-urn.
    MOVE     ' '                         TO gs_data-link_url1.
    MOVE     ' '                         TO gs_data-link_typ1.
    MOVE     gs_vbsegk-gjahr             TO gs_data-fisc_year.
    MOVE     gs_vbsegk-bschl             TO gs_data-pstg_key.
    MOVE     gs_vbkpf-bstat              TO gs_data-paym_stat.
    MOVE     gs_vbsegk-vertn             TO gs_data-contract.

    CLEAR    lv_string.

    DO lv_cnt_flds TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE gs_data TO <fs_attr>.
      IF       ( sy-index EQ 1 ).
        MOVE     <fs_attr>                TO lv_string.
      ELSE.
        CONCATENATE lv_string <fs_attr> INTO lv_string
                                SEPARATED BY lc_sep.
      ENDIF.
    ENDDO.

    IF     ( p_appl IS NOT INITIAL ).

      TRANSFER lv_string                 TO gv_filename.
      ADD      1                         TO gv_rec_count.

    ELSE.

      APPEND   lv_string                 TO gt_data_dl.

    ENDIF.

    CLEAR    gs_bsik.
  ENDLOOP.

ENDFORM.                    " f_complete_parked_items
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_dataset
*&---------------------------------------------------------------------*
*       Open the dataset
*----------------------------------------------------------------------*
FORM f_open_dataset.

  DATA:    lv_msg_text                 TYPE text80.

  IF     ( p_appl IS INITIAL ).
    RETURN.
  ENDIF.

  CONCATENATE   p_path2 p_erpid '_' p_file2 INTO gv_filename.

  OPEN     DATASET gv_filename FOR OUTPUT IN TEXT MODE
                               ENCODING DEFAULT
                               MESSAGE lv_msg_text.
  IF     ( sy-subrc NE 0 ).
    WRITE: text-014, lv_msg_text, gv_filename.
    EXIT.
  ENDIF.

ENDFORM.                    " f_open_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_close_dataset
*&---------------------------------------------------------------------*
*       Close the dataset
*----------------------------------------------------------------------*
FORM f_close_dataset.

  DATA:    lv_records                  TYPE string.

  IF     ( p_appl IS INITIAL ).
    RETURN.
  ENDIF.

  CLOSE    DATASET gv_filename.

* Output the total number of records extracted
  lv_records = gv_rec_count.
  WRITE: / text-009, lv_records.

ENDFORM.                    " f_close_dataset
*eject
*&---------------------------------------------------------------------*
*&      Form  f_create_header
*&---------------------------------------------------------------------*
*       Create the header record in the output dataset
*----------------------------------------------------------------------*
FORM f_create_header.

  DATA:    lv_string                   TYPE string.

  CONSTANTS: lc_sep                    TYPE char2 VALUE '|~'.

  CLEAR    lv_string.

  CONCATENATE text-c01 text-c02 text-c03 text-c04 text-c05
              text-c06 text-c07 text-c08 text-c09 text-c10
              text-c11 text-c12 text-c13 text-c14 text-c15
              text-c16 text-c17 text-c18 text-c19 text-c20
              text-c21 text-c22 text-c23 text-c24 text-c25
              text-c26 text-c27 text-c28 text-c29 text-c30
              text-c31 text-c32 text-c33 text-c34 text-c35
                                       INTO lv_string
                               SEPARATED BY lc_sep.

  IF     ( p_appl IS NOT INITIAL ).

    TRANSFER lv_string                   TO gv_filename.
    ADD      1                           TO gv_rec_count.

  ELSE.

    APPEND   lv_string                   TO gt_data_dl.

  ENDIF.

ENDFORM.                    " f_create_header
*eject
*&---------------------------------------------------------------------*
*&      Form  f_download_data
*&---------------------------------------------------------------------*
*       Download the data
*----------------------------------------------------------------------*
FORM f_download_data.

  DATA:    lv_filename                 TYPE string,
           lv_records                  TYPE string.

  CLEAR                                     lv_filename.
  MOVE     p_path1                       TO lv_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_filename
    TABLES
      data_tab                = gt_data_dl
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
      OTHERS                  = 22.
  IF     ( sy-subrc NE 0 ).

    MESSAGE  ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

* Output the total number of records extracted
  CLEAR                                     gv_rec_count.
  DESCRIBE TABLE gt_data_dl           LINES gv_rec_count.

  lv_records = gv_rec_count.
  WRITE: / text-009, lv_records.

ENDFORM.                    " f_download_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_format_date
*&---------------------------------------------------------------------*
*       Format the date "MM/DD/YYYY"
*----------------------------------------------------------------------*
FORM f_format_date
  USING    iv_datum                    TYPE sydatum
  CHANGING cv_date                     TYPE char10.

  CLEAR    cv_date.

  IF     ( iv_datum IS INITIAL ).
    RETURN.
  ENDIF.

  CONCATENATE iv_datum+4(2) '/'
              iv_datum+6(2) '/'
              iv_datum+0(4)     INTO cv_date.

ENDFORM.                    " f_format_date
