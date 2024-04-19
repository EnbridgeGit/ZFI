*&---------------------------------------------------------------------*
*&  Include           ZFAPI111_VENDOR_EXTRACT_F01
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFAPI111_VENDOR_EXTRACT                       *
* Program Include    :   ZFAPI111_VENDOR_EXTRACT_F01                   *
* Author             :                                                 *
* Date               :   Feb 01, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   ATCAT Vendor Master Interface                 *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  CPALYAM     D30K928649  CHG0100816-Initial development  *
*                          D30K928653, D30K928683, D30K928691          *
*                          D30K928693, D30K928882, D30K928892          *
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
        p_file2 = gv_pathsplit.
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
*&      Form  f_process_main
*&---------------------------------------------------------------------*
*       Main process
*----------------------------------------------------------------------*
FORM f_process_main.

  DATA:    lt_chngs_lfa1          TYPE gtt_chngs,
           ls_chngs_lfa1          TYPE gty_chngs,
           lt_chngs_lfb1          TYPE gtt_chngs,
           ls_chngs_lfb1          TYPE gty_chngs,
           lt_lfa1_key            TYPE gtt_lfa1_key,
           lt_lfa1_key_p          TYPE gtt_lfa1_key,
           ls_lfa1_key            TYPE gty_lfa1_key,
           lt_lfb1_key            TYPE gtt_lfb1_key,
           lt_lfb1_key_p          TYPE gtt_lfb1_key,
           ls_lfb1_key            TYPE gty_lfb1_key.

  DATA:    lv_rowcnt              TYPE syindex.

  CLEAR    gt_update_last[].

  CLEAR    gv_append.
  CLEAR    gv_rec_count.

  CLEAR    lt_chngs_lfa1[].
  CLEAR    lt_chngs_lfb1[].
  CLEAR    lt_lfa1_key[].
  CLEAR    lt_lfb1_key[].

*eject
  IF     ( p_full IS NOT INITIAL ).

    SELECT   lifnr
      INTO   TABLE lt_lfa1_key
      FROM   lfa1
     WHERE   lifnr IN s_lifnr
       AND   ktokk IN s_ktokk
       AND   erdat IN s_erdat.
    IF     ( sy-subrc EQ 0 ).
      SORT   lt_lfa1_key ASCENDING BY lifnr.
    ELSE.
      CLEAR  lt_lfa1_key[].
    ENDIF.

  ELSE.

* Select the change documents
    PERFORM  f_select_changes        TABLES lt_chngs_lfa1
                                            lt_chngs_lfb1.

    DELETE     lt_chngs_lfa1          WHERE lifnr NOT IN s_lifnr.

    IF       ( lt_chngs_lfa1[]           IS NOT INITIAL ).

      SORT     lt_chngs_lfa1   ASCENDING BY lifnr.
      DELETE   ADJACENT DUPLICATES     FROM lt_chngs_lfa1
                                  COMPARING lifnr.

      SELECT   lifnr
        INTO   TABLE lt_lfa1_key
        FROM   lfa1 FOR ALL ENTRIES IN lt_chngs_lfa1
       WHERE   lifnr  = lt_chngs_lfa1-lifnr
         AND   ktokk IN s_ktokk
         AND   erdat IN s_erdat.
      IF     ( sy-subrc EQ 0 ).
        SORT   lt_lfa1_key ASCENDING BY lifnr.
      ELSE.
        CLEAR  lt_lfa1_key[].
      ENDIF.

    ENDIF.

  ENDIF.

*eject
  IF     ( lt_lfa1_key[]                 IS NOT INITIAL ).

    IF     ( p_full                      IS NOT INITIAL ).

      SELECT   lifnr  bukrs
        INTO   TABLE lt_lfb1_key
        FROM   lfb1 FOR ALL ENTRIES IN lt_lfa1_key
       WHERE   lifnr  = lt_lfa1_key-lifnr
         AND   bukrs IN s_bukrs.
      IF     ( sy-subrc EQ 0 ).
        SORT   lt_lfb1_key ASCENDING BY lifnr bukrs.
      ELSE.
        CLEAR  lt_lfb1_key[].
      ENDIF.

    ELSE.

      CLEAR    lt_chngs_lfa1[].

      lt_chngs_lfa1[] = lt_chngs_lfb1[].

      DELETE   lt_chngs_lfa1 WHERE bukrs IS NOT INITIAL
                                OR lifnr NOT IN s_lifnr.

      IF       ( lt_chngs_lfa1[]         IS NOT INITIAL ).

        SORT     lt_chngs_lfa1 ASCENDING BY lifnr.
        DELETE   ADJACENT DUPLICATES   FROM lt_chngs_lfa1
                                  COMPARING lifnr.

        SELECT   lifnr  bukrs
          INTO   TABLE lt_lfb1_key
          FROM   lfb1 FOR ALL ENTRIES IN lt_chngs_lfa1
         WHERE   lifnr  = lt_chngs_lfa1-lifnr
           AND   bukrs IN s_bukrs.

      ENDIF.

*eject
      DELETE   lt_chngs_lfb1 WHERE bukrs IS INITIAL
                                OR bukrs NOT IN s_bukrs
                                OR lifnr NOT IN s_lifnr.

      IF       ( lt_chngs_lfb1[]         IS NOT INITIAL ).

        SORT     lt_chngs_lfb1 ASCENDING BY lifnr bukrs.
        DELETE   ADJACENT DUPLICATES   FROM lt_chngs_lfb1
                                  COMPARING lifnr bukrs.

        SELECT   lifnr  bukrs
     APPENDING   TABLE lt_lfb1_key
          FROM   lfb1 FOR ALL ENTRIES IN lt_chngs_lfb1
         WHERE   lifnr  = lt_chngs_lfb1-lifnr
           AND   bukrs  = lt_chngs_lfb1-bukrs
           AND   bukrs IN s_bukrs.

      ENDIF.

    ENDIF.

  ENDIF.

  IF   ( ( lt_lfa1_key[] IS INITIAL ) OR
         ( lt_lfb1_key[] IS INITIAL )    ).
    MESSAGE 'No Data Found'(001) TYPE 'I'.
    RETURN.
  ENDIF.

*eject
  IF     ( p_appl IS NOT INITIAL ).
    PERFORM  f_dataset_open.
  ENDIF.

  SORT     lt_lfb1_key ASCENDING BY lifnr bukrs.
  DELETE   ADJACENT DUPLICATES FROM lt_lfb1_key
                          COMPARING lifnr bukrs.

  CLEAR                                     ls_lfb1_key.
  LOOP AT    lt_lfb1_key               INTO ls_lfb1_key.

    APPEND   ls_lfb1_key                 TO lt_lfb1_key_p.

    AT END OF lifnr.

      CLEAR                                 ls_lfa1_key.
      MOVE     ls_lfb1_key-lifnr         TO ls_lfa1_key-lifnr.
      APPEND   ls_lfa1_key               TO lt_lfa1_key_p.
      ADD      1                         TO lv_rowcnt.

      IF     ( lv_rowcnt                 GE 2000 ).
        CLEAR  lv_rowcnt.

        PERFORM  f_get_data          TABLES lt_lfa1_key_p
                                            lt_lfb1_key_p.

        CLEAR    lt_lfa1_key_p[].
        CLEAR    lt_lfb1_key_p[].

      ENDIF.

    ENDAT.

    AT LAST.

      IF     ( lv_rowcnt                 GE 1    ).
        CLEAR  lv_rowcnt.

        PERFORM  f_get_data          TABLES lt_lfa1_key_p
                                            lt_lfb1_key_p.

        CLEAR    lt_lfa1_key_p[].
        CLEAR    lt_lfb1_key_p[].

      ENDIF.

    ENDAT.

    CLEAR  ls_lfb1_key.
  ENDLOOP.

  IF     ( p_appl IS NOT INITIAL ).
    PERFORM  f_dataset_close.
  ENDIF.

ENDFORM.                    " f_process_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_data
*&---------------------------------------------------------------------*
*       Get the vendor data
*----------------------------------------------------------------------*
FORM f_get_data
  TABLES   it_lfa1_key            TYPE gtt_lfa1_key
           it_lfb1_key            TYPE gtt_lfb1_key.

  CONSTANTS:
           lc_abtnr_0009          TYPE abtnr_pa  VALUE '0009',
           lc_pafkt_13            TYPE pafkt     VALUE '13'.

  CLEAR    gt_output[].
  CLEAR    gt_lfa1[].
  CLEAR    gt_lfb1[].
  CLEAR    gt_knvk[].
  CLEAR    gt_adrc[].
  CLEAR    gt_adr6[].

  SELECT   lifnr  land1  name1  name2  name3  name4
           ort01  pfach  pstl2  pstlz  regio  stras
           adrnr  ernam  ktokk  loevm  sperr
           stcd1  stcd2  telf1  telf2  telfx
           stceg  lfurl  updat  j_sc_currency
    INTO   TABLE gt_lfa1
    FROM   lfa1 FOR ALL ENTRIES IN it_lfa1_key
   WHERE   lifnr = it_lfa1_key-lifnr.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_lfa1 ASCENDING BY lifnr.
  ELSE.
    RETURN.
  ENDIF.

  SELECT   lifnr  bukrs  sperr  loevm  zterm
    INTO   TABLE gt_lfb1
    FROM   lfb1 FOR ALL ENTRIES IN it_lfb1_key
   WHERE   lifnr = it_lfb1_key-lifnr
     AND   bukrs = it_lfb1_key-bukrs.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_lfb1 ASCENDING BY lifnr bukrs.
  ELSE.
    RETURN.
  ENDIF.

*eject
  SELECT   parnr  lifnr  prsnr
    INTO   TABLE gt_knvk
    FROM   knvk FOR ALL ENTRIES IN it_lfa1_key
   WHERE   lifnr = it_lfa1_key-lifnr
     AND   abtnr = lc_abtnr_0009
     AND   pafkt = lc_pafkt_13.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_knvk     ASCENDING BY lifnr parnr.
    DELETE ADJACENT DUPLICATES FROM gt_knvk
                          COMPARING lifnr parnr.
  ELSE.
    CLEAR  gt_knvk[].
  ENDIF.

  SELECT   addrnumber  date_from  str_suppl1  str_suppl2  str_suppl3
    INTO   TABLE gt_adrc
    FROM   adrc FOR ALL ENTRIES IN gt_lfa1
   WHERE   addrnumber = gt_lfa1-adrnr.
  IF     ( sy-subrc EQ 0 ).
    SORT   gt_adrc               BY addrnumber ASCENDING
                                    date_from  DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_adrc
                          COMPARING addrnumber.
  ELSE.
    CLEAR  gt_adrc[].
  ENDIF.

  SELECT   addrnumber  persnumber  date_from  smtp_addr
    INTO   TABLE gt_adr6
    FROM   adr6 FOR ALL ENTRIES IN gt_lfa1
   WHERE   addrnumber = gt_lfa1-adrnr.
  IF     ( sy-subrc EQ 0 ).
    DELETE gt_adr6            WHERE smtp_addr IS INITIAL.
    SORT   gt_adr6               BY addrnumber ASCENDING
                                    persnumber ASCENDING
                                    date_from  DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_adr6
                          COMPARING addrnumber persnumber.
  ELSE.
    CLEAR  gt_adr6[].
  ENDIF.

  IF     ( p_full IS NOT INITIAL ).

    CLEAR    gt_update_last[].

    PERFORM  f_get_last_update.

  ENDIF.

  PERFORM  f_fill_data.

ENDFORM.                    " f_get_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_fill_data
*&---------------------------------------------------------------------*
*       Fill the output record
*----------------------------------------------------------------------*
FORM f_fill_data.

  DATA:    lv_subrc        TYPE sysubrc,
           lv_tabix        TYPE sytabix,
           lv_status       TYPE char1.

  CONSTANTS: lc_sep        TYPE char1 VALUE '_'.

  FIELD-SYMBOLS: <fs_knvk> TYPE gty_knvk.

  CLEAR    gs_lfa1.
  CLEAR    gs_knvk.
  CLEAR    gs_adrc.
  CLEAR    gs_adr6.

* Loop at the vendor-company master data
  CLEAR                                     gs_lfb1.
  LOOP AT  gt_lfb1                     INTO gs_lfb1.

* Read the associated vendor, vendor contact, and SMTP address records
    IF       ( gs_lfa1-lifnr             NE gs_lfb1-lifnr ).

      CLEAR    gs_lfa1.
      CLEAR    gs_knvk.
      CLEAR    gs_adrc.
      CLEAR    gs_adr6.

      READ     TABLE gt_lfa1           INTO gs_lfa1
                                   WITH KEY lifnr = gs_lfb1-lifnr
                              BINARY SEARCH.
      IF     ( sy-subrc EQ 0 ).

        IF     ( gs_lfa1-adrnr           IS NOT INITIAL ).

          READ     TABLE gt_adrc       INTO gs_adrc
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                              BINARY SEARCH.
          IF     ( sy-subrc NE 0 ).
            CLEAR  gs_adrc.
          ENDIF.

        ENDIF.

        READ     TABLE gt_knvk         INTO gs_knvk
                                   WITH KEY lifnr = gs_lfa1-lifnr
                              BINARY SEARCH.
        lv_subrc = sy-subrc.
        lv_tabix = sy-tabix.

*eject
        IF     ( lv_subrc EQ 0 ).

          IF     ( gs_lfa1-adrnr         IS NOT INITIAL ).

            LOOP AT  gt_knvk      ASSIGNING <fs_knvk>
                                       FROM lv_tabix.

              IF     ( <fs_knvk>-lifnr   NE gs_lfa1-lifnr ).
                EXIT.
              ENDIF.

              IF     ( <fs_knvk>-prsnr   IS INITIAL ).
                CONTINUE.
              ENDIF.

              CLEAR                         gs_adr6.
              READ     TABLE gt_adr6   INTO gs_adr6
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                                            persnumber = <fs_knvk>-prsnr
                              BINARY SEARCH.
              IF     ( sy-subrc EQ 0 ).
                CLEAR                       gs_knvk.
                MOVE     <fs_knvk>       TO gs_knvk.
                EXIT.
              ELSE.
                CLEAR    gs_adr6.
              ENDIF.

            ENDLOOP.

            IF     ( gs_adr6-addrnumber  IS INITIAl ).

              CLEAR                         gs_adr6.
              READ     TABLE gt_adr6   INTO gs_adr6
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                                            persnumber = SPACE
                              BINARY SEARCH.
              IF     ( sy-subrc NE 0 ).
                CLEAR  gs_adr6.
              ENDIF.

            ENDIF.

          ENDIF.

        ELSE.
          CLEAR    gs_knvk.
          CLEAR    gs_adr6.
        ENDIF.

*eject
* Read the last update date and user
        CLEAR                               gs_update_last.
        READ TABLE gt_update_last      INTO gs_update_last
                                   WITH KEY lifnr = gs_lfa1-lifnr
                              BINARY SEARCH.
        IF     ( sy-subrc EQ 0 ).
          CLEAR                             gs_lfa1-ernam.
          MOVE   gs_update_last-username TO gs_lfa1-ernam.
          CLEAR                             gs_lfa1-updat.
          MOVE   gs_update_last-udate    TO gs_lfa1-updat.
        ENDIF.

      ELSE.
        CLEAR  gs_lfa1.
      ENDIF.

    ENDIF.

    IF       ( gs_lfa1-lifnr IS INITIAL ).
      CLEAR    gs_lfa1.
      CLEAR    gs_lfb1.
      CONTINUE.
    ENDIF.

* Set the status
    CLEAR                                   lv_status.
    IF     ( ( gs_lfa1-loevm             IS NOT INITIAL ) OR
             ( gs_lfb1-loevm             IS NOT INITIAL )    ).
      MOVE     '3'                       TO lv_status.
    ELSEIF ( ( gs_lfa1-sperr             IS NOT INITIAL ) OR
             ( gs_lfb1-sperr             IS NOT INITIAL )    ).
      MOVE     '2'                       TO lv_status.
    ELSE.
      MOVE     '1'                       TO lv_status.
    ENDIF.

    IF   ( ( ( lv_status EQ '2' ) AND ( p_incblk IS INITIAL ) ) OR
           ( ( lv_status EQ '3' ) AND ( p_incdel IS INITIAL ) )    ).
      CLEAR    gs_lfb1.
      CONTINUE.
    ENDIF.

*eject
* Build the output record
    CLEAR    gs_output.

    CONCATENATE               gs_lfb1-lifnr gs_lfb1-bukrs
                                       INTO gs_output-vulid
                               SEPARATED BY lc_sep.

    MOVE     gs_lfa1-lifnr               TO gs_output-lifnr.
    MOVE     gs_lfa1-name1               TO gs_output-name1.
    MOVE     gs_lfb1-zterm               TO gs_output-zterm.
    MOVE     gs_lfb1-bukrs               TO gs_output-bukrs.
    MOVE     gs_lfa1-ktokk               TO gs_output-ktokk.
    MOVE     lv_status                   TO gs_output-status.

* Set the address line and postal code
    IF     ( gs_lfa1-stras               IS NOT INITIAL ).
      MOVE   gs_lfa1-stras               TO gs_output-stras.
      MOVE   gs_lfa1-pstlz               TO gs_output-pstlz.
    ELSEIF ( gs_lfa1-pfach               IS NOT INITIAL ).
      CONCATENATE 'P.O. Box'                gs_lfa1-pfach
                                       INTO gs_output-stras
                         SEPARATED BY SPACE.
      IF   ( gs_lfa1-pstl2               IS NOT INITIAL ).
        MOVE gs_lfa1-pstl2               TO gs_output-pstlz.
      ELSE.
        MOVE gs_lfa1-pstlz               TO gs_output-pstlz.
      ENDIF.
    ENDIF.

*eject
    MOVE     gs_lfa1-ort01               TO gs_output-ort01.
    MOVE     gs_lfa1-regio               TO gs_output-regio.
    MOVE     gs_lfa1-land1               TO gs_output-land1.
    MOVE     gs_lfa1-telf1               TO gs_output-telf1.
    MOVE     gs_lfa1-name2               TO gs_output-name2.
    MOVE     gs_adrc-str_suppl1          TO gs_output-str_suppl1.
    MOVE     gs_adrc-str_suppl2          TO gs_output-str_suppl2.
    MOVE     gs_adrc-str_suppl3          TO gs_output-str_suppl3.

* Tax ID Number
    IF     ( gs_lfa1-stcd1               IS NOT INITIAL ) AND
           ( gs_lfa1-stcd1               CN ' 9-'       ).
      MOVE   gs_lfa1-stcd1               TO gs_output-stcd1.
    ENDIF.

*   WRITE    gs_lfa1-updat               TO gs_output-updat1 MM/DD/YYYY.
*   WRITE    gs_lfa1-updat               TO gs_output-updat2 MM/DD/YYYY.
    PERFORM  f_format_date            USING gs_lfa1-updat
                                   CHANGING gs_output-updat2.
    MOVE     '0'                         TO gs_output-prefvend.
    MOVE     p_erpid                     TO gs_output-erpid.
    MOVE     gs_lfa1-j_sc_currency       TO gs_output-waers.
    MOVE     gs_lfa1-ernam               TO gs_output-ernam.
    MOVE     gs_lfa1-telf2               TO gs_output-telf2.
    MOVE     gs_lfa1-telfx               TO gs_output-telfx.
    MOVE     gs_adr6-smtp_addr           TO gs_output-lfurl.
*   MOVE     ?????                       TO gs_output-comments.

    IF     ( p_appl                      IS NOT INITIAL ).
      CLEAR                                 gs_data_tab.
      CONCATENATE      gs_output-vulid      gs_output-lifnr
                       gs_output-name1      gs_output-zterm
                       gs_output-bukrs      gs_output-ktokk
                       gs_output-status     gs_output-stras
                       gs_output-ort01      gs_output-regio
                       gs_output-pstlz      gs_output-land1
                       gs_output-telf1      gs_output-name2
                       gs_output-str_suppl1 gs_output-str_suppl2
                       gs_output-str_suppl3 gs_output-stcd1
                       gs_output-updat1     gs_output-updat2
                       gs_output-prefvend   gs_output-erpid
                       gs_output-waers      gs_output-ernam
                       gs_output-telf2      gs_output-telfx
                       gs_output-lfurl      gs_output-comments
                                       INTO gs_data_tab
                               SEPARATED BY '|~'.
      TRANSFER gs_data_tab               TO gv_filename2.
      ADD      1                         TO gv_rec_count.
    ELSE.
      APPEND   gs_output                   TO gt_output.
    ENDIF.

    CLEAR  gs_lfb1.
  ENDLOOP.

*eject
  IF     ( p_pres                      IS NOT INITIAL ).

    PERFORM  f_dataset_download.

  ENDIF.

ENDFORM.                    " f_fill_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_select_changes
*&---------------------------------------------------------------------*
*       Select the change documents
*----------------------------------------------------------------------*
FORM f_select_changes
  TABLES   ct_chngs_lfa1               TYPE gtt_chngs
           ct_chngs_lfb1               TYPE gtt_chngs.

  DATA:    lt_chngs                    TYPE gtt_chngs,
           ls_chngs                    TYPE gty_chngs,
           lt_chngs_lfa1               TYPE gtt_chngs,
           ls_chngs_lfa1               TYPE gty_chngs,
           lt_chngs_lfb1               TYPE gtt_chngs,
           ls_chngs_lfb1               TYPE gty_chngs,
           lt_cdhdr                    TYPE STANDARD TABLE of cdhdr,
           ls_cdhdr                    TYPE cdhdr,
           lt_cdpos                    TYPE STANDARD TABLE of cdpos,
           ls_cdpos                    TYPE cdpos.

  TYPES: BEGIN OF lty_lfa1,
           lifnr                       TYPE lifnr,
           adrnr                       TYPE adrnr,
         END   OF lty_lfa1.

  TYPES: BEGIN OF lty_knvk,
           parnr                       TYPE parnr,
           lifnr                       TYPE lifnr,
           prsnr                       TYPE ad_persnum,
  END   OF lty_knvk.

  DATA:    ls_lfa1                     TYPE lty_lfa1,
           ls_knvk                     TYPE lty_knvk.

  DATA:    lv_parnr                    TYPE parnr,
           lv_addrnumber               TYPE ad_addrnum,
           lv_persnumber               TYPE ad_persnum.

  CONSTANTS: lc_0009                   TYPE abtnr_pa VALUE '0009',
             lc_0013                   TYPE pafkt    VALUE '13'.

  CLEAR    ct_chngs_lfa1[].
  CLEAR    ct_chngs_lfb1[].

  CLEAR    lt_chngs_lfa1[].
  CLEAR    lt_chngs_lfb1[].

*eject
* Select the address changes (ADRC)
  CLEAR    lt_cdhdr[].
  CLEAR    lt_cdpos[].

  SELECT   *
    INTO   TABLE lt_cdhdr
    FROM   cdhdr
   WHERE   objectclas  = 'ADRESSE'
     AND   udate      IN s_updat.
  IF     ( sy-subrc EQ 0 ).

    SELECT   *
      INTO   TABLE lt_cdpos
      FROM   cdpos FOR ALL ENTRIES IN lt_cdhdr
     WHERE   objectclas = lt_cdhdr-objectclas
       AND   objectid   = lt_cdhdr-objectid
       AND   changenr   = lt_cdhdr-changenr.
    IF     ( sy-subrc EQ 0 ).

      DELETE   lt_cdpos    WHERE tabname NE 'ADRC'.

      IF     ( lt_cdpos[]                IS NOT INITIAL ).

        SORT   lt_cdhdr ASCENDING BY objectclas objectid changenr.
        SORT   lt_cdpos ASCENDING BY objectclas objectid changenr.

        CLEAR                               ls_cdpos.
        LOOP AT  lt_cdpos              INTO ls_cdpos.

          IF   ( ( ls_cdhdr-objectclas   EQ ls_cdpos-objectclas ) AND
                 ( ls_cdhdr-objectid     EQ ls_cdpos-objectid   ) AND
                 ( ls_cdhdr-changenr     EQ ls_cdpos-changenr   )     ).
          ELSE.
            CLEAR                      ls_cdhdr.
            READ TABLE lt_cdhdr   INTO ls_cdhdr
                              WITH KEY objectclas = ls_cdpos-objectclas
                                       objectid   = ls_cdpos-objectid
                                       changenr   = ls_cdpos-changenr
                         BINARY SEARCH.
          ENDIF.

*eject
          CLEAR                              lv_addrnumber.
          MOVE     ls_cdpos-tabkey+03(10) TO lv_addrnumber.

          CLEAR    ls_lfa1.
          SELECT   SINGLE lifnr  adrnr
            INTO   ls_lfa1
            FROM   lfa1
           WHERE   adrnr = lv_addrnumber.
          IF     ( sy-subrc EQ 0 ).
            CLEAR                           ls_chngs_lfa1.
            MOVE     ls_lfa1-lifnr       TO ls_chngs_lfa1-lifnr.
            MOVE     SPACE               TO ls_chngs_lfa1-bukrs.
            MOVE     ls_cdhdr-udate      TO ls_chngs_lfa1-udate.
            MOVE     ls_cdhdr-changenr   TO ls_chngs_lfa1-changenr.
            MOVE     ls_cdhdr-username   TO ls_chngs_lfa1-username.
            APPEND   ls_chngs_lfa1       TO lt_chngs_lfa1.
            APPEND   ls_chngs_lfa1       TO lt_chngs_lfb1.
          ENDIF.

          CLEAR  ls_cdhdr.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDIF.

*eject
* Select the URL changes (ADR6)
  CLEAR    lt_cdhdr[].
  CLEAR    lt_cdpos[].

  SELECT   *
    INTO   TABLE lt_cdhdr
    FROM   cdhdr
   WHERE   objectclas  = 'ADRESSE3'
     AND   udate      IN s_updat.
  IF     ( sy-subrc EQ 0 ).

    SELECT   *
      INTO   TABLE lt_cdpos
      FROM   cdpos FOR ALL ENTRIES IN lt_cdhdr
     WHERE   objectclas = lt_cdhdr-objectclas
       AND   objectid   = lt_cdhdr-objectid
       AND   changenr   = lt_cdhdr-changenr.
    IF     ( sy-subrc EQ 0 ).

      DELETE   lt_cdpos    WHERE tabname NE 'ADR6'.

      IF     ( lt_cdpos[]                IS NOT INITIAL ).

        SORT   lt_cdhdr ASCENDING BY objectclas objectid changenr.
        SORT   lt_cdpos ASCENDING BY objectclas objectid changenr.

        CLEAR                               ls_cdpos.
        LOOP AT  lt_cdpos              INTO ls_cdpos.

          IF   ( ( ls_cdhdr-objectclas   EQ ls_cdpos-objectclas ) AND
                 ( ls_cdhdr-objectid     EQ ls_cdpos-objectid   ) AND
                 ( ls_cdhdr-changenr     EQ ls_cdpos-changenr   )     ).
          ELSE.
            CLEAR                      ls_cdhdr.
            READ TABLE lt_cdhdr   INTO ls_cdhdr
                              WITH KEY objectclas = ls_cdpos-objectclas
                                       objectid   = ls_cdpos-objectid
                                       changenr   = ls_cdpos-changenr
                         BINARY SEARCH.
          ENDIF.

*eject
          CLEAR                              lv_addrnumber.
          MOVE     ls_cdpos-tabkey+03(10) TO lv_addrnumber.
          CLEAR                              lv_persnumber.
          MOVE     ls_cdpos-tabkey+13(10) TO lv_persnumber.

          CLEAR    ls_knvk.
          SELECT   SINGLE parnr  lifnr  prsnr
            INTO   ls_knvk
            FROM   knvk
           WHERE   lifnr > SPACE
             AND   prsnr = lv_persnumber
             AND   abtnr = lc_0009
             AND   pafkt = lc_0013.
          IF     ( sy-subrc EQ 0 ).
            CLEAR                           ls_chngs_lfa1.
            MOVE     ls_knvk-lifnr       TO ls_chngs_lfa1-lifnr.
            MOVE     SPACE               TO ls_chngs_lfa1-bukrs.
            MOVE     ls_cdhdr-udate      TO ls_chngs_lfa1-udate.
            MOVE     ls_cdhdr-changenr   TO ls_chngs_lfa1-changenr.
            MOVE     ls_cdhdr-username   TO ls_chngs_lfa1-username.
            APPEND   ls_chngs_lfa1       TO lt_chngs_lfa1.
            APPEND   ls_chngs_lfa1       TO lt_chngs_lfb1.
          ENDIF.

          CLEAR  ls_cdhdr.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDIF.

*eject
* Select the vendor changes (LFA1, LFB1, KNVK)
  CLEAR    lt_cdhdr[].
  CLEAR    lt_cdpos[].

  SELECT   *
    INTO   TABLE lt_cdhdr
    FROM   cdhdr
   WHERE   objectclas  = 'KRED'
     AND   udate      IN s_updat.
  IF     ( sy-subrc EQ 0 ).

    SELECT   *
      INTO   TABLE lt_cdpos
      FROM   cdpos FOR ALL ENTRIES IN lt_cdhdr
     WHERE   objectclas = lt_cdhdr-objectclas
       AND   objectid   = lt_cdhdr-objectid
       AND   changenr   = lt_cdhdr-changenr.
    IF     ( sy-subrc EQ 0 ).

      DELETE   lt_cdpos    WHERE tabname NE 'LFA1'
                             AND tabname NE 'LFB1'
                             AND tabname NE 'KNVK'.

      IF     ( lt_cdpos[]                IS NOT INITIAL ).

        SORT   lt_cdhdr ASCENDING BY objectclas objectid changenr.
        SORT   lt_cdpos ASCENDING BY objectclas objectid changenr.

        CLEAR                               ls_cdpos.
        LOOP AT  lt_cdpos              INTO ls_cdpos.

          IF   ( ( ls_cdhdr-objectclas   EQ ls_cdpos-objectclas ) AND
                 ( ls_cdhdr-objectid     EQ ls_cdpos-objectid   ) AND
                 ( ls_cdhdr-changenr     EQ ls_cdpos-changenr   )     ).
          ELSE.
            CLEAR                      ls_cdhdr.
            READ TABLE lt_cdhdr   INTO ls_cdhdr
                              WITH KEY objectclas = ls_cdpos-objectclas
                                       objectid   = ls_cdpos-objectid
                                       changenr   = ls_cdpos-changenr
                         BINARY SEARCH.
          ENDIF.

*eject
          IF     ( ls_cdpos-tabname         EQ 'LFA1' ).
            CLEAR                              ls_chngs_lfa1.
            MOVE     ls_cdpos-objectid(10)  TO ls_chngs_lfa1-lifnr.
            MOVE     SPACE                  TO ls_chngs_lfa1-bukrs.
            MOVE     ls_cdhdr-udate         TO ls_chngs_lfa1-udate.
            MOVE     ls_cdhdr-changenr      TO ls_chngs_lfa1-changenr.
            MOVE     ls_cdhdr-username      TO ls_chngs_lfa1-username.
            APPEND   ls_chngs_lfa1          TO lt_chngs_lfa1.
            APPEND   ls_chngs_lfa1          TO lt_chngs_lfb1.
          ELSEIF ( ls_cdpos-tabname         EQ 'LFB1' ).
            CLEAR                              ls_chngs_lfb1.
            MOVE     ls_cdpos-objectid(10)  TO ls_chngs_lfb1-lifnr.
            MOVE     SPACE                  TO ls_chngs_lfb1-bukrs.
            MOVE     ls_cdhdr-udate         TO ls_chngs_lfb1-udate.
            MOVE     ls_cdhdr-changenr      TO ls_chngs_lfb1-changenr.
            MOVE     ls_cdhdr-username      TO ls_chngs_lfb1-username.
            APPEND   ls_chngs_lfb1          TO lt_chngs_lfa1.
            MOVE     ls_cdpos-tabkey+13(04) TO ls_chngs_lfb1-bukrs.
            APPEND   ls_chngs_lfb1          TO lt_chngs_lfb1.
          ELSEIF ( ls_cdpos-tabname         EQ 'KNVK' ).

            CLEAR                              lv_parnr.
            MOVE     ls_cdpos-tabkey+03(10) TO lv_parnr.

            CLEAR    ls_knvk.
            SELECT   SINGLE parnr  lifnr  prsnr
              INTO   ls_knvk
              FROM   knvk
             WHERE   parnr = lv_parnr
               AND   abtnr = lc_0009
               AND   pafkt = lc_0013.
            IF     ( sy-subrc EQ 0 ).
              CLEAR                            ls_chngs_lfa1.
              MOVE   ls_cdpos-objectid(10)  TO ls_chngs_lfa1-lifnr.
              MOVE   SPACE                  TO ls_chngs_lfa1-bukrs.
              MOVE   ls_cdhdr-udate         TO ls_chngs_lfa1-udate.
              MOVE   ls_cdhdr-changenr      TO ls_chngs_lfa1-changenr.
              MOVE   ls_cdhdr-username      TO ls_chngs_lfa1-username.
              APPEND ls_chngs_lfa1          TO lt_chngs_lfa1.
              APPEND ls_chngs_lfa1          TO lt_chngs_lfb1.
            ENDIF.
          ENDIF.

          CLEAR  ls_cdhdr.
        ENDLOOP.

      ENDIF.
    ENDIF.
  ENDIF.

*eject
  SORT     lt_chngs_lfa1                 BY lifnr     ASCENDING
                                            udate     DESCENDING
                                            changenr  DESCENDING.
  DELETE   ADJACENT DUPLICATES         FROM lt_chngs_lfa1
                                  COMPARING lifnr.

  SORT     lt_chngs_lfb1                 BY lifnr     ASCENDING
                                            bukrs     ASCENDING
                                            udate     DESCENDING
                                            changenr  DESCENDING.
  DELETE   ADJACENT DUPLICATES         FROM lt_chngs_lfb1
                                  COMPARING lifnr bukrs.

  APPEND   LINES OF lt_chngs_lfa1        TO lt_chngs.
  APPEND   LINES OF lt_chngs_lfb1        TO lt_chngs.

  SORT     lt_chngs                      BY lifnr     ASCENDING
                                            udate     DESCENDING
                                            changenr  DESCENDING.
  DELETE   ADJACENT DUPLICATES         FROM lt_chngs
                                  COMPARING lifnr.

  CLEAR                                     ls_chngs.
  LOOP AT  lt_chngs                    INTO ls_chngs.
    CLEAR                                   gs_update_last.
    MOVE   ls_chngs-lifnr                TO gs_update_last-lifnr.
    MOVE   ls_chngs-udate                TO gs_update_last-udate.
    MOVE   ls_chngs-username             TO gs_update_last-username.
    APPEND gs_update_last                TO gt_update_last.
    CLEAR  ls_chngs.
  ENDLOOP.

  SORT     gt_update_last      ASCENDING BY lifnr.

  ct_chngs_lfa1[] = lt_chngs_lfa1[].
  ct_chngs_lfb1[] = lt_chngs_lfb1[].

ENDFORM.                    " f_select_changes
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_last_update
*&---------------------------------------------------------------------*
*       Read the last update date and user
*----------------------------------------------------------------------*
FORM f_get_last_update.

  DATA:    lt_objectid                 TYPE gtt_objectid,
           ls_objectid                 TYPE gty_objectid,
           lt_cdhdr                    TYPE gtt_cdhdr.

  FIELD-SYMBOLS: <fs_lfa1>             TYPE gty_lfa1,
                 <fs_cdhdr>            TYPE gty_cdhdr.

  CLEAR    gt_update_last[].

  CLEAR    lt_objectid[].
  CLEAR    lt_cdhdr[].

  LOOP AT    gt_lfa1              ASSIGNING <fs_lfa1>.
    CLEAR                                   ls_objectid.
    MOVE     <fs_lfa1>-lifnr             TO ls_objectid-objectid.
    APPEND   ls_objectid                 TO lt_objectid.
  ENDLOOP.

  SORT     lt_objectid         ASCENDING BY objectid.

  SELECT   *
    INTO   TABLE lt_cdhdr
    FROM   cdhdr FOR ALL ENTRIES IN lt_objectid
   WHERE   objectclas = 'KRED'
     AND   objectid   = lt_objectid-objectid.
  IF     ( sy-subrc NE 0 ).
    RETURN.
  ENDIF.

  SORT     lt_cdhdr                      BY objectid ASCENDING
                                            udate    DESCENDING
                                            changenr DESCENDING.
  DELETE   ADJACENT DUPLICATES         FROM lt_cdhdr
                                  COMPARING objectid.

  LOOP AT    lt_cdhdr             ASSIGNING <fs_cdhdr>.
    CLEAR                                   gs_update_last.
    MOVE     <fs_cdhdr>-objectid         TO gs_update_last-lifnr.
    MOVE     <fs_cdhdr>-udate            TO gs_update_last-udate.
    MOVE     <fs_cdhdr>-username         TO gs_update_last-username.
    APPEND   gs_update_last              TO gt_update_last.
  ENDLOOP.

ENDFORM.                    " f_get_last_update
*eject
*&---------------------------------------------------------------------*
*&      Form  f_dataset_open
*&---------------------------------------------------------------------*
*       Open the dataset
*----------------------------------------------------------------------*
FORM f_dataset_open.

  DATA:      lv_msg        TYPE string.

  CONSTANTS: lc_error_type TYPE msgty VALUE 'E'.

  IF     ( p_appl IS INITIAL ).
    RETURN.
  ENDIF.

* Open the dataset
  CLEAR                                     gv_filename2.
  CONCATENATE     p_path2     gc_fslash     p_file2
                                       INTO gv_filename2.

  OPEN DATASET gv_filename2 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF     ( sy-subrc NE 0 ).
    CLEAR    lv_msg.
    CONCATENATE text-e04 gv_filename1 INTO lv_msg.
    MESSAGE  lv_msg TYPE lc_error_type.
    RETURN.
  ENDIF.

* Write the column headers to the dataset
  CLEAR                                     gs_data_tab.
  CONCATENATE    text-f01 text-f02 text-f03 text-f04 text-f05
                 text-f06 text-f07 text-f08 text-f09 text-f10
                 text-f11 text-f12 text-f13 text-f14 text-f15
                 text-f16 text-f17 text-f18 text-f19 text-f20
                 text-f21 text-f22 text-f23 text-f24 text-f25
                 text-f26 text-f27 text-f28
                                       INTO gs_data_tab
                               SEPARATED BY '|~'.

  TRANSFER gs_data_tab                   TO gv_filename2.
  ADD      1                             TO gv_rec_count.

ENDFORM.                    " f_dataset_open
*eject
*&---------------------------------------------------------------------*
*&      Form  f_dataset_close
*&---------------------------------------------------------------------*
*       Close the dataset
*----------------------------------------------------------------------*
FORM f_dataset_close.

  IF     ( p_appl IS INITIAL ).
    RETURN.
  ENDIF.

  CLOSE    DATASET gv_filename2.

ENDFORM.                    " f_dataset_close
*eject
*&---------------------------------------------------------------------*
*&      Form  f_dataset_download
*&---------------------------------------------------------------------*
*       Download the dataset to the presentation server
*----------------------------------------------------------------------*
FORM f_dataset_download.

  DATA:    lv_append TYPE xflag,
           lv_lines  TYPE syindex.

  CLEAR    lv_append.

  IF     ( gv_rec_count IS INITIAL ).

    CLEAR                                   gs_output.
    MOVE     text-f01                    TO gs_output-vulid.
    MOVE     text-f02                    TO gs_output-lifnr.
    MOVE     text-f03                    TO gs_output-name1.
    MOVE     text-f04                    TO gs_output-zterm.
    MOVE     text-f05                    TO gs_output-bukrs.
    MOVE     text-f06                    TO gs_output-ktokk.
    MOVE     text-f07                    TO gs_output-status.
    MOVE     text-f08                    TO gs_output-stras.
    MOVE     text-f09                    TO gs_output-ort01.
    MOVE     text-f10                    TO gs_output-regio.
    MOVE     text-f11                    TO gs_output-pstlz.
    MOVE     text-f12                    TO gs_output-land1.
    MOVE     text-f13                    TO gs_output-telf1.
    MOVE     text-f14                    TO gs_output-name2.
    MOVE     text-f15                    TO gs_output-str_suppl1.
    MOVE     text-f16                    TO gs_output-str_suppl2.
    MOVE     text-f17                    TO gs_output-str_suppl3.
    MOVE     text-f18                    TO gs_output-stcd1.
    MOVE     text-f19                    TO gs_output-updat1.
    MOVE     text-f20                    TO gs_output-updat2.
    MOVE     text-f21                    TO gs_output-prefvend.
    MOVE     text-f22                    TO gs_output-erpid.
    MOVE     text-f23                    TO gs_output-waers.
    MOVE     text-f24                    TO gs_output-ernam.
    MOVE     text-f25                    TO gs_output-telf2.
    MOVE     text-f26                    TO gs_output-telfx.
    MOVE     text-f27                    TO gs_output-lfurl.
    MOVE     text-f28                    TO gs_output-comments.
    INSERT   gs_output                 INTO gt_output INDEX 1.

    CLEAR                                   gv_filename1.
    CONCATENATE   p_path1   gv_fname1  INTO gv_filename1
                               SEPARATED BY gc_fslash.
  ELSE.
    lv_append = 'X'.
  ENDIF.

  IF     ( gt_output[] IS INITIAL ).
    RETURN.
  ENDIF.

*eject
  CLEAR    gt_data_tab[].
  CLEAR                                     gs_output.
  LOOP AT  gt_output                   INTO gs_output.
    CLEAR                                   gs_data_tab.
    gref_util->add_pipes( EXPORTING im_rec    = gs_output
                          IMPORTING ex_outrec = gs_data_tab ).
    APPEND   gs_data_tab                 TO gt_data_tab.
  ENDLOOP.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
*     PERCENTAGE = 0
      text       = text-d05.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      FILENAME                = gv_filename1
      APPEND                  = lv_append
      WRITE_FIELD_SEPARATOR   = SPACE
    TABLES
      DATA_TAB                = gt_data_tab
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
* Implement suitable error handling here
  ENDIF.

  CLEAR                                     lv_lines.
  DESCRIBE TABLE gt_output            LINES lv_lines.
  ADD            lv_lines                TO gv_rec_count.

ENDFORM.                    " f_dataset_download
*eject
*&---------------------------------------------------------------------*
*&      Form  f_write_record_count
*&---------------------------------------------------------------------*
*       Write the record count
*----------------------------------------------------------------------*
FORM f_write_record_count.

  DATA:    lv_msg TYPE STRING.

  IF     ( gv_rec_count IS INITIAL ).

    MESSAGE  'No Data Found'(001) TYPE 'I'.

  ELSE.

    CLEAR    lv_msg.
    lv_msg = gv_rec_count.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_msg
      IMPORTING
        output = lv_msg.

    CONCATENATE 'Total number of records extracted:'(002)
                lv_msg INTO lv_msg SEPARATED BY SPACE.

    WRITE lv_msg.

  ENDIF.

ENDFORM.                    " f_write_record_count
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
