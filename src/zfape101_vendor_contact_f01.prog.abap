*&---------------------------------------------------------------------*
*&  Include           ZFAPE101_VENDOR_CONTACT_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_DEFAULT_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_default_filename .

  CLEAR:p_file1,p_file3.

  IF p_file1 IS INITIAL.

    CONCATENATE text-p00
                sy-datum '_'
                sy-uzeit '.TXT'
                INTO p_file1.

  ENDIF.

  IF p_file3 IS INITIAL.
    CONCATENATE text-p00
                text-p03
                sy-datum '_'
                sy-uzeit '.TXT'
                INTO p_file3.
  ENDIF.

  IF p_path1 IS INITIAL OR p_path3 IS INITIAL.

    p_path3 = p_path1 = 'V:'.

  ENDIF.
ENDFORM.                    " F_DEFAULT_FILENAME
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_TO_PRESENTATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_write_to_presentation .
  DATA: lv_msg TYPE string,
        lv_delim(2) TYPE c.

  CONSTANTS lc_error_type TYPE msgty VALUE 'E'.

  IF gt_output[] IS NOT INITIAL.
**    REFRESH gt_tab_data[].
    CLEAR: gs_output,
           gs_tab_data.

    IF NOT p_tab IS INITIAL.
      CONCATENATE text-f00
                  text-f01 text-f02 text-f03
                  text-f04 text-f05 text-f06
                  text-f07 text-f08 text-f09
                  text-f10 text-f11 text-f12
                  text-f13
                    INTO gs_tab_data SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    ELSE.
      CONCATENATE text-f00
                  text-f01 text-f02 text-f03
                  text-f04 text-f05 text-f06
                  text-f07 text-f08 text-f09
                  text-f10 text-f11 text-f12
                  text-f13
                      INTO gs_tab_data SEPARATED BY '|'.
    ENDIF.
    APPEND gs_tab_data TO gt_tab_data.

    LOOP AT  gt_output INTO  gs_output.
      CLEAR gs_tab_data.
      IF NOT p_tab IS INITIAL.
        CLEAR lv_delim.
        lv_delim = cl_abap_char_utilities=>horizontal_tab.
        gref_util->add_pipes( EXPORTING im_rec    = gs_output
                                        im_delim  = lv_delim
                              IMPORTING ex_outrec = gs_tab_data ).
      ELSE.
        gref_util->add_pipes( EXPORTING im_rec    = gs_output
                                        im_delim  = '|'
                              IMPORTING ex_outrec = gs_tab_data ).
      ENDIF.


      APPEND gs_tab_data TO gt_tab_data.
    ENDLOOP.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = text-d05.

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

* To populate log file
  IF NOT gt_error[] IS INITIAL.
    CLEAR gv_filename1.
    CONCATENATE gv_fold2
    gv_fname2
    INTO gv_filename1
    SEPARATED BY gc_fslash.
    TRY.
        CALL METHOD zcl_iap_interface_util=>download_file_pc
          EXPORTING
            im_filename = gv_filename1
            im_data_tab = gt_error[].
*            im_delim    = 'X'.
      CATCH cx_sy_file_io.
        CLEAR lv_msg.
        CONCATENATE text-e04 gv_filename1 INTO lv_msg.
        MESSAGE lv_msg TYPE lc_error_type.
    ENDTRY.
  ENDIF.
ENDFORM.                    " WRITE_TO_PRESENTATION
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data .

  TYPES: BEGIN OF lty_temp,
         lifnr TYPE lifnr,
         adrnr TYPE adrnr,
         prsnr TYPE ad_persnum,
         END OF   lty_temp.

  DATA: l_cursor1 TYPE cursor,
        lt_lfa1   TYPE STANDARD TABLE OF gty_lfa1,
        lt_knvk   TYPE STANDARD TABLE OF gty_knvk,
        lt_temp   TYPE STANDARD TABLE OF lty_temp,
        ls_temp   TYPE lty_temp.

* Fetching using cursor technique
  OPEN CURSOR WITH HOLD l_cursor1 FOR
      SELECT lifnr
             land1
             name1
             adrnr
             ktokk
             stcd1
             stcd2
              FROM lfa1
              WHERE lifnr IN s_lifnr[]
              AND   ktokk IN s_ktokk[]
              AND   loevm EQ space.
  DO.
    CLEAR lt_lfa1[].
    FETCH NEXT CURSOR l_cursor1
                      APPENDING TABLE lt_lfa1
                      PACKAGE SIZE 200.
    IF sy-subrc NE 0.
      CLOSE CURSOR l_cursor1.
      EXIT.
    ENDIF.

    IF NOT lt_lfa1[] IS INITIAL.
      APPEND LINES OF lt_lfa1 TO gt_lfa1. "Moving data to final table

      CLEAR lt_knvk[].
      SELECT  parnr
              abtnr
              pafkt
              lifnr
              prsnr
                FROM knvk
                INTO TABLE lt_knvk
                FOR ALL ENTRIES IN lt_lfa1
                WHERE ( ( abtnr EQ gc_0009 AND pafkt EQ gc_13 )
                OR    ( abtnr EQ gc_0002 AND pafkt EQ gc_12 ) )
                AND   lifnr = lt_lfa1-lifnr.
      IF sy-subrc EQ 0.
* As there is a chance of having multiple records saving extracted data
        APPEND LINES OF lt_knvk TO gt_knvk.

* Deleting duplicate entries for using FOR ALL ENTRIES
        SORT lt_knvk BY prsnr.
        DELETE ADJACENT DUPLICATES FROM lt_knvk COMPARING prsnr.

      ENDIF.

      IF NOT lt_knvk[] IS INITIAL.
        SELECT persnumber
               name_first
               name_last
               langu
                FROM adrp
                APPENDING TABLE gt_adrp
                FOR ALL ENTRIES IN lt_knvk
                WHERE persnumber = lt_knvk-prsnr.
        IF sy-subrc EQ 0.
          SORT gt_adrp BY persnumber.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.

* Sort the internal tables
  SORT : gt_lfa1[],
         gt_knvk[],
         gt_adrp[].

* As there is a chance of having duplicate records
  DELETE ADJACENT DUPLICATES FROM: gt_lfa1 COMPARING ALL FIELDS,
                                   gt_knvk COMPARING ALL FIELDS,
                                   gt_adrp COMPARING ALL FIELDS.

  SELECT sprsl
         sptxt
          FROM t002t
          INTO TABLE gt_t002t
          WHERE spras EQ gc_en.
  IF sy-subrc EQ 0.
    SORT gt_t002t BY sprsl.
  ENDIF.

  SELECT land1
         landx
          FROM t005t
          INTO TABLE gt_t005t
          WHERE spras EQ gc_en.
  IF sy-subrc EQ 0.
    SORT gt_t005t BY land1.
  ENDIF.

  CLEAR lt_lfa1[].
  lt_lfa1[] = gt_lfa1[].
  SORT lt_lfa1 BY ktokk.
  DELETE ADJACENT DUPLICATES FROM lt_lfa1 COMPARING ktokk.
  IF NOT lt_lfa1[] IS INITIAL.
    SELECT ktokk
           txt30
            FROM t077y
            INTO TABLE gt_t077y
            FOR ALL ENTRIES IN lt_lfa1
            WHERE spras = gc_en
            AND   ktokk EQ lt_lfa1-ktokk.
    IF sy-subrc EQ 0.
      SORT gt_t077y BY ktokk.
    ENDIF.
  ENDIF.

* Build a temporary table to retrieve address details
  LOOP AT gt_knvk INTO gs_knvk.

    CLEAR: gs_lfa1,
           ls_temp.

    IF gs_knvk-prsnr IS INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_knvk-lifnr
                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE: gs_lfa1-lifnr TO ls_temp-lifnr,
            gs_lfa1-adrnr TO ls_temp-adrnr,
            gs_knvk-prsnr TO ls_temp-prsnr.
      APPEND ls_temp TO lt_temp.
    ENDIF.

  ENDLOOP.

  SORT lt_temp BY adrnr prsnr.
  DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING adrnr prsnr.

  IF NOT lt_temp[] IS INITIAL.
    SELECT addrnumber
           persnumber
           country
           tel_number
                FROM adr2
                INTO TABLE gt_adr2
                FOR ALL ENTRIES IN lt_temp
                WHERE addrnumber = lt_temp-adrnr
                AND   persnumber = lt_temp-prsnr.
    IF sy-subrc EQ 0.
      SORT gt_adr2 BY addrnumber persnumber.
    ENDIF.

    SELECT addrnumber
           persnumber
           smtp_addr
                 FROM adr6
                 INTO TABLE gt_adr6
                 FOR ALL ENTRIES IN lt_temp
                 WHERE addrnumber = lt_temp-adrnr
                 AND   persnumber = lt_temp-prsnr.
    IF sy-subrc EQ 0.
      SORT gt_adr6 BY addrnumber persnumber.
    ENDIF.
  ENDIF.

  FREE: lt_lfa1[],
        lt_knvk[],
        lt_temp[].
ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_data .

  DATA: lv_cnt(10)  TYPE c.

  CONSTANTS: lc_sc(2) TYPE c VALUE 'SC',
             lc_ap(2) TYPE c VALUE 'AP'.

  SORT: gt_lfa1 BY lifnr,
        gt_adrp BY persnumber,
        gt_knvk BY lifnr.

  LOOP AT gt_knvk INTO gs_knvk.

    CLEAR: gs_lfa1,
           gs_adrp,
           gs_output,
           gs_t077y,
           gs_t002t,
           gs_adr2,
           gs_adr6.

    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_knvk-lifnr
                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE: gs_lfa1-lifnr TO gs_output-lifnr,
            gs_lfa1-land1 TO gs_output-land1,
            gs_lfa1-name1 TO gs_output-name1.

*  Tax ID Number for SW & UG
      IF     ( gs_lfa1-stcd1             IS NOT INITIAL ) AND
             ( gs_lfa1-stcd1             CN ' 9-'       ).
        MOVE   gs_lfa1-stcd1             TO gs_output-stcd1.
      ENDIF.

      READ TABLE gt_t077y INTO gs_t077y WITH KEY ktokk = gs_lfa1-ktokk
                                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        CONCATENATE gs_t077y-ktokk gs_t077y-txt30 INTO gs_output-ktokk
                                                  SEPARATED BY '-'.
      ENDIF.
    ENDIF.

* To populate Dept.
    IF gs_knvk-abtnr EQ gc_0002.
      gs_output-dept = lc_sc.
    ELSEIF gs_knvk-abtnr EQ gc_0009.
      gs_output-dept = lc_ap.
    ENDIF.

* To Populate Function
    IF gs_knvk-pafkt EQ gc_12.
      gs_output-func = text-v01. " General PO & SRO
    ELSEIF gs_knvk-pafkt EQ gc_13.
      gs_output-func = text-v02. " Pymt Remit. Advice
    ENDIF.

    READ TABLE gt_adr2 INTO gs_adr2 WITH KEY addrnumber = gs_lfa1-adrnr
                                             persnumber = gs_knvk-prsnr
                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE: gs_adr2-tel_number TO gs_output-tel_number.

      READ TABLE gt_t005t INTO gs_t005t WITH KEY land1 = gs_adr2-country
                                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE gs_t005t-landx TO gs_output-landx.
      ENDIF.

    ENDIF.

    READ TABLE gt_adr6 INTO gs_adr6 WITH KEY addrnumber = gs_lfa1-adrnr
                                             persnumber = gs_knvk-prsnr
                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE: gs_adr6-smtp_addr TO gs_output-smtp_addr.
    ENDIF.


    READ TABLE gt_adrp INTO gs_adrp WITH KEY persnumber = gs_knvk-prsnr
                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE: gs_adrp-name_first TO gs_output-name_first,
            gs_adrp-name_last  TO gs_output-name_last.

      READ TABLE gt_t002t INTO gs_t002t WITH KEY sprsl = gs_adrp-langu
                                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE: gs_t002t-sptxt TO gs_output-sptxt.
      ENDIF.
    ENDIF.

    gs_output-erpid = p_erpid.
    APPEND gs_output TO gt_output.

  ENDLOOP.

* Move the data though there is no contact data available
  LOOP AT gt_lfa1 INTO gs_lfa1.

    CLEAR: gs_output,
           gs_t077y.

    READ TABLE gt_knvk WITH KEY lifnr = gs_lfa1-lifnr
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.

      MOVE: gs_lfa1-lifnr TO gs_output-lifnr,
            gs_lfa1-land1 TO gs_output-land1,
            gs_lfa1-name1 TO gs_output-name1.

*  Tax ID Number for SW & UG
      IF     ( gs_lfa1-stcd1             IS NOT INITIAL ) AND
             ( gs_lfa1-stcd1             CN ' 9-'       ).
        MOVE   gs_lfa1-stcd1             TO gs_output-stcd1.
      ENDIF.

      READ TABLE gt_t077y INTO gs_t077y WITH KEY ktokk = gs_lfa1-ktokk
                                                 BINARY SEARCH.
      IF sy-subrc EQ 0.
        CONCATENATE gs_t077y-ktokk gs_t077y-txt30 INTO gs_output-ktokk
                                                  SEPARATED BY '-'.
      ENDIF.

      gs_output-erpid = p_erpid.
      APPEND gs_output TO gt_output.

    ENDIF.
  ENDLOOP.

  SORT gt_output by lifnr.

  IF NOT gt_output[] IS INITIAL.
    CLEAR: gs_error,
           lv_cnt.
    gs_error-err_txt  = 'Total number of records updated'(002).
    lv_cnt = lines( gt_output ).
    CONCATENATE gs_error-err_txt lv_cnt INTO gs_error-err_txt SEPARATED BY space.
    APPEND gs_error TO gt_error.
  ENDIF.
ENDFORM.                    " F_FILL_DATA
