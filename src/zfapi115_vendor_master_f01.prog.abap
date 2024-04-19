*&---------------------------------------------------------------------*
*& Report  ZFAPI115_VENDOR_MASTER_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI115_VENDOR_MASTER                        *
* Include            :   ZFAPI115_VENDOR_MASTER_F01                    *
* Author             :   Vijay Rajaputra                               *
* Date               :   03-Mar-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Vendor Master Extraction for IAP               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 03-Mar-2018  VRAJAPUTRA  D30K928789 CHG0105961 - Initial development *
*                          D30K928848                                  *
*&---------------------------------------------------------------------*

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
*&      Form  f_get_f4_help_file_path1
*&---------------------------------------------------------------------*
*       Help dialog to retrieve the presentation server filename
*----------------------------------------------------------------------*
FORM f_get_f4_help_file_path1.

  DATA: lv_repid TYPE syrepid,
        lv_dynnr TYPE sydynnr,
        lv_file  TYPE localfile.

  lv_repid = sy-repid.
  lv_dynnr = sy-dynnr.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = lv_repid
      dynpro_number = lv_dynnr
    CHANGING
      file_name     = lv_file
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF     ( sy-subrc NE 0 ).
    MESSAGE ID sy-msgid
          TYPE sy-msgty
        NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE     lv_file TO p_path1.

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

  CLEAR    gt_lifnr[].
  CLEAR    gt_lfa1_key[].
  CLEAR    gt_lfa1[].
  CLEAR    gt_lfbk_key[].
  CLEAR    gt_lfbk[].
  CLEAR    gt_tbchain21_key[].
  CLEAR    gt_tbchain21[].
  CLEAR    gt_lfb1_key[].
  CLEAR    gt_lfb1[].
  CLEAR    gt_lfza_key[].
  CLEAR    gt_lfza[].
  CLEAR    gt_lfbw_key[].
  CLEAR    gt_lfbw[].
  CLEAR    gt_lfm1_key[].
  CLEAR    gt_lfm1[].
  CLEAR    gt_wyt3_key[].
  CLEAR    gt_wyt3[].
  CLEAR    gt_knvk[].
  CLEAR    gt_adrc[].
  CLEAR    gt_adr3[].
  CLEAR    gt_adr2[].
  CLEAR    gt_adr6[].

  CLEAR    gv_cn_recs_01.
  CLEAR    gv_cn_recs_02.
  CLEAR    gv_cn_recs_03.
  CLEAR    gv_cn_recs_04.
  CLEAR    gv_cn_recs_05.
  CLEAR    gv_cn_recs_06.
  CLEAR    gv_cn_recs_07.
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
  IF       ( p_appl       IS NOT INITIAL ).
    PERFORM  f_open_dataset.
  ELSEIF   ( p_pres       IS NOT INITIAL ).
    PERFORM  f_create_header_rec.
  ENDIF.

* Perform a full or delta extract
  IF       ( p_xfull      IS NOT INITIAL ).
    PERFORM  f_get_full_extract.
  ELSEIF   ( p_xdelta     IS NOT INITIAL ).
    PERFORM  f_get_delta_extract.
  ENDIF.

* Close the application server dataset
  IF       ( p_appl       IS NOT INITIAL ).
    IF     ( gv_filename  IS NOT INITIAL ).
      CLOSE  DATASET gv_filename.
    ENDIF.
  ENDIF.

* Download the output data
  IF       ( p_pres       IS NOT INITIAL ).
    PERFORM  f_download_output.
  ENDIF.

*eject
* Write the record counts to the spool
  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_01                 TO lv_cn_recs.
  WRITE:  /001 text-051, 016 text-061,  035 lv_cn_recs.

  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_02                 TO lv_cn_recs.
  WRITE:  /001 text-052, 016 text-061,  035 lv_cn_recs.

  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_03                 TO lv_cn_recs.
  WRITE:  /001 text-053, 016 text-061,  035 lv_cn_recs.

  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_04                 TO lv_cn_recs.
  WRITE:  /001 text-054, 016 text-061,  035 lv_cn_recs.

  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_05                 TO lv_cn_recs.
  WRITE:  /001 text-055, 016 text-061,  035 lv_cn_recs.

  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_06                 TO lv_cn_recs.
  WRITE:  /001 text-056, 016 text-061,  035 lv_cn_recs.

  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_07                 TO lv_cn_recs.
  WRITE:  /001 text-057, 016 text-061,  035 lv_cn_recs.

  SKIP     1.

  CLEAR                                     lv_cn_recs.
  WRITE    gv_cn_recs_total              TO lv_cn_recs.
  WRITE:  /001 text-062,                035 lv_cn_recs.

ENDFORM.                    " f_process_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_records
*&---------------------------------------------------------------------*
*       Process the records
*----------------------------------------------------------------------*
FORM f_process_records
  TABLES   it_lfa1_key                 TYPE gtt_lfa1_key.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_tabix_01_fr              TYPE sytabix,
           lv_tabix_01_to              TYPE sytabix,
           lv_tabix_02_fr              TYPE sytabix,
           lv_tabix_02_to              TYPE sytabix,
           lv_tabix_03_fr              TYPE sytabix,
           lv_tabix_03_to              TYPE sytabix,
           lv_tabix_04_fr              TYPE sytabix,
           lv_tabix_04_to              TYPE sytabix,
           lv_tabix_05_fr              TYPE sytabix,
           lv_tabix_05_to              TYPE sytabix,
           lv_tabix_06_fr              TYPE sytabix,
           lv_tabix_06_to              TYPE sytabix,
           lv_tabix_07_fr              TYPE sytabix,
           lv_tabix_07_to              TYPE sytabix.

* Process records in order by vendor number
  lv_tabix_01_fr = 1.
  lv_tabix_01_to = LINES( gt_lfa1 ).
  lv_tabix_02_fr = 1.
  lv_tabix_02_to = LINES( gt_lfbk ).
  lv_tabix_03_fr = 1.
  lv_tabix_03_to = LINES( gt_lfb1 ).
  lv_tabix_04_fr = 1.
  lv_tabix_04_to = LINES( gt_lfza ).
  lv_tabix_05_fr = 1.
  lv_tabix_05_to = LINES( gt_lfbw ).
  lv_tabix_06_fr = 1.
  lv_tabix_06_to = LINES( gt_lfm1 ).
  lv_tabix_07_fr = 1.
  lv_tabix_07_to = LINES( gt_wyt3 ).

  CLEAR                                     gs_lfa1_key.
  LOOP AT  it_lfa1_key                 INTO gs_lfa1_key.

    IF   ( ( gs_lfa1_key-lifnr           IN s_lifnr ) AND
           ( gs_lfa1_key-ktokk           IN s_ktokk )     ).
    ELSE.
      CONTINUE.
    ENDIF.

*eject
* 01 - vendor legal entity
    IF   ( gt_lfa1[] IS NOT INITIAL ) AND ( cb_file1 IS NOT INITIAL ).

      LOOP AT  gt_lfa1                 INTO gs_lfa1
                                       FROM lv_tabix_01_fr
                                         TO lv_tabix_01_to.
        lv_tabix = sy-tabix.

        IF     ( gs_lfa1-lifnr           LT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfa1.
          CONTINUE.
        ELSEIF ( gs_lfa1-lifnr           GT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfa1.
          EXIT.
        ENDIF.

        PERFORM  f_prep_rec_01.

        CLEAR  gs_lfa1.
      ENDLOOP.

      lv_tabix_01_fr = lv_tabix.

    ENDIF.

* 02 - vendor remittance account
    IF   ( gt_lfbk[] IS NOT INITIAL ) AND ( cb_file2 IS NOT INITIAL ).

      LOOP AT  gt_lfbk                 INTO gs_lfbk
                                       FROM lv_tabix_02_fr
                                         TO lv_tabix_02_to.
        lv_tabix = sy-tabix.

        IF     ( gs_lfbk-lifnr           LT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfbk.
          CONTINUE.
        ELSEIF ( gs_lfbk-lifnr           GT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfbk.
          EXIT.
        ENDIF.

        PERFORM  f_prep_rec_02.

        CLEAR  gs_lfbk.
      ENDLOOP.

      lv_tabix_02_fr = lv_tabix.

    ENDIF.

*eject
* 03 - vendor company
    IF   ( gt_lfb1[] IS NOT INITIAL ) AND ( cb_file3 IS NOT INITIAL ).

      LOOP AT  gt_lfb1                 INTO gs_lfb1
                                       FROM lv_tabix_03_fr
                                         TO lv_tabix_03_to.
        lv_tabix = sy-tabix.

        IF     ( gs_lfb1-lifnr           LT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfb1.
          CONTINUE.
        ELSEIF ( gs_lfb1-lifnr           GT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfb1.
          EXIT.
        ENDIF.

        IF       ( gs_lfb1-bukrs         IN s_bukrs ).
          PERFORM  f_prep_rec_03.
        ENDIF.

        CLEAR  gs_lfb1.
      ENDLOOP.

      lv_tabix_03_fr = lv_tabix.

    ENDIF.

* 04 - vendor permitted alternate payee
    IF   ( gt_lfza[] IS NOT INITIAL ) AND ( cb_file4 IS NOT INITIAL ).

      LOOP AT  gt_lfza                 INTO gs_lfza
                                       FROM lv_tabix_04_fr
                                         TO lv_tabix_04_to.
        lv_tabix = sy-tabix.

        IF     ( gs_lfza-lifnr           LT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfza.
          CONTINUE.
        ELSEIF ( gs_lfza-lifnr           GT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfza.
          EXIT.
        ENDIF.

        IF     ( ( gs_lfza-bukrs         IS INITIAL ) OR
                 ( gs_lfza-bukrs         IN s_bukrs )    ).
          PERFORM  f_prep_rec_04.
        ENDIF.

        CLEAR  gs_lfza.
      ENDLOOP.

      lv_tabix_04_fr = lv_tabix.

    ENDIF.

*eject
* 05 - vendor withholding tax
    IF   ( gt_lfbw[] IS NOT INITIAL ) AND ( cb_file5 IS NOT INITIAL ).

      LOOP AT  gt_lfbw                 INTO gs_lfbw
                                       FROM lv_tabix_05_fr
                                         TO lv_tabix_05_to.
        lv_tabix = sy-tabix.

        IF     ( gs_lfbw-lifnr           LT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfbw.
          CONTINUE.
        ELSEIF ( gs_lfbw-lifnr           GT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfbw.
          EXIT.
        ENDIF.

        IF       ( gs_lfbw-bukrs         IN s_bukrs ).
          PERFORM  f_prep_rec_05.
        ENDIF.

        CLEAR  gs_lfbw.
      ENDLOOP.

      lv_tabix_05_fr = lv_tabix.

    ENDIF.

* 06 - vendor purchasing organization
    IF   ( gt_lfm1[] IS NOT INITIAL ) AND ( cb_file6 IS NOT INITIAL ).

      LOOP AT  gt_lfm1                 INTO gs_lfm1
                                       FROM lv_tabix_06_fr
                                         TO lv_tabix_06_to.
        lv_tabix = sy-tabix.

        IF     ( gs_lfm1-lifnr           LT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfm1.
          CONTINUE.
        ELSEIF ( gs_lfm1-lifnr           GT gs_lfa1_key-lifnr ).
          CLEAR  gs_lfm1.
          EXIT.
        ENDIF.

        IF       ( gs_lfm1-ekorg         IN s_ekorg ).
          PERFORM  f_prep_rec_06.
        ENDIF.

        CLEAR  gs_lfm1.
      ENDLOOP.

      lv_tabix_06_fr = lv_tabix.

    ENDIF.

*eject
    IF   ( gt_wyt3[] IS NOT INITIAL ) AND ( cb_file7 IS NOT INITIAL ).

* 07 - vendor partner
      LOOP AT  gt_wyt3                 INTO gs_wyt3
                                       FROM lv_tabix_07_fr
                                         TO lv_tabix_07_to.
        lv_tabix = sy-tabix.

        IF     ( gs_wyt3-lifnr           LT gs_lfa1_key-lifnr ).
          CLEAR  gs_wyt3.
          CONTINUE.
        ELSEIF ( gs_wyt3-lifnr           GT gs_lfa1_key-lifnr ).
          CLEAR  gs_wyt3.
          EXIT.
        ENDIF.

        IF       ( gs_wyt3-ekorg         IN s_ekorg ).
          PERFORM  f_prep_rec_07.
        ENDIF.

        CLEAR  gs_wyt3.
      ENDLOOP.

      lv_tabix_07_fr = lv_tabix.

    ENDIF.

    IF     ( p_appl IS NOT INITIAL ).

      PERFORM  f_transfer_data.

      CLEAR    gt_output[].

    ENDIF.

    CLEAR  gs_lfa1_key.
  ENDLOOP.

ENDFORM.                    " f_process_records
*eject
*&---------------------------------------------------------------------*
*&      Form  f_prep_rec_01
*&---------------------------------------------------------------------*
*       Prepare the lfa1 data for output - record type 01
*----------------------------------------------------------------------*
FORM f_prep_rec_01.

  DATA:    ls_data_comp                TYPE gty_data_comp,
           ls_output                   TYPE gty_output.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_tabix_p                  TYPE sytabix,
           lv_tabix_ap_1               TYPE sytabix,
           lv_tabix_ap_2               TYPE sytabix,
           lv_tabix_sc                 TYPE sytabix,
           lv_parnr_ap_1               TYPE parnr,
           lv_parnr_ap_2               TYPE parnr,
           lv_parnr_sc                 TYPE parnr,
           lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_knvk>             TYPE gty_knvk,
                 <fs_data_comp>        TYPE gty_data_comp,
                 <fs_attr>             TYPE ANY.

*eject
* Read the vendor contacts
  CLEAR    lv_tabix_ap_1.
  CLEAR    lv_tabix_ap_2.
  CLEAR    lv_tabix_sc.
  CLEAR    lv_parnr_ap_1.
  CLEAR    lv_parnr_ap_2.
  CLEAR    lv_parnr_sc.

  READ     TABLE gt_knvk          ASSIGNING <fs_knvk>
                                    WITH KEY lifnr = gs_lfa1-lifnr
                               BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.
  IF     ( lv_subrc EQ 0 ).

    LOOP AT  gt_knvk              ASSIGNING <fs_knvk>
                                       FROM lv_tabix.
      lv_tabix_p = sy-tabix.

      IF     ( <fs_knvk>-lifnr           NE gs_lfa1-lifnr ).
        EXIT.
      ENDIF.

      IF       ( ( <fs_knvk>-abtnr       EQ '0009' ) AND
                 ( <fs_knvk>-pafkt       EQ '13'   )     ).
        IF       ( lv_parnr_ap_1         IS INITIAL ).
          IF   ( ( <fs_knvk>-namev       CS 'REMIT' ) OR
                 ( <fs_knvk>-name1       CS 'REMIT' )    ).
            MOVE   <fs_knvk>-parnr       TO lv_parnr_ap_1.
            MOVE   lv_tabix_p            TO lv_tabix_ap_1.
          ENDIF.
        ENDIF.
        IF       ( lv_parnr_ap_2         IS INITIAL ).
          MOVE   <fs_knvk>-parnr         TO lv_parnr_ap_2.
          MOVE   lv_tabix_p              TO lv_tabix_ap_2.
        ENDIF.
      ENDIF.

      IF       ( ( <fs_knvk>-abtnr       EQ '0002' ) AND
                 ( <fs_knvk>-pafkt       EQ '12'   )     ).
        IF       ( lv_parnr_sc           IS INITIAL ).
          MOVE   <fs_knvk>-parnr         TO lv_parnr_sc.
          MOVE   lv_tabix_p              TO lv_tabix_sc.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDIF.

*eject
  CLEAR                                     ls_data_comp.
*  MOVE     gs_lfa1_key-zz_iap_ritm_id    TO ls_data_comp-iap_ritm_id.
  MOVE     p_erpid                       TO ls_data_comp-erp_id.
  MOVE     '01'                          TO ls_data_comp-rec_type.
*  MOVE     gs_lfa1_key-zz_iap_vendor_id  TO ls_data_comp-iap_vendor_id.
  MOVE     gs_lfa1-lifnr                 TO ls_data_comp-lifnr.
  MOVE     gs_lfa1-name1                 TO ls_data_comp-name1.
  MOVE     gs_lfa1-name2                 TO ls_data_comp-name2.
  MOVE     gs_lfa1-mcod1                 TO ls_data_comp-mcod1.
  MOVE     gs_lfa1-mcod2                 TO ls_data_comp-mcod2.
  MOVE     gs_lfa1-ktokk                 TO ls_data_comp-ktokk.
  MOVE     gs_lfa1-begru                 TO ls_data_comp-begru.
  MOVE     gs_lfa1-konzs                 TO ls_data_comp-konzs.
  MOVE     gs_lfa1-bahns                 TO ls_data_comp-bahns.
  MOVE     gs_lfa1-land1                 TO ls_data_comp-land1.

* Vendor contact 1 - AP
  CLEAR    gs_knvk.
  CLEAR    gs_adr3.
  CLEAR    gs_adr6.

  IF     ( lv_parnr_ap_1                 IS NOT INITIAL ).
    READ     TABLE gt_knvk             INTO gs_knvk
                                      INDEX lv_tabix_ap_1.
  ELSEIF ( lv_parnr_ap_2                 IS NOT INITIAL ).
    READ     TABLE gt_knvk             INTO gs_knvk
                                      INDEX lv_tabix_ap_2.
  ENDIF.

  IF     ( gs_knvk-parnr                 IS NOT INITIAL ).

    READ     TABLE gt_adr3             INTO gs_adr3
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                                            persnumber = gs_knvk-prsnr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_adr3.
    ENDIF.

    READ     TABLE gt_adr6             INTO gs_adr6
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                                            persnumber = gs_knvk-prsnr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_adr6.
    ENDIF.

  ENDIF.

*eject
  MOVE     gs_knvk-parnr                 TO ls_data_comp-parnr_ap.
  MOVE     gs_knvk-name1                 TO ls_data_comp-name1_ap.
  MOVE     gs_knvk-namev                 TO ls_data_comp-namev_ap.
  MOVE     gs_knvk-abtnr                 TO ls_data_comp-abtnr_ap.
  MOVE     gs_knvk-pafkt                 TO ls_data_comp-pafkt_ap.
  MOVE     gs_knvk-telf1                 TO ls_data_comp-telf1_ap.
  MOVE     gs_adr3-fax_number            TO ls_data_comp-fax_ap.
  MOVE     gs_adr6-smtp_addr             TO ls_data_comp-email_ap.

* Vendor contact 2 - SC
  CLEAR    gs_knvk.
  CLEAR    gs_adr3.
  CLEAR    gs_adr6.

  IF     ( lv_parnr_sc                   IS NOT INITIAL ).
    READ     TABLE gt_knvk             INTO gs_knvk
                                      INDEX lv_tabix_sc.
  ENDIF.

  IF     ( gs_knvk-parnr                 IS NOT INITIAL ).

    READ     TABLE gt_adr3             INTO gs_adr3
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                                            persnumber = gs_knvk-prsnr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_adr3.
    ENDIF.

    READ     TABLE gt_adr6             INTO gs_adr6
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                                            persnumber = gs_knvk-prsnr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_adr6.
    ENDIF.

  ENDIF.

  MOVE     gs_knvk-parnr                 TO ls_data_comp-parnr_sc.
  MOVE     gs_knvk-name1                 TO ls_data_comp-name1_sc.
  MOVE     gs_knvk-namev                 TO ls_data_comp-namev_sc.
  MOVE     gs_knvk-abtnr                 TO ls_data_comp-abtnr_sc.
  MOVE     gs_knvk-pafkt                 TO ls_data_comp-pafkt_sc.
  MOVE     gs_knvk-telf1                 TO ls_data_comp-telf1_sc.
  MOVE     gs_adr3-fax_number            TO ls_data_comp-fax_sc.
  MOVE     gs_adr6-smtp_addr             TO ls_data_comp-email_sc.

*eject
  CLEAR                                     gs_adr2.
  READ     TABLE gt_adr2               INTO gs_adr2
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                                            persnumber = space
                              BINARY SEARCH.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gs_adr2.
  ENDIF.

  CLEAR                                     gs_adr3.
  READ     TABLE gt_adr3               INTO gs_adr3
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                                            persnumber = space
                              BINARY SEARCH.
  IF     ( sy-subrc NE 0 ).
    CLEAR  gs_adr3.
  ENDIF.

  MOVE     gs_adr2-tel_number            TO ls_data_comp-telf1.
  MOVE     gs_adr2-tel_extens            TO ls_data_comp-extn_tel.
  MOVE     gs_adr3-fax_number            TO ls_data_comp-telfx.
  MOVE     gs_adr3-fax_extens            TO ls_data_comp-extn_fax.
  MOVE     gs_lfa1-stcd1                 TO ls_data_comp-stcd1.
  MOVE     gs_lfa1-stcd2                 TO ls_data_comp-stcd2.
  IF     ( gs_lfa1-loevm                 IS NOT INITIAL ).
    MOVE   abap_true                     TO ls_data_comp-loevm.
  ENDIF.
  IF     ( gs_lfa1-sperr                 IS NOT INITIAL ).
    MOVE   abap_true                     TO ls_data_comp-sperr.
  ENDIF.
  IF     ( gs_lfa1-sperm                 IS NOT INITIAL ).
    MOVE   abap_true                     TO ls_data_comp-sperm.
  ENDIF.
  MOVE     gs_lfa1-sperz                 TO ls_data_comp-sperz.

*eject
* Read the vendor address
  CLEAR    gs_adrc.

  IF     ( gs_lfa1-adrnr                 IS NOT INITIAL ).

    READ     TABLE gt_adrc             INTO gs_adrc
                                   WITH KEY addrnumber = gs_lfa1-adrnr
                              BINARY SEARCH.
    IF     ( sy-subrc NE 0 ).
      CLEAR  gs_adrc.
    ENDIF.

  ENDIF.

  MOVE     gs_adrc-name1                 TO ls_data_comp-name1_adrc.
  MOVE     gs_adrc-street                TO ls_data_comp-street.
  MOVE     gs_adrc-str_suppl2            TO ls_data_comp-str_suppl2.
  MOVE     gs_adrc-city1                 TO ls_data_comp-city1.
  MOVE     gs_adrc-region                TO ls_data_comp-region.
  MOVE     gs_adrc-post_code1            TO ls_data_comp-post_code1.
  MOVE     gs_adrc-country               TO ls_data_comp-country.
  MOVE     gs_lfa1-lnrza                 TO ls_data_comp-lnrza.
  MOVE     gs_lfa1-xlfza                 TO ls_data_comp-xlfza.

  PERFORM  f_append_output            USING ls_data_comp.
  ADD      1                             TO gv_cn_recs_01.

ENDFORM.                    " f_prep_rec_01
*eject
*&---------------------------------------------------------------------*
*&      Form  f_prep_rec_02
*&---------------------------------------------------------------------*
*       Prepare the lfbk data for output - record type 02
*----------------------------------------------------------------------*
FORM f_prep_rec_02.

  DATA:    ls_data_comp                TYPE gty_data_comp,
           ls_output                   TYPE gty_output.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_tbchain21>        TYPE gty_tbchain21,
                 <fs_data_comp>        TYPE gty_data_comp,
                 <fs_attr>             TYPE ANY.

  CLEAR                                     ls_data_comp.
*  MOVE     gs_lfa1_key-zz_iap_ritm_id    TO ls_data_comp-iap_ritm_id.
  MOVE     p_erpid                       TO ls_data_comp-erp_id.
  MOVE     '02'                          TO ls_data_comp-rec_type.
*  MOVE     gs_lfa1_key-zz_iap_vendor_id  TO ls_data_comp-iap_vendor_id.
  MOVE     gs_lfbk-lifnr                 TO ls_data_comp-lifnr.
  MOVE     gs_lfbk-bvtyp                 TO ls_data_comp-bvtyp.
  MOVE     gs_lfbk-banks                 TO ls_data_comp-banks.
  MOVE     gs_lfbk-bankl                 TO ls_data_comp-bankl.
  MOVE     gs_lfbk-bankn                 TO ls_data_comp-bankn.
  MOVE     gs_lfbk-bkref                 TO ls_data_comp-bkref.
  MOVE     gs_lfbk-bkont                 TO ls_data_comp-bkont.

  IF   ( ( gs_lfbk-chngind               EQ 'D' ) OR
         ( gs_lfbk-chngind               EQ 'E' )    ).
    MOVE   'I'                           TO ls_data_comp-chngind_lfbk.
  ELSE.
    MOVE   'A'                           TO ls_data_comp-chngind_lfbk.
  ENDIF.

*eject
* Read the bank chain
  READ     TABLE gt_tbchain21     ASSIGNING <fs_tbchain21>
                                   WITH KEY mandt    = sy-mandt
                                            banksrec = gs_lfbk-banks
                                            bankkrec = gs_lfbk-bankl
                                            banknrec = gs_lfbk-bankn
                              BINARY SEARCH.
  lv_subrc = sy-subrc.
  lv_tabix = sy-tabix.

  IF     ( lv_subrc EQ 0 ).

    LOOP AT  gt_tbchain21         ASSIGNING <fs_tbchain21>
                                       FROM lv_tabix.

      IF ( ( <fs_tbchain21>-banksrec     NE gs_lfbk-banks ) OR
           ( <fs_tbchain21>-bankkrec     NE gs_lfbk-bankl ) OR
           ( <fs_tbchain21>-banknrec     NE gs_lfbk-bankn )    ).
        EXIT.
      ENDIF.

      CLEAR                                 ls_data_comp-waers_lfbk.
      MOVE   <fs_tbchain21>-waers        TO ls_data_comp-waers_lfbk.
      CLEAR                                 ls_data_comp-bankssnd.
      MOVE   <fs_tbchain21>-bankssnd     TO ls_data_comp-bankssnd.
      CLEAR                                 ls_data_comp-bankksnd.
      MOVE   <fs_tbchain21>-bankksnd     TO ls_data_comp-bankksnd.
      CLEAR                                 ls_data_comp-chainno.
      MOVE   <fs_tbchain21>-chainno      TO ls_data_comp-chainno.
      CLEAR                                 ls_data_comp-chainbankt.
      MOVE   <fs_tbchain21>-chainbankt   TO ls_data_comp-chainbankt.
      CLEAR                                 ls_data_comp-chainbanks.
      MOVE   <fs_tbchain21>-chainbanks   TO ls_data_comp-chainbanks.
      CLEAR                                 ls_data_comp-chainbankk.
      MOVE   <fs_tbchain21>-chainbankk   TO ls_data_comp-chainbankk.
      CLEAR                                 ls_data_comp-chainbankn.
      MOVE   <fs_tbchain21>-chainbankn   TO ls_data_comp-chainbankn.

      PERFORM  f_append_output        USING ls_data_comp.
      ADD      1                         TO gv_cn_recs_02.

    ENDLOOP.

  ELSE.

    PERFORM  f_append_output          USING ls_data_comp.
    ADD      1                           TO gv_cn_recs_02.

  ENDIF.

ENDFORM.                    " f_prep_rec_02
*eject
*&---------------------------------------------------------------------*
*&      Form  f_prep_rec_03
*&---------------------------------------------------------------------*
*       Prepare the lfb1 data for output - record type 03
*----------------------------------------------------------------------*
FORM f_prep_rec_03.

  DATA:    ls_data_comp                TYPE gty_data_comp,
           ls_output                   TYPE gty_output.

  DATA:    lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_data_comp>        TYPE gty_data_comp,
                 <fs_attr>             TYPE ANY.

  CLEAR                                     ls_data_comp.
*  MOVE     gs_lfa1_key-zz_iap_ritm_id    TO ls_data_comp-iap_ritm_id.
  MOVE     p_erpid                       TO ls_data_comp-erp_id.
  MOVE     '03'                          TO ls_data_comp-rec_type.
*  MOVE     gs_lfa1_key-zz_iap_vendor_id  TO ls_data_comp-iap_vendor_id.
  MOVE     gs_lfb1-lifnr                 TO ls_data_comp-lifnr.
  MOVE     gs_lfb1-bukrs                 TO ls_data_comp-bukrs.
  MOVE     gs_lfb1-zterm                 TO ls_data_comp-zterm_lfb1.
  MOVE     gs_lfb1-zwels                 TO ls_data_comp-zwels.
  IF     ( gs_lfb1-loevm                 IS NOT INITIAL ).
    MOVE   abap_true                     TO ls_data_comp-loevm_lfb1.
  ENDIF.
  IF     ( gs_lfb1-sperr                 IS NOT INITIAL ).
    MOVE   abap_true                     TO ls_data_comp-sperr_lfb1.
  ENDIF.
  MOVE     gs_lfb1-zahls                 TO ls_data_comp-zahls.
  MOVE     gs_lfb1-busab                 TO ls_data_comp-busab.
  MOVE     gs_lfb1-akont                 TO ls_data_comp-akont.
  MOVE     gs_lfb1-zuawa                 TO ls_data_comp-zuawa.
  MOVE     gs_lfb1-fdgrv                 TO ls_data_comp-fdgrv.
  MOVE     gs_lfb1-mindk                 TO ls_data_comp-mindk.
  MOVE     gs_lfb1-cerdt                 TO ls_data_comp-cerdt.
  MOVE     gs_lfb1-zindt                 TO ls_data_comp-zindt.
  MOVE     gs_lfb1-reprf                 TO ls_data_comp-reprf.
  MOVE     gs_lfb1-lnrzb                 TO ls_data_comp-lnrzb.
  MOVE     gs_lfb1-xlfzb                 TO ls_data_comp-xlfzb.
  MOVE     gs_lfb1-hbkid                 TO ls_data_comp-hbkid.
  MOVE     gs_lfb1-xpore                 TO ls_data_comp-xpore.
  MOVE     gs_lfb1-uzawe                 TO ls_data_comp-uzawe.
  MOVE     gs_lfb1-qland                 TO ls_data_comp-qland.

  PERFORM  f_append_output            USING ls_data_comp.
  ADD      1                             TO gv_cn_recs_03.

ENDFORM.                    " f_prep_rec_03
*eject
*&---------------------------------------------------------------------*
*&      Form  f_prep_rec_04
*&---------------------------------------------------------------------*
*       Prepare the lfza data for output - record type 04
*----------------------------------------------------------------------*
FORM f_prep_rec_04.

  DATA:    ls_data_comp                TYPE gty_data_comp,
           ls_output                   TYPE gty_output.

  DATA:    lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_data_comp>        TYPE gty_data_comp,
                 <fs_attr>             TYPE ANY.

  CLEAR                                     ls_data_comp.
*  MOVE     gs_lfa1_key-zz_iap_ritm_id    TO ls_data_comp-iap_ritm_id.
  MOVE     p_erpid                       TO ls_data_comp-erp_id.
  MOVE     '04'                          TO ls_data_comp-rec_type.
*  MOVE     gs_lfa1_key-zz_iap_vendor_id  TO ls_data_comp-iap_vendor_id.
  MOVE     gs_lfza-lifnr                 TO ls_data_comp-lifnr.
  MOVE     gs_lfza-bukrs                 TO ls_data_comp-bukrs.
  MOVE     gs_lfza-empfk                 TO ls_data_comp-empfk.

  IF   ( ( gs_lfza-chngind               EQ 'D' ) OR
         ( gs_lfza-chngind               EQ 'E' )    ).
    MOVE     'I'                         TO ls_data_comp-chngind_lfza.
  ELSE.
    MOVE     'A'                         TO ls_data_comp-chngind_lfza.
  ENDIF.

  PERFORM  f_append_output            USING ls_data_comp.
  ADD      1                             TO gv_cn_recs_04.

ENDFORM.                    " f_prep_rec_04
*eject
*&---------------------------------------------------------------------*
*&      Form  f_prep_rec_05
*&---------------------------------------------------------------------*
*       Prepare the lfbw data for output - record type 05
*----------------------------------------------------------------------*
FORM f_prep_rec_05.

  DATA:    ls_data_comp                TYPE gty_data_comp,
           ls_output                   TYPE gty_output.

  DATA:    lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_data_comp>        TYPE gty_data_comp,
                 <fs_attr>             TYPE ANY.

  CLEAR                                     ls_data_comp.
*  MOVE     gs_lfa1_key-zz_iap_ritm_id    TO ls_data_comp-iap_ritm_id.
  MOVE     p_erpid                       TO ls_data_comp-erp_id.
  MOVE     '05'                          TO ls_data_comp-rec_type.
*  MOVE     gs_lfa1_key-zz_iap_vendor_id  TO ls_data_comp-iap_vendor_id.
  MOVE     gs_lfbw-lifnr                 TO ls_data_comp-lifnr.
  MOVE     gs_lfbw-bukrs                 TO ls_data_comp-bukrs.
  MOVE     gs_lfbw-witht                 TO ls_data_comp-witht.
  MOVE     gs_lfbw-wt_withcd             TO ls_data_comp-wt_withcd.
  MOVE     gs_lfbw-wt_subjct             TO ls_data_comp-wt_subjct.
  MOVE     gs_lfbw-qsrec                 TO ls_data_comp-qsrec.
  MOVE     gs_lfbw-wt_wtstcd             TO ls_data_comp-wt_wtstcd.

  IF   ( ( gs_lfbw-chngind               EQ 'D' ) OR
         ( gs_lfbw-chngind               EQ 'E' )    ).
    MOVE     'I'                         TO ls_data_comp-chngind_lfbw.
  ELSE.
    MOVE     'A'                         TO ls_data_comp-chngind_lfbw.
  ENDIF.

  PERFORM  f_append_output            USING ls_data_comp.
  ADD      1                             TO gv_cn_recs_05.

ENDFORM.                    " f_prep_rec_05
*eject
*&---------------------------------------------------------------------*
*&      Form  f_prep_rec_06
*&---------------------------------------------------------------------*
*       Prepare the lfm1 data for output - record type 06
*----------------------------------------------------------------------*
FORM f_prep_rec_06.

  DATA:    ls_data_comp                TYPE gty_data_comp,
           ls_output                   TYPE gty_output.

  DATA:    lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_data_comp>        TYPE gty_data_comp,
                 <fs_attr>             TYPE ANY.

  CLEAR                                     ls_data_comp.
*  MOVE     gs_lfa1_key-zz_iap_ritm_id    TO ls_data_comp-iap_ritm_id.
  MOVE     p_erpid                       TO ls_data_comp-erp_id.
  MOVE     '06'                          TO ls_data_comp-rec_type.
*  MOVE     gs_lfa1_key-zz_iap_vendor_id  TO ls_data_comp-iap_vendor_id.
  MOVE     gs_lfm1-lifnr                 TO ls_data_comp-lifnr.
  MOVE     gs_lfm1-ekorg                 TO ls_data_comp-ekorg.
  IF     ( gs_lfm1-sperm                 IS NOT INITIAL ).
    MOVE   abap_true                     TO ls_data_comp-sperm_lfm1.
  ENDIF.
  IF     ( gs_lfm1-loevm                 IS NOT INITIAL ).
    MOVE   abap_true                     TO ls_data_comp-loevm_lfm1.
  ENDIF.
  MOVE     gs_lfm1-waers                 TO ls_data_comp-waers_lfm1.
  MOVE     gs_lfm1-zterm                 TO ls_data_comp-zterm_lfm1.
  MOVE     gs_lfm1-inco1                 TO ls_data_comp-inco1.
  MOVE     gs_lfm1-inco2                 TO ls_data_comp-inco2.
  MOVE     gs_lfm1-kalsk                 TO ls_data_comp-kalsk.
  MOVE     gs_lfm1-meprf                 TO ls_data_comp-meprf.
  MOVE     gs_lfm1-eikto                 TO ls_data_comp-eikto.
  MOVE     gs_lfm1-webre                 TO ls_data_comp-webre.
  MOVE     gs_lfm1-kzaut                 TO ls_data_comp-kzaut.

  PERFORM  f_append_output            USING ls_data_comp.
  ADD      1                             TO gv_cn_recs_06.

ENDFORM.                    " f_prep_rec_06
*eject
*&---------------------------------------------------------------------*
*&      Form  f_prep_rec_07
*&---------------------------------------------------------------------*
*       Prepare the wyt3 data for output - record type 07
*----------------------------------------------------------------------*
FORM f_prep_rec_07.

  DATA:    ls_data_comp                TYPE gty_data_comp,
           ls_output                   TYPE gty_output.

  DATA:    lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_data_comp>        TYPE gty_data_comp,
                 <fs_attr>             TYPE ANY.

  CLEAR                                     ls_data_comp.
*  MOVE     gs_lfa1_key-zz_iap_ritm_id    TO ls_data_comp-iap_ritm_id.
  MOVE     p_erpid                       TO ls_data_comp-erp_id.
  MOVE     '07'                          TO ls_data_comp-rec_type.
*  MOVE     gs_lfa1_key-zz_iap_vendor_id  TO ls_data_comp-iap_vendor_id.
  MOVE     gs_wyt3-lifnr                 TO ls_data_comp-lifnr.
  MOVE     gs_wyt3-ekorg                 TO ls_data_comp-ekorg.
  MOVE     gs_wyt3-parvw                 TO ls_data_comp-parvw.
  MOVE     gs_wyt3-parza                 TO ls_data_comp-parza.
  MOVE     gs_wyt3-lifn2                 TO ls_data_comp-lifn2.
  MOVE     gs_wyt3-defpa                 TO ls_data_comp-defpa.

  IF   ( ( gs_wyt3-chngind               EQ 'D' ) OR
         ( gs_wyt3-chngind               EQ 'E' )    ).
    MOVE     'I'                         TO ls_data_comp-chngind_wyt3.
  ELSE.
    MOVE     'A'                         TO ls_data_comp-chngind_wyt3.
  ENDIF.

  PERFORM  f_append_output            USING ls_data_comp.
  ADD      1                             TO gv_cn_recs_07.

ENDFORM.                    " f_prep_rec_07
*eject
*&---------------------------------------------------------------------*
*&      Form  f_append_output
*&---------------------------------------------------------------------*
*       Append the output
*----------------------------------------------------------------------*
FORM f_append_output
  USING    is_data_comp                TYPE gty_data_comp.

  DATA:    ls_output                   TYPE gty_output.

  DATA:    lv_dtype                    TYPE c,
           lv_cn_comp                  TYPE i.

  FIELD-SYMBOLS: <fs_data_comp>        TYPE gty_data_comp,
                 <fs_attr>             TYPE ANY.

  ASSIGN          is_data_comp    TO <fs_data_comp>.

  DESCRIBE FIELD <fs_data_comp> TYPE lv_dtype COMPONENTS lv_cn_comp.

  CLEAR    ls_output.

  DO lv_cn_comp TIMES.

    ASSIGN COMPONENT sy-index OF STRUCTURE <fs_data_comp> TO <fs_attr>.
    IF     ( sy-index EQ 1 ).
      ls_output = <fs_attr>.
    ELSE.
      CONCATENATE ls_output <fs_attr>
             INTO ls_output SEPARATED BY gc_delim.
    ENDIF.

  ENDDO.

  APPEND   ls_output                     TO gt_output.
  ADD      1                             TO gv_cn_recs_total.

ENDFORM.                    " f_append_output
*eject
*&---------------------------------------------------------------------*
*&      Form  f_transfer_data
*&---------------------------------------------------------------------*
*       Transfer data to the application server
*----------------------------------------------------------------------*
FORM f_transfer_data.

  DATA:    lv_cn_recs                  TYPE i,
           lv_cn_recs_buff             TYPE i.

  FIELD-SYMBOLS: <fs_output>           TYPE gty_output.

  IF     ( p_appl IS INITIAL ).
    RETURN.
  ENDIF.

  IF     ( gt_output[] IS INITIAL ).
    RETURN.
  ENDIF.

* Check if the maximum number of records per file is exceeded
  lv_cn_recs_buff = LINES( gt_output ).

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

    LOOP AT       gt_output       ASSIGNING <fs_output>.
      TRANSFER   <fs_output>             TO gv_filename.
    ENDLOOP.

    ADD      lv_cn_recs_buff             TO gv_cn_recs_file.

  ENDIF.

ENDFORM.                    " f_transfer_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_open_dataset
*&---------------------------------------------------------------------*
*       Open dataset
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
  CONCATENATE text-a01  text-a02  text-a03  text-a04 "lfa1 - 01
              text-a05  text-a60
              text-a06  text-a07  text-a08
              text-a09  text-a10  text-a11  text-a12
              text-a13  text-a14  text-a15  text-a16
              text-a17  text-a18  text-a19  text-a20
              text-a21  text-a22  text-a23  text-a24
              text-a25  text-a26  text-a27  text-a28
              text-a29  text-a30  text-a31  text-a32
              text-a33  text-a34  text-a35  text-a36
              text-a37  text-a38  text-a39  text-a40
              text-a41  text-a42  text-a43  text-a44
              text-a45  text-a46  text-a47  text-a48
              text-a49
                        text-b02  text-b03  text-b04 "lfbk - 02
              text-b05  text-b06  text-b07  text-b08
              text-b09  text-b10  text-b11  text-b12
              text-b13  text-b14  text-b15  text-b16
                        text-c02  text-c03  text-c04 "lfb1 - 03
              text-c05  text-c06  text-c07  text-c08
              text-c09  text-c10  text-c11  text-c12
              text-c13  text-c14  text-c15  text-c16
              text-c17  text-c18  text-c19  text-c20
              text-c21
                                  text-d03  text-d04 "lfza - 04
                                  text-e03  text-e04 "lfbw - 05
              text-e05  text-e06  text-e07  text-e08
                        text-f02  text-f03  text-f04 "lfm1 - 06
              text-f05  text-f06  text-f07  text-f08
              text-f09  text-f10  text-f11  text-f12
              text-f13
                                  text-g03  text-g04 "wyt3 - 07
              text-g05  text-g06  text-g07
                                       INTO ls_output
                               SEPARATED BY gc_delim.

  IF       ( p_appl                      IS NOT INITIAL ).
    TRANSFER ls_output                   TO gv_filename.
  ELSEIF   ( p_pres                      IS NOT INITIAL ).
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
