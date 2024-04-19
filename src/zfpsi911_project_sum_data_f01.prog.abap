*&---------------------------------------------------------------------*
*&  Include           ZFPSI911_PROJECT_SUM_DATA_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFPSI911_PROJECT_SUM_DATA_F01                 *
*& Author             :  Kavya M B                                     *
*& Creation Date      :  13/09/2019                                    *
*& Object ID          :  I_P2C_PS_911                                  *
*& Application Area   :  FICO                                          *
*& Description        :  This interface is to send Project System      *
*                        master data and transactions data in a flat   *
*                        file to a dedicated location on Windows that  *
*                        to be picked up by Workato and delivered to   *
*                        destination system with SQL database.         *
*&-------------------------------------------------------------------- *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 18-10-2019   KMB         D30K930178  CHG0159956 PS PMO reporting     *
* 21-10-2019   KMB         D30K930220  CHG0159956 - performance issue  *
* 22-10-2019   KMB         D30K930222  CHG0159956 - FUT issue          *
* 20-11-2019   KMB         D30K930293  CHG0159956 - FUT issue          *
* 28-11-2019   KMB         D30K930313  CHG0159956 - FUT issue          *
* 29-11-2019   KMB         D30K930315  CHG0159956 - FUT issue          *
* 02-12-2019   KMB         D30K930317  CHG0159956 - FUT issue          *
* 03-12-2019   KMB         D30K930319  CHG0159956 - FUT issue          *
* 24-08-2020   AHMADT      D30K930659  CHG0189892 - MTD issue fising   *
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  F_GET_F4_HELP_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_f4_help_file_path.
  IF p_app = abap_true.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE' "Server path
      IMPORTING
        serverfile       = p_fpath1
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSEIF p_pre = abap_true.
*Browse the path from presentation server
    CALL FUNCTION 'F4_FILENAME'         "Allows user to select path/file
          EXPORTING
               program_name  = syst-repid
               dynpro_number = syst-dynnr
               field_name    = 'P_FPATH1'
          IMPORTING
               file_name     = p_fpath1.
  ENDIF.
ENDFORM.                    " F_GET_F4_HELP_FILE_PATH
*&---------------------------------------------------------------------*
*&      Form  F_FOR_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_for_get_data.

  SELECT pspnr posid objnr psphi stufe
       FROM prps
       INTO TABLE gt_f_prps
       FOR ALL ENTRIES IN gt_prps
       WHERE posid = gt_prps-posid.
  IF sy-subrc = 0 AND gt_f_prps IS NOT INITIAL.
    SELECT pspnr
      FROM proj
      INTO TABLE gt_f_proj
      FOR ALL ENTRIES IN gt_f_prps
      WHERE pspnr = gt_f_prps-psphi.
    IF sy-subrc <> 0.
      CLEAR gt_f_proj.
    ENDIF.

    "BOC by KMB 15-11-2019  CHG0159956 - FUT issue fixing
    CLEAR: lv_date, lv_cal_date, lv_high_year.

    CONCATENATE p_gjahr p_monat '01' INTO lv_date.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_date
        days      = '00'
        months    = '72'
        signum    = '+'
        years     = '00'
      IMPORTING
        calc_date = lv_cal_date.
    IF sy-subrc = 0.
      lv_high_year = lv_cal_date+0(4).
    ENDIF.
    "EOC by KMB 15-11-2019  CHG0159956 - FUT issue fixing

*BOC by KMB 28-11-2019  CHG0159956 - FUT issue fixing
*    SELECT objnr wrttp gjahr versn twaer wlp01 wlp02
    SELECT objnr wrttp gjahr versn twaer wlp00 wlp01 wlp02
*EOC by KMB 28-11-2019  CHG0159956 - FUT issue fixing
           wlp03 wlp04 wlp05 wlp06 wlp07 wlp08
           wlp09 wlp10 wlp11 wlp12 wlp13 wlp14
           wlp15 wlp16
    FROM rpsco
    INTO TABLE gt_f_rpsco
    FOR ALL ENTRIES IN gt_f_prps
    WHERE objnr = gt_f_prps-objnr AND
          wrttp = gc_1
      AND lednr = '0001' "Added by KMB on 02.12.2019 CHG0159956 FUT issue fixing
      AND gjahr >= p_gjahr AND gjahr <= lv_high_year. "Added by KMB 15-11-2019  CHG0159956 - FUT issue fixing
    IF sy-subrc = 0 AND gt_f_rpsco IS NOT INITIAL.

      SORT gt_f_rpsco ASCENDING BY objnr gjahr versn.

    ENDIF.
  ENDIF.
ENDFORM.                    " F_FOR_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_ALL_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_all_get_data .

  SELECT pspnr posid post1 objnr psphi vernr prart stufe
    FROM prps
    INTO TABLE gt_a_prps
    FOR ALL ENTRIES IN gt_prps
    WHERE posid = gt_prps-posid.
  IF sy-subrc = 0 AND gt_a_prps IS NOT INITIAL.

*BOC by KMB on 20.11.2019 CHG0159956 - FUT issue
*    SELECT pspnr pspid post1 objnr astnr vbukr plfaz plsez eprog
    SELECT pspnr pspid post1 objnr vbukr plfaz plsez eprog
*EOC by KMB on 20.11.2019 CHG0159956 - FUT issue
      FROM proj
      INTO TABLE gt_a_proj
      FOR ALL ENTRIES IN gt_a_prps
      WHERE pspnr = gt_a_prps-psphi.
    IF sy-subrc <> 0.
      CLEAR gt_a_proj.
    ENDIF.

    SELECT objnr stat inact
      FROM jest
      INTO TABLE gt_jest
      FOR ALL ENTRIES IN gt_a_prps
      WHERE objnr = gt_a_prps-objnr.
    IF sy-subrc <> 0.
      CLEAR gt_jest.
    ENDIF.

    SELECT prart pratx
      FROM tcj1t
      INTO TABLE gt_pratx
      FOR ALL ENTRIES IN gt_a_prps
      WHERE prart = gt_a_prps-prart AND
            langu = sy-langu.
    IF sy-subrc <> 0.
      CLEAR gt_pratx.
    ENDIF.

    SELECT vernr verna
      FROM tcj04
      INTO TABLE gt_tcj04
      FOR ALL ENTRIES IN gt_a_prps
      WHERE vernr = gt_a_prps-vernr.
    IF sy-subrc <> 0.
      CLEAR gt_tcj04.
    ENDIF.

    SELECT objnr wrttp gjahr versn twaer wlp00 wlp01 wlp02 wlp03 wlp04
           wlp05 wlp06 wlp07 wlp08 wlp09 wlp10
           wlp11 wlp12 wlp13 wlp14 wlp15 wlp16
      FROM rpsco
      INTO TABLE gt_a_rpsco
      FOR ALL ENTRIES IN gt_a_prps
      WHERE objnr = gt_a_prps-objnr AND
       lednr = '0001' AND "Added by KMB on 02.12.2019 CHG0159956 FUT issue fixing
            ( wrttp = gc_4 OR wrttp = gc_41 ).
    IF sy-subrc = 0 AND gt_a_rpsco IS NOT INITIAL.

      SORT gt_a_rpsco ASCENDING BY objnr gjahr versn.

    ENDIF.
  ENDIF.
ENDFORM.                    " F_ALL_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FOR_FORMAT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_for_format_data .
  DATA : lv_pspnr TYPE proj-pspid,
         lv_psphi TYPE proj-pspid.

*BOC by KMB 15-11-2019  CHG0159956 - FUT issue fixing
  DATA: lv_amount_1 TYPE string,
        lv_amount_2 TYPE string,
        lv_amount_3 TYPE string,
        lv_amount_4 TYPE string,
        lv_amount_5 TYPE string,
        lv_amount_6 TYPE string,
        lv_amount_7 TYPE string,
        lv_amount_8 TYPE string,
        lv_amount_9 TYPE string,
        lv_amount_10 TYPE string,
        lv_amount_11 TYPE string,
        lv_amount_12 TYPE string.

  CLEAR: lv_pspnr, gs_f_rpsco, gs_f_prps, gs_f_final.

  LOOP AT gt_f_rpsco INTO gs_f_rpsco.

    SORT gt_f_prps BY objnr.
    READ TABLE gt_f_prps INTO gs_f_prps WITH KEY objnr = gs_f_rpsco-objnr BINARY SEARCH.
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
        EXPORTING
          input  = gs_f_prps-psphi
        IMPORTING
          output = lv_psphi
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        CLEAR lv_psphi.
      ENDIF.

      gs_f_final-psphi = lv_psphi.
      gs_f_final-stufe = gs_f_prps-stufe.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
        EXPORTING
          input  = gs_f_prps-pspnr
        IMPORTING
          output = lv_pspnr
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        CLEAR lv_pspnr.
      ENDIF.

      gs_f_final-pspnr = lv_pspnr.

      IF gs_f_prps-stufe > 1.
        CLEAR gs_f_prps1.
        SORT gt_f_prps BY stufe.
        READ TABLE gt_f_prps INTO gs_f_prps1 WITH KEY stufe = '002' BINARY SEARCH.
        IF sy-subrc = 0.
          CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
            EXPORTING
              input  = gs_f_prps1-posid
            IMPORTING
              output = gs_f_final-wbslv
            EXCEPTIONS
              OTHERS = 1.
          IF sy-subrc <> 0.
            CLEAR gs_f_final-wbslv.
          ENDIF.
        ENDIF.
      ENDIF.

      gs_f_final-wrttp = gs_f_rpsco-wrttp.
      gs_f_final-versn = gs_f_rpsco-versn.
**BOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
*      gs_f_final-twaer = gs_f_rpsco-twaer.
      gs_f_final-twaer = 'CAD'.
**EOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing

      IF gs_f_rpsco-wlp01 IS NOT INITIAL.
        lv_amount_1 = gs_f_rpsco-wlp01 + lv_amount_1.
      ENDIF.
      IF gs_f_rpsco-wlp02 IS NOT INITIAL.
        lv_amount_2 = gs_f_rpsco-wlp02 + lv_amount_2.
      ENDIF.
      IF gs_f_rpsco-wlp03 IS NOT INITIAL.
        lv_amount_3 = gs_f_rpsco-wlp03 + lv_amount_3.
      ENDIF.
      IF gs_f_rpsco-wlp04 IS NOT INITIAL.
        lv_amount_4 = gs_f_rpsco-wlp04 + lv_amount_4.
      ENDIF.
      IF gs_f_rpsco-wlp05 IS NOT INITIAL.
        lv_amount_5 = gs_f_rpsco-wlp05 + lv_amount_5.
      ENDIF.
      IF gs_f_rpsco-wlp06 IS NOT INITIAL.
        lv_amount_6 = gs_f_rpsco-wlp06 + lv_amount_6.
      ENDIF.
      IF gs_f_rpsco-wlp07 IS NOT INITIAL.
        lv_amount_7 = gs_f_rpsco-wlp07 + lv_amount_7.
      ENDIF.
      IF gs_f_rpsco-wlp08 IS NOT INITIAL.
        lv_amount_8 = gs_f_rpsco-wlp08 + lv_amount_8.
      ENDIF.
      IF gs_f_rpsco-wlp09 IS NOT INITIAL.
        lv_amount_9 = gs_f_rpsco-wlp09 + lv_amount_9.
      ENDIF.
      IF gs_f_rpsco-wlp10 IS NOT INITIAL.
        lv_amount_10 = gs_f_rpsco-wlp10 + lv_amount_10.
      ENDIF.
      IF gs_f_rpsco-wlp11 IS NOT INITIAL.
        lv_amount_11 = gs_f_rpsco-wlp11 + lv_amount_11.
      ENDIF.
      IF gs_f_rpsco-wlp12 IS NOT INITIAL OR gs_f_rpsco-wlp13 IS NOT INITIAL  OR gs_f_rpsco-wlp14 IS NOT INITIAL
        OR gs_f_rpsco-wlp15 IS NOT INITIAL OR gs_f_rpsco-wlp16 IS NOT INITIAL.
        lv_amount_12 = gs_f_rpsco-wlp12 + gs_f_rpsco-wlp13 + gs_f_rpsco-wlp14 + gs_f_rpsco-wlp15 + gs_f_rpsco-wlp16 + lv_amount_12.
      ENDIF.

      AT END OF versn.
        IF lv_amount_1 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '01'.
          gs_f_final-amount = lv_amount_1.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_2 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '02'.
          gs_f_final-amount = lv_amount_2.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_3 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '03'.
          gs_f_final-amount = lv_amount_3.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_4 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '04'.
          gs_f_final-amount = lv_amount_4.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_5 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '05'.
          gs_f_final-amount = lv_amount_5.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_6 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '06'.
          gs_f_final-amount = lv_amount_6.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_7 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '07'.
          gs_f_final-amount = lv_amount_7.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_8 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '08'.
          gs_f_final-amount = lv_amount_8.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_9 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '09'.
          gs_f_final-amount = lv_amount_9.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_10 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '10'.
          gs_f_final-amount = lv_amount_10.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_11 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '11'.
          gs_f_final-amount = lv_amount_11.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        IF lv_amount_12 IS NOT INITIAL.
          gs_f_final-gjahr = gs_f_rpsco-gjahr.
          gs_f_final-period = '12'.
          gs_f_final-amount = lv_amount_12.
          IF gs_f_final-amount CA '-'. "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
            REPLACE ALL OCCURRENCES OF '-' IN gs_f_final-amount WITH space.
            CONCATENATE '-' gs_f_final-amount INTO gs_f_final-amount.
            CONDENSE gs_f_final-amount.
          ENDIF.                      "Added by KMB 03-12-2019  CHG0159956 - FUT issue fixing
          APPEND gs_f_final TO gt_f_final.
        ENDIF.
        CLEAR : lv_amount_1, lv_amount_2, lv_amount_3, lv_amount_4 ,lv_amount_5, lv_amount_6, lv_amount_7 ,
                lv_amount_8 , lv_amount_9 , lv_amount_10, lv_amount_11, lv_amount_12, gs_f_final.
      ENDAT.

      DELETE gt_f_final WHERE gjahr = p_gjahr AND period < p_monat.
      DELETE gt_f_final WHERE gjahr = lv_high_year AND period > p_monat.
    ENDIF.
  ENDLOOP.
*EOC by KMB 15-11-2019  CHG0159956 - FUT issue fixing


  APPEND LINES OF gt_f_final TO gt_f_final1.
ENDFORM.                    " F_FOR_FORMAT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FOR_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_for_transport_data.

  DATA : lv_path   TYPE string,
*BOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing
*         lv_msg    TYPE text100,
*         ls_output TYPE string,
*         lv_string TYPE string,
         lv_f_total TYPE n LENGTH 15,
         lv_num_file TYPE f,
         lv_number TYPE n LENGTH 3,
*         lv_name TYPE string,"Commented by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
         lv_n_file TYPE i,
         lv_low TYPE n LENGTH 15,
         lv_high TYPE n LENGTH 15,
         lv_rem TYPE n LENGTH 15,
         gt_f_new TYPE TABLE OF lty_f_final,
         lv_round TYPE f,
*EOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing
           lv_fil    TYPE dxfile-filename,
           lv_flg    TYPE c,
           lv_dir    TYPE epsf-epsdirnam,
           lv_file   TYPE epsf-epsfilnam.

  CLEAR: lv_path, lv_msg, gs_f_final.

*BOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing

  IF p_record IS NOT INITIAL.

    CLEAR lv_f_total.
    DESCRIBE TABLE gt_f_final LINES lv_f_total.
    IF gv_first IS INITIAL.
      gv_first = 'X'.
      CLEAR: lv_name, gv_path_f, lv_actual.
      lv_name = p_fname2.
      REPLACE 'XXX' IN lv_name WITH lv_index1.
      REPLACE 'yyyymmdd' IN lv_name WITH sy-datum.
      CONCATENATE p_fpath1 lv_name INTO gv_path_f.
      PERFORM f_for_transfer_data USING gt_f_final lv_name.
      lv_actual = lv_actual + lv_f_total.
    ELSE.
      CLEAR lv_left.
      lv_left = p_record - lv_actual.
      IF lv_left GE lv_f_total.
        PERFORM f_for_transfer_data USING gt_f_final lv_name.
        lv_actual = lv_actual + lv_f_total.
      ELSEIF lv_left GT 0 AND lv_left LT lv_f_total.
        CLEAR gt_f_new[].
        APPEND LINES OF gt_f_final FROM 1 TO lv_left TO gt_f_new.
        PERFORM f_for_transfer_data USING gt_f_new lv_name.
        lv_actual = lv_actual + lv_left.
        CLEAR lv_name.
        lv_index1 = lv_index1 + 1.
        lv_name = p_fname2.
        REPLACE 'XXX' IN lv_name WITH lv_index1.
        REPLACE 'yyyymmdd' IN lv_name WITH sy-datum.
        CONCATENATE p_fpath1 lv_name INTO gv_path_f.
        CLEAR: gt_f_new, lv_actual,gv_flag.
        APPEND LINES OF gt_f_final  FROM lv_left + 1 TO p_record TO gt_f_new.
        PERFORM f_for_transfer_data USING gt_f_new lv_name.
        DESCRIBE TABLE gt_f_new LINES lv_f_total.
        lv_actual = lv_actual + lv_f_total.
      ELSEIF lv_left LE 0 AND lv_left LT lv_f_total.
        CLEAR lv_name.
        lv_index1 = lv_index1 + 1.
        lv_name = p_fname2.
        REPLACE 'XXX' IN lv_name WITH lv_index1.
        REPLACE 'yyyymmdd' IN lv_name WITH sy-datum.
        CONCATENATE p_fpath1 lv_name INTO gv_path_f.
        CLEAR: gt_f_new, lv_actual,gv_flag.
        APPEND LINES OF gt_f_final  FROM 1 TO p_record TO gt_f_new.
        PERFORM f_for_transfer_data USING gt_f_new lv_name.
        DESCRIBE TABLE gt_f_new LINES lv_f_total.
        lv_actual = lv_actual + lv_f_total.
      ENDIF.
    ENDIF.
  ELSE.
    CLEAR: lv_name, gv_path_f.
    lv_name = p_fname2.
    REPLACE 'XXX' IN lv_name WITH '001'.
    REPLACE 'yyyymmdd' IN lv_name WITH sy-datum.
    CONCATENATE p_fpath1 lv_name INTO gv_path_f.

    PERFORM f_for_transfer_data USING gt_f_final lv_name.

  ENDIF.
*EOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing

ENDFORM.                    " F_FOR_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_VALIDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_validate.

  IF p_erpid IS INITIAL.
    MESSAGE text-011 TYPE gc_e.
  ELSEIF p_gjahr IS INITIAL.
    MESSAGE text-012 TYPE gc_e.
  ELSEIF p_monat IS INITIAL.
    MESSAGE text-013 TYPE gc_e.
  ENDIF.

  IF p_all IS INITIAL AND p_for IS INITIAL.
    MESSAGE text-005 TYPE gc_e.
  ENDIF.
ENDFORM.                    " F_VALIDATE
*&---------------------------------------------------------------------*
*&      Form  F_ALL_FORMAT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_all_format_data.
  DATA : lv_pspnr TYPE prps-posid,
         lv_line  TYPE bsvx-sttxt,
         lv_uline TYPE bsvx-sttxt,
         lv_psphi TYPE proj-pspid.

  CLEAR: gv_wlpitd, lv_pspnr, gv_wlpytd, lv_psphi.

  LOOP AT gt_a_rpsco INTO gs_a_rpsco.
    SORT gt_a_prps BY objnr.
    READ TABLE gt_a_prps INTO gs_a_prps WITH KEY objnr = gs_a_rpsco-objnr BINARY SEARCH.
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = gs_a_prps-posid
        IMPORTING
          output = lv_pspnr
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        CLEAR lv_pspnr.
      ENDIF.

      gs_a_final-pspnr = lv_pspnr.
      gs_a_final-post1 = gs_a_prps-post1.

      IF gs_a_prps-stufe > 1.
        CLEAR gs_a_prps1.
        SORT gt_a_prps BY stufe.
        READ TABLE gt_a_prps INTO gs_a_prps1 WITH KEY stufe = '002' BINARY SEARCH.
        IF sy-subrc = 0.
          CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
            EXPORTING
              input  = gs_a_prps1-posid
            IMPORTING
              output = gs_a_final-wbslv
            EXCEPTIONS
              OTHERS = 1.
          IF sy-subrc <> 0.
            CLEAR gs_a_final-wbslv.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_KONPD_OUTPUT'
        EXPORTING
          input  = gs_a_prps-psphi
        IMPORTING
          output = lv_psphi
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        CLEAR lv_psphi.
      ENDIF.

      gs_a_final-psphi = lv_psphi.
      gs_a_final-stufe = gs_a_prps-stufe.

      SORT gt_jest BY objnr.
      READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_a_prps-objnr BINARY SEARCH.
      IF sy-subrc = 0.
        CLEAR: lv_line, lv_uline.
        CALL FUNCTION 'STATUS_TEXT_EDIT'
          EXPORTING
            client           = sy-mandt
            objnr            = gs_jest-objnr
            only_active      = gc_x
            spras            = sy-langu
          IMPORTING
            line             = lv_line
            user_line        = lv_uline
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
          MESSAGE text-007 TYPE gc_e.
        ELSEIF sy-subrc = 0.
*BOC by KMB on 20.11.2019 CHG0159956 - FUT issue
*          CONCATENATE lv_line lv_uline INTO gs_a_final-stat SEPARATED BY space.
          gs_a_final-stat = lv_line.
*EOC by KMB on 20.11.2019 CHG0159956 - FUT issue
        ENDIF.
      ENDIF.

      gs_a_final-prart = gs_a_prps-prart.

      SORT gt_a_proj BY pspnr.
      READ TABLE gt_a_proj INTO gs_a_proj WITH KEY pspnr = gs_a_prps-psphi BINARY SEARCH.
      IF sy-subrc = 0.
        gs_a_final-vbukr = gs_a_proj-vbukr.
        gs_a_final-plfaz = gs_a_proj-plfaz.
        gs_a_final-plsez = gs_a_proj-plsez.
*        gs_a_final-astnr = gs_a_proj-astnr. *Changed by KMB on 20.11.2019 CHG0159956 - FUT issue
        gs_a_final-eprog = gs_a_proj-eprog.
      ENDIF.

      SORT gt_pratx BY prart.
      READ TABLE gt_pratx INTO gs_pratx WITH KEY prart = gs_a_prps-prart BINARY SEARCH.
      IF sy-subrc = 0.
        gs_a_final-pratx = gs_pratx-pratx.
      ENDIF.

      SORT gt_tcj04 BY vernr.
      READ TABLE gt_tcj04 INTO gs_tcj04 WITH KEY vernr = gs_a_prps-vernr BINARY SEARCH.
      IF sy-subrc = 0.
        gs_a_final-verna = gs_tcj04-verna.
      ENDIF.

*BOC by KMB on 20.11.2019 CHG0159956 - FUT issue fixing
*      gs_a_final-twaer = gs_f_rpsco-twaer.
**BOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
*      gs_a_final-twaer = gs_a_rpsco-twaer..
      gs_a_final-twaer = 'CAD'.
**EOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
*EOC by KMB on 20.11.2019 CHG0159956 - FUT issue fixing

      IF gs_a_rpsco-wrttp = gc_41 AND gs_a_rpsco-gjahr IS INITIAL.
        gs_a_final-total = gs_a_final-total + gs_a_rpsco-wlp00.
      ELSEIF gs_a_rpsco-wrttp = gc_41 AND gs_a_rpsco-gjahr IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      IF gs_a_rpsco-gjahr < p_gjahr.
        gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 + gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 +
         "BOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
*                    gs_a_rpsco-wlp04 + gs_f_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                    gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
         "EOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
                    gs_a_rpsco-wlp08 + gs_a_rpsco-wlp09 + gs_a_rpsco-wlp10 + gs_a_rpsco-wlp11 +
                    gs_a_rpsco-wlp12 + gs_a_rpsco-wlp13 + gs_a_rpsco-wlp14 + gs_a_rpsco-wlp15 +  gs_a_rpsco-wlp16.
      ELSEIF gs_a_rpsco-gjahr = p_gjahr.
        CASE p_monat.
          WHEN gc_1.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp01.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp01.
* End of changes byAHMADT for CHG0189892
          WHEN gc_2.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp02.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp02.
* End of changes byAHMADT for CHG0189892
          WHEN gc_3.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp03.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp03.
* End of changes byAHMADT for CHG0189892
          WHEN gc_4.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp04.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp04.
* End of changes byAHMADT for CHG0189892
          WHEN gc_5.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp05.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp05.
* End of changes byAHMADT for CHG0189892
          WHEN gc_6.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp06.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp06.
* End of changes byAHMADT for CHG0189892
          WHEN gc_7.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp07.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp07.
* End of changes byAHMADT for CHG0189892
          WHEN gc_8.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp08.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp08.
* End of changes byAHMADT for CHG0189892
          WHEN gc_9.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08 + gs_a_rpsco-wlp09.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08 + gs_a_rpsco-wlp09.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp09.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp09.
* End of changes byAHMADT for CHG0189892
          WHEN gc_10.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08 + gs_a_rpsco-wlp09 + gs_a_rpsco-wlp10.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08 + gs_a_rpsco-wlp09 + gs_a_rpsco-wlp10.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp10.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp10.
* End of changes byAHMADT for CHG0189892
          WHEN gc_11.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08 + gs_a_rpsco-wlp09 + gs_a_rpsco-wlp10 + gs_a_rpsco-wlp11.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08 + gs_a_rpsco-wlp09 + gs_a_rpsco-wlp10 + gs_a_rpsco-wlp11.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd = gs_a_rpsco-wlp11.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp11.
* End of changes byAHMADT for CHG0189892
          WHEN gc_12.
            gv_wlpitd  = gv_wlpitd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08 + gs_a_rpsco-wlp09 + gs_a_rpsco-wlp10 + gs_a_rpsco-wlp11 + gs_a_rpsco-wlp12 + gs_a_rpsco-wlp13 +
                         gs_a_rpsco-wlp14 + gs_a_rpsco-wlp15 + gs_a_rpsco-wlp16.
            gv_wlpytd  = gv_wlpytd + gs_a_rpsco-wlp01 +
                         gs_a_rpsco-wlp02 + gs_a_rpsco-wlp03 + gs_a_rpsco-wlp04 + gs_a_rpsco-wlp05 + gs_a_rpsco-wlp06 + gs_a_rpsco-wlp07 +
                         gs_a_rpsco-wlp08 + gs_a_rpsco-wlp09 + gs_a_rpsco-wlp10 + gs_a_rpsco-wlp11 + gs_a_rpsco-wlp12 + gs_a_rpsco-wlp13 +
                         gs_a_rpsco-wlp14 + gs_a_rpsco-wlp15 + gs_a_rpsco-wlp16.
* Start of changes by AHMADT for CHG0189892
*            gs_a_final-wlpmtd =  gs_a_rpsco-wlp12 + gs_a_rpsco-wlp13 + gs_a_rpsco-wlp14 + gs_a_rpsco-wlp15 + gs_a_rpsco-wlp16.
             gs_a_final-wlpmtd = gs_a_final-wlpmtd + gs_a_rpsco-wlp12 + gs_a_rpsco-wlp13 + gs_a_rpsco-wlp14 + gs_a_rpsco-wlp15 + gs_a_rpsco-wlp16.
* End of changes byAHMADT for CHG0189892
        ENDCASE.
      ENDIF.
    ENDIF.
    AT END OF versn.
      "BOC by KMB on 02.12.2019 CHG0159956 FUT issue fixing
*      SORT gt_a_final BY pspnr psphi.
*      READ TABLE gt_a_final INTO gs_a_fin WITH KEY pspnr = lv_pspnr psphi = gs_a_final-psphi BINARY SEARCH.
*      IF sy-subrc = 0.
*        gv_wlpitd = gv_wlpitd + gs_a_fin-wlpitd.
*      ENDIF.
      "EOC by KMB on 02.12.2019 CHG0159956 FUT issue fixing
      IF gs_a_final IS NOT INITIAL.
        gs_a_final-wlpitd = gv_wlpitd.
        gs_a_final-wlpytd = gv_wlpytd.
        gs_a_final-gjahr  = gs_a_rpsco-gjahr.
        REPLACE ALL OCCURRENCES OF gc_delim IN gs_a_final-pratx WITH space.
        REPLACE ALL OCCURRENCES OF gc_delim IN gs_a_final-post1 WITH space.
        REPLACE ALL OCCURRENCES OF gc_delim IN gs_a_final-verna WITH space.

        IF gs_a_final-total CA '-'.
          REPLACE ALL OCCURRENCES OF '-' IN gs_a_final-total WITH space.
          CONCATENATE '-' gs_a_final-total INTO gs_a_final-total.
          CONDENSE gs_a_final-total.
        ELSEIF gs_a_final-wlpitd CA '-'.
          REPLACE ALL OCCURRENCES OF '-' IN gs_a_final-wlpitd WITH space.
          CONCATENATE '-' gs_a_final-wlpitd INTO gs_a_final-wlpitd.
          CONDENSE gs_a_final-wlpitd.
        ELSEIF gs_a_final-wlpmtd CA '-'.
          REPLACE ALL OCCURRENCES OF '-' IN gs_a_final-wlpmtd WITH space.
          CONCATENATE '-' gs_a_final-wlpmtd INTO gs_a_final-wlpmtd.
          CONDENSE gs_a_final-wlpmtd.
        ELSEIF gs_a_final-wlpytd CA '-'.
          REPLACE ALL OCCURRENCES OF '-' IN gs_a_final-wlpytd WITH space.
          CONCATENATE '-' gs_a_final-wlpytd INTO gs_a_final-wlpytd.
          CONDENSE gs_a_final-wlpytd.
        ENDIF.

        APPEND gs_a_final TO gt_a_final .

        CLEAR : gs_a_final, gs_a_prps, gv_wlpytd, lv_pspnr, gv_wlpitd.
      ENDIF.
    ENDAT.

    CLEAR: gs_f_rpsco, gs_f_prps, gs_pratx, gs_a_proj, gs_tcj04, gs_jest.

  ENDLOOP.
  APPEND LINES OF gt_a_final TO gt_a_final1.
ENDFORM.                    " F_ALL_FORMAT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_ALL_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_all_transport_data .
  DATA :   lv_path   TYPE string,
           lv_msg    TYPE text100,
           ls_output TYPE string,
           lv_string TYPE string,
           lv_fil    TYPE dxfile-filename,
           lv_flg    TYPE c,
           lv_dir    TYPE epsf-epsdirnam,
           lv_file   TYPE epsf-epsfilnam.

  CLEAR: lv_path, lv_msg, gs_f_final.

*BOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing
  DATA : lv_a_total TYPE n LENGTH 15,
         lv_num_file TYPE f,
         lv_number TYPE n LENGTH 3,
*         lv_a_name TYPE string,      "Commented by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
         lv_n_file TYPE i,
         lv_low TYPE n LENGTH 15,
         lv_high TYPE n LENGTH 15,
         lv_rem TYPE n LENGTH 15,
         gt_a_new TYPE TABLE OF lty_a_final,
         lv_round TYPE f.

  IF p_record IS NOT INITIAL AND gt_a_final IS NOT INITIAL.

    DESCRIBE TABLE gt_a_final LINES lv_a_total.
    IF gv_a_first IS INITIAL.
      gv_a_first = 'X'.
      CLEAR: lv_a_name, gv_path_a, lv_a_actual.
      lv_a_name = p_fname1.
      REPLACE 'XXX' IN lv_a_name WITH lv_index_a.
      REPLACE 'yyyymmdd' IN lv_a_name WITH sy-datum.
      CONCATENATE p_fpath1 lv_a_name INTO gv_path_a.
      PERFORM f_all_transfer_data USING gt_a_final  lv_a_name.
      lv_a_actual = lv_a_actual + lv_a_total.
    ELSE.
      CLEAR lv_a_left.
      lv_a_left = p_record - lv_a_actual.
      IF lv_a_left GE lv_a_total.
        PERFORM f_all_transfer_data USING gt_a_final  lv_a_name.
        lv_a_actual = lv_a_actual + lv_a_total.
      ELSEIF lv_a_left GT 0 AND lv_a_left LT lv_a_total.
        CLEAR : gt_a_new[], lv_a_actual.
        APPEND LINES OF gt_a_final FROM 1 TO lv_a_left TO gt_a_new.
        PERFORM f_all_transfer_data USING gt_a_new lv_a_name.
        lv_a_actual = lv_a_actual + lv_a_left.
        CLEAR:lv_a_name, gv_path_a.
        lv_index_a = lv_index_a + 1.
        lv_a_name = p_fname1.
        REPLACE 'XXX' IN lv_a_name WITH lv_index_a.
        REPLACE 'yyyymmdd' IN lv_a_name WITH sy-datum.
        CONCATENATE p_fpath1 lv_a_name INTO gv_path_a.
        CLEAR: gt_a_new, lv_a_actual,gv_flag1.
        "BOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
*        APPEND LINES OF gt_a_final  FROM lv_left + 1 TO p_record TO gt_a_new.
        APPEND LINES OF gt_a_final  FROM lv_a_left + 1 TO p_record TO gt_a_new.
        "EOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
        PERFORM f_all_transfer_data USING gt_a_new lv_a_name.
        DESCRIBE TABLE gt_a_new LINES lv_a_total.
        lv_a_actual = lv_a_actual + lv_a_total.
      ELSEIF lv_a_left LE 0 AND lv_a_left LT lv_a_total.
        CLEAR lv_a_name.
        lv_index_a = lv_index_a + 1.
        lv_a_name = p_fname1.
        REPLACE 'XXX' IN lv_a_name WITH lv_index_a.
        REPLACE 'yyyymmdd' IN lv_a_name WITH sy-datum.
        CONCATENATE p_fpath1 lv_a_name INTO gv_path_a.
        CLEAR: gt_a_new, lv_a_actual,gv_flag1.
        APPEND LINES OF gt_a_final  FROM 1 TO p_record TO gt_a_new.
        PERFORM f_all_transfer_data USING gt_a_new lv_a_name.
        DESCRIBE TABLE gt_a_new LINES lv_a_total.
        lv_a_actual = lv_a_actual + lv_a_total.
      ENDIF.

    ENDIF.

  ELSE.

    CLEAR lv_a_name.
    lv_a_name = p_fname1.
    REPLACE 'XXX' IN lv_a_name WITH '001'.
    REPLACE 'yyyymmdd' IN lv_a_name WITH sy-datum.
    CONCATENATE p_fpath1 lv_a_name INTO gv_path_a.

    PERFORM f_all_transfer_data USING gt_a_final lv_a_name.

  ENDIF.
*EOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing

ENDFORM.                    " F_ALL_TRANSPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FILL_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FPATH1  text
*----------------------------------------------------------------------*
FORM f_fill_file_path  CHANGING cv_fpath1.
  CONSTANTS : lc_f1 TYPE string VALUE '\\sapfileshare.gtna.gt.ds\FI\',
              lc_f2 TYPE string VALUE '\Out\I_P2C_PS_911\'.
  CONCATENATE lc_f1 sy-sysid lc_f2 INTO cv_fpath1.
ENDFORM.                    " F_FILL_FILE_PATH
*&---------------------------------------------------------------------*
*&      Form  F_ALL_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_all_download_data.

  DATA: "lv_file TYPE string , "Commented by KMB on 15.11.2019 CHG0159956 - FUT issue fixing
        lv_lines TYPE i ,
        lv_records TYPE string,
        lv_succ TYPE string.

*BOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing
*    DATA: lt_tab_data       TYPE STANDARD TABLE OF string,
*          ls_tab_data       TYPE string.
  DATA : lv_a_total TYPE n LENGTH 15,
         lv_num_file TYPE f,
         lv_number TYPE n LENGTH 3,
         lv_name TYPE string,
         lv_n_file TYPE i,
         lv_low TYPE n LENGTH 15,
         lv_high TYPE n LENGTH 15,
         lv_rem TYPE n LENGTH 15,
         lv_round TYPE f.

  DESCRIBE TABLE gt_a_final1 LINES lv_a_total.
  IF p_record IS NOT INITIAL AND lv_a_total > p_record.

    CLEAR : lv_num_file, lv_round, lv_n_file.
    lv_num_file = lv_a_total / p_record.

    CALL FUNCTION 'ROUND'
      EXPORTING
        input        = lv_num_file
        sign         = '+'
      IMPORTING
        output       = lv_round
      EXCEPTIONS
        input_invald = 01
        overflow     = 02
        type_invalid = 03.
    IF sy-subrc <> 0.
      MESSAGE 'Error while downloading' TYPE 'E'.
    ELSE.
      lv_n_file = lv_round.
    ENDIF.

    DO lv_n_file TIMES.
      CLEAR: gv_flag, lv_number, lv_name.
      lv_number = sy-index.
      lv_name = p_fname1.
      REPLACE 'XXX' IN lv_name WITH lv_number.
      REPLACE 'yyyymmdd' IN lv_name WITH sy-datum.
      CONCATENATE p_fpath1 lv_name INTO lv_file.

      CASE sy-index.
        WHEN 1.
          lv_low = 1.
          lv_high = p_record.
          CLEAR: gt_a_new[].
          APPEND LINES OF gt_a_final1 FROM lv_low TO lv_high TO gt_a_new.
          lv_rem = lv_a_total - lv_high.

        WHEN OTHERS.
          IF lv_rem > p_record.
            lv_low = lv_high + 1.
            lv_high = p_record * sy-index.
            CLEAR: gt_a_new[].
            APPEND LINES OF gt_a_final1 FROM lv_low TO lv_high TO gt_a_new.
            lv_rem = lv_a_total - lv_high.
          ELSE.
            lv_low = lv_high + 1.
            lv_high = lv_high + lv_rem.
            CLEAR: gt_a_new[].
            APPEND LINES OF gt_a_final1 FROM lv_low TO lv_high TO gt_a_new.
          ENDIF.
      ENDCASE.
      PERFORM f_all_download USING gt_a_new lv_file.
    ENDDO.

  ELSE.
    CLEAR lv_name.
    lv_name = p_fname1.
    REPLACE 'XXX' IN lv_name WITH '001'.
    REPLACE 'yyyymmdd' IN lv_name WITH sy-datum.
    CONCATENATE p_fpath1 lv_name INTO lv_file.

    PERFORM f_all_download USING gt_a_final1 lv_file.
  ENDIF.
*EOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing

***********    write output in the spool
  CLEAR lv_lines .

  DESCRIBE TABLE  gt_a_final1 LINES lv_lines.

  lv_records = lv_lines.
  CONCATENATE text-008 lv_records INTO lv_succ SEPARATED BY space.
  WRITE lv_succ.
ENDFORM.                    " F_ALL_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  F_FOR_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_for_download_data.

  DATA: lv_file TYPE string ,
        lv_lines TYPE i ,
        lv_records TYPE string,
        lv_succ TYPE string.

  DATA: lt_tab_data       TYPE STANDARD TABLE OF string,
        ls_tab_data       TYPE string.

  CLEAR : ls_tab_data, lt_tab_data.

*BOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing
  DATA : lv_f_total TYPE n LENGTH 15,
         lv_num_file TYPE f,
         lv_number TYPE n LENGTH 3,
         lv_name TYPE string,
         lv_n_file TYPE i,
         lv_low TYPE n LENGTH 15,
         lv_high TYPE n LENGTH 15,
         lv_rem TYPE n LENGTH 15,
         lv_round TYPE f.

  DESCRIBE TABLE gt_f_final1 LINES lv_f_total.
  IF p_record IS NOT INITIAL AND lv_f_total > p_record.

    CLEAR : lv_num_file, lv_round, lv_n_file.
    lv_num_file = lv_f_total / p_record.

    CALL FUNCTION 'ROUND'
      EXPORTING
        input        = lv_num_file
        sign         = '+'
      IMPORTING
        output       = lv_round
      EXCEPTIONS
        input_invald = 01
        overflow     = 02
        type_invalid = 03.
    IF sy-subrc <> 0.
      MESSAGE 'Error while downloading' TYPE 'E'.
    ELSE.
      lv_n_file = lv_round.
    ENDIF.

    DO lv_n_file TIMES.
      CLEAR: gv_flag, lv_number, lv_name.
      lv_number = sy-index.
      lv_name = p_fname2.
      REPLACE 'XXX' IN lv_name WITH lv_number.
      REPLACE 'yyyymmdd' IN lv_name WITH sy-datum.
      CONCATENATE p_fpath1 lv_name INTO lv_file.

      CASE sy-index.
        WHEN 1.
          lv_low = 1.
          lv_high = p_record.
          CLEAR: gt_f_new[].
          APPEND LINES OF gt_f_final1 FROM lv_low TO lv_high TO gt_f_new.
          lv_rem = lv_f_total - lv_high.

        WHEN OTHERS.
          IF lv_rem > p_record.
            lv_low = lv_high + 1.
            lv_high = p_record * sy-index.
            CLEAR: gt_f_new[].
            APPEND LINES OF gt_f_final1 FROM lv_low TO lv_high TO gt_f_new.
            lv_rem = lv_f_total - lv_high.
          ELSE.
            lv_low = lv_high + 1.
            lv_high = lv_high + lv_rem.
            CLEAR: gt_f_new[].
            APPEND LINES OF gt_f_final1 FROM lv_low TO lv_high TO gt_f_new.
          ENDIF.
      ENDCASE.

      PERFORM f_for_download USING gt_f_new lv_file.

    ENDDO.

  ELSE.
    CLEAR lv_name.
    lv_name = p_fname2.
    REPLACE 'XXX' IN lv_name WITH '001'.
    REPLACE 'yyyymmdd' IN lv_name WITH sy-datum.
    CONCATENATE p_fpath1 lv_name INTO lv_file.

    PERFORM f_for_download USING gt_f_final1 lv_file.
  ENDIF.
*EOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing


***********    write output in the spool
  CLEAR lv_lines .
  DESCRIBE TABLE  gt_f_final1 LINES lv_lines.
  lv_records = lv_lines.
  CONCATENATE text-008 lv_records INTO lv_succ SEPARATED BY space.
  WRITE lv_succ.

ENDFORM.                    " F_FOR_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data.
  TYPES: BEGIN OF lty_prps,
         pspnr TYPE prps-pspnr,
         posid TYPE prps-posid,
         pbukr TYPE prps-pbukr,
         prart TYPE prps-prart,
         END OF lty_prps.

  DATA : lv_pspnr TYPE proj-pspnr,
         lt_wbslvl2 TYPE TABLE OF lty_prps,
         ls_wbslvl2 TYPE lty_prps.

*BOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing
  lv_index1 = '001'.
  lv_index_a = '001'.
*EOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing

*BOC by KMB on 29.11.2019 CHG0159956 - Adding file size field
  CLEAR    gv_file_size.
  CLEAR    gv_file_size_brk.
  gv_file_size_brk = p_fbrk * 1000000.
*EOC by KMB on 29.11.2019 CHG0159956 - Adding file size field


  CLEAR: gs_psphi, lt_wbslvl2[],ls_wbslvl2, gv_flag, gv_flag1.
  SORT gt_psphi.
  DELETE ADJACENT DUPLICATES FROM gt_psphi.
  LOOP AT gt_psphi INTO gs_psphi.
    CLEAR lt_wbslvl2[].
    SELECT pspnr posid pbukr prart
      FROM prps
      INTO TABLE lt_wbslvl2
      WHERE psphi = gs_psphi-psphi AND loevm = space AND stufe = '002'.
    IF sy-subrc = 0.
      SORT lt_wbslvl2 ASCENDING BY posid.
      LOOP AT lt_wbslvl2 INTO ls_wbslvl2.
*BOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing
*        IF ls_wbslvl2-posid NOT IN s_wbs OR ls_wbslvl2-pbukr NOT IN s_bukrs OR ls_wbslvl2-prart NOT IN s_ptype.
        IF ls_wbslvl2-posid NOT IN s_wbs.
          CONTINUE.
        ELSEIF ls_wbslvl2-pbukr NOT IN s_bukrs.
          CONTINUE.
        ELSEIF ls_wbslvl2-prart NOT IN s_ptype.
          CONTINUE.
*EOC by KMB on 15.11.2019 CHG0159956 - FUT issue fixing
        ELSE.
          CLEAR : gt_i_wbs[], gt_e_wbs[], gt_msg[], gt_a_proj[], gt_a_prps[], gt_a_rpsco[], gt_pratx[],  gt_tcj04[], gt_jest[], gt_prps[].
          CLEAR : gt_f_proj[], gt_f_prps[], gt_f_rpsco[], gt_prps[], gt_a_final[], gt_f_final[].

          gs_i_wbs-wbs_element = ls_wbslvl2-posid.
          APPEND gs_i_wbs TO gt_i_wbs.

          CALL FUNCTION 'BAPI_PROJECT_GETINFO'
            EXPORTING
              with_subtree        = gc_x
            TABLES
              i_wbs_element_table = gt_i_wbs
              e_wbs_element_table = gt_e_wbs
              e_message_table     = gt_msg
            EXCEPTIONS
              OTHERS              = 1.
          IF sy-subrc = 0 AND gt_e_wbs IS NOT INITIAL.
            CLEAR: gs_prps, gt_prps[].
            LOOP AT gt_e_wbs INTO gs_e_wbs.
              CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
                EXPORTING
                  input  = gs_e_wbs-wbs_element
                IMPORTING
                  output = gs_prps-posid
                EXCEPTIONS
                  OTHERS = 1.
              IF sy-subrc = 0.
                APPEND gs_prps TO gt_prps.
                CLEAR gs_prps.
              ENDIF.
            ENDLOOP.
            READ TABLE gt_e_wbs INTO gs_e_wbs INDEX 1.
            CALL FUNCTION 'CONVERSION_EXIT_ABPSN_INPUT'
              EXPORTING
                input  = gs_e_wbs-project_definition
              IMPORTING
                output = gs_prps-posid
              EXCEPTIONS
                OTHERS = 1.
            IF sy-subrc = 0.
              APPEND gs_prps TO gt_prps.
              CLEAR gs_prps.
            ENDIF.

**if all data checkbox selected
            IF gt_prps IS NOT INITIAL.
              IF p_all = abap_true.
                PERFORM f_all_get_data.              "Get data from database & prepare final output
                PERFORM f_all_format_data.

                IF p_app = abap_true AND gt_a_final IS NOT INITIAL.
                  PERFORM f_all_transport_data .          " To place the file in Application layer.
                ENDIF.

              ENDIF.

*If Forecast checkbox data selected
              IF p_for = abap_true.
                PERFORM f_for_get_data.              "Get data from database & prepare final output
                PERFORM f_for_format_data.

                IF p_app = abap_true AND gt_f_final IS NOT INITIAL.
                  PERFORM f_for_transport_data.          " To place the file in Application layer.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR ls_wbslvl2.
      ENDLOOP.
    ENDIF.
    CLEAR gs_psphi.
  ENDLOOP.

  IF p_all = abap_true AND p_pre = abap_true AND  gt_a_final1 IS NOT INITIAL.
    PERFORM f_all_download_data.            " To place the file in Presentation layer.
  ENDIF.

  IF p_for = abap_true AND p_pre = abap_true AND gt_f_final1 IS NOT INITIAL..
    PERFORM f_for_download_data.           " To place the file in Presentation layer.
  ENDIF.

ENDFORM.                    " F_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA_FROM_SEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_data_from_sel.
  SELECT DISTINCT psphi
      FROM prps
      INTO TABLE gt_psphi
      WHERE posid IN s_wbs AND
            pbukr IN s_bukrs AND
            prart IN s_ptype.
  IF sy-subrc <> 0.
    CLEAR gt_psphi[].
  ENDIF.
ENDFORM.                    " F_GET_DATA_FROM_SEL
*&---------------------------------------------------------------------*
*&      Form  F_FOR_TRANSFER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_F_FINAL  text
*----------------------------------------------------------------------*
FORM f_for_transfer_data USING value(gt_f_fin) LIKE gt_f_final  value(lv_name_f) LIKE lv_name.

*BOC by KMB on 29.11.2019 CHG0159956 - Adding file size field
  DATA : gv_check_f TYPE c,
       lv_to_f TYPE n LENGTH 5,
       gt_f_new1 LIKE gt_f_final,
       gv_f_lines TYPE n LENGTH 10,
"BOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
*       gv_path_f TYPE epsf-epsdirnam,
       gv_path_new TYPE epsf-epsdirnam,
"EOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
       gv_name_f TYPE epsf-epsfilnam.

  CLEAR gv_check_f.
*EOC by KMB on 29.11.2019 CHG0159956 - Adding file size field

  IF gv_path_f IS NOT INITIAL.

    IF gv_flag IS INITIAL.

      OPEN DATASET gv_path_f FOR OUTPUT IN TEXT MODE
                    ENCODING DEFAULT MESSAGE lv_msg.
      IF     ( sy-subrc NE 0 ).
        WRITE:   /001  text-006, lv_msg.
        MESSAGE  e000(zfi01) WITH text-006 lv_msg.
      ENDIF.
    ELSE.

      OPEN DATASET gv_path_f FOR APPENDING IN TEXT MODE
                           ENCODING DEFAULT MESSAGE lv_msg.
      IF     ( sy-subrc NE 0 ).
        WRITE:   /001  text-006, lv_msg.
        MESSAGE  e000(zfi01) WITH text-006 lv_msg.
      ENDIF.

*BOC by KMB on 29.11.2019 CHG0159956 - Adding file size field
      IF p_fbrk IS NOT INITIAL.
        "BOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
*        gv_path_f = p_fpath1.
        gv_path_new = p_fpath1.
        "EOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
        gv_name_f = lv_name_f.
        CALL FUNCTION 'EPS_GET_FILE_ATTRIBUTES'
          EXPORTING
            file_name              = gv_name_f
                    "BOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
*           dir_name               = gv_path_f
            dir_name               = gv_path_new
                    "EOC by KMB on 02.12.2019 CHG0159956 - FUT issue fixing
          IMPORTING
            file_size              = lv_file_size_f
          EXCEPTIONS
            read_directory_failed  = 1
            read_attributes_failed = 2
            OTHERS                 = 3.

        IF sy-subrc <> 0.
* Implement suitable error handling here
          MESSAGE 'File size error!' TYPE 'E'.
        ELSE.
          gv_check_f = 'X'.
          lv_left_app_size_f = gv_file_size_brk - lv_file_size_f.
        ENDIF.
      ENDIF.
*EOC by KMB on 29.11.2019 CHG0159956 - Adding file size field

    ENDIF.

    "Create header record
    IF gv_flag IS INITIAL.
      CLEAR ls_output.
      CONCATENATE text-f01 text-f07 text-f08 text-f10 text-f02 text-f03
      text-f09 text-f04 text-f05 text-f06 INTO ls_output SEPARATED BY gc_delim.
      TRANSFER ls_output TO gv_path_f.
      gv_flag = 'X'.
    ENDIF.

    "Transferring data
    IF gt_f_fin[] IS NOT INITIAL.

      LOOP AT gt_f_fin INTO gs_f_final.

        CONCATENATE gs_f_final-pspnr
                    gs_f_final-psphi
                    gs_f_final-stufe
                    gs_f_final-wbslv
                    gs_f_final-wrttp
                    gs_f_final-versn
                    gs_f_final-gjahr
                    gs_f_final-period
                    gs_f_final-amount
*                        gs_f_final-wlpitd
*                        gs_f_final-wlpytd
                    gs_f_final-twaer
        INTO lv_string SEPARATED BY gc_delim.

*BOC by KMB on 29.11.2019 CHG0159956 - Adding file size field
        IF p_fbrk IS NOT INITIAL AND gv_check_f = 'X'.
          lv_strlen_f = strlen( lv_string ).
          ADD lv_strlen_f TO gv_file_size_f.
          IF lv_left_app_size_f GT gv_file_size_f.
            TRANSFER lv_string TO gv_path_f.
            CLEAR : gs_f_final, lv_string .
            lv_left_app_size_f = lv_left_app_size_f - gv_file_size_f.
            lv_left_app_size_f = abs( lv_left_app_size_f ).
          ELSE.
            CLOSE DATASET gv_path_f.
            CLEAR lv_index_new.
            lv_index_new = lv_index1 + 1.

            REPLACE lv_index1 IN gv_path_f WITH lv_index_new.
            lv_index1 = lv_index1 + 1.
            CLEAR gv_flag.

            OPEN DATASET gv_path_f FOR OUTPUT IN TEXT MODE
                        ENCODING DEFAULT MESSAGE lv_msg.
            IF     ( sy-subrc NE 0 ).
              WRITE:   /001  text-006, lv_msg.
              MESSAGE  e000(zfi01) WITH text-006 lv_msg.
            ENDIF.

            CLEAR ls_output.

            CONCATENATE text-f01 text-f07 text-f08 text-f10 text-f02 text-f03
            text-f09 text-f04 text-f05 text-f06 INTO ls_output SEPARATED BY gc_delim.
            TRANSFER ls_output TO gv_path_f.

            CLEAR lv_string.
            CONCATENATE gs_f_final-pspnr
            gs_f_final-psphi
            gs_f_final-stufe
            gs_f_final-wbslv
            gs_f_final-wrttp
            gs_f_final-versn
            gs_f_final-gjahr
            gs_f_final-period
            gs_f_final-amount
            gs_f_final-twaer
            INTO lv_string SEPARATED BY gc_delim.

            TRANSFER lv_string TO gv_path_f.
            CLEAR : gs_f_final, gv_file_size_f.
            CLEAR lv_left_app_size_f.
            lv_strlen_f = strlen( lv_string ).
            ADD lv_strlen_f TO gv_file_size_f.

            lv_left_app_size_f = gv_file_size_brk - gv_file_size_f.
            gv_check_f = 'X'.
            CLEAR lv_string.
          ENDIF.
        ELSE.
          TRANSFER lv_string TO gv_path_f.
          CLEAR : gs_f_final, lv_string , gv_file_size_f.
        ENDIF.

*        TRANSFER lv_string TO gv_path_f.
*        CLEAR : gs_f_final, lv_string .
*EOC by KMB on 29.11.2019 CHG0159956 - Adding file size field

      ENDLOOP.
    ENDIF.

    CLOSE DATASET gv_path_f.
    CLEAR gv_file_size_f. "Added by KMB on 02.12.2019 CHG0159956 - FUT issue fixing

  ENDIF.
ENDFORM.                    " F_FOR_TRANSFER_DATA
*&---------------------------------------------------------------------*
*&      Form  F_ALL_TRANSFER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_A_FINAL  text
*----------------------------------------------------------------------*
FORM f_all_transfer_data  USING value(gt_a_fin) LIKE gt_a_final value(lv_name) LIKE lv_a_name.

*BOC by KMB on 29.11.2019 CHG0159956 - Adding file size field
  DATA : gv_check TYPE c,
         lv_to TYPE n LENGTH 5,
         gt_a_new1 TYPE TABLE OF lty_a_final,
         gv_a_lines TYPE n LENGTH 10,
         gv_path TYPE epsf-epsdirnam,
         gv_name TYPE epsf-epsfilnam.

  CLEAR gv_check.
*EOC by KMB on 29.11.2019 CHG0159956 - Adding file size field

  IF gv_path_a IS NOT INITIAL.

    IF gv_flag1 IS INITIAL.
      OPEN DATASET gv_path_a FOR OUTPUT IN TEXT MODE
                    ENCODING DEFAULT MESSAGE lv_msg.
      IF     ( sy-subrc NE 0 ).
        WRITE:   /001  text-006, lv_msg.
        MESSAGE  e000(zfi01) WITH text-006 lv_msg.
      ENDIF.
    ELSE.

      OPEN DATASET gv_path_a FOR APPENDING IN TEXT MODE
                           ENCODING DEFAULT MESSAGE lv_msg.
      IF     ( sy-subrc NE 0 ).
        WRITE:   /001  text-006, lv_msg.
        MESSAGE  e000(zfi01) WITH text-006 lv_msg.
      ENDIF.

*BOC by KMB on 29.11.2019 CHG0159956 - Adding file size field
      IF p_fbrk IS NOT INITIAL.
        gv_path = p_fpath1.
        gv_name = lv_name.
        CALL FUNCTION 'EPS_GET_FILE_ATTRIBUTES'
          EXPORTING
            file_name              = gv_name
            dir_name               = gv_path
          IMPORTING
            file_size              = lv_file_size
          EXCEPTIONS
            read_directory_failed  = 1
            read_attributes_failed = 2
            OTHERS                 = 3.

        IF sy-subrc <> 0.
* Implement suitable error handling here
          MESSAGE 'File size error!' TYPE 'E'.
        ELSE.
          gv_check = 'X'.
          lv_left_app_size = gv_file_size_brk - lv_file_size.
        ENDIF.
      ENDIF.
*EOC by KMB on 29.11.2019 CHG0159956 - Adding file size field

    ENDIF.

    "Create header record
    IF gv_flag1 IS INITIAL.

      CLEAR ls_output.
      CONCATENATE text-a01 text-a02 text-a03 text-a04 text-a05 text-a19 text-a20 text-a21 text-a06

                  text-a07 text-a08 text-a09 text-a17 text-a10 text-a11 text-a12


                  "*BOC by KMB on 20.11.2019 CHG0159956 - FUT issue
*                  text-a13 text-a14 text-a15 text-a16
                  text-a13 text-a14 text-a15
                  "*EOC by KMB on 20.11.2019 CHG0159956 - FUT issue
      INTO ls_output SEPARATED BY gc_delim.
      TRANSFER ls_output TO gv_path_a.
      gv_flag1 = 'X'.
    ENDIF.

    "Transferring data
    IF gt_a_fin[] IS NOT INITIAL.

      LOOP AT gt_a_fin INTO gs_a_final.

        CONCATENATE  gs_a_final-vbukr
                     gs_a_final-prart
                     gs_a_final-pratx
                     gs_a_final-pspnr
                     gs_a_final-post1
                     gs_a_final-psphi
                     gs_a_final-stufe
                     gs_a_final-wbslv
                     gs_a_final-verna
                     gs_a_final-stat
                     gs_a_final-plfaz
                     gs_a_final-plsez
                     gs_a_final-eprog
                     gs_a_final-gjahr

                     gs_a_final-total
                     gs_a_final-wlpitd
                     gs_a_final-wlpmtd
                     gs_a_final-wlpytd
                     gs_a_final-twaer
*                     gs_a_final-astnr "*Changed by KMB on 20.11.2019 CHG0159956 - FUT issue

        INTO lv_string SEPARATED BY gc_delim.
*BOC by KMB on 29.11.2019 CHG0159956 - Adding file size field

        IF p_fbrk IS NOT INITIAL AND gv_check = 'X'.
          lv_strlen = strlen( lv_string ).
          ADD lv_strlen TO gv_file_size.
          IF lv_left_app_size GT gv_file_size.
            TRANSFER lv_string TO gv_path_a.
            CLEAR : gs_a_final, lv_string .
            lv_left_app_size = lv_left_app_size - gv_file_size.
            lv_left_app_size = abs( lv_left_app_size ).
          ELSE.
            CLOSE DATASET gv_path_a.
            lv_index_new = lv_index_a + 1.

            REPLACE lv_index_a IN gv_path_a WITH lv_index_new.
            lv_index_a = lv_index_a + 1.
            CLEAR gv_flag1.

            OPEN DATASET gv_path_a FOR OUTPUT IN TEXT MODE
                        ENCODING DEFAULT MESSAGE lv_msg.
            IF     ( sy-subrc NE 0 ).
              WRITE:   /001  text-006, lv_msg.
              MESSAGE  e000(zfi01) WITH text-006 lv_msg.
            ENDIF.

            CLEAR ls_output.
            CONCATENATE text-a01 text-a02 text-a03 text-a04 text-a05 text-a19 text-a20 text-a21 text-a06
                  text-a07 text-a08 text-a09 text-a17 text-a10 text-a11 text-a12

                  text-a13 text-a14 text-a15
           INTO ls_output SEPARATED BY gc_delim.
            TRANSFER ls_output TO gv_path_a.

            CONCATENATE  gs_a_final-vbukr
                         gs_a_final-prart
                         gs_a_final-pratx
                         gs_a_final-pspnr
                         gs_a_final-post1
                         gs_a_final-psphi
                         gs_a_final-stufe
                         gs_a_final-wbslv
                         gs_a_final-verna
                         gs_a_final-stat
                         gs_a_final-plfaz
                         gs_a_final-plsez
                         gs_a_final-eprog
                         gs_a_final-gjahr
                         gs_a_final-total
                         gs_a_final-wlpitd
                         gs_a_final-wlpmtd
                         gs_a_final-wlpytd
                         gs_a_final-twaer
            INTO lv_string SEPARATED BY gc_delim.

            TRANSFER lv_string TO gv_path_a.
            CLEAR : gs_a_final, gv_file_size.
            CLEAR lv_left_app_size.
            lv_strlen = strlen( lv_string ).
            ADD lv_strlen TO gv_file_size.

            lv_left_app_size = gv_file_size_brk - gv_file_size.
            gv_check = 'X'.
            CLEAR lv_string.

          ENDIF.
        ELSE.
          TRANSFER lv_string TO gv_path_a.
          CLEAR : gs_a_final, lv_string , gv_file_size.
        ENDIF.

*        TRANSFER lv_string TO gv_path_a.
*        CLEAR : gs_a_final, lv_string .
*EOC by KMB on 29.11.2019 CHG0159956 - Adding file size field
      ENDLOOP.
    ENDIF.

    CLOSE DATASET gv_path_a.
    CLEAR gv_file_size. "Added by KMB on 02.12.2019 CHG0159956 - FUT issue fixing

  ENDIF.
ENDFORM.                    " F_ALL_TRANSFER_DATA
*&---------------------------------------------------------------------*
*&      Form  F_ALL_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_A_NEW  text
*----------------------------------------------------------------------*
FORM f_all_download  USING value(gt_a_fin) LIKE gt_a_new value(lv_file) LIKE lv_file.

  DATA: lt_tab_data       TYPE STANDARD TABLE OF string,
      ls_tab_data       TYPE string.

  CLEAR lt_tab_data.

  CONCATENATE text-a01 text-a02 text-a03 text-a04 text-a05 text-a19 text-a20 text-a21 text-a06
                  text-a07 text-a08 text-a09 text-a17 text-a10 text-a11 text-a12

                  "*BOC by KMB on 20.11.2019 CHG0159956 - FUT issue
*                  text-a13 text-a14 text-a15 text-a16
                  text-a13 text-a14 text-a15
                  "*EOC by KMB on 20.11.2019 CHG0159956 - FUT issue
      INTO ls_tab_data SEPARATED BY gc_delim.
  APPEND ls_tab_data TO lt_tab_data.

  CLEAR ls_tab_data.
  CLEAR gs_a_final.

  LOOP AT  gt_a_fin INTO gs_a_final.

    CLEAR ls_tab_data.

    CONCATENATE  gs_a_final-vbukr
                     gs_a_final-prart
                     gs_a_final-pratx
                     gs_a_final-pspnr
                     gs_a_final-post1
                     gs_a_final-psphi
                     gs_a_final-stufe
                     gs_a_final-wbslv
                     gs_a_final-verna
                     gs_a_final-stat
                     gs_a_final-plfaz
                     gs_a_final-plsez
                     gs_a_final-eprog
                     gs_a_final-gjahr

                     gs_a_final-total
                     gs_a_final-wlpitd
                     gs_a_final-wlpmtd
                     gs_a_final-wlpytd
                     gs_a_final-twaer
*                     gs_a_final-astnr "changed by KMB on 20.11.2019 CHG0159956 - FUT issue
         INTO ls_tab_data SEPARATED BY gc_delim.

    APPEND ls_tab_data TO lt_tab_data.
    CLEAR : gs_a_final.
  ENDLOOP.

  IF lt_tab_data IS NOT INITIAL.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_file
      TABLES
        data_tab                = lt_tab_data
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
    IF sy-subrc <> 0.
      MESSAGE text-009 TYPE gc_e.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_ALL_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  F_FOR_DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_F_NEW  text
*----------------------------------------------------------------------*
FORM f_for_download  USING value(gt_f_fin) LIKE gt_f_new value(lv_file) LIKE lv_file.
  DATA: lt_tab_data     TYPE STANDARD TABLE OF string,
      ls_tab_data       TYPE string.

  CLEAR lt_tab_data.

  CONCATENATE text-f01 text-f07 text-f08 text-f10 text-f02 text-f03
  text-f09 text-f04 text-f05 text-f06
  INTO ls_tab_data SEPARATED BY gc_delim.

  APPEND ls_tab_data TO lt_tab_data.

  CLEAR : gs_f_final, ls_tab_data.

  LOOP AT  gt_f_fin INTO gs_f_final.

    CLEAR ls_tab_data.
    CONCATENATE gs_f_final-pspnr
                    gs_f_final-psphi
                    gs_f_final-stufe
                    gs_f_final-wbslv
                    gs_f_final-wrttp
                    gs_f_final-versn
                    gs_f_final-gjahr
*                        gs_f_final-wlpitd
*                        gs_f_final-wlpytd
                    gs_f_final-period
                    gs_f_final-amount
                    gs_f_final-twaer
         INTO ls_tab_data SEPARATED BY gc_delim.

    APPEND ls_tab_data TO lt_tab_data.
    CLEAR : gs_f_final, ls_tab_data.
  ENDLOOP.

*BOC by KMB on 20.11.2019 CHG0159956 - FUT issue fixing
*  IF ls_tab_data IS NOT INITIAL.
  IF lt_tab_data IS NOT INITIAL.
*EOC by KMB on 20.11.2019 CHG0159956 - FUT issue fixing
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = lv_file
      TABLES
        data_tab                = lt_tab_data
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
    IF sy-subrc <> 0.
      MESSAGE text-009 TYPE gc_e.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_FOR_DOWNLOAD
