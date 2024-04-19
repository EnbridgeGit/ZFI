*&---------------------------------------------------------------------*
*&  Include           ZFI_UPDATE_SETTLEMENT_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_new.
  REFRESH:gt_prps,gt_jest,gt_cobrb.
  SELECT pspnr objnr belkz FROM prps INTO TABLE gt_prps
                               WHERE pspnr IN s_pspnr
                               AND   belkz = 'X'.
  IF sy-subrc IS INITIAL.
    SELECT objnr stat FROM jest INTO TABLE gt_jest
                           FOR ALL ENTRIES IN gt_prps
                           WHERE objnr = gt_prps-objnr
                           AND   inact = space.
    IF sy-subrc IS INITIAL.
      SORT gt_jest BY objnr.
    ENDIF.
    SORT gt_prps BY pspnr.
    IF p_hkont IS INITIAL.
      p_hkont = '0000115998'.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_hkont
        IMPORTING
          output = p_hkont.
    ENDIF.
    CLEAR:gv_kostl.
    SELECT SINGLE kostl INTO gv_kostl FROM tka3a WHERE bukrs = 'UGL'
      AND kstar = p_hkont.

    SELECT objnr konty hkont FROM cobrb INTO TABLE gt_cobrb
                             FOR ALL ENTRIES IN gt_prps
                             WHERE objnr = gt_prps-objnr.
*                             AND   KONTY = 'SK'.
    IF sy-subrc IS INITIAL.
      SORT gt_cobrb BY objnr.
    ENDIF.
  ELSE.
    MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SRULE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_srule_new .

  TYPES :BEGIN OF lty_cobrb,
         objnr TYPE j_objnr,
         lfdnr TYPE br_lfdnr,
         hkont TYPE saknr,
         konty TYPE konty,
         END OF lty_cobrb.
  DATA:lt_cobrb TYPE TABLE OF lty_cobrb,
       lv_pspnr(24) TYPE c,
       ls_cobrb TYPE lty_cobrb.
  REFRESH:gt_insert.
  LOOP AT gt_prps INTO  gs_prps.
    READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
    IF sy-subrc IS INITIAL AND gs_jest-stat NE 'I0046'.
      READ TABLE gt_cobrb INTO gs_cobrb WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        gs_insert-extnr = '001'.
        gs_insert-konty = 'SK'.
        gs_insert-perbz = 'GES'.
        gs_insert-lfdnr = '001'.
        gs_insert-bukrs = 'UGL'.
        gs_insert-avorg = 'KOAO'.
        gs_insert-kokrs = '10'.
        gs_insert-prozs = '100.00'.
        gs_insert-objnr = gs_prps-objnr.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
          EXPORTING
            input  = gs_prps-pspnr
          IMPORTING
            output = lv_pspnr.
        IF lv_pspnr+10(1) = '9'.
          IF p_hkont IS INITIAL.
            p_hkont = '0000115998'.
          ENDIF.
        ELSE.
          IF p_hkont IS INITIAL.
            p_hkont = '0000115996'.
          ENDIF.
        ENDIF.
        IF p_hkont IS INITIAL.
          p_hkont = '0000115996'.
        ENDIF.
        gs_insert-hkont = p_hkont.
        gs_insert-kostl = gv_kostl. "'0000020310'.
        CONCATENATE 'SK01  000' gs_insert-hkont INTO gs_insert-rec_objnr1.
        APPEND gs_insert TO gt_insert.
        CLEAR:gs_insert.
      ELSE.
        IF p_hkont IS INITIAL.
          p_hkont = '0000115996'.
        ENDIF.
        gs_output-pspnr = gs_prps-pspnr.
        gs_output-hkont = p_hkont.
*        gs_output-message = ' G/L Account number already exist'.
        gs_output-message = ' Its not a new project to update G/L account'.
        APPEND gs_output TO gt_output.
        CLEAR:gs_output.
      ENDIF.
    ELSE.
      IF p_hkont IS INITIAL.
        p_hkont = '0000115996'.
      ENDIF.
      gs_output-pspnr = gs_prps-pspnr.
      gs_output-hkont = p_hkont.
      gs_output-message = ' Closed WBS number'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ENDIF.
    CLEAR:gs_insert,gs_prps,gs_cobrb,gs_jest.
  ENDLOOP.

  IF gt_insert[] IS NOT INITIAL.
    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
        t_cobrb_insert    = gt_insert
      exceptions
        srule_utask_error = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 5 SECONDS.
    CLEAR:lt_cobrb[].
    SELECT objnr lfdnr hkont konty FROM cobrb INTO TABLE lt_cobrb
                             FOR ALL ENTRIES IN gt_insert
                             WHERE objnr = gt_insert-objnr
                             AND konty = 'SK'.
    IF sy-subrc IS INITIAL.
      SORT lt_cobrb BY objnr.
    ENDIF.
    LOOP AT gt_insert INTO gs_insert.
      READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_insert-objnr
       lfdnr = gs_insert-lfdnr
       konty = 'SK'.
      IF sy-subrc IS INITIAL.
        gs_output-message = 'Updated Sucessfully'.
      ELSE.
        gs_output-message = 'Update Failed'.
      ENDIF.
      gs_output-hkont = gs_insert-hkont.
      READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_insert-objnr.

      IF sy-subrc IS INITIAL.
        gs_output-pspnr = gs_prps-pspnr.
      ENDIF.
      APPEND gs_output TO gt_output.
      CLEAR:gs_insert,ls_cobrb,gs_prps,gs_output.
    ENDLOOP.
  ENDIF.
*BEGIN OF INSERT FOR SETC CHANGES BY KBANERJEE
  PERFORM f_set_wbsstat_setc.
*END OF INSERT FOR SETC CHANGES BY KBANERJEE
ENDFORM.                    " UPDATE_SRULE
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  PERFORM field_cat USING 'PSPNR' 'WBS Element' .
  PERFORM field_cat USING 'HKONT' 'G/L account Number' .
  PERFORM field_cat USING 'ANLN1' 'FXA Number' .
  PERFORM field_cat USING 'MESSAGE' 'Message' .
ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0316   text
*      -->P_0317   text
*----------------------------------------------------------------------*
FORM field_cat  USING p_field p_label.

  fieldcatalog-fieldname   = p_field.
  fieldcatalog-seltext_m   = p_label.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

ENDFORM.                    " FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
  IF gt_output IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        it_fieldcat        = fieldcatalog[]
      TABLES
        t_outtab           = gt_output
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc EQ 0.
      " DISPLAY REPORT OUTPUT
    ENDIF.
  ENDIF.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SRULE_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_srule_exist.
  TYPES :BEGIN OF lty_cobrb,
         objnr TYPE j_objnr,
         lfdnr TYPE br_lfdnr,
         hkont TYPE saknr,
         END OF lty_cobrb.
  DATA:lt_cobrb TYPE TABLE OF lty_cobrb,
       lv_pspnr(24) TYPE c,
       lv_index TYPE sy-tabix,
       lv_anln1(12) TYPE c,
       ls_cobrb TYPE lty_cobrb.
  IF p_perbz IS INITIAL.
    p_perbz = 'VOR'.
  ENDIF.
  gt_cobrb_ex[] = gt_cobrb_o1[].
  SORT gt_cobrb_ex[] BY objnr.
  SORT gt_prps[] BY objnr.
  LOOP AT gt_prps INTO  gs_prps.
    READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
    IF sy-subrc IS INITIAL AND gs_jest-stat NE 'I0046'.
      READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr.
      IF sy-subrc IS INITIAL.
        READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                                         konty = 'SK'.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE gt_cobrb_o1 INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                                         konty = 'AN'
                                                         perbz = 'GES'.
          IF gs_cobrb_o-gabpe IS NOT INITIAL AND gs_cobrb_o-gabja IS NOT INITIAL.
            READ TABLE gt_cobrb_o1 INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                                            hkont = p_hkont.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr.
              IF sy-subrc IS INITIAL.
                gs_insert-extnr = gs_cobrb_o-extnr + 1.
                gs_insert-lfdnr = gs_cobrb_o-lfdnr + 1.
              ENDIF.
              gs_insert-betrr = gs_cobrb_o-betrr.
              gs_insert-bwaer = gs_cobrb_o-bwaer.
              gs_insert-brest = gs_cobrb_o-brest.
              gs_insert-konty = 'SK'.
              gs_insert-perbz = p_perbz. " 'GES'.
              gs_insert-gabja = p_year.   "SY-DATUM+0(4).
              gs_insert-gabpe = p_period. "SY-DATUM+4(2).
              gs_insert-bukrs = 'UGL'.
              gs_insert-avorg = 'KOAO'.
              gs_insert-kokrs = '10'.
              gs_insert-prozs = '100.00'.
              gs_insert-objnr = gs_prps-objnr.
              CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                EXPORTING
                  input  = gs_prps-pspnr
                IMPORTING
                  output = lv_pspnr.
              IF lv_pspnr+10(1) = '9'.
                IF p_hkont IS INITIAL.
                  p_hkont = '0000115998'.
                ENDIF.
              ELSE.
                IF p_hkont IS INITIAL.
                  p_hkont = '0000115996'.
                ENDIF.
              ENDIF.
              IF p_hkont IS INITIAL.
                p_hkont = '0000115996'.
              ENDIF.
              gs_insert-hkont = p_hkont. "'0000303200'.
              gs_insert-kostl = gv_kostl. "'0000020310'.
              CONCATENATE 'SK01  000' gs_insert-hkont INTO gs_insert-rec_objnr1.
              APPEND gs_insert TO gt_insert.
              CLEAR:gs_insert.
*              data:lv_num type i.
*              DESCRIBE TABLE gt_cobrb_o LINES lv_num.
              MOVE-CORRESPONDING gs_cobrb_o TO gs_update.
              IF p_period = '1'.
                gs_update-gbisp = '12'.
                gs_update-gbisj = p_year - 1.
              ELSE.
                gs_update-gbisp = p_period - 1.
                gs_update-gbisj = p_year.
              ENDIF.
              APPEND gs_update TO gt_update.
              CLEAR:gs_update.
            ELSE.
              IF p_hkont IS INITIAL.
                p_hkont = '0000115996'.
              ENDIF.
              gs_output-pspnr = gs_prps-pspnr.
              gs_output-hkont = p_hkont.
              gs_output-message = 'Dummy G/L account already updated'.
              APPEND gs_output TO gt_output.
              CLEAR:gs_output.
            ENDIF.
          ELSE.
            CLEAR: lv_anln1.
            IF sy-sysid = 'Q02' OR sy-sysid = 'P01'.
              lv_anln1 = '475010199999'.
            ELSEIF sy-sysid = 'D22' OR sy-sysid = 'D30'.
              lv_anln1 = '475010100200'.
            ELSEIF sy-sysid = 'S01'.
              lv_anln1 = '475010404200'.
            ENDIF.

            READ TABLE gt_cobrb_o1 INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                                            anln1 = lv_anln1.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE gt_cobrb_o1 INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                                              hkont = p_hkont.
              IF sy-subrc IS NOT INITIAL.
                READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr.
                IF sy-subrc IS INITIAL.
                  gs_insert-extnr = gs_cobrb_o-extnr + 1.
                  gs_insert-lfdnr = gs_cobrb_o-lfdnr + 1.
                ENDIF.
                gs_insert-betrr = gs_cobrb_o-betrr.
                gs_insert-bwaer = gs_cobrb_o-bwaer.
                gs_insert-brest = gs_cobrb_o-brest.
                gs_insert-konty = 'SK'.
                gs_insert-perbz = p_perbz.
                gs_insert-gabja = p_year.   "SY-DATUM+0(4).
                gs_insert-gabpe = p_period. "SY-DATUM+4(2).
                gs_insert-bukrs = 'UGL'.
                gs_insert-avorg = 'KOAO'.
                gs_insert-kokrs = '10'.
                gs_insert-prozs = '100.00'.
                gs_insert-objnr = gs_prps-objnr.
                CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                  EXPORTING
                    input  = gs_prps-pspnr
                  IMPORTING
                    output = lv_pspnr.
                IF lv_pspnr+10(1) = '9'.
                  IF p_hkont IS INITIAL.
                    p_hkont = '0000115998'.
                  ENDIF.
                ELSE.
                  IF p_hkont IS INITIAL.
                    p_hkont = '0000115996'.
                  ENDIF.
                ENDIF.
                IF p_hkont IS INITIAL.
                  p_hkont = '0000115996'.
                ENDIF.
                gs_insert-hkont = p_hkont. "'0000303200'.
                gs_insert-kostl = gv_kostl. "'0000020310'.
                CONCATENATE 'SK01  000' gs_insert-hkont INTO gs_insert-rec_objnr1.
                APPEND gs_insert TO gt_insert.
                CLEAR:gs_insert.
***- Logic add TO Period and TO Year for all SRULE RECORDS
                CLEAR:lv_index.
                READ TABLE gt_cobrb_ex INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  lv_index = sy-tabix.
                  LOOP AT gt_cobrb_ex INTO gs_cobrb_o FROM lv_index.
                    IF gs_cobrb_o-objnr <> gs_prps-objnr.
                      EXIT.
                    ENDIF.
                    MOVE-CORRESPONDING gs_cobrb_o TO gs_update.
                    IF p_period = '1'.
                      gs_update-gbisp = '12'.
                      gs_update-gbisj = p_year - 1.
                    ELSE.
                      gs_update-gbisp = p_period - 1.
                      gs_update-gbisj = p_year.
                    ENDIF.
                    IF gs_update-gbisp = '1'.
                      gs_update-gabpe = '12'.
                      gs_update-gabja = gs_update-gbisj - 1.
                    ELSE.
                      gs_update-gabpe = gs_update-gbisp - 1.
                      gs_update-gabja = gs_update-gbisj.
                    ENDIF.
                    APPEND gs_update TO gt_update.
                    CLEAR:gs_update.
                  ENDLOOP.
                ENDIF.
              ELSE.
                IF p_hkont IS INITIAL.
                  p_hkont = '0000115996'.
                ENDIF.
                gs_output-pspnr = gs_prps-pspnr.
                gs_output-hkont = p_hkont.
                gs_output-message = 'Dummy G/L account already updated'.
                APPEND gs_output TO gt_output.
                CLEAR:gs_output.
              ENDIF.
            ELSE.
              IF p_hkont IS INITIAL.
                p_hkont = '0000115996'.
              ENDIF.
              gs_output-pspnr = gs_prps-pspnr.
              gs_output-hkont = p_hkont.
              gs_output-message = 'Settlement Not completed'.
              APPEND gs_output TO gt_output.
              CLEAR:gs_output.
            ENDIF.
          ENDIF.
        ELSE.
          IF p_hkont IS INITIAL.
            p_hkont = '0000115996'.
          ENDIF.
          gs_output-pspnr = gs_prps-pspnr.
          gs_output-hkont = p_hkont.
          gs_output-message = ' G/L Account Already exist'.
          APPEND gs_output TO gt_output.
          CLEAR:gs_output.
        ENDIF.
      ELSE.
        IF p_hkont IS INITIAL.
          p_hkont = '0000115996'.
        ENDIF.
        gs_output-pspnr = gs_prps-pspnr.
        gs_output-hkont = p_hkont.
        gs_output-message = 'No settlemnt rule exist'.
        APPEND gs_output TO gt_output.
        CLEAR:gs_output.
      ENDIF.
    ELSE.
      IF p_hkont IS INITIAL.
        p_hkont = '0000115996'.
      ENDIF.
      gs_output-pspnr = gs_prps-pspnr.
      gs_output-hkont = p_hkont.
      gs_output-message = ' Closed WBS number'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ENDIF.
    CLEAR:gs_insert,gs_prps,gs_cobrb_o,gs_jest.
  ENDLOOP.

  IF gt_insert[] IS NOT INITIAL.
    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
        t_cobrb_insert    = gt_insert
        t_cobrb_update    = gt_update
      exceptions
        srule_utask_error = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 5 SECONDS.
    CLEAR:lt_cobrb[].
    SELECT objnr lfdnr hkont FROM cobrb INTO TABLE lt_cobrb
                             FOR ALL ENTRIES IN gt_insert
                             WHERE objnr = gt_insert-objnr
                             AND hkont = p_hkont.
    IF sy-subrc IS INITIAL.
      SORT lt_cobrb BY objnr." hkont.
    ENDIF.
    LOOP AT gt_insert INTO gs_insert.
      READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_insert-objnr
                                                 lfdnr = gs_insert-lfdnr.
      IF sy-subrc IS INITIAL.
        gs_output-message = 'Updated Sucessfully'.
      ELSE.
        gs_output-message = 'Update Failed'.
      ENDIF.
      gs_output-hkont = gs_insert-hkont.
      READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_insert-objnr.
      IF sy-subrc IS INITIAL.
        gs_output-pspnr = gs_prps-pspnr.
      ENDIF.
      APPEND gs_output TO gt_output.
      CLEAR:gs_insert,ls_cobrb,gs_prps,gs_output.
    ENDLOOP.
  ENDIF.
*BEGIN OF INSERT FOR SETC CHANGES BY KBANERJEE
  PERFORM f_set_wbsstat_setc.
*END OF INSERT FOR SETC CHANGES BY KBANERJEE
ENDFORM.                    " UPDATE_SRULE_EXIST
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_exist.
  CLEAR: gt_prps[],gt_jest[],gt_cobrb_o1[],gt_cobrb_o[].
  SELECT pspnr objnr belkz FROM prps INTO TABLE gt_prps
                                WHERE pspnr IN s_pspnr
                                AND   belkz = 'X'.
  IF sy-subrc IS INITIAL.
    SELECT objnr stat FROM jest INTO TABLE gt_jest
                           FOR ALL ENTRIES IN gt_prps
                           WHERE objnr = gt_prps-objnr
                           AND   inact = space.
    IF sy-subrc IS INITIAL.
      SORT gt_jest BY objnr.
    ENDIF.
    SORT gt_prps BY pspnr.
    IF p_hkont IS INITIAL.
      p_hkont = '0000115996'.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_hkont
        IMPORTING
          output = p_hkont.
    ENDIF.
    CLEAR:gv_kostl.
    SELECT SINGLE kostl INTO gv_kostl FROM tka3a WHERE bukrs = 'UGL'
      AND kstar = p_hkont.
    SELECT * FROM cobrb INTO TABLE gt_cobrb_o
                              FOR ALL ENTRIES IN gt_prps
                              WHERE objnr = gt_prps-objnr.
    IF sy-subrc IS INITIAL.
*      delete gt_cobrb_o WHERE GABPE eq ' '
*                        and   GABJA eq ' '.
      gt_cobrb_o1 = gt_cobrb_o.
      SORT gt_cobrb_o BY objnr extnr DESCENDING.
      SORT gt_cobrb_o1 BY objnr anln1 DESCENDING.
    ENDIF.
  ELSE.
    MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " GET_DATA_EXIST
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_OPEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_open .
  CLEAR:gt_prps[],gt_jest[],gt_cobrb_o[],gt_cobrb_o1[],gt_cobrb_o2[].
  SELECT pspnr objnr belkz FROM prps INTO TABLE gt_prps
*
                               WHERE pspnr IN s_pspnr "= GT_PRHI-POSNR
                               AND   belkz = 'X'.
  IF sy-subrc IS INITIAL.
    SORT gt_prps BY objnr.
    SELECT objnr stat FROM jest INTO TABLE gt_jest
                           FOR ALL ENTRIES IN gt_prps
                           WHERE objnr = gt_prps-objnr
                           AND   ( stat  = 'I0002' OR stat  = 'I0045' OR stat = 'I0001' )
                           AND   inact = space.
    IF sy-subrc IS INITIAL.
      SORT gt_jest BY objnr.

*      SELECT OBJNR LFDNR PERBZ BETRR BWAER BREST KONTY HKONT ANLN1
      SELECT *
                              FROM cobrb INTO TABLE gt_cobrb_o
                              FOR ALL ENTRIES IN gt_jest
                              WHERE objnr = gt_jest-objnr.
      IF sy-subrc IS INITIAL.
        gt_cobrb_o1 = gt_cobrb_o.
        gt_cobrb_o2 = gt_cobrb_o.
        SORT gt_cobrb_o BY objnr anln1 DESCENDING.
        SORT gt_cobrb_o1 BY objnr anln1 ASCENDING.
        SORT gt_cobrb_o2  BY objnr extnr DESCENDING.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " GET_DATA_OPEN
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SRULE_OPEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*-------------
*---------------------------------------------------------*
FORM update_srule_open .
  TYPES :BEGIN OF lty_cobrb,
         objnr TYPE j_objnr,
         lfdnr TYPE br_lfdnr,
         hkont TYPE saknr,
         anln1 TYPE anln1,
         END OF lty_cobrb.
  TYPES:BEGIN OF ty_cobrb,
       objnr TYPE j_objnr,
       anln1 TYPE cobrb-anln1,
        END OF ty_cobrb.
  DATA:lt_cobrb TYPE TABLE OF lty_cobrb,
        lt_cobrb_f TYPE TABLE OF ty_cobrb,
        ls_cobrb_f TYPE ty_cobrb,
        lv_flag TYPE flag,
        ls_cobrb TYPE lty_cobrb.
  DATA:lt_cobrb_u TYPE TABLE OF lty_cobrb,
     ls_cobrb_u TYPE lty_cobrb.
  IF p_anln1 IS INITIAL.
    p_anln1 = '473010100600'.
  ENDIF.

  LOOP AT gt_prps INTO  gs_prps.
    READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr.
      IF sy-subrc IS INITIAL.
        READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                                   konty = 'SK'.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE gt_cobrb_o1 INTO gs_cobrb_o1 WITH KEY objnr = gs_prps-objnr
                                                 konty = 'AN'.
          IF ( gs_cobrb_o1-anln1+0(1) = '1' OR gs_cobrb_o1-anln1+0(1) = '2' ) AND gs_cobrb_o1-perbz = 'JHR'.
            CLEAR:lv_flag.
            LOOP AT gt_cobrb_o2 INTO gs_cobrb_o2 WHERE objnr = gs_prps-objnr.
              READ TABLE gt_cobrb_o2 INTO gs_cobrb_o1 WITH KEY objnr = gs_prps-objnr
                                                               anln1 = p_anln1.
              IF sy-subrc IS NOT INITIAL.
                IF lv_flag IS INITIAL.
                  lv_flag = 'X'.
                  READ TABLE gt_cobrb_o2 INTO gs_cobrb_o2 WITH KEY objnr = gs_prps-objnr.
                  IF sy-subrc IS INITIAL.
                    gs_insert-extnr = gs_cobrb_o2-extnr + 1.
                    gs_insert-lfdnr = gs_cobrb_o2-lfdnr + 1.
                  ENDIF.
                  gs_insert-objnr = gs_cobrb_o2-objnr.
                  gs_insert-betrr = gs_cobrb_o2-betrr.
                  gs_insert-bwaer = gs_cobrb_o2-bwaer.
                  gs_insert-brest = gs_cobrb_o2-brest.
                  gs_insert-gabpe = p_period.
                  gs_insert-gabja = p_year.
                  gs_insert-konty = 'AN'.
                  gs_insert-perbz = 'GES'.
                  gs_insert-bukrs = 'UGL'.
                  gs_insert-avorg = 'KOAO'.
                  gs_insert-kokrs = '10'.
                  gs_insert-prozs = '100.00'.
                  IF p_anln1 IS INITIAL.
                    p_anln1 = '473010100600'.
                  ENDIF.
                  gs_insert-anln1 = p_anln1.
                  gs_insert-anln2 = '0'.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = gs_insert-anln1
                    IMPORTING
                      output = gs_insert-anln1.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = gs_insert-anln2
                    IMPORTING
                      output = gs_insert-anln2.
                  CONCATENATE 'AN01  ' gs_insert-anln1 gs_insert-anln2
                                       INTO gs_insert-rec_objnr1.
                  APPEND gs_insert TO gt_insert.
                  CLEAR:gs_insert.
                  MOVE-CORRESPONDING gs_cobrb_o2 TO gs_update.
                  IF p_period = '1'.
                    gs_update-gbisp = '12'.
                    gs_update-gbisj = p_year - 1.
                  ELSE.
                    gs_update-gbisp = p_period - 1.
                    gs_update-gbisj = p_year.
                  ENDIF.
                  gs_update-prozs = '100.00'.
                  IF gs_cobrb_o2-anln1+0(1) = '1' OR gs_cobrb_o2-anln1+0(1) = '2'.
                    CLEAR:gs_update-gbisp,gs_update-gbisj.
                  ENDIF.
                  CLEAR:gs_update-gabpe,
                        gs_update-gabja.
                  APPEND gs_update TO gt_update.
                  CLEAR:gs_update.
                ELSE.
                  MOVE-CORRESPONDING gs_cobrb_o2 TO gs_update.
                  IF p_period = '1'.
                    gs_update-gbisp = '12'.
                    gs_update-gbisj = p_year - 1.
                  ELSE.
                    gs_update-gbisp = p_period - 1.
                    gs_update-gbisj = p_year.
                  ENDIF.
                  IF gs_cobrb_o2-anln1+0(1) = '1' OR gs_cobrb_o2-anln1+0(1) = '2'.
                    CLEAR:gs_update-gbisp,gs_update-gbisj.
                  ENDIF.
                  gs_update-prozs = '100.00'.
                  CLEAR:gs_update-gabpe,
                        gs_update-gabja.
                  APPEND gs_update TO gt_update.
                  CLEAR:gs_update.
                ENDIF.
              ELSE.
                gs_output-pspnr = gs_prps-pspnr.
                gs_insert-anln1 = p_anln1.
                gs_output-anln1 = gs_insert-anln1.
                gs_output-message = 'Dummy FXA Number already updated'.
                APPEND gs_output TO gt_output.
                CLEAR:gs_output.
                EXIT.
              ENDIF.
              CLEAR:gs_cobrb_o2.
            ENDLOOP.
          ELSE.
            gs_output-pspnr = gs_prps-pspnr.
            gs_insert-anln1 = p_anln1.
            gs_output-anln1 = gs_insert-anln1.
            gs_output-message = 'Update failed due to no settlement rule exist stating with 1 or 2'.
            APPEND gs_output TO gt_output.
            CLEAR:gs_output.
          ENDIF.
        ELSE.
          gs_output-pspnr = gs_prps-pspnr.
          gs_insert-anln1 = p_anln1.
          gs_output-anln1 = gs_insert-anln1.
          gs_output-message = 'Settlement rule updated with G/L acount number'.
          APPEND gs_output TO gt_output.
          CLEAR:gs_output.
        ENDIF.
      ELSE.
        gs_output-pspnr = gs_prps-pspnr.
        gs_insert-anln1 = p_anln1.
        gs_output-anln1 = gs_insert-anln1.
        gs_output-message = 'No settlement rule exist'.
        APPEND gs_output TO gt_output.
        CLEAR:gs_output.
      ENDIF.
    ELSE.
      gs_output-pspnr = gs_prps-pspnr.
      gs_insert-anln1 = p_anln1.
      gs_output-anln1 = gs_insert-anln1.
      gs_output-message = 'WBS Number not in REL/TECO status'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ENDIF.
    CLEAR:gs_prps,
gs_jest,
gs_cobrb_o,
gs_cobrb_o1,
gs_cobrb_o2.
  ENDLOOP.

  IF gt_insert[] IS NOT INITIAL OR gt_update[] IS NOT INITIAL.
    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
        t_cobrb_insert    = gt_insert
        t_cobrb_update    = gt_update
      exceptions
        srule_utask_error = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 10 SECONDS.
    CLEAR:lt_cobrb[].
    IF gt_insert IS NOT INITIAL.
      SELECT objnr lfdnr hkont anln1 FROM cobrb INTO TABLE lt_cobrb
                               FOR ALL ENTRIES IN gt_insert
                               WHERE objnr = gt_insert-objnr
                               AND anln1 = p_anln1.
      IF sy-subrc IS INITIAL.
        SORT lt_cobrb BY objnr." hkont.
      ENDIF.
    ENDIF.
    LOOP AT gt_insert INTO gs_insert.
      READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_insert-objnr
                                                 anln1 = gs_insert-anln1.

      IF sy-subrc IS INITIAL.
        gs_output-message = 'Updated Sucessfully'.
      ELSE.
        gs_output-message = 'Update Failed'.
      ENDIF.
      gs_output-hkont = gs_insert-hkont.
      gs_output-anln1 = gs_insert-anln1.
      READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_insert-objnr.
      IF sy-subrc IS INITIAL.
        gs_output-pspnr = gs_prps-pspnr.
      ENDIF.

      APPEND gs_output TO gt_output.
      CLEAR:gs_insert,ls_cobrb,gs_prps,gs_output.
    ENDLOOP.
  ENDIF.
*BEGIN OF INSERT FOR SETC CHANGES BY KBANERJEE
  PERFORM f_set_wbsstat_setc.
*END OF INSERT FOR SETC CHANGES BY KBANERJEE
ENDFORM.                    " UPDATE_SRULE_OPEN
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SRULE_AGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_srule_age .

  TYPES :BEGIN OF lty_cobrb,
       objnr TYPE j_objnr,
       lfdnr TYPE br_lfdnr,
       hkont TYPE saknr,
       anln1 TYPE anln1,
       END OF lty_cobrb.
  DATA:lt_cobrb TYPE TABLE OF lty_cobrb,
*       lv_pspnr(24) type c,
       ls_cobrb TYPE lty_cobrb.
  DATA:lv_count TYPE i,
       lv_pspnr(24) TYPE c.
  REFRESH:gt_insert.

  LOOP AT gt_prps INTO  gs_prps.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = gs_prps-pspnr
      IMPORTING
        output = lv_pspnr.
    IF lv_pspnr+10(1) = '9'.
      READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr.
        IF sy-subrc IS INITIAL.
          gt_cobrb_ot[] = gt_cobrb_o[].
          DELETE gt_cobrb_ot WHERE objnr NE gs_prps-objnr.
          DELETE gt_cobrb_ot WHERE konty NE 'SK'.
          DESCRIBE TABLE gt_cobrb_ot LINES lv_count.
          IF lv_count = 1.
            READ TABLE  gt_cobrb_ot INTO  gs_cobrb_ot INDEX 1.
            IF  gs_cobrb_ot-hkont NE p_hkont.
              READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                                       konty = 'SK'.
              IF sy-subrc IS INITIAL.
                READ TABLE gt_cobrb_o2 INTO gs_cobrb_o2 WITH KEY objnr = gs_prps-objnr.
                IF sy-subrc = 0.
                  gs_insert-extnr = gs_cobrb_o2-extnr + 1.
                  gs_insert-lfdnr = gs_cobrb_o2-lfdnr + 1.
                ENDIF.
                gs_insert-konty = 'SK'.
                gs_insert-perbz = 'GES'.
                gs_insert-bukrs = 'UGL'.
                gs_insert-avorg = 'KOAO'.
                gs_insert-kokrs = '10'.
                gs_insert-prozs = '100.00'.
                gs_insert-objnr = gs_prps-objnr.
                gs_insert-hkont = p_hkont.
                gs_insert-kostl = gv_kostl. "'0000020310'.
                gs_insert-gabpe = p_period.
                gs_insert-gabja = p_year.
                CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
                  EXPORTING
                    input  = gs_prps-pspnr
                  IMPORTING
                    output = lv_pspnr.
                IF p_hkont IS INITIAL.
                  p_hkont = '0000115998'.
                ENDIF.
                CONCATENATE 'SK01  000' gs_insert-hkont INTO gs_insert-rec_objnr1.
                APPEND gs_insert TO gt_insert.
                CLEAR gs_insert.
                MOVE-CORRESPONDING gs_cobrb_o TO gs_update.
                IF p_period = '1'.
                  gs_update-gbisp = '12'.
                  gs_update-gbisj = p_year - 1.
                ELSE.
                  gs_update-gbisp = p_period - 1.
                  gs_update-gbisj = p_year.
                ENDIF.
                gs_update-prozs = '100.00'.
                CLEAR:gs_update-gabpe,
                      gs_update-gabja.
                APPEND gs_update TO gt_update.
                CLEAR:gs_update.

              ELSE.
                IF p_hkont IS INITIAL.
                  p_hkont = '0000115998'.
                ENDIF.
                gs_output-pspnr = gs_prps-pspnr.
                gs_output-hkont = p_hkont.
                gs_output-message = ' G/L Account number already exist'.
                APPEND gs_output TO gt_output.
                CLEAR:gs_output.
              ENDIF.
            ELSE.
              IF p_hkont IS INITIAL.
                p_hkont = '0000115998'.
              ENDIF.
              gs_output-pspnr = gs_prps-pspnr.
              gs_output-hkont = p_hkont.
              gs_output-message = ' G/L Account number already exist'.
              APPEND gs_output TO gt_output.
              CLEAR:gs_output.
            ENDIF.
          ELSE.
            IF p_hkont IS INITIAL.
              p_hkont = '0000115998'.
            ENDIF.
            gs_output-pspnr = gs_prps-pspnr.
            gs_output-hkont = p_hkont.
            gs_output-message = 'G/L Account does not exist or more than one G/L account exist'.
            APPEND gs_output TO gt_output.
            CLEAR:gs_output.
          ENDIF.
        ELSE.
          IF p_hkont IS INITIAL.
            p_hkont = '0000115998'.
          ENDIF.
          gs_output-pspnr = gs_prps-pspnr.
          gs_output-hkont = p_hkont.
          gs_output-message = 'No settlement Rule exist'.
          APPEND gs_output TO gt_output.
          CLEAR:gs_output.
        ENDIF.
      ELSE.
        IF p_hkont IS INITIAL.
          p_hkont = '0000115998'.
        ENDIF.
        gs_output-pspnr = gs_prps-pspnr.
        gs_output-hkont = p_hkont.
        gs_output-message = ' WBS element is not in TECO or release status'.
        APPEND gs_output TO gt_output.
        CLEAR:gs_output.
      ENDIF.
      CLEAR:gs_insert,gs_prps,gs_cobrb,gs_jest.
    ELSE.
      IF p_hkont IS INITIAL.
        p_hkont = '0000115998'.
      ENDIF.
      gs_output-pspnr = gs_prps-pspnr.
      gs_output-hkont = p_hkont.
      gs_output-message = ' WBS element is not in 9 series'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ENDIF.
    CLEAR:gt_cobrb_ot[],gs_prps,gs_jest,gs_cobrb_o,gs_cobrb_ot.
  ENDLOOP.
  IF gt_insert[] IS NOT INITIAL OR gt_update[] IS NOT INITIAL.
    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
        t_cobrb_insert    = gt_insert
        t_cobrb_update    = gt_update
      exceptions
        srule_utask_error = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 10 SECONDS.
    CLEAR:lt_cobrb[].
    IF gt_insert IS NOT INITIAL.
      SELECT objnr lfdnr hkont anln1 FROM cobrb INTO TABLE lt_cobrb
                               FOR ALL ENTRIES IN gt_insert
                               WHERE objnr = gt_insert-objnr
                               AND hkont = p_hkont.
      IF sy-subrc IS INITIAL.
        SORT lt_cobrb BY objnr." hkont.
      ENDIF.
    ENDIF.
    LOOP AT gt_insert INTO gs_insert.
      READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_insert-objnr
                                                 hkont = p_hkont.

      IF sy-subrc IS INITIAL.
        gs_output-message = 'Updated Sucessfully'.
      ELSE.
        gs_output-message = 'Update Failed'.
      ENDIF.
      gs_output-hkont = gs_insert-hkont.
      gs_output-anln1 = gs_insert-anln1.
      READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_insert-objnr.
      IF sy-subrc IS INITIAL.
        gs_output-pspnr = gs_prps-pspnr.
      ENDIF.

      APPEND gs_output TO gt_output.
      CLEAR:gs_insert,ls_cobrb,gs_prps,gs_output.
    ENDLOOP.
  ENDIF.
*BEGIN OF INSERT FOR SETC CHANGES BY KBANERJEE
  PERFORM f_set_wbsstat_setc.
*END OF INSERT FOR SETC CHANGES BY KBANERJEE
ENDFORM.                    " UPDATE_SRULE_AGE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_DEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_del .
  REFRESH:gt_prps,gt_jest,gt_cobrb,gt_cobrb_o,gt_cobrb_o1,gt_cobrb_o2.
  SELECT pspnr objnr belkz FROM prps INTO TABLE gt_prps
                              " FOR ALL ENTRIES IN GT_PRHI
                               WHERE pspnr IN s_pspnr
                               AND   belkz = 'X'.
  IF sy-subrc IS INITIAL.
    SORT gt_prps BY objnr.
    SELECT objnr stat FROM jest INTO TABLE gt_jest
                           FOR ALL ENTRIES IN gt_prps
                           WHERE objnr = gt_prps-objnr
                           AND   inact = space.
    IF sy-subrc IS INITIAL.
      SORT gt_jest BY objnr.
    ENDIF.
    SORT gt_prps BY pspnr.
    IF p_hkont IS INITIAL.
      p_hkont = '0000115998'.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_hkont
        IMPORTING
          output = p_hkont.
    ENDIF.
    SELECT * FROM cobrb INTO TABLE gt_cobrb_o
                             FOR ALL ENTRIES IN gt_prps
                             WHERE objnr = gt_prps-objnr.
    IF sy-subrc IS INITIAL.
      gt_cobrb_o1[] = gt_cobrb_o[].
      gt_cobrb_o2[] = gt_cobrb_o[].
      SORT gt_cobrb_o BY objnr.
      SORT gt_cobrb_o1 BY objnr konty hkont ASCENDING lfdnr DESCENDING.
      SORT gt_cobrb_o2 BY objnr konty  anln1 ASCENDING lfdnr DESCENDING.
    ENDIF.
  ELSE.
    MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " GET_DATA_DEL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SRULE_DEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_srule_del .

  TYPES :BEGIN OF lty_cobrb,
     objnr TYPE j_objnr,
     lfdnr TYPE br_lfdnr,
     hkont TYPE saknr,
     anln1 TYPE anln1,
     END OF lty_cobrb.
  DATA:lt_cobrb TYPE TABLE OF lty_cobrb,
       ls_cobrb TYPE lty_cobrb.
  DATA:lv_anln1 TYPE cobrb-anln1,
       lv_hkont TYPE cobrb-hkont,
       lv_flag TYPE flag,
       lv_flag_p  TYPE flag.
  LOOP AT gt_prps INTO gs_prps.
    LOOP AT gt_cobrb_o1 INTO gs_cobrb_o WHERE objnr = gs_prps-objnr.
      lv_flag_p = 'X'.
      IF gs_cobrb_o-konty EQ 'AN'.
        IF lv_anln1 IS INITIAL.
          lv_anln1 = gs_cobrb_o-anln1.
        ELSE.
          IF lv_anln1 EQ gs_cobrb_o-anln1.
            APPEND gs_cobrb_o TO gt_delete.
            lv_flag = 'X'.
            CLEAR:gs_cobrb_o1.
          ELSE.
            lv_anln1 = gs_cobrb_o-anln1.
          ENDIF.
        ENDIF.
      ELSEIF gs_cobrb_o-konty EQ 'SK'.
        IF lv_hkont IS INITIAL.
          lv_hkont = gs_cobrb_o-hkont.
        ELSE.
          IF lv_hkont EQ gs_cobrb_o-hkont.
            APPEND gs_cobrb_o TO gt_delete.
            lv_flag = 'X'.
            CLEAR:gs_cobrb_o1.
          ELSE.
            lv_hkont = gs_cobrb_o-hkont.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lv_flag IS INITIAL AND lv_flag_p IS NOT INITIAL.
      gs_output-pspnr = gs_prps-pspnr.
      gs_output-message = 'No Duplicate records exist'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ELSEIF lv_flag_p IS INITIAL.
      gs_output-pspnr = gs_prps-pspnr.
      gs_output-message = 'No Settlement rule exist'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ENDIF.
    CLEAR:gs_cobrb_o,lv_anln1,lv_hkont,gs_prps,lv_flag,lv_flag_p.
  ENDLOOP.

  IF gt_delete[] IS NOT INITIAL.
    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
        t_cobrb_delete    = gt_delete
      exceptions
        srule_utask_error = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 10 SECONDS.
    CLEAR:lt_cobrb[].
    IF gt_delete IS NOT INITIAL.
      SELECT objnr lfdnr hkont anln1 FROM cobrb INTO TABLE lt_cobrb
                               FOR ALL ENTRIES IN gt_delete
                               WHERE objnr = gt_delete-objnr.
      IF sy-subrc IS INITIAL.
        SORT lt_cobrb BY objnr." hkont.
      ENDIF.
    ENDIF.

    LOOP AT gt_delete INTO gs_delete.
      IF gs_delete-anln1 IS NOT INITIAL.
        READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_delete-objnr
                                                   lfdnr = gs_delete-lfdnr
                                                   anln1 = gs_delete-anln1.
        IF sy-subrc IS NOT INITIAL.
          gs_output-message = 'Updated Sucessfully'.
        ELSE.
          gs_output-message = 'Update Failed'.
        ENDIF.
        gs_output-hkont = gs_delete-hkont.
        gs_output-anln1 = gs_delete-anln1.
        READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_delete-objnr.
        IF sy-subrc IS INITIAL.
          gs_output-pspnr = gs_prps-pspnr.
        ENDIF.

        APPEND gs_output TO gt_output.
      ELSEIF gs_delete-hkont IS NOT INITIAL.
        READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_delete-objnr
                                                   lfdnr = gs_delete-lfdnr
                                                   hkont = gs_delete-hkont.
        IF sy-subrc IS NOT INITIAL.
          gs_output-message = 'Updated Sucessfully'.
        ELSE.
          gs_output-message = 'Update Failed'.
        ENDIF.
        gs_output-hkont = gs_delete-hkont.
        gs_output-anln1 = gs_delete-anln1.
        READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_delete-objnr.
        IF sy-subrc IS INITIAL.
          gs_output-pspnr = gs_prps-pspnr.
        ENDIF.
        APPEND gs_output TO gt_output.
      ENDIF.
      CLEAR:gs_insert,ls_cobrb,gs_prps,gs_output.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " UPDATE_SRULE_DEL
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_WIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_wip .
  REFRESH:gt_prps,gt_jest,gt_cobrb.
  SELECT pspnr objnr belkz FROM prps INTO TABLE gt_prps
                               WHERE pspnr IN s_pspnr
                               AND   belkz = 'X'.
  IF sy-subrc IS INITIAL.
    SELECT objnr stat FROM jest INTO TABLE gt_jest
                           FOR ALL ENTRIES IN gt_prps
                           WHERE objnr = gt_prps-objnr
                           AND   inact = space.
    IF sy-subrc IS INITIAL.
      SORT gt_jest BY objnr.
    ENDIF.
    SORT gt_prps BY pspnr.
    SELECT * FROM cobrb INTO TABLE gt_cobrb_o
                             FOR ALL ENTRIES IN gt_prps
                             WHERE objnr = gt_prps-objnr.
    IF sy-subrc IS INITIAL.
      SORT gt_cobrb BY objnr.
    ENDIF.
  ELSE.
    MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " GET_DATA_WIP
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SRULE_WIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_srule_wip .

  TYPES :BEGIN OF lty_cobrb,
     objnr TYPE j_objnr,
     lfdnr TYPE br_lfdnr,
     hkont TYPE saknr,
     anln1 TYPE anln1,
     END OF lty_cobrb.
  DATA:lt_cobrb TYPE TABLE OF lty_cobrb,
       ls_cobrb TYPE lty_cobrb.
  DATA:lv_tabix TYPE sy-tabix,
        lv_lfdnr TYPE lfdnr,
       lv_pspnr(24) TYPE c.
  SORT gt_prps BY objnr.
  SORT gt_cobrb_o BY objnr.
  LOOP AT gt_prps INTO  gs_prps.
    READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
    IF sy-subrc IS INITIAL AND gs_jest-stat NE 'I0046'.
      READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        lv_tabix = sy-tabix.
        LOOP AT gt_cobrb_o INTO gs_cobrb_o FROM lv_tabix.
          IF gs_cobrb_o-objnr <> gs_prps-objnr.
            EXIT.
          ENDIF.
          IF gs_cobrb_o-hkont = '0000303200' OR gs_cobrb_o-hkont = '0000319500'
            OR gs_cobrb_o-hkont = '0000309501' OR gs_cobrb_o-hkont = '0000115994'.
            gs_cobrb_o-hkont = '0000115998'.
            gs_cobrb_o-gabja = p_year.   "SY-DATUM+0(4).
            gs_cobrb_o-gabpe = p_period. "SY-DATUM+4(2).
            APPEND gs_cobrb_o TO gt_update.
            lv_lfdnr = gs_cobrb_o-lfdnr - 1.
            READ TABLE gt_cobrb_o INTO gs_cobrb_o2 WITH KEY objnr = gs_cobrb_o-objnr
                                                            lfdnr = lv_lfdnr.
            IF sy-subrc IS INITIAL.
              IF p_period = '12'.
*                gs_cobrb_o2-GBISP = '1'.
*                gs_cobrb_o2-GBISJ = P_YEAR - 1.
                gs_cobrb_o2-gabja = p_year - 1.   "SY-DATUM+0(4).
                gs_cobrb_o2-gabpe = '1'. "SY-DATUM+4(2)
              ELSE.
*                gs_cobrb_o2-GBISP = p_period - 1.
*                gs_cobrb_o2-GBISJ = P_YEAR.
                gs_cobrb_o2-gabja = p_year.   "SY-DATUM+0(4).
                gs_cobrb_o2-gabpe = p_period - 1. "SY-DATUM+4(2)            ENDIF.
                APPEND gs_cobrb_o2 TO gt_update.
                CLEAR:gs_cobrb_o2.
              ENDIF.

              CLEAR:gs_cobrb_o.
            ELSE.
              gs_output-pspnr = gs_prps-pspnr.
              gs_output-hkont = p_hkont.
              gs_output-message = 'Update not required'.
              APPEND gs_output TO gt_output.
              CLEAR:gs_output.
            ENDIF.
          ENDIF.
          CLEAR:lv_lfdnr.
        ENDLOOP.
      ELSE.
        gs_output-pspnr = gs_prps-pspnr.
        gs_output-hkont = p_hkont.
        gs_output-message = 'No Settlement Rule Exist'.
        APPEND gs_output TO gt_output.
        CLEAR:gs_output.
      ENDIF.
    ELSE.
      gs_output-pspnr = gs_prps-pspnr.
      gs_output-hkont = p_hkont.
      gs_output-message = ' Closed WBS number'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ENDIF.

    CLEAR:gs_prps,gs_cobrb_o,gs_jest.
  ENDLOOP.

  IF gt_update[] IS NOT INITIAL.
    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
        t_cobrb_update    = gt_update
      exceptions
        srule_utask_error = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 10 SECONDS.
    IF gt_update[] IS NOT INITIAL.
      SELECT objnr lfdnr hkont anln1 FROM cobrb INTO TABLE lt_cobrb
                               FOR ALL ENTRIES IN gt_insert
                               WHERE objnr = gt_insert-objnr
                               AND hkont = '0000115998'.
      IF sy-subrc IS INITIAL.
        SORT lt_cobrb BY objnr." hkont.
      ENDIF.
    ENDIF.

    LOOP AT gt_update INTO gs_update.
      READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_update-objnr.

      IF sy-subrc IS INITIAL.
        gs_output-message = 'Updated Sucessfully'.
      ELSE.
        gs_output-message = 'Update Failed'.
      ENDIF.
      gs_output-hkont = gs_update-hkont.
      gs_output-anln1 = gs_update-anln1.
      READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_update-objnr.
      IF sy-subrc IS INITIAL.
        gs_output-pspnr = gs_prps-pspnr.
      ENDIF.

      APPEND gs_output TO gt_output.
      CLEAR:gs_update,ls_cobrb,gs_prps,gs_output.
    ENDLOOP.
  ENDIF.
*BEGIN OF INSERT FOR SETC CHANGES BY KBANERJEE
  PERFORM f_set_wbsstat_setc.
*END OF INSERT FOR SETC CHANGES BY KBANERJEE
ENDFORM.                    " UPDATE_SRULE_WIP

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_SR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_sr .
  CLEAR:gt_prps[],gt_jest[],gt_cobrb_o[],gt_cobrb_o1[],gt_cobrb_o2[].
  SELECT pspnr objnr belkz FROM prps INTO TABLE gt_prps
*
                               WHERE pspnr IN s_pspnr "= GT_PRHI-POSNR
                               AND   belkz = 'X'.
  IF sy-subrc IS INITIAL.
    SORT gt_prps BY objnr.
    SELECT objnr stat FROM jest INTO TABLE gt_jest
                           FOR ALL ENTRIES IN gt_prps
                           WHERE objnr = gt_prps-objnr
                           AND   ( stat  = 'I0002' OR stat  = 'I0045' )
                           AND   inact = space.
    IF sy-subrc IS INITIAL.
      SORT gt_jest BY objnr.
      SELECT * FROM cobrb INTO TABLE gt_cobrb_o
               FOR ALL ENTRIES IN gt_jest
               WHERE objnr = gt_jest-objnr.
      IF sy-subrc IS INITIAL.
        gt_cobrb_o1 = gt_cobrb_o.
        gt_cobrb_o2 = gt_cobrb_o.
        SORT gt_cobrb_o BY objnr anln1 DESCENDING.
        SORT gt_cobrb_o1 BY objnr anln1 ASCENDING.
        SORT gt_cobrb_o2  BY objnr extnr DESCENDING.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " GET_DATA_SR
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SRULE_SR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_srule_sr .

  TYPES :BEGIN OF lty_cobrb,
         objnr TYPE j_objnr,
         lfdnr TYPE br_lfdnr,
         hkont TYPE saknr,
         anln1 TYPE anln1,
         END OF lty_cobrb.
  TYPES:BEGIN OF ty_cobrb,
       objnr TYPE j_objnr,
       anln1 TYPE cobrb-anln1,
        END OF ty_cobrb.
  DATA:lt_cobrb TYPE TABLE OF lty_cobrb,
        lt_cobrb_f TYPE TABLE OF ty_cobrb,
        ls_cobrb_f TYPE ty_cobrb,
        lv_flag TYPE flag,
        ls_cobrb TYPE lty_cobrb.
  DATA:lt_cobrb_u TYPE TABLE OF lty_cobrb,
     ls_cobrb_u TYPE lty_cobrb.
  DATA:lv_count TYPE i.
  LOOP AT gt_prps INTO gs_prps.
    READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr.
      IF sy-subrc IS INITIAL.
        gt_cobrb_ot[] = gt_cobrb_o[].
        DELETE gt_cobrb_ot WHERE objnr NE gs_prps-objnr.
        DESCRIBE TABLE gt_cobrb_ot LINES lv_count.
        IF lv_count = 1.
          IF gs_cobrb_o-hkont IS NOT INITIAL.
            IF p_hkont IS NOT INITIAL.
*              MOVE GS_COBRB_O TO GS_UPDATE.
              MOVE-CORRESPONDING gs_cobrb_o TO gs_update.
              gs_update-hkont = p_hkont.
              APPEND gs_update TO gt_update.
              CLEAR:gs_update.
            ELSE.
              gs_output-pspnr = gs_prps-pspnr.
              gs_output-hkont = p_hkont.
              gs_output-message = 'G/L Account number is blank in Selection screen'.
              APPEND gs_output TO gt_output.
              CLEAR:gs_output.
            ENDIF.
          ELSE.
            IF gs_cobrb_o-anln1 IS NOT INITIAL.
              IF p_hkont IS NOT INITIAL.
*                MOVE GS_COBRB_O TO GS_UPDATE.
                MOVE-CORRESPONDING gs_cobrb_o TO gs_update.
                CLEAR:gs_update-anln1,gs_update-anln1,gs_update-rec_objnr1.
                gs_update-hkont = p_hkont.
                gs_update-konty = 'SK'.
                CONCATENATE 'SK01  000' gs_update-hkont INTO gs_update-rec_objnr1.
                APPEND gs_update TO gt_update.
                CLEAR:gs_update.
              ELSE.
                gs_output-pspnr = gs_prps-pspnr.
                gs_output-hkont = p_hkont.
                gs_output-message = 'G/L Account number is blank in Selection screen'.
                APPEND gs_output TO gt_output.
                CLEAR:gs_output.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          gs_output-pspnr = gs_prps-pspnr.
          gs_output-hkont = p_hkont.
          gs_output-message = 'Multiple settlement rules exist'.
          APPEND gs_output TO gt_output.
          CLEAR:gs_output.
        ENDIF.
      ELSE.
        gs_output-pspnr = gs_prps-pspnr.
        gs_output-hkont = p_hkont.
        gs_output-message = 'Settlement Rule does not exist'.
        APPEND gs_output TO gt_output.
        CLEAR:gs_output.
      ENDIF.
    ELSE.
      gs_output-pspnr = gs_prps-pspnr.
      gs_output-hkont = p_hkont.
      gs_output-message = 'No records found with Status TECO/REL'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ENDIF.
    CLEAR:gs_prps,gs_cobrb_o,gs_jest.
  ENDLOOP.
  IF gt_update[] IS NOT INITIAL.

    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
*       T_COBRB_INSERT    = GT_INSERT
        t_cobrb_update    = gt_update
      exceptions
        srule_utask_error = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 10 SECONDS.

    CLEAR:lt_cobrb[].
    IF gt_update[] IS NOT INITIAL.
      SELECT objnr lfdnr hkont anln1 FROM cobrb INTO TABLE lt_cobrb
                               FOR ALL ENTRIES IN gt_update
                               WHERE objnr = gt_update-objnr
                               AND anln1 = p_anln1.
      IF sy-subrc IS INITIAL.
        SORT lt_cobrb BY objnr." hkont.
      ENDIF.
    ENDIF.
    LOOP AT gt_update INTO gs_update.
      READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_update-objnr
                                                 hkont = p_hkont.
      IF sy-subrc IS INITIAL.
        gs_output-message = 'Updated Sucessfully'.
      ELSE.
        gs_output-message = 'Update Failed'.
      ENDIF.
      gs_output-hkont = gs_update-hkont.
      gs_output-anln1 = gs_update-anln1.
      READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_update-objnr.
      IF sy-subrc IS INITIAL.
        gs_output-pspnr = gs_prps-pspnr.
      ENDIF.

      APPEND gs_output TO gt_output.
      CLEAR:gs_update,ls_cobrb,gs_prps,gs_output.
    ENDLOOP.
  ENDIF.
*BEGIN OF INSERT FOR SETC CHANGES BY KBANERJEE
  PERFORM f_set_wbsstat_setc.
*END OF INSERT FOR SETC CHANGES BY KBANERJEE
ENDFORM.                    " UPDATE_SRULE_SR
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_DDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_ddate .

  CLEAR:gt_prps[],gt_jest[],gt_cobrb_o[],gt_cobrb_o1[],gt_cobrb_o2[].
  SELECT pspnr objnr belkz FROM prps INTO TABLE gt_prps
*
                               WHERE pspnr IN s_pspnr "= GT_PRHI-POSNR
                               AND   belkz = 'X'.
  IF sy-subrc IS INITIAL.
    SORT gt_prps BY objnr.
    SELECT objnr stat FROM jest INTO TABLE gt_jest
                           FOR ALL ENTRIES IN gt_prps
                           WHERE objnr = gt_prps-objnr
                           AND   ( stat  = 'I0002' OR stat  = 'I0045')
                           AND   inact = space.
    IF sy-subrc IS INITIAL.
      SORT gt_jest BY objnr.
      SELECT *
                              FROM cobrb INTO TABLE gt_cobrb_o
                              FOR ALL ENTRIES IN gt_jest
                              WHERE objnr = gt_jest-objnr.
      IF sy-subrc IS INITIAL.
        gt_cobrb_o1 = gt_cobrb_o.
        gt_cobrb_o2 = gt_cobrb_o.
        SORT gt_cobrb_o BY objnr anln1 DESCENDING.
        SORT gt_cobrb_o1 BY objnr anln1 ASCENDING.
        SORT gt_cobrb_o2  BY objnr extnr DESCENDING.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " GET_DATA_DDATE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SRULE_DDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_srule_ddate .


  TYPES :BEGIN OF lty_cobrb,
       objnr TYPE j_objnr,
       lfdnr TYPE br_lfdnr,
       gabja TYPE gabja,
       gabpe TYPE gabpe,
       gbisj TYPE gbisj,
       gbisp TYPE gbisp,
       hkont TYPE saknr,
       anln1 TYPE anln1,
       END OF lty_cobrb.
  DATA:lt_cobrb TYPE TABLE OF lty_cobrb,
*       lv_pspnr(24) type c,
       ls_cobrb TYPE lty_cobrb.
  DATA:lv_count TYPE i,
       lv_pspnr(24) TYPE c.
  REFRESH:gt_insert.

  LOOP AT gt_prps INTO  gs_prps.
    READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                                     hkont = p_hkont.
      IF sy-subrc IS INITIAL AND p_hkont IS NOT INITIAL.
        MOVE-CORRESPONDING gs_cobrb_o TO gs_update.
        gs_update-gbisp = p_period.
        gs_update-gbisj = p_year.
        gs_update-gabpe = p_per_f.
        gs_update-gabja = p_year_f.
        APPEND gs_update TO gt_update.
        CLEAR:gs_update.
      ELSE.
        READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                               anln1 = p_anln1.
        IF sy-subrc IS INITIAL AND p_anln1 IS NOT INITIAL.
          MOVE-CORRESPONDING gs_cobrb_o TO gs_update.
          gs_update-gbisp = p_period.
          gs_update-gbisj = p_year.
          gs_update-gabpe = p_per_f.
          gs_update-gabja = p_year_f.
          APPEND gs_update TO gt_update.
          CLEAR:gs_update.
        ELSE.
          gs_output-pspnr = gs_prps-pspnr.
          gs_output-hkont = p_hkont.
          gs_output-anln1 = p_anln1.
          gs_output-message = 'No settlement Rule exist with given FXA or G/L Number'.
          APPEND gs_output TO gt_output.
          CLEAR:gs_output.
        ENDIF.
      ENDIF.
    ELSE.
      gs_output-pspnr = gs_prps-pspnr.
      gs_output-hkont = p_hkont.
      gs_output-anln1 = p_anln1.
      gs_output-message = 'WBS element is not in TECO or release status'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ENDIF.
    CLEAR:gs_prps,gs_cobrb,gs_jest.
    CLEAR:gt_cobrb_ot[],gs_prps,gs_jest,gs_cobrb_o,gs_cobrb_ot.
  ENDLOOP.
  IF gt_update[] IS NOT INITIAL.
    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
*       T_COBRB_INSERT    = GT_INSERT
        t_cobrb_update    = gt_update
      exceptions
        srule_utask_error = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 5 SECONDS.
    CLEAR:lt_cobrb[].
    IF p_hkont IS NOT INITIAL.
      SELECT objnr lfdnr gabja gabpe gbisj gbisp hkont anln1 FROM cobrb INTO TABLE lt_cobrb
                               FOR ALL ENTRIES IN gt_update
                               WHERE objnr = gt_update-objnr
                               AND   hkont = p_hkont.
      IF sy-subrc IS INITIAL.
        SORT lt_cobrb BY objnr." hkont.
      ENDIF.
    ELSEIF p_anln1 IS NOT INITIAL.
      SELECT objnr lfdnr gabja gabpe gbisj gbisp hkont anln1 FROM cobrb INTO TABLE lt_cobrb
                           FOR ALL ENTRIES IN gt_update
                           WHERE objnr = gt_update-objnr
                           AND   anln1 = p_anln1.
      IF sy-subrc IS INITIAL.
        SORT lt_cobrb BY objnr." hkont.
      ENDIF.
    ENDIF.
    LOOP AT gt_update INTO gs_update.
      READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_update-objnr.
      IF ls_cobrb-gbisj EQ p_year AND ls_cobrb-gbisp EQ p_period
      AND ls_cobrb-gabja EQ p_year_f AND ls_cobrb-gabpe EQ p_per_f.
        gs_output-message = 'Updated Sucessfully'.
      ELSE.
        gs_output-message = 'Update Failed'.
      ENDIF.
      gs_output-hkont = gs_update-hkont.
      gs_output-anln1 = gs_update-anln1.
      READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_update-objnr.
      IF sy-subrc IS INITIAL.
        gs_output-pspnr = gs_prps-pspnr.
      ENDIF.
      APPEND gs_output TO gt_output.
      CLEAR:gs_update,ls_cobrb,gs_prps,gs_output.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " UPDATE_SRULE_DDATE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_fxa .
  CLEAR:gt_prps[],gt_jest[],gt_cobrb_o[],gt_cobrb_o1[],gt_cobrb_o2[].
  SELECT pspnr objnr belkz FROM prps INTO TABLE gt_prps
*
                               WHERE pspnr IN s_pspnr "= GT_PRHI-POSNR
                               AND   belkz = 'X'.
  IF sy-subrc IS INITIAL.
    SORT gt_prps BY objnr.
    SELECT objnr stat FROM jest INTO TABLE gt_jest
                           FOR ALL ENTRIES IN gt_prps
                           WHERE objnr = gt_prps-objnr
                           AND   ( stat  = 'I0002' OR stat  = 'I0045')
                           AND   inact = space.
    IF sy-subrc IS INITIAL.
      SORT gt_jest BY objnr.
      SELECT *
                              FROM cobrb INTO TABLE gt_cobrb_o
                              FOR ALL ENTRIES IN gt_jest
                              WHERE objnr = gt_jest-objnr.
      IF sy-subrc IS INITIAL.
        gt_cobrb_o1 = gt_cobrb_o.
        gt_cobrb_o2 = gt_cobrb_o.
        SORT gt_cobrb_o BY objnr anln1 DESCENDING.
        SORT gt_cobrb_o1 BY objnr anln1 ASCENDING.
        SORT gt_cobrb_o2  BY objnr extnr DESCENDING.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " GET_DATA_FXA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SRULE_FXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_srule_fxa .


  TYPES :BEGIN OF lty_cobrb,
       objnr TYPE j_objnr,
       lfdnr TYPE br_lfdnr,
       gbisj TYPE gbisj,
       gbisp TYPE gbisp,
       hkont TYPE saknr,
       anln1 TYPE anln1,
       END OF lty_cobrb.
  DATA:lt_cobrb TYPE TABLE OF lty_cobrb,
*       lv_pspnr(24) type c,
       ls_cobrb TYPE lty_cobrb.
  DATA:lv_count TYPE i,
       lv_pspnr(24) TYPE c.
  REFRESH:gt_insert.

  LOOP AT gt_prps INTO  gs_prps.
    READ TABLE gt_jest INTO gs_jest WITH KEY objnr = gs_prps-objnr BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                                     hkont = p_hkont.
      IF sy-subrc IS INITIAL AND p_hkont IS NOT INITIAL.
        MOVE-CORRESPONDING gs_cobrb_o TO gs_delete.
        APPEND gs_delete TO gt_delete.
        CLEAR:gs_delete.
      ELSE.
        READ TABLE gt_cobrb_o INTO gs_cobrb_o WITH KEY objnr = gs_prps-objnr
                                               anln1 = p_anln1.
        IF sy-subrc IS INITIAL AND p_anln1 IS NOT INITIAL.
          MOVE-CORRESPONDING gs_cobrb_o TO gs_delete.
          APPEND gs_delete TO gt_delete.
          CLEAR:gs_delete.
        ELSE.
          gs_output-pspnr = gs_prps-pspnr.
          gs_output-hkont = p_hkont.
          gs_output-anln1 = p_anln1.
          gs_output-message = 'No settlement Rule exist with given FXA or G/L Number'.
          APPEND gs_output TO gt_output.
          CLEAR:gs_output.
        ENDIF.
      ENDIF.
    ELSE.
      gs_output-pspnr = gs_prps-pspnr.
      gs_output-hkont = p_hkont.
      gs_output-anln1 = p_anln1.
      gs_output-message = 'WBS element is not in TECO or release status'.
      APPEND gs_output TO gt_output.
      CLEAR:gs_output.
    ENDIF.
    CLEAR:gs_prps,gs_cobrb,gs_jest.
    CLEAR:gt_cobrb_ot[],gs_prps,gs_jest,gs_cobrb_o,gs_cobrb_ot.
  ENDLOOP.
  IF gt_delete[] IS NOT INITIAL.
    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
*       T_COBRB_INSERT    = GT_INSERT
*       T_COBRB_UPDATE    = GT_UPDATE
        t_cobrb_delete    = gt_delete
      exceptions
        srule_utask_error = 1
        OTHERS            = 2.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    WAIT UP TO 5 SECONDS.
    CLEAR:lt_cobrb[].
    IF p_hkont IS NOT INITIAL.
      SELECT objnr lfdnr gbisj gbisp hkont anln1 FROM cobrb INTO TABLE lt_cobrb
                               FOR ALL ENTRIES IN gt_delete
                               WHERE objnr = gt_delete-objnr
                               AND   hkont = p_hkont.
      IF sy-subrc IS INITIAL.
        SORT lt_cobrb BY objnr." hkont.
      ENDIF.
    ELSEIF p_anln1 IS NOT INITIAL.
      SELECT objnr lfdnr gbisj gbisp hkont anln1 FROM cobrb INTO TABLE lt_cobrb
                           FOR ALL ENTRIES IN gt_delete
                           WHERE objnr = gt_delete-objnr
                           AND   anln1 = p_anln1.
      IF sy-subrc IS INITIAL.
        SORT lt_cobrb BY objnr." hkont.
      ENDIF.
    ENDIF.
    LOOP AT gt_delete INTO gs_delete.
      IF p_hkont IS NOT INITIAL.
        READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_delete-objnr
                                                   hkont = p_hkont.
        IF sy-subrc IS NOT INITIAL.
          gs_output-message = 'Updated Sucessfully'.
        ELSE.
          gs_output-message = 'Update Failed'.
        ENDIF.
      ENDIF.
      IF p_anln1 IS NOT INITIAL.
        READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = gs_delete-objnr
                                                   anln1 = p_anln1.
        IF sy-subrc IS NOT INITIAL.
          gs_output-message = 'Updated Sucessfully'.
        ELSE.
          gs_output-message = 'Update Failed'.
        ENDIF.
      ENDIF.
      gs_output-hkont = gs_delete-hkont.
      gs_output-anln1 = gs_delete-anln1.
      READ TABLE gt_prps INTO gs_prps WITH KEY objnr = gs_delete-objnr.
      IF sy-subrc IS INITIAL.
        gs_output-pspnr = gs_prps-pspnr.
      ENDIF.
      APPEND gs_output TO gt_output.
      CLEAR:gs_delete,ls_cobrb,gs_prps,gs_output.
    ENDLOOP.
  ENDIF.
*BEGIN OF INSERT FOR SETC CHANGES BY KBANERJEE
  PERFORM f_set_wbsstat_setc.
*END OF INSERT FOR SETC CHANGES BY KBANERJEE
ENDFORM.                    " UPDATE_SRULE_FXA
*BEGIN OF INSERT FOR SETC CHANGES BY KBANERJEE
*&---------------------------------------------------------------------*
*&      Form  F_SET_WBSSTAT_SETC
*&---------------------------------------------------------------------*
*   Set status of WBS to SETC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_set_wbsstat_setc.
  DATA:lt_rsparams   TYPE STANDARD TABLE OF rsparams INITIAL SIZE 0,
       lt_r_wbs_setc TYPE STANDARD TABLE OF rsobjnr  INITIAL SIZE 0.
  DATA:ls_r_wbs_setc TYPE rsobjnr,
        ls_rsparams  TYPE rsparams.
  LOOP AT gt_prps INTO gs_prps.
    READ TABLE gt_jest INTO gs_jest
                       WITH KEY objnr = gs_prps-objnr
                       BINARY SEARCH.
    IF sy-subrc IS INITIAL AND gs_jest-stat NE 'I0028'.
      ls_r_wbs_setc-low = gs_jest-objnr.
      APPEND ls_r_wbs_setc TO lt_r_wbs_setc.
      CLEAR:ls_r_wbs_setc,gs_prps,gs_jest.
    ENDIF.
  ENDLOOP.
  IF lt_r_wbs_setc IS NOT INITIAL.
**Object numbers
*    ls_rsparams-selname = 'S_OBJNR'.
*    ls_rsparams-kind    = 'S'.
*    ls_rsparams-sign    = 'I'.
*    ls_rsparams-option  = 'IN'.
*    ls_rsparams-low     = lt_r_wbs_setc.
*    APPEND ls_rsparams TO lt_rsparams.
**Test Parameter
*    ls_rsparams-selname = 'P_TEST'.
*    ls_rsparams-kind    = 'P'.
*    ls_rsparams-sign    = 'I'.
*    ls_rsparams-option  = 'EQ'.
*    ls_rsparams-low     = space.
*    APPEND ls_rsparams TO lt_rsparams.
    SUBMIT zfirkacor24 WITH s_objnr IN lt_r_wbs_setc
                       WITH p_test  EQ space
                       AND RETURN."SELECTION-TABLE lt_rsparams AND RETURN.
  ENDIF.
ENDFORM.                    " F_SET_WBSSTAT_SETC
*END OF INSERT FOR SETC CHANGES BY KBANERJEE
