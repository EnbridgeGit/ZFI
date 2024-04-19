*&---------------------------------------------------------------------*
*&  Include           ZFIPS_PROJECTACTUAL_C55_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZFIPS_PROJECTACTUAL_C55                        *
* Include            :  ZFIPS_PROJECTACTUAL_C55_F01                    *
* Author             :  Rajeshwar Reddy                                *
* Date               :  12th-Nov-2019                                  *
* Technical Contact  :  Rajeshwar Reddy                                *
* Business Contact   :                                                 *
* Purpose            :  WNew Interface from SAP to C55                 *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 12th-Nov-2019  JOOKONTR    D30K930280 CHG0160132 Initial Development *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 25th-AUG-2020  KMB         D30K930655 CHG0190906 Erro in interface   *
*                            D30K930657                                *
*                            D30K930661                                *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 22nd-SEP-2020  KMB         D30K930689 CHG0193497 IDF performance     *
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  CHECK_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_file_path .
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_FILE_PATH
*&---------------------------------------------------------------------*
*&      Form  GET_DB_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_db_data .
  SELECT perio objnr gjahr wrttp kstar versn wogbtr bukrs
    INTO CORRESPONDING FIELDS OF TABLE t_coep
    FROM coep
    WHERE perio =  p_perio
      AND objnr IN s_objnr
      AND gjahr =  p_gjahr
      AND wrttp IN s_wrttp
      AND kstar IN s_kstar
      AND versn IN s_versn.
  SELECT * FROM zfit_c55_mapping INTO TABLE gt_mapping.
  SELECT * FROM zfi_c55_proj_reg INTO TABLE gt_proj_reg.
ENDFORM.                    " GET_DB_DATA
*&---------------------------------------------------------------------*
*&      Form  SUMARIZE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sumarize_data .
  SORT t_coep ASCENDING BY gjahr perio objnr kstar.

  LOOP AT t_coep INTO s_coep.
*Last sorted field is kstar
    AT END OF kstar.
      SUM.
      APPEND s_coep TO t_coep_sum.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " SUMARIZE_DATA
*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_report .

  TYPES:BEGIN OF ty_oa, "'OVERHEAD & ALLOCATIONS'.
         acc_name TYPE zacc_name,
         amount   TYPE coep-wogbtr,
        END OF ty_oa.
  TYPES:BEGIN OF ty_co, "CONTRIBUTIONS.
         acc_name TYPE zacc_name,
         project(16) TYPE c,
*         IOSWBS(16)  TYPE C,
         amount      TYPE coep-wogbtr,
        END OF ty_co.
  TYPES:BEGIN OF ty_project,
         acc_name    TYPE zacc_name,
         project(16) TYPE c,
         amount      TYPE coep-wogbtr,
        END OF ty_project,
        BEGIN OF ty_wbs_invcd,
          pspnr      TYPE prps-pspnr,
          poski      TYPE prps-poski,
          usr00      TYPE usr00prps,
        END OF ty_wbs_invcd.
  DATA: lt_oa TYPE TABLE OF ty_oa,
        ls_oa TYPE ty_oa,
        lt_co TYPE TABLE OF ty_co,
        ls_co TYPE ty_co,
        lt_cc TYPE TABLE OF ty_oa,
        ls_cc TYPE ty_oa,
        lt_dc TYPE TABLE OF ty_project,
        ls_dc TYPE ty_project,
        lt_rt TYPE TABLE OF ty_project,
        ls_rt TYPE ty_project,
        lt_wbs_invcd TYPE STANDARD TABLE OF ty_wbs_invcd INITIAL SIZE 0,
        ls_wbs_invcd TYPE ty_wbs_invcd.
  DATA: lv_prj(6) TYPE c.
  DATA: lv_poski(16) TYPE c,
        lv_amount TYPE wogxxx.
  DATA: ls_mapping LIKE LINE OF gt_mapping,
        ls_actual LIKE LINE OF gt_actual.
  DATA: gs_mapping TYPE zfit_c55_mapping.
  LOOP AT t_coep_sum INTO s_coep.
    CLEAR:  lv_poski.

    gs_actual-account = s_coep-kstar+4.
    gs_actual-entity  = s_coep-bukrs.

    SELECT SINGLE affil
      INTO gs_actual-intco
      FROM zacctnew
      WHERE cocode  = s_coep-bukrs
        AND glacct  = s_coep-kstar.

    IF gs_actual-intco = ''.
      SELECT SINGLE affil
        INTO gs_actual-intco
        FROM zacctnew
        WHERE cocode  = ''
          AND glacct  = s_coep-kstar.
      IF lv_intco = ''.
        gs_actual-intco = 'No_IntCo'.
      ELSE.
        CONCATENATE gc_ic gs_actual-intco INTO gs_actual-intco.
      ENDIF.
    ELSE.
      CONCATENATE gc_ic gs_actual-intco INTO gs_actual-intco.
    ENDIF.

    CONCATENATE s_coep-gjahr s_coep-perio+1(2) '00' INTO gs_actual-time.
    CLEAR:lv_poski.
    SELECT SINGLE poski
      INTO lv_poski
      FROM prps
      WHERE pspnr = s_coep-objnr+2.
    IF sy-subrc IS INITIAL.
      gs_actual-wbs = lv_poski.
      gs_actual-project = lv_poski(9).
*      REPLACE ALL OCCURRENCES OF '-' IN gs_actual-project WITH '.'.

      lv_ioswbs = lv_poski+10.
      CONCATENATE gc_wbsu lv_ioswbs INTO gs_actual-ioswbs.

      lv_division = lv_poski(2).
      CONCATENATE gc_div lv_division INTO gs_actual-division.
    ENDIF.

    gs_actual-amount = s_coep-wogbtr.
*      SHIFT GS_ACTUAL-AMOUNT LEFT DELETING LEADING ' '.

    IF s_coep-wogbtr <> 0.
      APPEND gs_actual TO gt_actual.
    ENDIF.
    CLEAR: gs_actual.
  ENDLOOP.
*&------------------------------------------------------*
*&--Start of new logic for the C55
*&------------------------------------------------------*
  gt_actual_temp = gt_actual .
  SORT gt_actual BY account project ioswbs.
  SORT gt_actual_temp BY account project ioswbs.
  LOOP AT gt_actual INTO gs_actual.
    CLEAR ls_mapping.
    READ TABLE gt_mapping INTO ls_mapping WITH KEY account = gs_actual-account.
    IF sy-subrc EQ 0.
      CASE ls_mapping-acc_name.
        WHEN gc_exclude.
          CONTINUE.
        WHEN gc_oa.
          "'OVERHEAD & ALLOCATIONS' :
          ls_oa-acc_name = gc_oa.           "'OVERHEAD & ALLOCATIONS'.
          ls_oa-amount = gs_actual-amount.
          COLLECT ls_oa INTO lt_oa.
          CLEAR ls_oa.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ELSE.
      CLEAR ls_mapping.
      READ TABLE gt_mapping INTO ls_mapping WITH KEY ios_wbs = gs_actual-ioswbs.
      IF sy-subrc EQ 0.
        CASE ls_mapping-acc_name.
          WHEN gc_co.
            "CONTRIBUTIONS Logic :
            ls_co-acc_name = gc_co.
            ls_co-project  = gs_actual-project.
*            LS_CO-IOSWBS   = GS_ACTUAL-IOSWBS.
            ls_co-amount   = gs_actual-amount.
            COLLECT ls_co INTO lt_co.
            CLEAR ls_co.
          WHEN gc_cc.
            "COST OF CAPITAL Logic :
            ls_cc-acc_name = gc_cc.
            ls_cc-amount   = gs_actual-amount.
            COLLECT ls_cc INTO lt_cc.
            CLEAR ls_cc.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      ELSE.
        IF gs_actual-ioswbs+4(1) = 9.
          lv_prj = gc_wbs.
          CLEAR ls_mapping.
          READ TABLE gt_mapping INTO ls_mapping WITH KEY ios_wbs = lv_prj.
          IF sy-subrc EQ 0.
            CASE ls_mapping-acc_name.
              WHEN gc_rt.
                "RETIREMENTS Logic
                ls_rt-acc_name = gc_rt.
                ls_rt-project  = gs_actual-project.
                ls_rt-amount   = gs_actual-amount.
                COLLECT ls_rt INTO lt_rt.
                CLEAR ls_rt.
              WHEN OTHERS.
                CONTINUE.
            ENDCASE.
          ENDIF.
        ELSE.
          "DIRECT CAPITAL Logic
          ls_dc-acc_name = gc_dc.
          ls_dc-project = gs_actual-project.
          ls_dc-amount = gs_actual-amount.
          COLLECT ls_dc INTO lt_dc.
          CLEAR ls_dc.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DATA: lv_flm TYPE sy-datum,
        lv_fcm TYPE sy-datum,
        lv_cm_date(10) TYPE c,
        lv_lm_date(10) TYPE c.
  CASE p_perio.
      "Calculation for Last Month of Actuals and Month Date based on Period entered on selection screen
    WHEN '001' OR '1' OR '01'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '01'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '002' OR '2' OR '02'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '02'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '003' OR '3' OR '03'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '03'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '004' OR '4' OR '04'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '04'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '005' OR '5' OR '05'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '05'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '006' OR '6' OR '06'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '06'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '007' OR '7' OR '07'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '07'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '008' OR '8' OR '08'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '08'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '009' OR '9' OR '09'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '09'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '010' OR '10'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '10'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '011' OR '11'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '11'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '012' OR '12'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '12'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN OTHERS.
  ENDCASE.
*&--Get the USR00 value based on the WBS
  SELECT pspnr poski usr00
    FROM prps
    INTO TABLE lt_wbs_invcd
    FOR ALL ENTRIES IN gt_actual
    WHERE poski = gt_actual-project
    AND  slwid = 'ZALELEM'.
  IF sy-subrc IS INITIAL.
    SORT lt_wbs_invcd BY pspnr.
  ENDIF.

  SORT lt_co BY project.
  SORT gt_proj_reg BY project.
  SORT gt_actual BY account project ioswbs.
  SORT lt_wbs_invcd BY pspnr poski usr00.
  SORT lt_rt BY acc_name project.
  SORT lt_dc BY acc_name project.
  CONCATENATE text-001 text-002 text-003 text-004
              text-005 text-006 text-007 text-008
              INTO st_datarec SEPARATED BY delimtr.
  APPEND st_datarec TO gt_data.
*&-------------------------------------*
*    Overhead and allocations
*&-------------------------------------*
  CLEAR ls_oa.
  READ TABLE lt_oa INTO ls_oa INDEX 1.
  IF sy-subrc EQ 0.
    CLEAR gs_mapping.
    READ TABLE gt_mapping INTO gs_mapping
                          WITH KEY acc_name = gc_oa. "'OVERHEAD & ALLOCATIONS'
    IF sy-subrc EQ 0.
      gs_final-inv_code = gs_mapping-invst_code.
      gs_final-acc_code = gs_mapping-acc_name.
    ENDIF.
    IF gs_final-inv_code IS INITIAL.
      gs_final-inv_code = 'XXXXXXXXX'.
    ENDIF.
    gs_final-last_mnth  = lv_lm_date.
    gs_final-month_date = lv_cm_date.
    gs_final-res_pcode  = ''.
    gs_final-month_val  = ls_oa-amount.
    gs_final-month_valc = ls_oa-amount.
    SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
    SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
    IF ls_oa-amount < 0.
      gs_final-month_val = gs_final-month_val * -1.
      gs_final-month_valc = gs_final-month_val * -1.
      CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
      CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
      CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
    ENDIF.

*    gs_final-acc_code   = 'OVERHEAD & ALLOCATIONS'.
    gs_final-replace    = gc_y.
    CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                gs_final-res_pcode gs_final-month_val gs_final-month_valc
                gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
    APPEND st_datarec TO gt_data.
    CLEAR gs_final.
    CLEAR ls_oa.
  ENDIF.
*&-------------------------------------*
*           Contributions
*&-------------------------------------*
  DATA: gs_proj_reg TYPE zfi_c55_proj_reg.
  LOOP AT lt_co INTO ls_co.
    CLEAR gs_proj_reg.
    READ TABLE gt_proj_reg INTO gs_proj_reg WITH KEY project = ls_co-project.
    IF sy-subrc EQ 0.
      IF gs_proj_reg-regulated IS NOT INITIAL.
        gs_final-inv_code   = gs_proj_reg-invst_reg."Regulated
        IF gs_final-inv_code IS INITIAL.
          gs_final-inv_code = ls_co-project.
        ENDIF.
        lv_amount        = ( ls_co-amount * gs_proj_reg-regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.
        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_co-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_co.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'CONTRIBUTIONS'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data.
        CLEAR: gs_final,lv_amount.
      ENDIF.
      IF gs_proj_reg-un_regulated IS NOT INITIAL.
        gs_final-inv_code   = gs_proj_reg-invst_unreg."Unregulated
        IF gs_final-inv_code IS INITIAL.
          gs_final-inv_code = ls_co-project.
        ENDIF.
        lv_amount        = ( ls_co-amount * gs_proj_reg-un_regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.
        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_co-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_co.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'CONTRIBUTIONS'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data.
        CLEAR: gs_final,lv_amount.
      ENDIF.
    ELSE.
      CLEAR gs_mapping.
      READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_co.       "'CONTRIBUTIONS'
*                                                     IOS_WBS  = LS_CO-IOSWBS.
      IF sy-subrc EQ 0.
        gs_final-acc_code = gs_mapping-acc_name.
      ENDIF.
      IF gs_final-inv_code IS INITIAL.
        CLEAR gs_actual.
        READ TABLE gt_actual INTO gs_actual WITH KEY project = ls_co-project.
*                                                     IOSWBS = LS_CO-IOSWBS.
        IF sy-subrc EQ 0.
          CLEAR  ls_wbs_invcd.
          READ TABLE lt_wbs_invcd INTO ls_wbs_invcd WITH KEY poski = gs_actual-project.
          IF sy-subrc IS INITIAL.
            gs_final-inv_code = ls_wbs_invcd-usr00.
          ENDIF.
        ENDIF.
      ENDIF.
      IF gs_final-inv_code IS INITIAL.
        gs_final-inv_code = ls_co-project.
      ENDIF.
      gs_final-last_mnth  = lv_lm_date.
      gs_final-month_date = lv_cm_date.
      gs_final-res_pcode  = ''.
      gs_final-month_val  = ls_co-amount.
      gs_final-month_valc = ls_co-amount.
      SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
      SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
      IF ls_co-amount < 0.
        gs_final-month_val = gs_final-month_val * -1.
        gs_final-month_valc = gs_final-month_val * -1.
        CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
        CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
        CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
      ENDIF.
      gs_final-acc_code   = gc_co. "'CONTRIBUTIONS'.
      gs_final-replace    = gc_y.
      CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                  gs_final-res_pcode gs_final-month_val gs_final-month_valc
                  gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
      APPEND st_datarec TO gt_data.
      CLEAR gs_final.
    ENDIF.
    CLEAR ls_co.
  ENDLOOP.
*&-------------------------------------*
*          Cost of Capital
*&-------------------------------------*
  LOOP AT lt_cc INTO ls_cc.
    CLEAR gs_mapping.
    READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_cc.           "'COST OF CAPITAL'.
    IF sy-subrc EQ 0.
      gs_final-inv_code = gs_mapping-invst_code.
      gs_final-acc_code = gs_mapping-acc_name.
    ENDIF.
    IF gs_final-inv_code IS INITIAL.
      gs_final-inv_code = 'XXXXXXXXX'.
    ENDIF.
    gs_final-last_mnth  = lv_lm_date.
    gs_final-month_date = lv_cm_date.
    gs_final-res_pcode  = ''.
    gs_final-month_val  = ls_cc-amount.
    gs_final-month_valc = ls_cc-amount.
    SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
    SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
    IF ls_cc-amount < 0.
      gs_final-month_val = gs_final-month_val * -1.
      gs_final-month_valc = gs_final-month_val * -1.
      CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
      CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
      CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
    ENDIF.
    "gs_final-acc_code   = 'COST OF CAPITAL'.
    gs_final-replace    = gc_y.
    CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                gs_final-res_pcode gs_final-month_val gs_final-month_valc
                gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
    APPEND st_datarec TO gt_data.
    CLEAR gs_final.
    CLEAR ls_cc.
  ENDLOOP.
*&-------------------------------------*
*            Retirements
*&-------------------------------------*
  LOOP AT lt_rt INTO ls_rt.
    CLEAR gs_proj_reg.
    READ TABLE gt_proj_reg INTO gs_proj_reg WITH KEY project = ls_rt-project.
    IF sy-subrc EQ 0.
      IF gs_proj_reg-regulated IS NOT INITIAL.
        gs_final-inv_code   = gs_proj_reg-invst_reg."Regulated
        IF gs_final-inv_code IS INITIAL.
          gs_final-inv_code = ls_rt-project.
        ENDIF.
        lv_amount        = ( ls_rt-amount * gs_proj_reg-regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.
        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_rt-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_rt.      "'RETIREMENTS'.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'RETIREMENTS'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data.
        CLEAR: gs_final,lv_amount.
      ENDIF.
      IF gs_proj_reg-un_regulated IS NOT INITIAL.
        gs_final-inv_code   = gs_proj_reg-invst_unreg."Unregulated
        IF gs_final-inv_code IS INITIAL.
          gs_final-inv_code = ls_rt-project.
        ENDIF.
        lv_amount        = ( ls_rt-amount * gs_proj_reg-un_regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.
        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_rt-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_rt.          "'RETIREMENTS'.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'RETIREMENTS'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data.
        CLEAR: gs_final,lv_amount.
      ENDIF.
    ELSE.
      CLEAR gs_mapping.
      READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_rt.        "'RETIREMENTS'.
      IF sy-subrc EQ 0.
        gs_final-acc_code = gs_mapping-acc_name.
      ENDIF.
      IF gs_final-inv_code IS INITIAL.
        CLEAR gs_actual.
        READ TABLE gt_actual INTO gs_actual WITH KEY project = ls_rt-project.
        IF sy-subrc EQ 0.
          CLEAR  ls_wbs_invcd.
          READ TABLE lt_wbs_invcd INTO ls_wbs_invcd WITH KEY poski = gs_actual-project.
          IF sy-subrc IS INITIAL.
            gs_final-inv_code = ls_wbs_invcd-usr00.
          ENDIF.
        ENDIF.
      ENDIF.
      IF gs_final-inv_code IS INITIAL.
        gs_final-inv_code = ls_rt-project.
      ENDIF.
      gs_final-last_mnth  = lv_lm_date.
      gs_final-month_date = lv_cm_date.
      gs_final-res_pcode  = ''.
      gs_final-month_val  = ls_rt-amount.
      gs_final-month_valc = ls_rt-amount.
      SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
      SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
      IF ls_rt-amount < 0.
        gs_final-month_val = gs_final-month_val * -1.
        gs_final-month_valc = gs_final-month_val * -1.
        CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
        CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
        CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
      ENDIF.
      "gs_final-acc_code   = 'RETIREMENTS'.
      gs_final-replace    = gc_y.
      CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                  gs_final-res_pcode gs_final-month_val gs_final-month_valc
                  gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
      APPEND st_datarec TO gt_data.
      CLEAR gs_final.
    ENDIF.
    CLEAR ls_rt.
  ENDLOOP.
*&-------------------------------------*
*              Direct Capital
*&-------------------------------------*
  LOOP AT lt_dc INTO ls_dc.
    CLEAR gs_proj_reg.
    READ TABLE gt_proj_reg INTO gs_proj_reg WITH KEY project = ls_dc-project BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF gs_proj_reg-regulated IS NOT INITIAL.
        gs_final-inv_code   = gs_proj_reg-invst_reg."Regulated
        IF gs_final-inv_code IS INITIAL.
          gs_final-inv_code = ls_dc-project.
        ENDIF.
        lv_amount        = ( ls_dc-amount * gs_proj_reg-regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.
        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_dc-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_dc.        "'DIRECT CAPITAL'.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'DIRECT CAPITAL'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data.
        CLEAR: gs_final,lv_amount.
      ENDIF.
      IF gs_proj_reg-un_regulated IS NOT INITIAL.
        gs_final-inv_code   = gs_proj_reg-invst_unreg."Unregulated
        IF gs_final-inv_code IS INITIAL.
          gs_final-inv_code = ls_dc-project.
        ENDIF.
        lv_amount        = ( ls_dc-amount * gs_proj_reg-un_regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.
        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_dc-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_dc.            "'DIRECT CAPITAL'.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'DIRECT CAPITAL'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data.
        CLEAR: gs_final,lv_amount.
      ENDIF.
    ELSE.
      CLEAR gs_mapping.
      READ TABLE gt_mapping INTO gs_mapping WITH KEY acc_name = gc_dc.      "'DIRECT CAPITAL'.
      IF sy-subrc EQ 0.
        gs_final-inv_code = gs_mapping-invst_code.
        gs_final-acc_code = gs_mapping-acc_name.
      ENDIF.

      IF gs_final-inv_code IS INITIAL.
        CLEAR gs_actual.
        READ TABLE gt_actual INTO gs_actual WITH KEY project = ls_dc-project.
        IF sy-subrc EQ 0.
          CLEAR  ls_wbs_invcd.
          READ TABLE lt_wbs_invcd INTO ls_wbs_invcd WITH KEY poski = gs_actual-project.
          IF sy-subrc IS INITIAL.
            gs_final-inv_code = ls_wbs_invcd-usr00.
          ENDIF.
        ENDIF.
      ENDIF.
      IF gs_final-inv_code IS INITIAL.
        gs_final-inv_code = ls_dc-project.
      ENDIF.
      gs_final-last_mnth  = lv_lm_date.
      gs_final-month_date = lv_cm_date.
      gs_final-res_pcode  = ''.
      gs_final-month_val  = ls_dc-amount.
      gs_final-month_valc = ls_dc-amount.
      SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
      SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
      IF ls_dc-amount < 0.
        gs_final-month_val = gs_final-month_val * -1.
        gs_final-month_valc = gs_final-month_val * -1.
        CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
        CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
        CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
      ENDIF.
      "gs_final-acc_code   = 'DIRECT CAPITAL'.
      gs_final-replace    = gc_y.
      CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                  gs_final-res_pcode gs_final-month_val gs_final-month_valc
                  gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
      APPEND st_datarec TO gt_data.
      CLEAR gs_final.
    ENDIF.
    CLEAR ls_dc.
  ENDLOOP.
*&--End of new logic for the C55
ENDFORM.                    " PRINT_REPORT
*&---------------------------------------------------------------------*
*&      Form  SAVE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_file .
  IF lv_local = 0.

    PERFORM open_csvfile.

    LOOP AT gt_data INTO st_datarec.
      TRANSFER st_datarec TO csvfile.
    ENDLOOP.

    PERFORM close_csvfile.
    WRITE: text-009, csvfile.

  ELSE.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_file
      TABLES
        data_tab                = gt_data
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
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    WRITE: text-011, p_file.
  ENDIF.

ENDFORM.                    " SAVE_FILE
*&---------------------------------------------------------------------*
*&      Form  OPEN_CSVFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM open_csvfile .
  OPEN DATASET csvfile FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    " OPEN_CSVFILE
*&---------------------------------------------------------------------*
*&      Form  CLOSE_CSVFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM close_csvfile .
  CLOSE DATASET csvfile.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH text-010 csvfile msg.
    STOP.
  ENDIF.
ENDFORM.                    " CLOSE_CSVFILE
*&---------------------------------------------------------------------*
*&      Form  CHECK_FILE_PATH1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_file_path1 .
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file1
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_FILE_PATH1
*&---------------------------------------------------------------------*
*&      Form  GET_DB_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_db_data1 .
  CLEAR : t_coep1[], gt_mapping1[], gt_proj_reg1[].
  SELECT perio objnr gjahr wrttp kstar versn wogbtr bukrs
    INTO CORRESPONDING FIELDS OF TABLE t_coep1
    FROM coep
    WHERE perio <=  p_perio
      AND objnr IN s_objnr
      AND gjahr =  p_gjahr
      AND wrttp IN s_wrttp
      AND kstar IN s_kstar
      AND versn IN s_versn.
    IF sy-subrc = 0.
        SORT t_coep1 ASCENDING BY gjahr perio objnr kstar.
    ENDIF.
  SELECT * FROM zfit_c55_mapping INTO TABLE gt_mapping1.
  SELECT * FROM zfi_c55_proj_reg INTO TABLE gt_proj_reg1.
ENDFORM.                    " GET_DB_DATA1
*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_report1 .

  TYPES:BEGIN OF ty_oa, "'OVERHEAD & ALLOCATIONS'.
         acc_name TYPE zacc_name,
         amount   TYPE coep-wogbtr,
        END OF ty_oa.
  TYPES:BEGIN OF ty_co, "CONTRIBUTIONS.
         acc_name TYPE zacc_name,
         project(16) TYPE c,
*         IOSWBS(16)  TYPE C,
         amount      TYPE coep-wogbtr,
        END OF ty_co.
  TYPES:BEGIN OF ty_project,
         acc_name    TYPE zacc_name,
         project(16) TYPE c,
         amount      TYPE coep-wogbtr,
        END OF ty_project,
        BEGIN OF ty_wbs_invcd,
          pspnr      TYPE prps-pspnr,
          poski      TYPE prps-poski,
          usr00      TYPE usr00prps,
        END OF ty_wbs_invcd.
  DATA: lt_oa TYPE TABLE OF ty_oa,
        ls_oa TYPE ty_oa,
        lt_co TYPE TABLE OF ty_co,
        ls_co TYPE ty_co,
        lt_cc TYPE TABLE OF ty_oa,
        ls_cc TYPE ty_oa,
        lt_dc TYPE TABLE OF ty_project,
        ls_dc TYPE ty_project,
        lt_rt TYPE TABLE OF ty_project,
        ls_rt TYPE ty_project,
        lt_wbs_invcd TYPE STANDARD TABLE OF ty_wbs_invcd INITIAL SIZE 0,
        ls_wbs_invcd TYPE ty_wbs_invcd.
  DATA: lv_prj(6) TYPE c.
  DATA: lv_poski(16) TYPE c,
        lv_amount TYPE wogxxx.
  DATA: ls_mapping LIKE LINE OF gt_mapping,
        ls_actual LIKE LINE OF gt_actual.
  DATA: gs_mapping TYPE zfit_c55_mapping.

  CLEAR : lt_oa[], ls_oa, lt_co[],ls_co,lt_cc[], ls_cc,lt_dc[],ls_dc,lt_rt[],ls_rt, s_coep, gs_actual1, lt_wbs_invcd[],
          ls_wbs_invcd,ls_mapping, ls_actual, lv_prj, lv_poski, lv_amount, gs_mapping,  gt_actual_temp1[], gs_final.

  LOOP AT t_coep_sum1 INTO s_coep.
    CLEAR:  lv_poski.

    gs_actual1-account = s_coep-kstar+4.
    gs_actual1-entity  = s_coep-bukrs.

    SELECT SINGLE affil
      INTO gs_actual1-intco
      FROM zacctnew
      WHERE cocode  = s_coep-bukrs
        AND glacct  = s_coep-kstar.

    IF gs_actual1-intco = ''.
      SELECT SINGLE affil
        INTO gs_actual1-intco
        FROM zacctnew
        WHERE cocode  = ''
          AND glacct  = s_coep-kstar.
      IF lv_intco = ''.
        gs_actual1-intco = 'No_IntCo'.
      ELSE.
        CONCATENATE gc_ic gs_actual1-intco INTO gs_actual1-intco.
      ENDIF.
    ELSE.
      CONCATENATE gc_ic gs_actual1-intco INTO gs_actual1-intco.
    ENDIF.

    CONCATENATE s_coep-gjahr s_coep-perio+1(2) '00' INTO gs_actual1-time.
    CLEAR:lv_poski.
    SELECT SINGLE poski
      INTO lv_poski
      FROM prps
      WHERE pspnr = s_coep-objnr+2.
    IF sy-subrc IS INITIAL.
      CONDENSE lv_poski.
      gs_actual1-wbs = lv_poski.

      gs_actual1-project = lv_poski(9).
*      REPLACE ALL OCCURRENCES OF '-' IN GS_ACTUAL1-project WITH '.'.

      lv_ioswbs = lv_poski+10.
      CONCATENATE gc_wbsu lv_ioswbs INTO gs_actual1-ioswbs.

      lv_division = lv_poski(2).
      CONCATENATE gc_div lv_division INTO gs_actual1-division.
    ENDIF.

    gs_actual1-amount = s_coep-wogbtr.
*      SHIFT GS_ACTUAL1-AMOUNT LEFT DELETING LEADING ' '.

    IF s_coep-wogbtr <> 0.
      APPEND gs_actual1 TO gt_actual1.
    ENDIF.
    CLEAR: gs_actual1.
  ENDLOOP.
*&------------------------------------------------------*
*&--Start of new logic for the C55
*&------------------------------------------------------*
  gt_actual_temp1 = gt_actual1 .
  SORT gt_actual1 BY account project ioswbs.
  SORT gt_actual_temp1 BY account project ioswbs.
  LOOP AT gt_actual1 INTO gs_actual1.
    CLEAR ls_mapping.
    READ TABLE gt_mapping1 INTO ls_mapping WITH KEY account = gs_actual1-account.
    IF sy-subrc EQ 0.
      CASE ls_mapping-acc_name.
        WHEN gc_exclude.
          CONTINUE.
        WHEN gc_oa.
          "'OVERHEAD & ALLOCATIONS' :
          ls_oa-acc_name = gc_oa.           "'OVERHEAD & ALLOCATIONS'.
          ls_oa-amount = gs_actual1-amount.
          COLLECT ls_oa INTO lt_oa.
          CLEAR ls_oa.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ELSE.
      CLEAR ls_mapping.
      READ TABLE gt_mapping1 INTO ls_mapping WITH KEY ios_wbs = gs_actual1-ioswbs.
      IF sy-subrc EQ 0.
        CASE ls_mapping-acc_name.
          WHEN gc_co.
            "CONTRIBUTIONS Logic :
            ls_co-acc_name = gc_co.
            ls_co-project  = gs_actual1-project.
*            LS_CO-IOSWBS   = GS_ACTUAL1-IOSWBS.
            ls_co-amount   = gs_actual1-amount.
            COLLECT ls_co INTO lt_co.
            CLEAR ls_co.
          WHEN gc_cc.
            "COST OF CAPITAL Logic :
            ls_cc-acc_name = gc_cc.
            ls_cc-amount   = gs_actual1-amount.
            COLLECT ls_cc INTO lt_cc.
            CLEAR ls_cc.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      ELSE.
        IF gs_actual1-ioswbs+4(1) = 9.
          lv_prj = gc_wbs.
          CLEAR ls_mapping.
          READ TABLE gt_mapping1 INTO ls_mapping WITH KEY ios_wbs = lv_prj.
          IF sy-subrc EQ 0.
            CASE ls_mapping-acc_name.
              WHEN gc_rt.
                "RETIREMENTS Logic
                ls_rt-acc_name = gc_rt.
                ls_rt-project  = gs_actual1-project.
                ls_rt-amount   = gs_actual1-amount.
                COLLECT ls_rt INTO lt_rt.
                CLEAR ls_rt.
              WHEN OTHERS.
                CONTINUE.
            ENDCASE.
          ENDIF.
        ELSE.
          "DIRECT CAPITAL Logic
          ls_dc-acc_name = gc_dc.
          ls_dc-project = gs_actual1-project.
          ls_dc-amount = gs_actual1-amount.
          COLLECT ls_dc INTO lt_dc.
          CLEAR ls_dc.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DATA: lv_flm TYPE sy-datum,
        lv_fcm TYPE sy-datum,
        lv_cm_date(10) TYPE c,
        lv_lm_date(10) TYPE c.
  CLEAR : lv_flm, lv_fcm, lv_cm_date, lv_lm_date.
  CASE p_perio.
      "Calculation for Last Month of Actuals and Month Date based on Period entered on selection screen
    WHEN '001' OR '1' OR '01'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '01'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '002' OR '2' OR '02'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '02'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '003' OR '3' OR '03'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '03'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '004' OR '4' OR '04'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '04'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '005' OR '5' OR '05'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '05'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '006' OR '6' OR '06'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '06'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '007' OR '7' OR '07'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '07'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '008' OR '8' OR '08'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '08'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '009' OR '9' OR '09'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '09'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '010' OR '10'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '10'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '011' OR '11'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '11'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN '012' OR '12'.
      lv_flm = sy-datum.
      lv_flm+0(4) = p_gjahr.
      lv_flm+4(2) = '12'.
      lv_flm+6(2) = '01'.
      lv_fcm = lv_flm.
      CONCATENATE lv_fcm+4(2) '/' lv_fcm+6(2) '/' lv_fcm+0(4) INTO lv_cm_date.
      lv_flm = lv_flm - 1.
      lv_flm+6(2) = '01'.
      CONCATENATE lv_flm+4(2) '/' lv_flm+6(2) '/' lv_flm+0(4) INTO lv_lm_date.
    WHEN OTHERS.
  ENDCASE.
*&--Get the USR00 value based on the WBS
  CLEAR : lt_wbs_invcd[].
  SELECT pspnr poski usr00
    FROM prps
    INTO TABLE lt_wbs_invcd
    FOR ALL ENTRIES IN gt_actual1
    WHERE poski = gt_actual1-project
    AND  slwid = 'ZALELEM'.
  IF sy-subrc IS INITIAL.
    SORT lt_wbs_invcd BY pspnr.
  ENDIF.

  SORT lt_co BY project.
  SORT gt_proj_reg1 BY project.
  "BOC by kmb on 25.8.2020 CHG0190906
*  SORT gt_actual1 BY account project ioswbs.
  SORT gt_actual1 BY project.
  "EOC by kmb on 25.8.2020 CHG0190906
  SORT lt_wbs_invcd BY pspnr poski usr00.
  SORT lt_rt BY acc_name project.
  SORT lt_dc BY acc_name project.
  CONCATENATE text-001 text-002 text-003 text-004
              text-012 text-013 text-007 text-008 text-014
              INTO st_datarec SEPARATED BY delimtr.
  APPEND st_datarec TO gt_data1.
*&-------------------------------------*
*    Overhead and allocations
*&-------------------------------------*
  CLEAR ls_oa.
  READ TABLE lt_oa INTO ls_oa INDEX 1.
  IF sy-subrc EQ 0.
    CLEAR gs_mapping.
    READ TABLE gt_mapping1 INTO gs_mapping
                          WITH KEY acc_name = gc_oa. "'OVERHEAD & ALLOCATIONS'
    IF sy-subrc EQ 0.
      gs_final-inv_code = gs_mapping-invst_code.
      gs_final-acc_code = gs_mapping-acc_name.
    ENDIF.
    IF gs_final-inv_code IS INITIAL.
      gs_final-inv_code = 'XXXXXXXXX'.
    ENDIF.
    gs_final-last_mnth  = lv_lm_date.
    gs_final-month_date = lv_cm_date.
    gs_final-res_pcode  = ''.
    gs_final-month_val  = ls_oa-amount.
    gs_final-month_valc = ls_oa-amount.
    SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
    SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
    IF ls_oa-amount < 0.
      gs_final-month_val = gs_final-month_val * -1.
      gs_final-month_valc = gs_final-month_val * -1.
      CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
      CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
      CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
    ENDIF.

*    gs_final-acc_code   = 'OVERHEAD & ALLOCATIONS'.
    gs_final-replace    = gc_y.
    CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                gs_final-res_pcode gs_final-month_val gs_final-month_valc
                gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
    APPEND st_datarec TO gt_data1.
    CLEAR gs_final.
    CLEAR ls_oa.
  ENDIF.
*&-------------------------------------*
*           Contributions
*&-------------------------------------*
  DATA: gs_proj_reg TYPE zfi_c55_proj_reg.
  LOOP AT lt_co INTO ls_co.
    CLEAR gs_proj_reg.
    READ TABLE gt_proj_reg1 INTO gs_proj_reg WITH KEY project = ls_co-project.
    IF sy-subrc EQ 0.
      IF gs_proj_reg-regulated IS NOT INITIAL.

        gs_final-inv_code   = gs_proj_reg-invst_reg."Regulated
        "BOC by kmb on 28.8.2020 CHG0190906
*        IF gs_final-inv_code IS INITIAL.
*          gs_final-inv_code = ls_co-project.
*        ENDIF.
        "EOC by kmb on 28.8.2020 CHG0190906
        lv_amount        = ( ls_co-amount * gs_proj_reg-regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.

        gs_final-project = ls_co-project.

        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_co-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_co.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'CONTRIBUTIONS'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace gs_final-project INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data1.
        CLEAR: gs_final,lv_amount.
      ENDIF.
      IF gs_proj_reg-un_regulated IS NOT INITIAL.

        gs_final-inv_code   = gs_proj_reg-invst_unreg."Unregulated
        "BOC by kmb on 28.8.2020 CHG0190906
*        IF gs_final-inv_code IS INITIAL.
*          gs_final-inv_code = ls_co-project.
*        ENDIF.
        "EOC by kmb on 28.8.2020 CHG0190906
        lv_amount        = ( ls_co-amount * gs_proj_reg-un_regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.

         gs_final-project = ls_co-project.

        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_co-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_co.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'CONTRIBUTIONS'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace  gs_final-project INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data1.
        CLEAR: gs_final,lv_amount.
      ENDIF.
    ELSE.
      CLEAR gs_mapping.
      READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_co.       "'CONTRIBUTIONS'
*                                                     IOS_WBS  = LS_CO-IOSWBS.
      IF sy-subrc EQ 0.
        gs_final-acc_code = gs_mapping-acc_name.
      ENDIF.
      IF gs_final-inv_code IS INITIAL.
        CLEAR gs_actual1.
        "BOC by kmb on 25.8.2020 CHG0190906
*        READ TABLE gt_actual1 INTO gs_actual1 WITH KEY project = ls_co-project.
        READ TABLE gt_actual1 INTO gs_actual1 WITH KEY project = ls_co-project BINARY SEARCH.
        "EOC by kmb on 25.8.2020 CHG0190906
*                                                     IOSWBS = LS_CO-IOSWBS.
        IF sy-subrc EQ 0.
          CLEAR  ls_wbs_invcd.
          READ TABLE lt_wbs_invcd INTO ls_wbs_invcd WITH KEY poski = gs_actual1-project.
          IF sy-subrc IS INITIAL.
            gs_final-inv_code = ls_wbs_invcd-usr00.
          ENDIF.
        ENDIF.
      ENDIF.
      "BOC by kmb on 28.8.2020 CHG0190906
*      IF gs_final-inv_code IS INITIAL.
*        gs_final-inv_code = ls_co-project.
*      ENDIF.
      "EOC by kmb on 28.8.2020 CHG0190906
      gs_final-last_mnth  = lv_lm_date.
      gs_final-month_date = lv_cm_date.
      gs_final-res_pcode  = ''.
      gs_final-month_val  = ls_co-amount.
      gs_final-month_valc = ls_co-amount.

       gs_final-project = ls_co-project.

      SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
      SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
      IF ls_co-amount < 0.
        gs_final-month_val = gs_final-month_val * -1.
        gs_final-month_valc = gs_final-month_val * -1.
        CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
        CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
        CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
      ENDIF.
      gs_final-acc_code   = gc_co. "'CONTRIBUTIONS'.
      gs_final-replace    = gc_y.
      CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                  gs_final-res_pcode gs_final-month_val gs_final-month_valc
                  gs_final-acc_code gs_final-replace  gs_final-project INTO st_datarec SEPARATED BY delimtr.
      APPEND st_datarec TO gt_data1.
      CLEAR gs_final.
    ENDIF.
    CLEAR ls_co.
  ENDLOOP.
*&-------------------------------------*
*          Cost of Capital
*&-------------------------------------*
  LOOP AT lt_cc INTO ls_cc.
    CLEAR gs_mapping.
    READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_cc.           "'COST OF CAPITAL'.
    IF sy-subrc EQ 0.
      gs_final-inv_code = gs_mapping-invst_code.
      gs_final-acc_code = gs_mapping-acc_name.
    ENDIF.
    IF gs_final-inv_code IS INITIAL.
      gs_final-inv_code = 'XXXXXXXXX'.
    ENDIF.
    gs_final-last_mnth  = lv_lm_date.
    gs_final-month_date = lv_cm_date.
    gs_final-res_pcode  = ''.
    gs_final-month_val  = ls_cc-amount.
    gs_final-month_valc = ls_cc-amount.
    SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
    SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
    IF ls_cc-amount < 0.
      gs_final-month_val = gs_final-month_val * -1.
      gs_final-month_valc = gs_final-month_val * -1.
      CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
      CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
      CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
    ENDIF.
    "gs_final-acc_code   = 'COST OF CAPITAL'.
    gs_final-replace    = gc_y.
    CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                gs_final-res_pcode gs_final-month_val gs_final-month_valc
                gs_final-acc_code gs_final-replace INTO st_datarec SEPARATED BY delimtr.
    APPEND st_datarec TO gt_data1.
    CLEAR gs_final.
    CLEAR ls_cc.
  ENDLOOP.
*&-------------------------------------*
*            Retirements
*&-------------------------------------*
  LOOP AT lt_rt INTO ls_rt.
    CLEAR gs_proj_reg.
    READ TABLE gt_proj_reg1 INTO gs_proj_reg WITH KEY project = ls_rt-project.
    IF sy-subrc EQ 0.
      IF gs_proj_reg-regulated IS NOT INITIAL.

        gs_final-inv_code   = gs_proj_reg-invst_reg."Regulated
        "BOC by kmb on 28.8.2020 CHG0190906
*        IF gs_final-inv_code IS INITIAL.
*          gs_final-inv_code = ls_rt-project.
*        ENDIF.
        "EOC by kmb on 28.8.2020 CHG0190906
        lv_amount        = ( ls_rt-amount * gs_proj_reg-regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.

        gs_final-project = ls_rt-project.

        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_rt-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_rt.      "'RETIREMENTS'.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'RETIREMENTS'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace gs_final-project INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data1.
        CLEAR: gs_final,lv_amount.
      ENDIF.
      IF gs_proj_reg-un_regulated IS NOT INITIAL.

        gs_final-inv_code   = gs_proj_reg-invst_unreg."Unregulated
        "BOC by kmb on 28.8.2020 CHG0190906
*        IF gs_final-inv_code IS INITIAL.
*          gs_final-inv_code = ls_rt-project.
*        ENDIF.
        "EOC by kmb on 28.8.2020 CHG0190906
        lv_amount        = ( ls_rt-amount * gs_proj_reg-un_regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.

        gs_final-project = ls_rt-project.

        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_rt-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_rt.          "'RETIREMENTS'.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'RETIREMENTS'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace gs_final-project INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data1.
        CLEAR: gs_final,lv_amount.
      ENDIF.
    ELSE.
      CLEAR gs_mapping.
      READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_rt.        "'RETIREMENTS'.
      IF sy-subrc EQ 0.
        gs_final-acc_code = gs_mapping-acc_name.
      ENDIF.
      IF gs_final-inv_code IS INITIAL.
        CLEAR gs_actual1.
        "BOC by kmb on 25.8.2020 CHG0190906
*        READ TABLE gt_actual INTO gs_actual1 WITH KEY project = ls_rt-project.
        READ TABLE gt_actual1 INTO gs_actual1 WITH KEY project = ls_rt-project BINARY SEARCH.
        "EOC by kmb on 25.8.2020 CHG0190906
        IF sy-subrc EQ 0.
          CLEAR  ls_wbs_invcd.
          READ TABLE lt_wbs_invcd INTO ls_wbs_invcd WITH KEY poski = gs_actual1-project.
          IF sy-subrc IS INITIAL.
            gs_final-inv_code = ls_wbs_invcd-usr00.
          ENDIF.
        ENDIF.
      ENDIF.
      "BOC by kmb on 28.8.2020 CHG0190906
*      IF gs_final-inv_code IS INITIAL.
*        gs_final-inv_code = ls_rt-project.
*      ENDIF.
      "EOC by kmb on 28.8.2020 CHG0190906
      gs_final-last_mnth  = lv_lm_date.
      gs_final-month_date = lv_cm_date.
      gs_final-res_pcode  = ''.
      gs_final-month_val  = ls_rt-amount.
      gs_final-month_valc = ls_rt-amount.

      gs_final-project = ls_rt-project.

      SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
      SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
      IF ls_rt-amount < 0.
        gs_final-month_val = gs_final-month_val * -1.
        gs_final-month_valc = gs_final-month_val * -1.
        CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
        CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
        CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
      ENDIF.
      "gs_final-acc_code   = 'RETIREMENTS'.
      gs_final-replace    = gc_y.
      CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                  gs_final-res_pcode gs_final-month_val gs_final-month_valc
                  gs_final-acc_code gs_final-replace gs_final-project INTO st_datarec SEPARATED BY delimtr.
      APPEND st_datarec TO gt_data1.
      CLEAR gs_final.
    ENDIF.
    CLEAR ls_rt.
  ENDLOOP.
*&-------------------------------------*
*              Direct Capital
*&-------------------------------------*
  LOOP AT lt_dc INTO ls_dc.
    CLEAR gs_proj_reg.
    READ TABLE gt_proj_reg1 INTO gs_proj_reg WITH KEY project = ls_dc-project BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF gs_proj_reg-regulated IS NOT INITIAL.

        gs_final-inv_code   = gs_proj_reg-invst_reg."Regulated
        "BOC by kmb on 28.8.2020 CHG0190906
*        IF gs_final-inv_code IS INITIAL.
*          gs_final-inv_code = ls_dc-project.
*        ENDIF.
        "EOC by kmb on 28.8.2020 CHG0190906
        lv_amount        = ( ls_dc-amount * gs_proj_reg-regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.

        gs_final-project = ls_dc-project.

        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_dc-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_dc.        "'DIRECT CAPITAL'.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'DIRECT CAPITAL'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace gs_final-project INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data1.
        CLEAR: gs_final,lv_amount.
      ENDIF.
      IF gs_proj_reg-un_regulated IS NOT INITIAL.

        gs_final-inv_code   = gs_proj_reg-invst_unreg."Unregulated
        "BOC by kmb on 28.8.2020 CHG0190906
*        IF gs_final-inv_code IS INITIAL.
*          gs_final-inv_code = ls_dc-project.
*        ENDIF.
        "EOC by kmb on 28.8.2020 CHG0190906
        lv_amount        = ( ls_dc-amount * gs_proj_reg-un_regulated ) / 100.
        gs_final-last_mnth  = lv_lm_date.
        gs_final-month_date = lv_cm_date.
        gs_final-res_pcode  = ''.
        gs_final-month_val  = lv_amount.
        gs_final-month_valc = lv_amount.

        gs_final-project = ls_dc-project.

        SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
        SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
        IF ls_dc-amount < 0.
          gs_final-month_val = gs_final-month_val * -1.
          gs_final-month_valc = gs_final-month_val * -1.
          CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
          CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
          CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
        ENDIF.
        CLEAR gs_mapping.
        READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_dc.            "'DIRECT CAPITAL'.
        IF sy-subrc EQ 0.
          gs_final-acc_code = gs_mapping-acc_name.
        ENDIF.
        "gs_final-acc_code   = 'DIRECT CAPITAL'.
        gs_final-replace    = gc_y.
        CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                    gs_final-res_pcode gs_final-month_val gs_final-month_valc
                    gs_final-acc_code gs_final-replace gs_final-project INTO st_datarec SEPARATED BY delimtr.
        APPEND st_datarec TO gt_data1.
        CLEAR: gs_final,lv_amount.
      ENDIF.
    ELSE.
      CLEAR gs_mapping.
      READ TABLE gt_mapping1 INTO gs_mapping WITH KEY acc_name = gc_dc.      "'DIRECT CAPITAL'.
      IF sy-subrc EQ 0.
        gs_final-inv_code = gs_mapping-invst_code.
        gs_final-acc_code = gs_mapping-acc_name.
      ENDIF.

      IF gs_final-inv_code IS INITIAL.
        CLEAR gs_actual1.
        "BOC by kmb on 25.8.2020 CHG0190906
*        READ TABLE gt_actual INTO gs_actual1 WITH KEY project = ls_dc-project.
        READ TABLE gt_actual1 INTO gs_actual1 WITH KEY project = ls_dc-project BINARY SEARCH.
        "EOC by kmb on 25.8.2020 CHG0190906
        IF sy-subrc EQ 0.
          CLEAR  ls_wbs_invcd.
          READ TABLE lt_wbs_invcd INTO ls_wbs_invcd WITH KEY poski = gs_actual1-project.
          IF sy-subrc IS INITIAL.
            gs_final-inv_code = ls_wbs_invcd-usr00.
          ENDIF.
        ENDIF.
      ENDIF.
      "BOC by kmb on 28.8.2020 CHG0190906
*      IF gs_final-inv_code IS INITIAL.
*        gs_final-inv_code = ls_dc-project.
*      ENDIF.
      "EOC by kmb on 28.8.2020 CHG0190906
      gs_final-last_mnth  = lv_lm_date.
      gs_final-month_date = lv_cm_date.
      gs_final-res_pcode  = ''.
      gs_final-month_val  = ls_dc-amount.
      gs_final-month_valc = ls_dc-amount.

      gs_final-project = ls_dc-project.

      SHIFT gs_final-month_val LEFT DELETING LEADING ' '.
      SHIFT gs_final-month_valc LEFT DELETING LEADING ' '.
      IF ls_dc-amount < 0.
        gs_final-month_val = gs_final-month_val * -1.
        gs_final-month_valc = gs_final-month_val * -1.
        CONCATENATE '-' gs_final-month_val INTO gs_final-month_val.
        CONCATENATE '-' gs_final-month_valc INTO gs_final-month_valc.
        CONDENSE : gs_final-month_val NO-GAPS, gs_final-month_valc NO-GAPS.
      ENDIF.
      "gs_final-acc_code   = 'DIRECT CAPITAL'.
      gs_final-replace    = gc_y.
      CONCATENATE gs_final-inv_code gs_final-last_mnth gs_final-month_date
                  gs_final-res_pcode gs_final-month_val gs_final-month_valc
                  gs_final-acc_code gs_final-replace gs_final-project INTO st_datarec SEPARATED BY delimtr.
      APPEND st_datarec TO gt_data1.
      CLEAR gs_final.
    ENDIF.
    CLEAR ls_dc.
  ENDLOOP.
*&--End of new logic for the C55
ENDFORM.                    " PRINT_REPORT1
*&---------------------------------------------------------------------*
*&      Form  SUMARIZE_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sumarize_data1 .
  CLEAR t_coep_sum1[].
  CLEAR s_coep.
  LOOP AT t_coep1 INTO s_coep.
*Last sorted field is kstar
    AT END OF kstar.
      SUM.
      APPEND s_coep TO t_coep_sum1.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " SUMARIZE_DATA1
*&---------------------------------------------------------------------*
*&      Form  SAVE_FILE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_file1 .
  IF lv_local1 = 0.

*BOC by KMB on 22.9.2020 CHG0193497 IDF performance
*    PERFORM open_csvfile.
*    LOOP AT gt_data INTO st_datarec.
*      TRANSFER st_datarec TO csvfile.
*    ENDLOOP.

*    PERFORM close_csvfile.

    PERFORM open_csvfile1.

    LOOP AT gt_data1 INTO st_datarec.
      TRANSFER st_datarec TO csvfile1.
    ENDLOOP.

    PERFORM close_csvfile1.
*EOC by KMB on 22.9.2020 CHG0193497 IDF performance

    WRITE: text-009, csvfile1.

  ELSE.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = p_file1
      TABLES
        data_tab                = gt_data1
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
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    WRITE: text-011, p_file1.
  ENDIF.

ENDFORM.                    " SAVE_FILE1
*&---------------------------------------------------------------------*
*&      Form  OPEN_CSVFILE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OPEN_CSVFILE1 .
  OPEN DATASET csvfile1 FOR OUTPUT IN TEXT MODE MESSAGE msg ENCODING DEFAULT.
  IF sy-subrc NE '0'.
    MESSAGE e002 WITH csvfile1 msg.
    STOP.
  ENDIF.
ENDFORM.                    " OPEN_CSVFILE1
*&---------------------------------------------------------------------*
*&      Form  CLOSE_CSVFILE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSE_CSVFILE1 .
  CLOSE DATASET csvfile1.
  IF sy-subrc NE '0'.
    MESSAGE e019 WITH text-010 csvfile1 msg.
    STOP.
  ENDIF.
ENDFORM.                    " CLOSE_CSVFILE1
