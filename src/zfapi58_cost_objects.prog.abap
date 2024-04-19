*&---------------------------------------------------------------------*
*& Report  ZFAPI58_COST_OBJECTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 07/05/2019   akmadasu D30K929808    REMOVE "" Codes from desc field  *
*&---------------------------------------------------------------------*

REPORT  zfapi58_cost_objects.
TABLES: csks,
        coas,
        caufv,
        prps,
        cobrb,
        aufk.
TYPES: BEGIN OF ty_output,
        cost_object TYPE string,
        sp1(1),
        object_name TYPE string,
        sp2(1),
        object_desc TYPE string,
        sp3(1),
        currency TYPE string,
        sp4(1),
*        owner TYPE string,
*        sp5(1),
        eccid TYPE string,
       END OF ty_output.
CONSTANTS:
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.
DATA: gv_filename TYPE string,
      gt_output TYPE TABLE OF ty_output,
      gt_output1 TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X'
                                      USER-COMMAND cmd,
            p_sfile   LIKE        rfpdo-rfbifile MODIF ID srv, "dsp,
"            p_ifile   LIKE        rfpdo-rfbifile MODIF ID srv,
            r_local   RADIOBUTTON GROUP rad1,
            p_file    TYPE        rfpdo-rfbifile DEFAULT 'H:\'
                                                  MODIF ID lcl,
            p_eccid(2) DEFAULT 'CE' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
PARAMETERS: p_incc TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_kostl FOR csks-kostl,
                s_cbukrs FOR csks-bukrs, " OBLIGATORY,
                s_kosar FOR csks-kosar,
                s_datbi FOR csks-datbi OBLIGATORY "DEFAULT sy-datum
                                       no intervals.
PARAMETERS:     p_bkzkp TYPE csks-bkzkp.
SELECTION-SCREEN END OF BLOCK b2.

PARAMETERS: p_incio TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: s_aufnr FOR coas-aufnr,
                s_auart FOR coas-auart,
                s_autyp FOR coas-autyp,
                s_ibukrs FOR coas-bukrs, " OBLIGATORY,
                s_werks FOR coas-werks,
                s_perbz for cobrb-perbz OBLIGATORY
                                        no INTERVALS,
                s_avorg FOR cobrb-avorg OBLIGATORY
                                        no INTERVALS.
PARAMETERS:     p_days  TYPE i OBLIGATORY DEFAULT 365,
                p_cdate  TYPE sy-datum OBLIGATORY
                                       DEFAULT sy-datum.
PARAMETERS: p_phas1 TYPE coas-phas1 DEFAULT 'X' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.

PARAMETERS: p_incsto TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE text-006.
SELECT-OPTIONS: "s_aufnr FOR coas-aufnr,
                s_auart2 FOR aufk-auart,
                s_autyp2 FOR aufk-autyp,
                s_sbukrs FOR aufk-bukrs, " OBLIGATORY,
                s_swerks FOR aufk-werks.
"s_IDAT1 FOR aufk-AUFK-IDAT1.
PARAMETERS: p_phas12 TYPE aufk-phas1 DEFAULT 'X',
            p_PHAS32 TYPE AUFK-PHAS3.
SELECTION-SCREEN END OF BLOCK b6.

PARAMETERS: p_incnet TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
SELECT-OPTIONS: s_naufnr FOR caufv-aufnr,
                s_nauart FOR caufv-auart,
                s_nautyp FOR caufv-autyp DEFAULT '20', " OBLIGATORY,
                s_nbukrs FOR caufv-bukrs, " OBLIGATORY,
                s_nwerks FOR caufv-werks.
PARAMETERS:     p_nloekz TYPE caufv-loekz.
SELECTION-SCREEN END OF BLOCK b4.

PARAMETERS: p_incwbs TYPE xfeld DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-005.
SELECT-OPTIONS: "s_pspnr FOR prps-pspnr,
                s_posid FOR prps-posid,
                s_pbukr FOR prps-pbukr, " OBLIGATORY.
                s_perbzw for cobrb-perbz OBLIGATORY
                                        no INTERVALS,
                s_avorgw FOR cobrb-avorg OBLIGATORY
                                        no INTERVALS.
PARAMETERS:     p_daysw  TYPE i OBLIGATORY DEFAULT 365,
                p_cdatew  TYPE sy-datum OBLIGATORY
                                       DEFAULT sy-datum.
PARAMETERS: p_belkz TYPE prps-belkz DEFAULT 'X',
            p_loevm TYPE prps-loevm.
SELECTION-SCREEN END OF BLOCK b5.

INITIALIZATION.
  s_auart-sign = 'E'.
  s_auart-option = 'EQ'.
  s_auart-low = '100'.
  APPEND s_auart.
  s_auart-sign = 'E'.
  s_auart-option = 'EQ'.
  s_auart-low = '275'.
  APPEND s_auart.
  s_auart-sign = 'E'.
  s_auart-option = 'EQ'.
  s_auart-low = '300'.
  APPEND s_auart.
  s_auart-sign = 'E'.
  s_auart-option = 'EQ'.
  s_auart-low = '325'.
  APPEND s_auart.
  s_auart-sign = 'E'.
  s_auart-option = 'EQ'.
  s_auart-low = '410'.
  APPEND s_auart.
  s_auart-sign = 'E'.
  s_auart-option = 'EQ'.
  s_auart-low = 'IC01'.
  APPEND s_auart.
  s_auart-sign = 'E'.
  s_auart-option = 'EQ'.
  s_auart-low = 'M001'.
  APPEND s_auart.

  s_auart2-sign = 'I'.
  s_auart2-option = 'EQ'.
  s_auart2-low = '0500'.
  APPEND s_auart2.
  s_auart2-sign = 'I'.
  s_auart2-option = 'EQ'.
  s_auart2-low = '0550'.
  APPEND s_auart2.
  s_auart2-sign = 'I'.
  s_auart2-option = 'EQ'.
  s_auart2-low = '0600'.
  APPEND s_auart2.

  s_datbi-sign = 'I'.
  s_datbi-option = 'GE'.
  s_datbi-low = sy-datum.
  APPEND s_datbi.

  s_perbz-sign = 'I'.
  s_perbz-option = 'EQ'.
  s_perbz-low = 'PER'.
  APPEND s_perbz.
  s_perbz-sign = 'I'.
  s_perbz-option = 'EQ'.
  s_perbz-low = 'FUL'.
  APPEND s_perbz.
  s_avorg-sign = 'I'.
  s_avorg-option = 'EQ'.
  s_avorg-low = 'KOAO'.
  append s_avorg.

AT SELECTION-SCREEN OUTPUT.
  PERFORM  toggle_functionality.

AT SELECTION-SCREEN ON s_datbi.
  IF s_datbi-low < sy-datum.
    MESSAGE e000(zfi01) WITH 'Valid-To Date should be >= to current Date'
                              ' ' ' '  ' '.
  ENDIF.

START-OF-SELECTION.

  "Cost Center
  IF p_incc is NOT INITIAL AND
     s_cbukrs[] is INITIAL AND
     s_datbi is INITIAL.
    WRITE : / 'Company Code and Valid-to Date is required for Cost Center parameters.'.
    stop.
  ENDIF.
  "Internal Order
  IF p_incio is NOT INITIAL AND
     s_ibukrs[] is INITIAL.
    WRITE : / 'Company Code is required for Internal Order parameters.'.
    stop.
  ENDIF.
  "Statistical order
  IF p_incsto is NOT INITIAL AND
     s_sbukrs[] is INITIAL.
    WRITE : / 'Company Code is required for Statistical Order parameters.'.
    stop.
  ENDIF.
  "Network
  IF p_incnet is NOT INITIAL AND
     s_nbukrs[] is INITIAL AND
     s_nautyp[] is INITIAL.
    WRITE : / 'Company Code and Type are required for Network parameters.'.
    stop.
  ENDIF.
  "WBS
  IF p_incwbs is NOT INITIAL AND
     s_pbukr[] is INITIAL.
    WRITE : / 'Company Code is required for WBS Parameters.'.
    stop.
  ENDIF.
  IF r_server IS NOT INITIAL AND
     p_sfile IS INITIAL.
    WRITE : / 'Enter Application Server File Path Only'.
    STOP.
  ENDIF.
  IF r_local IS NOT INITIAL AND
     p_file IS INITIAL.
    WRITE : / 'Enter PC File Path Only'.
    STOP.
  ENDIF.
  IF r_local IS NOT INITIAL.
    CONCATENATE p_file 'FG_COSTOBJ' '_' p_eccid '_' sy-datum '_'
                sy-uzeit '.TXT' INTO gv_filename.
  ELSE.
    CONCATENATE p_sfile 'FG_COSTOBJ' '_' p_eccid '_' sy-datum '_'
                sy-uzeit '.TXT' INTO gv_filename.
  ENDIF.
  CLEAR: gt_output,
         gt_output1,
         gs_output.

  PERFORM print_header.

  if p_incc is NOT INITIAL.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = 0
        TEXT       = 'Cost Centers extraction is in progress'.

    PERFORM get_costcenters.
  ENDIF.
  "--------------
  if p_incio is not INITIAL.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = 10
        TEXT       = 'Internal Orders extraction is in progress'.
    PERFORM get_ios.
  endif.
  "------------------
  IF p_incsto is NOT INITIAL.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = 30
        TEXT       = 'Statistical Orders extraction is in progress'.
    PERFORM get_stat_order.
  ENDIF.
  "-------------------------
  if p_incnet is NOT INITIAL.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = 40
        TEXT       = 'Network & Activity extraction is in progress'.
    PERFORM get_networks_new.
  endif.
  "------------------------------------
  if p_incwbs is NOT INITIAL.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        PERCENTAGE = 65
        TEXT       = 'Project/WBS extraction is in progress'.
    PERFORM get_wbs.
  endif.
  "----------------------
  IF gt_output[] IS NOT INITIAL.
    PERFORM download_data.
  ELSE.
    WRITE: / ' No Cost objects data to download..'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM toggle_functionality .
  LOOP AT SCREEN.
* Set the screen fields to display only
    IF  screen-group1 EQ gc_modif_id_dsp.
      screen-input = 0.
    ENDIF.
    IF r_local = 'X'.
      IF screen-group1 = 'LCL'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'LCL'.
        screen-input = 0.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.
ENDFORM.                    " TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  GET_COSTCENTERS
*&---------------------------------------------------------------------*
*       Get Cost Centers
*----------------------------------------------------------------------*
FORM get_costcenters.

  DATA: lv_bukrs(4),
        lv_kostl(10),
        lt_csks TYPE TABLE OF csks,
        ls_csks TYPE csks,
        lt_cskt TYPE TABLE OF cskt,
        ls_cskt TYPE cskt.

  SELECT *  FROM csks INTO TABLE lt_csks WHERE kostl IN s_kostl
                                           AND datbi in s_datbi
                                           AND bukrs IN s_cbukrs
                                           AND kosar IN s_kosar
                                           AND bkzkp = p_bkzkp.
  IF lt_csks[] IS NOT INITIAL.
    SELECT * FROM cskt INTO TABLE lt_cskt
      FOR ALL ENTRIES IN lt_csks
           WHERE spras = sy-langu
             AND kokrs = lt_csks-kokrs
             AND kostl = lt_csks-kostl
             AND datbi = lt_csks-datbi.
  ENDIF.

  LOOP AT lt_csks INTO ls_csks.
    CLEAR: ls_cskt.
    READ TABLE lt_cskt INTO ls_cskt
                    WITH KEY spras = sy-langu
                             kokrs = ls_csks-kokrs
                             kostl = ls_csks-kostl
                             datbi = ls_csks-datbi.

    "remove leading zero
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_csks-kostl
      IMPORTING
        OUTPUT = lv_kostl.

    lv_bukrs = ls_csks-bukrs.
    CONDENSE lv_bukrs.
    CONCATENATE 'CCC |' lv_bukrs '|' lv_kostl  "ls_csks-kostl
                        INTO gs_output-cost_object
                        SEPARATED BY space.
    CONDENSE ls_cskt-ktext.
    CONDENSE ls_cskt-ltext.
    CONDENSE ls_csks-waers.
    CONDENSE p_eccid.
    gs_output-sp1 = '~'.
    gs_output-sp2 = '~'.
    gs_output-sp3 = '~'.
    gs_output-sp4 = '~'.
*    gs_output-sp5 = '~'.
    if ls_cskt-ktext is NOT INITIAL.
      gs_output-object_name = ls_cskt-ktext.
    else.
      gs_output-object_name = 'Name missing'.
    endif.
    if ls_cskt-ltext is NOT INITIAL.
      gs_output-object_desc = ls_cskt-ltext.
    else.
      gs_output-object_desc = 'Name missing'.
    endif.
    gs_output-currency = ls_csks-waers.
*    gs_output-owner = space.
    gs_output-eccid = p_eccid.
    "remove uwanted characters from text.
    PERFORM string_remove_spcl_char CHANGING gs_output-object_name.
    PERFORM string_remove_spcl_char CHANGING gs_output-object_desc.
    "------------------
    APPEND gs_output TO gt_output.
  ENDLOOP.
ENDFORM.                    " GET_COSTCENTERS
*&---------------------------------------------------------------------*
*&      Form  GET_IOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ios .
  DATA: lv_period TYPE cobrb-letpe,
        lv_year TYPE cobrb-letja,
        lv_days TYPE i,
        lv_bukrs(4),
        lv_aufnr(12),
        lt_coas TYPE TABLE OF coas,
        ls_coas TYPE coas,
        lt_cobrb TYPE TABLE OF cobrb,
        ls_cobrb TYPE cobrb,
        lt_jest TYPE TABLE OF jest,
        ls_jest TYPE jest.
  CONSTANTS: lc_release TYPE jest-stat VALUE 'I0002',
             lc_close TYPE jest-stat VALUE 'I0046',
             lc_TECO TYPE jest-stat VALUE 'I0045',
             lc_caa  TYPE jest-stat VALUE 'I0064',
             lc_locked TYPE jest-stat VALUE 'I0043'.


  SELECT * FROM coas INTO TABLE lt_coas
    WHERE aufnr IN s_aufnr
      AND auart IN s_auart
      AND autyp IN s_autyp
      AND bukrs IN s_ibukrs
      AND werks IN s_werks
      AND ( phas1 = 'X' OR "= p_phas1.
            phas2 = 'X' ).
  IF lt_coas[] IS NOT INITIAL.
    SELECT * FROM cobrb INTO TABLE lt_cobrb
      FOR ALL ENTRIES IN lt_coas
      WHERE objnr = lt_coas-objnr
        AND perbz In s_perbz "( perbz = 'PER' or perbz = 'FUL' )
        AND avorg in s_avorg. "= 'KOAO'.
    "------------------
  ENDIF.
  LOOP AT lt_coas INTO ls_coas.

    lv_days = sy-datum - ls_coas-erdat.
    IF lv_days > p_days. "365.
      CLEAR ls_cobrb.
      lv_year = p_cdate(4). "sy-datum(4).
      lv_period = p_cdate+4(2). "sy-datum+4(2).
      READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = ls_coas-objnr.
      IF ls_cobrb-letja <> lv_year.
        lv_year = lv_year - 1.
        IF ls_cobrb-letja < lv_year.
          CONTINUE.
        ENDIF.
        IF ls_cobrb-letja = lv_year AND
           ls_cobrb-letpe < lv_period.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_coas-aufnr
      IMPORTING
        OUTPUT = lv_aufnr.

    lv_bukrs = ls_coas-bukrs.
    CONDENSE lv_aufnr.
    CONDENSE lv_bukrs.
    CONCATENATE 'ORD |' lv_bukrs '|' lv_aufnr   "ls_coas-aufnr
                        INTO gs_output-cost_object
                        SEPARATED BY space.
    CONDENSE ls_coas-ktext.
    CONDENSE ls_coas-ktext.
    CONDENSE ls_coas-waers.
    CONDENSE p_eccid.
    gs_output-sp1 = '~'.
    gs_output-sp2 = '~'.
    gs_output-sp3 = '~'.
    gs_output-sp4 = '~'.
*    gs_output-sp5 = '~'.
    if ls_coas-ktext is not INITIAL.
      gs_output-object_name = ls_coas-ktext.
      gs_output-object_desc = ls_coas-ktext.
    else.
      gs_output-object_name = 'Name missing'.
      gs_output-object_desc = 'Name missing'.
    endif.
    gs_output-currency = ls_coas-waers.
*    gs_output-owner = space.
    gs_output-eccid = p_eccid.
    "remove uwanted characters from text.
    PERFORM string_remove_spcl_char CHANGING gs_output-object_name.
    gs_output-object_desc = gs_output-object_name.
    "------------------
    APPEND gs_output TO gt_output.

  ENDLOOP.
ENDFORM.                    " GET_IOS
*&---------------------------------------------------------------------*
*&      Form  GET_NETWORKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_networks .
  DATA: lv_bukrs(4),
        lt_caufv TYPE TABLE OF caufv,
        ls_caufv TYPE caufv.

  SELECT * FROM caufv INTO TABLE lt_caufv
    WHERE aufnr IN s_naufnr
      AND auart IN s_nauart
      AND autyp IN s_nautyp
      AND bukrs IN s_nbukrs
      AND werks IN s_nwerks
      AND loekz = p_nloekz.
  LOOP AT lt_caufv INTO ls_caufv.
    lv_bukrs = ls_caufv-bukrs.
    CONDENSE lv_bukrs.
    CONCATENATE 'NET |' lv_bukrs '|' ls_caufv-aufnr
                        INTO gs_output-cost_object
                        SEPARATED BY space.
    gs_output-object_name = ls_caufv-ktext.
    gs_output-object_desc = ls_caufv-ktext.
    gs_output-currency = ls_caufv-waers.
*    gs_output-owner = space.
    gs_output-eccid = p_eccid.
    APPEND gs_output TO gt_output.
  ENDLOOP.
ENDFORM.                    " GET_NETWORKS
*&---------------------------------------------------------------------*
*&      Form  GET_WBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_wbs.
  DATA: lv_bukrs(4),
        lt_prps TYPE TABLE OF prps,
        ls_prps TYPE prps,
        lt_proj TYPE TABLE OF proj,
        ls_proj TYPE proj,
        lt_cobrb TYPE TABLE OF cobrb,
        ls_cobrb TYPE cobrb,
        lv_days type i,
        lv_PSPNR(24), " TYPE prps-PSPNR.
        lv_period TYPE cobrb-letpe,
        lv_year TYPE cobrb-letja,
        lt_jest TYPE TABLE OF jest,
        ls_jest TYPE jest.
  CONSTANTS: lc_release TYPE jest-stat VALUE 'I0002',
*           lc_close TYPE jest-stat VALUE 'I0046',
             lc_TECO TYPE jest-stat VALUE 'I0045'.
*           lc_caa  TYPE jest-stat VALUE 'I0064',
*           lc_locked TYPE jest-stat VALUE 'I0043'.

  SELECT * FROM prps INTO TABLE lt_prps
    WHERE "pspnr IN s_pspnr
          posid in s_posid
      AND pbukr IN s_pbukr
      AND belkz = p_belkz
      AND loevm = p_loevm.
  IF lt_prps[] IS NOT INITIAL.
    SELECT * FROM cobrb INTO TABLE lt_cobrb
      FOR ALL ENTRIES IN lt_prps
      WHERE objnr = lt_prps-objnr
        AND perbz In s_perbzw
        AND avorg in s_avorgw.
    "------------------
    SELECT * from jest INTO TABLE lt_jest
      FOR ALL ENTRIES IN lt_prps
      WHERE objnr = lt_prps-objnr
        AND ( stat = lc_release or
              stat = lc_teco ).
    "----------------------
    SELECT * from proj INTO TABLE lt_proj
      FOR ALL ENTRIES IN lt_prps
      WHERE PSPNR = lt_prps-psphi.
  ENDIF.
  LOOP AT lt_prps INTO ls_prps.

    READ TABLE lt_jest INTO ls_jest with key
                             objnr = ls_prps-objnr
                             stat  = lc_release
                             inact = space.
    IF sy-subrc <> 0.
      READ TABLE lt_jest INTO ls_jest with key
                              objnr = ls_prps-objnr
                              stat  = lc_teco
                              inact = space.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
    "----------------------
    lv_days = sy-datum - ls_prps-erdat.
    IF lv_days > p_daysw. "365.
      CLEAR ls_cobrb.
      lv_year = p_cdatew(4). "sy-datum(4).
      lv_period = p_cdatew+4(2). "sy-datum+4(2).
      READ TABLE lt_cobrb INTO ls_cobrb WITH KEY objnr = ls_prps-objnr.
      IF ls_cobrb-letja <> lv_year.
        lv_year = lv_year - 1.
        IF ls_cobrb-letja < lv_year.
          CONTINUE.
        ENDIF.
        IF ls_cobrb-letja = lv_year AND
           ls_cobrb-letpe < lv_period.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
    "----------------------
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = ls_prps-pspnr
      IMPORTING
        OUTPUT = lv_PSPNR.

*    CONDENSE ls_prps-pspnr.
    CLEAR ls_proj.
    READ TABLE lt_proj INTO ls_proj with key PSPNR = ls_prps-psphi.
    lv_bukrs = ls_prps-pbukr.
    CONDENSE lv_bukrs.
    CONCATENATE 'WBS |' lv_bukrs '|' lv_PSPNR "ls_prps-pspnr
                        INTO gs_output-cost_object
                        SEPARATED BY space.
    CONDENSE ls_prps-post1.
    CONDENSE ls_prps-pwpos.
    CONDENSE p_eccid.
    CONDENSE ls_proj-post1.
    gs_output-sp1 = '~'.
    gs_output-sp2 = '~'.
    gs_output-sp3 = '~'.
    gs_output-sp4 = '~'.
*    gs_output-sp5 = '~'.
    if ls_prps-post1 IS NOT INITIAL.
      CONCATENATE ls_proj-post1 '|' ls_prps-post1 INTO gs_output-object_name.
      gs_output-object_desc = ls_prps-post1.
    else.
      gs_output-object_name = 'Name missing'.
      gs_output-object_desc = 'Name missing'.
    endif.
    gs_output-currency = ls_prps-pwpos.
*    gs_output-owner = space.
    gs_output-eccid = p_eccid.
    "remove uwanted characters from text.
    PERFORM string_remove_spcl_char CHANGING gs_output-object_name.
    gs_output-object_desc = gs_output-object_name.
    "------------------
    APPEND gs_output TO gt_output.
  ENDLOOP.

ENDFORM.                    " GET_WBS
*&---------------------------------------------------------------------*
*&      Form  GET_NETWORKS_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_networks_new .
  DATA: lv_tabix TYPE sy-tabix,
        lv_bukrs(4),
        lt_caufv TYPE TABLE OF caufv,
        ls_caufv TYPE caufv,
        lt_afvc TYPE TABLE OF afvc,
        ls_afvc TYPE afvc,
        lt_jest TYPE TABLE OF jest,
        ls_jest TYPE jest,
        lt_prps TYPE TABLE OF prps,
        ls_prps TYPE prps,
        ls_jest_proj TYPE jest,
        lt_jest_proj TYPE TABLE OF jest.
  "REL I0002 - Releassed
  CONSTANTS: lc_release TYPE jest-stat VALUE 'I0002',
             lc_close TYPE jest-stat VALUE 'I0046',
             lc_TECO TYPE jest-stat VALUE 'I0045',
             lc_caa  TYPE jest-stat VALUE 'I0064'.

  SELECT * FROM caufv INTO TABLE lt_caufv
    WHERE aufnr IN s_naufnr
      AND auart IN s_nauart
      AND autyp IN s_nautyp
      AND bukrs IN s_nbukrs
      AND werks IN s_nwerks
      AND loekz = p_nloekz.
  SORT lt_caufv BY aufpl.
  DELETE ADJACENT DUPLICATES FROM lt_caufv COMPARING aufpl.
  IF lt_caufv[] IS NOT INITIAL.
    SELECT * FROM afvc INTO TABLE lt_afvc
      FOR ALL ENTRIES IN lt_caufv
      WHERE aufpl = lt_caufv-aufpl.
    if lt_afvc[] is NOT INITIAL.
      SELECT * FROM jest INTO TABLE lt_jest
        FOR ALL ENTRIES IN lt_afvc
        WHERE objnr = lt_afvc-objnr
          AND ( stat  = lc_release OR
                stat  = lc_close    OR
                stat  = lc_caa )
          AND inact = space.
    endif.
    IF lt_afvc[] IS NOT INITIAL.
      SELECT * FROM prps INTO TABLE lt_prps
        FOR ALL ENTRIES IN lt_afvc
        WHERE pspnr = lt_afvc-projn.
      IF lt_prps[] IS NOT INITIAL.
        SELECT * FROM jest INTO TABLE lt_jest_proj
          FOR ALL ENTRIES IN lt_prps
          WHERE objnr = lt_prps-objnr
            AND ( stat  = lc_release OR
                  stat  = lc_close OR
                  stat  = lc_teco )
            AND inact = space.
      ENDIF.
    ENDIF.
  ENDIF.
  SORT lt_afvc BY aufpl aplzl.
  LOOP AT lt_caufv INTO ls_caufv.
    CLEAR: lv_tabix.
    READ TABLE lt_afvc WITH KEY aufpl = ls_caufv-aufpl
                              TRANSPORTING NO FIELDS.
    lv_tabix = sy-tabix.
    LOOP AT lt_afvc INTO ls_afvc FROM lv_tabix.
      IF ls_afvc-aufpl <> ls_caufv-aufpl.
        EXIT.
      ENDIF.
      CLEAR: ls_prps,
             ls_jest_proj,
             ls_jest.
      "Activity is not closed
      READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_afvc-objnr
                                               stat  = lc_close.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      "Activity is not released
      READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_afvc-objnr
                                               stat  = lc_release.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      "Activity closed account assignment
      READ TABLE lt_jest INTO ls_jest WITH KEY objnr = ls_afvc-objnr
                                               stat  = lc_caa.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      "get Project
      READ TABLE lt_prps INTO ls_prps WITH KEY pspnr = ls_afvc-projn.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      "Project/WBS is not closed
      READ TABLE lt_jest_proj INTO ls_jest_proj
                              WITH KEY objnr = ls_prps-objnr
                                       stat  = lc_close.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      "Project_/WBS is not released
      READ TABLE lt_jest_proj INTO ls_jest_proj
                              WITH KEY objnr = ls_prps-objnr
                                       stat  = lc_release  .

      IF sy-subrc <> 0.
        "CONTINUE.
        READ TABLE lt_jest_proj INTO ls_jest_proj
                              WITH KEY objnr = ls_prps-objnr
                                       stat  = lc_teco.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      lv_bukrs = ls_afvc-bukrs. "ls_caufv-bukrs.
      CONDENSE lv_bukrs.
      CONDENSE ls_caufv-aufnr.
      CONDENSE ls_afvc-vornr.
      CONCATENATE 'NET |' lv_bukrs '|' ls_caufv-aufnr '-' ls_afvc-vornr
                          INTO gs_output-cost_object
                          SEPARATED BY space.
      READ TABLE gt_output WITH KEY cost_object = gs_output-cost_object
                                    TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.
      CONDENSE ls_caufv-ktext.
      CONDENSE ls_afvc-ltxa1.
      CONDENSE ls_caufv-waers.
      CONDENSE p_eccid.

      gs_output-sp1 = '~'.
      gs_output-sp2 = '~'.
      gs_output-sp3 = '~'.
      gs_output-sp4 = '~'.
*      gs_output-sp5 = '~'.
      if ls_caufv-ktext is not INITIAL.
        gs_output-object_name = ls_caufv-ktext.  "ls_afvc-ltxa1
      else.
        gs_output-object_name = 'Name missing'.
      endif.
      if ls_afvc-ltxa1 IS NOT INITIAL.
        gs_output-object_desc = ls_afvc-ltxa1.
      else.
        gs_output-object_desc = 'Name missing'.
      endif.
      gs_output-currency = ls_caufv-waers.
*      gs_output-owner = space.
      gs_output-eccid = p_eccid.
      "remove uwanted characters from text.
      PERFORM string_remove_spcl_char CHANGING gs_output-object_name.
      PERFORM string_remove_spcl_char CHANGING gs_output-object_desc.
      "------------------
      APPEND gs_output TO gt_output.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " GET_NETWORKS_NEW
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_data .
**Begin of changes for CHG0126284
** Remove Quotes and commas from the output file.
  loop at gt_output INTO gs_output.
    REPLACE all OCCURRENCES OF '"' in gs_output-OBJECT_DESC WITH space.
    REPLACE all OCCURRENCES OF ',' in gs_output-OBJECT_desc WITH space.
    REPLACE all OCCURRENCES OF '"' in gs_output-OBJECT_name WITH space.
    REPLACE all OCCURRENCES OF ',' in gs_output-OBJECT_name WITH space.
    APPEND gs_output to gt_output1.
  endloop.
*  APPEND LINES OF gt_output to gt_output1.
**End of changes for CHG0126284
  IF r_server IS NOT INITIAL.
    PERFORM download_server.
  ELSE.
    PERFORM download_pc.
  ENDIF.
ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_server .
  DATA: lv_tabfield(1) TYPE x VALUE '09',
        lv_string TYPE string,
        lv_crlf TYPE C.

  lv_crlf = CL_ABAP_CHAR_UTILITIES=>CR_LF.

  OPEN DATASET gv_filename FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    WRITE:/ 'Unable to open file for output.'.
  ELSE.
    LOOP AT gt_output1 INTO gs_output.
      CONCATENATE gs_output-cost_object
                  gs_output-sp1
                  gs_output-object_name
                  gs_output-sp2
                  gs_output-object_desc
                  gs_output-sp3
                  gs_output-currency
                  gs_output-sp4
*                  gs_output-owner
*                  gs_output-sp5
                  gs_output-eccid
                  lv_crlf
      INTO lv_string.
*       SEPARATED BY
*           cl_abap_char_utilities=>horizontal_tab.
      "lv_tabfield.
      TRANSFER lv_string TO gv_filename.
*      TRANSFER gs_output TO gv_filename.
    ENDLOOP.
    CLOSE DATASET gv_filename.
    WRITE: / 'File is downloaded', gv_filename.
  ENDIF.

ENDFORM.                    " DOWNLOAD_SERVER
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_pc .
  DATA: lv_filename TYPE string.
*Download Canadian file
  MOVE gv_filename TO lv_filename.
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename                  = lv_filename
      filetype                  = 'ASC'
*     write_field_separator     = 'X'
      trunc_trailing_blanks_eol = space "'X'
    CHANGING
      data_tab                  = gt_output1
    EXCEPTIONS
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      not_supported_by_gui      = 22
      error_no_gui              = 23
      OTHERS                    = 24.
  IF sy-subrc <> 0.
    WRITE: / 'Error with downloading file at PC ', sy-subrc.
  ELSE.
    WRITE: / 'Successfully created at ', gv_filename.
  ENDIF.
ENDFORM.                    " DOWNLOAD_PC
*&---------------------------------------------------------------------*
*&      Form  PRINT_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_HEADER .

  "---------------------Header
  CLEAR gs_output.
  gs_output-cost_object = 'Type=Upload Cost Objects'.
  APPEND gs_output to gt_output1.
  CLEAR gs_output.
  APPEND gs_output to gt_output1.  "Blank line
  gs_output-sp1 = '~'.
  gs_output-sp2 = '~'.
  gs_output-sp3 = '~'.
  gs_output-sp4 = '~'.
*    gs_output-sp5 = '~'.
  gs_output-cost_object = 'Financial Cost Object Code'.
  gs_output-object_name = 'Financial Cost Object Name'.
  gs_output-object_desc = 'Financial Cost Object Description'..
  gs_output-currency = 'Currency'..
*    gs_output-owner = space.
  gs_output-eccid = 'ECC Code'.
  APPEND gs_output TO gt_output1.
  "-----------------------
ENDFORM.                    " PRINT_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_STAT_ORDER
*&---------------------------------------------------------------------*
*       Statistical orders
*----------------------------------------------------------------------*
FORM GET_STAT_ORDER .
  DATA: lv_kostl TYPE aufk-kostl,
        lv_bukrs TYPE aufk-bukrs,
        lv_aufnr TYPE aufk-aufnr,
        LT_CAUFV TYPE TABLE OF CAUFV,
        LS_CAUFV TYPE caufv,
        lt_Aufk TYPE TABLE OF aufk,
        ls_aufk TYPE aufk.

  select * from aufk INTO TABLE LT_aufk
    WHERE AUART IN s_auart2
      AND AUTYP IN s_autyp2
      AND BUKRS IN s_sbukrs
      AND WERKS IN s_swerks
      AND IDAT1 < SY-DATUM
      AND PHAS1 = p_phas12
      AND PHAS3 = p_PHAS32
      AND KOSTV <> space.
  LOOP AT lt_aufk INTO ls_aufk.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_aufk-aufnr
      IMPORTING
        OUTPUT = lv_aufnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_AUFK-KOSTV
      IMPORTING
        OUTPUT = lv_kostl.
    lv_bukrs = ls_aufk-bukrs.
    CONDENSE lv_aufnr.
    CONDENSE lv_bukrs.
    CONDENSE lv_kostl.
    CONCATENATE 'STT |' lv_bukrs '|' lv_aufnr lv_kostl
                        INTO gs_output-cost_object
                        SEPARATED BY space.
    CONDENSE ls_aufk-ktext.
    CONDENSE ls_aufk-ktext.
    CONDENSE ls_aufk-waers.
    CONDENSE p_eccid.
    gs_output-sp1 = '~'.
    gs_output-sp2 = '~'.
    gs_output-sp3 = '~'.
    gs_output-sp4 = '~'.
*    gs_output-sp5 = '~'.
    if ls_aufk-ktext is not INITIAL.
      gs_output-object_name = ls_aufk-ktext.
      gs_output-object_desc = ls_aufk-ktext.
    else.
      gs_output-object_name = 'Name missing'.
      gs_output-object_desc = 'Name missing'.
    endif.
    gs_output-currency = ls_aufk-waers.
*    gs_output-owner = space.
    gs_output-eccid = p_eccid.
    "remove uwanted characters from text.
    PERFORM string_remove_spcl_char CHANGING gs_output-object_name.
    gs_output-object_desc = gs_output-object_name.
    "------------------
    APPEND gs_output TO gt_output.

  ENDLOOP.
ENDFORM.                    " GET_STAT_ORDER
*&---------------------------------------------------------------------*
*&      Form  STRING_REMOVE_SPCL_CHAR
*&---------------------------------------------------------------------*
*       Remove unwanted characters
*----------------------------------------------------------------------*
FORM STRING_REMOVE_SPCL_CHAR  CHANGING  cv_text TYPE string.

  DATA: lv_strlen TYPE syindex,
        lv_ptr1   TYPE syindex,
        lv_text   TYPE text100.

  FIELD-SYMBOLS: <fs1> TYPE ANY.

  IF cv_text IS INITIAL.
    RETURN.
  ENDIF.
  CLEAR: lv_strlen,
         lv_ptr1.
  lv_text = cv_text.
  ASSIGN lv_text TO <fs1>.
  lv_strlen = STRLEN( lv_text ).
  DO lv_strlen TIMES.
    IF ( <fs1>+lv_ptr1(1) LT SPACE  ) OR
       ( <fs1>+lv_ptr1(1) GT  '~' ).
      "MOVE SPACE TO <fs1>+lv_ptr1(1).
      REPLACE ALL OCCURRENCES OF <fs1>+lv_ptr1(1) in <fs1> with space.
    ENDIF.
    ADD 1 TO lv_ptr1.
  ENDDO.
  cv_text = lv_text.

ENDFORM.                    " STRING_REMOVE_SPCL_CHAR
