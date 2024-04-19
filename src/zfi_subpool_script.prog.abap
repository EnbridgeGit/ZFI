*&---------------------------------------------------------------------*
*& Subroutine Pool   ZFI_SUBPOOL_SCRIPT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

PROGRAM  zfi_subpool_script.

*&---------------------------------------------------------------------*
*&      Form  f_get_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_PAR     text
*      -->OUT_PAR    text
*----------------------------------------------------------------------*
FORM f_get_text TABLES in_par STRUCTURE itcsy
                       out_par STRUCTURE itcsy.

  DATA :  lv_runon TYPE regup-laufd,
          lv_date TYPE c LENGTH 10,
          lv_id TYPE regup-laufi,
          lv_len TYPE n LENGTH 4,
          lit_regup TYPE TABLE OF regup,
          lwa_regup TYPE regup,
          lv_name TYPE thead-tdname,
          lv_count TYPE n LENGTH 4,
          lv_oldcount TYPE thead-tdtxtlines,
          lit_lines TYPE TABLE OF tline,
         lv_xvorl TYPE regup-xvorl,
         lv_zbukr TYPE regup-zbukr,
         lv_lifnr TYPE regup-lifnr,
         lv_kunnr TYPE regup-kunnr,
         lv_empfg TYPE regup-empfg,
         lv_vblnr TYPE regup-vblnr,
         lv_bukrs TYPE regup-bukrs,
         lv_belnr TYPE regup-belnr,
         lv_gjahr TYPE regup-gjahr,
         lv_buzei TYPE regup-buzei.

  FIELD-SYMBOLS <out_par> TYPE itcsy.

  READ TABLE in_par WITH KEY 'REGUP-LAUFD'.
  IF sy-subrc = 0.
    lv_date = in_par-value.
    CONCATENATE lv_date+0(4) lv_date+5(2) lv_date+8(2) INTO lv_runon.
  ENDIF.

  READ TABLE in_par WITH KEY 'REGUP-LAUFI'.
  IF sy-subrc = 0.
    lv_id = in_par-value.
  ENDIF.

  SELECT *
   FROM regup
   INTO TABLE lit_regup
   WHERE laufd = lv_runon AND
         laufi = lv_id AND
         zlsch = 'P'.
  IF sy-subrc = 0.

    LOOP AT lit_regup INTO lwa_regup.
      lv_len = strlen( lwa_regup-bukrs ).
      IF lv_len < '4'.
        CONCATENATE lwa_regup-bukrs lwa_regup-belnr INTO lv_name SEPARATED BY space.
      ELSEIF lv_len = '4'.
        CONCATENATE lwa_regup-bukrs lwa_regup-belnr INTO lv_name.
      ENDIF.
      CONCATENATE lv_name lwa_regup-gjahr INTO lv_name.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client           = sy-mandt
          id               = '0003'
          language         = sy-langu
          name             = lv_name
          object           = 'BELEG'
        IMPORTING
          old_line_counter = lv_oldcount
        TABLES
          lines            = lit_lines.
      IF sy-subrc = 0.
        lv_count = lv_count + lv_oldcount.
      ENDIF.
    ENDLOOP.

    READ TABLE in_par WITH KEY 'REGUP-XVORL'.
    IF sy-subrc = 0.
      lv_xvorl = in_par-value.
    ENDIF.

    READ TABLE in_par WITH KEY 'REGUP-ZBUKR'.
    IF sy-subrc = 0.
      lv_zbukr = in_par-value.
    ENDIF.

    READ TABLE in_par WITH KEY 'REGUP-LIFNR'.
    IF sy-subrc = 0.
      lv_lifnr = in_par-value.
    ENDIF.

    READ TABLE in_par WITH KEY 'REGUP-KUNNR'.
    IF sy-subrc = 0.
      lv_kunnr = in_par-value.
    ENDIF.

    READ TABLE in_par WITH KEY 'REGUP-EMPFG'.
    IF sy-subrc = 0.
      lv_empfg = in_par-value.
    ENDIF.

    READ TABLE in_par WITH KEY 'REGUP-VBLNR'.
    IF sy-subrc = 0.
      lv_vblnr = in_par-value.
    ENDIF.

    READ TABLE in_par WITH KEY 'REGUP-BUKRS'.
    IF sy-subrc = 0.
      lv_bukrs = in_par-value.
    ENDIF.

    READ TABLE in_par WITH KEY 'REGUP-BELNR'.
    IF sy-subrc = 0.
      lv_belnr = in_par-value.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_belnr
        IMPORTING
          output = lv_belnr.
    ENDIF.

    READ TABLE in_par WITH KEY 'REGUP-GJAHR'.
    IF sy-subrc = 0.
      lv_gjahr = in_par-value.
    ENDIF.

    READ TABLE in_par WITH KEY 'REGUP-BUZEI'.
    IF sy-subrc = 0.
      lv_buzei = in_par-value.
    ENDIF.

    SORT lit_regup ASCENDING BY laufd laufi xvorl zbukr lifnr kunnr empfg vblnr bukrs belnr gjahr buzei.
    READ TABLE lit_regup INTO lwa_regup INDEX 1.
    IF sy-subrc = 0.
      IF   lv_runon = lwa_regup-laufd AND
           lv_id = lwa_regup-laufi AND
           lv_xvorl = lwa_regup-xvorl AND
           lv_zbukr = lwa_regup-zbukr AND
           lv_lifnr = lwa_regup-lifnr AND
           lv_kunnr = lwa_regup-kunnr AND
           lv_empfg = lwa_regup-empfg AND
           lv_vblnr = lwa_regup-vblnr AND
           lv_bukrs = lwa_regup-bukrs AND
           lv_belnr = lwa_regup-belnr AND
           lv_gjahr = lwa_regup-gjahr AND
           lv_buzei = lwa_regup-buzei.
        IF lv_count > '25'.
          READ TABLE out_par ASSIGNING <out_par> WITH KEY name = 'FLAG'.
          CHECK <out_par> IS ASSIGNED.
          <out_par>-value = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.
    READ TABLE out_par ASSIGNING <out_par> WITH KEY name = 'COUNT'.
    CHECK <out_par> IS ASSIGNED.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lv_count
      IMPORTING
        output = lv_count.
    <out_par>-value = lv_count.
  ENDIF.

ENDFORM.                    "f_get_text
