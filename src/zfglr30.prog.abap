*&------------------------  ---------------------------------------------*
*& Report  ZFGLR30
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfglr30.
TABLES: catsco,
        catspm.
TYPES: BEGIN OF ty_output,
        workdate  TYPE catsco-workdate,
        raufnr    TYPE catsco-raufnr,
        pernr     TYPE catsco-pernr,
        skostl    TYPE catsco-skostl,
        lstar     TYPE catsco-lstar,
        catshours TYPE catsco-catshours,
        price     TYPE tkgxxx,
        lcost  TYPE tkgxxx,
       END OF ty_output.
DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_wdate FOR catsco-workdate OBLIGATORY,
                s_pernr FOR catsco-pernr,
                s_raufnr FOR catsco-raufnr,
                s_lstar FOR catsco-hrlstar,
                s_skostl FOR catsco-skostl.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  PERFORM get_data.
  IF gt_output[] IS NOT INITIAL.
    PERFORM display_alv.
  ELSE.
    WRITE : / 'No data to output...'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  CONSTANTS: lc_lednr TYPE cost-lednr VALUE '00',
             lc_wrttp TYPE cost-wrttp VALUE '01',
             lc_versn TYPE cost-versn VALUE '000',
             lc_tarkz TYPE cost-tarkz VALUE '003'.
  FIELD-SYMBOLS <comp> TYPE any.
  DATA: lv_gjahr TYPE cost-gjahr,
        lv_objnr TYPE c LENGTH 16,
        lv_tkg TYPE char14,
        lv_month TYPE char2,
        ls_cost TYPE cost,
        lv_price TYPE tkgxxx.

  CLEAR gt_output.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 5
      text       = 'Data Extraction is in progress '.

  SELECT * FROM catsco INTO CORRESPONDING FIELDS OF TABLE gt_output
    WHERE workdate IN s_wdate
      AND pernr IN s_pernr
      AND raufnr IN s_raufnr
      AND lstar  IN s_lstar
      AND skostl IN s_skostl
      AND transfer <> space
      AND stokz = space.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 20
      text       = 'Data Extraction is in progress '.
  SELECT * FROM catspm APPENDING CORRESPONDING FIELDS OF TABLE gt_output
    WHERE workdate IN s_wdate
      AND pernr IN s_pernr
      AND raufnr IN s_raufnr
      AND lstar IN s_lstar
      AND skostl IN s_skostl
      AND transfer <> space
      AND stokz = space.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 40
      text       = 'Data Extraction is in progress '.

  LOOP AT gt_output INTO gs_output.
    lv_gjahr = gs_output-workdate(4).
    lv_month = gs_output-workdate+4(2).
    CONCATENATE gs_output-skostl gs_output-lstar INTO lv_objnr.
    CLEAR ls_cost.
    SELECT * FROM cost INTO ls_cost
        WHERE lednr = lc_lednr
          AND gjahr = lv_gjahr
          AND wrttp = lc_wrttp
          AND versn = lc_versn
          AND tarkz = lc_tarkz.
      CHECK ls_cost-objnr+6(16) = lv_objnr.
      CONCATENATE 'TKG0' lv_month INTO lv_tkg.
      ASSIGN COMPONENT lv_tkg OF STRUCTURE ls_cost TO <comp>.
      IF sy-subrc = 0.
        lv_price = <comp>.
        gs_output-price = lv_price.
        gs_output-lcost = lv_price * gs_output-catshours.
        EXIT.
      ENDIF.
    ENDSELECT.
    MODIFY gt_output FROM gs_output TRANSPORTING lcost price.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv.

  DATA: ls_key         TYPE salv_s_layout_key,
        lr_table       TYPE REF TO cl_salv_table,
        lr_functions   TYPE REF TO cl_salv_functions,
        lr_display     TYPE REF TO cl_salv_display_settings,
        lr_columns     TYPE REF TO cl_salv_columns_table,
        lr_column      TYPE REF TO cl_salv_column_table.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lr_table
        CHANGING
          t_table      = gt_output.
    CATCH cx_salv_msg .
  ENDTRY.
*Function settings
  lr_functions = lr_table->get_functions( ).
  lr_functions->set_all( abap_true ).
*Display Setting
  lr_display = lr_table->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
*Get columns
  CALL METHOD lr_table->get_columns
    RECEIVING
      value = lr_columns.
******Change ALV Fields  - title etc.
  PERFORM alv_fields USING lr_columns lr_column.
******Display ALV
  CALL METHOD lr_table->display.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_fields USING    io_columns TYPE REF TO cl_salv_columns_table
                         io_column  TYPE REF TO cl_salv_column_table.

  DATA: lv_column     TYPE lvc_fname,
        lv_long_text  TYPE scrtext_l,
        lv_short_text TYPE scrtext_s,
        lv_med_text   TYPE scrtext_m.

  TRY.
      lv_column = 'LCOST'.
      lv_long_text = 'Standard Labor Cost'.
      lv_short_text = 'Std.LCost'.
      lv_med_text   = 'Std.Lbr.Cost'.
      io_column ?= io_columns->get_column( lv_column ).
      CALL METHOD io_column->set_long_text
        EXPORTING
          value = lv_long_text.
      CALL METHOD io_column->set_short_text
        EXPORTING
          value = lv_short_text.
      CALL METHOD io_column->set_medium_text
        EXPORTING
          value = lv_med_text.
      lv_column = 'PRICE'.
      lv_long_text = 'Labor Rate'.
      lv_short_text = 'Lbr. Rate'.
      lv_med_text   = 'Labor Rate'.
      io_column ?= io_columns->get_column( lv_column ).
      CALL METHOD io_column->set_long_text
        EXPORTING
          value = lv_long_text.
      CALL METHOD io_column->set_short_text
        EXPORTING
          value = lv_short_text.
      CALL METHOD io_column->set_medium_text
        EXPORTING
          value = lv_med_text.
    CATCH cx_salv_not_found .
  ENDTRY.

ENDFORM.                    " ALV_FIELDS
