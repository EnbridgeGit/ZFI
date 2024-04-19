*&---------------------------------------------------------------------*
*& Report  ZFAPR20_INV_GL_TAX
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zfapr21_inv_gl_tax.
*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: hotspot_click  FOR EVENT link_click OF cl_salv_events_table
                         IMPORTING row column.
ENDCLASS.               "LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl_event_handler
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD hotspot_click.
    PERFORM handle_click USING row column.
  ENDMETHOD.                    "hotspot_click1
ENDCLASS.               "lcl_event_handler

TABLES: bkpf,
        bsik,
        bsis.
TYPES: BEGIN OF ty_output,
        bukrs  TYPE bkpf-bukrs,
        lifnr  TYPE bsik-lifnr,
        name1  TYPE lfa1-name1,
        blart  TYPE bkpf-blart,
        gjahr  TYPE bkpf-gjahr,
        belnr  TYPE bkpf-belnr,
        bstat  TYPE bkpf-bstat,
        bldat  TYPE bkpf-bldat,
        waers  TYPE bkpf-waers,
        budat  TYPE bkpf-budat,
        monat  TYPE bkpf-monat,
        usnam  TYPE bkpf-usnam,
        xblnr  TYPE bkpf-xblnr,
        bschl  TYPE bsik-bschl,
        shkzg  TYPE bsik-shkzg,
        dmbtr  TYPE bsik-dmbtr, "local Cur
        wrbtr  TYPE bsik-wrbtr, "document Cur
        mwskz  TYPE bsik-mwskz,
        hkont  TYPE bsis-hkont,
        taxdoc TYPE bsis-wrbtr,
        taxlcl TYPE bsis-dmbtr,

       END OF ty_output.

CONSTANTS: c_saknr1  TYPE bsis-hkont VALUE '0000256950',  "256601 for west
           c_saknr2  TYPE bsis-hkont VALUE '0000256961'.  "256606 for west

DATA: gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs  FOR bkpf-bukrs OBLIGATORY,
                s_budat  FOR bkpf-budat,
                s_bldat  FOR bkpf-bldat,
                s_blart  FOR bkpf-blart,
*                s_saknr  FOR bsik-saknr,
                s_mwskz  FOR bsis-mwskz,
                s_lifnr  FOR bsik-lifnr,
                s_bstat  FOR bkpf-bstat.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  IF s_bukrs  IS INITIAL AND
     s_budat  IS INITIAL AND
     s_bldat  IS INITIAL AND
     s_blart  IS INITIAL AND
*     s_saknr  IS INITIAL AND
     s_mwskz  IS INITIAL AND
     s_lifnr  IS INITIAL AND
     s_bstat IS INITIAL.
    WRITE : / 'Please enter selection parameter(s)'.
    STOP.
  ENDIF.

  PERFORM get_data.
  PERFORM display_alv.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Extract data
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lv_tabix1 TYPE sy-tabix,
        lv_tabix2 TYPE sy-tabix,
        lv_check  TYPE c,
        lv_exist  TYPE c,
        lt_vbsegk TYPE TABLE OF vbsegk,
        ls_vbsegk TYPE vbsegk,
        lt_vbsegs TYPE TABLE OF vbsegs,
        ls_vbsegs TYPE vbsegs,
        lt_bkpf   TYPE TABLE OF bkpf,
        ls_bkpf   TYPE bkpf,
        lt_bsik   TYPE TABLE OF bsik,
        ls_bsik   TYPE bsik,
        lt_bsis   TYPE TABLE OF bsis,
        ls_bsis   TYPE bsis,
        lt_lfa1   TYPE TABLE OF lfa1,
        ls_lfa1   TYPE lfa1,
        lt_bsak   TYPE TABLE OF bsak,
        ls_bsak   TYPE bsak,
        lt_bsas   TYPE TABLE OF bsas,
        ls_bsas   TYPE bsas.

  CLEAR: gt_output.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 0
      text       = 'Extracting Header data '.

  SELECT * FROM bkpf INTO TABLE lt_bkpf
    WHERE bukrs IN s_bukrs
      AND budat IN s_budat
      AND bldat IN s_bldat
      AND blart IN s_blart
      AND bstat IN s_bstat.

  IF lt_bkpf[] IS INITIAL.
    WRITE : / 'No data to output..'.
    STOP.
  ENDIF.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 20
      text       = 'Extracting Line items data '.
  SELECT * FROM bsik INTO TABLE lt_bsik
    FOR ALL ENTRIES IN lt_bkpf
    WHERE bukrs = lt_bkpf-bukrs
      AND belnr = lt_bkpf-belnr
      AND gjahr = lt_bkpf-gjahr
      AND lifnr IN s_lifnr.
  SELECT * FROM bsak INTO TABLE lt_bsak
   FOR ALL ENTRIES IN lt_bkpf
   WHERE bukrs = lt_bkpf-bukrs
     AND belnr = lt_bkpf-belnr
     AND gjahr = lt_bkpf-gjahr
     AND lifnr IN s_lifnr.
*        AND saknr in s_saknr.
  SELECT * FROM vbsegk INTO TABLE lt_vbsegk
    FOR ALL ENTRIES IN lt_bkpf
    WHERE bukrs = lt_bkpf-bukrs
      AND belnr = lt_bkpf-belnr
      AND gjahr = lt_bkpf-gjahr
      AND lifnr IN s_lifnr.
  SELECT * FROM bsis INTO  TABLE lt_bsis
    FOR ALL ENTRIES IN lt_bkpf
    WHERE bukrs = lt_bkpf-bukrs
      AND belnr = lt_bkpf-belnr
      AND gjahr = lt_bkpf-gjahr
      AND mwskz IN s_mwskz
      AND ( hkont = c_saknr1 OR
            hkont = c_saknr2 ).
  SELECT * FROM bsas INTO  TABLE lt_bsas
   FOR ALL ENTRIES IN lt_bkpf
   WHERE bukrs = lt_bkpf-bukrs
     AND belnr = lt_bkpf-belnr
     AND gjahr = lt_bkpf-gjahr
     AND mwskz IN s_mwskz
     AND ( hkont = c_saknr1 OR
           hkont = c_saknr2 ).
  SELECT * FROM vbsegs INTO  TABLE lt_vbsegs
   FOR ALL ENTRIES IN lt_bkpf
   WHERE bukrs = lt_bkpf-bukrs
     AND belnr = lt_bkpf-belnr
     AND gjahr = lt_bkpf-gjahr
     AND mwskz IN s_mwskz
     AND ( saknr = c_saknr1 OR
           saknr = c_saknr2 ).
  SELECT * FROM lfa1 INTO TABLE lt_lfa1.
  SORT lt_bkpf BY bukrs belnr gjahr.
  SORT lt_vbsegk BY bukrs belnr gjahr mwskz.
  SORT lt_bsik BY bukrs belnr gjahr mwskz.
  SORT lt_bsak BY bukrs belnr gjahr mwskz.
  SORT lt_vbsegs BY bukrs belnr gjahr saknr mwskz.
  SORT lt_bsis BY bukrs belnr gjahr hkont mwskz.
  SORT lt_bsas BY bukrs belnr gjahr hkont mwskz.
  LOOP AT lt_bkpf INTO ls_bkpf.
    CLEAR: lv_exist,
           lv_tabix1,
           gs_output,
           ls_lfa1.
    gs_output-bukrs = ls_bkpf-bukrs.
    gs_output-blart = ls_bkpf-blart.
    gs_output-gjahr = ls_bkpf-gjahr.
    gs_output-belnr = ls_bkpf-belnr.
    gs_output-bstat = ls_bkpf-bstat.
    gs_output-bldat = ls_bkpf-bldat.
    gs_output-waers = ls_bkpf-waers.
    gs_output-budat = ls_bkpf-budat.
    gs_output-monat = ls_bkpf-monat.
    gs_output-usnam = ls_bkpf-usnam.
    gs_output-xblnr = ls_bkpf-xblnr.
    IF ls_bkpf-bstat = 'V'.  "Parked document
      READ TABLE lt_vbsegk WITH KEY bukrs = ls_bkpf-bukrs
                                    belnr = ls_bkpf-belnr
                                    gjahr = ls_bkpf-gjahr
                                    TRANSPORTING NO FIELDS.
      lv_tabix1 = sy-tabix.
      LOOP AT lt_vbsegk INTO ls_vbsegk FROM lv_tabix1.
        IF ls_vbsegk-bukrs <> ls_bkpf-bukrs OR
           ls_vbsegk-belnr <> ls_bkpf-belnr OR
           ls_vbsegk-gjahr <> ls_bkpf-gjahr.
          EXIT.
        ENDIF.
        gs_output-lifnr = ls_vbsegk-lifnr.
*        gs_output-mwskz = ls_vbsegk-mwskz.
        gs_output-shkzg = ls_vbsegk-shkzg.
        gs_output-bschl = ls_vbsegk-bschl.
        IF gs_output-shkzg = 'S'. "DR
          gs_output-dmbtr = gs_output-dmbtr + ls_vbsegk-dmbtr.
          gs_output-wrbtr = gs_output-wrbtr + ls_vbsegk-wrbtr.
        ELSE.
          gs_output-dmbtr = gs_output-dmbtr - ls_vbsegk-dmbtr.
          gs_output-wrbtr = gs_output-wrbtr - ls_vbsegk-wrbtr.
        ENDIF.
        lv_exist = 'X'.
      ENDLOOP.
      IF lv_exist = 'X'.
        CLEAR lv_tabix1.
        READ TABLE lt_vbsegs WITH KEY bukrs = ls_bkpf-bukrs
                                      belnr = ls_bkpf-belnr
                                      gjahr = ls_bkpf-gjahr
                                      TRANSPORTING NO FIELDS.
        lv_tabix1 = sy-tabix.
        LOOP AT lt_vbsegs INTO ls_vbsegs FROM lv_tabix1.
          IF ls_vbsegs-bukrs <> ls_bkpf-bukrs OR
             ls_vbsegs-belnr <> ls_bkpf-belnr OR
             ls_vbsegs-gjahr <> ls_bkpf-gjahr.
            EXIT.
          ENDIF.
          IF gs_output-hkont IS INITIAL.
            gs_output-hkont = ls_vbsegs-saknr.
          ELSE.
            IF gs_output-hkont <> ls_vbsegs-saknr.
              gs_output-hkont = '**'.
            ENDIF.
          ENDIF.
          IF gs_output-mwskz IS INITIAL.
            gs_output-mwskz = ls_vbsegs-mwskz.
          ELSE.
            IF gs_output-mwskz <> ls_vbsegs-mwskz.
              gs_output-mwskz = 'M*'.
            ENDIF.
          ENDIF.
          IF ls_vbsegs-shkzg = 'S'. "DR
            gs_output-taxdoc = gs_output-taxdoc + ls_vbsegs-wrbtr.
            gs_output-taxlcl = gs_output-taxlcl + ls_vbsegs-dmbtr.
          ELSE.
            gs_output-taxdoc = gs_output-taxdoc - ls_vbsegs-wrbtr.
            gs_output-taxlcl = gs_output-taxlcl - ls_vbsegs-dmbtr.
          ENDIF.
        ENDLOOP.
        READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY
                                lifnr = gs_output-lifnr.
        gs_output-name1 = ls_lfa1-name1.
        APPEND gs_output TO gt_output.
      ENDIF.
    ELSE.         "Posted Documents
      READ TABLE lt_bsik WITH KEY bukrs = ls_bkpf-bukrs
                                  belnr = ls_bkpf-belnr
                                  gjahr = ls_bkpf-gjahr
                                  TRANSPORTING NO FIELDS.
      lv_tabix1 = sy-tabix.
      LOOP AT lt_bsik INTO ls_bsik FROM lv_tabix1.
        IF ls_bsik-bukrs <> ls_bkpf-bukrs OR
           ls_bsik-belnr <> ls_bkpf-belnr OR
           ls_bsik-gjahr <> ls_bkpf-gjahr.
          EXIT.
        ENDIF.
        gs_output-lifnr = ls_bsik-lifnr.
        gs_output-bschl = ls_bsik-bschl.
        gs_output-shkzg = ls_bsik-shkzg.
*        gs_output-mwskz = ls_bsik-mwskz.
        IF gs_output-shkzg = 'S'. "DR
          gs_output-dmbtr = gs_output-dmbtr + ls_bsik-dmbtr.
          gs_output-wrbtr = gs_output-wrbtr + ls_bsik-wrbtr.
        ELSE.
          gs_output-dmbtr = gs_output-dmbtr - ls_bsik-dmbtr.
          gs_output-wrbtr = gs_output-wrbtr - ls_bsik-wrbtr.
        ENDIF.
        lv_exist = 'X'.
      ENDLOOP.
      IF lv_exist = 'X'.
        CLEAR lv_tabix1.
        READ TABLE lt_bsis WITH KEY bukrs = ls_bkpf-bukrs
                                    belnr = ls_bkpf-belnr
                                    gjahr = ls_bkpf-gjahr
                                    TRANSPORTING NO FIELDS.
        lv_tabix1 = sy-tabix.
        LOOP AT lt_bsis INTO ls_bsis FROM lv_tabix1.
          IF ls_bsis-bukrs <> ls_bkpf-bukrs OR
             ls_bsis-belnr <> ls_bkpf-belnr OR
             ls_bsis-gjahr <> ls_bkpf-gjahr.
            EXIT.
          ENDIF.
          IF gs_output-hkont IS INITIAL.
            gs_output-hkont = ls_bsis-hkont.
          ELSE.
            IF gs_output-hkont <> ls_bsis-hkont.
              gs_output-hkont = '**'.
            ENDIF.
          ENDIF.
          IF gs_output-mwskz IS INITIAL.
            gs_output-mwskz = ls_bsis-mwskz.
          ELSE.
            IF gs_output-mwskz <> ls_bsis-mwskz.
              gs_output-mwskz = 'M*'.
            ENDIF.
          ENDIF.
          IF ls_bsis-shkzg = 'S'. "DR
            gs_output-taxdoc = gs_output-taxdoc + ls_bsis-wrbtr.
            gs_output-taxlcl = gs_output-taxlcl + ls_bsis-dmbtr.
          ELSE.
            gs_output-taxdoc = gs_output-taxdoc - ls_bsis-wrbtr.
            gs_output-taxlcl = gs_output-taxlcl - ls_bsis-dmbtr.
          ENDIF.
        ENDLOOP.
      ELSE.  "If not in Open document then check in clear document
        CLEAR lv_tabix1.
        READ TABLE lt_bsak WITH KEY bukrs = ls_bkpf-bukrs
                                   belnr = ls_bkpf-belnr
                                   gjahr = ls_bkpf-gjahr
                                   TRANSPORTING NO FIELDS.
        lv_tabix1 = sy-tabix.
        LOOP AT lt_bsak INTO ls_bsak FROM lv_tabix1.
          IF ls_bsak-bukrs <> ls_bkpf-bukrs OR
             ls_bsak-belnr <> ls_bkpf-belnr OR
             ls_bsak-gjahr <> ls_bkpf-gjahr.
            EXIT.
          ENDIF.
          gs_output-lifnr = ls_bsak-lifnr.
          gs_output-bschl = ls_bsak-bschl.
          gs_output-shkzg = ls_bsak-shkzg.
*          gs_output-mwskz = ls_bsak-mwskz.
          IF gs_output-shkzg = 'S'. "DR
            gs_output-dmbtr = gs_output-dmbtr + ls_bsak-dmbtr.
            gs_output-wrbtr = gs_output-wrbtr + ls_bsak-wrbtr.
          ELSE.
            gs_output-dmbtr = gs_output-dmbtr - ls_bsak-dmbtr.
            gs_output-wrbtr = gs_output-wrbtr - ls_bsak-wrbtr.
          ENDIF.
        ENDLOOP.
        CLEAR lv_tabix1.
        READ TABLE lt_bsas WITH KEY bukrs = ls_bkpf-bukrs
                                    belnr = ls_bkpf-belnr
                                    gjahr = ls_bkpf-gjahr
                                    TRANSPORTING NO FIELDS.
        lv_tabix1 = sy-tabix.
        LOOP AT lt_bsas INTO ls_bsas FROM lv_tabix1.
          IF ls_bsas-bukrs <> ls_bkpf-bukrs OR
             ls_bsas-belnr <> ls_bkpf-belnr OR
             ls_bsas-gjahr <> ls_bkpf-gjahr.
            EXIT.
          ENDIF.
          IF gs_output-hkont IS INITIAL.
            gs_output-hkont = ls_bsas-hkont.
          ELSE.
            IF gs_output-hkont <> ls_bsas-hkont.
              gs_output-hkont = '**'.
            ENDIF.
          ENDIF.
          IF gs_output-mwskz IS INITIAL.
            gs_output-mwskz = ls_bsas-mwskz.
          ELSE.
            IF gs_output-mwskz <> ls_bsas-mwskz.
              gs_output-mwskz = 'M*'.
            ENDIF.
          ENDIF.
          IF ls_bsas-shkzg = 'S'. "DR
            gs_output-taxdoc = gs_output-taxdoc + ls_bsas-wrbtr.
            gs_output-taxlcl = gs_output-taxlcl + ls_bsas-dmbtr.
          ELSE.
            gs_output-taxdoc = gs_output-taxdoc - ls_bsas-wrbtr.
            gs_output-taxlcl = gs_output-taxlcl - ls_bsas-dmbtr.
          ENDIF.
        ENDLOOP.
      ENDIF.
      READ TABLE lt_lfa1 INTO ls_lfa1 WITH KEY
                             lifnr = gs_output-lifnr.
      gs_output-name1 = ls_lfa1-name1.
      APPEND gs_output TO gt_output.
*      ENDIF.
    ENDIF.
  ENDLOOP.
  IF s_mwskz[] IS NOT INITIAL.
    DELETE gt_output WHERE mwskz NOT IN s_mwskz.
  ENDIF.
  IF gt_output[] IS INITIAL.
    WRITE : / 'No data to output..'.
    STOP.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 90
      text       = 'Preparing to display ALV..'.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       Output ALV
*----------------------------------------------------------------------*
FORM display_alv .

  DATA:  ls_key         TYPE salv_s_layout_key,
         lr_table       TYPE REF TO cl_salv_table,
         lr_layout      TYPE REF TO cl_salv_layout,
         lr_functions   TYPE REF TO cl_salv_functions,
         lr_display     TYPE REF TO cl_salv_display_settings,
         lr_columns     TYPE REF TO cl_salv_columns_table,
         lr_column      TYPE REF TO cl_salv_column_table,
         lr_events_salv TYPE REF TO cl_salv_events_table,
         lr_event       TYPE REF TO lcl_event_handler.

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
*Event
  lr_events_salv = lr_table->get_event( ).
  CREATE OBJECT lr_event.
  SET HANDLER: lr_event->hotspot_click
               FOR lr_events_salv.
*Set layout
  lr_layout = lr_table->get_layout( ).
  ls_key-report = sy-repid.
  lr_layout->set_key( ls_key ).
  lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*  CALL METHOD lr_layout->set_initial_layout
*    EXPORTING
*      value = p_vari.
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
*&      Form  HANDLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_click  USING  iv_row TYPE salv_de_row
                          iv_column TYPE salv_de_column.
  CLEAR gs_output.

  READ TABLE gt_output INTO gs_output INDEX iv_row.
  CHECK sy-subrc = 0.

  CASE iv_column.
    WHEN 'BELNR'.
      SET PARAMETER ID 'BLN' FIELD gs_output-belnr.
      SET PARAMETER ID 'BUK' FIELD gs_output-bukrs.
      SET PARAMETER ID 'GJR' FIELD gs_output-gjahr.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.                    " HANDLE_CLICK
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
      io_column ?= io_columns->get_column( 'BELNR' ).
      CALL METHOD io_column->set_cell_type
        EXPORTING
          value = if_salv_c_cell_type=>hotspot.
      io_column ?= io_columns->get_column( 'TAXDOC' ).
      lv_long_text  =  'Tax in Doc. Currency'.
      lv_short_text = 'Tax DCurr'.
      lv_med_text   = 'Tax Doc Curr.'.
      CALL METHOD io_column->set_long_text
        EXPORTING
          value = lv_long_text.
      CALL METHOD io_column->set_short_text
        EXPORTING
          value = lv_short_text.
      CALL METHOD io_column->set_medium_text
        EXPORTING
          value = lv_med_text.
      io_column ?= io_columns->get_column( 'TAXLCL' ).
      lv_long_text  =  'Tax in Loc. Currency'.
      lv_short_text = 'Tax LCurr'.
      lv_med_text   = 'Tax Loc Curr.'.
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
