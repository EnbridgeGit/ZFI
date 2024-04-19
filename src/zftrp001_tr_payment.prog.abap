*&---------------------------------------------------------------------*
*& Report  ZFTRP001_TR_PAYMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zftrp001_tr_payment.

TABLES: regut.

DATA: gt_regut TYPE TABLE OF regut.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_zbukr FOR regut-zbukr OBLIGATORY,
                s_laufd FOR regut-laufd OBLIGATORY,
                s_laufi FOR regut-laufi. " OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  PERFORM get_data.
  PERFORM alv_display.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Extract Regut Data
*----------------------------------------------------------------------*

FORM get_data .

  CLEAR: gt_regut.

  SELECT * FROM regut INTO TABLE gt_regut
    WHERE zbukr IN s_zbukr
      AND laufd IN s_laufd
      AND laufi IN s_laufi.
  IF gt_regut IS INITIAL.
    WRITE : / 'No data to display....'.
    STOP.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
FORM alv_display .

  DATA: lr_table TYPE REF TO cl_salv_table,
        lr_layout TYPE REF TO cl_salv_layout,
        ls_key TYPE salv_s_layout_key,
        lr_functions TYPE REF TO cl_salv_functions,
        lr_display TYPE REF TO cl_salv_display_settings,
        lv_function TYPE salv_de_function,
        lv_pos TYPE salv_de_function_pos.

  DATA: lr_content TYPE REF TO cl_salv_form_element.
  DATA: lr_grid     TYPE REF TO cl_salv_form_layout_grid.

  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
        IMPORTING
          r_salv_table   = lr_table
        CHANGING
          t_table        = gt_regut.
    CATCH cx_salv_msg .
  ENDTRY.
*Function settings
  lr_functions = lr_table->get_functions( ).
  lr_functions->set_all( abap_true ).
*Display Setting
  lr_display = lr_table->get_display_settings( ).
  lr_display->set_striped_pattern( cl_salv_display_settings=>true ).
*  CONCATENATE 'Client ' sy-sysid sy-MANDT sy-datum sy-UZEIT
*                            INTO lv_msg separated by space.
*  lr_display->set_list_header( lv_msg ).

*Set layout
  lr_layout = lr_table->get_layout( ).
  ls_key-report = sy-repid.
  lr_layout->set_key( ls_key ).
  lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*  CALL METHOD lr_layout->set_initial_layout
*    EXPORTING
*      value = p_vari.
******Set ALV Header
*... Create top_of_list contents.
*  CREATE OBJECT lr_grid.
*  lr_grid->create_label(
*          row    = 1
*          column = 1
*          text   = p_dsc1 ).
*    lr_grid->create_label(
*      row    = 2
*      column = 1
*      text   = p_dsc2 ).
*
*  lr_table->set_top_of_list( lr_content ).
******Display ALV
CALL METHOD lr_table->display.

ENDFORM.                    " ALV_DISPLAY
