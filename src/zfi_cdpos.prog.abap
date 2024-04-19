*&---------------------------------------------------------------------*
*& Report  ZFI_CDPOS                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_CDPOS                                     *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  17-Dec-2020                                   *
*& Change Request     :  CHG0199398                                    *
*& Purpose            :  Process Mining Report                         *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 17-Dec-2020  AHMADT        D30K930775 CHG0199398 Initial Development*
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zfi_cdpos.

INCLUDE zfi_cdpos_top.
INCLUDE zfi_cdpos_ss.
INCLUDE zfi_cdpos_form.

INITIALIZATION.
  PERFORM f_initialize_ss.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_ss_output.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pres.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_pres.

START-OF-SELECTION.

  PERFORM f_get_data.

  IF p_app_r = 'X'.
    IF p_rec IS INITIAL.
      MESSAGE text-013 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      PERFORM f_upload_data_app.
    ENDIF.
  ELSEIF p_pre_r = 'X'.
    PERFORM f_upload_data_pre.
  ENDIF.
