*&---------------------------------------------------------------------*
*& Report  ZFI_REPORT_WRAPPER                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_REPORT_WRAPPER                            *
*& Include Name       :  ZFI_REPORT_WRAPPER                            *
*& Author             :  Tawfeeq Ahamd                                 *
*& Date               :  19-Oct-2020                                   *
*& Change Request     :  CHG0191644                                    *
*& Purpose            :  Wrapper program to send FBL1N output in CSV   *
*&                       format as an email attachment                 *
*&---------------------------------------------------------------------*
*&                      Modification Log                               *
*&                                                                     *
*& Changed On   Changed By    CTS        Description                   *
*& --------------------------------------------------------------------*
*& 19-Oct 2020  AHMADT        CHG0191644 D30K930705 Initial Development*
*&                                       D30K930727                    *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

REPORT  zfi_report_wrapper.

INCLUDE zfi_report_wrapper_top.
INCLUDE zfi_report_wrapper_ss.
INCLUDE zfi_report_wrapper_form.

START-OF-SELECTION.

  PERFORM get_data.
  PERFORM fill_attachment.
  PERFORM send_mail.
