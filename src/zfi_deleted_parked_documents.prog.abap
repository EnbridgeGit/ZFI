*&---------------------------------------------------------------------*
*& Report  ZFI_DELETED_PARKED_DOCUMENTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report Name          : ZFI_DELETED_PARKED_DOCUMENTS                 *
*& Author               : AHMADT                                       *
*& Creation Date        : 18-September-2019                            *
*& Transport no.        : D30K930149                                   *
*& Object ID            : CHG0147640                                   *
*& Application Area     : FI                                           *
*& Description          : This program sends an email with excel       *
*&                        attachment of all documents which have been  *
*&                        parked by the user                           *
*&---------------------------------------------------------------------*
*&Change Log:
*& D30K930149    AHMADT
*&               Subroutine F_create_content was modified to rectify
*&               performance error and SUBMIT statement was removed.
*&               FM CHANGEDOCUMENT_READ_HEADERS is used to get details
*&               of user who deleted documnet it's date and time.
*&----------------------------------------------------------------------

REPORT  zfi_deleted_parked_documents.


INCLUDE: zfi_deleted_parked_top,
         zfi_deleted_parked_scr,
         zfi_deleted_parked_validations,
         zfi_deleted_parked_forms.

INITIALIZATION.
  PERFORM f_refresh.

START-OF-SELECTION.

  PERFORM: f_create_content,
           f_date_convert,
           f_system_check,
           f_attachment_name,
           f_send_mail.
