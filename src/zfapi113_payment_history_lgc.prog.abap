*&---------------------------------------------------------------------*
*&  Include           ZFAPI113_PAYMENT_HISTORY_LGC
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :  ZFAPI113_PAYMENT_HISTORY                       *
* Include            :  ZFAPI113_PAYMENT_HISTORY_LGC                   *
* Author             :  Kalinga Keshari Rout                           *
* Creation Date      :  February 01, 2018                              *
* Object ID          :                                                 *
* Application Area   :  FICO                                           *
* Description        :  The Payment History interface lists all paid   *
*                       invoices with their corresponding payment      *
*                       information.  It includes all payments that    *
*                       have been processed from Checks, EFT Payments, *
*                       Wire Transfers or any other form of payment.   *
*                       The Payment History file should include one    *
*                       record per invoice AND payment.  Multiple      *
*                       invoices paid on the same payment should have  *
*                       one record for each invoice.  Multiple         *
*                       payments for the same invoice should have one  *
*                       record for each payment.                       *
*----------------------------------------------------------------------*
*                       Modification Log                               *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 09-Apr-2018  KROUT       D30K928652  CHG0100818#Initial development  *
*                          D30K928716, D30K928741, D30K928766,         *
*                          D30K928774, D30K928886                      *
*----------------------------------------------------------------------*

INITIALIZATION.

  PERFORM  f_get_file_path         CHANGING p_path2.

AT SELECTION-SCREEN    ON VALUE-REQUEST FOR p_path2.

  PERFORM  f_get_f4_help_file_path.

AT SELECTION-SCREEN    ON VALUE-REQUEST FOR p_path1.

  PERFORM  f_get_f4_help_file_path1.

START-OF-SELECTION.

  PERFORM  f_get_data.

END-OF-SELECTION.

  IF   ( p_dis IS NOT INITIAL ).

    PERFORM  f_display_data.

  ENDIF .
