*&---------------------------------------------------------------------*
*& Report  ZFAPI113_PAYMENT_HISTORY
*&---------------------------------------------------------------------*
REPORT  zfapi113_payment_history.

*----------------------------------------------------------------------*
* Program Name       :  ZFAPI113_PAYMENT_HISTORY                       *
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

INCLUDE zfapi113_payment_history_top.

INCLUDE zfapi113_payment_history_scr.

INCLUDE zfapi113_payment_history_lgc.

INCLUDE zfapi113_payment_history_f01.
