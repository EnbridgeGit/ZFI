*&---------------------------------------------------------------------*
*& Report  ZFAPI112_OPEN_INVOICES
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI112_OPEN_INVOICES                        *
* Author             :   Paul Karunakar                                *
* Date               :   Feb 1, 2018                                   *
* Technical Contact  :   John Hartung                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Extract interface of Open and Parked          *
*                        Invoices to IAP                               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  KPAL        D30K928583  CHG0100815  Initial Development *
*                          D30K928768, D30K928884                      *
*----------------------------------------------------------------------*
REPORT  zfapi112_open_invoices  MESSAGE-ID ZFI01
                                NO STANDARD PAGE HEADING.

INCLUDE zfapi112_open_invoices_top.

INCLUDE zfapi112_open_invoices_scr.

INCLUDE zfapi112_open_invoices_f01.

INCLUDE zfapi112_open_invoices_lgc.
