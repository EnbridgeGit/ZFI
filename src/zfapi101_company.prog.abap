*&---------------------------------------------------------------------*
*& Report  ZFAPI101_COMPANY
*&
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI101_COMPANY                              *
* Author             :                                                 *
* Date               :   11-Jan-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Company Codes Extraction  for IAP              *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 15-FEB-2018  VRAJAPUTRA  D30K928562 CHG0100805 - Initial development *
*                          D30K928598, D30K928823                      *
*&---------------------------------------------------------------------*

REPORT  ZFAPI101_COMPANY MESSAGE-ID ZFI01 NO STANDARD PAGE HEADING.

* Include for data declarations
INCLUDE ZFAPI101_COMPANY_TOP.

* Include for selection screen
INCLUDE ZFAPI101_COMPANY_SCR.

* Include for events
INCLUDE ZFAPI101_COMPANY_LGC.

* Include for the subroutines
INCLUDE ZFAPI101_COMPANY_F01.
