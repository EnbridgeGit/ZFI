*&---------------------------------------------------------------------*
*& Report  ZFAPE101_VENDOR_CONTACT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPE101_VENDOR_CONTACT                       *
* Author             :                                                 *
* Date               :   Jan 29, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   For Vendor Contact Data through interface     *
*                        for IAP                                       *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By       CTS          Description               *
* ---------------------------------------------------------------------*
* 29-JAN-2018  Chaitanya Palyam D30K928552   CHG0100815-Initial Dev    *
*                               D30K928771                             *
*                                                                      *
*----------------------------------------------------------------------*
REPORT  zfape101_vendor_contact LINE-SIZE 300 LINE-COUNT 65
NO STANDARD PAGE HEADING.

INCLUDE zfape101_vendor_contact_top.
INCLUDE zfape101_vendor_contact_scr.
INCLUDE zfape101_vendor_contact_f01.
INCLUDE zfape101_vendor_contact_lgc.
