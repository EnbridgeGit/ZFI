*&---------------------------------------------------------------------*
*& Report  ZFAPI115_VENDOR_MASTER
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI115_VENDOR_MASTER                        *
* Author             :   Vijay Rajaputra                               *
* Date               :   03-Mar-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Vendor Master Extraction for IAP               *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 03-Mar-2018  VRAJAPUTRA  D30K928789 CHG0105961 - Initial development *
*                          D30K928848, D30K929258                      *
*&---------------------------------------------------------------------*
REPORT  zfapi115_vendor_master  MESSAGE-ID ZFI01
                                NO STANDARD PAGE HEADING.

INCLUDE zfapi115_vendor_master_top.

INCLUDE zfapi115_vendor_master_scr.

INCLUDE zfapi115_vendor_master_lgc.

INCLUDE zfapi115_vendor_master_f01.

INCLUDE zfapi115_vendor_master_f02.
