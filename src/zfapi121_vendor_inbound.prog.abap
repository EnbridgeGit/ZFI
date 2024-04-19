*&---------------------------------------------------------------------*
*& Report  ZFAPI121_VENDOR_INBOUND
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFAPI121_VENDOR_INBOUND                       *
* Author             :                                                 *
* Date               :   Apr 15, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Vendor Inbound Interface                      *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 15-Apr-2018  CPALYAM     D30K929071-Initial development              *
*----------------------------------------------------------------------*

REPORT  zfapi121_vendor_inbound LINE-SIZE 300 LINE-COUNT 65
                                 NO STANDARD PAGE HEADING.

INCLUDE zfapi121_vendor_inbound_top.
INCLUDE zfapi121_vendor_inbound_scr.
INCLUDE zfapi121_vendor_inbound_lgc.
INCLUDE zfapi121_vendor_inbound_f01.
