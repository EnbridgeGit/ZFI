*&---------------------------------------------------------------------*
*& Report  ZFAPI111_VENDOR_EXTRACT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFAPI111_VENDOR_EXTRACT                       *
* Author             :                                                 *
* Date               :   Feb 01, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   ATCAT Vendor Master Interface                 *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  CPALYAM     D30K928649  CHG0100816-Initial development  *
*                          D30K928653, D30K928683, D30K928691          *
*                          D30K928693, D30K928882, D30K928892          *
*----------------------------------------------------------------------*

REPORT  zfapi111_vendor_extract  LINE-SIZE 300 LINE-COUNT 65
                                 NO STANDARD PAGE HEADING.

INCLUDE zfapi111_vendor_extract_top.
INCLUDE zfapi111_vendor_extract_scr.
INCLUDE zfapi111_vendor_extract_lgc.
INCLUDE zfapi111_vendor_extract_f01.
