*&---------------------------------------------------------------------*
*& Report  ZFAPI105_WBS_ELEMENT
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPI105_WBS_ELEMENT                          *
* Author             :                                                 *
* Date               :   Jan 10, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   For WBS data through interface for IAP        *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 10-Jan-2018  CPALYAM     D30K928578  CHG0100809-Initial Development  *                                                                     *
*                          D30K928836, D30K928894, D30K929025          *
*                          D30K929043                                  *
*----------------------------------------------------------------------*

REPORT  zfapi105_wbs_element  LINE-SIZE 300 LINE-COUNT 65
                              NO STANDARD PAGE HEADING.

INCLUDE zfapi105_wbs_element_top.
INCLUDE zfapi105_wbs_element_scr.
INCLUDE zfapi105_wbs_element_f01.
INCLUDE zfapi105_wbs_element_lgc.
