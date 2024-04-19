************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI122_OPEN_INVC_ITEMS                       *
* Author             :  Paul Karunakar                                 *
* Creation Date      :  16-Apr-2018                                    *
* Application Area   :  FICO                                           *
* Description        :  Open Invoice data from each of the three SAP   *
*                       instances will be extracted in a delimited     *
*                       file and sent to IAP.                          *
*                                                                      *
*&---------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 16-Apr-2018  KPAL        D30K928646  CHG0108943  Initial Development *
*                          D30K928913, D30K929061, D30K929260          *
*----------------------------------------------------------------------*
REPORT  zfapi122_open_invc_items  MESSAGE-ID zfi01
                                  LINE-COUNT 65
                                  LINE-SIZE 121
                                  NO STANDARD PAGE HEADING.

INCLUDE zfapi122_open_invc_items_top.

INCLUDE zfapi122_open_invc_items_scr.

INCLUDE zfapi122_open_invc_items_f01.

INCLUDE zfapi122_open_invc_items_lgc.
