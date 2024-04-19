************************************************************************
*                               Enbridge                               *
************************************************************************
* Program Name       :  ZFAPI123_PAYMENT_HISTORY                       *
* Author             :  Paul Karunakar                                 *
* Creation Date      :  02-May-2018                                    *
* Application Area   :  FICO                                           *
* Description        :  Payment History data from each of the three    *
*                       SAP instances will be extracted in a delimited *
*                       file and sent to IAP.                          *
*                                                                      *
*&---------------------------------------------------------------------*
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  --------------------------------*
* 02-May-2018  KPAL        D30K928709  CHG0108944  Initial Development *
*----------------------------------------------------------------------*
REPORT  zfapi123_payment_history  MESSAGE-ID zfi01
                                  LINE-COUNT 65
                                  LINE-SIZE 121
                                  NO STANDARD PAGE HEADING.

INCLUDE zfapi123_payment_history_top.

INCLUDE zfapi123_payment_history_scr.

INCLUDE zfapi123_payment_history_f01.

INCLUDE zfapi123_payment_history_lgc.
