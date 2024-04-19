************************************************************************
*                               Enbridge                               *
************************************************************************
*& Program Name       :  ZFAPI116_USER_EXTRACT                         *
*& Author             :  Kalinga Keshari Rout / Paul Karunakar         *
*& Creation Date      :  08-Mar-2018                                   *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  User data from each of the three SAP          *
*&                       instances will be extracted in a delimited    *
*&                       file and sent to IAP.                         *
*&                                                                     *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 08-Mar-2018  KROUT       D30K928695, CHG0105965  Initial development *
*                          D30K928697, D30K928840, D30K929003,         *
*                          D30K929075, D30K929081, D30K929083          *
************************************************************************
REPORT  zfapi116_user_extract  MESSAGE-ID zfi01
                               NO STANDARD PAGE HEADING.

INCLUDE zfapi116_user_extract_top.

INCLUDE zfapi116_user_extract_scr.

INCLUDE zfapi116_user_extract_lgc.

INCLUDE zfapi116_user_extract_f01.
