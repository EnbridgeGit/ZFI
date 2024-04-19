*&---------------------------------------------------------------------*
*& Report  ZFAPI126_PURCHASE_ORDER
*&---------------------------------------------------------------------*
************************************************************************
*                               Enbridge                               *
************************************************************************
*& Program Name       :  ZFAPI126_PURCHASE_ORDER                       *
*& Author             :  Paul Karunakar                                *
*& Creation Date      :  02-Apr-2018                                   *
*& Object ID          :                                                *
*& Application Area   :  FICO                                          *
*& Description        :  Purchase Order data from each of the three    *
*&                       SAP instances will be extracted in a          *
*&                       delimited file and sent to IAP.               *
*&                                                                     *
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 02-Apr-2018  KPAL        D30K928646  CHG0107206 Initial Development  *
*                          D30K928846, D30K928994, D30K929262          *
************************************************************************
REPORT  zfapi126_purchase_order  MESSAGE-ID zfi01
                                 NO STANDARD PAGE HEADING.

INCLUDE zfapi126_purchase_order_top.

INCLUDE zfapi126_purchase_order_scr.

INCLUDE zfapi126_purchase_order_lgc.

INCLUDE zfapi126_purchase_order_f01.
