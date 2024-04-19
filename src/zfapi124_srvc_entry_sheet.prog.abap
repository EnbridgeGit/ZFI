*&---------------------------------------------------------------------*
*& Report  ZFAPI124_SRVC_ENTRY_SHEET
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       : ZFAPI124_SRVC_ENTRY_SHEET                       *
* Author             :                                                 *
* Date               : Mar 26, 2018                                    *
* Technical Contact  : Paul Karunakar                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            : The Service Entry Sheet (SES) master data       *
*                      from each of the three SAP instances will be    *
*                      extracted in a delimited file and sent to IAP.  *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 26-Mar-2018  KPAL        D30K928726  CHG0106361  Initial Development *
*                          D30K928730, D30K928878, D30K928919          *
*                          D30K928927                                  *
*----------------------------------------------------------------------*
REPORT  zfapi124_srvc_entry_sheet MESSAGE-ID zfi01
                                  NO STANDARD PAGE HEADING.

INCLUDE zfapi124_srvc_entry_sheet_top.

INCLUDE zfapi124_srvc_entry_sheet_scr.

INCLUDE zfapi124_srvc_entry_sheet_f01.

INCLUDE zfapi124_srvc_entry_sheet_lgc.
