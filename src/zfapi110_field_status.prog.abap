*&---------------------------------------------------------------------*
*& Report  ZFAPI110_FIELD_STATUS
*&---------------------------------------------------------------------*
* Program Name       : ZFAPI110_FIELD_STATUS                           *
* Author             :                                                 *
* Date               : Apr 30, 2018                                    *
* Technical Contact  : Paul Karunakar                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            : IAP requires Field Status data to enable correct*
*                      entry of data on the G/L Lines in the IAP system*
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 30-Apr-2018  KPAL        D30K928711  CHG0109112 # Initial Development*
*                          D30K928838, D30K928943                      *
*----------------------------------------------------------------------*

REPORT  ZFAPI110_FIELD_STATUS.

INCLUDE ZFAPI110_FIELD_STATUS_TOP.

INCLUDE ZFAPI110_FIELD_STATUS_SCR.

INCLUDE ZFAPI110_FIELD_STATUS_F01.

INCLUDE ZFAPI110_FIELD_STATUS_LGC.
