*&---------------------------------------------------------------------*
*& Report  ZFAPI120_BANK_KEY
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFAPI120_BANK_KEY                             *
* Author             :                                                 *
* Date               :   Mar 22, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Bank Key Extract Interface                    *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 01-Feb-2018  CPALYAM     D30K928630  CHG0106152-Initial development  *
*                          D30K928713, D30K928842, D30K928902          *
*                          D30K929027                                  *
*----------------------------------------------------------------------*
REPORT  zfapi120_bank_key  LINE-SIZE 300 LINE-COUNT 65
                           NO STANDARD PAGE HEADING.

INCLUDE zfapi120_bank_key_top.
INCLUDE zfapi120_bank_key_scr.
INCLUDE zfapi120_bank_key_lgc.
INCLUDE zfapi120_bank_key_f01.
