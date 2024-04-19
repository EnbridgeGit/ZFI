*&---------------------------------------------------------------------*
*& Report  ZFI_TUESDAY_PROCESS
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_TUESDAY_PROCESS                           *
* Author             :                                                 *
* Date               :   Oct 17, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   PRRW Tuesday Process Automation               *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 17-Oct-2018  CPALYAM     D30K929184-Initial development              *
*----------------------------------------------------------------------*

REPORT  zfi_tuesday_process LINE-SIZE 300 LINE-COUNT 65
                                 NO STANDARD PAGE HEADING.

INCLUDE zfi_tuesday_process_top.
INCLUDE zfi_tuesday_process_scr.
INCLUDE zfi_tuesday_process_lgc.
INCLUDE zfi_tuesday_process_f01.
