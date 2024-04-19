*&---------------------------------------------------------------------*
*& Report  ZFAPI102_GL_ACCOUNT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :   ZFAPI102_GL_ACCOUNT                           *
* Author             :                                                 *
* Date               :   16-Feb-2018                                   *
* Technical Contact  :   Vijay Rajaputra                               *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  G/L Accounts Extraction  for IAP               *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS        Description                      *
* ---------------------------------------------------------------------*
* 16-Feb-2018  VRAJAPUTRA  D30K928566 CHG0100806 # Initial development *
*                          D30K928825, D30K928874, D30K928888          *
*&---------------------------------------------------------------------*
REPORT  ZFAPI102_GL_ACCOUNT MESSAGE-ID ZFI01 NO STANDARD PAGE HEADING.

* Include for data declarations
INCLUDE ZFAPI102_GL_ACCOUNT_TOP.

* Include for selection screen
INCLUDE ZFAPI102_GL_ACCOUNT_SCR.

* Include for events
INCLUDE ZFAPI102_GL_ACCOUNT_LGC.

* Include for the subroutines
INCLUDE ZFAPI102_GL_ACCOUNT_F01.
