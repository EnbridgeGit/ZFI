*&---------------------------------------------------------------------*
*& Report  ZFAPI114_PAYMENT_VOIDS                                      *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZFAPI114_PAYMENT_VOIDS                         *
* Author             :  Vijay Rajaputra                                *
* Date               :  05-Mar-2018                                    *
* Technical Contact  :  Vijay Rajaputra                                *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  ATCAT Payment Voids Extract Interface          *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 05-Mar-2018  VRAJAPUTRA    D30K928605 CHG0100819 Initial Development *
*                            D30K928780                                *
*&---------------------------------------------------------------------*
REPORT  ZFAPI114_PAYMENT_VOIDS  MESSAGE-ID ZFI01
                                NO STANDARD PAGE HEADING.

INCLUDE ZFAPI114_PAYMENT_VOIDS_TOP.

INCLUDE ZFAPI114_PAYMENT_VOIDS_SCR.

INCLUDE ZFAPI114_PAYMENT_VOIDS_LGC.

INCLUDE ZFAPI114_PAYMENT_VOIDS_F01.
