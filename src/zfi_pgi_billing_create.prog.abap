*&---------------------------------------------------------------------*
*& Report  ZFI_PGI_BILLING_CREATE
*&
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_PGI_BILLING_CREATE                        *
* Author             :   AKMADASU                                      *
* Date               :   Jan 23, 2022                                  *
* Technical Contact  :   Ashok Madasu                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   To cretae PGI and Billing                     *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 23-Jan-2022  AKMADASU    D30K931986-Initial development              *
*----------------------------------------------------------------------*

REPORT  ZFI_PGI_BILLING_CREATE LINE-SIZE 300 LINE-COUNT 65
                               NO STANDARD PAGE HEADING.

INCLUDE ZFI_PGI_BILLING_CREATE_TOP.
INCLUDE ZFI_PGI_BILLING_CREATE_SCR.
INCLUDE ZFI_PGI_BILLING_CREATE_LGC.
INCLUDE ZFI_PGI_BILLING_CREATE_F01.
