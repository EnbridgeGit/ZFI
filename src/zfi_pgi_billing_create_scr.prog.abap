*&---------------------------------------------------------------------*
*&  Include           ZFI_PGI_BILLING_CREATE_SCR
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_PGI_BILLING_CREATE                        *
* Program Include    :   ZFI_PGI_BILLING_CREATE_SCR                    *
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

SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECTION-SCREEN: SKIP 1.

SELECT-OPTIONS S_VBELN FOR LIPS-VBELN.

SELECTION-SCREEN: END   OF BLOCK ssb1.
