*&---------------------------------------------------------------------*
*&  Include           ZFI_PGI_BILLING_CREATE_LGC
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_PGI_BILLING_CREATE                        *
* Program Include    :   ZFI_PGI_BILLING_CREATE_LGC                    *
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

START-OF-SELECTION.

PERFORM GET_DATA.
PERFORM CRETAE_PGI.
PERFORM ALV_DISPLAY.
