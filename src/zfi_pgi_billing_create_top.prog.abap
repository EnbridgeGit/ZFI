*&---------------------------------------------------------------------*
*&  Include           ZFI_PGI_BILLING_CREATE_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFI_PGI_BILLING_CREATE                        *
* Program Include    :   ZFI_PGI_BILLING_CREATE_TOP                    *
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

TABLES:LIPS.

************************************************************************
*                           Types                                      *
************************************************************************
TYPES : BEGIN OF GTY_OUTPUT,
        VBELN TYPE VBELN_VL,
        MSG TYPE STRING,
        END OF GTY_OUTPUT.


************************************************************************
*                              Structures                              *
************************************************************************
DATA : GS_VBKOK TYPE VBKOK,
       GS_LIPS  TYPE LIPS,
       GS_VERKO TYPE VERKO,
       GS_OUTPUT TYPE GTY_OUTPUT,
       GS_VBPOK TYPE VBPOK.


************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA : GT_VBKOK TYPE TABLE OF VBKOK,
       GT_LIPS  TYPE TABLE OF LIPS,
       GT_VERKO TYPE TABLE OF VERKO,
       GT_OUTPUT TYPE TABLE OF GTY_OUTPUT,
       GT_VBPOK TYPE TABLE OF VBPOK.
