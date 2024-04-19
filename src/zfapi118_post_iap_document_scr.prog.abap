*&---------------------------------------------------------------------*
*&  Include           ZFAPI118_POST_IAP_DOCUMENT_SCR
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFAPI118_POST_IAP_DOCUMENT                        *
*  Include:          ZFAPI118_POST_IAP_DOCUMENT_SCR                    *
*  Author:           Vijay Rajaputra ( Copy of US System )             *
*  Date:             Aug, 01 2018                                      *
*  Application Area: FICO AP                                           *
*                                                                      *
*  Description:      Post IAP Documents for PO and Non-PO Invoices     *
*                    and Credit Memos                                  *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 08/01/18 VRAJAPUTRA D30K928896 - CHG0109670 - Initial program        *
*                                                                      *
*----------------------------------------------------------------------*
************************************************************************

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************
* Run Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_erpid  TYPE char5.           "ERP ID               "
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       cb_test  AS CHECKBOX           "Test Run             "
                           DEFAULT 'X'.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_email  FOR adr6-smtp_addr
                           NO INTERVALS.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb1.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       rb_fsapl RADIOBUTTON GROUP rb1 "File Server - Applictn
                           DEFAULT 'X'
                           USER-COMMAND CMD.
PARAMETERS:       rb_fsprs RADIOBUTTON GROUP rb1."File Server - Presentn
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_fpth1a TYPE text128          "Filepath - Inbound   "
                           MODIF ID FAS.
PARAMETERS:       p_fnam1a TYPE text128          "Filename - Inbound   "
                           MODIF ID FAS.
PARAMETERS:       p_fregxa TYPE text128          "File REGEX - Inbound "
                           MODIF ID FAS.
PARAMETERS:       p_farc1a TYPE text128          "Filepath - In Archive"
                           MODIF ID FAS.
PARAMETERS:       p_fnam1p TYPE text255          "Filename - Pres Server
                           MODIF ID FPS.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_fpth2a TYPE text128          "Filepath - Outbound  "
                           MODIF ID FAS.
PARAMETERS:       p_fnam2a TYPE text128          "Filename - Outbound  "
                           MODIF ID FAS.
PARAMETERS:       p_farc2a TYPE text128          "Filepath - Out Archive
                           MODIF ID FAS.
PARAMETERS:       p_fnam2p TYPE text255          "Filename - Pres Server
                           MODIF ID FPS.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_fpth3a TYPE text128          "Filepath - Log       "
                           MODIF ID FAS.
PARAMETERS:       p_fnam3a TYPE text128          "Filename - Log       "
                           MODIF ID FAS.
PARAMETERS:       p_fnam3p TYPE text255          "Filename - Pres Server
                           MODIF ID FPS.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb2.
