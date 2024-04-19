*&---------------------------------------------------------------------*
*&  Include           ZFAPI121_VENDOR_INBOUND_SCR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFAPI121_VENDOR_INBOUND                       *
* Program Include    :   ZFAPI121_VENDOR_INBOUND_SCR                   *
* Author             :                                                 *
* Date               :   Apr 15, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   Vendor Inbound Interface                      *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 15-Apr-2018  CPALYAM     D30K929071-Initial development              *
*----------------------------------------------------------------------*

SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_erpid  TYPE char5.           "ERP ID               "
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       cb_test  AS CHECKBOX           "Test Run             "
                           DEFAULT 'X'.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_email  FOR gv_smtp_addr
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
