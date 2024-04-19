*&---------------------------------------------------------------------*
*&  Include           ZFI_WBS_STATUSCHANGE_SCR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
* Program Name       :  ZFI_WBS_STATUSCHANGE                           *
* Include            :  ZFI_WBS_STATUSCHANGE_SCR                       *
* Author             :  Ashok Madasu                                   *
* Date               :  20-Aug-2018                                    *
* Technical Contact  :  Ashok Madasu                                   *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  WBS status change report                       *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 22/08/2019   akmadasu    D30K930092  CHG0138619 - in-service and     *
*                          D30K930231  completion date updation to WBSe*
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:S_PSPNR1 FOR PROJ-PSPNR. " MODIF ID BCD.
SELECT-OPTIONS:S_PSPNR FOR PRPS-PSPNR . " MODIF ID ABC.
SELECTION-SCREEN END OF BLOCK B1.
**--START OF CHANGES BY AKMADASU CHG0138619
**SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
**PARAMETERS: r_rtot  RADIOBUTTON GROUP sgrp DEFAULT 'X'
**                                           USER-COMMAND ucomm,
**            r_ttor  RADIOBUTTON GROUP sgrp,
**            r_clsd  RADIOBUTTON GROUP sgrp,
**            r_rev   RADIOBUTTON GROUP sgrp,
**"CHG0121630 - Powerplan development for Req 11
**            r_crrl  RADIOBUTTON GROUP sgrp.
**PARAMETERS:p_isrvdt TYPE prps-usr08 MODIF ID rtc,
**           p_cmpldt TYPE prps-usr09 MODIF ID ttc,
**           p_estrdt TYPE proj-plfaz MODIF ID ctr,
**           p_efnsdt TYPE proj-plsez MODIF ID ctr,
**           p_eisrdt TYPE proj-eprog MODIF ID ctr.
**"CHG0121630 - Powerplan development for Req 11
**SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: R_RTOT  RADIOBUTTON GROUP SGRP DEFAULT 'X'.
SELECTION-SCREEN:COMMENT 5(25) TEXT-110 FOR FIELD R_RTOT.
SELECTION-SCREEN:POSITION 60,COMMENT 45(15) TEXT-111 FOR FIELD P_USR08.
PARAMETERS: P_USR08 TYPE PRPS-USR08.
SELECTION-SCREEN: END   OF LINE.
PARAMETERS:R_TTOR  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: R_CLSD  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN:COMMENT 5(40) TEXT-210 FOR FIELD R_CLSD.
SELECTION-SCREEN:POSITION 60,COMMENT 45(15) TEXT-211 FOR FIELD P_USR09.
PARAMETERS: P_USR09 TYPE PRPS-USR09.
SELECTION-SCREEN: END   OF LINE.
*&--Start of code added by JOOKONTR CHG0138619
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: RT_CLSD  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN:COMMENT 5(40) TEXT-212 FOR FIELD RT_CLSD.
SELECTION-SCREEN: END   OF LINE.
*&--End of code added by JOOKONTR CHG0138619
PARAMETERS:R_REV   RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:R_CRTOR RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN:COMMENT 5(25) TEXT-310 FOR FIELD R_CRTOR.
SELECTION-SCREEN:POSITION 60,COMMENT 45(15) TEXT-311 FOR FIELD P_PLFAZ.
PARAMETERS: P_PLFAZ TYPE PROJ-PLFAZ.
SELECTION-SCREEN: END   OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN:POSITION 60,COMMENT 45(15) TEXT-411 FOR FIELD P_PLSEZ.
PARAMETERS: P_PLSEZ TYPE PROJ-PLSEZ.
SELECTION-SCREEN: END   OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN:POSITION 60,COMMENT 45(15) TEXT-511 FOR FIELD P_EPROG.
PARAMETERS: P_EPROG TYPE PROJ-EPROG.
SELECTION-SCREEN: END   OF LINE.

SELECTION-SCREEN END OF BLOCK B2.
**--END OF CHANGES BY AKMADASU CHG0138619
