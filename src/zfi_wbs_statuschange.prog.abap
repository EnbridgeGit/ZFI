*&---------------------------------------------------------------------*
*& Report  ZFI_WBS_STATUSCHANGE                                      *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Program Name       :  ZFI_WBS_STATUSCHANGE                           *
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
* 22/08/2019   JOOKONTR    D30K930297  CHG0164542 Changes in all       *
*                                      options                         *
*&---------------------------------------------------------------------*

REPORT  zfi_wbs_statuschange.

INCLUDE zfi_wbs_statuschange_top.    " Data definition
INCLUDE zfi_wbs_statuschange_scr.    " Selection screen
INCLUDE zfi_wbs_statuschange_frm.    " Subroutines
INCLUDE zfi_wbs_statuschange_mn.     " Main
