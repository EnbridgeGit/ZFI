
*&---------------------------------------------------------------------*
* Program Name       :   ZPS_UPDATE_SETTLEMENT_RULE                    *
* Author             :   Ashok Kumar M                                 *
* Date               :   Aug 10, 2018                                  *
* Functional Contact :   Robert Ju                                     *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  Update Settlement rule for Account assignment  *
*                       indicator is flged for each WBS numbers        *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*

*----------------------------------------------------------------------*

REPORT  ZFI_UPDATE_SETTLEMENT_RULE.

INCLUDE ZFI_UPDATE_SETTLEMENT_TOP.    " Data definition
INCLUDE ZFI_UPDATE_SETTLEMENT_SCR.    " Selection screen
INCLUDE ZFI_UPDATE_SETTLEMENT_FRM.    " Subroutines
INCLUDE ZFI_UPDATE_SETTLEMENT_MN.     " Main
