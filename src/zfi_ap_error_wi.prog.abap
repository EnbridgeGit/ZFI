*&---------------------------------------------------------------------*
*& Report  ZFI_AP_ERROR_WI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_AP_ERROR_WI                               *
*& Author             :  Kavya M B                                     *
*& Creation Date      :  06/12/2019                                    *
*& Object ID          :                                     *
*& Application Area   :  FICO                                          *
*& Description        :  AP CI-SAP - automate job to locate invoice    *
*                        documents in workflow error status            *
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 06-12-2019   KMB         D30K930323   AP CI-SAP automate   *
*                                      job to locate invoice documents *
*                                      in workflow error status        *
************************************************************************

REPORT  zfi_ap_error_wi.

******data declaration ******
INCLUDE zfi_ap_error_wi_top.

******selection screen
INCLUDE zfi_ap_error_wi_scr.

********program logic****
INCLUDE zfi_ap_error_wi_lgc.

******* form routines******
INCLUDE zfi_ap_error_wi_f01.
