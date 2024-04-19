*&---------------------------------------------------------------------*
*&  Include           ZFI_AP_ERROR_WI_SCR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_AP_ERROR_WI_SCR                           *
*& Author             :  Kavya M B                                     *
*& Creation Date      :  14/11/2019                                    *
*& Object ID          :  CHG0153857                                    *
*& Application Area   :  FICO                                          *
*& Description        :  AP CI-SAP - automate job to locate invoice    *
*                        documents in workflow error status            *
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 14-11-2019   KMB         DECK920334  AP CI-SAP automate job to locate*
*                                      invoice documents in workflow   *
*                                      error status                    *
************************************************************************

SELECT-OPTIONS : s_coco FOR gv_s_coco,
                 s_date FOR gv_s_date OBLIGATORY.

PARAMETERS : p_task  TYPE sww_task DEFAULT 'WS98300017' OBLIGATORY,
             p_email TYPE sza5_d0700-smtp_addr.
