*&---------------------------------------------------------------------*
*&  Include           ZFI_AP_ERROR_WI_LGC
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       :  ZFI_AP_ERROR_WI_LGC                           *
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

INITIALIZATION.
  "make task non editable
  PERFORM f_screen_fields.

AT SELECTION-SCREEN.
  "validation for CC, email.
  PERFORM f_validation.

START-OF-SELECTION.
  "Fetch data
  PERFORM f_get_data.

END-OF-SELECTION.
  "collect and format data
  PERFORM f_form_data.

  "Send mail with attachment
  PERFORM f_sendmail.
