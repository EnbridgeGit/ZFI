*&---------------------------------------------------------------------*
*&  Include           ZFI_ORD_IDOC_PROCESS_LGC
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFI_ORD_IDOC_PROCESS                          *
* Include            :   ZFI_ORD_IDOC_PROCESS_LGC                      *
* Author             :   AKMADASU                                      *
* Date               :   Jan 23, 2022                                  *
* Technical Contact  :   Ashok Madasu                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   To Process Order IDOCS                        *
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
  PERFORM submit_idoc_process.
  PERFORM submit_pgi_billing.
  PERFORM display_output.
