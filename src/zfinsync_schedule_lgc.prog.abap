*&---------------------------------------------------------------------*
*&  Include           ZFINSYNC_SCHEDULE_LGC
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFINSYNC_SCHEDULE                             *
* Include            :   ZFINSYNC_SCHEDULE_LGC                         *
* Author             :   AKMADASU                                      *
* Date               :   Jan 23, 2022                                  *
* Technical Contact  :   Ashok Madasu                                  *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :  To Schedule Finance Sync IDOC Process, PGI AMD *
*                    :  Billing Jobs paralelly in BACKGROUND           *
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
PERFORM get_server.
PERFORM get_process_idocs.
