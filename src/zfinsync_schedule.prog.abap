*&---------------------------------------------------------------------*
*& Report  ZFINSYNC_SCHEDULE
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Name       :   ZFINSYNC_SCHEDULE                             *
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

REPORT  ZFINSYNC_SCHEDULE.

INCLUDE ZFINSYNC_SCHEDULE_TOP.
INCLUDE ZFINSYNC_SCHEDULE_SCN.
INCLUDE ZFINSYNC_SCHEDULE_LGC.
INCLUDE ZFINSYNC_SCHEDULE_F01.
