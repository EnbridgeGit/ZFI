*&---------------------------------------------------------------------*
*&  Include           ZFI_TUESDAY_PROCESS_SCR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_TUESDAY_PROCESS                           *
* Program Include    :   ZFI_TUESDAY_PROCESS_SCR                       *
* Author             :                                                 *
* Date               :   Oct 17, 2018                                  *
* Technical Contact  :   Chaitanya Palyam                              *
* Business Contact   :                                                 *
*                                                                      *
* Purpose            :   PRRW Tuesday Process Automation               *
* Notes              :                                                 *
*                                                                      *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By  CTS         Description                     *
* ---------------------------------------------------------------------*
* 17-Oct-2018  CPALYAM     D30K929184-Initial development              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By    CTS        Description                    *
* ---------------------------------------------------------------------*
* 14-AUG-2020  KMB          D30K930651 CHG0188574 ZFI_MONDAY_PROCESS   *
*                                      ZFI_TUESDAY_PROCESS are not able*
*                                      to write to SAP Fileshare in US *
*                                      s/m & to UNIX servr in UG/SW s/m*
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECTION-SCREEN: SKIP 1.
*BOC by KMB on 14.8.2020 CHG0188574
*parameters:       p_fpth1a type string default 'P:\SAPExpense\2018\'.          "Filepath
parameters:       p_fpth1a type ibipparms-path default 'P:\SAPExpense\2018\'.          "Filepath
*EOC by KMB on 14.8.2020 CHG0188574
SELECTION-SCREEN: SKIP 1.
PARAMETERS        p_job    TYPE btcjob.
SELECTION-SCREEN: SKIP 1.

*BOC by KMB on 14.8.2020 CHG0188574
PARAMETERS : p_app   RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND cmnd. "Application Server
SELECTION-SCREEN: SKIP 1.
PARAMETERS : p_pre   RADIOBUTTON GROUP rad2.
*EOC by KMB on 14.8.2020 CHG0188574


SELECTION-SCREEN: END   OF BLOCK ssb1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_umail  FOR gv_smtp_addr " User Mail
                           NO INTERVALS.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_email  FOR gv_smtp_addr " Function Team Mail
                           NO INTERVALS.

SELECTION-SCREEN: END   OF BLOCK ssb2.

*BOC by KMB on 14.8.2020 CHG0188574
DATA : gv_fpth1a TYPE string,
       lv_fpth TYPE string VALUE '/usr/sap/interfaces/',
       lv_pre TYPE string VALUE '/usr/',
       lv_fpth1a TYPE string.
gv_fpth1a = p_fpth1a.
*EOC by KMB on 14.8.2020 CHG0188574


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpth1a.
*BOC by KMB on 14.8.2020 CHG0188574
  IF p_pre = 'X'.
*EOC by KMB on 14.8.2020 CHG0188574
  cl_gui_frontend_services=>directory_browse(
    CHANGING
*BOC by KMB on 14.8.2020 CHG0188574
*      selected_folder      = p_fpth1a
      selected_folder      = gv_fpth1a
*EOC by KMB on 14.8.2020 CHG0188574
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
         ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*BOC by KMB on 14.8.2020 CHG0188574
  ELSEIF p_app = 'X'.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
        directory        = gv_fpth1a
      IMPORTING
        serverfile       = gv_fpth1a
      EXCEPTIONS
        canceled_by_user = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      MESSAGE 'File Path not found' TYPE 'E'.
    ENDIF.
  ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574
