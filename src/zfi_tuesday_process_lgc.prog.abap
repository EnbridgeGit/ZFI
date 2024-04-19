*&---------------------------------------------------------------------*
*&  Include           ZFI_TUESDAY_PROCESS_LGC
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Program Name       :   ZFI_TUESDAY_PROCESS                           *
* Program Include    :   ZFI_TUESDAY_PROCESS_LGC                       *
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

*BOC by KMB on 14.8.2020 CHG0188574
AT SELECTION-SCREEN.
  CLEAR lv_fpth1a.
  CONCATENATE lv_fpth sy-sysid '/' INTO lv_fpth1a.
  IF p_pre = 'X' AND p_fpth1a CS lv_pre.
    MESSAGE e000(zfi01) WITH text-009.
  ENDIF.
  IF p_app = 'X' AND p_fpth1a NS lv_fpth1a.
    MESSAGE e000(zfi01) WITH text-009.
  ENDIF.
*EOC by KMB on 14.8.2020 CHG0188574

*=======================================================================
* START-OF-SELECTION
*=======================================================================
START-OF-SELECTION.

* Get data from PRRW Transaction
  PERFORM f_get_prrw.

* To Delete Runids
  IF NOT gt_nod[] IS INITIAL .
    PERFORM f_delete_run.
  ENDIF.

* To download the spool file of the job
  IF NOT p_job IS INITIAL.
    PERFORM f_spool_file.
  ENDIF.

* To read the next level doc details
  IF NOT gt_posted[] IS INITIAL.
    PERFORM f_doc_read  TABLES gt_posted
                        USING  ''.         "For Posted Flag
  ENDIF.
  IF NOT gt_partial[] IS INITIAL.
    PERFORM f_doc_read  TABLES gt_partial
                        USING  'X'.        "For Partial Flag
  ENDIF.

  IF NOT gt_partial[] IS INITIAL OR
     NOT gt_posted[]  IS INITIAL.
    PERFORM f_user_mail.
  ENDIF.
