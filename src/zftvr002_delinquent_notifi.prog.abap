*&---------------------------------------------------------------------*
*& Report  ZFTVR002_DELINQUENT_NOTIFI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* 08Mar17 JRHARTUNG ACR-3885 D30K928029 retrieve the manager email ID from HR
* 20Nov27 KMB       ACR-5194 D30K928408 Update workflow and delinquent transaction emails
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 10-01-2019   KMB         D30K929469  CHG0101193 - Update workflow    *
*                                      delinquent transaction emails   *
************************************************************************
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 02-08-2019   KMB         D30K930077  CHG0154273: Change the contact  *
*                                      details in ZTRIP                *
************************************************************************

REPORT  zftvr002_delinquent_notifi.

TYPES: BEGIN OF ty_without_trip,
          bdatu TYPE ptk34-bdatu,
          betrg TYPE ptk34-betrg,
          waers TYPE ptk34-waers,
          c_txt TYPE ptk34-c_txt,
          spkzl TYPE ptk34-spkzl,
       END OF ty_without_trip.
TYPES: BEGIN OF ty_with_trip,
          bdatu TYPE ptk34_inbel-bdatu,
          betrg TYPE ptk34_inbel-betrg,
          waers TYPE ptk34_inbel-waers,
          c_txt TYPE ptk34_inbel-c_txt,
          spkzl TYPE ptk34_inbel-spkzl,
          reinr TYPE ptk34_inbel-reinr,
       END OF ty_with_trip.

TYPES: BEGIN OF card_trans,
          bdatu TYPE ptk34_trans-bdatu,
          betrg TYPE ptk34_trans-betrg,
          waers TYPE ptk34_trans-waers,
          c_txt TYPE ptk34_trans-c_txt,
          spkzl TYPE ptk34_trans-spkzl,
          reinr TYPE ptk34_trans-reinr,
       END OF card_trans.
TYPES: BEGIN OF ty_email_data,
       filename      TYPE sdokpath-pathname,      " Filepath with Name
       log           TYPE string,                 " Error Log
       END OF ty_email_data.
TYPES:  BEGIN OF ty_msg,
        text TYPE char200,
        END OF ty_msg.

TYPES: BEGIN OF ty_username_pernr,                          "D30K928029
        username TYPE username,                             "D30K928029
        pernr    TYPE pernr_d,                              "D30K928029
       END   OF ty_username_pernr.                          "D30K928029

DATA: gs_username_pernr  TYPE ty_username_pernr.            "D30K928029

DATA: gt_pa0000          TYPE TABLE OF pa0000,
      gs_pa0000          TYPE pa0000,
      gs_pa0002          TYPE pa0002,
      gs_pa0105          TYPE pa0105,
      gs_pa0105_p        TYPE pa0105,                       "D30K928029
      gs_pa0105_approver TYPE pa0105,
      gs_pa0105_workitem TYPE pa0105,
      gs_pa0105_manager TYPE pa0105.
DATA: gt_withouttrip_e  TYPE TABLE OF ty_without_trip,
      gt_withouttrip_m  TYPE TABLE OF ty_without_trip,
      gt_withouttrip_ap TYPE TABLE OF ty_without_trip,
      gs_withouttrip    TYPE ty_without_trip,
      gt_with_trip_e    TYPE TABLE OF ty_with_trip,
      gt_with_trip_m    TYPE TABLE OF ty_with_trip,
      gt_with_trip_ap   TYPE TABLE OF ty_with_trip,
      gs_with_trip      TYPE ty_with_trip,
      gt_wait_appr_e    TYPE TABLE OF ty_with_trip,
      gt_wait_appr_s    TYPE TABLE OF ty_with_trip,
      gt_wait_appr_m    TYPE TABLE OF ty_with_trip,
      gt_wait_appr_s2    TYPE TABLE OF ty_with_trip,
      gt_wait_appr_m2    TYPE TABLE OF ty_with_trip,
      gt_wait_appr_s3    TYPE TABLE OF ty_with_trip,
      gt_wait_appr_ap   TYPE TABLE OF ty_with_trip,
      gt_msg            TYPE TABLE OF ty_msg,
      gs_msg            TYPE ty_msg.

DATA: lv_objtyp LIKE swotobjid-objtype,
      lv_objkey LIKE swotobjid-objkey,
      lt_wiid TYPE TABLE OF swr_wihdr WITH HEADER LINE.

DATA: lv_wiid LIKE swwwihead-wi_id,
      lv_app LIKE swwuserwi-user_id.

DATA: gv_rfcdest TYPE rfcdest.                              "D30K928029

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t02.
PARAMETERS: p_unproc TYPE c LENGTH 3,  "xx
            p_iees   TYPE c LENGTH 3,  "yy
            p_uees   TYPE c LENGTH 3.  "zz
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t03.
PARAMETERS:
            p_notim  TYPE c LENGTH 3,  "aa - note to manager
            p_notiap TYPE c LENGTH 3,  "bb - note to AP
            p_ap     TYPE string." DEFAULT
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.

  PERFORM get_data.
*Output messages
  SKIP 2.
  LOOP AT gt_msg INTO gs_msg.
    WRITE : / gs_msg-text.
  ENDLOOP.
  SKIP 3.
  WRITE : / 'Process completed............'.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_ccbel TYPE ptk34_t,
        ls_ccbel TYPE LINE OF ptk34_t,
        lt_inbel TYPE ptk34_inbel_t,
        ls_inbel TYPE LINE OF ptk34_inbel_t,
        lt_trans TYPE ptk34_trans_t,
        ls_trans TYPE ptk34_trans_t,
        ls_letzt TYPE ptk34_letzt,
        lv_subrc TYPE sy-subrc,
        lv_manager TYPE pernr_d,
        lv_manager_user TYPE username,
        lv_approver TYPE pernr_d,
        lv_approver_user TYPE username,
        lv_datum TYPE sy-datum,
        ls_ptrv_head TYPE ptrv_head,
        ls_ptrv_perio TYPE ptrv_perio,
        lt_ptrv_perio TYPE TABLE OF ptrv_perio,
        lt_ptrv_head TYPE TABLE OF ptrv_head,
        lv_days TYPE n LENGTH 3,
        lv_days1 TYPE n LENGTH 3.

  CLEAR: gt_msg.

  SELECT * FROM pa0000 INTO TABLE gt_pa0000
           WHERE stat2 = '3'
             AND endda >= sy-datum.
  DELETE ADJACENT DUPLICATES FROM gt_pa0000 COMPARING pernr.
  LOOP AT gt_pa0000 INTO gs_pa0000.
    CLEAR:  lt_ccbel,
            lt_inbel,
            lt_trans,
            ls_letzt,
            lv_subrc,
            lv_approver_user,
            lv_approver,
            lv_manager_user,
            lv_manager,
            gs_pa0105,
            gs_pa0105_approver,
            gs_pa0105_workitem,
            gs_pa0105_manager .

    CALL FUNCTION 'PTRM_UTIL_IMPORT_TC_CLUSTER'
      EXPORTING
        i_pernr                = gs_pa0000-pernr
*       I_SW_DECRYPTION        = 'X'
      IMPORTING
        et_ccbel               = lt_ccbel "Trip is not assigned
        et_inbel               = lt_inbel  "Trip is assigned to transaction
        et_trans               = lt_trans
        e_letzt                = ls_letzt
        e_subrc                = lv_subrc
      EXCEPTIONS
        ccnum_decryption_error = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    IF lv_subrc <> 0.
      CONTINUE.
    ENDIF.
    IF lt_ccbel[] IS INITIAL AND
       lt_inbel[] IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR: gt_withouttrip_e,
           gt_withouttrip_m,
           gt_withouttrip_ap,
           gs_withouttrip,
           gt_with_trip_e,
           gt_with_trip_m,
           gt_with_trip_ap,
           gs_with_trip,
           gt_wait_appr_e,
           gt_wait_appr_s,
           gt_wait_appr_m,
           gt_wait_appr_s2,
           gt_wait_appr_m2,
           gt_wait_appr_s3,
           gt_wait_appr_ap,
           lt_ptrv_perio,
           lt_ptrv_head.
*Process Credit card transations not assigned to TRIP
    LOOP AT lt_ccbel INTO ls_ccbel.
      gs_withouttrip-bdatu = ls_ccbel-bdatu.
      gs_withouttrip-betrg = ls_ccbel-betrg.
      gs_withouttrip-waers = ls_ccbel-waers.
      gs_withouttrip-c_txt = ls_ccbel-c_txt.
      gs_withouttrip-spkzl = ls_ccbel-spkzl.
      lv_days = sy-datum - ls_ccbel-bdatu.
      IF NOT p_unproc IS INITIAL.   "Insert by SPARGA SDP 61777
        IF lv_days >= p_unproc.
          APPEND gs_withouttrip TO gt_withouttrip_e.
        ENDIF.
        IF NOT p_notim IS INITIAL.   "Insert by SPARGA SDP 61777
          lv_days1 = p_unproc + p_notim.
          IF lv_days >= lv_days1.
            APPEND gs_withouttrip TO gt_withouttrip_m.
          ENDIF.
        ENDIF.
        IF NOT p_notiap IS INITIAL.   "Insert by SPARGA SDP 61777
          lv_days1 =  p_unproc + p_notiap .
*      lv_datum = sy-datum - ( p_unproc + p_notiap ).
          IF lv_days >= lv_days1.
            APPEND gs_withouttrip TO gt_withouttrip_ap.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
*Process Credit card transations assigned to TRIP
    IF lt_inbel IS NOT INITIAL.
      SELECT * FROM ptrv_perio INTO TABLE lt_ptrv_perio
        FOR ALL ENTRIES IN lt_inbel
        WHERE pernr = gs_pa0000-pernr
          AND reinr = lt_inbel-reinr.
      SELECT * FROM ptrv_head INTO TABLE lt_ptrv_head
        FOR ALL ENTRIES IN lt_inbel
        WHERE pernr = gs_pa0000-pernr
          AND reinr = lt_inbel-reinr.
    ENDIF.
    LOOP AT lt_inbel INTO ls_inbel.
      CLEAR: ls_ptrv_head,
             ls_ptrv_perio.
      gs_with_trip-bdatu = ls_inbel-bdatu.
      gs_with_trip-betrg = ls_inbel-betrg.
      gs_with_trip-waers = ls_inbel-waers.
      gs_with_trip-c_txt = ls_inbel-c_txt.
      gs_with_trip-spkzl = ls_inbel-spkzl.
      gs_with_trip-reinr = ls_inbel-reinr.
      READ TABLE lt_ptrv_perio INTO ls_ptrv_perio
           WITH KEY pernr = gs_pa0000-pernr
                    reinr = ls_inbel-reinr.
      READ TABLE lt_ptrv_head INTO ls_ptrv_head
           WITH KEY pernr = gs_pa0000-pernr
                    reinr = ls_inbel-reinr.
      lv_days = sy-datum - ls_inbel-bdatu.
      IF ( ls_ptrv_perio-antrg = '3' AND "Completed
         ls_ptrv_perio-abrec = '0' )  OR "Open
        ( ls_ptrv_perio-antrg = '6' AND "Trip Awaiting Docs
         ls_ptrv_perio-abrec = '1' ).    "Settled
        IF NOT p_iees IS INITIAL.  " Insert SPARGA SDP 61777
*        lv_datum = sy-datum -  p_iees.
          lv_days1 = p_iees.
*        IF ls_ptrv_head-dates >= lv_datum.
          IF lv_days >= lv_days1.
            APPEND gs_with_trip TO gt_with_trip_e.
          ENDIF.
          IF NOT p_notim IS INITIAL.   "Insert by SPARGA SDP 61777
            lv_days1 = p_iees + p_notim .
            IF lv_days >= lv_days1.
              APPEND gs_with_trip TO gt_with_trip_m.
            ENDIF.
          ENDIF.
          IF NOT p_notiap IS INITIAL.   "Insert by SPARGA SDP 61777
            lv_days1 = p_iees + p_notiap.
            IF lv_days >= lv_days1.
              APPEND gs_with_trip TO gt_with_trip_ap.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ls_ptrv_perio-antrg = '3' AND "Completed
         ls_ptrv_perio-abrec = '1'.    "To Be Settled
        IF NOT p_uees IS INITIAL.  " Insert SPARGA SDP 61777
          lv_days1 =  p_uees. "zz
          IF lv_days >= lv_days1.
            APPEND gs_with_trip TO gt_wait_appr_s. "approver
            APPEND gs_with_trip TO gt_wait_appr_e. "employee
          ENDIF.
          IF NOT p_notim IS INITIAL.   "Insert by SPARGA SDP 61777
            lv_days1 = p_uees + p_notim . "zz+aa
            IF lv_days >= lv_days1.
              APPEND gs_with_trip TO gt_wait_appr_m.
            ENDIF.
          ENDIF.
          IF NOT p_notiap IS INITIAL.   "Insert by SPARGA SDP 61777
            lv_days1 = p_uees + p_notiap . "zz+bb
            IF lv_days >= lv_days1.
              APPEND gs_with_trip TO gt_wait_appr_ap.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
****Function module to extract HR DATA - HREIC_READ_INFTY_DATA
    SELECT SINGLE * FROM pa0002 INTO gs_pa0002
                    WHERE pernr = gs_pa0000-pernr
                      AND endda >= sy-datum.
    IF sy-subrc <> 0.
      CONCATENATE 'Unable to find Info Type 0002 for Employee:'
                   gs_pa0000-pernr INTO gs_msg-text SEPARATED BY ' '.
      APPEND gs_msg TO gt_msg.
      CONTINUE.
    ENDIF.
    SELECT SINGLE * FROM pa0105 INTO gs_pa0105
                    WHERE pernr = gs_pa0000-pernr
                      AND subty = '0010'
                      AND endda >= sy-datum.
    IF sy-subrc <> 0.
      CONCATENATE 'Unable to find Info Type 0105 for Employee:'
                  gs_pa0000-pernr INTO gs_msg-text SEPARATED BY ' '.
      APPEND gs_msg TO gt_msg.
      CONTINUE.
    ENDIF.
*Get Approver
    CALL FUNCTION 'ZHR_GET_MANAGER'
      EXPORTING
*       IMP_USERNAME     =
        imp_pernr        = gs_pa0000-pernr
      IMPORTING
        exp_manager_user = lv_approver_user
        exp_manager_num  = lv_approver
      EXCEPTIONS
        nobody_found     = 1
        no_manager_found = 2
        no_data_supplied = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      CONCATENATE 'Unable to find Approver for Employee:'
                   gs_pa0000-pernr INTO gs_msg-text SEPARATED BY ' '.
      APPEND gs_msg TO gt_msg.
*   CONTINUE.
    ELSE.
*     SELECT SINGLE * INTO gs_pa0105_approver               "D30K928029
*                     FROM pa0105                           "D30K928029
*                    WHERE pernr = lv_approver              "D30K928029
*                      AND subty = '0010'                   "D30K928029
*                      AND endda >= sy-datum.               "D30K928029
      CLEAR    gs_pa0105_approver.                          "D30K928029
      CLEAR    lv_subrc.                                    "D30K928029
      PERFORM  f_get_it0105           USING lv_approver     "D30K928029
                                   CHANGING gs_pa0105_approver "K928029
                                            lv_subrc.       "D30K928029
*     IF sy-subrc <> 0.                                     "D30K928029
      IF     ( lv_subrc NE 0 ).                             "D30K928029
        CONCATENATE 'Unable to find Info Type 0105 for Approver:'
                  lv_approver INTO gs_msg-text SEPARATED BY ' '.
        APPEND gs_msg TO gt_msg.
      ENDIF.
    ENDIF.

* Get actual approver who has workitem
* Insert by SPARGA SDP 68154
    gt_wait_appr_s2[] = gt_wait_appr_s[].
    gt_wait_appr_m2[] = gt_wait_appr_m[].
    gt_wait_appr_s3[] = gt_wait_appr_s[].

    SORT  gt_wait_appr_s3 BY reinr.
    DELETE ADJACENT DUPLICATES FROM gt_wait_appr_s3 COMPARING reinr.

    IF NOT gt_wait_appr_s[] IS INITIAL.
      LOOP AT gt_wait_appr_s3 INTO gs_with_trip.
        CLEAR: lv_objkey, gt_wait_appr_s, gt_wait_appr_m, lv_wiid, lv_app.
        REFRESH: lt_wiid, gt_wait_appr_s, gt_wait_appr_m.
        gt_wait_appr_s[] = gt_wait_appr_s2[].
        gt_wait_appr_m[] = gt_wait_appr_m2[].
        DELETE gt_wait_appr_s WHERE reinr NE gs_with_trip-reinr.
        DELETE gt_wait_appr_m WHERE reinr NE gs_with_trip-reinr.

        CONCATENATE gs_pa0000-pernr gs_with_trip-reinr INTO lv_objkey.
        CONDENSE lv_objkey NO-GAPS.
        lv_objtyp = 'BUS2089'.
        CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
          EXPORTING
            objtype  = lv_objtyp
            objkey   = lv_objkey
          TABLES
            worklist = lt_wiid.
        READ TABLE lt_wiid INDEX 1.
        IF sy-subrc = 0.
          SELECT SINGLE wi_id FROM swwwihead INTO lv_wiid
            WHERE wi_type = 'W' AND
                  wi_stat IN ('READY', 'STARTED', 'RESERVED') AND
                  wi_rh_task = 'TS02000044' AND
                  top_wi_id = lt_wiid-wi_id.
          IF sy-subrc = 0.
            SELECT SINGLE user_id FROM swwuserwi INTO lv_app
              WHERE wi_id = lv_wiid AND
                    no_sel NE 'X'.
          ENDIF.
        ENDIF.
*       SELECT SINGLE PERNR FROM PA0105 INTO lv_approver    "D30K928029
*                      WHERE SUBTY = '0001' AND             "D30K928029
*                            USRID = LV_APP.                "D30K928029
        CLEAR    lv_approver.                               "D30K928029
        CLEAR    lv_subrc.                                  "D30K928029
        PERFORM  f_get_personnel_number                     "D30K928029
                                    USING lv_app            "D30K928029
                                 CHANGING lv_approver       "D30K928029
                                          lv_subrc.         "D30K928029
*       IF SY-SUBRC = 0.                                    "D30K928029
        IF     ( lv_subrc EQ 0 ).                           "D30K928029
*         SELECT SINGLE * FROM pa0105 INTO gs_pa0105_WORKITEM  "K928029
*                        WHERE pernr = lv_approver          "D30K928029
*                          AND subty = '0010'               "D30K928029
*                          AND endda >= sy-datum.           "D30K928029
          CLEAR    gs_pa0105_workitem.                      "D30K928029
          CLEAR    lv_subrc.                                "D30K928029
          PERFORM  f_get_it0105       USING lv_approver     "D30K928029
                                   CHANGING gs_pa0105_workitem "K928029
                                            lv_subrc.       "D30K928029
*         IF sy-subrc <> 0.                                 "D30K928029
          IF     ( lv_subrc NE 0 ).                         "D30K928029
            CONCATENATE 'Unable to find Info Type 0105 for Approver:'
                      lv_approver INTO gs_msg-text SEPARATED BY ' '.
            APPEND gs_msg TO gt_msg.
          ENDIF.
        ELSE.
          CONCATENATE 'Unable to find Info Type 0105 for Approver:'
                    lv_app INTO gs_msg-text SEPARATED BY ' '.
          APPEND gs_msg TO gt_msg.
        ENDIF.
* End of Insert SPARGA
        IF gt_wait_appr_m[] IS NOT INITIAL AND NOT lv_app IS INITIAL.
*    Get Approver's Manager
          CALL FUNCTION 'ZHR_GET_MANAGER'
            EXPORTING
              imp_username     = lv_app
*             imp_pernr        = lv_approver
            IMPORTING
              exp_manager_user = lv_manager_user
              exp_manager_num  = lv_manager
            EXCEPTIONS
              nobody_found     = 1
              no_manager_found = 2
              no_data_supplied = 3
              OTHERS           = 4.
          IF sy-subrc <> 0.
            CONCATENATE 'Unable to find the Manager of the current Approver:'
                        lv_app INTO gs_msg-text SEPARATED BY ' '.
            APPEND gs_msg TO gt_msg.
*       CONTINUE.
          ELSE.
*           SELECT SINGLE *                                 "D30K928029
*                    INTO gs_pa0105_manager                 "D30K928029
*                    FROM pa0105                            "D30K928029
*                   WHERE pernr = lv_manager                "D30K928029
*                     AND subty = '0010'                    "D30K928029
*                     AND endda >= sy-datum.                "D30K928029
            CLEAR    gs_pa0105_manager.                     "D30K928029
            CLEAR    lv_subrc.                              "D30K928029
            PERFORM  f_get_it0105     USING lv_manager      "D30K928029
                                   CHANGING gs_pa0105_manager "K928029
                                            lv_subrc.       "D30K928029
*           IF sy-subrc <> 0.                               "D30K928029
            IF     ( lv_subrc NE 0 ).                       "D30K928029
              CONCATENATE 'Unable to find Info Type 0105 for Approvers Manager:'
                        lv_manager INTO gs_msg-text SEPARATED BY ' '.
              APPEND gs_msg TO gt_msg.
            ENDIF.
          ENDIF.
        ENDIF.
* Insert by SPARGA SDP 68154
        IF gt_wait_appr_s[] IS NOT INITIAL AND
           gs_pa0105_workitem-usrid_long IS NOT INITIAL.
          PERFORM send_email USING '8'.
        ENDIF.
        IF gt_wait_appr_m[] IS NOT INITIAL AND
          gs_pa0105_manager-usrid_long IS NOT INITIAL.
          PERFORM send_email USING '9'.
        ENDIF.
      ENDLOOP.
    ENDIF.
* End of Insert SPARGA

    PERFORM email_without_trip.
    PERFORM email_with_trip.

  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  EMAIL_WITHOUT_TRIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM email_without_trip.

  IF gt_withouttrip_e[] IS NOT INITIAL AND
     gs_pa0105-usrid_long IS NOT INITIAL.
    PERFORM send_email USING '1'.
  ENDIF.
  IF gt_withouttrip_m[] IS NOT INITIAL AND
     gs_pa0105_approver-usrid_long IS NOT INITIAL.
    PERFORM send_email USING '2'.
  ENDIF.
  IF gt_withouttrip_ap[] IS NOT INITIAL AND
     p_ap IS NOT INITIAL.
    PERFORM send_email USING '3'.
  ENDIF.

ENDFORM.                    " EMAIL_WITHOUT_TRIP
*&---------------------------------------------------------------------*
*&      Form  EMAIL_WITH_TRIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM email_with_trip .

  IF gt_with_trip_e[] IS NOT INITIAL AND
     gs_pa0105-usrid_long IS NOT INITIAL.
    PERFORM send_email USING '4'.
  ENDIF.
  IF gt_with_trip_m[] IS NOT INITIAL AND
     gs_pa0105_approver-usrid_long IS NOT INITIAL.
    PERFORM send_email USING '5'.
  ENDIF.
  IF gt_with_trip_ap[] IS NOT INITIAL AND
     p_ap IS NOT INITIAL.
    PERFORM send_email USING '6'.
  ENDIF.
***********waiting for approval
  IF gt_wait_appr_e[] IS NOT INITIAL AND
     gs_pa0105-usrid_long IS NOT INITIAL.
    PERFORM send_email USING '7'.
  ENDIF.
*  IF gt_wait_appr_s[] IS NOT INITIAL AND
*     gs_pa0105_approver-usrid_long IS NOT INITIAL.
*    PERFORM send_email USING '8'.
*  ENDIF.
*  IF gt_wait_appr_m[] IS NOT INITIAL AND
*    gs_pa0105_manager-usrid_long IS NOT INITIAL.
*    PERFORM send_email USING '9'.
*  ENDIF.
  IF gt_wait_appr_ap[] IS NOT INITIAL AND
     p_ap IS NOT INITIAL.
    PERFORM send_email USING '10'.
  ENDIF.

ENDFORM.                    " EMAIL_WITH_TRIP
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_email USING p_flag TYPE i.

  DATA: lt_objtxt    TYPE TABLE OF solisti1,
          lt_objpack   TYPE TABLE OF sopcklsti1,
          lt_reclist   TYPE TABLE OF somlreci1,
          lt_objhead   TYPE soli_tab.

  DATA: lv_lines     TYPE i,
        lv_string    TYPE string,
        lwa_objpack  TYPE sopcklsti1,
        lwa_objtxt   TYPE solisti1,
        lwa_doc_chng TYPE sodocchgi1,
        lwa_reclist  TYPE somlreci1,
        lv_space     TYPE char01  VALUE ' '.

  CONSTANTS: lc_f(1)      TYPE c VALUE 'F',
             lc_u(1)      TYPE c VALUE 'U',
             lc_int(3)    TYPE c VALUE 'INT',
             lc_htm(3)    TYPE c VALUE 'HTM',
             lc_x(1)      TYPE c VALUE 'X'.

* Prepare Email Content
  PERFORM f_build_mail_content USING p_flag
                               CHANGING lt_objtxt.

* Object with main text of the mail.
  lwa_objtxt = lv_space.
  APPEND lwa_objtxt TO lt_objtxt.
  CLEAR lwa_objtxt.

  DESCRIBE TABLE lt_objtxt LINES lv_lines.
  IF p_flag < '4'.
    lv_string = text-003.
  ELSEIF ( p_flag > '3' AND p_flag < '7' ).
    lv_string = text-004.
  ELSE.
    lv_string = text-011.
  ENDIF.

  lwa_doc_chng-obj_descr  = lv_string.
  lwa_doc_chng-sensitivty = lc_f.
  lwa_doc_chng-doc_size   = lv_lines * 255.

* Pack to main body.
  lwa_objpack-head_start = 1.
  lwa_objpack-head_num   = 0.
  lwa_objpack-body_start = 1.
  lwa_objpack-body_num   = lv_lines.
  lwa_objpack-doc_type   = lc_htm.
  APPEND lwa_objpack TO lt_objpack.
  CLEAR lwa_objpack.

  lwa_reclist-copy = lc_x.

* Map Email ID(s)
  IF p_flag = '1' OR
     p_flag = '4' OR
     p_flag = '7'.
    lwa_reclist-receiver = gs_pa0105-usrid_long.
  ENDIF.
  IF p_flag = '2' OR
     p_flag = '5'.
    lwa_reclist-receiver = gs_pa0105_approver-usrid_long.
  ENDIF.
  IF p_flag = '8'.
    lwa_reclist-receiver = gs_pa0105_workitem-usrid_long.
  ENDIF.
  IF p_flag = '3' OR
     p_flag = '6' OR
     p_flag = '10'.
    lwa_reclist-receiver = p_ap.
  ENDIF.
  IF p_flag = '9'.
    lwa_reclist-receiver = gs_pa0105_manager-usrid_long.
  ENDIF.
  lwa_reclist-rec_type   = lc_u.
  lwa_reclist-com_type   = lc_int.
  lwa_reclist-notif_del  = lc_x.
  lwa_reclist-notif_ndel = lc_x.
  lwa_reclist-copy       = space.
  APPEND lwa_reclist TO lt_reclist.

* Funcion module for sending email.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = lwa_doc_chng
      put_in_outbox              = lc_x
      commit_work                = lc_x
    TABLES
      packing_list               = lt_objpack
      object_header              = lt_objhead
      contents_txt               = lt_objtxt
      receivers                  = lt_reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc <> 0.
    MESSAGE i000(zfi01) WITH text-006.
  ELSE.

  ENDIF.

ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_MAIL_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_build_mail_content  USING p_flag TYPE i
                           CHANGING p_lt_objtxt TYPE table.

  DATA: lwa_objtxt       TYPE solisti1,
        lwa_email_data   TYPE ty_email_data,
        lv_name TYPE char120.

*Prepare HTML mail header
  IF p_flag < '4'.
    CONCATENATE '<html>' '<body>' '<h2 style="font-family:arial"><caption><b><u>'
                text-003 '</u></b><caption></h2>' '<ul>' '</ul>'
                INTO lwa_objtxt SEPARATED BY space.
  ELSEIF ( p_flag > '3' AND p_flag < '7' ).
    CONCATENATE '<html>' '<body>' '<h2 style="font-family:arial"><caption><b><u>'
               text-004 '</u></b><caption></h2>' '<ul>' '</ul>'
               INTO lwa_objtxt SEPARATED BY space.
  ELSE.
    CONCATENATE '<html>' '<body>' '<h2 style="font-family:arial"><caption><b><u>'
               text-011 '</u></b><caption></h2>' '<ul>' '</ul>'
               INTO lwa_objtxt SEPARATED BY space.
  ENDIF.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
* Insert SPARGA SDP 68154
*Insert Paragraph
  IF p_flag < '4'.
    CONCATENATE text-t04
                text-t05
                INTO lwa_objtxt
                SEPARATED BY space.
    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
* BOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
*    MOVE text-T06 to lwa_objtxt.
    "BOC by KMB on 8.2.2019 CHG0154273: Change the contact details in the work flow email notifications of ZTRIP.
    "BOC by KMB on 10.01.19 CHG0101193 - Update workfow delinquent transaction emails
    CONCATENATE text-T11
                text-T12
                INTO lwa_objtxt
                SEPARATED BY space.
*    lwa_objtxt = text-t11.
    "EOC by KMB on 10.01.19 CHG0101193 - Update workfow delinquent transaction emails
    "EOC by KMB on 8.2.2019 CHG0154273: Change the contact details in the work flow email notifications of ZTRIP.
* EOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
  ELSEIF ( p_flag > '3' AND p_flag < '7' ).
* BOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
*    CONCATENATE text-T04
*                text-T07
*                INTO lwa_objtxt
*                SEPARATED BY space.
    CONCATENATE text-t04
                text-t05
                INTO lwa_objtxt
                SEPARATED BY space.
* EOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
* BOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
*    MOVE text-T06 to lwa_objtxt.
    "BOC by KMB on 8.2.2019 CHG0154273: Change the contact details in the work flow email notifications of ZTRIP.
    "BOC by KMB on 10.01.19 CHG0101193 - Update workfow delinquent transaction emails
    CONCATENATE text-T11
                text-T12
                INTO lwa_objtxt
                SEPARATED BY space.
*    lwa_objtxt = text-t11.
    "EOC by KMB on 10.01.19 CHG0101193 - Update workfow delinquent transaction emails
    "EOC by KMB on 8.2.2019 CHG0154273: Change the contact details in the work flow email notifications of ZTRIP.
* EOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
  ELSE.
    CLEAR: lwa_objtxt.
* BOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
*    CONCATENATE text-T09
*                text-T10
*                INTO lwa_objtxt
*                SEPARATED BY space.
    CONCATENATE text-t13
                text-t14
                INTO lwa_objtxt
                SEPARATED BY space.
* EOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
    CONCATENATE text-t04
                text-t08
                INTO lwa_objtxt
                SEPARATED BY space.
    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
* BOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
*    MOVE text-T06 to lwa_objtxt.
    "BOC by KMB on 8.2.2019 CHG0154273: Change the contact details in the work flow email notifications of ZTRIP.
    "BOC by KMB on 10.01.19 CHG0101193 - Update workfow delinquent transaction emails
    CONCATENATE text-T11
                text-T12
                INTO lwa_objtxt
                SEPARATED BY space.
*    lwa_objtxt = text-t11.
    "EOC by KMB on 10.01.19 CHG0101193 - Update workfow delinquent transaction emails
    "EOC by KMB on 8.2.2019 CHG0154273: Change the contact details in the work flow email notifications of ZTRIP.
* EOC by KMB 20.11.2017 ACR-5194 Update workflow and delinquent transaction emails
    APPEND lwa_objtxt TO p_lt_objtxt.
    CLEAR: lwa_objtxt.
  ENDIF.
* End of Insert SPARGA

  CONCATENATE 'Personnel: ' gs_pa0002-pernr ',' gs_pa0002-vorna gs_pa0002-nachn
              INTO lv_name SEPARATED BY space.
  CONCATENATE '<h4 style="font-family:arial" align="left" ><u>'
              lv_name '</u></h4>'
              INTO lwa_objtxt SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  CONCATENATE '<table border="2" width="100%">'
              '<tr> <td>'
              INTO lwa_objtxt
              SEPARATED BY space.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  IF p_flag < '4'.
    CONCATENATE text-005 '</td><td>'
                text-006 '</td><td>'
                text-007 '</td><td>'
                text-008 '</td><td>'
                text-009 '</td></tr>'
*                text-010 '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  ELSE.
    CONCATENATE text-010 '</td><td>'
                text-005 '</td><td>'
                text-006 '</td><td>'
                text-007 '</td><td>'
                text-008 '</td><td>'
                text-009 '</td></tr>'
                INTO lwa_objtxt
                SEPARATED BY space.
  ENDIF.
  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

  IF p_flag = '1'.
    LOOP AT gt_withouttrip_e INTO gs_withouttrip.

* Prepare Table
*    CONCATENATE '<br> <table border="2" width="50%">'
*                '<tr> <td>'
*                INTO lwa_objtxt
*                SEPARATED BY space.

*    APPEND lwa_objtxt TO p_lt_objtxt.
*    CLEAR: lwa_objtxt.
***T70C

      CONCATENATE  '<tr> <td>'
                  gs_withouttrip-bdatu '</td><td>'
                  gs_withouttrip-betrg '</td><td>'
                  gs_withouttrip-waers '</td><td>'
                  gs_withouttrip-c_txt '</td><td>'
                  gs_withouttrip-spkzl '</td></tr>'
*                  '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.
  IF p_flag = '2'.
    LOOP AT gt_withouttrip_m INTO gs_withouttrip.

      CONCATENATE  '<tr> <td>'
*                          '</td></tr>'
                  gs_withouttrip-bdatu '</td><td>'
                  gs_withouttrip-betrg '</td><td>'
                  gs_withouttrip-waers '</td><td>'
                  gs_withouttrip-c_txt '</td><td>'
                  gs_withouttrip-spkzl '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.
  IF p_flag = '3'.
    LOOP AT gt_withouttrip_ap INTO gs_withouttrip.

      CONCATENATE  '<tr> <td>'
*                          '</td></tr>'
                  gs_withouttrip-bdatu '</td><td>'
                  gs_withouttrip-betrg '</td><td>'
                  gs_withouttrip-waers '</td><td>'
                  gs_withouttrip-c_txt '</td><td>'
                  gs_withouttrip-spkzl '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.
  IF p_flag = '4'.
    LOOP AT gt_with_trip_e INTO gs_with_trip.

      CONCATENATE  '<tr> <td>'
                  gs_with_trip-reinr '</td><td>'
                  gs_with_trip-bdatu '</td><td>'
                  gs_with_trip-betrg '</td><td>'
                  gs_with_trip-waers '</td><td>'
                  gs_with_trip-c_txt '</td><td>'
                  gs_with_trip-spkzl '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.
  IF p_flag = '5'.
    LOOP AT gt_with_trip_m INTO gs_with_trip.

      CONCATENATE  '<tr> <td>'
                  gs_with_trip-reinr '</td><td>'
                  gs_with_trip-bdatu '</td><td>'
                  gs_with_trip-betrg '</td><td>'
                  gs_with_trip-waers '</td><td>'
                  gs_with_trip-c_txt '</td><td>'
                  gs_with_trip-spkzl '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.
  IF p_flag = '6'.
    LOOP AT gt_with_trip_ap INTO gs_with_trip.

      CONCATENATE  '<tr> <td>'
                  gs_with_trip-reinr '</td><td>'
                  gs_with_trip-bdatu '</td><td>'
                  gs_with_trip-betrg '</td><td>'
                  gs_with_trip-waers '</td><td>'
                  gs_with_trip-c_txt '</td><td>'
                  gs_with_trip-spkzl '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.
  IF p_flag = '7'.
    LOOP AT gt_wait_appr_e INTO gs_with_trip.

      CONCATENATE  '<tr> <td>'
                  gs_with_trip-reinr '</td><td>'
                  gs_with_trip-bdatu '</td><td>'
                  gs_with_trip-betrg '</td><td>'
                  gs_with_trip-waers '</td><td>'
                  gs_with_trip-c_txt '</td><td>'
                  gs_with_trip-spkzl '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.
  IF p_flag = '8'.
    LOOP AT gt_wait_appr_s INTO gs_with_trip.

      CONCATENATE  '<tr> <td>'
                  gs_with_trip-reinr '</td><td>'
                  gs_with_trip-bdatu '</td><td>'
                  gs_with_trip-betrg '</td><td>'
                  gs_with_trip-waers '</td><td>'
                  gs_with_trip-c_txt '</td><td>'
                  gs_with_trip-spkzl '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.
  IF p_flag = '9'.
    LOOP AT gt_wait_appr_m INTO gs_with_trip.

      CONCATENATE  '<tr> <td>'
                  gs_with_trip-reinr '</td><td>'
                  gs_with_trip-bdatu '</td><td>'
                  gs_with_trip-betrg '</td><td>'
                  gs_with_trip-waers '</td><td>'
                  gs_with_trip-c_txt '</td><td>'
                  gs_with_trip-spkzl '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.
  IF p_flag = '10'.
    LOOP AT gt_wait_appr_ap INTO gs_with_trip.

      CONCATENATE  '<tr> <td>'
                  gs_with_trip-reinr '</td><td>'
                  gs_with_trip-bdatu '</td><td>'
                  gs_with_trip-betrg '</td><td>'
                  gs_with_trip-waers '</td><td>'
                  gs_with_trip-c_txt '</td><td>'
                  gs_with_trip-spkzl '</td></tr>'
                  INTO lwa_objtxt
                  SEPARATED BY space.

      APPEND lwa_objtxt TO p_lt_objtxt.
      CLEAR: lwa_objtxt.

    ENDLOOP.
  ENDIF.

  MOVE: '</table> <br>' TO lwa_objtxt.
  APPEND: lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.
*End of HTML text here
  CONCATENATE '</body>'
              '</html>'
         INTO lwa_objtxt
         SEPARATED BY space.

  APPEND lwa_objtxt TO p_lt_objtxt.
  CLEAR: lwa_objtxt.

ENDFORM.                    " F_BUILD_MAIL_CONTENT
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_personnel_number                        "D30K928029
*&---------------------------------------------------------------------*
*       Retrieve the email id from the HR system
*----------------------------------------------------------------------*
FORM f_get_personnel_number                                 "D30K928029
  USING    iv_username                 TYPE username
  CHANGING cv_pernr                    TYPE pernr_d
           cv_subrc                    TYPE sysubrc.

  DATA:    lv_pernr                    TYPE pernr_d.

  CLEAR    cv_pernr.
  CLEAR    cv_subrc.

  IF     ( iv_username IS INITIAL ).
    cv_subrc = 11.
    RETURN.
  ENDIF.

  IF     ( gv_rfcdest IS INITIAL ).

    CALL FUNCTION 'ZFI_GET_RFC_DEST'
      EXPORTING
        imp_paramtype = 'HR'
      IMPORTING
        exp_rfcdest   = gv_rfcdest.

  ENDIF.

  IF     ( gv_rfcdest IS INITIAL ).
    cv_subrc = 12.
    RETURN.
  ENDIF.

  IF     ( gs_username_pernr-username    EQ iv_username ).
    CLEAR                                   cv_pernr.
    MOVE   gs_username_pernr-pernr       TO cv_pernr.
    RETURN.
  ENDIF.

*eject
  CLEAR    lv_pernr.

  CALL FUNCTION 'ZFI_CONVERT_USER_TO_PERNR'
    DESTINATION gv_rfcdest
    EXPORTING
      imp_username = iv_username
    IMPORTING
      exp_pernr    = lv_pernr.

  IF     ( lv_pernr IS INITIAL ).
    cv_subrc = 13.
    RETURN.
  ENDIF.

  CLEAR                                     gs_username_pernr.
  MOVE     iv_username                   TO gs_username_pernr-username.
  MOVE     lv_pernr                      TO gs_username_pernr-pernr.

  CLEAR                                     cv_pernr.
  MOVE     gs_username_pernr-pernr       TO cv_pernr.

ENDFORM.                    " f_get_personnel_number        "D30K928029
*eject
*&---------------------------------------------------------------------*
*&      Form  f_get_it0105                                  "D30K928029
*&---------------------------------------------------------------------*
*       Retrieve the email id from the HR system
*----------------------------------------------------------------------*
FORM f_get_it0105                                           "D30K928029
  USING    iv_pernr                    TYPE pernr_d
  CHANGING cs_pa0105                   TYPE pa0105
           cv_subrc                    TYPE sysubrc.

  DATA:    ls_return                   TYPE bapireturn1,
           ls_communication            TYPE bapip0105nl,
           lt_communication            TYPE STANDARD TABLE
                                         OF bapip0105nl.

  CLEAR    cs_pa0105.
  CLEAR    cv_subrc.

  IF     ( iv_pernr IS INITIAL ).
    cv_subrc = 21.
    RETURN.
  ENDIF.

  IF     ( gv_rfcdest IS INITIAL ).

    CALL FUNCTION 'ZFI_GET_RFC_DEST'
      EXPORTING
        imp_paramtype = 'HR'
      IMPORTING
        exp_rfcdest   = gv_rfcdest.

  ENDIF.

  IF     ( gv_rfcdest IS INITIAL ).
    cv_subrc = 22.
    RETURN.
  ENDIF.

  IF     ( gs_pa0105_p-pernr             EQ iv_pernr ).
    CLEAR                                   cs_pa0105.
    MOVE   gs_pa0105_p                   TO cs_pa0105.
    RETURN.
  ENDIF.

*eject
  CLEAR    lt_communication[].
  CLEAR    ls_return.

  CALL FUNCTION 'BAPI_EMPLCOMM_GETDETAILEDLIST'
    DESTINATION gv_rfcdest
    EXPORTING
      employeenumber   = iv_pernr
      subtype          = '0010'
      timeintervallow  = sy-datum
      timeintervalhigh = sy-datum
    IMPORTING
      return           = ls_return
    TABLES
      communication    = lt_communication.

  IF     ( ls_return-type CA 'AaEe' ).
    cv_subrc = 23.
    RETURN.
  ENDIF.

  CLEAR                                     ls_communication.
  READ     TABLE lt_communication      INTO ls_communication INDEX 1.
  IF     ( sy-subrc NE 0 ).
    cv_subrc = 24.
    RETURN.
  ENDIF.

  CLEAR                                     gs_pa0105_p.
  MOVE     ls_communication-employeeno   TO gs_pa0105_p-pernr.
  MOVE     ls_communication-subtype      TO gs_pa0105_p-subty.
  MOVE     ls_communication-objectid     TO gs_pa0105_p-objps.
  MOVE     ls_communication-lockindic    TO gs_pa0105_p-sprps.
  MOVE     ls_communication-validend     TO gs_pa0105_p-endda.
  MOVE     ls_communication-validbegin   TO gs_pa0105_p-begda.
  MOVE     ls_communication-recordnr     TO gs_pa0105_p-seqnr.
  MOVE     ls_communication-commtype     TO gs_pa0105_p-usrty.
  MOVE     ls_communication-id           TO gs_pa0105_p-usrid_long.

  CLEAR                                     cs_pa0105.
  MOVE     gs_pa0105_p                   TO cs_pa0105.

ENDFORM.                    " f_get_it0105                  "D30K928029
