REPORT  zfapr018_workflowreport.
*&---------------------------------------------------------------------*
*& Report  ZWORKFLOWREPORT                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*  Author:      Brian Boundy                                           *
*  Date:        July, 2012.                                            *
*  Description: This will show all the AP Workflow with Invoice detail *
*                                                                      *
*                                                                      *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*                                                                      *
*                                                                      *
*&---------------------------------------------------------------------*
TABLES: swwwihead,
        vbsegk.
TYPE-POOLS: slis.
TYPES:  BEGIN OF ty_nonpo,
          wi_id       LIKE sww_contob-wi_id,
          wf_ver      TYPE swd_versio,
          startd      TYPE swwwihead-wi_cd,
          startt      TYPE swwwihead-wi_ct,
          year        LIKE vbkpf-gjahr,
          docnum      LIKE vbkpf-belnr,
          cocode      LIKE vbkpf-ausbk,
          vendor      TYPE lifnr,
          amount      TYPE dmbtr,
          parkind(1)  TYPE c,
          wf_init     TYPE swp_initia,
          rco         TYPE swp_initia,
          approver    TYPE swp_initia,
          agent       LIKE swwwihead-wi_aagent,
          agtype      TYPE string,
          status      LIKE swwwihead-wi_stat,
          wi_cd       LIKE swwwihead-wi_cd,
          wi_ct       LIKE swwwihead-wi_ct,
          "objtype   LIKE sww_contob-objtype,
          "objkey    LIKE sww_contob-objkey,
        END OF ty_nonpo.


DATA:   gs_workflow TYPE ty_nonpo,
        gt_workflow LIKE TABLE OF gs_workflow,
        "ls_nonpo   TYPE ty_nonpo,
        "lt_nonpo   LIKE TABLE OF ls_nonpo,
        "ls_sropo   TYPE ty_sropo,
        "lt_sropo   LIKE TABLE OF ls_sropo,

        ls_vbkpf    TYPE vbkpf,
        ls_vbsegk   TYPE vbsegk,
        ls_rbkp     TYPE rbkp,

        gv_return   TYPE integer,

        gv_lines    TYPE integer,
        gv_percent  TYPE integer,
        gv_mod      TYPE integer,
        gv_curper   TYPE integer.



SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_wfid    FOR swwwihead-wi_id,
                s_wfdate  FOR swwwihead-wi_cd,
*                s_agtype  FOR swwwihead-wi_rh_task.
                s_lifnr   FOR vbsegk-lifnr,
                s_bukrs   FOR vbsegk-bukrs.
SELECTION-SCREEN END OF BLOCK a1.
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
*PARAMETERS  : r_all  RADIOBUTTON GROUP r1 DEFAULT 'X',
*              r_post RADIOBUTTON GROUP r1,
*              r_park RADIOBUTTON GROUP r1.
*SELECTION-SCREEN END OF BLOCK b1.
***********************************************************************
*                      START-OF-SELECTION                             *
***********************************************************************
START-OF-SELECTION.

  PERFORM get_db_data.
  PERFORM display_alv.
***********************************************************************
*                      Get data from DB                               *
***********************************************************************
FORM get_db_data.

  DATA: ls_swwwihead  TYPE swwwihead,
        ls_lastitem   TYPE swwwihead,
        lt_swwwihead  LIKE TABLE OF ls_swwwihead,
        ls_container  TYPE swr_cont,
        lt_container  LIKE TABLE OF ls_container,
        ls_recipient  TYPE swragent,
        lt_recipient  LIKE TABLE OF ls_recipient.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 1
      text       = 'Looking for AP Workflow'.

  SELECT *
    FROM swwwihead
    INTO CORRESPONDING FIELDS OF TABLE lt_swwwihead
    WHERE wi_stat <> 'CANCELLED'
      AND wi_stat <> 'COMPLETED'
      AND wi_id IN s_wfid
      AND wi_cd IN s_wfdate
      AND ( wi_rh_task = 'WS02000002'
        OR  wi_rh_task = 'WS02000003' ).

  DESCRIBE TABLE lt_swwwihead LINES gv_lines.

  gv_percent = gv_lines DIV 20.

  IF gv_percent = 0.
    gv_percent = 1.
  ENDIF.

  LOOP AT lt_swwwihead INTO ls_swwwihead.
    gv_mod = sy-tabix MOD gv_percent.

    IF gv_mod = 0.
      gv_curper = sy-tabix DIV gv_percent.
      gv_curper = gv_curper * 5.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = gv_curper
          text       = 'Getting Workflow Detail'.
    ENDIF.
    IF ls_swwwihead-wi_rh_task = 'WS02000002'.
      "Non PO
      CLEAR gs_workflow.
      gs_workflow-wi_id  = ls_swwwihead-wi_id.
      gs_workflow-startd = ls_swwwihead-wi_cd.
      gs_workflow-startt = ls_swwwihead-wi_ct.
      CLEAR lt_container.
      CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
        EXPORTING
          workitem_id      = ls_swwwihead-wi_id
        IMPORTING
          return_code      = gv_return
        TABLES
          simple_container = lt_container.
      IF gv_return = 0.
        LOOP AT lt_container INTO ls_container.
          CASE ls_container-element.
            WHEN '_WF_INITIATOR'.
              gs_workflow-wf_init = ls_container-value+2.
            WHEN '_WF_VERSION'.
              gs_workflow-wf_ver = ls_container-value.
            WHEN 'AMOUNT'.
              gs_workflow-amount = ls_container-value.
            WHEN 'VENDOR'.
              gs_workflow-vendor = ls_container-value.
              SHIFT gs_workflow-vendor LEFT DELETING LEADING '0'.
             WHEN 'ROUTECODEOWNER'.
              gs_workflow-rco = ls_container-value+2.
              "WHEN 'RC_OWNER_DECISION'.
              "gs_workflow-decision = ls_container-value.
            WHEN 'FINALAPPROVER'.
              gs_workflow-approver = ls_container-value+2.
            WHEN 'FIPP'.
              gs_workflow-cocode  = ls_container-value+20(4).
              gs_workflow-docnum  = ls_container-value+24(10).
              gs_workflow-year    = ls_container-value+34(4).
          ENDCASE.
        ENDLOOP.
      ENDIF.
      CLEAR ls_lastitem.
      SELECT wi_id wi_aagent wi_stat wi_cd wi_ct
        INTO CORRESPONDING FIELDS OF ls_lastitem
        FROM swwwihead
        WHERE top_wi_id = ls_swwwihead-wi_id
          AND wi_type = 'W'
          AND wi_stat <> 'COMPLETED'
          AND wi_stat <> 'CANCELLED'.
        gs_workflow-agent  = ls_lastitem-wi_aagent.
        gs_workflow-wi_cd  = ls_lastitem-wi_cd.
        gs_workflow-wi_ct  = ls_lastitem-wi_ct.
        IF ls_lastitem-wi_stat = 'STARTED'.
          gs_workflow-status = 'In Process'.
        ELSEIF ls_lastitem-wi_stat = 'READY'.
          gs_workflow-status = 'No Action'.
        ELSE.
          gs_workflow-status = ls_lastitem-wi_stat.
        ENDIF.
        IF gs_workflow-agent IS INITIAL.
          CALL FUNCTION 'SAP_WAPI_WORKITEM_RECIPIENTS'
            EXPORTING
              workitem_id = ls_lastitem-wi_id
            IMPORTING
              return_code = gv_return
            TABLES
              recipients  = lt_recipient.
          IF gv_return = 0.
            LOOP AT lt_recipient INTO ls_recipient.
              gs_workflow-agent = ls_recipient-objid.
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.
        EXIT.
      ENDSELECT.
      IF gs_workflow-agent IS INITIAL.
        gs_workflow-agtype = 'NPO WF Error'.
      ELSEIF gs_workflow-agent = gs_workflow-rco.
        gs_workflow-agtype = 'NPO RCO'.
      ELSEIF gs_workflow-agent = gs_workflow-approver.
        gs_workflow-agtype = 'NPO Approver'.
      ELSEIF gs_workflow-agent = gs_workflow-wf_init.
        gs_workflow-agtype = 'NPO Initiator'.
      ELSE.
        gs_workflow-agtype = 'NPO Other'.
      ENDIF.
      "Get parked indicator
      CLEAR ls_vbkpf.
      SELECT SINGLE bukrs belnr gjahr bstat
        INTO CORRESPONDING FIELDS OF ls_vbkpf
        FROM vbkpf
        WHERE bukrs = gs_workflow-cocode
          AND belnr = gs_workflow-docnum
          AND gjahr = gs_workflow-year.
      IF ls_vbkpf IS INITIAL.
        gs_workflow-parkind = 'X'.
      ELSE.
        "Get Vendor.
        IF gs_workflow-vendor IS INITIAL.
          SELECT SINGLE lifnr
            INTO CORRESPONDING FIELDS OF ls_vbsegk
            FROM vbsegk
            WHERE bukrs = ls_vbkpf-bukrs
              AND belnr = ls_vbkpf-belnr
              AND gjahr = ls_vbkpf-gjahr.
          gs_workflow-vendor = ls_vbsegk-lifnr.
        ENDIF.
      ENDIF.
      SHIFT gs_workflow-vendor LEFT DELETING LEADING '0'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = gs_workflow-vendor
                IMPORTING
                  OUTPUT        = gs_workflow-vendor.
    ELSEIF ls_swwwihead-wi_rh_task = 'WS02000003'.
      "SRO Workflow
      CLEAR gs_workflow.
      gs_workflow-wi_id  = ls_swwwihead-wi_id.
      gs_workflow-startd = ls_swwwihead-wi_cd.
      gs_workflow-startt = ls_swwwihead-wi_ct.
      gs_workflow-rco    = 'SRO Workflow'.
      CLEAR lt_container.
      CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
        EXPORTING
          workitem_id      = ls_swwwihead-wi_id
        IMPORTING
          return_code      = gv_return
        TABLES
          simple_container = lt_container.
      IF gv_return = 0.
        LOOP AT lt_container INTO ls_container.
          CASE ls_container-element.
            WHEN '_WF_INITIATOR'.
              gs_workflow-wf_init = ls_container-value+2.
            WHEN '_WF_VERSION'.
              gs_workflow-wf_ver = ls_container-value.
            WHEN 'AMOUNT'.
              gs_workflow-amount = ls_container-value.
            WHEN 'APPROVER'.
              gs_workflow-approver = ls_container-value+2.
            WHEN 'INVOICE'.
              gs_workflow-docnum  = ls_container-value+20(10).
              gs_workflow-year    = ls_container-value+30(4).
          ENDCASE.
        ENDLOOP.
      ENDIF.
      CLEAR ls_lastitem.
      SELECT wi_id wi_aagent wi_stat wi_cd wi_ct
        INTO CORRESPONDING FIELDS OF ls_lastitem
        FROM swwwihead
        WHERE top_wi_id = ls_swwwihead-wi_id
          AND wi_type = 'W'
          AND wi_stat <> 'COMPLETED'
          AND wi_stat <> 'CANCELLED'.
        gs_workflow-agent  = ls_lastitem-wi_aagent.
        gs_workflow-wi_cd  = ls_lastitem-wi_cd.
        gs_workflow-wi_ct  = ls_lastitem-wi_ct.
        IF ls_lastitem-wi_stat = 'STARTED'.
          gs_workflow-status = 'In Process'.
        ELSEIF ls_lastitem-wi_stat = 'READY'.
          gs_workflow-status = 'No Action'.
        ELSE.
          gs_workflow-status = ls_lastitem-wi_stat.
        ENDIF.
        IF gs_workflow-agent IS INITIAL.
          CALL FUNCTION 'SAP_WAPI_WORKITEM_RECIPIENTS'
            EXPORTING
              workitem_id = ls_lastitem-wi_id
            IMPORTING
              return_code = gv_return
            TABLES
              recipients  = lt_recipient.

          IF gv_return = 0.
            LOOP AT lt_recipient INTO ls_recipient.
              gs_workflow-agent = ls_recipient-objid.
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.
        EXIT.
      ENDSELECT.
      IF gs_workflow-agent IS INITIAL.
        gs_workflow-agtype = 'SRO WF Error'.
      ELSEIF gs_workflow-agent = gs_workflow-approver.
        gs_workflow-agtype = 'SRO Confirmer'.
      ELSEIF gs_workflow-agent = gs_workflow-wf_init.
        gs_workflow-agtype = 'SRO Initiator'.
      ELSE.
        gs_workflow-agtype = 'SRO Other'.
      ENDIF.
      CLEAR: ls_rbkp.
      SELECT SINGLE *
        "bukrs belnr gjahr lifnr rbstat
        INTO CORRESPONDING FIELDS OF ls_rbkp
        FROM rbkp
        WHERE belnr = gs_workflow-docnum
          AND gjahr = gs_workflow-year.
      IF ls_rbkp-rmwwr IS NOT INITIAL.
        gs_workflow-amount  = ls_rbkp-rmwwr.
      ENDIF.
      gs_workflow-cocode  = ls_rbkp-bukrs.
      gs_workflow-vendor = ls_rbkp-lifnr.
      SHIFT gs_workflow-vendor LEFT DELETING LEADING '0'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = gs_workflow-vendor
                IMPORTING
                  OUTPUT        = gs_workflow-vendor.
      CASE ls_rbkp-rbstat.
        WHEN 'A' OR 'B' OR 'C' OR 'E'.
          gs_workflow-parkind = ''.
        WHEN OTHERS.
          gs_workflow-parkind = 'X'.
      ENDCASE.
    ENDIF.
    APPEND gs_workflow TO gt_workflow.
  ENDLOOP.
*  IF s_agtype[] IS NOT INITIAL.
*    DELETE gt_workflow WHERE agtype NOT IN s_agtype[].
*  ENDIF.
  IF s_lifnr[] is NOT INITIAL.
     DELETE gt_workflow WHERE vendor NOT IN s_lifnr[].
  ENDIF.
  IF s_bukrs[] IS NOT INITIAL.
     DELETE gt_workflow WHERE cocode NOT IN s_bukrs[].
  ENDIF.
*  IF r_park IS NOT INITIAL.
*     DELETE gt_workflow WHERE parkind = 'X'.
*  ENDIF.
*  IF r_post IS NOT INITIAL.
*     DELETE gt_workflow WHERE parkind <> 'X'.
*  ENDIF.
ENDFORM.                    "get_db_data

***********************************************************************
*                      Display ALV Grid                               *
***********************************************************************
FORM display_alv.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat LIKE LINE OF lt_fieldcat,
        ls_layout   TYPE slis_layout_alv,
        ls_variant  LIKE disvariant,
        lt_sort     TYPE slis_t_sortinfo_alv,
        ls_sort     LIKE LINE OF lt_sort.

  "Create field catalog
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'WI_ID'.
  ls_fieldcat-seltext_m   = 'WF ID'.
  ls_fieldcat-col_pos     = 0.
  ls_fieldcat-emphasize   = 'X'.
  ls_fieldcat-key         = 'X'.
  ls_fieldcat-hotspot     = 'X'.
  "ls_fieldcat-outputlen   = 10.
  "ls_fieldcat-do_sum      = 'X'.
  "ls_fieldcat-no_zero     = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'WF_VER'.
  ls_fieldcat-seltext_m   = 'WF Ver'.
  ls_fieldcat-col_pos     = 1.
  ls_fieldcat-outputlen   = 6.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'STARTD'.
  ls_fieldcat-seltext_m   = 'WF Date'.
  ls_fieldcat-col_pos     = 2.
  ls_fieldcat-outputlen   = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'STARTT'.
  ls_fieldcat-seltext_m   = 'WF Time'.
  ls_fieldcat-col_pos     = 3.
  ls_fieldcat-outputlen   = 8.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'YEAR'.
  ls_fieldcat-seltext_m   = 'Year'.
  ls_fieldcat-col_pos     = 4.
  ls_fieldcat-outputlen   = 5.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'DOCNUM'.
  ls_fieldcat-seltext_m   = 'Parked Doc'.
  ls_fieldcat-col_pos     = 5.
  ls_fieldcat-outputlen   = 10.
  ls_fieldcat-hotspot     = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'COCODE'.
  ls_fieldcat-seltext_m   = 'CoCd'.
  ls_fieldcat-col_pos     = 6.
  ls_fieldcat-outputlen   = 4.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'VENDOR'.
  ls_fieldcat-seltext_m   = 'Vendor'.
  ls_fieldcat-col_pos     = 7.
  ls_fieldcat-outputlen   = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'AMOUNT'.
  ls_fieldcat-seltext_m   = 'Amount'.
  ls_fieldcat-col_pos     = 8.
  ls_fieldcat-outputlen   = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'PARKIND'.
  ls_fieldcat-seltext_m   = 'Post/Del'.
  ls_fieldcat-col_pos     = 9.
  ls_fieldcat-outputlen   = 8.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'WF_INIT'.
  ls_fieldcat-seltext_m   = 'WF Initiator'.
  ls_fieldcat-col_pos     = 10.
  ls_fieldcat-outputlen   = 13.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'RCO'.
  ls_fieldcat-seltext_m   = 'RCO'.
  ls_fieldcat-col_pos     = 11.
  ls_fieldcat-outputlen   = 13.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'APPROVER'.
  ls_fieldcat-seltext_m   = 'Approver'.
  ls_fieldcat-col_pos     = 12.
  ls_fieldcat-outputlen   = 13.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'AGENT'.
  ls_fieldcat-seltext_m   = 'Actual Agent'.
  ls_fieldcat-col_pos     = 13.
  ls_fieldcat-outputlen   = 15.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'AGTYPE'.
  ls_fieldcat-seltext_m   = 'Agent Type'.
  ls_fieldcat-col_pos     = 14.
  ls_fieldcat-outputlen   = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'STATUS'.
  ls_fieldcat-seltext_m   = 'Item Status'.
  ls_fieldcat-col_pos     = 15.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'WI_CD'.
  ls_fieldcat-seltext_m   = 'Last WI Date'.
  ls_fieldcat-col_pos     = 16.
  ls_fieldcat-outputlen   = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'WI_CT'.
  ls_fieldcat-seltext_m   = 'Last WI Time'.
  ls_fieldcat-col_pos     = 17.
  ls_fieldcat-outputlen   = 8.
  APPEND ls_fieldcat TO lt_fieldcat.

  "Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat             = lt_fieldcat
      is_layout               = ls_layout
      i_callback_program      = sy-repid
          "i_callback_top_of_page  = 'ALV_TOP_OF_PAGE'
      i_callback_user_command = 'ALV_USER_COMMAND'
      i_save                  = 'A'
      is_variant              = ls_variant
      it_sort                 = lt_sort
*     it_events               = i_event
    TABLES
      t_outtab                = gt_workflow
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

ENDFORM.                    "display_alv


*************************************************************
*                        TOP OF PAGE                        *
*************************************************************
FORM alv_user_command USING lv_ucomm     LIKE sy-ucomm
                            ls_selfield TYPE slis_selfield.

  IF ls_selfield-tabindex LE 0 OR ls_selfield-sumindex > 0.
    EXIT.
  ENDIF.
  CLEAR gs_workflow.
  READ TABLE gt_workflow INTO gs_workflow INDEX ls_selfield-tabindex.

  CASE lv_ucomm.
    WHEN '&IC1'.
      CASE ls_selfield-fieldname.
        WHEN 'WI_ID'.
          CALL FUNCTION 'SWL_WI_DISPATCH'
            EXPORTING
              wi_id         = gs_workflow-wi_id
              wi_first_time = 'X'
              wi_function   = 'WIFI'.
          .
          IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.


        WHEN 'DOCNUM'.
          IF gs_workflow-rco = 'SRO Workflow'.
            SET PARAMETER ID 'RBN' FIELD gs_workflow-docnum.
            SET PARAMETER ID 'GJR' FIELD gs_workflow-year.
            CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
          ELSE.
            SET PARAMETER ID 'BUK' FIELD gs_workflow-cocode.
            SET PARAMETER ID 'BLP' FIELD gs_workflow-docnum.
            SET PARAMETER ID 'GJR' FIELD gs_workflow-year.
            CALL TRANSACTION 'FBV3' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
  ENDCASE.
ENDFORM.                    "alv_user_command


*************************************************************
*                        TOP OF PAGE                        *
*************************************************************
FORM alv_top_of_page.
  DATA: ls_line TYPE slis_listheader.
  DATA: lt_top_of_page TYPE slis_t_listheader.

*1- Heading Line: Type H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = sy-title.             "sy-title.
  APPEND ls_line TO lt_top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.
ENDFORM.                    "ALV_TOP_OF_PAGE
