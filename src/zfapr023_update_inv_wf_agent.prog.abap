*&---------------------------------------------------------------------*
*& Report  ZFAPR023_UPDATE_INV_WF_AGENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------
* Version: MZH01
* Date : Feb 14 2023
* Developer : Mohammed Hossain(MZH01)
* Business Contact : Siva
* Business Technical Developer : Zakir
* Description : DOA Migration from US to UG.
*"----------------------------------------------------------------------

REPORT  zfapr023_update_inv_wf_agent MESSAGE-ID zfi01.

TABLES: swwwihead, bkpf.

TYPES: BEGIN OF ty_out,
        wiid LIKE swwwihead-wi_id,
        topwi LIKE swwwihead-wi_id,
        bukrs LIKE bkpf-bukrs,
        belnr LIKE bkpf-belnr,
        gjahr LIKE bkpf-gjahr,
        step TYPE char10,
        agent TYPE char12,
       END OF ty_out.

DATA: t_out TYPE TABLE OF ty_out WITH HEADER LINE,
      wa_out TYPE ty_out.

DATA: BEGIN OF t_wi OCCURS 0,
      wi_id LIKE swwwihead-wi_id,
      wi_type LIKE swwwihead-wi_type,
      wi_stat LIKE swwwihead-wi_stat,
      wi_rh_task LIKE swwwihead-wi_rh_task ,
      top_wi_id LIKE swwwihead-top_wi_id,
      END OF t_wi.

DATA: w_handle TYPE REF TO if_swf_cnt_container,
      t_cont TYPE TABLE OF swcont WITH HEADER LINE.

DATA: BEGIN OF t_bkpf OCCURS 0,
      ausbk LIKE vbkpf-ausbk,
      belnr LIKE vbkpf-belnr,
      gjahr LIKE vbkpf-gjahr,
      END OF t_bkpf.

DATA: BEGIN OF t_rbkp OCCURS 0,
      belnr LIKE rbkp-belnr,
      gjahr LIKE rbkp-gjahr,
      bukrs LIKE rbkp-bukrs,
      END OF t_rbkp.

DATA: w_src LIKE usr01-bname,
      w_rco LIKE usr01-bname.

* Data for ALV
TYPE-POOLS: slis.
DATA: t_fc   TYPE slis_t_fieldcat_alv WITH HEADER LINE,
     t_sort TYPE slis_t_sortinfo_alv.
DATA: z_variant LIKE disvariant.
DATA: z_varid LIKE disvariant-variant VALUE '/DEFAULT'.
DATA: g_variant_flag.
DATA: g_selmod.

DATA: z_layout TYPE slis_layout_alv.
DATA: t_header   TYPE slis_t_listheader.
DATA: z_text(60).
DATA: gs_line TYPE slis_listheader.

SELECTION-SCREEN BEGIN OF BLOCK rma WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_wiid FOR swwwihead-wi_id,
                s_belnr FOR bkpf-belnr,
                s_gjahr FOR bkpf-gjahr,
                s_bukrs FOR bkpf-bukrs.
SELECTION-SCREEN END OF BLOCK rma.
SELECTION-SCREEN BEGIN OF BLOCK abc WITH FRAME TITLE text-003.
PARAMETERS:     p_ap LIKE usr01-bname,
                p_appr LIKE usr01-bname.
SELECTION-SCREEN END OF BLOCK abc.
SELECTION-SCREEN BEGIN OF BLOCK xyz WITH FRAME TITLE text-002.
PARAMETERS:   p_npo TYPE char1 RADIOBUTTON GROUP ab DEFAULT 'X' ,
              p_po TYPE char1 RADIOBUTTON GROUP ab.
SELECTION-SCREEN END OF BLOCK xyz.

AT SELECTION-SCREEN.
  IF s_wiid[] IS INITIAL AND s_belnr[] IS INITIAL AND
     s_gjahr[] IS INITIAL AND s_bukrs[] IS INITIAL.
    MESSAGE e000 WITH 'Specify At Least One Selection Criteria'.
  ENDIF.

  IF NOT s_wiid[] IS INITIAL.
    IF NOT s_belnr[] IS INITIAL OR NOT s_gjahr[] IS INITIAL OR NOT s_bukrs[] IS INITIAL.
      MESSAGE e000 WITH 'Specify Only Workitem or Invoice Details'.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  PERFORM select_data.

END-OF-SELECTION.
  PERFORM prepare_output.
  PERFORM output_alv_report.
*&---------------------------------------------------------------------*
*&      Form  PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_output .

  DATA: lv_ind TYPE i.
  DATA: t_nsrc TYPE TABLE OF swcont WITH HEADER LINE,
        t_npo TYPE TABLE OF swr_cont WITH HEADER LINE.

  LOOP AT t_out.
    REFRESH: t_nsrc, t_npo.
    lv_ind = sy-tabix.
    CLEAR: w_src, w_rco.
    CASE t_out-step.
      WHEN 'SRC'.
        PERFORM get_new_src USING t_out-belnr t_out-gjahr CHANGING w_src.
        t_out-agent = w_src.
        t_nsrc-element = 'SRC'.
        t_nsrc-elemlength = '014'.
        t_nsrc-type = 'C'.
        CONCATENATE 'US' w_src INTO t_nsrc-value.
        CONDENSE t_nsrc-value NO-GAPS.
        APPEND t_nsrc.
        CALL FUNCTION 'SWW_WI_CONTAINER_MODIFY'
          EXPORTING
            wi_id        = t_out-topwi
          TABLES
            wi_container = t_nsrc.

        CALL FUNCTION 'SAP_WAPI_ADM_WORKITEM_REDORULE'
          EXPORTING
            workitem_id = t_out-wiid.

      WHEN 'RCO'.
        PERFORM get_new_rco USING t_out-bukrs t_out-belnr t_out-gjahr CHANGING w_rco.
        t_out-agent = w_rco.
        IF NOT w_rco IS INITIAL.
          t_npo-element = 'ROUTECODEOWNER'.
*          CONCATENATE 'US' W_RCO INTO T_NPO-VALUE.
*          CONDENSE T_NPO-VALUE NO-GAPS.
          t_npo-value = w_rco.
          APPEND t_npo.
          CALL FUNCTION 'SAP_WAPI_WRITE_CONTAINER'
            EXPORTING
              workitem_id      = t_out-topwi
            TABLES
              simple_container = t_npo.

          CALL FUNCTION 'SAP_WAPI_ADM_WORKITEM_REDORULE'
            EXPORTING
              workitem_id = t_out-wiid.
        ENDIF.
      WHEN 'AP'.
        t_out-agent = p_ap.
        IF NOT p_ap IS INITIAL.
          t_npo-element = '_WF_INITIATOR'.
          CONCATENATE 'US' p_ap INTO t_npo-value.
          CONDENSE t_npo-value NO-GAPS.
          APPEND t_npo.
          CALL FUNCTION 'SAP_WAPI_WRITE_CONTAINER'
            EXPORTING
              workitem_id      = t_out-topwi
            TABLES
              simple_container = t_npo.

          CALL FUNCTION 'SAP_WAPI_ADM_WORKITEM_REDORULE'
            EXPORTING
              workitem_id = t_out-wiid.
        ELSE.
          t_out-agent = 'MISSING'.
        ENDIF.
      WHEN 'APPROVAL'.
        t_out-agent = p_appr.
        IF NOT p_appr IS INITIAL.
          t_npo-element = 'FINALAPPROVER'.
          CONCATENATE 'US' p_appr INTO t_npo-value.
          CONDENSE t_npo-value NO-GAPS.
          APPEND t_npo.
          CALL FUNCTION 'SAP_WAPI_WRITE_CONTAINER'
            EXPORTING
              workitem_id      = t_out-topwi
            TABLES
              simple_container = t_npo.

          CALL FUNCTION 'SAP_WAPI_ADM_WORKITEM_REDORULE'
            EXPORTING
              workitem_id = t_out-wiid.
        ELSE.
          t_out-agent = 'MISSING'.
        ENDIF.
      WHEN OTHERS.
        DELETE t_out INDEX lv_ind.
        CONTINUE.
    ENDCASE.
    MODIFY t_out INDEX lv_ind TRANSPORTING agent.
  ENDLOOP.

ENDFORM.                    " PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM output_alv_report .
  PERFORM build_fc.

  CLEAR z_layout.
  z_layout-info_fieldname    = 'LINECOLOR'.
  z_layout-colwidth_optimize = 'X'.
  z_layout-detail_popup = 'X'.
  z_layout-numc_sum = 'X'.
  z_layout-get_selinfos = 'X'.
  z_layout-confirmation_prompt = 'X'.
  z_layout-box_rollname = 'S'.

* variant settings
  z_variant-report = sy-repid.
  z_variant-variant = z_varid.

* call ALV function to output report

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
*     I_CALLBACK_TOP_OF_PAGE  = 'TOP_OF_PAGE'
*     I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      is_layout               = z_layout
      it_fieldcat             = t_fc[]
      i_default               = 'X'
      i_save                  = 'X'
      is_variant              = z_variant
    TABLES
      t_outtab                = t_out.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " OUTPUT_ALV_REPORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fc .
  DATA: w_ind TYPE i.

  w_ind = 0.

  t_fc-fieldname   = 'WIID'.
  t_fc-seltext_m   = 'Workitem ID'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'TOPWI'.
  t_fc-seltext_m   = 'Top Workitem ID'.
  t_fc-no_out   = 'X'.
*  t_fc-COL_POS     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-fieldname   = 'BUKRS'.
  t_fc-seltext_m   = 'Company Code'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'BELNR'.
  t_fc-seltext_m   = 'Invoice Number'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'GJAHR'.
  t_fc-seltext_m   = 'Year'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'STEP'.
  t_fc-seltext_m   = 'Approval Stage'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-fieldname   = 'AGENT'.
  t_fc-seltext_m   = 'New Agent'.
  t_fc-col_pos     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.
ENDFORM.                    " BUILD_FC
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data .

  DATA: lv_npo TYPE char1,
        lv_po TYPE char1,
        lv_objtyp LIKE swotobjid-objtype,
        lv_key LIKE swotobjid-objkey,
        lt_list TYPE TABLE OF swr_wihdr WITH HEADER LINE.

  IF NOT s_wiid[] IS INITIAL.
    SELECT wi_id wi_type wi_stat wi_rh_task top_wi_id FROM swwwihead INTO TABLE t_wi
      WHERE wi_id IN s_wiid.

    LOOP AT t_wi.
      CLEAR: wa_out, lv_npo, lv_po.
      REFRESH: t_cont.
      wa_out-wiid = t_wi-wi_id.
      wa_out-topwi = t_wi-top_wi_id.
      CASE t_wi-wi_rh_task.
        WHEN 'TS02000010'.  "SRC
          wa_out-step = 'SRC'.
          lv_po = 'X'.
        WHEN 'TS02000013'.  "RCO
          lv_npo = 'X'.
      ENDCASE.

      CALL FUNCTION 'SWW_WI_CONTAINER_READ'
        EXPORTING
          wi_id                    = t_wi-top_wi_id
        IMPORTING
          wi_container_handle      = w_handle
        TABLES
          wi_container             = t_cont
        EXCEPTIONS
          container_does_not_exist = 1
          read_failed              = 2
          OTHERS                   = 3.

      IF lv_po = 'X'.
        READ TABLE t_cont WITH KEY element = 'INVOICE'.
        IF sy-subrc = 0.
*          WA_OUT-BUKRS = T_CONT-VALUE+20(4).
          wa_out-belnr = t_cont-value+20(10).
          wa_out-gjahr = t_cont-value+30(4).
        ENDIF.
        APPEND wa_out TO t_out.
      ELSEIF lv_npo = 'X'.
        READ TABLE t_cont WITH KEY element = 'FIPP'.
        IF sy-subrc = 0.
          wa_out-bukrs = t_cont-value+20(4).
          wa_out-belnr = t_cont-value+24(10).
          wa_out-gjahr = t_cont-value+34(4).
        ENDIF.

        REFRESH: t_cont.
        CALL FUNCTION 'SWW_WI_CONTAINER_READ'
          EXPORTING
            wi_id                    = t_wi-wi_id
          IMPORTING
            wi_container_handle      = w_handle
          TABLES
            wi_container             = t_cont
          EXCEPTIONS
            container_does_not_exist = 1
            read_failed              = 2
            OTHERS                   = 3.
        READ TABLE t_cont WITH KEY element = 'DECISIONTYPE'.
        IF sy-subrc = 0.
          CASE t_cont-value.
            WHEN 'INIT'.
              wa_out-step = 'AP'.
            WHEN 'RCODE'.
              wa_out-step = 'RCO'.
            WHEN 'POST'.
              wa_out-step = 'APPROVAL'.
          ENDCASE.
        ENDIF.

        APPEND wa_out TO t_out.
      ENDIF.

    ENDLOOP.
  ELSE.
    IF p_npo = 'X'.
      SELECT ausbk belnr gjahr FROM vbkpf INTO TABLE t_bkpf
        WHERE ausbk IN s_bukrs AND
              belnr IN s_belnr AND
              gjahr IN s_gjahr.

      LOOP AT t_bkpf.
        CLEAR wa_out.
        REFRESH lt_list.

        wa_out-bukrs = t_bkpf-ausbk.
        wa_out-belnr = t_bkpf-belnr.
        wa_out-gjahr = t_bkpf-gjahr.
        lv_objtyp = 'FIPP'.
*        CONCATENATE T_BKPF-AUSBK T_BKPF-BELNR T_BKPF-GJAHR INTO LV_KEY.
*        CONDENSE LV_KEY NO-GAPS.
        lv_key+0(4) = t_bkpf-ausbk.
        lv_key+4(10) = t_bkpf-belnr.
        lv_key+14(4) = t_bkpf-gjahr.
        CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
          EXPORTING
            objtype         = lv_objtyp
            objkey          = lv_key
            top_level_items = ' '
          TABLES
            worklist        = lt_list.

        READ TABLE lt_list WITH KEY wi_type = 'F' wi_rh_task = 'WS02000002'.
        IF sy-subrc = 0.
          wa_out-topwi = lt_list-wi_id.
          SELECT SINGLE wi_id FROM swwwihead INTO wa_out-wiid
            WHERE wi_type = 'W' AND
                  ( wi_stat = 'READY' OR wi_stat = 'STARTED' ) AND
                  wi_rh_task = 'TS02000013' AND
                  top_wi_id = lt_list-wi_id.
          IF sy-subrc = 0.
            REFRESH: t_cont.
            CALL FUNCTION 'SWW_WI_CONTAINER_READ'
              EXPORTING
                wi_id                    = wa_out-wiid
              IMPORTING
                wi_container_handle      = w_handle
              TABLES
                wi_container             = t_cont
              EXCEPTIONS
                container_does_not_exist = 1
                read_failed              = 2
                OTHERS                   = 3.
            READ TABLE t_cont WITH KEY element = 'DECISIONTYPE'.
            IF sy-subrc = 0.
              CASE t_cont-value.
                WHEN 'INIT'.
                  wa_out-step = 'AP'.
                WHEN 'RCODE'.
                  wa_out-step = 'RCO'.
                WHEN 'POST'.
                  wa_out-step = 'APPROVAL'.
              ENDCASE.
            ENDIF.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.

        APPEND wa_out TO t_out.
      ENDLOOP.

    ELSE.
      SELECT belnr gjahr bukrs FROM rbkp INTO TABLE t_rbkp
        WHERE belnr IN s_belnr AND
              gjahr IN s_gjahr AND
              bukrs IN s_bukrs.

      LOOP AT t_rbkp.
        CLEAR wa_out.
        REFRESH lt_list.

        wa_out-bukrs = t_rbkp-bukrs.
        wa_out-belnr = t_rbkp-belnr.
        wa_out-gjahr = t_rbkp-gjahr.
        lv_objtyp = 'BUS2081'.
        CONCATENATE t_rbkp-belnr t_rbkp-gjahr INTO lv_key.
        CONDENSE lv_key NO-GAPS.
        CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
          EXPORTING
            objtype         = lv_objtyp
            objkey          = lv_key
            top_level_items = ' '
          TABLES
            worklist        = lt_list.

        READ TABLE lt_list WITH KEY wi_type = 'F' wi_rh_task = 'WS02000003'.
        IF sy-subrc = 0.
          wa_out-topwi = lt_list-wi_id.
          LOOP AT lt_list WHERE wi_type = 'W' AND wi_rh_task = 'TS02000010' AND
            ( wi_stat = 'READY' OR wi_stat = 'STARTED' ).
            wa_out-wiid = lt_list-wi_id.
            wa_out-step = 'SRC'.
            EXIT.
          ENDLOOP.
        ELSE.
          CONTINUE.
        ENDIF.

        APPEND wa_out TO t_out.
      ENDLOOP.
    ENDIF.

  ENDIF.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_NEW_SRC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_OUT_BELNR  text
*      -->P_T_OUT_GJAHR  text
*      <--P_W_SRC  text
*----------------------------------------------------------------------*
FORM get_new_src  USING    p_belnr
                           p_gjahr
                  CHANGING p_src.

  DATA: lv_ebeln TYPE rseg-ebeln.

  CLEAR p_src.
  SELECT ebeln FROM rseg INTO lv_ebeln
    WHERE belnr = p_belnr AND
          gjahr = p_gjahr AND
          ebeln NE ' '.
    SELECT SINGLE zzariba_approver FROM ekko INTO p_src
      WHERE ebeln = lv_ebeln.
    IF NOT p_src IS INITIAL.
      EXIT.
    ENDIF.
  ENDSELECT.

ENDFORM.                    " GET_NEW_SRC
*&---------------------------------------------------------------------*
*&      Form  GET_NEW_RCO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_OUT_BELNR  text
*      -->P_T_OUT_GJAHR  text
*      <--P_W_RCO  text
*----------------------------------------------------------------------*
FORM get_new_rco  USING    p_bukrs
                           p_belnr
                           p_gjahr
                  CHANGING p_rco.

  DATA: w_xref TYPE xref3,
        w_user TYPE username,
        ltp_rc_owner   TYPE wfsyst-initiator,
        ltp_log_dest   TYPE tb_rfcdest.

  CLEAR p_rco.
  SELECT SINGLE xref3 FROM vbsegk INTO w_xref
    WHERE ausbk = p_bukrs
      AND belnr = p_belnr
      AND gjahr = p_gjahr
      AND xref3 NE ' '.
  IF sy-subrc = 0.
**Get Logical Destination
*    Begin of comment MZH01
*    CALL FUNCTION 'ZFI_GET_LOGICAL_DEST'
*      EXPORTING
*        imp_paramtype = 'ECCUS'
*      IMPORTING
*        exp_rfcdest   = ltp_log_dest.
*End of comment MZH01
    IF w_xref IS NOT INITIAL.
*      CALL FUNCTION 'ZFI_GET_ROUTCODE' DESTINATION ltp_log_dest  "MZH01
      CALL FUNCTION 'ZFI_GET_ROUTCODE' "MZH01
            EXPORTING
              imp_route_code = w_xref
            IMPORTING
              exp_rc_owner = ltp_rc_owner
            EXCEPTIONS
              nobody_found.
*     Return owner as agent of this rule
      MOVE ltp_rc_owner TO p_rco.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_NEW_RCO
