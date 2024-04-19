*&---------------------------------------------------------------------*
*& Report  Z_EXP_DETAILS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_EXP_DETAILS MESSAGE-ID ZFI_WORKFLOW.

TABLES: SWWWIHEAD, PTRV_HEAD, PTRV_SCOS, PA0001.

*---- Parameters ------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK RMA WITH FRAME title text-001.
select-OPTIONS: S_PERNR for PA0001-PERNR,
                S_NAME for PA0001-SNAME,
                S_COST for PTRV_SCOS-COSTCENTER,
                S_WBS  for PTRV_SCOS-WBS_ELEMT,
                S_ORDER  for PTRV_SCOS-INTERNAL_ORDER,
                S_REINR for PTRV_HEAD-REINR.

PARAMETER:  P_DATV1 like PTRV_HEAD-DATV1 OBLIGATORY,
            P_DATB1 like PTRV_Head-DATB1 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK RMA.


*---- ORIGINAL ------------------------------------------------------*
RANGES: R_TEXT for SWWWIHEAD-WI_TEXT,
        S_WBS1 FOR PTRV_SCOS-WBS_ELEMT.
DATA: W_WILD TYPE C LENGTH 1 VALUE '*',
      W_IND TYPE I,
      W_TID TYPE TRVTEXTID.


DATA: BEGIN OF T_HEADER OCCURS 0,
      PERNR like PA0001-PERNR,
      NAME  like PA0001-SNAME,
      REINR like PTRV_HEAD-REINR,
      DATV1 TYPE CHAR10,
      DATB1 TYPE CHAR10,
      REASON like PTRV_HEAD-KUNDE,
      Amount like PTRV_SHDR-TRIP_TOTAL,
      REM_AMT like PTRV_SHDR-SUM_REIMBU,
      CCODE  like PTRV_SCOS-COMP_CODE,
      CCENTER like PTRV_SCOS-COSTCENTER,
      WBS    like PTRV_SCOS-WBS_ELEMT,
      Order  like PTRV_SCOS-INTERNAL_ORDER,
      country like BAPITRADDI-COUNTRY,
      region like BAPITRADDI-REGION,
      status like PTRV_PERIO-ANTRG,
      settle like PTRV_PERIO-ABREC,
      FI_TR  like PTRV_PERIO-UEBRF,
      Approver like swwwihead-WI_AAGENT,
      App_dt  TYPE CHAR10,
     END OF T_HEADER.

DATA: BEGIN OF T_DETAIL OCCURS 0,
      PERNR like PA0001-PERNR,
      REINR like PTRV_HEAD-REINR,
      MILES  like BAPITRVMIL-M_total,
      Loc_From like BAPITRVMIL-LOC_FROM,
      Loc_to like BAPITRVMIL-LOC_to,
      CCODE  like PTRV_SCOS-COMP_CODE,
      CCENTER like PTRV_SCOS-COSTCENTER,
      WBS    like PTRV_SCOS-WBS_ELEMT,
      Order  like PTRV_SCOS-INTERNAL_ORDER,
      Purpose like BAPITRADDI-BUS_PURPO,
      partner like BAPITRADDI-BUS_REASON,
      TYPE    like BAPITRVREO-EXP_TYPE,
      exp_text like BAPITRVREO-EXP_TEXT,
      exp_amt like PTRV_SHDR-TRIP_TOTAL,
      exp_curr like BAPITRVREO-REC_CURR,
      exp_dt  TYPE CHAR10,
      region like BAPITRADDI-REGION,
      Comm type TEXTR,
      merchant like BAPITRADDI-C_TXT,
     END OF T_DETAIL.

DATA: BEGIN OF T_PHEAD OCCURS 0,
      PERNR TYPE PERNR_D,
      REINR TYPE REINR,
      HDVRS TYPE PTRV_HDVRS,
      KUNDE TYPE RKUNDE,
      DATV1 TYPE REBED,
      DATB1 TYPE REEND,
      END OF T_PHEAD.

DATA: BEGIN OF T_SCOS OCCURS 0,
      PERNR TYPE PERNR_D,
      REINR TYPE REINR,
      PERIO TYPE PTRV_PEROD,
      COSTSEQNO TYPE PTRV_COSTSEQNO,
      COSTCR TYPE KOSTL,
      ORDER TYPE AUFNR,
      WBS TYPE PS_POSID,
      END OF T_SCOS.

DATA: BEGIN OF T_SHDR OCCURS 0,
      PERNR TYPE PERNR_D,
      REINR TYPE REINR,
      PERIO TYPE PTRV_PEROD,
      REM_AMT TYPE PTRV_SUM_REIMBU,
      TOTAL TYPE PTRV_TRIP_TOTAL,
      END OF T_SHDR.

DATA: BEGIN OF T_0001 OCCURS 0,
      PERNR TYPE PERNR_D,
      NAME TYPE SMNAM,
      END OF T_0001.

DATA: W_OBJKEY TYPE SIBFBORIID,
      W_PID TYPE SWW_WIID.

DATA: BEGIN OF T_WI OCCURS 0,
      ID type sww_wiid,
      TEXT type sww_witext,
      STAT type SWW_WISTAT,
      AED type SWW_AED,
      AGENT type sww_aagent,
      PERNR TYPE PERNR_D,
      REINR TYPE REINR,
      END OF T_WI.

DATA: FRAMEDATA LIKE BAPITRMAIN,
      STATUS LIKE BAPITRSTAO.

DATA: RECEIPTS TYPE TABLE OF BAPITRVREO WITH HEADER LINE,
      ADDINFO TYPE TABLE OF BAPITRADDI WITH HEADER LINE,
      T_TEXT TYPE TABLE OF BAPITRTEXT WITH HEADER LINE,
      MILEAGE TYPE TABLE OF BAPITRVMIL WITH HEADER LINE,
      COST_TRIP TYPE TABLE OF BAPITRVCOT WITH HEADER LINE,
      COST_RECE TYPE TABLE OF BAPITRVCOR WITH HEADER LINE,
      T_AMOUNTS TYPE TABLE OF BAPITRVSUM WITH HEADER LINE.

* Data for ALV
type-pools: SLIS.
DATA: t_fc   TYPE slis_t_fieldcat_alv with HEADER LINE,
     t_sort TYPE slis_t_sortinfo_alv.
DATA: z_variant LIKE disvariant.
DATA: z_varid LIKE disvariant-variant VALUE '/ZCUSTOM'.
DATA: z_layout TYPE slis_layout_alv.
DATA: alv_keyinfo      TYPE slis_keyinfo_alv.


At selection-screen.


INITIALIZATION.


START-OF-SELECTION.

  SELECT PERNR REINR HDVRS KUNDE DATV1 DATB1 FROM PTRV_HEAD INTO TABLE T_PHEAD
    WHERE PERNR IN S_PERNR AND
          REINR IN S_REINR AND
          DATV1 GE P_DATV1 AND
          DATB1 LE P_DATB1.

  IF SY-SUBRC = 0.
    S_WBS1[] = S_WBS[].
    LOOP AT S_WBS.
      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
        EXPORTING
          INPUT     = S_WBS-LOW
        IMPORTING
          OUTPUT    = S_WBS-LOW
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      IF NOT S_WBS-HIGH IS INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
          EXPORTING
            INPUT     = S_WBS-HIGH
          IMPORTING
            OUTPUT    = S_WBS-HIGH
          EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
      ENDIF.
      MODIFY S_WBS INDEX SY-TABIX.
    ENDLOOP.

    SELECT PERNR REINR PERIO COSTSEQNO COSTCENTER INTERNAL_ORDER WBS_ELEMT FROM PTRV_SCOS INTO TABLE T_SCOS
      FOR ALL ENTRIES IN T_PHEAD
      WHERE PERNR = T_PHEAD-PERNR AND
            REINR = T_PHEAD-REINR AND
            COSTCENTER IN S_COST AND
            INTERNAL_ORDER IN S_ORDER AND
            WBS_ELEMT IN S_WBS.
    IF SY-SUBRC = 0.
      SELECT PERNR SNAME FROM PA0001 INTO TABLE T_0001
        FOR ALL ENTRIES IN T_SCOS
        WHERE PERNR = T_SCOS-PERNR AND
              SNAME IN S_NAME.

      SELECT PERNR REINR PERIO SUM_REIMBU TRIP_TOTAL FROM PTRV_SHDR INTO TABLE T_SHDR
        FOR ALL ENTRIES IN T_SCOS
        WHERE PERNR = T_SCOS-PERNR AND
              REINR = T_SCOS-REINR AND
              PERIO = T_SCOS-PERIO.

      clear R_TEXT.
      refresh r_text.

      LOOP AT T_SCOS.
        CONCATENATE T_SCOS-PERNR W_WILD T_SCOS-REINR W_WILD INTO R_text-LOW.
        R_text-OPTION = 'CP'.
        R_text-SIGN = 'I'.
        APPEND R_text.
      ENDLOOP.

      select WI_ID WI_TEXT WI_STAT WI_AED WI_AAGENT from SWWWIHEAD
        into TABLE T_WI
        where WI_TYPE = 'W' AND
              WI_TEXT in R_TEXT AND
              WI_STAT EQ 'COMPLETED' AND
              WI_RH_TASK = 'TS02000044'.

      LOOP at T_WI.
        T_WI-PERNR = T_WI-TEXT+0(8).
        T_WI-REINR = T_WI-TEXT+11(10).
        w_ind = sy-tabix.

        modify T_WI INDEX w_ind TRANSPORTING PERNR REINR.
      ENDLOOP.

      SORT T_WI BY AED DESCENDING.
    ENDIF.
  ENDIF.

  SORT T_SCOS BY PERNR REINR.

  DELETE ADJACENT DUPLICATES FROM T_SCOS COMPARING PERNR REINR.

END-OF-SELECTION.

  LOOP AT T_SCOS.
    READ TABLE T_0001 WITH KEY PERNR = T_SCOS-PERNR.
    CHECK SY-SUBRC = 0.

    CALL FUNCTION 'BAPI_TRIP_GET_DETAILS'
      EXPORTING
        EMPLOYEENUMBER = T_SCOS-PERNR
        TRIPNUMBER     = T_SCOS-REINR
      IMPORTING
        FRAMEDATA      = FRAMEDATA
        STATUS         = STATUS
      TABLES
        RECEIPTS       = RECEIPTS
        ADDINFO        = ADDINFO
        TEXT           = T_TEXT
        MILEAGE        = MILEAGE
        COSTDIST_TRIP  = COST_TRIP
        COSTDIST_RECE  = COST_RECE
        AMOUNTS        = T_AMOUNTS.

    CLEAR: T_HEADER, T_DETAIL, T_PHEAD, T_SHDR, COST_TRIP, COST_RECE.
    T_HEADER-PERNR = T_SCOS-PERNR.
    T_HEADER-REINR = T_SCOS-REINR.
    T_HEADER-NAME = T_0001-NAME.

    READ TABLE T_PHEAD WITH KEY PERNR = T_SCOS-PERNR REINR = T_SCOS-REINR.
    WRITE T_PHEAD-DATV1 TO T_HEADER-DATV1.
    WRITE T_PHEAD-DATB1 TO T_HEADER-DATB1.
    T_HEADER-REASON = T_PHEAD-KUNDE.

    READ TABLE T_SHDR WITH KEY  PERNR = T_SCOS-PERNR REINR = T_SCOS-REINR.
    T_HEADER-AMOUNT = T_SHDR-TOTAL.
    T_HEADER-REM_AMT = T_SHDR-REM_AMT.

*    READ TABLE COST_TRIP INDEX 1.
    LOOP AT COST_TRIP WHERE COSTCENTER IN S_COST AND WBS_ELEMT IN S_WBS AND ORDER IN S_ORDER.

    ENDLOOP.
    IF SY-SUBRC NE 0.
      READ TABLE COST_TRIP INDEX 1.
    ENDIF.

    T_HEADER-CCODE = COST_TRIP-COMP_CODE.
    T_HEADER-CCENTER = COST_TRIP-COSTCENTER.
    T_HEADER-WBS = COST_TRIP-WBS_ELEMT.
    T_HEADER-ORDER = COST_TRIP-ORDER.

    T_HEADER-COUNTRY = FRAMEDATA-COUNTRY.
    T_HEADER-REGION = FRAMEDATA-REGION.
    T_HEADER-STATUS = STATUS-APPROVED.
    T_HEADER-SETTLE = STATUS-ACCOUNT.
    T_HEADER-FI_TR = STATUS-TRAN_FI.

    IF STATUS-APPROVED = 4.
      READ TABLE T_WI WITH KEY PERNR = T_SCOS-PERNR REINR = T_SCOS-REINR.
      IF SY-SUBRC = 0.
        T_HEADER-APPROVER = T_WI-AGENT.
        WRITE T_WI-AED TO T_HEADER-APP_DT.
      ENDIF.
    ENDIF.

    APPEND T_HEADER.

    T_DETAIL-PERNR = T_SCOS-PERNR.
    T_DETAIL-REINR = T_SCOS-REINR.

    LOOP AT MILEAGE.
      T_DETAIL-MILES = MILEAGE-M_TOTAL.
      T_DETAIL-LOC_FROM = MILEAGE-LOC_FROM.
      T_DETAIL-LOC_TO = MILEAGE-LOC_TO.
      APPEND T_DETAIL.
    ENDLOOP.
    CLEAR T_DETAIL.

    LOOP AT RECEIPTS.
      T_DETAIL-PERNR = T_SCOS-PERNR.
      T_DETAIL-REINR = T_SCOS-REINR.
      READ TABLE COST_RECE WITH KEY RECEIPTNO =  RECEIPTS-RECEIPTNO.
      IF SY-SUBRC = 0.
        IF COST_RECE-COSTCENTER IN S_COST AND
           COST_RECE-WBS_ELEMT IN S_WBS1 AND
           COST_RECE-ORDER IN S_ORDER.
          T_DETAIL-CCODE = COST_RECE-COMP_CODE.
          T_DETAIL-CCENTER = COST_RECE-COSTCENTER.
          T_DETAIL-WBS = COST_RECE-WBS_ELEMT.
          T_DETAIL-ORDER = COST_RECE-ORDER.
        ELSE.
          CONTINUE.
        ENDIF.
      ELSE.
        IF T_HEADER-CCENTER IN S_COST AND
           T_HEADER-WBS IN S_WBS1 AND
           T_HEADER-ORDER IN S_ORDER.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.

      READ TABLE ADDINFO WITH KEY RECEIPTNO =  RECEIPTS-RECEIPTNO.
      IF SY-SUBRC = 0.
        T_DETAIL-PURPOSE = ADDINFO-DESCRIPT.
        T_DETAIL-PARTNER = ADDINFO-BUS_REASON.
        T_DETAIL-REGION = ADDINFO-REGION.
        T_DETAIL-MERCHANT = ADDINFO-C_TXT.
      ENDIF.

      T_DETAIL-TYPE = RECEIPTS-EXP_TYPE.
      T_DETAIL-EXP_TEXT = RECEIPTS-EXP_TEXT.
      T_DETAIL-EXP_AMT = RECEIPTS-REC_AMOUNT.
      T_DETAIL-EXP_CURR = RECEIPTS-REC_CURR.
      WRITE RECEIPTS-REC_DATE TO T_DETAIL-EXP_DT .

      CONCATENATE 'R' RECEIPTS-RECEIPTNO INTO W_TID.
      CONDENSE W_TID NO-GAPS.
      READ TABLE T_TEXT WITH KEY TEXTID = W_TID .
      IF SY-SUBRC = 0.
        T_DETAIL-COMM = T_TEXT-TEXTLINE.
      ENDIF.

      APPEND T_DETAIL.
      CLEAR T_DETAIL.
    ENDLOOP.

  ENDLOOP.

  perform output_alv_report.

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ALV_REPORT
*&---------------------------------------------------------------------*
*   Output ALV Report
*----------------------------------------------------------------------*
FORM output_alv_report.

  PERFORM build_FC.

  CLEAR z_layout.
*  z_layout-colwidth_optimize = 'X'.
  Z_layout-detail_popup = 'X'.
*  Z_layout-no_vline = 'X'.
  Z_layout-numc_sum = 'X'.
*  Z_layout-EXPAND_ALL = 'X'.
  Z_layout-get_selinfos = 'X'.
  Z_layout-confirmation_prompt = 'X'.
*  Z_layout-box_rollname = 'S'.

* variant settings
  z_variant-report = sy-repid.
  z_variant-variant = z_varid.


  alv_keyinfo-header01 = 'PERNR'.
  alv_keyinfo-header02 = 'REINR'.
  alv_keyinfo-item01   = 'PERNR'.
  alv_keyinfo-item02   = 'REINR'.
* call ALV function to output report

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      is_layout        = Z_layout
      it_fieldcat      = t_fc[]
      i_default        = 'X'
      i_save           = 'A'
      is_variant       = Z_VARIANT
      i_tabname_header = 'T_HEADER'
      i_tabname_item   = 'T_DETAIL'
      is_keyinfo       = alv_keyinfo
    TABLES
      t_outtab_header  = T_HEADER
      t_outtab_item    = T_DETAIL
    EXCEPTIONS
      program_error    = 1
      OTHERS           = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FC  text
*----------------------------------------------------------------------*
FORM BUILD_FC.

  perform fill_fields tables t_fc
    using 'T_HEADER' 'PERNR' 'Employee' 'Employee' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'NAME' 'Name' 'Name' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'REINR' 'Trip Number' 'Trip' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'DATV1' 'Date From' 'From' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'DATB1' 'Date To' 'To' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'REASON' 'Reason' 'Reason' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'AMOUNT' 'Total Cost' 'Total' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'REM_AMT' 'Reimbursement' 'Rem Amt' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'CCODE' 'Company Code' 'C Code' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'CCENTER' 'Cost Center' 'C Center' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'WBS' 'WBS' 'WBS' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'ORDER' 'Order' 'Order' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'COUNTRY' 'Country' 'Country' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'REGION' 'Region' 'Region' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'STATUS' 'Status' 'Status' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'SETTLE' 'Settlement' 'Settle' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'FI_TR' 'FI Transfer' 'FI' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'APPROVER' 'Approver' 'Approver' .
  perform fill_fields tables t_fc
    using 'T_HEADER' 'APP_DT' 'Approval Date' 'App Date' .

  perform fill_fields tables t_fc
    using 'T_DETAIL' 'PERNR' 'Employee' 'Employee' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'REINR' 'Trip' 'Trip' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'MILES' 'Miles' 'Miles' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'LOC_FROM' 'From' 'From' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'LOC_TO' 'To' 'To' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'CCODE' 'Company Code' 'C Code' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'CCENTER' 'Cost Center' 'C Center' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'WBS' 'WBS' 'WBS' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'ORDER' 'Order' 'Order' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'PURPOSE' 'Purpose' 'Purpose' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'PARTNER' 'Partner' 'Partner' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'TYPE' 'Type' 'Type' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'EXP_TEXT' 'Type Name' 'Name' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'EXP_AMT' 'Amount' 'Amount' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'EXP_CURR' 'Currency' 'Currency' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'EXP_DT' 'Date' 'Date' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'REGION' 'Region' 'Region' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'COMM' 'Comments' 'Comments' .
  perform fill_fields tables t_fc
    using 'T_DETAIL' 'MERCHANT' 'Merchant' 'Merchant' .

ENDFORM.                    " BUILD_FC
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_FC  text
*      -->P_0431   text
*      -->P_0432   text
*      -->P_0433   text
*      -->P_0434   text
*----------------------------------------------------------------------*
FORM FILL_FIELDS  TABLES   fieldcatalog STRUCTURE T_FC
                  using p_tabname
                        p_fieldname
                        p_l  p_S.

  fieldcatalog-tabname = p_tabname.
  fieldcatalog-fieldname = p_fieldname.
  fieldcatalog-SELTEXT_L = p_L.
  fieldcatalog-SELTEXT_M = p_L.
*  fieldcatalog-SELTEXT_S = p_S.
  fieldcatalog-emphasize = '1234'.
  fieldcatalog-just = 'C'.
  fieldcatalog-decimals_out = '2'.

  IF p_tabname = 'T_DETAIL'.
    case p_fieldname.
      when 'PERNR' or 'REINR'.
        fieldcatalog-NO_OUT = 'X'.
    endcase.
  ENDIF.

  append fieldcatalog.
  clear fieldcatalog.

ENDFORM.                    " FILL_FIELDS
