*&---------------------------------------------------------------------*
*& Report  ZFI_INV_APPROVAL_DETAILS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFI_INV_APPROVAL_DETAILS MESSAGE-ID ZFI01.

TABLES: BKPF, TOA01, RBKP, BSEG.

DATA: BEGIN OF T_OUT OCCURS 0,
      INST  TYPE CHAR10,
      MBELNR TYPE RE_BELNR,
      MGJAHR TYPE GJAHR,
      BELNR TYPE BELNR_D,
      BUKRS TYPE BUKRS,
      GJAHR TYPE GJAHR,
      LIFNR TYPE LIFNR,
      NAME1 TYPE NAME1_GP,
      BLART TYPE BLART,
      docdate type char10,
      postdate type char10,
      clrdate type char10,
      AMT   TYPE WRBTR,
      CURR  TYPE WAERS,
      IDATE TYPE CHAR10,
      ITIME TYPE CHAR10,
      IEDATE TYPE CHAR10,
      PDATE TYPE CHAR10,
      PTIME TYPE CHAR10,
      RDATE TYPE CHAR10,
      REXDATE TYPE CHAR10,
      REJECTED(3) TYPE C,
      ODATE TYPE CHAR10,
      OTIME TYPE CHAR10,
      APPREJ(3) TYPE C,
      ZFBDT TYPE CHAR10,
      ZTERM TYPE DZTERM,
      ZBD1T TYPE DZBD1T,
      ZBD2T TYPE DZBD2T,
      ZBD3T TYPE DZBD3T,
      ARREAR TYPE PEINH,
      CAL1(3) TYPE C,
      CAL2(3) TYPE C,
      CAL3(3) TYPE C,
      CAL4(3) TYPE C,
      CAL5(3) TYPE C,
      CAL6(3) TYPE C,
      CAL7(3) TYPE C,
      CAL8(3) TYPE C,
      UPARK TYPE USNAM,
      UPOST TYPE USNAM,
      XREF3 TYPE XREF3,
     END OF T_OUT.

DATA: BEGIN OF T_BKPF OCCURS 0,
      BUKRS TYPE BUKRS,
      BELNR TYPE BELNR_D,
      GJAHR TYPE GJAHR,
      BLART TYPE BLART,
      BLDAT TYPE BLDAT,
      BUDAT TYPE BUDAT,
      CPUDT TYPE CPUDT,
      CPUTM TYPE CPUTM,
      USNAM TYPE USNAM,
      WAERS TYPE WAERS,
      AWKEY TYPE AWKEY,
      PPNAM TYPE PPNAM,
     END OF T_BKPF.

DATA: BEGIN OF T_BSEG OCCURS 0,
      BUKRS TYPE BUKRS,
      BELNR TYPE BELNR_D,
      GJAHR TYPE GJAHR,
      AUGDT type AUGDT,
      KOART type KOART,
      WRBTR TYPE WRBTR,
      LIFNR TYPE LIFNR,
      ZFBDT TYPE DZFBDT,
      ZTERM TYPE DZTERM,
      ZBD1T TYPE DZBD1T,
      ZBD2T TYPE DZBD2T,
      ZBD3T TYPE DZBD3T,
      XREF3 TYPE XREF3,
      END OF T_BSEG.

DATA: BEGIN OF t_swwwihead OCCURS 0,
      wi_id type sww_wiid,
      wi_cd type sww_cd,
      wi_ct type sww_ct,
      wi_aed type sww_aed,
      WI_AAGENT type SWW_AAGENT,
      wi_rh_task type swW_task,
      END OF t_swwwihead.

DATA: BEGIN OF T_LFA1 OCCURS 0,
      LIFNR TYPE LIFNR,
      NAME1 TYPE NAME1_GP,
      END OF T_LFA1.

DATA: lt_objtype like SWOTOBJID-OBJTYPE,
      lt_worklist TYPE swrtwihdr,
      lw_worklist TYPE SWR_WIHDR,
      lw_objkey   TYPE swotobjid-objkey,
      LW_WIID TYPE SWW_WIID,
      LT_CONT TYPE TABLE OF SWR_CONT WITH HEADER LINE.

* Data for ALV
type-pools: SLIS.
DATA: t_fc   TYPE slis_t_fieldcat_alv with HEADER LINE,
     t_sort TYPE slis_t_sortinfo_alv.
DATA: z_variant LIKE disvariant.
DATA: z_varid LIKE disvariant-variant VALUE '/DEFAULT'.
DATA: g_variant_flag.
DATA: g_selmod.

DATA: z_layout TYPE slis_layout_alv.
DATA: T_header   TYPE slis_t_listheader.
data: z_text(60).
DATA: gs_line TYPE slis_listheader.

SELECTION-SCREEN BEGIN OF BLOCK RMA WITH FRAME title text-001.
select-OPTIONS: S_BUKRS for BKPF-BUKRS OBLIGATORY,
                S_BELNR for BKPF-BELNR.
PARAMETERS:     P_GJAHR like BKPF-GJAHR OBLIGATORY.
select-OPTIONS: S_BLART for BKPF-BLART,
                S_BLDAT for BKPF-BLDAT,
                S_BUDAT for BKPF-BUDAT,
                S_AUGDT for BSEG-AUGDT,
                S_CPUDT for BKPF-CPUDT.
SELECTION-SCREEN END OF BLOCK RMA.

START-OF-SELECTION.

  PERFORM get_data.

END-OF-SELECTION.

  PERFORM CREATE_OUTPUT.

  perform output_alv_report.
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_ALV_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OUTPUT_ALV_REPORT .
  PERFORM build_FC.

  CLEAR z_layout.
  z_layout-info_fieldname    = 'LINECOLOR'.
  z_layout-colwidth_optimize = 'X'.
  Z_layout-detail_popup = 'X'.
  Z_layout-numc_sum = 'X'.
  Z_layout-get_selinfos = 'X'.
  Z_layout-confirmation_prompt = 'X'.
  Z_layout-box_rollname = 'S'.

* variant settings
  z_variant-report = sy-repid.
  z_variant-variant = z_varid.

* call ALV function to output report

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = sy-repid
*     I_CALLBACK_TOP_OF_PAGE  = 'TOP_OF_PAGE'
*     I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IS_LAYOUT               = z_Layout
      IT_FIELDCAT             = T_fc[]
      I_DEFAULT               = 'X'
      I_SAVE                  = 'X'
      IS_VARIANT              = z_variant
    TABLES
      t_outtab                = T_OUT.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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
FORM BUILD_FC .
  t_fc-FIELDNAME   = 'INST'.
  t_fc-SELTEXT_M   = 'Instance'.
  t_fc-COL_POS     = 0.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'MBELNR'.
  t_fc-SELTEXT_M   = 'MM Inv No.'.
  t_fc-COL_POS     = 1.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'MGJAHR'.
  t_fc-SELTEXT_M   = 'Year'.
  t_fc-COL_POS     = 2.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'BELNR'.
  t_fc-SELTEXT_M   = 'Doc Number'.
  t_fc-COL_POS     = 3.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'BUKRS'.
  t_fc-SELTEXT_M   = 'Company Code'.
  t_fc-COL_POS     = 4.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'GJAHR'.
  t_fc-SELTEXT_M   = 'Year'.
  t_fc-COL_POS     = 5.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'LIFNR'.
  t_fc-SELTEXT_M   = 'Vendor'.
  t_fc-COL_POS     = 6.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'NAME1'.
  t_fc-SELTEXT_M   = 'Name'.
  t_fc-COL_POS     = 7.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'BLART'.
  t_fc-SELTEXT_M   = 'Doc Type'.
  t_fc-COL_POS     = 8.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'DOCDATE'.
  t_fc-SELTEXT_M   = 'Doc Date'.
  t_fc-COL_POS     = 9.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'POSTDATE'.
  t_fc-SELTEXT_M   = 'Posting Date'.
  t_fc-COL_POS     = 10.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CLRDATE'.
  t_fc-SELTEXT_M   = 'Clearing Date'.
  t_fc-COL_POS     = 11.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'AMT'.
  t_fc-SELTEXT_M   = 'Amount'.
  t_fc-COL_POS     = 12.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CURR'.
  t_fc-SELTEXT_M   = 'Currency'.
  t_fc-COL_POS     = 13.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'IDATE'.
  t_fc-SELTEXT_M   = 'Image Date'.
  t_fc-COL_POS     = 14.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ITIME'.
  t_fc-SELTEXT_M   = 'Image Time'.
  t_fc-COL_POS     = 15.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'IEDATE'.
  t_fc-SELTEXT_M   = 'Image Exit Date'.
  t_fc-COL_POS     = 16.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'PDATE'.
  t_fc-SELTEXT_M   = 'Park Date'.
  t_fc-COL_POS     = 17.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'PTIME'.
  t_fc-SELTEXT_M   = 'Park Time'.
  t_fc-COL_POS     = 18.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'RDATE'.
  t_fc-SELTEXT_M   = 'RCO Receipt'.
  t_fc-COL_POS     = 19.
*  t_fc-No_out     = 'X'.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'REXDATE'.
  t_fc-SELTEXT_M   = 'RCO Completion'.
  t_fc-COL_POS     = 20.
*  t_fc-No_out     = 'X'.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'REJECTED'.
  t_fc-SELTEXT_M   = 'RCO/SRC Rejected'.
  t_fc-COL_POS     = 21.
*  t_fc-No_out     = 'X'.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ODATE'.
  t_fc-SELTEXT_M   = 'Entry Date'.
  t_fc-COL_POS     = 22.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'OTIME'.
  t_fc-SELTEXT_M   = 'Entry Time'.
  t_fc-COL_POS     = 23.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'APPREJ'.
  t_fc-SELTEXT_M   = 'Approver Rejected'.
  t_fc-COL_POS     = 24.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ZFBDT'.
  t_fc-SELTEXT_M   = 'Baseline Date'.
  t_fc-COL_POS     = 25.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ZTERM'.
  t_fc-SELTEXT_M   = 'Terms of Payment'.
  t_fc-COL_POS     = 26.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ZBD1T'.
  t_fc-SELTEXT_M   = 'Cash Disc Days 1'.
  t_fc-COL_POS     = 27.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ZBD2T'.
  t_fc-SELTEXT_M   = 'Cash Disc Days 2'.
  t_fc-COL_POS     = 28.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ZBD3T'.
  t_fc-SELTEXT_M   = 'Cash Disc Days 3'.
  t_fc-COL_POS     = 29.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ARREAR'.
  t_fc-SELTEXT_M   = 'Arrears'.
  t_fc-COL_POS     = 30.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CAL1'.
  t_fc-SELTEXT_M   = 'Doc to Image Date'.
  t_fc-COL_POS     = 31.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CAL2'.
  t_fc-SELTEXT_M   = 'Image to Park Date'.
  t_fc-COL_POS     = 32.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CAL3'.
  t_fc-SELTEXT_M   = 'RCO Compl to Entry'.
  t_fc-COL_POS     = 33.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CAL4'.
  t_fc-SELTEXT_M   = 'Image to Entry Date'.
  t_fc-COL_POS     = 34.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CAL5'.
  t_fc-SELTEXT_M   = 'Park to Entry Date'.
  t_fc-COL_POS     = 35.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CAL6'.
  t_fc-SELTEXT_M   = 'Entry to Clearing'.
  t_fc-COL_POS     = 36.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CAL7'.
  t_fc-SELTEXT_M   = 'Park to RCO Compl'.
  t_fc-COL_POS     = 37.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'CAL8'.
  t_fc-SELTEXT_M   = 'Invoice to Clearing'.
  t_fc-COL_POS     = 38.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'UPARK'.
  t_fc-SELTEXT_M   = 'Parked By'.
  t_fc-COL_POS     = 39.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'UPOST'.
  t_fc-SELTEXT_M   = 'Entered By'.
  t_fc-COL_POS     = 40.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'XREF3'.
  t_fc-SELTEXT_M   = 'Reference Key'.
  t_fc-COL_POS     = 41.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
ENDFORM.                    " BUILD_FC
*&---------------------------------------------------------------------*
*&      Form  CREATE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_OUTPUT .
  data: lv_blart type bkpf-blart,
        LV_AED TYPE DATUM,
        LV_IDATE TYPE DATUM,
        LV_DOCDATE TYPE DATUM,
        LV_PDATE TYPE DATUM,
        LV_ODATE TYPE DATUM,
        LV_REXDATE TYPE DATUM,
        LV_CLRDATE TYPE DATUM,
        LV_nwf TYPE c,
        lv_top   like swwwihead-wi_id.

  LOOP AT T_BKPF.
    CLEAR:  T_OUT, lw_objkey , LV_IDATE, LV_DOCDATE, LV_PDATE, LV_ODATE, LV_REXDATE, LV_CLRDATE, lv_nwf .
    REFRESH: lt_worklist.

    T_OUT-INST = 'UG'.

    IF T_BKPF-BLART = 'RE' OR T_BKPF-BLART = 'ZR'.
      T_OUT-MBELNR = T_BKPF-AWKEY+0(10).
      T_OUT-MGJAHR = T_BKPF-AWKEY+10(4).
    ELSE.
      SELECT SINGLE blart INTO lv_blart
        FROM zfit_valid_blart
        WHERE blart = T_BKPF-BLART
          AND workflow = 'AP'.
      IF sy-subrc IS NOT INITIAL.
        LV_NWF = 'X'.
*        CONTINUE.
      ENDIF.
    ENDIF.

    T_OUT-BELNR = T_BKPF-BELNR.
    T_OUT-GJAHR = T_BKPF-GJAHR.
    T_OUT-BUKRS = T_BKPF-BUKRS.
    T_OUT-BLART = T_BKPF-BLART.
    T_OUT-CURR  = T_BKPF-WAERS.
    write t_bkpf-bldat to t_out-docdate.
    LV_DOCDATE = t_bkpf-bldat.
    write t_bkpf-budat to t_out-postdate.
    WRITE T_BKPF-CPUDT TO T_OUT-ODATE.
    LV_ODATE = T_BKPF-CPUDT.
    WRITE T_BKPF-CPUTM TO T_OUT-OTIME.

    READ TABLE T_BSEG WITH KEY BUKRS = T_BKPF-BUKRS BELNR = T_BKPF-BELNR GJAHR = T_BKPF-GJAHR KOART = 'K'.
    IF SY-SUBRC = 0.
      T_OUT-LIFNR = T_BSEG-LIFNR.
      T_OUT-AMT   = T_BSEG-WRBTR.
      T_OUT-XREF3   = T_BSEG-XREF3.
      IF NOT S_AUGDT[] is INITIAL.
        if t_bseg-augdt in s_augdt.
          write t_bseg-augdt to t_out-clrdate.
          LV_CLRDATE = t_bseg-augdt.
        else.
          CONTINUE.
        endif.
      ELSE.
        write t_bseg-augdt to t_out-clrdate.
        LV_CLRDATE = t_bseg-augdt.
      endif.
*      write t_bseg-augdt to t_out-clrdate.
      write t_bseg-ZFBDT to t_out-ZFBDT.
      T_OUT-ZTERM = T_BSEG-ZTERM.
      T_OUT-ZBD1T = T_BSEG-ZBD1T.
      T_OUT-ZBD2T = T_BSEG-ZBD2T.
      T_OUT-ZBD3T = T_BSEG-ZBD3T.
      IF not t_bseg-ZFBDT is INITIAL AND t_bseg-ZFBDT GT '18000101' AND t_bseg-ZFBDT LT '22150101'. "SDP 80107
        IF NOT t_bseg-augdt is INITIAL.
          T_OUT-ARREAR = t_bseg-augdt - t_bseg-ZFBDT.
        ELSE.
          T_OUT-ARREAR = sy-datum - t_bseg-ZFBDT.
        ENDIF.

        IF T_BSEG-ZBD1T GE T_BSEG-ZBD2T AND T_BSEG-ZBD1T GE T_BSEG-ZBD3T.
          T_OUT-ARREAR = T_OUT-ARREAR - T_BSEG-ZBD1T.
        ELSEIF T_BSEG-ZBD2T GE T_BSEG-ZBD1T AND T_BSEG-ZBD2T GE T_BSEG-ZBD3T.
          T_OUT-ARREAR = T_OUT-ARREAR - T_BSEG-ZBD2T.
        ELSEIF T_BSEG-ZBD3T GE T_BSEG-ZBD1T AND T_BSEG-ZBD3T GE T_BSEG-ZBD2T.
          T_OUT-ARREAR = T_OUT-ARREAR - T_BSEG-ZBD3T.
        ENDIF.
      ENDIF.
      READ TABLE T_LFA1 WITH KEY LIFNR = T_BSEG-LIFNR.
      IF SY-SUBRC = 0.
        T_OUT-NAME1 = T_LFA1-NAME1.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    T_OUT-UPARK = T_BKPF-PPNAM.
    T_OUT-UPOST = T_BKPF-USNAM.

    IF T_BKPF-BLART = 'RE'  OR T_BKPF-BLART = 'ZR'.
      lt_objtype = 'BUS2081'.
      lw_objkey = T_BKPF-AWKEY+0(14).
    ELSE.
      lt_objtype = 'FIPP'.
      lw_objkey+0(4) = T_BKPF-BUKRS.
      lw_objkey+4(10) = T_BKPF-BELNR.
      lw_objkey+14(4) = T_BKPF-GJAHR.
    ENDIF.

    IF LV_NWF IS INITIAL.
      CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
        EXPORTING
          OBJTYPE                  = lt_objtype
          OBJKEY                   = lw_objkey
          TOP_LEVEL_ITEMS          = ' '
          SELECTION_STATUS_VARIANT = '0000'
        TABLES
          WORKLIST                 = lt_worklist.

      READ TABLE lt_worklist INTO LW_WORKLIST WITH KEY WI_RH_TASK = 'TS30001128'.
      IF SY-SUBRC = 0.
        WRITE LW_WORKLIST-WI_CD TO T_OUT-IDATE.
        LV_IDATE = LW_WORKLIST-WI_CD.
        WRITE LW_WORKLIST-WI_CT TO T_OUT-ITIME.
        SELECT SINGLE WI_AED FROM SWWWIHEAD INTO LV_AED
          WHERE WI_ID = LW_WORKLIST-WI_ID.
        IF SY-SUBRC = 0.
          WRITE LV_AED TO T_OUT-IEDATE.
        ENDIF.
      ENDIF.

      SORT LT_WORKLIST BY WI_CD WI_CT ASCENDING.
      IF T_BKPF-BLART = 'RE'  OR T_BKPF-BLART = 'ZR' .
        clear lv_top.
        READ TABLE lt_worklist INTO LW_WORKLIST WITH KEY WI_RH_TASK = 'WS02000003' WI_STAT = 'COMPLETED'.
        IF SY-SUBRC = 0.
          lv_top = LW_WORKLIST-WI_ID.
          WRITE LW_WORKLIST-WI_CD TO T_OUT-PDATE.
          LV_PDATE = LW_WORKLIST-WI_CD.
          WRITE LW_WORKLIST-WI_CT TO T_OUT-PTIME.
          T_OUT-UPARK = LW_WORKLIST-WI_creator+2.
*        IF LW_WORKLIST-WI_STAT = 'COMPLETED'.
*          WRITE T_BKPF-CPUDT TO T_OUT-ODATE.
*          WRITE T_BKPF-CPUTM TO T_OUT-OTIME.
*        ENDIF.
        ENDIF.
        REFRESH T_SWWWIHEAD.
        IF NOT lv_top is INITIAL.
          SELECT WI_ID WI_CD WI_CT WI_AED WI_AAGENT WI_RH_TASK FROM SWWWIHEAD INTO TABLE T_SWWWIHEAD
            WHERE WI_TYPE = 'W' AND
                  WI_RH_TASK = 'TS02000027' AND
                  TOP_WI_ID = LV_TOP.
          IF SY-SUBRC = 0.
            T_OUT-REJECTED = 'Yes'.
          ELSE.
            T_OUT-REJECTED = 'No'.
          ENDIF.

          REFRESH T_SWWWIHEAD.
          SELECT WI_ID WI_CD WI_CT WI_AED WI_AAGENT WI_RH_TASK FROM SWWWIHEAD INTO TABLE T_SWWWIHEAD
            WHERE WI_TYPE = 'W' AND
                  WI_RH_TASK = 'TS02000010' AND
                  TOP_WI_ID = LV_TOP.
          IF SY-SUBRC = 0.
            sort t_swwwihead by wi_cd DESCENDING wi_ct DESCENDING.
            READ TABLE t_swwwihead INDEX 1.
            T_OUT-upost = t_swwwihead-WI_AAGENT.
          ENDIF.
        ENDIF.
*      LOOP AT LT_WORKLIST INTO LW_WORKLIST WHERE
*        WI_RH_TASK = 'TS02000053' AND WI_STAT = 'COMPLETED'.
*        WRITE LW_WORKLIST-WI_CD TO T_OUT-ODATE.
*        WRITE LW_WORKLIST-WI_CT TO T_OUT-OTIME.
*      ENDLOOP.
      ELSE.
        clear lv_top.
        READ TABLE lt_worklist INTO LW_WORKLIST WITH KEY WI_RH_TASK = 'WS02000002' WI_STAT = 'COMPLETED'.
        IF SY-SUBRC = 0.
          lv_top = LW_WORKLIST-WI_ID.
          WRITE LW_WORKLIST-WI_CD TO T_OUT-PDATE.
          LV_PDATE = LW_WORKLIST-WI_CD.
          WRITE LW_WORKLIST-WI_CT TO T_OUT-PTIME.
*        IF LW_WORKLIST-WI_STAT = 'COMPLETED'.
*          WRITE T_BKPF-CPUDT TO T_OUT-ODATE.
*          WRITE T_BKPF-CPUTM TO T_OUT-OTIME.
*        ENDIF.
        ENDIF.

        clear: T_OUT-RDATE, T_OUT-REXDATE.
        REFRESH T_SWWWIHEAD.
        IF NOT lv_top is INITIAL.
          SELECT WI_ID WI_CD WI_CT WI_AED WI_AAGENT WI_RH_TASK FROM SWWWIHEAD INTO TABLE T_SWWWIHEAD
            WHERE WI_TYPE = 'W' AND
                  TOP_WI_ID = LV_TOP.
          IF SY-SUBRC = 0.
            SORT T_SWWWIHEAD BY WI_CD WI_CT ASCENDING.

            LOOP AT T_SWWWIHEAD WHERE WI_RH_TASK = 'TS02000013'.
              LW_WIID = T_SWWWIHEAD-WI_ID.
              REFRESH: LT_CONT.

              CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
                EXPORTING
                  WORKITEM_ID      = LW_WIID
                TABLES
                  SIMPLE_CONTAINER = LT_CONT.

              READ TABLE LT_CONT WITH KEY ELEMENT = 'DECISIONTYPE'.
              IF SY-SUBRC = 0.
                IF LT_CONT-VALUE = 'RCODE' AND T_OUT-RDATE IS INITIAL.
                  WRITE T_SWWWIHEAD-WI_CD TO T_OUT-RDATE.
*                CLEAR: T_OUT-RDATE, T_OUT-RTIME.
                ELSEIF LT_CONT-VALUE = 'POST'.
                  IF T_OUT-REXDATE IS INITIAL.
                    WRITE T_SWWWIHEAD-WI_CD TO T_OUT-REXDATE.
                    LV_REXDATE = T_SWWWIHEAD-WI_CD.
                  ENDIF.
                  t_out-upost = t_swwwihead-wi_aagent.
                ELSEIF LT_CONT-VALUE = 'INIT'.
                  IF T_OUT-REXDATE IS INITIAL.
                    T_OUT-REJECTED = 'Yes'.
                  ELSE.
                    T_OUT-APPREJ = 'Yes'.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDLOOP.
            IF T_OUT-REJECTED IS INITIAL.
              T_OUT-REJECTED = 'No'.
            ENDIF.
            IF T_OUT-APPREJ IS INITIAL.
              T_OUT-APPREJ = 'No'.
            ENDIF.
          ENDIF.
        ENDIF.

        REFRESH: lt_worklist.
        lt_objtype = 'BKPF'.

        CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
          EXPORTING
            OBJTYPE                  = lt_objtype
            OBJKEY                   = lw_objkey
            TOP_LEVEL_ITEMS          = ' '
            SELECTION_STATUS_VARIANT = '0000'
          TABLES
            WORKLIST                 = lt_worklist.

        READ TABLE lt_worklist INTO LW_WORKLIST WITH KEY WI_RH_TASK = 'TS30001128'.
        IF SY-SUBRC = 0.
          WRITE LW_WORKLIST-WI_CD TO T_OUT-IDATE.
          LV_IDATE = LW_WORKLIST-WI_CD.
          WRITE LW_WORKLIST-WI_CT TO T_OUT-ITIME.
        ENDIF.

      ENDIF.
    ENDIF.

    IF T_OUT-IDATE IS INITIAL.
      SELECT SINGLE AR_DATE FROM TOA01 into LV_IDATE  WHERE
        OBJECT_ID = lw_objkey.
      IF sy-subrc = 0.
        WRITE LV_IDATE to T_OUT-IDATE.
      ENDIF.
    ENDIF.

    IF NOT LV_IDATE IS INITIAL AND NOT LV_DOCDATE IS INITIAL.
      IF LV_IDATE GE LV_DOCDATE.
        T_OUT-CAL1 = LV_IDATE - LV_DOCDATE.
      ELSE.
        T_OUT-CAL1 = LV_DOCDATE - LV_IDATE.
      ENDIF.
    ENDIF.

    IF NOT LV_PDATE IS INITIAL AND NOT LV_IDATE IS INITIAL.
      IF LV_PDATE GE LV_IDATE.
        T_OUT-CAL2 = LV_PDATE - LV_IDATE.
      ELSE.
        T_OUT-CAL2 = LV_IDATE - LV_PDATE.
      ENDIF.
    ENDIF.

    IF NOT LV_ODATE IS INITIAL AND NOT LV_REXDATE IS INITIAL.
      IF LV_ODATE GE LV_REXDATE.
        T_OUT-CAL3 = LV_ODATE - LV_REXDATE.
      ELSE.
        T_OUT-CAL3 = LV_REXDATE - LV_ODATE.
      ENDIF.
    ENDIF.

    IF NOT LV_ODATE IS INITIAL AND NOT LV_IDATE IS INITIAL.
      IF LV_ODATE GE LV_IDATE.
        T_OUT-CAL4 = LV_ODATE - LV_IDATE.
      ELSE.
        T_OUT-CAL4 = LV_IDATE - LV_ODATE.
      ENDIF.
    ENDIF.

    IF NOT LV_ODATE IS INITIAL AND NOT LV_PDATE IS INITIAL.
      IF LV_ODATE GE LV_PDATE.
        T_OUT-CAL5 = LV_ODATE - LV_PDATE.
      ELSE.
        T_OUT-CAL5 = LV_PDATE - LV_ODATE.
      ENDIF.
    ENDIF.

    IF NOT LV_CLRDATE IS INITIAL AND NOT LV_ODATE IS INITIAL.
      IF LV_CLRDATE GE LV_ODATE.
        T_OUT-CAL6 = LV_CLRDATE - LV_ODATE.
      ELSE.
        T_OUT-CAL6 = LV_ODATE - LV_CLRDATE.
      ENDIF.
    ENDIF.

    IF NOT LV_PDATE IS INITIAL AND NOT LV_REXDATE IS INITIAL.
      IF LV_PDATE GE LV_REXDATE.
        T_OUT-CAL7 = LV_PDATE - LV_REXDATE.
      ELSE.
        T_OUT-CAL7 = LV_REXDATE - LV_PDATE.
      ENDIF.
    ENDIF.

    IF NOT LV_CLRDATE IS INITIAL AND NOT LV_DOCDATE IS INITIAL.
      IF LV_CLRDATE GE LV_DOCDATE.
        T_OUT-CAL8 = LV_CLRDATE - LV_DOCDATE.
      ELSE.
        T_OUT-CAL8 = LV_DOCDATE - LV_CLRDATE.
      ENDIF.
    ENDIF.
*    IF T_OUT-PDATE IS INITIAL. "No workflow for the doc   "71619 commented
*      CONTINUE.
*    ENDIF.
    APPEND T_OUT.
  ENDLOOP.

ENDFORM.                    " CREATE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

  SELECT BUKRS BELNR GJAHR BLART BLDAT BUDAT CPUDT CPUTM USNAM WAERS AWKEY PPNAM FROM BKPF INTO TABLE T_BKPF
    WHERE BUKRS IN S_BUKRS AND
          BELNR IN S_BELNR AND
          GJAHR = P_GJAHR AND
          BLART IN S_BLART AND
          BLDAT in S_BLDAT AND
          BUDAT in S_BUDAT AND
          CPUDT IN S_CPUDT AND
          BSTAT = ' '.
  IF SY-SUBRC = 0.
    SELECT BUKRS BELNR GJAHR AUGDT koart WRBTR LIFNR ZFBDT ZTERM ZBD1T ZBD2T ZBD3T XREF3 FROM BSEG INTO TABLE T_BSEG
      FOR ALL ENTRIES IN T_BKPF
      WHERE BUKRS = T_BKPF-BUKRS AND
            BELNR = T_BKPF-BELNR AND
            GJAHR = T_BKPF-GJAHR AND
            LIFNR NE ' '.
    IF SY-SUBRC = 0.
      SELECT LIFNR NAME1 FROM LFA1 INTO TABLE T_LFA1
        FOR ALL ENTRIES IN T_BSEG
        WHERE LIFNR = T_BSEG-LIFNR.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_DATA
