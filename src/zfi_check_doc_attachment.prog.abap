*&---------------------------------------------------------------------*
*& Report  ZFI_CHECK_DOC_ATTACHMENT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFI_CHECK_DOC_ATTACHMENT MESSAGE-ID ZFI_WORKFLOW.

TABLES: BKPF, TOA01, RBKP.

DATA: BEGIN OF T_OUT OCCURS 0,
      BUKRS TYPE BUKRS,
      BELNR TYPE BELNR_D,
      GJAHR TYPE GJAHR,
      Attach TYPE CHAR3,
      BLART TYPE BLART,
      BVORG TYPE BVORG,
      XBLNR TYPE XBLNR1,
      PPNAM TYPE PPNAM,
      OBJ   TYPE CHAR7,
     END OF T_OUT.

DATA: BEGIN OF T_BKPF OCCURS 0,
      BUKRS TYPE BUKRS,
      BELNR TYPE BELNR_D,
      GJAHR TYPE GJAHR,
      BLART TYPE BLART,
      BVORG TYPE BVORG,
      XBLNR TYPE XBLNR1,
      PPNAM TYPE PPNAM,
      awtyp type bkpf-awtyp,
     END OF T_BKPF.

DATA: BEGIN OF T_IMAGE OCCURS 0,
      BUKRS TYPE BUKRS,
      BELNR TYPE BELNR_D,
      GJAHR TYPE GJAHR,
      ARC_ID TYPE SAEARCHIVI,
      ARC_DOC_ID TYPE SAEARDOID,
     END OF T_image.

Data: BEGIN OF T_OBJ OCCURS 0,
      OBJ type char7,
      END OF T_OBJ.
DATA : T_return LIKE ddshretval OCCURS 0 WITH HEADER LINE.

DATA: W_DOC TYPE SAEOBJID,
      W_IND TYPE I.

SELECTION-SCREEN BEGIN OF BLOCK RMA WITH FRAME title text-001.
select-OPTIONS: S_BUKRS for BKPF-BUKRS,
                S_BELNR for BKPF-BELNR,
                S_GJAHR for BKPF-GJAHR,
                S_BLART for BKPF-BLART,
                S_BLDAT for BKPF-BLDAT,
                S_BUDAT for BKPF-BUDAT.
PARAMETERS:     "P_OBJ TYPE CHAR7 OBLIGATORY.
                p_awtyp type bkpf-awtyp DEFAULT 'BKPF'.
SELECTION-SCREEN END OF BLOCK RMA.


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

*At selection-screen on VALUE-REQUEST FOR p_obj.

*  refresh t_obj.
*  t_obj-obj = 'BKPF'.
*  APPEND t_obj.
*  t_obj-obj = 'BUS2081'.
*  APPEND t_obj.
*
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'P_OBJ'
*      value_org       = 'S'
*    TABLES
*      value_tab       = t_obj
*      return_tab      = t_return
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*
*  P_obj = t_return-fieldval.

INITIALIZATION.

START-OF-SELECTION.

  select bukrs belnr gjahr blart BVORG XBLNR ppnam awtyp
    from BKPF INTO TABLE T_BKPF
    WHERE BUKRS IN S_BUKRS AND
          BELNR IN S_BELNR AND
          GJAHR IN S_GJAHR AND
          BLART IN S_BLART AND
          BLDAT IN S_BLDAT AND
          BUDAT IN S_BUDAT AND
          awtyp = p_awtyp.

  LOOP AT T_BKPF.
    W_IND = SY-TABIX.
*    IF P_OBJ = 'BKPF'. " or
*       P_OBJ = 'FIPP'.
*      SELECT SINGLE * FROM RBKP WHERE
*        BELNR = T_BKPF-BELNR AND
*        GJAHR = T_BKPF-GJAHR.
     IF t_bkpf-awtyp = 'BKPF'.
        CLEAR: W_DOC, T_IMAGE.
        CONCATENATE T_BKPF-BUKRS T_BKPF-BELNR T_BKPF-GJAHR INTO W_DOC
                                                    RESPECTING BLANKS.
        CONDENSE W_DOC. " NO-GAPS.

        SELECT SINGLE * FROM TOA01
          WHERE ( SAP_OBJECT = 'BKPF' OR
                  SAP_OBJECT = 'FIPP' ) AND
                OBJECT_ID = W_DOC AND
                ARCHIV_ID = 'A1'.
        IF SY-SUBRC = 0.
          T_IMAGE-BUKRS = T_BKPF-BUKRS.
          T_IMAGE-BELNR = T_BKPF-BELNR.
          T_IMAGE-GJAHR = T_BKPF-GJAHR.
          T_IMAGE-ARC_ID = TOA01-ARCHIV_ID.
          T_IMAGE-ARC_DOC_ID = TOA01-ARC_DOC_ID.
          APPEND T_IMAGE.
        ENDIF.
*      ELSE.
*        DELETE T_BKPF INDEX W_IND.
*      ENDIF.
    ELSEIF t_bkpf-awtyp = 'MKPF'. "P_OBJ = 'BUS2081'.
*      SELECT SINGLE * FROM RBKP WHERE
*        BELNR = T_BKPF-BELNR AND
*        GJAHR = T_BKPF-GJAHR.
*      IF SY-SUBRC = 0.
        CLEAR: W_DOC, T_IMAGE.
        CONCATENATE T_BKPF-BELNR T_BKPF-GJAHR INTO W_DOC
                                      RESPECTING BLANKS.
        CONDENSE W_DOC. " NO-GAPS.

        SELECT SINGLE * FROM TOA01
          WHERE SAP_OBJECT = 'BUS2081' AND
                OBJECT_ID = W_DOC AND
                ARCHIV_ID = 'A1'.
        IF SY-SUBRC = 0.
          T_IMAGE-BUKRS = T_BKPF-BUKRS.
          T_IMAGE-BELNR = T_BKPF-BELNR.
          T_IMAGE-GJAHR = T_BKPF-GJAHR.
          T_IMAGE-ARC_ID = TOA01-ARCHIV_ID.
          T_IMAGE-ARC_DOC_ID = TOA01-ARC_DOC_ID.
          APPEND T_IMAGE.
        ENDIF.
*      ELSE.
*        DELETE T_BKPF INDEX W_IND.
*      ENDIF.
    ENDIF.
  ENDLOOP.

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

  t_fc-FIELDNAME   = 'BUKRS'.
  t_fc-SELTEXT_M   = 'Company Code'.
  t_fc-COL_POS     = 0.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'BELNR'.
  t_fc-SELTEXT_M   = 'Document No'.
  t_fc-COL_POS     = 1.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'GJAHR'.
  t_fc-SELTEXT_M   = 'Year'.
  t_fc-COL_POS     = 2.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'BLART'.
  t_fc-SELTEXT_M   = 'Doc Type'.
  t_fc-COL_POS     = 4.
*  t_fc-no_out     = 'X'.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'PPNAM'.
  t_fc-SELTEXT_M   = 'Parked By'.
  t_fc-COL_POS     = 5.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ATTACH'.
  t_fc-SELTEXT_M   = 'Attachment'.
  t_fc-COL_POS     = 3.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'XBLNR'.
  t_fc-SELTEXT_M   = 'Reference'.
  t_fc-COL_POS     = 6.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'BVORG'.
  t_fc-SELTEXT_M   = 'Cross CC Number'.
  t_fc-COL_POS     = 7.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'OBJ'.
  t_fc-SELTEXT_M   = 'Object Type'.
  t_fc-COL_POS     = 8.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
ENDFORM.                    " BUILD_FC

************************************************************************
* User commands
************************************************************************
FORM user_command USING rf_ucomm LIKE sy-ucomm
                       rs       TYPE slis_selfield.

* User command
  if RF_UCOMM = '&IC1'.
    read TABLE t_out index rs-tabindex.
    if sy-subrc = 0.
      set PARAMETER ID 'BLN' field t_out-belnr.   "Doc No
      set PARAMETER ID 'BUK' field t_out-bukrs.  "Com code
      set PARAMETER ID 'GJR' field t_out-gjahr.  "Year
      call TRANSACTION 'FB03' AND SKIP FIRST SCREEN .
    endif.
  ENDIF.
  CLEAR rf_ucomm.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_OUTPUT .

  LOOP AT T_BKPF.
    CLEAR T_OUT.
    T_OUT-BUKRS = T_BKPF-BUKRS.
    T_OUT-BELNR = T_BKPF-BELNR.
    T_OUT-GJAHR = T_BKPF-GJAHR.
    T_OUT-BLART = T_BKPF-BLART.
    T_OUT-BVORG = T_BKPF-BVORG.
    T_OUT-XBLNR = T_BKPF-XBLNR.
    T_OUT-PPNAM = T_BKPF-PPNAM.
    T_OUT-OBJ = p_awtyp. "P_OBJ.
    READ TABLE T_IMAGE WITH KEY BUKRS = T_BKPF-BUKRS BELNR = T_BKPF-BELNR GJAHR = T_BKPF-GJAHR.
    IF SY-SUBRC = 0.
      T_OUT-ATTACH = 'Yes'.
    ELSE.
      T_OUT-ATTACH = 'No'.
    ENDIF.
    APPEND T_OUT.
  ENDLOOP.

ENDFORM.                    " CREATE_OUTPUT
