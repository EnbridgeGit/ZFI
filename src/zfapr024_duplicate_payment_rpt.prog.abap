*&---------------------------------------------------------------------*
*& Report  ZFAPR024_DUPLICATE_PAYMENT_RPT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFAPR024_DUPLICATE_PAYMENT_RPT.
tables: REGUV.

TYPES: BEGIN OF ty_out,
        LAUFD TYPE CHAR10,
        LAUFI TYPE LAUFI,
        ANZER TYPE ANZER,
        ANZGB TYPE ANZGB,
        DIFF TYPE ANZER,
       END OF ty_out.

DATA: T_OUT TYPE TABLE OF TY_OUT WITH HEADER LINE,
      WA_OUT TYPE TY_OUT.

DATA: W_DIFF type c.

* Data for ALV
type-pools: SLIS.
DATA: t_fc   TYPE slis_t_fieldcat_alv with HEADER LINE,
     t_sort TYPE slis_t_sortinfo_alv.
DATA: z_variant LIKE disvariant.
DATA: z_varid LIKE disvariant-variant VALUE '/DEFAULT'.
DATA: g_variant_flag.
DATA: g_selmod.

DATA: z_layout TYPE slis_layout_alv.

SELECTION-SCREEN BEGIN OF BLOCK RMA WITH FRAME title text-001.
select-OPTIONS: S_LAUFD FOR REGUV-LAUFD,
                S_LAUFI FOR REGUV-LAUFI.
SELECTION-SCREEN END OF BLOCK RMA.

START-OF-SELECTION.
  perform select_data.

END-OF-SELECTION.
  perform prepare_output.
  perform output_alv_report.
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA .

  CLEAR W_DIFF.

  SELECT * FROM REGUV WHERE LAUFD IN S_LAUFD AND LAUFI IN S_LAUFI.
    WRITE REGUV-LAUFD TO WA_OUT-LAUFD.
    WA_OUT-LAUFI = REGUV-LAUFI.
    WA_OUT-ANZER = REGUV-ANZER.
    WA_OUT-ANZGB = REGUV-ANZGB.
    WA_OUT-DIFF = REGUV-ANZER - REGUV-ANZGB.
    IF WA_OUT-DIFF NE 0.
      w_DIFF = 'X'.
    ENDIF.
    APPEND WA_OUT TO T_OUT.
  ENDSELECT.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  PREPARE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARE_OUTPUT .

ENDFORM.                    " PREPARE_OUTPUT
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
  IF W_DIFF = 'X'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM      = sy-repid
*       I_CALLBACK_TOP_OF_PAGE  = 'TOP_OF_PAGE'
*       I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
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
  data: w_ind type i.

  w_ind = 0.

  t_fc-FIELDNAME   = 'LAUFD'.
  t_fc-SELTEXT_M   = 'Run Date'.
  t_fc-COL_POS     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_ind = w_ind + 1.

  t_fc-FIELDNAME   = 'LAUFI'.
  t_fc-SELTEXT_M   = 'Run ID'.
  t_fc-COL_POS     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ANZER'.
  t_fc-SELTEXT_M   = 'Payments Created'.
  t_fc-COL_POS     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'ANZGB'.
  t_fc-SELTEXT_M   = 'Payments Posted'.
  t_fc-COL_POS     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  t_fc-FIELDNAME   = 'DIFF'.
  t_fc-SELTEXT_M   = 'Difference'.
  t_fc-COL_POS     = w_ind.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
ENDFORM.                    " BUILD_FC
