*&---------------------------------------------------------------------*
*& Report  ZFTVR003_CREDITCARD_RECON
*&---------------------------------------------------------------------*
REPORT  zftvr003_creditcard_recon.

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZFTVR003_CREDITCARD_RECON                         *
*  Author:           Sudhir Pargaonkar                                 *
*  Date:             June 13, 2014                                     *
*  Track #:          TR928 Release 1 - Streamline                      *
*  Application Area: FICO TV                                           *
*                                                                      *
*  Description:      TV Credit-Card Reconciliation Report              *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 06/13/14 SPARGAONK D30K923721 - Tkt 52746 - Initial program          *
*                                 development.                         *
* 08/31/16 JRHARTUNG D30K927154 - ACR-1706 Add selection criteria and  *
*                                 ALV display variant to report        *
*----------------------------------------------------------------------*
************************************************************************

TABLES: ptrv_head,
        ptrv_perio.

TYPES: BEGIN OF ty_without_trip,
          pernr TYPE pernr_d,
          bdatu TYPE ptk34-bdatu,
          betrg TYPE ptk34-betrg,
          waers TYPE ptk34-waers,
          c_txt TYPE ptk34-c_txt,
          spkzl TYPE ptk34-spkzl,
          ccard TYPE ptk34-ccard,
       END OF ty_without_trip.
TYPES: BEGIN OF ty_with_trip,
          pernr TYPE pernr_d,
          bdatu TYPE ptk34_inbel-bdatu,
          betrg TYPE ptk34_inbel-betrg,
          waers TYPE ptk34_inbel-waers,
          c_txt TYPE ptk34_inbel-c_txt,
          spkzl TYPE ptk34_inbel-spkzl,
          reinr TYPE ptk34_inbel-reinr,
          ccard TYPE ptk34-ccard,
          pdate type PTRV_ACCDT,
          litem type NRBEL,
       END OF ty_with_trip.

TYPES: BEGIN OF card_trans,
          pernr TYPE pernr_d,
          bdatu TYPE ptk34_trans-bdatu,
          betrg TYPE ptk34_trans-betrg,
          waers TYPE ptk34_trans-waers,
          c_txt TYPE ptk34_trans-c_txt,
          spkzl TYPE ptk34_trans-spkzl,
          reinr TYPE ptk34_trans-reinr,
       END OF card_trans.
TYPES:  BEGIN OF ty_msg,
        text TYPE char200,
        END OF ty_msg.

DATA: BEGIN OF t_p0001 OCCURS 0,
      pernr LIKE pa0001-pernr,
      bukrs LIKE pa0001-bukrs,
      ename LIKE pa0001-ename,
      kostl LIKE pa0001-kostl,
      END OF t_p0001.

DATA: gt_pa0000          TYPE TABLE OF pa0000,
      gs_pa0000          TYPE pa0000,
      gs_pa0002          TYPE pa0002,
      gs_pa0105          TYPE pa0105.
DATA: gt_withouttrip_e  TYPE TABLE OF ty_without_trip,
      gs_withouttrip    TYPE ty_without_trip,
      gt_with_trip_e    TYPE TABLE OF ty_with_trip,
      gt_appr_trip      TYPE TABLE OF ty_with_trip,
      gt_post_trip      TYPE TABLE OF ty_with_trip,
      gs_with_trip      TYPE ty_with_trip,
      gt_wait_appr_e    TYPE TABLE OF ty_with_trip,
      gt_msg            TYPE TABLE OF ty_msg,
      gs_msg            TYPE ty_msg.

DATA: lv_objtyp LIKE swotobjid-objtype,
      lv_objkey LIKE swotobjid-objkey,
      lt_wiid TYPE TABLE OF swr_wihdr WITH HEADER LINE.

DATA: gv_awref like PTRV_DOC_IT-AWREF.

DATA: BEGIN OF t_output OCCURS 0,
        pernr TYPE pernr_d,
        name TYPE pad_cname,
        bukrs TYPE bukrs,
        trip TYPE reinr,
        dat TYPE sy-datum, "char10,
        amt TYPE ptrv_loc_amount,
        cur TYPE waers,
        txt TYPE c_txt,
        exp TYPE spkzl,
        stat TYPE char50,
        ccard TYPE c_num,
        PERIOD TYPE CHAR10,
        kostl TYPE kostl,
*        FIDOC like bkpf-belnr,
      END OF t_output.

DATA: w_output LIKE t_output,
      w_ind TYPE i,
      w_poper LIKE  T009B-POPER,
      w_bdatj LIKE  T009B-BDATJ.

* Data for ALV
TYPE-POOLS: slis.
DATA: t_fc   TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      t_sort TYPE slis_t_sortinfo_alv.
DATA: z_variant LIKE disvariant.
DATA: z_print TYPE  slis_print_alv.
DATA: z_varid LIKE disvariant-variant VALUE '/DEFAULT'.
DATA: g_variant_flag.
DATA: g_selmod.
DATA: z_layout TYPE slis_layout_alv.
DATA: gs_line TYPE slis_listheader.

DATA:   gv_repid         TYPE syrepid,           "Report ID            "
        gv_flag_exit     TYPE flag.              "Flag-Exit            "

DATA:   gs_variant       TYPE disvariant,        "LVC Display Variant  "
        gs_variant_p     TYPE disvariant.        "LVC Display Variant-Pr

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************
* Select Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS:   s_trans  FOR ptrv_head-datv1.
SELECT-OPTIONS:   s_post   FOR ptrv_head-datv1.
*ELECT-OPTIONS:   SKIP 1.
*ELECT-OPTIONS:   s_antrg  FOR ptrv_perio-antrg.
*ELECT-OPTIONS:   s_abrec  FOR ptrv_perio-abrec.
*ELECT-OPTIONS:   s_uebrf  FOR ptrv_perio-uebrf.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       p_chkbx1 AS CHECKBOX
                           DEFAULT 'X'.
SELECTION-SCREEN: COMMENT  03(50) text-cb1.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       p_chkbx2 AS CHECKBOX
                           DEFAULT 'X'.
SELECTION-SCREEN: COMMENT  03(50) text-cb2.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       p_chkbx3 AS CHECKBOX
                           DEFAULT 'X'.
SELECTION-SCREEN: COMMENT  03(50) text-cb3.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       p_chkbx4 AS CHECKBOX
                           DEFAULT 'X'.
SELECTION-SCREEN: COMMENT  03(50) text-cb4.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       p_chkbx5 AS CHECKBOX
                           DEFAULT 'X'.
SELECTION-SCREEN: COMMENT  03(50) text-cb5.
SELECTION-SCREEN: END   OF LINE.
SELECTION-SCREEN: END   OF BLOCK ssb1.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
PARAMETERS:       p_dspvar TYPE slis_vari.
SELECTION-SCREEN: END   OF BLOCK ssb2.

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

* Set the report id
  CLEAR                                gv_repid.
  MOVE     sy-repid                 TO gv_repid.

* Get the default LVC display variant

  PERFORM  f_lvc_variant_default_get.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dspvar.

* Value request "drop-down" for the LVC display variant

  PERFORM  f_lvc_variant_f4.

AT SELECTION-SCREEN.

* Validate that the LVC display variant exists

  PERFORM  f_lvc_variant_exists.

START-OF-SELECTION.

  PERFORM  get_data.

END-OF-SELECTION.

  PERFORM  prepare_output.

  PERFORM  output_alv_report.

*eject
*&---------------------------------------------------------------------*
*&      Form  f_lvc_variant_default_get
*&---------------------------------------------------------------------*
*       Get the default LVC display variant
*----------------------------------------------------------------------*
FORM f_lvc_variant_default_get.

  CLEAR                                gs_variant.
  CLEAR                                gs_variant_p.
  MOVE     gv_repid                 TO gs_variant-report.
  MOVE     gv_repid                 TO gs_variant_p-report.

  CHECK  ( p_dspvar IS INITIAL ).

* Get the default LVC display variant

  CALL FUNCTION 'LVC_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE        = 'A'
    CHANGING
      CS_VARIANT    = gs_variant_p
    EXCEPTIONS
      WRONG_INPUT   = 1
      NOT_FOUND     = 2
      PROGRAM_ERROR = 3
      OTHERS        = 4.

  IF ( sy-subrc EQ 0 ).
    CLEAR                              p_dspvar.
    MOVE     gs_variant_p-variant   TO p_dspvar.
  ELSE.
*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_lvc_variant_default_get
*eject
*&---------------------------------------------------------------------*
*&      Form  f_lvc_variant_f4
*&---------------------------------------------------------------------*
*       Value request "drop-down" for the LVC display variant
*----------------------------------------------------------------------*
FORM f_lvc_variant_f4.

* Display list of LVC variants

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = gs_variant
      I_SAVE        = 'A'
    IMPORTING
      E_EXIT        = gv_flag_exit
      ES_VARIANT    = gs_variant_p
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.

  IF ( sy-subrc EQ 2 ).
    MESSAGE ID sy-msgid TYPE 'S' NUMBER   sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF ( gv_flag_exit IS INITIAL ).
      CLEAR                            p_dspvar.
      MOVE     gs_variant_p-variant TO p_dspvar.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_lvc_variant_f4
*eject
*&---------------------------------------------------------------------*
*&      Form  f_lvc_variant_exists
*&---------------------------------------------------------------------*
*       Validate that the LVC display variant exists
*----------------------------------------------------------------------*
FORM f_lvc_variant_exists.

  IF ( NOT ( p_dspvar IS INITIAL ) ).

    CLEAR                              gs_variant_p.
    MOVE     gs_variant             TO gs_variant_p.
    CLEAR                              gs_variant_p-variant.
    MOVE     p_dspvar               TO gs_variant_p-variant.

* Check that the LVC display variant exists

    CALL FUNCTION 'LVC_VARIANT_EXISTENCE_CHECK'
      EXPORTING
        I_SAVE        = 'A'
      CHANGING
        CS_VARIANT    = gs_variant_p
      EXCEPTIONS
        WRONG_INPUT   = 1
        NOT_FOUND     = 2
        PROGRAM_ERROR = 3
        OTHERS        = 4.

    IF ( sy-subrc EQ 0 ).
      CLEAR                            gs_variant.
      MOVE     gs_variant_p         TO gs_variant.
    ELSE.
      CLEAR                            gs_variant.
      MOVE     gv_repid             TO gs_variant-report.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CLEAR                              gs_variant.
    MOVE       gv_repid             TO gs_variant-report.
  ENDIF.

ENDFORM.                    " f_lvc_variant_exists
*eject
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Get the data
*----------------------------------------------------------------------*
FORM get_data.

  DATA: lt_ccbel TYPE ptk34_t,
        ls_ccbel TYPE LINE OF ptk34_t,
        lt_inbel TYPE ptk34_inbel_t,
        ls_inbel TYPE LINE OF ptk34_inbel_t,
        lt_trans TYPE ptk34_trans_t,
        ls_trans TYPE ptk34_trans_t,
        ls_letzt TYPE ptk34_letzt,
        lv_subrc TYPE sy-subrc,
        lv_tabix TYPE sy-tabix,
        lv_manager TYPE pernr_d,
        lv_manager_user TYPE username,
        lv_approver TYPE pernr_d,
        lv_approver_user TYPE username,
        lv_datum TYPE sy-datum,
        ls_ptrv_head TYPE ptrv_head,
        ls_ptrv_perio TYPE ptrv_perio,
        lt_ptrv_perio TYPE TABLE OF ptrv_perio,
        lt_ptrv_head TYPE TABLE OF ptrv_head.

  CLEAR: gt_msg.

  SELECT * FROM pa0000 INTO TABLE gt_pa0000
           WHERE endda >= sy-datum.
*    stat2 = '3'
  SORT gt_pa0000 BY pernr.

  DELETE ADJACENT DUPLICATES FROM gt_pa0000 COMPARING pernr.

  LOOP AT gt_pa0000 INTO gs_pa0000.

    CLEAR:  lt_ccbel,
            lt_inbel,
            lt_trans,
            ls_letzt,
            lv_subrc,
            gs_pa0105.

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

    CLEAR: gs_withouttrip,
           gs_with_trip,
           lt_ptrv_perio,
           lt_ptrv_head.

*Process Credit card transations not assigned to TRIP
    LOOP AT lt_ccbel INTO ls_ccbel WHERE bdatu IN s_trans.

      gs_withouttrip-pernr = gs_pa0000-pernr.
      gs_withouttrip-bdatu = ls_ccbel-bdatu.
      gs_withouttrip-betrg = ls_ccbel-loc_amount.
      gs_withouttrip-waers = ls_ccbel-loc_curr.
      gs_withouttrip-c_txt = ls_ccbel-c_txt.
      gs_withouttrip-spkzl = ls_ccbel-spkzl.
      gs_withouttrip-ccard = ls_ccbel-ccard.
      APPEND gs_withouttrip TO gt_withouttrip_e.

    ENDLOOP.

    IF   ( ( p_chkbx2 IS INITIAL ) AND
           ( p_chkbx3 IS INITIAL ) AND
           ( p_chkbx4 IS INITIAL ) AND
           ( p_chkbx5 IS INITIAL )     ).
      RETURN.
    ENDIF.

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

*   CLEAR                                   ls_ptrv_perio.
*   LOOP AT  lt_ptrv_perio             INTO ls_ptrv_perio.
*     lv_tabix = sy-tabix.
*
*     IF   ( ( ls_ptrv_perio-antrg   IS NOT INITIAL ) AND
*            ( ls_ptrv_perio-antrg   NOT IN s_antrg )     ).
*       DELETE lt_ptrv_perio          INDEX lv_tabix.
*       CLEAR  ls_ptrv_perio.
*       CONTINUE.
*     ENDIF.
*     IF   ( ( ls_ptrv_perio-abrec   IS NOT INITIAL ) AND
*            ( ls_ptrv_perio-abrec   NOT IN s_abrec )     ).
*       DELETE lt_ptrv_perio          INDEX lv_tabix.
*       CLEAR  ls_ptrv_perio.
*       CONTINUE.
*     ENDIF.
*     IF   ( ( ls_ptrv_perio-uebrf   IS NOT INITIAL ) AND
*            ( ls_ptrv_perio-uebrf   NOT IN s_uebrf )     ).
*       DELETE lt_ptrv_perio          INDEX lv_tabix.
*       CLEAR  ls_ptrv_perio.
*       CONTINUE.
*     ENDIF.
*
*     CLEAR  ls_ptrv_perio.
*   ENDLOOP.

    LOOP AT lt_inbel INTO ls_inbel WHERE bdatu IN s_trans.

      CLEAR: ls_ptrv_head,
             ls_ptrv_perio.

      gs_with_trip-pernr = gs_pa0000-pernr.
      gs_with_trip-bdatu = ls_inbel-bdatu.
      gs_with_trip-betrg = ls_inbel-loc_amount.
      gs_with_trip-waers = ls_inbel-loc_curr.
      gs_with_trip-c_txt = ls_inbel-c_txt.
      gs_with_trip-spkzl = ls_inbel-spkzl.
      gs_with_trip-reinr = ls_inbel-reinr.
      gs_with_trip-litem = ls_inbel-belnr.
      gs_with_trip-ccard = ls_inbel-ccard.

      READ TABLE lt_ptrv_perio INTO ls_ptrv_perio
           WITH KEY pernr = gs_pa0000-pernr
                    reinr = ls_inbel-reinr.

      READ TABLE lt_ptrv_head INTO ls_ptrv_head
           WITH KEY pernr = gs_pa0000-pernr
                    reinr = ls_inbel-reinr.

      IF ( ls_ptrv_perio-antrg = '3' AND "Completed
         ls_ptrv_perio-abrec = '0' )  OR "Open
        ( ls_ptrv_perio-antrg = '6' AND "Trip Awaiting Docs
         ls_ptrv_perio-abrec = '1' ).    "Settled
        APPEND gs_with_trip TO gt_with_trip_e.
      ENDIF.

      IF ls_ptrv_perio-antrg = '3' AND "Completed
         ls_ptrv_perio-abrec = '1'.    "To Be Settled
        APPEND gs_with_trip TO gt_wait_appr_e. "employee
      ENDIF.

      IF ( ls_ptrv_perio-antrg = '4' AND
         ls_ptrv_perio-abrec = '1' )  OR
        ( ls_ptrv_perio-antrg = '4' AND
         ls_ptrv_perio-abrec = '2' AND
         ls_ptrv_perio-uebrf = '0' )  OR
        ( ls_ptrv_perio-antrg = '4' AND
         ls_ptrv_perio-abrec = '2' AND
         ls_ptrv_perio-uebrf IS INITIAL ).
        APPEND gs_with_trip TO gt_appr_trip.
      ENDIF.

      IF ( ls_ptrv_perio-antrg = '4' AND
         ls_ptrv_perio-abrec = '2' AND
         ls_ptrv_perio-uebrf = '1' ) AND
         ls_ptrv_perio-accdt IN s_post.
        gs_with_trip-pdate = ls_ptrv_perio-ACCDT.
        APPEND gs_with_trip TO gt_post_trip.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " get_data
*eject
*&---------------------------------------------------------------------*
*&      Form  prepare_output
*&---------------------------------------------------------------------*
*       Prepare the output
*----------------------------------------------------------------------*
FORM prepare_output.

  IF     ( p_chkbx1 IS NOT INITIAL ).

    LOOP AT gt_withouttrip_e INTO gs_withouttrip.
      CLEAR w_output.
      w_output-pernr = gs_withouttrip-pernr.
      w_output-dat  = gs_withouttrip-bdatu.
      w_output-amt = gs_withouttrip-betrg.
      w_output-cur = gs_withouttrip-waers.
      w_output-txt = gs_withouttrip-c_txt.
      w_output-exp = gs_withouttrip-spkzl.
      w_output-stat = 'Credit Card Expenses Not Assigned to Expense Rpt'.
      w_output-ccard = gs_withouttrip-ccard.
      APPEND w_output TO t_output.
      CLEAR  gs_withouttrip.
    ENDLOOP.

  ENDIF.

  IF     ( p_chkbx2 IS NOT INITIAL ).

    LOOP AT gt_with_trip_e INTO gs_with_trip.
      CLEAR w_output.
      w_output-pernr = gs_with_trip-pernr.
      w_output-trip = gs_with_trip-reinr.
      w_output-dat  = gs_with_trip-bdatu.
      w_output-amt = gs_with_trip-betrg.
      w_output-cur = gs_with_trip-waers.
      w_output-txt = gs_with_trip-c_txt.
      w_output-exp = gs_with_trip-spkzl.
      w_output-stat = 'Credit Card Expenses On Exp Report Not Submitted'.
      w_output-ccard = gs_with_trip-ccard.
      APPEND w_output TO t_output.
      CLEAR  gs_with_trip.
    ENDLOOP.

  ENDIF.

  IF     ( p_chkbx3 IS NOT INITIAL ).

    LOOP AT gt_wait_appr_e INTO gs_with_trip.
      CLEAR w_output.
      w_output-pernr = gs_with_trip-pernr.
      w_output-trip = gs_with_trip-reinr.
      w_output-dat  = gs_with_trip-bdatu.
      w_output-amt = gs_with_trip-betrg.
      w_output-cur = gs_with_trip-waers.
      w_output-txt = gs_with_trip-c_txt.
      w_output-exp = gs_with_trip-spkzl.
      w_output-stat = 'Credit Card Expenses Pending Manager Approval'.
      w_output-ccard = gs_with_trip-ccard.
      APPEND w_output TO t_output.
      CLEAR  gs_with_trip.
    ENDLOOP.

  ENDIF.

  IF     ( p_chkbx4 IS NOT INITIAL ).

    LOOP AT gt_appr_trip INTO gs_with_trip.
      CLEAR w_output.
      w_output-pernr = gs_with_trip-pernr.
      w_output-trip = gs_with_trip-reinr.
      w_output-dat  = gs_with_trip-bdatu.
      w_output-amt = gs_with_trip-betrg.
      w_output-cur = gs_with_trip-waers.
      w_output-txt = gs_with_trip-c_txt.
      w_output-exp = gs_with_trip-spkzl.
      w_output-stat = 'Credit Card Expenses Approved'.
      w_output-ccard = gs_with_trip-ccard.
      APPEND w_output TO t_output.
      CLEAR  gs_with_trip.
    ENDLOOP.

  ENDIF.

  IF     ( p_chkbx5 IS NOT INITIAL ).

    LOOP AT gt_post_trip INTO gs_with_trip.
      clear: w_output, w_poper, w_bdatj.
      w_output-pernr = gs_with_trip-pernr.
      w_output-trip = gs_with_trip-reinr.
      w_output-dat  = gs_with_trip-bdatu.
      w_output-amt = gs_with_trip-betrg.
      w_output-cur = gs_with_trip-waers.
      w_output-txt = gs_with_trip-c_txt.
      w_output-exp = gs_with_trip-spkzl.
      w_output-stat = 'Credit Card Expenses Posted'.
      w_output-ccard = gs_with_trip-ccard.
      CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
        EXPORTING
          I_DATE         = gs_with_trip-pdate
          I_PERIV        = 'K4'
        IMPORTING
          E_BUPER        = w_poper
          E_GJAHR        = w_bdatj
        EXCEPTIONS
          INPUT_FALSE    = 1
          T009_NOTFOUND  = 2
          T009B_NOTFOUND = 3
          OTHERS         = 4.

      IF sy-subrc = 0.
        CONCATENATE w_poper '/' w_bdatj into w_output-period.
        CONDENSE w_output-period NO-GAPS.
      ENDIF.

*    clear gv_awref.
*    SELECT SINGLE AWREF FROM PTRV_DOC_IT INTO GV_AWREF
*      WHERE EXBEL = gs_with_trip-reinr AND
*            BELNR = gs_with_trip-LITEM.
*    IF SY-SUBRC = 0.
*      SELECT SINGLE BELNR FROM BKPF INTO W_OUTPUT-FIDOC
*        WHERE BLART = 'TV' AND
*              AWTYP = 'TRAVL' AND
*              AWKEY = GV_AWREF.
*    ENDIF.

      APPEND w_output TO t_output.
      CLEAR  gs_with_trip.
    ENDLOOP.

  ENDIF.

  SELECT pernr bukrs ename kostl FROM pa0001 INTO TABLE t_p0001
    FOR ALL ENTRIES IN t_output
    WHERE pernr = t_output-pernr AND
          endda GE sy-datum.

  LOOP AT t_output.
    w_ind = sy-tabix.
    READ TABLE t_p0001 WITH KEY pernr = t_output-pernr.
    IF sy-subrc = 0.
      t_output-bukrs = t_p0001-bukrs.
      t_output-name = t_p0001-ename.
      WRITE t_p0001-kostl TO t_output-kostl NO-ZERO.
*      t_output-kostl = t_p0001-kostl.
    ENDIF.
    MODIFY t_output INDEX w_ind TRANSPORTING name bukrs kostl.
  ENDLOOP.

ENDFORM.                    " prepare_output
*eject
*&---------------------------------------------------------------------*
*&      Form  output_alv_report
*&---------------------------------------------------------------------*
*       Output the ALV report
*----------------------------------------------------------------------*
FORM output_alv_report.
  PERFORM build_fc.
  CLEAR z_layout.
  z_layout-info_fieldname    = 'LINECOLOR'.
  z_layout-colwidth_optimize = 'X'.
  z_layout-detail_popup = 'X'.
  z_layout-numc_sum = 'X'.
  z_layout-get_selinfos = 'X'.
  z_layout-confirmation_prompt = 'X'.
  z_layout-box_rollname = 'S'.
  z_layout-info_fieldname   = 'COLOR_LINE'.

* variant settings
  z_variant-report = sy-repid.
  z_variant-variant = z_varid.

  z_print-no_new_page = 'X'.
* call ALV function to output report

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = z_layout
      it_fieldcat        = t_fc[]
      i_default          = 'X'
      i_save             = 'U'
      is_variant         = gs_variant
      is_print           = z_print
    TABLES
      t_outtab           = t_output.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " output_alv_report
*eject
*&---------------------------------------------------------------------*
*&      Form  build_fc
*&---------------------------------------------------------------------*
*       Build the field catalog
*----------------------------------------------------------------------*
FORM build_fc.
  data: w_count type i.

  REFRESH T_FC. CLEAR T_FC.

  w_count = 0.

  t_fc-FIELDNAME   = 'PERNR'.
  t_fc-SELTEXT_M   = 'Employee #'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'NAME'.
  t_fc-SELTEXT_M   = 'Name'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'CCARD'.
  t_fc-SELTEXT_M   = 'Credit Card'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'BUKRS'.
  t_fc-SELTEXT_M   = 'Company Code'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'TRIP'.
  t_fc-SELTEXT_M   = 'Trip #'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'DAT'.
  t_fc-SELTEXT_M   = 'Date'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'AMT'.
  t_fc-SELTEXT_M   = 'Amount'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'CUR'.
  t_fc-SELTEXT_M   = 'Currency'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'TXT'.
  t_fc-SELTEXT_M   = 'Text'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'EXP'.
  t_fc-SELTEXT_M   = 'Expense'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.
  w_count = w_count + 1.

  t_fc-FIELDNAME   = 'STAT'.
  t_fc-SELTEXT_M   = 'Status'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  w_count = w_count + 1.
  t_fc-FIELDNAME   = 'PERIOD'.
  t_fc-SELTEXT_M   = 'Period'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

  w_count = w_count + 1.
  t_fc-FIELDNAME   = 'KOSTL'.
  t_fc-SELTEXT_M   = 'Cost Center'.
  t_fc-COL_POS     = w_count.
  APPEND t_fc TO t_fc.
  CLEAR  t_fc.

*  w_count = w_count + 1.
*  t_fc-FIELDNAME   = 'FIDOC'.
*  t_fc-SELTEXT_M   = 'FI Document'.
*  t_fc-COL_POS     = w_count.
*  APPEND t_fc TO t_fc.
*  CLEAR  t_fc.
ENDFORM.                    " build_fc
