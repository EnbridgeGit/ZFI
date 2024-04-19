*REPORT ZFFIR007 NO STANDARD PAGE HEADING LINE-COUNT 58 LINE-SIZE 170
*       message-id ZS.
REPORT ZFFIR007.
TYPE-POOLS: SLIS.
*
************************************************************************
*  Author:      M De Meester                                           *
*  Date:        February, 2004                                         *
*  Description:                                                        *
*     - The purpose of this program is to produce a listing of all     *
*        documents as required by the Audit Department.  This report   *
*        is the same display as FB03 with the additional field "Amount"*
* NOTE:  Cannot use BSEG since it causes a timeout error in QA and     *
*        Production.                               mdemeest 2004/04/19 *
************************************************************************
* CHANGES:                                                             *
* Date:      Issue:    By:      Remarks                                *
* 2009/10/22 Mohammad TR582  IFRS - Include BKPF-LDGRP in selection    *
*                            screen, DB data selection and report.     *
*                                                                      *
* 2006/09/14 Mohammad TR285  Include document status field in selection*
*                            screen & report. Change to ALV format.    *
* 2004/06/02           mdemeest Remove SESSION, add DOC Header Text    *
* 2004/04/18           mdemeest New report                             *
************************************************************************

TABLES: bkpf,             "Accounting document header
        sethier,          "STRUCTURE: Set Hierarchy        TR569
        zbseg.            "Accounting document detail      TR569

DATA:
  BEGIN OF itab  OCCURS 0,
   bukrs  LIKE bkpf-bukrs,            "company code
   ldgrp  LIKE bkpf-ldgrp,            "Ledger                   TR581
   gjahr  LIKE bkpf-gjahr,            "fiscal year
   belnr  LIKE bkpf-belnr,            "document number
   bstat  LIKE bkpf-bstat,            "document status          TR285
   blart  LIKE bkpf-blart,            "document type
   waers  LIKE bkpf-waers,            "Currency                 TR285
   tcode  LIKE bkpf-tcode,            "transaction code
   grpid  LIKE bkpf-grpid,            "Batch input session name TR285
   cpudt  LIKE bkpf-cpudt,            "entry date
   bldat  LIKE bkpf-bldat,            "document date
   budat  LIKE bkpf-budat,            "posting date
   monat  LIKE bkpf-monat,            "posting period
   usnam  LIKE bkpf-usnam,            "user name
   bktxt  LIKE bkpf-bktxt,            "doc. header text       2004/06/02
   xblnr  LIKE bkpf-xblnr,            "reference number
   ppnam  LIKE bkpf-ppnam,            "parked by
   checker LIKE bkpf-usnam,           "Checker
   apprv  LIKE bkpf-usnam,            "approve by
   cputm  LIKE bkpf-cputm,            "time of entry
   bvorg  LIKE bkpf-bvorg,           "cross company document number
  END OF itab.

DATA: es_variant    LIKE disvariant,
      is_variant    LIKE disvariant,
      w_head01(60)  TYPE c,
      w_head02(60)  TYPE c.

************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:  s_bukrs FOR bkpf-bukrs  OBLIGATORY MEMORY ID buk,
                 s_belnr FOR bkpf-belnr,
                 s_gjahr FOR bkpf-gjahr  OBLIGATORY DEFAULT sy-datum(4),
                 s_ldgrp FOR bkpf-ldgrp.                         "TR581
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_blart FOR bkpf-blart,
                s_bstat FOR bkpf-bstat,                 "TR285
                s_budat FOR bkpf-budat     OBLIGATORY,
                s_cpudt FOR bkpf-cpudt,
                s_xblnr FOR bkpf-xblnr,
                s_bldat FOR bkpf-bldat,
                s_monat FOR bkpf-monat,
                s_usnam FOR bkpf-usnam    NO INTERVALS,
                s_waers FOR bkpf-waers,
                s_tcode FOR bkpf-tcode,                "Transaction code
                s_grpid FOR bkpf-grpid    NO INTERVALS,
*                s_wrbtr for bsak-wrbtr.
                s_aufnr FOR zbseg-aufnr NO-DISPLAY,
                s_ccgrp FOR sethier-shortname NO INTERVALS NO-DISPLAY,
                s_kostl FOR zbseg-kostl NO-DISPLAY,
                s_bvorg FOR bkpf-bvorg.
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-109.
PARAMETERS:      pvariant LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK box4.

SELECTION-SCREEN END OF BLOCK box.

*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvariant.
  is_variant-report = 'ZFFIR007'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = is_variant
*     I_TABNAME_HEADER    =
*     I_TABNAME_ITEM      =
*     IT_DEFAULT_FIELDCAT =
      i_save              = 'A'
    IMPORTING
*     E_EXIT              =
      es_variant          = es_variant
    EXCEPTIONS
      not_found           = 1
      program_error       = 2
      OTHERS              = 3.
  pvariant = es_variant-variant.

************************************************************************
START-OF-SELECTION.
************************************************************************
*  perform print_selection_screen.

*  perform fi_authorization_check.                   "<<<Ins. C11K918747
  PERFORM fico_authorization_check.                  "TR569

  PERFORM select_from_bkpf.

  PERFORM display_alv_grid_data.

  INCLUDE zfico_auth_check_include.

*----------------------- SELECT_FROM_BKPF ------------------------------
* Select all rows from table BKPF and move info to interal table ITAB
*-----------------------------------------------------------------------
FORM select_from_bkpf.

  DATA:
        lv_objkey  TYPE swotobjid-objkey,
        lv_belnr TYPE bkpf-belnr,
        lv_bukrs TYPE bkpf-bukrs,
        lv_gjahr TYPE bkpf-gjahr,
        lv_ppnam TYPE bkpf-ppnam.

  SELECT bukrs ldgrp gjahr belnr bstat blart waers tcode grpid cpudt
         bldat budat monat usnam bktxt xblnr ppnam cputm bvorg
       FROM bkpf INTO CORRESPONDING FIELDS OF itab
       WHERE bukrs IN s_bukrs
         AND belnr IN s_belnr
         AND gjahr IN s_gjahr
         AND blart IN s_blart
         AND bldat IN s_bldat
         AND budat IN s_budat
         AND monat IN s_monat
         AND cpudt IN s_cpudt
         AND usnam IN s_usnam
         AND tcode IN s_tcode
         AND bstat IN s_bstat                      "TR285
         AND xblnr IN s_xblnr
         AND waers IN s_waers
         AND grpid IN s_grpid
         AND ldgrp IN s_ldgrp                     "TR581
         AND bvorg IN s_bvorg.

    itab-apprv = space.
    itab-checker = space.

    CONCATENATE itab-bukrs itab-belnr itab-gjahr INTO lv_objkey
                                             RESPECTING BLANKS.
    PERFORM get_approver USING lv_objkey
                         CHANGING itab-apprv
                                  itab-checker.
**********
    IF ( itab-ppnam IS INITIAL AND
       itab-bvorg IS NOT INITIAL ). " OR
*       ( itab-apprv IS INITIAL AND
*       itab-bvorg IS NOT INITIAL ).
      lv_belnr = itab-bvorg(10).
      lv_bukrs = itab-bvorg+10(4).
      lv_gjahr = itab-gjahr. "bvorg+14(2).
      if lv_belnr <> itab-belnr AND
         lv_bukrs <> itab-bukrs.
         CLEAR lv_ppnam.
         SELECT SINGLE ppnam INTO lv_ppnam FROM bkpf
             WHERE bukrs = lv_bukrs
               AND belnr = lv_belnr
               AND gjahr = lv_gjahr.
         itab-ppnam = lv_ppnam.
         itab-apprv = space.
         CONCATENATE lv_bukrs lv_belnr lv_gjahr INTO lv_objkey
                                               RESPECTING BLANKS.
         PERFORM get_approver USING lv_objkey
                           CHANGING itab-apprv
                                    itab-checker.
      endif.
    ENDIF.
**********
    APPEND itab.
  ENDSELECT.

  SORT itab BY bukrs gjahr bstat blart.

ENDFORM.                    "select_from_bkpf
*-----------------------------------------------------------------------
*                     DISPLAY_ALV_GRID_DATA.
*-----------------------------------------------------------------------
FORM display_alv_grid_data.

  DATA: fieldcat TYPE slis_t_fieldcat_alv,
        fc_str   TYPE slis_fieldcat_alv,
        layout   TYPE slis_layout_alv,
        title    TYPE lvc_title,
        repid    LIKE sy-repid,
        variant  LIKE disvariant,
        sort     TYPE slis_t_sortinfo_alv,
        sort_str TYPE slis_sortinfo_alv.

  MOVE text-clt  TO w_head02+0(7).
  MOVE sy-sysid  TO w_head02+8(5).
  MOVE sy-mandt  TO w_head02+14(4).
  MOVE text-dte  TO w_head02+21(5).
  WRITE sy-datum TO w_head02+27(10).
  MOVE text-tme  TO w_head02+40(5).
  WRITE sy-uzeit TO w_head02+46(10).

*  CONCATENATE TEXT-010
  repid = sy-repid.
  layout-colwidth_optimize = 'X'.   "Adjust column width as per data
  layout-get_selinfos      = 'X'.   "Print Selection Screen
  layout-zebra = 'X'.               "Striped Pattern
  variant-report = repid.
  variant-variant = pvariant.
  REFRESH fieldcat.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = repid
      i_internal_tabname     = 'ITAB'
      i_inclname             = repid
    CHANGING
      ct_fieldcat            = fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
  LOOP AT fieldcat INTO fc_str.

    CASE fc_str-fieldname.
      WHEN 'BUKRS'.
        fc_str-ddictxt = 'L'.
        fc_str-key    = ' '.                 " Key columns -not first
      WHEN 'LDGRP'.                                            "TR581
        fc_str-ddictxt = 'L'.
        fc_str-key    = ' '.                 " Key columns -not first
      WHEN 'GJAHR'.
        fc_str-ddictxt = 'L'.
        fc_str-key    = ' '.                 " Key columns -not first
      WHEN 'BELNR'.
        fc_str-ddictxt = 'L'.
        fc_str-key    = ' '.                 " Key columns -not first
      WHEN 'BSTAT'.
        fc_str-ddictxt = 'L'.
      WHEN 'BLART'.
        fc_str-ddictxt = 'L'.
      WHEN 'BLART'.
        fc_str-ddictxt = 'L'.
      WHEN 'WAERS'.
        fc_str-ddictxt = 'L'.
      WHEN 'GRPID'.
        fc_str-ddictxt = 'L'.
        fc_str-seltext_l = text-c07.          " Alternative colheader
      WHEN 'CPUDT'.
        fc_str-ddictxt = 'L'.                 " Use Large system text
*          FC_STR-KEY    = ' '.               " Key columns -not first
      WHEN 'BLDAT'.
        fc_str-ddictxt = 'L'.
      WHEN 'BUDAT'.
        fc_str-ddictxt = 'L'.
      WHEN 'MONAT'.
        fc_str-ddictxt = 'L'.                 " Use Large system text
      WHEN 'USNAM'.
        fc_str-ddictxt = 'L'.                 " Use Large system text
      WHEN 'BKTXT'.
        fc_str-ddictxt = 'L'.                 " Use Large system text
      WHEN 'XBLNR'.
        fc_str-ddictxt = 'L'.                 " Use Large system text
*          FC_STR-DO_SUM  = 'X'.                 " Do Sum
* fc_str-no_out = 'X'.           " hide column
      WHEN 'APPRV'.
        fc_str-seltext_l = 'Approver'.
        fc_str-seltext_m = 'Approver'.
        fc_str-seltext_s = 'Approver'.
        fc_str-ddictxt = 'L'.
        fc_str-text_fieldname = 'Approver'.
        fc_str-reptext_ddic = 'Approver'.
     WHEN 'CHECKER'.
        fc_str-seltext_l = 'Checker'.
        fc_str-seltext_m = 'Checker'.
        fc_str-seltext_s = 'Checker'.
        fc_str-ddictxt = 'L'.
        fc_str-text_fieldname = 'Checker'.
        fc_str-reptext_ddic = 'Checker'.
    ENDCASE.

    MODIFY fieldcat FROM fc_str.
  ENDLOOP.

*APPEND LAYOUT.

* Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat             = fieldcat
      is_layout               = layout
      i_callback_top_of_page  = 'ALV_TOP_OF_PAGE'
      i_callback_program      = repid
      i_save                  = 'A'
      is_variant              = variant
      it_sort                 = sort
*     I_GRID_TITLE            = TITLE
*     I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
      t_outtab                = itab
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

ENDFORM.                    "DISPLAY_ALV_GRID_DATA

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM alv_top_of_page.
  DATA: ls_line TYPE slis_listheader.
  DATA: lt_top_of_page TYPE slis_t_listheader.
  DATA: f_length     TYPE i,
        kount        TYPE  i,
        head_info_01 TYPE string,
        head_info_02 TYPE string,
        from_date(11) TYPE c,
        to_date(11)   TYPE c.

  MOVE sy-title+4(50) TO head_info_01.
*1- HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = head_info_01.             "sy-title.
*  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND ls_line TO lt_top_of_page.

*line 1:
  CLEAR ls_line.
  ls_line-typ   = 'H'.
  ls_line-key   = ''.
  ls_line-info  = w_head02.
  APPEND ls_line TO lt_top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.

ENDFORM.                    "ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  GET_APPROVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_approver  USING    iv_objkey TYPE swotobjid-objkey
                   CHANGING cv_apprv  TYPE bkpf-usnam
                            cv_check  TYPE bkpf-usnam.

  DATA: lv_wid    TYPE sww_contob-wi_id,
        lt_swr_wihdr  TYPE TABLE OF swr_wihdr,
        ls_swr_wihdr  TYPE swr_wihdr,
        lv_objtype TYPE swotobjid-objtype VALUE 'FIPP',
        lv_subrc  TYPE sy-subrc,
        lt_container TYPE TABLE OF swr_cont,
        ls_container TYPE swr_cont.

  CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
    EXPORTING
      objtype                  = lv_objtype
      objkey                   = iv_objkey
      top_level_items          = 'X'
      selection_status_variant = 0000    "All instances of workflow
    TABLES
      worklist                 = lt_swr_wihdr.

  DELETE lt_swr_wihdr WHERE wi_rh_task <> 'WS02000013'.
  "'WS90400001'. from sbox
  IF lt_swr_wihdr[] IS NOT INITIAL.
    READ TABLE lt_swr_wihdr INTO ls_swr_wihdr INDEX 1.
    lv_wid = ls_swr_wihdr-wi_id.
    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id      = lv_wid
      IMPORTING
        return_code      = lv_subrc
      TABLES
        simple_container = lt_container.
    LOOP AT lt_container INTO ls_container.
      CASE ls_container-element.
        WHEN 'APPROVEDBY'.
          cv_apprv = ls_container-value+2.
        WHEN 'FIRSTAPPROVER'.
          cv_check = ls_container-value+2.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_APPROVER
