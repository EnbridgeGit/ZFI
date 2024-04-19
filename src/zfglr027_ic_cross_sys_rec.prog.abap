REPORT  zfglr027_ic_cross_sys_rec.
*&---------------------------------------------------------------------*
*& Report  zfglr026_ic_cross_sys_rec                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*  Author:      Brian Boundy                                           *
*  Date:        May, 2013.                                             *
*  Description: This will show all EEM IC data for Reconcilitation     *
*                                                                      *
*                                                                      *
*&---------------------------------------------------------------------*
*CHANGES****                                                           *
*                                                                      *
*                                                                      *
*&---------------------------------------------------------------------*
"TYPE-POOLS: slis.

TYPES:  BEGIN OF ty_report,
          bukrs LIKE bseg-bukrs,
          belnr LIKE bseg-belnr,
          gjahr LIKE bseg-gjahr,
          buzei LIKE bseg-buzei,
          bschl LIKE bseg-bschl,
          zuonr LIKE bseg-zuonr,
          sgtxt LIKE bseg-sgtxt,
          hkont LIKE bseg-hkont,
          wrbtr LIKE bseg-wrbtr,
          waers LIKE bkpf-waers,
          kostl LIKE bseg-kostl,
          aufnr LIKE bseg-aufnr,
          poski LIKE prps-poski,
          prctr LIKE bseg-prctr,
          nplnr LIKE bseg-nplnr,
          aufpl LIKE bseg-aufpl,
          xref1 LIKE bseg-xref1,
        END OF ty_report.


DATA:   gs_report   TYPE ty_report,
        gt_report   LIKE TABLE OF gs_report,

        ls_bkpf     TYPE bkpf,
        lt_bkpf     TYPE TABLE OF bkpf,

        ls_bseg     TYPE bseg,
        lt_bseg     TYPE TABLE OF bseg,

        ls_prps     TYPE prps,
        lt_prps     TYPE TABLE OF prps,

        ls_zvarsys  TYPE zvarsys,
        lt_zvarsys  TYPE TABLE OF zvarsys,

        gv_return   TYPE integer,

        gv_lines    TYPE integer,
        gv_percent  TYPE integer,
        gv_mod      TYPE integer,
        gv_curper   TYPE integer.


SELECTION-SCREEN BEGIN OF BLOCK a1.

PARAMETER:
            p_blart   LIKE bkpf-blart OBLIGATORY,
            p_gjahr   LIKE bkpf-gjahr OBLIGATORY,
            p_budats  LIKE bkpf-budat OBLIGATORY,
            p_budate  LIKE bkpf-budat OBLIGATORY,
            p_xblnr   LIKE bkpf-xblnr.

SELECTION-SCREEN END OF BLOCK a1.



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

*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      percentage = 1
*      text       = 'Selecting Data'.


  SELECT * FROM zvarsys
    INTO CORRESPONDING FIELDS OF TABLE lt_zvarsys
    WHERE programm = 'LOGSYS'
      AND varname LIKE 'ECC%%_RFC'.

  LOOP AT lt_zvarsys INTO ls_zvarsys.
    CLEAR: lt_bkpf, lt_bseg.

        CALL FUNCTION 'ZFI_IC_POSTED_DOCS' DESTINATION ls_zvarsys-value1
          EXPORTING
            im_blart  = p_blart
            im_gjahr  = p_gjahr
            im_budats = p_budats
            im_budate = p_budate
            im_xblnr  = p_xblnr
          TABLES
            tb_bkpf   = lt_bkpf
            tb_bseg   = lt_bseg
            tb_prps   = lt_prps
          EXCEPTIONS
            others    = 1.
      if sy-subrc = 1.
        "Error calling function.
        CONTINUE.
      endif.


*  "Get the percentage line amount
*  DESCRIBE TABLE lt_cobk LINES gv_lines.
*  gv_percent = gv_lines DIV 20.
*  IF gv_percent = 0.
*    gv_percent = 1.
*  ENDIF.

    LOOP AT lt_bseg INTO ls_bseg.
*    "Update the progress indicator
*    gv_mod = sy-tabix MOD gv_percent.
*    IF gv_mod = 0.
*      gv_curper = sy-tabix DIV gv_percent.
*      gv_curper = gv_curper * 5.
*      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*        EXPORTING
*          percentage = gv_curper
*          text       = 'Getting additional Detail'.
*    ENDIF.

      CLEAR gs_report.

      gs_report-bukrs = ls_bseg-bukrs.
      gs_report-belnr = ls_bseg-belnr.
      gs_report-gjahr = ls_bseg-gjahr.
      gs_report-buzei = ls_bseg-buzei.
      gs_report-bschl = ls_bseg-bschl.
      gs_report-zuonr = ls_bseg-zuonr.
      gs_report-sgtxt = ls_bseg-sgtxt.
      gs_report-hkont = ls_bseg-hkont.
      gs_report-wrbtr = ls_bseg-wrbtr.
      gs_report-kostl = ls_bseg-kostl.
      gs_report-aufnr = ls_bseg-aufnr.
      "gs_report-projk = ls_bseg-projk.
      gs_report-prctr = ls_bseg-prctr.
      gs_report-nplnr = ls_bseg-nplnr.
      gs_report-aufpl = ls_bseg-aufpl.
      gs_report-xref1 = ls_bseg-xref1.

      READ TABLE lt_bkpf INTO ls_bkpf WITH KEY bukrs = ls_bseg-bukrs
                                               belnr = ls_bseg-belnr
                                               gjahr = ls_bseg-gjahr.

      IF sy-subrc = 0.
        gs_report-waers = ls_bkpf-waers.
      ENDIF. "sy-subrc

      READ TABLE lt_prps INTO ls_prps WITH KEY pspnr = ls_bseg-projk.

      IF sy-subrc = 0.
        gs_report-poski = ls_prps-poski.
      ENDIF. "sy-subrc


      APPEND gs_report TO gt_report.
    ENDLOOP. "lt_bseg
  ENDLOOP. "lt_zvarsys
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
  ls_fieldcat-fieldname   = 'BUKRS'.
  ls_fieldcat-seltext_m   = 'Co Code'.
  ls_fieldcat-col_pos     = 0.
  "ls_fieldcat-emphasize   = 'X'.
  ls_fieldcat-key         = 'X'.
  "ls_fieldcat-hotspot     = 'X'.
  "ls_fieldcat-outputlen   = 10.
  "ls_fieldcat-do_sum      = 'X'.
  "ls_fieldcat-no_zero     = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'BELNR'.
  ls_fieldcat-seltext_m   = 'Doc Num'.
  ls_fieldcat-col_pos     = 1.
  ls_fieldcat-key         = 'X'.
  "ls_fieldcat-outputlen   = 6.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'BUZEI'.
  ls_fieldcat-seltext_m   = 'Line Number'.
  ls_fieldcat-col_pos     = 2.
  ls_fieldcat-key         = 'X'.
  "ls_fieldcat-outputlen   = 20.
  "ls_fieldcat-hotspot     = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'GJAHR'.
  ls_fieldcat-seltext_m   = 'Year'.
  ls_fieldcat-col_pos     = 3.
  "ls_fieldcat-outputlen   = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'BSCHL'.
  ls_fieldcat-seltext_m   = 'Post Key'.
  ls_fieldcat-col_pos     = 4.
  "ls_fieldcat-outputlen   = 8.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'ZUONR'.
  ls_fieldcat-seltext_m   = 'Assignment Num'.
  ls_fieldcat-col_pos     = 5.
  "ls_fieldcat-outputlen   = 5.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'SGTXT'.
  ls_fieldcat-seltext_m   = 'Segment text'.
  ls_fieldcat-col_pos     = 6.
  ls_fieldcat-outputlen   = 20.
  "ls_fieldcat-hotspot     = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'HKONT'.
  ls_fieldcat-seltext_m   = 'GL'.
  ls_fieldcat-col_pos     = 7.
  "ls_fieldcat-outputlen   = 4.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'WRBTR'.
  ls_fieldcat-seltext_m   = 'Amount'.
  ls_fieldcat-col_pos     = 8.
  "ls_fieldcat-outputlen   = 6.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'WAERS'.
  ls_fieldcat-seltext_m   = 'Cur.'.
  ls_fieldcat-col_pos     = 9.
  "ls_fieldcat-outputlen   = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'KOSTL'.
  ls_fieldcat-seltext_m   = 'Cost Center'.
  ls_fieldcat-col_pos     = 10.
  "ls_fieldcat-outputlen   = 13.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'AUFNR'.
  ls_fieldcat-seltext_m   = 'Order'.
  ls_fieldcat-col_pos     = 11.
  "ls_fieldcat-outputlen   = 13.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'POSKI'.
  ls_fieldcat-seltext_m   = 'WBS'.
  ls_fieldcat-col_pos     = 12.
  "ls_fieldcat-outputlen   = 13.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'PRCTR'.
  ls_fieldcat-seltext_m   = 'Profit Center'.
  ls_fieldcat-col_pos     = 13.
  "ls_fieldcat-outputlen   = 15.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'NPLNR'.
  ls_fieldcat-seltext_m   = 'Network'.
  ls_fieldcat-col_pos     = 14.
  "ls_fieldcat-outputlen   = 15.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'AUFPL'.
  ls_fieldcat-seltext_m   = 'Operation'.
  ls_fieldcat-col_pos     = 15.
  "ls_fieldcat-outputlen   = 15.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'XREF1'.
  ls_fieldcat-seltext_m   = 'Reference'.
  ls_fieldcat-col_pos     = 16.
  "ls_fieldcat-outputlen   = 15.
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
      t_outtab                = gt_report
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

ENDFORM.                    "display_alv


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
