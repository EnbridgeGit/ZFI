REPORT  zfglr026_ic_shadoword_rec.
*&---------------------------------------------------------------------*
*& Report  zfglr026_ic_shadoword_rec                                   *
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
TABLES: cobk, aufk.

TYPES:  BEGIN OF ty_report,
          kokrs LIKE cobk-kokrs,
          gjahr   LIKE cobk-gjahr,
          budat   LIKE cobk-budat,
          belnr   LIKE cobk-belnr,
          blart   LIKE cobk-blart,
          bltxt   LIKE cobk-bltxt,
          buzei   LIKE coep-buzei,
          kstar   LIKE coep-kstar,
          wtgbtr  LIKE coep-wtgbtr,
          sgtxt   LIKE coep-sgtxt,
          aufnr   LIKE aufk-aufnr,
          user0   LIKE aufk-user0,
          user1   LIKE aufk-user1,
          user2   LIKE aufk-user2,
          user3   LIKE aufk-user3,
        END OF ty_report.


DATA:   gs_report   TYPE ty_report,
        gt_report   LIKE TABLE OF gs_report,

        ls_cobk     TYPE cobk,
        lt_cobk     TYPE TABLE OF cobk,

        ls_coep     TYPE coep,
        lt_coep     TYPE TABLE OF coep,

        ls_aufk     TYPE aufk,
        lt_aufk     TYPE TABLE OF aufk,

        lv_aufnr    TYPE aufk-aufnr,

        gv_return   TYPE integer,

        gv_lines    TYPE integer,
        gv_percent  TYPE integer,
        gv_mod      TYPE integer,
        gv_curper   TYPE integer.



SELECTION-SCREEN BEGIN OF BLOCK a1.

PARAMETER:
            p_kokrs TYPE cobk-kokrs OBLIGATORY,
            p_versn TYPE cobk-versn DEFAULT 0 OBLIGATORY,
            p_gjahr TYPE cobk-gjahr OBLIGATORY.

SELECT-OPTIONS:
            s_budat FOR cobk-budat OBLIGATORY.

PARAMETER:
            p_blart TYPE cobk-blart OBLIGATORY.

SELECT-OPTIONS:
            s_aufnr for aufk-aufnr OBLIGATORY.

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

  SELECT *
    FROM cobk
    INTO CORRESPONDING FIELDS OF TABLE lt_cobk
    WHERE kokrs = p_kokrs
      AND versn = p_versn
      AND gjahr = p_gjahr
      AND budat IN s_budat
      AND blart = p_blart.


  "Get the percentage line amount
  DESCRIBE TABLE lt_cobk LINES gv_lines.
  gv_percent = gv_lines DIV 20.
  IF gv_percent = 0.
    gv_percent = 1.
  ENDIF.

  LOOP AT lt_cobk INTO ls_cobk.
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


    "Get matching COEP data
    SELECT *
      FROM coep
      INTO CORRESPONDING FIELDS OF TABLE lt_coep
      WHERE kokrs = ls_cobk-kokrs
        AND belnr = ls_cobk-belnr.

    IF sy-subrc <> 0.
      "No line item found
      CONTINUE.
    ENDIF.

    LOOP AT lt_coep INTO ls_coep.
      "Only process Order Entries
      IF ls_coep-objnr(2) = 'OR'.
        lv_aufnr = ls_coep-objnr+2(12).

        SELECT SINGLE *
          FROM aufk
          INTO CORRESPONDING FIELDS OF ls_aufk
          WHERE aufnr = lv_aufnr
            AND aufnr in s_aufnr.

        IF sy-subrc = 0.
          "Something found.
          gs_report-kokrs   = ls_cobk-kokrs.
          gs_report-gjahr   = ls_cobk-gjahr.
          gs_report-budat   = ls_cobk-budat.
          gs_report-belnr   = ls_cobk-belnr.
          gs_report-blart   = ls_cobk-blart.
          gs_report-bltxt   = ls_cobk-bltxt.
          gs_report-buzei   = ls_coep-buzei.
          gs_report-kstar   = ls_coep-kstar.
          gs_report-wtgbtr  = ls_coep-wtgbtr.
          gs_report-sgtxt   = ls_coep-sgtxt.
          gs_report-aufnr   = ls_aufk-aufnr.
          gs_report-user0   = ls_aufk-user0.
          gs_report-user1   = ls_aufk-user1.
          gs_report-user2   = ls_aufk-user2.
          gs_report-user3   = ls_aufk-user3.
          APPEND gs_report TO gt_report.
        ENDIF. "subrc = 0 (AUFK lookup)
      ENDIF. "objnr(2) = 'OR'
    ENDLOOP. "lt_coep
  ENDLOOP. "lt_cobk
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
  ls_fieldcat-fieldname   = 'KOKRS'.
  ls_fieldcat-seltext_m   = 'Co Area'.
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
  ls_fieldcat-fieldname   = 'BUDAT'.
  ls_fieldcat-seltext_m   = 'Post Date'.
  ls_fieldcat-col_pos     = 4.
  "ls_fieldcat-outputlen   = 8.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'BLART'.
  ls_fieldcat-seltext_m   = 'Type'.
  ls_fieldcat-col_pos     = 5.
  "ls_fieldcat-outputlen   = 5.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'BLTXT'.
  ls_fieldcat-seltext_m   = 'Header Text'.
  ls_fieldcat-col_pos     = 6.
  ls_fieldcat-outputlen   = 20.
  "ls_fieldcat-hotspot     = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'KSTAR'.
  ls_fieldcat-seltext_m   = 'Cost Element'.
  ls_fieldcat-col_pos     = 7.
  "ls_fieldcat-outputlen   = 4.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'WTGBTR'.
  ls_fieldcat-seltext_m   = 'Value'.
  ls_fieldcat-col_pos     = 8.
  "ls_fieldcat-outputlen   = 6.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'SGTXT'.
  ls_fieldcat-seltext_m   = 'Segment text'.
  ls_fieldcat-col_pos     = 9.
  ls_fieldcat-outputlen   = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'AUFNR'.
  ls_fieldcat-seltext_m   = 'Order Num'.
  ls_fieldcat-col_pos     = 10.
  "ls_fieldcat-outputlen   = 8.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'USER0'.
  ls_fieldcat-seltext_m   = 'SYS'.
  ls_fieldcat-col_pos     = 11.
  "ls_fieldcat-outputlen   = 13.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'USER1'.
  ls_fieldcat-seltext_m   = 'CoCode'.
  ls_fieldcat-col_pos     = 12.
  "ls_fieldcat-outputlen   = 13.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'USER2'.
  ls_fieldcat-seltext_m   = 'Type'.
  ls_fieldcat-col_pos     = 13.
  "ls_fieldcat-outputlen   = 13.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname   = 'USER3'.
  ls_fieldcat-seltext_m   = 'Object'.
  ls_fieldcat-col_pos     = 14.
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
