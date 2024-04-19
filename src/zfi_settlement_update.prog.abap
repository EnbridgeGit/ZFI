*&---------------------------------------------------------------------*
*& Report  ZPS_SETTLEMENT_UPDATE
*&DESC: CRETAED A PROGRAM TO UPDATE SETTLEMENT RULE
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFI_SETTLEMENT_UPDATE.
TYPE-POOLS:SLIS.
tables:prps.

**--SELECTION SCREEN
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:     P_PBUKR LIKE PRPS-PBUKR DEFAULT 'UGL'.
SELECT-OPTIONS:s_pspnr FOR prps-pspnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:cb_test type c as CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.
TYPES: BEGIN OF TY_PRPS,
       PSPNR TYPE PS_POSNR,
       OBJNR TYPE J_OBJNR,
       PBUKR TYPE PS_PBUKR,
       END OF TY_PRPS,
       BEGIN OF ty_jest,
       objnr type J_OBJNR,
       stat  type J_STATUS,
       INACT type J_INACT,
       END OF ty_jest.
TYPES: BEGIN OF TY_FINAL,
       sno(100) type c,
       PSPNR TYPE PS_POSNR,
       OBJNR TYPE J_OBJNR,
        LFDNR TYPE BR_LFDNR,
       PERBZ_O TYPE PERBZ_LD,
       PERBZ_N TYPE PERBZ_LD,
       msg     type string,
       END OF TY_FINAL,
       BEGIN OF TY_COBRB,
         OBJNR TYPE J_OBJNR,
         BUREG TYPE BRSGR,
         LFDNR TYPE BR_LFDNR,
         PERBZ TYPE PERBZ_LD,
       END OF  TY_COBRB.
DATA:GT_FINAL TYPE TABLE OF TY_FINAL,
     GS_FINAL TYPE TY_FINAL.
DATA:GT_PRPS TYPE TABLE OF TY_PRPS,
     GS_PRPS TYPE TY_PRPS,
     LV_AUC TYPE FLAG,
     LV_STAT TYPE FLAG,
     gt_jest type TABLE OF ty_jest,
     gs_jest type ty_jest,
     GT_COBRB TYPE TABLE OF COBRB,
     LV_INDEX TYPE SY-INDEX,
     GT_COBRB1 TYPE TABLE OF COBRB,
     GT_COBRB2 TYPE TABLE OF COBRB,
     GT_COBRB_T TYPE TABLE OF TY_COBRB,
     GS_COBRB_T TYPE TY_COBRB,
     GS_COBRB TYPE COBRB,
     GS_COBRB2 TYPE COBRB,
     lv_index9 type sy-index,
     gt_fieldcatalog type slis_t_fieldcat_alv,
     gs_fieldcatalog like LINE OF gt_fieldcatalog.

START-OF-SELECTION.
  clear:gt_prps[],gt_cobrb[],gt_cobrb1[],gt_cobrb2[],gt_cobrb_t[].
  SELECT PSPNR OBJNR PBUKR FROM PRPS INTO TABLE GT_PRPS
                           WHERE PSPNR IN S_PSPNR
                           AND   PBUKR EQ P_PBUKR.
  IF SY-SUBRC IS INITIAL.
    SELECT * from cobrb INTO TABLE gt_cobrb FOR ALL ENTRIES IN
                                   GT_PRPS
                             WHERE OBJNR = GT_PRPS-OBJNR.
    IF sy-subrc is INITIAL.
      GT_COBRB2[] = GT_COBRB[].
      GT_COBRB1[] = GT_COBRB[].
      SORT GT_COBRB1 BY OBJNR.
      DELETE ADJACENT DUPLICATES FROM GT_COBRB1 COMPARING OBJNR.
      IF GT_COBRB1[] IS NOT INITIAL.
        SELECT objnr stat inact from jest into table gt_jest FOR ALL ENTRIES IN
                                     gt_cobrb1
                                where objnr = gt_cobrb1-objnr
                                and   ( stat = 'I0045' OR STAT = 'I0046' OR STAT = 'I0163' )
                                and   inact  eq space.
        IF sy-subrc is INITIAL.
          sort gt_jest by objnr.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  sort gt_prps by objnr.
  sort gt_jest by objnr.
  sort gt_cobrb by objnr.
  loop at gt_prps into gs_prps.
    READ TABLE gt_jest into gs_jest with key objnr = gs_prps-objnr BINARY SEARCH.
    IF sy-subrc is INITIAL.
      lv_index = sy-tabix.
      LOOP AT gt_jest into gs_jest from lv_index.
        IF gs_prps-objnr <> gs_jest-objnr.
          exit.
        ENDIF.
        IF gs_jest-stat = 'I0163'.
          lv_auc = 'X'.
        ELSEIF gs_jest-stat = 'I0046' OR gs_jest-stat = 'I0045'.
          lv_stat = 'X'.
        ENDIF.
        IF  LV_AUC = 'X' AND LV_STAT = 'X'.
          LOOP AT gt_cobrb into gs_cobrb where objnr eq gs_jest-objnr.
            IF gs_cobrb-PERBZ = 'JHR'.
              gs_cobrb-PERBZ = 'GES'.
*              gs_final-perbz_n = gs_cobrb-PERBZ.
*              gs_final-msg = 'Settlemnt type updated'.
*            else.
*              gs_final-msg =  'Same Settlemnt type'.
            ENDIF.
*            gs_final-pspnr = gs_prps-pspnr.
*            gs_final-objnr = gs_cobrb-objnr.
*            gs_final-lfdnr = gs_cobrb-lfdnr.
*            IF gs_final-perbz_n is INITIAL.
*            gs_final-perbz_n = gs_cobrb-perbz.
*            ENDIF.
*            gs_final-perbz_o = gs_cobrb-perbz.
*            lv_index9 = lv_index9 + 1.
*            gs_final-sno = lv_index9.
*            APPEND gs_final to gt_final.
            MODIFY GT_COBRB FROM GS_COBRB TRANSPORTING PERBZ.
            CLEAR:GS_COBRB,gs_final.
          ENDLOOP.
        ENDIF.
        CLEAR:GS_JEST.
      ENDLOOP.
      CLEAR:LV_AUC,LV_STAT.
    ENDIF.
    CLEAR:GS_PRPS,LV_INDEX.
  endloop.
  if cb_test is not INITIAL.
    LOOP AT GT_COBRB2 INTO GS_COBRB2.
      READ TABLE GT_COBRB INTO GS_COBRB WITH KEY OBJNR = GS_COBRB-OBJNR
                                                     BUREG = GS_COBRB-BUREG
                                                     LFDNR = GS_COBRB-LFDNR.
      IF sy-subrc is INITIAL.
        if gs_cobrb-perbz eq 'GES'.
          GS_FINAL-MSG = 'Record updated sucessfully'.
          gs_final-perbz_n = gs_cobrb-perbz.
          gs_final-perbz_o = gs_cobrb2-perbz.
        else.
          gs_final-msg = 'Record not updated sucessfully'.
          gs_final-perbz_n = gs_cobrb-perbz.
          gs_final-perbz_o = gs_cobrb2-perbz.
        ENDIF.
      else.
        gs_final-msg = 'Record not updated sucessfully'.
        gs_final-perbz_n = gs_cobrb2-perbz.
        gs_final-perbz_o = gs_cobrb2-perbz.
      ENDIF.
      lv_index9 = lv_index9 + 1.
      gs_final-sno = lv_index9.
      gs_final-objnr = gs_cobrb2-objnr.
      GS_FINAL-LFDNR = GS_COBRB2-LFDNR.
      READ TABLE GT_PRPS INTO GS_PRPS WITH KEY OBJNR = GS_COBRB2-OBJNR.
      IF SY-SUBRC IS INITIAL.
        GS_FINAL-PSPNR = GS_PRPS-PSPNR.
        CLEAR:GS_PRPS.
      ENDIF.
      append gs_final to gt_final.
      clear:gs_final,gs_cobrb,gs_cobrb2.
    endloop.
  endif.
  IF GT_COBRB[] IS NOT INITIAL.
    CALL FUNCTION 'K_SRULE_SAVE_UTASK' IN UPDATE TASK
      TABLES
*       T_COBRA_INSERT    =
*       T_COBRA_UPDATE    =
*       T_COBRA_DELETE    =
*       T_COBRB_INSERT    = GT_INSERT
        T_COBRB_UPDATE    = GT_COBRB
*       T_COBRB_DELETE    =
      EXCEPTIONS
        SRULE_UTASK_ERROR = 1
        OTHERS            = 2.
    IF sy-subrc is INITIAL.
      if cb_test is INITIAL.
        commit WORK.
      endif.
    ENDIF.
  ENDIF.
  if cb_test is INITIAL.
    SELECT OBJNR BUREG LFDNR PERBZ FROM COBRB INTO TABLE GT_COBRB_T FOR ALL ENTRIES IN GT_COBRB2
                                             WHERE OBJNR = GT_COBRB2-OBJNR.

    IF SY-SUBRC IS INITIAL.
      SORT GT_COBRB_T BY OBJNR.
    ENDIF.

    LOOP AT GT_COBRB2 INTO GS_COBRB.
      READ TABLE GT_COBRB_T INTO GS_COBRB_T WITH KEY OBJNR = GS_COBRB-OBJNR
                                                     BUREG = GS_COBRB-BUREG
                                                     LFDNR = GS_COBRB-LFDNR.
      IF SY-SUBRC IS INITIAL.
        GS_FINAL-PERBZ_N = GS_COBRB_T-PERBZ.
      ENDIF.
      IF GS_COBRB-PERBZ EQ GS_COBRB_T-PERBZ.
        IF GS_COBRB_T-PERBZ eq 'GES'.
          GS_FINAL-MSG = 'Same Settlement type'.
        else.
          gs_final-msg = 'Settlement type not updated'.
        ENDIF.
      ELSEIF GS_COBRB-PERBZ ne GS_COBRB_T-PERBZ.
        IF GS_COBRB_T-PERBZ eq 'GES'.
          GS_FINAL-MSG = 'Settlement type changed to Full Settlement'.
        else.
          gs_final-msg = 'Settlement type not updated'.
        ENDIF.
      ENDIF.
      GS_FINAL-PERBZ_O = GS_COBRB-PERBZ.
      GS_FINAL-OBJNR = GS_COBRB-OBJNR.
      GS_FINAL-LFDNR = GS_COBRB-LFDNR.
      READ TABLE GT_PRPS INTO GS_PRPS WITH KEY OBJNR = GS_COBRB-OBJNR.
      IF SY-SUBRC IS INITIAL.
        GS_FINAL-PSPNR = GS_PRPS-PSPNR.
        CLEAR:GS_PRPS.
      ENDIF.
      lv_index9 = lv_index9 + 1.
      gs_final-sno = lv_index9.
      APPEND GS_FINAL TO GT_FINAL.

      CLEAR:GS_COBRB,gs_final,GS_COBRB_T.
    ENDLOOP.
    clear:lv_index9.
  endif.

END-OF-SELECTION.

  PERFORM f_display.
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DISPLAY .

  PERFORM f_BUILD_FIELDCATALOG .
  if gt_final[] is not INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = sy-repid
        IT_FIELDCAT        = gt_FIELDCATALOG
      TABLES
        T_OUTTAB           = gt_final[]
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
    IF SY-SUBRC <> 0.
* Implement suitable error handling here
    ENDIF.
  else.
    WRITE: 'No data found.'.
  endif.

ENDFORM.                    " F_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUILD_FIELDCATALOG .
  Perform field_cat using 'SNO'     'S.No' .
  Perform field_cat using 'PSPNR'   'WBS Element' .
  Perform field_cat using 'OBJNR'   'Object number' .
  Perform field_cat using 'LFDNR'   'Sequence number' .
  Perform field_cat using 'PERBZ_N' 'Updated Settlement type' .
  Perform field_cat using 'PERBZ_O' 'Old Settlement type ' .
  Perform field_cat using 'MSG'     'Message' .

ENDFORM.                    " F_BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0558   text
*      -->P_0559   text
*----------------------------------------------------------------------*
FORM FIELD_CAT  USING    p_field p_label.
  gs_fieldcatalog-fieldname   = p_field.
  gs_fieldcatalog-seltext_m   = p_label.
  append gs_fieldcatalog to gt_fieldcatalog.
  clear  gs_fieldcatalog.
ENDFORM.                    " FIELD_CAT
