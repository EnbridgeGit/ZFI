"Name: \PR:SAPLCJWB\FO:BUCHEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_PROJ_IN_SERVICE_DATE_VALIDAT.
*****-- start of changes by akmadasu
*  data:lv_answer,
*       jest_tab TYPE TABLE OF jest_upd,
*       gv_flag  type flag,
*       jest_st  TYPE jest_upd.
*  if sy-tcode eq 'CJ02'.
*    CALL FUNCTION 'STATUS_CHANGES_GET'
*      TABLES
*        t_changes = jest_tab.
*    IF jest_tab[] is not INITIAL.
*    sort jest_tab by stat.
*   READ TABLE jest_tab into jest_st with key stat  = 'I0002' BINARY SEARCH.
*   IF SY-SUBRC IS INITIAL AND jest_st-INACT IS INITIAL .
*   if PROJ-PLFAZ is INITIAL and PROJ-EPROG is INITIAL.
*     MESSAGE 'The both Est. Start date and Est. In-Srv date are mandatory when release (REL).' type 'S' DISPLAY LIKE 'E'.
*     call screen 0998.
*   ENDIF.
*   ENDIF.
*   else.
*     DATA:LV_OBJNR TYPE J_OBJNR,
*            LV_STAT TYPE J_STATUS.
*     SELECT SINGLE OBJNR FROM PRPS INTO LV_OBJNR WHERE POSID = PROJ-PSPID.
*       IF SY-SUBRC IS INITIAL.
*   SELECT SINGLE STAT FROM JEST INTO LV_STAT WHERE OBJNR = LV_OBJNR
*                                             AND  STAT = 'I0002'
*                                             AND INACT = ' '.
*     IF SY-SUBRC IS INITIAL.
*     if PROJ-PLFAZ is INITIAL and PROJ-EPROG is INITIAL.
*     MESSAGE 'The both Est. Start date and Est. In-Srv date are mandatory when release (REL).' type 'S' DISPLAY LIKE 'E'.
*     call screen 0998.
*     endif.
*     ENDIF.
*       ENDIF.
*       ENDIF.
*  endif.
ENDENHANCEMENT.
