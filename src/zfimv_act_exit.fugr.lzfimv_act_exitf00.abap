*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZFIMV_ACT_EXIT..................................*
FORM GET_DATA_ZFIMV_ACT_EXIT.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZFIT_ACTIVE_EXIT WHERE
(VIM_WHERETAB) .
    CLEAR ZFIMV_ACT_EXIT .
ZFIMV_ACT_EXIT-MANDT =
ZFIT_ACTIVE_EXIT-MANDT .
ZFIMV_ACT_EXIT-EXITNAME =
ZFIT_ACTIVE_EXIT-EXITNAME .
ZFIMV_ACT_EXIT-ACTIVE =
ZFIT_ACTIVE_EXIT-ACTIVE .
<VIM_TOTAL_STRUC> = ZFIMV_ACT_EXIT.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZFIMV_ACT_EXIT .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZFIMV_ACT_EXIT.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZFIMV_ACT_EXIT-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZFIT_ACTIVE_EXIT WHERE
  EXITNAME = ZFIMV_ACT_EXIT-EXITNAME .
    IF SY-SUBRC = 0.
    DELETE ZFIT_ACTIVE_EXIT .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZFIT_ACTIVE_EXIT WHERE
  EXITNAME = ZFIMV_ACT_EXIT-EXITNAME .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZFIT_ACTIVE_EXIT.
    ENDIF.
ZFIT_ACTIVE_EXIT-MANDT =
ZFIMV_ACT_EXIT-MANDT .
ZFIT_ACTIVE_EXIT-EXITNAME =
ZFIMV_ACT_EXIT-EXITNAME .
ZFIT_ACTIVE_EXIT-ACTIVE =
ZFIMV_ACT_EXIT-ACTIVE .
    IF SY-SUBRC = 0.
    UPDATE ZFIT_ACTIVE_EXIT .
    ELSE.
    INSERT ZFIT_ACTIVE_EXIT .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZFIMV_ACT_EXIT-UPD_FLAG,
STATUS_ZFIMV_ACT_EXIT-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZFIMV_ACT_EXIT.
  SELECT SINGLE * FROM ZFIT_ACTIVE_EXIT WHERE
EXITNAME = ZFIMV_ACT_EXIT-EXITNAME .
ZFIMV_ACT_EXIT-MANDT =
ZFIT_ACTIVE_EXIT-MANDT .
ZFIMV_ACT_EXIT-EXITNAME =
ZFIT_ACTIVE_EXIT-EXITNAME .
ZFIMV_ACT_EXIT-ACTIVE =
ZFIT_ACTIVE_EXIT-ACTIVE .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZFIMV_ACT_EXIT USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZFIMV_ACT_EXIT-EXITNAME TO
ZFIT_ACTIVE_EXIT-EXITNAME .
MOVE ZFIMV_ACT_EXIT-MANDT TO
ZFIT_ACTIVE_EXIT-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZFIT_ACTIVE_EXIT'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZFIT_ACTIVE_EXIT TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZFIT_ACTIVE_EXIT'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
