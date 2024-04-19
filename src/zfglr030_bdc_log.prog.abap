*&---------------------------------------------------------------------*
*& Report  ZFGLR029_BDC_LOG
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
************************************************************************

REPORT  zfglr030_bdc_log.

TABLES: t100,
        apqi.

DATA: lm LIKE bdclm,
      save_mpar TYPE bdc_mpar.
DATA: BEGIN OF logtable OCCURS 50,  " plain log information in TemSe
          enterdate LIKE btctle-enterdate,
          entertime LIKE btctle-entertime,
          logmessage(400) TYPE c,
      END OF logtable.
DATA: it_logtable   LIKE TABLE OF logtable,
      ls_logtable LIKE logtable,
      gt_logtable LIKE TABLE OF logtable.
DATA: BEGIN OF bdc_protocol OCCURS 0.
DATA: batchid TYPE apqi-groupid.
        INCLUDE STRUCTURE bdclm.
DATA:
        longtext TYPE bdc_mpar,
      END OF bdc_protocol.
DATA BEGIN OF bdclm  OCCURS 0.     " ITabelle der Messageseintraege
        INCLUDE STRUCTURE bdclm.  " LogTabelle
DATA: counter TYPE i,
      longtext TYPE bdc_mpar,
      isdetail(1) TYPE c,
     END OF bdclm .

DATA:                                "Aufbereitung Messagetext
  BEGIN OF mt,
   off(02) TYPE n,
   len(02) TYPE n,
   text(99),
 END OF mt.
DATA:                                "Aufbereitung Messagetext
 BEGIN OF mttab  OCCURS 4,
  off(02) TYPE n,
  len(02) TYPE n,
  text(99),
END OF mttab.
DATA:                                "ParameterAufbereitung
  BEGIN OF par,
   len(02) TYPE n,
   text(254),
 END OF par.
DATA:
  digits(10) TYPE c VALUE '0123456789',
  mtext(124) TYPE c,                  "Messagetext
  mtext1(124) TYPE c,                 "Messagetext
  mtext2(273) TYPE c,                 "Messagetext
  do_condense TYPE c,
  mtvaroff TYPE i,
  showtyp(05) TYPE c,                 "showtyp
  lmapn(12) TYPE c,                  "Hilfsfeld log-mapn
  date1  TYPE d,
  linct0      LIKE sy-linct,
  parcnt  TYPE i,
  sp_len  TYPE i,
  charcnt TYPE i,
  wcnt TYPE i,
  mparcnt TYPE i,
  qfound(04) TYPE n,
  x(1) VALUE 'X'.
DATA:
       external_date(10),
       internal_date TYPE d.
FIELD-SYMBOLS:
 <mtxt>,
 <vtxt>.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_gid  FOR apqi-groupid OBLIGATORY,
                s_date FOR apqi-credate OBLIGATORY,  "getdate OBLIGATORY,
                s_qstate FOR apqi-qstate OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  s_qstate-sign = 'I'.
  s_qstate-option = 'EQ'.
  s_qstate-low = 'F'.    "posted
  APPEND s_qstate.
  s_qstate-sign = 'I'.
  s_qstate-option = 'EQ'.
  s_qstate-low = 'E'. "Error
  APPEND s_qstate.

  s_qstate-sign = 'I'.
  s_qstate-option = 'EQ'.
  s_qstate-low = ' '.
  APPEND s_qstate.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM output_data.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA: charcp LIKE rststype-charco VALUE '0000',
        lt_apql TYPE TABLE OF apql,
        ls_apql TYPE apql,
        lt_apqi TYPE TABLE OF apqi,
        ls_apqi TYPE apqi,
        lv_datatyp TYPE apq_dtyp VALUE 'BDC ',
        lv_fbhandle TYPE rststype-fbhandle.

  SELECT * FROM apqi INTO TABLE lt_apqi
  WHERE datatyp = lv_datatyp
    AND groupid IN s_gid
    AND credate IN s_date   "getdate IN s_date
    AND qstate IN s_qstate.

  CLEAR: bdc_protocol.
  REFRESH: bdc_protocol.
  CLEAR: gt_logtable,
         bdclm.
  REFRESH bdclm.
  LOOP AT lt_apqi INTO ls_apqi.
    CLEAR: lt_apql.
    CALL FUNCTION 'BDC_PROTOCOL_SELECT_QID'
      EXPORTING
        queue_id     = ls_apqi-qid
      TABLES
        apqltab      = lt_apql
      EXCEPTIONS
        invalid_data = 1
        OTHERS       = 2.
    LOOP AT lt_apql INTO ls_apql.
      CLEAR: it_logtable,
             "bdclm,
             lv_fbhandle.
      "REFRESH bdclm.
      CALL FUNCTION 'RSTS_OPEN_RLC'
        EXPORTING
          name           = ls_apql-temseid
*         CLIENT         = ls_APQL-MANDANT
          authority      = 'BATCH'
*         PROM           = 'I'
*         RECTYP         = 'VNL----'
*         CHARCO         = CHARCP
        IMPORTING
          fbhandle       = lv_fbhandle
        EXCEPTIONS
          fb_call_handle = 4
          fb_error       = 8
          fb_rsts_noconv = 12
          fb_rsts_other  = 16
          no_object      = 20
          OTHERS         = 24.
      IF sy-subrc > 0.
        EXIT.
      ENDIF.
      CALL FUNCTION 'RSTS_READ'
        EXPORTING
          fbhandle       = lv_fbhandle
        TABLES
          datatab        = it_logtable
        EXCEPTIONS
          fb_call_handle = 4
          fb_error       = 8
          fb_rsts_noconv = 12
          fb_rsts_other  = 16
          OTHERS         = 16.
      IF sy-subrc > 0.
        EXIT.
      ENDIF.
      CALL FUNCTION 'RSTS_CLOSE'
        EXPORTING
          fbhandle = lv_fbhandle
        EXCEPTIONS
          OTHERS   = 4.
      "-----------------------------
      APPEND LINES OF it_logtable TO gt_logtable.
    ENDLOOP.
  ENDLOOP.
  "--------------------------
  LOOP AT gt_logtable INTO ls_logtable.
    CALL 'DATE_CONV_INT_TO_EXT'
               ID 'DATINT' FIELD ls_logtable-enterdate
               ID 'DATEXT' FIELD external_date.

    CALL 'DATE_CONV_EXT_TO_INT'
         ID 'DATEXT' FIELD external_date
         ID 'DATINT' FIELD internal_date.
    IF sy-subrc NE 0.         " Datum ist nicht gültig
      CONTINUE.
    ENDIF.
    CLEAR bdclm.
*    REFRESH bdclm.
    bdclm-indate  = ls_logtable-enterdate.
    bdclm-intime  = ls_logtable-entertime.
    bdclm+14(352) = ls_logtable-logmessage.
    IF bdclm-mcnt > 0.
      bdclm-mcnt = bdclm-mcnt - 1.
    ENDIF.
    IF bdclm-mid EQ '00'.
      IF   ( bdclm-mnr EQ '162' )
        OR ( bdclm-mnr EQ '368' ).
        bdclm-isdetail = 'X'.
      ENDIF.
    ENDIF.
    APPEND bdclm.
  ENDLOOP.
  LOOP AT bdclm.
    lm = bdclm.
    save_mpar = bdclm-mpar.
    PERFORM get_text.
    bdclm-longtext = mtext.
    bdclm-mpar = save_mpar.
    MODIFY bdclm.
  ENDLOOP.
  "break sahmad.
  "------------------------------
  LOOP AT bdclm. " where tcnt = tcode_index_apqd.
    IF bdclm-isdetail = 'X'.
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING bdclm TO bdc_protocol.
    bdc_protocol-batchid = ls_apqi-groupid.
*    bdc_protocol-indate = ls_apqi-getdate.
    APPEND bdc_protocol.
  ENDLOOP.
  "------------------------------
  break sahmad.
*    ENDLOOP.
*  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_text .
  DATA: shiftln TYPE i,
        vartcnt TYPE i,
        fdpos LIKE sy-fdpos.

  IF bdclm-mparcnt CN digits.        "Korrupter Datensatz:
    bdclm-mparcnt = 0.               "z.B. Hexnullen
  ENDIF.

  SELECT SINGLE * FROM t100
   WHERE sprsl = sy-langu
   AND  arbgb  = bdclm-mid
   AND  msgnr  = bdclm-mnr.
*
  IF sy-subrc EQ 0.
    CLEAR: mtext,
           parcnt,
           mparcnt,
           charcnt,
           wcnt,
           mt,
           sp_len,
           sy-fdpos.
*
    MOVE bdclm-mparcnt TO mparcnt.
*
    IF t100-text CA '$&'.            "Kennung fuer parameter:
      MOVE t100-text TO mtext1.      " alt '$' --- neu '&'
    ELSE.
      MOVE t100-text TO mtext.
      EXIT.
    ENDIF.
* variable teile aus batch-input protokoll in mttab bringen.
    REFRESH mttab.
    CLEAR shiftln.
    DO mparcnt TIMES.
      CLEAR: par, mttab.
      MOVE bdclm-mpar TO par.
      IF par-len CN digits OR par-len EQ 0.       "convert_no_number
        par-len  = 1.                             "entschärfen
        par-text = ' '.
        shiftln  = 2.
      ELSE.
        shiftln = par-len + 2.
      ENDIF.
      WRITE par-text TO mttab-text(par-len).
      MOVE par-len  TO mttab-len.
      MOVE mparcnt  TO mttab-off.
      APPEND mttab.
      SHIFT bdclm-mpar BY shiftln PLACES.
    ENDDO.
*
    mtext2 = mtext1.
    IF bdclm-mid EQ  '00' AND    " sonderbehandlung s00368
       bdclm-mnr EQ '368' AND
       bdclm-mart EQ 'S'.
      CLEAR mtext2.
      CLEAR mttab.
      READ TABLE mttab INDEX 1.
      WRITE mttab-text TO mtext2+0(mttab-len).
      CLEAR mttab.
      READ TABLE mttab INDEX 2.
      WRITE mttab-text TO mtext2+35(mttab-len).
      mtext = mtext2.
      EXIT.
    ENDIF.

    do_condense = x.
    CLEAR: mt, vartcnt, mtvaroff.
    WHILE vartcnt LE 3.
      vartcnt = vartcnt + 1.
      IF mtext1 CA '$&'.
        parcnt = parcnt + 1.
        IF sy-fdpos GT 0.
          fdpos = sy-fdpos - 1.                    " neu sy-fdpos -1
        ELSE.
          fdpos = sy-fdpos.
        ENDIF.
        SHIFT mtext1 BY sy-fdpos PLACES.
        IF mtext1(1) EQ '&'.
          SHIFT mtext1 BY 1 PLACES.
          CASE mtext1(1).
            WHEN ' '.                              "'& '
              PERFORM replace_var USING '& ' parcnt fdpos.
            WHEN '$'.                              "'&&'
              PERFORM replace_var USING '&&' 0      fdpos.
            WHEN '1'.                              "'&1'
              PERFORM replace_var USING '&1' 1      fdpos.
            WHEN '2'.                              "'&2'
              PERFORM replace_var USING '&2' 2      fdpos.
            WHEN '3'.                              "'&3'
              PERFORM replace_var USING '&3' 3      fdpos.
            WHEN '4'.                              "'&4'
              PERFORM replace_var USING '&4' 4      fdpos.
            WHEN OTHERS.                           "'&'
              PERFORM replace_var USING '&<' parcnt fdpos.
          ENDCASE.
        ENDIF.
        IF mtext1(1) EQ '$'.
          SHIFT mtext1 BY 1 PLACES.
          CASE mtext1(1).
            WHEN ' '.                              "'$ '
              PERFORM replace_var USING '$ ' parcnt  fdpos.
            WHEN '$'.                              "'$$'
              PERFORM replace_var USING '$$' 0       fdpos.
            WHEN '1'.                              "'$1'
              PERFORM replace_var USING '$1' 1       fdpos.
            WHEN '2'.                              "'$2'
              PERFORM replace_var USING '$2' 2       fdpos.
            WHEN '3'.                              "'$3'
              PERFORM replace_var USING '$3' 3       fdpos.
            WHEN '4'.                              "'$4'
              PERFORM replace_var USING '$4' 4       fdpos.
            WHEN OTHERS.                           "'$'
              PERFORM replace_var USING '$<' parcnt  fdpos.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDWHILE.
*
    IF mtext2 CA '%%_D_%%'.
      REPLACE '%%_D_%%' WITH '$' INTO mtext2.
    ENDIF.
    IF mtext2 CA '%%_A_%%'.
      REPLACE '%%_A_%%' WITH '&' INTO mtext2.
    ENDIF.
    IF do_condense EQ space.
      mtext = mtext2.
    ELSE.
      CONDENSE mtext2 .
      mtext = mtext2.
    ENDIF.
  ELSE.
    mtext = '???????????????????????????????????????????????????'.
  ENDIF.
ENDFORM.                    " GET_TEXT
*&---------------------------------------------------------------------*
*&      Form  REPLACE_VAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0552   text
*      -->P_3      text
*      -->P_FDPOS  text
*----------------------------------------------------------------------*
FORM replace_var USING vark vari varpos.
*
*   ersetzen der variablen teile einer fehlermeldung
*
  DATA: var(02),
        var1,
        moff TYPE i.
*
  CLEAR: mttab , moff.
  var = vark.
  SHIFT var BY 1 PLACES.
  CASE var.
    WHEN ' '.                              "'& '
      READ TABLE mttab INDEX vari.
      IF sy-subrc EQ 0.
        moff = varpos + mtvaroff.
        ASSIGN mtext2+moff(*) TO <mtxt>.
        ASSIGN mttab-text(mttab-len) TO <vtxt>.
        var1 = vark.
        REPLACE var1 WITH <vtxt>     INTO <mtxt>.
        mtvaroff = mttab-len.
      ELSE.
        IF vari GT mparcnt.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          REPLACE vark WITH '  ' INTO <mtxt>.
          mtvaroff = 2.
        ELSE.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          REPLACE vark WITH '%%_Z_%%' INTO <mtxt>.
          mtvaroff = 7.
        ENDIF.
      ENDIF.
    WHEN '$'.                              "'&&'
      moff = varpos + mtvaroff.
      ASSIGN mtext2+moff(*) TO <mtxt>.
      REPLACE vark WITH '%%_D_%%' INTO <mtxt>.
      mtvaroff = 7.
    WHEN '&'.                              "'&&'
      moff = varpos + mtvaroff.
      ASSIGN mtext2+moff(*) TO <mtxt>.
      REPLACE vark WITH '%%_A_%%' INTO <mtxt>.
      mtvaroff = 7.
    WHEN '<'.                              "'&1'
      READ TABLE mttab INDEX vari.
      IF sy-subrc EQ 0.
        IF vark EQ '&<'.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          ASSIGN mttab-text(mttab-len) TO <vtxt>.
          REPLACE '&' WITH <vtxt>     INTO <mtxt>.
          mtvaroff = mttab-len.
        ENDIF.
        IF vark EQ '$<'.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          ASSIGN mttab-text(mttab-len) TO <vtxt>.
          REPLACE '$' WITH <vtxt>     INTO <mtxt>.
          mtvaroff = mttab-len.
        ENDIF.
      ELSE.
        IF vark EQ '&<'.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          REPLACE '&' WITH ' ' INTO <mtxt>.
          mtvaroff = 1.
        ENDIF.
        IF vark EQ '$<'.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          REPLACE '$' WITH ' ' INTO <mtxt>.
          mtvaroff = 1.
        ENDIF.
      ENDIF.
    WHEN '1'.                              "'&1'
      READ TABLE mttab INDEX 1.
      IF sy-subrc EQ 0.
        moff = varpos + mtvaroff.
        ASSIGN mtext2+moff(*) TO <mtxt>.
        ASSIGN mttab-text(mttab-len) TO <vtxt>.
        REPLACE vark WITH <vtxt>     INTO <mtxt>.
        mtvaroff = mttab-len.
      ELSE.
        IF vari GT mparcnt.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          REPLACE vark WITH '  ' INTO <mtxt>.
          mtvaroff = 2.
        ELSE.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          REPLACE vark WITH '%%_Z_%%' INTO <mtxt>.
          mtvaroff = 7.
        ENDIF.
      ENDIF.
    WHEN '2'.                              "'&2'
      READ TABLE mttab INDEX 2.
      IF sy-subrc EQ 0.
        moff = varpos + mtvaroff.
        ASSIGN mtext2+moff(*) TO <mtxt>.
        ASSIGN mttab-text(mttab-len) TO <vtxt>.
        REPLACE vark WITH <vtxt>     INTO <mtxt>.
        mtvaroff = mttab-len.
      ELSE.
        IF vari GT mparcnt.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          REPLACE vark WITH '  ' INTO <mtxt>.
          mtvaroff = 2.
        ELSE.
          moff = varpos + mtvaroff.
          ASSIGN mtext2+moff(*) TO <mtxt>.
          REPLACE vark WITH '%%_Z_%%' INTO <mtxt>.
          mtvaroff = 7.
        ENDIF.
      ENDIF.
    WHEN '3'.                              "'&3'
      READ TABLE mttab INDEX 3.
      IF sy-subrc EQ 0.
        moff = varpos + mtvaroff.                    "neu
        ASSIGN mtext2+moff(*) TO <mtxt>.              "neu
        ASSIGN mttab-text(mttab-len) TO <vtxt>.
        REPLACE vark WITH <vtxt>     INTO <mtxt>.     "neu
        mtvaroff = mttab-len.                        "neu
      ELSE.
        IF vari GT mparcnt.
          moff = varpos + mtvaroff.                    "neu
          ASSIGN mtext2+moff(*) TO <mtxt>.              "neu
          REPLACE vark WITH '  ' INTO <mtxt>.     "neu
          mtvaroff = 2.                        "neu
        ELSE.
          moff = varpos + mtvaroff.                    "neu
          ASSIGN mtext2+moff(*) TO <mtxt>.              "neu
          REPLACE vark WITH '%%_Z_%%' INTO <mtxt>.     "neu
          mtvaroff = 7.                   "neu
        ENDIF.
      ENDIF.
    WHEN '4'.                              "'&4'
      READ TABLE mttab INDEX 4.
      IF sy-subrc EQ 0.
        moff = varpos + mtvaroff.                    "neu
        ASSIGN mtext2+moff(*) TO <mtxt>.              "neu
        ASSIGN mttab-text(mttab-len) TO <vtxt>.
        REPLACE vark WITH <vtxt>     INTO <mtxt>.     "neu
        mtvaroff = mttab-len.                        "neu
      ELSE.
        IF vari GT mparcnt.
          moff = varpos + mtvaroff.                    "neu
          ASSIGN mtext2+moff(*) TO <mtxt>.              "neu
          REPLACE vark WITH '  ' INTO <mtxt>.     "neu
          mtvaroff = 2.                        "neu
        ELSE.
          moff = varpos + mtvaroff.                    "neu
          ASSIGN mtext2+moff(*) TO <mtxt>.              "neu
          REPLACE vark WITH '%%_Z_%%' INTO <mtxt>.     "neu
          mtvaroff = 7.                   "neu
        ENDIF.
      ENDIF.
*
  ENDCASE.
*
  do_condense = space.
ENDFORM.                    " REPLACE_VAR
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM output_data.

  DATA:    lv_date       TYPE sy-datum,
           lv_longtext   TYPE bdc_mpar,
           lv_count      TYPE i.

  CONSTANTS:
           lc_text1      TYPE bdc_mpar
                         VALUE 'Field',
           lc_text2      TYPE bdc_mpar
                         VALUE 'Cursor field',
           lc_text3      TYPE bdc_mpar
                         VALUE 'does not exist in the screen'.

  CLEAR    lv_longtext.
  CLEAR    lv_count.

  WRITE: / 'Current Date: ', sy-datum.
  ULINE.
  SORT bdc_protocol BY batchid indate intime.
  LOOP AT bdc_protocol.
    lv_date = bdc_protocol-indate.
    AT NEW batchid.
      SKIP 1.
      WRITE : / 'Session ID: ', bdc_protocol-batchid,
                'Date: ', lv_date.
      SKIP 1.
    ENDAT.

    IF       ( bdc_protocol-longtext     NE lv_longtext ).
      CLEAR                                 lv_longtext.
      MOVE     bdc_protocol-longtext     TO lv_longtext.
      CLEAR                                 lv_count.
      MOVE     1                         TO lv_count.
    ELSE.   "( bdc_protocol-longtext     EQ lv_longtext ).
      ADD      1                         TO lv_count.
    ENDIF.

    IF       ( lv_count                  LE 3 ).

      IF ( ( ( lv_longtext               CS lc_text1 ) OR
             ( lv_longtext               CS lc_text2 )    ) AND
           (   lv_longtext               CS lc_text3      )     ).

      ELSE.

        WRITE : / bdc_protocol-longtext.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " OUTPUT_DATA
