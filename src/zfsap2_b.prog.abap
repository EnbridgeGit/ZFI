*&---------------------------------------------------------------------*
*& Title:ZPSAP2_B
*
*&---------------------------------------------------------------------*
REPORT zfsap2 MESSAGE-ID f4 NO STANDARD PAGE HEADING.
TABLES:  vbkpf,
         t020.
DATA:    anzkr(6)     TYPE n,
         lsind        LIKE sy-lsind,
         xpick(1)     TYPE c,
         xpickc(1)    TYPE c,
         no_output    TYPE c.
DATA:   BEGIN OF tbkpf OCCURS 5.
        INCLUDE STRUCTURE vbkpf.
DATA:   END   OF tbkpf.
DATA:   BEGIN OF it020 OCCURS 0.
        INCLUDE STRUCTURE t020.
DATA:   END   OF it020.
DATA: gv_date TYPE sy-datum,
      gv_year1 TYPE i,
      gv_year2 TYPE c LENGTH 4.

SELECTION-SCREEN SKIP 2.
SELECT-OPTIONS:
         bukrs     FOR  vbkpf-bukrs MEMORY ID buk,
         belnr     FOR  vbkpf-belnr,
         gjahr     FOR  vbkpf-gjahr MEMORY ID gjr,
         budat     FOR  vbkpf-budat,
         bldat     FOR  vbkpf-bldat,
         blart     FOR  vbkpf-blart OBLIGATORY,
         xblnr     FOR  vbkpf-xblnr,
         bktxt     FOR  vbkpf-bktxt,
         usnam     FOR  vbkpf-usnam DEFAULT sy-uname,
         xwffr     FOR  vbkpf-xwffr,
         xprfg     FOR  vbkpf-xprfg,
         xfrge     FOR  vbkpf-xfrge.
PARAMETER: pdate    LIKE bkpf-budat DEFAULT gv_date. "sy-datlo.

LOAD-OF-PROGRAM.

  MOVE sy-datum(4) TO gv_year1.
  gv_year1 = gv_year1 + 1.
  gv_year2 = gv_year1.
  CONCATENATE gv_year2 '0101' INTO gv_date.


AT SELECTION-SCREEN.

START-OF-SELECTION.
  SET PF-STATUS 'P'.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  SELECT * FROM vbkpf WHERE bukrs IN bukrs
                      AND   ausbk IN bukrs
                      AND   belnr IN belnr
                      AND   gjahr IN gjahr
                      AND   budat IN budat
                      AND   bldat IN bldat
                      AND   blart IN blart
                      AND   bktxt IN bktxt
                      AND   xblnr IN xblnr
                      AND   usnam IN usnam
                      AND   bstat EQ 'V'
                      AND   xwffr IN xwffr
                      AND   xfrge IN xfrge
                      AND   xprfg IN xprfg.
    CHECK vbkpf-bukrs EQ vbkpf-ausbk.
    "xpick = 'X'.
    "CLEAR: xpickc.
    WRITE: space.
    PERFORM liste_schreiben.
  ENDSELECT.
  IF no_output = 'X'.
    WRITE: / 'No items'.
  ENDIF.

END-OF-SELECTION.

TOP-OF-PAGE.
  FORMAT COLOR COL_HEADING INTENSIFIED.
  "CLEAR: XPICK.
  "HIDE: XPICK.
  "XPICK = 'X'.

AT LINE-SELECTION.
  READ CURRENT LINE FIELD VALUE xpickc INTO xpick.
  IF xpick = 'X'.
    MODIFY CURRENT LINE FIELD VALUE xpickc FROM ' '.
  ELSE.
    MODIFY CURRENT LINE FIELD VALUE xpickc FROM 'X'.
  ENDIF.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'BUCH'.
      PERFORM tbkpf_fuellen.
      PERFORM fbv4_bdc.
    WHEN 'MALL'.
      PERFORM mall.
    WHEN 'EMAL'.
      PERFORM emal.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  LISTE_SCHREIBEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM liste_schreiben.
  WRITE: / xpickc AS CHECKBOX.
  FORMAT COLOR COL_KEY INTENSIFIED OFF.
  WRITE:   vbkpf-belnr,
           vbkpf-gjahr,
           vbkpf-bukrs.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:   vbkpf-blart,
       (8) vbkpf-budat DD/MM/YY,
           vbkpf-waers,
           vbkpf-xblnr,
           vbkpf-xwffr,
           vbkpf-xprfg,
           vbkpf-xfrge.
  HIDE: vbkpf-bukrs,
        vbkpf-belnr,
        vbkpf-gjahr,
        vbkpf-xwffr,
        vbkpf-xfrge,
        vbkpf-blart,
        vbkpf-tcode,
        xpick.
ENDFORM.                    "LISTE_SCHREIBEN
*&---------------------------------------------------------------------*
*&      Form  TBKPF_FUELLEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tbkpf_fuellen.
  lsind = sy-lsind - 1.
  CLEAR anzkr.
  REFRESH tbkpf.
  DO.
    READ LINE sy-index INDEX lsind FIELD VALUE xpickc.
    IF sy-subrc = 0.
      CHECK xpickc NE space.
      anzkr = anzkr + 1.
      MOVE-CORRESPONDING vbkpf TO tbkpf.
      APPEND tbkpf.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    "TBKPF_FUELLEN
*&---------------------------------------------------------------------*
*&      Form  MALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM mall.
  lsind = 1.
  DO.
    READ LINE lsind.
    IF sy-subrc = 0.
      MODIFY LINE lsind
             FIELD VALUE
             xpickc FROM 'X'.
      lsind = lsind + 1.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    "MALL
*&---------------------------------------------------------------------*
*&      Form  EMAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM emal.
  lsind = 1.
  DO.
    READ LINE lsind.
    IF sy-subrc = 0.
      MODIFY LINE lsind
             FIELD VALUE
             xpickc FROM ' '.
      lsind = lsind + 1.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    "EMAL
DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
DATA: BEGIN OF messtab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF messtab.
*&---------------------------------------------------------------------*
*&      Form  FBV4_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fbv4_bdc.
  REFRESH messtab.

  SELECT * FROM t020
        INTO TABLE it020
        ORDER BY PRIMARY KEY.
  LOOP AT tbkpf.
    PERFORM fbv4_bdc1 USING tbkpf-gjahr
                            tbkpf-belnr
                            tbkpf-bukrs
                            tbkpf-tcode
                            pdate.
  ENDLOOP.
  CHECK sy-subrc EQ 0.
  LOOP AT messtab.
    WRITE: / messtab.
  ENDLOOP.
ENDFORM.                                                    "FBV4_BDC
*&---------------------------------------------------------------------*
*&      Form  FBV4_BDC1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_GJAHR    text
*      -->I_BELNR    text
*      -->I_BUKRS    text
*      -->I_TCODE    text
*      -->I_PDATE    text
*----------------------------------------------------------------------*
FORM fbv4_bdc1 USING i_gjahr LIKE bkpf-gjahr
                     i_belnr LIKE bkpf-belnr
                     i_bukrs LIKE bkpf-bukrs
                     i_tcode LIKE bkpf-tcode
                     i_pdate LIKE bkpf-budat.
  DATA: xdate(10) TYPE c.
  REFRESH bdcdata.
  PERFORM bdc_dynpro      USING 'SAPMF05V' '0100'.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF05V-GJAHR'.
  PERFORM bdc_field       USING 'RF05V-BUKRS'
                                i_bukrs.
  PERFORM bdc_field       USING 'RF05V-BELNR'
                                i_belnr.
  PERFORM bdc_field       USING 'RF05V-GJAHR'
                                i_gjahr.

  WRITE i_pdate TO xdate(000010)
           DD/MM/YYYY.
  CLEAR it020.
  READ TABLE it020 WITH KEY tcode = tbkpf-tcode BINARY SEARCH.


  IF it020-gener <> '1'.

    PERFORM bdc_dynpro      USING 'SAPLF040' '0600'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'BKPF-BUDAT'.
    PERFORM bdc_field       USING 'BKPF-BUDAT'
                                  xdate.

  ELSE.
    IF     it020-koart = 'D'.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '1200'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                               'INVFO-BUDAT'.
      PERFORM bdc_field       USING 'INVFO-BUDAT'
                               xdate.
    ELSEIF it020-koart = 'K'.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '1100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                               'INVFO-BUDAT'.
      PERFORM bdc_field       USING 'INVFO-BUDAT'
                               xdate.
    ELSEIF it020-koart = 'S'.

      PERFORM bdc_dynpro      USING 'SAPMF05A' '1001'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                               'ACGL_HEAD-BUDAT'.
      PERFORM bdc_field       USING 'ACGL_HEAD-BUDAT'
                               xdate.

    ENDIF.
  ENDIF.

  PERFORM bdc_field       USING 'BDC_OKCODE'
                                'BP'.



  CALL TRANSACTION 'FBV4' USING bdcdata
                   MODE   'N'
                   UPDATE 'S'.


  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = sy-msgid
      msgnr               = sy-msgno
      msgv1               = sy-msgv1
      msgv2               = sy-msgv2
      msgv3               = sy-msgv3
      msgv4               = sy-msgv4
    IMPORTING
      message_text_output = messtab
    EXCEPTIONS
      OTHERS              = 4.
  APPEND messtab.
ENDFORM.                                                    "CM
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
