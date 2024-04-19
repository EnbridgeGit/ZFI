* 2009/01/19 TR580 mdemeest - Upgrade ERP 6.0/SAPWeaver 7.00        UGL
*                             All changes indicated with UGL in     UGL
*                             rightmost columns.
*&---------------------------------------------------------------------*
*&      Form  PREPARE_ITEM_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_item_text TABLES t_xekkn STRUCTURE ekkn
                              t_xaend STRUCTURE xaend.

  DATA: text_flag(1) TYPE p.

  IF ekpo-uptyp EQ space OR tmsi2-sitxd NE space.
* Lesen Positionstextheader--------------------------------------------*
    thead-tdobject = 'EKPO'.
    thead-tdspras = ekko-spras.
    thead-tdname = ekko-ebeln.
    thead-tdname+10(5) = ekpo-ebelp.
    thead-tdid = '*'.
    MOVE-CORRESPONDING thead TO xtheadkey.

    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        id         = thead-tdid
        language   = thead-tdspras
        name       = thead-tdname
        object     = thead-tdobject
      TABLES
        selections = xthead.
    SORT xthead BY tdid.

* Lesen Positionstexte ------------------------------------------------*
    REFRESH xt166p.
    CLEAR xt166p.
    SELECT * FROM t166p WHERE druvo = xdruvo
                        AND   bstyp = ekko-bstyp
                        AND   bsart = ekko-bsart
                        AND   pstyp = ekpo-pstyp.
      MOVE t166p TO xt166p.
      IF t166p-tdobject = thead-tdobject.
        xtheadkey-tdid = t166p-tdid.
        READ TABLE xthead WITH KEY xtheadkey BINARY SEARCH.
        IF sy-subrc EQ 0.
          APPEND xt166p.
        ENDIF.
      ELSE.
        CASE t166p-tdobject.   "Unnötige Zugriffe auf STXH sparen
          WHEN 'MATERIAL'.
            IF ekpo-matnr NE space.
              APPEND xt166p.
            ENDIF.
          WHEN 'EINA'.
            IF ekpo-infnr NE space.
              APPEND xt166p.
            ENDIF.
          WHEN 'EINE'.
            IF ekpo-infnr NE space.
              APPEND xt166p.
            ENDIF.
          WHEN 'VBBP'.
            IF ekpo-knttp NE space.
              READ TABLE t_xekkn WITH KEY     ebeln =  ekpo-ebeln
                                              ebelp =  ekpo-ebelp
                                              zekkn =  '01'.
              IF sy-subrc EQ 0 AND t_xekkn-vbeln NE space.
                APPEND xt166p.
              ENDIF.
            ENDIF.
          WHEN OTHERS.
            APPEND xt166p.
        ENDCASE.
      ENDIF.
    ENDSELECT.
    SORT xt166p BY drflg drpri.

* When no text changes have been made, remove item texts and all texts
* with lower priority
    IF xdruvo EQ aend OR xdruvo EQ lpae.
      LOOP AT t_xaend WHERE ebelp EQ ekpo-ebelp
                      AND text_case EQ 'X'.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        DO.
          READ TABLE xt166p WITH KEY druvo    = xdruvo
                                     bstyp    = ekko-bstyp
                                     bsart    = ekko-bsart
                                     pstyp    = ekpo-pstyp
                                     tdobject = 'EKPO'.
          IF sy-subrc EQ 0.
            DELETE xt166p WHERE druvo  = xdruvo
                          AND   bstyp  = ekko-bstyp
                          AND   bsart  = ekko-bsart
                          AND   pstyp  = ekpo-pstyp
                          AND   drflg  = xt166p-drflg
                          AND   drpri GE xt166p-drpri.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.

    CLEAR xdrflg.
* XT166P ergänzen/bereinigen ------------------------------------------*
    LOOP AT xt166p.
* Kein weiterer Text mit gleichem Reihenfolge-Kennzeichen -------------*
* außer bei Infobestelltext, da er evtl. den Materialbestelltext ------*
* noch verdrängen kann                                           ------*
      IF xt166p-drflg EQ xdrflg AND
         ( xt166p-tdobject NE 'EINE' OR xt166p-tdid NE 'BT' ).
        DELETE xt166p.
      ELSE.
        IF xt166p-tdobject EQ 'EKPO' OR
           xt166p-tdobject EQ 'TEXT'.
          xdrflg = xt166p-drflg.
        ELSE.
* Lesen Textheader zu MAterial/Infosatz-Texten-------------------------*
          thead-tdobject = xt166p-tdobject.
          thead-tdspras = ekko-spras.
          thead-tdid = xt166p-tdid.
          CASE xt166p-tdobject.
            WHEN 'MATERIAL'.
*
              IF NOT ekpo-mprof IS INITIAL.
                CALL FUNCTION 'MB_READ_TMPPF'
                  EXPORTING
                    profile        = ekpo-mprof
                  IMPORTING
                    mpn_parameters = tmppf.
              ENDIF.
              IF tmppf-mpbtx IS INITIAL.
*... Text aus best. gef. Material ....................................*
                thead-tdname = ekpo-matnr.
              ELSE.
*... Text aus HTN ....................................................*
                thead-tdname = ekpo-ematn.
              ENDIF.
*
            when 'EBAN'.                                    "569083
              thead-tdname       = ekpo-banfn.              "569083
              thead-tdname+10(6) = ekpo-bnfpo.              "569083
            WHEN 'EINE'.
              DATA: h_meico LIKE meico,"82403 Anfang
                    h_eine  LIKE eine.
              h_meico-infnr = ekpo-infnr.
              h_meico-ekorg = ekko-ekorg.
              IF ekpo-pstyp EQ pstyp-lohn.
                h_meico-esokz = esokz-lohn.
              ELSEIF ekpo-pstyp EQ pstyp-kons.
                h_meico-esokz = esokz-konsi.
              ELSE.
                h_meico-esokz = esokz-norm.
              ENDIF.
              h_meico-werks = ekpo-werks.
              CALL FUNCTION 'ME_READ_INFORECORD'
                EXPORTING
                  incom     = h_meico
                IMPORTING
                  einedaten = h_eine
                EXCEPTIONS
                  OTHERS    = 1.
              thead-tdname       = ekpo-infnr.
              thead-tdname+10(4) = ekko-ekorg.
              thead-tdname+14(1) = h_meico-esokz.
              thead-tdname+15(4) = h_eine-werks.
            WHEN 'VBBP'.
              thead-tdname       = ekkn-vbeln.
              thead-tdname+10(6) = ekkn-vbelp.
            WHEN 'ESLL'.
              thead-tdname       = ekkn-vbeln.
              thead-tdname+10(6) = ekkn-vbelp.
          ENDCASE.
          MOVE-CORRESPONDING thead TO xtheadkey.
          CALL FUNCTION 'SELECT_TEXT'
            EXPORTING
              id         = thead-tdid
              language   = thead-tdspras
              name       = thead-tdname
              object     = thead-tdobject
            TABLES
              selections = xthead.
          READ TABLE xthead INDEX 1.
          IF sy-subrc EQ 0.
            xt166p-txnam = xthead-tdname.
            MODIFY xt166p.
            xdrflg = xt166p-drflg.
            IF thead-tdobject EQ 'MATERIAL' OR
               ( thead-tdobject EQ 'EINE' AND thead-tdid EQ 'BT' ).
              text_flag = text_flag + 1.
            ENDIF.
          ELSE.
            DELETE xt166p.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

*------------------------------- Start for ERP 6.0 ----------------- UGL
* Clear Info Record.  Then get the correct info record               UGL

  clear eina.                                                       "UGL
  clear eine.                                                       "UGL
  select single * from eina where infnr = ekpo-infnr.               "UGL
*
* Get Quote Information on the purchasing documents                  UGL

  select * from eine where infnr = ekpo-infnr                       "UGL
                       and ekorg = ekko-ekorg                       "UGL
                       and esokz = '0'.                             "UGL
  endselect.                                                        "UGL
*------------------------------- End of ERP 6.0 Changes -----------  UGL


* Infobestelltext rausnehmen, wenn er durch die erste Bestellung
* erzeugt wurde und dies die erste Bestellung ist
    IF ( xdruvo EQ neu OR xdruvo EQ aend ) AND ekpo-spinf NE space AND
       ekpo-bstyp NE bstyp-anfr.
      LOOP AT xt166p WHERE tdobject EQ 'EINE'
                     AND   tdid     EQ 'BT'.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        LOOP AT xt165p WHERE vorga    EQ ekpo-bstyp
                       AND   tdobject EQ xt166p-tdobject
                       AND   koobject EQ 'EKPO'.
          LOOP AT xt166p WHERE tdobject EQ 'EKPO'
                         AND   tdid     EQ xt165p-koid.
            EXIT.
          ENDLOOP.
          EXIT.
        ENDLOOP.
        IF xt166p-tdobject EQ 'EKPO' AND xt166p-tdid EQ xt165p-koid.
* der Positionstext, aus dem der Bestelltext des Infosatzes erstellt
* wurde, wird ebenfalls gedruckt --> kann den Infobestelltext
* bei der Erstbestellung rausnehmen
          LOOP AT xt166p WHERE tdobject EQ 'EINE'
                         AND   tdid     EQ 'BT'.
            eine-infnr = xt166p-txnam(10).
            eine-ekorg = xt166p-txnam+10(4).
            eine-esokz = xt166p-txnam+14(1).
            eine-werks = xt166p-txnam+15(4).
* Holds store groups for Purchasing
            PERFORM retail_reference_site_select.
            IF sy-subrc <> 0.
              SELECT SINGLE * FROM eine WHERE infnr EQ eine-infnr
                                        AND   ekorg EQ eine-ekorg
                                        AND   esokz EQ eine-esokz
                                        AND   werks EQ eine-werks.
            ENDIF.
            IF sy-subrc EQ 0.
              MOVE eine TO *eine.      "für nächsten Schritt
              IF eine-netpr EQ 0 AND
                 eine-erdat EQ ekpo-aedat AND
                 eine-ebeln EQ ekpo-ebeln.
*    Infosatz wurde mit großer Wahrscheinlichkeit durch die Bestellung
*    erzeugt (wenn am Tag der ersten Bestellung auch gleich noch eine
*    zweite erfaßt wird, greift diese Abfrage natürlich nicht )
                text_flag = 1.
                DELETE xt166p.
              ENDIF.
            ENDIF.
            EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

* XT166P bereinigen (2. Runde wegen Ausschluß Materialbestelltext) ----*
    IF text_flag GT 1.
*  Sowohl Material- als auch Infobestelltext sind vorhanden
      LOOP AT xt166p WHERE tdobject EQ 'EINE'
                     AND   tdid     EQ 'BT'.
        eine-infnr = xt166p-txnam(10).
        eine-ekorg = xt166p-txnam+10(4).
        eine-esokz = xt166p-txnam+14(1).
        eine-werks = xt166p-txnam+15(4).
        IF eine-infnr EQ *eine-infnr AND eine-ekorg EQ *eine-ekorg AND
           eine-esokz EQ *eine-esokz AND eine-werks EQ *eine-werks.
          MOVE *eine TO eine.  "kann oben schon mal gelesen worden sein
          sy-subrc = 0.
        ELSE.
* Holds store groups for Purchasing
          PERFORM retail_reference_site_select.
          IF sy-subrc <> 0.
            SELECT SINGLE * FROM eine WHERE infnr EQ eine-infnr
                                      AND   ekorg EQ eine-ekorg
                                      AND   esokz EQ eine-esokz
                                      AND   werks EQ eine-werks.
          ENDIF.
        ENDIF.
        IF sy-subrc EQ 0 AND eine-mtxno NE space.
*       Materialbestelltext laut Infosatz nicht relevant
          text_flag = 9.
          DELETE xt166p.
        ENDIF.
        EXIT.
      ENDLOOP.
    ENDIF.

    IF text_flag EQ 9.
*  Materialbestelltext soll nicht gedruckt werden
*  ---> er wird ersetzt durch Infobestelltext
*  ---> Reihenfolge wird von Materialbestelltext genommen
      LOOP AT xt166p WHERE tdobject EQ 'MATERIAL'
                     AND   tdid     EQ 'BEST'.
        xt166p-tdobject     = 'EINE'.
        xt166p-tdid         = 'BT'.
        xt166p-txnam        = eine-infnr.
        xt166p-txnam+10(4)  = eine-ekorg.
        xt166p-txnam+14(1)  = eine-esokz.
        xt166p-txnam+15(4)  = eine-werks.
        MODIFY xt166p.
        EXIT.
      ENDLOOP.
    ENDIF.

    CLEAR xdrflg.
* XT166P bereinigen ---------------------------------------------------*
    LOOP AT xt166p.
* Kein weiterer Text mit gleichem Reihenfolge-Kennzeichen -------------*
* wegen Verdrängung Materialbestelltext durch Infobestelltext ---------*
      IF xt166p-drflg EQ xdrflg.
        DELETE xt166p.
      ELSE.
        xdrflg = xt166p-drflg.
      ENDIF.
    ENDLOOP.

    PERFORM lesen_qm_documents.
  ENDIF.

ENDFORM.                               " PREPARE_ITEM_TEXT
