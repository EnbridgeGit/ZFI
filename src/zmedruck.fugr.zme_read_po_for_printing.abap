FUNCTION ZME_READ_PO_FOR_PRINTING.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IX_NAST) LIKE  NAST STRUCTURE  NAST
*"     REFERENCE(IX_SCREEN)
*"  EXPORTING
*"     REFERENCE(EX_RETCO)
*"     REFERENCE(DOC) TYPE  MEEIN_PURCHASE_DOC_PRINT
*"     REFERENCE(EX_NAST) LIKE  NAST STRUCTURE  NAST
*"  CHANGING
*"     REFERENCE(CX_DRUVO) TYPE  DRUVO
*"     REFERENCE(CX_FROM_MEMORY) DEFAULT SPACE
*"-----------------------------------------------------
  	
*- 1. zuerst werden die Tabellen EKKO,EKPO und EKET  gefüllt
*- 2. dann werden Kopfdaten ermittelt
*- 3. Dann kommt eine gr. Schleife über alle Positionen, in der auch die
*-    Einteilungsbearbeitung stattfindet. In dem Loop wird auch die
*-    pekpo gefüllt. Die Prüfung auf Übereinstimmung der Positionsdaten
*-    gilt für alle Positionen nicht nur für die wegen Änderungen
*     übermittelten. Daher erst prüfen und dann bereinigen.
*-    Dies gilt aber nicht für gelöscht Positionen oder solche
*-    Unterpositionen, die grundsätzlich nicht gedruckt werden.
*- 4. nach dem Positionsloop werden die Pekkofelder gefüllt und
*-    die EKPO bereinigt.
*- 5. das Feld ex_nast-tcode wird verwendet um den Fall PREVIEW zu
*-    erkennen.
*- 6. die NAST wird nicht mittels changing übergeben, damit temporäre
*-    Änderungen nicht auf die Datenbank kommen
*- 7. das Feld ix_druvo soll änderbar sein, damit spare ich eine
*-    zusätzlich Variable, da bei Preview gewechselt werden muß
*- 8. ex_retco: 1 = keine druckrelevanten Änderungen für Änderungsdruck
*-              9 = Fehler bei Daten lesen EKKO/EKPO
*-       für Nast: 0 = erfolgreich     => nast-vstat = 1
*-                 1 = fehlerhaft      => nast-vstat = 2
*-                 3 = Beleg unvollst. => kein NAST update
*----------------------------------------------------------------------*

* docu: =======================
* docu: ME_READ_PO_FOR_PRINTING
* docu: =======================

* docu: lokale Hilfsfelder
  DATA: flag, flag1,                   "allgemeines Kennzeichen
        aendernsrv,                    "bei dienstleistungspos
        xetdrk,                        "Kz für Änderung aus Dispo
        h_tabix LIKE sy-tabix.
  DATA: CS_ISC_READ_DATA_FROM_DB TYPE ISC_READ_DATA_FROM_DB. " IS2ERP.


* docu: Zurücksetzen Tabellen
  CLEAR: doc, ex_retco.
  CLEAR: rm06p.
  ex_nast = ix_nast.

* Refresh internal partner tables,
* not in PREVIEW case,
* not in case the message is sent immediately
  IF IX_SCREEN eq space and ex_nast-vsztp ne 4.             "622813
    call function 'MM_REFRESH_PARTNERS'.
  ENDIF.

* Witch tables will be filled
  DEF_BREAK '051 - Printing '.                              "#EC *

* docu: Daten beschaffen
  PERFORM read_data_from_db TABLES doc-xekpo doc-xeket
                                   doc-xekek doc-xekeh doc-xekkn
                            USING  ix_nast
                                   cx_druvo
                            CHANGING cx_from_memory doc-xtkomv
                                     doc-xekko ex_retco
                                     CS_ISC_READ_DATA_FROM_DB.
*ENHANCEMENT-POINT ME_READ_PO_FOR_PRINTING_03 SPOTS ES_SAPLMEDRUCK.
*$*$-Start: ME_READ_PO_FOR_PRINTING_03-----------------------------$*$*
*ENHANCEMENT 25  ISAUTO_SCH_EMM_SAPLMEDRUCK.    "active version
* last gr auto2.0af
*  move cs_isc_read_data_from_db-t_isautoeklwes to doc-xisautoeklwes.
*ENDENHANCEMENT.
*$*$-End:  ME_READ_PO_FOR_PRINTING_03------------------------------$*$*

  CHECK ex_retco EQ 0.
  ekko = doc-xekko.
* docu: nur freigegebene Belege ausgeben.
  IF cx_from_memory IS INITIAL AND ix_screen IS INITIAL AND
      ( NOT ekko-frgrl IS INITIAL OR NOT ix_nast-snddr IS INITIAL ).
    PERFORM protocol_update USING '390' ekko-ebeln space space space.
    ex_retco = '3'.
    EXIT.
  ENDIF.

* damit auch bei Änderungsnachrichten der Preview funktioniert
* besser ist es, vorher schon eine Warnmeldung zu prozessieren
* um den Anwender über die Funktionalität zu informieren
  IF NOT cx_from_memory IS INITIAL.
    CLEAR ex_nast-aende.
    IF cx_druvo = aend.
      cx_druvo = neu.
    ENDIF.
  ENDIF.

* docu: Lesen Änderungsbelege bei Änderungsdruck
  IF cx_druvo EQ aend OR cx_druvo EQ lpae.
    SELECT datvr uhrvr INTO (*nast-datvr, *nast-uhrvr) FROM nast
             WHERE kappl EQ ix_nast-kappl
               AND kschl EQ ix_nast-kschl
               AND objky EQ ix_nast-objky
               AND vstat EQ '1'
       ORDER BY datvr DESCENDING uhrvr DESCENDING.
      EXIT.
    ENDSELECT.

* If no NEU-Message could be found above, we read any NEU-message of
* that purchase document and use the latest timestamp of these messages
* for the fields *nast-datvr and *nast-uhrvr.
    IF sy-subrc NE 0.                                   "Begin of 549924
      DATA: neu_messagetypes LIKE t161m OCCURS 0 WITH HEADER LINE.
      DATA: BEGIN OF time_tab OCCURS 0,
              datvr LIKE nast-datvr,
              uhrvr LIKE nast-uhrvr,
            END OF time_tab.

* Read all messagetypes that are allowed for NEU-Messages
      SELECT * FROM t161m INTO TABLE neu_messagetypes
        WHERE kvewe EQ 'B'
        AND   kappl EQ ix_nast-kappl
        AND   kschl NE ix_nast-kschl
        AND   druvo EQ neu.
      IF sy-subrc EQ 0.
        LOOP AT neu_messagetypes.
          SELECT datvr uhrvr INTO time_tab FROM nast
             WHERE kappl EQ ix_nast-kappl
               AND kschl EQ neu_messagetypes-kschl
               AND objky EQ ix_nast-objky
               AND vstat EQ '1'
             ORDER BY datvr DESCENDING uhrvr DESCENDING.
            EXIT.
          ENDSELECT.
          IF sy-subrc EQ 0.
            APPEND time_tab.
          ENDIF.
        ENDLOOP.
        SORT time_tab BY datvr DESCENDING uhrvr DESCENDING.
        READ TABLE time_tab INDEX 1.
        IF sy-subrc EQ 0.
           *nast-datvr = time_tab-datvr.
           *nast-uhrvr = time_tab-uhrvr.
        ENDIF.
      ENDIF.
    ENDIF.                                                "End of 549924

    CALL FUNCTION 'ME_READ_CHANGES_EINKBELEG'
      EXPORTING
        document        = doc-xekko
        date_of_change  = *nast-datvr
        time_of_change  = *nast-uhrvr
        print_operation = cx_druvo
      TABLES
        xekpo           = doc-xekpo
        xaend           = doc-xaend.
    SORT doc-xaend BY tabkey ctxnr.
  ENDIF.

* Neu bzw. Änderungsdruck ggf. mit Preisen
  IF cx_druvo EQ neu OR cx_druvo EQ aend.
    doc-xpekko-prsdr = 'X'.
  ELSE.
    CLEAR doc-xpekko-prsdr.
  ENDIF.


* docu: Beginn des Positionsloop
  LOOP AT doc-xekpo INTO ekpo WHERE ebeln = ekko-ebeln.
    h_tabix = sy-tabix.
    CLEAR xpekpo.
    xpekpo-ebelp = ekpo-ebelp.
*- falls space auf 0 setzen
    IF ekpo-ktpnr EQ space.
      CLEAR ekpo-ktpnr.
    ENDIF.
*- nicht zu druckende Unterpositionen löschen
    IF ekpo-uptyp NE space.
      PERFORM tmsi2_lesen USING ekpo-sikgr.
      IF xtmsi2-sidru NE space.
        DELETE doc-xekpo.
        CONTINUE.                      " statt exit.
      ENDIF.
    ENDIF.

*    ENHANCEMENT-POINT ME_READ_PO_FOR_PRINTING_01 SPOTS ES_SAPLMEDRUCK.
*$*$-Start: ME_READ_PO_FOR_PRINTING_01------------------------------$*$*
*ENHANCEMENT 34  OIO_SAPLMEDRUCK.    "active version
* Prevent printing RLM mobilization items
* call function 'OIO_RN_GET_SERVICE_ITEM'               "SOGK021584 IFG
*    exporting                                           "SOGK021584 IFG
*      i_ebeln         = ekpo-ebeln                      "SOGK021584 IFG
*      i_ebelp         = ekpo-ebelp                      "SOGK021584 IFG
*    exceptions                                          "SOGK021584 IFG
*      not_found       = 1                               "SOGK021584 IFG
*      others          = 2.                              "SOGK021584 IFG
*  if sy-subrc = 0.                                      "SOGK021584 IFG
*    delete doc-xekpo.                                   "SOGK021584 IFG
*    continue.                                           "SOGK021584 IFG
*  endif.                                                "SOGK021584 IFG
*ENDENHANCEMENT.
*$*$-End:   ME_READ_PO_FOR_PRINTING_01---------------------------$*$*
* docu: nur bei Änderungsdruck gelöschte Positionen mitnehmen
    IF cx_druvo NE aend AND ekpo-loekz NE space.
      DELETE doc-xekpo.
      CONTINUE.
    ENDIF.

* docu: für alte Bestellungen Herstellerteilenr füllen
    IF ekpo-ematn EQ space.
      ekpo-ematn = ekpo-matnr.
    ENDIF.

* docu: Preiskalkulation
    PERFORM calc_current_price USING    ekko
                                        ekpo
                                        sy-datlo
                               CHANGING ekpo-netpr
                                        ekpo-netwr
                                        ekpo-effwr
                                        ekpo-brtwr
                                        ekpo-zwert
                                        ekpo-bonba
                                        ekpo-kzwi1
                                        ekpo-kzwi2
                                        ekpo-kzwi3
                                        ekpo-kzwi4
                                        ekpo-kzwi5
                                        ekpo-kzwi6.

*- Bei Retourenbestellungen Wertfelder ändern
    IF ekpo-retpo NE space.
      ekpo-netwr = ekpo-netwr * -1.
      ekpo-brtwr = ekpo-brtwr * -1.
      ekpo-effwr = ekpo-effwr * -1.
      ekpo-bonba = ekpo-bonba * -1.
      ekpo-kzwi1 = ekpo-kzwi1 * -1.
      ekpo-kzwi2 = ekpo-kzwi2 * -1.
      ekpo-kzwi3 = ekpo-kzwi3 * -1.
      ekpo-kzwi4 = ekpo-kzwi4 * -1.
      ekpo-kzwi5 = ekpo-kzwi5 * -1.
      ekpo-kzwi6 = ekpo-kzwi6 * -1.
    ENDIF.

*- Bei Stammkonditionen und Zielmenge 0 Wertfelder zurücksetzen
    IF ( ekko-stako NE space OR ekko-bstyp EQ bstyp-kont ) AND
         ekpo-ktmng EQ 0.
      CLEAR: ekpo-zwert,    ekpo-brtwr,       ekpo-effwr,
             ekpo-bonba,    ekpo-kzwi1,       ekpo-kzwi2,
             ekpo-kzwi3,    ekpo-kzwi4,       ekpo-kzwi5,
             ekpo-kzwi6.
    ENDIF.

* docu: Kennzeichen für Hilfsstruktur PEKKO setzen
*- Kennzeichen Preisdruck gegebenenfalls zurücknehmen
    IF ekpo-prsdr EQ space AND NOT doc-xpekko-prsdr IS INITIAL.
      CLEAR doc-xpekko-prsdr.
    ENDIF.
*- Kennzeichen Unterpositionen
    IF ekpo-uptyp NE space AND doc-xpekko-hsamm IS INITIAL.
      doc-xpekko-hsamm = 'X'.
    ENDIF.
*- Position mit druckrelevanten Einteilungen aus der Disposition
    IF ekpo-etdrk EQ '1' AND cx_druvo EQ lpae.
      xetdrk = 'X'.
    ENDIF.

* docu: letzter WE, WE-FZ, Lieferscheinnummer, etc ermitteln
    CALL FUNCTION 'ME_READ_LAST_GR'
      EXPORTING
        i_ebeln  = ekpo-ebeln
        i_ebelp  = ekpo-ebelp
        i_number = 3
      IMPORTING
        e_lfdat  = xpekpo-lfdat
        e_xblnr  = xpekpo-xblnr
        e_menge  = xpekpo-lwemg
        e_budat  = xpekpo-budat
        e_fzwmg  = xpekpo-wemng
      TABLES
        e_pekpo  = doc-xpekpo
      EXCEPTIONS
        OTHERS   = 1.

* docu: Einteilung mit Bestätigungen für Mahndruck abmischen
    IF cx_druvo EQ lpma OR cx_druvo EQ mahn.
      PERFORM eket_ekes_abmischen TABLES doc-xeket
                                  USING  ekpo.
    ENDIF.

* To determine whether it is a reminder or a dunning          "^_1128069
* notice.
* Only for Scheduling Agreements with release creation profile.
      IF cx_druvo EQ lpma.
        PERFORM pruefen_mahnung_ekpo USING ekko ekpo
                                CHANGING ekpo-mahnz.
        MODIFY doc-xekpo FROM ekpo.
      ENDIF.                                                  "v_1128069

    REFRESH heket.
* docu: Begin des Einteilungsloop
    LOOP AT doc-xeket INTO eket WHERE ebeln = ekko-ebeln
                                  AND ebelp = ekpo-ebelp.
* docu: nichtrelevante Einteilungen löschen
      IF ekko-bstyp = bstyp-lfpl.                           "608519
* 1. Fall: Bei Lieferplänen mit und ohne Historie           608519
        IF cx_druvo = lpma.
          IF eket-ameng EQ 0 AND
             eket-menge LE 0.
            DELETE doc-xeket.
            CONTINUE.
          ENDIF.
        ELSE.
          IF eket-menge EQ eket-ameng AND
             eket-menge LE eket-wemng.                      "658316
            DELETE doc-xeket.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSEIF ekpo-pstyp NE pstyp-dien.                      "542761
* 2. Fall: bei Bestellung, Anfrage oder Kontrakt aber
*          nicht bei Dienstleistung
        IF cx_druvo = mahn.
          IF eket-menge EQ 0.
            DELETE doc-xeket.
            CONTINUE.
          ENDIF.
        ELSE.
          IF eket-menge LE eket-wemng.
            DELETE doc-xeket.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

* docu: Einheitliche Einteilungsdaten erfassen (Datum und Uhrzeit)
      IF xpekpo-eindt IS INITIAL AND xpekpo-lpein NE '*'.
        xpekpo-lpein = eket-lpein.
        xpekpo-uzeit = eket-uzeit.
        xpekpo-eindt = eket-eindt.
      ENDIF.

**-- Mehrere Termine pro Position                         "932937
If ekpo-pstyp NE pstyp-dien.                              "1042349
      IF xpekpo-lpein NE '*' AND ( xpekpo-eindt NE eket-eindt OR
                                  xpekpo-uzeit NE eket-uzeit OR
                                  xpekpo-lpein NE eket-lpein    ) .
        CLEAR: xpekpo-eindt, xpekpo-uzeit.
        xpekpo-lpein = '*'.
      ENDIF.
endif.                                                    "1042349

*-- Ergebnisse aus Änderungsbelegen in Einteilungstabelle übernehmen
      IF ekko-bstyp EQ bstyp-lfpl AND cx_druvo EQ lpae.
        MOVE-CORRESPONDING eket TO *eket.
        MOVE-CORRESPONDING eket TO heket.
        PERFORM alte_daten_ermitteln TABLES doc-xaend
                                     CHANGING heket flag.
        IF flag = 'X'.
          IF NOT heket-lpein IS INITIAL.
            eket-lpein = heket-lpein.
          ENDIF.
          IF NOT heket-tflag IS INITIAL.
            eket-uzeit = heket-uzeit.
          ENDIF.
          CLEAR eket-menge.
          CLEAR eket-wemng.
          CLEAR eket-mahnz.
          eket-eindt = eket-altdt.
          CLEAR *eket-ameng.
          CLEAR *eket-altdt.
          APPEND *eket TO heket.
        ELSE.                          "Nur Änderungen über MRP
*--  ansonsten auf basis altdt duplizieren
          IF eket-eindt NE eket-altdt  AND NOT eket-altdt IS INITIAL.
            CLEAR *eket-ameng.
            CLEAR *eket-altdt.
            CLEAR eket-menge.
            CLEAR eket-wemng.
            CLEAR eket-mahnz.
            eket-eindt = eket-altdt.
            APPEND *eket TO heket.
          ENDIF.
        ENDIF.
      ENDIF.
      IF *eket-eindt NE *eket-altdt.
        MODIFY doc-xeket FROM eket.
      ENDIF.

* docu: Lieferantencharge übernehmen, falls vorhanden
      IF NOT eket-licha IS INITIAL.
        xpekpo-licha = eket-licha.
      ENDIF.

* docu: setzen des Mahnzaehlers
      IF cx_druvo EQ mahn OR cx_druvo EQ lpma.
        PERFORM pruefen_mahnung USING ekko ekpo eket
                                CHANGING eket-mahnz.
        MODIFY doc-xeket FROM eket.
      ENDIF.
    ENDLOOP.
    APPEND LINES OF heket TO doc-xeket.
* docu: Ende Einteilungsloop

* docu: Lieferdatum und -uhrzeit festlegen
* Lieferdatum nicht für alle Positionen gleich ------------------------*
    IF doc-xpekko-lpein NE '*'.
      IF xpekpo-lpein EQ '*'.      "es gibt versch. Einteilungstermine
        doc-xpekko-lpein = '*'.
        CLEAR doc-xpekko-eindt.
      ELSE.
        IF xpekpo-lpein NE 0.
          IF doc-xpekko-lpein EQ space.
            MOVE xpekpo-eindt TO doc-xpekko-eindt.
            MOVE xpekpo-lpein TO doc-xpekko-lpein.
          ELSE.
            IF doc-xpekko-eindt NE xpekpo-eindt OR
               doc-xpekko-lpein NE xpekpo-lpein.
              CLEAR doc-xpekko-eindt.
              doc-xpekko-lpein = '*'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*- Bei Uhrzeitangabe Lieferdatum nicht im Kopf
    IF NOT xpekpo-uzeit IS INITIAL.
      CLEAR doc-xpekko-eindt.
      doc-xpekko-lpein = '*'.
    ENDIF.
*- Bei Anfrage und LP mit Abrufdokumentation Mahnzähler auf Position
    xpekpo-mahnz = ekpo-mahnz.

* docu: Feststellen, ob einheitliches Werk
    IF h_tabix EQ 1.
      MOVE ekpo-werks TO doc-xpekko-werks.
    ELSE.
      IF doc-xpekko-werks NE ekpo-werks.
        doc-xpekko-werks = '****'.
      ENDIF.
    ENDIF.
    IF ekpo-adrnr NE space OR
       ekpo-adrn2 NE space OR
       ekpo-emlif NE space OR
       ekpo-kunnr NE space.
      doc-xpekko-werks = '****'.
    ENDIF.
* docu: Feststellen, ob einheitliche Anlieferungsanschrift
    IF h_tabix EQ 1.
      MOVE ekpo-adrnr TO doc-xpekko-adrnr.
    ELSE.
      IF doc-xpekko-adrnr NE ekpo-adrnr.
        doc-xpekko-adrnr = '**********'.
      ENDIF.
    ENDIF.
* docu: Feststellen, ob einheitliche Adressnummer
    IF h_tabix EQ 1.
      MOVE ekpo-adrn2 TO doc-xpekko-adrn2.
    ELSE.
      IF doc-xpekko-adrn2 NE ekpo-adrn2.
        doc-xpekko-adrn2 = '**********'.
      ENDIF.
    ENDIF.
* docu: Feststellen, ob einheitliche Kundennummer
    IF h_tabix EQ 1.
      MOVE ekpo-kunnr TO doc-xpekko-kunnr.
    ELSE.
      IF doc-xpekko-kunnr NE ekpo-kunnr.
        doc-xpekko-kunnr = '**********'.
      ENDIF.
    ENDIF.
* docu: Feststellen, ob einheitliche Lieferantennummer
    IF h_tabix EQ 1.
      MOVE ekpo-emlif TO doc-xpekko-emlif.
    ELSE.
      IF doc-xpekko-emlif NE ekpo-emlif.
        doc-xpekko-emlif = '**********'.
      ENDIF.
    ENDIF.
* docu: Feststellen, ob einheitliche Auftragsbestätigung
    IF h_tabix EQ 1.
      MOVE ekpo-labnr TO doc-xpekko-labnr.
    ELSE.
      IF doc-xpekko-labnr NE ekpo-labnr.
        doc-xpekko-labnr = '****'.
      ENDIF.
    ENDIF.
* docu: Feststellen, ob Auftragsbestätigungspflicht in jeder Position
    IF h_tabix EQ 1.
      MOVE ekpo-kzabs TO doc-xpekko-kzabs.
    ELSE.
      IF doc-xpekko-kzabs NE ekpo-kzabs.
        doc-xpekko-kzabs = '*'.
      ENDIF.
    ENDIF.

* docu: Feststellen einheitliche Incoterms
    IF ekko-inco1 NE space AND ekpo-inco1 EQ space.
      ekpo-inco1 = ekko-inco1.
      ekpo-inco2 = ekko-inco2.
    ENDIF.
    IF ekpo-stapo EQ space OR ekpo-loekz NE space.
      IF flag1 IS INITIAL.
        doc-xpekko-inco1 = ekpo-inco1.
        doc-xpekko-inco2 = ekpo-inco2.
        flag1 = 'X'.
      ELSE.
        IF doc-xpekko-inco1 NE '***' AND
             ( ekpo-inco1 NE doc-xpekko-inco1 OR
               ekpo-inco2 NE doc-xpekko-inco2    ) .
          doc-xpekko-inco1 = '***'.
          CLEAR doc-xpekko-inco2.
        ENDIF.
      ENDIF.
    ENDIF.

* docu: Feststellen, ob einheitliches Angebotsdatum
    IF doc-xekko-bstyp EQ bstyp-anfr.
      IF ekpo-agdat NE doc-xekko-angdt.
        CLEAR doc-xekko-angdt.
      ENDIF.
    ENDIF.

* docu: Nettowert aufaddieren
    IF ekpo-stapo EQ space AND ekpo-loekz EQ space.
      doc-xpekko-netwr = doc-xpekko-netwr + ekpo-netwr.
    ENDIF.

* docu: Feststellen einheitl. Warenempfänger/Abladestelle in Position
    LOOP AT doc-xekkn INTO ekkn
                      WHERE ebeln EQ ekpo-ebeln
                      AND   ebelp EQ ekpo-ebelp
                      AND ( wempf NE space OR ablad NE space ).
      IF xpekpo-wempf IS INITIAL AND xpekpo-ablad IS INITIAL.
        xpekpo-wempf = ekkn-wempf.
        xpekpo-ablad = ekkn-ablad.
      ELSE.
        IF xpekpo-wempf NE ekkn-wempf OR xpekpo-ablad NE ekkn-ablad.
          CLEAR: xpekpo-ablad, xpekpo-wempf.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF NOT xpekpo-wempf IS INITIAL OR NOT xpekpo-ablad IS INITIAL.
      READ TABLE doc-xekkn INTO ekkn WITH KEY
                   ebeln = ekpo-ebeln
                   ebelp = ekpo-ebelp
                   ablad = space
                   wempf = space.
      IF sy-subrc EQ 0.
        CLEAR: xpekpo-wempf, xpekpo-ablad.
      ENDIF.
    ENDIF.

* docu: Ermitteln der insgesamt bereits bestätigten Menge
    IF cx_druvo EQ aufb AND ekpo-bstae NE space.
      SELECT SINGLE * FROM t163d WHERE ibtyp EQ '1'.
      SELECT SUM( menge ) INTO (xpekpo-bsmng) FROM ekes
                   WHERE ebeln = ekpo-ebeln
                     AND ebelp = ekpo-ebelp
                     AND ebtyp = t163d-ebtyp.
    ENDIF.

* docu: Feststellen einheitlicher Warenempfänger/Abladestelle im Beleg
    IF doc-xpekko-wempf NE '**' AND doc-xpekko-ablad NE '**'.
      IF h_tabix EQ 1.
        doc-xpekko-wempf = xpekpo-wempf.
        doc-xpekko-ablad = xpekpo-ablad.
      ELSE.
        IF doc-xpekko-wempf NE xpekpo-wempf OR
           doc-xpekko-ablad NE xpekpo-ablad.
          doc-xpekko-wempf = '**'.
          doc-xpekko-ablad = '**'.
        ENDIF.
      ENDIF.
    ENDIF.

* docu: Änderungsbelegtabelle ergänzen
    PERFORM ergaenzen_xaend USING xpekpo
                            CHANGING doc-xaend.
    LOOP AT doc-xpekpo INTO pekpo WHERE ebelp EQ ekpo-ebelp.
      pekpo-lpein = xpekpo-lpein.
      pekpo-eindt = xpekpo-eindt.
      pekpo-uzeit = xpekpo-uzeit.
      pekpo-wempf = xpekpo-wempf.
      pekpo-ablad = xpekpo-ablad.
      pekpo-bsmng = xpekpo-bsmng.
      MODIFY doc-xpekpo FROM pekpo.
    ENDLOOP.                           "pekpo
    IF sy-subrc NE 0.
      APPEND xpekpo TO doc-xpekpo.
    ENDIF.
    MODIFY doc-xekpo FROM ekpo.        "wegen Incoterms zum Beispiel
  ENDLOOP.                             "ekpo
* docu: Ende des Positionsloop

* erst mal die Kopfänderung rausschmeissen, kommt wieder rein ---------*
  IF ekko-bstyp EQ bstyp-anfr AND ekko-angdt NE 0.
    LOOP AT doc-xaend INTO xaend WHERE ctxnr EQ 'K4'.
      DELETE doc-xaend.
    ENDLOOP.
* keine Angebotsfriständerung auf Position ausgeben -------------------*
    PERFORM xaend_bereinigen USING '05' 'K4'
                             CHANGING doc-xaend.
  ENDIF.

*----------------------------------------------------------------------*
* ab hier wird doc-xpekko überarbeitet
*----------------------------------------------------------------------*
*- Wenn Lieferdatum nicht für alle Positionen gleich auch LPEIN clearen
  IF doc-xpekko-lpein EQ '*'.
    CLEAR doc-xpekko-lpein.
  ELSE.
*-- Bei Mahnung Lieferdatum immer auf Positionsebene drucken
    IF cx_druvo EQ mahn.
      CLEAR: doc-xpekko-lpein, doc-xpekko-eindt.
    ENDIF.
  ENDIF.

* Bei Lieferplanänderung stört PEKKO-EINDT, falls es sitzt ------------*
  IF ekko-bstyp EQ bstyp-lfpl AND ( cx_druvo EQ aend OR
                                    cx_druvo EQ aufb ).
    CLEAR: doc-xpekko-lpein, doc-xpekko-eindt.
  ENDIF.


* docu: Keine einheitlichen Incoterms
  IF doc-xpekko-inco1 EQ '***'.
    CLEAR doc-xpekko-inco1.
    CLEAR doc-xpekko-inco2.
  ENDIF.

* docu: Kein einheitliches Werk bei allen Positionen
  IF doc-xpekko-werks EQ '****'.
    CLEAR doc-xpekko-werks.
  ENDIF.

* docu: Keine einheitliche Kundennummer bei allen Positionen
  IF doc-xpekko-kunnr EQ '**********'.
    CLEAR doc-xpekko-kunnr.
  ENDIF.

* docu: Keine einheitliche Lieferantennummer bei allen Positionen
  IF doc-xpekko-emlif EQ '**********'.
    CLEAR doc-xpekko-emlif.
  ENDIF.

* docu: Keine einheitliche Adressnummer bei allen Positionen
  IF doc-xpekko-adrn2 EQ '**********'.
    CLEAR doc-xpekko-adrn2.
  ENDIF.

* docu: Keine einheitliche Anlieferungsanschrift bei allen Positionen
  IF doc-xpekko-adrnr EQ '**********'.
    CLEAR doc-xpekko-adrnr.
  ENDIF.

* docu: Keine einheitliche Auftragsbestaetigung bei allen Positionen
  IF doc-xpekko-labnr EQ '****'.
    CLEAR doc-xpekko-kzabs.
    CLEAR doc-xpekko-labnr.
  ENDIF.

* docu: Nicht in jeder Position Auftragsbestätigungspflicht
  IF doc-xpekko-kzabs EQ '*'.
    CLEAR doc-xpekko-kzabs.
  ENDIF.

* Lesen Anlieferungsanschrift -----------------------------------------*
* keine Anschriftenänderung auf Position ausgeben ---------------------*
* Einträge suchen, die von XXXXXXnummer auf Adresse geändert wurden ---*
  IF doc-xpekko-adrnr NE space OR
     doc-xpekko-emlif NE space OR
     doc-xpekko-adrn2 NE space OR
     doc-xpekko-kunnr NE space.
    PERFORM xaend_bereinigen USING '04' 'S5-1'
                             CHANGING  doc-xaend.
  ENDIF.

* docu: keine einheitliche Abladestelle/Warenempfänger
  IF doc-xpekko-ablad EQ '**' AND doc-xpekko-wempf EQ '**'.
    CLEAR: doc-xpekko-ablad, doc-xpekko-wempf.
  ENDIF.
  IF doc-xpekko-adrnr EQ space AND
     doc-xpekko-emlif EQ space AND
     doc-xpekko-adrn2 EQ space AND
     doc-xpekko-kunnr EQ space AND
     doc-xpekko-werks EQ space.
    CLEAR: doc-xpekko-ablad, doc-xpekko-wempf.
  ELSE.
    IF NOT ( doc-xpekko-ablad IS INITIAL AND
             doc-xpekko-wempf IS INITIAL ).
      PERFORM xaend_bereinigen USING '06' 'AC1'
                               CHANGING  doc-xaend.
    ENDIF.
  ENDIF.

* docu: Positionstabelle bereinigen
  CLEAR aendernsrv.
  PERFORM xekpo_bereinigen USING cx_druvo
                           CHANGING doc  aendernsrv.
* docu: Kundenspezifische Ergänzungen
  CALL CUSTOMER-FUNCTION '001'
         EXPORTING ix_druvo = cx_druvo
         IMPORTING ex_retco = ex_retco
         CHANGING cx_doc = doc
  EXCEPTIONS OTHERS = 1.
  IF sy-subrc NE 0.
    PERFORM protocol_update USING '140' ekko-ebeln space space space.
    ex_retco = 1.
    EXIT.
  ELSE.
    IF ex_retco NE 0.
      EXIT.
    ENDIF.
  ENDIF.

* Check whether items for the output are existing: note 736829
* header texts changes printed correctly - Note 794176
  LOOP AT doc-xaend INTO xaend.                             "794176
    IF xaend-text_case EQ space AND xaend-ebelp NE '00000'. "794176
      DESCRIBE TABLE doc-xekpo LINES sy-tfill.
      IF sy-tfill = 0.
        IF cx_druvo = 2.                                    "769216
          PERFORM protocol_update USING '140' ekko-ebeln space space
          space.
        ELSE.
          PERFORM protocol_update USING '141' ekko-ebeln space space
          space.
        ENDIF.
        ex_retco = 1.
        EXIT.
      ENDIF.
    ENDIF.                                                  "794176
  ENDLOOP.                                                  " 794176

* Lieferplanänderung --> entweder Änderungsbelege aus Dialog (ME38)
*                    --> oder Kennzeichen in Position aus Disposition
  IF cx_druvo EQ lpae.
    IF doc-xaend[] IS INITIAL AND xetdrk EQ space.
      PERFORM protocol_update USING '140' ekko-ebeln space space space.
      ex_retco = 1.
      EXIT.
    ENDIF.
  ENDIF.

* docu: Feststellen, ob Änderungen für Druck vorhanden sind
  IF cx_druvo EQ aend.

* In some cases the changed delivery address is considered as inserted
* address and could be print out though it's non print relevant in the
* customizing. This is not allowed.                         "583449
    DATA: changed(1) type c.                                "583449
    LOOP AT doc-xaend into xaend.
      IF xaend-TABNAME(3) = 'ADR' and xaend-FNAME is initial and
         xaend-CHNGIND = 'I'.                               "583449
        continue.                                           "583449
      ELSE.                                                 "583449
        changed = 'X'.                                      "583449
        exit.                                               "583449
      ENDIF.                                                "583449
    ENDLOOP.                                                "583449

    IF NOT changed = 'X'.                                   "583449
      PERFORM protocol_update USING '140'
                     ekko-ebeln space space space.          "583449
      ex_retco = 1.                                         "583449
      clear changed.                                        "583449
      EXIT.                                                 "583449
    ENDIF.                                                  "583449

  ENDIF.

* keine Ausgabe, falls keine Positionen oder
* nur noch Positionen mit Standardartikel vorhanden
  IF cx_druvo NE aend.
    LOOP AT doc-xekpo INTO ekpo WHERE stapo EQ space.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      PERFORM protocol_update USING '140' ekko-ebeln space space space.
      ex_retco = 1.
      EXIT.
    ENDIF.
  ENDIF.
  SORT doc-xeket BY ebeln ebelp eindt uzeit etenr.
*ENHANCEMENT-POINT ME_READ_PO_FOR_PRINTING_02 SPOTS ES_SAPLMEDRUCK.
*$*$-Start: ME_READ_PO_FOR_PRINTING_02------------------------------$*$*
*ENHANCEMENT 3  /NFM/MM_SAPLMEDRUCK.    "active version
* Check activation of NF Metal processing:                       "/NFM
* include /nfm/tbasic_get.                                        "/NFM/
* if /nfm/g_tbasic-active = 'X'.                                  "/NFM/
*  read NF data:                                                 "/NFM/
*   perform /nfm/orderpos_lesen using ekko-ebeln.                 "/NFM/
*endif.                                                          "/NFM/
*ENDENHANCEMENT.
*$*$-End: ME_READ_PO_FOR_PRINTING_02--------------------------------$*$*

ENDFUNCTION.

                              			
	

                              			
	
