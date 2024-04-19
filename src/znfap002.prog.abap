* 2000/08/31 mdemeest 4.6b Copied RFFORI06 to ZNFAP002

************************************************************************
*                                                                      *
* Includebaustein RFFORI06 zu den Formulardruckprogrammen RFFOxxxz     *
* mit Unterprogrammen für den Druck des Avises                         *
*                                                                      *
************************************************************************


*----------------------------------------------------------------------*
* FORM AVIS                                                            *
*----------------------------------------------------------------------*
* Druck Avis                                                           *
* Gerufen von END-OF-SELECTION (RFFOxxxz)                              *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM AVIS.

*------------------------------  UGL Change ------------------------ UGL
data:  old_zaldt  like reguh-zaldt,                                 "UGL
       old_chect  like regud-chect.                                 "UGL
*---------------------------  End of UGL Change -------------------- UGL

*----------------------------------------------------------------------*
* Abarbeiten der extrahierten Daten                                    *
*----------------------------------------------------------------------*
  IF FLG_SORT NE 2.
    SORT BY AVIS.
    FLG_SORT = 2.
  ENDIF.

  LOOP.


*-- Neuer zahlender Buchungskreis --------------------------------------
    AT NEW REGUH-ZBUKR.

      PERFORM BUCHUNGSKREIS_DATEN_LESEN.

    ENDAT.


*-- Neuer Zahlweg ------------------------------------------------------
    AT NEW REGUH-RZAWE.

      FLG_PROBEDRUCK = 0.              "für diesen Zahlweg wurde noch
      FLG_SGTXT      = 0.              "kein Probedruck durchgeführt

      IF REGUH-RZAWE NE SPACE.
        PERFORM ZAHLWEG_DATEN_LESEN.

*       Zahlungsformular nur zum Lesen öffnen
        IF NOT T042E-ZFORN IS INITIAL.
          CALL FUNCTION 'OPEN_FORM'
               EXPORTING
                    FORM     = T042E-ZFORN
                    DIALOG   = SPACE
                    DEVICE   = 'SCREEN'
                    LANGUAGE = T001-SPRAS
               EXCEPTIONS
                    FORM     = 1.
          IF SY-SUBRC EQ 0.            "Formular existiert

*           Formular auf Segmenttext (Global &REGUP-SGTXT) untersuchen
            IF PAR_XDTA EQ SPACE.
              IF T042E-XAVIS NE SPACE AND T042E-ANZPO NE 99.
                CALL FUNCTION 'READ_FORM_LINES'
                     EXPORTING
                          ELEMENT = HLP_EP_ELEMENT
                     TABLES
                          LINES   = TAB_ELEMENT
                     EXCEPTIONS
                          ELEMENT = 1.
                IF SY-SUBRC EQ 0.
                  LOOP AT TAB_ELEMENT.
                    IF    TAB_ELEMENT-TDLINE   CS 'REGUP-SGTXT'
                      AND TAB_ELEMENT-TDFORMAT NE '/*'.
                      FLG_SGTXT = 1.   "Global für Segmenttext existiert
                      EXIT.
                    ENDIF.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
            CALL FUNCTION 'CLOSE_FORM'.
          ENDIF.
        ENDIF.

      ENDIF.

*     Überschrift für den Formularabschluß modifizieren
      T042Z-TEXT1 = TEXT_001.

*     Vorschlag für die Druckparameter aufbauen
      PERFORM FILL_ITCPO USING PAR_PRIA
                               'LIST5S'
                               SPACE          "par_sofa via tab_ausgabe!
                               HLP_AUTH.

      EXPORT ITCPO TO MEMORY ID 'RFFORI06_ITCPO'.

    ENDAT.


*-- Neue Hausbank ------------------------------------------------------
    AT NEW REGUH-UBNKL.

      PERFORM HAUSBANK_DATEN_LESEN.

*     Felder für Formularabschluß initialisieren
      CNT_AVISE      = 0.
      CNT_AVEDI      = 0.
      CNT_AVFAX      = 0.
      SUM_ABSCHLUSS  = 0.
      SUM_ABSCHL_EDI = 0.
      SUM_ABSCHL_FAX = 0.
      REFRESH TAB_EDI_AVIS.

    ENDAT.


*-- Neue Empfängerbank -------------------------------------------------
    AT NEW REGUH-ZBNKL.

      PERFORM EMPFBANK_DATEN_LESEN.

    ENDAT.


*-- Neue Zahlungsbelegnummer -------------------------------------------
    AT NEW REGUH-VBLNR.

*     Prüfe, ob Avis auf Papier erzwungen wird
*     Check if advice on paper is forced
      IF FLG_PAPIERAVIS EQ 1.
        REGUH-EDIAV = SPACE.
      ENDIF.

*     Prüfe, ob HR-Formular zu verwenden ist
*     Check if HR-form is to be used
      HRXBLNR = REGUP-XBLNR.
      IF ( HLP_LAUFK EQ 'P' OR
           HRXBLNR-TXTSL EQ 'HR' AND HRXBLNR-TXERG EQ 'GRN' )
       AND HRXBLNR-XHRFO NE SPACE.
        HLP_XHRFO = 'X'.
      ELSE.
        HLP_XHRFO = SPACE.
      ENDIF.

*     HR-Formular besorgen
*     read HR form
      IF HLP_XHRFO EQ 'X'.
        PERFORM HR_FORMULAR_LESEN.
      ENDIF.

*     Prüfung, ob Avis erforderlich
      CNT_ZEILEN = 0.
      IF HLP_XHRFO EQ SPACE.
        IF FLG_SGTXT EQ 1.
          CNT_ZEILEN = REGUH-RPOST + REGUH-RTEXT.
        ELSE.
          CNT_ZEILEN = REGUH-RPOST.
        ENDIF.
      ELSE.
        DESCRIBE TABLE PFORM LINES CNT_ZEILEN.
      ENDIF.
      FLG_KEIN_DRUCK = 0.
      IF REGUH-EDIAV EQ 'V'.
*       Avis bereits versendet
        FLG_KEIN_DRUCK = 1.            "kein Druck erforderlich
      ELSEIF REGUH-RZAWE NE SPACE.
*       Avis zu Formular
        IF HLP_ZEILEN EQ 0 AND PAR_XDTA EQ SPACE.
          IF T042E-XAVIS EQ SPACE OR CNT_ZEILEN LE T042E-ANZPO.
            FLG_KEIN_DRUCK = 1.        "kein Druck erforderlich
          ENDIF.
*       Avis zum DTA
        ELSE.
          CLEAR TAB_KEIN_AVIS.
          MOVE-CORRESPONDING REGUH TO TAB_KEIN_AVIS.
          READ TABLE TAB_KEIN_AVIS.
          IF SY-SUBRC EQ 0.
            FLG_KEIN_DRUCK = 1.        "kein Druck erforderlich
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM ZAHLUNGS_DATEN_LESEN.
      IF REGUH-EDIAV NA ' V' AND HLP_XHRFO EQ SPACE.
        PERFORM SUMMENFELDER_INITIALISIEREN.
      ENDIF.

*     Schecknummer bei vornumerierten Schecks
      CLEAR REGUD-CHECT.
      READ TABLE TAB_SCHECKS WITH KEY
        ZBUKR = REGUH-ZBUKR
        VBLNR = REGUH-VBLNR.
      IF SY-SUBRC EQ 0.
        REGUD-CHECT = TAB_SCHECKS-CHECT.
      ELSEIF FLG_SCHECKNUM EQ 1.
        IF ZW_XVORL EQ SPACE.
          IF HLP_LAUFK NE 'P'.         "FI-Beleg vorhanden?
            SELECT * FROM PAYR
              WHERE ZBUKR EQ REGUH-ZBUKR
              AND   VBLNR EQ REGUH-VBLNR
              AND   GJAHR EQ REGUD-GJAHR
              AND   VOIDR EQ 0.
            ENDSELECT.
            SY-MSGV1 = REGUH-ZBUKR.
            SY-MSGV2 = REGUD-GJAHR.
            SY-MSGV3 = REGUH-VBLNR.
          ELSE.                        "HR-Abrechnung vorhanden?
            SELECT * FROM PAYR
              WHERE PERNR EQ REGUH-PERNR
              AND   SEQNR EQ REGUH-SEQNR
              AND   BTZNR EQ REGUH-BTZNR
              AND   VOIDR EQ 0.
            ENDSELECT.
            SY-MSGV1 = REGUH-PERNR.
            SY-MSGV2 = REGUH-SEQNR.
            SY-MSGV3 = REGUH-BTZNR.
          ENDIF.
          IF SY-SUBRC EQ 0.
            REGUD-CHECT = PAYR-CHECT.
          ELSE.
            READ TABLE ERR_FW_SCHECK WITH KEY
               ZBUKR = REGUH-ZBUKR
               VBLNR = REGUH-VBLNR.
            IF SY-SUBRC NE 0.
              IF SY-BATCH EQ SPACE.    "check does not exist
                MESSAGE A564(FS) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3.
              ELSE.
                MESSAGE S564(FS) WITH SY-MSGV1 SY-MSGV2 SY-MSGV3.
                MESSAGE S549(FS).
                STOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          REGUD-CHECT = 'TEST'.
        ENDIF.
      ENDIF.

*     Berechnung Anzahl benötigter Wechsel
      IF REGUH-WEAMX EQ 0.
        REGUD-WECAN = 1.
      ELSE.
        REGUD-WECAN = REGUH-WEAMX.
        IF REGUH-WEHRS NE 0.
          ADD 1 TO REGUD-WECAN.
        ENDIF.
      ENDIF.

    ENDAT.


*-- Verarbeitung der Einzelposten-Informationen ------------------------
    AT DATEN.

      PERFORM EINZELPOSTENFELDER_FUELLEN.
      IF FLG_PAPIERAVIS EQ 1.
        REGUH-EDIAV = SPACE.
      ENDIF.
      IF REGUH-EDIAV NA ' V' AND HLP_XHRFO EQ SPACE.
        PERFORM SUMMENFELDER_FUELLEN.
      ENDIF.

    ENDAT.


*-- Ende der Zahlungsbelegnummer ---------------------------------------
    AT END OF REGUH-VBLNR.

*     Zahlungsbelegnummer bei Saldo-Null-Mitteilungen und
*     Zahlungsanforderungen nicht ausgeben
      IF ( REGUH-RZAWE EQ SPACE AND REGUH-XVORL EQ SPACE )
        OR T042Z-XZANF NE SPACE.
        REGUH-VBLNR = SPACE.
      ENDIF.

*     Stets Ausgabe via EDI, falls möglich
      IF FLG_PAPIERAVIS EQ 1.
        REGUH-EDIAV = SPACE.
      ENDIF.
      CLEAR REGUD-AVEDN.
      IF REGUH-EDIAV NA ' V' AND HLP_XHRFO EQ SPACE.
*-------------------------  UGL Change  --------------------------- UGL
        read table tab_regup index 1.                              "UGL
* change posting date in IDOC to baseline date on invoice           UGL
        old_zaldt = reguh-zaldt.                                   "UGL
        reguh-zaldt = tab_regup-zfbdt.                             "UGL
* change document payment number in IDOC to SAP-assigned cheque#    UGL
        old_chect = regud-chect.                                   "UGL
        reguh-vblnr = regud-chect.                                 "UGL
*------------------------  End of UGL Change  --------------------- UGL
        CALL FUNCTION 'FI_EDI_REMADV_PEXR2001_OUT'
             EXPORTING
                  REGUH_IN   = REGUH
                  REGUD_IN   = REGUD
                  XEINZ_IN   = REGUD-XEINZ
             IMPORTING
                  DOCNUM_OUT = REGUD-AVEDN
             TABLES
                  TAB_REGUP  = TAB_REGUP
             EXCEPTIONS
                  OTHERS     = 4.
        IF SY-SUBRC EQ 0.
          ADD 1            TO CNT_AVEDI.
          ADD REGUH-RBETR  TO SUM_ABSCHL_EDI.
          WRITE:
            CNT_AVISE      TO REGUD-AVISE,
            CNT_AVEDI      TO REGUD-AVEDI,
            CNT_AVFAX      TO REGUD-AVFAX,
            SUM_ABSCHLUSS  TO REGUD-SUMME CURRENCY T001-WAERS,
            SUM_ABSCHL_EDI TO REGUD-SUEDI CURRENCY T001-WAERS,
            SUM_ABSCHL_FAX TO REGUD-SUFAX CURRENCY T001-WAERS.
          TRANSLATE:
            REGUD-AVISE USING ' *',
            REGUD-AVEDI USING ' *',
            REGUD-AVFAX USING ' *',
            REGUD-SUMME USING ' *',
            REGUD-SUEDI USING ' *',
            REGUD-SUFAX USING ' *'.
          TAB_EDI_AVIS-REGUH = REGUH.
          TAB_EDI_AVIS-REGUD = REGUD.
          APPEND TAB_EDI_AVIS.
          FLG_KEIN_DRUCK = 1.
        ENDIF.
*-------------------------------  UGL Change  --------------------- UGL
* restore posting date                                              UGL
  reguh-zaldt = old_zaldt.                                         "UGL
* restore payment document number                                   UGL
  regud-chect = old_chect.                                         "UGL
*------------------------------- End of UGL Change ---------------  UGL

      ENDIF.

*     Ausgabe auf Fax oder Drucker (nur falls notwendig)
      IF FLG_KEIN_DRUCK EQ 0.

        PERFORM AVIS_NACHRICHTENART.
        PERFORM AVIS_OEFFNEN USING 'X'.
        PERFORM ZAHLUNGS_DATEN_LESEN_HLP.
        PERFORM SUMMENFELDER_INITIALISIEREN.
        PERFORM AVIS_SCHREIBEN.
        PERFORM AVIS_SCHLIESSEN.

      ENDIF.

    ENDAT.


*-- Ende der Hausbank --------------------------------------------------
    AT END OF REGUH-UBNKL.

      IF ( CNT_AVISE NE 0              "Formularabschluß erforderlich
        OR CNT_AVEDI NE 0
        OR CNT_AVFAX NE 0 ) AND HLP_LAUFK NE '*'.

*       Formular für den Abschluß öffnen
        SET COUNTRY SPACE.
        CLEAR FINAA.
        FINAA-NACHA = 1.
        PERFORM AVIS_OEFFNEN USING SPACE.

*       Liste aller Avis-Zwischenbelege ausgeben
        IF CNT_AVEDI NE 0.
          REFRESH TAB_ELEMENTS.
          CALL FUNCTION 'READ_FORM_ELEMENTS'
               EXPORTING
                    FORM     = HLP_FORMULAR
                    LANGUAGE = T001-SPRAS
               TABLES
                    ELEMENTS = TAB_ELEMENTS
               EXCEPTIONS
                    OTHERS   = 3.
          READ TABLE TAB_ELEMENTS WITH KEY
            WINDOW  = 'MAIN'
            ELEMENT = '676'.
          IF SY-SUBRC EQ 0.
            CALL FUNCTION 'START_FORM'
                 EXPORTING
                      FORM      = HLP_FORMULAR
                      LANGUAGE  = T001-SPRAS
                      STARTPAGE = 'EDI'.
            CALL FUNCTION 'WRITE_FORM'
                 EXPORTING
                      ELEMENT = '675'
                      TYPE    = 'TOP'
                 EXCEPTIONS
                      OTHERS  = 4.
            SIC_REGUH = REGUH.
            SIC_REGUD = REGUD.
            LOOP AT TAB_EDI_AVIS.
              REGUH = TAB_EDI_AVIS-REGUH.
              REGUD = TAB_EDI_AVIS-REGUD.
              CALL FUNCTION 'WRITE_FORM'
                   EXPORTING
                        ELEMENT = '676'
                   EXCEPTIONS
                        OTHERS  = 4.
            ENDLOOP.
            CALL FUNCTION 'END_FORM'.
            REGUH = SIC_REGUH.
            REGUD = SIC_REGUD.
          ENDIF.
        ENDIF.

*       Formular für den Abschluß starten
        CALL FUNCTION 'START_FORM'
             EXPORTING
                  FORM      = HLP_FORMULAR
                  LANGUAGE  = T001-SPRAS
                  STARTPAGE = 'LAST'.

*       Ausgabe des Formularabschlusses
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  WINDOW = 'SUMMARY'
             EXCEPTIONS
                  WINDOW = 1.
        IF SY-SUBRC EQ 1.
          ERR_ELEMENT-FNAME = HLP_FORMULAR.
          ERR_ELEMENT-FENST = 'SUMMARY'.
          ERR_ELEMENT-ELEMT = SPACE.
          ERR_ELEMENT-TEXT  = SPACE.
          COLLECT ERR_ELEMENT.
        ENDIF.

*       Formular beenden
        CALL FUNCTION 'END_FORM'.
        PERFORM AVIS_SCHLIESSEN.

      ENDIF.

    ENDAT.


*-- Ende des Zahlwegs --------------------------------------------------
    AT END OF REGUH-RZAWE.

      FREE MEMORY ID 'RFFORI06_ITCPO'.

    ENDAT.

  ENDLOOP.

ENDFORM.                               "Avis




*----------------------------------------------------------------------*
* FORM AVIS_NACHRICHTENART                                             *
*----------------------------------------------------------------------*
* Nachrichtenart ermitteln (Druck oder Fax)                            *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM AVIS_NACHRICHTENART.

  DATA UP_FIMSG LIKE FIMSG OCCURS 0 WITH HEADER LINE.

* Nachrichtenart ermitteln lassen
  CLEAR FINAA.
  FINAA-NACHA = 1.
  CALL FUNCTION 'OPEN_FI_PERFORM_00002040_P'
       EXPORTING
            I_REGUH = REGUH
       TABLES
            T_FIMSG = UP_FIMSG
       CHANGING
            C_FINAA = FINAA.
  LOOP AT UP_FIMSG INTO FIMSG.
    PERFORM MESSAGE USING FIMSG-MSGNO.
  ENDLOOP.

* Nachrichtenart Fax (2) prüfen, sonst nur Druck (1) zulassen
  IF FINAA-NACHA NE 2.
    FINAA-NACHA = 1.
  ELSE.
    CALL FUNCTION 'TELECOMMUNICATION_NUMBER_CHECK'
         EXPORTING
              SERVICE = 'TELEFAX'
              NUMBER  = FINAA-TDTELENUM
              COUNTRY = FINAA-TDTELELAND
         EXCEPTIONS
              OTHERS  = 4.
    IF SY-SUBRC NE 0.
      FINAA-NACHA = 1.
      FINAA-FORNR = T042B-AFORN.
    ENDIF.
  ENDIF.

ENDFORM.



*----------------------------------------------------------------------*
* FORM AVIS_OEFFNEN                                                    *
*----------------------------------------------------------------------*
* Avis öffnen und bei Druck Probedruck erledigen                       *
*----------------------------------------------------------------------*
* GENUINE ist gesetzt bei echten Avisen, leer bei Formularabschluß     *
*----------------------------------------------------------------------*
FORM AVIS_OEFFNEN USING GENUINE.

  DATA: UP_DEVICE LIKE ITCPP-TDDEVICE.

* Formular ermitteln
  IF NOT FINAA-FORNR IS INITIAL.
    HLP_FORMULAR = FINAA-FORNR.
  ELSE.
    HLP_FORMULAR = T042B-AFORN.
  ENDIF.

* Vorschlag für die Druckvorgaben holen und anpassen, Device setzen
  IMPORT ITCPO FROM MEMORY ID 'RFFORI06_ITCPO'.
  IF FINAA-NACHA EQ '2'.
    ITCPO-TDSCHEDULE = FINAA-TDSCHEDULE.
    ITCPO-TDTELELAND = FINAA-TDTELELAND.
    ITCPO-TDTELENUM  = FINAA-TDTELENUM.
    ITCPO-TDFAXUSER  = FINAA-TDFAXUSER.
    ITCPO-TDSUFFIX1  = 'FAX'.
    UP_DEVICE = 'TELEFAX'.
  ELSE.
    UP_DEVICE = 'PRINTER'.
  ENDIF.
  CLEAR:
    TOA_DARA,
    ARC_PARAMS.

* Druckvorgaben modifizieren lassen
  IF GENUINE EQ 'X'.
    CALL FUNCTION 'OPEN_FI_PERFORM_00002050_P'
         EXPORTING
              I_REGUH          = REGUH
              I_GJAHR          = REGUD-GJAHR
              I_NACHA          = FINAA-NACHA
              I_AFORN          = HLP_FORMULAR
         CHANGING
              C_ITCPO          = ITCPO
              C_ARCHIVE_INDEX  = TOA_DARA
              C_ARCHIVE_PARAMS = ARC_PARAMS.
    IF ITCPO-TDARMOD GT 1 AND PAR_ANZP NE 0."#EC PORTABLE
      PAR_ANZP = 0.
      PERFORM MESSAGE USING '384'.
    ENDIF.
  ENDIF.

* Dialog nur, wenn bei Druck der Drucker unbekannt
  IF PAR_PRIA EQ SPACE AND FINAA-NACHA EQ 1.
    FLG_DIALOG = 'X'.
  ELSE.
    FLG_DIALOG = SPACE.
  ENDIF.

* Neue Spool-Id bei erstem Avis zum Druck oder bei Fax
  IF FLG_PROBEDRUCK EQ 0 OR FINAA-NACHA EQ 2.
    ITCPO-TDNEWID = 'X'.
  ELSE.
    ITCPO-TDNEWID = SPACE.
  ENDIF.

* Formular öffnen
  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
            ARCHIVE_INDEX  = TOA_DARA
            ARCHIVE_PARAMS = ARC_PARAMS
            FORM           = HLP_FORMULAR
            DEVICE         = UP_DEVICE
            LANGUAGE       = T001-SPRAS
            OPTIONS        = ITCPO
            DIALOG         = FLG_DIALOG
       IMPORTING
            RESULT         = ITCPP
       EXCEPTIONS
            FORM           = 1.
  IF SY-SUBRC EQ 1.                    "Abbruch:
    IF SY-BATCH EQ SPACE.              "Formular ist nicht aktiv!
      MESSAGE A069 WITH HLP_FORMULAR.
    ELSE.
      MESSAGE S069 WITH HLP_FORMULAR.
      MESSAGE S094.
      STOP.
    ENDIF.
  ENDIF.
  IF PAR_PRIA EQ SPACE AND FINAA-NACHA EQ 1.
    PAR_PRIA = ITCPP-TDDEST.
    PERFORM FILL_ITCPO_FROM_ITCPP.
    EXPORT ITCPO TO MEMORY ID 'RFFORI06_ITCPO'.
  ENDIF.

* Name des Elements mit dem Anschreiben zusammensetzen
  IF REGUH-RZAWE NE SPACE.
    HLP_ELEMENT   = '610-'.
    HLP_ELEMENT+4 = REGUH-RZAWE.
    HLP_ELETEXT   = TEXT_610.
    REPLACE '&ZAHLWEG' WITH REGUH-RZAWE INTO HLP_ELETEXT.
  ELSE.
    HLP_ELEMENT   = '611-'.
    HLP_ELEMENT+4 = REGUH-AVISG.
    HLP_ELETEXT   = TEXT_611.
  ENDIF.

* Probedruck
  IF FLG_PROBEDRUCK EQ 0               "Probedruck noch nicht erledigt
    AND FINAA-NACHA EQ 1.
    PERFORM DATEN_SICHERN.
    DO PAR_ANZP TIMES.
*     Probedruck-Formular starten
      CALL FUNCTION 'START_FORM'
           EXPORTING
                FORM     = HLP_FORMULAR
                LANGUAGE = T001-SPRAS.
*     Fenster mit Probedruck schreiben
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                WINDOW   = 'INFO'
                ELEMENT  = '605'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = HLP_ELEMENT
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      IF SY-SUBRC NE 0.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = '610'
             EXCEPTIONS
                  WINDOW  = 1
                  ELEMENT = 2.
      ENDIF.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '614'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '615'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      DO 5 TIMES.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT  = '625'
                  FUNCTION = 'APPEND'
             EXCEPTIONS
                  WINDOW   = 1
                  ELEMENT  = 2.
      ENDDO.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '630'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                WINDOW  = 'TOTAL'
                ELEMENT = '630'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                WINDOW   = 'INFO'
                ELEMENT  = '605'
                FUNCTION = 'DELETE'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
*     Probedruck-Formular beenden
      CALL FUNCTION 'END_FORM'.
    ENDDO.
    PERFORM DATEN_ZURUECK.
    FLG_PROBEDRUCK = 1.                "Probedruck erledigt
  ENDIF.

ENDFORM.



*----------------------------------------------------------------------*
* FORM AVIS_SCHREIBEN                                                  *
*----------------------------------------------------------------------*
* Avis in Druckform ausgeben                                           *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM AVIS_SCHREIBEN.

* Faxdeckblatt
  IF FINAA-NACHA EQ 2 AND FINAA-FORMC NE SPACE.
    PERFORM ADRESSE_LESEN USING T001-ADRNR.                    "SADR40A
    ITCFX-RTITLE     = REGUH-ZANRE.
    ITCFX-RNAME1     = REGUH-ZNME1.
    ITCFX-RNAME2     = REGUH-ZNME2.
    ITCFX-RNAME3     = REGUH-ZNME3.
    ITCFX-RNAME4     = REGUH-ZNME4.
    ITCFX-RPOCODE    = REGUH-ZPSTL.
    ITCFX-RCITY1     = REGUH-ZORT1.
    ITCFX-RCITY2     = REGUH-ZORT2.
    ITCFX-RPOCODE2   = REGUH-ZPST2.
    ITCFX-RPOBOX     = REGUH-ZPFAC.
    ITCFX-RPOPLACE   = REGUH-ZPFOR.
    ITCFX-RSTREET    = REGUH-ZSTRA.
    ITCFX-RCOUNTRY   = REGUH-ZLAND.
    ITCFX-RREGIO     = REGUH-ZREGI.
    ITCFX-RLANGU     = HLP_SPRACHE.
    ITCFX-RHOMECNTRY = T001-LAND1.
    ITCFX-RLINES     = '9'.
    ITCFX-RCTITLE    = SPACE.
    ITCFX-RCFNAME    = SPACE.
    ITCFX-RCLNAME    = SPACE.
    ITCFX-RCNAME1    = FINAA-NAMEP.
    ITCFX-RCNAME2    = SPACE.
    ITCFX-RCDEPTM    = FINAA-ABTEI.
    ITCFX-RCFAXNR    = FINAA-TDTELENUM.
    ITCFX-STITLE     = SADR-ANRED.
    ITCFX-SNAME1     = SADR-NAME1.
    ITCFX-SNAME2     = SADR-NAME2.
    ITCFX-SNAME3     = SADR-NAME3.
    ITCFX-SNAME4     = SADR-NAME4.
    ITCFX-SPOCODE    = SADR-PSTLZ.
    ITCFX-SCITY1     = SADR-ORT01.
    ITCFX-SCITY2     = SADR-ORT02.
    ITCFX-SPOCODE2   = SADR-PSTL2.
    ITCFX-SPOBOX     = SADR-PFACH.
    ITCFX-SPOPLACE   = SADR-PFORT.
    ITCFX-SSTREET    = SADR-STRAS.
    ITCFX-SCOUNTRY   = SADR-LAND1.
    ITCFX-SREGIO     = SADR-REGIO.
    ITCFX-SHOMECNTRY = REGUH-ZLAND.
    ITCFX-SLINES     = '9'.
    ITCFX-SCTITLE    = FSABE-SALUT.
    ITCFX-SCFNAME    = FSABE-FNAME.
    ITCFX-SCLNAME    = FSABE-LNAME.
    ITCFX-SCNAME1    = FSABE-NAMP1.
    ITCFX-SCNAME2    = FSABE-NAMP2.
    ITCFX-SCDEPTM    = FSABE-ABTEI.
    ITCFX-SCCOSTC    = FSABE-KOSTL.
    ITCFX-SCROOMN    = FSABE-ROOMN.
    ITCFX-SCBUILD    = FSABE-BUILD.
    ITCFX-SCPHONENR1 = FSABE-TELF1.
    ITCFX-SCPHONENR2 = FSABE-TELF2.
    ITCFX-SCFAXNR    = FSABE-TELFX.
    ITCFX-HEADER     = T042T-TXTKO.
    ITCFX-FOOTER     = T042T-TXTFU.
    ITCFX-SIGNATURE  = T042T-TXTUN.
    ITCFX-TDID       = T042T-TXTID.
    ITCFX-TDLANGU    = HLP_SPRACHE.
    ITCFX-SUBJECT    = SPACE.
    CALL FUNCTION 'START_FORM'
         EXPORTING
              ARCHIVE_INDEX = TOA_DARA
              FORM          = FINAA-FORMC
              LANGUAGE      = HLP_SPRACHE
              STARTPAGE     = 'FIRST'.
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW = 'RECEIVER'.
    CALL FUNCTION 'END_FORM'.
  ENDIF.

* Formular starten
  CALL FUNCTION 'START_FORM'
       EXPORTING
            ARCHIVE_INDEX = TOA_DARA
            FORM          = HLP_FORMULAR
            LANGUAGE      = HLP_SPRACHE.

  IF HLP_XHRFO EQ SPACE.

*   Fenster Info, Element Unsere Nummer (falls diese gefüllt ist)
    IF REGUH-EIKTO NE SPACE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                WINDOW   = 'INFO'
                ELEMENT  = '605'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'INFO'.
        ERR_ELEMENT-ELEMT = '605'.
        ERR_ELEMENT-TEXT  = TEXT_605.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

*   Fenster Carry Forward, Element Übertrag (außer letzte Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW  = 'CARRYFWD'
              ELEMENT = '635'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.
    IF SY-SUBRC EQ 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'CARRYFWD'.
      ERR_ELEMENT-ELEMT = '635'.
      ERR_ELEMENT-TEXT  = TEXT_635.
      COLLECT ERR_ELEMENT.
    ENDIF.

*   Hauptfenster, Element Anschreiben-x (nur auf der ersten Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = HLP_ELEMENT
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.
    IF SY-SUBRC EQ 2.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '610'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'MAIN'.
      ERR_ELEMENT-ELEMT = HLP_ELEMENT.
      ERR_ELEMENT-TEXT  = HLP_ELETEXT.
      COLLECT ERR_ELEMENT.
    ENDIF.

*   Hauptfenster, Element Abweichender Zahlungsemfänger
    IF REGUD-XABWZ EQ 'X'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '612'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '612'.
        ERR_ELEMENT-TEXT  = TEXT_612.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

*   Hauptfenster, Element Zahlung erfolgt im Auftrag von
    IF REGUH-ABSBU NE REGUH-ZBUKR.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '613'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '613'.
        ERR_ELEMENT-TEXT  = TEXT_613.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

*   Hauptfenster, Element Unterschrift
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = '614'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.

*   Hauptfenster, Element Überschrift (nur auf der ersten Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = '615'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.
    IF SY-SUBRC EQ 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'MAIN'.
      ERR_ELEMENT-ELEMT = '615'.
      ERR_ELEMENT-TEXT  = TEXT_615.
      COLLECT ERR_ELEMENT.
    ENDIF.

*   Hauptfenster, Element Überschrift (ab der zweiten Seite oben)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT = '615'
              TYPE    = 'TOP'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.             "Fehler bereits oben gemerkt

*   Hauptfenster, Element Übertrag (ab der zweiten Seite oben)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '620'
              TYPE     = 'TOP'
              FUNCTION = 'APPEND'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2.
    IF SY-SUBRC EQ 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'MAIN'.
      ERR_ELEMENT-ELEMT = '620'.
      ERR_ELEMENT-TEXT  = TEXT_620.
      COLLECT ERR_ELEMENT.
    ENDIF.

  ELSE.

*   HR-Formular ausgeben
*   write HR form
    LOOP AT PFORM.
      CHECK SY-TABIX GT T042E-ANZPO.
      REGUD-TXTHR = PFORM-LINDA.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '625-HR'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '625-HR'.
        ERR_ELEMENT-TEXT  = TEXT_625.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDLOOP.

  ENDIF.

* Ausgabe der Einzelposten
  FLG_DIFF_BUKRS = 0.
  LOOP AT TAB_REGUP.

    AT NEW BUKRS.
      REGUP-BUKRS = TAB_REGUP-BUKRS.
      IF  ( REGUP-BUKRS NE REGUH-ZBUKR OR FLG_DIFF_BUKRS EQ 1 )
      AND ( REGUH-ABSBU EQ SPACE OR REGUH-ABSBU EQ REGUH-ZBUKR ).
        FLG_DIFF_BUKRS = 1.
        SELECT SINGLE * FROM T001 INTO *T001
          WHERE BUKRS EQ REGUP-BUKRS.
        REGUD-ABSTX = *T001-BUTXT.
        REGUD-ABSOR = *T001-ORT01.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  ELEMENT = '613'
             EXCEPTIONS
                  WINDOW  = 1
                  ELEMENT = 2.
        IF SY-SUBRC EQ 2.
          ERR_ELEMENT-FNAME = HLP_FORMULAR.
          ERR_ELEMENT-FENST = 'MAIN'.
          ERR_ELEMENT-ELEMT = '613'.
          ERR_ELEMENT-TEXT  = TEXT_613.
          COLLECT ERR_ELEMENT.
        ENDIF.
      ENDIF.
    ENDAT.

    REGUP = TAB_REGUP.
    PERFORM EINZELPOSTENFELDER_FUELLEN.

    IF HLP_XHRFO EQ SPACE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '625'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '625'.
        ERR_ELEMENT-TEXT  = TEXT_625.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

    PERFORM SUMMENFELDER_FUELLEN.

    IF HLP_XHRFO EQ SPACE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '625-TX'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
    ENDIF.

    AT END OF BUKRS.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT  = '629'
                FUNCTION = 'APPEND'
           EXCEPTIONS
                WINDOW   = 1
                ELEMENT  = 2.
    ENDAT.
  ENDLOOP.

  PERFORM ZIFFERN_IN_WORTEN.

* Summenfelder hochzählen und aufbereiten
  IF FINAA-NACHA EQ 1.
    ADD 1           TO CNT_AVISE.
    ADD REGUH-RBETR TO SUM_ABSCHLUSS.
  ELSE.
    ADD 1           TO CNT_AVFAX.
    ADD REGUH-RBETR TO SUM_ABSCHL_FAX.
  ENDIF.
  WRITE:
    CNT_AVISE       TO REGUD-AVISE,
    CNT_AVEDI       TO REGUD-AVEDI,
    CNT_AVFAX       TO REGUD-AVFAX,
    SUM_ABSCHLUSS   TO REGUD-SUMME CURRENCY T001-WAERS,
    SUM_ABSCHL_EDI  TO REGUD-SUEDI CURRENCY T001-WAERS,
    SUM_ABSCHL_FAX  TO REGUD-SUFAX CURRENCY T001-WAERS.
  TRANSLATE:
    REGUD-AVISE USING ' *',
    REGUD-AVEDI USING ' *',
    REGUD-AVFAX USING ' *',
    REGUD-SUMME USING ' *',
    REGUD-SUEDI USING ' *',
    REGUD-SUFAX USING ' *'.

  IF HLP_XHRFO EQ SPACE.

*   Hauptfenster, Element Gesamtsumme (nur auf der letzten Seite)
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '630'
              FUNCTION = 'APPEND'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2.
    IF SY-SUBRC EQ 2.
      ERR_ELEMENT-FNAME = HLP_FORMULAR.
      ERR_ELEMENT-FENST = 'MAIN'.
      ERR_ELEMENT-ELEMT = '630'.
      ERR_ELEMENT-TEXT  = TEXT_630.
      COLLECT ERR_ELEMENT.
    ENDIF.

*   Hauptfenster, Element Bankgebühr (Japan)
    IF REGUH-PAYGR+18(2) EQ '$J'.
      WHILE REGUH-PAYGR(1) EQ 0.
        SHIFT REGUH-PAYGR(10) LEFT.
        IF SY-INDEX > 10. EXIT. ENDIF.
      ENDWHILE.
      SUBTRACT REGUH-RSPE1 FROM: REGUD-SWNET, SUM_ABSCHLUSS.
      WRITE:
         REGUD-SWNET TO REGUD-SWNES CURRENCY REGUH-WAERS,
         SUM_ABSCHLUSS  TO REGUD-SUMME CURRENCY T001-WAERS.
      TRANSLATE:
         REGUD-SWNES USING ' *',
         REGUD-SUMME USING ' *'.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                ELEMENT = '634'
           EXCEPTIONS
                WINDOW  = 1
                ELEMENT = 2.
      IF SY-SUBRC EQ 2.
        ERR_ELEMENT-FNAME = HLP_FORMULAR.
        ERR_ELEMENT-FENST = 'MAIN'.
        ERR_ELEMENT-ELEMT = '634'.
        ERR_ELEMENT-TEXT  = TEXT_634.
        COLLECT ERR_ELEMENT.
      ENDIF.
    ENDIF.

*   Fenster Carry Forward, Element Übertrag löschen
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW   = 'CARRYFWD'
              ELEMENT  = '635'
              FUNCTION = 'DELETE'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2.            "Fehler bereits oben gemerkt

*   Hauptfenster, Element Überschrift löschen
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '615'
              TYPE     = 'TOP'
              FUNCTION = 'DELETE'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2.            "Fehler bereits oben gemerkt

*   Hauptfenster, Element Übertrag löschen
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '620'
              TYPE     = 'TOP'
              FUNCTION = 'DELETE'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2.            "Fehler bereits oben gemerkt

*   Hauptfenster, Element Abschlußtext
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              ELEMENT  = '631'
              FUNCTION = 'APPEND'
         EXCEPTIONS
              WINDOW   = 1
              ELEMENT  = 2.            "Ausgabe ist freigestellt

*   Fenster Total, Element Gesamtsumme
    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              WINDOW  = 'TOTAL'
              ELEMENT = '630'
         EXCEPTIONS
              WINDOW  = 1
              ELEMENT = 2.             "Ausgabe ist freigestellt

  ENDIF.

* Formular beenden
  CALL FUNCTION 'END_FORM'.

ENDFORM.



*----------------------------------------------------------------------*
* FORM AVIS_SCHLIESSEN                                                 *
*----------------------------------------------------------------------*
* Avis schließen und Ausgabetabelle füllen                             *
*----------------------------------------------------------------------*
* keine USING-Parameter                                                *
*----------------------------------------------------------------------*
FORM AVIS_SCHLIESSEN.

* Abschluß des Formulars
  CALL FUNCTION 'CLOSE_FORM'
       IMPORTING
            RESULT = ITCPP.

  IF FINAA-NACHA EQ 1.
    IF ITCPP-TDSPOOLID NE 0.
      CLEAR TAB_AUSGABE.
      TAB_AUSGABE-NAME    = T042Z-TEXT1.
      TAB_AUSGABE-DATASET = ITCPP-TDDATASET.
      TAB_AUSGABE-SPOOLNR = ITCPP-TDSPOOLID.
      TAB_AUSGABE-IMMED   = PAR_SOFA.
      COLLECT TAB_AUSGABE.
    ENDIF.
  ELSE.
    CLEAR TAB_AUSGABE.
    TAB_AUSGABE-NAME      = TEXT_094.
    TAB_AUSGABE-DATASET   = ITCPP-TDDATASET.
    COLLECT TAB_AUSGABE.
  ENDIF.

ENDFORM.
