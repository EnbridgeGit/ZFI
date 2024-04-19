REPORT RFSOPO00  LINE-SIZE 132
*                LINE-COUNT 65(0)
                 NO STANDARD PAGE HEADING MESSAGE-ID FR.

TABLES:  SKA1, SKAT, SKB1, BSIS, BSEGA, T001, BHDGD, B0SG.

DATA: SUM1(8)          TYPE P,
      SUM2(8)          TYPE P,
      SUM3(8)          TYPE P,
      SUM4(8)          TYPE P,
      SOLL(2)          TYPE C,
      HABEN(2)         TYPE C,
      KONTO(10)        TYPE C,
      I(2)             TYPE P,
*       PH-FLAG = '1' := Ausgabe von Informationen (Sach.)
*                        am Anfang einer Seite
      PH-FLAG(1)       TYPE C VALUE 1,
*       ONE_GSB = ' ' := Initialwert, jeweils bei Neuem Sachkonto
*                 'T' := Mindestens 1 GSBER vorhanden, Abspeichern
*                 'F' := Mehr als 1 GSBER vorhanden, normale Ausgabe
      ONE_GSB,
      INTENS,
      FOUND.

* KONTTAB speichert Fremdw., S/H-Betrag in Fremdw., S/H-Betrag in Haus.
*         pro Sachkonto und Buchungskreis
DATA: BEGIN OF KONTTAB OCCURS 20,
      WAERS    LIKE BKPF-WAERS,
      WRSHB(8) TYPE P DECIMALS 2,
      DMSHB(8) TYPE P DECIMALS 2,
      END   OF KONTTAB,

* BUKRTAB speichert Fremdw., S/H-Betrag in Fremdw., S/H-Betrag in Haus.
*         pro Buchungskreis
      BEGIN OF BUKRTAB OCCURS 20,
      WAERS    LIKE BKPF-WAERS,
      WRSHB(8) TYPE P DECIMALS 2,
      DMSHB(8) TYPE P DECIMALS 2,
      END   OF BUKRTAB,

* TOTALTAB1 speichert Hausw., Soll-, Haben-, S/H- und Mehrwertsteuer-
*           Betrag fuer alle Buchungskreise
      BEGIN OF TOTALTAB1 OCCURS 20,
      HWAERS   LIKE BKPF-WAERS,
      DMSOL(8) TYPE P DECIMALS 2,
      DMHAB(8) TYPE P DECIMALS 2,
      DMSHB(8) TYPE P DECIMALS 2,
      MDSHB(8) TYPE P DECIMALS 2,
      END   OF TOTALTAB1,

* TOTALTAB2 speichert Hausw., Fremdwaehrung, S/H-Betrag in Fremdwaeh-
*           rung und Hauswaehrungskreise
      BEGIN OF TOTALTAB2 OCCURS 20,
      HWAERS   LIKE BKPF-WAERS,
      WAERS    LIKE BKPF-WAERS,
      WRSHB(8) TYPE P DECIMALS 2,
      DMSHB(8) TYPE P DECIMALS 2,
      END   OF TOTALTAB2.

* Feldleiste fuer Mikrofiche-Information
DATA: BEGIN OF MIKFI,
      BUKRS    LIKE BSIS-BUKRS,        "Buchungskreis
      HKONT    LIKE BSIS-HKONT,        "Sachkonto
      GSBER    LIKE BSIS-GSBER,        "Geschäftsbereich
      BUDAT    LIKE BKPF-BUDAT,        "Buchungsdatum
      END   OF MIKFI.

* Feldleiste fuer ersten Geschaeftsbereich eines Sachkontos
* Bei mehreren GSBERen wird Feldleiste AT NEW KDNUM vor 2.GSBER gelistet
DATA: BEGIN OF FIRST_GSB,
      BUKRS    LIKE BKPF-BUKRS,        "Buchungskreis
      HKONT    LIKE SKAT-SAKNR,        "Sachkonto
      GSBER    LIKE BSEG-GSBER,        "Geschaeftsbereich
      SUM1(8) TYPE P,                  "Soll-Summe
      SUM2(8) TYPE P,                  "Haben-Summe
      SUM3(8) TYPE P,                  "MWST-Summe
      SUM4(8) TYPE P,                  "Saldo-Summe
END OF FIRST_GSB.

* Zusatzkontierungen ZE1 - ZE3  fuer Kostenrechnung

* ZE1 = Kostenstelle und Werk-Nummmer
DATA: BEGIN OF ZE1,
      KOSTLTXT(13) TYPE C,
      ' ',
      KOSTL        LIKE BSIS-KOSTL,
      ' ',
      WERKSTXT(6)  TYPE C,
      ' ',
      WERKS        LIKE BSIS-WERKS,
      END OF ZE1,

* ZE2 = Projekt-Nr. und Projekt-Vorgangsart
      BEGIN OF ZE2,
      PROJNTXT(13) TYPE C VALUE ' ',
      ' ',
      PROJK        LIKE BBSEG-PROJK,          " bh961205
*     ' ',
*     PROJVTXT(19) TYPE C VALUE ' ',
      END OF ZE2,

* ZE3 = Auftrags-Nr.
      BEGIN OF ZE3,
      AUFNRTXT(15) TYPE C VALUE ' ',
      ' ',
      AUFNR        LIKE BSIS-AUFNR,
*     ' ',
*     AUFPSTXT(17) TYPE C VALUE ' ',
      END OF ZE3.


FIELD-GROUPS: HEADER, POSTEN.

INSERT BSIS-BUKRS
       BSIS-HKONT "Sachkonto-Nummer
       BSIS-GSBER
       BSIS-BUDAT
       BSIS-BLART
       BSIS-BELNR
       BSIS-WAERS
       BSIS-BUZEI
       INTO HEADER.

INSERT BSIS-BLDAT
       BSIS-BSCHL
*      BSEG-UMSKZ
*      BSEGA-GKART
*      BSEGA-GKONT
       BSIS-ZUONR
*      bsis-augdt  "MD7140
       BSIS-AUGBL
       BSEGA-MDSHB
       BSEGA-DMSOL
       BSEGA-DMHAB
       BSEGA-DMSHB
       BSEGA-WRSHB
       BSIS-SGTXT
       BSIS-VBUND
*      BKPF-BKTXT
       ZE1 ZE2 ZE3
       BSIS-XBLNR  "MD7140
       INTO POSTEN.

* Select-Options
BEGIN_OF_BLOCK 1.
*SELECT-OPTIONS: GESBER   FOR BSIS-GSBER,     "Geschaeftsbereich
*                FIVER    FOR BSIS-VBUND,     "Firmenverbund
*                BUSCHL   FOR BSIS-BSCHL,     "Buchungsschluessel
*                KOSTENST FOR BSIS-KOSTL,     "Kostenstelle
*                WERK     FOR BSIS-WERKS.     "Werk
*  Further Selections box                                       MD7140
PARAMETERS:     VBEL    LIKE RFPDO2-SOPOVBEL. "vorerfasste Belege
PARAMETERS:     OPVW    LIKE RFPDO2-SOPOOPVW. "nur OP-verwaltete Konten
END_OF_BLOCK 1.

* Parameter fuer Layout                                               *
*  OUTPUT CONTROL                                               MD7140
BEGIN_OF_BLOCK 2.
PARAMETERS:     TITLE       LIKE RFPDO1-ALLGLINE,
                LISTSEP     LIKE RFPDO-ALLGLSEP, "keine Listseparation
                MIKFICHE    LIKE RFPDO-ALLGMIKF. "Microfische
END_OF_BLOCK 2.


INITIALIZATION.
  GET_FRAME_TITLE: 1, 2.

START-OF-SELECTION.

* Fuellen zusätzlicher DB Sel-Opts
* COPY: GESBER TO SD_GSBER, FIVER TO SD_VBUND, BUSCHL TO SD_BSCHL,
*       KOSTENST TO SD_KOSTL, WERK TO SD_WERKS.

  B0SG-XSTAV = VBEL.

  IF OPVW <> ' '.
    OPVW = 'X'.
  ENDIF.

* Mikro-Fiche Informationen erwuenscht ? (Ja, falls MIKFICHE <> ' ')
  MOVE: MIKFICHE TO BHDGD-MIFFL.

  SY-TITLE = TEXT-003.
*   Zuweisungen fuer Batch-Heading
  MOVE: SY-LINSZ TO BHDGD-LINES,
        SY-UNAME TO BHDGD-UNAME,
        SY-REPID TO BHDGD-REPID,
        SY-TITLE TO BHDGD-LINE1,
        TITLE    TO BHDGD-LINE2,
             '0' TO BHDGD-INIFL.
  MOVE: LISTSEP  TO BHDGD-SEPAR,
        'BUKRS'  TO BHDGD-DOMAI.

  SOLL  = TEXT-300.
  HABEN = TEXT-301.

GET SKB1.
  IF OPVW = 'X'.
    CHECK SKB1-XOPVW = 'X'.
  ENDIF.

GET BSIS.
* CHECK:  GESBER,
*         FIVER,
*         BUSCHL,
*         KOSTENST,
*         WERK.
  PERFORM FILL_ZUSATZK.
* Löschen Fremdwährungsbetrag wenn Buchungszeile nur in Hauswährung.
  IF BSIS-WAERS EQ T001-WAERS.
     MOVE SPACE TO BSEGA-WRSHB.
  ENDIF.
  FOUND = 'X'.
  EXTRACT POSTEN.

END-OF-SELECTION.

  IF FOUND <> 'X'.
    CALL FUNCTION 'POPUP_NO_LIST'.
    EXIT.
  ENDIF.

  SORT.

  LOOP.

************* Neuer Buchungskreis ************************************
    AT NEW BSIS-BUKRS.

*   Zuweisung BUKRS fuer Batch-Heading
      MOVE BSIS-BUKRS TO BHDGD-BUKRS.
      MOVE BHDGD-BUKRS TO BHDGD-WERTE.
      PERFORM NEW-SECTION(RSBTCHH0).

      IF MIKFICHE EQ 'X'.
        PERFORM MIKFI_AKTU.            "BHDGD-GRPIN setzen
      ENDIF.

      IF T001-BUKRS <> BSIS-BUKRS.
        SELECT SINGLE * FROM T001 WHERE BUKRS = BSIS-BUKRS.
      ENDIF.
      REFRESH BUKRTAB.

    ENDAT.

************* Neues Sachkonto ****************************************
    AT NEW BSIS-HKONT.

      REFRESH KONTTAB.
      CLEAR SKAT.
      CALL FUNCTION 'READ_SKA1'
                    EXPORTING XKTOPL = T001-KTOPL
                              XSAKNR = BSIS-HKONT
                    IMPORTING XSKA1  = SKA1
                              XSKAT  = SKAT.
      ONE_GSB = ' '.
      KONTO =  SKA1-SAKAN.

    ENDAT.

************* Neuer Geschäftsbereich *********************************
    AT NEW BSIS-GSBER.

      IF MIKFICHE EQ 'X'.
        PERFORM MIKFI_AKTU.            "BHDGD-GRPIN setzen
      ENDIF.

      CASE ONE_GSB.
        WHEN ' '.                    "1.Geschaeftsbereich
          ONE_GSB = 'T'.     "Wichtig fuer Ende Geschaeftsbereich
                                     "d.h. Speicherung in FIRST_GSB

        WHEN 'T'.                    "2.Geschaeftsbereich
          ONE_GSB = 'F'.     "Wichtig fuer Ende Geschaeftsbereich
                                     "d.h. Normale Ausgabe
*         Ausgabe der Summeninf. fuer 1.Geschaeftsbereich
          SUMMARY.
          RESERVE 5 LINES.
          FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
          WRITE: SY-VLINE, FIRST_GSB-BUKRS,
                 FIRST_GSB-HKONT,
                 FIRST_GSB-GSBER, '**',
             109 FIRST_GSB-SUM1 CURRENCY T001-WAERS, SOLL, 132 SY-VLINE,
/ SY-VLINE, 109 FIRST_GSB-SUM2 CURRENCY T001-WAERS, HABEN, 132 SY-VLINE,
/ SY-VLINE, 66  FIRST_GSB-SUM3 CURRENCY T001-WAERS NO-ZERO,
             109 FIRST_GSB-SUM4 CURRENCY T001-WAERS, ' **',132 SY-VLINE.
          ULINE.  PH-FLAG = '0'.

        WHEN 'F'.                    "i(i>=3) Geschaeftsbereiche
*         Tue nichts

      ENDCASE.                       "ONE_GSB

      PH-FLAG = '0'.
      RESERVE 9 LINES.
      SUMMARY.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: / SY-VLINE, BSIS-BUKRS,
               KONTO,
               BSIS-GSBER,
               SKAT-TXT50, 132 SY-VLINE.

    ENDAT.

************* Verarbeitung der Posten-Informationen *******************
    IF MIKFICHE EQ 'X'.
      PERFORM MIKFI_AKTU.            "BHDGD-GRPIN setzen
    ENDIF.

    DETAIL.
    PERFORM AUSGABE_POSTEN.

*   Tabelle KONTTAB fuellen
    KONTTAB-WAERS = BSIS-WAERS.
    KONTTAB-WRSHB = BSEGA-WRSHB.
    KONTTAB-DMSHB = BSEGA-DMSHB.
    COLLECT KONTTAB.

************* Ende Geschäftsbereich ***********************************
    AT END OF BSIS-GSBER.

      RESERVE 5 LINES.
      SUMMARY.

      IF ONE_GSB EQ 'T'.           "1.GSBER wird abgespeichert
        CLEAR FIRST_GSB.
        FIRST_GSB-BUKRS = BSIS-BUKRS.
        FIRST_GSB-HKONT = KONTO.
        FIRST_GSB-GSBER = BSIS-GSBER.
        FIRST_GSB-SUM1 = SUM(BSEGA-DMSOL).
        FIRST_GSB-SUM2 = SUM(BSEGA-DMHAB).
        FIRST_GSB-SUM3 = SUM(BSEGA-MDSHB).
        FIRST_GSB-SUM4 = SUM(BSEGA-DMSHB).
      ELSE.
*       WRITE:/,/ BSIS-BUKRS, BSIS-HKONT, BSIS-GSBER, '**'.
        FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
        WRITE:/ SY-VLINE, BSIS-BUKRS, KONTO, BSIS-GSBER, '**'.
      ENDIF.
      IF ONE_GSB EQ 'T'.
*       keine Ausgabe der GSBER-Summe, da evtl. nur ein GSBER!
      ELSE.
        FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
        PERFORM AUSGABE_SUM USING ' **'.
        ULINE.
      ENDIF.
*     SKIP 1.
*     ULINE.
      PH-FLAG = '0'.

    ENDAT.

************* Ende Sachkonto *****************************************
    AT END OF BSIS-HKONT.

      RESERVE 9 LINES.
*     SKIP 1.
*     ULINE.
      SUMMARY.
      FORMAT COLOR COL_TOTAL INTENSIFIED ON.
      WRITE: SY-VLINE, BSIS-BUKRS, KONTO, '***'.
*     Summenberechnung + Ausgabe (Soll,Haben,Saldo)
      PERFORM AUSGABE_SUM USING ' ***'.
*     Ausgabe der Salden ind Fremd- und in Hauswaehrung pro
*     Konto und Fuellen der Tabelle BUKRTAB
*     SKIP 1.
*     ULINE.
      WRITE: / SY-VLINE, 83 TEXT-004, 132 SY-VLINE. "UNDER BSIS-WAERS.
*     SKIP 1.
      SORT KONTTAB.

      LOOP AT KONTTAB.
        WRITE:/ SY-VLINE, 83 KONTTAB-WAERS. "UNDER BSIS-WAERS.
        IF KONTTAB-WAERS NE T001-WAERS.
          WRITE: (18) KONTTAB-WRSHB CURRENCY KONTTAB-WAERS.
        ELSE.
          WRITE: (18) KONTTAB-WRSHB NO-ZERO.
        ENDIF.
        WRITE: (17) KONTTAB-DMSHB CURRENCY T001-WAERS, 132 SY-VLINE.
        MOVE-CORRESPONDING KONTTAB TO BUKRTAB.
        COLLECT BUKRTAB.
        CLEAR BUKRTAB.
      ENDLOOP.
      ULINE.
*     SKIP 1.

    ENDAT.

************* Ende Buchungskreis *************************************
    AT END OF BSIS-BUKRS.

      IF MIKFICHE EQ 'X'.
        BHDGD-GRPIN+4 = ' '.           "Nur Buchungskreis anzeigen
      ENDIF.

      NEW-PAGE.

      SUMMARY.
      WRITE: SY-VLINE, BSIS-BUKRS, '****'.
      WRITE: 83 T001-WAERS. "UNDER BSIS-WAERS.
*     Summenberechnung + Ausgabe (Soll,Haben,Saldo)
      FORMAT COLOR COL_TOTAL INTENSIFIED ON.
      PERFORM AUSGABE_SUM USING ' '.

*     Fuellen der Tabelle TOTALTAB1: Hauswaehrung, Summen fuer
*     Soll-, Haben-, Saldo- und Mehrwertsteuerbetrag
      MOVE: T001-WAERS TO TOTALTAB1-HWAERS,
            SUM(BSEGA-DMSOL) TO TOTALTAB1-DMSOL,
            SUM(BSEGA-DMHAB) TO TOTALTAB1-DMHAB,
            SUM(BSEGA-DMSHB) TO TOTALTAB1-DMSHB,
            SUM(BSEGA-MDSHB) TO TOTALTAB1-MDSHB.
      COLLECT TOTALTAB1.

*     Ausgabe der Salden in Fremd- und in Hauswaehrung pro
*     Buchungskreis und Fuellen der Tabelle TOTALTAB2
*     SKIP 1.
*     ULINE.
      WRITE: / SY-VLINE,  83 TEXT-004,       "verteilt nach Waehrungen:
                            132 SY-VLINE.
*     SKIP 1.
      SORT BUKRTAB.
      LOOP AT BUKRTAB.
        WRITE:  SY-VLINE, 83 BUKRTAB-WAERS. "UNDER BSIS-WAERS.
        IF BUKRTAB-WAERS NE T001-WAERS.
          WRITE: (18) BUKRTAB-WRSHB CURRENCY BUKRTAB-WAERS.
        ELSE.
          WRITE: (18) BUKRTAB-WRSHB NO-ZERO.
        ENDIF.
        WRITE: (17) BUKRTAB-DMSHB CURRENCY T001-WAERS, 132 SY-VLINE.

        MOVE: T001-WAERS TO TOTALTAB2-HWAERS,
              BUKRTAB-WAERS TO TOTALTAB2-WAERS,
              BUKRTAB-WRSHB TO TOTALTAB2-WRSHB,
              BUKRTAB-DMSHB TO TOTALTAB2-DMSHB.
        COLLECT TOTALTAB2.
      ENDLOOP.
      ULINE.
      PH-FLAG = '0'.

    ENDAT.

******************** Verarbeitungsende *******************************
    AT LAST.

*     Zuweisung BUKRS fuer Batch-Hdg
      MOVE '    ' TO BHDGD-BUKRS.           "witk
      MOVE BHDGD-BUKRS TO BHDGD-WERTE.
      PERFORM NEW-SECTION(RSBTCHH0).

      IF MIKFICHE EQ 'X'.
        BHDGD-GRPIN(4) = '0000'.
        BHDGD-GRPIN+4 = ' '.
      ENDIF.

      SUMMARY.
*     Ausgabe der Summeninformationen fuer alle Buchungskreise

      SORT TOTALTAB1.
      SORT TOTALTAB2.
      WRITE: SY-VLINE, '*****', 132 SY-VLINE.

      I = 0.                       "Index fuer TOTALTAB1

*     Ausgabe von TOTALTAB1 und TOTALTAB2
      LOOP AT TOTALTAB2.

        RESERVE 7 LINES.
        AT NEW HWAERS.
          I = I + 1.
          READ TABLE TOTALTAB1 INDEX I.
*         Summen in der Hauswaehrung
          WRITE: SY-VLINE, 83 TOTALTAB1-HWAERS, "UNDER BSIS-WAERS,
            108 TOTALTAB1-DMSOL CURRENCY TOTALTAB1-HWAERS, SOLL,
            132 SY-VLINE,
   / SY-VLINE, 108 TOTALTAB1-DMHAB CURRENCY TOTALTAB1-HWAERS, HABEN,
                                                        132 SY-VLINE,
   / SY-VLINE, 83  TOTALTAB1-MDSHB CURRENCY TOTALTAB1-HWAERS NO-ZERO,
            108 TOTALTAB1-DMSHB CURRENCY TOTALTAB1-HWAERS, 132 SY-VLINE.
*         SKIP 1.
          WRITE: / SY-VLINE, 83 TEXT-004,
                            132 SY-VLINE.    "verteilt nach Waehrungen
*         SKIP 1.
        ENDAT.

*       Summen in Fremd- und Hauswaehrung
        WRITE: / SY-VLINE, 83 TOTALTAB2-WAERS. "UNDER BSIS-WAERS.
        IF TOTALTAB2-WAERS NE TOTALTAB2-HWAERS.
          WRITE: (18) TOTALTAB2-WRSHB CURRENCY TOTALTAB2-WAERS.
        ELSE.
          WRITE: (18) TOTALTAB2-WRSHB NO-ZERO.
        ENDIF.
        WRITE: (17) TOTALTAB2-DMSHB CURRENCY TOTALTAB2-HWAERS.
        WRITE 132 SY-VLINE.
        AT END OF HWAERS.
          ULINE.
*         SKIP 1.
        ENDAT.

      ENDLOOP. "TOTALTAB2

    ENDAT." AT LAST

  ENDLOOP.

******************** Zeitpunkt: TOP-OF-PAGE ***************************
TOP-OF-PAGE.
  SUMMARY.
  PERFORM BATCH-HEADING(RSBTCHH0).
  ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE: / SY-VLINE, TEXT-050, 132 SY-VLINE.      "Spaltenueberschriften
  WRITE: / SY-VLINE, TEXT-051, 132 SY-VLINE.
  ULINE.
  IF PH-FLAG = '1'.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / SY-VLINE, BSIS-BUKRS,
             KONTO,
             BSIS-GSBER,
             SKAT-TXT50, 132 SY-VLINE.
  ENDIF.
  INTENS = SPACE.
  PH-FLAG = '1'.

FORM FILL_ZUSATZK.
* Zuweisung der Zusatzkontierungen

* Konstanten fuer Abfrage, ob entsprechende Felder gefuellt sind
  DATA: WAKOSTL LIKE BSEG-KOSTL,  "VALUE '          ',
        WAAUFNR LIKE BSEG-AUFNR,  "VALUE '0000000000000000',
        WABSTNR LIKE BSEG-EBELN,  "VALUE '          ',
        WAPROJK LIKE BSEG-PROJK,  "VALUE '                ',
        WAPERNR LIKE BSEG-PERNR.  "VALUE '00000000'.

  MOVE SPACE TO: ZE1, ZE2, ZE3.
  IF BSIS-KOSTL NE WAKOSTL.
    MOVE TEXT-005 TO ZE1-KOSTLTXT.
    MOVE TEXT-006 TO ZE1-WERKSTXT.
    MOVE BSIS-KOSTL TO ZE1-KOSTL.
    MOVE BSIS-WERKS TO ZE1-WERKS.
  ENDIF.

  IF BSIS-PROJK NE WAPROJK.
    MOVE TEXT-007 TO ZE2-PROJNTXT.
*   MOVE TEXT-008 TO ZE2-PROJVTXT.
*   move bsis-projk to ze2-projk.           " bh961205 commented out
    WRITE BSIS-PROJK TO ZE2-PROJK USING EDIT MASK '==KONPR'. " bh961205
  ENDIF.

  IF BSIS-AUFNR NE WAAUFNR.
    MOVE TEXT-011 TO ZE3-AUFNRTXT.
*   MOVE TEXT-012 TO ZE3-AUFPSTXT.
    MOVE BSIS-AUFNR TO ZE3-AUFNR.
  ENDIF.

ENDFORM. "FILL_ZUSATZK

FORM AUSGABE_POSTEN.

  IF INTENS = SPACE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    INTENS = 'X'.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    INTENS = SPACE.
  ENDIF.
  WRITE: SY-VLINE,
         BSIS-BUDAT DDMMYY,              "BUCHUNGSDATUM
         BSIS-BLART,                     "BELEGART
         BSIS-BELNR,                     "BELEGNUMMER
         BSIS-BLDAT DDMMYY,              "BELEGDATUM
         BSIS-BSCHL,                     "BUCHUNGSSCHLUESSEL
*        BSEG-UMSKZ,                     "UMSATZKENNZEICHEN
*        ' ',
         BSIS-ZUONR,                     "ZUORDNUNGSNUMMER
*        bsis-augdt no-zero ddmmyy,      "AUSGLEICHDATUM  MD7140
         BSIS-BUDAT      NO-ZERO,        "AUSGLEICHBELEGNUMMER
         (11) BSEGA-MDSHB NO-ZERO        "MWST-BETRAG IN HW
                CURRENCY T001-WAERS,
         BSIS-WAERS,                     "WAEHRUNGSSCHLUESSEL
         (18) BSEGA-WRSHB CURRENCY BSIS-WAERS NO-ZERO,"SOLL/HABEN IN FW
         (17) BSEGA-DMSHB NO-ZERO         "SOLL/HABEN IN HW
                       CURRENCY T001-WAERS, 132 SY-VLINE.
      WRITE: / 'md7140', BSIS-XBLNR, BSIS-SGTXT.
  IF BSIS-SGTXT <> SPACE.
    WRITE: / SY-VLINE, 13 BSIS-SGTXT, 132 SY-VLINE.  "UNDER BSIS-BLART,
  ENDIF.
*           / BKPF-BKTXT UNDER BKPF-BLART,
  IF ZE1 <> SPACE.
    WRITE: / SY-VLINE, 13 ZE1, 132 SY-VLINE.  "Kostenstelle/Werk
  ENDIF.
  IF ZE2 <> SPACE.
    WRITE: / SY-VLINE, 13 ZE2, 132 SY-VLINE.  "Projektnummr/-vorgangsart
  ENDIF.
  IF ZE3 <> SPACE.
    WRITE: / SY-VLINE, 13 ZE3, 132 SY-VLINE.  "Menge/Mengeneinheit
  ENDIF.

  PH-FLAG = '1'.

ENDFORM.                               "AUSGABE_POSTEN

FORM AUSGABE_SUM USING AUSS_STERN.

*   Ausgabe von SOLL-, HABEN-, STEUER- und SALDO- Summe
*     Zuweisungen zur Vermeidung eines Ueberlaufes
      SUM1 = SUM(BSEGA-DMSOL).
      SUM2 = SUM(BSEGA-DMHAB).
      SUM3 = SUM(BSEGA-MDSHB).
      SUM4 = SUM(BSEGA-DMSHB).

      WRITE: 109 SUM1 CURRENCY T001-WAERS, SOLL, 132 SY-VLINE,
           / SY-VLINE, 109 SUM2 CURRENCY T001-WAERS, HABEN,132 SY-VLINE,
           / SY-VLINE, 66  SUM3 CURRENCY T001-WAERS NO-ZERO,
             109 SUM4 CURRENCY T001-WAERS, AUSS_STERN, 132 SY-VLINE.

ENDFORM.

FORM MIKFI_AKTU.

  MOVE: BSIS-BUKRS TO MIKFI-BUKRS,       "Buchungskreis
        KONTO      TO MIKFI-HKONT,       "Sachkonto
        BSIS-GSBER TO MIKFI-GSBER,       "Geschäftsbereich
        BSIS-BUDAT TO MIKFI-BUDAT.       "Buchungsdatum
  MOVE MIKFI TO BHDGD-GRPIN.
ENDFORM.                               "MIKFI_AKTU
