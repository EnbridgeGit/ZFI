*report rfbpet00
REPORT ZRFBPET0
       NO STANDARD PAGE HEADING
       LINE-SIZE  170
*      line-size  132
      LINE-COUNT 58(0)
       MESSAGE-ID FR.
*----------------------  CHANGES  -------------------------------------*
* 97/09/16 md7140 - Limit document numbers to those that have account  *
*                   numbers that match variant                         *
* 97/06/20 md7140 - Dev.Req. DRAP0176                                  *
*                 - eliminated Customer Line, Asset Line, Material Line*
*                   Items check boxes                                  *
*                 - G/L Account Line Item mandatory - Add Vendor Name  *
* 98/10/13 - Nancy Gilligan, OmniLogic     D30K906156                  *
*                 - standardized headers, set company code based on    *
*                   client                                             *
*                                                                      *
*                                                                      *
*----------------------------------------------------------------------*

TABLES: B0SG,
*       BKDF,
        BKPF,
        BSEG,
        BSEGA,BSEGH,
        BSEC,
        BSED,
        BHDGD,
        T001,
        LFA1.                         "MD7140 - Vendor Name Table

* Dataset
DATA: BEGIN OF BL,
*     D     LIKE BKDF,                 "Dauerbuchungsinformationen
      K     LIKE BKPF,                 "Belegkopf    "
      P     LIKE BSEG,                 "Beleg        "
      C     LIKE BSEC,                 "CPD          "
      W     LIKE BSED,                 "Wechsel      "
*     S     LIKE BSET,                 "Steuer       "
      END   OF BL.

DATA: BEGIN OF SRTTABLE OCCURS 0,     "MD7140 - move output to table
      BUKRS   LIKE BKPF-BUKRS,        "for sorting & then outputting
      NAME1   LIKE LFA1-NAME1,        "Sort by Company Code, Vendor Name
      BELNR   LIKE BKPF-BELNR,        "Document #, Item #
      SELFLAG(3)   TYPE C,
      BUZEI   LIKE BSEG-BUZEI,
      LIFNR   LIKE BSEG-LIFNR,
      BLART   LIKE BKPF-BLART,
      BUDAT   LIKE BKPF-BUDAT,
      MONAT   LIKE BKPF-MONAT,
      GJAHR   LIKE BKPF-GJAHR,
      BLDAT   LIKE BKPF-BLDAT,
      CPUDT   LIKE BKPF-CPUDT,
      USNAM   LIKE BKPF-USNAM,
      XBLNR   LIKE BKPF-XBLNR,
      GSBER   LIKE BSEG-GSBER,
      BSCHL   LIKE BSEG-BSCHL,
      UMSKZ   LIKE BSEG-UMSKZ,
      MWSKZ   LIKE BSEG-MWSKZ,
      ZTERM   LIKE BSEG-ZTERM,
      KOART   LIKE BSEG-KOART,
      KONTONR(10) TYPE C,
      SHKZG   LIKE BSEG-SHKZG,
      DMSHB   LIKE BSEGA-DMSHB,
      WAERS   LIKE T001-WAERS,
      KURSF   LIKE BKPF-KURSF,
      WAERSA  LIKE BKPF-WAERS,
      WRBTR   LIKE BSEG-WRBTR,
      END OF SRTTABLE.
* Flags
DATA: TRC-FLAG(1) TYPE C VALUE ' ',    "1 - extr.Satz tracen
      TBK-FLAG(1) TYPE C VALUE ' ',    "1 - Buchungskreis tracen
      TBL-FLAG(1) TYPE C VALUE ' ',    "1 - Belegnummer tracen
      SW-ULINE(1) TYPE C VALUE '0',    "verhindert doppelte Linienausg.
      TEMPSEL(3) TYPE C, "MD7140
      BELNR   LIKE BKPF-BELNR.        "Document #, Item #
* Workfields
DATA: BEGIN OF MIF1,
      BUKRS    LIKE BKPF-BUKRS,
      BELNR    LIKE BKPF-BELNR,
      BUZEI(3) TYPE C,
      END   OF MIF1.

DATA: WFXBLNR  LIKE BKPF-XBLNR VALUE 'GRUNDWERT       ',
      WF-XUMSW    LIKE BSEG-XUMSW,
      WF-UMSATZKZ LIKE BSEG-UMSKZ,
      WF-BUKRS LIKE BKPF-BUKRS,
      WF-BELNR LIKE BKPF-BELNR,
      KONTONR(10) TYPE C,
      ALT_BUKRS LIKE BKPF-BUKRS,
      DS_NAM(50) TYPE C.

SELECT-OPTIONS: BMONATE     FOR  BKPF-MONAT.        "Fiscal Periods
PARAMETERS:     EXTRAKT     LIKE  RFPDO-BPETEXTR
                                  DEFAULT ' ',      "Extrakt gewuenscht
                DS_NAME     LIKE  RFPDO-BPETDSNA,   "Dataset-Name
                SORT        LIKE  RFPDO-BPETSORT,   "Beleg-Sortierung
                PROTOKOL    LIKE  RFPDO-BPETPROT
                                  DEFAULT 'X'.      "Protokoll gewuensch
*               daus        like  rfpdo-bpetdaus    "MD7140 - Removed
*                                 default 'X'.      "Customer Line Items
SELECT-OPTIONS: DKONTO      FOR   BSEG-KUNNR.       "Debitorenkonto
PARAMETERS:     KAUS        LIKE  RFPDO-BPETKAUS
                                  DEFAULT 'X'.      "Kreditorenteil
SELECT-OPTIONS: KKONTO      FOR   BSEG-LIFNR.       "Kreditorenkonto
PARAMETERS:     SAUS LIKE  RFPDO-BPETSAUS OBLIGATORY   "MD7140
                                  DEFAULT 'X'.      "G/L Acct Line Items
SELECT-OPTIONS: SKONTO      FOR   BSEG-HKONT.       "Kreditorenkonto
*parameters:     aaus        like  rfpdo-bpetaaus   "MD7140 removed
*                                  default 'X'.     "Asset Line Items
SELECT-OPTIONS: ANLAGE      FOR   BSEG-ANLN1.       "Anlagenkonto
*parameters:     maus       like  rfpdo-bpetmaus    "MD7140 - Removed
*                                  default 'X'.     "Material Line Items
SELECT-OPTIONS: ARTNUM      FOR   BSEG-MATNR.       "Artikelnummer

PARAMETERS:     N_BELEGE    LIKE  RFPDO-BPETNBEL
                                  DEFAULT 'X',      "Standard Documents
                STAT_BUC    LIKE  RFPDO-BPETSBEL.   "Statistical Documnt
*               DAUER_BU    LIKE  RFPDO-BPETDBEL.   "Dauerbuchungen.

PARAMETERS: TRACE           LIKE  RFPDO-BPETTRAC.   "Transfer Trace
*SELECT-OPTIONS: TBUKRS      FOR  WF-BUKRS.          "Buchungskreis
*SELECT-OPTIONS: TBELNR      FOR  WF-BELNR.          "Belegnummer
SELECT-OPTIONS: TBUKRS      FOR  BKPF-BUKRS.        "Buchungskreis
SELECT-OPTIONS: TBELNR      FOR  BKPF-BELNR.       "...with Company Code

SELECT-OPTIONS: BLGDATUM    FOR  BKPF-BLDAT,        "Document Date
                CPUDATUM    FOR  BKPF-CPUDT,        "CPU Date
                BU_DATUM    FOR  BKPF-BUDAT,        "Buchungsdatum
                BENUTZER    FOR  BKPF-USNAM,        "Erfasser
                WAEHRUNG    FOR  BKPF-WAERS,        "Waehrung
*               REFERENZ    FOR  BKPF-XBLNR,     "Referenzangabe
                BKTEXT      FOR  BKPF-BKTXT(20),    "Belegkopftext
                AENDDAT     FOR  BKPF-UPDDT,        "Datum letzte Aender
*               DBBUPER     FOR  BKDF-DBMON,       "Dauerbuchungsperiode
*               DBBEDAT     FOR  BKDF-DBBDT,       "Dauerbuchungsbeg.dat
*               DBABDAT     FOR  BKDF-DBATR,       "     abrechnungs.dat
*               DBENDAT     FOR  BKDF-DBEDT,       "            ende.dat
                WERTSTEL    FOR  BKPF-WWERT,        "Datum Wertstellung
                KURSVOR     FOR  BKPF-KURSF,        "Kursvorgabe
                GESBER      FOR  BSEG-GSBER,        "Geschaeftsbereich
                MITKONT     FOR  BSEG-HKONT,        "Mitbuchkonto
*               MAHNSCHL    FOR  BSEG-MSCHL,        "Mahnschluessel
*               ZAHLSCHL    FOR  BSEG-ZLSCH,        "Zahlungsschluessel
                BUSCHL      FOR  BSEG-BSCHL,        "Posting Keys
                ZUORDNR     FOR  BSEG-ZUONR,        "Allocation Number
                BEWEGART    FOR  BSEG-ANBWA,        "Transaction Type
                KOSTENST    FOR  BSEG-KOSTL,        "Cost Centre
                WERK        FOR  BSEG-WERKS.        "Plant

*PARAMETERS:     TITLE(40)   TYPE C LOWER CASE,   "Kommentar
PARAMETERS:     TITLE       LIKE RFPDO1-ALLGLINE, "Additional Header
                LISTSEP     LIKE RFPDO-ALLGLSEP,  "List Separation
                MIKFICHE    LIKE RFPDO-ALLGMIKF.  "Microfiche

FIELD-GROUPS: HEADER,
              DATES.

INSERT
       BKPF-BUKRS
       BKPF-BELNR
       BSEG-BUZEI
                INTO HEADER.

INSERT
       BL
       BKPF-XBLNR
       BKPF-BUDAT
       BKPF-MONAT
       BKPF-GJAHR
       BKPF-BLART
       BKPF-BLDAT
       BKPF-CPUDT
       BKPF-USNAM
       BSEG-BSCHL
       BSEG-KOART
       KONTONR
       BSEG-UMSKZ
       BSEG-XUMSW
       BKPF-WAERS
       BSEG-WRBTR
       BSEGA-DMSHB
       BKPF-KURSF
                INTO DATES.


***********************************************************************
*  Konvertierung wenn Selektion auf eine Kontonummer gewünscht ist    *
***********************************************************************

* initialization                                         ngilligan 98/10
INITIALIZATION.                                         "ngilligan 98/10
IF SY-MANDT+2(1) = '1'.                                 "ngilligan 98/10
   BR_BUKRS-LOW = 'UEC'.                                "ngilligan 98/10
   BR_BUKRS-SIGN = 'I'.                                 "ngilligan 98/10
   BR_BUKRS-OPTION = 'EQ'.                              "ngilligan 98/10
ELSEIF  SY-MANDT+2(1) = '0'.                            "ngilligan 98/10
   BR_BUKRS-LOW = 'UGL'.                                "ngilligan 98/10
   BR_BUKRS-SIGN = 'I'.                                 "ngilligan 98/10
   BR_BUKRS-OPTION = 'EQ'.                              "ngilligan 98/10
ENDIF.                                                  "ngilligan 98/10
APPEND BR_BUKRS.                                        "ngilligan 98/10





AT SELECTION-SCREEN ON DKONTO.
*    Konvertierung der Debitorenkontonummer
*    --------------------------------------
      LOOP AT DKONTO.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING DKONTO-LOW DKONTO-LOW.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING DKONTO-HIGH DKONTO-HIGH.
        MODIFY DKONTO.
      ENDLOOP.

AT SELECTION-SCREEN ON KKONTO.
*    Konvertierung der Kreditorenkontonummer
*    --------------------------------------
      LOOP AT KKONTO.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING KKONTO-LOW KKONTO-LOW.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING KKONTO-HIGH KKONTO-HIGH.
        MODIFY KKONTO.
      ENDLOOP.


AT SELECTION-SCREEN ON SKONTO.
*    Konvertierung der Sachkontonummer
*    ---------------------------------
      LOOP AT SKONTO.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING SKONTO-LOW SKONTO-LOW.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING SKONTO-HIGH SKONTO-HIGH.
        MODIFY SKONTO.
      ENDLOOP.

AT SELECTION-SCREEN ON ANLAGE.
*    Konvertierung der Anlagenummer
*    ------------------------------
      LOOP AT ANLAGE.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING ANLAGE-LOW ANLAGE-LOW.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING ANLAGE-HIGH ANLAGE-HIGH.
        MODIFY ANLAGE.
      ENDLOOP.

AT SELECTION-SCREEN ON ARTNUM.
*    Konvertierung der Artikelnummer
*    -------------------------------
      LOOP AT ARTNUM.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING ARTNUM-LOW ARTNUM-LOW.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING ARTNUM-HIGH ARTNUM-HIGH.
        MODIFY ARTNUM.
      ENDLOOP.

AT SELECTION-SCREEN ON MITKONT.
*    Konvertierung des Mitbuchkontos
*    -------------------------------
      LOOP AT MITKONT.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING MITKONT-LOW MITKONT-LOW.
        PERFORM ALPHAFORMAT(SAPFS000)
          USING MITKONT-HIGH MITKONT-HIGH.
        MODIFY MITKONT.
      ENDLOOP.

AT SELECTION-SCREEN.
   PERFORM CHECK_PATH_DSNAME.

START-OF-SELECTION.

* Lesen von normalen Belegen.
IF N_BELEGE EQ SPACE.
  B0SG-XSTAN = ' '.
ENDIF.
* Lesen von statistischen Belegen.
IF STAT_BUC <> SPACE.
  B0SG-XSTAS = 'X'.
ENDIF.
* Lesen von Dauerbuchungsurbelegen.
*IF DAUER_BU <> SPACE.
*  B0SG-XSTAD = 'X'.
*ENDIF.

* Init. Alt_Bukrs, damit beim 1. GET BKPF Tabelle 001 gelesen wird.
  ALT_BUKRS = SPACE.

  MOVE: '    '      TO BHDGD-BUKRS,
        SY-LINSZ    TO BHDGD-LINES,
        SY-UNAME    TO BHDGD-UNAME,
        SY-REPID    TO BHDGD-REPID,
        SY-TITLE    TO BHDGD-LINE1,
        TITLE       TO BHDGD-LINE2,
        MIKFICHE    TO BHDGD-MIFFL,
        '0'         TO BHDGD-INIFL,
        'GRUNDWERT' TO WFXBLNR,
        LISTSEP     TO BHDGD-SEPAR,
        'BUKRS'     TO BHDGD-DOMAI.

  IF EXTRAKT    NE SPACE. MOVE 'X' TO EXTRAKT.    ENDIF.
  IF SORT       NE SPACE. MOVE 'X' TO SORT.       ENDIF.
  IF PROTOKOL   NE SPACE. MOVE 'X' TO PROTOKOL.   ENDIF.
  IF SAUS       NE SPACE. MOVE 'X' TO SAUS.       ENDIF.
* if aaus       ne space. move 'X' to aaus.       endif.
* if maus       ne space. move 'X' to maus.       endif.  "MD7140
* if daus       ne space. move 'X' to daus.       endif.  "MD7140
  IF KAUS       NE SPACE. MOVE 'X' TO KAUS.       ENDIF.

* Komplettierung Dateipfad zur endgültigen Aufbereitung
  IF EXTRAKT EQ 'X'.
     DS_NAM = DS_NAME.
     DESCRIBE FIELD DS_NAM.
       WRITE SY-REPID      TO DS_NAM+SY-FDPOS.
       SY-FDPOS = SY-FDPOS + 8.
       WRITE '_'           TO DS_NAM+SY-FDPOS.
       SY-FDPOS = SY-FDPOS + 1.
       WRITE SY-UZEIT TO DS_NAM+SY-FDPOS.
  ENDIF.

GET BKPF.

* IF BKPF-BSTAT EQ 'D'.
* Dauerbuchungsurbelege ?
*   CHECK DAUER_BU EQ 'X'.
*     CALL FUNCTION 'READ_BKDF'
*                  EXPORTING XBELNR = BKPF-BELNR
*                            XBUKRS = BKPF-BUKRS
*                            XGJAHR = BKPF-GJAHR
*                  IMPORTING XBKDF  = BKDF
*                  EXCEPTIONS KEY_INCOMPLETE = 01
*                             NOT_AUTHORIZED = 02
*                             NOT_FOUND      = 03.

* Prüfung Select-Options fuer Dauerbuchungsurbelege
*                   CHECK BKDF-DBMON IN DBBUPER.
*                   CHECK BKDF-DBBDT IN DBBEDAT.
*                   CHECK BKDF-DBATR IN DBABDAT.
*                   CHECK BKDF-DBEDT IN DBENDAT.
* ENDIF.

CHECK SELECT-OPTIONS.

  IF TRACE NE SPACE(1).
    WF-BUKRS = BKPF-BUKRS.
    PERFORM TBUKRS-CHECK.
    WF-BELNR = BKPF-BELNR.
    PERFORM TBELNR-CHECK.
  ENDIF.

* T001 lesen fuer Waehrungschluessel
  IF ALT_BUKRS NE BKPF-BUKRS AND SORT EQ SPACE.
    CLEAR T001.
    T001-BUKRS = BKPF-BUKRS.
    READ TABLE T001.
    ALT_BUKRS = BKPF-BUKRS.
  ENDIF.

  BL-K = BKPF.

* !!!!! Uebernahem DAUERURBELEGKOPDATEN !!!!!
* IF BKPF-BSTAT EQ 'D' AND BKDF NE SPACE.
*         BL-D = BKDF.
* ENDIF.

  IF SORT = SPACE AND PROTOKOL = 'X'.
          BHDGD-GRPIN = MIF1.
          PERFORM AUSGABE-BELEG-KOPF.
  ENDIF.


GET BSEG.
* CHECKs abhaengig von der Kontoart
  CASE BSEG-KOART.
    WHEN 'D'.
*     check daus eq 'X'.      "MD7140
      CHECK DKONTO.           "Debitorenkonto
      IF BSEG-UMSKZ = SPACE.  "Kein Sonderhauptbuchvorgang
        CHECK MITKONT.
      ELSE.                   "Sonderhauptbuchvorgang
        KONTONR = BSEG-HKONT.
        BSEG-HKONT = BSEG-SAKNR.
        CHECK MITKONT.
        BSEG-HKONT = KONTONR.
      ENDIF.
      KONTONR = BSEG-KUNNR.

    WHEN 'K'.
      CHECK KAUS EQ 'X'.
      CHECK KKONTO.                     "KREDITORENKONTO
      IF BSEG-UMSKZ = SPACE.            "Kein Sonderhauptbuchvorgang
        CHECK MITKONT.
      ELSE.                             "Sonderhauptbuchvorgang
        KONTONR = BSEG-HKONT.
        BSEG-HKONT = BSEG-SAKNR.
        CHECK MITKONT.
        BSEG-HKONT = KONTONR.
      ENDIF.
      KONTONR = BSEG-LIFNR.

    WHEN 'S'.
*     check saus eq 'X'.                            "MD7140
      CHECK SKONTO.                                 "SACHKONTO
      CHECK BSEG-BUZEI NE 0.
      KONTONR = BSEG-HKONT.

    WHEN 'A'.
*     check aaus eq 'X'.                            "MD7140
      CHECK: ANLAGE.
      CHECK BSEG-BUZEI NE 0.
      KONTONR = BSEG-HKONT.

    WHEN 'M'.
*     check maus eq 'X'.              "MD7140
      CHECK: ARTNUM.
      CHECK BSEG-BUZEI NE 0.
      KONTONR = BSEG-HKONT.

    WHEN OTHERS.
      REJECT.
  ENDCASE.    "BP-KOART

  CHECK: GESBER,                       "GESCHAEFTSBEREICH
         BUSCHL,                       "BUCHUNGSSCHLUESSEL
         ZUORDNR,                      "ZUORDNUNGSNUMMER
         BEWEGART,                     "Bewegungsart
         KOSTENST,                     "Kostenstelle
         WERK.                         "Werk




  IF TRACE NE SPACE(1).
    SET TRANSFER TRACE OFF.
    TRC-FLAG = '0'.
    IF TBK-FLAG = '1'.
      IF TBL-FLAG = '1'.
        SET TRANSFER TRACE ON.
        TRC-FLAG = '1'.
      ENDIF.
    ENDIF.
  ENDIF.

  WRITE BSEG-BUZEI TO MIF1-BUZEI.

  BL-P = BSEG.

  IF BSEG-XCPDD NE SPACE.
          BL-C = BSEC.
  ENDIF.

  IF BSEG-UMSKS NE SPACE.
          BL-W = BSED.
  ENDIF.


  CASE SORT.
    WHEN 'X'.
      EXTRACT DATES.
    WHEN OTHERS.
      CASE PROTOKOL.
        WHEN 'X'.
          BHDGD-GRPIN = MIF1.
          PERFORM AUSGABE-BELEG-POSITION.
      ENDCASE.
      CASE EXTRAKT.
        WHEN 'X'.
          TRANSFER BL TO DS_NAM.
      ENDCASE.
  ENDCASE.


GET BKPF LATE.
  IF PROTOKOL EQ 'X' AND SORT EQ SPACE.
    MOVE '1' TO SW-ULINE.
*   uline.
    MOVE '0' TO SW-ULINE.
  ENDIF.

END-OF-SELECTION.
  CASE SORT.
    WHEN 'X'.
      SORT.
      LOOP.
*---------------------------------------------------------------------*
        MOVE BL-K TO BKPF.
        MOVE BL-P TO BSEG.
        MOVE BL-C TO BSEC.
*Dauerbuchungsdaten!!!!!!!!!!!!!
*       MOVE BL-D TO BKDF.
*Wechseldaten     !!!!!!!!!!!!!
        MOVE BL-W TO BSED.
        AT NEW BKPF-BUKRS.
          BHDGD-BUKRS = BKPF-BUKRS.
          MIF1-BUKRS  = BKPF-BUKRS.
          MOVE BHDGD-BUKRS TO BHDGD-WERTE.
          PERFORM NEW-SECTION(RSBTCHH0).
*         Lesen Waehrung
          CLEAR T001.
          T001-BUKRS = BKPF-BUKRS.
          READ TABLE T001.
        ENDAT.
*---------------------------------------------------------------------*
        AT NEW BKPF-BELNR.
          MIF1-BELNR  = BKPF-BELNR.
          IF PROTOKOL  NE SPACE.
             RESERVE 5 LINES.
             PERFORM AUSGABE-BELEG-KOPF.
          ENDIF.
        ENDAT.
*---------------------------------------------------------------------*
        AT NEW BSEG-BUZEI.
          WRITE BSEG-BUZEI TO MIF1-BUZEI.
          BHDGD-GRPIN = MIF1.
          IF PROTOKOL  NE SPACE.
             PERFORM AUSGABE-BELEG-POSITION.
          ENDIF.
        ENDAT.
*---------------------------------------------------------------------*
        AT DATES.
          IF TRC-FLAG = '1'.
            SET TRANSFER TRACE ON.
          ELSE.
            SET TRANSFER TRACE OFF.
          ENDIF.
          CASE EXTRAKT.
            WHEN 'X'.
              TRANSFER BL TO DS_NAM.
          ENDCASE.
        ENDAT.
*---------------------------------------------------------------------*
        AT END OF BKPF-BELNR.
          IF PROTOKOL EQ 'X'.
            MOVE '1' TO SW-ULINE.
*           uline.
            MOVE '0' TO SW-ULINE.
          ENDIF.
        ENDAT.
* Ausgabe dateiname.
        AT LAST.
          IF EXTRAKT EQ 'X'.
            IF PROTOKOL EQ 'X'.
             SKIP.
             WRITE: TEXT-104, SPACE, DS_NAM.
            ELSE.
             MESSAGE S128 WITH DS_NAM.
            ENDIF.
          ENDIF.
        ENDAT.
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
      ENDLOOP.
  ENDCASE.
* Ausgabe Dateiname.
      IF SORT NE 'X'.
        IF EXTRAKT EQ 'X'.
           IF PROTOKOL EQ 'X'.
              SKIP.
              WRITE: TEXT-104, SPACE, DS_NAM.
           ELSE.
             MESSAGE S128 WITH DS_NAM.
           ENDIF.
        ENDIF.
      ENDIF.
*-----------------------------------------------------------------------
* 97/06/20 MD7140 - sort table by company code, vendor #, document #
*                   and item number.  Loop & print table

  SORT SRTTABLE BY BUKRS NAME1 BELNR SELFLAG DESCENDING BUZEI.
  LOOP AT SRTTABLE.

    IF  SRTTABLE-SELFLAG = 'yes'.
        BELNR = SRTTABLE-BELNR.
    ENDIF.

    IF SRTTABLE-BELNR = BELNR.
       SRTTABLE-SELFLAG = 'yes'.
       MODIFY SRTTABLE.
    ELSE.
       DELETE SRTTABLE.
    ENDIF.

  ENDLOOP.

  SORT SRTTABLE BY BUKRS NAME1 BELNR SELFLAG BUZEI.
  LOOP AT SRTTABLE.
    IF  SRTTABLE-BUZEI = '001'.
*        perform write-header.
        PERFORM WRITE-DETAIL.
    ELSE.
        PERFORM WRITE-DETAIL.
    ENDIF.

  AT END OF BELNR.          "Separate documents for underline
     ULINE.
  ENDAT.

  ENDLOOP.
* 97/06/20 md7140 - end of code insert.
*-----------------------------------------------------------------------

* Seitenanfangsverarbeitung
TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         37 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
         80 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-HED UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10



  MOVE 'GRUNDWERT' TO WFXBLNR.
*  perform batch-heading(rsbtchh0).
  ULINE.
  IF T001-BUKRS NE BKPF-BUKRS.
     T001-BUKRS = BKPF-BUKRS.
     READ TABLE T001.
  ENDIF.
  WRITE /1    TEXT-100.
  DETAIL.
  WRITE /1    TEXT-101.
  SUMMARY.
  IF SW-ULINE = '0'.
    ULINE.
  ENDIF.

*---------------------------------------------------------------------*
* Pruefen Buchungskreis-Debugging                                     *
*---------------------------------------------------------------------*
FORM TBUKRS-CHECK.
   TBK-FLAG = '0'.
*  CHECK TBUKRS.
   CHECK WF-BUKRS IN TBUKRS.
   TBK-FLAG = '1'.
ENDFORM.

*---------------------------------------------------------------------*
* Pruefen Belegnummer-Debugging                                       *
*---------------------------------------------------------------------*
FORM TBELNR-CHECK.
  TBL-FLAG = '0'.
*  CHECK TBELNR.
   CHECK WF-BELNR IN TBELNR.
  TBL-FLAG = '1'.
ENDFORM.

*---------------------------------------------------------------------*
* Ausgabe eines Protokollsatzes                                       *
*---------------------------------------------------------------------*
FORM AUSGABE-BELEG-KOPF.
* write: /1 bkpf-bukrs,                         "MD7140
*           bkpf-belnr,
*           bkpf-blart,
*           bkpf-budat,
*        31 bkpf-monat,
*        33 bkpf-gjahr+2(2),
*           bkpf-bldat,
*           bkpf-cpudt,
*           bkpf-usnam,
*           bkpf-xblnr.
  CLEAR SRTTABLE.                "Clear header for new document
  SRTTABLE-BUKRS = BKPF-BUKRS.
  SRTTABLE-BELNR = BKPF-BELNR.
  SRTTABLE-BLART = BKPF-BLART.
  SRTTABLE-BUDAT = BKPF-BUDAT.
  SRTTABLE-MONAT = BKPF-MONAT.
  SRTTABLE-GJAHR = BKPF-GJAHR.
  SRTTABLE-BLDAT = BKPF-BLDAT.
  SRTTABLE-CPUDT = BKPF-CPUDT.
  SRTTABLE-USNAM = BKPF-USNAM.
  SRTTABLE-XBLNR = BKPF-XBLNR.
  SRTTABLE-KURSF = BKPF-KURSF.
  SRTTABLE-WAERSA = BKPF-WAERS.
ENDFORM.

FORM WRITE-HEADER.
 SUMMARY.
 WRITE: /1 SRTTABLE-BUKRS,                         "MD7140
           SRTTABLE-BELNR,
           SRTTABLE-BLART,
           SRTTABLE-BUDAT,
        31 SRTTABLE-MONAT,
        33 SRTTABLE-GJAHR+2(2),
           SRTTABLE-BLDAT,
           SRTTABLE-CPUDT,
           SRTTABLE-USNAM,
           SRTTABLE-XBLNR.
ENDFORM.

FORM AUSGABE-BELEG-POSITION.
* if bseg-xumsw = 'X'.
*    wf-xumsw    = 'X'.                             "Umsatzwirksam
*    wf-umsatzkz = space.
* else.
*    if bseg-umskz <> space.
*       wf-xumsw    = space.                        "Sonderumsatz
*       wf-umsatzkz = bseg-umskz.
*    else.
*       wf-xumsw    = space.                        "Nicht Umsatzwirksam
*       wf-umsatzkz = space.
*    endif.
* endif.
* detail.
* write: /6 bseg-gsber,
*           bseg-buzei,
*           bseg-bschl,
*           wf-umsatzkz,
*        22 bseg-mwskz,
*        26 bseg-zterm,
*        31 bseg-koart,
*        35 kontonr,
*           bseg-shkzg,
*        "  BSEG-DMBTR NO-ZERO CURRENCY T001-WAERS,
*           bsega-dmshb no-zero currency t001-waers,
*           t001-waers.
*           if bkpf-kursf ne 0.
*              write: bseg-wrbtr no-zero currency bkpf-waers,
*                     bkpf-waers.
*           endif.

*           if bkpf-bstat eq 'D'.
*              if bseg-mwsts <> 0.
*                 perform ausgabe_steuern.
*              endif.
*           endif.


* if bseg-xcpdd ne space.      "CPD-Daten
*    summary.
*    write: /11 text-004, bsec-name1,
*               text-005, bsec-ort01.
*    detail.
* endif.
* summary.
  SRTTABLE-GSBER = BSEG-GSBER.
  SRTTABLE-BUZEI = BSEG-BUZEI.
  SRTTABLE-BSCHL = BSEG-BSCHL.

  IF BSEG-XUMSW = 'X'.
     WF-XUMSW    = 'X'.                             "Umsatzwirksam
     WF-UMSATZKZ = SPACE.
  ELSE.
     IF BSEG-UMSKZ <> SPACE.
        WF-XUMSW    = SPACE.                        "Sonderumsatz
        WF-UMSATZKZ = BSEG-UMSKZ.
     ELSE.
        WF-XUMSW    = SPACE.                        "Nicht Umsatzwirksam
        WF-UMSATZKZ = SPACE.
     ENDIF.
  ENDIF.
  SRTTABLE-UMSKZ = WF-UMSATZKZ.

  SRTTABLE-MWSKZ = BSEG-MWSKZ.
  SRTTABLE-ZTERM = BSEG-ZTERM.
  SRTTABLE-KOART = BSEG-KOART.

  CLEAR SRTTABLE-SELFLAG.
  SRTTABLE-KONTONR = KONTONR.
  IF KONTONR = SKONTO+3(10).
     SRTTABLE-SELFLAG = 'yes'.
  ELSEIF SKONTO+3(10) = SPACE.
     SRTTABLE-SELFLAG = 'yes'.
  ENDIF.
  SRTTABLE-SHKZG = BSEG-SHKZG.
  SRTTABLE-DMSHB = BSEGA-DMSHB.
  SRTTABLE-WAERS = T001-WAERS.
  SRTTABLE-WRBTR = BSEG-WRBTR.

  IF BSEG-KOART = 'K'.               "Get Vendor Name
     SRTTABLE-LIFNR = BSEG-LIFNR.
     SELECT * FROM LFA1
        WHERE LIFNR = BSEG-LIFNR.
        SRTTABLE-NAME1 = LFA1-NAME1.
     ENDSELECT.
  ENDIF.


  APPEND SRTTABLE.

*           if bkpf-bstat eq 'D'.
*              if bseg-mwsts <> 0.
*                 perform ausgabe_steuern.
*              endif.
*           endif.


* if bseg-xcpdd ne space.      "CPD-Daten
*    summary.
*    write: /11 text-004, bsec-name1,
*               text-005, bsec-ort01.
*    detail.
* endif.
* summary.
ENDFORM.

FORM WRITE-DETAIL.
  IF SRTTABLE-KOART <> 'K'.   "Only Account Type 'K' s/h Vendor Name
     SRTTABLE-NAME1 = ' '.
  ENDIF.
  DETAIL.
  WRITE: /6 SRTTABLE-GSBER,
            SRTTABLE-BUZEI,
            SRTTABLE-BSCHL,
            SRTTABLE-UMSKZ,
         22 SRTTABLE-MWSKZ,
         26 SRTTABLE-ZTERM,
         31 SRTTABLE-KOART,
         35 SRTTABLE-KONTONR,
            SRTTABLE-SHKZG,
            SRTTABLE-DMSHB NO-ZERO CURRENCY SRTTABLE-WAERS,
            SRTTABLE-WAERS.

            IF  SRTTABLE-KURSF NE 0.
                WRITE: SRTTABLE-WRBTR NO-ZERO CURRENCY SRTTABLE-WAERSA,
                       SRTTABLE-WAERSA.
            ENDIF.

  WRITE: 130 SRTTABLE-NAME1.


*           if bkpf-bstat eq 'D'.
*              if bseg-mwsts <> 0.
*                 perform ausgabe_steuern.
*              endif.
*           endif.


* if bseg-xcpdd ne space.      "CPD-Daten
*    summary.
*    write: /11 text-004, bsec-name1,
*               text-005, bsec-ort01.
*    detail.
* endif.
* summary.
*        26 bseg-zterm,
*        31 bseg-koart,
*        35 kontonr,
*           bseg-shkzg,
*        "  BSEG-DMBTR NO-ZERO CURRENCY T001-WAERS,
*           bsega-dmshb no-zero currency t001-waers,
*           t001-waers.
*           if bkpf-kursf ne 0.
*              write: bseg-wrbtr no-zero currency bkpf-waers,
*                     bkpf-waers.
*           endif.

*           if bkpf-bstat eq 'D'.
*              if bseg-mwsts <> 0.
*                 perform ausgabe_steuern.
*              endif.
*           endif.


* if bseg-xcpdd ne space.      "CPD-Daten
*    summary.
*    write: /11 text-004, bsec-name1,
*               text-005, bsec-ort01.
*    detail.
* endif.
* summary.
ENDFORM.


FORM AUSGABE_STEUERN.
* Fuer Dauerbuchungsurbelege wird keine automatische Buchungszeile fuer
* den Steuerbetrag abgelegt. Die Routine gibt den Steuerbetrag für die
* Sachkontenzeile aus.
 CHECK BSEG-KOART EQ 'S'.
   WRITE: / TEXT-103 UNDER BSEG-BUZEI,
         46 BSEG-SHKZG.
            IF BSEG-SHKZG EQ 'H'.
               BSEG-MWSTS = BSEG-MWSTS * -1.
            ENDIF.
   WRITE:   BSEG-MWSTS CURRENCY T001-WAERS UNDER BSEGA-DMSHB,
            T001-WAERS UNDER T001-WAERS.
            IF BKPF-KURSF NE 0.
              WRITE: BSEG-WMWST CURRENCY BKPF-WAERS UNDER BSEG-WRBTR,
                                         BKPF-WAERS UNDER BKPF-WAERS.
            ENDIF.


ENDFORM.

FORM CHECK_PATH_DSNAME.
*---------------------------------------------------------------------*
*       FORM CHECK_PATH_DSNAME                                        *
*---------------------------------------------------------------------*
*       Die Reorganisationdirectory wird verprobt                     *
*---------------------------------------------------------------------*
FIELD-SYMBOLS: <F>.
IF EXTRAKT EQ 'X'.    "X - Extrakt gewünscht.
  IF DS_NAME IS INITIAL.
     SET CURSOR FIELD 'DS_NAME'.
     MESSAGE E126.
     EXIT.
  ENDIF.
*
*
* Ein kleiner UNIX-Service: '/' wird angehängt, falls nicht eingegeben
*
  IF DS_NAME CA ' '.
     CASE SY-OPSYS.
       WHEN 'HP-UX'.
         SET CURSOR FIELD 'DS_NAME'.
         DATA: FDPOS LIKE SY-FDPOS.
         DATA: RFILE(50)      TYPE C.        "Reorganisationsdateiname
         FDPOS = SY-FDPOS - 1.
         ASSIGN DS_NAME+FDPOS(1) TO <F>.
         IF <F> NE '/'.
            ASSIGN DS_NAME+SY-FDPOS(1) TO <F>.
            <F> = '/'.
         ENDIF.
     ENDCASE.
  ENDIF.
*
* Ein temporäres File wird aufgebaut und getestet
*
  RFILE = DS_NAME.
  IF RFILE CA ' '.
     ASSIGN RFILE+SY-FDPOS(3) TO <F>.
     <F> = 'tmp'.
  ENDIF.
*
  OPEN DATASET RFILE FOR OUTPUT.
  IF SY-SUBRC NE 0.
*
* Dateipfad existiert nicht
*
    SET CURSOR FIELD 'DS_NAME'.
    MESSAGE E127.
  ENDIF.
  CLOSE DATASET RFILE.
ELSE.
  MOVE SPACE TO DS_NAME.
ENDIF.
ENDFORM.
