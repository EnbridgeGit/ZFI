REPORT RFBIBLG0.
*-----------------------------------------------------------------------
*        Batch-Input für Belege:
*        Generierungsreport für Batch-Input-Coding.
*
*
*        Der Name des Rahmenreports ist            RFBIBL01.
*        Der Name des neu generierten Reports ist  RFBIBL00.
*-----------------------------------------------------------------------


*------- Tabellen ------------------------------------------------------
DATA:    BEGIN OF REP OCCURS 100,
           Z(72)        TYPE C,        " Reportzeile
         END OF REP.

TABLES:  D021S,
         D020S,
         TCOBF.

TABLES:  DD02L.                         " SAP-Tabellen
TABLES:  DD03L.                         " Tabellenfelder


DATA:    BEGIN OF HEADER OCCURS 50.
           INCLUDE STRUCTURE D020S.
DATA:    END OF HEADER.

DATA:    BEGIN OF FIELDS OCCURS 50.
           INCLUDE STRUCTURE D021S.
DATA:    END OF FIELDS.

DATA:    BEGIN OF XDD03L OCCURS 100.
           INCLUDE STRUCTURE DD03L.
DATA:    END OF XDD03L.

DATA:    BEGIN OF YDD03L OCCURS 10.
           INCLUDE STRUCTURE DD03L.
DATA:    END OF YDD03L.

DATA:    BEGIN OF ZDD03L OCCURS 100.
           INCLUDE STRUCTURE DD03L.
DATA:    END OF ZDD03L.


*-------interne Tabelle für FI relevante Felder-------------------------
DATA: BEGIN OF XTCOBF OCCURS 10.
        INCLUDE STRUCTURE TCOBF.
DATA: END OF XTCOBF.


*------- Aktive Kundenstrukturen (ZBSEG, ... ) -------------------------
DATA:    BEGIN OF TABTAB OCCURS 10,
           TABLE        LIKE DD02L-TABNAME,  "Tabellenname
         END OF TABTAB.

*------- Felder auf CPD-Dynpro  ----------------------------------------
DATA:    BEGIN OF XCPD OCCURS 10,
           FIELD(20)    TYPE C,
         END OF XCPD.

DATA:    BEGIN OF LOGIC OCCURS 50,
           TEXT(72),
         END OF LOGIC.

DATA:    BEGIN OF MATCHC OCCURS 50,
           FELD1(30),
           FELD2(3),
           FELD3(30),
         END OF MATCHC.



*------- Einzelfelder --------------------------------------------------
DATA:    CHAR(30)       TYPE C,              " Char. Hilfsfeld
         CHAR1(1)       TYPE C,              " Char. Hilfsfeld
         CHAR2(1)       TYPE C,              " Char. Hilfsfeld
         CHAR20(20)     TYPE C.              " Char. Hilfsfeld
DATA:    BFNAM1(33)     TYPE C.              " BI-Feldname
DATA:    BFNAM2(33)     TYPE C.              " BI-Feldname
DATA:    BFELDN_NAM1(33) TYPE C.             " BI-Feldname
DATA:    BSLVON_NAM1(33) TYPE C.             " BI-Feldname
DATA:    BSLBIS_NAM1(33) TYPE C.             " BI-Feldname
DATA:    BFELDN_NAM2(33) TYPE C.             " BI-Feldname
DATA:    BSLVON_NAM2(33) TYPE C.             " BI-Feldname
DATA:    BSLBIS_NAM2(33) TYPE C.             " BI-Feldname
DATA:    FNAM(33)        TYPE C.              " BI-Feldname
DATA:    FNAM2(33)       TYPE C.              " BI-Feldname
DATA:    FNAM_IND(2)    TYPE C.              " BI-Feldname
DATA:    DYN_NAME(12)   TYPE C.              " Dynproname
DATA:    FTAB(3)        TYPE C.              " BI-Feldtabelle
DATA:    N              TYPE I.              " Zähler
DATA:    REFE1(8)       TYPE P.              " Hilfsfeld
DATA:    TFILL_XDD03L   TYPE I.              " Tabeinträge in XDD03L
DATA:    TFILL_YDD03L   TYPE I.              " Tabeinträge in YDD03L

*------- Konstanten ----------------------------------------------------
DATA:    FMF1GES(1)     TYPE X VALUE '20'.  " Beide Flags aus: Input.
DATA:    FMB1NUM(1)     TYPE X VALUE '10'.  "       "
DATA:    FELDN_NAM(33)  TYPE C VALUE 'BSELP-FELDN_'.
DATA:    SLVON_NAM(33)  TYPE C VALUE 'BSELP-SLVON_'.
DATA:    SLBIS_NAM(33)  TYPE C VALUE 'BSELP-SLBIS_'.
DATA:    STERN(1)       TYPE C VALUE '*'.




*eject
************************************************************************
*        Hauptablauf
************************************************************************

*------- Report-Tabelle initialisieren / Rahmen-Report einlesen --------
REFRESH REP.
READ REPORT 'RFBIBL02' INTO REP.
*------- RFBIBL02 löschen ----------------------------------------------
REFRESH REP.
PERFORM G_GENERIERUNGS-INFO.
PERFORM G_TABLES_ANWEISUNGEN.
*PERFORM G_WA_BKPF_DATEN_UEBERTRAGEN.
PERFORM G_WA_DATEN_UEBERTRAGEN.
PERFORM G_FILL_FTPOST_WITH_BBKPF_DATA.
PERFORM G_FILL_FTPOST_WITH_BBSEG_DATA.
PERFORM G_FILL_FTTAX_WITH_BBTAX_DATA.
PERFORM G_FILL_FTCLEAR_WITH_BSELP_DATA.


*------- Erzeugten Report speichern ------------------------------------
INSERT REPORT 'RFBIBL02' FROM REP.


*eject
*-----------------------------------------------------------------------
*       Generierung G_TABLES_ANWEISUNGEN.
*-----------------------------------------------------------------------
FORM G_TABLES_ANWEISUNGEN.

*-----------------------------------------------------------------------
*        TABTAB aus DD02L füllen
*-----------------------------------------------------------------------
  REFRESH TABTAB.
  SELECT * FROM DD02L WHERE ( TABNAME = 'ZBSEG' OR
                              TABNAME = 'ZSELP' )
                      AND   AS4LOCAL = 'A'
                      AND   TABCLASS = 'INTTAB'.
    TABTAB-TABLE = DD02L-TABNAME.
    APPEND TABTAB.
  ENDSELECT.

*-----------------------------------------------------------------------
*        Generierung der Tables Anweisungen für ZBxxx
*-----------------------------------------------------------------------
  LOOP AT TABTAB.
    REP-Z = 'TABLES $ .'.
    REPLACE '$' WITH TABTAB-TABLE INTO REP-Z.
    APPEND REP.
  ENDLOOP.
  IF SY-SUBRC = 0.
    REP-Z = SPACE.
    APPEND REP.
    APPEND REP.
    APPEND REP.
  ENDIF.
ENDFORM.

*eject
*-----------------------------------------------------------------------
*       Generierung GENERIERUNGS-INFO.
*-----------------------------------------------------------------------
FORM G_GENERIERUNGS-INFO.
*------- Generierungs-Informationen erzeugen ---------------------------
* REP-Z = '*----------------------------------------------------------'.
* APPEND REP.
* REP-Z = '*        Gererated  DATE $.'.
* REPLACE '$' WITH SY-DATUM INTO REP-Z.
* APPEND REP.
* REP-Z = '*                   TIME $.'.
* REPLACE '$' WITH SY-UZEIT INTO REP-Z.
* APPEND REP.
* REP-Z = '*                   USER $.'.
* REPLACE '$' WITH SY-UNAME INTO REP-Z.
* APPEND REP.
* REP-Z = '*----------------------------------------------------------'.
* APPEND REP.
* REP-Z = SPACE.
* APPEND REP.
*------- Generierungs-Informationen erzeugen ---------------------------
  REP-Z = '*  INCLUDE RFBIBL02.'.
  APPEND REP.
  REP-Z = SY-ULINE. REP-Z(1) = STERN.
  APPEND REP.
  REP-Z = '*        Generated  Date'.
  REP-Z+30 = SY-DATUM.
  APPEND REP.
  REP-Z = '*                   Time'.
  REP-Z+30 = SY-UZEIT.
  APPEND REP.
  REP-Z = '*                   Release'.
  REP-Z+30 = SY-SAPRL.
  APPEND REP.
  REP-Z = '*                   System'.
  REP-Z+30 = SY-SYSID.
  APPEND REP.
  REP-Z = '*                   User'.
  REP-Z+30 = SY-UNAME.
  APPEND REP.
  REP-Z = SY-ULINE. REP-Z(1) = STERN.
  APPEND REP.
  REP-Z = SPACE.
  APPEND REP.
  APPEND REP.
  APPEND REP.
ENDFORM.


*eject
*-----------------------------------------------------------------------
*        Generierung der Routine WA_BBKPF_DATEN_UEBERTRAGEN.
*-----------------------------------------------------------------------
FORM G_WA_BKPF_DATEN_UEBERTRAGEN.

*------- Doku generieren -----------------------------------------------
  REP-Z = '*eject'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.
  REP-Z = '*        FORM WA_BKPF_DATEN_UEBERTRAGEN.'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.

*------- Form ... ------------------------------------------------------
  REP-Z = 'FORM WA_BKPF_DATEN_UEBERTRAGEN.'.
  APPEND REP.

  REP-Z = '  CASE WA+1(1).'.
  APPEND REP.
  REP-Z = '  WHEN ''B''.'.
  APPEND REP.
  REP-Z = '    BBKPF = WA.'.
  APPEND REP.

    LOOP AT TABTAB WHERE TABLE = 'ZBKPF'.
      REP-Z = '  WHEN ''Z''.'.
      APPEND REP.
      REP-Z = '    ZBKPF = WA.'.
      APPEND REP.
      REP-Z = '    MOVE-CORRESPONDING ZZBKPF TO BBKPF.'.
      APPEND REP.
    ENDLOOP.

  REP-Z = '  WHEN OTHERS.'.
  APPEND REP.
  REP-Z = '    MESSAGE I159 WITH BELEG_COUNT WA+1(10).'.
  APPEND REP.
  REP-Z = '    MESSAGE I016.'.
  APPEND REP.
  REP-Z = '    PERFORM DUMP_WA USING ''BBKPF''.'.
  APPEND REP.
  REP-Z = '    MESSAGE A013.'.
  APPEND REP.
  REP-Z = '  ENDCASE.'.
  APPEND REP.
  REP-Z = 'ENDFORM.'.
  APPEND REP.
ENDFORM.


*eject
*-----------------------------------------------------------------------
*        Generierung der Routine WA_DATEN_UEBERTRAGEN.
*-----------------------------------------------------------------------
FORM G_WA_DATEN_UEBERTRAGEN.

*------- Doku generieren -----------------------------------------------
  REP-Z = '*eject'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.
  REP-Z = '*        FORM WA_DATEN_UEBERTRAGEN.'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.

*------- Form ... ------------------------------------------------------
  REP-Z = 'FORM WA_DATEN_UEBERTRAGEN.'.
  APPEND REP.

  REP-Z = '  CASE WA+1(1).'.
  APPEND REP.
  REP-Z = '  WHEN ''B''.'.
  APPEND REP.
  REP-Z = '    CASE WA+2(9).'.
  APPEND REP.
  REP-Z = '      WHEN ''BSEG''.'.
  APPEND REP.
  REP-Z = '        BBSEG = I_BBSEG.'.
  APPEND REP.
  REP-Z = '        BBSEG = WA.'.
  APPEND REP.
  REP-Z = '      WHEN ''BTAX''.'.
  APPEND REP.
  REP-Z = '        BBTAX = I_BBTAX.'.
  APPEND REP.
  REP-Z = '        BBTAX = WA.'.
  APPEND REP.
  REP-Z = '      WHEN ''SELK''.'.
  APPEND REP.
  REP-Z = '        BSELK = I_BSELK.'.
  APPEND REP.
  REP-Z = '        BSELK = WA.'.
  APPEND REP.
  REP-Z = '      WHEN ''SELP''.'.
  APPEND REP.
  REP-Z = '        BSELP = I_BSELP.'.
  APPEND REP.
  REP-Z = '        BSELP = WA.'.
  APPEND REP.
  REP-Z = '    ENDCASE.'.
  APPEND REP.

    REP-Z = '  WHEN ''Z''.'.
    APPEND REP.
    REP-Z = '    CASE WA+2(9).'.
    APPEND REP.
    LOOP AT TABTAB WHERE TABLE = 'ZBSEG'.
      REP-Z = '    WHEN ''BSEG''.'.
      APPEND REP.
      REP-Z = '      BBSEG = I_BBSEG.'.
      APPEND REP.
      REP-Z = '      ZBSEG = WA.'.
      APPEND REP.
      REP-Z = '      MOVE-CORRESPONDING ZBSEG TO BBSEG.'.
      APPEND REP.
    ENDLOOP.
    LOOP AT TABTAB WHERE TABLE = 'ZSELP'.
      REP-Z = '    WHEN ''SELP''.'.
      APPEND REP.
      REP-Z = '      BSELP = I_BSELP.'.
      APPEND REP.
      REP-Z = '      ZSELP = WA.'.
      APPEND REP.
      REP-Z = '      MOVE-CORRESPONDING ZSELP TO BSELP.'.
      APPEND REP.
    ENDLOOP.
    REP-Z = '    ENDCASE.'.
    APPEND REP.

  REP-Z = '  ENDCASE.'.
  APPEND REP.
  REP-Z = 'ENDFORM.'.
  APPEND REP.
ENDFORM.

*eject
*-----------------------------------------------------------------------
*        Generierung der Routine 'FILL_FTPOST_WITH_BBKPF_DATA.'.
*-----------------------------------------------------------------------
FORM G_FILL_FTPOST_WITH_BBKPF_DATA.

*------- Doku generieren -----------------------------------------------
  REP-Z = '*eject'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.
  REP-Z = '*        FORM FILL_FTPOST_WITH_BBKPF_DATA.'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.

*------- Form ... ------------------------------------------------------
  REP-Z = 'FORM FILL_FTPOST_WITH_BBKPF_DATA.'.
  APPEND REP.

  REP-Z = '  CHECK FL_CHECK = SPACE.'.
  APPEND REP.
  REP-Z = '  CHECK FUNCTION NE ''D''.'.
  APPEND REP.
  REP-Z = '  CLEAR FTPOST.          '.
  APPEND REP.
  REP-Z = '  FTPOST-STYPE = ''K''.  '.
  APPEND REP.
  REP-Z = '  FTPOST-COUNT = ''001''.'.
  APPEND REP.

  PERFORM TABELLE_DD03L_LESEN USING 'BBKPF'.

*-------FTPOST füllen                                     --------------
  LOOP AT XDD03L.                                            " QHA
    PERFORM BBKPF_FIELDS_TO_FTPOST.
  ENDLOOP.

  REP-Z = 'ENDFORM.'.
  APPEND REP.
  REP-Z = SPACE.
  APPEND REP.
ENDFORM.



*eject
*-----------------------------------------------------------------------
*        Generierung der Routine 'FILL_FTPOST_WITH_BBSEG_DATA'
*-----------------------------------------------------------------------
FORM G_FILL_FTPOST_WITH_BBSEG_DATA.

*------- Doku generieren -----------------------------------------------
  REP-Z = '*eject'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.
  REP-Z = '*        FORM FILL_FTPOST_WITH_BBSEG_DATA USING COUNT    '.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.

*------- Form ... ------------------------------------------------------
  REP-Z = 'FORM FILL_FTPOST_WITH_BBSEG_DATA USING COUNT.    '.
  APPEND REP.

  REP-Z = '  CHECK FL_CHECK = SPACE.'.
  APPEND REP.
  REP-Z = '  CHECK FUNCTION NE ''D''.'.
  APPEND REP.
  REP-Z = '  CLEAR FTPOST.          '.
  APPEND REP.
  REP-Z = '  FTPOST-STYPE = ''P''.  '.
  APPEND REP.
  REP-Z = '  FTPOST-COUNT = COUNT.  '.
  APPEND REP.

  PERFORM TABELLE_DD03L_LESEN USING 'BBSEG'.

  PERFORM TABELLE_XCPD_FUELLEN.
  SELECT * FROM TCOBF INTO TABLE XTCOBF WHERE XBAT_FI = 'X'.

*-------FTPOST füllen                                     --------------
  LOOP AT XDD03L.                                            " QHA
    PERFORM BBSEG_FIELDS_TO_FTPOST.
  ENDLOOP.

  REP-Z = 'ENDFORM.'.
  APPEND REP.
  REP-Z = SPACE.
  APPEND REP.
ENDFORM.


*eject
*-----------------------------------------------------------------------
*        Generierung der Routine 'FILL_FTTAX_WITH_BBTAX_DATA.'.
*-----------------------------------------------------------------------
FORM G_FILL_FTTAX_WITH_BBTAX_DATA.

*------- Doku generieren -----------------------------------------------
  REP-Z = '*eject'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.
  REP-Z = '*        FORM FILL_FTTAX_WITH_BBTAX_DATA.'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.

*------- Form ... ------------------------------------------------------
  REP-Z = 'FORM FILL_FTTAX_WITH_BBTAX_DATA.'.
  APPEND REP.

  REP-Z = '  CHECK FL_CHECK = SPACE.'.
  APPEND REP.
  REP-Z = '  CHECK FUNCTION NE ''D''.'.
  APPEND REP.
  REP-Z = '  CLEAR FTTAX.          '.
  APPEND REP.

  PERFORM TABELLE_DD03L_LESEN USING 'BBTAX'.

*-------FTPOST füllen                                     --------------
  LOOP AT XDD03L.                                            " QHA
    PERFORM BBTAX_FIELDS_TO_FTTAX.
  ENDLOOP.

  REP-Z = '  IF NOT FTTAX IS INITIAL.'.
  APPEND REP.
  REP-Z = '    APPEND FTTAX.'.
  APPEND REP.
  REP-Z = '  ENDIF.'.
  APPEND REP.
  REP-Z = 'ENDFORM.'.
  APPEND REP.
  REP-Z = SPACE.
  APPEND REP.
ENDFORM.


*eject
*-----------------------------------------------------------------------
*        Generierung der Routine 'FILL_FTCLEAR_WITH_BSELP_DATA'
*-----------------------------------------------------------------------
FORM G_FILL_FTCLEAR_WITH_BSELP_DATA.

*------- Doku generieren -----------------------------------------------
  REP-Z = '*eject'.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.
  REP-Z = '*        FORM FILL_FTCLEAR_WITH_BSELP_DATA.    '.
  APPEND REP.
  REP-Z = '*----------------------------------------------------------'.
  APPEND REP.

*------- Form ... ------------------------------------------------------
  REP-Z = 'FORM FILL_FTCLEAR_WITH_BSELP_DATA.    '.
  APPEND REP.

  REP-Z = '  CHECK FL_CHECK = SPACE.'.
  APPEND REP.


*-------FTCLEAR füllen                                   --------------
  PERFORM BSELP_FIELDS_TO_FTCLEAR.

  REP-Z = 'ENDFORM.'.
  APPEND REP.
  REP-Z = SPACE.
  APPEND REP.
ENDFORM.




*eject
*-----------------------------------------------------------------------
*       Form TABELLE_DD03L_LESEN USING TAB.
*-----------------------------------------------------------------------
FORM TABELLE_DD03L_LESEN USING TAB.
  DATA: TABIX LIKE SY-TABIX.
  DATA: POS   LIKE SY-TABIX.
  DATA: ANZ_DEL TYPE I.

  REFRESH XDD03L.
  REFRESH ZDD03L.

  SELECT * FROM DD03L INTO TABLE XDD03L
    WHERE TABNAME = TAB
    AND   AS4LOCAL = 'A'.

  DESCRIBE TABLE XDD03L LINES TFILL_XDD03L.
  SORT XDD03L BY POSITION.
  LOOP AT XDD03L.
    ZDD03L = XDD03L.
    APPEND ZDD03L.
  ENDLOOP.

  LOOP AT ZDD03L WHERE FIELDNAME CP '.INCLU++++'.
*   TABIX = SY-TABIX.
    TABIX = SY-TABIX - ANZ_DEL.
    POS   = SY-TABIX - 1 - ANZ_DEL.
**  POS   = SY-TABIX - 1.
**  REFRESH YDD03L.
**  SELECT * FROM DD03L INTO TABLE YDD03L
*     WHERE TABNAME = XDD03L-ROLLNAME
**    WHERE TABNAME = ZDD03L-PRECFIELD
**    AND   AS4LOCAL = 'A'.
**  SORT YDD03L BY POSITION.
**  DESCRIBE TABLE YDD03L LINES TFILL_YDD03L.

* ----- Include aus XDD03L löschen und Position hochsetzen
    DELETE XDD03L INDEX TABIX.
    ANZ_DEL = ANZ_DEL + 1.
    LOOP AT XDD03L WHERE POSITION > POS.
**    XDD03L-POSITION = XDD03L-POSITION + TFILL_YDD03L - 1.
      XDD03L-POSITION = XDD03L-POSITION - 1.
      MODIFY XDD03L.
    ENDLOOP.
**  LOOP AT YDD03L.
**    YDD03L-POSITION = YDD03L-POSITION + POS.
*     XDD03L = XDD03L.                        "QHA
**    XDD03L = YDD03L.                        "QHA
**    APPEND XDD03L.
**  ENDLOOP.
    SORT XDD03L BY POSITION.

  ENDLOOP.


ENDFORM.

*eject
*-----------------------------------------------------------------------
*       Form  BSELP_FIELDS_TO_FTCLEAR
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FORM BSELP_FIELDS_TO_FTCLEAR.
  N = 0.
  BFELDN_NAM1  = FELDN_NAM.
  BSLVON_NAM1  = SLVON_NAM.
  BSLBIS_NAM1  = SLBIS_NAM.
  DO 18 TIMES.
    N = N + 1.
    PERFORM INDEX_SETZEN.
    BFELDN_NAM1+12(2) = FNAM_IND.
    BSLVON_NAM1+12(2) = FNAM_IND.
    BSLBIS_NAM1+12(2) = FNAM_IND.

    BFELDN_NAM2 = BFELDN_NAM1.
    BSLVON_NAM2 = BSLVON_NAM1.
    BSLBIS_NAM2 = BSLBIS_NAM1.

    BFELDN_NAM2+30 = '(1)'.
    BSLVON_NAM2+30 = '(1)'.
    BSLBIS_NAM2+30 = '(1)'.

    CONDENSE BFELDN_NAM2 NO-GAPS.
    CONDENSE BSLVON_NAM2 NO-GAPS.
    CONDENSE BSLBIS_NAM2 NO-GAPS.


*------- Batch-Input für eines der drei Felder vorhanden? --------------
    REP-Z = '  IF $ NE NODATA '.
    REPLACE '$' WITH BFELDN_NAM2 INTO REP-Z.
    APPEND REP.
    REP-Z = '  OR $ NE NODATA '.
    REPLACE '$' WITH BSLVON_NAM2 INTO REP-Z.
    APPEND REP.
    REP-Z = '  OR $ NE NODATA.'.
    REPLACE '$' WITH BSLBIS_NAM2 INTO REP-Z.
    APPEND REP.

    REP-Z = '    CLEAR FTCLEAR.         '.
    APPEND REP.
    REP-Z = '    FTCLEAR = SAVE_FTCLEAR.          '.
    APPEND REP.

*------- Batch-Input Feld FELDN vorhanden? ----------------------------
    REP-Z = '    IF $ NE NODATA.'.
    REPLACE '$' WITH BFELDN_NAM2 INTO REP-Z.
    APPEND REP.
    REP-Z = '      FTCLEAR-SELFD = $.          '.
    REPLACE '$' WITH BFELDN_NAM1 INTO REP-Z.
    APPEND REP.
    REP-Z = '    ENDIF.'.
    APPEND REP.

*------- Batch-Input Feld SLVON vorhanden? ----------------------------
    REP-Z = '    IF $ NE NODATA.'.
    REPLACE '$' WITH BSLVON_NAM2 INTO REP-Z.
    APPEND REP.
    REP-Z = '      FTCLEAR-SELVON = $.          '.
    REPLACE '$' WITH BSLVON_NAM1 INTO REP-Z.
    APPEND REP.
    REP-Z = '    ENDIF.'.
    APPEND REP.

*------- Batch-Input Feld SLBIS vorhanden? ----------------------------
    REP-Z = '    IF $ NE NODATA.'.
    REPLACE '$' WITH BSLBIS_NAM2 INTO REP-Z.
    APPEND REP.
    REP-Z = '      FTCLEAR-SELBIS = $.          '.
    REPLACE '$' WITH BSLBIS_NAM1 INTO REP-Z.
    APPEND REP.
    REP-Z = '    ENDIF.'.
    APPEND REP.

*------- Append FTCLEAR
    REP-Z = '    APPEND FTCLEAR.'.
    APPEND REP.
    REP-Z = '  ENDIF.'.
    APPEND REP.
  ENDDO.
ENDFORM.


*eject
*-----------------------------------------------------------------------
*       Form  BBSEG_FIELDS_TO_FTPOST.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FORM BBSEG_FIELDS_TO_FTPOST.
  CHECK XDD03L-FIELDNAME NE 'STYPE'.
  CHECK XDD03L-FIELDNAME NE 'TBNAM'.
  CHECK XDD03L-FIELDNAME NE 'DUMMY'.
  CHECK XDD03L-FIELDNAME NE 'DUMMY2'.
  CHECK XDD03L-FIELDNAME NE 'DUMMY3'.
  CHECK XDD03L-FIELDNAME NE 'DUMMY4'.
  CHECK XDD03L-FIELDNAME NE 'SENDE'.


*------- BFNAM generieren
  BFNAM1       = XDD03L-TABNAME.
  BFNAM1+10(1) = '-'.
  BFNAM1+11    = XDD03L-FIELDNAME.
  CONDENSE BFNAM1 NO-GAPS.

*------- BFNAM2 generieren
  BFNAM2       = BFNAM1.
  BFNAM2+30(3) = '(1)'.
  CONDENSE BFNAM2 NO-GAPS.

*------- FNAM2 gen. für Nicht SAKO-Dynpros bei COBL-Namensungleichheiten
  FNAM2       = 'BSEG'.                                       " 30C
  FNAM2+10(1)  = '-'.                                         " 30C
  FNAM2+11     = XDD03L-FIELDNAME.                            " 30C
  CONDENSE FNAM2 NO-GAPS.                                     " 30C

*------- Namensungleichheiten COBL und BBSEG ausgleichen
  IF XDD03L-FIELDNAME = 'VBEL2'.
    XDD03L-FIELDNAME = 'KDAUF'.             " Kundenauftragsnummer
  ENDIF.
  IF XDD03L-FIELDNAME = 'POSN2'.
    XDD03L-FIELDNAME = 'KDPOS'.             " Position im Kundenauftrag
  ENDIF.
  IF XDD03L-FIELDNAME = 'ETEN2'.
    XDD03L-FIELDNAME = 'KDEIN'.             " Einteilung im Kundenauftra
  ENDIF.
  IF XDD03L-FIELDNAME = 'BEWAR'.
    XDD03L-FIELDNAME = 'RMVCT'.             " Bewegungsart
  ENDIF.
  IF XDD03L-FIELDNAME = 'PROJK'.
    XDD03L-FIELDNAME = 'PS_PSP_PNR'.        " Kontierung Project
  ENDIF.
  IF XDD03L-FIELDNAME = 'PPRCT'.
    XDD03L-FIELDNAME = 'PPRCTR'.            " Partner Profitcenter
  ENDIF.

*------- FNAM generieren
  FNAM        = XDD03L-TABNAME+1.
  IF XDD03L-FIELDNAME = 'NEWBS'
  OR XDD03L-FIELDNAME = 'NEWKO'
  OR XDD03L-FIELDNAME = 'NEWUM'
  OR XDD03L-FIELDNAME = 'NEWBW'
  OR XDD03L-FIELDNAME = 'NEWBK'.
    FNAM      = 'RF05A'.
  ELSEIF ( XDD03L-FIELDNAME = 'WBANK'                    "QHA
           OR XDD03L-FIELDNAME = 'WORT1'                 "QHA
           OR XDD03L-FIELDNAME = 'WORT2'
           OR XDD03L-FIELDNAME = 'WNAME'                 "QHA
           OR XDD03L-FIELDNAME = 'WBZOG' ).              "QHA
    FNAM      = 'BSED'.                                  "QHA

  ELSEIF ( XDD03L-FIELDNAME = 'BANKL'                    "QHA
           OR XDD03L-FIELDNAME = 'BANKS'                 "QHA
           OR XDD03L-FIELDNAME = 'BKONT'                 "QHA
           OR XDD03L-FIELDNAME = 'BANKN' ).

    FNAM      = 'BSEC'.                                  "QHA


  ELSE.
    LOOP AT XCPD WHERE FIELD = XDD03L-FIELDNAME.
      CHECK XCPD-FIELD NE 'STCEG'.
      FNAM      = 'BSEC'.
    ENDLOOP.
    LOOP AT XTCOBF WHERE FIELD = XDD03L-FIELDNAME.
      FNAM      = 'COBL'.
    ENDLOOP.
  ENDIF.
  FNAM+10(1)  = '-'.
  FNAM+11     = XDD03L-FIELDNAME.
  CONDENSE FNAM NO-GAPS.


*------- Batch-Input für das Feld vorhanden ? --------------------------
  REP-Z = '  IF $ NE NODATA.'.
  REPLACE '$' WITH BFNAM2 INTO REP-Z.
  APPEND REP.

*------- Steuern nur in FTPOST, wenn kein Sachkonten-Buchungsschlüssel
  IF BFNAM1 = 'BBSEG-WMWST' OR BFNAM1 = 'BBSEG-MWSTS'.
    REP-Z = '   IF XTBSL-KOART NE ''S''.'.
    APPEND REP.
  ENDIF.

*------- Feldname in FTPOST---------------------------------------------
  REP-Z = '    CLEAR: FTPOST-FNAM, FTPOST-FVAL. '.
  APPEND REP.
  IF FNAM+0(4) = 'COBL'.
    REP-Z = '    IF XTBSL-KOART = ''S''.'.
    APPEND REP.
    REP-Z = '      FTPOST-FNAM = ''$''.'.
    REPLACE '$' WITH FNAM INTO REP-Z.
    APPEND REP.
    REP-Z = '    ELSE.'.
    APPEND REP.
    REP-Z = '      FTPOST-FNAM = ''$''.'.
*   FNAM+0(4) = 'BSEG'.                                         " 30C
*   REPLACE '$' WITH FNAM INTO REP-Z.                           " 30C
    REPLACE '$' WITH FNAM2 INTO REP-Z.                          " 30C
    APPEND REP.
    REP-Z = '    ENDIF.'.
    APPEND REP.
  ELSE.
    REP-Z = '    FTPOST-FNAM = ''$''.'.
    REPLACE '$' WITH FNAM INTO REP-Z.
    APPEND REP.
  ENDIF.

*------- Feldwert in FTPOST --------------------------------------------
  REP-Z = '    FTPOST-FVAL = $.'.
  REPLACE '$' WITH BFNAM1 INTO REP-Z.
  APPEND REP.
  REP-Z = '    APPEND FTPOST.'.
  APPEND REP.
*------- Steuern nur in FTPOST, wenn kein Sachkonten-Buchungsschlüssel
  IF BFNAM1 = 'BBSEG-WMWST' OR BFNAM1 = 'BBSEG-MWSTS'.
    REP-Z = '   ENDIF.'.
    APPEND REP.
  ENDIF.

  REP-Z = '  ENDIF.'.
  APPEND REP.
ENDFORM.

*eject
*-----------------------------------------------------------------------
*       Form TABELLE_XCPD_FUELLEN.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FORM TABELLE_XCPD_FUELLEN.
  DYN_NAME = 'SAPLFCPD0100'.
  IMPORT DYNPRO HEADER FIELDS LOGIC MATCHC ID DYN_NAME.

    LOOP AT FIELDS.
      CHECK FIELDS-FMB1     Z   FMF1GES
      AND   FIELDS-FMB1     Z   FMB1NUM.

      CHECK FIELDS-FNAM     NE 'OK-CODE'.


      CHECK FIELDS-FNAM(11) NE 'RF05A-NEWBS'
      AND   FIELDS-FNAM(11) NE 'RF05A-NEWKO'
      AND   FIELDS-FNAM(11) NE 'RF05A-NEWUM'
      AND   FIELDS-FNAM(11) NE 'RF05A-NEWBK'
      AND   FIELDS-FNAM(11) NE 'RF05A-NEWBW'.

      CHECK FIELDS-FNAM     NE 'BLOCK'.
      CHECK FIELDS-FNAM     NE 'BLOCK1'.
      CHECK FIELDS-FNAM     NE 'BSEG-EGBLD'.

      CHAR20 = FIELDS-FNAM.
      IF CHAR20 CA '-'. ENDIF.
      SHIFT CHAR20 LEFT  BY 1 PLACES.
      SHIFT CHAR20 LEFT  BY SY-FDPOS PLACES.
      XCPD-FIELD = CHAR20.
      APPEND XCPD.
    ENDLOOP.
ENDFORM.


*eject
*-----------------------------------------------------------------------
*       Form FIELDS_TO_FTPOST.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FORM BBKPF_FIELDS_TO_FTPOST.
  CHECK XDD03L-FIELDNAME NE 'STYPE'.
  CHECK XDD03L-FIELDNAME NE 'TCODE'.
  CHECK XDD03L-FIELDNAME NE 'AUGLV'.
  CHECK XDD03L-FIELDNAME NE 'SENDE'.

*------- BFNAM generieren
  BFNAM1       = XDD03L-TABNAME.
  BFNAM1+10(1) = '-'.
  BFNAM1+11    = XDD03L-FIELDNAME.
  CONDENSE BFNAM1 NO-GAPS.

*------- BFNAM2 generieren
  BFNAM2       = BFNAM1.
  BFNAM2+30(3) = '(1)'.
  CONDENSE BFNAM2 NO-GAPS.

*------- FNAM generieren
  FNAM        = BFNAM1+1.
  CASE XDD03L-FIELDNAME.
    WHEN 'PARGB'.
      FNAM+0(6) = 'RF05A-'.
      FNAM+6    = XDD03L-FIELDNAME.
    WHEN 'DOCID'.
      FNAM+0(6) = 'FS006-'.
      FNAM+6    = XDD03L-FIELDNAME.
    WHEN 'BARCD'.
      FNAM+0(6) = 'FS006-'.
      FNAM+6    = XDD03L-FIELDNAME.
  ENDCASE.

* IF XDD03L-FIELDNAME = 'PARGB'.
*   FNAM+0(6) = 'RF05A-'.
*   FNAM+6    = XDD03L-FIELDNAME.
* ENDIF.

*------- Batch-Input für das Feld vorhanden ? --------------------------
  REP-Z = '  IF $ NE NODATA.'.
  REPLACE '$' WITH BFNAM2 INTO REP-Z.
  APPEND REP.

*------- Feldname in FTPOST---------------------------------------------
  REP-Z = '    CLEAR: FTPOST-FNAM, FTPOST-FVAL. '.
  APPEND REP.
  REP-Z = '    FTPOST-FNAM = ''$''.'.
  REPLACE '$' WITH FNAM INTO REP-Z.
  APPEND REP.
*------- Feldwert in FTPOST --------------------------------------------
  REP-Z = '    FTPOST-FVAL = $.'.
  REPLACE '$' WITH BFNAM1 INTO REP-Z.
  APPEND REP.
  REP-Z = '    APPEND FTPOST.'.
  APPEND REP.
  REP-Z = '  ENDIF.'.
  APPEND REP.
ENDFORM.

*eject
*-----------------------------------------------------------------------
*       Form FIELDS_TO_FTTAX.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
FORM BBTAX_FIELDS_TO_FTTAX.
  CHECK XDD03L-FIELDNAME NE 'STYPE'.
  CHECK XDD03L-FIELDNAME NE 'TBNAM'.

*------- BFNAM generieren
  BFNAM1       = XDD03L-TABNAME.
  BFNAM1+10(1) = '-'.
  BFNAM1+11    = XDD03L-FIELDNAME.
  CONDENSE BFNAM1 NO-GAPS.

*------- BFNAM2 generieren
  BFNAM2       = BFNAM1.
  BFNAM2+30(3) = '(1)'.
  CONDENSE BFNAM2 NO-GAPS.

*------- FNAM generieren
  FNAM+0(2)   = 'FT'.
  FNAM+2      = BFNAM1+2.

*------- Batch-Input für das Feld vorhanden ? --------------------------
  REP-Z = '  IF $ NE NODATA.'.
  REPLACE '$' WITH BFNAM2 INTO REP-Z.
  APPEND REP.

*------- Move   --------------------------------------------------------
  REP-Z = ' $1 = $2 .'.
  REPLACE '$1' WITH FNAM   INTO REP-Z.
  REPLACE '$2' WITH BFNAM1 INTO REP-Z.
  CONDENSE REP-Z.
  SHIFT REP-Z RIGHT BY 4 PLACES.
  APPEND REP.

*------- Append --------------------------------------------------------
* REP-Z = '    APPEND FTTAX.'.
* APPEND REP.
  REP-Z = '  ENDIF.'.
  APPEND REP.
ENDFORM.

*eject
*-----------------------------------------------------------------------
*       Form FIELDTAB_FUELLEN_LOOP
*-----------------------------------------------------------------------
*       Bezieht sich nur auf Dynpro SAPMF05A0733
*       Je Dynprofeld:
*       -  gibt es Input für dieses Feld?
*          falls ja: -  Feldname in Feldtabelle stellen
*                    -  Feldwert in Feldtabelle stellen
*-----------------------------------------------------------------------
FORM FIELDTAB_FUELLEN_LOOP.

*------- Feldname des Schnittstellenfeldes ermitteln ------------
  CLEAR: BFNAM1, BFNAM2.
  BFNAM1(6) = 'BSELP-'.
  BFNAM1+6  = FIELDS-FNAM+6.
  IF FIELDS-FNAM+6 = 'SEL01'.
    BFNAM1+6 = 'SLVON'.
  ENDIF.
  IF FIELDS-FNAM+6 = 'SEL02'.
    BFNAM1+6 = 'SLBIS'.
  ENDIF.
  PERFORM INDEX_SETZEN.

*------- Batch-Input für das Feld vorhanden ? --------------------------
  REP-Z = '  IF $ NE NODATA.'.
  BFNAM2 = BFNAM1.
  BFNAM2+30(3) = '(1)'.
  CONDENSE BFNAM2 NO-GAPS.
  REPLACE '$' WITH BFNAM2 INTO REP-Z.
  APPEND REP.

*------- Feldname und Feldwert in FTA ----------------------------------
  REP-Z = '    CLEAR FTA.'.
  APPEND REP.
  REP-Z = '    FTA-FNAM = ''$''.'.
  FIELDS-FNAM+20(1) = '('.
  FIELDS-FNAM+21(2) = N.
  FIELDS-FNAM+23(1) = ')'.
  CONDENSE FIELDS-FNAM NO-GAPS.
  REPLACE '$' WITH FIELDS-FNAM INTO REP-Z.
  APPEND REP.
  REP-Z = '    FTA-FVAL = $.'.
  REPLACE '$' WITH BFNAM1 INTO REP-Z.
  APPEND REP.
  REP-Z = '    APPEND FTA.'.
  APPEND REP.
  REP-Z = '  ENDIF.'.
  APPEND REP.
ENDFORM.

*eject
*-----------------------------------------------------------------------
*        Form  INDEX_SETZEN
*-----------------------------------------------------------------------
*        Feldindex für Loopfelder auf 0733 setzen
*-----------------------------------------------------------------------
FORM INDEX_SETZEN.
  CASE N.
    WHEN 1.
      FNAM_IND(2) = '1 '.
    WHEN 2.
      FNAM_IND(2) = '2 '.
    WHEN 3.
      FNAM_IND(2) = '3 '.
    WHEN 4.
      FNAM_IND(2) = '4 '.
    WHEN 5.
      FNAM_IND(2) = '5 '.
    WHEN 6.
      FNAM_IND(2) = '6 '.
    WHEN 7.
      FNAM_IND(2) = '7 '.
    WHEN 8.
      FNAM_IND(2) = '8 '.
    WHEN 9.
      FNAM_IND(2) = '9 '.
    WHEN 10.
      FNAM_IND(2) = '10'.
    WHEN 11.
      FNAM_IND(2) = '11'.
    WHEN 12.
      FNAM_IND(2) = '12'.
    WHEN 13.
      FNAM_IND(2) = '13'.
    WHEN 14.
      FNAM_IND(2) = '14'.
    WHEN 15.
      FNAM_IND(2) = '15'.
    WHEN 16.
      FNAM_IND(2) = '16'.
    WHEN 17.
      FNAM_IND(2) = '17'.
    WHEN 18.
      FNAM_IND(2) = '18'.
  ENDCASE.
ENDFORM.



*-----------------------------------------------------------------------
*        Eingaberelevante FI-Felder des Kontierungsblocks
*        selektieren
*-----------------------------------------------------------------------
FORM FELDER_KONTBLOCK.
* SELECT * FROM TCOBF WHERE XBAT_FI = 'X'.
*   CLEAR HTCOBF.
*   MOVE-CORRESPONDING TCOBF TO HTCOBF.
*   APPEND HTCOBF.
* ENDSELECT.
* LOOP AT FIELDS.
*   IF  FIELDS-FMB1 Z     FMF1GES
*   AND FIELDS-FMB1 Z     FMB1NUM.
*     LOOP AT HTCOBF
*       WHERE FIELD = FIELDS-FNAM+5(10).
*       EXIT.
*     ENDLOOP.
*     IF SY-SUBRC NE 0.
*       DELETE FIELDS.
*     ENDIF.
*   ELSE.
*     DELETE FIELDS.
*   ENDIF.
* ENDLOOP.
ENDFORM.


*-----------------------------------------------------------------------
*        Cursor auf Feld RF05A-NEWKO oder n RF05A-AGKON setzen
*        für Matchcodeeingabe
*-----------------------------------------------------------------------
FORM CURSOR_SETZEN_FUER_MATCHCODE USING FELD.

*------- Feldname und Feldwert in FT -----------------------------------
  REP-Z = '*  Cursor auf Feld & setzen für Matchcodeeingabe'.
  REPLACE '&' WITH FELD INTO REP-Z.
  APPEND REP.
  REP-Z = '  CLEAR FTA.'.
  APPEND REP.
  REP-Z = '  FTA-FNAM = ''BDC_CURSOR''.'.
  APPEND REP.
  REP-Z = '  FTA-FVAL = ''&''.'.
  REPLACE '&' WITH FELD INTO REP-Z.
  APPEND REP.
  REP-Z = '  APPEND FTA.'.
  APPEND REP.
  REP-Z = SPACE.
  APPEND REP.
ENDFORM.
