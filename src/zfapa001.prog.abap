*2000/08/31 mdemeest 4.6B All changes are identified with UGL on     UGL
*                                                         right      UGL
*                        Copied RFFOUS_C to ZFAPA001                 UGL
*                        Copied RFFORI01 to ZNFAP001                 UGL
*                        Copied RFFORI06 to ZNFAP001                 UGL
************************************************************************
*                                                                      *
*  Check print program RFFOUS_C                                        *
*                                                                      *
************************************************************************


*----------------------------------------------------------------------*
* Program includes:                                                    *
*                                                                      *
* RFFORI0M  Definition of macros                                       *
* RFFORI00  international data definitions                             *
* RFFORI01  check                                                      *
* RFFORI06  remittance advice                                          *
* RFFORI07  payment summary list                                       *
* RFFORI99  international subroutines                                  *
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* report header                                                        *
*----------------------------------------------------------------------*
REPORT RFFOUS_C
  LINE-SIZE 132
  MESSAGE-ID F0
  NO STANDARD PAGE HEADING.



*----------------------------------------------------------------------*
*  segments and tables for prenumbered checks                          *
*----------------------------------------------------------------------*
TABLES:
  REGUH,
  REGUP.


*----------------------------------------------------------------------*
*  macro definitions                                                   *
*----------------------------------------------------------------------*
INCLUDE RFFORI0M.

INITIALIZATION.

*----------------------------------------------------------------------*
*  parameters and select-options                                       *
*----------------------------------------------------------------------*
  BLOCK 1.
  SELECT-OPTIONS:
    SEL_ZAWE FOR  REGUH-RZAWE,              "payment method
    SEL_UZAW FOR  REGUH-UZAWE,              "payment method supplement
    SEL_GSBR FOR  REGUH-SRTGB,              "business area
    SEL_HBKI FOR  REGUH-HBKID NO-EXTENSION NO INTERVALS, "house bank id
    SEL_HKTI FOR  REGUH-HKTID NO-EXTENSION NO INTERVALS. "account id
  SELECTION-SCREEN:
    BEGIN OF LINE,
    COMMENT 01(30) TEXT-106 FOR FIELD PAR_STAP,
    POSITION POS_LOW.
  PARAMETERS:
    PAR_STAP LIKE RFPDO-FORDSTAP.           "check lot number
  SELECTION-SCREEN:
    COMMENT 40(30) TEXTINFO FOR FIELD PAR_STAP,
    END OF LINE.
  PARAMETERS:
    PAR_RCHK LIKE RFPDO-FORDRCHK.           "Restart from
  SELECT-OPTIONS:
    SEL_WAER FOR  REGUH-WAERS,              "currency
    SEL_VBLN FOR  REGUH-VBLNR.              "payment document number
  SELECTION-SCREEN END OF BLOCK 1.

  BLOCK 2.
  AUSWAHL: ZDRU Z, AVIS A, BEGL B.
  SPOOL_AUTHORITY.                     "Spoolberechtigung
  SELECTION-SCREEN END OF BLOCK 2.

  BLOCK 3.
  PARAMETERS:
    PAR_ZFOR LIKE RFPDO1-FORDZFOR,          "different form
    PAR_FILL LIKE RFPDO2-FORDFILL,          "filler for spell_amount
    PAR_ANZP LIKE RFPDO-FORDANZP,           "number of test prints
    PAR_MAXP LIKE RFPDO-FORDMAXP,           "no of items in summary list
    PAR_BELP LIKE RFPDO-FORDBELP,           "payment doc. validation
    PAR_ESPR LIKE RFPDO-FORDESPR,           "texts in reciepient's lang.
    PAR_ISOC LIKE RFPDO-FORDISOC,           "currency in ISO code
    PAR_NOSU LIKE RFPDO2-FORDNOSU.          "no summary page
  SELECTION-SCREEN END OF BLOCK 3.

  SELECTION-SCREEN:
    BEGIN OF BLOCK 4 WITH FRAME TITLE TEXT-100,
    BEGIN OF LINE.
  PARAMETERS:
    PAR_NEUD AS CHECKBOX.
  SELECTION-SCREEN:
    COMMENT 03(70) TEXT-101 FOR FIELD PAR_NEUD,
    END OF LINE,
    BEGIN OF LINE,
    COMMENT 01(31) TEXTCHKF FOR FIELD PAR_CHKF,
    POSITION POS_LOW.
  PARAMETERS:
    PAR_CHKF LIKE PAYR-CHECF.
  SELECTION-SCREEN:
    COMMENT 52(05) TEXTCHKT FOR FIELD PAR_CHKT,
    POSITION POS_HIGH.
  PARAMETERS:
    PAR_CHKT LIKE PAYR-CHECT.
  SELECTION-SCREEN:
    END OF LINE,
    BEGIN OF LINE,
    COMMENT 01(30) TEXT-107 FOR FIELD PAR_VOID,
    POSITION POS_LOW.
  PARAMETERS:
    PAR_VOID LIKE PAYR-VOIDR.
  SELECTION-SCREEN:
    COMMENT 38(30) TEXTVOID FOR FIELD PAR_VOID,
    END OF LINE,
    END OF BLOCK 4.

  PARAMETERS:
    PAR_XDTA LIKE RFPDO-FORDXDTA  NO-DISPLAY,
    PAR_PRIW LIKE RFPDO-FORDPRIW  NO-DISPLAY,
    PAR_SOFW LIKE RFPDO1-FORDSOFW NO-DISPLAY,
    PAR_DTYP LIKE RFPDO-FORDDTYP  NO-DISPLAY,
    PAR_UNIX LIKE RFPDO2-FORDNAMD NO-DISPLAY,
    PAR_NENQ(1)  TYPE C           NO-DISPLAY,
    PAR_VARI(12) TYPE C           NO-DISPLAY,
    PAR_SOFO(1)  TYPE C           NO-DISPLAY.



*----------------------------------------------------------------------*
*  Default values for parameters and select-options                    *
*----------------------------------------------------------------------*
  PERFORM INIT.
  PERFORM TEXT(SAPDBPYF) USING 102 TEXTZDRU.
  PERFORM TEXT(RFCHKL00) USING: TEXTCHKF 200, TEXTCHKT 201.
  SEL_ZAWE-LOW    = 'C'.
  SEL_ZAWE-OPTION = 'EQ'.
  SEL_ZAWE-SIGN   = 'I'.
  APPEND SEL_ZAWE.

  PAR_BELP = SPACE.
  PAR_ZDRU = 'X'.
  PAR_XDTA = SPACE.
  PAR_DTYP = SPACE.
  PAR_AVIS = SPACE.
  PAR_BEGL = 'X'.
  PAR_FILL = SPACE.
  PAR_ANZP = 2.
  PAR_ESPR = SPACE.
  PAR_ISOC = SPACE.
  PAR_MAXP = 9999.



*----------------------------------------------------------------------*
*  tables / fields / field-groups / at selection-screen                *
*----------------------------------------------------------------------*
INCLUDE RFFORI00.

* AT SELECTION-SCREEN.

PERFORM SCHECKDATEN_EINGABE USING PAR_RCHK
                                  PAR_STAP
                                  TEXTINFO.

TEXTVOID = SPACE.
IF PAR_NEUD EQ 'X'.                    "Neu drucken / reprint
  IF PAR_RCHK NE SPACE.
    SET CURSOR FIELD 'PAR_RCHK'.
    MESSAGE E561(FS).                  "kein Neu drucken bei Restart
  ENDIF.                               "no reprint in restart mode
  IF ZW_XVORL NE SPACE.
    SET CURSOR FIELD 'ZW_XVORL'.
    MESSAGE E561(FS).                  "kein Neu drucken bei Vorschlag
  ENDIF.                               "no reprint if proposal run
  IF PAR_CHKF EQ SPACE AND PAR_CHKT NE SPACE.
    PAR_CHKF = PAR_CHKT.
  ENDIF.
  IF PAR_CHKT EQ SPACE.
    PAR_CHKT = PAR_CHKF.
  ENDIF.
  IF PAR_CHKT LT PAR_CHKF.
    SET CURSOR FIELD 'PAR_CHKF'.
    MESSAGE E650(DB).
  ENDIF.
  IF PAR_CHKF NE SPACE OR PAR_VOID NE 0.
    IF PAR_CHKF EQ SPACE.
      SET CURSOR FIELD 'PAR_CHKF'.
      MESSAGE E055(00).
    ENDIF.
    SELECT * FROM PAYR UP TO 1 ROWS    "im angegebenen Intervall müssen
      WHERE ZBUKR EQ ZW_ZBUKR-LOW      "Schecks vorhanden sein
      AND HBKID EQ SEL_HBKI-LOW        "check interval is not allowed to
        AND HKTID EQ SEL_HKTI-LOW      "be empty
        AND CHECF LE PAR_CHKT
        AND CHECT GE PAR_CHKF
        AND ICHEC EQ SPACE
        AND VOIDR EQ 0
        AND XBANC EQ SPACE.
    ENDSELECT.
    IF SY-SUBRC NE 0.
      SET CURSOR FIELD 'PAR_CHKF'.
      MESSAGE E509(FS).
    ENDIF.
    SELECT SINGLE * FROM TVOID WHERE VOIDR EQ PAR_VOID.
    IF SY-SUBRC NE 0 OR TVOID-XSYSE NE SPACE.
      SET CURSOR FIELD 'PAR_VOID'.
      MESSAGE E539(FS).
    ELSE.
      SELECT SINGLE * FROM TVOIT
        WHERE LANGU EQ SY-LANGU AND VOIDR EQ PAR_VOID.
      TEXTVOID = TVOIT-VOIDT.
    ENDIF.
  ENDIF.
ELSE.
  CLEAR:
    PAR_CHKF,
    PAR_CHKT,
    PAR_VOID.
ENDIF.



AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 1.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
    IF SCREEN-NAME EQ 'ZW_ZBUKR-HIGH' OR
       SCREEN-NAME EQ '%_ZW_ZBUKR_%_APP_%-VALU_PUSH'.
      SCREEN-ACTIVE = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PAR_STAP.
  CALL FUNCTION 'F4_CHECK_LOT'
       EXPORTING
            I_XDYNP      = 'X'
            I_DYNP_PROGN = 'RFFOUS_C'
            I_DYNP_DYNNR = '1000'
            I_DYNP_ZBUKR = 'ZW_ZBUKR-LOW'
            I_DYNP_HBKID = 'SEL_HBKI-LOW'
            I_DYNP_HKTID = 'SEL_HKTI-LOW'
       IMPORTING
            E_STAPL      = PAR_STAP
       EXCEPTIONS
            OTHERS       = 0.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PAR_ZFOR.
  PERFORM F4_FORMULAR USING PAR_ZFOR.

AT SELECTION-SCREEN ON PAR_ZFOR.
  IF PAR_ZFOR NE SPACE.
    SET CURSOR FIELD 'PAR_ZFOR'.
    CALL FUNCTION 'FORM_CHECK' EXPORTING I_PZFOR = PAR_ZFOR.
  ENDIF.



*----------------------------------------------------------------------*
*  batch heading (for the payment summary list)                        *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

  IF FLG_BEGLEITL EQ 1.
    PERFORM KOPF_ZEILEN.               "RFFORI07
  ENDIF.



*----------------------------------------------------------------------*
*  preparations                                                        *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  HLP_AUTH  = PAR_AUTH.                "spool authority
  HLP_TEMSE  = '----------'.           "Keine TemSe-Verwendung
  HLP_FILLER = PAR_FILL.
  PERFORM VORBEREITUNG.
  PERFORM SCHECKDATEN_PRUEFEN USING PAR_RCHK
                                    PAR_STAP.

  IF ZW_XVORL EQ SPACE AND PAR_ZDRU NE SPACE AND PAR_NEUD NE SPACE.
    IF PAR_CHKF NE SPACE.
      FLG_NEUD = 1.                    "neu drucken durchs Druckprogramm
      REFRESH TAB_CHECK.               "print program reprints checks
      TAB_CHECK-OPTION = 'EQ'.
      TAB_CHECK-SIGN   = 'I'.
      TAB_CHECK-HIGH   = SPACE.
      SELECT * FROM PAYR
        WHERE ZBUKR EQ ZW_ZBUKR-LOW
          AND HBKID EQ SEL_HBKI-LOW
          AND HKTID EQ SEL_HKTI-LOW
          AND CHECF LE PAR_CHKT
          AND CHECT GE PAR_CHKF
          AND ICHEC EQ SPACE
          AND VOIDR EQ 0
          AND XBANC EQ SPACE.
        TAB_CHECK-LOW = PAYR-CHECF.
        APPEND TAB_CHECK.
      ENDSELECT.
      INSERT *PAYR INTO DATEN.
    ELSE.
      REFRESH TAB_CHECK.
      FLG_NEUD = 2.
    ENDIF.
    SELECT SINGLE * FROM TVOID WHERE VOIDR EQ PAR_VOID.
  ENDIF.

  IF PAR_ZDRU EQ 'X'.
    IF SY-CALLD EQ SPACE.              "fremder Enqueue nur wenn
      PAR_NENQ = SPACE.                "Programm gerufen wurde
    ENDIF.                             "foreign enqueue only if called
    IF PAR_NENQ EQ SPACE.
      PERFORM SCHECKNUMMERN_SPERREN.   "RFFORI01
    ENDIF.
  ENDIF.



*----------------------------------------------------------------------*
*  check and extract data                                              *
*----------------------------------------------------------------------*
GET REGUH.

  CHECK SEL_ZAWE.
  CHECK SEL_UZAW.
  CHECK SEL_GSBR.
  CHECK SEL_HBKI.
  CHECK SEL_HKTI.
  CHECK SEL_WAER.
  CHECK SEL_VBLN.
  PERFORM PRUEFUNG.
  PERFORM SCHECKINFO_PRUEFEN.            "RFFORI01
  PERFORM EXTRACT_VORBEREITUNG.


GET REGUP.

  PERFORM EXTRACT.
  IF REGUH-ZBUKR NE REGUP-BUKRS.
    TAB_UEBERGREIFEND-ZBUKR = REGUH-ZBUKR.
    TAB_UEBERGREIFEND-VBLNR = REGUH-VBLNR.
    COLLECT TAB_UEBERGREIFEND.
  ENDIF.



*----------------------------------------------------------------------*
*  print checks, remittance advices and lists                          *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF FLG_SELEKTIERT NE 0.

    IF PAR_ZDRU EQ 'X'.
      HLP_ZFORN = PAR_ZFOR.
      HLP_CHECF_RESTART = PAR_RCHK.
      PERFORM SCHECK.                    "RFFORI01
      IF PAR_NENQ EQ SPACE.
        PERFORM SCHECKNUMMERN_ENTSPERREN."RFFORI01
      ENDIF.
    ENDIF.

    IF PAR_AVIS EQ 'X'.
      PERFORM AVIS.                      "RFFORI06
    ENDIF.

    IF PAR_BEGL EQ 'X' AND PAR_MAXP GT 0.
      FLG_BANKINFO = 1.
      PERFORM BEGLEITLISTE.              "RFFORI07
    ENDIF.

  ENDIF.

  PERFORM FEHLERMELDUNGEN.

  PERFORM INFORMATION.



*----------------------------------------------------------------------*
*  subroutines for check print and prenumbered checks                  *
*----------------------------------------------------------------------*
* INCLUDE RFFORI01.                                                 "UGL
  INCLUDE znfap001.                                                 "UGL



*----------------------------------------------------------------------*
*  subroutines for remittance advices                                  *
*----------------------------------------------------------------------*
* INCLUDE RFFORI06.                                                 "UGL
  INCLUDE znfap002.                                                 "UGL



*----------------------------------------------------------------------*
*  subroutines for the payment summary list                            *
*----------------------------------------------------------------------*
  INCLUDE RFFORI07.



*----------------------------------------------------------------------*
*  international subroutines                                           *
*----------------------------------------------------------------------*
  INCLUDE RFFORI99.
