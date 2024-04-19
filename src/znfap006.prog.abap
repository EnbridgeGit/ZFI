* 2004/09/13 mdemeest 4.6C upgrade - Copied from RFFOCA_T           UGL
*                                    Change RFFORIC9 to ZNFA007     UGL
************************************************************************
*                                                                      *
*  EFT program for bank transfer RFFOCA_T (Canada) - standard 005      *
*                                                                      *
************************************************************************

*----------------------------------------------------------------------*
* The program includes:                                                *
*                                                                      *
* RFFORI0M  definition of macros                                       *
* RFFORI00  international data definitions                             *
* RFFORIC9  domestic transfer (DME) Canada (standard 005)              *
* RFFORI06  remittance advice                                          *
* RFFORI07  payment summary list                                       *
* RFFORI99  international subroutines                                  *
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* Report Header                                                        *
*----------------------------------------------------------------------*
REPORT RFFOCA_T
  LINE-SIZE 132
  MESSAGE-ID F0
  NO STANDARD PAGE HEADING.



*----------------------------------------------------------------------*
*  Segments                                                            *
*----------------------------------------------------------------------*
TABLES:
  REGUH,
  REGUP,
  RFSDO.


*----------------------------------------------------------------------*
*  Macro definitions                                                   *
*----------------------------------------------------------------------*
INCLUDE RFFORI0M.

INITIALIZATION.

*----------------------------------------------------------------------*
*  Parameters / Select-Options                                         *
*----------------------------------------------------------------------*
  BLOCK 1.
  SELECT-OPTIONS:
    SEL_ZAWE FOR  REGUH-RZAWE,         "Zahlwege / payment methods
    SEL_UZAW FOR  REGUH-UZAWE,         "Zahlwegzusatz
    SEL_HBKI FOR  REGUH-HBKID,         "house bank short key
    SEL_HKTI FOR  REGUH-HKTID,         "account data short key
    SEL_WAER FOR  REGUH-WAERS,         "currency
    SEL_VBLN FOR  REGUH-VBLNR.         "payment document number
  SELECTION-SCREEN END OF BLOCK 1.

  BLOCK 2.
  AUSWAHL: XDTA W, AVIS A, BEGL B.
  SPOOL_AUTHORITY.                     "Spoolberechtigung
  SELECTION-SCREEN END OF BLOCK 2.

  BLOCK 3.
  PARAMETERS:
    PAR_UNIX LIKE RFPDO2-FORDNAMD,     "Dateiname für DTA und TemSe
    PAR_DTYP LIKE RFPDO-FORDDTYP,      "Ausgabeformat und -medium
    PAR_CRLF LIKE RFPDO1-FORDCRLF,     "Auswahl des Zeilenvorschubs
    PAR_ANZP LIKE RFPDO-FORDANZP,      "number of test prints
    PAR_MAXP LIKE RFPDO-FORDMAXP,      "number of items in summary list
    PAR_BELP LIKE RFPDO-FORDBELP,      "payment doc. validation
    PAR_ESPR LIKE RFPDO-FORDESPR,      "texts in reciepient's lang.
    PAR_ISOC LIKE RFPDO-FORDISOC.      "currency in ISO code
  SELECTION-SCREEN END OF BLOCK 3.

  PARAMETERS:
*   re-transmission without new CRNO
    par_retr as checkbox,
    par_crno like dtadcah-crno,                "file creation number
    PAR_ANZB LIKE RFPDO2-FORDANZB NO-DISPLAY,
    PAR_ZDRU LIKE RFPDO-FORDZDRU  NO-DISPLAY,
    PAR_PRIZ LIKE RFPDO-FORDPRIZ  NO-DISPLAY,
    PAR_SOFZ LIKE RFPDO1-FORDSOFZ NO-DISPLAY,
    PAR_VARI(12) TYPE C           NO-DISPLAY,
    PAR_SOFO(1)  TYPE C           NO-DISPLAY.



*----------------------------------------------------------------------*
*  default values for parameters and select-options                    *
*----------------------------------------------------------------------*
  PERFORM INIT.
  SEL_ZAWE-LOW     = 'T'.
  SEL_ZAWE-OPTION  = 'EQ'.
  SEL_ZAWE-SIGN    = 'I'.
  APPEND SEL_ZAWE.

  PAR_BELP = SPACE.
  PAR_ZDRU = SPACE.
  PAR_XDTA = 'X'.
  PAR_DTYP = '0'.
  PAR_CRLF = 3.
  PAR_AVIS = 'X'.
  PAR_BEGL = 'X'.
  PAR_ANZP = 2.
  PAR_ANZB = 2.
  PAR_ESPR = SPACE.
  PAR_ISOC = SPACE.
  PAR_MAXP = 9999.



*----------------------------------------------------------------------*
*  tables / fields / field-groups / AT SELECTION SCREEN                *
*----------------------------------------------------------------------*
INCLUDE RFFORI00.


*- special checks for Canadian DME--------------------------------------
IF PAR_XDTA EQ 'X'.
  IF PAR_DTYP EQ SPACE.                "0 = TemSe (default)
    PAR_DTYP = '0'.                    "2 = file system
  ENDIF.
  IF PAR_DTYP NA '02'.
    SET CURSOR FIELD 'PAR_DTYP'.
    MESSAGE E068.
  ENDIF.
ENDIF.

if      not par_retr  is initial
   and      par_crno  is initial.
     message e605.
endif.

if          par_retr  is initial
   and  not par_crno  is initial.
     message e606.
endif.


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
  HLP_TEMSE = '0---------'.
  PERFORM VORBEREITUNG.



*----------------------------------------------------------------------*
*  check and extract data                                              *
*----------------------------------------------------------------------*
GET REGUH.

  CHECK SEL_ZAWE.
  CHECK SEL_UZAW.
  CHECK SEL_HBKI.
  CHECK SEL_HKTI.
  CHECK SEL_WAER.
  CHECK SEL_VBLN.
  PERFORM PRUEFUNG.
  PERFORM EXTRACT_VORBEREITUNG.


GET REGUP.

  PERFORM EXTRACT.



*----------------------------------------------------------------------*
*  print forms, DME, remittance advices and lists                      *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF FLG_SELEKTIERT NE 0.

    IF PAR_XDTA EQ 'X'.
      PERFORM DME_CANADA.              "RFFORIC9
    ENDIF.

    IF PAR_AVIS EQ 'X'.
      PERFORM AVIS.                    "RFFORI06
    ENDIF.

    IF PAR_BEGL EQ 'X' AND PAR_MAXP GT 0.
      FLG_BANKINFO = 2.
      PERFORM BEGLEITLISTE.            "RFFORI07
    ENDIF.

  ENDIF.

  PERFORM FEHLERMELDUNGEN.

  PERFORM INFORMATION.



*----------------------------------------------------------------------*
*  subroutine for DME                                                  *
*----------------------------------------------------------------------*
*  INCLUDE RFFORIC9.                                                 UGL
  INCLUDE ZNFAP007.                                                 "UGL


*----------------------------------------------------------------------*
*  subroutine for remittance advices                                   *
*----------------------------------------------------------------------*
  INCLUDE RFFORI06.


*----------------------------------------------------------------------*
*  subroutine for the summary list                                     *
*----------------------------------------------------------------------*
  INCLUDE RFFORI07.


*----------------------------------------------------------------------*
*  international subroutines                                           *
*----------------------------------------------------------------------*
  INCLUDE RFFORI99.
