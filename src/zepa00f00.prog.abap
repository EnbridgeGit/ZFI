************************************************************************
*  P30K102051 / NOTE - 65205  RFSEPA02: Prüfung Konto nicht vollständig
*  P30K125324 / NOTE - 65877  RFSEPA01: Prüfung Konto zu stark
*  ALRK036824 / Erweiterung von CHECK_ACCOUNT für RFSEPA04
*  S02K90004 / NOTE 175960 - gymana - IFRS
************************************************************************

*-------------------------------------------------------------------
***INCLUDE EPA00F00 .
*-------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTHORITY_SKA1_BUK
*&---------------------------------------------------------------------*
*       Check Authority for BUKRS                                      *
*----------------------------------------------------------------------*
FORM CHECK_AUTHORITY_SKA1_BUK.

  AUTHORITY-CHECK OBJECT 'F_SKA1_BUK'
    ID 'BUKRS' FIELD P_BUKRS
    ID 'ACTVT' FIELD '02'.
  IF SY-SUBRC <> 0.
    MESSAGE E113 WITH P_BUKRS.
  ENDIF.
ENDFORM.                               " CHECK_AUTHORITY_SKA1_BUK

*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTHORITY_SKA1_BES
*&---------------------------------------------------------------------*
*       Check authority for BEGRU
*----------------------------------------------------------------------*
FORM CHECK_AUTHORITY_SKA1_BES.

  IF SKB1-BEGRU <> SPACE.

    AUTHORITY-CHECK OBJECT 'F_SKA1_BES'
      ID 'BRGRU' FIELD SKB1-BEGRU
      ID 'ACTVT' FIELD '02'.

    IF SY-SUBRC <> 0.
      MESSAGE E119.
    ENDIF.
  ENDIF.

ENDFORM.                               " CHECK_AUTHORITY_SKA1_BES

*&---------------------------------------------------------------------*
*&      Form  CHECK_ACCOUNT
*&---------------------------------------------------------------------*
FORM CHECK_ACCOUNT.

  SELECT SINGLE * FROM T001
         WHERE BUKRS = P_BUKRS.

  SELECT SINGLE * FROM SKA1
         WHERE KTOPL = T001-KTOPL
           AND SAKNR = P_SAKNR.
  IF SY-SUBRC NE 0.
    MESSAGE E004(FH) WITH P_SAKNR T001-KTOPL.
*   Konto & im Kontenplan & nicht vorgesehen
  ENDIF.

  SELECT SINGLE * FROM SKB1 INTO *SKB1
         WHERE  BUKRS       = P_BUKRS
         AND    SAKNR       = P_SAKNR.
  IF SY-SUBRC NE 0.
    MESSAGE E005 WITH P_SAKNR P_BUKRS.
*   Konto & im Buchungskreis & nicht vorgesehen
  ENDIF.

* all other checks are not relevant for RFSEPA04
  CHECK SY-REPID NE 'RFSEPA04'.

  IF *SKB1-XSPEB EQ SPACE AND SKA1-XSPEB EQ SPACE.
    MESSAGE E134 WITH P_SAKNR.
*   Konto & ist nicht zum Buchen gesperrt. -> Langtext
  ENDIF.
  SKB1 = *SKB1.

*  CHECK SY-REPID = 'RFSEPA02'.        "<<<< INSERT - NOTE 0065877
  CHECK SY-REPID = 'ZFSEPA02'.        "<<<< INSERT - NOTE 175960

*>>>> BEGIN OF INSERTION - NOTE 65205 <<<<
*------------------OP-Verwaltung nicht bei Kursdifferenzschlüssel ----*
  IF SKB1-KDFSL NE SPACE.
    MESSAGE E141.
*   Konto enthält Kursdifferenzschlüssel. OP-Verwaltung ist nicht vorge
  ENDIF.
*------------------OP-Verwaltung nur bei Bilanzkonten-----------------*
  IF SKA1-XBILK = SPACE.
    MESSAGE E017.
*   OP-Verwaltung nur für Bestandskonten erlaubt
  ENDIF.
*------------------Mitbuchkonto kann nicht offene Posten sein --------*
  IF SKB1-MITKZ <> SPACE.
    MESSAGE E031.
*   Offene Postenverwaltung bei Abstimmkonto nicht vorgesehen
  ENDIF.
*------------------OP-Verwaltung muß immer EP-Verwaltung haben -------*
  IF SKB1-XKRES = SPACE.
    MESSAGE E106.
*   Offene-Posten-Verwaltung nur bei Einzelpostenanzeige vorgesehen
  ENDIF.

*>>>> END OF INSERTION - NOTE 65205 <<<<
ENDFORM.                               " CHECK_ACCOUNT

*&---------------------------------------------------------------------*
*&      Form  SAVE_PROTOCOL
*&---------------------------------------------------------------------*
FORM SAVE_PROTOCOL USING BELNR     LIKE BKPF-BELNR
                         GJAHR     LIKE BKPF-GJAHR
                         SORT      TYPE N
                         TABNAME   TYPE C
                         TYPCHANGE TYPE C
                         DBCNR     LIKE SY-TFILL.
  IF P_LISTE = 'X'.
    T_LISTE-GJAHR = GJAHR.
    T_LISTE-BELNR = BELNR.
    T_LISTE-SORT  = SORT.
    T_LISTE-TABLE = TABNAME.
    T_LISTE-TYPE  = TYPCHANGE.
    T_LISTE-OK    = DBCNR.
    APPEND T_LISTE.
  ENDIF.
  IF DBCNR = 1.
    CASE TABNAME.
      WHEN 'BSEG'.
        CASE TYPCHANGE.
          WHEN 'U'.
            ADD 1 TO NUM_BSEG_UPDATE.
          WHEN 'S'.
            ADD 1 TO NUM_BSEG_SELECT.
        ENDCASE.
      WHEN 'BSIS'.
        CASE TYPCHANGE.
          WHEN CHAR_I.
            ADD 1 TO NUM_BSIS_INSERT.
          WHEN CHAR_U.
            ADD 1 TO NUM_BSIS_UPDATE.
          WHEN CHAR_S.
            ADD 1 TO NUM_BSIS_SELECT.
        ENDCASE.
      WHEN 'BSAS'.
        CASE TYPCHANGE.
          WHEN CHAR_I.
            ADD 1 TO NUM_BSAS_INSERT.
          WHEN CHAR_U.
            ADD 1 TO NUM_BSAS_UPDATE.
          WHEN CHAR_S.
            ADD 1 TO NUM_BSAS_SELECT.
        ENDCASE.
    ENDCASE.
  ENDIF.
ENDFORM.                               " SAVE_PROTOCOL
