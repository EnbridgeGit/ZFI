REPORT RFBIBLI0.
*-----------------------------------------------------------------------
*        Batch-Input für Buchhaltungsbelege:
*        Initialisierungsroutine
*-----------------------------------------------------------------------

*------- Tabellen ------------------------------------------------------
TABLES:  BGR00,                        " Batch-Input Mappenvorsatz
         BBKPF,                        " Belegkopf + Tcode
         BBSEG,                        " Belegsegment
         BBTAX,                        " Belegsteuern
         BSELK,                        " Selektionskopfdaten
         BSELP.                        " Selektionspositionen

*------- Feldinformationen aus NAMETAB ---------------------------------
DATA:    BEGIN OF NAMETAB OCCURS 120.
           INCLUDE STRUCTURE DNTAB.
DATA:    END OF NAMETAB.

*------- Initialstrukturen ---------------------------------------------
DATA:    BEGIN OF I_BGR00.
           INCLUDE STRUCTURE BGR00.    " Mappenvorsatz
DATA:    END OF I_BGR00.

DATA:    BEGIN OF I_BBKPF.
           INCLUDE STRUCTURE BBKPF.    " Belegkopf
DATA:    END OF I_BBKPF.

DATA:    BEGIN OF I_BBSEG.
           INCLUDE STRUCTURE BBSEG.    " Belegsegment
DATA:    END OF I_BBSEG.

DATA:    BEGIN OF I_BBTAX.
           INCLUDE STRUCTURE BBTAX.    " Belegsteuern
DATA:    END OF I_BBTAX.

DATA:    BEGIN OF I_BSELK.
           INCLUDE STRUCTURE BSELK.    " Selektionskopfdaten
DATA:    END OF I_BSELK.

DATA:    BEGIN OF I_BSELP.
           INCLUDE STRUCTURE BSELP.    " Selektionspositionen
DATA:    END OF I_BSELP.

*------- Einzelfelder, Konstanten, Fieldsymbols ------------------------
DATA:    CHAR(21)  TYPE C.             " Hilfsfeld
FIELD-SYMBOLS: <F1> .

*eject
************************************************************************
*---------------------------------------------------------------------*
*       SUBROUTINE INIT_STRUKTUREN_ERZEUGEN                           *
*---------------------------------------------------------------------*
*       Routine wird durch externen Perform aufgerufen:               *
*       Erzeugen der Initialstrukturen für die Belegtabellen.         *
*       Die Initialstrukturen werden zum Programmbeginn für alle      *
*       Tabellen einmal erzeugt.                                      *
*       Satztyp wird gesetzt: '0' Mappenvorsatz                       *
*                             '1' Transaktionscode + Belegkopfdaten   *
*                             '2' Belegsegmentdaten  BBSEG            *
*                             '2' Selektionskopf     BSELK            *
*                             '2' Selektionsposition BSELP            *
*---------------------------------------------------------------------*
FORM INIT_STRUKTUREN_ERZEUGEN USING NODATA.
  CLEAR I_BGR00.
  I_BGR00-STYPE  = '0'.
  I_BGR00-NODATA = NODATA.

  PERFORM INIT_STRUCTURES USING 'BBKPF' I_BBKPF NODATA.
  I_BBKPF-STYPE = '1'.

  PERFORM INIT_STRUCTURES USING 'BBSEG' I_BBSEG NODATA.
  I_BBSEG-STYPE = '2'.
  I_BBSEG-TBNAM = 'BBSEG'.

  PERFORM INIT_STRUCTURES USING 'BBTAX' I_BBTAX NODATA.
  I_BBTAX-STYPE = '2'.
  I_BBTAX-TBNAM = 'BBTAX'.

  PERFORM INIT_STRUCTURES USING 'BSELK' I_BSELK NODATA.
  I_BSELK-STYPE = '2'.
  I_BSELK-TBNAM = 'BSELK'.

  PERFORM INIT_STRUCTURES USING 'BSELP' I_BSELP NODATA.
  I_BSELP-STYPE = '2'.
  I_BSELP-TBNAM = 'BSELP'.
ENDFORM.

*eject
*---------------------------------------------------------------------*
*       SUBROUTINE INIT_BGR00                                         *
*---------------------------------------------------------------------*
*       'Initialisieren' von BGR00 Mappenvorsatz                      *
*---------------------------------------------------------------------*
FORM INIT_BGR00 USING BGR00.
  BGR00 = I_BGR00.
ENDFORM.

*---------------------------------------------------------------------*
*       SUBROUTINE INIT_BBKPF                                         *
*---------------------------------------------------------------------*
FORM INIT_BBKPF USING BBKPF.
  BBKPF = I_BBKPF.
ENDFORM.

*---------------------------------------------------------------------*
*       SUBROUTINE INIT_BBSEG                                         *
*---------------------------------------------------------------------*
FORM INIT_BBSEG USING BBSEG.
  BBSEG = I_BBSEG.
ENDFORM.

*---------------------------------------------------------------------*
*       SUBROUTINE INIT_BBTAX                                         *
*---------------------------------------------------------------------*
FORM INIT_BBTAX USING BBTAX.
  BBTAX = I_BBTAX.
ENDFORM.

*---------------------------------------------------------------------*
*       SUBROUTINE INIT_BSELK                                         *
*---------------------------------------------------------------------*
FORM INIT_BSELK USING BSELK.
  BSELK = I_BSELK.
ENDFORM.

*---------------------------------------------------------------------*
*       SUBROUTINE INIT_BSELP                                         *
*---------------------------------------------------------------------*
FORM INIT_BSELP USING BSELP.
  BSELP = I_BSELP.
ENDFORM.

*eject
*---------------------------------------------------------------------*
*       SUBROUTINE INIT_STRUCTURES                                    *
*---------------------------------------------------------------------*
*       Intern aufgerufene Routine.                                   *
*       Erzeugen einer Initialstruktur.                               *
*       Das Sonderzeichen '!' dient als Kennzeichnung dafür,          *
*       dass für ein Tabellenfeld kein Batch-Input erfolgen soll.     *
*---------------------------------------------------------------------*
FORM INIT_STRUCTURES USING TABNAME TAB I_NODATA.
  REFRESH NAMETAB.
  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING  LANGU          = SY-LANGU
                  TABNAME        = TABNAME
       TABLES     NAMETAB        = NAMETAB
       EXCEPTIONS NO_TEXTS_FOUND = 1.
  LOOP AT NAMETAB.
    CLEAR CHAR.
    CHAR(2)    = 'I_'.
    CHAR+2(5)  = NAMETAB-TABNAME.
    CHAR+7(1)  = '-'.
    CHAR+8(10) = NAMETAB-FIELDNAME.
    ASSIGN (CHAR) TO <F1>.
    <F1> = I_NODATA.
  ENDLOOP.
ENDFORM.

