REPORT ZFGLR018 LINE-SIZE 120 NO STANDARD PAGE HEADING.
************************************************************************
* PROGRAM:           ZFGLR018
* UTILITY PROGAM: FIND WRONG POPULATED FIELD BSEG-KSTRG
* AUTHOR :           NESH N. LAURENCIC / OMNILOGIC SYS. GROUP
************************************************************************


TABLES: BSEG, BKPF.

DATA: BEGIN OF IBSEG ,
      SHKZG LIKE BSEG-SHKZG,
      PSWBT LIKE BSEG-PSWBT,
      KSTRG LIKE BSEG-KSTRG,
      END OF IBSEG.

DATA: BEGIN OF IBKPF OCCURS 25000,
      BUKRS LIKE BSEG-BUKRS,
      BELNR LIKE BSEG-BELNR,
      GJAHR LIKE BSEG-GJAHR,
      BUDAT LIKE BKPF-BUDAT,
      USNAM LIKE BKPF-USNAM,
      END OF IBKPF.

PARAMETERS: P1 TYPE I OBLIGATORY DEFAULT '50' .
PARAMETERS: PBUKRS LIKE BSEG-BUKRS OBLIGATORY,
            PGJAHR LIKE BKPF-GJAHR DEFAULT '1997' OBLIGATORY.
SELECT-OPTIONS S_MONAT     FOR BKPF-MONAT OBLIGATORY.

DATA: INDEX TYPE I VALUE '0'.
DATA: STOP TYPE I VALUE '0'.

START-OF-SELECTION.

  SELECT BUKRS BELNR GJAHR BUDAT USNAM
         FROM BKPF
         INTO TABLE IBKPF
         WHERE       BUKRS EQ PBUKRS
         AND         GJAHR EQ PGJAHR
         AND         MONAT IN S_MONAT.

CLEAR IBKPF.
  LOOP AT IBKPF.
    SELECT SHKZG PSWBT KSTRG
         INTO IBSEG
         FROM BSEG
         WHERE           BUKRS EQ IBKPF-BUKRS
         AND             BELNR EQ IBKPF-BELNR
         AND             GJAHR EQ IBKPF-GJAHR
         AND             KSTRG NE SPACE
         AND             KSTRG NE '800411'.

      INDEX = INDEX + 1.
      FORMAT COLOR 2.
      WRITE: / INDEX UNDER TEXT-999,
               IBKPF-BELNR UNDER TEXT-001,
               IBKPF-USNAM UNDER TEXT-002,
               IBSEG-SHKZG UNDER TEXT-003,
               IBSEG-PSWBT UNDER TEXT-004,
               IBKPF-BUDAT UNDER TEXT-005,
               IBSEG-KSTRG UNDER TEXT-006.
      FORMAT RESET.
      IF INDEX = P1.
      ULINE.
        WRITE: / INDEX, 'Number of Hits reached ' COLOR 6.
        STOP = '1'.
        EXIT.
      ENDIF.

    ENDSELECT.

    IF STOP = '1'.
    CLEAR STOP.
    EXIT.
    ENDIF.

    CHECK SY-SUBRC = 0.
  ENDLOOP.

  IF INDEX IS INITIAL.
    WRITE: / TEXT-100.
  ENDIF.

TOP-OF-PAGE.
  FORMAT RESET.
  FORMAT COLOR 1.
  WRITE: /1 TEXT-RPT, SY-REPID COLOR 4 INTENSIFIED ON,
         22 SY-TITLE COLOR 4 INTENSIFIED ON,
        79 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.


  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID.

  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.

*  write: / 'Client:', sy-mandt.
*  write: 60 'Time:', sy-uzeit.
  SKIP.
  FORMAT COLOR 1.
  WRITE: /1 TEXT-999,
        12 TEXT-001, 22 TEXT-002, 32 TEXT-003, 36 TEXT-004, 55 TEXT-005,
          70 TEXT-006.
  ULINE.
  FORMAT RESET.