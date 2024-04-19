REPORT ZFGLR011 LINE-COUNT 58 LINE-SIZE 170 NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
*  Author: Selwyn Rodricks                                             *
*          OmniLogic Systems Group                                     *
*  Brief Description:                                                  *
*  - FI Postings as at certain date & time                             *
****** ****** ****** ****** ****** ****** ****** ****** ****** ****** **
*  modified by Nancy Gilligan, OmniLogic 98/10  D30K906131             *
*    - standardized headers, set company code based on client          *
*                                                                      *
*----------------------------------------------------------------------*
TABLES: BKPF, BSEG, TBSL, T001.

DATA:   DRAMT  LIKE BSEG-WRBTR,
        CRAMT  LIKE BSEG-WRBTR.

FIELD-GROUPS: HEADER, DATA.

INSERT BSEG-BUKRS                      "comp.code
  INTO HEADER.

INSERT "BKPF-BUDAT                      "Posting date
       "BSEG-BELNR                      "Doc.#
       DRAMT                           "Amount
       CRAMT                           "Amount
  INTO DATA.

PARAMETERS:     BUKRS LIKE BKPF-BUKRS,
                HKONT LIKE BSEG-HKONT,
                GJAHR LIKE BKPF-GJAHR,
                CPUDT LIKE BKPF-CPUDT,
                CPUTM LIKE BKPF-CPUTM.

SELECT-OPTIONS: USNAM FOR  BKPF-USNAM.

* initialization                                         ngilligan 98/10
INITIALIZATION.                                         "ngilligan 98/10
IF SY-MANDT+2(1) = '1'.                                 "ngilligan 98/10
   BUKRS = 'UEC'.                                       "ngilligan 98/10
ELSEIF  SY-MANDT+2(1) = '0'.                            "ngilligan 98/10
   BUKRS = 'UGL'.                                       "ngilligan 98/10
ENDIF.                                                  "ngilligan 98/10


TOP-OF-PAGE.
  WRITE: /1 TEXT-RPT, SY-REPID INTENSIFIED ON,          "ngilligan 98/10
         25 T001-BUTXT INTENSIFIED ON,                  "ngilligan 98/10
         80 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.     "ngilligan 98/10

  WRITE: /1 TEXT-CLT, SY-MANDT UNDER SY-REPID,          "ngilligan 98/10
                                        SY-SYSID.       "ngilligan 98/10
  WRITE: TEXT-HED UNDER T001-BUTXT.                     "ngilligan 98/10
  WRITE: TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.             "ngilligan 98/10
       SKIP 1.                                          "ngilligan 98/10
  ULINE.                                                "ngilligan 98/10


* START OF MAIN PROGRAM
START-OF-SELECTION.

* get company code text                                  ngilligan 98/10
SELECT SINGLE BUTXT FROM T001 INTO T001-BUTXT WHERE BUKRS = BUKRS.

SELECT * FROM BKPF WHERE BUKRS =  BUKRS  AND
                         GJAHR =  GJAHR  AND
                         CPUDT <= CPUDT  AND
                         USNAM IN USNAM.
  IF CPUDT = CPUDT.
    CHECK CPUTM <= CPUTM.
  ENDIF.
  SELECT * FROM BSEG WHERE BUKRS = BKPF-BUKRS AND
                           BELNR = BKPF-BELNR AND
                           GJAHR = BKPF-GJAHR AND
                           HKONT = HKONT.
    DRAMT = 0.
    CRAMT = 0.
    SELECT SINGLE * FROM TBSL WHERE BSCHL = BSEG-BSCHL.
    IF SY-SUBRC = 0.
      IF TBSL-SHKZG = 'H'.
        MOVE BSEG-WRBTR TO CRAMT.
      ELSE.
        MOVE BSEG-WRBTR TO DRAMT.
      ENDIF.
    ENDIF.

    EXTRACT DATA.
  ENDSELECT.
ENDSELECT.

WRITE: / 'Parameters'.
WRITE: / '------------------------',
       / 'Company Code:', BUKRS,
       / 'Year        :', GJAHR,
       / 'G/L Account :', HKONT,
       / 'CPU Date    :', CPUDT,
       / 'CPU Time    :', CPUTM.
WRITE: / '------------------------'.
WRITE: / 'Users       :'.

LOOP AT USNAM.
  WRITE: / USNAM-SIGN,
           USNAM-OPTION,
           USNAM-LOW.
ENDLOOP.
WRITE: / '------------------------'.


SORT.
LOOP.
* WRITE: / BKPF-BUDAT, BSEG-BELNR, BKPF-CPUDT, BKPF-CPUTM,
*                      DRAMT, CRAMT.
  AT END OF BSEG-BUKRS.
    WRITE: / 'Total DR:', SUM(DRAMT),
           / 'Total CR:', SUM(CRAMT).
  ENDAT.
ENDLOOP.
