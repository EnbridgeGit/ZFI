REPORT ZFFII007 MESSAGE-ID ZS.
************************************************************************
* 2002/04/29 mdemeest #--- Copied ZFFII007 as base
************************************************************************
* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETER: PHYFILE LIKE FILENAMECI-FILEEXTERN
                 DEFAULT '/usr/sap/interfaces/P01/CFMM001/ZFFII009.TXT'.
PARAMETER: RECSIZE TYPE I.
SELECTION-SCREEN END OF BLOCK BOX.
SKIP 1.
SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
PARAMETER: P_BUKRS LIKE BKPF-BUKRS DEFAULT 'UGL'.
PARAMETER: P_GJAHR LIKE BKPF-GJAHR DEFAULT SY-DATUM(4).
PARAMETER: P_MONAT LIKE BKPF-MONAT OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BOX3.

TABLES: BKPF, BSEG.

* Global Data Definitions
DATA: RECORD(144).


DATA: BEGIN OF WArec OCCURS 50000,
      BUDAT(8)    type c,
      BKTXT       LIKE BKPF-BKTXT,
      BUKRS       LIKE BKPF-BUKRS,
      aufnr       like bseg-aufnr,
      KOSTL       LIKE BSEG-KOSTL,
      PROD_COdE(3) type c,
*     SAKNR    LIKE BSEG-SAKNR,     "per Wilma Strybosch
      hkont       like bseg-hkont,
      dmbtr(13)  type c,
      BELNR LIKE BKPF-BELNR,
      end of warec.

PERFORM OPEN_OUTPUT_FILE.
perform column_headings.

SELECT * FROM BKPF                       "Selection based on fiscal year
   WHERE BUKRS = P_BUKRS                 "and month
     AND GJAHR = P_GJAHR
     AND MONAT = P_MONAT.

*  PERFORM MOVE_INFO_BKPF.                       "Move info to work area
*  TRANSFER WABKPF TO PHYFILE  LENGTH RECSIZE.   "Output BKPF

   SELECT * FROM BSEG
      WHERE BUKRS = BKPF-BUKRS
       AND BELNR = BKPF-BELNR
       AND GJAHR = BKPF-GJAHR.

     PERFORM MOVE_INFO_warec.                    "Move info to work area
     TRANSFER WArec TO PHYFILE LENGTH RECSIZE.
  ENDSELECT.                                     "END of BSEG
ENDSELECT.                                       "END of BKPF

loop at warec.
  write: / warec-budat under text-010,
           warec-bktxt under text-011,
           warec-bukrs under text-012,
           warec-aufnr under text-013,
           warec-kostl under text-014,
           warec-prod_code under text-015,
           warec-hkont under text-016,
           warec-dmbtr under text-017,
           warec-belnr under text-018.
endloop.


CLOSE DATASET: PHYFILE.


*********************  OPEN_OUTPUT_FILE  *******************************
* Routine to convert the logical file name provided as input
* to a physical file name and then to open the physical file
* for output in text mode.
************************************************************************
FORM OPEN_OUTPUT_FILE.
      OPEN DATASET PHYFILE FOR OUTPUT IN TEXT MODE.
ENDFORM.
*---------------------- MOVE_INFO_WAREC --------------------------------
* move fields from BSEG to OUTPUT RECORD
*-----------------------------------------------------------------------
FORM MOVE_INFO_warec.
  CLEAR WArec.
  MOVE: '////////'        to warec-budat,    "Posting date as requested
        BKPF-BUDAT+6(2)   TO WArec-BUDAT(2),
        bkpf-budat+4(2)   to warec-budat+3(2),
        bkpf-budat+2(2)   to warec-budat+6(2).

  if bseg-shkzg = 'H'.
     compute bseg-dmbtr = bseg-dmbtr * -1.
  endif.


  move:  BKPF-BKTXT        TO WArec-BKTXT,
        BKPF-BUKRS        TO WArec-BUKRS,
        BSEG-KOSTL        TO WArec-KOSTL,
        '000'             to warec-prod_code,
        bseg-hkont        to warec-hkont,
        bseg-aufnr        to warec-aufnr,
        BSEG-DMBTR        TO WArec-DMBTR,
        BKPF-BELNR        TO WArec-BELNR.
   append warec.

ENDFORM.

form column_headings.
write: /1  text-010,  10 text-011,  40 text-012, 45 text-013,
       60  text-014,  75 text-015,  80 text-016, 95 text-017,
      110  text-018.
endform.
